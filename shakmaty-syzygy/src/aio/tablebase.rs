use std::{
    cmp::{Reverse, max},
    ffi::OsStr,
    fmt, io,
    path::{Path, PathBuf},
};

use arrayvec::ArrayVec;
use async_lock::OnceCell;
use rustc_hash::FxHashMap;
use shakmaty::{Move, Position, Role};

use crate::{
    AmbiguousWdl,
    aio::{
        filesystem::Filesystem,
        table::{DtzTable, WdlTable},
    },
    errors::{ProbeResultExt as _, SyzygyError, SyzygyResult},
    material::{Material, NormalizedMaterial},
    types::{DecisiveWdl, Dtz, MaybeRounded, Metric, Syzygy, Wdl},
};

/// Additional probe information from a brief alpha-beta search.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum ProbeState {
    /// Normal probe.
    Normal,
    /// Best move is zeroing.
    ZeroingBestMove,
    /// Threatening to force a capture (in antichess variants, where captures
    /// are compulsory).
    Threat,
}

/// Table files, lazily opened, indexed by their normalized material key.
type TableSlots<T> = FxHashMap<NormalizedMaterial, (PathBuf, OnceCell<T>)>;

/// A collection of tables, backed by a custom provider for asynchronous I/O.
pub struct Tablebase<S, F: Filesystem> {
    filesystem: F,
    wdl: TableSlots<WdlTable<S, F::RandomAccessFile>>,
    dtz: TableSlots<DtzTable<S, F::RandomAccessFile>>,
    max_pieces: usize,
}

impl<S: Syzygy, F: Filesystem> fmt::Debug for Tablebase<S, F>
where
    S: fmt::Debug,
    F::RandomAccessFile: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Tablebase")
            .field("max_pieces", &self.max_pieces)
            .field("wdl", &self.wdl)
            .field("dtz", &self.dtz)
            .finish_non_exhaustive()
    }
}

impl<S, F: Filesystem> Tablebase<S, F> {
    /// Creates an empty collection of tables. A custom filesystem
    /// implementation will be used to read table files.
    pub fn with_filesystem(filesystem: F) -> Tablebase<S, F> {
        Tablebase {
            filesystem,
            wdl: FxHashMap::with_capacity_and_hasher(145, Default::default()),
            dtz: FxHashMap::with_capacity_and_hasher(145, Default::default()),
            max_pieces: 0,
        }
    }

    /// Returns the maximum number of pieces over all added tables.
    ///
    /// This number is updated when adding table files and very fast to read.
    #[inline]
    pub fn max_pieces(&self) -> usize {
        self.max_pieces
    }
}

impl<S: Syzygy, F: Filesystem> Tablebase<S, F> {
    /// Add all relevant tables from a directory.
    ///
    /// Tables are selected by filename, e.g., `KQvKP.rtbz`.
    ///
    /// The files are not actually opened. This happens lazily when probing.
    /// Eventually all files may be opened, so, depending on the underlying
    /// [`Filesystem`] implementation, configure resource limits accordingly.
    ///
    /// Probing generally requires tables for the specific material
    /// composition, as well as material compositions that are transitively
    /// reachable by captures and promotions. These are sometimes distributed
    /// separately, so make sure to add tables from all relevant directories.
    ///
    /// Returns the number of added table files.
    ///
    /// # Errors
    ///
    /// * General I/O errors forwarded from [`Filesystem::read_dir()`].
    /// * General I/O errors forwarded from [`Filesystem::regular_file_size()`].
    ///   Some tables may have already been added.
    /// * [`std::io::ErrorKind::InvalidData`] if there is a table in `path`
    ///   where the file size indicates that it must be corrupted. Some tables
    ///   may have already been added.
    pub async fn add_directory(&mut self, path: impl AsRef<Path>) -> io::Result<usize> {
        let mut num = 0;

        for entry in self.filesystem.read_dir(path.as_ref()).await? {
            match self.add_file_impl(&entry).await {
                Ok(()) => num += 1,
                Err(err) if err.kind() == io::ErrorKind::InvalidInput => (), // Not a table. Ignore
                Err(err) => return Err(err),
            }
        }

        Ok(num)
    }

    /// Add a table file.
    ///
    /// The file is not actually opened. This happens lazily when probing.
    ///
    /// Traverses symbolic links.
    ///
    /// # Errors
    ///
    /// Despite lazy opening, there are some immediate error conditions:
    ///
    /// * [`std::io::ErrorKind::InvalidInput`] if the filename does not indicate
    ///   that it is a valid table file (e.g., `KQvKP.rtbz`).
    /// * [`std::io::ErrorKind::InvalidInput`] if `path` is not pointing to a
    ///   regular file.
    /// * [`std::io::ErrorKind::InvalidData`] if the file size indicates that
    ///   the table file must be corrupted.
    /// * General I/O errors forwarded from [`Filesystem::regular_file_size()`].
    pub async fn add_file(&mut self, path: impl AsRef<Path>) -> io::Result<()> {
        self.add_file_impl(path.as_ref()).await
    }

    async fn add_file_impl(&mut self, path: &Path) -> io::Result<()> {
        // Validate filename.
        let Some(stem) = path.file_stem().and_then(OsStr::to_str) else {
            return Err(io::Error::from(io::ErrorKind::InvalidInput));
        };
        let Ok(material) = Material::from_str(stem) else {
            return Err(io::Error::from(io::ErrorKind::InvalidInput));
        };
        let pieces = material.count();
        if pieces > S::MAX_PIECES
            || material.by_color.white.count() < 1
            || material.by_color.black.count() < 1
        {
            return Err(io::Error::from(io::ErrorKind::InvalidInput));
        }

        // Check file extension.
        let Some(ext) = path.extension() else {
            return Err(io::Error::from(io::ErrorKind::InvalidInput));
        };
        let is_tbw = ext == S::TBW.ext
            || (!material.has_pawns() && S::PAWNLESS_TBW.is_some_and(|t| ext == t.ext));
        let is_tbz = ext == S::TBZ.ext
            || (!material.has_pawns() && S::PAWNLESS_TBZ.is_some_and(|t| ext == t.ext));
        if !is_tbw && !is_tbz {
            return Err(io::Error::from(io::ErrorKind::InvalidInput));
        }

        // Check meta data.
        if self.filesystem.regular_file_size(path).await? % 64 != 16 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "unexpected file size",
            ));
        }

        // Add path.
        let material = material.to_normalized();
        let path = path.to_path_buf();
        if is_tbw {
            self.wdl.insert(material, (path, OnceCell::new()));
        } else {
            self.dtz.insert(material, (path, OnceCell::new()));
        }
        self.max_pieces = max(self.max_pieces, pieces);
        Ok(())
    }

    async fn wdl_table(
        &self,
        material: &NormalizedMaterial,
    ) -> SyzygyResult<&WdlTable<S, F::RandomAccessFile>> {
        if let Some((path, table)) = self.wdl.get(material) {
            table
                .get_or_try_init(|| async {
                    WdlTable::new(self.filesystem.open(path).await?, material.inner()).await
                })
                .await
                .ctx(Metric::Wdl, material)
        } else {
            Err(SyzygyError::MissingTable {
                metric: Metric::Wdl,
                material: material.inner().clone(),
            })
        }
    }

    async fn dtz_table(
        &self,
        material: &NormalizedMaterial,
    ) -> SyzygyResult<&DtzTable<S, F::RandomAccessFile>> {
        if let Some((path, table)) = self.dtz.get(material) {
            table
                .get_or_try_init(|| async {
                    DtzTable::new(self.filesystem.open(path).await?, material.inner()).await
                })
                .await
                .ctx(Metric::Dtz, material)
        } else {
            Err(SyzygyError::MissingTable {
                metric: Metric::Dtz,
                material: material.inner().clone(),
            })
        }
    }
}

impl<S: Syzygy + Position + Clone, F: Filesystem> Tablebase<S, F> {
    /// Probe tables for the [`Wdl`] value of a position, assuming `pos`
    /// is reached directly after a capture or pawn move.
    ///
    /// Requires only WDL tables.
    ///
    /// # Errors
    ///
    /// See [`SyzygyError`] for possible error conditions.
    pub async fn probe_wdl_after_zeroing(&self, pos: &S) -> SyzygyResult<Wdl> {
        self.probe(pos).await.map(|entry| entry.wdl_after_zeroing())
    }

    /// Probe tables for the WDL value of a position, considering also
    /// the halfmove counter of `pos`. The result may be
    /// [ambiguous due to DTZ rounding](MaybeRounded).
    ///
    /// Requires both WDL and DTZ tables.
    ///
    /// # Errors
    ///
    /// See [`SyzygyError`] for possible error conditions.
    pub async fn probe_wdl(&self, pos: &S) -> SyzygyResult<AmbiguousWdl> {
        self.probe(pos)
            .await?
            .dtz()
            .await
            .map(|dtz| AmbiguousWdl::from_dtz_and_halfmoves(dtz, pos.halfmoves()))
    }

    /// Probe tables for the [`Dtz`] value of a position.
    ///
    /// Requires both WDL and DTZ tables.
    ///
    /// # Errors
    ///
    /// See [`SyzygyError`] for possible error conditions.
    pub async fn probe_dtz(&self, pos: &S) -> SyzygyResult<MaybeRounded<Dtz>> {
        self.probe(pos).await?.dtz().await
    }

    /// Get the recommended tablebase move.
    ///
    /// Following the tablebase mainline *starting from a capture or pawn move*
    /// guarantees achieving the optimal outcome under the 50-move rule.
    ///
    /// Otherwise (i.e., when not immediately following the tablebase mainline
    /// after the capture that crosses into tablebase territory),
    /// [some care needs to be taken due to DTZ rounding](MaybeRounded).
    ///
    /// Requires both WDL and DTZ tables.
    ///
    /// # Errors
    ///
    /// See [`SyzygyError`] for possible error conditions.
    pub async fn best_move(&self, pos: &S) -> SyzygyResult<Option<(Move, MaybeRounded<Dtz>)>> {
        const MAX_MOVES: usize = 256;

        struct WithAfter<S> {
            m: Move,
            after: S,
        }

        struct WithWdlEntry<'a, S, F: Filesystem> {
            m: Move,
            entry: WdlEntry<'a, S, F>,
        }

        struct WithDtz {
            m: Move,
            immediate_loss: bool,
            zeroing: bool,
            dtz: MaybeRounded<Dtz>,
        }

        // Build list of successor positions.
        let with_after = pos
            .legal_moves()
            .into_iter()
            .map(|m| {
                let mut after = pos.clone();
                after.play_unchecked(m);
                WithAfter { m, after }
            })
            .collect::<ArrayVec<_, MAX_MOVES>>();

        // Determine WDL for each move.
        let mut with_wdl = ArrayVec::<_, MAX_MOVES>::new();
        for e in with_after.iter() {
            let entry = self.probe(&e.after).await?;
            with_wdl.push(WithWdlEntry { m: e.m, entry });
        }

        // Consider only WDL-optimal moves.
        let best_wdl = with_wdl
            .iter()
            .map(|a| a.entry.wdl)
            .min()
            .unwrap_or(Wdl::Loss);

        with_wdl.retain(|a| a.entry.wdl == best_wdl);

        // Determine DTZ for each WDL-optimal move.
        let mut with_dtz = ArrayVec::<WithDtz, MAX_MOVES>::new();
        for e in with_wdl.iter() {
            let dtz = e.entry.dtz().await?;
            with_dtz.push(WithDtz {
                m: e.m,
                immediate_loss: dtz.ignore_rounding() == Dtz(-1)
                    && (e.entry.pos.is_checkmate() || e.entry.pos.variant_outcome().is_known()),
                zeroing: e.entry.state == ProbeState::ZeroingBestMove,
                dtz,
            });
        }

        // Select a DTZ-optimal move among the moves with best WDL.
        Ok(with_dtz
            .into_iter()
            .min_by_key(|m| {
                (
                    Reverse(m.immediate_loss),
                    m.zeroing ^ m.dtz.is_negative(), // zeroing is good/bad if winning/losing
                    Reverse(m.dtz.ignore_rounding()),
                )
            })
            .map(|m| (m.m, m.dtz)))
    }

    async fn probe<'a>(&'a self, pos: &'a S) -> SyzygyResult<WdlEntry<'a, S, F>> {
        // Probing resolves captures, so sometimes we can obtain results
        // for positions that have more pieces than the maximum amount of
        // supported pieces. We artificially limit this to one additional
        // level, to make sure search remains somewhat bounded.
        if pos.board().occupied().count() > S::MAX_PIECES + 1 {
            return Err(SyzygyError::TooManyPieces);
        }

        if pos.castles().any() {
            return Err(SyzygyError::Castling);
        }

        // Determine the WDL value of this position. This is also a
        // prerequisite for probing DTZ tables. There are two complications:
        //
        // (1) Resolving en passant captures.
        //
        // (2) When a position has a capture that achieves a particular result
        //     (e.g., there is a winning capture), then the position itself
        //     should have at least that value (e.g., it is winning). In this
        //     case the table can store an arbitrary lower value, whichever is
        //     best for compression.
        //
        //     If the best move is zeroing, then we need remember this to avoid
        //     probing the DTZ tables.

        if S::CAPTURES_COMPULSORY {
            // Tables for antichess variants take advantage of the rule that
            // captures are compulsory. Forced captures are resolved in a brief
            // alpha-beta search. Additionally 6-piece tables need a 1-ply
            // search to find threat moves that will force a losing capture.
            //
            // Here we search for threat moves unconditionally. Strictly
            // speaking this is not required when there are less than 6 pieces,
            // but we need to know if there are threat moves when continuing
            // with a DTZ probe.
            let (v, state) = self
                .probe_compulsory_captures(pos, Wdl::Loss, Wdl::Win, true)
                .await?;
            return Ok(WdlEntry {
                tablebase: self,
                pos,
                wdl: v,
                state,
            });
        } else if let Some(outcome) = pos.variant_outcome().known() {
            // Handle game-end postions of chess variants.
            return Ok(WdlEntry {
                tablebase: self,
                pos,
                wdl: Wdl::from_outcome(outcome, pos.turn()),
                state: ProbeState::ZeroingBestMove,
            });
        }

        // Resolve captures: Find the best non-ep capture and the best
        // en passant capture.
        let mut best_capture = Wdl::Loss;
        let mut best_ep = Wdl::Loss;

        let legals = pos.legal_moves();

        for m in legals.iter().copied().filter(|m| m.is_capture()) {
            let mut after = pos.clone();
            after.play_unchecked(m);
            let v = -self
                .probe_ab_no_ep(&after, Wdl::Loss, -best_capture)
                .await?;

            if v == Wdl::Win {
                return Ok(WdlEntry {
                    tablebase: self,
                    pos,
                    wdl: v,
                    state: ProbeState::ZeroingBestMove,
                });
            }

            if m.is_en_passant() {
                best_ep = max(best_ep, v);
            } else {
                best_capture = max(best_capture, v);
            }
        }

        // Probe table.
        let v = self.probe_wdl_table(pos).await?;

        // Now max(v, best_capture) is the WDL value of the position without
        // ep rights. Detect the case were an ep move is stricly better
        // (including blessed losing positions).
        if best_ep > max(v, best_capture) {
            return Ok(WdlEntry {
                tablebase: self,
                pos,
                wdl: best_ep,
                state: ProbeState::ZeroingBestMove,
            });
        }

        best_capture = max(best_capture, best_ep);

        // Now max(v, best_capture) is the WDL value of the position,
        // unless the position without ep rights is stalemate (and there are
        // ep moves).
        if best_capture >= v {
            return Ok(WdlEntry {
                tablebase: self,
                pos,
                wdl: best_capture,
                state: if best_capture > Wdl::Draw {
                    ProbeState::ZeroingBestMove
                } else {
                    ProbeState::Normal
                },
            });
        }

        // If the position would be stalemate without ep captures, then we are
        // forced to play the best en passant move.
        if v == Wdl::Draw && !legals.is_empty() && legals.iter().all(|m| m.is_en_passant()) {
            return Ok(WdlEntry {
                tablebase: self,
                pos,
                wdl: best_ep,
                state: ProbeState::ZeroingBestMove,
            });
        }

        Ok(WdlEntry {
            tablebase: self,
            pos,
            wdl: v,
            state: ProbeState::Normal,
        })
    }

    async fn probe_ab_no_ep(&self, pos: &S, mut alpha: Wdl, beta: Wdl) -> SyzygyResult<Wdl> {
        // Use alpha-beta to recursively resolve captures. This is only called
        // for positions without ep rights.
        assert!(pos.maybe_ep_square().is_none());

        for m in pos.capture_moves() {
            let mut after = pos.clone();
            after.play_unchecked(m);
            let v = -Box::pin(self.probe_ab_no_ep(&after, -beta, -alpha)).await?;
            if v >= beta {
                return Ok(v);
            }
            alpha = max(alpha, v);
        }

        let v = self.probe_wdl_table(pos).await?;
        Ok(max(alpha, v))
    }

    async fn probe_compulsory_captures(
        &self,
        pos: &S,
        mut alpha: Wdl,
        beta: Wdl,
        threats: bool,
    ) -> SyzygyResult<(Wdl, ProbeState)> {
        assert!(S::CAPTURES_COMPULSORY);

        if let Some(outcome) = pos.variant_outcome().known() {
            return Ok((
                Wdl::from_outcome(outcome, pos.turn()),
                ProbeState::ZeroingBestMove,
            ));
        }

        // Explore compulsory captures in antichess variants.
        if pos.them().count() > 1 {
            if let Some(v) = self.probe_captures(pos, alpha, beta).await? {
                return Ok((v, ProbeState::ZeroingBestMove));
            }
        } else {
            // The opponent only has one piece left. If we need to capture it
            // this immediately ends the game.
            if !pos.capture_moves().is_empty() {
                return Ok((Wdl::Loss, ProbeState::ZeroingBestMove));
            }
        }

        let mut threats_found = false;

        // For 6 piece endgames (or if indicated by the threats flag) also
        // explore threat moves that will force a capture on following move.
        if threats || pos.board().occupied().count() >= 6 {
            for threat in pos.legal_moves() {
                if threat.role() != Role::Pawn {
                    let mut after = pos.clone();
                    after.play_unchecked(threat);

                    if let Some(v_plus) = self.probe_captures(&after, -beta, -alpha).await? {
                        let v = -v_plus;
                        if v > alpha {
                            threats_found = true;
                            alpha = v;
                            if alpha >= beta {
                                return Ok((v, ProbeState::Threat));
                            }
                        }
                    }
                }
            }
        }

        let v = self.probe_wdl_table(pos).await?;
        if v > alpha {
            Ok((v, ProbeState::Normal))
        } else {
            Ok((
                alpha,
                if threats_found {
                    ProbeState::Threat
                } else {
                    ProbeState::Normal
                },
            ))
        }
    }

    async fn probe_captures(
        &self,
        pos: &S,
        mut alpha: Wdl,
        beta: Wdl,
    ) -> SyzygyResult<Option<Wdl>> {
        assert!(S::CAPTURES_COMPULSORY);

        // Explore capture moves in antichess variants. If captures exists they
        // are also the only moves, because captures are compulsory.
        let captures = pos.capture_moves();

        for m in pos.capture_moves() {
            let mut after = pos.clone();
            after.play_unchecked(m);

            let (v_plus, _) =
                Box::pin(self.probe_compulsory_captures(&after, -beta, -alpha, false)).await?;
            let v = -v_plus;

            alpha = max(v, alpha);
            if alpha >= beta {
                break;
            }
        }

        Ok(if captures.is_empty() {
            None
        } else {
            Some(alpha)
        })
    }

    async fn probe_wdl_table(&self, pos: &S) -> SyzygyResult<Wdl> {
        // Variant game end.
        if let Some(outcome) = pos.variant_outcome().known() {
            return Ok(Wdl::from_outcome(outcome, pos.turn()));
        }

        // Test for KvK.
        if S::ONE_KING && pos.board().kings() == pos.board().occupied() {
            return Ok(Wdl::Draw);
        }

        // More pieces than any opened table.
        if pos.board().occupied().count() > self.max_pieces {
            return Err(SyzygyError::TooManyPieces);
        }

        // Get raw WDL value from the appropriate table.
        let material = Material::from_board(pos.board()).to_normalized();
        let table = self.wdl_table(&material).await?;
        table.probe_wdl(pos).await.ctx(Metric::Wdl, &material)
    }

    async fn probe_dtz_table(
        &self,
        pos: &S,
        wdl: DecisiveWdl,
    ) -> SyzygyResult<Option<MaybeRounded<u32>>> {
        // Get raw DTZ value from the appropriate table.
        let material = Material::from_board(pos.board()).to_normalized();
        let table = self.dtz_table(&material).await?;
        table.probe_dtz(pos, wdl).await.ctx(Metric::Dtz, &material)
    }
}

/// WDL entry. Prerequisite for probing DTZ tables.
struct WdlEntry<'a, S, F: Filesystem> {
    tablebase: &'a Tablebase<S, F>,
    pos: &'a S,
    wdl: Wdl,
    state: ProbeState,
}

impl<'a, S: Position + Clone + Syzygy + 'a, F: Filesystem> WdlEntry<'a, S, F> {
    fn wdl_after_zeroing(&self) -> Wdl {
        self.wdl
    }

    async fn dtz(&self) -> SyzygyResult<MaybeRounded<Dtz>> {
        let Some(wdl) = self.wdl.decisive() else {
            return Ok(MaybeRounded::Precise(Dtz(0)));
        };

        if self.state == ProbeState::ZeroingBestMove || self.pos.us() == self.pos.our(Role::Pawn) {
            return Ok(MaybeRounded::Precise(Dtz::before_zeroing(wdl.into())));
        }

        if self.state == ProbeState::Threat && wdl >= DecisiveWdl::CursedWin {
            // The position is a win or a cursed win by a threat move.
            return Ok(MaybeRounded::Precise(
                Dtz::before_zeroing(wdl.into()).add_plies(1),
            ));
        }

        // If winning, check for a winning pawn move. No need to look at
        // captures again, they were already handled above.
        if wdl >= DecisiveWdl::CursedWin {
            let mut pawn_advances = self.pos.legal_moves();
            pawn_advances.retain(|m| !m.is_capture() && m.role() == Role::Pawn);

            for m in pawn_advances {
                let mut after = self.pos.clone();
                after.play_unchecked(m);
                let v = -self.tablebase.probe_wdl_after_zeroing(&after).await?;
                if v == wdl.into() {
                    return Ok(MaybeRounded::Precise(Dtz::before_zeroing(wdl.into())));
                }
            }
        }

        // At this point we know that the best move is not a capture. Probe the
        // table. DTZ tables store only one side to move.
        if let Some(plies) = self.tablebase.probe_dtz_table(self.pos, wdl).await? {
            return Ok(plies.map(|plies| Dtz::before_zeroing(wdl.into()).add_plies(plies)));
        }

        // We have to probe the other side of the table by doing
        // a 1-ply search.
        let mut moves = self.pos.legal_moves();
        moves.retain(|m| !m.is_zeroing());

        let mut best = if wdl >= DecisiveWdl::CursedWin {
            None
        } else {
            Some(MaybeRounded::Precise(Dtz::before_zeroing(wdl.into())))
        };

        for m in moves {
            let mut after = self.pos.clone();
            after.play_unchecked(m);
            let v = -Box::pin(self.tablebase.probe_dtz(&after)).await?;
            if v.ignore_rounding() == Dtz(1) && after.is_checkmate() {
                best = Some(MaybeRounded::Precise(Dtz(1)));
            } else if v.signum() == wdl.signum() {
                let v = v.map(|v| v.add_plies(1));
                best = match best {
                    None => Some(v),
                    Some(best) if v.ignore_rounding() < best.ignore_rounding() => Some(v),
                    _ => best,
                };
            }
        }

        (|| Ok(u!(best)))().ctx(
            Metric::Dtz,
            &Material::from_board(self.pos.board()).to_normalized(),
        )
    }
}
