use std::{
    cmp::{max, Reverse},
    fs, io,
    path::{Path, PathBuf},
};

use arrayvec::ArrayVec;
use once_cell::sync::OnceCell;
use positioned_io::RandomAccessFile;
use rustc_hash::FxHashMap;
use shakmaty::{Move, Position, Role};

use crate::{
    errors::{ProbeResultExt as _, SyzygyError, SyzygyResult},
    material::Material,
    table::{DtzTable, WdlTable},
    types::{DecisiveWdl, Dtz, MaybeRounded, Metric, Syzygy, Wdl},
    AmbiguousWdl,
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

/// A collection of tables.
#[derive(Debug)]
pub struct Tablebase<S: Position + Clone + Syzygy> {
    wdl: FxHashMap<Material, (PathBuf, OnceCell<WdlTable<S, RandomAccessFile>>)>,
    dtz: FxHashMap<Material, (PathBuf, OnceCell<DtzTable<S, RandomAccessFile>>)>,
    max_pieces: usize,
}

impl<S: Position + Clone + Syzygy> Default for Tablebase<S> {
    fn default() -> Tablebase<S> {
        Tablebase::new()
    }
}

impl<S: Position + Clone + Syzygy> Tablebase<S> {
    /// Create an empty collection of tables.
    pub fn new() -> Tablebase<S> {
        Tablebase {
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

    /// Add all relevant tables from a directory.
    ///
    /// Tables are selected by filename, e.g. `KQvKP.rtbz`. The files are not
    /// actually opened. This happens lazily when probing.
    ///
    /// Note that probing generally requires tables for the specific material
    /// composition, as well as material compositions that are transitively
    /// reachable by captures and promotions. These are sometimes distributed
    /// separately, so make sure to add tables from all relevant directories.
    ///
    /// Returns the number of added table files.
    ///
    /// # Errors
    ///
    /// Returns an error result when:
    ///
    /// * The `path` does not exist.
    /// * `path` is not a directory.
    /// * The process lacks permissions to list the directory.
    pub fn add_directory<P: AsRef<Path>>(&mut self, path: P) -> io::Result<usize> {
        let mut num = 0;

        for entry in fs::read_dir(path)? {
            if self.add_file(entry?.path()).is_ok() {
                num += 1;
            }
        }

        Ok(num)
    }

    /// Add a table file.
    ///
    /// The file is not actually opened. This happens lazily when probing.
    ///
    /// # Errors
    ///
    /// Returns an error when no file exists at the given path or the
    /// filename does not indicate that it is a valid table file
    /// (e.g. `KQvKP.rtbz`).
    pub fn add_file<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
        let path = path.as_ref();

        if !path.is_file() {
            return Err(io::Error::from(io::ErrorKind::InvalidInput));
        }

        let (stem, ext) = path
            .file_stem()
            .and_then(|s| s.to_str())
            .zip(path.extension())
            .ok_or_else(|| io::Error::from(io::ErrorKind::InvalidInput))?;

        let material =
            Material::from_str(stem).map_err(|_| io::Error::from(io::ErrorKind::InvalidInput))?;

        let pieces = material.count();

        if pieces > S::MAX_PIECES {
            return Err(io::Error::from(io::ErrorKind::InvalidInput));
        }

        if material.by_color.white.count() < 1 || material.by_color.black.count() < 1 {
            return Err(io::Error::from(io::ErrorKind::InvalidInput));
        }

        if ext == S::TBW.ext
            || (!material.has_pawns() && S::PAWNLESS_TBW.map_or(false, |t| ext == t.ext))
        {
            self.wdl
                .insert(material, (path.to_path_buf(), OnceCell::new()));
        } else if ext == S::TBZ.ext
            || (!material.has_pawns() && S::PAWNLESS_TBZ.map_or(false, |t| ext == t.ext))
        {
            self.dtz
                .insert(material, (path.to_path_buf(), OnceCell::new()));
        } else {
            return Err(io::Error::from(io::ErrorKind::InvalidInput));
        }

        self.max_pieces = max(self.max_pieces, pieces);
        Ok(())
    }

    fn wdl_table(&self, key: &Material) -> SyzygyResult<&WdlTable<S, RandomAccessFile>> {
        if let Some((path, table)) = self
            .wdl
            .get(key)
            .or_else(|| self.wdl.get(&key.clone().into_flipped()))
        {
            table
                .get_or_try_init(|| WdlTable::open(path, key))
                .ctx(Metric::Wdl, key.to_owned())
        } else {
            Err(SyzygyError::MissingTable {
                metric: Metric::Wdl,
                material: key.clone().into_normalized(),
            })
        }
    }

    fn dtz_table(&self, key: &Material) -> SyzygyResult<&DtzTable<S, RandomAccessFile>> {
        if let Some((path, table)) = self
            .dtz
            .get(key)
            .or_else(|| self.dtz.get(&key.clone().into_flipped()))
        {
            table
                .get_or_try_init(|| DtzTable::open(path, key))
                .ctx(Metric::Dtz, key.to_owned())
        } else {
            Err(SyzygyError::MissingTable {
                metric: Metric::Dtz,
                material: key.clone().into_normalized(),
            })
        }
    }

    /// Probe tables for the [`Wdl`] value of a position, assuming `pos`
    /// is reached directly after a capture or pawn move.
    ///
    /// # Errors
    ///
    /// See [`SyzygyError`] for possible error conditions.
    pub fn probe_wdl_after_zeroing(&self, pos: &S) -> SyzygyResult<Wdl> {
        self.probe(pos).map(|entry| entry.wdl_after_zeroing())
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
    pub fn probe_wdl(&self, pos: &S) -> SyzygyResult<AmbiguousWdl> {
        self.probe(pos)
            .and_then(|entry| entry.dtz())
            .map(|dtz| AmbiguousWdl::from_dtz_and_halfmoves(dtz, pos.halfmoves()))
    }

    /// Probe tables for the [`Dtz`] value of a position.
    ///
    /// Requires both WDL and DTZ tables.
    ///
    /// # Errors
    ///
    /// See [`SyzygyError`] for possible error conditions.
    pub fn probe_dtz(&self, pos: &S) -> SyzygyResult<MaybeRounded<Dtz>> {
        self.probe(pos).and_then(|entry| entry.dtz())
    }

    /// Get the recommended tablebase move.
    ///
    /// Following the tablebase mainline *starting from a capture or pawn move*
    /// guarantees achieving the optimal outcome under the 50-move rule.
    ///
    /// Otherwise (i.e. when not immediately following the tablebase mainline
    /// after the capture that crosses into tablebase territory),
    /// [some care needs to be taken due to DTZ rounding](MaybeRounded).
    ///
    /// Requires both WDL and DTZ tables.
    ///
    /// # Errors
    ///
    /// See [`SyzygyError`] for possible error conditions.
    pub fn best_move(&self, pos: &S) -> SyzygyResult<Option<(Move, MaybeRounded<Dtz>)>> {
        struct WithAfter<S> {
            m: Move,
            after: S,
        }

        struct WithWdlEntry<'a, S: Position + Clone + Syzygy> {
            m: Move,
            entry: WdlEntry<'a, S>,
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
                after.play_unchecked(&m);
                WithAfter { m, after }
            })
            .collect::<ArrayVec<_, 256>>();

        // Determine WDL for each move.
        let with_wdl = with_after
            .iter()
            .map(|e| {
                Ok(WithWdlEntry {
                    m: e.m.clone(),
                    entry: self.probe(&e.after)?,
                })
            })
            .collect::<SyzygyResult<ArrayVec<_, 256>>>()?;

        // Find best WDL.
        let best_wdl = with_wdl
            .iter()
            .map(|a| a.entry.wdl)
            .min()
            .unwrap_or(Wdl::Loss);

        // Select a DTZ-optimal move among the moves with best WDL.
        itertools::process_results(
            with_wdl
                .iter()
                .filter(|a| a.entry.wdl == best_wdl)
                .map(|a| {
                    let dtz = a.entry.dtz()?;
                    Ok(WithDtz {
                        immediate_loss: dtz.ignore_rounding() == Dtz(-1)
                            && (a.entry.pos.is_checkmate()
                                || a.entry.pos.variant_outcome().is_some()),
                        zeroing: a.m.is_zeroing(),
                        m: a.m.clone(),
                        dtz,
                    })
                }),
            |iter| {
                iter.min_by_key(|m| {
                    (
                        Reverse(m.immediate_loss),
                        m.zeroing ^ m.dtz.is_negative(), // zeroing is good/bad if winning/losing
                        Reverse(m.dtz.ignore_rounding()),
                    )
                })
                .map(|m| (m.m, m.dtz))
            },
        )
    }

    fn probe<'a>(&'a self, pos: &'a S) -> SyzygyResult<WdlEntry<'a, S>> {
        if pos.board().occupied().count() > S::MAX_PIECES {
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
        //     (e.g. there is a winning capture), then the position itself
        //     should have at least that value (e.g. it is winning). In this
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
            let (v, state) = self.probe_compulsory_captures(pos, Wdl::Loss, Wdl::Win, true)?;
            return Ok(WdlEntry {
                tablebase: self,
                pos,
                wdl: v,
                state,
            });
        } else if let Some(outcome) = pos.variant_outcome() {
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

        for m in legals.iter().filter(|m| m.is_capture()) {
            let mut after = pos.clone();
            after.play_unchecked(m);
            let v = -self.probe_ab_no_ep(&after, Wdl::Loss, -best_capture)?;

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
        let v = self.probe_wdl_table(pos)?;

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

    fn probe_ab_no_ep(&self, pos: &S, mut alpha: Wdl, beta: Wdl) -> SyzygyResult<Wdl> {
        // Use alpha-beta to recursively resolve captures. This is only called
        // for positions without ep rights.
        assert!(pos.maybe_ep_square().is_none());

        for m in pos.capture_moves() {
            let mut after = pos.clone();
            after.play_unchecked(&m);
            let v = -self.probe_ab_no_ep(&after, -beta, -alpha)?;
            if v >= beta {
                return Ok(v);
            }
            alpha = max(alpha, v);
        }

        let v = self.probe_wdl_table(pos)?;
        Ok(max(alpha, v))
    }

    fn probe_compulsory_captures(
        &self,
        pos: &S,
        mut alpha: Wdl,
        beta: Wdl,
        threats: bool,
    ) -> SyzygyResult<(Wdl, ProbeState)> {
        assert!(S::CAPTURES_COMPULSORY);

        if let Some(outcome) = pos.variant_outcome() {
            return Ok((
                Wdl::from_outcome(outcome, pos.turn()),
                ProbeState::ZeroingBestMove,
            ));
        }

        // Explore compulsory captures in antichess variants.
        if pos.them().count() > 1 {
            if let Some(v) = self.probe_captures(pos, alpha, beta)? {
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
                    after.play_unchecked(&threat);

                    if let Some(v_plus) = self.probe_captures(&after, -beta, -alpha)? {
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

        let v = self.probe_wdl_table(pos)?;
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

    fn probe_captures(&self, pos: &S, mut alpha: Wdl, beta: Wdl) -> SyzygyResult<Option<Wdl>> {
        assert!(S::CAPTURES_COMPULSORY);

        // Explore capture moves in antichess variants. If captures exists they
        // are also the only moves, because captures are compulsory.
        let captures = pos.capture_moves();

        for m in pos.capture_moves() {
            let mut after = pos.clone();
            after.play_unchecked(&m);

            let (v_plus, _) = self.probe_compulsory_captures(&after, -beta, -alpha, false)?;
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

    fn probe_wdl_table(&self, pos: &S) -> SyzygyResult<Wdl> {
        // Variant game end.
        if let Some(outcome) = pos.variant_outcome() {
            return Ok(Wdl::from_outcome(outcome, pos.turn()));
        }

        // Test for KvK.
        if S::ONE_KING && pos.board().kings() == pos.board().occupied() {
            return Ok(Wdl::Draw);
        }

        // Get raw WDL value from the appropriate table.
        let key = Material::from_board(pos.board());
        self.wdl_table(&key)
            .and_then(|table| table.probe_wdl(pos).ctx(Metric::Wdl, key))
    }

    fn probe_dtz_table(
        &self,
        pos: &S,
        wdl: DecisiveWdl,
    ) -> SyzygyResult<Option<MaybeRounded<u32>>> {
        // Get raw DTZ value from the appropriate table.
        let key = Material::from_board(pos.board());
        self.dtz_table(&key)
            .and_then(|table| table.probe_dtz(pos, wdl).ctx(Metric::Dtz, key))
    }
}

/// WDL entry. Prerequisite for probing DTZ tables.
#[derive(Debug)]
struct WdlEntry<'a, S: Position + Clone + Syzygy> {
    tablebase: &'a Tablebase<S>,
    pos: &'a S,
    wdl: Wdl,
    state: ProbeState,
}

impl<'a, S: Position + Clone + Syzygy + 'a> WdlEntry<'a, S> {
    fn wdl_after_zeroing(&self) -> Wdl {
        self.wdl
    }

    fn dtz(&self) -> SyzygyResult<MaybeRounded<Dtz>> {
        let wdl = match self.wdl.decisive() {
            Some(wdl) => wdl,
            None => return Ok(MaybeRounded::Precise(Dtz(0))),
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

            for m in &pawn_advances {
                let mut after = self.pos.clone();
                after.play_unchecked(m);
                let v = -self.tablebase.probe_wdl_after_zeroing(&after)?;
                if v == wdl.into() {
                    return Ok(MaybeRounded::Precise(Dtz::before_zeroing(wdl.into())));
                }
            }
        }

        // At this point we know that the best move is not a capture. Probe the
        // table. DTZ tables store only one side to move.
        if let Some(plies) = self.tablebase.probe_dtz_table(self.pos, wdl)? {
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

        for m in &moves {
            let mut after = self.pos.clone();
            after.play_unchecked(m);
            let v = -self.tablebase.probe_dtz(&after)?;
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

        (|| Ok(u!(best)))().ctx(Metric::Dtz, Material::from_board(self.pos.board()))
    }
}

#[cfg(test)]
mod tests {
    use shakmaty::{fen::Fen, CastlingMode, Chess, Square};

    use super::*;

    #[test]
    fn test_send_sync() {
        fn assert_send<T: Send>(_: T) {}
        fn assert_sync<T: Sync>(_: T) {}

        assert_send(Tablebase::<Chess>::new());
        assert_sync(Tablebase::<Chess>::new());
    }

    #[test]
    fn test_mating_best_move() {
        let mut tables = Tablebase::new();
        tables
            .add_directory("tables/chess")
            .expect("read directory");

        let pos: Chess = "5BrN/8/8/8/8/2k5/8/2K5 b - -"
            .parse::<Fen>()
            .expect("valid fen")
            .into_position(CastlingMode::Chess960)
            .expect("legal position");

        assert!(matches!(
            tables.best_move(&pos),
            Ok(Some((
                Move::Normal {
                    role: Role::Rook,
                    from: Square::G8,
                    capture: None,
                    to: Square::G1,
                    promotion: None,
                },
                MaybeRounded::Rounded(Dtz(-1))
            )))
        ));
    }

    #[test]
    fn test_black_escapes_via_underpromotion() {
        let mut tables = Tablebase::new();
        tables
            .add_directory("tables/chess")
            .expect("read directory");

        let pos: Chess = "8/6B1/8/8/B7/8/K1pk4/8 b - - 0 1"
            .parse::<Fen>()
            .expect("valid fen")
            .into_position(CastlingMode::Chess960)
            .expect("legal position");

        assert!(matches!(
            tables.best_move(&pos),
            Ok(Some((
                Move::Normal {
                    role: Role::Pawn,
                    from: Square::C2,
                    to: Square::C1,
                    capture: None,
                    promotion: Some(Role::Knight),
                },
                MaybeRounded::Rounded(Dtz(109))
            )))
        ));
    }

    #[test]
    #[ignore]
    fn test_many_pawns() {
        let mut tables = Tablebase::new();
        tables
            .add_directory("tables/chess")
            .expect("read directory");

        let pos: Chess = "3k4/5P2/8/8/4K3/2P3P1/PP6/8 w - - 0 1"
            .parse::<Fen>()
            .expect("valid fen")
            .into_position(CastlingMode::Chess960)
            .expect("legal position");

        assert!(matches!(
            tables.probe_dtz(&pos),
            Ok(MaybeRounded::Precise(Dtz(1)))
        ));
    }
}
