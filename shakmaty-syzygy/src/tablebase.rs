// This file is part of the shakmaty-syzygy library.
// Copyright (C) 2017-2018 Niklas Fiekas <niklas.fiekas@backscattering.de>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.

use std::cmp::{max, Reverse};
use std::fs;
use std::fs::File;
use std::io;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use arrayvec::ArrayVec;
use double_checked_cell::DoubleCheckedCell;
use hashbrown;
use itertools;

use shakmaty::{Material, Move, MoveList, Position, Role};

use errors::{ProbeError, ProbeResultExt, SyzygyError, SyzygyResult};
use table::{DtzTable, WdlTable};
use types::{DecisiveWdl, Dtz, Metric, Syzygy, Wdl};

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
    wdl: hashbrown::HashMap<Material, (PathBuf, DoubleCheckedCell<WdlTable<S, File>>)>,
    dtz: hashbrown::HashMap<Material, (PathBuf, DoubleCheckedCell<DtzTable<S, File>>)>,
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
            wdl: hashbrown::HashMap::with_capacity(145),
            dtz: hashbrown::HashMap::with_capacity(145),
        }
    }

    /// Add all relevant tables from a directory.
    ///
    /// Tables are selected by filename, e.g. `KQvKP.rtbz`. The files are not
    /// actually opened. This happens lazily when probing.
    ///
    /// # Errors
    ///
    /// Returns an error result when:
    ///
    /// * The `path` does not exist.
    /// * `path` is not a directory.
    /// * The process lacks permissions to list the directory.
    pub fn add_directory<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                continue;
            }

            let (stem, ext) = match (path.file_stem().and_then(|s| s.to_str()), path.extension()) {
                (Some(stem), Some(ext)) => (stem, ext),
                _ => continue,
            };

            let material = match Material::from_str(stem) {
                Ok(material) => material,
                _ => continue,
            };

            if material.count() > S::MAX_PIECES {
                continue;
            }

            if material.white.count() < 1 || material.black.count() < 1 {
                continue;
            }

            if ext == S::TBW.ext || (!material.has_pawns() && S::PAWNLESS_TBW.map_or(false, |t| ext == t.ext)) {
                self.wdl.insert(material, (path.clone(), DoubleCheckedCell::new()));
            } else if ext == S::TBZ.ext || (!material.has_pawns() && S::PAWNLESS_TBZ.map_or(false, |t| ext == t.ext)) {
                self.dtz.insert(material, (path.clone(), DoubleCheckedCell::new()));
            }
        }

        Ok(())
    }

    /// Probe tables for the [`Wdl`](enum.Wdl.html) value of a position.
    ///
    /// This indicates if the position is winning, lost or drawn with
    /// or without the 50-move rule.
    ///
    /// # Errors
    ///
    /// See [`SyzygyError`](enum.SyzygyError.html) for possible error
    /// conditions.
    pub fn probe_wdl(&self, pos: &S) -> SyzygyResult<Wdl> {
        self.probe(pos).map(|entry| entry.wdl())
    }

    /// Probe tables for the [`Dtz`](struct.Dtz.html) value of a position.
    ///
    /// Min-maxing the DTZ of the available moves guarantees achieving the
    /// optimal outcome under the 50-move rule.
    ///
    /// # Errors
    ///
    /// See [`SyzygyError`](enum.SyzygyError.html) for possible error
    /// conditions.
    pub fn probe_dtz(&self, pos: &S) -> SyzygyResult<Dtz> {
        self.probe(pos).and_then(|entry| entry.dtz())
    }

    /// Select a DTZ-optimal move.
    ///
    /// # Errors
    ///
    /// See [`SyzygyError`](enum.SyzygyError.html) for possible error
    /// conditions.
    pub fn best_move(&self, pos: &S) -> SyzygyResult<Option<(Move, Dtz)>> {
        struct WithAfter<S> {
            m: Move,
            after: S,
        }

        struct WithWdlEntry<'a, S: Position + Clone + Syzygy + 'a> {
            m: Move,
            entry: WdlEntry<'a, S>,
        }

        struct WithDtz {
            m: Move,
            immediate_loss: bool,
            zeroing: bool,
            dtz: Dtz,
        }

        // Build list of successor positions.
        let mut legals = MoveList::new();
        pos.legal_moves(&mut legals);
        let with_after = legals.into_iter().map(|m| {
            let mut after = pos.clone();
            after.play_unchecked(&m);
            WithAfter { m, after }
        }).collect::<ArrayVec<[_; 256]>>();

        // Determine WDL for each move.
        let with_wdl = with_after.iter().map(|e| Ok(WithWdlEntry {
            m: e.m.clone(),
            entry: self.probe(&e.after)?,
        })).collect::<SyzygyResult<ArrayVec<[_; 256]>>>()?;

        // Find best WDL.
        let best_wdl = with_wdl.iter().map(|a| a.entry.wdl).min().unwrap_or(Wdl::Loss);

        // Select a DTZ-optimal move among the moves with best WDL.
        itertools::process_results(with_wdl.iter().filter(|a| a.entry.wdl == best_wdl).map(|a| {
            let dtz = a.entry.dtz()?;
            Ok(WithDtz {
                immediate_loss: dtz == Dtz(-1) && (a.entry.pos.is_checkmate() || a.entry.pos.variant_outcome().is_some()),
                zeroing: a.m.is_zeroing(),
                m: a.m.clone(),
                dtz,
            })
        }), |iter| iter.min_by_key(|m| (
            Reverse(m.immediate_loss),
            m.zeroing ^ (m.dtz < Dtz(0)), // zeroing is good/bad if winning/losing
            Reverse(m.dtz),
        )).map(|m| (m.m, m.dtz)))
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

        let mut legals = MoveList::new();
        pos.legal_moves(&mut legals);

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
                state: if best_capture > Wdl::Draw { ProbeState::ZeroingBestMove } else { ProbeState::Normal },
            })
        }

        // If the position would be stalemate without ep captures, then we are
        // forced to play the best en passant move.
        if v == Wdl::Draw && !legals.is_empty() && legals.iter().all(|m| m.is_en_passant()) {
            return Ok(WdlEntry {
                tablebase: self,
                pos,
                wdl: best_ep,
                state: ProbeState::ZeroingBestMove,
            })
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
        assert!(pos.ep_square().is_none());

        let mut captures = MoveList::new();
        pos.capture_moves(&mut captures);

        for m in captures {
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

    fn probe_compulsory_captures(&self, pos: &S, mut alpha: Wdl, beta: Wdl, threats: bool) -> SyzygyResult<(Wdl, ProbeState)> {
        assert!(S::CAPTURES_COMPULSORY);

        if let Some(outcome) = pos.variant_outcome() {
            return Ok((Wdl::from_outcome(outcome, pos.turn()), ProbeState::ZeroingBestMove));
        }

        // Explore compulsory captures in antichess variants.
        if pos.them().count() > 1 {
            if let Some(v) = self.probe_captures(pos, alpha, beta)? {
                return Ok((v, ProbeState::ZeroingBestMove));
            }
        } else {
            // The opponent only has one piece left. If we need to capture it
            // this immediately ends the game.
            let mut captures = MoveList::new();
            pos.capture_moves(&mut captures);
            if !captures.is_empty() {
                return Ok((Wdl::Loss, ProbeState::ZeroingBestMove));
            }
        }

        let mut threats_found = false;

        // For 6 piece endgames (or if indicated by the threats flag) also
        // explore threat moves that will force a capture on following move.
        if threats || pos.board().occupied().count() >= 6 {
            let mut moves = MoveList::new();
            pos.legal_moves(&mut moves);
            for threat in moves {
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
            Ok((alpha, if threats_found { ProbeState::Threat } else { ProbeState::Normal }))
        }
    }

    fn probe_captures(&self, pos: &S, mut alpha: Wdl, beta: Wdl) -> SyzygyResult<Option<Wdl>> {
        assert!(S::CAPTURES_COMPULSORY);

        // Explore capture moves in antichess variants. If captures exists they
        // are also the only moves, because captures are compulsory.
        let mut captures = MoveList::new();
        pos.capture_moves(&mut captures);

        for m in &captures {
            let mut after = pos.clone();
            after.play_unchecked(&m);

            let (v_plus, _) = self.probe_compulsory_captures(&after, -beta, -alpha, false)?;
            let v = -v_plus;

            alpha = max(v, alpha);
            if alpha >= beta {
                break;
            }
        }

        Ok(if captures.is_empty() { None } else { Some(alpha) })
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
        let key = pos.board().material();
        if let Some(&(ref path, ref table)) = self.wdl.get(&key).or_else(|| self.wdl.get(&key.flipped())) {
            let table = table.get_or_try_init(|| WdlTable::open(path, &key)).ctx(Metric::Wdl, &key)?;
            table.probe_wdl(pos).ctx(Metric::Wdl, &key)
        } else {
            Err(SyzygyError::MissingTable {
                metric: Metric::Wdl,
                material: key.normalized(),
            })
        }
    }

    fn probe_dtz_table(&self, pos: &S, wdl: DecisiveWdl) -> SyzygyResult<Option<Dtz>> {
        // Get raw DTZ value from the appropriate table.
        let key = pos.board().material();
        if let Some(&(ref path, ref table)) = self.dtz.get(&key).or_else(|| self.dtz.get(&key.flipped())) {
            let table = table.get_or_try_init(|| DtzTable::open(path, &key)).ctx(Metric::Dtz, &key)?;
            table.probe_dtz(pos, wdl).ctx(Metric::Dtz, &key)
        } else {
            Err(SyzygyError::MissingTable {
                metric: Metric::Dtz,
                material: key.normalized(),
            })
        }
    }
}

/// WDL entry. Prerequisite for probing DTZ tables.
#[derive(Debug)]
struct WdlEntry<'a, S: Position + Clone + Syzygy + 'a> {
    tablebase: &'a Tablebase<S>,
    pos: &'a S,
    wdl: Wdl,
    state: ProbeState,
}

impl<'a, S: Position + Clone + Syzygy + 'a> WdlEntry<'a, S> {
    fn wdl(&self) -> Wdl {
        self.wdl
    }

    fn dtz(&self) -> SyzygyResult<Dtz> {
        let wdl = match self.wdl.decisive() {
            Some(wdl) => wdl,
            None => return Ok(Dtz(0)),
        };

        if self.state == ProbeState::ZeroingBestMove || self.pos.us() == self.pos.our(Role::Pawn) {
            return Ok(Dtz::before_zeroing(wdl));
        }

        if self.state == ProbeState::Threat && wdl >= DecisiveWdl::CursedWin {
            // The position is a win or a cursed win by a threat move.
            return Ok(Dtz::before_zeroing(wdl).add_plies(1));
        }

        // If winning, check for a winning pawn move. No need to look at
        // captures again, they were already handled above.
        if wdl >= DecisiveWdl::CursedWin {
            let mut pawn_advances = MoveList::new();
            self.pos.legal_moves(&mut pawn_advances);
            pawn_advances.retain(|m| !m.is_capture() && m.role() == Role::Pawn);

            for m in &pawn_advances {
                let mut after = self.pos.clone();
                after.play_unchecked(m);
                let v = -self.tablebase.probe_wdl(&after)?;
                if v == wdl.into() {
                    return Ok(Dtz::before_zeroing(wdl));
                }
            }
        }

        // At this point we know that the best move is not a capture. Probe the
        // table. DTZ tables store only one side to move.
        if let Some(Dtz(dtz)) = self.tablebase.probe_dtz_table(&self.pos, wdl)? {
            return Ok(Dtz::before_zeroing(wdl).add_plies(dtz));
        }

        // We have to probe the other side by doing a 1-ply search.
        let mut moves = MoveList::new();
        self.pos.legal_moves(&mut moves);
        moves.retain(|m| !m.is_zeroing());

        let mut best = if wdl >= DecisiveWdl::CursedWin {
            None
        } else {
            Some(Dtz::before_zeroing(wdl))
        };

        for m in &moves {
            let mut after = self.pos.clone();
            after.play_unchecked(m);
            let v = -self.tablebase.probe_dtz(&after)?;
            if v == Dtz(1) && after.is_checkmate() {
                best = Some(Dtz(1));
            } else if wdl >= DecisiveWdl::CursedWin {
                if v > Dtz(0) && best.map_or(true, |best| v + Dtz(1) < best) {
                    best = Some(v + Dtz(1));
                }
            } else if best.map_or(true, |best| v - Dtz(1) < best) {
                best = Some(v - Dtz(1));
            }
        }

        best.ok_or_else(|| SyzygyError::ProbeFailed {
            metric: Metric::Dtz,
            material: self.pos.board().material(),
            error: ProbeError::CorruptedTable(::failure::Backtrace::new()),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use shakmaty::fen::Fen;
    use shakmaty::{Chess, Square};

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
        tables.add_directory("tables/regular").expect("read directory");

        let pos: Chess = "5BrN/8/8/8/8/2k5/8/2K5 b - -"
            .parse::<Fen>()
            .expect("valid fen")
            .position()
            .expect("legal position");

        assert_matches!(tables.best_move(&pos), Ok(Some((Move::Normal {
            role: Role::Rook,
            from: Square::G8,
            capture: None,
            to: Square::G1,
            promotion: None,
        }, Dtz(-1)))));
    }

    #[test]
    fn test_black_escapes_via_underpromotion() {
        let mut tables = Tablebase::new();
        tables.add_directory("tables/regular").expect("read directory");

        let pos: Chess = "8/6B1/8/8/B7/8/K1pk4/8 b - - 0 1"
            .parse::<Fen>()
            .expect("valid fen")
            .position()
            .expect("legal position");

        assert_matches!(tables.best_move(&pos), Ok(Some((Move::Normal {
            role: Role::Pawn,
            from: Square::C2,
            to: Square::C1,
            capture: None,
            promotion: Some(Role::Knight),
        }, Dtz(109)))));
    }

    #[test]
    #[ignore]
    fn test_many_pawns() {
        let mut tables = Tablebase::new();
        tables.add_directory("tables/regular").expect("read directory");

        let pos: Chess = "3k4/5P2/8/8/4K3/2P3P1/PP6/8 w - - 0 1"
            .parse::<Fen>()
            .expect("valid fen")
            .position()
            .expect("legal position");

        assert_matches!(tables.probe_dtz(&pos), Ok(Dtz(1)));
    }
}
