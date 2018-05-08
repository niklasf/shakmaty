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

use std::cmp::{max, min};
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use double_checked_cell::DoubleCheckedCell;
use fxhash::FxHashMap;

use shakmaty::{MoveList, Position, Role};

use errors::{SyzygyError, SyzygyResult};
use material::Material;
use table::{DtzTag, Table, WdlTag};
use types::{Dtz, Syzygy, Wdl, MAX_PIECES};

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
    wdl: FxHashMap<Material, (PathBuf, DoubleCheckedCell<Table<WdlTag, S>>)>,
    dtz: FxHashMap<Material, (PathBuf, DoubleCheckedCell<Table<DtzTag, S>>)>,
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
            wdl: FxHashMap::default(),
            dtz: FxHashMap::default(),
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

            if material.count() > MAX_PIECES {
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
        if pos.board().occupied().count() > MAX_PIECES {
            return Err(SyzygyError::TooManyPieces);
        }
        if pos.castles().any() {
            return Err(SyzygyError::Castling);
        }

        // Probe.
        let (mut v, _) = self.probe_ab(pos, Wdl::Loss, Wdl::Win, false)?;

        if S::CAPTURES_COMPULSORY {
            return Ok(v);
        }

        // If en passant is not possible we are done.
        let mut ep_moves = MoveList::new();
        pos.en_passant_moves(&mut ep_moves);
        if ep_moves.is_empty() {
            return Ok(v);
        }

        // Now look at all legal en passant captures.
        let mut v1 = Wdl::Loss;
        for m in ep_moves {
            let mut after = pos.clone();
            after.play_unchecked(&m);

            let (v0_plus, _) = self.probe_ab(&after, Wdl::Loss, Wdl::Win, false)?;
            let v0 = -v0_plus;

            v1 = max(v0, v1);
        }

        if v1 >= v {
            v = v1;
        } else if v == Wdl::Draw {
            // If there is not at least one legal non-en-passant move we are
            // forced to play the losing en passant move.
            let mut moves = MoveList::new();
            pos.legal_moves(&mut moves);
            if moves.iter().all(|m| m.is_en_passant()) {
                v = v1;
            }
        }

        Ok(v)
    }

    fn probe_ab(&self, pos: &S, mut alpha: Wdl, beta: Wdl, threats: bool) -> SyzygyResult<(Wdl, ProbeState)> {
        if S::CAPTURES_COMPULSORY {
            if let Some(outcome) = pos.variant_outcome() {
                return Ok((Wdl::from_outcome(&outcome, pos.turn()), ProbeState::ZeroingBestMove));
            }

            return self.probe_compulsory_captures(pos, alpha, beta, threats);
        }

        // Search non-ep captures.
        let mut captures = MoveList::new();
        pos.capture_moves(&mut captures);
        captures.retain(|m| !m.is_en_passant());
        for m in captures {
            let mut after = pos.clone();
            after.play_unchecked(&m);

            let (v_plus, _) = self.probe_ab(&after, -beta, -alpha, false)?;
            let v = -v_plus;

            if v > alpha {
                if v >= beta {
                    return Ok((v, ProbeState::ZeroingBestMove));
                }
                alpha = v;
            }
        }

        // Probe table.
        let v = self.probe_wdl_table(pos)?;

        if alpha >= v {
            Ok((alpha, if alpha > Wdl::Draw { ProbeState::ZeroingBestMove } else { ProbeState::Normal }))
        } else {
            Ok((v, ProbeState::Normal))
        }
    }

    fn probe_compulsory_captures(&self, pos: &S, mut alpha: Wdl, beta: Wdl, threats: bool) -> SyzygyResult<(Wdl, ProbeState)> {
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
        // Explore capture moves in antichess variants. If captures exists they
        // are also the only moves, because captures are compulsory.
        let mut captures = MoveList::new();
        pos.capture_moves(&mut captures);

        for m in captures.iter() {
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
            return Ok(Wdl::from_outcome(&outcome, pos.turn()));
        }

        // Test for KvK.
        if S::ONE_KING && pos.board().kings() == pos.board().occupied() {
            return Ok(Wdl::Draw);
        }

        // Probe table.
        let key = Material::from_board(pos.board());
        if let Some(&(ref path, ref table)) = self.wdl.get(&key).or_else(|| self.wdl.get(&key.flipped())) {
            let table = table.get_or_try_init(|| Table::open(path, &key))?;
            table.probe_wdl_table(pos)
        } else {
            Err(SyzygyError::MissingTable { material: key.normalized() })
        }
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
        if pos.board().occupied().count() > MAX_PIECES {
            return Err(SyzygyError::TooManyPieces);
        }
        if pos.castles().any() {
            return Err(SyzygyError::Castling);
        }

        // Probe.
        let mut v = self.probe_dtz_no_ep(pos)?;

        if S::CAPTURES_COMPULSORY {
            return Ok(v);
        }

        // If en passant is not possible we are done.
        let mut ep_moves = MoveList::new();
        pos.en_passant_moves(&mut ep_moves);
        if ep_moves.is_empty() {
            return Ok(v);
        }

        // Check all en passant moves.
        let mut wdl = Wdl::Loss;

        for m in ep_moves {
            let mut after = pos.clone();
            after.play_unchecked(&m);
            let (v0, _) = self.probe_ab(&after, Wdl::Loss, Wdl::Win, false)?;
            wdl = max(v0, wdl);
        }

        let v1 = Dtz::before_zeroing(wdl);
        if v < Dtz(-100) {
            if v1 >= Dtz(0) {
                v = v1;
            }
        } else if v < Dtz(0) {
            if v1 >= Dtz(0) || v1 < Dtz(-100) {
                v = v1;
            }
        } else if v > Dtz(100) {
            if v1 > Dtz(0) {
                v = v1;
            }
        } else if v > Dtz(0) {
            if v1 == Dtz(1) {
                v = v1;
            }
        } else if v1 >= Dtz(0) {
            v = v1;
        } else {
            let mut moves = MoveList::new();
            pos.legal_moves(&mut moves);
            if moves.iter().all(|m| m.is_en_passant()) {
                v = v1;
            }
        }

        Ok(v)
    }

    fn probe_dtz_no_ep(&self, pos: &S) -> SyzygyResult<Dtz> {
        let (wdl, state) = self.probe_ab(pos, Wdl::Loss, Wdl::Win, true)?;

        if wdl == Wdl::Draw {
            return Ok(Dtz(0));
        }

        if state == ProbeState::ZeroingBestMove || pos.us() == pos.our(Role::Pawn) {
            return Ok(Dtz::before_zeroing(wdl));
        }

        if wdl > Wdl::Draw {
            // The position is a win or a cursed win by a threat move.
            if state == ProbeState::Threat {
                return Ok(Dtz(if wdl == Wdl::Win { 2 } else { 101 }));
            }

            let mut moves = MoveList::new();
            pos.legal_moves(&mut moves);
            moves.retain(|m| m.role() == Role::Pawn && !m.is_capture());
            for m in moves {
                let mut after = pos.clone();
                after.play_unchecked(&m);

                let v = -self.probe_wdl(&after)?;

                if v == wdl {
                    return Ok(Dtz(if v == Wdl::Win { 1 } else { 101 }));
                }
            }
        }

        if let Some(Dtz(dtz)) = self.probe_dtz_table(pos, wdl)? {
            return Ok(Dtz::before_zeroing(wdl).add_plies(dtz))
        }

        if wdl > Wdl::Draw {
            let mut best = None;

            let mut moves = MoveList::new();
            pos.legal_moves(&mut moves);
            moves.retain(|m| !m.is_zeroing());
            for m in moves {
                let mut after = pos.clone();
                after.play_unchecked(&m);

                let v = -self.probe_dtz(&after)?;

                if v > Dtz(0) && best.map_or(true, |best| v + Dtz(1) < best) {
                    best = Some(if v == Dtz(1) && after.is_checkmate() {
                        Dtz(1)
                    } else {
                        v + Dtz(1)
                    });
                }
            }

            Ok(u!(best))
        } else {
            let mut best = Dtz(-1);

            let mut moves = MoveList::new();
            pos.legal_moves(&mut moves);
            for m in moves {
                let mut after = pos.clone();
                after.play_unchecked(&m);

                let v = if m.is_zeroing() {
                    if wdl == Wdl::Loss {
                        Dtz(-1)
                    } else {
                        let (v, _) = self.probe_ab(&after, Wdl::CursedWin, Wdl::Win, true)?;
                        Dtz(if v == Wdl::Win { 0 } else { -101 })
                    }
                } else {
                    -self.probe_dtz(&after)? - Dtz(1)
                };

                best = min(v, best);
            }

            Ok(best)
        }
    }

    fn probe_dtz_table(&self, pos: &S, wdl: Wdl) -> SyzygyResult<Option<Dtz>> {
        let key = Material::from_board(pos.board());
        if let Some(&(ref path, ref table)) = self.dtz.get(&key).or_else(|| self.dtz.get(&key.flipped())) {
            let table = table.get_or_try_init(|| Table::open(path, &key))?;
            table.probe_dtz_table(pos, wdl)
        } else {
            Err(SyzygyError::MissingTable { material: key.normalized() })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use shakmaty::Chess;

    #[test]
    fn test_send_sync() {
        fn assert_send<T: Send>(_: T) { }
        fn assert_sync<T: Sync>(_: T) { }

        assert_send(Tablebase::<Chess>::new());
        assert_sync(Tablebase::<Chess>::new());
    }
}
