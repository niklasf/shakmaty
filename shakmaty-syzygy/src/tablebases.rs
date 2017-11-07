// This file is part of the shakmaty-syzygy library.
// Copyright (C) 2017 Niklas Fiekas <niklas.fiekas@backscattering.de>
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

use std::cmp::max;
use std::path::{Path, PathBuf};

use fnv::FnvHashMap;
use double_checked_cell::DoubleCheckedCell;

use shakmaty::{Role, Position, MoveList};

use types::{Syzygy, Wdl, Dtz, MAX_PIECES, SyzygyError, ErrorKind, SyzygyResult};
use material::Material;
use table::{WdlTag, DtzTag, Table};

fn rotate_role(role: Role) -> Role {
    match role {
        Role::Pawn => Role::Knight,
        Role::Knight => Role::Bishop,
        Role::Bishop => Role::Rook,
        Role::Rook => Role::Queen,
        Role::Queen => Role::King,
        Role::King => Role::Pawn,
    }
}

struct RoleRange {
    from: Role,
    to: Role,
}

impl RoleRange {
    fn excl(from: Role, to: Role) -> RoleRange {
        RoleRange { from, to }
    }

    fn incl(from: Role, to: Role) -> RoleRange {
        RoleRange { from, to: rotate_role(to) }
    }
}

impl Iterator for RoleRange {
    type Item = Role;

    fn next(&mut self) -> Option<Role> {
        if self.from != self.to {
            let from = self.from;
            self.from = rotate_role(from);
            Some(from)
        } else {
            None
        }
    }
}

enum ProbeState {
    /// Probe successful.
    Normal,
    /// Best move is zeroing.
    ZeroingBestMove,
    /// Threatening to force a capture.
    Threat,
}

/// A collection of tables.
#[derive(Debug)]
pub struct Tablebases<S: Position + Clone + Syzygy> {
    wdl: FnvHashMap<Material, (PathBuf, DoubleCheckedCell<Table<WdlTag, S>>)>,
    dtz: FnvHashMap<Material, (PathBuf, DoubleCheckedCell<Table<DtzTag, S>>)>,
}

impl<S: Position + Clone + Syzygy> Default for Tablebases<S> {
    fn default() -> Tablebases<S> {
        Tablebases::new()
    }
}

impl<S: Position + Clone + Syzygy> Tablebases<S> {
    pub fn new() -> Tablebases<S> {
        Tablebases {
            wdl: FnvHashMap::default(),
            dtz: FnvHashMap::default(),
        }
    }

    pub fn add_directory<P: AsRef<Path>>(&mut self, path: P) {
        use self::Role::*;

        let base = path.as_ref();

        if S::ONE_KING {
            for a in RoleRange::excl(Pawn, King) {
                self.add_both(base, &[King, a], &[King]);

                for b in RoleRange::incl(Pawn, a) {
                    self.add_both(base, &[King, a, b], &[King]);
                    self.add_both(base, &[King, a], &[King, b]);

                    for c in RoleRange::excl(Pawn, King) {
                        self.add_both(base, &[King, a, b], &[King, c]);
                    }

                    for c in RoleRange::incl(Pawn, b) {
                        self.add_both(base, &[King, a, b, c], &[King]);

                        for d in RoleRange::incl(Pawn, c) {
                            self.add_both(base, &[King, a, b, c, d], &[King]);
                        }

                        for d in RoleRange::excl(Pawn, King) {
                            self.add_both(base, &[King, a, b, c], &[King, d]);
                        }
                    }

                    for c in RoleRange::incl(Pawn, a) {
                        for d in RoleRange::incl(Pawn, if a == c { b } else { c }) {
                            self.add_both(base, &[King, a, b], &[King, c, d]);
                        }
                    }
                }
            }
        } else {
            for a in RoleRange::incl(Pawn, King) {
                for b in RoleRange::incl(Pawn, a) {
                    self.add_both(base, &[a], &[b]);

                    for c in RoleRange::incl(Pawn, King) {
                        self.add_both(base, &[a, b], &[c]);
                    }

                    for c in RoleRange::incl(Pawn, b) {
                        for d in RoleRange::incl(Pawn, King) {
                            self.add_both(base, &[a, b, c], &[d]);

                            for e in RoleRange::incl(Pawn, d) {
                                self.add_both(base, &[a, b, c], &[d, e]);
                            }
                        }

                        for d in RoleRange::incl(Pawn, c) {
                            for e in RoleRange::incl(Pawn, King) {
                                self.add_both(base, &[a, b, c, d], &[e]);

                                for f in RoleRange::incl(Pawn, e) {
                                    self.add_both(base, &[a, b, c, d], &[e, f]);
                                }
                            }

                            for e in RoleRange::incl(Pawn, d) {
                                for f in RoleRange::incl(Pawn, King) {
                                    self.add_both(base, &[a, b, c, d, e], &[f]);
                                }
                            }
                        }

                        for d in RoleRange::incl(Pawn, a) {
                            for e in RoleRange::incl(Pawn, if a == d { b } else { d }) {
                                for f in RoleRange::incl(Pawn, if a == d && b == e { c } else { e }) {
                                    self.add_both(base, &[a, b, c], &[d, e, f]);
                                }
                            }
                        }
                    }

                    for c in RoleRange::incl(Pawn, a) {
                        for d in RoleRange::incl(Pawn, if a == c { b } else { c }) {
                            self.add_both(base, &[a, b], &[c, d]);
                        }
                    }
                }
            }
        }
    }

    fn add_both(&mut self, base: &Path, white: &[Role], black: &[Role]) {
        let material = Material {
            white: white.iter().cloned().collect(),
            black: black.iter().cloned().collect(),
        };

        let mut path = PathBuf::from(base);
        path.push(material.to_string());

        path.set_extension(S::TBW_EXTENSION);
        if path.is_file() {
            self.wdl.insert(material.clone(), (path.clone(), DoubleCheckedCell::new()));
        }

        path.set_extension(S::TBZ_EXTENSION);
        if path.is_file() {
            self.dtz.insert(material, (path, DoubleCheckedCell::new()));
        }
    }

    pub fn probe_wdl(&self, pos: &S) -> SyzygyResult<Wdl> {
        if pos.board().occupied().count() > MAX_PIECES {
            return Err(SyzygyError::new(ErrorKind::TooManyPieces));
        }
        if pos.castling_rights().any() {
            return Err(SyzygyError::new(ErrorKind::Castling));
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

        let v = self.probe_wdl_table(pos)?;

        if alpha >= v {
            Ok((alpha, if alpha > Wdl::Draw { ProbeState::ZeroingBestMove } else { ProbeState::Normal }))
        } else {
            Ok((v, ProbeState::Normal))
        }
    }

    fn probe_compulsory_captures(&self, pos: &S, mut alpha: Wdl, beta: Wdl, threats: bool) -> SyzygyResult<(Wdl, ProbeState)> {
        if pos.them().count() > 1 {
            let (v, captures_found) = self.probe_captures(pos, alpha, beta)?;
            if captures_found {
                return Ok((v, ProbeState::ZeroingBestMove));
            }
        } else {
            let mut captures = MoveList::new();
            pos.capture_moves(&mut captures);
            if !captures.is_empty() {
                return Ok((Wdl::Loss, ProbeState::ZeroingBestMove));
            }
        }

        let mut threats_found = false;

        if threats || pos.board().occupied().count() >= 6 {
            let mut moves = MoveList::new();
            pos.legal_moves(&mut moves);
            for threat in moves {
                if threat.role() != Role::Pawn {
                    let mut after = pos.clone();
                    after.play_unchecked(&threat);

                    let (v_plus, captures_found) = self.probe_captures(&after, -beta, -alpha)?;
                    let v = -v_plus;

                    if captures_found && v > alpha {
                        threats_found = true;
                        alpha = v;
                        if alpha >= beta {
                            return Ok((v, ProbeState::Threat));
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

    fn probe_captures(&self, pos: &S, mut alpha: Wdl, beta: Wdl) -> SyzygyResult<(Wdl, bool)> {
        let mut captures = MoveList::new();
        pos.capture_moves(&mut captures);
        let captures_found = !captures.is_empty();

        for m in captures {
            let mut after = pos.clone();
            after.play_unchecked(&m);

            let (v_plus, _) = self.probe_compulsory_captures(pos, -beta, -alpha, false)?;
            let v = -v_plus;

            alpha = max(v, alpha);
            if alpha >= beta {
                break;
            }
        }

        Ok((alpha, captures_found))
    }

    fn probe_wdl_table(&self, pos: &S) -> SyzygyResult<Wdl> {
        println!("probe_wdl_table {}", ::shakmaty::fen::epd(pos, &::shakmaty::fen::FenOpts::default()));

        // Test for KvK.
        if S::ONE_KING && pos.board().kings() == pos.board().occupied() {
            return Ok(Wdl::Draw);
        }

        // Variant game end.
        if let Some(outcome) = pos.variant_outcome() {
            return Ok(Wdl::from_outcome(&outcome, pos.turn()));
        }

        // Probe table.
        let key = Material::from_board(pos.board());
        if let Some(&(ref path, ref table)) = self.wdl.get(&key).or_else(|| self.wdl.get(&key.flip())) {
            let table = table.get_or_try_init(|| Table::open(path, &key))?;
            table.probe_wdl_table(pos)
        } else {
            Err(SyzygyError::new(ErrorKind::MissingTable))
        }
    }

    pub fn probe_dtz(&self, pos: &S) -> SyzygyResult<Dtz> {
        if pos.board().occupied().count() > MAX_PIECES {
            return Err(SyzygyError::new(ErrorKind::TooManyPieces));
        }
        if pos.castling_rights().any() {
            return Err(SyzygyError::new(ErrorKind::Castling));
        }

        panic!("TODO: implement probe_dtz")
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

        assert_send(Tablebases::<Chess>::new());
        assert_sync(Tablebases::<Chess>::new());
    }
}
