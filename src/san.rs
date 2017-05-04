    /* fn san_candidates(&self, moves: &mut Vec<Move>, role: Role, target: Square) {
        let pos = self.position();
        self.legal_moves(moves);
        moves.retain(|m| match *m {
            Move::Normal { from, to, .. } =>
                to == target && pos.board().by_piece(role.of(pos.turn())).contains(from),
            _ => false
        })
    }

    fn san(self, m: &Move) -> String {
        fn suffix(pos: Situation, m: &Move) -> &'static str {
            let after = pos.do_move(m);

            if after.checkers().is_empty() {
                ""
            } else {
                let mut legals = Vec::new();
                after.legal_moves(&mut legals);
                if legals.is_empty() { "#" } else { "+" }
            }
        }

        match *m {
            Move::Normal { role, from, capture, to, promotion } => {
                let mut san = String::new();

                if role != Role::Pawn {
                    san.push(role.char().to_ascii_uppercase());

                    // Disambiguate.
                    let mut legals = Vec::new();
                    self.san_candidates(&mut legals, role, to);

                    let (rank, file) = legals.iter().fold((false, false), |(rank, file), c| match *c {
                        Move::Normal { from: candidate, .. } =>
                            if from == candidate {
                                (rank, file)
                            } else if from.rank() == candidate.rank() || from.file() != candidate.file() {
                                (rank, true)
                            } else {
                                (true, file)
                            },
                        _ => (rank, file)
                    });

                    if file {
                        san.push(from.file_char());
                    }
                    if rank {
                        san.push(from.rank_char());
                    }
                }

                if capture.is_some() {
                    if role == Role::Pawn {
                        san.push(from.file_char())
                    }
                    san.push('x');
                }

                san.push_str(&to.to_string());

                promotion.map(|r| {
                    san.push('=');
                    san.push(r.char().to_ascii_uppercase());
                });

                san.push_str(suffix(self, m));

                san
            },
            Move::EnPassant { from, to, .. } => format!("{}x{}{}", from.file_char(), to, suffix(self, m)),
            Move::Castle { .. } => format!("{}{}", m, suffix(self, m)),
            Move::Put { to, role } => format!("{}@{}{}", role.char().to_ascii_uppercase(), to, suffix(self, m)),
            Move::Null => "--".to_owned()
        }
    } */
