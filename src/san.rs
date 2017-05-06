//! Read and write Standard Algebraic Notation.

use types::{Move, Role};
use position::{Position, Outcome, MoveList};

use std::ascii::AsciiExt;

pub fn san<P: Position>(pos: P, m: &Move) -> String {
    fn suffix<P: Position>(pos: P, m: &Move) -> &'static str {
        let after = pos.play_unchecked(m);

        if let Some(Outcome::Decisive { .. }) = after.outcome() {
            "#"
        } else if after.checkers().any() {
            "+"
        } else {
            ""
        }
    }

    match *m {
        Move::Normal { role, from, capture, to, promotion } => {
            let mut san = String::new();

            if role != Role::Pawn {
                san.push(role.char().to_ascii_uppercase());

                // Disambiguate.
                let mut legals = MoveList::new();
                pos.legal_moves(&mut legals);

                let (rank, file) = legals.iter().fold((false, false), |(rank, file), c| match *c {
                    Move::Normal { role: r, to: t, from: candidate, .. } =>
                        if role != r || to != t || from == candidate {
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

            san.push_str(suffix(pos, m));

            san
        },
        Move::EnPassant { from, to, .. } => format!("{}x{}{}", from.file_char(), to, suffix(pos, m)),
        Move::Castle { .. } => format!("{}{}", m, suffix(pos, m)),
        Move::Put { to, role } => format!("{}@{}{}", role.char().to_ascii_uppercase(), to, suffix(pos, m)),
        Move::Null => "--".to_owned()
    }
}
