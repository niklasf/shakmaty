//! Read and write Standard Algebraic Notation.

use square::Square;
use types::{Move, Role};
use position::{Position, Outcome, MoveList};

use std::fmt;
use std::ascii::AsciiExt;
use option_filter::OptionFilterExt;

pub enum San {
    Normal { role: Role, file: Option<i8>, rank: Option<i8>, capture: bool, to: Square, promotion: Option<Role> },
    CastleShort,
    CastleLong,
    Put { role: Role, to: Square },
    Null,
}

pub struct SanPlus {
    pub san: San,
    pub check: bool,
    pub checkmate: bool,
}

impl fmt::Display for San {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            San::Normal { role, file, rank, capture, to, promotion } => {
                if role != Role::Pawn {
                    try!(write!(f, "{}", role.char().to_ascii_uppercase()));
                }
                if let Some(file) = file {
                    try!(write!(f, "{}", b'a' + file as u8));
                }
                if let Some(rank) = rank {
                    try!(write!(f, "{}", b'a' + rank as u8));
                }
                if capture {
                    try!(write!(f, "x"));
                }
                try!(write!(f, "{}", to));
                if let Some(promotion) = promotion {
                    try!(write!(f, "={}", promotion.char().to_ascii_uppercase()));
                }
                Ok(())
            },
            San::CastleShort => write!(f, "O-O"),
            San::CastleLong => write!(f, "O-O-O"),
            San::Put { role: Role::Pawn, to } =>
                write!(f, "@{}", to),
            San::Put { role, to } =>
                write!(f, "{}@{}", role.char().to_ascii_uppercase(), to),
            San::Null => write!(f, "--"),
        }
    }
}

impl fmt::Display for SanPlus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.checkmate {
            write!(f, "{}#", self.san)
        } else if self.check {
            write!(f, "{}+", self.san)
        } else {
            write!(f, "{}", self.san)
        }
    }
}

#[derive(Debug)]
pub enum SanError {
    IllegalSan,
    AmbiguousSan,
}

impl San {
    pub fn to_move<P: Position>(&self, pos: &P) -> Result<Move, SanError> {
        let mut legals = MoveList::new();
        pos.legal_moves(&mut legals);

        match *self {
            San::Normal { role, file, rank, capture, to, promotion } => legals.retain(|m| match *m {
                Move::Normal { role: r, from, capture: c, to: t, promotion: p } =>
                    role == r &&
                    file.map_or(true, |f| f == from.file()) &&
                    rank.map_or(true, |r| r == from.rank()) &&
                    capture == c.is_some() &&
                    to == t &&
                    promotion == p,
                Move::EnPassant { from, to: t } =>
                    role == Role::Pawn &&
                    file.map_or(true, |f| f == from.file()) &&
                    rank.map_or(true, |r| r == from.rank()) &&
                    capture &&
                    to == t &&
                    promotion.is_none(),
                _ => false,
            }),
            San::CastleShort => legals.retain(|m| match *m {
                Move::Castle { king, rook } => king.file() < rook.file(),
                _ => false,
            }),
            San::CastleLong => legals.retain(|m| match *m {
                Move::Castle { king, rook } => rook.file() < king.file(),
                _ => false,
            }),
            San::Put { role, to } => legals.retain(|m| match *m {
                Move::Put { role: r, to: t } => role == r && to == t,
                _ => false,
            }),
            San::Null => return Ok(Move::Null),
        }

        legals.split_first().map_or(Err(SanError::IllegalSan), |(m, others)| {
            if others.is_empty() {
                Ok(m.clone())
            } else {
                Err(SanError::AmbiguousSan)
            }
        })
    }
}

pub fn san_plus<P: Position>(pos: P, m: &Move) -> SanPlus {
    let san = san(&pos, m);
    let after = pos.play_unchecked(m);
    let checkmate = match after.outcome() {
        Some(Outcome::Decisive { .. }) => true,
        _ => false,
    };
    SanPlus { san, checkmate, check: !checkmate && after.checkers().any() }
}

pub fn san<P: Position>(pos: &P, m: &Move) -> San {
    match *m {
        Move::Normal { role: Role::Pawn, from, capture, to, promotion } =>
            San::Normal {
                role: Role::Pawn,
                file: Some(from.file()).filter(|_| capture.is_some()),
                rank: None,
                capture: capture.is_some(),
                to,
                promotion,
            },
        Move::Normal { role, from, capture, to, promotion } => {
            let mut legals = MoveList::new();
            pos.legal_moves(&mut legals);

            // Disambiguate.
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

            San::Normal {
                role,
                file: Some(from.file()).filter(|_| file),
                rank: Some(from.rank()).filter(|_| rank),
                capture: capture.is_some(),
                to,
                promotion
            }
        },
        Move::EnPassant { from, to, .. } => San::Normal {
            role: Role::Pawn, file: Some(from.file()), rank: None, capture: true, to, promotion: None },
        Move::Castle { rook, king } if rook.file() < king.file() => San::CastleLong,
        Move::Castle { .. } => San::CastleShort,
        Move::Put { role, to } => San::Put { role, to },
        Move::Null => San::Null,
    }
}
