//! Read and write Standard Algebraic Notation.
//!
//! # Examples
//!
//! Parse and write SANs:
//!
//! ```
//! use shakmaty::{Chess, Position, san::San};
//!
//! let san: San = "Nf3".parse()?;
//! assert_eq!(san.to_string(), "Nf3");
//! # Ok::<_, shakmaty::san::ParseSanError>(())
//! ```
//!
//! Converting to a move:
//!
//! ```
//! # use shakmaty::{Chess, Position, san::{ParseSanError, San, SanError}};
//! use shakmaty::{Square, Role, Move};
//! #
//! # let san: San = "Nf3".parse()?;
//! let pos = Chess::default();
//! let m = san.to_move(&pos)?;
//!
//! assert_eq!(m, Move::Normal {
//!     role: Role::Knight,
//!     from: Square::G1,
//!     capture: None,
//!     to: Square::F3,
//!     promotion: None,
//! });
//!
//! # #[derive(Debug)] struct CommonError;
//! # impl From<ParseSanError> for CommonError { fn from(_: ParseSanError) -> Self { Self } }
//! # impl From<SanError> for CommonError { fn from(_: SanError) -> Self { Self } }
//! # Ok::<_, CommonError>(())
//! ```
//!
//! Back to a (possibly disambiguated) SAN:
//!
//! ```
//! # use shakmaty::{Chess, Position, Role, san::{ParseSanError, San, SanError}};
//! #
//! # let pos = Chess::default();
//! # let san: San = "Nf3".parse()?;
//! # let m = san.to_move(&pos)?;
//! assert_eq!(San::from_move(&pos, &m).to_string(), "Nf3");
//!
//! # #[derive(Debug)] struct CommonError;
//! # impl From<ParseSanError> for CommonError { fn from(_: ParseSanError) -> Self { Self } }
//! # impl From<SanError> for CommonError { fn from(_: SanError) -> Self { Self } }
//! # Ok::<_, CommonError>(())
//! ```

use core::{fmt, str::FromStr};

use crate::{
    util::AppendAscii, CastlingSide, File, Move, MoveList, Outcome, Position, Rank, Role, Square,
};

/// Error when parsing a syntactically invalid SAN.
#[derive(Clone, Debug)]
pub struct ParseSanError;

impl fmt::Display for ParseSanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("invalid san")
    }
}

#[cfg(feature = "std")]
impl std::error::Error for ParseSanError {}

/// `IllegalSan` or `AmbiguousSan`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SanError {
    /// Standard algebraic notation does not match a legal move.
    IllegalSan,
    /// Standard algebraic notation matches multiple legal moves.
    AmbiguousSan,
}

impl fmt::Display for SanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match *self {
            SanError::IllegalSan => "illegal san",
            SanError::AmbiguousSan => "ambiguous san",
        })
    }
}

#[cfg(feature = "std")]
impl std::error::Error for SanError {}

/// A move in Standard Algebraic Notation.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum San {
    Normal {
        role: Role,
        file: Option<File>,
        rank: Option<Rank>,
        capture: bool,
        to: Square,
        promotion: Option<Role>,
    },
    Castle(CastlingSide),
    Put {
        role: Role,
        to: Square,
    },
    Null,
}

impl San {
    /// Parses a SAN. Ignores a possible check or checkmate suffix.
    ///
    /// # Errors
    ///
    /// Returns [`ParseSanError`] if `san` is not syntactically valid.
    pub fn from_ascii(mut san: &[u8]) -> Result<San, ParseSanError> {
        if san.ends_with(b"#") || san.ends_with(b"+") {
            san = &san[0..(san.len() - 1)];
        }

        if san == b"--" {
            Ok(San::Null)
        } else if san == b"O-O" {
            Ok(San::Castle(CastlingSide::KingSide))
        } else if san == b"O-O-O" {
            Ok(San::Castle(CastlingSide::QueenSide))
        } else if san.len() == 3 && san[0] == b'@' {
            Ok(San::Put {
                role: Role::Pawn,
                to: Square::from_ascii(&san[1..]).map_err(|_| ParseSanError)?,
            })
        } else if san.len() == 4 && san[1] == b'@' {
            Ok(San::Put {
                role: Role::from_char(char::from(san[0])).ok_or(ParseSanError)?,
                to: Square::from_ascii(&san[2..]).map_err(|_| ParseSanError)?,
            })
        } else {
            let mut chars = san.iter().copied();

            let ch = chars.next().ok_or(ParseSanError)?;
            let (role, next) = if ch.is_ascii_uppercase() {
                (
                    Role::from_char(char::from(ch)).ok_or(ParseSanError)?,
                    chars.next().ok_or(ParseSanError)?,
                )
            } else {
                (Role::Pawn, ch)
            };

            let (file, next) = if let Some(file) = File::from_char(char::from(next)) {
                (Some(file), chars.next().ok_or(ParseSanError)?)
            } else {
                (None, next)
            };

            let (rank, next) = if let Some(rank) = Rank::from_char(char::from(next)) {
                (Some(rank), chars.next())
            } else {
                (None, Some(next))
            };

            // This section is safe, because coordinates are already validated
            // by file_from_char or rank_from_char.
            let (capture, file, rank, to, next) = if let Some(next) = next {
                if next == b'x' {
                    let to_file = chars
                        .next()
                        .and_then(|ch| File::from_char(char::from(ch)))
                        .ok_or(ParseSanError)?;
                    let to_rank = chars
                        .next()
                        .and_then(|ch| Rank::from_char(char::from(ch)))
                        .ok_or(ParseSanError)?;
                    let square = Square::from_coords(to_file, to_rank);
                    (true, file, rank, square, chars.next())
                } else if next == b'=' {
                    let square =
                        Square::from_coords(file.ok_or(ParseSanError)?, rank.ok_or(ParseSanError)?);
                    (false, None, None, square, Some(b'='))
                } else {
                    let to_file = File::from_char(char::from(next)).ok_or(ParseSanError)?;
                    let to_rank = chars
                        .next()
                        .and_then(|ch| Rank::from_char(char::from(ch)))
                        .ok_or(ParseSanError)?;
                    let square = Square::from_coords(to_file, to_rank);
                    (false, file, rank, square, chars.next())
                }
            } else {
                let square =
                    Square::from_coords(file.ok_or(ParseSanError)?, rank.ok_or(ParseSanError)?);
                (false, None, None, square, None)
            };

            let promotion = match next {
                Some(b'=') => Some(
                    chars
                        .next()
                        .and_then(|r| Role::from_char(char::from(r)))
                        .ok_or(ParseSanError)?,
                ),
                Some(_) => return Err(ParseSanError),
                None => None,
            };

            Ok(San::Normal {
                role,
                file,
                rank,
                capture,
                to,
                promotion,
            })
        }
    }

    /// Converts a move to Standard Algebraic Notation.
    pub fn from_move<P: Position>(pos: &P, m: &Move) -> San {
        let legals = match *m {
            Move::Normal { role, to, .. } if role != Role::Pawn => pos.san_candidates(role, to),
            _ => MoveList::new(),
        };

        San::disambiguate(m, &legals)
    }

    /// Tries to convert the `San` to a legal move in the context of a
    /// position.
    ///
    /// # Errors
    ///
    /// Returns [`SanError`] if there is no unique matching legal move.
    pub fn to_move<P: Position>(&self, pos: &P) -> Result<Move, SanError> {
        match *self {
            San::Normal {
                role,
                file,
                rank,
                capture,
                to,
                promotion,
            } => {
                let mut legals = pos.san_candidates(role, to);
                legals.retain(|m| match *m {
                    Move::Normal {
                        from,
                        capture: c,
                        promotion: p,
                        ..
                    } => {
                        file.map_or(true, |f| f == from.file())
                            && rank.map_or(true, |r| r == from.rank())
                            && capture == c.is_some()
                            && promotion == p
                    }
                    Move::EnPassant { from, .. } => {
                        file.map_or(true, |f| f == from.file())
                            && rank.map_or(true, |r| r == from.rank())
                            && capture
                            && promotion.is_none()
                    }
                    _ => false,
                });
                legals
                    .split_first()
                    .map_or(Err(SanError::IllegalSan), |(m, others)| {
                        if others.is_empty() {
                            Ok(m.clone())
                        } else {
                            Err(SanError::AmbiguousSan)
                        }
                    })
            }
            San::Castle(side) => pos
                .castling_moves(side)
                .first()
                .cloned()
                .ok_or(SanError::IllegalSan),
            San::Put { role, to } => {
                let mut legals = pos.san_candidates(role, to);
                legals.retain(|m| matches!(*m, Move::Put { .. }));
                legals.first().cloned().ok_or(SanError::IllegalSan)
            }
            San::Null => Err(SanError::IllegalSan),
        }
    }

    pub fn disambiguate(m: &Move, moves: &MoveList) -> San {
        match *m {
            Move::Normal {
                role: Role::Pawn,
                from,
                capture,
                to,
                promotion,
            } => San::Normal {
                role: Role::Pawn,
                file: if capture.is_some() {
                    Some(from.file())
                } else {
                    None
                },
                rank: None,
                capture: capture.is_some(),
                to,
                promotion,
            },
            Move::Normal {
                role,
                from,
                capture,
                to,
                promotion,
            } => {
                // Disambiguate.
                let mut ambiguous = false;
                let mut ambiguous_file = false;
                let mut ambiguous_rank = false;
                for candidate in moves {
                    match *candidate {
                        Move::Normal {
                            role: r,
                            to: t,
                            promotion: p,
                            from: f,
                            ..
                        } if from != f && role == r && to == t && promotion == p => {
                            ambiguous = true;
                            if from.rank() == f.rank() {
                                ambiguous_rank = true;
                            }
                            if from.file() == f.file() {
                                ambiguous_file = true;
                            }
                        }
                        _ => {}
                    }
                }
                San::Normal {
                    role,
                    file: (ambiguous && (!ambiguous_file || ambiguous_rank)).then(|| from.file()),
                    rank: ambiguous_file.then(|| from.rank()),
                    capture: capture.is_some(),
                    to,
                    promotion,
                }
            }
            Move::EnPassant { from, to, .. } => San::Normal {
                role: Role::Pawn,
                file: Some(from.file()),
                rank: None,
                capture: true,
                to,
                promotion: None,
            },
            Move::Castle { rook, king } if rook.file() < king.file() => {
                San::Castle(CastlingSide::QueenSide)
            }
            Move::Castle { .. } => San::Castle(CastlingSide::KingSide),
            Move::Put { role, to } => San::Put { role, to },
        }
    }

    /// Searches a [`MoveList`] for a unique matching move.
    ///
    /// # Errors
    ///
    /// Returns [`SanError`] if there is no unique matching legal move.
    pub fn find_move<'a>(&self, moves: &'a MoveList) -> Result<&'a Move, SanError> {
        let mut filtered = moves.iter().filter(|m| self.matches(m));

        let Some(m) = filtered.next() else {
            return Err(SanError::IllegalSan);
        };

        if filtered.next().is_some() {
            Err(SanError::AmbiguousSan)
        } else {
            Ok(m)
        }
    }

    /// Test if the `San` can match the `Move` (in any position).
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Square, Role, Move, san::San};
    ///
    /// let m = Move::Normal {
    ///     role: Role::Knight,
    ///     from: Square::G1,
    ///     to: Square::F3,
    ///     capture: None,
    ///     promotion: None,
    /// };
    ///
    /// let nf3 = San::from_ascii(b"Nf3")?;
    /// assert!(nf3.matches(&m));
    ///
    /// let ng1f3 = San::from_ascii(b"Ng1f3")?;
    /// assert!(ng1f3.matches(&m));
    ///
    /// // capture does not match
    /// let nxf3 = San::from_ascii(b"Nxf3")?;
    /// assert!(!nxf3.matches(&m));
    ///
    /// // other file does not match
    /// let nef3 = San::from_ascii(b"Nef3")?;
    /// assert!(!nef3.matches(&m));
    ///
    /// # Ok::<_, shakmaty::san::ParseSanError>(())
    /// ```
    pub fn matches(&self, m: &Move) -> bool {
        match *self {
            San::Normal {
                role,
                file,
                rank,
                capture,
                to,
                promotion,
            } => match *m {
                Move::Normal {
                    role: r,
                    from,
                    capture: c,
                    to: t,
                    promotion: pr,
                } => {
                    role == r
                        && file.map_or(true, |f| f == from.file())
                        && rank.map_or(true, |r| r == from.rank())
                        && capture == c.is_some()
                        && to == t
                        && promotion == pr
                }
                Move::EnPassant { from, to: t } => {
                    role == Role::Pawn
                        && file.map_or(true, |f| f == from.file())
                        && rank.map_or(true, |r| r == from.rank())
                        && capture
                        && to == t
                        && promotion.is_none()
                }
                _ => false,
            },
            San::Castle(side) => m.castling_side().is_some_and(|s| side == s),
            San::Put { role, to } => match *m {
                Move::Put { role: r, to: t } => r == role && to == t,
                _ => false,
            },
            San::Null => false,
        }
    }

    fn append_to<W: AppendAscii>(&self, f: &mut W) -> Result<(), W::Error> {
        match *self {
            San::Normal {
                role,
                file,
                rank,
                capture,
                to,
                promotion,
            } => {
                if role != Role::Pawn {
                    f.append_ascii(role.upper_char())?;
                }
                if let Some(file) = file {
                    f.append_ascii(file.char())?;
                }
                if let Some(rank) = rank {
                    f.append_ascii(rank.char())?;
                }
                if capture {
                    f.append_ascii('x')?;
                }
                to.append_to(f)?;
                if let Some(promotion) = promotion {
                    f.append_ascii('=')?;
                    f.append_ascii(promotion.upper_char())?;
                }
            }
            San::Castle(CastlingSide::KingSide) => {
                f.append_ascii('O')?;
                f.append_ascii('-')?;
                f.append_ascii('O')?;
            }
            San::Castle(CastlingSide::QueenSide) => {
                f.append_ascii('O')?;
                f.append_ascii('-')?;
                f.append_ascii('O')?;
                f.append_ascii('-')?;
                f.append_ascii('O')?;
            }
            San::Put { role, to } => {
                if role != Role::Pawn {
                    f.append_ascii(role.upper_char())?;
                }
                f.append_ascii('@')?;
                to.append_to(f)?;
            }
            San::Null => {
                f.append_ascii('-')?;
                f.append_ascii('-')?;
            }
        }
        Ok(())
    }

    #[cfg(feature = "alloc")]
    pub fn append_to_string(&self, s: &mut alloc::string::String) {
        let _ = self.append_to(s);
    }

    #[cfg(feature = "alloc")]
    pub fn append_ascii_to(&self, buf: &mut alloc::vec::Vec<u8>) {
        let _ = self.append_to(buf);
    }

    #[cfg(feature = "std")]
    pub fn write_ascii_to<W: std::io::Write>(&self, w: W) -> std::io::Result<()> {
        self.append_to(&mut crate::util::WriteAscii(w))
    }
}

impl FromStr for San {
    type Err = ParseSanError;

    fn from_str(san: &str) -> Result<San, ParseSanError> {
        San::from_ascii(san.as_bytes())
    }
}

impl fmt::Display for San {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.append_to(f)
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for San {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // Longest syntactically valid SAN: Na1xa1=Q
        let mut s = arrayvec::ArrayString::<8>::new();
        let _ = self.append_to(&mut s);
        serializer.serialize_str(&s)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for San {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct SanVisitor;

        impl<'de> serde::de::Visitor<'de> for SanVisitor {
            type Value = San;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("SAN string")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                value.parse().map_err(serde::de::Error::custom)
            }
        }

        deserializer.deserialize_str(SanVisitor)
    }
}

/// Check (`+`) or checkmate (`#`) suffix.
#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum Suffix {
    Check,
    Checkmate,
}

impl Suffix {
    pub const fn char(self) -> char {
        match self {
            Suffix::Check => '+',
            Suffix::Checkmate => '#',
        }
    }

    pub const fn from_char(ch: char) -> Option<Suffix> {
        match ch {
            '+' => Some(Suffix::Check),
            '#' => Some(Suffix::Checkmate),
            _ => None,
        }
    }

    pub fn from_position<P: Position>(pos: &P) -> Option<Suffix> {
        if matches!(pos.outcome(), Some(Outcome::Decisive { .. })) {
            Some(Suffix::Checkmate)
        } else if pos.checkers().any() {
            Some(Suffix::Check)
        } else {
            None
        }
    }
}

impl fmt::Display for Suffix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.char())
    }
}

/// A [`San`] and possible check and checkmate suffixes.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct SanPlus {
    pub san: San,
    pub suffix: Option<Suffix>,
}

impl SanPlus {
    /// Parses a SAN and possible check and checkmate suffix.
    ///
    /// # Errors
    ///
    /// Returns [`ParseSanError`] if `san` is not syntactically valid.
    pub fn from_ascii(san: &[u8]) -> Result<SanPlus, ParseSanError> {
        San::from_ascii(san).map(|result| SanPlus {
            san: result,
            suffix: san
                .last()
                .copied()
                .and_then(|ch| Suffix::from_char(char::from(ch))),
        })
    }

    /// Converts a move to Standard Algebraic Notation including possible
    /// check and checkmate suffixes. Also plays the move.
    ///
    /// It is the callers responsibility to ensure the move is legal.
    ///
    /// # Panics
    ///
    /// Illegal moves can corrupt the state of the position and may
    /// (or may not) panic or cause panics on future calls.
    pub fn from_move_and_play_unchecked<P: Position>(pos: &mut P, m: &Move) -> SanPlus {
        let san = San::from_move(pos, m);
        pos.play_unchecked(m);
        SanPlus {
            san,
            suffix: Suffix::from_position(pos),
        }
    }

    pub fn from_move<P: Position>(mut pos: P, m: &Move) -> SanPlus {
        let moves = match *m {
            Move::Normal { role, to, .. } | Move::Put { role, to } => pos.san_candidates(role, to),
            Move::EnPassant { to, .. } => pos.san_candidates(Role::Pawn, to),
            Move::Castle { king, rook } if king.file() < rook.file() => {
                pos.castling_moves(CastlingSide::KingSide)
            }
            Move::Castle { .. } => pos.castling_moves(CastlingSide::QueenSide),
        };
        SanPlus {
            san: San::disambiguate(m, &moves),
            suffix: if moves.contains(m) {
                pos.play_unchecked(m);
                Suffix::from_position(&pos)
            } else {
                None
            },
        }
    }

    fn append_to<W: AppendAscii>(&self, f: &mut W) -> Result<(), W::Error> {
        self.san.append_to(f)?;
        if let Some(suffix) = self.suffix {
            f.append_ascii(suffix.char())?;
        }
        Ok(())
    }

    #[cfg(feature = "alloc")]
    pub fn append_to_string(&self, s: &mut alloc::string::String) {
        let _ = self.append_to(s);
    }

    #[cfg(feature = "alloc")]
    pub fn append_ascii_to(&self, buf: &mut alloc::vec::Vec<u8>) {
        let _ = self.append_to(buf);
    }

    #[cfg(feature = "std")]
    pub fn write_ascii_to<W: std::io::Write>(&self, w: W) -> std::io::Result<()> {
        self.append_to(&mut crate::util::WriteAscii(w))
    }
}

impl FromStr for SanPlus {
    type Err = ParseSanError;

    fn from_str(san: &str) -> Result<SanPlus, ParseSanError> {
        SanPlus::from_ascii(san.as_bytes())
    }
}

impl fmt::Display for SanPlus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.append_to(f)
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for SanPlus {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // Longest syntactically valid SAN with suffix: Na1xa1=Q#
        let mut s = arrayvec::ArrayString::<9>::new();
        let _ = self.append_to(&mut s);
        serializer.serialize_str(&s)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for SanPlus {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct SanPlusVisitor;

        impl<'de> serde::de::Visitor<'de> for SanPlusVisitor {
            type Value = SanPlus;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("SAN string with suffix")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                value.parse().map_err(serde::de::Error::custom)
            }
        }

        deserializer.deserialize_str(SanPlusVisitor)
    }
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "alloc")]
    use alloc::string::ToString;
    use core::mem;

    use super::*;
    use crate::{fen::Fen, uci::UciMove, CastlingMode, Chess};

    #[test]
    fn test_size() {
        assert!(mem::size_of::<San>() <= 8);
        assert!(mem::size_of::<SanPlus>() <= 8);
    }

    #[cfg(feature = "alloc")]
    #[test]
    fn test_read_write() {
        for san in &[
            "a1", "a8", "h1", "h8", "e4", "b6", "e4=Q", "f1=N#", "hxg7", "bxc1", "axe4", "bxc1+",
            "bxa8=R+", "Nf3", "Ba5", "Qh8", "Kh1", "Qh1=K", "Ba5", "Bba5", "N2c4", "Red3", "Qh1=K",
            "d1=N", "@e4#", "K@b3", "Ba5", "Bba5", "Ra1a8", "--", "O-O", "O-O-O+",
        ] {
            let result = san.parse::<SanPlus>().expect("valid san").to_string();
            assert_eq!(*san, result, "read {} write {}", san, result);
        }
    }

    #[test]
    fn test_pawn_capture_without_file() {
        let san = "f6".parse::<San>().expect("valid san");

        let pos = "4k3/8/5p2/4P3/8/8/8/4K3 w -"
            .parse::<Fen>()
            .expect("valid fen")
            .into_position::<Chess>(CastlingMode::Standard)
            .expect("legal fen");
        assert_eq!(san.to_move(&pos), Err(SanError::IllegalSan));

        let pos = "4k3/8/8/4Pp2/8/8/8/4K3 w - f6"
            .parse::<Fen>()
            .expect("valid fen")
            .into_position::<Chess>(CastlingMode::Standard)
            .expect("legal fen");
        assert_eq!(san.to_move(&pos), Err(SanError::IllegalSan));
    }

    #[test]
    fn test_disambiguation() {
        let chaos_fen = "N3k2N/8/8/3N4/N4N1N/2R5/1R6/4K3 w - -";
        let regression_fen = "8/2KN1p2/5p2/3N1B1k/5PNp/7P/7P/8 w - -";
        let illegal_alternatives_fen = "8/8/8/R2nkn2/8/8/2K5/8 b - -";
        let promotions_fen = "7k/1p2Npbp/8/2P5/1P1r4/3b2QP/3q1pPK/2RB4 b - -";

        for (fen, uci, san) in [
            (chaos_fen, "e1f1", "Kf1"),
            (chaos_fen, "c3c2", "Rcc2"),
            (chaos_fen, "b2c2", "Rbc2"),
            (chaos_fen, "a4b6", "N4b6"),
            (chaos_fen, "h8g6", "N8g6"),
            (chaos_fen, "h4g6", "Nh4g6"),
            (regression_fen, "d5f6", "N5xf6#"),
            (illegal_alternatives_fen, "f5e3", "Ne3+"),
            (promotions_fen, "f2f1q", "f1=Q"),
            (promotions_fen, "f2f1n", "f1=N+"),
        ] {
            let pos = fen
                .parse::<Fen>()
                .expect("valid fen")
                .into_position::<Chess>(CastlingMode::Standard)
                .expect("legal fen");
            let m = uci
                .parse::<UciMove>()
                .expect("valid uci")
                .to_move(&pos)
                .expect("legal uci");
            let san_plus = san.parse::<SanPlus>().expect("valid san");

            assert_eq!(San::disambiguate(&m, &pos.legal_moves()), san_plus.san);
            assert_eq!(SanPlus::from_move(pos, &m), san_plus);
        }
    }

    #[cfg(feature = "alloc")]
    #[test]
    fn test_lax_pawn_move_san_roundtrip() {
        let san = "6h8".parse::<San>().expect("kinda valid san");
        assert_eq!(
            san,
            San::Normal {
                role: Role::Pawn,
                file: None,
                rank: Some(Rank::Sixth),
                capture: false,
                to: Square::H8,
                promotion: None,
            }
        );
        assert_eq!(san.to_string(), "6h8");
    }
}
