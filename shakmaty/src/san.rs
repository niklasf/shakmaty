//! Read and write Standard Algebraic Notation.
//!
//! # Examples
//!
//! Parse and write SAN:
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
//! Back to (possibly disambiguated) SAN:
//!
//! ```
//! # use shakmaty::{Chess, Position, Role, san::{ParseSanError, San, SanError}};
//! #
//! # let pos = Chess::default();
//! # let san: San = "Nf3".parse()?;
//! # let m = san.to_move(&pos)?;
//! assert_eq!(San::from_move(&pos, m).to_string(), "Nf3");
//!
//! # #[derive(Debug)] struct CommonError;
//! # impl From<ParseSanError> for CommonError { fn from(_: ParseSanError) -> Self { Self } }
//! # impl From<SanError> for CommonError { fn from(_: SanError) -> Self { Self } }
//! # Ok::<_, CommonError>(())
//! ```

use core::{error, fmt, str::FromStr};

use crate::{
    CastlingSide, File, KnownOutcome, Move, MoveList, Outcome, Position, Rank, Role, Square,
    util::AppendAscii,
};

/// Error when parsing a syntactically invalid SAN.
#[derive(Clone, Debug)]
pub struct ParseSanError;

impl fmt::Display for ParseSanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("invalid san")
    }
}

impl error::Error for ParseSanError {}

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

impl error::Error for SanError {}

/// A move in Standard Algebraic Notation.
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[derive(Copy, Debug, PartialEq, Eq, Clone, Hash)]
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
    /// Parses the given ASCII bytes as a move in SAN. Ignores a possible check
    /// or checkmate suffix.
    ///
    /// # Errors
    ///
    /// Returns [`ParseSanError`] if `ascii` is not syntactically valid.
    pub fn from_ascii(ascii: &[u8]) -> Result<San, ParseSanError> {
        let mut reader = Reader::new(ascii);
        let san = reader.read_san().ok_or(ParseSanError)?;
        let _ = reader.eat(b'+') || reader.eat(b'#');
        if reader.remaining() != 0 {
            return Err(ParseSanError);
        }
        Ok(san)
    }

    /// Parses a move in SAN from the start of the given ASCII bytes. Does not
    /// consume a check or checkmate suffix.
    ///
    /// Every byte that might continue a move in SAN is eagerly consumed without
    /// backtracking.
    ///
    /// Returns the parsed SAN and the number of bytes consumed.
    ///
    /// # Errors
    ///
    /// Errors with [`ParseSanError`] if `ascii` does not start with a
    /// syntactically valid SAN or if backtracking would be required to parse it.
    ///
    /// For example, even though `Nf3=X` starts with `Nf3`, the parser commits
    /// to `Nf3=` and then fails on `X`.
    pub fn from_ascii_prefix(ascii: &[u8]) -> Result<(San, usize), ParseSanError> {
        let mut reader = Reader::new(ascii);
        let san = reader.read_san().ok_or(ParseSanError)?;
        Ok((san, ascii.len() - reader.remaining()))
    }

    /// Converts a move to Standard Algebraic Notation.
    pub fn from_move<P: Position>(pos: &P, m: Move) -> San {
        let legals = match m {
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
    pub fn to_move<P: Position>(self, pos: &P) -> Result<Move, SanError> {
        match self {
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
                        file.is_none_or(|f| f == from.file())
                            && rank.is_none_or(|r| r == from.rank())
                            && capture == c.is_some()
                            && promotion == p
                    }
                    Move::EnPassant { from, .. } => {
                        file.is_none_or(|f| f == from.file())
                            && rank.is_none_or(|r| r == from.rank())
                            && capture
                            && promotion.is_none()
                    }
                    _ => false,
                });
                legals
                    .split_first()
                    .map_or(Err(SanError::IllegalSan), |(m, others)| {
                        if others.is_empty() {
                            Ok(*m)
                        } else {
                            Err(SanError::AmbiguousSan)
                        }
                    })
            }
            San::Castle(side) => pos
                .castling_moves(side)
                .first()
                .copied()
                .ok_or(SanError::IllegalSan),
            San::Put { role, to } => {
                let mut legals = pos.san_candidates(role, to);
                legals.retain(|m| matches!(*m, Move::Put { .. }));
                legals.first().copied().ok_or(SanError::IllegalSan)
            }
            San::Null => Err(SanError::IllegalSan),
        }
    }

    pub fn disambiguate(m: Move, moves: &MoveList) -> San {
        match m {
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
    pub fn find_move(self, moves: &MoveList) -> Result<&Move, SanError> {
        let mut filtered = moves.iter().filter(|m| self.matches(**m));

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
    /// assert!(nf3.matches(m));
    ///
    /// let ng1f3 = San::from_ascii(b"Ng1f3")?;
    /// assert!(ng1f3.matches(m));
    ///
    /// // capture does not match
    /// let nxf3 = San::from_ascii(b"Nxf3")?;
    /// assert!(!nxf3.matches(m));
    ///
    /// // other file does not match
    /// let nef3 = San::from_ascii(b"Nef3")?;
    /// assert!(!nef3.matches(m));
    ///
    /// # Ok::<_, shakmaty::san::ParseSanError>(())
    /// ```
    pub fn matches(self, m: Move) -> bool {
        match self {
            San::Normal {
                role,
                file,
                rank,
                capture,
                to,
                promotion,
            } => match m {
                Move::Normal {
                    role: r,
                    from,
                    capture: c,
                    to: t,
                    promotion: pr,
                } => {
                    role == r
                        && file.is_none_or(|f| f == from.file())
                        && rank.is_none_or(|r| r == from.rank())
                        && capture == c.is_some()
                        && to == t
                        && promotion == pr
                }
                Move::EnPassant { from, to: t } => {
                    role == Role::Pawn
                        && file.is_none_or(|f| f == from.file())
                        && rank.is_none_or(|r| r == from.rank())
                        && capture
                        && to == t
                        && promotion.is_none()
                }
                _ => false,
            },
            San::Castle(side) => m.castling_side().is_some_and(|s| side == s),
            San::Put { role, to } => match m {
                Move::Put { role: r, to: t } => r == role && to == t,
                _ => false,
            },
            San::Null => false,
        }
    }

    fn append_to<W: AppendAscii>(self, f: &mut W) -> Result<(), W::Error> {
        match self {
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
    pub fn append_to_string(self, s: &mut alloc::string::String) {
        let _ = self.append_to(s);
    }

    #[cfg(feature = "alloc")]
    pub fn append_ascii_to(self, buf: &mut alloc::vec::Vec<u8>) {
        let _ = self.append_to(buf);
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

        impl serde::de::Visitor<'_> for SanVisitor {
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
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "bincode", derive(bincode::Encode, bincode::Decode))]
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
        if matches!(pos.outcome(), Outcome::Known(KnownOutcome::Decisive { .. })) {
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
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[derive(Copy, Debug, PartialEq, Eq, Clone, Hash)]
pub struct SanPlus {
    pub san: San,
    pub suffix: Option<Suffix>,
}

impl SanPlus {
    /// Parses the given ASCII bytes as a move in SAN and possible check or
    /// checkmate suffix.
    ///
    /// # Errors
    ///
    /// Errors with [`ParseSanError`] if `ascii` is not syntactically valid.
    pub fn from_ascii(ascii: &[u8]) -> Result<SanPlus, ParseSanError> {
        let mut reader = Reader::new(ascii);
        let san_plus = reader.read_san_plus().ok_or(ParseSanError)?;
        if reader.remaining() != 0 {
            return Err(ParseSanError);
        }
        Ok(san_plus)
    }

    /// Parses a move in SAN and possible check and checkmate suffix from the
    /// start of the given ASCII bytes.
    ///
    /// Every byte that might continue a move in SAN is eagerly consumed without
    /// backtracking.
    ///
    /// Returns the parsed SAN and the number of bytes consumed.
    ///
    /// # Errors
    ///
    /// Errors with [`ParseSanError`] if `ascii` does not start with a
    /// syntactically valid SAN or if backtracking would be required to parse it.
    ///
    /// For example, even though `Nf3=X` starts with `Nf3`, the parser commits
    /// to `Nf3=` and then fails on `X`.
    pub fn from_ascii_prefix(ascii: &[u8]) -> Result<(SanPlus, usize), ParseSanError> {
        let mut reader = Reader::new(ascii);
        let san_plus = reader.read_san_plus().ok_or(ParseSanError)?;
        Ok((san_plus, ascii.len() - reader.remaining()))
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
    pub fn from_move_and_play_unchecked<P: Position>(pos: &mut P, m: Move) -> SanPlus {
        let san = San::from_move(pos, m);
        pos.play_unchecked(m);
        SanPlus {
            san,
            suffix: Suffix::from_position(pos),
        }
    }

    pub fn from_move<P: Position>(mut pos: P, m: Move) -> SanPlus {
        let moves = match m {
            Move::Normal { role, to, .. } | Move::Put { role, to } => pos.san_candidates(role, to),
            Move::EnPassant { to, .. } => pos.san_candidates(Role::Pawn, to),
            Move::Castle { king, rook } if king.file() < rook.file() => {
                pos.castling_moves(CastlingSide::KingSide)
            }
            Move::Castle { .. } => pos.castling_moves(CastlingSide::QueenSide),
        };
        SanPlus {
            san: San::disambiguate(m, &moves),
            suffix: if moves.contains(&m) {
                pos.play_unchecked(m);
                Suffix::from_position(&pos)
            } else {
                None
            },
        }
    }

    fn append_to<W: AppendAscii>(self, f: &mut W) -> Result<(), W::Error> {
        self.san.append_to(f)?;
        if let Some(suffix) = self.suffix {
            f.append_ascii(suffix.char())?;
        }
        Ok(())
    }

    #[cfg(feature = "alloc")]
    pub fn append_to_string(self, s: &mut alloc::string::String) {
        let _ = self.append_to(s);
    }

    #[cfg(feature = "alloc")]
    pub fn append_ascii_to(self, buf: &mut alloc::vec::Vec<u8>) {
        let _ = self.append_to(buf);
    }
}

struct Reader<'a> {
    bytes: &'a [u8],
}

impl Reader<'_> {
    #[inline]
    fn new(bytes: &[u8]) -> Reader<'_> {
        Reader { bytes }
    }

    #[inline]
    fn remaining(&self) -> usize {
        self.bytes.len()
    }

    #[inline]
    fn peek(&self) -> Option<u8> {
        self.bytes.first().copied()
    }

    #[inline]
    fn bump(&mut self) {
        self.bytes = &self.bytes[1..];
    }

    #[inline]
    fn eat(&mut self, byte: u8) -> bool {
        if self.peek() == Some(byte) {
            self.bump();
            true
        } else {
            false
        }
    }

    #[inline]
    fn next(&mut self) -> Option<u8> {
        let byte = self.peek();
        if byte.is_some() {
            self.bump();
        }
        byte
    }

    #[inline]
    fn next_n(&mut self, n: usize) -> Option<&[u8]> {
        let (head, tail) = self.bytes.split_at_checked(n)?;
        self.bytes = tail;
        Some(head)
    }

    fn read_square(&mut self) -> Option<Square> {
        self.next_n(2)
            .and_then(|bytes| Square::from_ascii(bytes).ok())
    }

    fn read_san(&mut self) -> Option<San> {
        let role = match self.peek()? {
            b'N' => {
                self.bump();
                Role::Knight
            }
            b'B' => {
                self.bump();
                Role::Bishop
            }
            b'R' => {
                self.bump();
                Role::Rook
            }
            b'Q' => {
                self.bump();
                Role::Queen
            }
            b'K' => {
                self.bump();
                Role::King
            }
            b'O' => {
                self.bump();
                if !self.eat(b'-') || !self.eat(b'O') {
                    return None;
                }
                if !self.eat(b'-') {
                    return Some(San::Castle(CastlingSide::KingSide));
                }
                if !self.eat(b'O') {
                    return None;
                }
                return Some(San::Castle(CastlingSide::QueenSide));
            }
            b'-' => {
                self.bump();
                if self.eat(b'-') {
                    return Some(San::Null);
                } else {
                    return None;
                }
            }
            b'P' => {
                self.bump();
                Role::Pawn
            }
            _ => Role::Pawn,
        };

        Some(if self.eat(b'@') {
            San::Put {
                role,
                to: self.read_square()?,
            }
        } else {
            let file = File::from_char(char::from(self.peek()?));
            if file.is_some() {
                self.bump();
            }

            let rank = Rank::from_char(char::from(self.peek()?));
            if rank.is_some() {
                self.bump();
            }

            let (file, rank, capture, to) = if self.eat(b'x') {
                (file, rank, true, self.read_square()?)
            } else if let Some(to_file) = self.peek().and_then(|ch| File::from_char(char::from(ch)))
            {
                self.bump();
                let to_rank = Rank::from_char(char::from(self.next()?))?;
                (file, rank, false, Square::from_coords(to_file, to_rank))
            } else {
                (None, None, false, Square::from_coords(file?, rank?))
            };

            let promotion = if self.eat(b'=') {
                Some(Role::from_char(char::from(self.next()?))?)
            } else {
                None
            };

            San::Normal {
                role,
                file,
                rank,
                capture,
                to,
                promotion,
            }
        })
    }

    fn read_san_plus(&mut self) -> Option<SanPlus> {
        let san = self.read_san()?;

        let suffix = match self.peek() {
            Some(b'+') => {
                self.bump();
                Some(Suffix::Check)
            }
            Some(b'#') => {
                self.bump();
                Some(Suffix::Checkmate)
            }
            _ => None,
        };

        Some(SanPlus { san, suffix })
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

        impl serde::de::Visitor<'_> for SanPlusVisitor {
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
    use crate::{CastlingMode, Chess, fen::Fen, uci::UciMove};

    #[test]
    fn test_size() {
        assert!(mem::size_of::<San>() <= 8);
        assert!(mem::size_of::<SanPlus>() <= 8);
    }

    #[cfg(feature = "alloc")]
    #[test]
    fn test_read_write() {
        for san in [
            "a1", "a8", "h1", "h8", "e4", "b6", "e4=Q", "f1=N#", "hxg7", "bxc1", "axe4", "bxc1+",
            "bxa8=R+", "Nf3", "Ba5", "Qh8", "Kh1", "Qh1=K", "Ba5", "Bba5", "N2c4", "Red3", "Qh1=K",
            "d1=N", "@e4#", "K@b3", "Ba5", "Bba5", "Ra1a8", "--", "O-O", "O-O-O+",
        ] {
            let result = san
                .parse::<SanPlus>()
                .map_err(|_| san)
                .expect("valid san")
                .to_string();
            assert_eq!(*san, result, "read {san} write {result}");
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

            assert_eq!(San::disambiguate(m, &pos.legal_moves()), san_plus.san);
            assert_eq!(SanPlus::from_move(pos, m), san_plus);
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

    #[cfg(feature = "alloc")]
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_from_to_str() {
        use alloc::string::ToString as _;

        let check_san = |san: SanPlus| {
            assert_eq!(
                san.to_string()
                    .parse::<SanPlus>()
                    .map_err(|_| san)
                    .expect("roundtrip"),
                san
            );
        };

        // Normal
        for role in Role::ALL {
            for file in [
                None,
                Some(File::A),
                Some(File::B),
                Some(File::C),
                Some(File::D),
                Some(File::E),
                Some(File::F),
                Some(File::G),
                Some(File::H),
            ] {
                for rank in [
                    None,
                    Some(Rank::First),
                    Some(Rank::Second),
                    Some(Rank::Third),
                    Some(Rank::Fourth),
                    Some(Rank::Fifth),
                    Some(Rank::Sixth),
                    Some(Rank::Seventh),
                    Some(Rank::Eighth),
                ] {
                    for capture in [false, true] {
                        for to in Square::ALL {
                            for promotion in [
                                None,
                                Some(Role::Pawn),
                                Some(Role::Knight),
                                Some(Role::Bishop),
                                Some(Role::Rook),
                                Some(Role::Queen),
                                Some(Role::King),
                            ] {
                                for suffix in [None, Some(Suffix::Check), Some(Suffix::Checkmate)] {
                                    check_san(SanPlus {
                                        san: San::Normal {
                                            role,
                                            file,
                                            rank,
                                            capture,
                                            to,
                                            promotion,
                                        },
                                        suffix,
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }

        // Castle
        for castling_side in CastlingSide::ALL {
            for suffix in [None, Some(Suffix::Check), Some(Suffix::Checkmate)] {
                check_san(SanPlus {
                    san: San::Castle(castling_side),
                    suffix,
                });
            }
        }

        // Put
        for role in Role::ALL {
            for to in Square::ALL {
                for suffix in [None, Some(Suffix::Check), Some(Suffix::Checkmate)] {
                    check_san(SanPlus {
                        san: San::Put { role, to },
                        suffix,
                    });
                }
            }
        }

        // Null
        for suffix in [None, Some(Suffix::Check), Some(Suffix::Checkmate)] {
            check_san(SanPlus {
                san: San::Null,
                suffix,
            });
        }
    }
}
