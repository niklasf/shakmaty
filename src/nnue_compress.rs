use crate::{
    fen::Fen, Bitboard, CastlingMode, CastlingSide, Chess, Color, Piece, Position, Rank, Role,
    Square,
};
use std::string::String;
use std::vec::Vec;
#[derive(Debug, Clone, Copy)]
pub struct CompressedPosition {
    occupied: Bitboard,
    packed_state: [u8; 16],
}

impl CompressedPosition {
    // The thinking behind the encoding
    // Occupied bitboard has bits set for
    // each square with a piece on it.
    // Each packedState byte holds 2 values (nibbles).
    // First one at low bits, second one at high bits.
    // Values correspond to consecutive squares
    // in bitboard iteration order.
    // Nibble values:
    // these are the same as for Piece
    // knights, bishops, queens can just be copied
    //  0 : white pawn
    //  1 : black pawn
    //  2 : white knight
    //  3 : black knight
    //  4 : white bishop
    //  5 : black bishop
    //  6 : white rook
    //  7 : black rook
    //  8 : white queen
    //  9 : black queen
    // 10 : white king
    // 11 : black king
    //
    // these are special
    // 12 : pawn with ep square behind (white or black, depending on rank)
    // 13 : white rook with coresponding castling rights
    // 14 : black rook with coresponding castling rights
    // 15 : black king and black is side to move
    //
    // Let N be the number of bits set in occupied bitboard.
    // Only N nibbles are present. (N+1)/2 bytes are initialized.

    pub fn compress(position: &Chess) -> CompressedPosition {
        let mut cp = CompressedPosition {
            occupied: Bitboard::EMPTY,
            packed_state: [0u8; 16],
        };

        let board = position.board();
        let occupied_bitboard = board.occupied();
        cp.occupied = occupied_bitboard;

        let en_passant_squares: Vec<Square> = position
            .en_passant_moves()
            .into_iter()
            .map(|ep| ep.to())
            .collect();

        let mut nibble_values = Vec::new();

        for square in occupied_bitboard {
            let piece = board.piece_at(square).unwrap();

            let mut nibble_value = match piece {
                Piece {
                    color: Color::White,
                    role: Role::Pawn,
                } => 0,
                Piece {
                    color: Color::Black,
                    role: Role::Pawn,
                } => 1,
                Piece {
                    color: Color::White,
                    role: Role::Knight,
                } => 2,
                Piece {
                    color: Color::Black,
                    role: Role::Knight,
                } => 3,
                Piece {
                    color: Color::White,
                    role: Role::Bishop,
                } => 4,
                Piece {
                    color: Color::Black,
                    role: Role::Bishop,
                } => 5,
                Piece {
                    color: Color::White,
                    role: Role::Rook,
                } => 6,
                Piece {
                    color: Color::Black,
                    role: Role::Rook,
                } => 7,
                Piece {
                    color: Color::White,
                    role: Role::Queen,
                } => 8,
                Piece {
                    color: Color::Black,
                    role: Role::Queen,
                } => 9,
                Piece {
                    color: Color::White,
                    role: Role::King,
                } => 10,
                Piece {
                    color: Color::Black,
                    role: Role::King,
                } => 11,
            };

            //check for en passant pawn
            if piece.role == Role::Pawn
                && ((piece.color == Color::White && square.rank() == Rank::Fourth)
                    || (piece.color == Color::Black && square.rank() == Rank::Fifth))
            {
                let ep_check_square = match piece.color {
                    Color::White => Square::from_coords(square.file(), Rank::Third),
                    Color::Black => Square::from_coords(square.file(), Rank::Sixth),
                };
                if en_passant_squares.contains(&ep_check_square) {
                    nibble_value = 12; // Pawn with ep square behind
                }
            }

            // Rooks with corresponding castling rights
            // Rooks with corresponding castling rights
            if piece.role == Role::Rook {
                let castles = position.castles();
                let rook_square = square;

                let (kingside_rook, queenside_rook) = match piece.color {
                    Color::White => (
                        castles.rook(Color::White, CastlingSide::KingSide),
                        castles.rook(Color::White, CastlingSide::QueenSide),
                    ),
                    Color::Black => (
                        castles.rook(Color::Black, CastlingSide::KingSide),
                        castles.rook(Color::Black, CastlingSide::QueenSide),
                    ),
                };

                if Some(rook_square) == kingside_rook || Some(rook_square) == queenside_rook {
                    nibble_value = if piece.color == Color::White { 13 } else { 14 };
                }
            }

            // Black king and black to move
            if piece.role == Role::King
                && piece.color == Color::Black
                && position.turn() == Color::Black
            {
                nibble_value = 15;
            }

            nibble_values.push(nibble_value as u8);
        }

        // Pack nibbles into bytes
        let n = nibble_values.len();
        for i in 0..((n + 1) / 2) {
            let low_nibble = nibble_values[2 * i];
            let high_nibble = if 2 * i + 1 < n {
                nibble_values[2 * i + 1]
            } else {
                0
            };
            cp.packed_state[i] = low_nibble | (high_nibble << 4);
        }

        cp
    }

    pub fn decompress(&self) -> Chess {
        use std::collections::HashMap;
        use std::fmt::Write;

        let occupied_bitboard = self.occupied;
        let n = occupied_bitboard.count();

        // Extract nibbles from packed_state
        let mut nibble_values = Vec::with_capacity(n);
        for i in 0..((n + 1) / 2) {
            let byte = self.packed_state[i];
            let low_nibble = byte & 0x0F;
            let high_nibble = (byte >> 4) & 0x0F;
            nibble_values.push(low_nibble);
            if nibble_values.len() < n {
                nibble_values.push(high_nibble);
            }
        }

        let mut nibble_iter = nibble_values.into_iter();

        // Map squares to nibble values
        let mut square_nibbles = HashMap::new();
        for square in occupied_bitboard {
            let nibble_value = nibble_iter.next().unwrap();
            square_nibbles.insert(square, nibble_value);
        }

        let mut side_to_move = Color::White;
        let mut castling_rights = String::new();
        let mut en_passant_square = None;

        // Build the FEN string
        let mut fen = String::new();

        for rank in (0..8).rev() {
            if rank != 7 {
                fen.push('/');
            }
            let mut empty_count = 0;

            for file in 0..8 {
                let square_index = rank * 8u32 + file;
                let square = Square::new(square_index);
                if let Some(&nibble_value) = square_nibbles.get(&square) {
                    if empty_count > 0 {
                        write!(&mut fen, "{}", empty_count).unwrap();
                        empty_count = 0;
                    }

                    let (role, color) = match nibble_value {
                        0 => (Role::Pawn, Color::White),
                        1 => (Role::Pawn, Color::Black),
                        2 => (Role::Knight, Color::White),
                        3 => (Role::Knight, Color::Black),
                        4 => (Role::Bishop, Color::White),
                        5 => (Role::Bishop, Color::Black),
                        6 => (Role::Rook, Color::White),
                        7 => (Role::Rook, Color::Black),
                        8 => (Role::Queen, Color::White),
                        9 => (Role::Queen, Color::Black),
                        10 => (Role::King, Color::White),
                        11 => (Role::King, Color::Black),
                        12 => {
                            // Pawn with en passant square behind
                            let color = if rank >= 4 {
                                Color::Black
                            } else {
                                Color::White
                            };
                            let ep_square = match color {
                                Color::White => Square::from_coords(square.file(), Rank::Third),
                                Color::Black => Square::from_coords(square.file(), Rank::Sixth),
                            };
                            en_passant_square = Some(ep_square);
                            (Role::Pawn, color)
                        }
                        13 => {
                            // White rook with corresponding castling rights
                            if square == Square::A1 {
                                castling_rights.push('Q');
                            } else if square == Square::H1 {
                                castling_rights.push('K');
                            }
                            (Role::Rook, Color::White)
                        }
                        14 => {
                            // Black rook with corresponding castling rights
                            if square == Square::A8 {
                                castling_rights.push('q');
                            } else if square == Square::H8 {
                                castling_rights.push('k');
                            }
                            (Role::Rook, Color::Black)
                        }
                        15 => {
                            // Black king and black to move
                            side_to_move = Color::Black;
                            (Role::King, Color::Black)
                        }
                        _ => panic!("Invalid nibble value: {}", nibble_value),
                    };

                    let piece_char = match (role, color) {
                        (Role::Pawn, Color::White) => 'P',
                        (Role::Pawn, Color::Black) => 'p',
                        (Role::Knight, Color::White) => 'N',
                        (Role::Knight, Color::Black) => 'n',
                        (Role::Bishop, Color::White) => 'B',
                        (Role::Bishop, Color::Black) => 'b',
                        (Role::Rook, Color::White) => 'R',
                        (Role::Rook, Color::Black) => 'r',
                        (Role::Queen, Color::White) => 'Q',
                        (Role::Queen, Color::Black) => 'q',
                        (Role::King, Color::White) => 'K',
                        (Role::King, Color::Black) => 'k',
                    };

                    fen.push(piece_char);
                } else {
                    empty_count += 1;
                }
            }
            if empty_count > 0 {
                write!(&mut fen, "{}", empty_count).unwrap();
            }
        }

        // Side to move
        fen.push(' ');
        fen.push(match side_to_move {
            Color::White => 'w',
            Color::Black => 'b',
        });

        // Castling rights
        if castling_rights.is_empty() {
            castling_rights.push('-');
        }

        fen.push(' ');
        fen.push_str(&castling_rights);

        // En passant
        fen.push(' ');
        if let Some(ep_square) = en_passant_square {
            write!(fen, "{}", ep_square).unwrap();
        } else {
            fen.push('-');
        }

        // Halfmove clock and fullmove number
        fen.push_str(" 0 1");

        // Parse the FEN string
        let position = Fen::from_ascii(fen.as_bytes())
            .unwrap()
            .into_position(CastlingMode::Standard)
            .unwrap();
        position
    }
    /// Reads a `CompressedPosition` from a big-endian byte slice.
    pub fn read_from_big_endian(data: &[u8]) -> CompressedPosition {
        assert!(data.len() >= 24, "Data too short");
        let occupied = u64::from_be_bytes(data[0..8].try_into().unwrap());
        let mut packed_state = [0u8; 16];
        packed_state.copy_from_slice(&data[8..24]);
        CompressedPosition {
            occupied: Bitboard(occupied),
            packed_state,
        }
    }

    /// Writes the `CompressedPosition` to a mutable big-endian byte slice.
    pub fn write_to_big_endian(&self, data: &mut [u8]) {
        assert!(data.len() >= 24, "Data buffer too small");
        data[0..8].copy_from_slice(&self.occupied.0.to_be_bytes());
        data[8..24].copy_from_slice(&self.packed_state);
    }
}

use std::cmp::Ordering;

impl PartialEq for CompressedPosition {
    fn eq(&self, other: &Self) -> bool {
        self.occupied == other.occupied && self.packed_state == other.packed_state
    }
}

impl Eq for CompressedPosition {}

impl PartialOrd for CompressedPosition {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CompressedPosition {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.occupied.0.cmp(&other.occupied.0) {
            Ordering::Equal => self.packed_state.cmp(&other.packed_state),
            other => other,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{fen::Fen, Chess};

    #[test]
    fn test_compress_decompress_startpos() {
        let startpos = Chess::default();
        let cp = CompressedPosition::compress(&startpos);
        let decompressed = cp.decompress();
        assert_eq!(startpos, decompressed);
    }

    #[test]
    fn test_compress_decompress_with_en_passant() {
        let fen = " rnbqkbnr/ppp1ppp1/7p/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3";
        let position = Fen::from_ascii(fen.as_bytes())
            .unwrap()
            .into_position(CastlingMode::Standard)
            .unwrap();
        let cp = CompressedPosition::compress(&position);
        let decompressed = cp.decompress();
        assert_eq!(position, decompressed);
    }

    #[test]
    fn test_compress_decompress_with_castling_rights() {
        let fen = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1";
        let position = Fen::from_ascii(fen.as_bytes())
            .unwrap()
            .into_position(CastlingMode::Standard)
            .unwrap();
        let cp = CompressedPosition::compress(&position);
        let decompressed = cp.decompress();
        assert_eq!(position, decompressed);
    }

    #[test]
    fn test_compress_decompress_black_to_move() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1";
        let position = Fen::from_ascii(fen.as_bytes())
            .unwrap()
            .into_position(CastlingMode::Standard)
            .unwrap();
        let cp = CompressedPosition::compress(&position);
        let decompressed = cp.decompress();
        assert_eq!(position, decompressed);
    }

    #[test]
    fn test_read_write_big_endian() {
        let startpos = Chess::default();
        let cp = CompressedPosition::compress(&startpos);

        let mut data = [0u8; 24];
        cp.write_to_big_endian(&mut data);
        let cp_read = CompressedPosition::read_from_big_endian(&data);

        assert_eq!(cp, cp_read);
    }
}
