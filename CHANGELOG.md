# Changelog for shakmaty

## v0.26.0

- Change semantics of `PartialEq` for `Chess` ❗ (FIDE rules for repetitions instead of structural equality)
- Implement `Hash`, `PartialEq`, and `Eq` for all variant positions.
- Replace `build.rs` with const evaluation. Bump MSRV to 1.70.

## v0.25.0

- Remove `step` feature.
- Fix inconsistent `Hash` for `Chess` due to irrelevant en passant squares.
- Relax and more precisely document `CastlingMode::Chess960`.
- Rename `PositionErrorKinds::IMPOSSIBLE_MATERIAL` to `TOO_MUCH_MATERIAL`.
- Implement `BitXorAssign` for `Color`.
- Added `Bitboard::{isolate_first,isolate_last,discard_last,without_last}()`.

## v0.24.0

- Update `PositionErrorKinds` to `bitflags` 2.
- Remove previously deprecated methods.
- Bump minimum supported Rust version to 1.63.

## v0.23.1

- Significantly optimize performance of Zobrist hashing.
- Add `Variant::from_ascii()`.
- Implement `Default` for `VariantPosition`.
- Use `#[track_caller]` for panicking functions.

## v0.23.0

- Add optional `nohash-hasher` feature.
- Implement `Display` and `FromStr` for `Variant`.
- `Variant::from_uci()` now returns `Result` instead of `Option`.
- Make `Variant::distinguishes_promoted()` `const`.
- Introduce `Zobrist{8,16,32,64,128}` types for Zobrist hash values.
- Consider `EnPassantMode` for Zobrist hashing.
- Removed API for incremental Zobrist hashing which was never implemented.

## v0.22.0

- Let `Board::board_fen()` return struct that implements `Display` instead of
  `String`.
- Introduce `alloc` and `std` features, which can be disabled for `#![no_std]`
  support.
- Make many methods `const`.

## v0.21.4

- Add `Board::from_bitboards()` and `Board::into_bitboards()`.

## v0.21.3

- Optimize `Position::outcome()`.
- Detect Antichess insufficient material with only two knights.
- Fix off-backrank king influencing invalid castling notation in FEN.

## v0.21.2

- Fix castling rights without matching rooks failing FEN/EPD parser roundtrip.
- Fix pawn moves like `P6h8` failing SAN parser roundtrip.
- More precisely document FEN/EPD parser and writer.
- More strictly validate pawns on backrank in `Horde`.
- More strictly validate impossible material in `RacingKings`.
- Add `{Rank,File}::distance()`.
- Implement `Copy` for `ByRole` and `ByColor` when possible.
- Constify most `Bitboard` functions.

## v0.21.1

- Rename `Castles::discard_side()` to `discard_color()` and
  `Castles::has_side()` to `has_color()`.
- Add `Bitboard::without_first()`.
- Add `Bitboard::shift()`.
- Add `{Fen,Epd}::from_setup()` and `{Fen,Epd}::as_setup()`.
- Implement `Clone` for `PositionError` when possible.

## v0.21.0

- Refactor `Setup`: It is now a struct instead of a trait.
- Introduce `EnPassantMode` argument for `Position::ep_square()`.
- Refactor `fen` module: It is now based on `Fen` and `Epd` wrappers for
  `Setup` that implement `Display` and `FromStr`. More clearly describe
  the parser. Remove non-standard formatting options.
- Tweak `ByColor` and make it iterable.
- Introduce `ByRole`.
- Implement `IntoIterator` for `Board`, replacing `Board::pieces()`.
- Remove `Material` and `MaterialSide`. Use `ByColor<ByRole<u8>>` instead.
- Ensure `RemainingChecks` value is between 0 and 3.
- Fix integer overflow for invalid FEN that has too many ranks.
- Remove all deprecated methods.

## v0.20.7

- Fix move generation and validation for invalid positions where check is
  incompatible with the en passant square.
- Add `Square::xor()`.

## v0.20.6

- Fix parsing of empty string as `Fen`.
- Fix `PositionErrorKings::MISSING_KING` given for too many kings.
- Fix minimum required `bitflags` version (`-Z minimal-versions`).
- Tighten `PositionErrorKinds::IMPOSSIBLE_MATERIAL`.

## v0.20.5

- Accept duplicate spaces (and underscores) between FEN fields.
- Let FEN parser record and accept syntactically correct impossible castling
  rights. They will instead be rejected when setting up the position.
- Let FEN parser reject more than two castling rights per side.

## v0.20.4

- Accept underscores in FENs.
- Significantly optimize pawn capture generation (+10% perft speed).
- Addded `Square::offset_unchecked()`.
- Deprecated `Bitboard::rank()`. Use `Bitboard::from_rank()`.
- Deprecated `Bitboard::file()`. Use `Bitboard::from_file()`.
- Deprecated `Bitboard::relative_rank()`. Use `Color::relative_rank()`.

## v0.20.3

- Implement `FromStr` for `Outcome`.
- Add geometric transformation methods to `Board`.
- Introduce `Color::fold_wb()` in favor of `Color::fold()`.

## v0.20.2

- Minimum supported Rust version is now 1.56.
- Fixed `Atomic` position validation, where the remaining king is attacked, but
  the other king has exploded.
- Fixed `Material::from_ascii()`. Now requires `v` separator, and rejects
  inputs with more than 64 pieces in total.

## v0.20.1

- Fixed signature of `ByColor::new_with()`. Technically semver breaking.
- Deprecated `Bitboard::ALL`. Use `Bitboard::FULL` instead.
- Improved `Display` of `PositionError`.
- Removed implementations of deprecated `Error::description()`.
- Added `PositionError::ignore_impossible_check()`.
- Added `{Color,Role,CastlingSide,Rank,File,Square}::ALL`.
- Added `Outcome::from_winner()`.
- Added `ByColor::as_mut()`.
- Added `ByColor::zip()` and `ByColor::zip_color()`.
- Added `File::upper_char()`.
- Implement `IntoIterator` for `ByColor`.
- Implement `Display` and `FromStr` for `Color`.
- Added `Zobrist::as_inner()`.

## v0.20.0

- Removed tracking of promoted pieces from `Board` and added
  `Setup::promoted()` instead. This is used only by Crazyhouse. All other
  variants will return `Bitboard(0)`. Removed `FenOpts::promoted()`
  accordingly. Moved `fen::board_fen()` as a method to `Board`.
- Implemented Zobrist hashing.
- Fixed (or rather implemented) insufficient material detection for `Horde`.
- Added new feature `step` that implements the nightly `Step` trait for
  `File`, `Rank` and `Square`.
- Let `Bitboard` respect `Display` formatting options.
- Fixed return type of `File::offset()`.
- Implemented `Eq` and `Hash` for `Chess` and `Pieces`.

## v0.19.0

- Update `arrayvec` to 0.7.x. `MoveList` is now using const generics.
- Derive `Hash` for `Move`, `Outcome`, and `Fen`.
- Eliminate unsafe usage where no longer required for performance. Justify
  remaining cases.

## v0.18.0
- Remove `f32`, `f64`, `u128`, and `i128` conversions.
- Use `std::num::TryFromIntError`.
- Functions now return `MoveList` instead of using it as an out parameter.
  Return value optimization by hand is no longer required.
- Introduce `ByColor`, used as `Material` and `ByColor<RemainingChecks>`.
- Introduce `PlayError`.
- Renamed `variants` module to `variant`. Now gated behind a non-default
  feature `variant`.

## v0.17.2

- Parse `Uci::to_move()` where king captures unmoved rook.

## v0.17.1

- Also reject checker aligned with king and en passant square with
  `PositionErrorKinds::IMPOSSIBLE_CHECK`.

## v0.17.0

- Introduce `CastlingMode`, now required for
  `FromSetup::from_setup(..., mode)`, the `VariantPosition` analogon,
  `Fen::position(mode)`, and exposed by `Castles::mode()`.
- `Uci::from_move()` now takes the mode as context instead of the position.
  Give `pos.castles().mode()` for the old behavior.
- Add `Uci::from_standard()`.
- Rework `PositionError`. The original error kinds are available as
  `PositionError::kinds()`. There are now methods to safely ignore particular
  errors.
- Reject setups with impossible (i.e., too much) material with the new
  `PositionErrorKinds::IMPOSSIBLE_MATERIAL`. Can be ignored using
  `PositionError::ignore_impossible_material()` for the previous behavior.
- Reject setups with multiple aligned sliding checkers with the new
  `PositionErrorKinds::IMPOSSIBLE_CHECK`.
- Rename `BAD_CASTLING_RIGHTS` to `INVALID_CASTLING_RIGHTS`.
- Remove `IllegalMoveError`. In the context of UCI validation, replace with
  `IllegalUciError`. `Position::play()` instead returns the original position
  as the error.
- Change `fullmoves` from `u32` to `NonZeroU32` everywhere.
- Remove public `Castles::from_setup()`.
- Remove `Square::with_rank_of()`.
- Remove `Giveaway` in favor of `Antichess` (where players start without
  castling rights).
- Fix `swap_turn()` if en passant square exists. Would always fail, now
  discards the en passant square.
- Add `Move::to_uci()` convenience method.
- Add `CastlingSide::{king,rook}_to_file()`.
- Add `Variant::distinguishes_promoted()`.
- Add `Variant::uci()` and `Variant::from_uci()`.
- Future proof error types (remove some implemented traits and available
  constructors).

## v0.16.4

- Fix insufficient material detection with same-color bishops on both sides.
- Document limitations of `Position::is_irreversible()`. Moves that cede
  en passant are now considered irreversible.

## v0.16.3

- Implement `From<Role>` for nonzero integer types.
- Performance: Remove internal `BB_BETWEEN` table and compute it from rays at
  runtime, to improve cache efficiency.
- Support Miri.

## v0.16.2

- Fix Racing Kings game end detection: Black cannot catch up if their own
  pieces block the goal.
- Pawn drops in Crazyhouse are now considered zeroing.

## v0.16.1

- Fix (impossible) Crazyhouse insufficient material.
- Fix (impossible) castling rights of exploded king in Atomic chess.

## v0.16.0

- Update `arrayvec` to 0.5.x, which comes with significant performance
  improvements.
- The default `Giveaway` position now has castling rights.

## v0.15.3

- Follow FICS rules in Atomic castling edge cases.
- Use `#[repr(align)] enum` and `reverse_bits()` stabilized in Rust 1.37.

## v0.15.2

- In Horde chess, allow double pawn moves from the first rank.
- Added `{Square,Bitboard}::{flip_anti_diagonal,rotate_90,rotate_180,rotate_270}()`.

## v0.15.1

- FEN parser was expecting `~` before promoted pieces, but it should be
  after.

## v0.15.0

- Moved `Position::from_setup()` to a seperate new trait `FromSetup`.
- Square and file/rank index calculations are now performed with `u32` and
  `i32`, which is more performant than using the narrower types `u8` and
  `i8`. The types are still `#[repr(u8)]`.
- Renamed `Square::combine()` to `Square::with_rank_of()`.
- Added `Fen::from_setup()`.
- Added `FenOpts::scid()` for Scid/Lichess compatible FEN formatting.
  Of course the parser also accepts these formats.
- Added `variant::Variant` and `VariantPosition` for runtime selected
  chess variants. Limited to the built in variants.
- Added `{Material,MaterialSide}::is_empty()`.

## v0.14.1

- `TryFrom` now available on stable.

## v0.14.0

- `SanPlus::from_move()` no longer requires the move to be legal, which
  was an undocumented requirement. The new
  `SanPlus::from_move_and_play_unchecked()` is closest to the previous
  behavior.
- Added `San::disambiguate()` and `SanSuffix::from_position()`.
- Implement `TryFrom` for various types on nightly.
- Implement `Add`, `AddAssign`, `Sub`, `SubAssign` for `Material` and
  `MaterialSide`.
- Added `CastlingSide::from_{king|queen}_side()`.
- Use `u32` for `depth` argument of `perft()`.

## v0.13.1

- Performance improvements on nightly (aligned `Move` enum).

## v0.13.0

- Replaced `SanPlus::check` and `SanPlus::checkmate` with `san::Suffix` enum.
- Renamed `{Rank,File}::rotate()` to `flip_diagonal()`.
- Renamed `Bitboard::flip()` to `toggle()`.
- Added `Square::coords()`.
- Added `Bitboard::flip_{vertical,horizontal,diagonal}()`.
- Added `Position::promotion_moves()`.
- Derive `Hash` on `Board`, `FenOpts`, `San`, `SanPlus`, `Suffix`, `Color`,
  `Role`, `Piece`, `RemainingChecks`, `CastlingSide`, and `Uci`.
- Minimum Rust version 1.31.0.

## v0.12.0

- `Setup::halfmove_clock()` is now `Setup::halfmoves()`.
- New conversions: `f32::from(file)`, `f64::from(rank)`.
- Replace `Pockets` and `Pocket` by more general `Material` and
  `MaterialSide`. Note that their `Display` and `FromStr` implementations
  differ.
- Unify naming of error types.

## v0.11.2

- Fix Atomic insufficient material: KQN can mate.

## v0.11.1

- Fix Chess960 should not allow a-side castling over a blocking h-side rook.

## v0.11.0

- `Outcome` is now `Copy`.
- `Position::castling_uncovers_rank_attack()` is no longer public.

## v0.10.0

- Add `File`, `Rank`. These are now returned by `Square::{file, rank}`.
  `Square::from_coords(file, rank)` takes the new types and is no longer
  fallible.
- `Bitboard` is now `IntoIterator` instead of `Iterator` itself.
  Methods `Bitboard::{first, last, count, is_empty}` remain.
- `Bitboard::{add, flip, discard, with, without}` are now generic over
  `Into<Bitboard>`. Alternative methods `*_all` have been removed.
- `Bitboard::relative_shift(color, shift: u8)` now takes `u32`.
- `shakmaty::CarryRippler` is now `shakmaty::bitboard::CarryRippler`.
- Add new methods:
  `Bitboard::{pop_front, pop_back, is_disjoint, is_subset, is_superset}`.
- Add `Position::has_insufficient_material(color)`.

## v0.9.0

- Remove `uci`, `chess960_uci`, `san` and `san_plus` that were deprecated
  in 0.8.1.
- Rename `*::from_bytes()` to `*::from_ascii()`.
- Take small copy types by value: `Piece::char()`, `CastlingSide::*`.
- Add `Castles::has(color, side)`.
- `fen::{board_fen, epd, fen}` are now shortcuts for formatting with
  default options.
- Minimum rust version 1.27.0 (i128 conversions again, fused iterators again,
  `Option::filter()`, `dyn Trait`).
- Eliminate many uses of unsafe, including `TrustedLen` impls, at minor cost.

## v0.8.1

- Make `Role` discriminants public.
- Put `San`, `SanPlus` and `Uci` constructors on `San`, `SanPlus` and `Uci`
  respectively.

## v0.8.0

- Revert fused iterators and u128. Minimum Rust version back to 1.23.0.
- Change `Role` discriminants. Now starting with `Pawn = 1`.
- Performance improvements (~12% faster perft).
- Switch benchmarks to `criterion.rs`.

## v0.7.2

- Add `Outcome.winner()`.

## v0.7.1

- Minimum Rust version 1.26.0 (fused iterators, u128 conversions).

## v0.7.0

- Reintroduce the remaining Lichess variants: `Crazyhouse`, `KingOfTheHill`,
  `ThreeCheck`, `Horde`, `RacingKings`.
- Expose `Position.castles()` and replace `Position.is_chess960()` with
  `Position.castles().is_chess960()`.
- Fix insufficient material: KNvK was not given as drawn due to a typo.
- Fix insufficient material in `Atomic`: Two knights of different colors
  are not safe.
- Let `Pockets.count()` and `Pocket.count()` return `usize`.

## v0.6.7

- Giveaway starting position should have no castling rights.

## v0.6.6

- Fix missing king promotions in Giveaway.

## v0.6.5

- Validate color of missing king in atomic chess.
- Clear move buffers before generating variant moves.

## v0.6.4

- Reintroduce `Giveaway` and `Atomic` chess variants.

## v0.6.3

- New method `Move.is_zeroing()`.

## v0.6.2

- Make unit error types (`InvalidSquareName`, `InvalidUci`, `InvalidSan`)
  completely public.
- Documentation, coding style and debugging tweaks.

## v0.6.1

- Expose `attacks::bishop_mask` and `attacks::rook_mask`.
- Eliminate almost all unchecked indexing.

## v0.6.0

- Split `impl From<Move> for Uci` into `uci()` and `chess960_uci()`.
- Fix display of pawn drops.
- Move generating methods clear the move buffer (and therefor no longer panic on too full buffers).
- Added `Position.is_chess960()`, `Bitboard.without_all()`, `Role.upper_char()`, `Board.stepper()`.

## v0.5.1

- Fix `Uci::to_move()` for en passant moves. Thanks zxqfl.

## v0.5.0

- Use `u64` instead of `usize` for `perft()`.
- Export error type `InvalidSquareName`.
- New methods: `CastlingSide.is_{queen|king}_side()`, `San.matches()`, `Move.is_capture()`, `Move.is_promotion()`, `Move.castling_side()`, `Position.is_check()`.
- Derive `Ord` and `PartialOrd` for `Role`.
- Support running benchmarks on stable.

## v0.4.2

- Fix build error on beta due to the new nightly `option_filter` feature.
- Fix unterminated code block in documentation.

## v0.4.1

- Fix build error due to the new nightly
  [`option_filter`](https://github.com/rust-lang/rust/issues/45860) feature.

## v0.4.0

- Rename `Color::from_bool()` to `Color::from_white()`,
  add `Color::from_black()`.
- Add `Move::role()`, `Move::is_en_passant()` and `Move::is_castle()`.
- Add `Position::en_passant_moves()` and `Position::capture_moves()`.
- Implement `BitXor<bool>` for `Color`.
- Implement `FusedIterator` and `TrustedLen` on `Bitboard`.

## v0.3.0

- Switch to `#[repr(i8)]` for `Square`. Implement all lossless integer
  conversions `From<Square>`.
- Add `Square::flip_horizontal()`, `flip_vertical()` and `flip_diagonal()`.
- Efficiently implement `CarryRippler::last()` by @nvzqz.
- Eliminate some unchecked indexing by @nvzqz.
- Faster ASCII case conversions and tests by @nvzqz.

## v0.2.0

- `Square` is now a `#[repr(u8)]` enum.
- Use `bitflags` for `PositionError`.
- Rename `RemainingChecks::subtract()` to `decrement()`.
- Add `Position::swap_turn()`.

## v0.1.0

- First release with support for stable Rust.
