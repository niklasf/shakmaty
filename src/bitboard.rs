//! Sets of squares.

use core::{
    fmt,
    fmt::Write,
    iter::{FromIterator, FusedIterator},
    ops,
};

use crate::square::{File, Rank, Square};

/// A set of [squares](super::Square) represented by a 64 bit
/// integer mask.
///
/// # Examples
///
/// ```
/// use shakmaty::Bitboard;
///
/// let bitboard = Bitboard(0x1e22_2212_0e0a_1222);
/// // . 1 1 1 1 . . .
/// // . 1 . . . 1 . .
/// // . 1 . . . 1 . .
/// // . 1 . . 1 . . .
/// // . 1 1 1 . . . .
/// // . 1 . 1 . . . .
/// // . 1 . . 1 . . .
/// // . 1 . . . 1 . .
/// ```
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct Bitboard(pub u64);

impl Bitboard {
    /// A bitboard with a single square.
    #[inline]
    pub const fn from_square(sq: Square) -> Bitboard {
        Bitboard(1 << sq as usize)
    }

    /// Returns the bitboard containing all squares of the given rank.
    #[inline]
    pub const fn from_rank(rank: Rank) -> Bitboard {
        Bitboard(RANKS[rank as usize])
    }

    /// Returns the bitboard containing all squares of the given file.
    #[inline]
    pub const fn from_file(file: File) -> Bitboard {
        Bitboard(FILE_A << file as usize)
    }

    /// Silently overflowing bitwise shift with a signed offset, `<<` for
    /// positive values and `>>` for negative values.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// let bitboard = Bitboard(0x1e22_2212_0e0a_1222);
    /// assert_eq!(bitboard.shift(-8), Bitboard(0x001e_2222_120e_0a12));
    /// // . . . . . . . .
    /// // . 1 1 1 1 . . .
    /// // . 1 . . . 1 . .
    /// // . 1 . . . 1 . .
    /// // . 1 . . 1 . . .
    /// // . 1 1 1 . . . .
    /// // . 1 . 1 . . . .
    /// // . 1 . . 1 . . .
    ///
    /// assert_eq!(bitboard.shift(64), Bitboard(0));
    /// assert_eq!(bitboard.shift(i32::MIN), Bitboard(0));
    /// ```
    #[must_use]
    #[inline]
    pub const fn shift(self, offset: i32) -> Bitboard {
        Bitboard(if offset > 63 {
            0
        } else if offset >= 0 {
            self.0 << offset
        } else if offset >= -63 {
            self.0 >> -offset
        } else {
            0
        })
    }

    /// Tests if `self` is non-empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// assert!(Bitboard::BACKRANKS.any());
    /// ```
    #[must_use]
    #[inline]
    pub const fn any(self) -> bool {
        self.0 != 0
    }

    /// Tests if `self` is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// assert!(Bitboard::EMPTY.is_empty());
    /// ```
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    /// Tests if `self` contains the given square.
    #[inline]
    pub const fn contains(self, sq: Square) -> bool {
        self.intersect(Bitboard::from_square(sq)).any()
    }

    /// Adds `squares`.
    #[inline]
    pub fn add<T: Into<Bitboard>>(&mut self, squares: T) {
        *self |= squares;
    }

    /// Toggles `squares`.
    #[inline]
    pub fn toggle<T: Into<Bitboard>>(&mut self, squares: T) {
        *self ^= squares;
    }

    /// Discards `squares`.
    #[inline]
    pub fn discard<T: Into<Bitboard>>(&mut self, squares: T) {
        *self &= !squares.into();
    }

    /// Removes a square from the bitboard.
    ///
    /// Returns `true` if the square was in the set. Use
    /// [`Bitboard::discard()`] if you do not care about the return value.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Bitboard, Square};
    ///
    /// let mut bitboard = Bitboard::FULL;
    /// assert_eq!(bitboard.remove(Square::E4), true);
    /// assert_eq!(bitboard.remove(Square::E4), false);
    /// ```
    #[must_use = "use Bitboard::discard() if return value is not needed"]
    #[inline]
    pub fn remove(&mut self, sq: Square) -> bool {
        if self.contains(sq) {
            self.toggle(sq);
            true
        } else {
            false
        }
    }

    /// Conditionally adds or discards `square`.
    #[inline]
    pub fn set(&mut self, square: Square, v: bool) {
        if v {
            self.add(square);
        } else {
            self.discard(square);
        }
    }

    /// Clears all squares.
    #[inline]
    pub fn clear(&mut self) {
        self.0 = 0;
    }

    /// Returns the intersection of `self` and `squares`. Equivalent to bitwise `&`.
    #[must_use]
    pub const fn intersect(self, squares: Bitboard) -> Bitboard {
        Bitboard(self.0 & squares.0)
    }

    /// Returns the union of `self` and `squares`. Equivalent to bitwise `|`.
    #[doc(alias = "union")]
    #[must_use]
    #[inline]
    pub fn with<T: Into<Bitboard>>(self, squares: T) -> Bitboard {
        self.with_const(squares.into())
    }

    /// Same as the `with` method, but usable in `const` contexts.
    #[must_use]
    pub const fn with_const(self, squares: Bitboard) -> Bitboard {
        Bitboard(self.0 | squares.0)
    }

    /// Returns `self` without `squares` (set difference).
    #[doc(alias = "difference")]
    #[must_use]
    #[inline]
    pub fn without<T: Into<Bitboard>>(self, squares: T) -> Bitboard {
        self.without_const(squares.into())
    }

    /// Same as the `without` method, but usable in `const` contexts.
    #[must_use]
    pub const fn without_const(self, squares: Bitboard) -> Bitboard {
        Bitboard(self.0 & !squares.0)
    }

    /// Returns all squares that are in `self` or `squares` but not in both
    /// (symmetric set difference). Equivalent to bitwise `^`.
    #[doc(alias = "symmetric_difference")]
    #[must_use]
    #[inline]
    pub fn toggled<T: Into<Bitboard>>(self, squares: T) -> Bitboard {
        self.toggled_const(squares.into())
    }

    /// Same as the `toggled` method, but usable in `const` contexts.
    #[must_use]
    pub const fn toggled_const(self, squares: Bitboard) -> Bitboard {
        Bitboard(self.0 ^ squares.0)
    }

    /// Tests if `self` and `other` are disjoint.
    #[inline]
    pub fn is_disjoint<T: Into<Bitboard>>(self, other: T) -> bool {
        self.is_disjoint_const(other.into())
    }

    /// Same as the `is_disjoint` method, but usable in `const` contexts.
    pub const fn is_disjoint_const(self, other: Bitboard) -> bool {
        Bitboard(self.0 & other.0).is_empty()
    }

    /// Tests if `self` is a subset of `other`.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// assert!(Bitboard::DARK_SQUARES.is_subset(Bitboard::FULL));
    /// ```
    #[inline]
    pub fn is_subset<T: Into<Bitboard>>(self, other: T) -> bool {
        self.is_subset_const(other.into())
    }

    /// Same as the `is_subset` method, but usable in `const` contexts.
    pub const fn is_subset_const(self, other: Bitboard) -> bool {
        self.without_const(other).is_empty()
    }

    /// Tests if `self` is a superset of `other`.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// assert!(Bitboard::FULL.is_superset(Bitboard::LIGHT_SQUARES));
    /// ```
    #[inline]
    pub fn is_superset<T: Into<Bitboard>>(self, other: T) -> bool {
        self.is_superset_const(other.into())
    }

    /// Same as the `is_superset` method, but usable in `const` contexts.
    pub const fn is_superset_const(self, other: Bitboard) -> bool {
        other.is_subset_const(self)
    }

    /// Removes and returns the first square, if any.
    #[must_use = "use Bitboard::discard_first() if return value is not needed"]
    #[inline]
    pub fn pop_front(&mut self) -> Option<Square> {
        let square = self.first();
        self.discard_first();
        square
    }

    /// Returns the first square, if any.
    #[inline]
    pub const fn first(self) -> Option<Square> {
        if self.is_empty() {
            None
        } else {
            Some(Square::new(self.0.trailing_zeros()))
        }
    }

    /// Discards the first square.
    #[inline]
    pub fn discard_first(&mut self) {
        *self = self.without_first();
    }

    /// Returns `self` without the first square.
    #[must_use]
    #[inline]
    pub const fn without_first(self) -> Bitboard {
        let Bitboard(mask) = self;
        Bitboard(mask & mask.wrapping_sub(1))
    }

    /// Returns the bitboard with only the first square of `self`.
    #[must_use]
    #[inline]
    pub const fn isolate_first(self) -> Bitboard {
        let Bitboard(mask) = self;
        Bitboard(mask & mask.wrapping_neg())
    }

    /// Removes and returns the last square, if any.
    #[inline]
    pub fn pop_back(&mut self) -> Option<Square> {
        let square = self.last();
        *self ^= Bitboard::from_iter(square);
        square
    }

    /// Returns the last square.
    #[inline]
    pub const fn last(self) -> Option<Square> {
        if self.is_empty() {
            None
        } else {
            Some(Square::new(63 - self.0.leading_zeros()))
        }
    }

    /// Discards the last square.
    #[inline]
    pub fn discard_last(&mut self) {
        *self = self.without_last()
    }

    /// Returns `self` without the last square.
    #[must_use]
    #[inline]
    pub fn without_last(self) -> Bitboard {
        self ^ Bitboard::from_iter(self.last())
    }

    /// Returns the bitboard with only the last square of `self`.
    #[must_use]
    #[inline]
    pub fn isolate_last(self) -> Bitboard {
        Bitboard::from_iter(self.last())
    }

    /// Returns the number of squares in `self`.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// assert_eq!(Bitboard::CORNERS.count(), 4);
    /// ```
    #[doc(alias = "len")]
    #[inline]
    pub const fn count(self) -> usize {
        self.0.count_ones() as usize
    }

    /// Tests if there is more than one square in `self`.
    #[inline]
    pub const fn more_than_one(self) -> bool {
        self.without_first().any()
    }

    /// Gets the only square in the set, if there is exactly one.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Bitboard, Square, Rank};
    ///
    /// assert_eq!(Bitboard::from(Square::H5).single_square(), Some(Square::H5));
    /// assert_eq!(Bitboard::from(Rank::First).single_square(), None);
    /// ```
    #[inline]
    pub const fn single_square(self) -> Option<Square> {
        if self.more_than_one() {
            None
        } else {
            self.first()
        }
    }

    /// An iterator over the subsets of this bitboard.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// for subset in Bitboard::CENTER.carry_rippler() {
    ///     assert!(subset.is_subset(Bitboard::CENTER));
    /// }
    /// ```
    #[inline]
    pub const fn carry_rippler(self) -> CarryRippler {
        CarryRippler {
            bb: self.0,
            subset: 0,
            first: true,
        }
    }

    /// Mirror the bitboard vertically.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// let bitboard = Bitboard(0x1e22_2212_0e0a_1222);
    /// assert_eq!(bitboard.flip_vertical(), Bitboard(0x2212_0a0e_1222_221e));
    /// // . 1 . . . 1 . .
    /// // . 1 . . 1 . . .
    /// // . 1 . 1 . . . .
    /// // . 1 1 1 . . . .
    /// // . 1 . . 1 . . .
    /// // . 1 . . . 1 . .
    /// // . 1 . . . 1 . .
    /// // . 1 1 1 1 . . .
    /// ```
    #[must_use]
    #[inline]
    pub const fn flip_vertical(self) -> Bitboard {
        Bitboard(self.0.swap_bytes())
    }

    /// Mirror the bitboard horizontally.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// let bitboard = Bitboard(0x1e22_2212_0e0a_1222);
    /// assert_eq!(bitboard.flip_horizontal(), Bitboard(0x7844_4448_7050_4844));
    /// // . . . 1 1 1 1 .
    /// // . . 1 . . . 1 .
    /// // . . 1 . . . 1 .
    /// // . . . 1 . . 1 .
    /// // . . . . 1 1 1 .
    /// // . . . . 1 . 1 .
    /// // . . . 1 . . 1 .
    /// // . . 1 . . . 1 .
    /// ```
    #[must_use]
    pub const fn flip_horizontal(self) -> Bitboard {
        // https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating#Horizontal
        let k1 = 0x5555_5555_5555_5555;
        let k2 = 0x3333_3333_3333_3333;
        let k4 = 0x0f0f_0f0f_0f0f_0f0f;
        let x = self.0;
        let x = ((x >> 1) & k1) | ((x & k1) << 1);
        let x = ((x >> 2) & k2) | ((x & k2) << 2);
        let x = ((x >> 4) & k4) | ((x & k4) << 4);
        Bitboard(x)
    }

    /// Mirror the bitboard at the a1-h8 diagonal.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// let bitboard = Bitboard(0x1e22_2212_0e0a_1222);
    /// assert_eq!(bitboard.flip_diagonal(), Bitboard(0x0000_6192_8c88_ff00));
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // 1 . . . . 1 1 .
    /// // . 1 . . 1 . . 1
    /// // . . 1 1 . . . 1
    /// // . . . 1 . . . 1
    /// // 1 1 1 1 1 1 1 1
    /// // . . . . . . . .
    /// ```
    #[must_use]
    pub const fn flip_diagonal(self) -> Bitboard {
        // https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating#Diagonal
        let k1 = 0x5500_5500_5500_5500;
        let k2 = 0x3333_0000_3333_0000;
        let k4 = 0x0f0f_0f0f_0000_0000;
        let mut x = self.0;
        let t = k4 & (x ^ (x << 28));
        x ^= t ^ (t >> 28);
        let t = k2 & (x ^ (x << 14));
        x ^= t ^ (t >> 14);
        let t = k1 & (x ^ (x << 7));
        x ^= t ^ (t >> 7);
        Bitboard(x)
    }

    /// Mirror the bitboard at the h1-a8 diagonal.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// let bitboard = Bitboard(0x1e22_2212_0e0a_1222);
    /// assert_eq!(bitboard.flip_anti_diagonal(), Bitboard(0x00ff_1131_4986_0000));
    /// // . . . . . . . .
    /// // 1 1 1 1 1 1 1 1
    /// // 1 . . . 1 . . .
    /// // 1 . . . 1 1 . .
    /// // 1 . . 1 . . 1 .
    /// // . 1 1 . . . . 1
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// ```
    #[must_use]
    pub const fn flip_anti_diagonal(self) -> Bitboard {
        // https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating#Anti-Diagonal
        let k1 = 0xaa00_aa00_aa00_aa00;
        let k2 = 0xcccc_0000_cccc_0000;
        let k4 = 0xf0f0_f0f0_0f0f_0f0f;
        let mut x = self.0;
        let t = x ^ (x << 36);
        x ^= k4 & (t ^ (x >> 36));
        let t = k2 & (x ^ (x << 18));
        x ^= t ^ (t >> 18);
        let t = k1 & (x ^ (x << 9));
        x ^= t ^ (t >> 9);
        Bitboard(x)
    }

    /// Rotate the bitboard 90 degrees clockwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// let bitboard = Bitboard(0x1e22_2212_0e0a_1222);
    /// assert_eq!(bitboard.rotate_90(), Bitboard(0x00ff_888c_9261_0000));
    /// // . . . . . . . .
    /// // 1 1 1 1 1 1 1 1
    /// // . . . 1 . . . 1
    /// // . . 1 1 . . . 1
    /// // . 1 . . 1 . . 1
    /// // 1 . . . . 1 1 .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// ```
    #[must_use]
    pub const fn rotate_90(self) -> Bitboard {
        self.flip_diagonal().flip_vertical()
    }

    /// Rotate the bitboard 180 degrees.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// let bitboard = Bitboard(0x1e22_2212_0e0a_1222);
    /// assert_eq!(bitboard.rotate_180(), Bitboard(0x4448_5070_4844_4478));
    /// // . . 1 . . . 1 .
    /// // . . . 1 . . 1 .
    /// // . . . . 1 . 1 .
    /// // . . . . 1 1 1 .
    /// // . . . 1 . . 1 .
    /// // . . 1 . . . 1 .
    /// // . . 1 . . . 1 .
    /// // . . . 1 1 1 1 .
    /// ```
    #[must_use]
    #[inline]
    pub const fn rotate_180(self) -> Bitboard {
        Bitboard(self.0.reverse_bits())
    }

    /// Rotate the bitboard 270 degrees clockwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// let bitboard = Bitboard(0x1e22_2212_0e0a_1222);
    /// assert_eq!(bitboard.rotate_270(), Bitboard(0x0000_8649_3111_ff00));
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . 1 1 . . . . 1
    /// // 1 . . 1 . . 1 .
    /// // 1 . . . 1 1 . .
    /// // 1 . . . 1 . . .
    /// // 1 1 1 1 1 1 1 1
    /// // . . . . . . . .
    /// ```
    #[must_use]
    pub const fn rotate_270(self) -> Bitboard {
        self.flip_vertical().flip_diagonal()
    }

    /// An empty bitboard.
    ///
    /// ```
    /// # use shakmaty::Bitboard;
    /// #
    /// # let bitboard = Bitboard::EMPTY;
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// ```
    pub const EMPTY: Bitboard = Bitboard(0);

    /// A bitboard containing all squares.
    ///
    /// ```
    /// # use shakmaty::Bitboard;
    /// #
    /// # let bitboard = Bitboard::FULL;
    /// // 1 1 1 1 1 1 1 1
    /// // 1 1 1 1 1 1 1 1
    /// // 1 1 1 1 1 1 1 1
    /// // 1 1 1 1 1 1 1 1
    /// // 1 1 1 1 1 1 1 1
    /// // 1 1 1 1 1 1 1 1
    /// // 1 1 1 1 1 1 1 1
    /// // 1 1 1 1 1 1 1 1
    /// ```
    pub const FULL: Bitboard = Bitboard(!0);

    /// All dark squares.
    ///
    /// ```
    /// # use shakmaty::Bitboard;
    /// #
    /// # let bitboard = Bitboard::DARK_SQUARES;
    /// // . 1 . 1 . 1 . 1
    /// // 1 . 1 . 1 . 1 .
    /// // . 1 . 1 . 1 . 1
    /// // 1 . 1 . 1 . 1 .
    /// // . 1 . 1 . 1 . 1
    /// // 1 . 1 . 1 . 1 .
    /// // . 1 . 1 . 1 . 1
    /// // 1 . 1 . 1 . 1 .
    /// ```
    pub const DARK_SQUARES: Bitboard = Bitboard(0xaa55_aa55_aa55_aa55);

    /// All light squares.
    ///
    /// ```
    /// # use shakmaty::Bitboard;
    /// #
    /// # let bitboard = Bitboard::LIGHT_SQUARES;
    /// // 1 . 1 . 1 . 1 .
    /// // . 1 . 1 . 1 . 1
    /// // 1 . 1 . 1 . 1 .
    /// // . 1 . 1 . 1 . 1
    /// // 1 . 1 . 1 . 1 .
    /// // . 1 . 1 . 1 . 1
    /// // 1 . 1 . 1 . 1 .
    /// // . 1 . 1 . 1 . 1
    /// ```
    pub const LIGHT_SQUARES: Bitboard = Bitboard(0x55aa_55aa_55aa_55aa);

    /// The four corner squares.
    ///
    /// ```
    /// # use shakmaty::Bitboard;
    /// #
    /// # let bitboard = Bitboard::CORNERS;
    /// // 1 . . . . . . 1
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // 1 . . . . . . 1
    /// ```
    pub const CORNERS: Bitboard = Bitboard(0x8100_0000_0000_0081);

    /// The backranks.
    ///
    /// ```
    /// # use shakmaty::Bitboard;
    /// #
    /// # let bitboard = Bitboard::BACKRANKS;
    /// // 1 1 1 1 1 1 1 1
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // 1 1 1 1 1 1 1 1
    /// ```
    pub const BACKRANKS: Bitboard = Bitboard(0xff00_0000_0000_00ff);

    /// The four center squares.
    ///
    /// ```
    /// # use shakmaty::Bitboard;
    /// #
    /// # let bitboard = Bitboard::CENTER;
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . 1 1 . . .
    /// // . . . 1 1 . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// ```
    pub const CENTER: Bitboard = Bitboard(0x0000_0018_1800_0000);

    /// The northern half of the board.
    ///
    /// ```
    /// # use shakmaty::Bitboard;
    /// #
    /// # let bitboard = Bitboard::NORTH;
    /// // 1 1 1 1 1 1 1 1
    /// // 1 1 1 1 1 1 1 1
    /// // 1 1 1 1 1 1 1 1
    /// // 1 1 1 1 1 1 1 1
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// ```
    pub const NORTH: Bitboard = Bitboard(0xffff_ffff_0000_0000);

    /// The southern half of the board.
    ///
    /// ```
    /// # use shakmaty::Bitboard;
    /// #
    /// # let bitboard = Bitboard::SOUTH;
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // 1 1 1 1 1 1 1 1
    /// // 1 1 1 1 1 1 1 1
    /// // 1 1 1 1 1 1 1 1
    /// // 1 1 1 1 1 1 1 1
    /// ```
    pub const SOUTH: Bitboard = Bitboard(0x0000_0000_ffff_ffff);

    /// The western half of the board.
    ///
    /// ```
    /// # use shakmaty::Bitboard;
    /// #
    /// # let bitboard = Bitboard::WEST;
    /// // 1 1 1 1 . . . .
    /// // 1 1 1 1 . . . .
    /// // 1 1 1 1 . . . .
    /// // 1 1 1 1 . . . .
    /// // 1 1 1 1 . . . .
    /// // 1 1 1 1 . . . .
    /// // 1 1 1 1 . . . .
    /// // 1 1 1 1 . . . .
    /// ```
    pub const WEST: Bitboard = Bitboard(0x0f0f_0f0f_0f0f_0f0f);

    /// The eastern half of the board.
    ///
    /// ```
    /// # use shakmaty::Bitboard;
    /// #
    /// # let bitboard = Bitboard::EAST;
    /// // . . . . 1 1 1 1
    /// // . . . . 1 1 1 1
    /// // . . . . 1 1 1 1
    /// // . . . . 1 1 1 1
    /// // . . . . 1 1 1 1
    /// // . . . . 1 1 1 1
    /// // . . . . 1 1 1 1
    /// // . . . . 1 1 1 1
    /// ```
    pub const EAST: Bitboard = Bitboard(0xf0f0_f0f0_f0f0_f0f0);
}

/// Rank masks.
const RANKS: [u64; 8] = {
    let mut masks = [0; 8];
    let mut i = 0;
    while i < 8 {
        masks[i] = 0xff << (i * 8);
        i += 1;
    }
    masks
};

const FILE_A: u64 = 0x0101_0101_0101_0101;

#[derive(Copy, Clone)]
pub(crate) enum Direction {
    NorthWest,
    NorthEast,
    SouthWest,
    SouthEast,
}

impl Direction {
    #[inline(always)]
    pub const fn offset(self) -> i32 {
        match self {
            Direction::NorthWest => 7,
            Direction::SouthWest => -9,
            Direction::NorthEast => 9,
            Direction::SouthEast => -7,
        }
    }

    #[inline(always)]
    pub const fn translate(self, bitboard: Bitboard) -> Bitboard {
        Bitboard(match self {
            Direction::NorthWest => (bitboard.0 & !FILE_A) << 7,
            Direction::SouthWest => (bitboard.0 & !FILE_A) >> 9,
            Direction::NorthEast => (bitboard.0 << 9) & !FILE_A,
            Direction::SouthEast => (bitboard.0 >> 7) & !FILE_A,
        })
    }
}

impl fmt::Debug for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for rank in (0..8).map(Rank::new).rev() {
            for file in (0..8).map(File::new) {
                let sq = Square::from_coords(file, rank);
                f.write_char(if self.contains(sq) { '1' } else { '.' })?;
                f.write_char(if file < File::H { ' ' } else { '\n' })?;
            }
        }

        Ok(())
    }
}

impl fmt::UpperHex for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::UpperHex::fmt(&self.0, f)
    }
}

impl fmt::LowerHex for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::LowerHex::fmt(&self.0, f)
    }
}

impl fmt::Octal for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Octal::fmt(&self.0, f)
    }
}

impl fmt::Binary for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Binary::fmt(&self.0, f)
    }
}

impl From<Square> for Bitboard {
    #[inline]
    fn from(sq: Square) -> Bitboard {
        Bitboard::from_square(sq)
    }
}

impl From<Rank> for Bitboard {
    #[inline]
    fn from(rank: Rank) -> Bitboard {
        Bitboard::from_rank(rank)
    }
}

impl From<File> for Bitboard {
    #[inline]
    fn from(file: File) -> Bitboard {
        Bitboard::from_file(file)
    }
}

impl From<u64> for Bitboard {
    #[inline]
    fn from(bb: u64) -> Bitboard {
        Bitboard(bb)
    }
}

impl From<Bitboard> for u64 {
    #[inline]
    fn from(bb: Bitboard) -> u64 {
        bb.0
    }
}

impl<T> ops::BitAnd<T> for Bitboard
where
    T: Into<Bitboard>,
{
    type Output = Bitboard;

    #[inline]
    fn bitand(self, rhs: T) -> Bitboard {
        let Bitboard(rhs) = rhs.into();
        Bitboard(self.0 & rhs)
    }
}

impl<T> ops::BitAndAssign<T> for Bitboard
where
    T: Into<Bitboard>,
{
    #[inline]
    fn bitand_assign(&mut self, rhs: T) {
        let Bitboard(rhs) = rhs.into();
        self.0 &= rhs;
    }
}

impl<T> ops::BitOr<T> for Bitboard
where
    T: Into<Bitboard>,
{
    type Output = Bitboard;

    #[inline]
    fn bitor(self, rhs: T) -> Bitboard {
        let Bitboard(rhs) = rhs.into();
        Bitboard(self.0 | rhs)
    }
}

impl<T> ops::BitOrAssign<T> for Bitboard
where
    T: Into<Bitboard>,
{
    #[inline]
    fn bitor_assign(&mut self, rhs: T) {
        let Bitboard(rhs) = rhs.into();
        self.0 |= rhs;
    }
}

impl<T> ops::BitXor<T> for Bitboard
where
    T: Into<Bitboard>,
{
    type Output = Bitboard;

    #[inline]
    fn bitxor(self, rhs: T) -> Bitboard {
        let Bitboard(rhs) = rhs.into();
        Bitboard(self.0 ^ rhs)
    }
}

impl<T> ops::BitXorAssign<T> for Bitboard
where
    T: Into<Bitboard>,
{
    #[inline]
    fn bitxor_assign(&mut self, rhs: T) {
        let Bitboard(rhs) = rhs.into();
        self.0 ^= rhs;
    }
}

impl ops::Not for Bitboard {
    type Output = Bitboard;

    #[inline]
    fn not(self) -> Bitboard {
        Bitboard(!self.0)
    }
}

impl FromIterator<Square> for Bitboard {
    fn from_iter<T>(iter: T) -> Bitboard
    where
        T: IntoIterator<Item = Square>,
    {
        let mut result = Bitboard(0);
        result.extend(iter);
        result
    }
}

impl Extend<Square> for Bitboard {
    fn extend<T: IntoIterator<Item = Square>>(&mut self, iter: T) {
        for square in iter {
            self.add(square);
        }
    }
}

impl IntoIterator for Bitboard {
    type Item = Square;
    type IntoIter = IntoIter;

    #[inline]
    fn into_iter(self) -> IntoIter {
        IntoIter(self)
    }
}

/// Iterator over the squares of a [`Bitboard`].
#[derive(Debug, Clone)]
pub struct IntoIter(Bitboard);

impl Iterator for IntoIter {
    type Item = Square;

    #[inline]
    fn next(&mut self) -> Option<Square> {
        self.0.pop_front()
    }

    #[inline]
    fn count(self) -> usize {
        self.0.count()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.0.count();
        (len, Some(len))
    }

    #[inline]
    fn last(self) -> Option<Square> {
        self.0.last()
    }
}

impl ExactSizeIterator for IntoIter {
    #[inline]
    fn len(&self) -> usize {
        self.0.count()
    }
}

impl FusedIterator for IntoIter {}

impl DoubleEndedIterator for IntoIter {
    #[inline]
    fn next_back(&mut self) -> Option<Square> {
        self.0.pop_back()
    }
}

/// Iterator over the subsets of a [`Bitboard`].
///
/// See [`Bitboard::carry_rippler()`].
#[derive(Debug, Clone)]
pub struct CarryRippler {
    bb: u64,
    subset: u64,
    first: bool,
}

impl Iterator for CarryRippler {
    type Item = Bitboard;

    #[inline]
    fn next(&mut self) -> Option<Bitboard> {
        let subset = self.subset;
        if subset != 0 || self.first {
            self.first = false;
            self.subset = self.subset.wrapping_sub(self.bb) & self.bb;
            Some(Bitboard(subset))
        } else {
            None
        }
    }

    #[inline]
    fn last(self) -> Option<Bitboard> {
        if self.subset != 0 || self.first {
            Some(Bitboard(self.bb))
        } else {
            None
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, 1_usize.checked_shl(self.bb.count_ones()))
    }
}

impl FusedIterator for CarryRippler {}

#[cfg(test)]
mod tests {
    #[cfg(feature = "alloc")]
    use alloc::format;

    use super::*;

    #[test]
    fn test_more_than_one() {
        assert!(!Bitboard(0).more_than_one());
        assert!(!Bitboard(1).more_than_one());
        assert!(!Bitboard(2).more_than_one());
        assert!(Bitboard(3).more_than_one());
        assert!(Bitboard::FULL.more_than_one());
    }

    #[test]
    fn test_first() {
        assert_eq!(Bitboard::from_square(Square::A1).first(), Some(Square::A1));
        assert_eq!(Bitboard::from_square(Square::D2).first(), Some(Square::D2));
        assert_eq!(Bitboard(0).first(), None);
    }

    #[test]
    fn test_last() {
        assert_eq!(Bitboard::from_square(Square::A1).last(), Some(Square::A1));
        assert_eq!(
            Bitboard(0).with(Square::A1).with(Square::H1).last(),
            Some(Square::H1)
        );
        assert_eq!(Bitboard(0).last(), None);
    }

    #[test]
    fn test_isolate_first() {
        assert_eq!(
            Bitboard::from(Rank::Second).isolate_first(),
            Bitboard::from_square(Square::A2)
        );
        assert_eq!(Bitboard(0).isolate_first(), Bitboard(0));
    }

    #[test]
    fn test_isolate_last() {
        assert_eq!(
            Bitboard::from(File::C).isolate_last(),
            Bitboard::from_square(Square::C8)
        );
        assert_eq!(Bitboard(0).isolate_last(), Bitboard(0));
    }

    #[test]
    fn test_is_empty() {
        assert!(Bitboard(0).is_empty());
        assert!(!Bitboard(1).is_empty());
    }

    #[test]
    fn test_rank() {
        assert_eq!(Bitboard::from_rank(Rank::Fourth), Bitboard(0xff00_0000));
    }

    #[test]
    fn test_from_iter() {
        assert_eq!(Bitboard::from_iter(None), Bitboard(0));
        assert_eq!(
            Bitboard::from_iter(Some(Square::D2)),
            Bitboard::from_square(Square::D2)
        );
    }

    #[cfg(feature = "alloc")]
    #[test]
    fn test_upper_hex() {
        assert_eq!(format!("{:#0X}", Bitboard(42)), format!("{:#0X}", 42));
    }

    #[cfg(feature = "alloc")]
    #[test]
    fn test_lower_hex() {
        assert_eq!(format!("{:#0x}", Bitboard(42)), format!("{:#0x}", 42));
    }

    #[cfg(feature = "alloc")]
    #[test]
    fn test_octal() {
        assert_eq!(format!("{:#0o}", Bitboard(42)), format!("{:#0o}", 42));
    }

    #[cfg(feature = "alloc")]
    #[test]
    fn test_binary() {
        assert_eq!(format!("{:#0b}", Bitboard(42)), format!("{:#0b}", 42));
    }
}
