use core::{array, convert::identity, num};

use crate::{color::Color, types::Piece, util::overflow_error};

/// Piece types: `Pawn`, `Knight`, `Bishop`, `Rook`, `Queen`, `King`.
///
/// # Examples
///
/// ```
/// use shakmaty::Role;
///
/// // Piece types are indexed from 1 to 6.
/// assert_eq!(u32::from(Role::Pawn), 1);
/// assert_eq!(u32::from(Role::King), 6);
/// ```
#[allow(missing_docs)]
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Hash)]
pub enum Role {
    Pawn = 1,
    Knight = 2,
    Bishop = 3,
    Rook = 4,
    Queen = 5,
    King = 6,
}

impl Role {
    /// Gets the piece type from its English letter.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Role;
    ///
    /// assert_eq!(Role::from_char('K'), Some(Role::King));
    /// assert_eq!(Role::from_char('n'), Some(Role::Knight));
    ///
    /// assert_eq!(Role::from_char('X'), None);
    /// ```
    pub const fn from_char(ch: char) -> Option<Role> {
        match ch {
            'P' | 'p' => Some(Role::Pawn),
            'N' | 'n' => Some(Role::Knight),
            'B' | 'b' => Some(Role::Bishop),
            'R' | 'r' => Some(Role::Rook),
            'Q' | 'q' => Some(Role::Queen),
            'K' | 'k' => Some(Role::King),
            _ => None,
        }
    }

    /// Gets a [`Piece`] of the given color.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Color, Role};
    ///
    /// assert_eq!(Role::King.of(Color::Black), Color::Black.king());
    /// ```
    #[inline]
    pub const fn of(self, color: Color) -> Piece {
        Piece { color, role: self }
    }

    /// Gets the English letter for the piece type.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Role;
    ///
    /// assert_eq!(Role::Rook.char(), 'r');
    /// ```
    pub const fn char(self) -> char {
        match self {
            Role::Pawn => 'p',
            Role::Knight => 'n',
            Role::Bishop => 'b',
            Role::Rook => 'r',
            Role::Queen => 'q',
            Role::King => 'k',
        }
    }

    /// Gets the uppercase English letter for the piece type.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Role;
    ///
    /// assert_eq!(Role::Rook.upper_char(), 'R');
    /// ```
    pub const fn upper_char(self) -> char {
        match self {
            Role::Pawn => 'P',
            Role::Knight => 'N',
            Role::Bishop => 'B',
            Role::Rook => 'R',
            Role::Queen => 'Q',
            Role::King => 'K',
        }
    }

    /// `Pawn`, `Knight`, `Bishop`, `Rook`, `Queen`, and `King`, in this order.
    pub const ALL: [Role; 6] = [
        Role::Pawn,
        Role::Knight,
        Role::Bishop,
        Role::Rook,
        Role::Queen,
        Role::King,
    ];
}

macro_rules! int_from_role_impl {
    ($($t:ty)+) => {
        $(impl From<Role> for $t {
            #[inline]
            fn from(role: Role) -> $t {
                role as $t
            }
        })+
    }
}

int_from_role_impl! { u8 i8 u16 i16 u32 i32 u64 i64 usize isize }

macro_rules! nonzero_int_from_role_impl {
    ($($t:ty)+) => {
        $(impl From<Role> for $t {
            #[inline]
            fn from(role: Role) -> $t {
                <$t>::new(role.into()).expect("nonzero role discriminant")
            }
        })+
    }
}

nonzero_int_from_role_impl! {
    num::NonZeroU8 num::NonZeroI8
    num::NonZeroU16 num::NonZeroI16
    num::NonZeroU32 num::NonZeroI32
    num::NonZeroU64 num::NonZeroI64
    num::NonZeroUsize num::NonZeroIsize
}

macro_rules! try_role_from_int_impl {
    ($($t:ty)+) => {
        $(impl core::convert::TryFrom<$t> for Role {
            type Error = num::TryFromIntError;

            #[inline]
            fn try_from(value: $t) -> Result<Role, Self::Error> {
                Ok(match value {
                    1 => Role::Pawn,
                    2 => Role::Knight,
                    3 => Role::Bishop,
                    4 => Role::Rook,
                    5 => Role::Queen,
                    6 => Role::King,
                    _ => return Err(overflow_error()),
                })
            }
        })+
    }
}

try_role_from_int_impl! { u8 i8 u16 i16 u32 i32 u64 i64 usize isize }

/// Container with values for each [`Role`].
#[derive(Copy, Clone, Default, Eq, PartialEq, Debug, Hash)]
#[repr(C)]
pub struct ByRole<T> {
    pub pawn: T,
    pub knight: T,
    pub bishop: T,
    pub rook: T,
    pub queen: T,
    pub king: T,
}

impl<T> ByRole<T> {
    pub fn new_with<F>(mut init: F) -> ByRole<T>
    where
        F: FnMut(Role) -> T,
    {
        ByRole {
            pawn: init(Role::Pawn),
            knight: init(Role::Knight),
            bishop: init(Role::Bishop),
            rook: init(Role::Rook),
            queen: init(Role::Queen),
            king: init(Role::King),
        }
    }

    #[inline]
    pub const fn get(&self, role: Role) -> &T {
        // Safety: Trivial offset into #[repr(C)] struct.
        unsafe {
            &*(self as *const ByRole<T>)
                .cast::<T>()
                .offset(role as isize - 1)
        }
    }

    #[inline]
    pub fn get_mut(&mut self, role: Role) -> &mut T {
        // Safety: Trivial offset into #[repr(C)] struct.
        unsafe {
            &mut *(self as *mut ByRole<T>)
                .cast::<T>()
                .offset(role as isize - 1)
        }
    }

    #[inline]
    pub fn for_each<F>(self, mut f: F)
    where
        F: FnMut(T),
    {
        f(self.pawn);
        f(self.knight);
        f(self.bishop);
        f(self.rook);
        f(self.queen);
        f(self.king);
    }

    #[inline]
    pub fn map<U, F>(self, mut f: F) -> ByRole<U>
    where
        F: FnMut(T) -> U,
    {
        ByRole {
            pawn: f(self.pawn),
            knight: f(self.knight),
            bishop: f(self.bishop),
            rook: f(self.rook),
            queen: f(self.queen),
            king: f(self.king),
        }
    }

    #[inline]
    pub fn find<F>(&self, mut predicate: F) -> Option<Role>
    where
        F: FnMut(&T) -> bool,
    {
        if predicate(&self.pawn) {
            Some(Role::Pawn)
        } else if predicate(&self.knight) {
            Some(Role::Knight)
        } else if predicate(&self.bishop) {
            Some(Role::Bishop)
        } else if predicate(&self.rook) {
            Some(Role::Rook)
        } else if predicate(&self.queen) {
            Some(Role::Queen)
        } else if predicate(&self.king) {
            Some(Role::King)
        } else {
            None
        }
    }

    #[inline]
    pub const fn as_ref(&self) -> ByRole<&T> {
        ByRole {
            pawn: &self.pawn,
            knight: &self.knight,
            bishop: &self.bishop,
            rook: &self.rook,
            queen: &self.queen,
            king: &self.king,
        }
    }

    #[inline]
    pub fn as_mut(&mut self) -> ByRole<&mut T> {
        ByRole {
            pawn: &mut self.pawn,
            knight: &mut self.knight,
            bishop: &mut self.bishop,
            rook: &mut self.rook,
            queen: &mut self.queen,
            king: &mut self.king,
        }
    }

    pub fn zip<U>(self, other: ByRole<U>) -> ByRole<(T, U)> {
        ByRole {
            pawn: (self.pawn, other.pawn),
            knight: (self.knight, other.knight),
            bishop: (self.bishop, other.bishop),
            rook: (self.rook, other.rook),
            queen: (self.queen, other.queen),
            king: (self.king, other.king),
        }
    }

    pub fn zip_role(self) -> ByRole<(Role, T)> {
        ByRole::new_with(identity).zip(self)
    }

    pub fn iter(&self) -> array::IntoIter<&T, 6> {
        self.as_ref().into_iter()
    }

    pub fn iter_mut(&mut self) -> array::IntoIter<&mut T, 6> {
        self.as_mut().into_iter()
    }
}

#[cfg(feature = "variant")]
impl ByRole<u8> {
    pub(crate) fn count(&self) -> usize {
        self.iter().map(|c| usize::from(*c)).sum()
    }
}

impl<T: Copy> ByRole<&T> {
    pub fn copied(self) -> ByRole<T> {
        self.map(|item| *item)
    }
}

impl<T: Clone> ByRole<&T> {
    pub fn cloned(self) -> ByRole<T> {
        self.map(Clone::clone)
    }
}

impl<T> IntoIterator for ByRole<T> {
    type Item = T;
    type IntoIter = array::IntoIter<T, 6>;

    fn into_iter(self) -> Self::IntoIter {
        [
            self.pawn,
            self.knight,
            self.bishop,
            self.rook,
            self.queen,
            self.king,
        ]
        .into_iter()
    }
}
