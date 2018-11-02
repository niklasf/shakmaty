// This file is part of the pgn-reader library.
// Copyright (C) 2017-2018 Niklas Fiekas <niklas.fiekas@backscattering.de>
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

use shakmaty::Outcome;
use shakmaty::san::SanPlus;

use types::{RawHeader, Nag, Skip};

pub trait Visitor {
    type Result;

    fn begin_game(&mut self) { }

    fn begin_headers(&mut self) { }
    fn header(&mut self, _key: &[u8], _value: RawHeader<'_>) { }
    fn end_headers(&mut self) -> Skip { Skip(false) }

    fn san(&mut self, _san_plus: SanPlus) { }
    fn nag(&mut self, _nag: Nag) { }
    fn comment(&mut self, _comment: &[u8]) { }
    fn begin_variation(&mut self) -> Skip { Skip(false) }
    fn end_variation(&mut self) { }
    fn outcome(&mut self, _outcome: Option<Outcome>) { }

    fn end_game(&mut self) -> Self::Result;
}

pub(crate) struct SkipVisitor;

impl Visitor for SkipVisitor {
    type Result = ();

    fn end_headers(&mut self) -> Skip { Skip(true) }
    fn begin_variation(&mut self) -> Skip { Skip(true) }
    fn end_game(&mut self) { }
}

