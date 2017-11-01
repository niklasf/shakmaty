// This file is part of the shakmaty-syzygy library.
// Copyright (C) 2017 Niklas Fiekas <niklas.fiekas@backscattering.de>
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

use std::sync::Mutex;
use std::sync::atomic::{AtomicBool, Ordering};
use std::cell::UnsafeCell;

#[derive(Debug)]
pub struct Lazy<T> {
    initialized: AtomicBool,
    lock: Mutex<()>,
    value: UnsafeCell<Option<T>>,
}

impl<T> Lazy<T> {
    pub fn new() -> Lazy<T> {
        Lazy {
            initialized: AtomicBool::new(false),
            lock: Mutex::new(()),
            value: UnsafeCell::new(None),
        }
    }

    pub fn get_or_init<F, E>(&self, f: F) -> Result<&T, E>
        where F: FnOnce() -> Result<T, E>
    {
        // Double-checked locking:
        // https://stackoverflow.com/a/45692413/722291

        if !self.initialized.load(Ordering::Acquire) {
            let _lock = self.lock.lock().unwrap();

            if !self.initialized.load(Ordering::Relaxed) {
                let value = unsafe { &mut *self.value.get() };
                *value = Some(f()?);
                self.initialized.store(true, Ordering::Release);
            }
        }

        Ok(unsafe { &*self.value.get() }.as_ref().unwrap())
    }
}
