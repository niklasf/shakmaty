#![feature(cfg_target_feature)]
#![feature(asm)]
#![feature(zero_one)]

mod square;
mod bitboard;
mod magics;

fn main() {
    magics::init_rook_tables();
}
