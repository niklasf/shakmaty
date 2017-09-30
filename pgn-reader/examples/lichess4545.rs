// Filters games of lichess4545.com participants from PGNs.
// Usage: cargo run --release --example lichess4545 -- [PGN]...

extern crate pgn_reader;
extern crate memmap;
extern crate madvise;
extern crate unicase;

use std::env;
use std::str;
use std::collections::HashSet;

use pgn_reader::{Reader, Visitor, Skip};
use memmap::{Mmap, Protection};
use madvise::{AccessPattern, AdviseMemory};

struct ParticipantFilter<'a> {
    games: usize,
    matches: bool,
    participants: HashSet<unicase::Ascii<&'a str>>,
}

impl<'a> ParticipantFilter<'a> {
    fn lichess4545_season_9() -> ParticipantFilter<'a> {
        let mut participants = HashSet::new();

        participants.insert(unicase::Ascii::new("felixnl"));
        participants.insert(unicase::Ascii::new("badplayer_cm"));
        participants.insert(unicase::Ascii::new("mariontinsley"));
        participants.insert(unicase::Ascii::new("toj"));
        participants.insert(unicase::Ascii::new("archone"));
        participants.insert(unicase::Ascii::new("chill5555"));
        participants.insert(unicase::Ascii::new("boviced"));
        participants.insert(unicase::Ascii::new("nicholasdrakes"));
        participants.insert(unicase::Ascii::new("dmathman"));
        participants.insert(unicase::Ascii::new("dracoxvitae"));
        participants.insert(unicase::Ascii::new("jmhwsm"));
        participants.insert(unicase::Ascii::new("mrjv"));
        participants.insert(unicase::Ascii::new("eie24"));
        participants.insert(unicase::Ascii::new("mebeka"));
        participants.insert(unicase::Ascii::new("oignons"));
        participants.insert(unicase::Ascii::new("tmaus"));
        participants.insert(unicase::Ascii::new("be0wulf"));
        participants.insert(unicase::Ascii::new("rpimpulse"));
        participants.insert(unicase::Ascii::new("pronew"));
        participants.insert(unicase::Ascii::new("david-innes"));
        participants.insert(unicase::Ascii::new("deepblueincarnate"));
        participants.insert(unicase::Ascii::new("mooserohde"));
        participants.insert(unicase::Ascii::new("psimaster"));
        participants.insert(unicase::Ascii::new("forhavu"));
        participants.insert(unicase::Ascii::new("gorathger"));
        participants.insert(unicase::Ascii::new("rb52"));
        participants.insert(unicase::Ascii::new("fisher56"));
        participants.insert(unicase::Ascii::new("outerheaven92"));
        participants.insert(unicase::Ascii::new("pioki"));
        participants.insert(unicase::Ascii::new("getagripwhitby"));
        participants.insert(unicase::Ascii::new("sirturtle"));
        participants.insert(unicase::Ascii::new("roso97"));
        participants.insert(unicase::Ascii::new("rescinded"));
        participants.insert(unicase::Ascii::new("udaysatya"));
        participants.insert(unicase::Ascii::new("piotor"));
        participants.insert(unicase::Ascii::new("napoleon_solo"));
        participants.insert(unicase::Ascii::new("tipau"));
        participants.insert(unicase::Ascii::new("s2004k1993"));
        participants.insert(unicase::Ascii::new("checkmonk"));
        participants.insert(unicase::Ascii::new("exoprimal"));
        participants.insert(unicase::Ascii::new("tellum"));
        participants.insert(unicase::Ascii::new("gucelli"));
        participants.insert(unicase::Ascii::new("toffeeman"));
        participants.insert(unicase::Ascii::new("wolf21"));
        participants.insert(unicase::Ascii::new("truesacrifice"));
        participants.insert(unicase::Ascii::new("staincastle"));
        participants.insert(unicase::Ascii::new("glad_he_ate_her"));
        participants.insert(unicase::Ascii::new("flandoo"));
        participants.insert(unicase::Ascii::new("razorboy"));
        participants.insert(unicase::Ascii::new("lelouch_vi_brittania"));
        participants.insert(unicase::Ascii::new("benedictine"));
        participants.insert(unicase::Ascii::new("corridor"));
        participants.insert(unicase::Ascii::new("servasky"));
        participants.insert(unicase::Ascii::new("jivey"));
        participants.insert(unicase::Ascii::new("mdpotts22"));
        participants.insert(unicase::Ascii::new("anunzio"));
        participants.insert(unicase::Ascii::new("fradtheimpaler"));
        participants.insert(unicase::Ascii::new("sgis"));
        participants.insert(unicase::Ascii::new("spuntachessts"));
        participants.insert(unicase::Ascii::new("nojero"));
        participants.insert(unicase::Ascii::new("hetraie"));
        participants.insert(unicase::Ascii::new("lichie"));
        participants.insert(unicase::Ascii::new("pushedpawn"));
        participants.insert(unicase::Ascii::new("kjar"));
        participants.insert(unicase::Ascii::new("daveyjones01"));
        participants.insert(unicase::Ascii::new("theratrivertrapper"));
        participants.insert(unicase::Ascii::new("gogoyubari"));
        participants.insert(unicase::Ascii::new("dimitris22"));
        participants.insert(unicase::Ascii::new("delpire"));
        participants.insert(unicase::Ascii::new("michael_hackett"));
        participants.insert(unicase::Ascii::new("oddskill"));
        participants.insert(unicase::Ascii::new("rhohit"));
        participants.insert(unicase::Ascii::new("iongler"));
        participants.insert(unicase::Ascii::new("wardsstone"));
        participants.insert(unicase::Ascii::new("nombringer"));
        participants.insert(unicase::Ascii::new("quirked"));
        participants.insert(unicase::Ascii::new("isaypotato"));
        participants.insert(unicase::Ascii::new("dahdah"));
        participants.insert(unicase::Ascii::new("raitonvsfuuton"));
        participants.insert(unicase::Ascii::new("johnsonbronson"));
        participants.insert(unicase::Ascii::new("seb32"));
        participants.insert(unicase::Ascii::new("yungpirc"));
        participants.insert(unicase::Ascii::new("mamaduck"));
        participants.insert(unicase::Ascii::new("cpapa14"));
        participants.insert(unicase::Ascii::new("erinyu"));
        participants.insert(unicase::Ascii::new("kobol"));
        participants.insert(unicase::Ascii::new("lord_axe"));
        participants.insert(unicase::Ascii::new("cosmosg"));
        participants.insert(unicase::Ascii::new("toni4127"));
        participants.insert(unicase::Ascii::new("aljopeljhan"));
        participants.insert(unicase::Ascii::new("shetoo"));
        participants.insert(unicase::Ascii::new("atil4"));
        participants.insert(unicase::Ascii::new("tcgrif"));
        participants.insert(unicase::Ascii::new("johnjpershing"));
        participants.insert(unicase::Ascii::new("joeymellberg"));
        participants.insert(unicase::Ascii::new("arkensiel"));
        participants.insert(unicase::Ascii::new("kferapont"));
        participants.insert(unicase::Ascii::new("hicetnunc"));
        participants.insert(unicase::Ascii::new("mingpro"));
        participants.insert(unicase::Ascii::new("somethingpretentious"));
        participants.insert(unicase::Ascii::new("lynnpv"));
        participants.insert(unicase::Ascii::new("revoof"));
        participants.insert(unicase::Ascii::new("pokerram"));
        participants.insert(unicase::Ascii::new("scarff"));
        participants.insert(unicase::Ascii::new("michaelpren95"));
        participants.insert(unicase::Ascii::new("immortality"));
        participants.insert(unicase::Ascii::new("donkey-kong16"));
        participants.insert(unicase::Ascii::new("rsava"));
        participants.insert(unicase::Ascii::new("artykom"));
        participants.insert(unicase::Ascii::new("mariuseg"));
        participants.insert(unicase::Ascii::new("captncarter"));
        participants.insert(unicase::Ascii::new("krispl"));
        participants.insert(unicase::Ascii::new("cftsoc3"));
        participants.insert(unicase::Ascii::new("foxxxy_coxxxy"));
        participants.insert(unicase::Ascii::new("hackov"));
        participants.insert(unicase::Ascii::new("ed84"));
        participants.insert(unicase::Ascii::new("chess_patzer"));
        participants.insert(unicase::Ascii::new("luvgangster"));
        participants.insert(unicase::Ascii::new("nadgob"));
        participants.insert(unicase::Ascii::new("noklu"));
        participants.insert(unicase::Ascii::new("fabinou"));
        participants.insert(unicase::Ascii::new("foreverweak"));
        participants.insert(unicase::Ascii::new("chennis"));
        participants.insert(unicase::Ascii::new("marstem"));
        participants.insert(unicase::Ascii::new("slaiyn"));
        participants.insert(unicase::Ascii::new("infernaljumble"));
        participants.insert(unicase::Ascii::new("mird113"));
        participants.insert(unicase::Ascii::new("flaxl"));
        participants.insert(unicase::Ascii::new("dinomoomoo"));
        participants.insert(unicase::Ascii::new("dose7781"));
        participants.insert(unicase::Ascii::new("slavy"));
        participants.insert(unicase::Ascii::new("wargoblin"));
        participants.insert(unicase::Ascii::new("revhas"));
        participants.insert(unicase::Ascii::new("tomek188"));
        participants.insert(unicase::Ascii::new("chessanalyst"));
        participants.insert(unicase::Ascii::new("onbion"));
        participants.insert(unicase::Ascii::new("yago666"));
        participants.insert(unicase::Ascii::new("loukas435"));
        participants.insert(unicase::Ascii::new("tnan123"));
        participants.insert(unicase::Ascii::new("ruizbr"));
        participants.insert(unicase::Ascii::new("spiteknight"));
        participants.insert(unicase::Ascii::new("zharptytsia"));
        participants.insert(unicase::Ascii::new("zeikex"));
        participants.insert(unicase::Ascii::new("john465"));
        participants.insert(unicase::Ascii::new("lovlas"));
        participants.insert(unicase::Ascii::new("noluckonlyskill"));
        participants.insert(unicase::Ascii::new("gnarlygoat"));
        participants.insert(unicase::Ascii::new("f1nn33"));
        participants.insert(unicase::Ascii::new("prune2000"));
        participants.insert(unicase::Ascii::new("vadsamoht"));
        participants.insert(unicase::Ascii::new("odyn1982"));
        participants.insert(unicase::Ascii::new("jpokerflat"));
        participants.insert(unicase::Ascii::new("arex"));
        participants.insert(unicase::Ascii::new("tsatsa64"));
        participants.insert(unicase::Ascii::new("kylecuver1"));
        participants.insert(unicase::Ascii::new("dank-chessessities"));
        participants.insert(unicase::Ascii::new("alegre_river"));
        participants.insert(unicase::Ascii::new("moistvonlipwig"));
        participants.insert(unicase::Ascii::new("matt_p_14"));
        participants.insert(unicase::Ascii::new("jg777"));
        participants.insert(unicase::Ascii::new("axp156"));
        participants.insert(unicase::Ascii::new("robinhood76"));
        participants.insert(unicase::Ascii::new("materix235"));
        participants.insert(unicase::Ascii::new("hoikka"));
        participants.insert(unicase::Ascii::new("tothecloid"));
        participants.insert(unicase::Ascii::new("gatorricky"));
        participants.insert(unicase::Ascii::new("narwhalepete"));
        participants.insert(unicase::Ascii::new("vevochi"));

        ParticipantFilter { games: 0, matches: false, participants }
    }
}

impl<'a, 'pgn> Visitor<'pgn> for ParticipantFilter<'a> {
    type Result = ();

    fn end_game(&mut self, game: &'pgn [u8]) {
        if self.matches {
            self.games += 1;
            self.matches = false;
            print!("{}", str::from_utf8(game).expect("valid utf8"));
        }
    }

    fn header(&mut self, key: &'pgn [u8], value: &'pgn [u8]) {
        if !self.matches && (key == b"White" || key == b"Black") {
            if let Ok(name) = str::from_utf8(value) {
                self.matches = self.participants.contains(&unicase::Ascii::new(name));
            }
        }
    }

    fn end_headers(&mut self) -> Skip {
        Skip(true)
    }
}

fn main() {
    for arg in env::args().skip(1) {
        let mmap = Mmap::open_path(&arg, Protection::Read).expect("mmap");
        let pgn = unsafe { mmap.as_slice() };
        pgn.advise_memory_access(AccessPattern::Sequential).expect("madvise");

        let mut filter = ParticipantFilter::lichess4545_season_9();
        let total = Reader::new(&mut filter, pgn).into_iter().count();
        eprintln!("% {}: {} games total, {} matches", arg, total, filter.games);
    }
}
