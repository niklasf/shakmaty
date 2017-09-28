extern crate pgn_reader;
extern crate memmap;
extern crate madvise;

use std::env;
use std::str;
use std::collections::HashSet;
use std::ascii::AsciiExt;

use pgn_reader::{Reader, Visitor, Skip};
use memmap::{Mmap, Protection};
use madvise::{AccessPattern, AdviseMemory};

struct ParticipantFilter<'a> {
    games: usize,
    matches: bool,
    participants: HashSet<&'a str>,
}

impl<'a> ParticipantFilter<'a> {
    fn lichess4545_season_9() -> ParticipantFilter<'a> {
        let mut participants = HashSet::new();

        participants.insert("felixnl");
        participants.insert("badplayer_cm");
        participants.insert("mariontinsley");
        participants.insert("toj");
        participants.insert("archone");
        participants.insert("chill5555");
        participants.insert("boviced");
        participants.insert("nicholasdrakes");
        participants.insert("dmathman");
        participants.insert("dracoxvitae");
        participants.insert("jmhwsm");
        participants.insert("mrjv");
        participants.insert("eie24");
        participants.insert("mebeka");
        participants.insert("oignons");
        participants.insert("tmaus");
        participants.insert("be0wulf");
        participants.insert("rpimpulse");
        participants.insert("pronew");
        participants.insert("david-innes");
        participants.insert("deepblueincarnate");
        participants.insert("mooserohde");
        participants.insert("psimaster");
        participants.insert("forhavu");
        participants.insert("gorathger");
        participants.insert("rb52");
        participants.insert("fisher56");
        participants.insert("outerheaven92");
        participants.insert("pioki");
        participants.insert("getagripwhitby");
        participants.insert("sirturtle");
        participants.insert("roso97");
        participants.insert("rescinded");
        participants.insert("udaysatya");
        participants.insert("piotor");
        participants.insert("napoleon_solo");
        participants.insert("tipau");
        participants.insert("s2004k1993");
        participants.insert("checkmonk");
        participants.insert("exoprimal");
        participants.insert("tellum");
        participants.insert("gucelli");
        participants.insert("toffeeman");
        participants.insert("wolf21");
        participants.insert("truesacrifice");
        participants.insert("staincastle");
        participants.insert("glad_he_ate_her");
        participants.insert("flandoo");
        participants.insert("razorboy");
        participants.insert("lelouch_vi_brittania");
        participants.insert("benedictine");
        participants.insert("corridor");
        participants.insert("servasky");
        participants.insert("jivey");
        participants.insert("mdpotts22");
        participants.insert("anunzio");
        participants.insert("fradtheimpaler");
        participants.insert("sgis");
        participants.insert("spuntachessts");
        participants.insert("nojero");
        participants.insert("hetraie");
        participants.insert("lichie");
        participants.insert("pushedpawn");
        participants.insert("kjar");
        participants.insert("daveyjones01");
        participants.insert("theratrivertrapper");
        participants.insert("gogoyubari");
        participants.insert("dimitris22");
        participants.insert("delpire");
        participants.insert("michael_hackett");
        participants.insert("oddskill");
        participants.insert("rhohit");
        participants.insert("iongler");
        participants.insert("wardsstone");
        participants.insert("nombringer");
        participants.insert("quirked");
        participants.insert("isaypotato");
        participants.insert("dahdah");
        participants.insert("raitonvsfuuton");
        participants.insert("johnsonbronson");
        participants.insert("seb32");
        participants.insert("yungpirc");
        participants.insert("mamaduck");
        participants.insert("cpapa14");
        participants.insert("erinyu");
        participants.insert("kobol");
        participants.insert("lord_axe");
        participants.insert("cosmosg");
        participants.insert("toni4127");
        participants.insert("aljopeljhan");
        participants.insert("shetoo");
        participants.insert("atil4");
        participants.insert("tcgrif");
        participants.insert("johnjpershing");
        participants.insert("joeymellberg");
        participants.insert("arkensiel");
        participants.insert("kferapont");
        participants.insert("hicetnunc");
        participants.insert("mingpro");
        participants.insert("somethingpretentious");
        participants.insert("lynnpv");
        participants.insert("revoof");
        participants.insert("pokerram");
        participants.insert("scarff");
        participants.insert("michaelpren95");
        participants.insert("immortality");
        participants.insert("donkey-kong16");
        participants.insert("rsava");
        participants.insert("artykom");
        participants.insert("mariuseg");
        participants.insert("captncarter");
        participants.insert("krispl");
        participants.insert("cftsoc3");
        participants.insert("foxxxy_coxxxy");
        participants.insert("hackov");
        participants.insert("ed84");
        participants.insert("chess_patzer");
        participants.insert("luvgangster");
        participants.insert("nadgob");
        participants.insert("noklu");
        participants.insert("fabinou");
        participants.insert("foreverweak");
        participants.insert("chennis");
        participants.insert("marstem");
        participants.insert("slaiyn");
        participants.insert("infernaljumble");
        participants.insert("mird113");
        participants.insert("flaxl");
        participants.insert("dinomoomoo");
        participants.insert("dose7781");
        participants.insert("slavy");
        participants.insert("wargoblin");
        participants.insert("revhas");
        participants.insert("tomek188");
        participants.insert("chessanalyst");
        participants.insert("onbion");
        participants.insert("yago666");
        participants.insert("loukas435");
        participants.insert("tnan123");
        participants.insert("ruizbr");
        participants.insert("spiteknight");
        participants.insert("zharptytsia");
        participants.insert("zeikex");
        participants.insert("john465");
        participants.insert("lovlas");
        participants.insert("noluckonlyskill");
        participants.insert("gnarlygoat");
        participants.insert("f1nn33");
        participants.insert("prune2000");
        participants.insert("vadsamoht");
        participants.insert("odyn1982");
        participants.insert("jpokerflat");
        participants.insert("arex");
        participants.insert("tsatsa64");
        participants.insert("kylecuver1");
        participants.insert("dank-chessessities");
        participants.insert("alegre_river");
        participants.insert("moistvonlipwig");
        participants.insert("matt_p_14");
        participants.insert("jg777");
        participants.insert("axp156");
        participants.insert("robinhood76");
        participants.insert("materix235");
        participants.insert("hoikka");
        participants.insert("tothecloid");
        participants.insert("gatorricky");
        participants.insert("narwhalepete");
        participants.insert("vevochi");

        ParticipantFilter { games: 0, matches: false, participants }
    }
}

impl<'a> Visitor for ParticipantFilter<'a> {
    type Result = ();

    fn end_game(&mut self, game: &[u8]) {
        if self.matches {
            self.games += 1;
            self.matches = false;
            print!("{}", str::from_utf8(game).expect("valid utf8"));
        }
    }

    fn header(&mut self, key: &[u8], value: &[u8]) {
        if !self.matches && (key == b"White" || key == b"Black") {
            if let Ok(name) = str::from_utf8(value) {
                self.matches = self.participants.contains(name.to_ascii_lowercase().as_str());
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
