// Filters games of lichess4545.com participants from PGNs.
// Usage: cargo run --release --example lichess4545 -- [PGN]...

extern crate pgn_reader;
extern crate memmap;
extern crate madvise;
extern crate unicase;

use std::env;
use std::str;
use std::fs::File;
use std::collections::HashSet;

use pgn_reader::{Reader, Visitor, Skip};
use memmap::Mmap;
use madvise::{AccessPattern, AdviseMemory};

struct ParticipantFilter<'a> {
    games: usize,
    matches: bool,
    participants: HashSet<unicase::Ascii<&'a str>>,
}

impl<'a> ParticipantFilter<'a> {
    fn lichess4545_season_9() -> ParticipantFilter<'a> {
        let mut participants = HashSet::new();

        for &participant in [
            "Alegre_River", "AlwxR", "AmoryRacingKing", "Archone", "Arkensiel",
            "Artykom", "Asinya", "Assassin_in_White", "Axp156", "Benedictine",
            "Bjorgvin1", "Bletchlypark", "Boomer34", "Boviced", "Bratac38",
            "CamelClutcher", "ChessAndCoffee", "Chess_Patzer", "Davbond",
            "Delpire", "DirtyJs", "Djc76", "Doncyrleone", "Donkey-kong16",
            "DracoxVitae", "Dubarnik", "EinarGregersen", "Ender14123",
            "ErinYu", "FlokiTheCat", "Forhavu", "GCEDW2001", "GetAGripWhitby",
            "Giampy62", "Glaisepapale", "Gm1224", "GnarlyGoat", "GogoYubari",
            "Gokuba", "HELWILLEM", "Hackov", "HandKnit", "Heinie", "Hoikka",
            "Hsznet", "InfernalJumble", "JakeStateFarm", "JensenUVA",
            "JohnJPershing", "JohnsonBronson", "KFerapont", "Keresch", "Kjar",
            "Kraaft", "Laven", "LeDarkKnight", "Lelouch_Vi_Brittania",
            "LittleAnnieAdderall", "Lord_axe", "Lukhas", "Lynnpv",
            "MarionTinsley", "MasterSalomon", "MattyB", "MelodyMarathon",
            "Michael_hackett", "MoistvonLipwig", "MoneyMitch", "Mooserohde",
            "NOTFOXAnonymous", "Nairwolf", "NarwhalePete", "PaulCarrero",
            "PaulKagame", "Percevalxxxx", "Philgood84", "Poseh",
            "Programminglinguist", "Prune2000", "PsiMaster",
            "RodrigoDiazdeVivar", "RottenPawn", "Rsava", "SamuelIFowler",
            "Scrooge", "Seb32", "ShAlexander", "Shetoo", "SolarNight",
            "Somethingpretentious", "Sonata2", "Spiritchaser84", "SpiteKnight",
            "Star-Bearer", "SuedeStonn", "TCGRIF", "TMaus",
            "Th3RomanticWarrior", "TheFroo", "ThePasiP", "TheRatRiverTrapper",
            "Toffeeman", "Treppenhouse", "Trexchess", "Tsatsa64", "Urjah",
            "VicPez", "Zeggelaar", "Zharptytsia", "adamkit", "aliquantus",
            "arbisto", "be0wulf", "blstk", "bornacavrag", "brainyack",
            "cacheyourdreams", "cacoph", "carc", "caxmati", "cdoss",
            "checkmonk", "chennis", "chesshavoc", "colwem", "davejishnu",
            "david-innes", "deanmadden", "deep_glue", "dimitris22",
            "dinomoomoo", "dorjanel", "dose7781", "eie24", "ermilom",
            "fisher56", "flaxl", "gattican", "gauravsinghmd", "ghostult",
            "ghoul_1997", "glbert", "god666", "gucelli", "gyrating_kairos",
            "haggismcbean", "hawkins89", "hetraie", "hicetnunc", "iqb",
            "isaypotato", "ismitt", "javi0111", "jg777", "jivey", "jkobs",
            "kanakaishou", "kart0ffelsalaat", "kikiyop", "kleyveu", "kolnikov",
            "kponds", "krispl", "kylecuver1", "liamvo", "loukas435", "lovlas",
            "luvgangster", "lvew", "mariuseg", "miikka1", "moinmann",
            "morallygray", "myle", "nimzovit", "novemberyankee", "oddskill",
            "odyn1982", "omoueza", "paolone", "parametric", "pedropablo72",
            "pioki", "pmyourpepes", "pokerram", "qrs", "quirked", "rampichino",
            "rederik79", "revoof", "rgpchesspe", "rhohit", "rkjulian",
            "roso97", "rpimpulse", "s2004k1993", "sagy_dr", "scarff",
            "schubix", "sgis", "staincastle", "stinsonite", "supermaths",
            "superposition", "swohl19", "timoru", "tipau", "tnan123",
            "toni4127", "truesacrifice", "vgd123", "vike27", "wargoblin",
            "xearo", "yago666", "yungpirc", "zbakov"].iter()
        {
            participants.insert(unicase::Ascii::new(participant));
        }

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
        let file = File::open(&arg).expect("fopen");
        let pgn = unsafe { Mmap::map(&file).expect("mmap") };
        pgn.advise_memory_access(AccessPattern::Sequential).expect("madvise");

        let mut filter = ParticipantFilter::lichess4545_season_9();
        let total = Reader::new(&mut filter, &pgn[..]).into_iter().count();
        eprintln!("% {}: {} games total, {} matches", arg, total, filter.games);
    }
}
