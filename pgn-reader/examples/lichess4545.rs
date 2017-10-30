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

        for &participant in ["Alegre_River", "Anunzio", "Archone", "Arkensiel",
            "Axp156", "Benedictine", "Boviced", "CapKas", "CarlosMagnussen",
            "ChessBuzz17", "Chess_Patzer", "Corridor", "Daveyjones01",
            "Delpire", "Dmathman", "Doevert", "Donkey-Kong16", "DracoxVitae",
            "DzeiTi", "EXOprimal", "ErinYu", "Fabinou", "Flandoo",
            "GetAGripWhitby", "Glad_He_Ate_Her", "GnarlyGoat", "GogoYubari",
            "Hackov", "Hoikka", "Immortality", "InfernalJumble", "JMHWSM",
            "JoeyMellberg", "JohnJPershing", "JohnsonBronson", "Jubka",
            "KFerapont", "Kjar", "Kraaft", "LeDarkKnight",
            "Lelouch_Vi_Brittania", "Lord_axe", "Lukhas", "Lynnpv", "MMichael",
            "MarionTinsley", "MasterSalomon", "Materix235", "Mebeka",
            "MichaelPren95", "Michael_hackett", "MoistvonLipwig",
            "Mooserohde", "MrLegilimens", "NOTFOXAnonymous", "Nairwolf",
            "Napoleon_Solo", "NarwhalePete", "NoKlu", "NoLuckOnlySkill",
            "Nombringer", "OuterHeaven92", "Petruchio", "Pewaps",
            "Plasterhippy", "PsiMaster", "Rascar_Capac", "RazorBoy", "Redban",
            "Rescinded", "RodrigoDiazdeVivar", "Ronsaki", "RottenPawn",
            "RuizBR", "Saxton47", "Scrooge", "Seb32", "Servasky", "Shetoo",
            "SirTurtle", "Somethingpretentious", "Sonata2", "SpiteKnight",
            "Staincastle", "TCGRIF", "TMaus", "Tellum", "TheRatRiverTrapper",
            "Toffeeman", "Tsatsa64", "Vevochi", "Wolf21", "Zharptytsia",
            "alexiov", "aliquantus", "aljopeljhan", "arex", "artykom",
            "atil4", "badplayer_cm", "be0wulf", "cftsoc3", "checkmonk",
            "chennis", "chessanalyst", "chesshavoc", "chesswithcoach",
            "chill5555", "comped", "cpapa14", "dahdah", "dank-chessessities",
            "david-innes", "deanmadden", "dimitris22", "dinomoomoo", "dkillian",
            "dose7781", "ed84", "eie24", "f1nn33", "ffenliv", "fisher56",
            "flaxl", "foon", "foreverweak", "foxxxy_coxxxy", "fradtheimpaler",
            "gatorricky", "gauravsinghmd", "glbert", "god666", "gorathger",
            "guanm", "gucelli", "gyrating_kairos", "haggismcbean",
            "hangingprawns", "hetraie", "hicetnunc", "hsznet", "iamjadetulip",
            "iongler", "isaypotato", "jacobhess", "jg777", "jippiedoe",
            "jivey", "john465", "jpokerflat", "kamekura", "kobol", "krispl",
            "kylecuver1", "liamvo", "lichie", "loukas435", "lovlas",
            "luvgangster", "mamaduck", "mariuseg", "matt_p_14", "mdpotts22",
            "mingpro", "misiek91z", "mn8", "mocl125", "morallygray", "mrjv",
            "nadgob", "nathanj439", "nimzovit", "nojero", "oddskill",
            "odyn1982", "onbion", "pioki", "piotor", "pokerram", "prune2000",
            "pushedpawn", "qrs", "quirked", "raitonvsfuuton", "rb52", "revhas",
            "revoof", "rexro", "rgpchesspe", "rhohit", "robinhood76", "roso97",
            "rpimpulse", "rsava", "s2004k1993", "scarff", "schubix", "scurly",
            "seanysean", "sgis", "slaiyn", "slavy", "sprcow", "spuntachessts",
            "swohl19", "tipau", "tnan123", "toj", "tomek188", "toni4127",
            "tothecloid", "truesacrifice", "udaysatya", "vadsamoht", "vgd123",
            "wargoblin", "yago666", "yungpirc"].iter()
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
