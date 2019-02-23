use std::cmp::min;
use std::error::Error;
use std::path::PathBuf;
use structopt::StructOpt;

use failure::Fail;

use shakmaty::fen::{fen, Fen};
use shakmaty::san::SanPlus;
use shakmaty::{Chess, Color, MoveList, Position, Setup};
use shakmaty_syzygy::{Dtz, SyzygyError, Tablebase, Wdl};

#[derive(Debug, StructOpt)]
struct Opt {
    /// Tablebase diretories
    #[structopt(long = "path", parse(from_os_str))]
    path: Vec<PathBuf>,
    /// Prints the result only
    #[structopt(long = "test")]
    test: bool,
    /// The position to probe
    fen: String,
}

fn real_wdl(tb: &Tablebase<Chess>, pos: &Chess, dtz: Dtz) -> Result<Wdl, SyzygyError> {
    if let Some(outcome) = pos.outcome() {
        return Ok(Wdl::from_outcome(outcome, pos.turn()));
    }

    let halfmoves = min(101, pos.halfmoves()) as i32;
    let before_zeroing = dtz.add_plies(halfmoves);

    if before_zeroing.0.abs() != 100 || halfmoves == 0 {
        // Unambiguous.
        return Ok(Wdl::from_dtz_after_zeroing(before_zeroing));
    }

    if halfmoves == 1 && dtz.0.abs() == 99 {
        // This could only be a cursed/blessed result if the real DTZ was
        // 100 instead of 99. But tables with DTZ 100 will always
        // store precise DTZ values, hence it could not have been 100.
        return Ok(Wdl::from_dtz_after_zeroing(before_zeroing));
    }

    let best = tb.best_move(pos)?.expect("has moves");
    let mut after = pos.clone();
    after.play_unchecked(&best.0);
    Ok(-real_wdl(tb, &after, best.1)?)
}

fn main() -> Result<(), Box<Error>> {
    let opt = Opt::from_args();

    let mut tablebase = Tablebase::new();
    for path in opt.path {
        tablebase.add_directory(path)?;
    }

    let mut pos: Chess = opt.fen.parse::<Fen>()?.position()?;

    let fen = fen(&pos);
    let dtz = tablebase.probe_dtz(&pos).map_err(|e| e.compat())?;
    let wdl = real_wdl(&tablebase, &pos, dtz).map_err(|e| e.compat())?;

    let result = match wdl {
        Wdl::Loss => "0-1",
        Wdl::Win => "1-0",
        _ => "1/2-1/2",
    };

    let mut legals = MoveList::new();
    let mut winning_sans = Vec::new();
    let mut drawing_sans = Vec::new();
    let mut losing_sans = Vec::new();
    pos.legal_moves(&mut legals);
    for m in &legals {
        let san = SanPlus::from_move(pos.clone(), m);
        let mut after = pos.clone();
        after.play_unchecked(m);
        let dtz_after = tablebase.probe_dtz(&pos).map_err(|e| e.compat())?;
        let list = match real_wdl(&tablebase, &after, dtz_after).map_err(|e| e.compat())? {
            Wdl::Loss => &mut losing_sans,
            Wdl::Win => &mut winning_sans,
            _ => &mut drawing_sans,
        };
        list.push(san.to_string());
    }

    let mut movetext = Vec::new();

    let mut force_movenumber = true;

    loop {
        if pos.is_checkmate() {
            movetext.push("{ Checkmate }".to_owned());
            break;
        }
        if pos.is_stalemate() {
            movetext.push("{ Stalemate }".to_owned());
            break;
        }
        if pos.is_insufficient_material() {
            movetext.push("{ Insufficient material }".to_owned());
            break;
        }
        if pos.is_variant_end() {
            movetext.push("{ Variant end }".to_owned());
            break;
        }

        if pos.halfmoves() == 0 {
            let Dtz(dtz) = tablebase.probe_dtz(&pos).map_err(|e| e.compat())?;
            movetext.push(format!("{{ {} with DTZ {} }}", pos.board().material(), dtz));
            force_movenumber = true;
        }

        let (bestmove, dtz) = tablebase.best_move(&pos).map_err(|e| e.compat())?.expect("has moves");
        if dtz == Dtz(0) {
            movetext.push("{ Tablebase draw }".to_owned());
            break;
        }

        match pos.turn() {
            Color::White => movetext.push(format!("{}.", pos.fullmoves())),
            Color::Black if force_movenumber => movetext.push(format!("{}...", pos.fullmoves())),
            _ => (),
        }

        movetext.push(SanPlus::from_move(pos.clone(), &bestmove).to_string());
        pos.play_unchecked(&bestmove);
        force_movenumber = false;
    }

    movetext.push(result.to_owned());

    if opt.test {
        println!("{}", result);
    } else {
        println!("[Event \"\"]");
        println!("[Site \"\"]");
        println!("[Date \"????.??.??\"]");
        println!("[Round \"-\"]");
        println!("[White \"Syzygy\"]");
        println!("[Black \"Syzygy\"]");
        println!("[Result \"{}\"]", result);
        println!("[FEN \"{}\"]", fen);
        println!("[Annotator \"shakmaty-syzygy\"]");
        println!("[WDL \"{:?}\"]", wdl);
        println!("[DTZ \"{}\"]", dtz.0);
        println!("[WinningMoves \"{}\"]", winning_sans.join(", "));
        println!("[DrawingMoves \"{}\"]", drawing_sans.join(", "));
        println!("[LosingMoves \"{}\"]", losing_sans.join(", "));
        println!();
        println!("{}", movetext.join(" "));
        println!();
    }

    Ok(())
}
