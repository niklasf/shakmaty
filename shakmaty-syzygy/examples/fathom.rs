use std::{error::Error, path::PathBuf};

use clap::{builder::PathBufValueParser, Parser};
use shakmaty::{
    fen::Fen, san::SanPlus, CastlingMode, Chess, Color, EnPassantMode, Outcome, Position,
};
use shakmaty_syzygy::{Material, MaybeRounded, Tablebase};

#[derive(Debug, Parser)]
struct Opt {
    /// Tablebase diretories
    #[arg(long = "path", value_parser = PathBufValueParser::new())]
    path: Vec<PathBuf>,
    /// Prints the result only
    #[arg(long = "test")]
    test: bool,
    /// The position to probe
    fen: Fen,
}

fn main() -> Result<(), Box<dyn Error>> {
    let opt = Opt::parse();

    let mut tablebase = Tablebase::new();
    for path in opt.path {
        tablebase.add_directory(path)?;
    }

    let mut pos: Chess = opt.fen.into_position(CastlingMode::Chess960)?;
    let fen = Fen::from_position(pos.clone(), EnPassantMode::Legal);

    let material = Material::from_board(pos.board());
    let wdl = tablebase.probe_wdl(&pos)?;
    let dtz = tablebase.probe_dtz(&pos)?;

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

        if pos.halfmoves() == 100 {
            movetext.push("{ Draw claimed }".to_owned());
            force_movenumber = true;
        } else if pos.halfmoves() == 0 {
            movetext.push(match tablebase.probe_dtz(&pos)? {
                MaybeRounded::Precise(dtz) => format!(
                    "{{ {} with DTZ {} }}",
                    Material::from_board(pos.board()),
                    i32::from(dtz)
                ),
                MaybeRounded::Rounded(dtz) => format!(
                    "{{ {} with DTZ {} or {} }}",
                    Material::from_board(pos.board()),
                    i32::from(dtz),
                    i32::from(dtz.add_plies(1))
                ),
            });
            force_movenumber = true;
        }

        let (bestmove, dtz) = tablebase.best_move(&pos)?.expect("has moves");
        if dtz.is_zero() {
            movetext.push("{ Tablebase draw }".to_owned());
            break;
        }

        match pos.turn() {
            Color::White => movetext.push(format!("{}.", pos.fullmoves())),
            Color::Black if force_movenumber => movetext.push(format!("{}...", pos.fullmoves())),
            _ => (),
        }

        movetext.push(SanPlus::from_move_and_play_unchecked(&mut pos, &bestmove).to_string());
        force_movenumber = false;
    }

    let result = pos.outcome().unwrap_or(Outcome::Draw);

    movetext.push(result.to_string());

    if opt.test {
        println!("{result}");
    } else {
        println!("[Event \"{material}\"]");
        println!("[Site \"\"]");
        println!("[Date \"????.??.??\"]");
        println!("[Round \"-\"]");
        println!("[White \"Syzygy\"]");
        println!("[Black \"Syzygy\"]");
        println!("[Result \"{result}\"]");
        println!("[FEN \"{fen}\"]");
        println!("[Annotator \"shakmaty-syzygy\"]");
        println!("[WDL \"{wdl:?}\"]");
        match dtz {
            MaybeRounded::Precise(dtz) => println!("[DTZ \"{}\"]", i32::from(dtz)),
            MaybeRounded::Rounded(dtz) => println!(
                "[DTZ \"{} or {}\"]",
                i32::from(dtz),
                i32::from(dtz.add_plies(1))
            ),
        }
        println!();
        println!("{}", movetext.join(" "));
        println!();
    }

    Ok(())
}
