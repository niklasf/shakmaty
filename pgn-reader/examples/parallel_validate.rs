// Validates moves in PGNs, using multiple threads.
// Usage: cargo run --release --example parallel_validate -- [PGN]...
//
// One thread splits the input into chunks of consecutive games, using the
// fast Reader::skip_game(). Worker threads then parse and validate the
// chunks.

use std::{
    env,
    error::Error,
    fs::File,
    io::{self, Read},
    mem,
    ops::ControlFlow,
    process::ExitCode,
    sync::atomic::{AtomicBool, Ordering},
    thread,
};

use crossbeam::channel;
use pgn_reader::{
    RawTag, Reader, SanPlus, Visitor,
    shakmaty::{CastlingMode, Chess, Position, fen::Fen},
};

const CHUNK_GAMES: usize = 1000;

struct Tee<R> {
    inner: R,
    delivered: Vec<u8>,
}

impl<R: Read> Read for Tee<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let n = self.inner.read(buf)?;
        self.delivered.extend_from_slice(&buf[..n]);
        Ok(n)
    }
}

impl<R> Tee<R> {
    fn new(inner: R) -> Self {
        Self {
            inner,
            delivered: Vec::new(),
        }
    }

    /// Takes the bytes delivered so far, except for the last `unconsumed`
    /// bytes, which are kept for the next call.
    fn take(&mut self, unconsumed: usize, mut recycled_buffer: Vec<u8>) -> Vec<u8> {
        let cut = self.delivered.len() - unconsumed;
        recycled_buffer.clear();
        recycled_buffer.extend_from_slice(&self.delivered[cut..]);
        self.delivered.truncate(cut);
        mem::replace(&mut self.delivered, recycled_buffer)
    }
}

struct Chunk {
    base_game_index: usize,
    bytes: Vec<u8>,
}

fn split<R: Read>(
    reader: R,
    send: channel::Sender<Chunk>,
    recycle: channel::Receiver<Vec<u8>>,
) -> io::Result<()> {
    let mut reader = Reader::new(Tee::new(reader));

    let mut base_game_index = 0;
    loop {
        let mut games = 0;
        while games < CHUNK_GAMES && reader.skip_game()? {
            games += 1;
        }
        if games == 0 {
            return Ok(());
        }

        let unconsumed = reader.buffer().len();
        let chunk = Chunk {
            base_game_index,
            bytes: reader
                .get_mut()
                .take(unconsumed, recycle.try_recv().unwrap_or_default()),
        };
        send.send(chunk).expect("send chunk");

        base_game_index += games;
    }
}

struct Validator;

impl Visitor for Validator {
    type Tags = Option<Chess>;
    type Movetext = Chess;
    type Output = Result<(), Box<dyn Error>>;

    fn begin_tags(&mut self) -> ControlFlow<Self::Output, Self::Tags> {
        ControlFlow::Continue(None)
    }

    fn tag(
        &mut self,
        tags: &mut Self::Tags,
        name: &[u8],
        value: RawTag<'_>,
    ) -> ControlFlow<Self::Output> {
        // Support games from a non-standard starting position.
        if name == b"FEN" {
            let pos = Fen::from_ascii(&value.decode())
                .map_err(Box::from)
                .and_then(|fen| Ok(fen.into_position(CastlingMode::Chess960)?));
            match pos {
                Ok(pos) => *tags = Some(pos),
                Err(err) => return ControlFlow::Break(Err(err)),
            }
        }
        ControlFlow::Continue(())
    }

    fn begin_movetext(&mut self, tags: Self::Tags) -> ControlFlow<Self::Output, Self::Movetext> {
        ControlFlow::Continue(tags.unwrap_or_default())
    }

    fn san(
        &mut self,
        movetext: &mut Self::Movetext,
        san_plus: SanPlus,
    ) -> ControlFlow<Self::Output> {
        match san_plus.san.to_move(movetext) {
            Ok(m) => {
                movetext.play_unchecked(m);
                ControlFlow::Continue(())
            }
            Err(err) => ControlFlow::Break(Err(err.into())),
        }
    }

    fn end_game(&mut self, _movetext: Self::Movetext) -> Self::Output {
        Ok(())
    }
}

fn main() -> io::Result<ExitCode> {
    let workers = 8;

    let mut complete_success = true;

    for arg in env::args().skip(1) {
        let success = AtomicBool::new(true);

        let file = File::open(&arg)?;

        let uncompressed: Box<dyn io::Read + Send> = if arg.ends_with(".zst") {
            Box::new(zstd::Decoder::new(file)?)
        } else {
            Box::new(file)
        };

        let (send, recv) = channel::bounded::<Chunk>(2 * workers);
        let (send_recycle, recv_recycle) = channel::unbounded::<Vec<u8>>();

        thread::scope(|scope| {
            for _ in 0..workers {
                let recv = recv.clone();
                let send_recycle = send_recycle.clone();
                let success = &success;
                scope.spawn(move || {
                    for chunk in recv {
                        let mut reader = Reader::new(&chunk.bytes[..]);
                        let mut index = chunk.base_game_index;
                        while let Some(result) =
                            reader.read_game(&mut Validator).expect("in-memory i/o")
                        {
                            index += 1;
                            if let Err(err) = result {
                                eprintln!("error in game {index}: {err}");
                                success.store(false, Ordering::Relaxed);
                            }
                        }
                        let _ = send_recycle.send(chunk.bytes);
                    }
                });
            }

            split(uncompressed, send, recv_recycle)
        })?;

        let success = success.load(Ordering::Relaxed);
        println!("{}: {}", arg, if success { "success" } else { "errors" });
        complete_success &= success;
    }

    Ok(if complete_success {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    })
}
