all: target/debug/fuzz-fen target/debug/fuzz-uci

.PHONY: fuzz-fen
fuzz-fen: target/debug/fuzz-fen
	./target/debug/fuzz-fen corpora/fen

.PHONY: fuzz-uci
fuzz-uci: target/debug/fuzz-uci
	./target/debug/fuzz-uci corpora/uci

.PHONY: target/debug/fuzz-fen
target/debug/fuzz-fen:
	cargo rustc --bin fuzz-fen -- -C passes='sancov' -C llvm-args='-sanitizer-coverage-level=3' -Z sanitizer=address

.PHONY: target/debug/fuzz-uci
target/debug/fuzz-uci:
	cargo rustc --bin fuzz-uci -- -C passes='sancov' -C llvm-args='-sanitizer-coverage-level=3' -Z sanitizer=address
