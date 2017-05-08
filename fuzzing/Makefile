.PHONY: fuzz-fen
fuzz-fen: target/debug/fuzz-fen
	./target/debug/fuzz-fen corpora/fen

.PHONY: fuzz-uci
fuzz-uci: target/debug/fuzz-uci
	./target/debug/fuzz-uci corpora/uci -max_len=16

.PHONY: fuzz-san
fuzz-san: target/debug/fuzz-san
	./target/debug/fuzz-san corpora/san -max_len=16

.PHONY: target/debug/fuzz-fen
target/debug/fuzz-fen:
	cargo rustc --bin fuzz-fen -- -C passes='sancov' -C llvm-args='-sanitizer-coverage-level=3' -Z sanitizer=address

.PHONY: target/debug/fuzz-uci
target/debug/fuzz-uci:
	cargo rustc --bin fuzz-uci -- -C passes='sancov' -C llvm-args='-sanitizer-coverage-level=3' -Z sanitizer=address

.PHONY: target/debug/fuzz-san
target/debug/fuzz-san:
	cargo rustc --bin fuzz-san -- -C passes='sancov' -C llvm-args='-sanitizer-coverage-level=3' -Z sanitizer=address
