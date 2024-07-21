#!/usr/bin/env fish
cargo watch -c -s 'tree-sitter generate && tree-sitter build && tree-sitter test -f "terra"'  -w grammar.js -w 'test/corpus/statements.txt' --why
