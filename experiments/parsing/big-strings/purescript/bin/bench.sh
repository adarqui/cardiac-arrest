#!/bin/sh -e

PATH="./bin/:$PATH"

bench '["purescript-string-parsers", "purescript-simple-parser"]' --output assets/benchmarks.html --time-limit 1

wkhtmltoimage assets/benchmarks.html assets/benchmarks.png
