#!/bin/bash

# Benchmark script for comparing Lox implementations using poop
# Usage: ./bench.sh <lox_file> [<lox_file> ...]

if [ $# -eq 0 ]; then
    echo "Usage: $0 <lox_file> [<lox_file> ...]"
    exit 1
fi

for LOX_FILE in "$@"; do
    if [ ! -f "$LOX_FILE" ]; then
        echo "Error: File '$LOX_FILE' not found"
        exit 1
    fi
done

for LOX_FILE in "$@"; do
    echo "----------- Benchmarking: $LOX_FILE ----------"
    poop \
        "./target/release/rlox c $LOX_FILE" \
        "/home/egr/code/craftinginterpreters/build/clox $LOX_FILE"
    echo
done
