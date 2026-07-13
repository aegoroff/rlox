# Rules for rlox project

**rlox** is a Rust implementation of the Lox programming language from [Crafting Interpreters book](https://craftinginterpreters.com/), with only bytecode virtual machine implemented. For user-facing documentation (CLI options, examples), see [README.md](README.md).

## Development

All commands are run from the repository root.

```sh
# Build (debug)
cargo build

# Build release (LTO + strip + panic=abort per release profile)
cargo build --release

# Tests
cargo test

# Format
cargo fmt

# Lint (must pass before finishing)
cargo clippy -- -W clippy::pedantic
```

### Toolchain

- Rust **1.88.0** or newer
- Edition **2024**

## Coding Conventions

### Do

- Add tests for new functionality.
- Write tests in the **AAA** pattern (Arrange, Act, Assert).
- Use the `test-case` crate when tests can be parameterized.
- Place tests in `#[cfg(test)]` modules inside the same source file (there is no `tests/` integration directory).
- Write code comments in **English** only.
- Run `cargo fmt` and ensure `cargo clippy -- -W clippy::pedantic` passes before finishing.

### Don't

- Don't use `unsafe` code.
- Don't use `unwrap` or `expect` on `Option` or `Result` in non-test code. Exceptions: compile-time invariants such as valid regex literals in `LazyLock::new` or `Regex::new`.
- Don't add concurrency (`spawn`, threads, parallel iterators). Async I/O via `tokio` is the established pattern.
- Don't write tests that check performance, copy-paste duplication, or architecture — focus on behavior.
- Don't suppress clippy warnings without a good reason. Existing exceptions in the codebase (`too_many_lines`, `cast_precision_loss`) are acceptable for specific cases; don't add new `#[allow(...)]` casually.
- Don't write trivial comments that restate what the code already says.

## Error Handling

The project uses `miette` for error reporting:

- Propagate errors with `?`.
