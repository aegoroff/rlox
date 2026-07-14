# Rules for rlox project

**rlox** is a Rust implementation of the Lox programming language from [Crafting Interpreters](https://craftinginterpreters.com/). It provides two execution modes: a tree-walk interpreter and a bytecode compiler with a virtual machine. For user-facing documentation (CLI options, examples), see [README.md](README.md).

## Architecture

Cargo workspace with four crates. Dependency flow: `scanner` → `interpreter` / `compiler` → `rlox` (CLI).

| Crate | Role |
|-------|------|
| `scanner` | Lexical analyzer — tokenizes Lox source |
| `interpreter` | Tree-walk interpreter: parser → AST → resolver → evaluator |
| `compiler` | Bytecode compiler + VM (`compile`, `chunk`, `vm`, `object`, `value`) |
| `rlox` | CLI binary (`cargo run` default package) |

### Execution paths

- **Interpret** (`rlox n [PATH]`): `Parser` → `Resolver` → `Interpreter`
- **Compile** (`rlox c [--printcode] [PATH]`): `VirtualMachine::interpret` (scan → compile → execute)

### Key modules

- `interpreter/src/ast.rs` — AST node types
- `interpreter/src/parser.rs` — recursive-descent parser
- `interpreter/src/resolver.rs` — static analysis (scopes, `this`/`super`)
- `interpreter/src/int.rs` — tree-walk evaluator
- `compiler/src/compile.rs` — bytecode emitter
- `compiler/src/vm.rs` — stack-based VM
- `compiler/src/chunk.rs` — bytecode + disassembly
- `compiler/src/object.rs`, `value.rs` — heap objects and tagged values

## Development

All commands are run from the repository root.

```sh
# Build (debug)
cargo build

# Build release (LTO + strip + panic=abort per release profile)
cargo build --release

# Tests (unit tests in #[cfg(test)] modules)
cargo test

# Tests via nextest (used in justfile)
cargo nextest run --workspace --all-features

# Format
cargo fmt

# Lint (must pass before finishing)
cargo clippy -- -W clippy::pedantic
```

`just build` and `just test` wrap the workspace build and nextest commands.

### Toolchain

- Rust **1.97.0** or newer (CI uses `registry.egoroff.spb.ru/rustcross:1.97`)
- Edition **2024**
- `unsafe` is forbidden at workspace level (`[workspace.lints.rust]`), except in the **`compiler`** crate where scoped `unsafe` is allowed for VM hot-path optimizations (unchecked stack / bytecode reads). Document invariants at each site.

### Reference tests

`test-lox.sh` runs [Crafting Interpreters](https://github.com/munificent/craftinginterpreters) conformance tests against the bytecode VM (`rlox c`). Requires a release build and the craftinginterpreters repo:

```sh
cargo build --release
CRAFTING_INTERPRETERS=/path/to/craftinginterpreters ./test-lox.sh [filter]
```

### Cross-compilation

Release builds for multiple targets are configured in `Cross.toml`, `build.sh`, and `.forgejo/workflows/ci.yaml` (zigbuild / xwin).

## Coding Conventions

### Do

- Add tests for new functionality.
- Write tests in the **AAA** pattern (Arrange, Act, Assert).
- Use the `test-case` crate when tests can be parameterized.
- Place tests in `#[cfg(test)]` modules inside the same source file (there is no `tests/` integration directory).
- Write code comments in **English** only.
- Run `cargo fmt` and ensure `cargo clippy -- -W clippy::pedantic` passes before finishing.
- Use `miette::LabeledSpan` for errors that benefit from source highlighting.

### Don't

- Don't use `unsafe` outside the `compiler` crate. Inside `compiler`, keep `unsafe` minimal, document invariants, and prefer safe alternatives when they are equally fast.
- Don't use `unwrap` or `expect` on `Option` or `Result` in non-test code. Exceptions: compile-time invariants such as valid regex literals in `LazyLock::new` or `Regex::new`.
- Don't add concurrency (`spawn`, threads, parallel iterators).
- Don't write tests that check performance, copy-paste duplication, or architecture — focus on behavior.
- Don't suppress clippy warnings without a good reason. Existing crate-level exceptions (`missing_errors_doc`, `cast_precision_loss`, `cast_possible_wrap`, `cast_possible_truncation`, `borrowed_box`) are acceptable for specific cases; don't add new `#[allow(...)]` casually.
- Don't write trivial comments that restate what the code already says.

## Error Handling

The project uses `miette` for rich error reporting. Propagate errors with `?`.

| Layer | Error type | Notes |
|-------|-----------|-------|
| `scanner` | `miette::Result<T>` | `LabeledSpan` on scan errors |
| `interpreter` | `LoxError` | `Error(miette::Report)` for diagnostics; `Return(LoxValue)` for control flow |
| `compiler` | `miette::Report` | `RuntimeError` enum converted to reports in VM |
| `rlox` CLI | `miette::Result<()>` | Attaches source code via `.with_source_code(content)` |

In the CLI, interpreter errors are mapped to attach the original source:

```rust
resolver.interpret(&stmts).map_err(|err| match err {
    LoxError::Error(e) => e.with_source_code(content),
    LoxError::Return(val) => miette!("Unexpected return value: {val}"),
})
```

## Testing Patterns

VM and interpreter tests typically compile/interpret a Lox snippet and assert on printed output:

```rust
#[cfg(test)]
mod tests {
    use test_case::test_case;

    #[test_case("print 1 + 2;", "3")]
    fn example(source: &str, expected: &str) {
        // Arrange: create VM/interpreter with captured stdout
        // Act: run source
        // Assert: output matches expected
    }
}
```

See `compiler/src/vm.rs` and `interpreter/src/int.rs` for extensive examples.
