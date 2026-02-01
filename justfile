build:
  cargo build --workspace --all-features

test:
  cargo nextest run --workspace --all-features
