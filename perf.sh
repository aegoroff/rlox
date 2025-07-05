TARGET=$1
BIN_PATH="/home/egr/code/rlox/target/release/rlox"
REF_PATH="/home/egr/code/craftinginterpreters/build/clox"

function perf() {
	target=$1
	tool=$2
	ref_key=$3
	$tool "$BIN_PATH c f $target" $ref_key "$REF_PATH $target"
}

perf $TARGET poop
perf $TARGET hyperfine --reference
