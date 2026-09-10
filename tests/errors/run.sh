#!/bin/sh
#
# Runs the error tests under tnylpo and checks each against the output its
# own source says it should produce. Every one of these terminates on
# purpose, so "passed" means: it printed exactly what the header promised,
# no more and no less.
#
# The expectation is taken from the test itself, from the block introduced
# by "Expected output:" in the header comment -- keeping it next to the code
# it describes rather than in a table somewhere else.
#
# overnest is skipped for the same reason as in build.sh: it needs overlays,
# which CP/M does not have.
#
# Usage: ./run.sh [directory ...]       (default: the tests next to this script)
#        Build first with ./build.sh

cd "$(dirname "$0")"

DIRS=${*:-"."}
SKIP="overnest"

passed=0
failed=0
failures=""

for dir in $DIRS; do
    [ -d "$dir" ] || { echo "no such directory: $dir" >&2; exit 1; }

    for src in "$dir"/*.pas; do
        [ -e "$src" ] || continue
        name=$(basename "$src" .pas)
        com="$dir/$name.com"

        case " $SKIP " in *" $name "*) continue;; esac

        if [ ! -f "$com" ]; then
            failed=$((failed + 1))
            failures="$failures\n  $dir/$name: not built"
            echo "  $name: NOT BUILT"
            continue
        fi

        # What the header says it should print.
        expected=$(awk '
            /Expected output:/ && !seen { seen = 1; match($0, /^[ \t]*/); base = RLENGTH; next }
            seen { match($0, /^[ \t]*/); if (RLENGTH <= base) exit; print substr($0, RLENGTH + 1) }
        ' "$src")

        if [ -z "$expected" ]; then
            failed=$((failed + 1))
            failures="$failures\n  $dir/$name: header has no \"Expected output:\" block"
            echo "  $name: NO EXPECTATION"
            continue
        fi

        # Trailing blank lines and trailing spaces are not interesting here.
        actual=$(tnylpo "$com" 2>/dev/null | sed 's/[ \t]*$//')
        expected=$(printf '%s\n' "$expected" | sed 's/[ \t]*$//')

        if [ "$actual" = "$expected" ]; then
            passed=$((passed + 1))
            echo "  $name: ok"
        else
            failed=$((failed + 1))
            failures="$failures\n  $dir/$name"
            echo "  $name: FAILED"
            echo "      expected: $(printf '%s' "$expected" | tr '\n' '|')"
            echo "      actual:   $(printf '%s' "$actual" | tr '\n' '|')"
        fi

        rm -f "$dir"/*.tmp
    done
done

echo
echo "************************"
echo "Passed tests: $passed"
echo "Failed tests: $failed"
echo "************************"
echo

if [ "$failed" -ne 0 ]; then
    printf 'Failing:%b\n\n' "$failures"
    exit 1
fi
