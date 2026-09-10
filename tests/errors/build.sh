#!/bin/sh
#
# Builds the error tests. Each of these terminates on purpose, so they cannot
# be assertions inside one of the suites -- one program per case is the point.
#
# Only CP/M for now. overnest needs overlays, which CP/M does not have, so it
# is skipped rather than silently building into something that proves nothing.
#
# Usage: ./build.sh [directory ...]      (default: the tests next to this script)

set -e
cd "$(dirname "$0")"

DIRS=${*:-"."}
FLAGS="--cpm --dep --opt"
SKIP="overnest"

built=0
failed=0

for dir in $DIRS; do
    [ -d "$dir" ] || { echo "no such directory: $dir" >&2; exit 1; }

    for src in "$dir"/*.pas; do
        [ -e "$src" ] || continue
        name=$(basename "$src" .pas)

        case " $SKIP " in *" $name "*) continue;; esac

        if out=$(pasta $FLAGS "$src" 2>&1); then
            built=$((built + 1))
        else
            failed=$((failed + 1))
            echo "FAILED TO BUILD: $src"
            echo "$out" | grep '\*\*\* Error' || echo "$out" | tail -3
        fi
    done
done

echo
echo "Built $built, failed $failed"
[ "$failed" -eq 0 ]
