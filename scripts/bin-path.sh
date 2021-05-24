#!/usr/bin/env bash

# Find the path to the built executable in the cabal plan.

exe="$1"

if which jq > /dev/null; then
  bin="$(jq -r '."install-plan"[] | select(."component-name" == "exe:'$exe'") | ."bin-file"' dist-newstyle/cache/plan.json | head -n 1)"

  if [ -f "$bin" ]; then
    echo "$bin"
  else
    echo "Error: $exe not built" 1>&2
    exit 1
  fi
else
  echo "Error: jq not installed" 1>&2
  exit 1
fi
