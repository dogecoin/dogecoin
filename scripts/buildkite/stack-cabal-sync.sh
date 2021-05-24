#!/bin/sh
set -eu

# This script checks that the `stack.yaml` and `cabal.project` files have
# consistent git hashes for the packages they depend on. We use
# `cardano-repo-tool`'s `update-cabal-project` command which modifies
# `cabal.project` to be consistent with `stack.yaml`s versions. If the
# diff is non-empty, we know they're out of sync.

# Check that functions are defined.
HELP_TEXT="cardano-repo-tool not found."
type cardano-repo-tool > /dev/null 2>&1 || { echo "${HELP_TEXT}"; exit 1; }
HELP_TEXT="git not found."
type git > /dev/null 2>&1 || { echo "${HELP_TEXT}"; exit 1; }

# Update `cabal.project` from the `stack.yaml` file.
cardano-repo-tool update-cabal-project

git diff cabal.project | tee stack-cabal.patch

if test "$(wc -l < stack-cabal.patch)" -gt 0 ; then
  buildkite-agent artifact upload stack-cabal.patch --job "$BUILDKITE_JOB_ID"
  exit 1
  fi

exit 0
