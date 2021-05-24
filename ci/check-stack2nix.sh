#!/usr/bin/env nix-shell
#!nix-shell -i bash -p git bash nixStable

 # Check and warn if stack-to-nix auto-generated code is out of date.

 set -xe

 fail_stack2nix_check() {
  git diff -w --text > /tmp/stack2nix.patch
  buildkite-agent artifact upload /tmp/stack2nix.patch --job "$BUILDKITE_JOB_ID"
  echo "ERROR: you need to (run ./nix/regenerate.sh or apply the patch in the buildkite artifact) and commit the changes" >&2
  exit 1
}

 # Get relative path to script directory
scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")

 "${scriptDir}/../nix/regenerate.sh"

 git diff -w --text --exit-code || fail_stack2nix_check
