#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2154,SC2155,SC2034,SC2039,SC2240,SC2124

. "$(dirname "$0")"/common.sh
. "$(dirname "$0")"/lib-cli.sh

outdir=${1?scripts.genesis.Error: no output directory specified}
start_future_offset=${2:-1 minute}

genesis "${outdir}" "${start_future_offset}"
