#!/usr/bin/env bash
# install.sh — initialize the Tsukimarf/dogecoin RPC index database using
# the sqlite3 CLI directly (no Node/Python dependency). Mirrors install.py
# and install.js; use whichever fits your toolchain.
#
# Usage: ./install.sh [DB_PATH] [SCHEMA_PATH] [--reset]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DB_PATH="${1:-dogeindex.sqlite3}"
SCHEMA_PATH="${2:-$SCRIPT_DIR/schema.sql}"
RESET=false

for arg in "$@"; do
  if [[ "$arg" == "--reset" ]]; then
    RESET=true
  fi
done

if ! command -v sqlite3 >/dev/null 2>&1; then
  echo "[install.sh] ERROR: sqlite3 CLI not found on PATH." >&2
  exit 1
fi

if [[ ! -f "$SCHEMA_PATH" ]]; then
  echo "[install.sh] ERROR: schema file not found: $SCHEMA_PATH" >&2
  exit 1
fi

if [[ "$RESET" == true && -f "$DB_PATH" ]]; then
  rm -f "$DB_PATH"
  echo "[install.sh] removed existing database at $DB_PATH"
fi

sqlite3 "$DB_PATH" < "$SCHEMA_PATH"

echo "[install.sh] database ready: $DB_PATH"
sqlite3 "$DB_PATH" \
  "SELECT 'schema_version=' || schema_version || ' synced_height=' || synced_height FROM index_meta WHERE id = 1;"
sqlite3 "$DB_PATH" \
  "SELECT 'blocks_index=' || (SELECT COUNT(*) FROM blocks_index) \
    || ' tx_index=' || (SELECT COUNT(*) FROM tx_index) \
    || ' external_chain_refs=' || (SELECT COUNT(*) FROM external_chain_refs);"
