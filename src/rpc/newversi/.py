#!/usr/bin/env python3
"""
install.py — initialize the Tsukimarf/dogecoin RPC index database.

Usage:
    python3 install.py [--db PATH] [--schema PATH] [--reset]

Creates (or updates) a SQLite database at --db using schema.sql, then
verifies the seeded demo rows and prints a summary. Safe to re-run.
"""
import argparse
import sqlite3
import sys
from pathlib import Path

DEFAULT_DB = "dogeindex.sqlite3"
DEFAULT_SCHEMA = "schema.sql"


def install(db_path: Path, schema_path: Path, reset: bool) -> None:
    if reset and db_path.exists():
        db_path.unlink()
        print(f"[install.py] removed existing database at {db_path}")

    if not schema_path.exists():
        print(f"[install.py] ERROR: schema file not found: {schema_path}", file=sys.stderr)
        sys.exit(1)

    schema_sql = schema_path.read_text(encoding="utf-8")

    conn = sqlite3.connect(str(db_path))
    try:
        conn.execute("PRAGMA foreign_keys = ON;")
        conn.executescript(schema_sql)
        conn.commit()

        cur = conn.cursor()
        cur.execute("SELECT schema_version, synced_height FROM index_meta WHERE id = 1;")
        version, height = cur.fetchone()

        cur.execute("SELECT COUNT(*) FROM blocks_index;")
        n_blocks = cur.fetchone()[0]

        cur.execute("SELECT COUNT(*) FROM tx_index;")
        n_tx = cur.fetchone()[0]

        cur.execute("SELECT COUNT(*) FROM external_chain_refs;")
        n_refs = cur.fetchone()[0]

        print(f"[install.py] database ready: {db_path}")
        print(f"[install.py] schema_version={version} synced_height={height}")
        print(f"[install.py] blocks_index={n_blocks} tx_index={n_tx} external_chain_refs={n_refs}")
    finally:
        conn.close()


def main() -> None:
    parser = argparse.ArgumentParser(description="Initialize the RPC index database.")
    parser.add_argument("--db", type=Path, default=Path(DEFAULT_DB), help="Path to the SQLite database file.")
    parser.add_argument("--schema", type=Path, default=Path(__file__).parent / DEFAULT_SCHEMA,
                         help="Path to schema.sql.")
    parser.add_argument("--reset", action="store_true", help="Delete any existing database first.")
    args = parser.parse_args()

    install(args.db, args.schema, args.reset)


if __name__ == "__main__":
    main()
