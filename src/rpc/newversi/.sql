-- schema.sql
-- Local RPC-side index database for Tsukimarf/dogecoin (src/rpc/indexdb.cpp).
-- Engine: SQLite3. Safe to re-run (IF NOT EXISTS everywhere).
--
-- Tables:
--   index_meta            - single-row bookkeeping (schema version, sync height)
--   blocks_index           - lightweight per-block summary
--   tx_index                - lightweight per-tx summary, FK -> blocks_index
--   external_chain_refs    - cross-chain reference table (Pi Network/Stellar
--                             Soroban, Solana, Ethereum) so a dogecoin tx can
--                             be tagged as related to an event on another
--                             chain (bridge/notarization/watcher use cases).

PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS index_meta (
    id              INTEGER PRIMARY KEY CHECK (id = 1),  -- single row
    schema_version  INTEGER NOT NULL,
    synced_height   INTEGER NOT NULL DEFAULT 0,
    updated_at      INTEGER NOT NULL DEFAULT (strftime('%s','now'))
);

CREATE TABLE IF NOT EXISTS blocks_index (
    height          INTEGER PRIMARY KEY,
    hash            TEXT NOT NULL UNIQUE,
    prev_hash       TEXT,
    time            INTEGER NOT NULL,
    tx_count        INTEGER NOT NULL DEFAULT 0,
    difficulty      REAL,
    inserted_at     INTEGER NOT NULL DEFAULT (strftime('%s','now'))
);
CREATE INDEX IF NOT EXISTS idx_blocks_time ON blocks_index(time);

CREATE TABLE IF NOT EXISTS tx_index (
    txid            TEXT PRIMARY KEY,
    block_height    INTEGER NOT NULL REFERENCES blocks_index(height) ON DELETE CASCADE,
    value_out       REAL NOT NULL DEFAULT 0,
    fee             REAL,
    vin_count       INTEGER NOT NULL DEFAULT 0,
    vout_count      INTEGER NOT NULL DEFAULT 0,
    inserted_at     INTEGER NOT NULL DEFAULT (strftime('%s','now'))
);
CREATE INDEX IF NOT EXISTS idx_tx_block ON tx_index(block_height);

CREATE TABLE IF NOT EXISTS external_chain_refs (
    id              INTEGER PRIMARY KEY AUTOINCREMENT,
    doge_txid       TEXT NOT NULL REFERENCES tx_index(txid) ON DELETE CASCADE,
    chain           TEXT NOT NULL CHECK (chain IN (
                        'pi-network', 'stellar-soroban', 'solana', 'ethereum'
                    )),
    external_ref    TEXT NOT NULL,   -- foreign tx hash / contract address / ledger id
    ref_type        TEXT NOT NULL DEFAULT 'watch',  -- watch|bridge|notarization
    created_at      INTEGER NOT NULL DEFAULT (strftime('%s','now')),
    UNIQUE(doge_txid, chain, external_ref)
);
CREATE INDEX IF NOT EXISTS idx_extrefs_chain ON external_chain_refs(chain);

INSERT OR IGNORE INTO index_meta (id, schema_version, synced_height)
VALUES (1, 1, 0);

-- ---------------------------------------------------------------------
-- Seeded demo data (safe no-ops on re-run via INSERT OR IGNORE)
-- ---------------------------------------------------------------------

INSERT OR IGNORE INTO blocks_index (height, hash, prev_hash, time, tx_count, difficulty) VALUES
    (1, '82bc68038f6034c0596b6e313729793a887fded6e92a31fbdf70863f89d9bea',
        '0000000000000000000000000000000000000000000000000000000000000',
        1386474927, 1, 0.000244140625),
    (2, 'f5854be613ef6ede9c2a37b1e6a5d5c8b7c62b6c8bb8d1a0b9d17a1a3c8ec9f1',
        '82bc68038f6034c0596b6e313729793a887fded6e92a31fbdf70863f89d9bea',
        1386475020, 3, 0.000244140625);

INSERT OR IGNORE INTO tx_index (txid, block_height, value_out, fee, vin_count, vout_count) VALUES
    ('5b2a3d9e4c1f0a8b7d6e5c4b3a2918f7e6d5c4b3a2918f7e6d5c4b3a2918f7e6', 1, 500000.0, 0.0, 0, 1),
    ('a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c6d7e8f9', 2, 10000.0, 1.0, 1, 2),
    ('c9d8e7f6a5b4c3d2e1f0a9b8c7d6e5f4a3b2c1d0e9f8a7b6c5d4e3f2a1b0c9d8', 2, 2500.5, 0.5, 1, 2);

INSERT OR IGNORE INTO external_chain_refs (doge_txid, chain, external_ref, ref_type) VALUES
    ('a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c6d7e8f9',
        'pi-network', 'PI_TX_0f3a9c2b8e1d4f5a', 'bridge'),
    ('c9d8e7f6a5b4c3d2e1f0a9b8c7d6e5f4a3b2c1d0e9f8a7b6c5d4e3f2a1b0c9d8',
        'stellar-soroban', 'CD7X2K...SOROBAN_CONTRACT_REF', 'notarization');

UPDATE index_meta SET synced_height = 2, updated_at = strftime('%s','now') WHERE id = 1;
