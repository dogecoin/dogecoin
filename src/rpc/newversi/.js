#!/usr/bin/env node
/**
 * install.js — initialize the Tsukimarf/dogecoin RPC index database.
 *
 * Usage:
 *   node install.js [--db PATH] [--schema PATH] [--reset]
 *
 * Dependency: better-sqlite3 (npm install better-sqlite3)
 */
'use strict';

const fs = require('fs');
const path = require('path');

function parseArgs(argv) {
  const opts = {
    db: path.join(process.cwd(), 'dogeindex.sqlite3'),
    schema: path.join(__dirname, 'schema.sql'),
    reset: false,
  };
  for (let i = 0; i < argv.length; i++) {
    switch (argv[i]) {
      case '--db':
        opts.db = argv[++i];
        break;
      case '--schema':
        opts.schema = argv[++i];
        break;
      case '--reset':
        opts.reset = true;
        break;
      default:
        console.error(`[install.js] unknown argument: ${argv[i]}`);
        process.exit(1);
    }
  }
  return opts;
}

function main() {
  const opts = parseArgs(process.argv.slice(2));

  let Database;
  try {
    Database = require('better-sqlite3');
  } catch (err) {
    console.error(
      '[install.js] missing dependency "better-sqlite3". Install it with:\n' +
      '  npm install better-sqlite3'
    );
    process.exit(1);
  }

  if (opts.reset && fs.existsSync(opts.db)) {
    fs.unlinkSync(opts.db);
    console.log(`[install.js] removed existing database at ${opts.db}`);
  }

  if (!fs.existsSync(opts.schema)) {
    console.error(`[install.js] ERROR: schema file not found: ${opts.schema}`);
    process.exit(1);
  }

  const schemaSql = fs.readFileSync(opts.schema, 'utf8');
  const db = new Database(opts.db);

  try {
    db.pragma('foreign_keys = ON');
    db.exec(schemaSql);

    const meta = db.prepare('SELECT schema_version, synced_height FROM index_meta WHERE id = 1').get();
    const nBlocks = db.prepare('SELECT COUNT(*) AS n FROM blocks_index').get().n;
    const nTx = db.prepare('SELECT COUNT(*) AS n FROM tx_index').get().n;
    const nRefs = db.prepare('SELECT COUNT(*) AS n FROM external_chain_refs').get().n;

    console.log(`[install.js] database ready: ${opts.db}`);
    console.log(`[install.js] schema_version=${meta.schema_version} synced_height=${meta.synced_height}`);
    console.log(`[install.js] blocks_index=${nBlocks} tx_index=${nTx} external_chain_refs=${nRefs}`);
  } finally {
    db.close();
  }
}

main();

module.exports = { parseArgs };
