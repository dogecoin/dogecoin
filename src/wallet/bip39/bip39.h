// Copyright (c) 2025 The Dogecoin Foundation
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef BITCOIN_WALLET_BIP39_H
#define BITCOIN_WALLET_BIP39_H
#pragma once
#include <cstddef>
#include <cstdint>

/* BIP32 512-bit seed */
#define MAX_SEED_SIZE 64
typedef uint8_t SEED [MAX_SEED_SIZE];

/* BIP39 sizes/aliases */
#define ENT_STRING_SIZE 3
typedef char ENTROPY_SIZE [ENT_STRING_SIZE];

#define MAX_HEX_ENT_SIZE (64 + 1)
typedef char HEX_ENTROPY [MAX_HEX_ENT_SIZE];

#define MAX_MNEMONIC_SIZE 1024
typedef char MNEMONIC [MAX_MNEMONIC_SIZE];

#define MAX_PASS_SIZE 256
typedef char PASS [MAX_PASS_SIZE];

#define KEY_PATH_MAX_LENGTH 255
#define KEY_PATH_MAX_SIZE (KEY_PATH_MAX_LENGTH + 1)
typedef char KEY_PATH [KEY_PATH_MAX_SIZE];

/** Verify mnemonic checksum against the given word list. Returns 0 on success, -1 on failure. */
int verifyMnemonic(const char* mnemonic,
                   const char* language,   /* e.g. "eng","spa","sc","tc","jpn","ita","fra","kor","cze","por" */
                   const char* space);      /* delimiter (UTF-8, default " " if null/empty) */

/** Derive 64-byte seed via PBKDF2-HMAC-SHA512 after NFKD normalization. Returns 0 on success, -1 on failure. */
int seedFromMnemonic(const char* mnemonic,
                     const char* passphrase, /* may be null -> "" */
                     unsigned char out_seed[MAX_SEED_SIZE]);

/** Generate a mnemonic for a given entropy size and language (header wordlists). Returns 0/-1. */
int generateMnemonic(const ENTROPY_SIZE entropy_size, /* "128","160","192","224","256" */
                     const char* language,            /* same 3-letter codes as above */
                     const char* space,               /* word separator, usually " " */
                     const char* entropy_hex,         /* optional hex entropy; if null -> random */
                     char* entropy_out_hex,           /* optional buffer to receive the used entropy (hex) */
                     size_t* mnemonic_size,           /* out: size incl. '\0' */
                     char* words_out);                /* optional out buffer (MNEMONIC) */

/** Convenience: English mnemonic with provided hex entropy or random. Returns 0/-1. */
int generateEnglishMnemonic(const HEX_ENTROPY entropy_hex,
                            const ENTROPY_SIZE size_bits,
                            MNEMONIC mnemonic);

#endif // BITCOIN_WALLET_BIP39_H
