// Copyright (c) 2025 The Dogecoin Foundation
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "wallet/bip39/bip39.h"

#include "crypto/sha256.h"
#include "crypto/hmac_sha512.h"
#include "random.h"
#include "support/allocators/secure.h"

#include <algorithm>
#include <array>
#include <cctype>
#include <cstdint>
#include <cstring>
#include <string>
#include <unordered_map>
#include <vector>

extern "C" {
#define UTF8PROC_STATIC 1
#include "wallet/bip39/utf8proc/utf8proc.h"
}

#include "wallet/bip39/wordlists/index.h"

namespace {

constexpr int BIP39_BITS_PER_WORD = 11;
constexpr int BIP39_WORDS = 2048;

static std::string NFKD(const std::string& s) {
    if (s.empty()) return {};
    utf8proc_uint8_t* out = utf8proc_NFKD(reinterpret_cast<const utf8proc_uint8_t*>(s.c_str()));
    if (!out) return {};
    std::string res(reinterpret_cast<char*>(out));
    free(out);
    return res;
}

static void PBKDF2_HMAC_SHA512(const unsigned char* pass, size_t passlen,
                               const unsigned char* salt, size_t saltlen,
                               int iterations, unsigned char out[64]) {
    unsigned char u[CHMAC_SHA512::OUTPUT_SIZE];
    unsigned char t[CHMAC_SHA512::OUTPUT_SIZE];
    // U1
    {
        CHMAC_SHA512 h(pass, passlen);
        h.Write(salt, saltlen);
        static const unsigned char be1[4] = {0,0,0,1};
        h.Write(be1, 4);
        h.Finalize(u);
        std::memcpy(t, u, 64);
    }
    // U2..Uc
    for (int i = 2; i <= iterations; ++i) {
        CHMAC_SHA512 hi(pass, passlen);
        hi.Write(u, 64);
        hi.Finalize(u);
        for (int j = 0; j < 64; ++j) t[j] ^= u[j];
    }
    std::memcpy(out, t, 64);
}

static inline const char* const* LangToArray(const std::string& in) {
    // Accept the 3-letter codes (matching get_words)
    const std::string s = [&]{
        std::string t; t.reserve(in.size());
        for (unsigned char c : in) t.push_back(std::tolower(c));
        return t;
    }();
    if (s == "eng") return wordlist_eng;
    if (s == "spa") return wordlist_spa;
    if (s == "jpn") return wordlist_jpn;
    if (s == "ita") return wordlist_ita;
    if (s == "fra") return wordlist_fra;
    if (s == "kor") return wordlist_kor;
    if (s == "sc"  || s == "chs") return wordlist_sc;   // simplified Chinese
    if (s == "tc"  || s == "cht") return wordlist_tc;   // traditional Chinese
    if (s == "cze" || s == "ces") return wordlist_cze;
    if (s == "por") return wordlist_por;
    return nullptr;
}

static std::vector<std::string> SplitWords(const std::string& s, const std::string& sep) {
    std::vector<std::string> out;
    size_t pos = 0, prev = 0;
    while ((pos = s.find(sep, prev)) != std::string::npos) {
        if (pos > prev) out.emplace_back(s.substr(prev, pos - prev));
        prev = pos + sep.size();
    }
    if (prev < s.size()) out.emplace_back(s.substr(prev));
    return out;
}

static bool LoadWordlist(const char* language,
                         std::vector<std::string>& out) {
    out.clear();
    if (!(language && *language)) return false;

    const char* const* wl = LangToArray(language);
    if (!wl) return false;

    out.reserve(BIP39_WORDS);
    for (int i = 0; i < BIP39_WORDS; ++i) out.emplace_back(wl[i]);
    return out.size() == BIP39_WORDS;
}

static bool MnemonicChecksumOK(const std::vector<std::string>& words,
                               const std::vector<std::string>& wordlist) {
    const size_t N = words.size();
    if (N < 12 || N > 24 || (N % 3) != 0) return false;

    std::unordered_map<std::string,int> index;
    index.reserve(BIP39_WORDS);
    for (int i = 0; i < BIP39_WORDS; ++i) index.emplace(wordlist[i], i);

    std::string bits; bits.reserve(N * BIP39_BITS_PER_WORD);
    for (const auto& w : words) {
        auto it = index.find(w);
        if (it == index.end()) return false;
        for (int b = 10; b >= 0; --b)
            bits.push_back(((it->second >> b) & 1) ? '1' : '0');
    }

    const int total_bits    = int(bits.size());           // N*11
    const int checksum_bits = total_bits / 33;
    const int entropy_bits  = total_bits - checksum_bits;
    const int entropy_bytes = entropy_bits / 8;

    std::vector<unsigned char> entropy(entropy_bytes, 0);
    for (int i = 0; i < entropy_bytes; ++i) {
        unsigned char byte = 0;
        for (int b = 0; b < 8; ++b)
            if (bits[i*8 + b] == '1') byte |= (1u << (7 - b));
        entropy[i] = byte;
    }

    unsigned char hash[32];
    CSHA256().Write(entropy.data(), entropy.size()).Finalize(hash);

    for (int i = 0; i < checksum_bits; ++i) {
        char exp = ((hash[0] >> (7 - i)) & 1) ? '1' : '0';
        if (bits[entropy_bits + i] != exp) return false;
    }
    return true;
}

static bool HexToBytes(const char* hex, std::vector<unsigned char>& out) {
    out.clear();
    if (!hex) return false;
    const size_t n = std::strlen(hex);
    if (n % 2) return false;
    out.reserve(n / 2);
    auto fromHex = [](char c)->int {
        if (c >= '0' && c <= '9') return c - '0';
        if (c >= 'a' && c <= 'f') return c - 'a' + 10;
        if (c >= 'A' && c <= 'F') return c - 'A' + 10;
        return -1;
    };
    for (size_t i = 0; i < n; i += 2) {
        int hi = fromHex(hex[i]);
        int lo = fromHex(hex[i+1]);
        if (hi < 0 || lo < 0) return false;
        out.push_back((unsigned char)((hi << 4) | lo));
    }
    return true;
}

static void BytesToHex(const unsigned char* p, size_t len, char* out /*len*2+1*/) {
    static const char* kHex = "0123456789abcdef";
    for (size_t i = 0; i < len; ++i) {
        out[2*i]   = kHex[(p[i] >> 4) & 0xF];
        out[2*i+1] = kHex[p[i] & 0xF];
    }
    out[2*len] = '\0';
}

static bool EntropySizeOK(const char* s, int& bits_out) {
    if (!s) return false;
    if (std::strcmp(s, "128")==0) { bits_out = 128; return true; }
    if (std::strcmp(s, "160")==0) { bits_out = 160; return true; }
    if (std::strcmp(s, "192")==0) { bits_out = 192; return true; }
    if (std::strcmp(s, "224")==0) { bits_out = 224; return true; }
    if (std::strcmp(s, "256")==0) { bits_out = 256; return true; }
    return false;
}

} // namespace

int verifyMnemonic(const char* mnemonic,
                   const char* language,
                   const char* space) {
    if (!mnemonic || !language) return -1;
    std::vector<std::string> wl;
    if (!LoadWordlist(language, wl)) return -1;

    const std::string sep = (space && *space) ? std::string(space) : std::string(" ");
    auto words = SplitWords(std::string(mnemonic), sep);
    return MnemonicChecksumOK(words, wl) ? 0 : -1;
}

int seedFromMnemonic(const char* mnemonic,
                     const char* passphrase,
                     unsigned char out_seed[MAX_SEED_SIZE]) {
    if (!mnemonic || !out_seed) return -1;
    const std::string m_nfkd = NFKD(std::string(mnemonic));
    if (m_nfkd.empty() && *mnemonic) return -1;
    const std::string p = passphrase ? passphrase : "";
    const std::string salt_nfkd = NFKD(std::string("mnemonic") + p);
    if (salt_nfkd.empty() && (!p.empty())) return -1;

    PBKDF2_HMAC_SHA512(reinterpret_cast<const unsigned char*>(m_nfkd.data()), m_nfkd.size(),
                       reinterpret_cast<const unsigned char*>(salt_nfkd.data()), salt_nfkd.size(),
                       2048, out_seed);
    return 0;
}

int generateMnemonic(const ENTROPY_SIZE entropy_size,
                     const char* language,
                     const char* space,
                     const char* entropy_hex,   // optional
                     char* entropy_out_hex,     // optional
                     size_t* mnemonic_size,     // out (size incl '\0')
                     char* words_out) {
    if (!language || !entropy_size || !mnemonic_size) return -1;

    int ent_bits = 0;
    if (!EntropySizeOK(entropy_size, ent_bits)) return -1;
    const int ent_bytes = ent_bits / 8;
    const int cs_bits   = ent_bits / 32;  // BIP39: CS = ENT / 32
    const int total_bits = ent_bits + cs_bits;
    const int words = total_bits / 11;

    std::vector<std::string> wl;
    if (!LoadWordlist(language, wl)) return -1;

    // 1) obtain entropy
    std::vector<unsigned char, secure_allocator<unsigned char>> entropy(ent_bytes);
    if (entropy_hex && *entropy_hex) {
        std::vector<unsigned char> tmp;
        if (!HexToBytes(entropy_hex, tmp) || (int)tmp.size() != ent_bytes) return -1;
        std::copy(tmp.begin(), tmp.end(), entropy.begin());
    } else {
        GetStrongRandBytes(entropy.data(), ent_bytes);
    }
    if (entropy_out_hex) {
        BytesToHex(entropy.data(), ent_bytes, entropy_out_hex);
    }

    // 2) compute checksum bits
    unsigned char sha[32];
    CSHA256().Write(entropy.data(), entropy.size()).Finalize(sha);

    // 3) build bitstring ENT||CS
    std::string bits; bits.reserve(total_bits);
    for (int i = 0; i < ent_bytes; ++i) {
        for (int b = 7; b >= 0; --b) bits.push_back(((entropy[i] >> b) & 1) ? '1' : '0');
    }
    for (int i = 0; i < cs_bits; ++i) {
        bits.push_back(((sha[0] >> (7 - i)) & 1) ? '1' : '0');
    }

    // 4) split to 11-bit indices and join words
    std::string sep = (space && *space) ? std::string(space) : std::string(" ");
    std::string out;
    out.reserve(words * 8 * 5); // rough

    for (int w = 0; w < words; ++w) {
        int idx = 0;
        for (int b = 0; b < 11; ++b) idx = (idx << 1) | (bits[w*11 + b] == '1' ? 1 : 0);
        if (idx < 0 || idx >= BIP39_WORDS) return -1;
        if (w) out += sep;
        out += wl[idx];
    }

    // size reporting / buffer copy
    *mnemonic_size = out.size() + 1;
    if (words_out) {
        std::memcpy(words_out, out.c_str(), out.size() + 1);
    }
    // wipe sensitive material
    memory_cleanse(entropy.data(), entropy.size());
    return 0;
}

int generateEnglishMnemonic(const HEX_ENTROPY entropy_hex,
                            const ENTROPY_SIZE size_bits,
                            MNEMONIC mnemonic) {
    size_t msz = 0;
    return generateMnemonic(size_bits, "eng", " ", entropy_hex,
                            nullptr, &msz, mnemonic);
}
