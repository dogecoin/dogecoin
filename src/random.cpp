// Copyright (c) 2009-2010 Satoshi Nakamoto
// Copyright (c) 2009-2016 The Bitcoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "random.h"

#include "crypto/sha512.h"
#include "support/cleanse.h"
#ifdef WIN32
#include "compat.h" // for Windows API
#include <wincrypt.h>
#endif
#include "util.h"             // for LogPrint()
#include "utilstrencodings.h" // for GetTime()

#include <stdlib.h>
#include <limits>

#ifndef WIN32
#include <sys/time.h>
#endif

#include <openssl/err.h>
#include <openssl/rand.h>
#include <lowlevel.h>

static void RandFailure()
{
    LogPrintf("Failed to read randomness, aborting\n");
    abort();
}

static inline int64_t GetPerformanceCounter()
{
    int64_t nCounter = 0;
#ifdef WIN32
    QueryPerformanceCounter((LARGE_INTEGER*)&nCounter);
#else
    timeval t;
    gettimeofday(&t, NULL);
    nCounter = (int64_t)(t.tv_sec * 1000000 + t.tv_usec);
#endif
    return nCounter;
}

void RandAddSeed()
{
    // Seed with CPU performance counter
    int64_t nCounter = GetPerformanceCounter();
    RAND_add(&nCounter, sizeof(nCounter), 1.5);
    memory_cleanse((void*)&nCounter, sizeof(nCounter));
}

static void RandAddSeedPerfmon()
{
    RandAddSeed();

#ifdef WIN32
    // Don't need this on Linux, OpenSSL automatically uses /dev/urandom
    // Seed with the entire set of perfmon data

    // This can take up to 2 seconds, so only do it every 10 minutes
    static int64_t nLastPerfmon;
    if (GetTime() < nLastPerfmon + 10 * 60)
        return;
    nLastPerfmon = GetTime();

    std::vector<unsigned char> vData(250000, 0);
    long ret = 0;
    unsigned long nSize = 0;
    const size_t nMaxSize = 10000000; // Bail out at more than 10MB of performance data
    while (true) {
        nSize = vData.size();
        ret = RegQueryValueExA(HKEY_PERFORMANCE_DATA, "Global", NULL, NULL, vData.data(), &nSize);
        if (ret != ERROR_MORE_DATA || vData.size() >= nMaxSize)
            break;
        vData.resize(std::max((vData.size() * 3) / 2, nMaxSize)); // Grow size of buffer exponentially
    }
    RegCloseKey(HKEY_PERFORMANCE_DATA);
    if (ret == ERROR_SUCCESS) {
        RAND_add(vData.data(), nSize, nSize / 100.0);
        memory_cleanse(vData.data(), nSize);
        LogPrint("rand", "%s: %lu bytes\n", __func__, nSize);
    } else {
        static bool warned = false; // Warn only once
        if (!warned) {
            LogPrintf("%s: Warning: RegQueryValueExA(HKEY_PERFORMANCE_DATA) failed with code %i\n", __func__, ret);
            warned = true;
        }
    }
#endif
}

/** Get 32 bytes of system entropy. */
static void GetOSRand(unsigned char *ent32)
{
#ifdef WIN32
    HCRYPTPROV hProvider;
    int ret = CryptAcquireContextW(&hProvider, NULL, NULL, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT);
    if (!ret) {
        RandFailure();
    }
    ret = CryptGenRandom(hProvider, 32, ent32);
    if (!ret) {
        RandFailure();
    }
    CryptReleaseContext(hProvider, 0);
#else
    int f = open("/dev/urandom", O_RDONLY);
    if (f == -1) {
        RandFailure();
    }
    int have = 0;
    do {
        ssize_t n = read(f, ent32 + have, 32 - have);
        if (n <= 0 || n + have > 32) {
            RandFailure();
        }
        have += n;
    } while (have < 32);
    close(f);
#endif
}

void GetRandBytes(unsigned char* buf, int num)
{
    if (RAND_bytes(buf, num) != 1) {
        RandFailure();
    }
}


/* /fn int getRDSEED( uint64_t *buff, int num)
 * /brief Use Intel's SP 800-90B & C compliant Hardware implemented
 *        instruction to add entropy to the pool use to create keys. 
 *
 *        For details, see: https://software.intel.com/content/www/us/en/develop/blogs/the-difference-between-rdrand-and-rdseed.html
 * 
 *  /param buff Buffer to fill with random bits.
 *  /param num  Number of 64-bit words to write to buffer.
 *  /return     Returns 1 if the hardware returned  a valid stream of random numbers, 0 otherwise.
 *
 */
int getRDSEED( uint64_t *buff, uint32_t num){
    //Initialize state to 0, or failed.
    uint32_t  cumulativeStatus = 0;

    #ifdef LOWELEVEL_USE_RDSEEED
        //Check to see if the processor running this code has 
        //this Instruction. Most Intel/AMD CPUs after 2015
        //should have this instruction.
        uint32_t level, eax, ebx, ecx, edx; 
        
        //Level where the RDSEED feature is described
        level = 1;
    
        //Extract feature vector
        __get_cpuid(level, &eax, &ebx, &ecx, &edx);    
    
        //Check if RDSEED exists on this processor
        if( (ecx & bit_RDSEED) == bit_RDSEED ) {
    
            //Re-initialize value to 1, or success 
            cumulativeStatus = 1;
    
            for( uint32_t kk = 0; kk < num; kk++){
                //Get RBG data
                uint32_t rng_high, rng_low;

                int32_t status1 = _rdseed32_step( &rng_low );
                int32_t status2 = _rdseed32_step( &rng_high );

                //Place values into a 64-bit register
                uint64_t temp = rng_high;
                temp <<= 32;
                temp += rng_low;

                //Store into buffer
                buff[kk] =  temp; 
    
                //Update status value. 
                //cumulativeStatus will be 1 iff all iterations are 1.
                cumulativeStatus &= status1 & status2;
            }
        } 
    #endif /*LOWLEVEL_USE_RDSEED*/

    return cumulativeStatus;
}

void GetStrongRandBytes(unsigned char* out, int num)
{
    assert(num <= 64);
    CSHA512 hasher;
    unsigned char buf[64];

    // First source: OpenSSL's RNG
    RandAddSeedPerfmon();
    GetRandBytes(buf, 64);
    hasher.Write(buf, 64);

    // Second source: OS RNG
    GetOSRand(buf);
    hasher.Write(buf, 64);

    //Third Source: Intel's NIST SP 800-90B & C compliant RBG
    //if available, use it.
    #ifdef LOWLEVEL_H    
        block_512bits buf2;
        if ( getRDSEED(buf2.qword, 8) == 1  )
            hasher.Write(buf2.ubyte, 64);
    #endif

    // Produce output
    hasher.Finalize(buf);
    memcpy(out, buf, num);
    memory_cleanse(buf, 64);
}

uint64_t GetRand(uint64_t nMax)
{
    if (nMax == 0)
        return 0;

    // The range of the random source must be a multiple of the modulus
    // to give every possible output value an equal possibility
    uint64_t nRange = (std::numeric_limits<uint64_t>::max() / nMax) * nMax;
    uint64_t nRand = 0;
    do {
        GetRandBytes((unsigned char*)&nRand, sizeof(nRand));
    } while (nRand >= nRange);
    return (nRand % nMax);
}

int GetRandInt(int nMax)
{
    return GetRand(nMax);
}

uint256 GetRandHash()
{
    uint256 hash;
    GetRandBytes((unsigned char*)&hash, sizeof(hash));
    return hash;
}

FastRandomContext::FastRandomContext(bool fDeterministic)
{
    // The seed values have some unlikely fixed points which we avoid.
    if (fDeterministic) {
        Rz = Rw = 11;
    } else {
        uint32_t tmp;
        do {
            GetRandBytes((unsigned char*)&tmp, 4);
        } while (tmp == 0 || tmp == 0x9068ffffU);
        Rz = tmp;
        do {
            GetRandBytes((unsigned char*)&tmp, 4);
        } while (tmp == 0 || tmp == 0x464fffffU);
        Rw = tmp;
    }
}

