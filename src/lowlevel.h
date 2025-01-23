// Copyright (c) 2021 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

//Here be dragons, wizards, witches and master of the dark arts. 
//Careful thou who wonders 'round these parts without magic spells,
//mana potions and doggy treats...especially magical doggy treats.


//Only define this header on x86
#if ( defined(i386)     || defined(__i386)  || defined(__i386__) || \
      defined( __386)   || defined(_X86_)   || defined( _M_I86)  || \
      defined(__i386__) || defined(__X86__) || defined(__x86_64) ) 

    #ifndef LOWLEVEL_H
        #define LOWLEVEL_H
        #define LOWLEVEL_USE_RDSEED (1)
        
        #if ( defined(_WIN32)     || defined(WIN32)   || \
              defined(__WIN32__)  || defined(__NT__) )
            #include <intrin.h>
        #else
            #include <x86intrin.h>
        #endif
        
        #include <cpuid.h>

        #ifndef bit_RDSEED
            #define bit_RDSEED  (1 << 18)
        #endif

        /* To address compatibility issues with MacOS, given the lacking implementation
         * of intrinsics we here implement rdseed in byte code so that we may use it 
         * anywhere the ISA extension permits it.
         *
         * See: https://software.intel.com/content/www/us/en/develop/articles/the-drng-library-and-manual.html
         * You will find this function definition at:
         *    libdrng-1.0.tar.gz: https://software.intel.com/file/469237/download
         */
        #if defined(__APPLE__)
            #  define _rdseed16_step(x) ({ unsigned char err; asm volatile(".byte 0x66; .byte 0x0f; .byte 0xc7; .byte 0xf8; setc %1":"=a"(*x), "=qm"(err)); err; })
            #  define _rdseed32_step(x) ({ unsigned char err; asm volatile(".byte 0x0f; .byte 0xc7; .byte 0xf8; setc %1":"=a"(*x), "=qm"(err)); err; })
        #endif

        /* \union Useful for buffers that will need to be hashed
         *        where data gather comes from different int types.       
         */
        union block_512bits {
            char                    byte[64];
            unsigned char           ubyte[64];
            uint16_t                word[32];
            uint32_t                dword[16];
            uint64_t                qword[8];
            unsigned long long int  ulli[8]; 
        };
        
    #endif  /*LOWLEVEL_H*/
#endif  /*Detect x86*/
