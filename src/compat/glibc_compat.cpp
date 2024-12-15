// Copyright (c) 2009-2014 The Bitcoin Core developers
// Copyright (c) 2018-2023 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#if defined(HAVE_CONFIG_H)
#include "config/bitcoin-config.h"
#endif

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <stdarg.h>
#include <time.h>

#if defined(HAVE_SYS_SELECT_H)
#include <sys/select.h>
#endif

// Prior to GLIBC_2.14, memcpy was aliased to memmove.
extern "C" void* memmove(void* a, const void* b, size_t c);
extern "C" void* memcpy(void* a, const void* b, size_t c)
{
    return memmove(a, b, c);
}

extern "C" void __chk_fail(void) __attribute__((__noreturn__));
extern "C" FDELT_TYPE __fdelt_warn(FDELT_TYPE a)
{
    if (a >= FD_SETSIZE)
        __chk_fail();
    return a / __NFDBITS;
}
extern "C" FDELT_TYPE __fdelt_chk(FDELT_TYPE) __attribute__((weak, alias("__fdelt_warn")));

#if defined(__i386__) || defined(__arm__)

extern "C" int64_t __udivmoddi4(uint64_t u, uint64_t v, uint64_t* rp);

extern "C" int64_t __wrap___divmoddi4(int64_t u, int64_t v, int64_t* rp)
{
    int32_t c1 = 0, c2 = 0;
    int64_t uu = u, vv = v;
    int64_t w;
    int64_t r;

    if (uu < 0) {
        c1 = ~c1, c2 = ~c2, uu = -uu;
    }
    if (vv < 0) {
        c1 = ~c1, vv = -vv;
    }

    w = __udivmoddi4(uu, vv, (uint64_t*)&r);
    if (c1)
        w = -w;
    if (c2)
        r = -r;

    *rp = r;
    return w;
}
#endif

extern "C" float log2f_old(float x);
#ifdef __i386__
__asm(".symver log2f_old,log2f@GLIBC_2.1");
#elif defined(__amd64__)
__asm(".symver log2f_old,log2f@GLIBC_2.2.5");
#elif defined(__arm__)
__asm(".symver log2f_old,log2f@GLIBC_2.4");
#elif defined(__aarch64__)
__asm(".symver log2f_old,log2f@GLIBC_2.17");
#endif
extern "C" float __wrap_log2f(float x)
{
    return log2f_old(x);
}

extern "C" double exp_old(double x);
#ifdef __i386__
__asm(".symver exp_old,exp@GLIBC_2.0");
#elif defined(__amd64__)
__asm(".symver exp_old,exp@GLIBC_2.2.5");
#elif defined(__arm__)
__asm(".symver exp_old,exp@GLIBC_2.4");
#elif defined(__aarch64__)
__asm(".symver exp_old,exp@GLIBC_2.17");
#endif
extern "C" double __wrap_exp(double x) {
    return exp_old(x);
}

extern "C" double log_old(double x);
#ifdef __i386__
__asm(".symver log_old,log@GLIBC_2.0");
#elif defined(__amd64__)
__asm(".symver log_old,log@GLIBC_2.2.5");
#elif defined(__arm__)
__asm(".symver log_old,log@GLIBC_2.4");
#elif defined(__aarch64__)
__asm(".symver log_old,log@GLIBC_2.17");
#endif
extern "C" double __wrap_log(double x) {
    return log_old(x);
}

extern "C" double pow_old(double x, double y);
#ifdef __i386__
__asm(".symver pow_old,pow@GLIBC_2.0");
#elif defined(__amd64__)
__asm(".symver pow_old,pow@GLIBC_2.2.5");
#elif defined(__arm__)
__asm(".symver pow_old,pow@GLIBC_2.4");
#elif defined(__aarch64__)
__asm(".symver pow_old,pow@GLIBC_2.17");
#endif
extern "C" double __wrap_pow(double x, double y) {
    return pow_old(x,y);
}

extern "C" int clock_gettime_old(clockid_t a, struct timespec *b);
#ifdef __i386__
__asm(".symver clock_gettime_old,clock_gettime@GLIBC_2.2");
#elif defined(__amd64__)
__asm(".symver clock_gettime_old,clock_gettime@GLIBC_2.2.5");
#elif defined(__arm__)
__asm(".symver clock_gettime_old,clock_gettime@GLIBC_2.4");
#elif defined(__aarch64__)
__asm(".symver clock_gettime_old,clock_gettime@GLIBC_2.17");
#endif
extern "C" int __wrap_clock_gettime(clockid_t a, struct timespec *b) {
    return clock_gettime_old(a, b);
}

// Wrap fcntl and fcntl64 for 32-bit linux only (both ARM and intel)
#if defined(__i386__) || defined(__arm__)
extern "C" int fcntl_old(int fd, int cmd, ...);

# if defined(__i386__)
__asm(".symver fcntl_old,fcntl@GLIBC_2.0");
# elif defined(__arm__)
__asm(".symver fcntl_old,fcntl@GLIBC_2.4");
# endif

extern "C" int __wrap_fcntl(int fd, int cmd, ...) {
     va_list ap;
     va_start(ap, cmd);
     void* arg = va_arg(ap, void*);
     va_end(ap);
     return fcntl_old(fd, cmd, arg);
}

extern "C" int __wrap_fcntl64(int fd, int cmd, ...) {
     va_list ap;
     va_start(ap, cmd);
     void* arg = va_arg(ap, void*);
     va_end(ap);
     return fcntl_old(fd, cmd, arg);
}
#endif //defined(__i386__) || defined(__arm__)
