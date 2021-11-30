#!/bin/sh
# Copyright (c) 2017-2019 The Bitcoin Core developers
# Copyright (c) 2021 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

# Install libdb5.3 (Berkeley DB).

export LC_ALL=C
set -e

if [ -z "${1}" ]; then
  echo "Usage: $0 <base-dir> [<extra-bdb-configure-flag> ...]"
  echo
  echo "Must specify a single argument: the directory in which db5 will be built."
  echo "This is probably \`pwd\` if you're at the root of the bitcoin repository."
  exit 1
fi

expand_path() {
  cd "${1}" && pwd -P
}

BDB_PREFIX="$(expand_path ${1})/db5"; shift;
BDB_VERSION='db-5.3.28.NC'
BDB_HASH='76a25560d9e52a198d37a31440fd07632b5f1f8f9f2b6d5438f4bc3e7c9013ef'
BDB_URL="https://download.oracle.com/berkeley-db/${BDB_VERSION}.tar.gz"

check_exists() {
  command -v "$1" >/dev/null
}

sha256_check() {
  # Args: <sha256_hash> <filename>
  #
  if check_exists sha256sum; then
    echo "${1}  ${2}" | sha256sum -c
  elif check_exists sha256; then
    if [ "$(uname)" = "FreeBSD" ]; then
      sha256 -c "${1}" "${2}"
    else
      echo "${1}  ${2}" | sha256 -c
    fi
  else
    echo "${1}  ${2}" | shasum -a 256 -c
  fi
}

http_get() {
  # Args: <url> <filename> <sha256_hash>
  #
  # It's acceptable that we don't require SSL here because we manually verify
  # content hashes below.
  #
  if [ -f "${2}" ]; then
    echo "File ${2} already exists; not downloading again"
  elif check_exists curl; then
    curl --insecure --retry 5 "${1}" -o "${2}"
  else
    wget --no-check-certificate "${1}" -O "${2}"
  fi

  sha256_check "${3}" "${2}"
}

mkdir -p "${BDB_PREFIX}"
http_get "${BDB_URL}" "${BDB_VERSION}.tar.gz" "${BDB_HASH}"
tar -xzvf ${BDB_VERSION}.tar.gz -C "$BDB_PREFIX"
cd "${BDB_PREFIX}/${BDB_VERSION}/"

# Apply a patch necessary when building with clang and c++11 (see https://community.oracle.com/thread/3952592)
patch --ignore-whitespace -p1 << 'EOF'
diff --git a/src/dbinc/atomic.h b/src/dbinc/atomic.h
index 6a858f7..9f338dc 100644
--- a/src/dbinc/atomic.h
+++ b/src/dbinc/atomic.h
@@ -70,7 +70,7 @@ typedef struct {
  * These have no memory barriers; the caller must include them when necessary.
  */
 #define	atomic_read(p)		((p)->value)
-#define	atomic_init(p, val)	((p)->value = (val))
+#define	atomic_init_db(p, val)	((p)->value = (val))
 
 #ifdef HAVE_ATOMIC_SUPPORT
 
@@ -144,7 +144,7 @@ typedef LONG volatile *interlocked_val;
 #define	atomic_inc(env, p)	__atomic_inc(p)
 #define	atomic_dec(env, p)	__atomic_dec(p)
 #define	atomic_compare_exchange(env, p, o, n)	\
-	__atomic_compare_exchange((p), (o), (n))
+	__atomic_compare_exchange_db((p), (o), (n))
 static inline int __atomic_inc(db_atomic_t *p)
 {
 	int	temp;
@@ -176,7 +176,7 @@ static inline int __atomic_dec(db_atomic_t *p)
  * http://gcc.gnu.org/onlinedocs/gcc-4.1.0/gcc/Atomic-Builtins.html
  * which configure could be changed to use.
  */
-static inline int __atomic_compare_exchange(
+static inline int __atomic_compare_exchange_db(
 	db_atomic_t *p, atomic_value_t oldval, atomic_value_t newval)
 {
 	atomic_value_t was;
@@ -206,7 +206,7 @@ static inline int __atomic_compare_exchange(
 #define	atomic_dec(env, p)	(--(p)->value)
 #define	atomic_compare_exchange(env, p, oldval, newval)		\
 	(DB_ASSERT(env, atomic_read(p) == (oldval)),		\
-	atomic_init(p, (newval)), 1)
+	atomic_init_db(p, (newval)), 1)
 #else
 #define atomic_inc(env, p)	__atomic_inc(env, p)
 #define atomic_dec(env, p)	__atomic_dec(env, p)
diff --git a/src/mp/mp_fget.c b/src/mp/mp_fget.c
index 16de695..d0dcc29 100644
--- a/src/mp/mp_fget.c
+++ b/src/mp/mp_fget.c
@@ -649,7 +649,7 @@ alloc:		/* Allocate a new buffer header and data space. */
 
 		/* Initialize enough so we can call __memp_bhfree. */
 		alloc_bhp->flags = 0;
-		atomic_init(&alloc_bhp->ref, 1);
+		atomic_init_db(&alloc_bhp->ref, 1);
 #ifdef DIAGNOSTIC
 		if ((uintptr_t)alloc_bhp->buf & (sizeof(size_t) - 1)) {
 			__db_errx(env, DB_STR("3025",
@@ -955,7 +955,7 @@ alloc:		/* Allocate a new buffer header and data space. */
 			MVCC_MPROTECT(bhp->buf, mfp->pagesize,
 			    PROT_READ);
 
-		atomic_init(&alloc_bhp->ref, 1);
+		atomic_init_db(&alloc_bhp->ref, 1);
 		MUTEX_LOCK(env, alloc_bhp->mtx_buf);
 		alloc_bhp->priority = bhp->priority;
 		alloc_bhp->pgno = bhp->pgno;
diff --git a/src/mp/mp_mvcc.c b/src/mp/mp_mvcc.c
index 770bad8..e28cce0 100644
--- a/src/mp/mp_mvcc.c
+++ b/src/mp/mp_mvcc.c
@@ -276,7 +276,7 @@ __memp_bh_freeze(dbmp, infop, hp, bhp, need_frozenp)
 #else
 	memcpy(frozen_bhp, bhp, SSZA(BH, buf));
 #endif
-	atomic_init(&frozen_bhp->ref, 0);
+	atomic_init_db(&frozen_bhp->ref, 0);
 	if (mutex != MUTEX_INVALID)
 		frozen_bhp->mtx_buf = mutex;
 	else if ((ret = __mutex_alloc(env, MTX_MPOOL_BH,
@@ -428,7 +428,7 @@ __memp_bh_thaw(dbmp, infop, hp, frozen_bhp, alloc_bhp)
 #endif
 		alloc_bhp->mtx_buf = mutex;
 		MUTEX_LOCK(env, alloc_bhp->mtx_buf);
-		atomic_init(&alloc_bhp->ref, 1);
+		atomic_init_db(&alloc_bhp->ref, 1);
 		F_CLR(alloc_bhp, BH_FROZEN);
 	}
 
diff --git a/src/mp/mp_region.c b/src/mp/mp_region.c
index 4952030..47645f8 100644
--- a/src/mp/mp_region.c
+++ b/src/mp/mp_region.c
@@ -245,7 +245,7 @@ __memp_init(env, dbmp, reginfo_off, htab_buckets, max_nreg)
 			     MTX_MPOOL_FILE_BUCKET, 0, &htab[i].mtx_hash)) != 0)
 				return (ret);
 			SH_TAILQ_INIT(&htab[i].hash_bucket);
-			atomic_init(&htab[i].hash_page_dirty, 0);
+			atomic_init_db(&htab[i].hash_page_dirty, 0);
 		}
 
 		/*
@@ -302,7 +302,7 @@ no_prealloc:
 		} else
 			hp->mtx_hash = mtx_base + (i % dbenv->mp_mtxcount);
 		SH_TAILQ_INIT(&hp->hash_bucket);
-		atomic_init(&hp->hash_page_dirty, 0);
+		atomic_init_db(&hp->hash_page_dirty, 0);
 #ifdef HAVE_STATISTICS
 		hp->hash_io_wait = 0;
 		hp->hash_frozen = hp->hash_thawed = hp->hash_frozen_freed = 0;
diff --git a/src/mutex/mut_method.c b/src/mutex/mut_method.c
index 09353b0..177353c 100644
--- a/src/mutex/mut_method.c
+++ b/src/mutex/mut_method.c
@@ -474,7 +474,7 @@ atomic_compare_exchange(env, v, oldval, newval)
 	MUTEX_LOCK(env, mtx);
 	ret = atomic_read(v) == oldval;
 	if (ret)
-		atomic_init(v, newval);
+		atomic_init_db(v, newval);
 	MUTEX_UNLOCK(env, mtx);
 
 	return (ret);
diff --git a/src/mutex/mut_tas.c b/src/mutex/mut_tas.c
index 106b161..fc4de9d 100644
--- a/src/mutex/mut_tas.c
+++ b/src/mutex/mut_tas.c
@@ -47,7 +47,7 @@ __db_tas_mutex_init(env, mutex, flags)
 
 #ifdef HAVE_SHARED_LATCHES
 	if (F_ISSET(mutexp, DB_MUTEX_SHARED))
-		atomic_init(&mutexp->sharecount, 0);
+		atomic_init_db(&mutexp->sharecount, 0);
 	else
 #endif
 	if (MUTEX_INIT(&mutexp->tas)) {
@@ -536,7 +536,7 @@ __db_tas_mutex_unlock(env, mutex)
 			F_CLR(mutexp, DB_MUTEX_LOCKED);
 			/* Flush flag update before zeroing count */
 			MEMBAR_EXIT();
-			atomic_init(&mutexp->sharecount, 0);
+			atomic_init_db(&mutexp->sharecount, 0);
 		} else {
 			DB_ASSERT(env, sharecount > 0);
 			MEMBAR_EXIT();
EOF

# Apply a patch for compatibility with X-Code 12.1
patch --ignore-whitespace -p1 << 'EOF'
--- a/dist/configure	2013-09-09 16:35:02.000000000 +0100
+++ b/dist/configure	2021-11-30 19:46:10.341486676 +0000
@@ -21275,7 +21275,7 @@
 		static lwp_cond_t ci = SHAREDCV;
 		lwp_mutex_t mutex = mi;
 		lwp_cond_t cond = ci;
-		exit (
+		return (
 		_lwp_mutex_lock(&mutex) ||
 		_lwp_mutex_unlock(&mutex));
 
@@ -21305,7 +21305,7 @@
 		mutex_t mutex;
 		cond_t cond;
 		int type = USYNC_PROCESS;
-		exit (
+		return (
 		mutex_init(&mutex, type, NULL) ||
 		cond_init(&cond, type, NULL) ||
 		mutex_lock(&mutex) ||
@@ -21335,7 +21335,7 @@
 		mutex_t mutex;
 		cond_t cond;
 		int type = USYNC_PROCESS;
-		exit (
+		return (
 		mutex_init(&mutex, type, NULL) ||
 		cond_init(&cond, type, NULL) ||
 		mutex_lock(&mutex) ||
@@ -21370,7 +21370,7 @@
 {
 
 	#if (defined(i386) || defined(__i386__)) && defined(__GNUC__)
-		exit(0);
+		return (0);
 	#else
 		FAIL TO COMPILE/LINK
 	#endif
@@ -21393,7 +21393,7 @@
 {
 
 	#if (defined(x86_64) || defined(__x86_64__)) && defined(__GNUC__)
-		exit(0);
+		return (0);
 	#else
 		FAIL TO COMPILE/LINK
 	#endif
@@ -21445,7 +21445,7 @@
 
 	#if defined(__sparc__) && defined(__GNUC__)
 		asm volatile ("membar #StoreStore|#StoreLoad|#LoadStore");
-		exit(0);
+		return (0);
 	#else
 		FAIL TO COMPILE/LINK
 	#endif
@@ -21516,7 +21516,7 @@
 	msem_init(&x, 0);
 	msem_lock(&x, 0);
 	msem_unlock(&x, 0);
-	exit(0);
+	return (0);
 #else
 	FAIL TO COMPILE/LINK
 #endif
@@ -21548,7 +21548,7 @@
 	msem_init(&x, 0);
 	msem_lock(&x, 0);
 	msem_unlock(&x, 0);
-	exit(0);
+	return (0);
 
   ;
   return 0;
@@ -21600,7 +21600,7 @@
 {
 
 #if defined(__USLC__)
-	exit(0);
+	return (0);
 #else
 	FAIL TO COMPILE/LINK
 #endif
@@ -21731,7 +21731,7 @@
 {
 
 #if defined(__alpha) && defined(__DECC)
-	exit(0);
+	return (0);
 #else
 	FAIL TO COMPILE/LINK
 #endif
@@ -21756,7 +21756,7 @@
 {
 
 #if defined(__alpha) && defined(__GNUC__)
-	exit(0);
+	return (0);
 #else
 	FAIL TO COMPILE/LINK
 #endif
@@ -21781,7 +21781,7 @@
 {
 
 #if defined(__arm__) && defined(__GNUC__)
-	exit(0);
+	return (0);
 #else
 	FAIL TO COMPILE/LINK
 #endif
@@ -21806,7 +21806,7 @@
 {
 
 #if (defined(__mips) || defined(__mips__)) && defined(__GNUC__)
-	exit(0);
+	return (0);
 #else
 	FAIL TO COMPILE/LINK
 #endif
@@ -21831,7 +21831,7 @@
 {
 
 #if (defined(__hppa) || defined(__hppa__)) && defined(__GNUC__)
-	exit(0);
+	return (0);
 #else
 	FAIL TO COMPILE/LINK
 #endif
@@ -21856,7 +21856,7 @@
 {
 
 #if (defined(__powerpc__) || defined(__ppc__)) && defined(__GNUC__)
-	exit(0);
+	return (0);
 #else
 	FAIL TO COMPILE/LINK
 #endif
@@ -21881,7 +21881,7 @@
 {
 
 #if (defined(mc68020) || defined(sun3)) && defined(__GNUC__)
-	exit(0);
+	return (0);
 #else
 	FAIL TO COMPILE/LINK
 #endif
@@ -21906,7 +21906,7 @@
 {
 
 #if defined(__MVS__) && defined(__IBMC__)
-	exit(0);
+	return (0);
 #else
 	FAIL TO COMPILE/LINK
 #endif
@@ -21931,7 +21931,7 @@
 {
 
 #if defined(__s390__) && defined(__GNUC__)
-	exit(0);
+	return (0);
 #else
 	FAIL TO COMPILE/LINK
 #endif
@@ -21956,7 +21956,7 @@
 {
 
 #if defined(__ia64) && defined(__GNUC__)
-	exit(0);
+	return (0);
 #else
 	FAIL TO COMPILE/LINK
 #endif
@@ -21981,7 +21981,7 @@
 {
 
 #if defined(_UTS)
-	exit(0);
+	return (0);
 #else
 	FAIL TO COMPILE/LINK
 #endif
@@ -22464,9 +22464,9 @@
 {
 
 	#if ((defined(i386) || defined(__i386__)) && defined(__GNUC__))
-		exit(0);
+		return (0);
 	#elif ((defined(x86_64) || defined(__x86_64__)) && defined(__GNUC__))
-		exit(0);
+		return (0);
 	#else
 		FAIL TO COMPILE/LINK
 	#endif
@@ -22491,7 +22491,7 @@
 {
 
 	volatile unsigned val = 1;
-	exit (atomic_inc_uint_nv(&val) != 2 ||
+	return (atomic_inc_uint_nv(&val) != 2 ||
 	      atomic_dec_uint_nv(&val) != 1 ||
 	      atomic_cas_32(&val, 1, 3) != 3);
 
@@ -24192,6 +24192,11 @@
   cat confdefs.h - <<_ACEOF >conftest.$ac_ext
 /* end confdefs.h.  */
 
+#include <stdio.h>
+#if HAVE_STRING_H
+#include <string.h>
+#endif
+
 int
 main ()
 {
@@ -24226,7 +24231,12 @@
   cat confdefs.h - <<_ACEOF >conftest.$ac_ext
 /* end confdefs.h.  */
 
-		main() {
+#include <stdio.h>
+#if HAVE_STRING_H
+#include <string.h>
+#endif
+
+		int main() {
 			$db_cv_seq_type l;
 			unsigned $db_cv_seq_type u;
 			char buf[100];
@@ -24309,6 +24319,9 @@
     #include <fcntl.h>
     #include <sys/mman.h>
     #include <signal.h>
+    #if HAVE_UNISTD_H
+    #include <unistd.h>
+    #endif
 
     #define TEST_MMAP_BUFSIZE	(16 * 1024)
     #define TEST_MMAP_EXTENDSIZE	(16 * 1024 * 1024)
@@ -24319,10 +24332,10 @@
     int catch_sig(sig)
 	    int sig;
     {
-	    exit(1);
+	    return (1);
     }
 
-    main() {
+    int main() {
 	    const char *underlying;
 	    unsigned gapsize;
 	    char *base;
EOF

cd build_unix/

"${BDB_PREFIX}/${BDB_VERSION}/dist/configure" \
  --enable-cxx --disable-shared --disable-replication --with-pic --prefix="${BDB_PREFIX}" \
  "${@}"

make install

echo
echo "db5 build complete."
echo
# shellcheck disable=SC2016
echo 'When compiling bitcoind, run `./configure` in the following way:'
echo
echo "  export BDB_PREFIX='${BDB_PREFIX}'"
# shellcheck disable=SC2016
echo '  ./configure BDB_LIBS="-L${BDB_PREFIX}/lib -ldb_cxx-5.3" BDB_CFLAGS="-I${BDB_PREFIX}/include" ...'
