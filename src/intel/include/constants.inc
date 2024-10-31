;;
;; Copyright (c) 2012-2024, Intel Corporation
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;;     * Redistributions of source code must retain the above copyright notice,
;;       this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     * Neither the name of Intel Corporation nor the names of its contributors
;;       may be used to endorse or promote products derived from this software
;;       without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;

;;;  Generic constants
%define PTR_SZ                  8

;;; hash constants

%define MD5_DIGEST_WORD_SIZE	4
%define SHA1_DIGEST_WORD_SIZE	4
%define SHA256_DIGEST_WORD_SIZE	4
%define SHA512_DIGEST_WORD_SIZE	8
;; AVX512 constants
%define MAX_MD5_LANES		32
%define MAX_SHA1_LANES		16
%define MAX_SHA256_LANES	16
%define MAX_SHA512_LANES	8

%define NUM_MD5_DIGEST_WORDS	4
%define NUM_SHA1_DIGEST_WORDS	5
%define NUM_SHA256_DIGEST_WORDS	8
%define NUM_SHA512_DIGEST_WORDS	8

%define MD5_DIGEST_ROW_SIZE	(MAX_MD5_LANES    * MD5_DIGEST_WORD_SIZE)
%define SHA1_DIGEST_ROW_SIZE	(MAX_SHA1_LANES   * SHA1_DIGEST_WORD_SIZE)
%define SHA256_DIGEST_ROW_SIZE	(MAX_SHA256_LANES * SHA256_DIGEST_WORD_SIZE)
%define SHA512_DIGEST_ROW_SIZE	(MAX_SHA512_LANES * SHA512_DIGEST_WORD_SIZE)

%define MD5_DIGEST_SIZE		(MD5_DIGEST_ROW_SIZE    * NUM_MD5_DIGEST_WORDS)
%define SHA1_DIGEST_SIZE	(SHA1_DIGEST_ROW_SIZE   * NUM_SHA1_DIGEST_WORDS)
%define SHA256_DIGEST_SIZE	(SHA256_DIGEST_ROW_SIZE * NUM_SHA256_DIGEST_WORDS)
%define SHA512_DIGEST_SIZE	(SHA512_DIGEST_ROW_SIZE * NUM_SHA512_DIGEST_WORDS)

;; Used only by SHA-NI implementations
;; Sanity checks to fail build if not satisfied
%define SHA1NI_DIGEST_ROW_SIZE	 (NUM_SHA1_DIGEST_WORDS * SHA1_DIGEST_WORD_SIZE)
%define SHA256NI_DIGEST_ROW_SIZE (NUM_SHA256_DIGEST_WORDS * SHA256_DIGEST_WORD_SIZE)
%define SHA512NI_DIGEST_ROW_SIZE (NUM_SHA512_DIGEST_WORDS * SHA512_DIGEST_WORD_SIZE)

%define MD5_BLK_SZ              128  ; in bytes
%define SHA1_BLK_SZ             64   ; in bytes
%define SHA256_BLK_SZ           64   ; in bytes
%define SHA512_BLK_SZ           128  ; in bytes
