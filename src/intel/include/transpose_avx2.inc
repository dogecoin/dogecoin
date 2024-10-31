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

%ifndef _TRANSPOSE_AVX2_INC_
%define _TRANSPOSE_AVX2_INC_

%include "include/reg_sizes.inc"

; LOAD ALL 8 LANES FOR 8x8 32-BIT TRANSPOSE
;
; r0-r7       [out] ymm registers which will contain the data to be transposed
; addr0-addr7 [in]  pointers to the next 32-byte block of data to be fetch for all 8 lanes
; ptr_offset  [in] offset to be applied on all pointers (addr0-addr7)
%macro TRANSPOSE8_U32_LOAD8 17
%define %%r0 %1
%define %%r1 %2
%define %%r2 %3
%define %%r3 %4
%define %%r4 %5
%define %%r5 %6
%define %%r6 %7
%define %%r7 %8
%define %%addr0 %9
%define %%addr1 %10
%define %%addr2 %11
%define %%addr3 %12
%define %%addr4 %13
%define %%addr5 %14
%define %%addr6 %15
%define %%addr7 %16
%define %%ptr_offset %17

; Expected output data
;
; r0 = {e3 e2 e1 e0  a3 a2 a1 a0}
; r1 = {f3 f2 f1 f0  b3 b2 b1 b0}
; r2 = {g3 g2 g1 g0  c3 c2 c1 c0}
; r3 = {h3 h2 h1 h0  d3 d2 d1 d0}
; r4 = {e7 e6 e5 e4  a7 a6 a5 a4}
; r5 = {f7 f6 f5 f4  b7 b6 b5 b4}
; r6 = {g7 g6 g5 g4  c7 c6 c5 c4}
; r7 = {h7 h6 h5 h4  d7 d6 d5 d4}

	vmovups	XWORD(%%r0),[%%addr0+%%ptr_offset]
	vmovups	XWORD(%%r1),[%%addr1+%%ptr_offset]
	vmovups	XWORD(%%r2),[%%addr2+%%ptr_offset]
	vmovups	XWORD(%%r3),[%%addr3+%%ptr_offset]
	vmovups	XWORD(%%r4),[%%addr0+%%ptr_offset+16]
	vmovups	XWORD(%%r5),[%%addr1+%%ptr_offset+16]
	vmovups	XWORD(%%r6),[%%addr2+%%ptr_offset+16]
	vmovups	XWORD(%%r7),[%%addr3+%%ptr_offset+16]

	vinserti128 %%r0, %%r0, [%%addr4+%%ptr_offset], 0x01
	vinserti128 %%r1, %%r1, [%%addr5+%%ptr_offset], 0x01
	vinserti128 %%r2, %%r2, [%%addr6+%%ptr_offset], 0x01
	vinserti128 %%r3, %%r3, [%%addr7+%%ptr_offset], 0x01
	vinserti128 %%r4, %%r4, [%%addr4+%%ptr_offset+16], 0x01
	vinserti128 %%r5, %%r5, [%%addr5+%%ptr_offset+16], 0x01
	vinserti128 %%r6, %%r6, [%%addr6+%%ptr_offset+16], 0x01
	vinserti128 %%r7, %%r7, [%%addr7+%%ptr_offset+16], 0x01

%endmacro

; 8x8 32-BIT TRANSPOSE
;
; Before calling this macro, TRANSPOSE8_U32_LOAD8 must be called.
;
; r0-r3          [in/out]    ymm registers containing bytes 0-15 of each 32B block (e.g. ymm0 = [e3-e0 a3-a0])
; r4-r7          [in/out]    ymm registers containing bytes 16-31 of each 32B block (e.g. ymm4 = [e4-e7 a4-a7])
; t0-t1          [clobbered] ymm temporary registers
%macro TRANSPOSE8_U32_PRELOADED 10
%define %%r0 %1
%define %%r1 %2
%define %%r2 %3
%define %%r3 %4
%define %%r4 %5
%define %%r5 %6
%define %%r6 %7
%define %%r7 %8
%define %%t0 %9
%define %%t1 %10
; Input looks like: {r0 r1 r2 r3 r4 r5 r6 r7}
; r0 = {e3 e2 e1 e0   a3 a2 a1 a0}
; r1 = {f3 f2 f1 f0   b3 b2 b1 b0}
; r2 = {g3 g2 g1 g0   c3 c2 c1 c0}
; r3 = {h3 h2 h1 h0   d3 d2 d1 d0}
; r4 = {e7 e6 e5 e4   a7 a6 a5 a4}
; r5 = {f7 f6 f5 f4   b7 b6 b5 b4}
; r6 = {g7 g6 g5 g4   c7 c6 c5 c4}
; r7 = {h7 h6 h5 h4   d7 d6 d5 d4}
;
; Output looks like: {r0 r1 r2 r3 r4 r5 r6 r7}
; r0 = {h0 g0 f0 e0   d0 c0 b0 a0}
; r1 = {h1 g1 f1 e1   d1 c1 b1 a1}
; r2 = {h2 g2 f2 e2   d2 c2 b2 a2}
; r3 = {h3 g3 f3 e3   d3 c3 b3 a3}
; r4 = {h4 g4 f4 e4   d4 c4 b4 a4}
; r5 = {h5 g5 f5 e5   d5 c5 b5 a5}
; r6 = {h6 g6 f6 e6   d6 c6 b6 a6}
; r7 = {h7 g7 f7 e7   d7 c7 b7 a7}
;
	; process top half (r0..r3)
	vshufps	%%t0, %%r0, %%r1, 0x44	; t0 = {f1 f0 e1 e0   b1 b0 a1 a0}
	vshufps	%%r0, %%r0, %%r1, 0xEE	; r0 = {f3 f2 e3 e2   b3 b2 a3 a2}
	vshufps %%t1, %%r2, %%r3, 0x44	; t1 = {h1 h0 g1 g0   d1 d0 c1 c0}
	vshufps	%%r2, %%r2, %%r3, 0xEE	; r2 = {h3 h2 g3 g2   d3 d2 c3 c2}

	vshufps	%%r1, %%t0, %%t1, 0xDD	; r1 = {h1 g1 f1 e1   d1 c1 b1 a1}
	vshufps	%%r3, %%r0, %%r2, 0xDD	; r3 = {h3 g3 f3 e3   d3 c3 b3 a3}
	vshufps	%%r2, %%r0, %%r2, 0x88	; r2 = {h2 g2 f2 e2   d2 c2 b2 a2}
	vshufps	%%r0, %%t0, %%t1, 0x88	; r0 = {h0 g0 f0 e0   d0 c0 b0 a0}

	;; process bottom half (r4..r7)
	vshufps	%%t0, %%r4, %%r5, 0x44	; t0 = {f5 f4 e5 e4   b5 b4 a5 a4}
	vshufps	%%r4, %%r4, %%r5, 0xEE	; r4 = {f7 f6 e7 e6   b7 b6 a7 a6}
	vshufps %%t1, %%r6, %%r7, 0x44	; t1 = {h5 h4 g5 g4   d5 d4 c5 c4}
	vshufps	%%r6, %%r6, %%r7, 0xEE	; r6 = {h7 h6 g7 g6   d7 d6 c7 c6}

	vshufps	%%r5, %%t0, %%t1, 0xDD	; r5 = {h5 g5 f5 e5   d5 c5 b5 a5}
	vshufps	%%r7, %%r4, %%r6, 0xDD	; r7 = {h7 g7 f7 e7   d7 c7 b7 a7}
	vshufps	%%r6, %%r4, %%r6, 0x88	; r6 = {h6 g6 f6 e6   d6 c6 b6 a6}
	vshufps	%%r4, %%t0, %%t1, 0x88	; r4 = {h4 g4 f4 e4   d4 c4 b4 a4}
%endmacro

%macro TRANSPOSE8_U32 10
%define %%r0 %1
%define %%r1 %2
%define %%r2 %3
%define %%r3 %4
%define %%r4 %5
%define %%r5 %6
%define %%r6 %7
%define %%r7 %8
%define %%t0 %9
%define %%t1 %10
       ; process top half (r0..r3) {a...d}
       vshufps %%t0, %%r0, %%r1, 0x44  ; t0 = {b5 b4 a5 a4   b1 b0 a1 a0}
       vshufps %%r0, %%r0, %%r1, 0xEE  ; r0 = {b7 b6 a7 a6   b3 b2 a3 a2}
       vshufps %%t1, %%r2, %%r3, 0x44  ; t1 = {d5 d4 c5 c4   d1 d0 c1 c0}
       vshufps %%r2, %%r2, %%r3, 0xEE  ; r2 = {d7 d6 c7 c6   d3 d2 c3 c2}
       vshufps %%r3, %%t0, %%t1, 0xDD  ; r3 = {d5 c5 b5 a5   d1 c1 b1 a1}
       vshufps %%r1, %%r0, %%r2, 0x88  ; r1 = {d6 c6 b6 a6   d2 c2 b2 a2}
       vshufps %%r0, %%r0, %%r2, 0xDD  ; r0 = {d7 c7 b7 a7   d3 c3 b3 a3}
       vshufps %%t0, %%t0, %%t1, 0x88  ; t0 = {d4 c4 b4 a4   d0 c0 b0 a0}

       ; use r2 in place of t0
       ; process bottom half (r4..r7) {e...h}
       vshufps %%r2, %%r4, %%r5, 0x44  ; r2 = {f5 f4 e5 e4   f1 f0 e1 e0}
       vshufps %%r4, %%r4, %%r5, 0xEE  ; r4 = {f7 f6 e7 e6   f3 f2 e3 e2}
       vshufps %%t1, %%r6, %%r7, 0x44  ; t1 = {h5 h4 g5 g4   h1 h0 g1 g0}
       vshufps %%r6, %%r6, %%r7, 0xEE  ; r6 = {h7 h6 g7 g6   h3 h2 g3 g2}
       vshufps %%r7, %%r2, %%t1, 0xDD  ; r7 = {h5 g5 f5 e5   h1 g1 f1 e1}
       vshufps %%r5, %%r4, %%r6, 0x88  ; r5 = {h6 g6 f6 e6   h2 g2 f2 e2}
       vshufps %%r4, %%r4, %%r6, 0xDD  ; r4 = {h7 g7 f7 e7   h3 g3 f3 e3}
       vshufps %%t1, %%r2, %%t1, 0x88  ; t1 = {h4 g4 f4 e4   h0 g0 f0 e0}

       vperm2f128      %%r6, %%r5, %%r1, 0x13  ; h6...a6
       vperm2f128      %%r2, %%r5, %%r1, 0x02  ; h2...a2
       vperm2f128      %%r5, %%r7, %%r3, 0x13  ; h5...a5
       vperm2f128      %%r1, %%r7, %%r3, 0x02  ; h1...a1
       vperm2f128      %%r7, %%r4, %%r0, 0x13  ; h7...a7
       vperm2f128      %%r3, %%r4, %%r0, 0x02  ; h3...a3
       vperm2f128      %%r4, %%t1, %%t0, 0x13  ; h4...a4
       vperm2f128      %%r0, %%t1, %%t0, 0x02  ; h0...a0
%endmacro

; LOAD ALL 4 LANES FOR 4x4 64-BIT TRANSPOSE
;
; r0-r3 [out] ymm registers which will contain the data to be transposed
; addr0-addr3 [in] pointers to the next 32-byte block of data to be fetch for the 4 lanes
; ptr_offset  [in] offset to be applied on all pointers (addr0-addr3)
%macro TRANSPOSE4_U64_LOAD4 9
%define %%r0 %1
%define %%r1 %2
%define %%r2 %3
%define %%r3 %4
%define %%addr0 %5
%define %%addr1 %6
%define %%addr2 %7
%define %%addr3 %8
%define %%ptr_offset %9

; Expected output data
;
; r0 = {c1 c0 a1 a0}
; r1 = {d1 d0 b1 b0}
; r2 = {c3 c2 a3 a2}
; r3 = {d3 d2 b3 b2}

	vmovupd	XWORD(%%r0),[%%addr0+%%ptr_offset]
	vmovupd	XWORD(%%r1),[%%addr1+%%ptr_offset]
	vmovupd	XWORD(%%r2),[%%addr0+%%ptr_offset+16]
	vmovupd	XWORD(%%r3),[%%addr1+%%ptr_offset+16]

	vinserti128 %%r0, %%r0, [%%addr2+%%ptr_offset], 0x01
	vinserti128 %%r1, %%r1, [%%addr3+%%ptr_offset], 0x01
	vinserti128 %%r2, %%r2, [%%addr2+%%ptr_offset+16], 0x1
	vinserti128 %%r3, %%r3, [%%addr3+%%ptr_offset+16], 0x01

%endmacro

; 4x4 64-BIT TRANSPOSE
;
; Before calling this macro, TRANSPOSE4_U64_LOAD4 must be called.
;
; This macro takes 4 registers as input (r0-r3)
; and transposes their content (64-bit elements)
; outputting the data in registers (o0,r1,o2,r3),
; using two additional registers
%macro TRANSPOSE4_U64 6
%define %%r0 %1 ; [in]     ymm register for row 0 input (c0-c1 a1-a0)
%define %%r1 %2 ; [in/out] ymm register for row 1 input (d0-d1 b1-b0) and output
%define %%r2 %3 ; [in]     ymm register for row 2 input (c3-c2 a3-a2)
%define %%r3 %4 ; [in/out] ymm register for row 3 input (d3-d2 b3-b2) and output
%define %%o0 %5 ; [out]    ymm register for row 0 output
%define %%o2 %6 ; [out]    ymm register for row 2 output
; Input looks like: {r0 r1 r2 r3}
; r0 = {c1 c0 a1 a0}
; r1 = {d1 d0 b1 b0}
; r2 = {c3 c2 a3 a2}
; r3 = {d3 d2 b3 b2}
;
; output looks like: {o0 r1 o2 r3}
; o0 = {d0 c0 b0 a0}
; r1 = {d1 c1 b1 a1}
; o2 = {d2 c2 b2 a2}
; r3 = {d3 c3 b3 a3}
	; vshufps does not cross the mid-way boundary and hence is cheaper
	vshufps	%%o0, %%r0, %%r1, 0x44	; o0 = {d0 c0 b0 a0}
	vshufps	%%r1, %%r0, %%r1, 0xEE	; r1 = {d1 d0 b1 b0}

	vshufps	%%o2, %%r2, %%r3, 0x44	; o1 = {d2 c2 b2 a2}
	vshufps	%%r3, %%r2, %%r3, 0xEE	; r3 = {d3 c3 b3 a3}
%endmacro

%endif ;; _TRANSPOSE_AVX2_INC_
