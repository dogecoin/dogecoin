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

;; code to compute oct SHA256 using SSE-256
;; outer calling routine takes care of save and restore of XMM registers
;; Logic designed/laid out by JDG

;; Function clobbers: rax, rcx, rdx,   rbx, rsi, rdi, r9-r15; ymm0-15
;; Stack must be aligned to 32 bytes before call
;; Windows clobbers:  rax rbx     rdx rsi rdi     r8 r9 r10 r11 r12 r13 r14
;; Windows preserves:         rcx             rbp                           r15
;;
;; Linux clobbers:    rax rbx rcx rdx rsi         r8 r9 r10 r11 r12 r13 r14
;; Linux preserves:                       rdi rbp                           r15
;;
;; clobbers ymm0-15

%include "include/os.inc"
;%define DO_DBGPRINT
%include "include/dbgprint.inc"
%include "include/mb_mgr_datastruct.inc"
%include "include/transpose_avx2.inc"
%include "include/clear_regs.inc"

mksection .rodata
default rel
align 64
;global K256_8
K256_8:
	dq	0x428a2f98428a2f98, 0x428a2f98428a2f98
	dq	0x428a2f98428a2f98, 0x428a2f98428a2f98
	dq	0x7137449171374491, 0x7137449171374491
	dq	0x7137449171374491, 0x7137449171374491
	dq	0xb5c0fbcfb5c0fbcf, 0xb5c0fbcfb5c0fbcf
	dq	0xb5c0fbcfb5c0fbcf, 0xb5c0fbcfb5c0fbcf
	dq	0xe9b5dba5e9b5dba5, 0xe9b5dba5e9b5dba5
	dq	0xe9b5dba5e9b5dba5, 0xe9b5dba5e9b5dba5
	dq	0x3956c25b3956c25b, 0x3956c25b3956c25b
	dq	0x3956c25b3956c25b, 0x3956c25b3956c25b
	dq	0x59f111f159f111f1, 0x59f111f159f111f1
	dq	0x59f111f159f111f1, 0x59f111f159f111f1
	dq	0x923f82a4923f82a4, 0x923f82a4923f82a4
	dq	0x923f82a4923f82a4, 0x923f82a4923f82a4
	dq	0xab1c5ed5ab1c5ed5, 0xab1c5ed5ab1c5ed5
	dq	0xab1c5ed5ab1c5ed5, 0xab1c5ed5ab1c5ed5
	dq	0xd807aa98d807aa98, 0xd807aa98d807aa98
	dq	0xd807aa98d807aa98, 0xd807aa98d807aa98
	dq	0x12835b0112835b01, 0x12835b0112835b01
	dq	0x12835b0112835b01, 0x12835b0112835b01
	dq	0x243185be243185be, 0x243185be243185be
	dq	0x243185be243185be, 0x243185be243185be
	dq	0x550c7dc3550c7dc3, 0x550c7dc3550c7dc3
	dq	0x550c7dc3550c7dc3, 0x550c7dc3550c7dc3
	dq	0x72be5d7472be5d74, 0x72be5d7472be5d74
	dq	0x72be5d7472be5d74, 0x72be5d7472be5d74
	dq	0x80deb1fe80deb1fe, 0x80deb1fe80deb1fe
	dq	0x80deb1fe80deb1fe, 0x80deb1fe80deb1fe
	dq	0x9bdc06a79bdc06a7, 0x9bdc06a79bdc06a7
	dq	0x9bdc06a79bdc06a7, 0x9bdc06a79bdc06a7
	dq	0xc19bf174c19bf174, 0xc19bf174c19bf174
	dq	0xc19bf174c19bf174, 0xc19bf174c19bf174
	dq	0xe49b69c1e49b69c1, 0xe49b69c1e49b69c1
	dq	0xe49b69c1e49b69c1, 0xe49b69c1e49b69c1
	dq	0xefbe4786efbe4786, 0xefbe4786efbe4786
	dq	0xefbe4786efbe4786, 0xefbe4786efbe4786
	dq	0x0fc19dc60fc19dc6, 0x0fc19dc60fc19dc6
	dq	0x0fc19dc60fc19dc6, 0x0fc19dc60fc19dc6
	dq	0x240ca1cc240ca1cc, 0x240ca1cc240ca1cc
	dq	0x240ca1cc240ca1cc, 0x240ca1cc240ca1cc
	dq	0x2de92c6f2de92c6f, 0x2de92c6f2de92c6f
	dq	0x2de92c6f2de92c6f, 0x2de92c6f2de92c6f
	dq	0x4a7484aa4a7484aa, 0x4a7484aa4a7484aa
	dq	0x4a7484aa4a7484aa, 0x4a7484aa4a7484aa
	dq	0x5cb0a9dc5cb0a9dc, 0x5cb0a9dc5cb0a9dc
	dq	0x5cb0a9dc5cb0a9dc, 0x5cb0a9dc5cb0a9dc
	dq	0x76f988da76f988da, 0x76f988da76f988da
	dq	0x76f988da76f988da, 0x76f988da76f988da
	dq	0x983e5152983e5152, 0x983e5152983e5152
	dq	0x983e5152983e5152, 0x983e5152983e5152
	dq	0xa831c66da831c66d, 0xa831c66da831c66d
	dq	0xa831c66da831c66d, 0xa831c66da831c66d
	dq	0xb00327c8b00327c8, 0xb00327c8b00327c8
	dq	0xb00327c8b00327c8, 0xb00327c8b00327c8
	dq	0xbf597fc7bf597fc7, 0xbf597fc7bf597fc7
	dq	0xbf597fc7bf597fc7, 0xbf597fc7bf597fc7
	dq	0xc6e00bf3c6e00bf3, 0xc6e00bf3c6e00bf3
	dq	0xc6e00bf3c6e00bf3, 0xc6e00bf3c6e00bf3
	dq	0xd5a79147d5a79147, 0xd5a79147d5a79147
	dq	0xd5a79147d5a79147, 0xd5a79147d5a79147
	dq	0x06ca635106ca6351, 0x06ca635106ca6351
	dq	0x06ca635106ca6351, 0x06ca635106ca6351
	dq	0x1429296714292967, 0x1429296714292967
	dq	0x1429296714292967, 0x1429296714292967
	dq	0x27b70a8527b70a85, 0x27b70a8527b70a85
	dq	0x27b70a8527b70a85, 0x27b70a8527b70a85
	dq	0x2e1b21382e1b2138, 0x2e1b21382e1b2138
	dq	0x2e1b21382e1b2138, 0x2e1b21382e1b2138
	dq	0x4d2c6dfc4d2c6dfc, 0x4d2c6dfc4d2c6dfc
	dq	0x4d2c6dfc4d2c6dfc, 0x4d2c6dfc4d2c6dfc
	dq	0x53380d1353380d13, 0x53380d1353380d13
	dq	0x53380d1353380d13, 0x53380d1353380d13
	dq	0x650a7354650a7354, 0x650a7354650a7354
	dq	0x650a7354650a7354, 0x650a7354650a7354
	dq	0x766a0abb766a0abb, 0x766a0abb766a0abb
	dq	0x766a0abb766a0abb, 0x766a0abb766a0abb
	dq	0x81c2c92e81c2c92e, 0x81c2c92e81c2c92e
	dq	0x81c2c92e81c2c92e, 0x81c2c92e81c2c92e
	dq	0x92722c8592722c85, 0x92722c8592722c85
	dq	0x92722c8592722c85, 0x92722c8592722c85
	dq	0xa2bfe8a1a2bfe8a1, 0xa2bfe8a1a2bfe8a1
	dq	0xa2bfe8a1a2bfe8a1, 0xa2bfe8a1a2bfe8a1
	dq	0xa81a664ba81a664b, 0xa81a664ba81a664b
	dq	0xa81a664ba81a664b, 0xa81a664ba81a664b
	dq	0xc24b8b70c24b8b70, 0xc24b8b70c24b8b70
	dq	0xc24b8b70c24b8b70, 0xc24b8b70c24b8b70
	dq	0xc76c51a3c76c51a3, 0xc76c51a3c76c51a3
	dq	0xc76c51a3c76c51a3, 0xc76c51a3c76c51a3
	dq	0xd192e819d192e819, 0xd192e819d192e819
	dq	0xd192e819d192e819, 0xd192e819d192e819
	dq	0xd6990624d6990624, 0xd6990624d6990624
	dq	0xd6990624d6990624, 0xd6990624d6990624
	dq	0xf40e3585f40e3585, 0xf40e3585f40e3585
	dq	0xf40e3585f40e3585, 0xf40e3585f40e3585
	dq	0x106aa070106aa070, 0x106aa070106aa070
	dq	0x106aa070106aa070, 0x106aa070106aa070
	dq	0x19a4c11619a4c116, 0x19a4c11619a4c116
	dq	0x19a4c11619a4c116, 0x19a4c11619a4c116
	dq	0x1e376c081e376c08, 0x1e376c081e376c08
	dq	0x1e376c081e376c08, 0x1e376c081e376c08
	dq	0x2748774c2748774c, 0x2748774c2748774c
	dq	0x2748774c2748774c, 0x2748774c2748774c
	dq	0x34b0bcb534b0bcb5, 0x34b0bcb534b0bcb5
	dq	0x34b0bcb534b0bcb5, 0x34b0bcb534b0bcb5
	dq	0x391c0cb3391c0cb3, 0x391c0cb3391c0cb3
	dq	0x391c0cb3391c0cb3, 0x391c0cb3391c0cb3
	dq	0x4ed8aa4a4ed8aa4a, 0x4ed8aa4a4ed8aa4a
	dq	0x4ed8aa4a4ed8aa4a, 0x4ed8aa4a4ed8aa4a
	dq	0x5b9cca4f5b9cca4f, 0x5b9cca4f5b9cca4f
	dq	0x5b9cca4f5b9cca4f, 0x5b9cca4f5b9cca4f
	dq	0x682e6ff3682e6ff3, 0x682e6ff3682e6ff3
	dq	0x682e6ff3682e6ff3, 0x682e6ff3682e6ff3
	dq	0x748f82ee748f82ee, 0x748f82ee748f82ee
	dq	0x748f82ee748f82ee, 0x748f82ee748f82ee
	dq	0x78a5636f78a5636f, 0x78a5636f78a5636f
	dq	0x78a5636f78a5636f, 0x78a5636f78a5636f
	dq	0x84c8781484c87814, 0x84c8781484c87814
	dq	0x84c8781484c87814, 0x84c8781484c87814
	dq	0x8cc702088cc70208, 0x8cc702088cc70208
	dq	0x8cc702088cc70208, 0x8cc702088cc70208
	dq	0x90befffa90befffa, 0x90befffa90befffa
	dq	0x90befffa90befffa, 0x90befffa90befffa
	dq	0xa4506ceba4506ceb, 0xa4506ceba4506ceb
	dq	0xa4506ceba4506ceb, 0xa4506ceba4506ceb
	dq	0xbef9a3f7bef9a3f7, 0xbef9a3f7bef9a3f7
	dq	0xbef9a3f7bef9a3f7, 0xbef9a3f7bef9a3f7
	dq	0xc67178f2c67178f2, 0xc67178f2c67178f2
	dq	0xc67178f2c67178f2, 0xc67178f2c67178f2

PSHUFFLE_BYTE_FLIP_MASK: ;ddq 0x0c0d0e0f08090a0b0405060700010203
                         ;ddq 0x0c0d0e0f08090a0b0405060700010203
	dq 0x0405060700010203, 0x0c0d0e0f08090a0b
	dq 0x0405060700010203, 0x0c0d0e0f08090a0b

extern K256
mksection .text

%define XMM_STORAGE     10*16
%define GP_STORAGE      9*8

%define VARIABLE_OFFSET XMM_STORAGE + GP_STORAGE
%define GP_OFFSET XMM_STORAGE

%macro FUNC_SAVE 0
    mov      r11, rsp
    sub      rsp, VARIABLE_OFFSET
    and      rsp, ~31	; align rsp to 32 bytes

    mov      [rsp + 0*8],  rbx
    mov      [rsp + 1*8],  rbp
    mov      [rsp + 2*8],  r12
    mov      [rsp + 3*8],  r13
    mov      [rsp + 4*8],  r14
    mov      [rsp + 5*8],  r15
%ifndef LINUX
    mov      [rsp + 6*8],  rsi
    mov      [rsp + 7*8],  rdi
    vmovdqa  [rsp + 4*16], xmm6
    vmovdqa  [rsp + 5*16], xmm7
    vmovdqa  [rsp + 6*16], xmm8
    vmovdqa  [rsp + 7*16], xmm9
    vmovdqa  [rsp + 8*16], xmm10
    vmovdqa  [rsp + 9*16], xmm11
    vmovdqa  [rsp + 10*16], xmm12
    vmovdqa  [rsp + 11*16], xmm13
    vmovdqa  [rsp + 12*16], xmm14
    vmovdqa  [rsp + 13*16], xmm15
%endif ; LINUX
    mov      [rsp + 14*16], r11 ;; rsp pointer
%endmacro

%macro FUNC_RESTORE 0
    mov      rbx,  [rsp + 0*8]
    mov      rbp,  [rsp + 1*8]
    mov      r12,  [rsp + 2*8]
    mov      r13,  [rsp + 3*8]
    mov      r14,  [rsp + 4*8]
    mov      r15,  [rsp + 5*8]
%ifndef LINUX
    mov      rsi,   [rsp + 6*8]
    mov      rdi,   [rsp + 7*8]
    vmovdqa  xmm6,  [rsp + 4*16]
    vmovdqa  xmm7,  [rsp + 5*16]
    vmovdqa  xmm8,  [rsp + 6*16]
    vmovdqa  xmm9,  [rsp + 7*16]
    vmovdqa  xmm10, [rsp + 8*16]
    vmovdqa  xmm11, [rsp + 9*16]
    vmovdqa  xmm12, [rsp + 10*16]
    vmovdqa  xmm13, [rsp + 11*16]
    vmovdqa  xmm14, [rsp + 12*16]
    vmovdqa  xmm15, [rsp + 13*16]

%ifdef SAFE_DATA
    vpxor    xmm5, xmm5, xmm5
    vmovdqa  xmm5,  [rsp + 4*16]
    vmovdqa  xmm5,  [rsp + 5*16]
    vmovdqa  xmm5,  [rsp + 6*16]
    vmovdqa  xmm5,  [rsp + 7*16]
    vmovdqa  xmm5,  [rsp + 8*16]
    vmovdqa  xmm5,  [rsp + 9*16]
    vmovdqa  xmm5,  [rsp + 10*16]
    vmovdqa  xmm5,  [rsp + 11*16]
    vmovdqa  xmm5,  [rsp + 12*16]
    vmovdqa  xmm5,  [rsp + 13*16]
%endif
%endif ; LINUX
    mov      rsp,   [rsp + 14*16] ;; rsp pointer
%endmacro

%ifdef LINUX
     %define arg1 	rdi
     %define arg2	rsi
     %define reg3	rcx
     %define reg4	rdx
%else
 ; Windows definitions
     %define arg1 	rcx
     %define arg2 	rdx
     %define reg3	rsi
     %define reg4	rdi
%endif

; Common definitions
%define STATE    arg1
%define INP_SIZE arg2

%define IDX     rax
%define ROUND	rbx
%define TBL	reg3

%define inp0 r9
%define inp1 r10
%define inp2 r11
%define inp3 r12
%define inp4 r13
%define inp5 r14
%define inp6 r8
%define inp7 reg4

; ymm0	a
; ymm1	b
; ymm2	c
; ymm3	d
; ymm4	e
; ymm5	f
; ymm6	g	TMP0
; ymm7	h	TMP1
; ymm8	T1	TT0
; ymm9		TT1
; ymm10		TT2
; ymm11		TT3
; ymm12	a0	TT4
; ymm13	a1	TT5
; ymm14	a2	TT6
; ymm15	TMP	TT7

%define a ymm0
%define b ymm1
%define c ymm2
%define d ymm3
%define e ymm4
%define f ymm5
%define g ymm6
%define h ymm7

%define T1  ymm8

%define a0 ymm12
%define a1 ymm13
%define a2 ymm14
%define TMP ymm15

%define TMP0 ymm6
%define TMP1 ymm7

%define TT0 ymm8
%define TT1 ymm9
%define TT2 ymm10
%define TT3 ymm11
%define TT4 ymm12
%define TT5 ymm13
%define TT6 ymm14
%define TT7 ymm15

%define SZ8	8*SHA256_DIGEST_WORD_SIZE	; Size of one vector register
%define ROUNDS 64*SZ8

; Define stack usage

;; Assume stack aligned to 32 bytes before call
;; Therefore FRAMESZ mod 32 must be 32-8 = 24
struc stack_frame
  .data		resb	16*SZ8
  .digest	resb	8*SZ8
  .ytmp		resb	4*SZ8
  .align	resb	24
endstruc
%define FRAMESZ	stack_frame_size
%define _DIGEST	stack_frame.digest
%define _YTMP	stack_frame.ytmp

%define YTMP0	rsp + _YTMP + 0*SZ8
%define YTMP1	rsp + _YTMP + 1*SZ8
%define YTMP2	rsp + _YTMP + 2*SZ8
%define YTMP3	rsp + _YTMP + 3*SZ8

%define VMOVPS	vmovups

%macro ROTATE_ARGS 0
%xdefine TMP_ h
%xdefine h g
%xdefine g f
%xdefine f e
%xdefine e d
%xdefine d c
%xdefine c b
%xdefine b a
%xdefine a TMP_
%endm

; PRORD reg, imm, tmp
%macro PRORD 3
%define %%reg %1
%define %%imm %2
%define %%tmp %3
	vpslld	%%tmp, %%reg, (32-(%%imm))
	vpsrld	%%reg, %%reg, %%imm
	vpor	%%reg, %%reg, %%tmp
%endmacro

; non-destructive
; PRORD_nd reg, imm, tmp, src
%macro PRORD_nd 4
%define %%reg %1
%define %%imm %2
%define %%tmp %3
%define %%src %4
	;vmovdqa	%%tmp, %%reg
	vpslld	%%tmp, %%src, (32-(%%imm))
	vpsrld	%%reg, %%src, %%imm
	vpor	%%reg, %%reg, %%tmp
%endmacro

; PRORD dst/src, amt
%macro PRORD 2
	PRORD	%1, %2, TMP
%endmacro

; PRORD_nd dst, src, amt
%macro PRORD_nd 3
	PRORD_nd	%1, %3, TMP, %2
%endmacro

;; arguments passed implicitly in preprocessor symbols i, a...h
%macro ROUND_00_15 2
%define %%T1 %1
%define %%i  %2
	PRORD_nd	a0, e, (11-6)	; sig1: a0 = (e >> 5)

	vpxor	a2, f, g	; ch: a2 = f^g
	vpand	a2, a2, e		; ch: a2 = (f^g)&e
	vpxor	a2, a2, g		; a2 = ch

	PRORD_nd	a1, e, 25		; sig1: a1 = (e >> 25)
	vmovdqa	[SZ8*(%%i&0xf) + rsp], %%T1
	vpaddd	%%T1, %%T1, [TBL + ROUND]	; T1 = W + K
	vpxor	a0, a0, e	; sig1: a0 = e ^ (e >> 5)
	PRORD	a0, 6		; sig1: a0 = (e >> 6) ^ (e >> 11)
	vpaddd	h, h, a2	; h = h + ch
	PRORD_nd	a2, a, (13-2)	; sig0: a2 = (a >> 11)
	vpaddd	h, h, %%T1	; h = h + ch + W + K
	vpxor	a0, a0, a1	; a0 = sigma1
	PRORD_nd	a1, a, 22	; sig0: a1 = (a >> 22)
	vpxor	%%T1, a, c	; maj: T1 = a^c
	add	ROUND, SZ8	; ROUND++
	vpand	%%T1, %%T1, b	; maj: T1 = (a^c)&b
	vpaddd	h, h, a0

	vpaddd	d, d, h

	vpxor	a2, a2, a	; sig0: a2 = a ^ (a >> 11)
	PRORD	a2, 2		; sig0: a2 = (a >> 2) ^ (a >> 13)
	vpxor	a2, a2, a1	; a2 = sig0
	vpand	a1, a, c	; maj: a1 = a&c
	vpor	a1, a1, %%T1	; a1 = maj
	vpaddd	h, h, a1	; h = h + ch + W + K + maj
	vpaddd	h, h, a2	; h = h + ch + W + K + maj + sigma0

	ROTATE_ARGS
%endm

;; arguments passed implicitly in preprocessor symbols i, a...h
%macro ROUND_16_XX 2
%define %%T1 %1
%define %%i  %2
	vmovdqa	%%T1, [SZ8*((%%i-15)&0xf) + rsp]
	vmovdqa	a1, [SZ8*((%%i-2)&0xf) + rsp]
	vmovdqa	a0, %%T1
	PRORD	%%T1, 18-7
	vmovdqa	a2, a1
	PRORD	a1, 19-17
	vpxor	%%T1, %%T1, a0
	PRORD	%%T1, 7
	vpxor	a1, a1, a2
	PRORD	a1, 17
	vpsrld	a0, a0, 3
	vpxor	%%T1, %%T1, a0
	vpsrld	a2, a2, 10
	vpxor	a1, a1, a2
	vpaddd	%%T1, %%T1, [SZ8*((%%i-16)&0xf) + rsp]
	vpaddd	a1, a1, [SZ8*((%%i-7)&0xf) + rsp]
	vpaddd	%%T1, %%T1, a1

	ROUND_00_15 %%T1, %%i

%endm

;; SHA256_ARGS:
;;   UINT128 digest[8];  // transposed digests
;;   UINT8  *data_ptr[4];
;;

;; void sha256_oct_avx2(SHA256_ARGS *args, UINT64 bytes);
;; arg 1 : STATE : pointer to array of pointers to input data
;; arg 2 : INP_SIZE  : size of input in blocks
MKGLOBAL(sha256_oct_avx2,function,internal)
align 16
sha256_oct_avx2:
	; general registers preserved in outer calling routine
	; outer calling routine saves all the XMM registers
	sub	rsp, FRAMESZ

	;; Load the pre-transposed incoming digest.
	vmovdqu	a,[STATE + 0*SHA256_DIGEST_ROW_SIZE]
	vmovdqu	b,[STATE + 1*SHA256_DIGEST_ROW_SIZE]
	vmovdqu	c,[STATE + 2*SHA256_DIGEST_ROW_SIZE]
	vmovdqu	d,[STATE + 3*SHA256_DIGEST_ROW_SIZE]
	vmovdqu	e,[STATE + 4*SHA256_DIGEST_ROW_SIZE]
	vmovdqu	f,[STATE + 5*SHA256_DIGEST_ROW_SIZE]
	vmovdqu	g,[STATE + 6*SHA256_DIGEST_ROW_SIZE]
	vmovdqu	h,[STATE + 7*SHA256_DIGEST_ROW_SIZE]

	lea	TBL,[rel K256_8]

	;; load the address of each of the 4 message lanes
	;; getting ready to transpose input onto stack
	mov	inp0,[STATE + _data_ptr_sha256 + 0*PTR_SZ]
	mov	inp1,[STATE + _data_ptr_sha256 + 1*PTR_SZ]
	mov	inp2,[STATE + _data_ptr_sha256 + 2*PTR_SZ]
	mov	inp3,[STATE + _data_ptr_sha256 + 3*PTR_SZ]
	mov	inp4,[STATE + _data_ptr_sha256 + 4*PTR_SZ]
	mov	inp5,[STATE + _data_ptr_sha256 + 5*PTR_SZ]
	mov	inp6,[STATE + _data_ptr_sha256 + 6*PTR_SZ]
	mov	inp7,[STATE + _data_ptr_sha256 + 7*PTR_SZ]

	xor	IDX, IDX
lloop:
	xor	ROUND, ROUND

	;; save old digest
	vmovdqa	[rsp + _DIGEST + 0*SZ8], a
	vmovdqa	[rsp + _DIGEST + 1*SZ8], b
	vmovdqa	[rsp + _DIGEST + 2*SZ8], c
	vmovdqa	[rsp + _DIGEST + 3*SZ8], d
	vmovdqa	[rsp + _DIGEST + 4*SZ8], e
	vmovdqa	[rsp + _DIGEST + 5*SZ8], f
	vmovdqa	[rsp + _DIGEST + 6*SZ8], g
	vmovdqa	[rsp + _DIGEST + 7*SZ8], h
	DBGPRINTL_YMM "transposed digest ", a,b,c,d,e,f,g,h
%assign i 0
%rep 2
	TRANSPOSE8_U32_LOAD8 TT0, TT1, TT2, TT3, TT4, TT5, TT6, TT7, \
			     inp0, inp1, inp2, inp3, inp4, inp5, \
			     inp6, inp7, IDX+i*32

	vmovdqa	[YTMP0], g
	vmovdqa	[YTMP1], h
	TRANSPOSE8_U32_PRELOADED TT0, TT1, TT2, TT3, TT4, TT5, TT6, TT7,   TMP0, TMP1
	DBGPRINTL_YMM "transposed input ", TT0, TT1, TT2, TT3, TT4, TT5, TT6, TT7
	vmovdqa	TMP1, [rel PSHUFFLE_BYTE_FLIP_MASK]
	vmovdqa	g, [YTMP0]
	vpshufb	TT0, TT0, TMP1
	vpshufb	TT1, TT1, TMP1
	vpshufb	TT2, TT2, TMP1
	vpshufb	TT3, TT3, TMP1
	vpshufb	TT4, TT4, TMP1
	vpshufb	TT5, TT5, TMP1
	vpshufb	TT6, TT6, TMP1
	vpshufb	TT7, TT7, TMP1
	vmovdqa	h, [YTMP1]
	vmovdqa	[YTMP0], TT4
	vmovdqa	[YTMP1], TT5
	vmovdqa	[YTMP2], TT6
	vmovdqa	[YTMP3], TT7
	ROUND_00_15	TT0,(i*8+0)
	vmovdqa	TT0, [YTMP0]
	ROUND_00_15	TT1,(i*8+1)
	vmovdqa	TT1, [YTMP1]
	ROUND_00_15	TT2,(i*8+2)
	vmovdqa	TT2, [YTMP2]
	ROUND_00_15	TT3,(i*8+3)
	vmovdqa	TT3, [YTMP3]
	ROUND_00_15	TT0,(i*8+4)
	ROUND_00_15	TT1,(i*8+5)
	ROUND_00_15	TT2,(i*8+6)
	ROUND_00_15	TT3,(i*8+7)
%assign i (i+1)
%endrep
	add	IDX, 4*4*4

%assign i (i*8)

	jmp	Lrounds_16_xx
align 16
Lrounds_16_xx:
%rep 16
	ROUND_16_XX	T1, i
%assign i (i+1)
%endrep

	cmp	ROUND,ROUNDS
	jb	Lrounds_16_xx

	;; add old digest
	vpaddd	a, a, [rsp + _DIGEST + 0*SZ8]
	vpaddd	b, b, [rsp + _DIGEST + 1*SZ8]
	vpaddd	c, c, [rsp + _DIGEST + 2*SZ8]
	vpaddd	d, d, [rsp + _DIGEST + 3*SZ8]
	vpaddd	e, e, [rsp + _DIGEST + 4*SZ8]
	vpaddd	f, f, [rsp + _DIGEST + 5*SZ8]
	vpaddd	g, g, [rsp + _DIGEST + 6*SZ8]
	vpaddd	h, h, [rsp + _DIGEST + 7*SZ8]

	sub	INP_SIZE, 1  ;; unit is blocks
	jne	lloop

	; write back to memory (state object) the transposed digest
	vmovdqu	[STATE + 0*SHA256_DIGEST_ROW_SIZE],a
	vmovdqu	[STATE + 1*SHA256_DIGEST_ROW_SIZE],b
	vmovdqu	[STATE + 2*SHA256_DIGEST_ROW_SIZE],c
	vmovdqu	[STATE + 3*SHA256_DIGEST_ROW_SIZE],d
	vmovdqu	[STATE + 4*SHA256_DIGEST_ROW_SIZE],e
	vmovdqu	[STATE + 5*SHA256_DIGEST_ROW_SIZE],f
	vmovdqu	[STATE + 6*SHA256_DIGEST_ROW_SIZE],g
	vmovdqu	[STATE + 7*SHA256_DIGEST_ROW_SIZE],h
	DBGPRINTL_YMM "sha256 digest on exit ", a,b,c,d,e,f,g,h

	; update input pointers
	add	inp0, IDX
	mov	[STATE + _data_ptr_sha256 + 0*8], inp0
	add	inp1, IDX
	mov	[STATE + _data_ptr_sha256 + 1*8], inp1
	add	inp2, IDX
	mov	[STATE + _data_ptr_sha256 + 2*8], inp2
	add	inp3, IDX
	mov	[STATE + _data_ptr_sha256 + 3*8], inp3
	add	inp4, IDX
	mov	[STATE + _data_ptr_sha256 + 4*8], inp4
	add	inp5, IDX
	mov	[STATE + _data_ptr_sha256 + 5*8], inp5
	add	inp6, IDX
	mov	[STATE + _data_ptr_sha256 + 6*8], inp6
	add	inp7, IDX
	mov	[STATE + _data_ptr_sha256 + 7*8], inp7

	;;;;;;;;;;;;;;;;
	;; Postamble

%ifdef SAFE_DATA
        ;; Clear stack frame ((16+8+4)*32 bytes)
        clear_all_ymms_asm
%assign i 0
%rep (16+8+4)
	vmovdqa [rsp + i*SZ8], ymm0
%assign i (i+1)
%endrep
%endif

	add rsp, FRAMESZ
	ret

; void call_sha256_oct_avx2_from_c(SHA256_ARGS *args, UINT32 size_in_blocks);
MKGLOBAL(call_sha256_oct_avx2_from_c,function,internal)
call_sha256_oct_avx2_from_c:
	FUNC_SAVE
	call sha256_oct_avx2
	FUNC_RESTORE
	ret

mksection stack-noexec
