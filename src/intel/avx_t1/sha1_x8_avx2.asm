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

;; Stack must be aligned to 32 bytes before call
;; Windows clobbers:  rax	  rdx		  r8 r9 r10 r11 r12 r13 r14 r15
;; Windows preserves:	  rbx rcx     rsi rdi rbp
;;
;; Linux clobbers:    rax	  rdx rsi	     r9 r10 r11 r12 r13 r14 r15
;; Linux preserves:	  rbx rcx	  rdi rbp r8
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
align 32
PSHUFFLE_BYTE_FLIP_MASK: ;ddq 0x0c0d0e0f08090a0b0405060700010203
			 ;ddq 0x0c0d0e0f08090a0b0405060700010203
	dq 0x0405060700010203, 0x0c0d0e0f08090a0b
	dq 0x0405060700010203, 0x0c0d0e0f08090a0b
K00_19:                  ;ddq 0x5A8279995A8279995A8279995A827999
	                 ;ddq 0x5A8279995A8279995A8279995A827999
	dq 0x5A8279995A827999, 0x5A8279995A827999
	dq 0x5A8279995A827999, 0x5A8279995A827999
K20_39:                  ;ddq 0x6ED9EBA16ED9EBA16ED9EBA16ED9EBA1
	                 ;ddq 0x6ED9EBA16ED9EBA16ED9EBA16ED9EBA1
	dq 0x6ED9EBA16ED9EBA1, 0x6ED9EBA16ED9EBA1
	dq 0x6ED9EBA16ED9EBA1, 0x6ED9EBA16ED9EBA1
K40_59:                  ;ddq 0x8F1BBCDC8F1BBCDC8F1BBCDC8F1BBCDC
			 ;ddq 0x8F1BBCDC8F1BBCDC8F1BBCDC8F1BBCDC
	dq 0x8F1BBCDC8F1BBCDC, 0x8F1BBCDC8F1BBCDC
	dq 0x8F1BBCDC8F1BBCDC, 0x8F1BBCDC8F1BBCDC
K60_79:                  ;ddq 0xCA62C1D6CA62C1D6CA62C1D6CA62C1D6
	                 ;ddq 0xCA62C1D6CA62C1D6CA62C1D6CA62C1D6
	dq 0xCA62C1D6CA62C1D6, 0xCA62C1D6CA62C1D6
	dq 0xCA62C1D6CA62C1D6, 0xCA62C1D6CA62C1D6

mksection .text

%define XMM_STORAGE     16*10
%define GP_STORAGE      8*5

%define VARIABLE_OFFSET XMM_STORAGE + GP_STORAGE
%define GP_OFFSET XMM_STORAGE

%ifdef LINUX
%define arg1	rdi
%define arg2	rsi
%define reg3	rdx
%else
%define arg1	rcx
%define arg2	rdx
%define reg3	r8
%endif

%define state arg1
%define num_blks arg2

%define inp0 r9
%define inp1 r10
%define inp2 r11
%define inp3 r12
%define inp4 r13
%define inp5 r14
%define inp6 r15
%define inp7 reg3

%define IDX  rax

; ymm0	A
; ymm1	B
; ymm2	C
; ymm3	D
; ymm4	E
; ymm5		F	AA
; ymm6		T0	BB
; ymm7		T1	CC
; ymm8		T2	DD
; ymm9		T3	EE
; ymm10		T4	TMP
; ymm11		T5	FUN
; ymm12		T6	K
; ymm13		T7	W14
; ymm14		T8	W15
; ymm15		T9	W16

%define A	ymm0
%define B	ymm1
%define C	ymm2
%define D	ymm3
%define E	ymm4

%define F	ymm5
%define T0	ymm6
%define T1	ymm7
%define T2	ymm8
%define T3	ymm9
%define T4	ymm10
%define T5	ymm11
%define T6	ymm12
%define T7	ymm13
%define T8	ymm14
%define T9	ymm15

%define AA	ymm5
%define BB	ymm6
%define CC	ymm7
%define DD	ymm8
%define EE	ymm9
%define TMP	ymm10
%define FUN	ymm11
%define K	ymm12
%define W14	ymm13
%define W15	ymm14
%define W16	ymm15

;; Assume stack aligned to 32 bytes before call
;; Therefore FRAMESIZE mod 32 must be 32-8 = 24
%define FRAMESZ	32*16 + 16*10 + 24

%define VMOVPS	vmovups

;;
;; Magic functions defined in FIPS 180-1
;;
;MAGIC_F0 MACRO regF:REQ,regB:REQ,regC:REQ,regD:REQ,regT:REQ ;; ((D ^ (B & (C ^ D)))
%macro MAGIC_F0 5
%define %%regF %1
%define %%regB %2
%define %%regC %3
%define %%regD %4
%define %%regT %5
	;vmovdqa  %%regF,%%regC
	vpxor  %%regF, %%regC,%%regD
	vpand  %%regF, %%regF,%%regB
	vpxor  %%regF, %%regF,%%regD
%endmacro

;MAGIC_F1 MACRO regF:REQ,regB:REQ,regC:REQ,regD:REQ,regT:REQ ;; (B ^ C ^ D)
%macro MAGIC_F1 5
%define %%regF %1
%define %%regB %2
%define %%regC %3
%define %%regD %4
%define %%regT %5
	;vmovdqa  %%regF,%%regD
	vpxor  %%regF,%%regD,%%regC
	vpxor  %%regF,%%regF,%%regB
%endmacro

;MAGIC_F2 MACRO regF:REQ,regB:REQ,regC:REQ,regD:REQ,regT:REQ ;; ((B & C) | (B & D) | (C & D))
%macro MAGIC_F2 5
%define %%regF %1
%define %%regB %2
%define %%regC %3
%define %%regD %4
%define %%regT %5
	;vmovdqa  %%regF,%%regB
	;vmovdqa  %%regT,%%regB
	vpor   %%regF,%%regB,%%regC
	vpand  %%regT,%%regB,%%regC
	vpand  %%regF,%%regF,%%regD
	vpor   %%regF,%%regF,%%regT
%endmacro

;MAGIC_F3 MACRO regF:REQ,regB:REQ,regC:REQ,regD:REQ,regT:REQ
%macro MAGIC_F3 5
%define %%regF %1
%define %%regB %2
%define %%regC %3
%define %%regD %4
%define %%regT %5
	MAGIC_F1 %%regF,%%regB,%%regC,%%regD,%%regT
%endmacro

; PROLD reg, imm, tmp
%macro PROLD 3
%define %%reg %1
%define %%imm %2
%define %%tmp %3
	;vmovdqa	%%tmp, %%reg
	vpsrld	%%tmp, %%reg, (32-%%imm)
	vpslld	%%reg, %%reg, %%imm
	vpor	%%reg, %%reg, %%tmp
%endmacro

; PROLD reg, imm, tmp
%macro PROLD_nd 4
%define %%reg %1
%define %%imm %2
%define %%tmp %3
%define %%src %4
	;vmovdqa	%%tmp, %%reg
	vpsrld	%%tmp, %%src, (32-%%imm)
	vpslld	%%reg, %%src, %%imm
	vpor	%%reg, %%reg, %%tmp
%endmacro

%macro SHA1_STEP_00_15 10
%define %%regA	%1
%define %%regB	%2
%define %%regC	%3
%define %%regD	%4
%define %%regE	%5
%define %%regT	%6
%define %%regF	%7
%define %%memW	%8
%define %%immCNT %9
%define %%MAGIC	%10
	vpaddd	%%regE, %%regE,%%immCNT
	vpaddd	%%regE, %%regE,[rsp + (%%memW * 32)]
	;vmovdqa	%%regT,%%regA
	PROLD_nd	%%regT,5, %%regF,%%regA
	vpaddd	%%regE, %%regE,%%regT
	%%MAGIC	%%regF,%%regB,%%regC,%%regD,%%regT	;; FUN	= MAGIC_Fi(B,C,D)
	PROLD	%%regB,30, %%regT
	vpaddd	%%regE, %%regE,%%regF
%endmacro

%macro SHA1_STEP_16_79 10
%define %%regA	%1
%define %%regB	%2
%define %%regC	%3
%define %%regD	%4
%define %%regE	%5
%define %%regT	%6
%define %%regF	%7
%define %%memW	%8
%define %%immCNT %9
%define %%MAGIC	%10
	vpaddd	%%regE, %%regE,%%immCNT

	vmovdqa	W14, [rsp + ((%%memW - 14) & 15) * 32]
	vpxor	W16, W16, W14
	vpxor	W16, W16, [rsp + ((%%memW -  8) & 15) * 32]
	vpxor	W16, W16, [rsp + ((%%memW -  3) & 15) * 32]

	;vmovdqa	%%regF, W16
	vpsrld	%%regF, W16, (32-1)
	vpslld	W16, W16, 1
	vpor	%%regF, %%regF, W16
	ROTATE_W

	vmovdqa	[rsp + ((%%memW - 0) & 15) * 32],%%regF
	vpaddd	%%regE, %%regE,%%regF

	;vmovdqa	%%regT,%%regA
	PROLD_nd	%%regT,5, %%regF, %%regA
	vpaddd	%%regE, %%regE,%%regT
	%%MAGIC	%%regF,%%regB,%%regC,%%regD,%%regT	;; FUN	= MAGIC_Fi(B,C,D)
	PROLD	%%regB,30, %%regT
	vpaddd	%%regE,%%regE,%%regF
%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%macro ROTATE_ARGS 0
%xdefine TMP_ E
%xdefine E D
%xdefine D C
%xdefine C B
%xdefine B A
%xdefine A TMP_
%endm

%macro ROTATE_W 0
%xdefine TMP_ W16
%xdefine W16 W15
%xdefine W15 W14
%xdefine W14 TMP_
%endm

%macro FUNC_SAVE 0
    mov     r11, rsp
    sub     rsp, VARIABLE_OFFSET
    and     rsp, ~31	; align rsp to 32 bytes
%ifndef LINUX
    vmovdqa  [rsp + 0*16], xmm6
    vmovdqa  [rsp + 1*16], xmm7
    vmovdqa  [rsp + 2*16], xmm8
    vmovdqa  [rsp + 3*16], xmm9
    vmovdqa  [rsp + 4*16], xmm10
    vmovdqa  [rsp + 5*16], xmm11
    vmovdqa  [rsp + 6*16], xmm12
    vmovdqa  [rsp + 7*16], xmm13
    vmovdqa  [rsp + 8*16], xmm14
    vmovdqa  [rsp + 9*16], xmm15
%endif
    mov     [rsp + GP_OFFSET], r12
    mov     [rsp + GP_OFFSET + 8], r13
    mov     [rsp + GP_OFFSET + 2*8], r14
    mov     [rsp + GP_OFFSET + 3*8], r15
    mov     [rsp + GP_OFFSET + 4*8], r11 ;; rsp pointer
%endmacro

%macro FUNC_RESTORE 0
%ifndef LINUX
    vmovdqa  xmm6, [rsp + 0*16]
    vmovdqa  xmm7, [rsp + 1*16]
    vmovdqa  xmm8, [rsp + 2*16]
    vmovdqa  xmm9, [rsp + 3*16]
    vmovdqa  xmm10, [rsp + 4*16]
    vmovdqa  xmm11, [rsp + 5*16]
    vmovdqa  xmm12, [rsp + 6*16]
    vmovdqa  xmm13, [rsp + 7*16]
    vmovdqa  xmm14, [rsp + 8*16]
    vmovdqa  xmm15, [rsp + 9*16]

%ifdef SAFE_DATA
    vpxor    xmm5, xmm5, xmm5
    vmovdqa  [rsp + 0*16], xmm5
    vmovdqa  [rsp + 1*16], xmm5
    vmovdqa  [rsp + 2*16], xmm5
    vmovdqa  [rsp + 3*16], xmm5
    vmovdqa  [rsp + 4*16], xmm5
    vmovdqa  [rsp + 5*16], xmm5
    vmovdqa  [rsp + 6*16], xmm5
    vmovdqa  [rsp + 7*16], xmm5
    vmovdqa  [rsp + 8*16], xmm5
    vmovdqa  [rsp + 9*16], xmm5
%endif
%endif
    mov     r12, [rsp + GP_OFFSET]
    mov     r13, [rsp + GP_OFFSET + 8]
    mov     r14, [rsp + GP_OFFSET + 2*8]
    mov     r15, [rsp + GP_OFFSET + 3*8]
    mov     rsp, [rsp + GP_OFFSET + 4*8] ;; rsp pointer
%endmacro

align 32

; void sha1_x8_avx2(void *state, int num_blks)
; arg 1 : rcx : pointer to array[4] of pointer to input data
; arg 2 : rdx  : size (in blocks) ;; assumed to be >= 1
MKGLOBAL(sha1_x8_avx2,function,internal)
sha1_x8_avx2:
	sub	rsp, FRAMESZ

%ifndef LINUX
        vmovdqa  [rsp + 32*16 + 0*16], xmm6
        vmovdqa  [rsp + 32*16 + 1*16], xmm7
        vmovdqa  [rsp + 32*16 + 2*16], xmm8
        vmovdqa  [rsp + 32*16 + 3*16], xmm9
        vmovdqa  [rsp + 32*16 + 4*16], xmm10
        vmovdqa  [rsp + 32*16 + 5*16], xmm11
        vmovdqa  [rsp + 32*16 + 6*16], xmm12
        vmovdqa  [rsp + 32*16 + 7*16], xmm13
        vmovdqa  [rsp + 32*16 + 8*16], xmm14
        vmovdqa  [rsp + 32*16 + 9*16], xmm15
%endif

	;; Initialize digests
	vmovdqu	A, [state + 0*SHA1_DIGEST_ROW_SIZE]
	vmovdqu	B, [state + 1*SHA1_DIGEST_ROW_SIZE]
	vmovdqu	C, [state + 2*SHA1_DIGEST_ROW_SIZE]
	vmovdqu	D, [state + 3*SHA1_DIGEST_ROW_SIZE]
	vmovdqu	E, [state + 4*SHA1_DIGEST_ROW_SIZE]
	DBGPRINTL_YMM "Sha1-AVX2 incoming transposed digest", A, B, C, D, E

	;; transpose input onto stack
	mov	inp0,[state+_data_ptr_sha1+0*PTR_SZ]
	mov	inp1,[state+_data_ptr_sha1+1*PTR_SZ]
	mov	inp2,[state+_data_ptr_sha1+2*PTR_SZ]
	mov	inp3,[state+_data_ptr_sha1+3*PTR_SZ]
	mov	inp4,[state+_data_ptr_sha1+4*PTR_SZ]
	mov	inp5,[state+_data_ptr_sha1+5*PTR_SZ]
	mov	inp6,[state+_data_ptr_sha1+6*PTR_SZ]
	mov	inp7,[state+_data_ptr_sha1+7*PTR_SZ]

	xor	IDX, IDX
lloop:
	vmovdqa	F, [rel PSHUFFLE_BYTE_FLIP_MASK]
%assign I 0
%rep 2
	TRANSPOSE8_U32_LOAD8 T0, T1, T2, T3, T4, T5, T6, T7, \
			     inp0, inp1, inp2, inp3, inp4, inp5, \
			     inp6, inp7, IDX

	TRANSPOSE8_U32_PRELOADED T0, T1, T2, T3, T4, T5, T6, T7, T8, T9
	DBGPRINTL_YMM "Sha1-AVX2 incoming transposed input", T0, T1, T2, T3, T4, T5, T6, T7, T8, T9
	vpshufb	T0, T0, F
	vmovdqa	[rsp+(I*8+0)*32],T0
	vpshufb	T1, T1, F
	vmovdqa	[rsp+(I*8+1)*32],T1
	vpshufb	T2, T2, F
	vmovdqa	[rsp+(I*8+2)*32],T2
	vpshufb	T3, T3, F
	vmovdqa	[rsp+(I*8+3)*32],T3
	vpshufb	T4, T4, F
	vmovdqa	[rsp+(I*8+4)*32],T4
	vpshufb	T5, T5, F
	vmovdqa	[rsp+(I*8+5)*32],T5
	vpshufb	T6, T6, F
	vmovdqa	[rsp+(I*8+6)*32],T6
	vpshufb	T7, T7, F
	vmovdqa	[rsp+(I*8+7)*32],T7
	add	IDX, 32
%assign I (I+1)
%endrep

	; save old digests
	vmovdqa	AA, A
	vmovdqa	BB, B
	vmovdqa	CC, C
	vmovdqa	DD, D
	vmovdqa	EE, E

;;
;; perform 0-79 steps
;;
	vmovdqa	K, [rel K00_19]
;; do rounds 0...15
%assign I 0
%rep 16
	SHA1_STEP_00_15 A,B,C,D,E, TMP,FUN, I, K, MAGIC_F0
	ROTATE_ARGS
%assign I (I+1)
%endrep

;; do rounds 16...19
	vmovdqa	W16, [rsp + ((16 - 16) & 15) * 32]
	vmovdqa	W15, [rsp + ((16 - 15) & 15) * 32]
%rep 4
	SHA1_STEP_16_79 A,B,C,D,E, TMP,FUN, I, K, MAGIC_F0
	ROTATE_ARGS
%assign I (I+1)
%endrep

;; do rounds 20...39
	vmovdqa	K, [rel K20_39]
%rep 20
	SHA1_STEP_16_79 A,B,C,D,E, TMP,FUN, I, K, MAGIC_F1
	ROTATE_ARGS
%assign I (I+1)
%endrep

;; do rounds 40...59
	vmovdqa	K, [rel K40_59]
%rep 20
	SHA1_STEP_16_79 A,B,C,D,E, TMP,FUN, I, K, MAGIC_F2
	ROTATE_ARGS
%assign I (I+1)
%endrep

;; do rounds 60...79
	vmovdqa	K, [rel K60_79]
%rep 20
	SHA1_STEP_16_79 A,B,C,D,E, TMP,FUN, I, K, MAGIC_F3
	ROTATE_ARGS
%assign I (I+1)
%endrep

	vpaddd	A,A,AA
	vpaddd	B,B,BB
	vpaddd	C,C,CC
	vpaddd	D,D,DD
	vpaddd	E,E,EE

	sub	num_blks, 1
	jne	lloop

	; write out digests
	vmovdqu	[state + 0*SHA1_DIGEST_ROW_SIZE], A
	vmovdqu	[state + 1*SHA1_DIGEST_ROW_SIZE], B
	vmovdqu	[state + 2*SHA1_DIGEST_ROW_SIZE], C
	vmovdqu	[state + 3*SHA1_DIGEST_ROW_SIZE], D
	vmovdqu	[state + 4*SHA1_DIGEST_ROW_SIZE], E
	DBGPRINTL_YMM "Sha1-AVX2 outgoing transposed digest", A, B, C, D, E
	;; update input pointers
	add	inp0, IDX
	add	inp1, IDX
	add	inp2, IDX
	add	inp3, IDX
	add	inp4, IDX
	add	inp5, IDX
	add	inp6, IDX
	add	inp7, IDX
	mov	[state+_data_ptr_sha1+0*PTR_SZ], inp0
	mov	[state+_data_ptr_sha1+1*PTR_SZ], inp1
	mov	[state+_data_ptr_sha1+2*PTR_SZ], inp2
	mov	[state+_data_ptr_sha1+3*PTR_SZ], inp3
	mov	[state+_data_ptr_sha1+4*PTR_SZ], inp4
	mov	[state+_data_ptr_sha1+5*PTR_SZ], inp5
	mov	[state+_data_ptr_sha1+6*PTR_SZ], inp6
	mov	[state+_data_ptr_sha1+7*PTR_SZ], inp7

	;;;;;;;;;;;;;;;;
	;; Postamble

        ;; Clear stack frame (16*32 bytes)
%ifdef SAFE_DATA
	clear_all_ymms_asm
%assign i 0
%rep 16
        vmovdqa [rsp + i*32], ymm0
%assign i (i+1)
%endrep
%endif

%ifndef LINUX
        vmovdqa  xmm6, [rsp + 32*16 + 0*16]
        vmovdqa  xmm7, [rsp + 32*16 + 1*16]
        vmovdqa  xmm8, [rsp + 32*16 + 2*16]
        vmovdqa  xmm9, [rsp + 32*16 + 3*16]
        vmovdqa  xmm10, [rsp + 32*16 + 4*16]
        vmovdqa  xmm11, [rsp + 32*16 + 5*16]
        vmovdqa  xmm12, [rsp + 32*16 + 6*16]
        vmovdqa  xmm13, [rsp + 32*16 + 7*16]
        vmovdqa  xmm14, [rsp + 32*16 + 8*16]
        vmovdqa  xmm15, [rsp + 32*16 + 9*16]

%ifdef SAFE_DATA
	; xmm0 already 0
%assign i 0
%rep 10
        vmovdqa	[rsp + 32*16 + i*16], xmm0
%assign i (i+1)
%endrep
%endif
%endif
	add 	rsp, FRAMESZ

	ret

; void call_sha1_x8_avx2_from_c(SHA1_ARGS *args, UINT32 size_in_blocks);
MKGLOBAL(call_sha1_x8_avx2_from_c,function,internal)
call_sha1_x8_avx2_from_c:
	FUNC_SAVE
	call sha1_x8_avx2
	FUNC_RESTORE
	ret

mksection stack-noexec
