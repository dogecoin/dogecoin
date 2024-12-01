;;
;; Copyright (c) 2012-2023, Intel Corporation
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

; SHA1 code, hybrid, rolled, interleaved
; Uses AVX instructions
%include "include/os.inc"
%include "include/clear_regs.inc"

mksection .rodata
default rel
align 16
PSHUFFLE_BYTE_FLIP_MASK: ;ddq 0x0c0d0e0f08090a0b0405060700010203
	dq 0x0405060700010203, 0x0c0d0e0f08090a0b
K00_19:                  ;ddq 0x5A8279995A8279995A8279995A827999
	dq 0x5A8279995A827999, 0x5A8279995A827999
K20_39:                  ;ddq 0x6ED9EBA16ED9EBA16ED9EBA16ED9EBA1
	dq 0x6ED9EBA16ED9EBA1, 0x6ED9EBA16ED9EBA1
K40_59:                  ;ddq 0x8F1BBCDC8F1BBCDC8F1BBCDC8F1BBCDC
	dq 0x8F1BBCDC8F1BBCDC, 0x8F1BBCDC8F1BBCDC
K60_79:                  ;ddq 0xCA62C1D6CA62C1D6CA62C1D6CA62C1D6
	dq 0xCA62C1D6CA62C1D6, 0xCA62C1D6CA62C1D6

mksection .text

%define	VMOVDQ vmovdqu ;; assume buffers not aligned

%ifdef LINUX
%define INP	rdi ; 1st arg
%define CTX     rsi ; 2nd arg
%define REG3	edx
%define REG4	ecx
%else
%define INP	rcx ; 1st arg
%define CTX     rdx ; 2nd arg
%define REG3	edi
%define REG4	esi
%endif

%define FRAMESZ 3*16 + 1*8
%define _RSP	FRAMESZ-1*8 + rsp

%define a eax
%define b ebx
%define c REG3
%define d REG4
%define e r8d
%define T1 r9d
%define f  r10d
%define RND r11d
%define g r12d
%define h r13d

%define XTMP0 xmm0
%define XTMP1 xmm1
%define XK    xmm2

%xdefine X0 xmm3
%xdefine X1 xmm4
%xdefine X2 xmm5
%xdefine X3 xmm6
%xdefine X4 xmm7

%define XFER  xmm8

%define SZ 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Define Macros

%macro rotate_Xs 0
%xdefine X_ X0
%xdefine X0 X1
%xdefine X1 X2
%xdefine X2 X3
%xdefine X3 X4
%xdefine X4 X_
%endmacro

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

;; Magic functions defined in FIPS 180-1
;;
; macro MAGIC_F0 F,B,C,D,T   ;; F = (D ^ (B & (C ^ D)))
%macro MAGIC_F0 5
%define %%regF %1
%define %%regB %2
%define %%regC %3
%define %%regD %4
%define %%regT %5
	mov  %%regF,%%regC
	xor  %%regF,%%regD
	and  %%regF,%%regB
	xor  %%regF,%%regD
%endmacro

; macro MAGIC_F1 F,B,C,D,T   ;; F = (B ^ C ^ D)
%macro MAGIC_F1 5
%define %%regF %1
%define %%regB %2
%define %%regC %3
%define %%regD %4
%define %%regT %5
	mov  %%regF,%%regD
	xor  %%regF,%%regC
	xor  %%regF,%%regB
%endmacro

; macro MAGIC_F2 F,B,C,D,T   ;; F = ((B & C) | (B & D) | (C & D))
%macro MAGIC_F2 5
%define %%regF %1
%define %%regB %2
%define %%regC %3
%define %%regD %4
%define %%regT %5
	mov  %%regF,%%regB
	mov  %%regT,%%regB
	or   %%regF,%%regC
	and  %%regT,%%regC
	and  %%regF,%%regD
	or   %%regF,%%regT
%endmacro

; macro MAGIC_F3 F,B,C,D,T   ;; F = (B ^ C ^ D)
%macro MAGIC_F3 5
%define %%regF %1
%define %%regB %2
%define %%regC %3
%define %%regD %4
%define %%regT %5
	MAGIC_F1 %%regF,%%regB,%%regC,%%regD,%%regT
%endmacro

;; input is T1
%macro ROUND 1
%define %%MAGIC	%1
	add   e,T1
	mov   T1,a
	rol   T1,5
	add   e,T1
	%%MAGIC     h,b,c,d,T1      ;; FUN  = MAGIC_Fi(B,C,D)
	rol   b,30
	add   h,e
ROTATE_ARGS
%endmacro

%macro do_4i 1
		vpaddd	XFER, XK, X0
		vpextrd	T1, XFER, 0
	;ROUND %1
	add   e,T1
		;SCHEDULE_4
		vpalignr XTMP0, X1, X0, 8		; XTMP0 = W[-14]
	mov   T1,a
	rol   T1,5
		vpxor	XTMP1, X2, X0			; XTMP1 = W[-8] ^ W[-16]
	add   e,T1
		vpxor	XTMP0, XTMP0, XTMP1		; XTMP0 = W[-8] ^ W[-14] ^ W[-16]
	%1     h,b,c,d,T1      ;; FUN  = MAGIC_Fi(B,C,D)

		;; Finish low half
	rol   b,30
		vpsrldq	X4, X3, 4			; X4 = W[-3] {xxBA}
	add   h,e
ROTATE_ARGS
		vpextrd	T1, XFER, 1
	;ROUND %1
	add   e,T1
		vpxor	X4, X4, XTMP0
	mov   T1,a
	rol   T1,5
		;; rotate X4 left 1
		vpsrld	XTMP1, X4, (32-1)
	add   e,T1
		vpslld	X4, X4, 1
	%1     h,b,c,d,T1      ;; FUN  = MAGIC_Fi(B,C,D)
		vpxor	X4, X4, XTMP1			; X4 = W[0] {xxBA}
	rol   b,30
	add   h,e
ROTATE_ARGS
		vpextrd	T1, XFER, 2
	;ROUND %1
	add   e,T1
	mov   T1,a

		;; Finish high half
		vpalignr XTMP1, X4, X3, 4		; XTMP1 = w[-3] {DCxx}
	rol   T1,5
	add   e,T1
		vpxor	XTMP0, XTMP0, XTMP1
	%1     h,b,c,d,T1      ;; FUN  = MAGIC_Fi(B,C,D)
		;; rotate XTMP0 left 1
		vpsrld	XTMP1, XTMP0, (32-1)
	rol   b,30
	add   h,e
ROTATE_ARGS
		vpextrd	T1, XFER, 3
	;ROUND %1
	add   e,T1
	mov   T1,a
		vpslld	XTMP0, XTMP0, 1
	rol   T1,5
	add   e,T1
		vpxor	XTMP0, XTMP0, XTMP1		; XTMP0 = W[0] {DCxx}
	%1     h,b,c,d,T1      ;; FUN  = MAGIC_Fi(B,C,D)
		;; COMBINE HALVES
		vshufps	X4, X4, XTMP0, 11100100b	; X4 = X[0] {DCBA}
	rol   b,30
	add   h,e

		rotate_Xs
ROTATE_ARGS
%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; void sha1_block_avx(void *input_data, UINT32 digest[5])
;; arg 1 : (in) pointer to input data
;; arg 2 : (in/out) pointer to read/write digest
MKGLOBAL(sha1_block_avx,function,internal)
align 32
sha1_block_avx:
	push	rbx
	push	rsi
	push	rdi
	push	r12
	push	r13

	vmovdqa	XTMP0, [rel PSHUFFLE_BYTE_FLIP_MASK]

%ifndef LINUX
	mov	rax,rsp			; copy rsp
	sub	rsp,FRAMESZ
	and	rsp,-16			; align stack frame
	mov	[_RSP],rax		; save copy of rsp
	vmovdqa	[rsp + 0 * 16], xmm6
	vmovdqa	[rsp + 1 * 16], xmm7
	vmovdqa	[rsp + 2 * 16], xmm8
%endif

	VMOVDQ	X0, [INP + 0*16]
	VMOVDQ	X1, [INP + 1*16]

	;; load next message block
	VMOVDQ	X2, [INP + 2*16]
	VMOVDQ	X3, [INP + 3*16]

        ;; set up a-f based on h0-h4
	;; byte swap first 16 dwords
	mov	a, [SZ*0 + CTX]
	vpshufb	X0, XTMP0
	mov	b, [SZ*1 + CTX]
	vpshufb	X1, XTMP0
	mov	c, [SZ*2 + CTX]
	vpshufb	X2, XTMP0
	mov	d, [SZ*3 + CTX]
	vpshufb	X3, XTMP0
	mov	e, [SZ*4 + CTX]

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; do rounds 00-19
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	vmovdqa	XK, [rel K00_19]
	mov	RND, 3
	ROTATE_ARGS
	ROTATE_ARGS
	ROTATE_ARGS
	ROTATE_ARGS
	rotate_Xs
	rotate_Xs
	rotate_Xs
	rotate_Xs
	jmp	loop1_5
align 16
loop1:

	do_4i	MAGIC_F0

loop1_5:
	do_4i	MAGIC_F0

	rotate_Xs
	rotate_Xs
	rotate_Xs
	rotate_Xs
	vmovdqa	X0, X2
	vmovdqa	X2, X4
	vmovdqa	X4, X1
	vmovdqa	X1, X3

	sub	RND, 1
	jne	loop1

	rotate_Xs
	rotate_Xs
	rotate_Xs
	rotate_Xs

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; end rounds 00-19
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; do rounds 20-39
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	vmovdqa	XK, [rel K20_39]
	mov	RND, 3
	ROTATE_ARGS
	ROTATE_ARGS
	ROTATE_ARGS
	ROTATE_ARGS
	rotate_Xs
	rotate_Xs
	rotate_Xs
	rotate_Xs
	jmp	loop2_5
align 16
loop2:

	do_4i	MAGIC_F1

loop2_5:
	do_4i	MAGIC_F1

	rotate_Xs
	rotate_Xs
	rotate_Xs
	rotate_Xs
	vmovdqa	X0, X2
	vmovdqa	X2, X4
	vmovdqa	X4, X1
	vmovdqa	X1, X3

	sub	RND, 1
	jne	loop2

	rotate_Xs
	rotate_Xs
	rotate_Xs
	rotate_Xs

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; end rounds 20-39
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; do rounds 40-59
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	vmovdqa	XK, [rel K40_59]
	mov	RND, 3
	ROTATE_ARGS
	ROTATE_ARGS
	ROTATE_ARGS
	ROTATE_ARGS
	rotate_Xs
	rotate_Xs
	rotate_Xs
	rotate_Xs
	jmp	loop3_5
align 16
loop3:

	do_4i	MAGIC_F2

loop3_5:
	do_4i	MAGIC_F2

	rotate_Xs
	rotate_Xs
	rotate_Xs
	rotate_Xs
	vmovdqa	X0, X2
	vmovdqa	X2, X4
	vmovdqa	X4, X1
	vmovdqa	X1, X3

	sub	RND, 1
	jne	loop3

	rotate_Xs
	rotate_Xs
	rotate_Xs
	rotate_Xs

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; end rounds 40-59
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; do rounds 60-79
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	vmovdqa	XK, [rel K60_79]

	do_4i	MAGIC_F3

	vpaddd	XFER, XK, X0
	vpextrd	T1, XFER, 0
	ROUND MAGIC_F3
	vpextrd	T1, XFER, 1
	ROUND MAGIC_F3
	vpextrd	T1, XFER, 2
	ROUND MAGIC_F3
	vpextrd	T1, XFER, 3
	ROUND MAGIC_F3

	vpaddd	XFER, XK, X1
	vpextrd	T1, XFER, 0
	ROUND MAGIC_F3
	vpextrd	T1, XFER, 1
	ROUND MAGIC_F3
	vpextrd	T1, XFER, 2
	ROUND MAGIC_F3
	vpextrd	T1, XFER, 3
	ROUND MAGIC_F3

	vpaddd	XFER, XK, X2
	vpextrd	T1, XFER, 0
	ROUND MAGIC_F3
	vpextrd	T1, XFER, 1
	ROUND MAGIC_F3
	vpextrd	T1, XFER, 2
	ROUND MAGIC_F3
	vpextrd	T1, XFER, 3
	ROUND MAGIC_F3

	vpaddd	XFER, XK, X3
	vpextrd	T1, XFER, 0
	ROUND MAGIC_F3
	vpextrd	T1, XFER, 1
	ROUND MAGIC_F3
	vpextrd	T1, XFER, 2
	ROUND MAGIC_F3
	vpextrd	T1, XFER, 3
	ROUND MAGIC_F3

        ;; update result digest h0-h4
	add	[SZ*0 + CTX], a
	add	[SZ*1 + CTX], b
	add	[SZ*2 + CTX], c
	add	[SZ*3 + CTX], d
	add	[SZ*4 + CTX], e

%ifndef LINUX
	vmovdqa	xmm8, [rsp + 2 * 16]
	vmovdqa	xmm7, [rsp + 1 * 16]
	vmovdqa	xmm6, [rsp + 0 * 16]

%ifdef SAFE_DATA
        ;; Clear potential sensitive data stored in stack
        clear_xmms_avx xmm0, xmm1, xmm2, xmm3, xmm4, xmm5
        vmovdqa [rsp + 0 * 16], xmm0
        vmovdqa [rsp + 1 * 16], xmm0
        vmovdqa [rsp + 2 * 16], xmm0
%endif

	mov	rsp,[_RSP]
%else ;; LINUX
%ifdef SAFE_DATA
	clear_all_xmms_avx_asm
%endif
%endif ;; LINUX

	pop	r13
	pop	r12
	pop	rdi
	pop	rsi
	pop	rbx

	ret

mksection stack-noexec
