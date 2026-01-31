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

; This code schedules 1 blocks at a time, with 4 lanes per block
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%include "include/os.inc"
%include "include/clear_regs.inc"

%define	MOVDQ movdqu ;; assume buffers not aligned

%ifndef FUNC
%define FUNC sha512_block_sse
%endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Define Macros

; COPY_XMM_AND_BSWAP xmm, [mem], byte_flip_mask
; Load xmm with mem and byte swap each dword
%macro COPY_XMM_AND_BSWAP 3
	MOVDQ %1, %2
	pshufb %1, %3
%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%define X0 xmm4
%define X1 xmm5
%define X2 xmm6
%define X3 xmm7
%define X4 xmm8
%define X5 xmm9
%define X6 xmm10
%define X7 xmm11

%define XTMP0 xmm0
%define XTMP1 xmm1
%define XTMP2 xmm2
%define XTMP3 xmm3
%define XFER  xmm13

%define BYTE_FLIP_MASK	xmm12

%ifdef LINUX
%define CTX	rsi	; 2nd arg
%define INP	rdi	; 1st arg

%define SRND	rdi	; clobbers INP
%define c	rcx
%define d 	r8
%define e 	rdx
%else
%define CTX	rdx 	; 2nd arg
%define INP	rcx 	; 1st arg

%define SRND	rcx	; clobbers INP
%define c 	rdi
%define d	rsi
%define e 	r8

%endif
%define TBL	rbp
%define a rax
%define b rbx

%define f r9
%define g r10
%define h r11

%define y0 r13
%define y1 r14
%define y2 r15

struc STACK
%ifndef LINUX
_XMM_SAVE:	reso	8
%endif
_XFER:		reso	1
endstruc

; rotate_Xs
; Rotate values of symbols X0...X7
%macro rotate_Xs 0
%xdefine X_ X0
%xdefine X0 X1
%xdefine X1 X2
%xdefine X2 X3
%xdefine X3 X4
%xdefine X4 X5
%xdefine X5 X6
%xdefine X6 X7
%xdefine X7 X_
%endm

; ROTATE_ARGS
; Rotate values of symbols a...h
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

%macro TWO_ROUNDS_AND_SCHED 0

		;; compute s0 four at a time and s1 two at a time
		;; compute W[-16] + W[-7] 4 at a time
		movdqa	XTMP0, X5
	mov	y0, e		; y0 = e
	mov	y1, a		; y1 = a
	ror	y0, (41-18)	; y0 = e >> (41-18)
		palignr	XTMP0, X4, 8	; XTMP0 = W[-7]
	xor	y0, e		; y0 = e ^ (e >> (41-18))
	mov	y2, f		; y2 = f
	ror	y1, (39-34)	; y1 = a >> (39-34)
	xor	y1, a		; y1 = a ^ (a >> (39-34)
		movdqa	XTMP1, X1
	ror	y0, (18-14)	; y0 = (e >> (18-14)) ^ (e >> (41-14))
	xor	y2, g		; y2 = f^g
		paddq	XTMP0, X0	; XTMP0 = W[-7] + W[-16]
	ror	y1, (34-28)	; y1 = (a >> (34-28)) ^ (a >> (39-28))
	xor	y0, e		; y0 = e ^ (e >> (18-14)) ^ (e >> (41-14))
	and	y2, e		; y2 = (f^g)&e
		;; compute s0
		palignr	XTMP1, X0, 8	; XTMP1 = W[-15]
	xor	y1, a		; y1 = a ^ (a >> (34-28)) ^ (a >> (39-28))
	xor	y2, g		; y2 = CH = ((f^g)&e)^g
		movdqa	XTMP2, XTMP1	; XTMP2 = W[-15]
	ror	y0, 14		; y0 = S1 = (e>>14) & (e>>18) ^ (e>>41)
	add	y2, y0		; y2 = S1 + CH
	add	y2, [rsp + _XFER + 0*8] ; y2 = k + w + S1 + CH
	ror	y1, 28		; y1 = S0 = (a>>28) ^ (a>>34) ^ (a>>39)
		movdqa	XTMP3, XTMP1	; XTMP3 = W[-15]
	mov	y0, a		; y0 = a
	add	h, y2		; h = h + S1 + CH + k + w
		psllq	XTMP1, (64-1)
	mov	y2, a		; y2 = a
	or	y0, c		; y0 = a|c
		psrlq	XTMP2, 1
	add	d, h		; d = d + t1
	and	y2, c		; y2 = a&c
		por	XTMP1, XTMP2	; XTMP1 = W[-15] ror 1
	and	y0, b		; y0 = (a|c)&b
	add	h, y1		; h = t1 + S0
		movdqa	XTMP2, XTMP3	; XTMP2 = W[-15]
		psrlq	XTMP2, 8
	or	y0, y2		; y0 = MAJ = (a|c)&b)|(a&c)
	add	h, y0		; h = t1 + S0 + MAJ
		movdqa	X0, XTMP3	; X0 = W[-15]
		psllq	XTMP3, (64-8)

ROTATE_ARGS
		pxor	XTMP1, XTMP3
		psrlq	X0, 7		; X0 = W[-15] >> 7
	mov	y0, e		; y0 = e
	mov	y1, a		; y1 = a
		pxor	XTMP1, XTMP2	; XTMP1 = W[-15] ror 1 ^ W[-15] ror 8
	ror	y0, (41-18)	; y0 = e >> (41-18)
	xor	y0, e		; y0 = e ^ (e >> (41-18))
	mov	y2, f		; y2 = f
		pxor	XTMP1, X0	; XTMP1 = s0
	ror	y1, (39-34)	; y1 = a >> (39-34)
	xor	y1, a		; y1 = a ^ (a >> (39-34)
		;; compute s1
		movdqa	XTMP2, X7	; XTMP2 = W[-2]
	ror	y0, (18-14)	; y0 = (e >> (18-14)) ^ (e >> (41-14))
	xor	y2, g		; y2 = f^g
		paddq	XTMP0, XTMP1	; XTMP0 = W[-16] + W[-7] + s0
	ror	y1, (34-28)	; y1 = (a >> (34-28)) ^ (a >> (39-28))
	xor	y0, e		; y0 = e ^ (e >> (18-14)) ^ (e >> (41-14))
		movdqa	XTMP3, XTMP2	; XTMP3 = W[-2]
		movdqa	X0, XTMP2	; X0 = W[-2]
	and	y2, e		; y2 = (f^g)&e
	ror	y0, 14		; y0 = S1 = (e>>14) & (e>>18) ^ (e>>41)
	xor	y1, a		; y1 = a ^ (a >> (34-28)) ^ (a >> (39-28))
		psllq	XTMP3, (64-19)
	xor	y2, g		; y2 = CH = ((f^g)&e)^g
	add	y2, y0		; y2 = S1 + CH
	add	y2, [rsp + _XFER + 1*8] ; y2 = k + w + S1 + CH
		psrlq	X0, 19
	ror	y1, 28		; y1 = S0 = (a>>28) ^ (a>>34) ^ (a>>39)
	mov	y0, a		; y0 = a
	add	h, y2		; h = h + S1 + CH + k + w
		por	XTMP3, X0	; XTMP3 = W[-2] ror 19
	mov	y2, a		; y2 = a
	or	y0, c		; y0 = a|c
		movdqa	X0, XTMP2	; X0 = W[-2]
		movdqa	XTMP1, XTMP2	; XTMP1 = W[-2]
	add	d, h		; d = d + t1
	and	y2, c		; y2 = a&c
		psllq	X0, (64-61)
	and	y0, b		; y0 = (a|c)&b
	add	h, y1		; h = t1 + S0
		psrlq	XTMP1, 61
	or	y0, y2		; y0 = MAJ = (a|c)&b)|(a&c)
	add	h, y0		; h = t1 + S0 + MAJ
		por	X0, XTMP1	; X0 = W[-2] ror 61
		psrlq	XTMP2, 6	; XTMP2 = W[-2] >> 6
		pxor	XTMP2, XTMP3
		pxor	X0, XTMP2	; X0 = s1
		paddq	X0, XTMP0	; X0 = {W[1], W[0]}

ROTATE_ARGS
rotate_Xs
%endm

;; input is [rsp + _XFER + %1 * 8]
%macro DO_ROUND 1
	mov	y0, e		; y0 = e
	ror	y0, (41-18)	; y0 = e >> (41-18)
	mov	y1, a		; y1 = a
	xor	y0, e		; y0 = e ^ (e >> (41-18))
	ror	y1, (39-34)	; y1 = a >> (39-34)
	mov	y2, f		; y2 = f
	xor	y1, a		; y1 = a ^ (a >> (39-34)
	ror	y0, (18-14)	; y0 = (e >> (18-14)) ^ (e >> (41-14))
	xor	y2, g		; y2 = f^g
	xor	y0, e		; y0 = e ^ (e >> (18-14)) ^ (e >> (25-6))
	ror	y1, (34-28)	; y1 = (a >> (34-28)) ^ (a >> (39-28))
	and	y2, e		; y2 = (f^g)&e
	xor	y1, a		; y1 = a ^ (a >> (34-28)) ^ (a >> (39-28))
	ror	y0, 14		; y0 = S1 = (e>>14) & (e>>18) ^ (e>>41)
	xor	y2, g		; y2 = CH = ((f^g)&e)^g
	add	y2, y0		; y2 = S1 + CH
	ror	y1, 28		; y1 = S0 = (a>>28) ^ (a>>34) ^ (a>>39)
	add	y2, [rsp + _XFER + %1*8] ; y2 = k + w + S1 + CH
	mov	y0, a		; y0 = a
	add	h, y2		; h = h + S1 + CH + k + w
	mov	y2, a		; y2 = a
	or	y0, c		; y0 = a|c
	add	d, h		; d = d + t1
	and	y2, c		; y2 = a&c
	and	y0, b		; y0 = (a|c)&b
	add	h, y1		; h = t1 + S0
	or	y0, y2		; y0 = MAJ = (a|c)&b)|(a&c)
	add	h, y0		; h = t1 + S0 + MAJ
	ROTATE_ARGS
%endm

mksection .rodata
default rel
align 64
K512:
	dq	0x428a2f98d728ae22,0x7137449123ef65cd
	dq	0xb5c0fbcfec4d3b2f,0xe9b5dba58189dbbc
	dq	0x3956c25bf348b538,0x59f111f1b605d019
	dq	0x923f82a4af194f9b,0xab1c5ed5da6d8118
	dq	0xd807aa98a3030242,0x12835b0145706fbe
	dq	0x243185be4ee4b28c,0x550c7dc3d5ffb4e2
	dq	0x72be5d74f27b896f,0x80deb1fe3b1696b1
	dq	0x9bdc06a725c71235,0xc19bf174cf692694
	dq	0xe49b69c19ef14ad2,0xefbe4786384f25e3
	dq	0x0fc19dc68b8cd5b5,0x240ca1cc77ac9c65
	dq	0x2de92c6f592b0275,0x4a7484aa6ea6e483
	dq	0x5cb0a9dcbd41fbd4,0x76f988da831153b5
	dq	0x983e5152ee66dfab,0xa831c66d2db43210
	dq	0xb00327c898fb213f,0xbf597fc7beef0ee4
	dq	0xc6e00bf33da88fc2,0xd5a79147930aa725
	dq	0x06ca6351e003826f,0x142929670a0e6e70
	dq	0x27b70a8546d22ffc,0x2e1b21385c26c926
	dq	0x4d2c6dfc5ac42aed,0x53380d139d95b3df
	dq	0x650a73548baf63de,0x766a0abb3c77b2a8
	dq	0x81c2c92e47edaee6,0x92722c851482353b
	dq	0xa2bfe8a14cf10364,0xa81a664bbc423001
	dq	0xc24b8b70d0f89791,0xc76c51a30654be30
	dq	0xd192e819d6ef5218,0xd69906245565a910
	dq	0xf40e35855771202a,0x106aa07032bbd1b8
	dq	0x19a4c116b8d2d0c8,0x1e376c085141ab53
	dq	0x2748774cdf8eeb99,0x34b0bcb5e19b48a8
	dq	0x391c0cb3c5c95a63,0x4ed8aa4ae3418acb
	dq	0x5b9cca4f7763e373,0x682e6ff3d6b2b8a3
	dq	0x748f82ee5defb2fc,0x78a5636f43172f60
	dq	0x84c87814a1f0ab72,0x8cc702081a6439ec
	dq	0x90befffa23631e28,0xa4506cebde82bde9
	dq	0xbef9a3f7b2c67915,0xc67178f2e372532b
	dq	0xca273eceea26619c,0xd186b8c721c0c207
	dq	0xeada7dd6cde0eb1e,0xf57d4f7fee6ed178
	dq	0x06f067aa72176fba,0x0a637dc5a2c898a6
	dq	0x113f9804bef90dae,0x1b710b35131c471b
	dq	0x28db77f523047d84,0x32caab7b40c72493
	dq	0x3c9ebe0a15c9bebc,0x431d67c49c100d4c
	dq	0x4cc5d4becb3e42b6,0x597f299cfc657e2a
	dq	0x5fcb6fab3ad6faec,0x6c44198c4a475817

align 16
PSHUFFLE_BYTE_FLIP_MASK: ;ddq 0x08090a0b0c0d0e0f0001020304050607
	dq 0x0001020304050607, 0x08090a0b0c0d0e0f

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; void FUNC(void *input_data, UINT64 digest[8])
;; arg 1 : pointer to input data
;; arg 2 : pointer to digest
mksection .text
MKGLOBAL(FUNC,function,internal)
align 32
FUNC:
	push	rbx
%ifndef LINUX
	push	rsi
	push	rdi
%endif
	push	rbp
	push	r13
	push	r14
	push	r15

	sub	rsp,STACK_size
%ifndef LINUX
	movdqa	[rsp + _XMM_SAVE + 0*16],xmm6
	movdqa	[rsp + _XMM_SAVE + 1*16],xmm7
	movdqa	[rsp + _XMM_SAVE + 2*16],xmm8
	movdqa	[rsp + _XMM_SAVE + 3*16],xmm9
	movdqa	[rsp + _XMM_SAVE + 4*16],xmm10
	movdqa	[rsp + _XMM_SAVE + 5*16],xmm11
	movdqa	[rsp + _XMM_SAVE + 6*16],xmm12
	movdqa	[rsp + _XMM_SAVE + 7*16],xmm13
%endif

	;; load initial digest
	mov	a, [8*0 + CTX]
	mov	b, [8*1 + CTX]
	mov	c, [8*2 + CTX]
	mov	d, [8*3 + CTX]
	mov	e, [8*4 + CTX]
	mov	f, [8*5 + CTX]
	mov	g, [8*6 + CTX]
	mov	h, [8*7 + CTX]

	movdqa	BYTE_FLIP_MASK, [rel PSHUFFLE_BYTE_FLIP_MASK]

	lea	TBL,[rel K512]

	;; byte swap first 16 qwords
	COPY_XMM_AND_BSWAP	X0, [INP + 0*16], BYTE_FLIP_MASK
	COPY_XMM_AND_BSWAP	X1, [INP + 1*16], BYTE_FLIP_MASK
	COPY_XMM_AND_BSWAP	X2, [INP + 2*16], BYTE_FLIP_MASK
	COPY_XMM_AND_BSWAP	X3, [INP + 3*16], BYTE_FLIP_MASK
	COPY_XMM_AND_BSWAP	X4, [INP + 4*16], BYTE_FLIP_MASK
	COPY_XMM_AND_BSWAP	X5, [INP + 5*16], BYTE_FLIP_MASK
	COPY_XMM_AND_BSWAP	X6, [INP + 6*16], BYTE_FLIP_MASK
	COPY_XMM_AND_BSWAP	X7, [INP + 7*16], BYTE_FLIP_MASK

	;; schedule 64 input qwords, by doing 4 iterations of 16 rounds
	mov	SRND, 4
align 16
loop1:

%assign i 0
%rep 7
	movdqa	XFER, X0
	paddq	XFER, [TBL + i*16]
	movdqa	[rsp + _XFER], XFER
	TWO_ROUNDS_AND_SCHED
%assign i (i+1)
%endrep

	movdqa	XFER, X0
	paddq	XFER, [TBL + 7*16]
	movdqa	[rsp + _XFER], XFER
	add	TBL, 8*16
	TWO_ROUNDS_AND_SCHED

	sub	SRND, 1
	jne	loop1

	mov	SRND, 2
	jmp loop2a
loop2:
	movdqa	X0, X4
	movdqa	X1, X5
	movdqa	X2, X6
	movdqa	X3, X7

loop2a:
	paddq	X0, [TBL + 0*16]
	movdqa	[rsp + _XFER], X0
	DO_ROUND 0
	DO_ROUND 1

	paddq	X1, [TBL + 1*16]
	movdqa	[rsp + _XFER], X1
	DO_ROUND 0
	DO_ROUND 1

	paddq	X2, [TBL + 2*16]
	movdqa	[rsp + _XFER], X2
	DO_ROUND 0
	DO_ROUND 1

	paddq	X3, [TBL + 3*16]
	movdqa	[rsp + _XFER], X3
	add	TBL, 4*16
	DO_ROUND 0
	DO_ROUND 1

	sub	SRND, 1
	jne	loop2

	add	[8*0 + CTX], a
	add	[8*1 + CTX], b
	add	[8*2 + CTX], c
	add	[8*3 + CTX], d
	add	[8*4 + CTX], e
	add	[8*5 + CTX], f
	add	[8*6 + CTX], g
	add	[8*7 + CTX], h

done_hash:
%ifndef LINUX
	movdqa	xmm6,[rsp + _XMM_SAVE + 0*16]
	movdqa	xmm7,[rsp + _XMM_SAVE + 1*16]
	movdqa	xmm8,[rsp + _XMM_SAVE + 2*16]
	movdqa	xmm9,[rsp + _XMM_SAVE + 3*16]
	movdqa	xmm10,[rsp + _XMM_SAVE + 4*16]
	movdqa	xmm11,[rsp + _XMM_SAVE + 5*16]
	movdqa	xmm12,[rsp + _XMM_SAVE + 6*16]
	movdqa	xmm13,[rsp + _XMM_SAVE + 7*16]

%ifdef SAFE_DATA
        ;; Clear potential sensitive data stored in stack
        clear_xmms_sse xmm0, xmm1, xmm2, xmm3, xmm4, xmm5
        movdqa  [rsp + _XMM_SAVE + 0 * 16], xmm0
        movdqa  [rsp + _XMM_SAVE + 1 * 16], xmm0
        movdqa  [rsp + _XMM_SAVE + 2 * 16], xmm0
        movdqa  [rsp + _XMM_SAVE + 3 * 16], xmm0
        movdqa  [rsp + _XMM_SAVE + 4 * 16], xmm0
        movdqa  [rsp + _XMM_SAVE + 5 * 16], xmm0
        movdqa  [rsp + _XMM_SAVE + 6 * 16], xmm0
        movdqa  [rsp + _XMM_SAVE + 7 * 16], xmm0
%endif
%else ;; LINUX
%ifdef SAFE_DATA
	clear_all_xmms_sse_asm
%endif
%endif ;; LINUX

	add	rsp, STACK_size

	pop	r15
	pop	r14
	pop	r13
	pop	rbp
%ifndef LINUX
	pop	rdi
	pop	rsi
%endif
	pop	rbx

	ret

mksection stack-noexec
