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

; define d and w variants for registers

%ifndef _REG_SIZES_INC_
%define _REG_SIZES_INC_

%define	raxq	rax
%define eaxq    rax
%define	raxd	eax
%define raxw	ax
%define raxb	al

%define	rbxq	rbx
%define ebxq    rbx
%define	rbxd	ebx
%define rbxw	bx
%define rbxb	bl

%define	rcxq	rcx
%define ecxq    rcx
%define	rcxd	ecx
%define rcxw	cx
%define rcxb	cl

%define	rdxq	rdx
%define edxq    rdx
%define	rdxd	edx
%define rdxw	dx
%define rdxb	dl

%define	rsiq	rsi
%define esiq    rsi
%define	rsid	esi
%define rsiw	si
%define rsib	sil

%define	rdiq	rdi
%define ediq    rdi
%define	rdid	edi
%define rdiw	di
%define rdib	dil

%define	rbpq	rbp
%define ebpq    rbp
%define	rbpd	ebp
%define rbpw	bp
%define rbpb	bpl

%define r8dq    r8
%define r9dq    r9
%define r10dq   r10
%define r11dq   r11
%define r12dq   r12
%define r13dq   r13
%define r14dq   r14
%define r15dq   r15

%define r8q     r8
%define r9q     r9
%define r10q    r10
%define r11q    r11
%define r12q    r12
%define r13q    r13
%define r14q    r14
%define r15q    r15

%xdefine zmm0x xmm0
%xdefine zmm1x xmm1
%xdefine zmm2x xmm2
%xdefine zmm3x xmm3
%xdefine zmm4x xmm4
%xdefine zmm5x xmm5
%xdefine zmm6x xmm6
%xdefine zmm7x xmm7
%xdefine zmm8x xmm8
%xdefine zmm9x xmm9
%xdefine zmm10x xmm10
%xdefine zmm11x xmm11
%xdefine zmm12x xmm12
%xdefine zmm13x xmm13
%xdefine zmm14x xmm14
%xdefine zmm15x xmm15
%xdefine zmm16x xmm16
%xdefine zmm17x xmm17
%xdefine zmm18x xmm18
%xdefine zmm19x xmm19
%xdefine zmm20x xmm20
%xdefine zmm21x xmm21
%xdefine zmm22x xmm22
%xdefine zmm23x xmm23
%xdefine zmm24x xmm24
%xdefine zmm25x xmm25
%xdefine zmm26x xmm26
%xdefine zmm27x xmm27
%xdefine zmm28x xmm28
%xdefine zmm29x xmm29
%xdefine zmm30x xmm30
%xdefine zmm31x xmm31

%xdefine ymm0x xmm0
%xdefine ymm1x xmm1
%xdefine ymm2x xmm2
%xdefine ymm3x xmm3
%xdefine ymm4x xmm4
%xdefine ymm5x xmm5
%xdefine ymm6x xmm6
%xdefine ymm7x xmm7
%xdefine ymm8x xmm8
%xdefine ymm9x xmm9
%xdefine ymm10x xmm10
%xdefine ymm11x xmm11
%xdefine ymm12x xmm12
%xdefine ymm13x xmm13
%xdefine ymm14x xmm14
%xdefine ymm15x xmm15
%xdefine ymm16x xmm16
%xdefine ymm17x xmm17
%xdefine ymm18x xmm18
%xdefine ymm19x xmm19
%xdefine ymm20x xmm20
%xdefine ymm21x xmm21
%xdefine ymm22x xmm22
%xdefine ymm23x xmm23
%xdefine ymm24x xmm24
%xdefine ymm25x xmm25
%xdefine ymm26x xmm26
%xdefine ymm27x xmm27
%xdefine ymm28x xmm28
%xdefine ymm29x xmm29
%xdefine ymm30x xmm30
%xdefine ymm31x xmm31

%xdefine xmm0x xmm0
%xdefine xmm1x xmm1
%xdefine xmm2x xmm2
%xdefine xmm3x xmm3
%xdefine xmm4x xmm4
%xdefine xmm5x xmm5
%xdefine xmm6x xmm6
%xdefine xmm7x xmm7
%xdefine xmm8x xmm8
%xdefine xmm9x xmm9
%xdefine xmm10x xmm10
%xdefine xmm11x xmm11
%xdefine xmm12x xmm12
%xdefine xmm13x xmm13
%xdefine xmm14x xmm14
%xdefine xmm15x xmm15
%xdefine xmm16x xmm16
%xdefine xmm17x xmm17
%xdefine xmm18x xmm18
%xdefine xmm19x xmm19
%xdefine xmm20x xmm20
%xdefine xmm21x xmm21
%xdefine xmm22x xmm22
%xdefine xmm23x xmm23
%xdefine xmm24x xmm24
%xdefine xmm25x xmm25
%xdefine xmm26x xmm26
%xdefine xmm27x xmm27
%xdefine xmm28x xmm28
%xdefine xmm29x xmm29
%xdefine xmm30x xmm30
%xdefine xmm31x xmm31

%xdefine zmm0y ymm0
%xdefine zmm1y ymm1
%xdefine zmm2y ymm2
%xdefine zmm3y ymm3
%xdefine zmm4y ymm4
%xdefine zmm5y ymm5
%xdefine zmm6y ymm6
%xdefine zmm7y ymm7
%xdefine zmm8y ymm8
%xdefine zmm9y ymm9
%xdefine zmm10y ymm10
%xdefine zmm11y ymm11
%xdefine zmm12y ymm12
%xdefine zmm13y ymm13
%xdefine zmm14y ymm14
%xdefine zmm15y ymm15
%xdefine zmm16y ymm16
%xdefine zmm17y ymm17
%xdefine zmm18y ymm18
%xdefine zmm19y ymm19
%xdefine zmm20y ymm20
%xdefine zmm21y ymm21
%xdefine zmm22y ymm22
%xdefine zmm23y ymm23
%xdefine zmm24y ymm24
%xdefine zmm25y ymm25
%xdefine zmm26y ymm26
%xdefine zmm27y ymm27
%xdefine zmm28y ymm28
%xdefine zmm29y ymm29
%xdefine zmm30y ymm30
%xdefine zmm31y ymm31

%xdefine ymm0y ymm0
%xdefine ymm1y ymm1
%xdefine ymm2y ymm2
%xdefine ymm3y ymm3
%xdefine ymm4y ymm4
%xdefine ymm5y ymm5
%xdefine ymm6y ymm6
%xdefine ymm7y ymm7
%xdefine ymm8y ymm8
%xdefine ymm9y ymm9
%xdefine ymm10y ymm10
%xdefine ymm11y ymm11
%xdefine ymm12y ymm12
%xdefine ymm13y ymm13
%xdefine ymm14y ymm14
%xdefine ymm15y ymm15
%xdefine ymm16y ymm16
%xdefine ymm17y ymm17
%xdefine ymm18y ymm18
%xdefine ymm19y ymm19
%xdefine ymm20y ymm20
%xdefine ymm21y ymm21
%xdefine ymm22y ymm22
%xdefine ymm23y ymm23
%xdefine ymm24y ymm24
%xdefine ymm25y ymm25
%xdefine ymm26y ymm26
%xdefine ymm27y ymm27
%xdefine ymm28y ymm28
%xdefine ymm29y ymm29
%xdefine ymm30y ymm30
%xdefine ymm31y ymm31

%xdefine xmm0y ymm0
%xdefine xmm1y ymm1
%xdefine xmm2y ymm2
%xdefine xmm3y ymm3
%xdefine xmm4y ymm4
%xdefine xmm5y ymm5
%xdefine xmm6y ymm6
%xdefine xmm7y ymm7
%xdefine xmm8y ymm8
%xdefine xmm9y ymm9
%xdefine xmm10y ymm10
%xdefine xmm11y ymm11
%xdefine xmm12y ymm12
%xdefine xmm13y ymm13
%xdefine xmm14y ymm14
%xdefine xmm15y ymm15
%xdefine xmm16y ymm16
%xdefine xmm17y ymm17
%xdefine xmm18y ymm18
%xdefine xmm19y ymm19
%xdefine xmm20y ymm20
%xdefine xmm21y ymm21
%xdefine xmm22y ymm22
%xdefine xmm23y ymm23
%xdefine xmm24y ymm24
%xdefine xmm25y ymm25
%xdefine xmm26y ymm26
%xdefine xmm27y ymm27
%xdefine xmm28y ymm28
%xdefine xmm29y ymm29
%xdefine xmm30y ymm30
%xdefine xmm31y ymm31

%xdefine xmm0z zmm0
%xdefine xmm1z zmm1
%xdefine xmm2z zmm2
%xdefine xmm3z zmm3
%xdefine xmm4z zmm4
%xdefine xmm5z zmm5
%xdefine xmm6z zmm6
%xdefine xmm7z zmm7
%xdefine xmm8z zmm8
%xdefine xmm9z zmm9
%xdefine xmm10z zmm10
%xdefine xmm11z zmm11
%xdefine xmm12z zmm12
%xdefine xmm13z zmm13
%xdefine xmm14z zmm14
%xdefine xmm15z zmm15
%xdefine xmm16z zmm16
%xdefine xmm17z zmm17
%xdefine xmm18z zmm18
%xdefine xmm19z zmm19
%xdefine xmm20z zmm20
%xdefine xmm21z zmm21
%xdefine xmm22z zmm22
%xdefine xmm23z zmm23
%xdefine xmm24z zmm24
%xdefine xmm25z zmm25
%xdefine xmm26z zmm26
%xdefine xmm27z zmm27
%xdefine xmm28z zmm28
%xdefine xmm29z zmm29
%xdefine xmm30z zmm30
%xdefine xmm31z zmm31

%xdefine ymm0z zmm0
%xdefine ymm1z zmm1
%xdefine ymm2z zmm2
%xdefine ymm3z zmm3
%xdefine ymm4z zmm4
%xdefine ymm5z zmm5
%xdefine ymm6z zmm6
%xdefine ymm7z zmm7
%xdefine ymm8z zmm8
%xdefine ymm9z zmm9
%xdefine ymm10z zmm10
%xdefine ymm11z zmm11
%xdefine ymm12z zmm12
%xdefine ymm13z zmm13
%xdefine ymm14z zmm14
%xdefine ymm15z zmm15
%xdefine ymm16z zmm16
%xdefine ymm17z zmm17
%xdefine ymm18z zmm18
%xdefine ymm19z zmm19
%xdefine ymm20z zmm20
%xdefine ymm21z zmm21
%xdefine ymm22z zmm22
%xdefine ymm23z zmm23
%xdefine ymm24z zmm24
%xdefine ymm25z zmm25
%xdefine ymm26z zmm26
%xdefine ymm27z zmm27
%xdefine ymm28z zmm28
%xdefine ymm29z zmm29
%xdefine ymm30z zmm30
%xdefine ymm31z zmm31

%xdefine zmm0z zmm0
%xdefine zmm1z zmm1
%xdefine zmm2z zmm2
%xdefine zmm3z zmm3
%xdefine zmm4z zmm4
%xdefine zmm5z zmm5
%xdefine zmm6z zmm6
%xdefine zmm7z zmm7
%xdefine zmm8z zmm8
%xdefine zmm9z zmm9
%xdefine zmm10z zmm10
%xdefine zmm11z zmm11
%xdefine zmm12z zmm12
%xdefine zmm13z zmm13
%xdefine zmm14z zmm14
%xdefine zmm15z zmm15
%xdefine zmm16z zmm16
%xdefine zmm17z zmm17
%xdefine zmm18z zmm18
%xdefine zmm19z zmm19
%xdefine zmm20z zmm20
%xdefine zmm21z zmm21
%xdefine zmm22z zmm22
%xdefine zmm23z zmm23
%xdefine zmm24z zmm24
%xdefine zmm25z zmm25
%xdefine zmm26z zmm26
%xdefine zmm27z zmm27
%xdefine zmm28z zmm28
%xdefine zmm29z zmm29
%xdefine zmm30z zmm30
%xdefine zmm31z zmm31

%define QWORD(reg) reg %+ q
%define DWORD(reg) reg %+ d
%define WORD(reg)  reg %+ w
%define BYTE(reg)  reg %+ b

%define XWORD(reg) reg %+ x
%define YWORD(reg) reg %+ y
%define ZWORD(reg) reg %+ z

%endif ;; _REG_SIZES_INC_
