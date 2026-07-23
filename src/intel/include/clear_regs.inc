;;
;; Copyright (c) 2019-2023, Intel Corporation
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

%ifndef _CLEAR_REGS_INC_
%define _CLEAR_REGS_INC_

%include "include/os.inc"

;
; This macro clears any GP registers passed
;
%macro clear_gps 1-16
%define %%NUM_REGS %0
%rep %%NUM_REGS
        xor %1, %1
%rotate 1
%endrep
%endmacro

;
; This macro clears any XMM registers passed on SSE
;
%macro clear_xmms_sse 1-16
%define %%NUM_REGS %0
%rep %%NUM_REGS
        pxor    %1, %1
%rotate 1
%endrep
%endmacro

;
; This macro clears any XMM registers passed on AVX
;
%macro clear_xmms_avx 1-16
%define %%NUM_REGS %0
%rep %%NUM_REGS
        vpxor   %1, %1
%rotate 1
%endrep
%endmacro

;
; This macro clears any ZMM registers passed on
;
%macro clear_zmms_avx512 1-32

%assign num_reg %0
%assign num_xors 3

; clear up to 3 registers with vpxorq
%if num_reg < 3
%assign num_xors num_reg
%endif

; save 1st register for later
%xdefine %%SAVE_XMM XWORD(%1)

%rep num_xors
        vpxorq          XWORD(%1), XWORD(%1), XWORD(%1)
%rotate 1
%endrep

; clear the rest of the registers with move from the 1st register
%assign num_reg (num_reg - num_xors)

%rep num_reg
        vmovdqa64       XWORD(%1), %%SAVE_XMM
%rotate 1
%endrep

%undef %%SAVE_XMM

%endmacro

;
; This macro clears scratch GP registers
; for Windows or Linux
;
%macro clear_scratch_gps_asm 0
        clear_gps rax, rcx, rdx, r8, r9, r10, r11
%ifdef LINUX
        clear_gps rdi, rsi
%endif
%endmacro

;
; This macro clears scratch XMM registers on SSE
;
%macro clear_scratch_xmms_sse_asm 0
%ifdef LINUX
%assign i 0
%rep 16
        pxor    xmm %+ i, xmm %+ i
%assign i (i+1)
%endrep
; On Windows, XMM0-XMM5 registers are scratch registers
%else
%assign i 0
%rep 6
        pxor    xmm %+ i, xmm %+ i
%assign i (i+1)
%endrep
%endif ; LINUX
%endmacro

;
; This macro clears scratch XMM registers on AVX
;
%macro clear_scratch_xmms_avx_asm 0
%ifdef LINUX
%assign i 0
%rep 16
        vpxor   xmm %+ i, xmm %+ i
%assign i (i+1)
%endrep
%else
; On Windows, XMM0-XMM5 registers are scratch registers
%assign i 0
%rep 6
        vpxor   xmm %+ i, xmm %+ i
%assign i (i+1)
%endrep
%endif ; LINUX
%endmacro

;
; This macro clears scratch YMM registers
;
; It should be called before restoring the XMM registers
; for Windows (XMM6-XMM15)
;
%macro clear_scratch_ymms_asm 0
%ifdef LINUX
; On Linux, all YMM registers are scratch registers
        vzeroall
%else
; On Windows, XMM0-XMM5 registers are scratch registers
%assign i 0
%rep 6
        vpxor   xmm %+ i, xmm %+ i
%assign i (i+1)
%endrep
%endif ; LINUX
%endmacro

;
; This macro clears scratch ZMM registers
;
; It should be called before restoring the XMM registers
; for Windows (XMM6-XMM15). YMM registers are used
; on purpose, since XOR'ing YMM registers is faster
; than XOR'ing ZMM registers, and the operation clears
; also the upper 256 bits
;
%macro clear_scratch_zmms_asm 0
%ifdef LINUX
; On Linux, all ZMM registers are scratch registers
        vpxorq  xmm0, xmm0, xmm0
        vpxorq  xmm1, xmm1, xmm1
        vpxorq  xmm2, xmm2, xmm2
%assign i 3
%rep (16 + 13)
%assign j (i % 3)
        vmovdqa64  xmm %+ i, xmm %+ j
%assign i (i+1)
%endrep
%else
; On Windows, XMM0-XMM5 registers are scratch registers
        vpxorq  xmm0, xmm0, xmm0
        vpxorq  xmm1, xmm1, xmm1
        vpxorq  xmm2, xmm2, xmm2
        vmovdqa64  xmm3, xmm0
        vmovdqa64  xmm4, xmm1
        vmovdqa64  xmm5, xmm2
%assign i 16
%rep 16
%assign j (i % 3)
        vmovdqa64  xmm %+ i, xmm %+ j
%assign i (i+1)
%endrep
%endif ; LINUX
       vzeroupper
%endmacro

;
; This macro clears all XMM registers on SSE
;
%macro clear_all_xmms_sse_asm 0
%assign i 0
%rep 16
        pxor    xmm %+ i, xmm %+ i
%assign i (i+1)
%endrep
%endmacro

;
; This macro clears all XMM registers on AVX
;
%macro clear_all_xmms_avx_asm 0
%assign i 0
%rep 16
        vpxor   xmm %+ i, xmm %+ i
%assign i (i+1)
%endrep
%endmacro

;
; This macro clears all YMM registers
;
%macro clear_all_ymms_asm 0
       vzeroall
%endmacro

;
; This macro clears all ZMM registers
;
; YMM registers are used on purpose, since XOR'ing YMM registers
; is faster than XOR'ing ZMM registers, and the operation clears
; also the upper 256 bits
;
%macro clear_all_zmms_asm 0
        vpxorq  xmm0, xmm0, xmm0
        vpxorq  xmm1, xmm1, xmm1
        vpxorq  xmm2, xmm2, xmm2
%assign i 3
%rep (16 + 13)
%assign j (i % 3)
        vmovdqa64  xmm %+ i, xmm %+ j
%assign i (i+1)
%endrep
        vzeroupper
%endmacro

%endif ;; _CLEAR_REGS_INC_
