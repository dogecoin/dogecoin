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

; Macros for "printing" for debug purposes from within asm code
;
; The basic macros are:
;   DBGPRINT16, DBGPRINT32, DBGPRINT64, DBGPRINT_XMM, DBGPRINT_YMM, DBGPRINT_ZMM
; These are called with 1 or more arguments, all of which are of the
; size/type as specified in the name. E.g.
;   DBGPRINT64 reg1, reg2, reg3, ...
;
; There is also a macro DEBUGPRINTL that takes one argument, a string. E.g.
;   DBGPRINTL "hit this point in the code"
;
; There are also variations on these with the "DBGPRINT" suffixed with "L", e.g.
; DBGPRINTL64. These take two or more arguments, where the first is a string,
; and the rest are of the specified type, e.g.
;   DBGPRINTL64 "Rindex", Rindex
; Essentially, this is the same as a DBGPRINTL followed by DBGPRINT64.
;
; If DO_DBGPRINT is defined, then the macros write the debug information into
; a buffer. If DO_DBGPRINT is *not* defined, then the macros expand to nothing.
;
; CAVEAT: The macros need a GPR. Currently, it uses R15. If the first register
; argument is R15, then it will use R14. This means that if you try
;   DBGPRINTL64 "text", rax, r15
; you will not get the proper value of r15.
; One way to avoid this issue is to not use multiple registers on the same line
; if the register types are GPR (i.e. this is not an issue for printing XMM
; registers). E.g the above could be done with:
;   DBGPRINTL64 "test", rax
;   DBGPRINT64 r15
;
; Note also that the macros only check for r15. Thus is you tried something
; like (after token expansion):
;   DBGPRINT32 r15d
; you won't get the right results. If you want to display r15d, you should
; print it as the 64-bit r15.
;
; To actually print the data, from your C code include the file
; "dbgprint.h". The default buffer size is 16kB. If you want to change
; that, #define DBG_BUFFER_SIZE before including "dbgprint.h".
;
; Then, (after your asm routine(s) have returned, call
;   print_debug()    or    print_debug(file pointer)
; If you do not specify a file pointer, it defaults to stdout.
;
; Printing the debug data also resets the write pointer to the beginning,
; effectively "deleting" the previous messages.
;
%ifndef DBGPRINT_INC_INCLUDED
%define DBGPRINT_INC_INCLUDED

;%define DO_DBGPRINT
%ifdef DO_DBGPRINT
extern pDebugBuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DBGPRINT_INT size, param, ...
%macro DBGPRINT_INT 2-*
%ifidni %2,r15
%xdefine %%reg r14
%else
%xdefine %%reg r15
%endif
%xdefine %%size %1
%rotate 1
	push	%%reg
	mov	%%reg, [pDebugBuffer]
%rep %0 - 1
	mov	byte [%%reg], %%size
	%if (%%size == 2)
	mov	word [%%reg+1], %1
	%elif (%%size == 4)
	mov	dword [%%reg+1], %1
	%elif (%%size == 8)
	mov	qword [%%reg+1], %1
	%elif (%%size == 16)
	movdqu	oword [%%reg+1], %1
	%elif (%%size == 32)
	vmovdqu	[%%reg+1], %1
	%elif (%%size == 64)
	vmovdqu32 [%%reg+1], %1
	%else
	%error invalid size %%size
	%endif
	add	%%reg, %%size+1
%rotate 1
%endrep
	mov	[pDebugBuffer], %%reg
	pop	%%reg
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DBGPRINTL_INT size, label, param, ...
%macro DBGPRINTL_INT 3-*
%ifidni %3,r15
%xdefine %%reg r14
%else
%xdefine %%reg r15
%endif
%xdefine %%size %1
%rotate 1
	push	%%reg
	mov	%%reg, [pDebugBuffer]

	mov	byte [%%reg], 0x57
mksection .rodata
%%lab: db %1, 0
mksection .text
	mov	qword [%%reg+1], %%lab
	add	%%reg, 8+1
%rotate 1

%rep %0 - 2
	mov	byte [%%reg], %%size
%if (%%size == 2)
	mov	word [%%reg+1], %1
%elif (%%size == 4)
	mov	dword [%%reg+1], %1
%elif (%%size == 8)
	mov	qword [%%reg+1], %1
%elif (%%size == 16)
	movdqu	oword [%%reg+1], %1
%elif (%%size == 32)
	vmovdqu	[%%reg+1], %1
%elif (%%size == 64)
	vmovdqu32 [%%reg+1], %1
%else
%error invalid size %%size
%endif
	add	%%reg, %%size+1
%rotate 1
%endrep
	mov	[pDebugBuffer], %%reg
	pop	%%reg
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DBGPRINTL* data, ...
%macro DBGPRINT16 1+
	DBGPRINT_INT 2, %1
%endmacro
%macro DBGPRINT32 1+
	DBGPRINT_INT 4, %1
%endmacro
%macro DBGPRINT64 1+
	DBGPRINT_INT 8, %1
%endmacro
%macro DBGPRINT_XMM 1+
	DBGPRINT_INT 16, %1
%endmacro
%macro DBGPRINT_YMM 1+
	DBGPRINT_INT 32, %1
%endmacro
%macro DBGPRINT_ZMM 1+
	DBGPRINT_INT 64, %1
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DBGPRINTL* label, data, ...
%macro DBGPRINTL16 2+
	DBGPRINTL_INT 2, %1, %2
%endmacro
%macro DBGPRINTL32 2+
	DBGPRINTL_INT 4, %1, %2
%endmacro
%macro DBGPRINTL64 2+
	DBGPRINTL_INT 8, %1, %2
%endmacro
%macro DBGPRINTL_XMM 2+
	DBGPRINTL_INT 16, %1, %2
%endmacro
%macro DBGPRINTL_YMM 2+
	DBGPRINTL_INT 32, %1, %2
%endmacro
%macro DBGPRINTL_ZMM 2+
	DBGPRINTL_INT 64, %1, %2
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro DBGPRINTL 1
	push	r15
	mov	r15, [pDebugBuffer]

	mov	byte [r15], 0x57
mksection .rodata
%%lab: db %1, 0
mksection .text
	mov	qword [r15+1], %%lab
	add	r15, 8+1

	mov	[pDebugBuffer], r15
	pop	r15
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%else
%macro DBGPRINT16 1+
%endmacro
%macro DBGPRINT32 1+
%endmacro
%macro DBGPRINT64 1+
%endmacro
%macro DBGPRINT_XMM 1+
%endmacro
%macro DBGPRINT_YMM 1+
%endmacro
%macro DBGPRINT_ZMM 1+
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro DBGPRINTL16 2+
%endmacro
%macro DBGPRINTL32 2+
%endmacro
%macro DBGPRINTL64 2+
%endmacro
%macro DBGPRINTL_XMM 2+
%endmacro
%macro DBGPRINTL_YMM 2+
%endmacro
%macro DBGPRINTL_ZMM 2+
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro DBGPRINTL 1
%endmacro
%endif

%if 0 ; OLD
%macro DBGPRINTL_ZMM 2-*
	push	rax
	mov	rax, [pDebugBuffer]

	mov	byte [rax], 0x57
mksection .rodata
%%lab: db %1, 0
mksection .text
	mov	qword [rax+1], %%lab
	add	rax, 8+1
%rotate 1

%rep %0 - 1
	mov	byte [rax], 64
	vmovdqu32 [rax+1], %1
%rotate 1
	add	rax, 64+1
%endrep
	mov	[pDebugBuffer], rax
	pop	rax
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro DBGPRINT_ZMM 1-*
	push	rax
	mov	rax, [pDebugBuffer]
%rep %0
	mov	byte [rax], 64
	vmovdqu32 [rax+1], %1
%rotate 1
	add	rax, 64+1
%endrep
	mov	[pDebugBuffer], rax
	pop	rax
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro DBGPRINT_YMM 1-*
	push	rax
	mov	rax, [pDebugBuffer]
%rep %0
	mov	byte [rax], 32
	vmovdqu	[rax+1], %1
%rotate 1
	add	rax, 32+1
%endrep
	mov	[pDebugBuffer], rax
	pop	rax
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro DBGPRINT_XMM 1-*
	push	rax
	mov	rax, [pDebugBuffer]
%rep %0
	mov	byte [rax], 16
	vmovdqu	oword [rax+1], %1
%rotate 1
	add	rax, 16+1
%endrep
	mov	[pDebugBuffer], rax
	pop	rax
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro DBGPRINTL64 2-*
	push	rax
	mov	rax, [pDebugBuffer]

	mov	byte [rax], 0x57
mksection .rodata
%%lab: db %1, 0
mksection .text
	mov	qword [rax+1], %%lab
	add	rax, 8+1
%rotate 1

%rep %0 - 1
	mov	byte [rax], 8
	mov	qword [rax+1], %1
%rotate 1
	add	rax, 8+1
%endrep
	mov	[pDebugBuffer], rax
	pop	rax
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro DBGPRINT64 1-*
	push	rax
	mov	rax, [pDebugBuffer]
%rep %0
	mov	byte [rax], 8
	mov	qword [rax+1], %1
%rotate 1
	add	rax, 8+1
%endrep
	mov	[pDebugBuffer], rax
	pop	rax
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro DBGPRINT32 1-*
	push	rax
	mov	rax, [pDebugBuffer]
%rep %0
	mov	byte [rax], 4
	mov	dword [rax+1], %1
%rotate 1
	add	rax, 4+1
%endrep
	mov	[pDebugBuffer], rax
	pop	rax
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro DBGPRINT16 1-*
	push	rax
	mov	rax, [pDebugBuffer]
%rep %0
	mov	byte [rax], 2
	mov	word [rax+1], %1
%rotate 1
	add	rax, 2+1
%endrep
	mov	[pDebugBuffer], rax
	pop	rax
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro DBGPRINT_LAB 1
	push	rax
	mov	rax, [pDebugBuffer]

	mov	byte [rax], 0x57
mksection .rodata
%%lab: db %1, 0
mksection .text
	mov	qword [rax+1], %%lab
	add	rax, 8+1

	mov	[pDebugBuffer], rax
	pop	rax
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro DBGHIST 2
	inc	dword [%1 + 4 * %2]
%endmacro
%macro DBGPRINT_ZMM 1-*
%endmacro
%macro DBGPRINT_YMM 1-*
%endmacro
%macro DBGPRINT_XMM 1-*
%endmacro
%macro DBGPRINT64 1-*
%endmacro
%macro DBGPRINT32 1-*
%endmacro
%macro DBGPRINT16 1-*
%endmacro
%macro DBGHIST 2
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%endif ; ifdef 0 ; OLD

%endif ; DBGPRINT_INC_INCLUDED
