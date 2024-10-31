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

; Macros for defining data structures

; Usage example

;START_FIELDS	; JOB_AES
;;;	name		size	align
;FIELD	_plaintext,	8,	8	; pointer to plaintext
;FIELD	_ciphertext,	8,	8	; pointer to ciphertext
;FIELD	_IV,		16,	8	; IV
;FIELD	_keys,		8,	8	; pointer to keys
;FIELD	_len,		4,	4	; length in bytes
;FIELD	_status,	4,	4	; status enumeration
;FIELD	_user_data,	8,	8	; pointer to user data
;UNION  _union,         size1,  align1, \
;	                size2,  align2, \
;	                size3,  align3, \
;	                ...
;END_FIELDS
;%assign _JOB_AES_size	_FIELD_OFFSET
;%assign _JOB_AES_align	_STRUCT_ALIGN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Alternate "struc-like" syntax:
;	STRUCT job_aes2
;	RES_Q	.plaintext,	1
;	RES_Q	.ciphertext, 	1
;	RES_DQ	.IV,		1
;	RES_B	.nested,	_JOB_AES_SIZE, _JOB_AES_ALIGN
;	RES_U	.union,		size1, align1, \
;				size2, align2, \
;				...
;	ENDSTRUCT
;	; Following only needed if nesting
;	%assign job_aes2_size	_FIELD_OFFSET
;	%assign job_aes2_align	_STRUCT_ALIGN
;
; RES_* macros take a name, a count and an optional alignment.
; The count in in terms of the base size of the macro, and the
; default alignment is the base size.
; The macros are:
; Macro    Base size
; RES_B	    1
; RES_W	    2
; RES_D     4
; RES_Q     8
; RES_DQ   16
; RES_Y    32
; RES_Z    64
;
; RES_U defines a union. It's arguments are a name and two or more
; pairs of "size, alignment"
;
; The two assigns are only needed if this structure is being nested
; within another. Even if the assigns are not done, one can still use
; STRUCT_NAME_size as the size of the structure.
;
; Note that for nesting, you still need to assign to STRUCT_NAME_size.
;
; The differences between this and using "struc" directly are that each
; type is implicitly aligned to its natural length (although this can be
; over-ridden with an explicit third parameter), and that the structure
; is padded at the end to its overall alignment.
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%ifndef _DATASTRUCT_INC_
%define _DATASTRUCT_INC_

;; START_FIELDS
%macro START_FIELDS 0
%assign _FIELD_OFFSET 0
%assign _STRUCT_ALIGN 0
%endm

;; FIELD name size align
%macro FIELD 3
%define %%name  %1
%define %%size  %2
%define %%align %3

%assign _FIELD_OFFSET (_FIELD_OFFSET + (%%align) - 1) & (~ ((%%align)-1))
%%name	equ	_FIELD_OFFSET
%assign _FIELD_OFFSET _FIELD_OFFSET + (%%size)
%if (%%align > _STRUCT_ALIGN)
%assign _STRUCT_ALIGN %%align
%endif
%endm

;; END_FIELDS
%macro END_FIELDS 0
%assign _FIELD_OFFSET (_FIELD_OFFSET + _STRUCT_ALIGN-1) & (~ (_STRUCT_ALIGN-1))
%endm

%macro UNION 5-*
%if (0 == (%0 & 1))
	%error EVEN number of parameters to UNION Macro
	%err
%endif
%rotate 1
	%assign _UNION_SIZE %1
	%assign _UNION_ALIGN %2
%rep (%0 - 3)/2
	%rotate 2
	%if (%1 > _UNION_SIZE)
		%assign _UNION_SIZE %1
	%endif
	%if (%2 > _UNION_ALIGN)
		%assign _UNION_ALIGN %2
	%endif
%endrep
%rotate 2
FIELD	%1, _UNION_SIZE, _UNION_ALIGN
%endm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%macro STRUCT 1
START_FIELDS
struc %1
%endm

%macro ENDSTRUCT 0
%assign %%tmp _FIELD_OFFSET
END_FIELDS
%assign %%tmp (_FIELD_OFFSET - %%tmp)
%if (%%tmp > 0)
	resb	%%tmp
%endif
endstruc
%endm

;; RES_int name size align
%macro RES_int 3
%define %%name  %1
%define %%size  %2
%define %%align %3

%assign _FIELD_OFFSET (_FIELD_OFFSET + (%%align) - 1) & (~ ((%%align)-1))
align %%align
%%name	resb	%%size
%assign _FIELD_OFFSET _FIELD_OFFSET + (%%size)
%if (%%align > _STRUCT_ALIGN)
%assign _STRUCT_ALIGN %%align
%endif
%endm

; macro RES_B name, size [, align]
%macro RES_B 2-3 1
RES_int %1, %2, %3
%endm

; macro RES_W name, size [, align]
%macro RES_W 2-3 2
RES_int %1, 2*(%2), %3
%endm

; macro RES_D name, size [, align]
%macro RES_D 2-3 4
RES_int %1, 4*(%2), %3
%endm

; macro RES_Q name, size [, align]
%macro RES_Q 2-3 8
RES_int %1, 8*(%2), %3
%endm

; macro RES_DQ name, size [, align]
%macro RES_DQ 2-3 16
RES_int %1, 16*(%2), %3
%endm

; macro RES_Y name, size [, align]
%macro RES_Y 2-3 32
RES_int %1, 32*(%2), %3
%endm

; macro RES_Z name, size [, align]
%macro RES_Z 2-3 64
RES_int %1, 64*(%2), %3
%endm

%macro RES_U 5-*
%if (0 == (%0 & 1))
	%error EVEN number of parameters to RES_U Macro
	%err
%endif
%rotate 1
	%assign _UNION_SIZE %1
	%assign _UNION_ALIGN %2
%rep (%0 - 3)/2
	%rotate 2
	%if (%1 > _UNION_SIZE)
		%assign _UNION_SIZE %1
	%endif
	%if (%2 > _UNION_ALIGN)
		%assign _UNION_ALIGN %2
	%endif
%endrep
%rotate 2
RES_int	%1, _UNION_SIZE, _UNION_ALIGN
%endm

%endif ; end ifdef _DATASTRUCT_INC_
