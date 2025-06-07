;-------------------------------------------------
; Macro-definitions for BOOT.SYS file.
; Assembler: MADS-Assembler
; V0.1: 22-05-25, Emile first version
;-------------------------------------------------

.macro	PRHEX16	b16				; Print WORD (16-bit) as 2 hex-bytes
	LDA	:b16+1				; print MSB
	JSR	HEXOUT
	LDA	:b16
	JSR	HEXOUT				; print LSB
.endm

.macro	PRHEX32	b32				; Print DWORD (32-bit) as 4 hex-bytes
	PRHEX16	:b32+2				; Print MSB word first
	PRHEX16	:b32				; Print LSB word
.endm

.macro	LDXYI	b16
	LDX	<:b16				; MADS makes this immediate
	LDY	>:b16
.endm

.macro	LDXY	b16				; Load [X,Y] Pointer
	LDX	:b16
	LDY	:b16+1
.endm

.macro	STXY	b16				; Store [X,Y] Pointer
	STX	:b16
	STY	:b16+1
.endm

.macro	PRSTR	str				; Print String
	LDXYI	:str
	JSR	OS_STRING_OUT
.endm

.macro	PRCH	ch				; Print single character
	LDA	#:ch
	JSR	COUT
.endm
	
.macro	ASL32	b32				; ASL of 32-bit DWORD
	ASL     :b32		  		; SHL with C=0
        ROL     :b32+1		  
        ROL     :b32+2
        ROL     :b32+3		  
.endm

.macro	LSR32	b32				; LSR of 32-bit DWORD
	LSR     :b32+3		  		; SHR with C=0
        ROR     :b32+2
        ROR     :b32+1
        ROR     :b32
.endm

.macro	LSR16	b16				; LSR of 16-bit WORD
	LSR	:b16+1
	ROR	:b16
.endm

.macro	PHW	b16				; Push WORD onto stack
	LDA	:b16
	PHA
	LDA	:b16+1
	PHA
.endm

.macro	PLW	b16				; Pull WORD from stack
	PLA
	STA	:b16+1
	PLA
	STA	:b16
.endm

.macro	PHY					; Push Y-register
	TYA
	PHA
.endm

.macro	PLY					; Pull Y-register
	PLA
	TAY
.endm

.macro	MVAX	n src dst			; Move n bytes from SRC to DST using X as index
	LDX	#:n-1
lp	MVA	:src,X :dst,X-
	BPL	lp
.endm
	
.macro	MVAY	n src dst			; Move n bytes from SRC to DST using Y as index
	LDY	#:n-1
lp	MVA	:src,Y :dst,Y-
	BPL	lp
.endm

; Macros used for printing a cluster nr, which consists of 2 words
.macro	PRCLB	c st				; This prints a byte and optionally saves it
		LDA	(:c),Y
	ift :0>1	; st also given?
		STA	:st
	eif	
		JSR	HEXOUT			; changes Y!
.endm		

.macro	PRCLW	indx	curr_dir stor		; Print a word of a cluster nr and save it (optional)
		LDY	#:indx+1		; Print MSB of word
	ift :0>2	; stor also given?	
		PRCLB	:curr_dir :stor+1
	els	
		PRCLB	:curr_dir
	eif
		LDY	#:indx			; Print LSB of word
	ift :0>2	; stor also given?	
		PRCLB	:curr_dir :stor
	els	
		PRCLB	:curr_dir
	eif
.endm

BTST	.macro ' '	b1 b2 lbl1		; Bit test (7-0) combined with branch if equal
	ift :b2>7
		ERT "Only bits 7-0 allowed"
	els
		LDA	:b1
		AND	#(1 << :b2)
		BEQ	:lbl1
	eif
.endm
	
; ----------------------------------------------
; Compare + Branch Macros
; ----------------------------------------------
CMP.EQ	.macro ' '	b1 lbl			; Compare A with b1 and branch if equal
		CMP	:b1
		BEQ	:lbl
.endm		; A is now not equal to b1
	
CMP.NE	.macro ' '	b1 lbl			; Compare A with b1 and branch if not equal
		CMP	:b1
		BNE	:lbl
.endm		; A is now equal to b1

	; Compare A with b1 and branch if Carry is 0 (A-b1 < 0)
CMP.CC	.macro ' '	b1 lbl
		CMP	:b1			; 
		BCC	:lbl			; branch if C=0 (A-b1 < 0)
.endm		; C is now 1 (A-b1 is >= 0)

	; Compare A with b1 and branch if Carry is 1 (A-b1 >= 0)
CMP.CS	.macro ' '	b1 lbl
		CMP	:b1			; 
		BCS	:lbl			; branch if C=1 (A-b1 >= 0)
.endm		; C is now 0 (A-b1 is < 0)

CPX.EQ	.macro ' '	b1 lbl			; Compare X with b1 and branch if equal
		CPX	:b1
		BEQ	:lbl
.endm		; X is now not equal to b1
	
CPX.NE	.macro ' '	b1 lbl			; Compare X with b1 and branch if not equal
		CPX	:b1
		BNE	:lbl
.endm		; X is now equal to b1

	; Compare X with b1 and branch if Carry is 0 (A-b1 < 0)
CPX.CC	.macro ' '	b1 lbl
		CPX	:b1			; 
		BCC	:lbl			; branch if C=0 (X-b1 < 0)
.endm		; C is now 1 (X-b1 is >= 0)

	; Compare X with b1 and branch if Carry is 1 (X-b1 >= 0)
CPX.CS	.macro ' '	b1 lbl
		CPX	:b1			; 
		BCS	:lbl			; branch if C=1 (X-b1 >= 0)
.endm		; C is now 0 (X-b1 is < 0)

CPY.EQ	.macro ' '	b1 lbl			; Compare Y with b1 and branch if equal
		CPY	:b1
		BEQ	:lbl
.endm		; Y is now not equal to b1
	
CPY.NE	.macro ' '	b1 lbl			; Compare Y with b1 and branch if not equal
		CPY	:b1
		BNE	:lbl
.endm		; Y is now equal to b1

	; Compare Y with b1 and branch if Carry is 0 (Y-b1 < 0)
CPY.CC	.macro ' '	b1 lbl
		CPY	:b1			; 
		BCC	:lbl			; branch if C=0 (Y-b1 < 0)
.endm		; C is now 1 (Y-b1 is >= 0)

	; Compare Y with b1 and branch if Carry is 1 (Y-b1 >= 0)
CPY.CS	.macro ' '	b1 lbl
		CPY	:b1			; 
		BCS	:lbl			; branch if C=1 (Y-b1 >= 0)
.endm		; C is now 0 (Y-b1 is < 0)

; ----------------------------------------------
; Increment/Decrement + Branch Macros
; ----------------------------------------------
DEC.EQ	.macro 	' '	b1 lbl			; DEC + BEQ
		DEC	:b1
		BEQ	:lbl
.endm

DEC.NE	.macro 	' '	b1 lbl			; DEC + BNE
		DEC	:b1
		BNE	:lbl
.endm

INC.EQ	.macro 	' '	b1 lbl			; INC + BEQ
		INC	:b1
		BEQ	:lbl
.endm

INC.NE	.macro 	' '	b1 lbl			; INC + BNE
		INC	:b1
		BNE	:lbl
.endm

; ----------------------------------------------
DEX.NE	.macro 	lbl				; DEX + BNE
		DEX
		BNE	:lbl
.endm

DEX.EQ	.macro 	lbl				; DEX + BEQ
		DEX
		BEQ	:lbl
.endm

DEX.PL	.macro 	lbl				; DEX + BPL 
		DEX
		BPL	:lbl
.endm

DEX.MI	.macro 	lbl				; DEX + BMI
		DEX
		BMI	:lbl
.endm

DEX.CC	.macro 	lbl				; DEX + BCC 
		DEX
		BCC	:lbl
.endm

DEX.CS	.macro 	lbl				; DEX + BCS
		DEX
		BCS	:lbl
.endm

; ----------------------------------------------
DEY.EQ	.macro 	lbl				; DEY + BEQ
		DEY
		BEQ	:lbl
.endm

DEY.NE	.macro 	lbl				; DEY + BNE
		DEY
		BNE	:lbl
.endm

DEY.PL	.macro 	lbl				; DEY + BPL 
		DEY
		BPL	:lbl
.endm

DEY.MI	.macro 	lbl				; DEY + BMI
		DEY
		BMI	:lbl
.endm

DEY.CC	.macro 	lbl				; DEY + BCC 
		DEY
		BCC	:lbl
.endm

DEY.CS	.macro 	lbl				; DEY + BCS
		DEY
		BCS	:lbl
.endm
; ----------------------------------------------

; ----------------------------------------------
; LOAD/STORE + Branch Macros
; ----------------------------------------------
LDA.EQ	.macro 	' '	b1 lbl			; LDA + BEQ
		LDA	:b1
		BEQ	:lbl
.endm

LDA.NE	.macro 	' '	b1 lbl			; LDA + BNE
		LDA	:b1
		BNE	:lbl
.endm

STA.EQ	.macro 	' '	b1 lbl			; STA + BEQ
		STA	:b1
		BEQ	:lbl
.endm

STA.NE	.macro 	' '	b1 lbl			; STA + BNE
		STA	:b1
		BNE	:lbl
.endm

; ----------------------------------------------
; AND/OR + Branch Macros
; ----------------------------------------------
AND.EQ	.macro 	' '	p1 p2 p3		; LDA + AND + BEQ
	ift :0>2	; LDA + AND + BEQ	
		LDA	:p1
		AND	:p2
		BEQ	:p3
	els		; AND + BEQ
		AND	:p1
		BEQ	:p2
	eif
.endm

AND.NE	.macro 	' '	p1 p2 p3		; LDA + AND + BNE
	ift :0>2	; LDA + AND + BNE	
		LDA	:p1
		AND	:p2
		BNE	:p3
	els		; AND + BNE
		AND	:p1
		BNE	:p2
	eif
.endm
