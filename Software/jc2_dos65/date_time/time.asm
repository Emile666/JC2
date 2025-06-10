;---------------------------------------------------------------------------
; TIME.COM program
;---------------------------------------------------------------------------
		OPT h- ; do not add file header
		OPT f+ ; save as single block

BS        	EQU     $08    		; backspace key
CR              EQU     $0D             ; Carriage Return ASCII Code
CENTURY		EQU	$20		; the 20th century. change to travel in time
TIMEDIV		EQU	':'		; divider char for date string
DIVCHAR		EQU	$F2             ; current divider char (. or / for date : for time)

PSTR            EQU     $EA		; Used by STROUT and WRSTR routines
PSTRL     	EQU   	$EA      	; lower address byte of output string pointer
PSTRH     	EQU   	$EB      	; upper address byte of output string pointer
ACC		EQU	$F3		; accumulator
YREG		EQU	$F4		; y-register
XREG		EQU	$F5		; x-register
TEMP      	EQU   	$FC     	; temp storage
YSAV      	EQU   	$FD     	; Y register storage

STRBUF		EQU	$1400		; string buffer used by STRIN routine

MON_WARM_START	EQU  	$E003       	; BIOS monitor warm start
CIN		EQU	$E047		; Read Character Routine
COUT		EQU     $E052           ; print 1 character
SPCOUT		EQU	$E05E		; Write single space char to terminal
STRIN		EQU	$E062		; Read string and store in STRBUF ($1400)
STROUT          EQU     $E083
WRSTR		EQU	$E085		; Write String From Index Routine
HEXOUT          EQU     $E091           ; print 2-byte hex number
WRITETIME2	EQU	$E29F		; Write Time to RTC
LOADSTRING	EQU	$E543		; Load String pointer
PRINTTIME	EQU	$EC05		; Print Time routine
SPRINT		EQU	$F682		; Print a string to the terminal (BIOS v1.2.1)

.if	USE_XMODEM = 1
	.word	RUN_ADDR		; Needed for XMODEM lm command loading .bin files
.endif
		ORG $3000		; .COM file loads to $3000 by DOS
	
;----------------------------------------------------------------------------
; This is the main program-entry
;----------------------------------------------------------------------------
RUN_ADDR 	LDX #<TXT_TITLE     	; Print title text
		LDY #>TXT_TITLE     	;
		JSR SPRINT          	;

		JSR PRINTTIME		; Print current Time
		LDX #<TXT_INPUT     	; Print Enter new Time
		LDY #>TXT_INPUT     	;
		JSR SPRINT          	;
		JSR SETTIME		; Set new Time in RTC
		
		;JMP MON_WARM_START	; back to Monitor program
		RTS			; return to DOS
		
;----------------------------------------------------------------------------
; This routine sets a new time
;----------------------------------------------------------------------------
SETTIME		JSR	LOADSTRING
		LDA	#':'
		STA	DIVCHAR
		LDX 	#<TIME_INPUT     	; Print title text
		LDY 	#>TIME_INPUT     	;
		JSR 	SPRINT          	;
		
		LDY	#$23			
		JSR	GETDIGIT
		CMP	#CR
		BEQ	SETTIMEX		; Exit if CR pressed
		
		STA	ACC			; ACC = Hours
		LDA	DIVCHAR
		JSR	COUT
		LDY	#$59
		JSR	GETDIGIT
		CMP	#CR
		BEQ	SETTIMEX		; Exit if CR pressed
		
		STA	XREG			; XREG = Minutes
		LDA	DIVCHAR
		JSR	COUT
		LDY	#$59
		JSR	GETDIGIT
		CMP	#CR
		BEQ	SETTIMEX		; Exit if CR pressed

		STA	YREG			; ACC = Seconds
		JMP	WRITETIME2		; Write to RTC
		
SETTIMEX	RTS				; Return

;----------------------------------------------------------------------------
; This routine gets a digit or a CR
;----------------------------------------------------------------------------
GETDIGIT	INY
		STY	YSAV
GETDIGIT1	JSR	NUMINPUT	; Get Digit
		CMP	#CR		; Exit if CR
		BNE	GDIG1		; Branch if not a CR
		
		RTS			; Return, A = CR
		
GDIG1		CPX	#0		; 0 = Not a number
		BEQ	GETDIGIT1	; Branch if not a number or CR
		
		TAX
		SBC	#'0'
		ASL	
		ASL	
		ASL	
		ASL	
		CMP	YSAV
		BCS	GETDIGIT1
		
		STA	TEMP
		TXA
		JSR	COUT
GETDIGIT2	JSR	NUMINPUT	; Get Digit
		CMP	#CR		; Exit if CR
		BNE	GDIG2

		RTS			; Return, A = CR

GDIG2		CPX	#0		; 0 = Not a number
		BCC	GETDIGIT2	; Branch if not a number or CR
		
		TAX
		SBC	#'0'
		ORA	TEMP
		CMP	YSAV
		BCS	GETDIGIT2	; Branch if number too large
		
		STA	TEMP
		LDA	#'-'
		CMP	DIVCHAR
		BNE	GETDIGITEND
		
		LDA	TEMP
		BEQ	GETDIGIT2

GETDIGITEND	TXA
		JSR	COUT
		LDA	TEMP
		RTS

NUMINPUT	JSR	CIN
		CMP	#'0'
		BCC	NOTNUM
		
		CMP	#':'
		BCC	ISNUM
		
NOTNUM		LDX 	#0		; X=0, not a number
		RTS
ISNUM		LDX	#1		; X=1, a number
		RTS

TIME_INPUT	.by	'hh' TIMEDIV 'mm' TIMEDIV 'ss' BS BS BS BS BS BS BS BS $00

TXT_TITLE	.by 'The current time is: ' $00
TXT_INPUT	.by CR 'Enter the new time: ' $00

		
		