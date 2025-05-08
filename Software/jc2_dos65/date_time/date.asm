;---------------------------------------------------------------------------
; DATE.COM program
;---------------------------------------------------------------------------
		OPT h- ; do not add file header
		OPT f+ ; save as single block

BS        	EQU     $08    		; backspace key
CR              EQU     $0D             ; Carriage Return ASCII Code
CENTURY		EQU	$20		; the 20th century. change to travel in time
DATEDIV		EQU	'-'		; divider char for date string
DIVCHAR		EQU	$F2             ; current divider char (. or / for date : for time)

PSTR            EQU     $EA		; Used by STROUT and WRSTR routines
PSTRL     	EQU   	$EA      	; lower address byte of output string pointer
PSTRH     	EQU   	$EB      	; upper address byte of output string pointer
ACC		EQU	$F3		; accumulator
YREG		EQU	$F4		; y-register
XREG		EQU	$F5		; x-register
NUM32      	EQU   	$F8     	; low 32 bit number byte
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
WRITEDOW	EQU	$E283		; Write DOW, Mon=1 - Sun=7
READDOW		EQU	$E292		; Read DOW, Mon=1 - Sun=7
WRITEDATE2	EQU	$E2B7		; Write Date to RTC
READTIME        EQU     $E2DE		; Read Current time
READDATE        EQU     $E2E2		; Read Current date
LOADSTRING	EQU	$E543		; Load String pointer

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

		JSR PRINTFULLDATE	; Print current Date
		LDX #<TXT_INPUT     	; Print Enter new Date
		LDY #>TXT_INPUT     	;
		JSR SPRINT          	;
		JSR SETDATE		; Set new Date in RTC
		
		JSR DAYOFWEEK		; Calc. day-of-Week
		JSR WRITEDOW		; Write DOW into RTC
		JMP MON_WARM_START	; back to Monitor program
		
;----------------------------------------------------------------------------
; This routine prints the current date including the Day of the Week
;----------------------------------------------------------------------------
PRINTFULLDATE	JSR	READDOW
		ASL	
		ASL	
		TAY
		LDA  	#<(DAYS-4)
		STA  	PSTRL
		LDA  	#>(DAYS-4)
		STA  	PSTRH
		JSR	WRSTR
		JSR	SPCOUT

; **** Print Date **************************************************************
; ******************************************************************************
PRINTDATE	LDA	#DATEDIV	; load divider char
		STA	DIVCHAR
		JSR	READDATE	; read current date
		STA	TEMP		; store year value in TEMP
		TYA
		JSR	PRINTDIGIT	; print day
		TXA
		JSR	PRINTDIGIT	; print month
		LDA	#CENTURY
		JSR	HEXOUT		; print century
		LDA	TEMP
		JMP	HEXOUT		; print year

PRINTDIGIT	JSR	HEXOUT		; print digit
PRINTDIVCHAR	LDA	DIVCHAR		; print divider char
		JMP	COUT		; print and return

;----------------------------------------------------------------------------
; This routine sets a new date
;----------------------------------------------------------------------------
SETDATE		JSR	LOADSTRING
		LDA	#'-'
		STA	DIVCHAR
		LDX 	#<DATE_INPUT     	; Print title text
		LDY 	#>DATE_INPUT     	;
		JSR 	SPRINT          	;
		
		LDY	#$31			
		JSR	GETDIGIT
		CMP	#CR
		BEQ	SETDATEX		; Exit if CR pressed
		
		STA	YREG			; YREG = Day
		JSR	PRINTDIVCHAR
		LDY	#$12
		JSR	GETDIGIT
		CMP	#CR
		BEQ	SETDATEX		; Exit if CR pressed
		
		STA	XREG			; XREG = Month
		JSR	PRINTDIVCHAR
		LDY	#$99
		JSR	GETDIGIT
		CMP	#CR
		BEQ	SETDATEX		; Exit if CR pressed

		STA	ACC			; ACC = Year
		JMP	WRITEDATE2		; Write to RTC
		
SETDATEX	RTS				; Return

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

;----------------------------------------------------------------------------
; This routine divides a 16-bit number by a 8-bit number by means of 
; subtraction. 
; Input: TELLER: 16-bit
;        N8    :  8-bit divisor
;        RESULT: 16 bit result
;----------------------------------------------------------------------------
DIVU16_8	LDA	#0
		STA	RESULT
		STA	RESULT+1	; result = 0
DIVULP		LDA	TELLER		; teller -= N8
		SEC
		SBC	N8		; Subtract 8-bit noemer
		STA	TELLER
		LDA	TELLER+1	
		SBC	#0
		STA	TELLER+1
		INC	RESULT		; teller++
		BNE	DIV1
		
		INC	RESULT+1
DIV1		LDA	TELLER+1
		BNE	DIVULP		; branch if more to divide
		
		LDA	TELLER
		CMP	N8
		BCS	DIVULP		; branch if TELLER >= N8
		
		RTS			; RESULT now contains the result

;**** Convert BCD Number To 8 Bit Binary ***************************************
; INPUT:  A = BCD Number
; Output: A = Binary Number
; ******************************************************************************
BCD_TO_BIN      STA     NUM32               ; save BCD number
                AND     #$F0                ; and clear ones digit in A
                LSR                         ; calc tens digit * 8
                STA     NUM32+1             ; and store result
                LSR     
                LSR                         ; calc tens digit * 2
                CLC
                ADC     NUM32+1             ; add it with tens digit * 8
                STA     NUM32+1             ; and store result
                LDA     NUM32               ; reload BCD number int A
                AND     #$0F                ; and clear tens digit in A
                ADC     NUM32+1             ; finally add both result
                RTS

;----------------------------------------------------------------------------
; This routine prints a string to the terminal: X=LSB, Y=MSB
;----------------------------------------------------------------------------
SPRINT		STX PSTR	    	; LSB of text-pointer
		STY PSTR+1	    	; MSB of text-pointer
		JMP STROUT	    	; BIOS print string routine

;----------------------------------------------------------------------------
; Text-strings needed for Printing
;----------------------------------------------------------------------------
DAYS		.by	'Mon' $00
		.by	'Tue' $00
		.by	'Wed' $00
		.by	'Thu' $00
		.by	'Fri' $00
		.by	'Sat' $00
		.by	'Sun' $00

DATE_INPUT	.by	'dd' DATEDIV 'mm' DATEDIV 'yy' BS BS BS BS BS BS BS BS $00

TXT_TITLE	.by 'The current date is: ' $00
TXT_INPUT	.by CR 'Enter the new date: ' $00

;----------------------------------------------------------------------------
; Variables needed for Day-of-Week conversion
;----------------------------------------------------------------------------
TELLER		.ds 	2
N8		.ds 	1
RESULT		.ds 	2
TARR		.byte 	0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4
YEAR		.ds 	2
Y4		.ds 	2
Y100		.ds 	2
Y400		.ds 	2
DOW		.ds	1

;----------------------------------------------------------------------------
;dayofweek(y, m, d)	/* 1 <= m <= 12, y > 1752 (in the U.K.) */
;{
;    static int t[] = {0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4};
;    if ( m < 3 )
;    {
;        y -= 1;
;    }
;    return (y + y/4 - y/100 + y/400 + t[m-1] + d) % 7;
;}
;----------------------------------------------------------------------------
DAYOFWEEK	LDA	ACC
		JSR	BCD_TO_BIN
		STA	ACC
		CLC
		LDA	#<2000
		ADC	ACC
		STA	YEAR
		LDA	#>2000
		ADC	#0
		STA	YEAR+1		; YEAR = 2000 + ACC
		
		LDA	XREG		; XREG = month
		JSR	BCD_TO_BIN
		STA	XREG
		CMP	#3
		BCS	HMON		; branch if month >= 3
		
		SEC
		LDA	YEAR
		SBC	#1
		STA	YEAR
		LDA	YEAR+1
		SBC	#0
		STA	YEAR+1		; YEAR -= 1
		
HMON		LDA	YEAR
		STA	Y4
		LDA	YEAR+1
		STA	Y4+1		; Y4 = YEAR
		LSR	Y4+1
		ROR	Y4
		LSR	Y4+1
		ROR	Y4		; Y4 = YEAR/4
		
		LDA	YEAR
		STA	TELLER
		LDA	YEAR+1
		STA	TELLER+1	; Y100 = YEAR
		LDA	#100
		STA	N8		; divisor
		JSR	DIVU16_8	; divide year / 100
		LDA	RESULT
		STA	Y100		; y100 = year / 100
		STA	Y400		; y400 = year / 100
		LDA	RESULT+1
		STA	Y100+1		; Y100 = YEAR/100
		STA	Y400+1		; Y400 = YEAR/100
		
		LSR	Y400+1
		ROR	Y400
		LSR	Y400+1
		ROR	Y400		; Y400 = Y100/4 = YEAR/400
		
		CLC
		LDA	YEAR
		ADC	Y4
		STA	YEAR
		LDA	YEAR+1
		ADC	Y4+1
		STA	YEAR+1		; y = y + y/4
		
		SEC
		LDA	YEAR
		SBC	Y100
		STA	YEAR
		LDA	YEAR+1
		SBC	Y100+1
		STA	YEAR+1		; y = y + y/4 - y/100

		CLC
		LDA	YEAR
		ADC	Y400
		STA	YEAR
		LDA	YEAR+1
		ADC	Y400+1
		STA	YEAR+1		; y = y + y/4 - y/100 + y/400
		
		LDX	XREG		; XREG = month
		DEX			; X = month-1
		LDA	TARR,X 		; get t[m-1]
		CLC
		ADC	YEAR
		STA	YEAR
		LDA	YEAR+1
		ADC	#0
		STA	YEAR+1		; y = y + y/4 - y/100 + y/400 + t[m-1]
		
		LDA	YREG		; YREG = Day
		JSR	BCD_TO_BIN
		STA	YREG

		CLC
		ADC	YEAR
		STA	YEAR
		STA	TELLER
		LDA	YEAR+1
		ADC	#0
		STA	YEAR+1		; y = y + y/4 - y/100 + y/400 + t[m-1] + d
		STA	TELLER+1
		
		LDA	#7		; divide by 7
		STA	N8
		JSR	DIVU16_8	; YEAR / 7, with remainder in TELLER
		LDA	TELLER
		BNE	NOTSUNDAY	; branch if 1-6 (Mon-Sat)
		
		LDA	#7		; 7 = Sunday for RTC
NOTSUNDAY	STA	DOW		; dow = (y + y/4 - y/100 + y/400 + t[m-1] + d) % 7
		
		RTS			; return
		
		