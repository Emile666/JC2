;---------------------------------------------------------------------------
; ROM2RAM.COM program
; This program copies the existing BIOS at $E000-$FFFF to RAM and then 
; resets the JC2, so that it now runs from RAM.
; This program serves as a starting point for additions to the existing BIOS.
;---------------------------------------------------------------------------
		OPT h- ; do not add file header
		OPT f+ ; save as single block

BS        	EQU     $08    		; backspace key
CR              EQU     $0D             ; Carriage Return ASCII Code

BIOS_EN		EQU	$01		; PORTB bit 0: 1 = enable BIOS at $E000
MON_EN		EQU	$02		; PORTB bit 1: 1 = enable Monitor at $1C00

ROM_START	EQU	$E000		; Start of BIOS
RAM_START	EQU	$4000		; Temp. RAM area

SRC		EQU	$CA 		; Source pointer
DEST		EQU	$CC 		; Destination pointer
PSTR      	EQU   	$EA      	; 2-byte BIOS output string Pointer

STRBUF		EQU	$1400		; string buffer used by STRIN routine

PORTB		EQU	$1702		; CRB-2=1: PIA PORTB register

MON_COLD_START	EQU  	$E000       	; BIOS monitor cold start, RESET vector
MON_WARM_START	EQU  	$E003       	; BIOS monitor warm start
CIN		EQU	$E047		; Read Character Routine
COUT		EQU     $E052           ; print 1 character
STRIN		EQU	$E062		; Read string and store in STRBUF ($1400)
STROUT          EQU     $E083
HEXOUT          EQU     $E091           ; print 2-byte hex number

		.word RUN_ADR		; Needed for lm command (XMODEM load program)

		ORG $2000
	
;----------------------------------------------------------------------------
; This is the main program-entry
;----------------------------------------------------------------------------
RUN_ADR 	LDX #<TXT_TITLE     	; Print title text
		LDY #>TXT_TITLE     	;
		JSR SPRINT          	;

		SEI		   	; disable interrupts
		JSR BIOS_ROM		; Start with BIOS ROM enabled
		
		LDA #<ROM_START		; SRC = ROM_START
		STA SRC
		LDA #>ROM_START
		STA SRC+1
		LDA #<RAM_START		; DEST = RAM_START
		STA DEST
		LDA #>RAM_START
		STA DEST+1
		
		LDY #0
PASS1		LDA (SRC),Y		; Copy ROM to RAM
		STA (DEST),Y
		INY
		BNE PASS1		; branch if not done yet
		
		INC DEST+1		; move to next page
		INC SRC+1
		BNE PASS1		; branch if not done yet
		
		JSR BIOS_RAM		; Now switch to RAM area
		LDA #<RAM_START		; SRC is now RAM_START
		STA SRC
		LDA #>RAM_START
		STA SRC+1
		LDA #<ROM_START		; DEST is now ROM_START
		STA DEST
		LDA #>ROM_START
		STA DEST+1
PASS2		LDA (SRC),Y		; Move RAM to ROM address
		STA (DEST),Y
		INY
		BNE PASS2		; branch if not done yet
		
		INC SRC+1		; move to next page
		INC DEST+1
		BNE PASS2 
		
		CLI			; Enable interrupts again
		JMP MON_COLD_START	; Reset system and start from RAM
		
;----------------------------------------------------------------------------
; This routine enables the RAM behind the BIOS ROM.
;----------------------------------------------------------------------------
BIOS_RAM	LDA PORTB
		AND #~BIOS_EN	   	
		STA PORTB	   	; disable BIOS ROM, enable RAM behind it
		RTS
		
;----------------------------------------------------------------------------
; This routine enables the BIOS ROM (and disables the RAM).
;----------------------------------------------------------------------------
BIOS_ROM	LDA PORTB
		ORA #BIOS_EN	   	; enable BIOS ROM again
		STA PORTB
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
TXT_TITLE	.by 'Copy BIOS ROM at $E000-$FFFF to RAM followed by Cold Start' CR $00

