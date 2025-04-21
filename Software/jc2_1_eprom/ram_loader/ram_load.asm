;---------------------------------------------------------------------------
; RAM_LOAD.COM program
; This program loads an entire ROM from $B000-$FFFF into RAM.
;---------------------------------------------------------------------------
		OPT h- ; do not add file header
		OPT f+ ; save as single block
		
BS        	EQU     $08    		; backspace key
CR              EQU     $0D             ; Carriage Return ASCII Code

BIOS_EN		EQU	$01		; PORTB bit 0: 1 = enable BIOS at $E000
MON_EN		EQU	$02		; PORTB bit 1: 1 = enable Monitor at $1C00

RAM_START	EQU	$B000		; Start of destination RAM area

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
		JSR BIOS_RAM		; Start with BIOS ROM disabled
		JSR BASIC_RAM		; Start with BASIC ROM disabled
		
		MWA #ROM_IMG_BEGIN SRC	; SRC = ROM_IMG_BEGIN
		MWA #RAM_START DEST	; DEST = RAM_START
		
		LDY #0
PASS1		LDA (SRC),Y		; Copy ROM-image to RAM
		STA (DEST),Y
		INW SRC			; SRC++
		INW DEST		; DEST++
		BNE PASS1		; branch if not $FFFF+1 done yet
		
		LDX #<TXT_FIN     	; Print end text
		LDY #>TXT_FIN     	;
		JSR SPRINT          	;
		CLI			; Enable interrupts again
		;JMP MON_WARM_START	; Back to Monitor
		JMP MON_COLD_START	; Reset system and start from RAM
		
;----------------------------------------------------------------------------
; This routine enables the BIOS ROM and disables the RAM behind it.
;----------------------------------------------------------------------------
BIOS_ROM	LDA PORTB
		ORA #BIOS_EN	   	
		STA PORTB	   	; enable BIOS ROM, disable RAM behind it
		RTS

;----------------------------------------------------------------------------
; This routine enables the RAM behind the BIOS ROM.
;----------------------------------------------------------------------------
BIOS_RAM	LDA PORTB
		AND #~BIOS_EN	   	
		STA PORTB	   	; disable BIOS ROM, enable RAM behind it
		RTS
		
;----------------------------------------------------------------------------
; This routine enables the RAM behind the BASIC ROM (and disables the ROM).
;----------------------------------------------------------------------------
BASIC_RAM	LDA 	$0820		; Read from $0820 enables RAM area.
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
TXT_TITLE	.by 'BIOS RAM Loader, loads ROM-image into $B000-$FFFF' CR $00
TXT_FIN		.by 'Copy finished, now rebooting...' CR $00
ROM_IMG_BEGIN
		
