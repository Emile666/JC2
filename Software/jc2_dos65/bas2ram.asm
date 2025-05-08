;---------------------------------------------------------------------------
; BAS2RAM.COM program
; This program copies the existing BASIC ROM at $B000-$DFFF to RAM and then 
; runs Basic from RAM.
; This program serves as a starting point for patches to the existing Basic.
;---------------------------------------------------------------------------
		OPT h- ; do not add file header
		OPT f+ ; save as single block

BS        	EQU     $08    		; backspace key
CR              EQU     $0D             ; Carriage Return ASCII Code

BIOS_EN		EQU	$01		; PORTB bit 0: 1 = enable BIOS ROM at $E000
MON_EN		EQU	$02		; PORTB bit 1: 1 = enable Monitor ROM at $1C00

BAS_START	EQU	$B000		; Start of BASIC-ROM
RAM_START	EQU	$4000		; Temp. RAM area

SRC		EQU	$CA 		; Source pointer
DEST		EQU	$CC 		; Destination pointer
PSTR      	EQU   	$EA      	; 2-byte BIOS output string Pointer

BLKBUF		EQU	$DC             ; pointer to block buffer
BLKBUFL		EQU	$DC             ; lower byte of block buffer pointer
BLKBUFH		EQU	$DD             ; upper byte of block buffer pointer

; Pointer to Logical Block Address *********************************************
PLBA		EQU	$E6		; LBA pointer
PLBAL		EQU	$E6		; LBA pointer low byte
PLBAH		EQU	$E7		; LBA pointer high byte
PSAV            EQU     $EE

DIG0      	EQU   	$F8     	; 10^0 digit
DIG1	  	EQU   	$F9	 	; 10^1 digit
DIG2      	EQU   	$FA     	; 10^2 digit

STRBUF		EQU	$1400		; string buffer used by STRIN routine

PORTB		EQU	$1702		; CRB-2=1: PIA PORTB register

BAS_COLD_START	EQU	$B000		; Basic Cold-Start entry
MON_WARM_START	EQU  	$E003       	; BIOS monitor warm start
COUT		EQU     $E052           ; print 1 character
STROUT          EQU     $E083
HEXOUT          EQU     $E091           ; print 2-byte hex number
NUMOUT		EQU	$E0A7		; Print a byte as decimal number
DEC2STR		EQU	$E0BD		; Convert a byte to Decimal string
DELAY		EQU     $E14D		; Delay routine
SPRINT		EQU	$F682		; Print routine in BIOS v1.2.1
BAS2RAM		EQU	$FD86		; Basic to RAM routine in BIOS v1.2.0
BAS2ROM		EQU	$FD8F		; Basic to ROM routine in BIOS v1.2.0

		.word RUN_ADR		; Needed for lm command (XMODEM load program)

		ORG $3000
	
;----------------------------------------------------------------------------
; This is the main program-entry
;----------------------------------------------------------------------------
RUN_ADR 	LDX #<TXT_TITLE     	; Print title text
		LDY #>TXT_TITLE     	;
		JSR SPRINT          	;

		SEI		   	; disable interrupts
		JSR BAS2ROM		; Start with BIOS ROM enabled
		
		LDA #<BAS_START		; SRC = ROM_START
		STA SRC
		LDA #>BAS_START
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
		INC SRC+1		; 
		LDA SRC+1		;
		CMP #$E0 		; Begin of BIOS ROM, end of BASIC ROM
		BNE PASS1		; branch if not done yet
		
		JSR BAS2RAM		; Now switch to RAM area
		LDA #<RAM_START		; SRC is now RAM_START
		STA SRC
		LDA #>RAM_START
		STA SRC+1
		LDA #<BAS_START		; DEST is now BAS_START
		STA DEST
		LDA #>BAS_START
		STA DEST+1
PASS2		LDA (SRC),Y		; Move RAM to ROM address
		STA (DEST),Y
		INY
		BNE PASS2		; branch if not done yet
		
		INC SRC+1		; move to next page
		INC DEST+1
		LDA DEST+1
		CMP #$E0 		; Begin of BIOS ROM, end of BASIC ROM
		BNE PASS2 		; branch if not done yet
	
DONE		CLI			; Enable interrupts again
		
		; Patching BASIC stuff
BAS_PATCH	LDA	$D359		; 6 in 'Ehanced BASIC 2.26'
		CMP	#'6'
		BNE	WRADR
		
		;LDA	#4
		;STA	$D8D7
	
		;LDA	$D8D7		; Check if byte in RAM was changed
		;CMP	#$4
		BNE	NOPATCH		; Branch if not patched
		
		LDX 	#<TXT_OK    	; Print OK text
		LDY 	#>TXT_OK    	;
		JSR 	SPRINT
		JMP	BAS_COLD_START	; Goto Basic with cold-start

WRADR		LDX 	#<TXT_WRADR    	; Print Wrong Basic Version
		LDY 	#>TXT_WRADR    	;
		JSR 	SPRINT
		JMP	MON_WARM_START	; Back to monitor program
		
NOPATCH		LDX 	#<TXT_NOPATCH  	; Print Could not Patch
		LDY 	#>TXT_NOPATCH  	;
		JSR 	SPRINT
		JMP	MON_WARM_START	; Back to monitor program
		
;----------------------------------------------------------------------------
; Text-strings needed for Printing
;----------------------------------------------------------------------------
TXT_TITLE	.by 	'Copy BASIC ROM at $B000-$DFFF to RAM followed by Cold Start' CR $00
TXT_OK		.by	'BASIC now in RAM + successfully patched. Starting Basic...' CR $00
TXT_WRADR	.by	'Wrong BASIC ROM version' CR $00
TXT_NOPATCH	.by	'Could not patch BASIC RAM locations' CR $00
		END