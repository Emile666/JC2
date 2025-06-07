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

ROM_START	EQU	$E000		; Start of BIOS
RAM_START	EQU	$9000		; Temp. RAM area

SRC		EQU	$CA 		; Source pointer
DEST		EQU	$CC 		; Destination pointer
PSTR      	EQU   	$EA      	; 2-byte BIOS output string Pointer
BLKBUF		EQU	$DC
BLKBUFL		EQU	$DC             ; Pointer to block buffer, same in BIOS
BLKBUFH		EQU	$DD

STRBUF		EQU	$1400		; string buffer used by STRIN routine

BIOS2ROM	EQU	$DFD8		; Bios to ROM routine in Bios V1.2.2
BIOS2RAM	EQU	$DFE1		; Bios to RAM routine in Bios V1.2.2
BAS2RAM		EQU	$E006		; BASIC to RAM routine in Bios V1.2.2
BAS2ROM		EQU	$E00A		; BASIC to ROM routine in Bios V1.2.2
MON_COLD_START	EQU  	$E000       	; BIOS monitor cold start, RESET vector
MON_WARM_START	EQU  	$E003       	; BIOS monitor warm start
CIN		EQU	$E047		; Read Character Routine
COUT		EQU     $E052           ; print 1 character
CROUT           EQU     $E05A
STRIN		EQU	$E062		; Read string and store in STRBUF ($1400)
STROUT          EQU     $E083
HEXOUT          EQU     $E091           ; print 2-byte hex number
SPRINT		EQU	$F682		; String Print routine in Bios V1.2.2

LOAD_LBA_CF	EQU	$FC85		; Load LBA into the CF-card
CF_RD_LBLK	EQU	$FCBF		; Bios V1.2.2 routine for CMD_READ and CMD_READ_BUF
CF_WR_LBLK	EQU	$FCFA		; Bios V1.2.2 routine for CMD_WRITE and CMD_WRITE_BUF

		.word RUN_ADR		; Needed for lm command (XMODEM load program)

		ORG $8000
	
;----------------------------------------------------------------------------
; This is the main program-entry
;----------------------------------------------------------------------------
RUN_ADR 	LDX #<TXT_TITLE     	; Print title text
		LDY #>TXT_TITLE     	;
		JSR SPRINT          	;

		SEI		   	; disable interrupts
		JSR BAS2ROM		; Start with BASIC ROM enabled
		JSR BIOS2ROM		; Start with BIOS ROM enabled
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
		
		JSR BIOS2RAM		; Now switch to RAM area
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
		JSR PATCH1		; Patches BIOS Read function
		JSR PATCH2		; Patches BIOS Write function
		JMP MON_WARM_START	; Reset system and start from RAM

;----------------------------------------------------------------------------
; This routine patches the CF_RD_LBLK routine with debug info. This routine
; is used for the CF-IDE interface with a CMD_READ and a CMD_READ_BUF command.
;----------------------------------------------------------------------------
PATCH1		LDA 	CF_RD_LBLK		; $FCBF 20 85 FC JSR LOAD_LBA_CF
		CMP 	#$20			; $20 = JSR
		BNE 	VER_ERR			; branch if wrong BIOS version

		LDA 	CF_RD_LBLK+1
		CMP 	#<LOAD_LBA_CF
		BNE 	VER_ERR			; branch if wrong BIOS version

		LDA 	CF_RD_LBLK+2
		CMP 	#>LOAD_LBA_CF
		BNE 	VER_ERR			; branch if wrong BIOS version

		LDA 	#<CF_DBG_PR_RD
		STA 	CF_RD_LBLK+1
		LDA 	#>CF_DBG_PR_RD
		STA 	CF_RD_LBLK+2
		LDA 	CF_RD_LBLK+1
		CMP 	#<CF_DBG_PR_RD
		BNE 	PATCH_ERR		; branch if patch not successful
		
		LDA 	CF_RD_LBLK+2
		CMP 	#>CF_DBG_PR_RD
		BNE 	PATCH_ERR		; branch if patch not successful
		
PATCH_OK	LDX 	#<TXT_PATCH_OK
		LDY 	#>TXT_PATCH_OK
		JMP 	SPRINT			; Print OK and return
VER_ERR		LDX 	#<TXT_BIOS_VERR
		LDX 	#>TXT_BIOS_VERR
		JMP 	SPRINT			; Print ERR and Return
PATCH_ERR	LDX 	#<TXT_PATCH_ERR
		LDX 	#>TXT_PATCH_ERR
		JMP 	SPRINT			; Print ERR and Return
		
;----------------------------------------------------------------------------
; This routine patches the CF_WR_LBLK routine with debug info. This routine
; is used for the CF-IDE interface with a CMD_WRITE and a CMD_WRITE_BUF command.
;----------------------------------------------------------------------------
PATCH2		LDA 	CF_WR_LBLK		; $FCFA 20 85 FC JSR LOAD_LBA_CF
		CMP 	#$20			; $20 = JSR
		BNE 	VER_ERR			; branch if wrong BIOS version

		LDA 	CF_WR_LBLK+1
		CMP 	#<LOAD_LBA_CF
		BNE 	VER_ERR			; branch if wrong BIOS version

		LDA 	CF_WR_LBLK+2
		CMP 	#>LOAD_LBA_CF
		BNE 	VER_ERR			; branch if wrong BIOS version

		; CF_WR_LBLK	JSR	LOAD_LBA_CF		; Load LBA into CF-card
								; fall through to CF_WR_BLK
		; Replaced by:
		; CF_WR_LBLK	JMP	LOAD_LBA_CF		; Load LBA into CF-card and return
		; Remove these two lines if you want to enable Write actions.
		LDA	#$4C			; $4C = JMP, replace JSR with JMP
		STA	CF_WR_LBLK		; This disables the Write Action!
		
		LDA 	#<CF_DBG_PR_WR
		STA 	CF_WR_LBLK+1
		LDA 	#>CF_DBG_PR_WR
		STA 	CF_WR_LBLK+2
		LDA 	CF_WR_LBLK+1
		CMP 	#<CF_DBG_PR_WR
		BNE 	PATCH_ERR		; branch if patch not successful
		
		LDA 	CF_WR_LBLK+2
		CMP 	#>CF_DBG_PR_WR
		BNE 	PATCH_ERR		; branch if patch not successful
		BEQ	PATCH_OK		; branch always
		
;--------------------------------------------------------------------------
; Start of inserted Routine: $FCBF 20 85 FC JSR LOAD_LBA_CF
; JSR LOAD_LBA_CF is replaced by JSR CF_DBG_PR_RD
;--------------------------------------------------------------------------
CF_DBG_PR_RD	STA	SAVA			; Save Read command
		STX	SAVX			; save pointer LSB
		STY	SAVY			; Save pointer MSB
		LDX	#<TXT_RD1		; Print 'RLBA $'
		LDY	#>TXT_RD1
CF_DBG_PR_RW	JSR	SPRINT			; Print it
		LDA	SAVX			; Pointer LSB to LBA
		STA	SRC
		LDA 	SAVY			; pointer MSB to LBA
		STA	SRC+1
		LDY	#3
PR_RD_DBG_LP	TYA
		PHA				; Save Y
		LDA	(SRC),Y
		JSR	HEXOUT			; Print LBA as 4 hex bytes
		PLA
		TAY				; restore Y
		DEY
		BPL	PR_RD_DBG_LP		; branch if not 4 bytes printed yet

		LDX	#<TXT_RD2		; Print ', Buf $'
		LDY	#>TXT_RD2
		JSR	SPRINT			; Print it
		LDA	BLKBUFH			; Print Buffer-pointer
		JSR	HEXOUT
		LDA	BLKBUFL
		JSR	HEXOUT
		JSR	CROUT			; Print CR

PR_RD_X		LDA	SAVA			; Restore Read command
		LDX	SAVX			; Restore LSB of LBA pointer
		LDY	SAVY			; Restore MSB of LBA pointer
		JMP 	LOAD_LBA_CF		; execute patched line in BIOS and continue
	
;--------------------------------------------------------------------------
; Start of inserted Routine: $FCFA 20 85 FC JSR LOAD_LBA_CF
; JSR LOAD_LBA_CF is replaced by JSR CF_DBG_PR_WR
;--------------------------------------------------------------------------
CF_DBG_PR_WR	STA	SAVA			; Save Read command
		STX	SAVX			; save pointer LSB
		STY	SAVY			; Save pointer MSB
		LDX	#<TXT_WR1		; Print 'WLBA $'
		LDY	#>TXT_WR1
		JMP	CF_DBG_PR_RW		; branch always

SAVA		.byte 	$00			; Temp. save for A-register
SAVX		.byte 	$00			; Temp. save for X-register
SAVY		.byte 	$00			; Temp. save for Y-register

;----------------------------------------------------------------------------
; Text-strings needed for Printing
;----------------------------------------------------------------------------
TXT_TITLE	.by 	'Copy BIOS ROM at $E000-$FFFF to RAM' CR $00
TXT_PATCH_OK	.by 	'Patch OK' CR $00
TXT_PATCH_ERR	.by 	'Error: could not patch' CR $00
TXT_BIOS_VERR	.by 	'Error: wrong BIOS' CR $00
TXT_RD1		.by	'RLBA $' $00
TXT_RD2		.by	', Buf $' $00
TXT_WR1		.by	'WLBA $' $00
