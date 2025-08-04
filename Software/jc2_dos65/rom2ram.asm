;---------------------------------------------------------------------------
; ROM2RAM.COM program
; This program copies the existing BIOS at $E000-$FFFF to RAM and then 
; resets the JC2, so that it now runs from RAM.
; This program serves as a starting point for additions to the existing BIOS.
; V1: works with Bios V1.2.2
; V2: post read/write operations error messages added
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
DBG_PRINT	EQU	$1828		; 1 = Print debug info

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

INIT_BLKBUF	EQU	$F68B		; Init block-buffer
LOAD_LBA_CF	EQU	$FC85		; Load LBA into the CF-card
CF_RD_LBLK_BUF	EQU	$FCBC		; Bios V1.2.4 routine for CMD_READ_BUF
CF_RD_LBLK	EQU	$FCBF		; Bios V1.2.4 routine for CMD_READ and CMD_READ_BUF
CF_WR_LBLK_BUF	EQU	$FCF7		; Bios V1.2.4 routine for CMD_WRITE_BUF
CF_WR_LBLK	EQU	$FCFA		; Bios V1.2.4 routine for CMD_WRITE and CMD_WRITE_BUF

CMD_READ	EQU	34              ; Read data block from device
CMD_WRITE	EQU	35              ; Write data block to device
CMD_READ_BUF	EQU	37              ; Read data block from device to standard buffer
CMD_WRITE_BUF	EQU	38              ; Write data block to device from standard buffer


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
		MVA #1 DBG_PRINT	; Enable debug print info for boot.sys
		JMP MON_WARM_START	; Reset system and start from RAM

;----------------------------------------------------------------------------
; This routine patches the CF_RD_LBLK routine with debug info. This routine
; is used for the CF-IDE interface with a CMD_READ and a CMD_READ_BUF command.
; It also patches the CF_RD_LBLK_BUF routine, since this routine does not
; preserve the LOAD command in A.
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

		LDA 	#<CF_DBG_PR_RD		; Patch CF_RD_LBLK routine
		STA 	CF_RD_LBLK+1
		LDA 	#>CF_DBG_PR_RD
		STA 	CF_RD_LBLK+2

		LDA 	#<INIT_BLKBUFA		; Patch CF_RD_LBLK_BUF routine
		STA 	CF_RD_LBLK_BUF+1
		LDA 	#>INIT_BLKBUFA
		STA 	CF_RD_LBLK_BUF+2

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
; It also patches the CF_WR_LBLK_BUF routine, since this routine does not
; preserve the SAVE command in A.
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
.if	NO_WRITE = 1
		LDA	#$4C			; $4C = JMP, replace JSR with JMP
		STA	CF_WR_LBLK		; This disables the Write Action!
.endif
		LDA 	#<CF_DBG_PR_WR		; Patch CF_WR_LBLK routine
		STA 	CF_WR_LBLK+1
		LDA 	#>CF_DBG_PR_WR
		STA 	CF_WR_LBLK+2

		LDA 	#<INIT_BLKBUFA		; Patch CF_WR_LBLK_BUF routine
		STA 	CF_WR_LBLK_BUF+1
		LDA 	#>INIT_BLKBUFA
		STA 	CF_WR_LBLK_BUF+2

		LDA 	CF_WR_LBLK+1
		CMP 	#<CF_DBG_PR_WR
		BNE 	PATCH_ERR		; branch if patch not successful
		
		LDA 	CF_WR_LBLK+2
		CMP 	#>CF_DBG_PR_WR
		BNE 	PATCH_ERR		; branch if patch not successful
		BEQ	PATCH_OK		; branch always
		
;--------------------------------------------------------------------------
; Start of inserted Routine: $FCFA 20 85 FC JSR LOAD_LBA_CF
; JSR LOAD_LBA_CF is replaced by JSR CF_DBG_PR_WR
;--------------------------------------------------------------------------
CF_DBG_PR_WR	JSR	SAVE_AXY		; Save Write command and LBA pointer
		LDX	#<TXT_WR1		; Print 'WLBA $'
		LDY	#>TXT_WR1
		JMP	CF_DBG_PR_RW		; branch always

;--------------------------------------------------------------------------
; Start of inserted Routine: $FCBF 20 85 FC JSR LOAD_LBA_CF
; JSR LOAD_LBA_CF is replaced by JSR CF_DBG_PR_RD
;--------------------------------------------------------------------------
CF_DBG_PR_RD	JSR	SAVE_AXY		; Save Read command and LBA pointer
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

		JSR	LOAD_AXY		; Restore R/W command and LBA pointer
		JSR 	LOAD_LBA_CF		; execute subroutine that was patched
		LDA	SAVA
		CMP	#CMD_READ
		BEQ	RW_CONT
		CMP	#CMD_READ_BUF
		BEQ	RW_CONT
		CMP	#CMD_WRITE
		BEQ	RW_CONT
		CMP	#CMD_WRITE_BUF
		BEQ	RW_CONT

		LDX	#<TXT_CMDERR		; Print 'Unknown command:$'
		LDY	#>TXT_CMDERR
		JSR	SPRINT			; Print
		LDA	SAVA
		JSR	HEXOUT			; Print command in hex
		LDA	#','
		JSR	COUT
		TSX
		INX
STRACE		LDA	$100,X			; stack trace
		STX	SAVX
		JSR	HEXOUT
		LDA	#','
		JSR	COUT
		LDX	SAVX
		INX
		BNE	STRACE
		JMP	CROUT			; Print CR and return

		; A RTS here would continue at CF_RD_BLK ($FCC2 = CF_RD_LBLK+3) or at CF_WR_BLK ($FCFD = CF_WR_LBLK+3)
RW_CONT		LDA	#>(PR_RW_POST-1)
		PHA				; Push MSB of return-address - 1 first
		LDA	#<(PR_RW_POST-1)
		PHA				; Push LSB of return-address - 1
		LDA	SAVA			; Restore R/W command
		RTS				; Will now continue at CF_RD_BLK or CF_WR_BLK
		
		; Returns here if CF_RD_BLK or CF_WR_BLK is finished
PR_RW_POST	STX	SAVX			; Result from load buffer operation, X=1 (page 0), X=0 (page 1)
		STY	SAVY			; Result from load buffer operation, Y is index in page
		BCC	ERR_RW			; branch if C=0 (error)

		LDX	#<TXT_OK		;
		LDY	#>TXT_OK
		BNE	POST_PR			; branch always
		
ERR_RW		LDX	#<TXT_ERR1		;
		LDY	#>TXT_ERR1
POST_PR		JSR	SPRINT			; Print it
		LDA	SAVX			; Print X value
		JSR	HEXOUT
		LDX	#<TXT_ERR2		;
		LDY	#>TXT_ERR2
		JSR	SPRINT			; Print it
		LDA	SAVY			; Print Y value		
		JSR	HEXOUT
		JMP	CROUT			; Print CR and return

LOAD_AXY	LDA	SAVA
		LDX	SAVX
		LDY	SAVY
		RTS
		
SAVE_AXY	STA	SAVA
		STX	SAVX
		STY	SAVY
		RTS

INIT_BLKBUFA	PHA
		JSR	INIT_BLKBUF
		PLA
		RTS
		
TXT_ERR1	.by	', C=0 Error, X=$' $00		; Error message
TXT_ERR2	.by	', Y=$' $00	
TXT_OK		.by	', C=1 OK, X=$' $00		; OK message
TXT_CMDERR	.by	', unknown command:$' $00	; No read or write command
	
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
