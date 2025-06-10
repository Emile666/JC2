;---------------------------------------------------------------------------
; Compact-Flash Test Program for JC ][ CF_IDE interface
; v1: first version, copied from Daniel Tufvession 2013, https://www.waveguide.se/?article=26&file=cftest.asm
;---------------------------------------------------------------------------
        OPT h- ; do not add file header
        OPT f+ ; save as single block

; Defines and Constants ********************************************************
CR        	EQU     $0D    		; carriage return
LF	  	EQU     $0A    		; line feed

; ********************************
; * CF REGISTERS
; ********************************
CFBASE		EQU 	$0C80
CFREG0		EQU	CFBASE+0	; Data port
CFREG1		EQU	CFBASE+1	; Read: error-code, write: feature
CFREG2		EQU	CFBASE+2	; Number of sectors to transfer
CFREG3		EQU	CFBASE+3	; Sector address LBA 0 [0:7] (LSB)
CFREG4		EQU	CFBASE+4	; Sector address LBA 1 [8:15]
CFREG5		EQU	CFBASE+5	; Sector address LBA 2 [16:23]
CFREG6		EQU	CFBASE+6	; Sector address LBA 3 [24:27] (MSB)
CFREG7		EQU	CFBASE+7	; Read: Status, Write: Command
; CFREG7 Status: Bit7 Bit6 Bit5 Bit4 Bit3 Bit2 Bit1 Bit0
;                BSY   RDY WFT  SKC  DRQ  ECC  IDX  ERR
CFREG8		EQU	CFBASE+8	; A write with 0x00 is a HW reset

; $1B00-$1BFF is a 256-byte RAM area
CFLBA0          EQU     $1B00		; LBA 0 [0:7]
CFLBA1          EQU     CFLBA0+1	; LBA 1 [8:15]
CFLBA2          EQU     CFLBA0+2	; LBA 2 [16:23]
CFLBA3          EQU     CFLBA0+3	; LBA 3 [24:27]
PART00		EQU	CFLBA3+1	; Partition 0 begin LBA (LSB)
PART01		EQU	PART00+1
PART02		EQU	PART01+1
PART03		EQU	PART02+1	; MSB
NUMLO		EQU	PART03+1
NUMHI		EQU	NUMLO+1
DIG0      	EQU   	NUMHI+1     	; 10^0 digit
DIG1	  	EQU   	DIG0+1	 	; 10^1 digit
DIG2      	EQU   	DIG1+1     	; 10^2 digit
DIG3		EQU	DIG2+1		; 10^3 digit
DIG4		EQU	DIG3+1		; 10^4 digit
DIGCNT		EQU	DIG4+1		
LZSPC		EQU	DIGCNT+1	; Bit 1 = Add leading 0, Bit 0 = Add leading space
PRFLAG		EQU	LZSPC+1
WORK1		EQU	PRFLAG+1	; LSB
WORK2		EQU	WORK1+1
WORK3		EQU	WORK2+1
WORK4		EQU	WORK3+1		; MSB
SECT_PER_FAT	EQU	WORK4+1		; 4-bytes: #Sectors per FAT
; 3 more bytes
FATLBA0		EQU	SECT_PER_FAT+4	; LSB of FAT LBA begin sector
FATLBA1		EQU	FATLBA0+1	
FATLBA2		EQU	FATLBA1+1
FATLBA3		EQU	FATLBA2+1	; MSB of FAT LBA begin sector
CLUSLBA0	EQU	FATLBA3+1	; LSB of Root-dir LBA (= Cluster begin LBA)
CLUSLBA1	EQU	CLUSLBA0+1	
CLUSLBA2	EQU	CLUSLBA1+1
CLUSLBA3	EQU	CLUSLBA2+1	; MSB of Root-dir LBA (= Cluster begin LBA)	
SUBDIRLBA0	EQU	CLUSLBA3+1	; LSB of sub-dir LBA
SUBDIRLBA1	EQU	SUBDIRLBA0+1	
SUBDIRLBA2	EQU	SUBDIRLBA1+1
SUBDIRLBA3	EQU	SUBDIRLBA2+1	; MSB of sub-dir LBA	
SECT_CLUST	EQU	SUBDIRLBA3+1	; #Sectors/Cluster
SECT_IN_CLUST	EQU	SECT_CLUST+1	; Current sector within cluster [0..SECT_CLUST-1]
CURR_DIR_NR	EQU	SECT_IN_CLUST+1	; Current entry in directory-sector (needed for dir read)
NEW_DIR_CLUS	EQU	CURR_DIR_NR+1	; 3-bytes: Cluster number of new directory
CURR_DIR_CLUS	EQU	NEW_DIR_CLUS+3	; 3-bytes: Cluster number of current directoty
; 2 more bytes
WORK_CLUS_NR0	EQU	CURR_DIR_CLUS+3	; Working cluster number read from dir and converted to LBA (LSB)
WORK_CLUS_NR1	EQU	WORK_CLUS_NR0+1	; 
WORK_CLUS_NR2	EQU	WORK_CLUS_NR1+1	; MSB 
TEMP0 		EQU	WORK_CLUS_NR2+1 ;
TEMP1		EQU 	TEMP0+1
FATDWORD0	EQU	TEMP1+1		; LSB of FAT DWORD for a cluster
FATDWORD1	EQU	FATDWORD0+1     ;
FATDWORD2	EQU	FATDWORD1+1     ;
FATDWORD3	EQU	FATDWORD2+1     ;
CURR_FAT_NR	EQU	FATDWORD3+1	; Current sector in FAT-table (0 = FATLBA, 1 = FATLBA+1 etc.)
;CURR_FAT_NR+1	
FATLBAW0	EQU	CURR_FAT_NR+2	; LSB of FAT LBA work pointer
FATLBAW1	EQU	FATLBAW0+1	
FATLBAW2	EQU	FATLBAW1+1
FATLBAW3	EQU	FATLBAW2+1	; MSB of FAT LBA work pointer

; 1 more byte
	
; Zero-page variables
RSTACT		EQU	$C0		; 1-byte 1 = Reset pending
MSEC            EQU     $C1		; 1-byte msec. counter
TOCNTR		EQU	$C2		; 1-byte counts #resets applied
BUFPTR		EQU	$C3		; 2-byte buffer-pointer on zero-page
FATPTR		EQU	$C5		; 2-byte buffer-pointer on zero-page
PSTR      	EQU   	$EA      	; 2-byte BIOS output string Pointer
NUM32      	EQU   	$F8     	; low 32 bit number byte
SUM32           EQU     $FC             ; low 32 bit number byte

; $1800-$19FF is a 512-byte RAM area
CFBUF		EQU	$1800		; 512 byte buffer for CF-card reads

MBR_BUF		EQU	$7400		; 512 byte MBR buffer $7400-$75FF
VOLID_BUF	EQU	$7600		; 512 byte VOL-ID buffer $7600-$77FF
FAT_BUF		EQU	$7800		; 512 byte FAT buffer $7800-$79FF
DIR_BUF		EQU	$7A00		; 512 byte Directory buffer $7A00-$7BFF
DIR_SEC_BUF	EQU	$7C00		; 32 byte Dir Entry buffer $7C00-$7C1F

;CF_BUF		EQU	$7C00		; 512 byte CF-card data buffer $7C00-7DFF

; BIOS v1.1.4 Routines *********************************************************

STRBUF		EQU	$1400		; string buffer used by STRIN routine

MON_WARM_START	EQU  	$E003       	; BIOS monitor warm start
STROUT		EQU     $E083           ; string write routine v1.1.4 bios
HEXOUT          EQU     $E091           ; print 2-byte hex number
BOUT		EQU     $E044           ; print 1 byte
CIN             EQU     $E047           ; Read character routine
COUT		EQU     $E052           ; print 1 character
STRIN		EQU	$E062		; Read string and store in STRBUF ($1400)
DELAY		EQU     $E14D		; Delay routine
READTIME        EQU     $E2DE		; Read Current time
READDATE        EQU     $E2E2		; Read Current date

		.word RUN_ADR		; Needed for lm command (XMODEM load program)

		ORG $2000
	
;----------------------------------------------------------------------------
; This is the main program for the CF Tester.
;----------------------------------------------------------------------------
RUN_ADR 	LDA #<TXT_TITLE     	; Print title text
		LDY #>TXT_TITLE
		JSR SPRINT	    	; print
		JSR INIT_LBA		; Init LBA to 0
		JSR INIT_P0_LBA		; Init Partition 0 LBA to 0
                JSR CFINIT		; Init. CF-card with HW Reset
		BCS CF_EXIT		; Branch if Reset did not work
		
                JSR CFINFO		; Print CF-card info
PROMPT          JSR CIN			; get char.
                AND #$DF                ; to upper case
                CMP #'Q'		; Quit ?
		BNE MLP1

CF_EXIT         JMP MON_WARM_START  	; exit program, jump to JC monitor again

MLP1            CMP #'I'		; Info?
                BNE MLP2		; branch if not info

		JSR MENU_I		; Info command
		JMP PROMPT

MLP2		CMP #'R'		; Read-sector?
		BNE MLP3		; branch if not Read-sector
		
		JSR MENU_R		; Read-sector command
		JSR CFPRINT		; Print sector (512-bytes) to screen
		JMP PROMPT
		
MLP3		CMP #'H'		; HW-Reset ?
		BNE MLP4		; branch if no HW-Reset
		
		JSR CFINIT	    	; HW-reset + init. CF-card
		JMP PROMPT
		
MLP4		CMP #'M'		; MBR Read?
		BNE MLP5
		
		JSR INIT_LBA		; Set LBA to 0 (MBR)
		JSR READ_SEC		; Read sector and print contents
		JSR CFPRINT		; Print sector (512-bytes) to screen
		JSR MBR_PRINT		; Print MBR info
		LDA PART00
		ORA PART01
		ORA PART02
		ORA PART03
		BEQ MLP5		; Branch if no valid Partition 0 LBA found
		
		LDA PART00		; Copy Partition begin LBA into CFLBA
		STA CFLBA0
		LDA PART01
		STA CFLBA1
		LDA PART02
		STA CFLBA2
		LDA PART03
		STA CFLBA3
		JSR READ_SEC		; Read Partition 0 Volume ID
		JSR VOL_ID_PRINT	; Print Volume ID info
		LDA FATLBA0		; Copy FAT LBA begin into FATLBAW
		STA FATLBAW0
		LDA FATLBA1
		STA FATLBAW1
		LDA FATLBA2
		STA FATLBAW2
		LDA FATLBA3
		STA FATLBAW3
		LDA #0
		STA CURR_FAT_NR		; Set CURR_FAT_NR to 0 (0 = FAT LBA begin)
		STA CURR_FAT_NR+1
		JSR READ_FAT		; Read FAT begin sector into FAT_BUF
		JMP PROMPT
		
MLP5		CMP #'D'		; Directory command
		BNE MLP6
		
		JSR READ_DIR		; Read directory
		JMP PROMPT		; branch always
		
MLP6		CMP #'C'		; Change DIR?
		BNE MLP7
		JSR CH_DIR		; Change Dir
		JMP PROMPT		; branch always
		
MLP7		CMP #'K'		; Change DIR?
		BNE MLP8
		JSR MK_DIR		; Make Dir
		JMP PROMPT		; branch always
		
MLP8		CMP #'?'
		BNE MLP9
		
		LDA #<TXT_TITLE     	; Print title text
		LDY #>TXT_TITLE
		JSR SPRINT	    	; print

MLP9		LDA #'?'		; Unknown input
		JSR COUT
		JMP PROMPT              ; branch always

;----------------------------------------------------------------------------
; This routine reads a sector from the CF-card and display it in hex.
;----------------------------------------------------------------------------
MENU_I		LDA #CR     		; new-line
		JSR COUT
		JMP CFINFO		; Print CF-card info and return

;----------------------------------------------------------------------------
; This routine reads a sector from the CF-card and displays it in hex.
;----------------------------------------------------------------------------
MENU_R		LDA #<TXT_LBA_INP     	; Print Enter LBA number
		LDY #>TXT_LBA_INP
		JSR SPRINT

		JSR STRIN	    	; Input LBA number in hex
		LDX #1
MRLP1		LDA STRBUF,X		; start of number
		CMP #CR
		BEQ FEOS		; End-of-string found

		INX			; next character
		BNE MRLP1		; branch always

FEOS		DEX			; x points to last hex number again
		LDY #7			; points to lsb position
MRLP2		LDA STRBUF,X		; move bytes to end of buffer
		STA STRBUF,Y
		DEY
		DEX
		BNE MRLP2		; continue block move

; Now add leading zeros for all LBA bytes
		LDA #'0'
MRLP3		STA STRBUF,Y
		DEY
		BNE MRLP3

PRLBA		LDA #<TXT_LBA_NR     	; Print LBA sector-nr text
		LDY #>TXT_LBA_NR
		JSR SPRINT
		LDX #1
PRLBA1		LDA STRBUF,X		; Get LBA sector nr digit
		JSR COUT		; print it
		INX
		CPX #8			; all bytes printed?
		BNE PRLBA1		; branch if not all printed
		LDA #CR			; new-line
		JSR COUT
		
; STRBUF[1..7] now contains a 7-digit ASCII number (1=MSB, 7=LSB)
		LDA STRBUF+6		; LSB-1 of LBA-number in ascii
		JSR ASC2HEX		; convert to hex number
		ASL
		ASL
		ASL
		ASL
		STA CFLBA0		; LBA 0 (LSB) highest 4 bits
		LDA STRBUF+7		; LSB of LBA number in ascii
		JSR ASC2HEX
		AND #$0F
		ORA CFLBA0		; LBA 0 lowest 4 bits		
		STA CFLBA0		; Bits 7..0 of LBA

		LDA STRBUF+4		; next LBA-number in ascii
		JSR ASC2HEX		; convert to hex number
		ASL
		ASL
		ASL
		ASL
		STA CFLBA1		; LBA 1 highest 4 bits
		LDA STRBUF+5
		JSR ASC2HEX
		AND #$0F
		ORA CFLBA1		; LBA 1 lowest 4 bits		
		STA CFLBA1

		LDA STRBUF+2		; next LBA-number in ascii
		JSR ASC2HEX		; convert to hex number
		ASL
		ASL
		ASL
		ASL
		STA CFLBA2		; LBA 2 highest 4 bits
		LDA STRBUF+3
		JSR ASC2HEX
		AND #$0F
		ORA CFLBA2		; LBA 2 lowest 4 bits		
		STA CFLBA2

		LDA STRBUF+1		; MSB of LBA-number in ascii
		JSR ASC2HEX		; convert to hex number
		STA CFLBA3
		; Fall-through to READ_SEC
		
;----------------------------------------------------------------------------
; This routine copies the LBA-address from CFLBA0..3 into the CF-card and
; then reads 512 bytes (1 sector) from it.
;----------------------------------------------------------------------------
READ_SEC	JSR CFSLBA		; LBA to CF-IDE card
		LDA #$01
		STA CFREG2		; Read one Sector
		JSR CFWAITF		; Wait until CF-card ready
		LDA #$20		; Read Sector Command
		STA CFREG7		; CF command register
		JSR CFREAD_CBUF		; Read 512 bytes in CBUF
		JSR CFCHERR 		; check for error
		RTS			; return

;----------------------------------------------------------------------------
; This routine copies the LBA-address from CFLBA0..3 into the CF-card and
; then reads 512 bytes (1 sector) from it.
;----------------------------------------------------------------------------
WRITE_SEC	JSR CFSLBA		; LBA to CF-IDE card
		;LDA #$01
		;STA CFREG2		; Write one Sector
		;JSR CFWAITF		; Wait until CF-card ready
		;LDA #$30		; Write Sector Command
		;STA CFREG7		; CF command register
		;JSR CFWRITE_CBUF	; Write 512 bytes from CBUF
		;JSR CFCHERR		; check for error
		LDA #<TXT_LBA_NR     	; Print LBA sector-nr text
		LDY #>TXT_LBA_NR
		JSR SPRINT
		LDA CFLBA3		; Print LBA nr in hex
		JSR HEXOUT
		LDA CFLBA2
		JSR HEXOUT
		LDA CFLBA1
		JSR HEXOUT
		LDA CFLBA0
		JSR HEXOUT
		LDA #','
		JSR COUT
		LDA #'$'
		JSR COUT
		LDA BUFPTR+1		; Print buffer address in hex
		JSR HEXOUT
		LDA BUFPTR
		JSR HEXOUT
		LDA #CR			; new-line
		JSR COUT
		LDX #0			; init. #16-bytes counter
		LDY #0			; init. byte counter
		JSR CFPRLP1		; Print 512 hex bytes from BUFPTR
		RTS
		
;----------------------------------------------------------------------------
; This routine reads the root-directory from the CF-card.
;----------------------------------------------------------------------------
READ_DIR	LDA CLUSLBA0		; Test if MBR + Vol.ID have been read
		ORA CLUSLBA1
		ORA CLUSLBA2
		ORA CLUSLBA3
		BNE DIR_RD1
		
		LDA #<TXT_NO_DIR     	; Print Read MBR + Vol. ID first
		LDY #>TXT_NO_DIR
		JMP SPRINT		; print and return
		
DIR_RD1		JSR RD_ROOTDIR_SEC	; Read Root directory sector into CFBUF
DIR_LP		LDX #0
		LDY #0
		LDA (BUFPTR),Y		
		CMP #$E5		; Deleted entry?
		BNE DIR_LP1		; branch if normal entry

		JMP NEXT_DIR		; Next entry
		
DIR_LP1		LDY #$0B		; Attribute byte of Dir. entry
		LDA (BUFPTR),Y
		BNE ISDIRENTRY		; branch if valid dir. entry
		
END_OF_DIR	RTS			; Return if end-of-Dir
		
ISDIRENTRY	CMP #$0F		; Long file-name?
		BEQ LFNAME		; branch if long name

;--------------------------------------------------------------------------
; Read a short filename + extension
;--------------------------------------------------------------------------
SFNAME		LDY #0			; index in (BUFPTR)
GET_NMLP	LDA (BUFPTR),Y		; Get char. of filename
		CMP #' '		; Ready with name?
		BEQ GET_EXT		; branch if space, done with filename
		
		STA LFN_BUF,X		; Save char. of filename
		INX			; next. pos. in buffer
		INY			; next. char. to read from filename
		CPY #8			; all chars of filename done?
		BNE GET_NMLP		; branch if more chars to read
		
GET_EXT		LDY #8			; index now at extension
		LDA (BUFPTR),Y
		CMP #' '
		BEQ EXT_END		; branch if no extension
		
		PHA			; save char.
		LDA #'.'		; There's an extension, so add the '.'
		STA LFN_BUF,X
		INX
		PLA			; get char. back
EXT_LP		STA LFN_BUF,X		; first char of extension
		INX			; next. pos. in buffer
		INY			; next. char. to read from extension
		LDA (BUFPTR),Y		; get char. from extension
		CMP #' '		; Space?
		BEQ EXT_END		; branch if space, done with extension
		CPY #11			; Max. length of name+extension
		BNE EXT_LP		; branch if more ext chars
		
EXT_END		BEQ LFN_END		; branch always

;--------------------------------------------------------------------------
; Read a long name, this could span multiple dir. entries
;--------------------------------------------------------------------------
LFNAME		LDY #0
LFN_LPNXT	LDX #0			; index in LFN_TBL
LFN_LP		LDA LFN_TBL,X		; Load entries where LFN bytes are stored
		TAY
		LDA (BUFPTR),Y		; get char. from filename
		BEQ LFN_END		; 0 = end-of-filename
		
		STA LFN_BUF,X		; Store in long filename buffer
		INX
		CPX LFN_TBL_SZ		; Contains length of LFN_TBL
		BNE LFN_LP

		; 1 dir. entry done, LFN spans multiple dir entries, read next one
		;JSR NEXT_DIR_ENTRY	; Read next one
		;JMP LFN_LPNXT		; Continue reading LFN

LFN_END		LDA #0
		STA LFN_BUF,X		; Conclude LFN with a '\0'
		; JMP DIR_RD_CONT	; Continue reading directory
		
;--------------------------------------------------------------------------
DIR_RD_CONT	LDY #$0B		; Attribute byte of Dir. entry
		LDA (BUFPTR),Y
		CMP #$08		; Volume?
		BNE DIR_RD2

		LDA #<TXT_VOL     	; Print Volume
		LDY #>TXT_VOL
		JSR SPRINT
		JSR PR_DIR_NAME		; Print dir. name
		LDA #CR
		JSR COUT		; new-line
		JMP NEXT_DIR		; branch always
		
DIR_RD2		CMP #$10		; Directory?
		BNE DIR_RD3

		; Directory Entry
		JSR PR_DIR_NAME		; Print dir. name first
		LDA #<TXT_DIR     	; Print <DIR>
		LDY #>TXT_DIR
		JSR SPRINT
		; Print Date and Time
		JSR PR_DIR_DATE		; Print Date
		LDA #' '
		JSR COUT
		JSR PR_DIR_TIME		; Print Time
		LDA #CR
		JSR COUT
		JMP NEXT_DIR		; branch always

DIR_RD3		;CMP #$0F		; Long filename?
		;BEQ DIR_RD4
		
		AND #2			; Hidden flag?
		BNE NEXT_DIR		; branch if hidden file, print nothing

		; Normal file
DIR_RD4		JSR PR_DIR_NAME		; Print filename

		; Print Size in bytes
		LDY #$1D		
		LDA (BUFPTR),Y		; Get MSB of size
		STA NUMHI
		DEY 
		LDA (BUFPTR),Y		; Get LSB of size
		STA NUMLO
		LDX #1
		STX LZSPC		; Leading spaces
		JSR NUMOUT5		; Print max. 5 decimals
		LDA #' '		; next line
		JSR COUT		
		
		; Print Date and Time
		JSR PR_DIR_DATE		; Print Date
		LDA #' '
		JSR COUT
		JSR PR_DIR_TIME		; Print Time
		LDA #CR
		JSR COUT		; new-line
		
NEXT_DIR	JSR NEXT_DIR_ENTRY	; Get next. dir. entry
		JMP DIR_LP		; Continue reading dir.
		
RD_DIR_X	RTS			; Ready, return

;----------------------------------------------------------------------------
; This routine prints a directory entry name, up to 32 chars.
;----------------------------------------------------------------------------
PR_DIR_NAME	LDY #0
DIRNMPR		LDA LFN_BUF,Y		; Get filename char.
		BEQ DIRPRX		; branch if ready printing
		
		JSR COUTXY		; Print to screen
		INY 
		CPY #32			; Print up to a max. of 32 chars
		BNE DIRNMPR		; Continue printing filename

DIRPRX		LDA #' '		; Trailing spaces up to 32
		JSR COUT
		INY
		CPY #32
		BNE DIRPRX		; branch if not at pos. 32 yet
		
		RTS

;----------------------------------------------------------------------------
; This routine prints the date of a directory entry.
; If more sectors are needed, the next sector is read as well.
;----------------------------------------------------------------------------
		; Day
PR_DIR_DATE	LDY #$18		; LSB of date
		LDA (BUFPTR),Y		; Y:15-9, M: 8-5, D: 4-0
		PHA			; Save LSB for Month
		AND #$1F		; Mask out Day
		JSR NUMOUT2		; Print Day in Decimal with LZ
		LDA #'-'
		JSR COUT
		
		; Month
		LDY #$19		; MSB of date
		LDA (BUFPTR),Y		;
		ROR			; high bit of Month now in C
		PLA			; get LSB back
		ROR			; Month now in bits 7-4
		ROR
		ROR
		ROR
		ROR
		AND #$0F		; Month now in bits 3-0
		JSR NUMOUT2		; Print Day in Decimal
		LDA #'-'
		JSR COUT
		
		; Year
		LDY #$19		; MSB of date
		LDA (BUFPTR),Y		;		
		ROR			; Year now in bits 6-0
		AND #$7F		; Only use bits 6-0
		CLC
		ADC #<1980
		STA NUMLO
		LDA #>1980
		ADC #0
		STA NUMHI
		LDX #0
		STX LZSPC		; No leading zero/space
		JMP NUMOUT5		; Print Year and return

;----------------------------------------------------------------------------
; This routine prints the time of a directory entry.
; If more sectors are needed, the next sector is read as well.
;----------------------------------------------------------------------------
PR_DIR_TIME	; Hours
		LDY #$17		; MSB of Time
		LDA (BUFPTR),Y		; H:15-11, M: 10-5, S: 4-0 (*2)
		PHA			; Save MSB for Minutes
		ROR			; 
		ROR			;
		ROR			; Hours now in bits 4-0
		AND #$1F		; Only use bits 4-0
		JSR NUMOUT2
		LDA #':'
		JSR COUT
		
		; Minutes
		PLA 			; Restore MSB
		STA NUMLO		; temp. register
		LDY #$16		; Get LSB of Time
		LDA (BUFPTR),Y		; 
		PHA			; Save LSB for Seconds
		ROR NUMLO		; bit 8 now in C
		ROR			; rotate into A
		ROR NUMLO		; bit 9 now in C
		ROR			; rotate into A
		ROR NUMLO		; bit 10 now in C
		ROR			; Bits 10-5 of time now in A bits 7-2
		ROR			; 
		ROR
		AND #$1F		; Minutes now in bits 5-0
		JSR NUMOUT2		; Print Minutes in Decimal
		LDA #':'
		JSR COUT

		; Seconds
		PLA			; Get LSB of Time
		AND #$1F		; Mask out Seconds
		ASL			; Seconds *= 2
		JMP NUMOUT2		; Print Seconds and return
		
;----------------------------------------------------------------------------
; This routine increases BUFPTR to point to the next dir. entry.
; If more sectors and/or clusters are needed, the next sector is read as well.
;----------------------------------------------------------------------------
NEXT_DIR_ENTRY	INC CURR_DIR_NR		; current dir. entry + 1
		CLC
		LDA BUFPTR
		ADC #$20		; Next dir. entry
		STA BUFPTR
		LDA BUFPTR+1
		ADC #0
		STA BUFPTR+1
		CMP #>CFBUF+2		; 1 sector done?
		BNE NEXT_DIR_X		; branch if not done with this sector
		
		INC SECT_IN_CLUST	; Current sector within Cluster
		LDA SECT_IN_CLUST	; 
		CMP SECT_CLUST		; #sectors/ Cluster
		BEQ NEXT_DIR_X		; TODO: Use FAT to get next cluster and read It
		
		JSR INC_CFLBA		; LBA+1, next dir. sector
		JSR READ_SEC		; Read next sector into buffer
		;JSR CFPRINT		; DEBUG
		JSR INIT_BUFPTR		; Reset buffer-pointer
NEXT_DIR_X	RTS			; return
	
;----------------------------------------------------------------------------
; This routine decreases BUFPTR to point to the next dir. entry.
; If more sectors are needed, the next sector is read as well.
;----------------------------------------------------------------------------
PREV_DIR_ENTRY	DEC CURR_DIR_NR		; current dir. entry - 1
		SEC
		LDA BUFPTR
		SBC #$20		; Next dir. entry
		STA BUFPTR
		LDA BUFPTR+1
		SBC #0
		STA BUFPTR+1
		CMP #>CFBUF-1		; 1 sector done?
		BNE PREV_DIR_X		; branch if not done with this sector
		
		DEC SECT_IN_CLUST	; Current sector within Cluster
		BEQ PREV_DIR_X		; TODO: Use FAT to get previous cluster and read it
		
		JSR DEC_CFLBA		; LBA-1, previous dir. sector
		JSR READ_SEC		; Read next sector into buffer
PREV_DIR_X	RTS			; return

;----------------------------------------------------------------------------
; This routine increases the cluster dir. sector nr. CFLBA0..3 by 1.
;----------------------------------------------------------------------------
INC_CFLBA	CLC
		LDA CFLBA0		; CFLBA = CFLBA + 1
		ADC #1
		STA CFLBA0
		LDA CFLBA1
		ADC #0
		STA CFLBA1
		LDA CFLBA2
		ADC #0
		STA CFLBA2
		LDA CFLBA3
		ADC #0
		STA CFLBA3
		RTS			; return
		
;----------------------------------------------------------------------------
; This routine decreases the cluster dir. sector nr. CFLBA0..3 by 1.
;----------------------------------------------------------------------------
DEC_CFLBA	SEC
		LDA CFLBA0
		SBC #1
		STA CFLBA0
		LDA CFLBA1
		SBC #0
		STA CFLBA1
		LDA CFLBA2
		SBC #0
		STA CFLBA2
		LDA CFLBA3
		SBC #0
		STA CFLBA3
		RTS

;----------------------------------------------------------------------------
; This routine increases the cluster dir. sector nr. CFLBA0..3 by 1.
;----------------------------------------------------------------------------
INC_FATLBA	CLC
		LDA FATLBAW0		; FATLBAW = FATLBAW + 1
		ADC #1
		STA FATLBAW0
		LDA FATLBAW1
		ADC #0
		STA FATLBAW1
		LDA FATLBAW2
		ADC #0
		STA FATLBAW2
		LDA FATLBAW3
		ADC #0
		STA FATLBAW3
		
		CLC
		LDA CURR_FAT_NR		; CURR_FAT_NR ++
		ADC #1
		STA CURR_FAT_NR
		LDA CURR_FAT_NR+1
		ADC #0
		STA CURR_FAT_NR+1
		RTS			; return

;----------------------------------------------------------------------------
; This routine reads one directory sector.
; It also resets BUFPTR to the beginning of this sector.
;----------------------------------------------------------------------------
RD_ROOTDIR_SEC	LDA CLUSLBA0		; Use Cluster LBA Begin for read
		STA CFLBA0
		STA WORK1		; Also save as current dir.
		LDA CLUSLBA1
		STA CFLBA1
		STA WORK2
		LDA CLUSLBA2
		STA CFLBA2
		STA WORK3
		LDA CLUSLBA3
		STA CFLBA3
		STA WORK4
		
		JSR READ_SEC		; Read the Cluster begin sector
		LDA #0
		STA CURR_DIR_NR		; Init. index in current directory-sector
		STA SECT_IN_CLUST	; Current sector within cluster
		STA CURR_DIR_CLUS+1
		STA CURR_DIR_CLUS+2
		LDA #2			; Root-dir
		STA CURR_DIR_CLUS	; Root-dir starts at cluster 2
		JMP INIT_BUFPTR		; Init. BUFPTR and return
		
;----------------------------------------------------------------------------
; This routine reads one directory sector.
; Input: WORK1..WORK4: the LBA of the dir. to read
; It also resets BUFPTR to the beginning of this sector.
;----------------------------------------------------------------------------
RD_SUBDIR_SEC	LDA WORK1		; Use Work LBA Begin for read
		STA CFLBA0
		LDA WORK2
		STA CFLBA1
		LDA WORK3
		STA CFLBA2
		LDA WORK4
		STA CFLBA3
		
		JSR READ_SEC		; Read the Cluster begin sector
		LDA #0
		STA CURR_DIR_NR		; Init. index in current directory-sector
		STA SECT_IN_CLUST	; Current sector within cluster
		
		JMP INIT_BUFPTR		; Init. BUFPTR and return

;----------------------------------------------------------------------------
; This routine converts a lower-case letter into upper-case and leaves all
; other characters intact.
;----------------------------------------------------------------------------
TOUPPER		CMP #'z'+1
		BCS TOUPX		; branch if not a letter
		
		CMP #'a'
		BCC TOUPX		; branch if not a letter
		
		AND #$DF		; make uppercase
TOUPX		RTS

;----------------------------------------------------------------------------
; This routine compares 2 strings: one in STRBUF and one in (BUFPTR)
;----------------------------------------------------------------------------
STRCMP		LDY #0
STRCMPLP	LDA STRBUF+1,Y		; Input-buffer, starts at 1
		CMP #CR
		BEQ STRCMPYES		; CR, we are done
		
		JSR TOUPPER		; uppercase
		STA STRBUF+1,Y
		LDA (BUFPTR),Y
		JSR TOUPPER		; uppercase too
		CMP STRBUF+1,Y		; CF-card dir. sector
		BNE STRCMPNO		; branch if not the same

		INY
		CPY #11			; max len of dirname
		BNE STRCMPLP		; branch if not at end yet

STRCMPYES	CPY #0
		BEQ STRCMPNO		; no strcmp if 1st char is a CR
		
		SEC			; C=1, strings are the same
		RTS
		
STRCMPNO	CLC			; C=0, not found
		RTS			; return

;----------------------------------------------------------------------------
; This routine Finds a subdir name in the current directory. If found, it
; copies the Cluster number into WORK_CLUS_NR0..2.
;----------------------------------------------------------------------------
FIND_SUBDIR	JSR RD_SUBDIR_SEC	; Read begin of current dir again
FIND_LP		JSR STRCMP		; Find string in dir. sector
		BCS SUBDIRFND		; branch if subdir found
		
		JSR NEXT_DIR_ENTRY	; goto next dir. entry
		LDY #0
		LDA (BUFPTR),Y		; 1st byte of Name, 0 = end-of-Dir
		BEQ NOTFND		; branch if not found
		
		JMP FIND_LP		; branch always

NOTFND		SEC			; C=1, not found
		RTS
		
SUBDIRFND	LDA #'!'
		JSR COUT
		LDA CURR_DIR_NR
		JSR HEXOUT
		LDA #'!'
		JSR COUT
		LDY #$14		; 1st cluster high, LSB
		LDA (BUFPTR),Y
		STA WORK_CLUS_NR2	; Copy cluster found into work-cluster
		JSR HEXOUT
		LDY #$1B		; 1st cluster low, MSB
		LDA (BUFPTR),Y
		STA WORK_CLUS_NR1	; Copy cluster found into work-cluster
		JSR HEXOUT
		LDY #$1A		; 1st cluster low, LSB
		LDA (BUFPTR),Y
		STA WORK_CLUS_NR0
		JSR HEXOUT
		LDA #'!'
		JSR COUT
		CLC			; C=0, A Subdir name was found
		RTS

;----------------------------------------------------------------------------
; This routine Converts a Cluster-nr (24-bit) into a LBA number (32-bit)
; Formula used: if (nr < 2) nr = 2 else nr = WORK_CLUS_NR
;               LBA = Cluster_Begin_LBA + (nr - 2) * Sectors_Cluster
; Input : CLuster nr in WORK_CLUS_NR0..2
; Output: LBA in WORK1..WORK4
;----------------------------------------------------------------------------
CLUS_NR_TO_LBA	LDA WORK_CLUS_NR2
		BNE NORM_CLUS		; branch if WORK_CLUS >= 2
		
		LDA WORK_CLUS_NR1
		BNE NORM_CLUS		; branch if WORK_CLUS >= 2
		
		LDA WORK_CLUS_NR0
		CMP #2
		BCS NORM_CLUS		; branch if WORK_CLUS >= 2

		LDA #2
		STA WORK_CLUS_NR0	; Root-dir reads as 0, so set it to 2
NORM_CLUS	SEC
		LDA WORK_CLUS_NR0	; WORK = WORK_CLUS - 2
		SBC #2
		STA WORK1
		LDA WORK_CLUS_NR1
		SBC #0
		STA WORK2
		LDA WORK_CLUS_NR2
		SBC #0
		STA WORK3
		LDA #0
		STA WORK4
		LDA SECT_CLUST		; #sectors/cluster
		LSR
		BEQ ADD_START_CLUS
		
CL2LBALP1	LDX #0
		LDY #4
		CLC
CL2LBALP2	ROL WORK1,X		; WORK = WORK * 2
		INX
		DEY
		BNE CL2LBALP2
		
		LSR
		BNE CL2LBALP1		; branch if not /4 yet

ADD_START_CLUS	CLC
		LDX #0
		LDY #4
CL2LBALP3	LDA WORK1,X		; WORK = WORK + CLUSLBA
		ADC CLUSLBA0,X
		STA WORK1,X
		INX
		DEY
		BNE CL2LBALP3
		
		LDA WORK4
		JSR HEXOUT
		LDA WORK3
		JSR HEXOUT
		LDA WORK2
		JSR HEXOUT
		LDA WORK1
		JSR HEXOUT
		LDA #'!'
		JSR COUT
		RTS			; return
		
;----------------------------------------------------------------------------
; This routine executes the 'Change Dir' command
;----------------------------------------------------------------------------
CH_DIR		LDA #<TXT_CHDIR     	; Print Enter Directory Name
		LDY #>TXT_CHDIR
		JSR SPRINT	    	; print
		JSR STRIN		; Input string and store in STRBUF
		JSR FIND_SUBDIR		; Find subdir string in current dir, cluster in WORK_CLUS_NR0..2
		BCC PATH_FND		; branch if subdir name found
		
		LDA #<TXT_PATHNFND     	; Print Path not found
		LDY #>TXT_PATHNFND
		JMP SPRINT		; print and return
		
PATH_FND	; Subdir name was found
		LDA WORK_CLUS_NR0	; Save Dir. Cluster Nr.
		STA CURR_DIR_CLUS
		LDA WORK_CLUS_NR1
		STA CURR_DIR_CLUS+1
		LDA WORK_CLUS_NR2
		STA CURR_DIR_CLUS+2
		JSR CLUS_NR_TO_LBA	; Convert WORK_CLUS_NR0..2 cluster found to WORK1..4 LBA
		JSR RD_SUBDIR_SEC	; Read a sub-directory with WORK1..4 LBA
		JSR DIR_LP		; Print directory listing
		RTS			; return
		
;----------------------------------------------------------------------------
; This routine reads a sector of the FAT into FAT_BUF.
; INPUT: FATLBAW (32 bit), contains the LBA of a FAT sector.
;----------------------------------------------------------------------------
READ_FAT	LDX #3
RDFLP1		LDA FATLBAW0,X		; Copy FAT LBA into CF LBA
		STA CFLBA0,X
		DEX
		BPL RDFLP1
		
		JSR CFSLBA		; LBA to CF-IDE card
		LDA #$01
		STA CFREG2		; Read one Sector
		JSR CFWAITF		; Wait until CF-card ready
		LDA #$20		; Read Sector Command
		STA CFREG7		; CF command register

		LDA #'['
		JSR COUT
		LDA CFLBA3
		JSR HEXOUT
		LDA CFLBA2
		JSR HEXOUT
		LDA CFLBA1
		JSR HEXOUT
		LDA CFLBA0
		JSR HEXOUT
		LDA #']'
		JSR COUT
		JSR CFREAD_FAT		; Read the 1st sector of the FAT
		JMP CFCHERR 		; check for error and return
		
;----------------------------------------------------------------------------
; This routine converts a 24-bit cluster number (as found in a directory for a
; filename or directory) into a FAT LBA number. A FAT sector contains 128
; entries of 4 bytes each.
; Formula used: CFLBA = FATLBA + WORK_CLUS/128
;----------------------------------------------------------------------------
CLUS2FATLBA	LDX #7			; SHR 7 = divide by 128
C2FLP1		CLC
		ROR WORK_CLUS_NR2
		ROR WORK_CLUS_NR1
		ROR WORK_CLUS_NR0
		DEX
		BNE C2FLP1

		CLC			; CFLBA = FATLBA + WORK_CLUS/128
		LDA FATLBA0		; FATLBA is root-sector of FAT table
		ADC WORK_CLUS_NR0
		STA CFLBA0
		LDA FATLBA1
		ADC WORK_CLUS_NR1
		STA CFLBA1
		LDA FATLBA2
		ADC WORK_CLUS_NR2
		STA CFLBA2
		LDA FATLBA3
		ADC #0
		STA CFLBA3
		RTS

;----------------------------------------------------------------------------
; This routine initializes the FAT buffer-pointer
;----------------------------------------------------------------------------
INIT_FATPTR	LDA #<FAT_BUF		; Init. FATPTR to FAT-buffer
		STA BUFPTR
		LDA #>FAT_BUF
		STA BUFPTR+1
		RTS
		
;----------------------------------------------------------------------------
; This routine finds the 1st entry in the FAT table that is empty and
; returns the cluster-number in NEW_DIR_CLUS.
;----------------------------------------------------------------------------
FIND_1ST_FAT_FREE
		LDY #0			;		
		JSR INIT_FATPTR		; Init. FATPTR to FAT-buffer
CHKFATLP1	LDA (BUFPTR),Y
		BNE CHKFATNXT
		
CHK_FAT1	INY			; next byte
CHK_FAT2	CPY #4			; 4 bytes in a DWORD
		BNE CHKFATLP1		; branch if not all bytes checked yet

		; A free FAT DWORD is found at CURR_FAT_NR*128 + (FAT_PTR index in FAT_BUF)/4
		; Now calculate the cluster-number from it
		LDA CURR_FAT_NR		; LSB
		STA WORK_CLUS_NR0
		LDA CURR_FAT_NR+1	; MSB
		STA WORK_CLUS_NR1
		LDA #0
		STA WORK_CLUS_NR2
		LDX #7			; SHL7 = multiply with 128; 128 DWORDS in 1 sectors
MUL128		ASL WORK_CLUS_NR0
		ROL WORK_CLUS_NR1
		ROL WORK_CLUS_NR2
		DEX
		BNE MUL128

		SEC			; BUFPTR -= FAT_BUF, this is the index in FAT_BUF
		LDA BUFPTR
		STA FATPTR		; Save for FAT write sector
		SBC #<FAT_BUF
		STA BUFPTR		; Store temporary in BUFPTR
		LDA BUFPTR+1
		STA FATPTR+1		; Save for FAT write sector
		SBC #>FAT_BUF
		STA BUFPTR+1

		LSR BUFPTR+1		; Divide by 4 to get the index (= SHR 2)
		ROR BUFPTR
		LSR BUFPTR+1
		ROR BUFPTR
		
		CLC			; NEW_DIR_CLUS = WORK_CLUS + BUFPTR
		LDA WORK_CLUS_NR0
		ADC BUFPTR
		STA NEW_DIR_CLUS
		LDA WORK_CLUS_NR1
		ADC BUFPTR+1
		STA NEW_DIR_CLUS+1
		LDA WORK_CLUS_NR2
		ADC #0
		STA NEW_DIR_CLUS+2
		RTS			; We are ready, now return
		
CHKFATNXT	LDY #0
		CLC			; Next entry in FAT (32-bits)
		LDA BUFPTR
		ADC #4			; 4 bytes = 32 bits
		STA BUFPTR
		LDA BUFPTR+1
		ADC #0
		STA BUFPTR+1
		CMP #>FAT_BUF+2		; Entire FAT sector (2*256 = 512 bytes)?
		BNE CHKFATLP1		; branch if not entire sector checked

		; 1 sector (512 bytes) processed, get next FAT sector
CHK_FAT3	JSR INC_FATLBA		; FATLBAW+1, CURR_FAT_NR++, next FAT dir. sector
		JSR READ_FAT		; Read next FAT sector (FATLBAW) into FAT_BUF
		JSR INIT_FATPTR		; Init. BUFPTR to begin of FAT-buffer again
		; TODO Check op #sectors in FAT
		LDY #0
		BEQ CHKFATLP1		; branch always
		
;----------------------------------------------------------------------------
; This routine finds the 1st entry in the current directory that is empty 
; and returns the cluster-number in WORK_CLUS. It assumes that a directory is
; read in until the last entry, so the 00 (free entry) should be in this buffer.
;----------------------------------------------------------------------------
FIND_1ST_DIR_FREE
		LDY #0			;	
		LDX #1			; 2 pages, 512 bytes total
		JSR INIT_BUFPTR		; Init. BUFPTR to CF-buffer
CHKDIRLP1	LDA (BUFPTR),Y
		BEQ FREEDIRFND		; 0 = a free Entry
		
		CMP #$E5		; A deleted entry, can be reused again
		BEQ FREEDIRFND		;
		
		TYA			; Y += $20
		CLC
		ADC #$20		; Next dir. Entry
		TAY
		BNE CHKDIRLP1		; branch if not on a page yet
		
		INC BUFPTR+1		; Next page
		DEX
		BPL CHKDIRLP1		; branch if not done yet
		
		SEC			; C=1, error, no free entry found
		RTS			; return

FREEDIRFND	CLC			; Free entry in (BUFPTR),Y
		RTS

;----------------------------------------------------------------------------
; This routine creates a new 32 byte record (DIR_SEC_BUF) 
; for a subdir in the current directory.
;----------------------------------------------------------------------------
CREATE_DIR_REC	LDX #$20		; 32 byte buffer
		LDA #0
CRDR1LP1	STA DIR_SEC_BUF,X	; clear dir entry buffer

		CPX #12			; bytes 0-11 (filename) should be init to spaces
		BNE CRDR1LP2
		
		LDA #' '		; space
CRDR1LP2	DEX
		BPL CRDR1LP1		; branch if not done
		
		INX			; X = 0 again
CRDR1LP3	LDA STRBUF+1,X
		CMP #CR
		BEQ CRDR1LP4
		
		STA DIR_SEC_BUF,X	; store filename in buffer
		INX 			; next byte
		CPX #12			; length of filename + 1
		BNE CRDR1LP3		; branch if filename not copied completely

CRDR1LP4	LDA #$10		; $10 = Directory
		STA DIR_SEC_BUF+$0B	; Save Dir attribute-byte
		JSR OS_FILEDATE		; Get Current Date from BIOS
		STY DIR_SEC_BUF+$19	; MSB of date into Last Write Date MSB
		STX DIR_SEC_BUF+$18	; LSB of date into Last Write Date LSB
		STY DIR_SEC_BUF+$13	; MSB of date into Last Access Date MSB
		STX DIR_SEC_BUF+$12	; LSB of date into Last Access Date LSB
		STY DIR_SEC_BUF+$11	; MSB of date into Create Date MSB
		STX DIR_SEC_BUF+$10	; LSB of date into Create Date LSB

		JSR OS_FILETIME		; Get Current Time from BIOS
		STY DIR_SEC_BUF+$17	; MSB of time into Last Write Time MSB
		STX DIR_SEC_BUF+$16	; LSB of time into Last Write Time LSB
		STY DIR_SEC_BUF+$0F	; MSB of time into Create Time MSB
		STX DIR_SEC_BUF+$0E	; LSB of time into Create Time LSB

		LDA NEW_DIR_CLUS	; Get LSB of new empty dir. cluster
		STA DIR_SEC_BUF+$1A	; Starting cluster (low word) LSB
		LDA NEW_DIR_CLUS+1	; 
		STA DIR_SEC_BUF+$1B	; Starting cluster (low word) MSB
		LDA NEW_DIR_CLUS+2	; Get MSB of new empty dir. cluster
		STA DIR_SEC_BUF+$14	; Starting cluster (high word) LSB
		; Filesize in $1C-$1F, set to 0 for dir.
		RTS

;----------------------------------------------------------------------------
; This routine creates a new record (512 bytes) in DIR_BUF for a subdir 
; with the . and .. dir entries filled in.
;----------------------------------------------------------------------------
CREATE_SUBDIR	LDX #0
		TXA 
CSDLP1		STA DIR_BUF,X		; Init. subdir sector with all zeros
		STA DIR_BUF+$100,X	; two pages, 512 bytes
		INX
		BNE CSDLP1		; branch if one page not done
		
		LDX #0
		LDA #' '		; Init. filenames with spaces
CSDLP2		STA DIR_BUF,X		; . dir entry
		STA DIR_BUF+$20,X	; .. dir entry
		INX
		CPX #12			; length of dirname + 1
		BNE CSDLP2		; branch if not done yet
		
		LDA #'.'		; dirname
		STA DIR_BUF+$00		; dirname .
		STA DIR_BUF+$20		; dirname ..
		STA DIR_BUF+$21	
		
		LDA #$10		; $10 = Directory
		STA DIR_BUF+$0B		; Save Dir attribute-byte for .
		STA DIR_BUF+$2B		; Save Dir attribyte byte for ..
		JSR OS_FILEDATE		; Get Current Date from BIOS
		STY DIR_BUF+$19		; MSB of date into Date MSB for .
		STX DIR_BUF+$18		; LSB of date into Date LSB for .
		STY DIR_BUF+$39		; MSB of date into Date MSB for ..
		STX DIR_BUF+$38		; LSB of date into Date LSB for ..
		STY DIR_BUF+$13		; MSB of date into Last Access Date MSB for .
		STX DIR_BUF+$12		; LSB of date into Last Access Date LSB for .
		STY DIR_BUF+$33		; MSB of date into Last Access Date MSB for ..
		STX DIR_BUF+$32		; LSB of date into Last Access Date LSB for ..
		STY DIR_BUF+$11		; MSB of date into Create Date MSB for .
		STX DIR_BUF+$10		; LSB of date into Create Date LSB for .
		STY DIR_BUF+$31		; MSB of date into Create Date MSB for ..
		STX DIR_BUF+$30		; LSB of date into Create Date LSB for ..
		JSR OS_FILETIME		; Get Current Time from BIOS
		STY DIR_BUF+$17		; MSB of time into Time MSB for .
		STX DIR_BUF+$16		; LSB of time into Time LSB for .
		STY DIR_BUF+$37		; MSB of time into Time MSB for .
		STX DIR_BUF+$36		; LSB of time into Time LSB for .
		STY DIR_BUF+$0F		; MSB of time into Create Time MSB for .
		STX DIR_BUF+$0E		; LSB of time into Create Time LSB for .
		STY DIR_BUF+$2F		; MSB of time into Create Time MSB for .
		STX DIR_BUF+$2E		; LSB of time into Create Time LSB for .

		LDA NEW_DIR_CLUS	; Get LSB of new dir. cluster
		STA DIR_BUF+$1A		; Starting cluster (low word) LSB for . (new)
		LDA NEW_DIR_CLUS+1	; 
		STA DIR_BUF+$1B		; Starting cluster (low word) MSB for . (new)
		LDA NEW_DIR_CLUS+2	; Get MSB of new dir. cluster
		STA DIR_BUF+$14		; Starting cluster (high word) LSB for . (new)

		LDA CURR_DIR_CLUS	; Get LSB of current dir. cluster
		STA DIR_BUF+$3A		; Starting cluster (low word) LSB for .. (parent)
		LDA CURR_DIR_CLUS+1	; 
		STA DIR_BUF+$3B		; Starting cluster (low word) MSB for .. (parent)
		LDA CURR_DIR_CLUS+2	; Get MSB of dir. cluster
		STA DIR_BUF+$34		; Starting cluster (high word) LSB for .. (parent)
		
		LDA CURR_DIR_CLUS+2	; Now test if parent dir is rootdir
		ORA CURR_DIR_CLUS+1
		BNE NOROOTDIR		; branch if not a rootdir
		
		LDA CURR_DIR_CLUS
		CMP #2			; Root-dir?
		BNE NOROOTDIR		; branch if not a rootdir
		
		LDA #0
		STA DIR_BUF+$3A		; Rootdir is noted as 0 in dir. entry
		; Filesize in $1C-$1F, set to 0 for dir.
NOROOTDIR	RTS

;----------------------------------------------------------------------------
; This routine save 3 sectors to the CF-card:
; 1) The current FAT sector in FAT_BUF is updated with the FAT for the new subdir
; 2) The current dir in CFBUF is updated with the new subdir entry in DIR_SEC_BUF
; 3) The new subdir cluster in DIR_BUF is written with the . and .. entries
;----------------------------------------------------------------------------
SAVE_DIR_AND_FAT
		LDY #0			; Set FAT entry to $FFFFFF0F
		LDA #$FF
		STA (FATPTR),Y
		INY
		STA (FATPTR),Y
		INY
		STA (FATPTR),Y
		INY
		LDA #$0F
		STA (FATPTR),Y
		
		JSR FIND_1ST_DIR_FREE	; Find first free entry in current dir.
		BCC SVDROK
		
		LDA #<TXT_NOFREEDIR   	; No free directory entry
		LDY #>TXT_NOFREEDIR
		JMP SPRINT		; Print and return

SVDROK		LDX #0
SVDROKLP	LDA DIR_SEC_BUF,X	; New dir. entry
		STA (BUFPTR),Y		; Copy into free dir. entry of current dir
		INY
		INX
		CPX #$20
		BNE SVDROKLP		; branch if not done yet

		LDA #<FAT_BUF		; Write current FAT sector
		STA BUFPTR
		LDA #>FAT_BUF
		STA BUFPTR+1
		; FATLBAW0 LBA
		JSR WRITE_SEC		; Write FAT sector
		
		LDA #<DIR_BUF		; Write new subdir sector
		STA BUFPTR
		LDA #>DIR_BUF
		STA BUFPTR+1
		LDA NEW_DIR_CLUS	; NEW_DIR_CLUS -> WORK_CLUS
		STA WORK_CLUS_NR0
		LDA NEW_DIR_CLUS+1
		STA WORK_CLUS_NR1
		LDA NEW_DIR_CLUS+2
		STA WORK_CLUS_NR2
		JSR CLUS_NR_TO_LBA	; WORK_CLUS_NR0..2 -> WORK1..4 LBA
		LDX #3
SVDRLP1		LDA WORK1,X		; Copy WORK LBA into CF LBA
		STA CFLBA0,X
		DEX
		BPL SVDRLP1		; branch if not done yet
		JSR WRITE_SEC		; Write new subdir sector

		LDA #<CFBUF		; Write current DIR sector
		STA BUFPTR
		LDA #>CFBUF
		STA BUFPTR+1
		LDA CURR_DIR_CLUS	; Current Dir. Cluster
		STA WORK_CLUS_NR0
		LDA CURR_DIR_CLUS+1
		STA WORK_CLUS_NR1
		LDA CURR_DIR_CLUS+2
		STA WORK_CLUS_NR2
		JSR CLUS_NR_TO_LBA	; WORK_CLUS_NR0..2 -> WORK1..4 LBA
		CLC			; Now add current sector within cluster
		LDA SECT_IN_CLUST
		ADC WORK1
		STA WORK1
		LDA WORK2
		ADC #0
		STA WORK2
		LDA WORK3
		ADC #0
		STA WORK3
		LDA WORK4
		ADC #0
		STA WORK4
		LDX #3
SVDRLP2		LDA WORK1,X		; Copy WORK LBA into CF LBA
		STA CFLBA0,X
		DEX
		BPL SVDRLP2		; branch if not done yet
		JSR WRITE_SEC		; Write current DIR sector

		RTS			; return

;----------------------------------------------------------------------------
; This routine creates a new directory in the current sub-directory
;----------------------------------------------------------------------------
MK_DIR		LDX #3
		LDA #<TXT_CHDIR     	; Print Enter Directory Name
		LDY #>TXT_CHDIR
		JSR SPRINT	    	; print
		JSR STRIN		; Input string and store in STRBUF
		JSR FIND_SUBDIR		; Find subdir string in current dir
		BCS PATH_NFND		; branch if subdir name was not found
		
		LDA #<TXT_PATHFND     	; Print Dir already exists
		LDY #>TXT_PATHFND
		JMP SPRINT		; print and return
		
PATH_NFND	; Subdir name was not found
		JSR FIND_1ST_FAT_FREE	; NEW_DIR_CLUS now contains nr of 1st free cluster
		
		LDA #<TXT_FATFREE     	; Debug
		LDY #>TXT_FATFREE
		JSR SPRINT	    	; print
		LDA NEW_DIR_CLUS+2	; NEW_DIR_CLUS contains the 1st free cluster nr
		STA WORK_CLUS_NR2	; Needed for CLUS_NR_TO_LBA function
		JSR HEXOUT
		LDA NEW_DIR_CLUS+1
		STA WORK_CLUS_NR1	; Needed for CLUS_NR_TO_LBA function
		JSR HEXOUT
		LDA NEW_DIR_CLUS
		STA WORK_CLUS_NR0	; Needed for CLUS_NR_TO_LBA function
		JSR HEXOUT
		LDA #','
		JSR COUT
		JSR CLUS_NR_TO_LBA	; WORK_CLUS_NR0..2 -> WORK1..4 LBA & print it
		LDA #CR
		JSR COUT
		JSR CREATE_DIR_REC	; Create a new directory struct for the current dir
		JSR CREATE_SUBDIR	; Create a new subdir with the . and .. entries
		JSR SAVE_DIR_AND_FAT	; Write 3 sectors: 1) FAT 2) Dir 3) New subdir
		RTS
		
;----------------------------------------------------------------------------
; This routine finds a FAT entry within a FAT sector. 
; A FAT sector contains 128 entries of 4 bytes each.
; It assumes that the proper FAT sector has already been read into FAT_BUF!
; Formula used: return *(FAT_BUF + 4 * (WORK_CLUS MOD 128))
;----------------------------------------------------------------------------
RD_FATDWORD	LDA #<FAT_BUF
		STA BUFPTR
		LDA #>FAT_BUF
		STA BUFPTR+1

		LDA WORK_CLUS_NR0	; LSB of WORK_CLUS
		AND #$7F		; Mask out bit 6-0 (0-127 decimal)
		STA TEMP0		;
		LDA #0
		STA TEMP1		; TEMP - WORK_CLUS MOD 128
		
		ASL TEMP0		; SHL 2 = multiply with 4
		ROL TEMP1
		ASL TEMP0
		ROL TEMP1		; TEMP = 4 * (WORK_CLUS MOD 128)
		
		CLC			; BUFPTR = BUFPTR + 4 * TEMP
		LDA TEMP0
		ADC BUFPTR
		STA BUFPTR
		LDA TEMP1
		ADC BUFPTR+1
		STA BUFPTR+1
		
		LDY #3
RDFD1		LDA (BUFPTR),Y
		STA FATDWORD0,Y		; Save FAT result in FATDWORD
		DEY
		BPL RDFD1
		
		RTS
		
;----------------------------------------------------------------------------
; This routine initializes the buffer-pointer
;----------------------------------------------------------------------------
INIT_BUFPTR	LDA #<CFBUF		; Bufptr points to start of directory
		STA BUFPTR
		LDA #>CFBUF
		STA BUFPTR+1
		RTS

;----------------------------------------------------------------------------
; This routine reads CF information and prints it.
;----------------------------------------------------------------------------
CFINFO		JSR CFWAITF	    	; Wait until CF-card ready
		LDA #$EC	    	; Drive ID command
		STA CFREG7
		JSR CFREAD_CBUF		; Read 512 bytes
		
		LDA #CR
		JSR COUT	    	; new-line

; Print serial number
		LDA #<TXT_SER     	; Print Serial text
		LDY #>TXT_SER
		JSR SPRINT	    	; print
		LDA #<(CFBUF+20)
		STA BUFPTR
		LDA #>(CFBUF+20)
		STA BUFPTR+1
		LDX #20			; len = 20
		JSR PRTRSN		; Print serial-number info
		LDA #CR
		JSR COUT	    	; new-line

; Print Firmware revision
		LDA #<TXT_FW     	; Print Firmware text
		LDY #>TXT_FW
		JSR SPRINT	    	; print
		LDA #<(CFBUF+46)
		STA BUFPTR
		LDA #>(CFBUF+46)
		STA BUFPTR+1
		LDX #8			; len = 8
		JSR PRTRSN		; Print firmware info
		LDA #CR
		JSR COUT	    	; new-line

; Print Model number
		LDA #<TXT_MOD     	; Print Model number text
		LDY #>TXT_MOD
		JSR SPRINT	    	; print
		LDA #<(CFBUF+54)
		STA BUFPTR
		LDA #>(CFBUF+54)
		STA BUFPTR+1
		LDX #40			; len = 40
		JSR PRTRSN		; Print firmware info
		LDA #CR
		JMP COUT	    	; new-line and return
	
;----------------------------------------------------------------------------
; This routine initializes the CF-card.
;----------------------------------------------------------------------------
CFINIT		LDA #$00		; Reset command
                STA CFREG8		; HW reset command
		LDA #1
		STA RSTACT		; 1 = Reset pending
		JSR CFWAIT
		BCC INITOK		; branch if CF-card init OK
		
		RTS			; return if error (C=1)
		
INITOK		LDA #$E0		; LBA3=0, Master, Mode=LBA
		STA CFREG6
		LDA #$01		; 8-bit transfers
		STA CFREG1
		LDA #$EF		; Set feature command
		STA CFREG7
		LDA #0
		STA RSTACT		; 0 = No Reset pending
		JMP CFWAIT		; Wait and return

;----------------------------------------------------------------------------
; This routine waits until the CF-card is ready.
;----------------------------------------------------------------------------
CFWAIT		LDA #0
		STA MSEC		; msec counter
		STA TOCNTR		; time-out counter
		STA NUMHI		; MSB of NUMOUT5 Input
CFWLP		LDA #10			; delay = 10 msec.
		JSR DELAY		; delay 10 msec.
		INC MSEC		; msec-counter
		;LDA MSEC
		BEQ CFWLP2		; branch after 2550 msec. and no reset
		
		LDA CFREG7		; read status register
		AND #$80		; check busy flag
		BNE CFWLP		; branch if BSY flag is still set
		
		; Busy flag cleared
		LDA CFREG7
		AND #$50		; check for RDY and DSC flags
		CMP #$50	
		BNE CFWLP		; branch if RDY and DSC not both set

		LDA RSTACT		; 1 = Reset pending
		BEQ PRENDC0		; branch if no Reset pending
		
		LDA #0
		STA RSTACT		; Reset no longer pending
		LDA #<TXT_RSTOK     	; Print Reset OK + msec
		LDY #>TXT_RSTOK
		JSR SPRINT	    	; print
		LDA MSEC		; #msec. * 10
		STA NUMLO		; LSB for NUMOUT5
		LDX #0
		STX LZSPC		; No leading zero/space
		JSR NUMOUT5		; Print decimal number
		LDA #<TXT_MSEC     	; Print msec
		LDY #>TXT_MSEC
		JSR SPRINT	    	; print
PRENDC0		CLC			; C=0, no error
		RTS			; return if BSY=0 and RDY=DSC=1
	
CFWLP2		LDA #<TXT_TO1     	; Print No Reset after 2550 msec
		LDY #>TXT_TO1
		JSR SPRINT	    	; print
		INC TOCNTR		; inc time-out counter
		LDA TOCNTR
		CMP #5			; approx. 1 second
		BEQ CFWERR		; branch if resets did not work
		
		JMP CFWLP		; branch always

CFWERR		LDA #<TXT_HWERR     	; Print HW error
		LDY #>TXT_HWERR
		JSR SPRINT	    	; print		
		LDA CFREG7		; Status-register
		JSR HEXOUT		; Print and return
		SEC			; C=1, error
		RTS			; return
		
;----------------------------------------------------------------------------
; This routine waits until the CF-card is ready.
;----------------------------------------------------------------------------
CFWAITF		LDA #0
		STA MSEC		; msec counter
CFWFLP1		INC MSEC		; msec-counter
		LDA MSEC
		BEQ CFWFLP2		; branch after 255 tries and no reset
		
		LDA CFREG7		; read status register
		AND #$80		; check busy flag
		BNE CFWFLP1		; branch if BSY flag is still set
		
		; Busy flag cleared
		LDA CFREG7
		AND #$50		; check for RDY and DSC flags
		CMP #$50	
		BNE CFWFLP1		; branch if RDY and DSC not both set

		CLC			; C=0, no error
		RTS			; return if BSY=0 and RDY=DSC=1
	
CFWFLP2		LDA #<TXT_BSY   	; Print error message
		LDY #>TXT_BSY
		JSR SPRINT
		SEC			; C=1, error
		RTS			; return

;----------------------------------------------------------------------------
; This routine checks for a CF-card error.
;----------------------------------------------------------------------------
CFCHERR		LDA CFREG7		; read status register
		AND #$01		; mask out error bit
		BEQ CFNERR		; branch if ERR bit is not set

		LDA #<TXT_ERR   	; Print error message
		LDY #>TXT_ERR
		JMP SPRINT		; Print and return
		
CFNERR		LDA #<TXT_NO_ERR   	; Print Init. OK
		LDY #>TXT_NO_ERR
		JMP SPRINT		; Print and return

;----------------------------------------------------------------------------
; This routine reads 512 data-bytes (1 sector) from the FAT into FAT_BUF.
;----------------------------------------------------------------------------
CFREAD_FAT	LDY #0			; init. page counter
		STY RSTACT		; 0 = No Reset pending
		LDA #<FAT_BUF
		STA BUFPTR
		LDA #>FAT_BUF
		STA BUFPTR+1
		LDX #1		 	; read 2 pages
		BNE CFREAD1		; branch always

;----------------------------------------------------------------------------
; This routine reads 512 data-bytes (1 sector) from the CF-card into CBUF.
;----------------------------------------------------------------------------
CFREAD_CBUF	LDY #0			; init. page counter
		STY RSTACT		; 0 = No Reset pending
		LDX #1		 	; read 2 pages
		JSR INIT_BUFPTR		; 512 byte buffer

;----------------------------------------------------------------------------
; This routine reads 512 data-bytes (1 sector) from the CF-card.
; Input: BUFPTR points to buffer where to store the bytes read from the CF-card.
;----------------------------------------------------------------------------
CFREAD1		TYA
		PHA			; Save Y
		TXA
		PHA			; Save X
		JSR CFWAITF	    	; Wait until CF-card ready
		PLA
		TAX			; Restore X
		PLA
		TAY			; Restore Y
		LDA CFREG7		; CF status-register
		AND #$08		; Filter out DRQ
		BEQ CFREADE		; branch if DRQ is no longer set

		LDA CFREG0		; read data-bytes
		STA (BUFPTR),Y		; store in buffer
		INY			; next byte
		BNE CFREAD1		; branch if more bytes to read
		
		INC BUFPTR+1		; Next page
		DEX			; next page
		BPL CFREAD1		; branch if 2nd page to read
CFREADE		LDA #<TXT_BREAD   	; Print #bytes read
		LDY #>TXT_BREAD
		JMP SPRINT		; Print and return

;----------------------------------------------------------------------------
; This routine writes 512 data-bytes (1 sector) from CFBUF to the CF-card.
;----------------------------------------------------------------------------
CFWRITE_CBUF	LDY #0			; init. page counter
		STY RSTACT		; 0 = No Reset pending
		LDX #1		 	; read 2 pages
		JSR INIT_BUFPTR		; 512 byte buffer

;----------------------------------------------------------------------------
; This routine writes 512 data-bytes (1 sector) to the CF-card.
; Input: BUFPTR points to buffer where to read the bytes from.
;----------------------------------------------------------------------------
CFWRITE1	TYA
		PHA			; Save Y
		TXA
		PHA			; Save X
		JSR CFWAITF	    	; Wait until CF-card ready
		PLA
		TAX			; Restore X
		PLA
		TAY			; Restore Y
		LDA CFREG7		; CF status-register
		AND #$08		; Filter out DRQ
		BEQ CFWRITE		; branch if DRQ is no longer set

		LDA (BUFPTR),Y		; store in buffer
		STA CFREG0		; write to CF-card
		INY			; next byte
		BNE CFWRITE1		; branch if more bytes to write
		
		INC BUFPTR+1		; Next page
		DEX			; next page
		BPL CFWRITE1		; branch if 2nd page to write
CFWRITE		LDA #<TXT_BWRITE   	; Print #bytes written
		LDY #>TXT_BWRITE
		JMP SPRINT		; Print and return

;----------------------------------------------------------------------------
; This routine prints 512 data-bytes from the CF-card
;----------------------------------------------------------------------------
CFPRINT		LDX #0			; init. #16-bytes counter
		LDY #0			; init. byte counter
		JSR INIT_BUFPTR		; Init. buffer pointer to CFBUF
		
CFPRLP1		LDA (BUFPTR),Y		; get byte
		JSR HEXOUT		; print as hex
		LDA #' '
		JSR COUTXY		; space between numbers
		INY
		CPY #16			
		BNE CFPRLP1		; branch if not 16 bytes yet
		
		LDA #CR			; new-line
		JSR COUTXY		
		LDA BUFPTR
		CLC
		ADC #16
		STA BUFPTR
		BCC CFPRLP2
		
		INC BUFPTR+1		; increment MSB of pointer
CFPRLP2		LDY #0			; reset byte-counter
		INX			; next 16-bytes
		CPX #32
		BNE CFPRLP1		; branch if not 32*16=512 bytes yet

		JMP INIT_BUFPTR		; Reset buffer-pointer to CFBUF and return
		
;----------------------------------------------------------------------------
; This routine sets the LBA for the CF-card
;----------------------------------------------------------------------------
CFSLBA		LDA CFLBA0	    	; LBA 0 (LSB)
		STA CFREG3
		LDA CFLBA1		; LBA 1
		STA CFREG4
		LDA CFLBA2		; LBA 2
		STA CFREG5
		LDA CFLBA3		; LBA 3 (MSB)
		AND #$0F		; Filter out LBA bits
		ORA #$E0		; Mode LBA, master dev
		STA CFREG6		; MSB
		RTS

;----------------------------------------------------------------------------
; This routine inits the LBA for the CF-card and sets it to 0 (the MBR)
;----------------------------------------------------------------------------
INIT_LBA	LDA #0
		STA CFLBA0	    	; LBA 0 (LSB)
		STA CFLBA1
		STA CFLBA2
		STA CFLBA3		; LBA 3 (MSB)
		RTS

;----------------------------------------------------------------------------
; This routine inits the LBA for the CF-card and sets it to 0 (the MBR)
;----------------------------------------------------------------------------
INIT_P0_LBA	LDA #0
		STA PART00	    	; LBA 0 (LSB)
		STA PART01
		STA PART02
		STA PART03		; LBA 3 (MSB)
		STA CLUSLBA0		; Cluster begin LBA 0
		STA CLUSLBA1		; Cluster begin LBA 1
		STA CLUSLBA2		; Cluster begin LBA 2
		STA CLUSLBA3		; Cluster begin LBA 3
		RTS

;----------------------------------------------------------------------------
; This routine prints the info from the MBR (sector 0)
;----------------------------------------------------------------------------
MBR_PRINT	LDA #<TXT_55AA   	; Print #bytes read
		LDY #>TXT_55AA
		JSR SPRINT
		LDA CFBUF+$1FE		; Should be $55
		CMP #$55
		BNE MPR_ERR
		LDA CFBUF+$1FF		; Should be $AA
		CMP #$AA
		BNE MPR_ERR
		
		JSR PR_OK		; Print OK
		JMP MBR2		; Next check

MPR_ERR		JMP MBR_PR_ERR		; Print error and return

MBR2		LDA #<TXT_PART0		; Partition 0 text
		LDY #>TXT_PART0
		JSR SPRINT
		LDA CFBUF+$1BE		; Partition 0 boot-flag
		CMP #$80
		BEQ MBR3		; branch if bootable
		
		JSR PR_NO		; Print No (Error)
		JMP MBR3_1		; branch always

MBR3		JSR PR_OK		; Print Yes (OK)
MBR3_1		LDA #<TXT_TYPE		; Partition type-code
		LDY #>TXT_TYPE
		JSR SPRINT
		LDA CFBUF+$1C2		; type-code
		CMP #$0B
		BEQ MBR4		; branch if FAT32 code found
		
		CMP #$0C
		BEQ MBR4		; branch if FAT32 code found

		JSR PR_NO		; Print No (Error)
		JMP MBR4_1		; branch always
		
MBR4		JSR PR_OK		; Print OK
MBR4_1		LDA #<TXT_6502		; Partition ID code found ($65 $02)?
		LDY #>TXT_6502
		JSR SPRINT		; Print
		LDA CFBUF+$1BC		; Should be $65 for a JC2 bootable disc
		CMP #$65		
		BNE MBR5		; branch if not present
		
		LDA CFBUF+$1BD		; Should be $02 for a JC2 bootable disc
		CMP #$02
		BNE MBR5		; branch if not present
		
		JSR PR_OK		; Print Yes (OK)
		JMP MBR5_1

MBR5		JSR PR_NO		; Print No (Error)
MBR5_1		LDA #<TXT_LBABGN
		LDY #>TXT_LBABGN
		JSR SPRINT
		LDA CFBUF+$1C9		; Print Partition 0 LBA begin sector
		STA PART03		; Partition 0 LBA MSB
		STA WORK4		; Needed for printing #MB
		STA FATLBA3		; Needed for calc. of FAT LBA
MBR40		LDA CFBUF+$1C8		
		STA PART02
		STA WORK3
		STA FATLBA2
MBR41		LDA CFBUF+$1C7
		STA PART01
		STA WORK2
		STA FATLBA1
MBR42		LDA CFBUF+$1C6
		STA PART00
		STA WORK1		; LSB
		STA FATLBA0
		JSR PR32HEXNOLZ		; Print 32-bit hex number in WORK
		LDA #<TXT_LBASEC
		LDY #>TXT_LBASEC
		JSR SPRINT

		LDA CFBUF+$1CD		; Print LBA #sectors MSB
		STA WORK4		; MSB
		LDA CFBUF+$1CC
		STA WORK3
		LDA CFBUF+$1CB
		STA WORK2
		LDA CFBUF+$1CA
		STA WORK1		; LSB
		JSR PR32HEXNOLZ		; Print 32-bit hex number
		
		LDA #' '
		JSR COUT
		LDA #'('
		JSR COUT
		LDX #11			; #sectors/2/1024 = # MB
SHR11		CLC
		ROR WORK4
		ROR WORK3
		ROR WORK2
		ROR WORK1
		DEX
		BNE SHR11		; branch if not done shifting yet
		LDA WORK2
		STA NUMHI		; MSB of #MB
		LDA WORK1
		STA NUMLO		; LSB OF #MB
		LDX #0
		STX LZSPC		; No leading zero/space
		JSR NUMOUT5		; Print #MB as a decimal Number
		LDA #<TXT_MB		; Print ' MB)'
		LDY #>TXT_MB
		JMP SPRINT		; Print and return
		
;----------------------------------------------------------------------------
; This routine prints the info from the Partition 0 Volume ID Sector
;----------------------------------------------------------------------------
VOL_ID_PRINT	LDA #<TXT_VOL_ID   	; Print Volume ID Title
		LDY #>TXT_VOL_ID
		JSR SPRINT
		LDA #<TXT_55AA   	; Print #bytes read
		LDY #>TXT_55AA
		JSR SPRINT
		LDA CFBUF+$1FE		; Should be $55
		CMP #$55
		BNE VOL_ID_ERR
		LDA CFBUF+$1FF		; Should be $AA
		CMP #$AA
		BNE VOL_ID_ERR
		
		JSR PR_OK		; Print OK
		JMP VOL_ID2		; Next check

VOL_ID_ERR	JMP VOLID_PR_ERR	; Print error and return

VOL_ID2		LDA #<TXT_VOL_ID1   	; Print #Bytes/Sector
		LDY #>TXT_VOL_ID1
		JSR SPRINT
		LDA CFBUF+11		; Bytes/Sector LSB
		STA NUMLO
		LDA CFBUF+12		; Bytes/Sector MSB
		STA NUMHI
		LDX #0
		STX LZSPC		; No leading zero/space
		JSR NUMOUT5		; Print in decimal bytes
		LDA #<TXT_VOL2   	; Print Must be 512
		LDY #>TXT_VOL2
		JSR SPRINT
		LDA #0
		STA NUMHI		; MSB = 0
		LDA CFBUF+13		; Sectors/Cluster
		STA NUMLO		; LSB of #sectors/Cluster
		STA SECT_CLUST		; Save value
		JSR NUMOUT5	
		LDA #<TXT_VOL3   	; Print #Reserved Sectors
		LDY #>TXT_VOL3
		JSR SPRINT
		LDA CFBUF+14		; #Reserved Sectors LSB
		STA NUMLO
		LDA CFBUF+15		; #Reserved Sectors MSB
		STA NUMHI
		JSR NUMOUT5		; Print as decimal number
		LDA #<TXT_VOL4   	; Print #FATs
		LDY #>TXT_VOL4
		JSR SPRINT
		LDA #0
		STA NUMHI		; MSB of #FATs
		LDA CFBUF+16		; #FATs
		STA NUMLO		; LSB of #FATs
		JSR NUMOUT5	
		LDA #<TXT_VOL5   	; #Sectors per FAT
		LDY #>TXT_VOL5
		JSR SPRINT
		LDA CFBUF+39
		STA WORK4		; MSB
		STA SECT_PER_FAT+3	; Save in SECT_PER_FAT
		LDA CFBUF+38
		STA WORK3
		STA SECT_PER_FAT+2
		LDA CFBUF+37
		STA WORK2
		STA SECT_PER_FAT+2
		LDA CFBUF+36
		STA WORK1
		STA SECT_PER_FAT
		JSR PR32HEXNOLZ		; Print 32-bit hex number
		LDA #<TXT_VOL6   	; Root Dir first Cluster
		LDY #>TXT_VOL6
		JSR SPRINT
		LDA CFBUF+47
		STA WORK4		; MSB
		LDA CFBUF+46
		STA WORK3
		LDA CFBUF+45
		STA WORK2
		LDA CFBUF+44
		STA WORK1
		JSR PR32HEXNOLZ		; Print 32-bit hex number
		
		; FAT Begin LBA = Partition begin (32-bit) + #reserved_sectors (16 bit)
		CLC
		LDA	FATLBA0		; LSB
		ADC	CFBUF+14	; #reserved sectors LSB
		STA	FATLBA0
		STA	WORK1		; Needed for printing
		LDA	FATLBA1
		ADC	CFBUF+15	; #reserved sectors MSB
		STA	FATLBA1
		STA	WORK2		; 
		LDA	FATLBA2
		ADC	#0
		STA	FATLBA2
		STA	WORK3
		LDA	FATLBA3		; MSB
		ADC	#0
		STA	FATLBA3
		STA	WORK4
		LDA 	#<TXT_VOL7   	; FAT begin LBA
		LDY 	#>TXT_VOL7
		JSR 	SPRINT
		JSR 	PR32HEXNOLZ	; Print 32-bit hex number

		; Cluster_begin_lba = fat_begin_lba + #FATs * Sectors_per_fat
		; #FATs is always 2, so SHL1 sectors_per_fat and add to Cluster_begin_lba
		LDA CFBUF+39		; #sectors per FAT
		STA WORK4		; MSB
		LDA CFBUF+38
		STA WORK3
		LDA CFBUF+37
		STA WORK2
		LDA CFBUF+36
		STA WORK1		; LSB

		CLC
		ROL WORK1
		ROL WORK2
		ROL WORK3
		ROL WORK4		; Work = 2 * #sectors_per_FAT
		CLC
		LDA WORK1
		ADC FATLBA0		; LSB of #sectors per FAT
		STA CLUSLBA0
		STA WORK1		; Needed for printing
		LDA WORK2
		ADC FATLBA1
		STA CLUSLBA1
		STA WORK2		; Needed for printing
		LDA WORK3
		ADC FATLBA2
		STA CLUSLBA2
		STA WORK3		; Needed for printing
		LDA WORK4
		ADC FATLBA3
		STA CLUSLBA3
		STA WORK4		; Needed for printing
		LDA #<TXT_VOL8   	; Cluster begin LBA
		LDY #>TXT_VOL8
		JSR SPRINT
		JSR PR32HEXNOLZ		; Print 32-bit hex number
		
		LDA #CR
		JSR COUT		; New-line
		RTS
		
;----------------------------------------------------------------------------
; This routine print a message YES (OK) to the screen
;----------------------------------------------------------------------------
PR_OK		LDA #<TXT_YES   	; Print OK
		LDY #>TXT_YES
		JMP SPRINT		; print and return

;----------------------------------------------------------------------------
; This routine print a message NO (Error) to the screen
;----------------------------------------------------------------------------
PR_NO		LDA #<TXT_NO   		; Print No (Error)
		LDY #>TXT_NO
		JMP SPRINT		; print and return

;----------------------------------------------------------------------------
; This routine print an error message to the screen
;----------------------------------------------------------------------------
MBR_PR_ERR	LDA #<TXT_NOMBR		; Print NO, not a MBR
		LDY #>TXT_NOMBR
		JMP SPRINT		; print and return

;----------------------------------------------------------------------------
; This routine print an error message to the screen
;----------------------------------------------------------------------------
VOLID_PR_ERR	LDA #<TXT_NOVOLID	; Print NO, not a Volume ID
		LDY #>TXT_NOVOLID
		JMP SPRINT		; print and return

;----------------------------------------------------------------------------
; This routine print a Big-Endian string of N characters:
; BUFPTR: points to begin of buffer to print
; X     : #bytes to print
;----------------------------------------------------------------------------
PRTRN		LDY #1			; start at MSB of 1st word
PRNLP1		LDA (BUFPTR),Y		; get MSB	
		JSR COUTXY		; output MSB to screen
		DEY			; LSB now
		DEX			; #bytes to print
		LDA (BUFPTR),Y		
		JSR COUTXY		; output LSB to screen
		INY
		INY			; points to next LSB
		INY			; points to next MSB
		DEX			; #bytes to print
		BNE PRNLP1		; branch if more to print
		
		RTS

;----------------------------------------------------------------------------
; This routine print a Big-Endian string of N characters, skipping all spaces.
; BUFPTR: points to begin of buffer to print
; X     : #bytes to print
;----------------------------------------------------------------------------
PRTRSN		LDY #1			; start at MSB
PRTRSN1		LDA (BUFPTR),Y		; get MSB 		
		CMP #' '		; skip if space
		BEQ PRSNLP2		; branch if space

		JSR COUTXY		; output MSB to screen
PRSNLP2		DEY			; LSB now
		DEX			; #bytes to print
		LDA (BUFPTR),Y		
		CMP #' '		; skip if space
		BEQ PRSNLP3		; branch if space

		JSR COUTXY		; output LSB to screen
PRSNLP3		INY			; 
		INY			; points to next LSB
		INY			; points to next MSB
		DEX			; #bytes to print
		BNE PRTRSN1		; branch if more to print
		
		RTS			; return

;----------------------------------------------------------------------------
; This routine calls COUT while preserving the values of X and Y
;----------------------------------------------------------------------------
COUTXY		STX SAVEX
		STY SAVEY
		JSR COUT
		LDY SAVEY
		LDX SAVEX
		RTS

SAVEX		.byte 0
SAVEY		.byte 0

;----------------------------------------------------------------------------
; This routine converts a hex number in ASCII to a number
;----------------------------------------------------------------------------
ASC2HEX		AND #$DF		; convert to upper case
		CMP #'A'
		BCC A2H09		; branch if '0'..'9'

		CMP #'G'
		BCC A2HAF		; branch if 'A'..'F'

		LDA #0			; Error
		RTS			; return

; '0'..'9', convert to 0..9
A2H09		SEC			; A = A -'0'
		SBC #'0'
		RTS			; return

; 'A'..'F', convert to 10..15
A2HAF		SEC			; A = A + 10 - 'A'
		SBC #'A'
		CLC
		ADC #10
		RTS			; 

; **** Print A DWORD (4 bytes) As a HEX number with no leading Zeros ***********
; Input: WORK1 (LSB)..WORK4 (MSB)
; ******************************************************************************
PR32HEXNOLZ	LDA 	#0
		STA	PRFLAG
		
		; Print WORK4
		LDA 	WORK4
		BEQ	PRH32_3
		
		JSR	HEXOUT
		INC	PRFLAG
		
		; Print WORK3
PRH32_3	        LDA	WORK3
		BNE	PRH32_31
		
		LDX	PRFLAG
		BEQ	PRH32_2

PRH32_31	JSR	HEXOUT
		INC	PRFLAG
		
		; Print WORK2
PRH32_2		LDA	WORK2
		BNE	PRH32_21

		LDX	PRFLAG
		BEQ	PRH32_1
		
PRH32_21	JSR	HEXOUT

		; Print WORK1, always print LSB
PRH32_1		LDA	WORK1
		JMP	HEXOUT		; print and return

; **** Print A Word (2 bytes) as a Decimal Number ******************************
; Input : NUMHI, NUMLO: number 0000..FFFF (0..65535)
; Output: DIG4..DIG0 
; ******************************************************************************
NUMOUT5		LDX 	#'0'
		STX	DIG0		; 10^0
		STX	DIG1		; 10^1
		STX	DIG2		; 10^2
		STX	DIG3		; 10^3
		STX	DIG4		; 10^4
		LDX	#1		; At least 1 number
		STX	DIGCNT		; Number of non-0 digits
GETDIG4		LDA	NUMHI
		CMP	#>10000		; 10.000 = $2710
		BCC	GETDIG3		; branch if < 10.000
		BNE	GE10K		; branch if > 10.000
		LDA 	NUMLO
		CMP	#<10000
		BCC	GETDIG3		; branch if < 10.000
		; Number is >= 10.000
GE10K		LDA 	NUMLO
		SEC			; NUM = NUM - 10.000
		SBC	#<10000
		STA	NUMLO
		LDA	NUMHI
		SBC	#>10000
		STA	NUMHI
		INC	DIG4		
		JMP	GETDIG4
		
		; Number is < 10.000
GETDIG3		LDA	NUMHI
		CMP	#>1000		; 1.000 = $03E8
		BCC	GETDIG2		; branch if < 1.000
		BNE	GE1K		; branch if > 1.000
		LDA 	NUMLO
		CMP	#<1000
		BCC	GETDIG2
		; Number is >= 1.000
GE1K		LDA 	NUMLO
		SEC			; NUM = NUM - 1.000
		SBC	#<1000
		STA	NUMLO
		LDA	NUMHI
		SBC	#>1000
		STA	NUMHI
		INC	DIG3		
		JMP	GETDIG3

		; Number is < 1.000
GETDIG2		LDA	NUMHI
		BNE	GE100		; branch if > 100
		LDA 	NUMLO
		CMP	#<100
		BCC	GETDIG1		; branch if < 100
		; Number is >= 100
GE100		LDA 	NUMLO
		SEC			; NUM = NUM - 1.000
		SBC	#<100
		STA	NUMLO
		LDA	NUMHI
		SBC	#>100
		STA	NUMHI
		INC	DIG2		
		JMP	GETDIG2

		; Number is < 100
GETDIG1		LDA 	NUMLO
		CMP	#<10
		BCC	GETDIG0
		; Number is >= 10
GE10		LDA 	NUMLO
		SEC			; NUM = NUM - 10
		SBC	#<10
		STA	NUMLO
		INC	DIG1		
		JMP	GETDIG1
		
GETDIG0		LDA	NUMLO
		CLC
		ADC	#'0'
		STA	DIG0

		; Count the number of non-0 digits
		LDA	DIG4
		CMP	#'0'
		BEQ	TD3
		
		LDA	#5
		STA	DIGCNT
		BNE	TLSPZ		; branch always
		
TD3		LDA 	DIG3
		CMP 	#'0'
		BEQ	TD2
		
		LDA	#4
		STA	DIGCNT
		BNE	TLSPZ		; branch always

TD2		LDA 	DIG2
		CMP 	#'0'
		BEQ	TD1
		
		LDA	#3
		STA	DIGCNT
		BNE	TLSPZ		; branch always

TD1		LDA 	DIG1
		CMP 	#'0'
		BEQ	TLSPZ		
		
		LDA	#2
		STA	DIGCNT

		; Test Leading spaces / zeros
TLSPZ		LDA	LZSPC		; Leading 0 or spaces?
		BEQ	PRD50		; No, print digits directly

		AND	#1		; Bit 0 = leading space
		BNE	PRLSPC		; branch if leading space

		LDA	#'0'		; Leading Zero
		BNE	PRLZSPC
		
PRLSPC		LDA	#' '		; Leading space
PRLZSPC		LDX	DIGCNT
		CPX	#5
		BEQ	PRD50		; Exit print leading 0/space

		PHA			; Save leading zero/space
		JSR	COUT		; Print leading zero/space
		INC	DIGCNT
		PLA
		BNE	PRLZSPC		; branch always
		
		; Now print everything
PRD50		LDA	#0
		STA 	PRFLAG		; 0 = Nothing printed yet
		
		; Print DIG4
		LDA	DIG4
		CMP	#'0'
		BEQ	PRDIG3
		
		JSR	COUT		; print DIG4
		INC	PRFLAG		; a non-LZ number is printed

		; Print DIG3
PRDIG3		LDA	DIG3
		CMP	#'0'
		BNE	DIG3PR
		
		LDX	PRFLAG
		BEQ	PRDIG2
		
DIG3PR		JSR	COUT
		INC	PRFLAG

		; Print DIG2
PRDIG2		LDA	DIG2
		CMP	#'0'
		BNE	DIG2PR
		
		LDX	PRFLAG
		BEQ	PRDIG1
		
DIG2PR		JSR	COUT
		INC	PRFLAG

		; Print DIG1
PRDIG1		LDA	DIG1
		CMP	#'0'
		BNE	DIG1PR
		
		LDX	PRFLAG
		BEQ	PRDIG0
		
DIG1PR		JSR	COUT
		INC	PRFLAG

		; Print DIG0 (always print)
PRDIG0		LDA	DIG0
		JMP	COUT		; print and return
		
;----------------------------------------------------------------------------
; This routine adds a 16-bit number (NUMLO, NUMHI) to a 32-bit number (WORK1,WORK4).
;----------------------------------------------------------------------------
ADD32_16	CLC
		LDA	WORK1		; LSB
		ADC	NUMLO
		STA	WORK1
		LDA	WORK2
		ADC	NUMHI
		STA	WORK2
		LDA	WORK3
		ADC	#0
		STA	WORK3
		LDA	WORK4		; MSB
		ADC	#0
		STA	WORK4
		RTS
		
; **** Print A Byte As Decimal Number with a Leading Zero **********************
; Input: A - number 00..63 (0..99)
; ******************************************************************************
NUMOUT2		JSR	DEC2STR
	        LDA 	DIG1
		JSR	BOUT
NU20:		LDA	DIG0
		JMP	BOUT		; print and return

; ******************************************************************************
; NUM32 shl X
; ******************************************************************************
SHL_32          ASL     NUM32
                ROL     NUM32+1
                ROL     NUM32+2
                ROL     NUM32+3
                DEX
                BNE     SHL_32
                RTS

; ******************************************************************************
; NUM32 shr X
; ******************************************************************************
SHR_32          LSR     NUM32+3
                ROR     NUM32+2
                ROR     NUM32+1
                ROR     NUM32
                DEX
                BNE     SHR_32
                RTS

; ******************************************************************************
; A shl N
; ******************************************************************************
SHL_A_5         ASL     
SHL_A_4         ASL     
SHL_A_3         ASL     
                ASL     
                ASL     
                RTS

; **** Convert a Byte To Decimal String ****************************************
; Input:  A - number 00..FF (0..255)
; Output; DIG0 (10^0), DIG1 (10^1), DIG2 (10^2)
; ******************************************************************************
DEC2STR		LDX	#48
		STX	DIG0		; initialize digit counter 0 to '0'
		STX	DIG1		; initialize digit counter 1 to '0'
		STX	DIG2		; initialize digit counter 2 to '0'
DECDIG2		CMP	#100		; is A >= 100?
		BCC	DECDIG1		; no, convert next digit
		
		SBC	#100		; yes, subract 100 from A
		INC	DIG2		; and increment digit counter 2
		BNE	DECDIG2		; branch always
		
DECDIG1		CMP	#10		; is A >= 10?
		BCC	DECDIG0		; no, convert next digit
		SBC	#10		; yes, subract 10 from A
		INC	DIG1		; and increment digit counter 1
		BNE	DECDIG1		; branch always
		
DECDIG0		ADC	DIG0		; add digit counter 0 to remainder in A
		STA	DIG0		; and store it back to digit counter 0
		RTS

; **** Get Current Time As File Time *******************************************
; Output: File Time = Word[X,Y]
; ******************************************************************************
OS_FILETIME     JSR     READTIME            ; read current time

; **** Convert Time To File Time ***********************************************
; Input:  A - HOUR 	in BCD ($00-$23)
;	  X - MINUTE 	in BCD ($00-$59)
;	  Y - SECONDS	in BCD ($00-$59)
; Output: File Time = Word[X,Y]
; ******************************************************************************
OS_TIME_TO_FILETIME
                JSR     CONVERT_DATETIME    ; convert BCD date values into binary
                STA     NUM32               ; store HOUR into lower byte of Word[NUM32:NUM32+1]
                LDA     #$00
                STA     NUM32+1             ; clear upper byte of result
                LDX     #$06
                JSR     SHL_32              ; shift left NUM32 by 6 bits
                LDA     SUM32+1             ; load MINUTE into A
                ORA     NUM32               ; and add value into result
                STA     NUM32
                LDX     #$05
                JSR     SHL_32              ; shift left NUM32 by 5 bits
                LDA     SUM32+2             ; load SECONDS into A
                ROR                         ; divide SECONDS by 2
                ORA     NUM32               ; and add value into result
                TAX
                LDY     NUM32+1             ; result is in Word[X,Y]
                RTS
                
; **** Get Current Date As File Date *******************************************
; Output: File Date = Word[X,Y]
; ******************************************************************************
OS_FILEDATE     JSR     READDATE            ; read current date

; **** Convert Date To File Date ***********************************************
; Input:  A - YEAR 	in BCD ($00-$99)
; 	  X - MONTH 	in BCD ($01-$12)
; 	  Y - DAY	in BCD ($01-$31)
; Output: File Date = Word[X,Y]
; ******************************************************************************
OS_DATE_TO_FILEDATE
                JSR     CONVERT_DATETIME    ; converte BCD date values into binary
                CLC                         ; file date starts from 1980, so we have
                ADC     #20                 ; to add 20 to our year 2000 based RTC date
                STA     NUM32               ; store YEAR into lower byte of Word[NUM32:NUM32+1]
                LDA     #$00
                STA     NUM32+1             ; clear upper byte of result
                LDX     #$04
                JSR     SHL_32              ; shift left NUM32 by 4 bits
                LDA     SUM32+1             ; load MONTH into A
                ORA     NUM32               ; and add value into result
                STA     NUM32
                LDX     #$05
                JSR     SHL_32              ; shift left NUM32 by 5 bits
                LDA     SUM32+2             ; load DAY into A
                ORA     NUM32               ; and add value into result
                TAX
                LDY     NUM32+1             ; result is in Word[X,Y]
                RTS
                
; **** Convert BCD Date Or Time Values Into Unpacked Binary ********************
CONVERT_DATETIME
                STA     SUM32		    ;
                STX     SUM32+1
                STY     SUM32+2
                LDY     #$02
CONVERT_BCD     LDA     SUM32,Y
                JSR     BCD_TO_BIN
                STA     SUM32,Y
                DEY
                BPL     CONVERT_BCD
                RTS

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
; This routine prints a string to the terminal: A=LSB, Y=MSB
;----------------------------------------------------------------------------
SPRINT		STA PSTR	    	; LSB of text-pointer
		STY PSTR+1	    	; MSB of text-pointer
		JMP STROUT	    	; BIOS print string routine
		
LFN_TBL		.byte 1,3,5,7,9,14,16,18,20,22,24,28,30
LFN_TBL_SZ	.byte LFN_TBL_SZ - LFN_TBL

;----------------------------------------------------------------------------
; This block contains all text-lines to be printed to the screen.
;----------------------------------------------------------------------------
TXT_TITLE	.by     'JC ][ Compact-Flash IDE Tester v0.10 by Emile' CR
		.by     'I:Info H:HW-Reset M:MBR-Read R:Read-Sector Q:Quit' CR 
		.by	'C:CH-DIR D:Read-DIR K:Make-DIR' CR $00
TXT_ERR		.by	'CF-card ERR bit is set' CR $00
TXT_BSY		.by	'CF-card BSY bit is still set' CR $00
TXT_NO_ERR	.by	'CF-card OK' CR $00
TXT_SER		.by     '  Serial: ' $00
TXT_FW		.by     'Firmware: ' $00
TXT_MOD		.by     '   Model: ' $00
TXT_LBA		.by     'LBA Size: ' $00
TXT_LBA_NR	.by     'LBA Sector-number: $' $00
TXT_LBA_INP	.by	'Enter LBA sector number in hex: $' $00
TXT_TO1         .by     'Reset Time-out, trying another reset' CR $00
TXT_HWRST       .by     'HW Reset applied' CR $00
TXT_RSTOK       .by     'CF-Card Reset after ' $00
TXT_MSEC	.by	'0 msec.' CR $00
TXT_BREAD	.by	'512 bytes read, ' $00
TXT_BWRITE	.by	'512 bytes written' CR $00
TXT_HWERR       .by     'Could not Reset CF-card, Status=$' $00
TXT_55AA	.by     CR 'Last 2 bytes are $55 and $AA? ' $00
TXT_YES		.by	'Yes (OK)' CR $00
TXT_NO		.by	'No (Error)' CR $00
TXT_NOMBR	.by	'No, not a MBR' CR $00
TXT_NOVOLID	.by	'No, not a Volume ID' CR $00
TXT_PART0	.by	'Partition 0 Boot-Flag ($80) set? ' $00
TXT_TYPE	.by	'Partition 0 Type-code is FAT32 ($0B or $0C)? ' $00
TXT_6502	.by	'Partition ID code found-code ($65 $02)? ' $00
TXT_LBABGN	.by	'Partition 0 LBA begin-sector is $' $00
TXT_LBASEC	.by	CR 'Partition 0 #Sectors is $' $00
TXT_MB		.by	' MB)' CR $00
TXT_VOL_ID	.by	CR 'Reading Partition 0 Volume ID Sector:' CR $00
TXT_VOL_ID1	.by	'Number of Bytes per sector: ' $00
TXT_VOL2	.by	' bytes, must be 512' CR 
		.by	'Sectors per Cluster       : ' $00
TXT_VOL3	.by	', is 1,2,4,8,16,32 or 128' CR
                .by     'Number of Reserved Sectors: ' $00 
TXT_VOL4	.by	', is usually 32' CR
                .by     'Number of FATs            : ' $00 
TXT_VOL5	.by	', always 2' CR
                .by     'Number of Sectors per FAT : $' $00 
TXT_VOL6	.by	CR 'Root-dir First Cluster    : $' $00 
TXT_VOL7	.by	CR 'FAT Begin LBA             : $' $00 
TXT_VOL8	.by	CR 'Cluster Begin LBA         : $' $00 
TXT_NO_DIR	.by	'Error: No MBR and Volume ID read' CR $00 
TXT_VOL		.by 	'Volume Label: ' $00
TXT_DIR		.by	'<DIR> ' $00
TXT_CHDIR	.by	'Enter Directory Name: ' $00
TXT_PATHNFND	.by     'Path not found' CR $00
TXT_PATHFND	.by     'A subdirectory or file already exists' CR $00
TXT_FATFREE	.by	'First free cluster at $' $00
TXT_NOFREEDIR	.by 	'Error: No free entry in current directory' CR $00
LFN_BUF		.ds   255	; Buffer for long filenames
			
CF_TEST_END = *
