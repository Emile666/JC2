;---------------------------------------------------------------------------
; JC2 CF-IDE Device Driver & MMU related functions.
; This program adds the necessary files for the CF-IDE Interface and the
; MMU related routines. 
;---------------------------------------------------------------------------

;----------------------------------------------------------------------------
; This routine reads CF information and prints it.
;----------------------------------------------------------------------------
CF_INFO		JSR CFWAIT	    	; Wait until CF-card ready
		LDA #$EC	    	; Drive ID command
		STA CFREG7		; CF command register
		JSR INIT_BLKBUF		; Init. buffer-pointer
		JSR CF_RD_INFO		; Read 512 bytes (= CF_RD_BLK without the Read 1 sector commands)

; Print serial number
		LDX #<TXT_SER     	; Print Serial text
		LDY #>TXT_SER
		JSR SPRINT	    	; print
		LDA #<(BLOCK_BUF+20)
		STA BLKBUF
		LDA #>(BLOCK_BUF+20)
		STA BLKBUF+1
		LDX #20			; len = 20
		JSR PRTRSN		; Print serial-number info

; Print Firmware revision
		LDX #<TXT_FW     	; Print Firmware text
		LDY #>TXT_FW
		JSR SPRINT	    	; print
		LDA #<(BLOCK_BUF+46)
		STA BLKBUF
		LDA #>(BLOCK_BUF+46)
		STA BLKBUF+1
		LDX #8			; len = 8
		JSR PRTRSN		; Print firmware info

; Print Model number
		LDX #<TXT_MOD     	; Print Model number text
		LDY #>TXT_MOD
		JSR SPRINT	    	; print
		LDA #<(BLOCK_BUF+54)
		STA BLKBUF
		LDA #>(BLOCK_BUF+54)
		STA BLKBUF+1
		LDX #40			; len = 40
		JSR PRTRSN		; Print firmware info
		JSR CROUT		; Print CR
		SEC			; C=1 (no error)
		RTS

;----------------------------------------------------------------------------
; This routine print a Big-Endian string of N characters, skipping all spaces.
; BUFPTR: points to begin of buffer to print
; X     : #bytes to print
;----------------------------------------------------------------------------
PRTRSN		LDY #1			; start at MSB
PRTRSN1		LDA (BLKBUF),Y		; get MSB 		
		CMP #' '		; skip if space
		BEQ PRSNLP2		; branch if space

		JSR COUTXY		; output MSB to screen
PRSNLP2		DEY			; LSB now
		DEX			; #bytes to print
		LDA (BLKBUF),Y		
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
COUTXY		STX SAVEX		; Save X	
		STY SAVEY		; Save Y
		JSR COUT		; Print char.
		LDY SAVEY		; Get Y back
		LDX SAVEX		; Get X back
		RTS			; return

;----------------------------------------------------------------------------
; This routine sets the LBA for the CF-card to 0 (the MBR) and loads it
; into the CF-card.
;----------------------------------------------------------------------------
INIT_LBA	LDA #0
		STA CFLBA0	    	; LBA 0 (LSB)
		STA CFLBA1
		STA CFLBA2
		STA CFLBA3		; LBA 3 (MSB)
		JMP LOAD_CFLBA		; Load into CF-card and return

;----------------------------------------------------------------------------
; This routine loads the Logical Block Address (LBA) into the CF-card.
; Input :  X,Y = Ptr[LO:HI] to 32 Bit LBA Address
; Output: LBA in CFLBA3..CFLBA0
;----------------------------------------------------------------------------
LOAD_LBA_CF	STX PLBA		; Store pointer
		STY PLBA+1
		LDY #0
		LDA (PLBA),Y		; LBA 0 (LSB)
		STA CFLBA0
		INY
		LDA (PLBA),Y		; LBA 1
		STA CFLBA1
		INY
		LDA (PLBA),Y		; LBA 2
		STA CFLBA2
		INY
		LDA (PLBA),Y		; LBA 3 (MSB)
		STA CFLBA3
		;JMP LOAD_CFLBA		; Load into CF-card and return

;----------------------------------------------------------------------------
; This routine loads the Logical Block Address (LBA) into the CF-card.
; Input : CFLBA3..CFLBA0
; Output: -
;----------------------------------------------------------------------------
LOAD_CFLBA	LDA CFLBA0		; CFLBA0 -> CFREG3
		STA CFREG3		; 
		LDA CFLBA1		; CFLBA1 -> CFREG4
		STA CFREG4
		LDA CFLBA2		; CFLBA2 -> CFREG5
		STA CFREG5
		LDA CFLBA3		; CFLBA3 -> CFREG6
		AND #$0F		; Filter out LBA bits
		ORA #$E0		; Mode LBA, master dev
		STA CFREG6		; Store in CFREG6
		RTS
		
;----------------------------------------------------------------------------
; Command: None, Read Single Data Block to Std. Block Buffer
; Input  : CFLBA3..CFLBA0 = 32 Bit Command Block Source Address. 
;          NOTE: These have to loaded prior to calling this function!!!
; Output : C = 0 Error, C = 1 Read OK
;	   A = Error Code
;----------------------------------------------------------------------------
CF_RD_BLK_BUF	JSR	INIT_BLKBUF		; set pointer to block buffer
		BEQ	CF_RD_BLK		; branch always

;----------------------------------------------------------------------------
; Command: CMD_READ_BUF, Read Single Data Block from Logical Address to Std. Block Buffer
; Input  :  X,Y = Ptr[LO:HI] to 32 Bit LBA Source Address
; Output :  C   = 0 Error, C = 1 Data OK
;	    A   = Error Code
;----------------------------------------------------------------------------
CF_RD_LBLK_BUF	JSR	INIT_BLKBUF		; set pointer to block buffer
						; fall through to CF_RD_LBLK

;----------------------------------------------------------------------------
; Command: CMD_READ, Read Single Data Block from Logical Address
; Input  : X,Y = Ptr[LO:HI] to 32 Bit LBA Source Address
;	   BLKBUF,BLKBUFH = 16 Bit Destination Address
; Output : C = 0 Error, C = 1 Data OK
;	   A = Error Code
;----------------------------------------------------------------------------
CF_RD_LBLK	JSR	LOAD_LBA_CF		; Load LBA into CF-card
						; fall through to CF_RD_BLK

;----------------------------------------------------------------------------
; Read Single Data Block
; Input:  CFLBA3..CFLBA0 = 32 Bit LBA Address
;         BLKBUF,BLKBUFH = 16 Bit Destination Address
; Output: C = 0 Error, C = 1 Read OK
;	  A = Error Code
;----------------------------------------------------------------------------
CF_RD_BLK	LDA 	#$01
		STA 	CFREG2			; Read one Sector
		JSR 	CFWAIT			; Wait until CF-card ready
		BCC 	CF_RD_END		; branch on error

		LDA 	#$20			; Read Sector Command
		STA 	CFREG7			; CF command register
CF_RD_INFO	LDX	#$01			; initialize page counter
		LDY	#$00			; initialize byte counter
CF_RD_BLK0	JSR	CFWAIT			; Wait until CF-card ready
		BCC	CF_RD_END		; Exit on CF-card error

		LDA 	CFREG7			; CF status register
		AND 	#$08			; Filter out DRQ
		BEQ 	CF_RD_END		; branch if DRQ is no longer set

		LDA 	CFREG0			; read data-bytes
		STA 	(BLKBUF),Y		; store in buffer
		INY				; next byte
		BNE 	CF_RD_BLK0		; branch if more bytes to read

		INC	BLKBUF+1		; yes, increment block buffer page
		DEX
		BPL	CF_RD_BLK0		; two pages read? no, read next byte
		
		SEC				; yes, all data read, set C = 1 (no error)
		RTS
CF_RD_END	CLC				; C=0 (error), DRQ reset should not happen
		RTS

;----------------------------------------------------------------------------
; Command: None, Write Single Data Block from Std. Block Buffer
; Input  : CFLBA3..CFLBA0 = 32 Bit LBA Address to write to
;          NOTE: These have to loaded prior to calling this function!!!
; Output : C = 0 Error, C = 1 Read OK
;	   A = Error Code
;----------------------------------------------------------------------------
CF_WR_BLK_BUF	JSR	INIT_BLKBUF		; set pointer to block buffer
		BEQ	CF_WR_BLK		; branch always

;----------------------------------------------------------------------------
; Command: WRITE_BUF, Write Single Data Block from Std. Block Buffer to Logical Address
; Input  : X,Y = Ptr[LO:HI] to 32 Bit LBA Destination Address
; Output : C = 0 Error, C = 1 Data OK
;	   A = Error Code
;----------------------------------------------------------------------------
CF_WR_LBLK_BUF	JSR	INIT_BLKBUF		; set pointer to block buffer
						; fall through to CR_WR_LBLK

;----------------------------------------------------------------------------
; Command: CMD_WRITE, Write Single Data Block to Logical Address
; Input  : X,Y = Ptr[LO:HI] to 32 Bit LBA Destination Address
;	   BLKBUF,BLKBUFH = 16 Bit Source Address
; Output : C = 0 Error, C = 1 Data OK
;	   A = Error Code
;----------------------------------------------------------------------------
CF_WR_LBLK	JSR	LOAD_LBA_CF		; Load LBA into CF-card
						; fall through to CF_WR_BLK

;----------------------------------------------------------------------------
; Write Single Data Block
; Input:  X,Y = Ptr[LO:HI] to 32 Bit LBA Destination Address
;	  BLKBUF,BLKBUFH = 16 Bit Source Address
; Output: C = 0 Error, C = 1 Write OK
;	  A = Error Code
;----------------------------------------------------------------------------
CF_WR_BLK	LDA 	#$01
		STA 	CFREG2			; Read one Sector
		JSR 	CFWAIT			; Wait until CF-card ready
		BCC 	CF_WR_END		; branch on error

		LDA 	#$30			; Write Sector Command
		STA 	CFREG7			; CF command register
CF_WR_INFO	LDX	#$01			; initialize page counter
		LDY	#$00			; initialize byte counter
CF_WR_BLK0	JSR	CFWAIT			; Wait until CF-card ready
		BCC	CF_WR_END		; Exit on CF-card error

		LDA 	CFREG7			; CF status register
		AND 	#$08			; Filter out DRQ
		BEQ 	CF_WR_END		; branch if DRQ is no longer set

		LDA 	(BLKBUF),Y		; read from buffer
		STA 	CFREG0			; Write to CF-card
		INY				; next byte
		BNE 	CF_WR_BLK0		; branch if more bytes to write

		INC	BLKBUF+1		; yes, increment block buffer page
		DEX
		BPL	CF_WR_BLK0		; two pages read? no, read next byte
		
		SEC				; yes, all data read, set C = 1 (no error)
		RTS
		
CF_WR_END	CLC				; C=0 (error), DRQ reset should not happen
		RTS

;----------------------------------------------------------------------------
; Text-strings needed for Printing
;----------------------------------------------------------------------------
TXT_SER		.by        '   Serial: ' $00
TXT_FW		.by     CR ' Firmware: ' $00
TXT_MOD		.by     CR '    Model: ' $00

;----------------------------------------------------------------------------
; This function enables a RAM-bank at $4000-$7FFF.
; Input: A: the RAM-bank number to enable [0..28]. A RAM-bank starts at 4,
;           so 0..3 disables the RAM-banks and enables main-memory
;----------------------------------------------------------------------------
SET_RAMBANK	CMP	#29		; RAM-bank 28 is the highest nr
		BCS	SET_RAMB0	; branch if >= 29
		CMP	#4		; <= 28, now check for >= 4
		BCS	RAMBVLD		; branch if >= 4
		
SET_RAMB0	LDA	#0		; set to no RAM-bank (main-memory)
RAMBVLD		ASL
		ASL			; nr now in RAM-bank bits 6..2
		ORA	MMU		; add to MMU-register
		STA	MMU		; Enable RAM-bank
		RTS			; return
		
;----------------------------------------------------------------------------
; This function returns the currently active RAM-bank at $4000-$7FFF.
; Output: A: the active RAM-bank number [4..28] or 0 if no RAM-bank is selected.
;----------------------------------------------------------------------------
GET_RAMBANK	LDA	MMU		; MMU-register
		AND	#$7C		; Only RAM-bank bits
		LSR
		LSR			; Now in bits 4..0
		CMP	#4		; Is it 4 or more?
		BCS	RAMBX		; branch if >= 4, RAM-bank selected
		
		LDA	#0		; RAM-bank 0..3 => main memory
RAMBX		RTS			; return		
		
;----------------------------------------------------------------------------
; This function enables Monitor-ROM at $1C00-$1FFF and disables the RAM behind it.
;----------------------------------------------------------------------------
MON2ROM		LDA	MMU		; MMU-register
		ORA	#MON_EN		; 1 = enable Monitor ROM
		STA	MMU		; Activate Monitor ROM
		RTS			; return
		
;----------------------------------------------------------------------------
; This function Enables Monitor-RAM at $1C00-$1FFF and disables Monitor-ROM.
;----------------------------------------------------------------------------
MON2RAM		LDA	MMU		; MMU-register
		AND	#~MON_EN	; 0 = enable Monitor RAM, disable ROM
		STA	MMU		; Activate Monitor RAM
		RTS			; return

;----------------------------------------------------------------------------------		
; This routine disables the BASIC ROM and enables the RAM behind it.
;----------------------------------------------------------------------------------		
BAS2RAM		LDA	MMU			; MMU register		
		AND 	#~BAS_EN		; Set bit to 0, disable BASIC		   
		STA 	MMU	   		; disable BIOS ROM, enable RAM behind it
		RTS				; return

;----------------------------------------------------------------------------------		
; This routine enables the BASIC ROM and disables the RAM behind it.
;----------------------------------------------------------------------------------		
BAS2ROM		LDA	MMU			; MMU register		
		ORA 	#BAS_EN			; Set bit to 0, disable BASIC		   
		STA 	MMU	   		; disable BIOS ROM, enable RAM behind it
		RTS				; return

;----------------------------------------------------------------------------
; Check both ROMs: 
; 1) Monitor ROM at $1C00-$1FFF
; 2) BASIC + BIOS ROM at $B000-$FFF0
; Input: A: MSB of ROM begin ($B0 for BIOS, $1C for Monitor).
;        X: LSB of end-Address
;        Y: MSB of end-Address
;----------------------------------------------------------------------------
CHECK_ROMS	LDA	#$1C		; MSB of $1C00
		LDX	#$00		; LSB of $1FFF+1 (end-address)
		LDY	#$20		; MSB of $1FFF+1 (end-address)
		JSR	CHECK_ROM_STRT	; Check Monitor ROM Checksum
		LDA	#$B0		; MSB of $B000
		LDX	#$00		; LSB of $DFFF+1 (end-address)
		LDY	#$E0		; MSB of $DFFF+1 (end-address)
		JSR	CHECK_ROM_STRT	; Check BASIC ROM Checksum
		LDA	#$E0		; MSB of $B000
		LDX	#$F0		; LSB of $FFF0 (end-address)
		LDY	#$FF		; MSB of $FFF0 (end-address)
		;JMP 	CHECK_ROM_STRT	; Check BIOS ROM checksum and return
		
;----------------------------------------------------------------------------
; This routine is the entry-point for the ROM checksum routines.
; Input: A: MSB of ROM begin ($B0 for BIOS, $1C for Monitor).
;        X: LSB of end-Address
;        Y: MSB of end-Address
;----------------------------------------------------------------------------
CHECK_ROM_STRT	STX	END_PTR		; LSB of end-address
		STY	END_PTR+1	; MSB of end-address
		LDX	#0
		STX 	ROM_CS		; Init ROM checksum
		STX 	ROM_CS+1
		STX	ROM_PTR		; LSB of begin-address
		STA	ROM_PTR+1	; MSB of begin-address
		CMP	#$1C		; Monitor ROM?
		BEQ	MON_CHK_ROM	; branch if Monitor ROM
		
		CMP	#$B0		; BASIC ROM?
		BEQ	BAS_CHK_ROM	; branch if BASIC ROM

		LDX	#<TXT_ROM	; BIOS ROM
		LDY	#>TXT_ROM
		BNE 	PR_CHKROM_TXT	; branch always
		
BAS_CHK_ROM	LDX	#<TXT_BAS	; BASIC ROM
		LDY	#>TXT_BAS
		BNE 	PR_CHKROM_TXT	; branch always

MON_CHK_ROM	LDX	#<TXT_MON	; Monitor ROM
		LDY	#>TXT_MON
PR_CHKROM_TXT	JSR	SPRINT
		LDX	#<TXT_CS	; Print ' KB ROM '
		LDY	#>TXT_CS
		JSR	SPRINT
		LDY	#0
		;JMP	ROM_LP1		; fall-through to ROM_LP1

;----------------------------------------------------------------------------
; This routine calculates the checksum of the entire ROM area from 
; ROM_PTR to END_PTR.
;----------------------------------------------------------------------------
ROM_LP1		LDA 	(ROM_PTR),Y	; get byte from ROM
		ADCAW	ROM_CS		; ROM_CS = ROM_CS + A
		INY			; next byte
		BNE	ROM_CHK_END	; branch if not on a new page
		
		INC 	ROM_PTR+1	; MSB, next page
ROM_CHK_END	LDA	ROM_PTR+1	; Current ROM address MSB
		CMP	END_PTR+1	; MSB of end-address
		BNE	ROM_LP1		; branch if not done yet
		
		CPY	END_PTR		; End-address?
		BNE	ROM_LP1		; branch if not at end-address yet
		
		LDA	END_PTR+1
		CMP	#$FF		; BIOS ROM?
		BNE	ROM_CHK2	; branch if not BIOS ROM
		
		CPW	ROM_CS ROM_CS16	; Compare 2 words BIOS checksum
		BNE	ROM_CS_ERR	; branch if not the same
		BEQ	ROM_CS_OK	; branch if the same

ROM_CHK2	CMP	#$E0		; BASIC ROM?
		BNE	MON_ROM_CMP	; branch if Monitor ROM
		
		CPW	ROM_CS BAS_CS16	; Compare 2 words BASIC checksum
		BNE	ROM_CS_ERR	; branch if not the same
		BEQ	ROM_CS_OK	; branch if the same
		
MON_ROM_CMP	CPW	ROM_CS MON_CS16	; Compare 2 words Monitor checksum
		BNE	ROM_CS_ERR	; branch if not the same

ROM_CS_OK	LDX	#<TXT_CS_OK	; Print 'OKE'
		LDY	#>TXT_CS_OK
		JMP	SPRINT		; Print and return
ROM_CS_ERR	LDX	#<TXT_CS_ERR	; Print 'Error'
		LDY	#>TXT_CS_ERR
		JMP	SPRINT		; Print and return

;----------------------------------------------------------------------------
; This routine prints a string to the terminal: X=LSB, Y=MSB.
; In order not to interfere with the BIOS STROUT / WRSTR with PSTR, a copy
; of these routines is made with SPRINT and SPROUT.
;----------------------------------------------------------------------------
SPROUT		LDY  	#$00       	; index y is 0
SPROUTLP	LDA  	(PRSTR),Y   	; load char at string pos y
		BEQ  	ENDSPROUT  	; exit, if NULL char
		
		JSR  	COUT       	; write character
		INY             	; next index
		BNE  	SPROUTLP	; branch always
		
ENDSPROUT	RTS			; return

TXT_MON		.by	' JC-MON 1' $00
TXT_BAS		.by	' BASIC 12' $00
TXT_ROM		.by	' BIOS   8' $00
TXT_CS		.by	' KB ROM ' $00
TXT_CS_OK	.by	'OK' CR $00
TXT_CS_ERR	.by	'Error' CR $00		
		

		