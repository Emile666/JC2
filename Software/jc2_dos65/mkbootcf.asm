;---------------------------------------------------------------------------
;MKBOOT for CF-IDE Interface
;V0.1: version for new Eprom BIOS V1.2.1 + bug-fix BIOS addresses + BOOT_CODE = $4000
;V0.2: NUM32/SUM32 moved from $F8/$FC to $C0/$C3, same as boot.sys V0.23.
;---------------------------------------------------------------------------
        OPT h- ; do not add file header
        OPT f+ ; save as single block
	
RES             EQU     $20
PCNT		EQU	$21		; 1-byte, Partition number in MBR [0..3]
FIRST_PART      EQU     $22
NUM_PART        EQU     $23
IS_FAT16	EQU	$24		; $01 = FAT32 detected, #00 = FAT12/FAT16
BLKBUF		EQU	$DC             ; pointer to block buffer
SRC		EQU	$DC             ; 2-byte pointer to source memory
PLBA		EQU	$E6		; 2-byte LBA pointer
DST      	EQU   	$E8     	; 2-byte pointer to destination memory
STOL		EQU	$E8		; Store address Low
STOH		EQU	$E9		; Store address High
PSTR            EQU     $EA		; 2-byte ACIA output pointer, same in BIOS
CNT             EQU     $EC		; 2-byte counter
PSAV            EQU     $EE

NUM32      	EQU   	$C0     	; low 32 bit number byte, same as boot.sys V0.23
SUM32           EQU     $C4             ; low 32 bit number byte, same as boot.sys V0.23
YSAV            EQU     $FD             ; Temp Saved Device ID
TEMP            EQU     $FF

CR        	EQU     $0D    		; carriage return
ESC       	EQU     $1B    		; ESC

MON_COLD_START	EQU	$E000		; Monitor cold start
MON_WARM_START	EQU	$E003		; Monitor warm start
SWITCH_TO_RAM	EQU	$E006		; Switch To RAM Page (B000..DFFF) 
CIN		EQU 	$E047		; Read Character Routine
COUT		EQU	$E052		; Write Character Routine
CROUT		EQU	$E05A		; Write CR/LF to Terminal
STROUT		EQU     $E083	        ; Write String Routine
HEXOUT          EQU     $E091           ; print 2-byte hex number
NUMOUT		EQU	$E0A7		; Print a byte as decimal number
CMDDEV		EQU	$E0BA		; Call Opened Device Command routine
DELAY		EQU     $E14D		; Delay routine
OPEN_DEVICE	EQU	$E1AA		; Open Device for Read/Write
INIT_BLKBUF	EQU	$F68B		; Init. Block-buffer pointer
SYS_LD_BOOTBLK	EQU	$F6E7		; Load Boot block from device
SYS_MBR_ID	EQU	$F6EE		; Load Boot block from device
SYS_CHECK_OS	EQU	$F700		; Check OS OEM String
DEV_ADD		EQU	$F76F		; Add Device Driver
CFC_DEV_ROM	EQU	$F9D0		; Device driver in eprom V1.2.0

; Device Command Constants *****************************************************
CMD_INIT	EQU	0               ; Init device
CMD_READ	EQU	34              ; Read data block from device
CMD_WRITE	EQU	35              ; Write data block to device
CMD_BOOT        EQU     36              ; Boot from device
CMD_READ_BUF	EQU	37              ; Read data block from device to standard buffer
CMD_WRITE_BUF	EQU	38              ; Write data block to device 

HDD1_ID		EQU	$25   		; Harddisk 1 device ID

; Tables and Buffers ***********************************************************
CURR_VOLUME     EQU     $0400           ; Current Volume Descriptor
BLOCK_BUF      	EQU     $0600           ; Data Block Buffer
MBR             EQU     BLOCK_BUF       ; Master Boot Block Code
PART0		EQU	$07BE		; Partition 0 start in MBR
PART0_START	EQU	PART0 + 8 	; Partition 0 relative sector field
BOOTBLK_TAG     EQU     $07FE           ; Address of Boot Block Tag ($55 $AA)

; Constants in Volume ID (=Partition 1st sector) *******************
Sectors_Per_Cluster EQU  $060D		; 1-byte, should be a power of 2
Reserved_Sectors    EQU  $060E		; 2-byte
Number_of_FATs      EQU  $0610		; 1-byte, typical 2
Num_Root_Dir_Entr   EQU  $0611		; 2-byte, max. entries per dir.
Num_Log_Sectors     EQU  $0613		; 2-byte, if 0 -> 4 bytes at $0620
Medium_Descr_Byte   EQU  $0615		; 1-byte
Sectors_Per_FAT16   EQU  $0616		; 2-byte, #sectors / FAT (0 for FAT32)
Sectors_Per_FAT32   EQU  $0624		; 2-byte
Dir_Start_Cluster   EQU  $062C		; 4-byte, aka root_dir_first_cluster, copied to D_START_DIR
File_System_Type    EQU  $0636		; 8-byte, filesystem type e.g. "FAT12   ", "FAT16   "

BOOT_CODE       EQU  	$4000		; Must be aligned with ORG of boot.sys!

DIR_BLK_BUF     EQU  	$0200		
MOUNTED_DEVS    EQU     $0400           ; Table of mounted devices
BOOT_PART       EQU     MOUNTED_DEVS    ; Boot Medium Descriptor

D_PART_START    EQU     BOOT_PART	; 4 Bytes - Partition Start LBA, set by CF/SD Boot routine
D_PART_SIZE     EQU     BOOT_PART+$04	; 4 Bytes - FAT12 Partition Size
D_DEV_ID        EQU     BOOT_PART+$08
D_MEDIUM_DESCR  EQU     BOOT_PART+$09
D_FAT_TYPE      EQU     BOOT_PART+$0A
D_SECT_PER_CLST EQU     BOOT_PART+$0B
D_NUM_OF_FAT    EQU     BOOT_PART+$0C
D_NUM_ROOT_DIR  EQU     BOOT_PART+$0D
D_START_CLUSTER EQU     BOOT_PART+$10
D_START_FAT1    EQU     BOOT_PART+$14
D_START_FAT2    EQU     BOOT_PART+$18
D_START_DIR     EQU     BOOT_PART+$1C

FAT12_Type      EQU     $02
FAT16_Type      EQU     $01
FAT32_Type      EQU     $00

;----------------------------------------------------------------------------
; $1B00-$1BFF is a 256-byte RAM area
;----------------------------------------------------------------------------
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
RSTACT		EQU	SECT_PER_FAT+1	; 1 = Reset pending
MSEC		EQU	RSTACT+1	; msec. counter

;----------------------------------------------------------------------------
; CF-IDE Hardware Registers
;----------------------------------------------------------------------------
CFBASE		EQU 	$0C80		; Base card-address for CF-IDE card
CFREG0		EQU	CFBASE+0	; Data port
CFREG1		EQU	CFBASE+1	; Read: error-code, write: feature
CFREG2		EQU	CFBASE+2	; Number of sectors to transfer
CFREG3		EQU	CFBASE+3	; Sector address LBA 0 [0:7] (LSB)
CFREG4		EQU	CFBASE+4	; Sector address LBA 1 [8:15]
CFREG5		EQU	CFBASE+5	; Sector address LBA 2 [16:23]
CFREG6		EQU	CFBASE+6	; Sector address LBA 3 [24:27] (MSB)
CFREG7		EQU	CFBASE+7	; Read: Status, Write: Command
CFREG8		EQU	CFBASE+8	; A write with 0x00 is a HW reset

.if	USE_XMODEM = 1
	.word	RUN_ADDR		; Needed for XMODEM lm command loading .bin files
.endif
		ORG $3000

RUN_ADDR	LDX 	#<TXT_TITLE    	; Print title text
		LDY 	#>TXT_TITLE    	;
		JSR 	SPRINT         	; Print title text
		
		; Now check if device-driver is already added (in eprom V1.2.0 and higher)
		LDA	#HDD1_ID
		ASL
		TAX
		LDA	$1A08,X
		CMP	#<CFC_DEV_ROM
		BNE	NO_DEV_PRES
		
		LDA	$1A09,X
		CMP	#>CFC_DEV_ROM
		BEQ	OPENDD		; branch if device driver present in ROM

		; Copy CF device descriptor into dev. descr. table
NO_DEV_PRES	LDX     #<CFC1_DEV
                LDY     #>CFC1_DEV
                JSR     DEV_ADD         ; add CF-card driver
		BCS	OPENDD		; branch on success
		
		PHA			; Save error code
		LDX	#<TXT_ERR_DEV1	; Print Error
		LDY     #>TXT_ERR_DEV1 	; 
                JSR     SPRINT          ; print
		PLA
		JSR	HEXOUT		
		LDX	#<TXT_ERR_DEV2	; Print Error
		LDY     #>TXT_ERR_DEV2 	; 
		JMP	PRX		; Print and Exit
		
OPENDD		LDA	#HDD1_ID	; Code for CF-card
		JSR	OPEN_DEVICE	; Open device Driver
		BCS	KBD_INP		; branch on success

		LDX	#<TXT_ERR_DEV3	; Print Error
		LDY     #>TXT_ERR_DEV3 	; 
PRX             JSR     SPRINT          ; print
		JMP	MON_WARM_START	; exit program, back to monitor
		
KBD_INP		JSR 	CIN           	; get input from keyboard
		CMP 	#CR           	;
		BNE 	KBD_INP        	; branch if not EOL

		JSR 	CROUT           ; Print CR
		
		LDA 	#CMD_INIT       ; Init. CF-card
		JSR 	CMDDEV          ; ... and reset CF-card
		BCC 	PR_CF_ERR       ; branch if CF-card error

		JSR 	CF_INFO		; Print CF-Card Info
		LDX 	#$00           	;
		STX 	NUM_PART       	; Reset flag for FAT code copied
		STX 	FIRST_PART     	; 
		DEC 	FIRST_PART     	; Init. partition nr to $FF
READ_PART	STX 	PCNT            ; 0 = 1st partition in MBR
		LDX	#<TXT_MBR	; Print Now Reading MBR
		LDY     #>TXT_MBR 	; 
		JSR     SPRINT 
		JSR 	READ_MBR        ; Clear curr.vol. descr. (MBR) and load MBR in buffer
		LDX 	PCNT  	        ; get partition number
		JSR 	RD_PARTITION 	; read begin-LBA of partition into $0400
		BEQ 	NEXT_PART      	; branch if begin-LBA = 0 (= no partition)

		LDX	#<TXT_PARTSEC	; Print Now Reading Partition Volume ID 
		LDY     #>TXT_PARTSEC 	; 
		JSR     SPRINT 
		JSR	PR_LBA		; Print LBA number in hex
		LDA	#CR
		JSR	COUT
		JSR 	RD_BUF         	; Read first sector of partition (Volume ID)
		JMP 	PR_WR_PART     	; Print and write partition

		; A partition entry was not found
NEXT_PART	LDX 	PCNT         	; get partition number
		INX                 	; Next partition in MBR
		CPX 	#$04           	; All 4 partitions read?
		BNE 	READ_PART      	; Branch if not all read
	
		JMP 	WRITE_MBR  	; Make partitition bootable and write MBR

		; First sector of Partition (Volume ID) is loaded into the buffer
PR_WR_PART	JSR PR_PART_INFO       	; Print partition info
GETYN		JSR CIN           	; get character
		AND #$DF            	; uppercase chars only
		CMP #'Y'            	;
		BEQ WR_PART	       	; if Y then write Partition

		CMP #'N'            	;
		BEQ NEXT_PART          	; if N then next partition

		BNE GETYN           	; branch always

WR_PART		LDA IS_FAT16           	; Is it FAT16?
		BNE CFAT32           	; branch if not FAT16

		JSR COPY_FAT16         	; Copy FAT16 code into page 6 (Volume ID)
		JMP L3068           	; branch always

CFAT32		JSR COPY_FAT32        	; Copy FAT32 code into page 6 (Volume ID)
L3068		INC NUM_PART           	; Flag that FAT code is copied
		LDX FIRST_PART         	; Partition number
		INX                 	; 
		BNE WR_IT           	; branch if not first partition

		LDX PCNT            	; Partition number [0..3]
		STX FIRST_PART         	; 
WR_IT		JSR WR_OEM_NAME        	; Write 'JC OS' into OEM name/version in Volume ID
		LDX #<TXT_WRVOLID	; Print Message	
		LDY #>TXT_WRVOLID	; 
                JSR SPRINT		; Print it
		JSR WR_BUF           	; Write buffer to CF-card
		JMP NEXT_PART          	; Next partition

PR_CF_ERR	LDX #<TXT_ERR		; Print Message
		LDY #>TXT_ERR 		; 
                JSR SPRINT		; Print CF-card error
		JMP MKBOOT_END         	; Print 'Reinsert system Drive'

;------------------------------------------------------------------------
; Copy code with size (CNT, CNT+1) from a 
; source-address (SRC,SRC+1) to a destination address (DST,DST+1)
;------------------------------------------------------------------------
COPY_SRC_DST	LDY #$00            	;
COPY_LP1	LDA (SRC),Y        	; get source byte
		STA (DST),Y         	; store in destination address
		INC SRC            	; increment LSB of source address
		BNE COPY_LP2           	;

		INC SRC+1               ; increment MSB of source address
COPY_LP2	INC DST             	; increment LSB of destination address
		BNE COPY_LP3           	;

		INC DST+1             	; increment MSB of destination address
COPY_LP3	LDA CNT             	;
		BNE COPY_LP4           	; LSB of size not 0 yet

		DEC CNT+1             	; decrement MSB of size
COPY_LP4	DEC CNT             	; decrement LSB of size
		BNE COPY_LP1           	; branch if not done yet

		LDA CNT+1             	; 
		BNE COPY_LP1           	; branch if not done yet

		RTS                 	; return

;------------------------------------------------------------------------
; Read the Begin LBA of a Partition, X = partition number
; Returns: A=0: no LBA-begin address, A>0 LBA-begin address present
;------------------------------------------------------------------------
RD_PARTITION	LDA #$04            	; 
		STA RES             	; Nr. of non-zero digits
		TXA                 	; partition number
		ASL                	;	
		ASL                	;
		ASL                	;
		ASL                	;
		ORA #$03           	;
		TAX                 	; X = 16*PCNT+3
		LDY #$03            	; LBA is 4 bytes
?L1		LDA PART0_START,X      	; LBA-begin area
		STA CURR_VOLUME,Y       ; save LBA-begin in CURR_VOLUME ($400)
		BNE ?L2           	; 

		DEC RES            	; Nr. of non-zero digits
?L2		DEX                 	;
		DEY                 	;
		BPL ?L1           	; branch if not all digits read

		LDA RES             	; 0 = LBA-begin address is zero
		RTS                 	; return

;------------------------------------------------------------------------
; Print Partition Info, including volume-label (FAT12, FAT16, FAT32)
;------------------------------------------------------------------------
PR_PART_INFO	LDX #<TXT_PART	; Print Message
		LDY #>TXT_PART 	; 
                JSR SPRINT
		LDA PCNT           	; Get partition number
		CLC                 	;
		ADC #'1'            	; print '1'
		JSR COUT  
		LDX #<TXT_FMT		; Print 'format is'
		LDY #>TXT_FMT 		; 
                JSR SPRINT         	;
		LDX #$36            	; Volume label for FAT12 & FAT16
		LDA BLOCK_BUF,X       	;
		CMP #'F'            	;
		BNE TRYFAT32           	; branch if no FAT12 or FAT16 label detected

MSG_FAT_TYPE1	LDA BLOCK_BUF,X       	; Copy volume-label
		JSR COUT           	; To terminal output
		INX                 	;
		CPX #$3B            	; Copy 5 characters
		BNE MSG_FAT_TYPE1      	;

		LDA #$00            	; $00 = FAT16 
		STA IS_FAT16           	; FAT16 indicator
		BEQ L3108           	; branch if FAT16

TRYFAT32	LDX #$52            	; FAT32 volume-lable entry
		LDA BLOCK_BUF,X       	;
		CMP #'F'            	;
		BNE L310D           	; No FAT32 found

MSG_FAT_TYPE3	LDA BLOCK_BUF,X       	; Copy volume-label
		JSR COUT           	; To terminal output
		INX                 	;
		CPX #$57            	; Copy 5 characters
		BNE MSG_FAT_TYPE3      	;

		LDA #$01            	; $01 = FAT32
		STA IS_FAT16           	;
L3108		LDX #<TXT_WRBB		; Print 'Write Boot block (y/n)?'
		LDY #>TXT_WRBB 		; 
                JSR SPRINT		; print it
L310D		RTS                 	;

;--------------------------------------------------------------------------------
; This routine copies the FAT16 boot-code into Page 6
;--------------------------------------------------------------------------------
COPY_FAT16	LDA FAT16_BLK          	; LSB of FAT16 code destination-address
		STA DST             	;
		LDA FAT16_BLK+1       	; MSB of FAT16 code destination-address
		STA DST+1             	;
		LDA FAT16_BLK+2	       	; LSB of FAT16 code-size
		STA CNT             	;
		LDA FAT16_BLK+3        	; MSB of FAT16 code-size
		STA CNT+1             	;
		LDA #<FAT16_START      	; LSB of FAT16 code source-address
		STA SRC            	;
		LDA #>FAT16_START      	; MSB of FAT16 code source-address
		STA SRC+1            	;
		JSR COPY_SRC_DST       	; copy FAT16 code to page 6
		RTS                 	; return

;--------------------------------------------------------------------------------
; This routine copies the FAT32 boot-code into Page 6
;--------------------------------------------------------------------------------
COPY_FAT32	LDX #<TXT_FAT32C	; Print Now writing FAT32 Boot-code
		LDY #>TXT_FAT32C
		JSR SPRINT
		LDA FAT32_BLK          	; LSB of FAT32 code destination-address
		STA DST             	;
		LDA FAT32_BLK+1        	; MSB of FAT32 code destination-address
		STA DST+1             	;
		LDA FAT32_BLK+2        	; LSB of FAT32 code-size
		STA CNT             	;
		LDA FAT32_BLK+3        	; MSB of FAT32 code-size
		STA CNT+1             	;
		LDA #<FAT32_START      	; LSB of FAT32 code source-address
		STA SRC            	;
		LDA #>FAT32_START      	; MSB of FAT32 code source-address
		STA SRC+1            	;
		JSR COPY_SRC_DST       	; copy FAT32 code to page 6
		RTS                 	; return

;--------------------------------------------------------------------------------
; This routine copies the MBR boot-code into Page 6.
;--------------------------------------------------------------------------------
MKBOOT_MBR	LDX #<TXT_MBRC		; Print Now writing MBR Boot-code
		LDY #>TXT_MBRC
		JSR SPRINT
		LDA MBRBOOT_BLK        	; LSB of destination address
		STA DST             	;
		LDA MBRBOOT_BLK+1      	; MSB of destination address
		STA DST+1             	;
		LDA MBRBOOT_BLK+2      	; LSB of code-size
		STA CNT             	;
		LDA MBRBOOT_BLK+3      	; MSB of code-size
		STA CNT+1             	;
		LDA #<MBRBOOT_START    	; LSB of source address
		STA SRC            	;
		LDA #>MBRBOOT_START   	; MSB of source address
		STA SRC+1            	;
		JSR COPY_SRC_DST       	; copy $01BE bytes from $3615 to $0600
		RTS                 	; return

; Write JC2 OS text to OEM name/version in Volume ID sector
WR_OEM_NAME	LDX #<TXT_OEM		; Print Now writing OEM name to Volume ID
		LDY #>TXT_OEM
		JSR SPRINT
		LDX #$07            	; string length
WRNXTCHR	LDA TXT_JCOS,X         	; 'JCOS    ' string
		STA BLOCK_BUF+3,X      	; OEM name/version bytes
		DEX                 	;
		BPL WRNXTCHR           	; branch if not done yet
		RTS                 	; return

; Set to boot-flag to Active of the current partition
WRITE_MBR	LDA FIRST_PART         	; Partition number
		PHA
		ASL                	;
		ASL                	;
		ASL                	;
		ASL                	;
		TAX                 	; X = 16 * Partition number
		LDA #$80            	; $80 = Active
		STA PART0,X         	; Boot-flag (byte 0 of 16-byte partition entry)
		LDX #<TXT_BFLAG		; Print Adding boot-flag
		LDY #>TXT_BFLAG
		JSR SPRINT
		PLA
		CLC                 	;
		ADC #'1'            	; print '1'
		JSR COUT  
		LDX #<TXT_BFLAGE	; Print ' in MBR'
		LDY #>TXT_BFLAGE
		JSR SPRINT
		LDX NUM_PART           	; FAT code copied?
		BEQ MKBOOT_END         	; branch if FAT code not yet copied

		;DEX                 	; 
		;BEQ WR_MBR           	;

		JSR MKBOOT_MBR       	; copy $01BE bytes from MBRBOOT_START to $0600
WR_MBR		LDX #<TXT_WRMBR		; Print Writing MBR
		LDY #>TXT_WRMBR 	; 
                JSR SPRINT		; print it
		JSR WR_BUF           	; write MBR back to CF-card
MKBOOT_END	LDX #<TXT_REI		; Print "Reinsert system drive'
		LDY #>TXT_REI
		JMP SPRINT		; print and return

;----------------------------------------------------------------------------
; This routine clears the current volume descriptor, so that the LBA = 0
; and then it reads the MBR.
;----------------------------------------------------------------------------
READ_MBR	LDA #$00            	; Clear current volume descriptor
		STA CURR_VOLUME       	;
		STA CURR_VOLUME+1      	;
		STA CURR_VOLUME+2     	;
		STA CURR_VOLUME+3    	;
	
RD_BUF 		LDX #<CURR_VOLUME      	;
		LDY #>CURR_VOLUME     	;
		LDA #CMD_READ_BUF       ;
		JMP CMDDEV           	;

;----------------------------------------------------------------------------
; This routine writes a sector to the CF-card, with a LBA set in $0400
;----------------------------------------------------------------------------
WR_BUF		LDX #<CURR_VOLUME	;
		LDY #>CURR_VOLUME	;
		LDA #CMD_WRITE_BUF	;
		JMP CMDDEV		;

;----------------------------------------------------------------------------
; Addition for Standard Driver Descriptors table
;----------------------------------------------------------------------------
CFC1_DEV	.byte	HDD1_ID, $00     ; CF-Card Driver Descriptor
		.word	_EMPTY_
		.word	_EMPTY_
		.word   CFC_CMD

;----------------------------------------------------------------------------
; This routine contains the entry routines for the CF-card routines
;----------------------------------------------------------------------------
CFC_CMD         CMP     #CMD_INIT
                BNE     CFC_READ
		
                JMP     CF_INIT		; Init. CF-card with HW-reset
		
CFC_READ        CMP     #CMD_READ
                BNE     CFC_WRITE
		
                JMP     CF_RD_LBLK	; used a lot in boot.sys and mkboot.sys

CFC_WRITE       CMP     #CMD_WRITE
                BNE     CFC_RD_BUF
		
                JMP     CF_WR_LBLK

CFC_RD_BUF      CMP     #CMD_READ_BUF
                BNE     CFC_WR_BUF
		
                JMP     CF_RD_LBLK_BUF

CFC_WR_BUF      CMP     #CMD_WRITE_BUF
                BNE     CFC_BOOT
		
                JMP     CF_WR_LBLK_BUF
		
CFC_BOOT        CMP     #CMD_BOOT
                BNE     _EMPTY_
                JMP     CF_BOOT

_EMPTY_		CLC
		RTS

;----------------------------------------------------------------------------
; Command: CMD_INIT, Initialize CF-Card
; Output : C = 1 Init OK, C = 0 Error
;----------------------------------------------------------------------------
CF_INIT		LDA #$00		; Reset command
                STA CFREG8		; HW reset command
		LDA #1
		STA RSTACT		; 1 = Reset pending
		JSR CFWAIT
		BCS INITOK		; branch if CF-card init OK
		
CF_ERR		LDA #$80
		RTS			; return if error (C=0)
		
INITOK		LDA #$E0		; LBA3=0, Master, Mode=LBA
		STA CFREG6
		LDA #$01		; 8-bit transfers
		STA CFREG1
		LDA #$EF		; Set feature command
		STA CFREG7		; CF command register
		JSR CFWAIT		; Wait and return
		BCC CF_ERR		; branch if Error
		
		RTS			; Return with C=1 (init ok)
		
;----------------------------------------------------------------------------
; Command: CMD_BOOT, Boot from CF-Card
; Output: C = 1 Init OK, C = 0 Error
;----------------------------------------------------------------------------
CF_BOOT		
		JSR	INIT_LBA		; CFLBA0..CFLBA3 = 0 (MBR) and load into CF-card
		JSR	CF_RD_BLK_BUF		; Read MBR and store in BLOCK_BUF ($0600)
		BCC	CF_END			; error reading MBR, exit

		JSR     SYS_MBR_ID              ; check boot block ID tag
                BCC     CF_END                  ; error, wrong ID. Exit

                LDA     PART0-2                 ; check if partition ID1 is $65
                CMP     #$65
                BNE     LOAD_PART0              ; no, just load partition 0
		
                LDA     PART0-1                 ; check if partition ID2 is $02
                CMP     #$02
                BNE     LOAD_PART0              ; no, just load partition 0
		
                JSR     MBR                     ; partition ID $65 $02 found. Call MBR code
                BNE     LOAD_PART1              ; is boot menu result 1,2,3, or 4 ?
		
SYS_MSG_ERR     CLC                             ; no, ESC pressed or no valid partition found
                RTS                             ; abort booting from CF-Card

LOAD_PART1      DEX                             ; set result to 0,1,2 or 3
                TXA                             ; transfer result to Accu
                TAY                             ; and to Y-Register
                ASL                        	; multiply result by 16
                ASL     
                ASL     
                ASL     
                ORA     #$08                    ; and add 8
                TAX                             ; move partition table index into X
                TYA
                CLC
                ADC     #'1'                    ; convert partition number to ASCII char (+1)
                STA     PSAV                    ; and store it to PSAV
                BNE     LOAD_PART               ; branch always
		
LOAD_PART0      LDX     #$08                    ; for partition 0 the table index is 8
                LDA     #'1'                    ; partition 0 number as ASCII char (+1)
                STA     PSAV                    ; store it in PSAV
		JSR 	COUT			; DEBUG
                LDA     PART0                   ; read boot indicator
                BEQ     SYS_MSG_ERR             ; if $00 then exit

LOAD_PART       LDY     #$08
CF_BOOT1        LDA     PART0_START,X           ; load partition start and length
                STA     BOOT_PART,Y             ; and save it to boot device descriptor
                DEX
                DEY
                BPL     CF_BOOT1
		
                LDX	#<BOOT_PART             ; read partition boot blk ptr
		LDY	#>BOOT_PART
		JSR     SYS_LD_BOOTBLK          ; load partition boot block
                BCC     CF_END                  ; block not found. Exit
		
		LDX	#<TXT_PROGRESS
		LDY	#>TXT_PROGRESS
		JSR	SPRINT
                JSR     SYS_CHECK_OS            ; check OS OEM string
                BCC     CF_END                  ; wrong OEM string. Exit

                LDX	#<TXT_CFDEV
		LDY     #>TXT_CFDEV        	; load pointer to device name
                JSR     SPRINT                 	; print device name to screen
                LDA     PSAV                    ; add partition number to name (_1.._4)
                JSR     COUT
                SEC                             ; normal boot, set carry flag
		
CF_END          RTS                     	; abort booting from CF-Card

;----------------------------------------------------------------------------
; This routine reads CF information and prints it.
;----------------------------------------------------------------------------
CF_INFO		JSR CFWAIT	    	; Wait until CF-card ready
		LDA #$EC	    	; Drive ID command
		STA CFREG7		; CF command register
		JSR INIT_BLKBUF		; Init. buffer-pointer ($0600)
		JSR CF_RD_INFO		; Read 512 bytes (= CF_RD_BLK without the Read 1 sector commands)
		LDA #CR
		JSR COUT	    	; new-line

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
		LDA #CR
		JSR COUT	    	; new-line

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
		LDA #CR
		JSR COUT	    	; new-line

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
		LDA #CR
		JSR COUT	    	; new-line
		LDA #CR
		JMP COUT	    	; new-line and return

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
COUTXY		STX SAVX		; Save X	
		STY SAVY		; Save Y
		JSR COUT		; Print char.
		LDY SAVY		; Get Y back
		LDX SAVX		; Get X back
		RTS			; return
SAVX	.ds 1
SAVY	.ds 1

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
LOAD_LBA	STX PLBA		; Store pointer
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

PR_LBA		LDA CURR_VOLUME+3
		JSR HEXOUT
		LDA CURR_VOLUME+2
		JSR HEXOUT
		LDA CURR_VOLUME+1
		JSR HEXOUT
		LDA CURR_VOLUME+0
		JMP HEXOUT		;Print and return
		
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
;	   BLKBUF,BLKBUF+1 = 16 Bit Destination Address
; Output : C = 0 Error, C = 1 Data OK
;	   A = Error Code
;----------------------------------------------------------------------------
CF_RD_LBLK	JSR	LOAD_LBA		; Load LBA into CF-card
						; fall through to cf_rd_blk

;----------------------------------------------------------------------------
; Read Single Data Block
; Input:  CFLBA3..CFLBA0 = 32 Bit LBA Address
;         BLKBUF,BLKBUF+1 = 16 Bit Destination Address
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
; Command: WRITE_BUF, Write Single Data Block from Std. Block Buffer to Logical Address
; Input  : X,Y = Ptr[LO:HI] to 32 Bit LBA Destination Address
; Output : C = 0 Error, C = 1 Data OK
;	   A = Error Code
;----------------------------------------------------------------------------
CF_WR_LBLK_BUF	JSR	INIT_BLKBUF		; set pointer to block buffer
						; fall through to cf_rd_lblk

;----------------------------------------------------------------------------
; Command: CMD_WRITE, Write Single Data Block to Logical Address
; Input  : X,Y = Ptr[LO:HI] to 32 Bit LBA Destination Address
;	   BLKBUF,BLKBUF+1 = 16 Bit Source Address
; Output : C = 0 Error, C = 1 Data OK
;	   A = Error Code
;----------------------------------------------------------------------------
CF_WR_LBLK	JSR	LOAD_LBA		; convert LBA CMD ADR
						; fall through to cf_rd_blk

;----------------------------------------------------------------------------
; Write Single Data Block
; Input:  X,Y = Ptr[LO:HI] to 32 Bit LBA Destination Address
;	  BLKBUF,BLKBUF+1 = 16 Bit Source Address
; Output: C = 0 Error, C = 1 Write OK
;	  A = Error Code
;----------------------------------------------------------------------------
CF_WR_BLK	LDA 	#$01
		STA 	CFREG2			; Read one Sector
		JSR 	CFWAIT			; Wait until CF-card ready
		BCC 	CF_WR_END		; branch on error

		LDA 	#$30			; Write Sector Command
		STA 	CFREG7			; CF command register
		LDX	#$01			; initialize page counter
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
; This routine waits until the CF-card is ready.
;----------------------------------------------------------------------------
CFWAIT		LDA #0
		STA MSEC		; msec counter
CFWLP		LDA RSTACT		; 1= Reset pending
		BEQ NO_DLY10		; branch if no 10 msec. delay Needed
		
		LDA #10			; delay = 10 msec.
		JSR DELAY		; delay 10 msec.
NO_DLY10	INC MSEC		; msec-counter
		LDA MSEC
		BEQ CFWLPTO		; branch after 255 tries and no reset
		
		LDA CFREG7		; read status register
		AND #$80		; check busy flag
		BNE CFWLP		; branch if BSY flag is still set
		
		; Busy flag cleared
		LDA CFREG7		; read status register
		AND #$50		; check for RDY and DSC flags
		CMP #$50		; BSY and DSC flags both set?
		BNE CFWLP		; branch if RDY and DSC not both set

		LDA RSTACT		; 1 = Reset pending
		BEQ PRENDOK		; branch if no Reset pending
		
		LDA #0
		STA RSTACT		; Reset no longer pending
		LDX #<TXT_RSTOK     	; Print Reset OK + msec
		LDY #>TXT_RSTOK
		JSR SPRINT	    	; print
		LDA MSEC		; #msec. * 10
		JSR NUMOUT		; Print decimal number
		LDX #<TXT_MSEC     	; Print msec
		LDY #>TXT_MSEC
		JSR SPRINT	    	; print
PRENDOK		SEC			; C=1, no error
		RTS			; return if BSY=0 and RDY=DSC=1
	
CFWLPTO		LDX #<TXT_HWERR     	; Print HW error
		LDY #>TXT_HWERR
		JSR SPRINT	    	; print		
		LDA CFREG7		; Status register
		JSR HEXOUT		; Print and return
		CLC			; C=0, error
		RTS			; return

;----------------------------------------------------------------------------
; This routine prints a string to the terminal: X=LSB, Y=MSB
;----------------------------------------------------------------------------
SPRINT		STX PSTR	    	; LSB of text-pointer
		STY PSTR+1	    	; MSB of text-pointer
		JMP STROUT	    	; BIOS print string routine
		
;----------------------------------------------------------------------------
; Text strings for printing
;----------------------------------------------------------------------------
TXT_TITLE	.by 	CR 'MKBOOT CF-IDE 0.2 for DOS-65' CR CR 
		.by 	'Insert CF-Card and press <ENTER>' $00
TXT_PART	.by 	CR 'Partition ' $00
TXT_FMT		.by 	' format is ' $00	
TXT_WRBB	.by 	'. Write Boot Block? (y/n)' $00
TXT_ERR		.by 	CR 'Error: CF-Card not detected / Incompatible CF-Card.' $00
TXT_REI		.by 	CR CR 'Reinsert System Drive!' $00
TXT_JCOS	.by 	'JCOS    ' 
TXT_MBR		.by 	'Reading MBR (sector 0)' CR $00
TXT_OEM		.by	'Writing OEM name (JCOS) to Volume ID sector' CR $00
TXT_FAT32C	.by	CR 'Writing FAT32 boot-code to Volume ID sector' CR $00
TXT_MBRC	.by	'Writing boot-code to MBR (sector 0)' CR $00
TXT_PARTSEC	.by 	'Reading Volume ID of Partition 1 at sector $' $00
TXT_HWERR       .by	'Could not Reset CF-card, Status=$' $00
TXT_RSTOK       .by     'CF-Card Reset after ' $00
TXT_MSEC	.by	'0 msec.' CR $00
TXT_CFDEV	.by	'CFC1_' $00
TXT_SER		.by     '  Serial: ' $00
TXT_FW		.by     'Firmware: ' $00
TXT_MOD		.by     '   Model: ' $00
TXT_ERR_DEV1	.by	'Error: ' $00
TXT_ERR_DEV2	.by	' , could not add CF Device Driver' CR $00
TXT_ERR_DEV3	.by	'Could not open CF Device Driver' CR $00
TXT_PROGRESS	.by	'Boot-block loaded OK' CR $00
TXT_BFLAG	.by	'Writing boot-flag $80 to Partition ' $00
TXT_BFLAGE	.by	' in MBR' CR $00
TXT_WRVOLID	.by	'Writing back Volume ID sector to CF-card' CR $00
TXT_WRMBR	.by	'Writing back MBR (sector 0) to CF-card' CR $00

FAT16_BLK	.word FAT16_BLOCK.MAIN
		.word FAT32_BLK - FAT16_START
FAT16_START
;-------------------------------------------------------------------------------------------------------
FAT16_BLOCK	.local, $063E
;-------------------------------------------------------------------------------------------------------
MAIN		LDA 	YSAV		    ; get current device id
                STA     D_DEV_ID            ; save device id to descriptor
                LDA     Sectors_Per_Cluster ; get Sectors Per Cluster
                STA     D_SECT_PER_CLST     ; save Sectors Per Cluster to descriptor
                LDA     Medium_Descr_Byte   ; get Medium_Descriptor_Byte
                STA     D_MEDIUM_DESCR      ; save Medium_Descriptor_Byte to descriptor
                LDA     File_System_Type+4  ; get file system type (6 = FAT16, 2 = FAT12)
                CMP     #'6'                ; is it FAT16?
                BEQ     SET_FAT16           ; yes, set FAT type to FAT16
		
                LDA     Num_Log_Sectors     ; no, its FAT12...
                STA     D_PART_SIZE         ; so set partition size to...
                LDA     Num_Log_Sectors+1   ; number of logical sectors...
                STA     D_PART_SIZE+1
                LDX     #FAT12_Type         ; format type is FAT12
                BNE     SET_FAT_TYPE        ; branch always
		
SET_FAT16       LDX     #FAT16_Type         ; set FAT16 type ID
SET_FAT_TYPE    STX     D_FAT_TYPE          ; store FAT type
		LDX     #<D_PART_START      ; load pointer to Partition_Start_Sector
                LDY     #>D_PART_START
                JSR     LOAD_32             ; NUM32 = D_PART_START
                LDX     Reserved_Sectors    ; load low byte of Reserved_Sectors
                LDY     Reserved_Sectors+1  ; load high byte of Reserved_Sectors
                JSR     ADD_32_16           ; NUM32 += Reserved_Sectors
                LDX     #<D_START_FAT1
                LDY     #>D_START_FAT1
                JSR     STORE_32            ; D_START_FAT1 = D_PART_START + Reserved_Sectors
                LDX     Sectors_Per_FAT16   ; load low byte of Sectors_Per_FAT
                LDY     Sectors_Per_FAT16+1 ; load high byte of Sectors_Per_FAT
                JSR     ADD_32_16           ; add Sectors_Per_FAT to Start_Of_FAT1 Pointer
                LDX     #<D_START_FAT2
                LDY     #>D_START_FAT2
                JSR     STORE_32            ; D_START_FAT2 = D_START_FAT1 + Sectors_Per_FAT16
                LDY     Number_of_FATs      ; get Number_of_FATs
                STY     D_NUM_OF_FAT        ; save Number_of_FATs to descriptor
                DEY
                BNE     LOOP_CALC_ROOT
		
                JMP     BOOT_ERR_MSG        ; error if number of FATs is zero
		
LOOP_CALC_ROOT  JSR     ADD_32_32           ; add Sectors_Per_FAT to Start_Of_FAT1 Pointer
                DEY                         ; until Number_Of_FATs is 0
                BNE     LOOP_CALC_ROOT
		
		LDX     #<D_START_DIR
                LDY     #>D_START_DIR
                JSR     STORE_32            ; and store result as Start_Of_Root_Dir Pointer
                LDX     Num_Root_Dir_Entr   ; load low byte of Num_Root_Dir_Entries
                STX     D_NUM_ROOT_DIR      ; save Num_Root_Dir_Entries_low to descriptor
                LDY     Num_Root_Dir_Entr+1 ; load high byte of Num_Root_Dir_Entries
                STY     D_NUM_ROOT_DIR+1    ; save Num_Root_Dir_Entries_high to descriptor
		STX     SUM32		    ; calculate start cluster
                STY     SUM32+1
                LDX     #$04
LOOP_CALC_SC    LSR     SUM32+1             ; divide Num_Of_Root_Dir_Entries by 16 (512 = 32 * 16)
                ROR     SUM32               ; to calculate number of sectors
                DEX
                BNE     LOOP_CALC_SC
                STX     SUM32+2
                STX     SUM32+3
                JSR     ADD_32_32           ; and add result to Start_Of_Root_Dir
                LDX     #<D_START_CLUSTER
                LDY     #>D_START_CLUSTER
                JSR     STORE_32            ; save result as Start_Of_Clusters to descriptor
                
LOAD_ROOT_DIR   LDA     #<DIR_BLK_BUF	    ; buffer ($0200-$03FF) for boot-sector
                STA     BLKBUF
                STA     PSTR
                LDA     #>DIR_BLK_BUF
                STA     BLKBUF+1
                STA     PSTR+1
                LDX     #<D_START_DIR
                LDY     #>D_START_DIR
                JSR     DEV_RD_LBLK         ; read first block of root directory

READ_DIR_ENTRY  LDY     #$00
READ_FIRST_CHR  LDA     (PSTR),Y            ; read first char of filename
                BEQ     BOOT_ERR_MSG        ; is entry empty? Yes, BOOT.SYS not found
		
READ_NAME       LDA     (PSTR),Y
                CMP     #$20		    ; Is it a space? Then dir. entry is empty
                BCC     GET_NEXT_ENTRY
		
                CMP     #'a'
                BCC     COMPARE_CHR         ; is it a upper case char?
		
                AND   	#$DF	 	    ; no, convert to uppercase char
COMPARE_CHR     CMP     BOOT_FILE,Y	    ; compare with 'BOOT    SYS'
                BNE     GET_NEXT_ENTRY	    ; branch if no match
                INY
                CPY     #$0B		    ; all chars checked?
                BCC     READ_NAME	    ; branch if not all checked

		; boot.sys is the 1st file, now load it into BOOT_CODE ($2000)
                LDA     #<BOOT_CODE         ;
                STA     BLKBUF		    ; LSB
                LDA     #>BOOT_CODE         ;
                STA     BLKBUF+1	    ; MSB
                
                SEC
                LDY     #$1A		    ; LSB of starting cluster
                LDA     (PSTR),Y	    ; 
                SBC     #$02		    ; 
                STA     STOL		    ; STOL = starting cluster - 2
                INY			    ; MSB of starting cluster
                LDA     (PSTR),Y
                SBC     #$00
                STA     STOH		    ; STOH/STOL = starting cluster - 2
                LDA     D_SECT_PER_CLST	    ; sectors/cluster
CALC_OFFS       LSR     
                BEQ     CALC_CLUSTER	    ; branch if done multiplying with sectors/cluster
		
                ASL     STOL
                ROL     STOH		    ; STOH/STOL = 
                JMP     CALC_OFFS
		
CALC_CLUSTER    LDX     STOL
                LDY     STOH
                JSR     ADD_32_16	    ;
                LDX     #<NUM32
                LDY     #>NUM32
                JSR     DEV_RD_LBLK         ; read BOOT.SYS file in memory
                JSR     SWITCH_TO_RAM       ; switch $B000..$DFFF to RAM bank
                JMP     BOOT_CODE           ; jump to boot code

DEV_RD_LBLK     LDA     #CMD_READ	    ; Read from CF-card
                JMP     CMDDEV

; Get next directory entry (each entry consists of 32 bytes)
GET_NEXT_ENTRY  CLC			     ; get next. dir. entry
                LDA     #32
                ADC     PSTR
                STA     PSTR
                LDA     #$00
                ADC     PSTR+1
                STA     PSTR+1
                CMP     #$04		    ; 512 bytes processed?
                BCC     READ_DIR_ENTRY	    ; branch if buffer not completely processed
                
; Print Boot Error message and exit
BOOT_ERR_MSG    LDA     #<MSG               ; no BOOT.SYS file found
                STA     PSTR                ; load error message
                LDA     #>MSG
                STA     PSTR+1
                JSR     STROUT              ; and print it
                JSR     CIN                 ; wait for any key pressed
                JMP     MON_COLD_START      ; and reset
                
; **** Load a 32 Bit Value Into SUM32 ******************************************
; Input:  X,Y = Ptr[LO:HI] to 32 Bit Integer
; ******************************************************************************
LOAD_S32        STX     STOL
                LDX     #$07
                BNE     LOAD_32_1

; **** Load a 32 Bit Value Into NUM32 ******************************************
; Input:  X,Y = Ptr[LO:HI] to 32 Bit Integer
; ******************************************************************************
LOAD_32         STX     STOL
                LDX     #$03
LOAD_32_1       STY     STOH
                LDY     #$03
LOOP_LOAD_32    LDA     (STOL),Y
                STA     NUM32,X
                DEX
                DEY
                BPL     LOOP_LOAD_32
                RTS

; Input:  X,Y = Ptr[LO:HI] to 32 Bit Integer
STORE_32        STX     STOL
                STY     STOH
                LDY     #$03
LOOP_STORE_32   LDA     NUM32,Y
                STA     (STOL),Y
                DEY
                BPL     LOOP_STORE_32
                RTS
                
; Input X,Y = 16 Bit Integer, NUM32[0:3] = 32 Bit Integer - NUM32 = UInt[X,Y]+NUM32
ADD_32_16       STX     SUM32
                STY     SUM32+1
                LDX     #$00
                STX     SUM32+2
                STX     SUM32+3
                
; Input SUM32[0:3] = 32 Bit Integer, NUM32[0:3] = 32 Bit Integer  - NUM32 = SUM32+NUM32
ADD_32_32       CLC
                LDX     #$00
                PHP
ADD_LOOP        PLP
                LDA     SUM32,X
                ADC     NUM32,X
                STA     NUM32,X
                PHP
                INX
                CPX     #$04
                BNE     ADD_LOOP
                PLP
                RTS

; Boot Messages
MSG		.by CR CR ' No valid boot disk'
		.by CR    ' Press any key to restart' $00
BOOT_FILE	.by 'BOOT    SYS'

		ORG BOOTBLK_TAG

		.byte $55,$AA			; FAT signature at offset 510-511
;-------------------------------------------------------------------------------------------------------
.endl
;-------------------------------------------------------------------------------------------------------

FAT32_BLK	.word FAT32_BLOCK.MAIN
		.word MBRBOOT_BLK - FAT32_START
FAT32_START
;-------------------------------------------------------------------------------------------------------
; This is the FAT32 boot-code that is written into the Volume ID (Partition begin LBA) sector.
; Called from MAIN in BIOS with Carry set by a JMP BLOCK_BUF ($0600) -> BMI FAT32_BLOCK.MAIN
FAT32_BLOCK	.local, $065A
;-------------------------------------------------------------------------------------------------------
MAIN		LDA 	YSAV			; get current device id
		STA 	D_DEV_ID		; save device id to descriptor
		LDA 	Medium_Descr_Byte  	; get Medium_Descriptor_Byte
		STA 	D_MEDIUM_DESCR     	; save Medium_Descriptor_Byte to descriptor
SET_FAT32	LDX 	#FAT32_Type        	; set FAT type ID
		STX 	D_NUM_ROOT_DIR      	; set number of root dir entries to 0
		STX 	D_NUM_ROOT_DIR+1    	;
SET_FAT_TYPE	STX 	D_FAT_TYPE          	; store FAT type
		LDX 	#<D_PART_START     	; load pointer to Partition_Start_Sector
		LDY 	#>D_PART_START     	;
                JSR     LOAD_32             	; load Start_Sector value
                LDX     Reserved_Sectors    	; load low byte of Reserved_Sectors
                LDY     Reserved_Sectors+1  	; load high byte of Reserved_Sectors
                JSR     ADD_32_16           	; add Reserved_Sectors to Start_Sector
                LDX     #<D_START_FAT1
                LDY     #>D_START_FAT1
                JSR     STORE_32            	; store result to Start_Of_FAT1 Pointer
                LDX     #<Sectors_Per_FAT32 	; load low byte of Sectors_Per_FAT
                LDY     #>Sectors_Per_FAT32 	; load high byte of Sectors_Per_FAT
                JSR     LOAD_S32
                JSR     ADD_32_32           	; add Sectors_Per_FAT to Start_Of_FAT1 Pointer
                LDX     #<D_START_FAT2
                LDY     #>D_START_FAT2
                JSR     STORE_32            	; and store result to Start_Of_FAT2 Pointer
                LDY     Number_of_FATs      	; get Number_of_FATs
                STY     D_NUM_OF_FAT        	; save Number_of_FATs to descriptor
                DEY
                BNE     LOOP_CALC_ROOT
                JMP     BOOT_ERR_MSG        	; error if number of FATs is zero
LOOP_CALC_ROOT  JSR     ADD_32_32           	; add Sectors_Per_FAT to Start_Of_FAT1 Pointer
                DEY                         	; until Number_Of_FATs is 0
                BNE     LOOP_CALC_ROOT
SET_CLSTR_START LDX     #<D_START_CLUSTER
                LDY     #>D_START_CLUSTER
                JSR     STORE_32            	; and store result as Start_Of_Clusters to descriptor
                LDX     #$03
SET_DIR_START   LDA     Dir_Start_Cluster,X 	; copy directory start cluster into volume descriptor
                STA     D_START_DIR,X
                STA     NUM32,X             	; and NUM32[0:3]
                DEX
                BPL     SET_DIR_START

; calc LBA of first directory block ********************************************
		SEC                 		; NUM32 = D_START_DIR - 2
		LDA 	#$02            	;
		TAX                 		;
		SBC 	NUM32             	;
		STA 	NUM32             	;
		LDA 	#$00            	;
SUB_OFFS	SBC 	NUM32,X           	;
		STA 	NUM32,X           	;
		DEX                 		;
		BPL 	SUB_OFFS           	;

		LDA 	Sectors_Per_Cluster  	; get Sectors Per Cluster
		STA 	D_SECT_PER_CLST    	; load number of sectors per cluster
		LSR                		;
		BEQ 	ADD_CLUSTER       	; more than one sector?

LOOP1		LDX 	#$00            	; yes, multiply directory start cluster
		LDY 	#$04            	; with number of sectors per cluster
		CLC                 		;
LOOP2		ROL 	NUM32,X           	;
		INX                 		;
		DEY                 		;
		BNE 	LOOP2           	;

		LSR                		;
		BNE 	LOOP1           	;

ADD_CLUSTER	LDX     #<D_START_CLUSTER
                LDY     #>D_START_CLUSTER
                JSR     LOAD_S32            	; load start cluster LBA
                JSR     ADD_32_32           	; and add it to directory start LBA

; load first root directory block **********************************************
LOAD_ROOT_DIR   LDA     #<DIR_BLK_BUF   	; set read buffer to DIR_BLK_BUFF
                STA     BLKBUF
                STA     PSTR
                LDA     #>DIR_BLK_BUF
                STA     BLKBUF+1
                STA     PSTR+1
                LDX     #<NUM32
                LDY     #>NUM32
                JSR     DEV_RD_LBLK          	; read first block of root directory

READ_DIR_ENTRY  LDY     #$00
READ_FIRST_CHR  LDA     (PSTR),Y            	; read first char of filename
                BEQ     BOOT_ERR_MSG        	; is entry empty? Yes, BOOT.SYS not found

READ_NAME       LDA     (PSTR),Y
                CMP     #$20
                BCC     GET_NEXT_ENTRY
                CMP     #$61
                BCC     COMPARE_CHR         	; is it a upper case char?
                AND   	#$DF	 	    	; no, convert to uppercase char
COMPARE_CHR     CMP     BOOT_FILE,Y
                BNE     GET_NEXT_ENTRY
                INY
                CPY     #$0B
                BCC     READ_NAME

                LDA     #<BOOT_CODE     	;
                STA     BLKBUF
                LDA     #>BOOT_CODE     	;
                STA     BLKBUF+1
		SEC
		
		LDY 	#$1A            	;
		LDA 	(PSTR),Y         	;
		SBC 	#$02            	;
		STA 	STOL             	;
		INY                 		;
		LDA 	(PSTR),Y         	;
		SBC 	#$00            	;
		STA 	STOH             	;
		LDA 	D_SECT_PER_CLST         ;
CALC_OFFS	LSR                		;
		BEQ 	CALC_CLUSTER           	;

		ASL 	STOL             	;
		ROL 	STOH             	;
		JMP 	CALC_OFFS          	;

CALC_CLUSTER	LDX 	STOL             	;
		LDY 	STOH             	;
                JSR     ADD_32_16
                LDX     #<NUM32
                LDY     #>NUM32
                JSR     DEV_RD_LBLK         	; read first block of BOOT.SYS
                JSR     SWITCH_TO_RAM       	; switch $B000..$DFFF to RAM bank
                JMP     BOOT_CODE           	; jump to boot code

DEV_RD_LBLK     LDA     #CMD_READ
                JMP     CMDDEV

GET_NEXT_ENTRY  CLC
                LDA     #32
                ADC     PSTR
                STA     PSTR
                LDA     #$00
                ADC     PSTR+1
                STA     PSTR+1
                CMP     #$04
                BCC     READ_DIR_ENTRY
                
BOOT_ERR_MSG    LDA     #<MSG              	; no BOOT.SYS file found
                STA     PSTR                	; load error message
                LDA     #>MSG
                STA     PSTR+1
                JSR     STROUT              	; and print it
                JSR     CIN                 	; wait for any key pressed
                JMP     MON_COLD_START         	; and reset
                
; **** Load a 32 Bit Value Into SUM32 ******************************************
; Input:  X,Y = Ptr[LO:HI] to 32 Bit Integer
; ******************************************************************************
LOAD_S32        STX     STOL
                LDX     #$07
                BNE     LOAD_32_1

; **** Load a 32 Bit Value Into NUM32 ******************************************
; Input:  X,Y = Ptr[LO:HI] to 32 Bit Integer
; ******************************************************************************
LOAD_32         STX     STOL
                LDX     #$03
LOAD_32_1       STY     STOH
                LDY     #$03
LOOP_LOAD_32    LDA     (STOL),Y
                STA     NUM32,X
                DEX
                DEY
                BPL     LOOP_LOAD_32
                RTS

; Input:  X,Y = Ptr[LO:HI] to 32 Bit Integer
STORE_32        STX     STOL
                STY     STOH
                LDY     #$03
LOOP_STORE_32   LDA     NUM32,Y
                STA     (STOL),Y
                DEY
                BPL     LOOP_STORE_32
                RTS

; Input X,Y = 16 Bit Integer, NUM32[0:3] = 32 Bit Integer - NUM32 = UInt[X,Y]+NUM32
                
ADD_32_16       STX     SUM32
                STY     SUM32+1
                LDX     #$00
                STX     SUM32+2
                STX     SUM32+3

; Input SUM32[0:3] = 32 Bit Integer, NUM32[0:3] = 32 Bit Integer  - NUM32 = SUM32+NUM32
                
ADD_32_32       CLC
                LDX     #$00
                PHP
ADD_LOOP        PLP
                LDA     SUM32,X
                ADC     NUM32,X
                STA     NUM32,X
                PHP
                INX
                CPX     #$04
                BNE     ADD_LOOP
                PLP
                RTS

; Boot Messages
MSG		.by CR CR ' No valid boot disk' CR
		.by       ' Press any key to restart' $00
BOOT_FILE	.by 'BOOT    SYS'

		ORG BOOTBLK_TAG
		.byte $55,$AA				; FAT signature at offset 510-511
;-------------------------------------------------------------------------------------------------------
.endl
;-------------------------------------------------------------------------------------------------------

MBRBOOT_BLK	.word MBRBOOT_BLOCK.MAIN
		.word MBRBOOT_END - MBRBOOT_START
MBRBOOT_START
;-------------------------------------------------------------------------------------------------------
; This routine is called from within the BIOS SD_BOOT routine with a JSR MBR. It should return with 
; the partition number to boot from in X, which is a number from 1 to 4.
;
MBRBOOT_BLOCK	.local, $0600
;-------------------------------------------------------------------------------------------------------
MAIN            LDX     #$00
                STX     TEMP				; 0 = only 1 partition available
                INX					; Start checking partition 2
                STX     PSAV				; Partition number [1..4]
CHK_PART        JSR     CHK_PART_AVAIL			; Check if partitions 2,3,4 are available
                BCC     CHK_NEXT			; Branch if partition is not available
		
                INC     TEMP				; TEMP=1,2,3
CHK_NEXT        INX					; Next partition to check X=2,3,4
                CPX     #$04				; All partitions checked?
                BNE     CHK_PART			; branch if more partitions to check
                
                LDA     TEMP				; 0 = only 1 partition available
                BNE     MAIN2				; branch if more partitions are available
		
		LDX	#< MSG3				; Print 'Booting from partition 1'
		LDY	#> MSG3
		JSR     SPRINT				; Print it
                LDX     PSAV				; X = partition number 1
                RTS					; Return
            
		; More partitions are available. Present a boot-menu
MAIN2           LDX	#< MSG				; Print 'Enter boot partition'
		LDY	#> MSG
		JSR     SPRINT				; Print it
                LDX     #$00
                STX     TEMP				; Reset TEMP
L1              JSR     CHK_PART_AVAIL			; Check partition 0
                INX					; X=1
                BCC     L2				; branch if partition is not available
		
                LDA     TEMP				; 1= there's already a previous boot-partition
                BEQ     L3				; branch if this is the 1st boot-partition
		
                LDA     #','
                JSR     COUT				; Print ',' 
L3              STX     TEMP				; Save boot-partition
                TXA
                CLC
                ADC     #'0'				; Partition number as ASCII
                JSR     COUT				; Print it
		
L2              CPX     #$04				; All partitions done?
                BNE     L1				; branch if more partitions to check
		
		LDX	#< MSG2				; Print '). Press ESC to abort.'
		LDY	#> MSG2
		JSR     SPRINT				; Print it
READ_CHAR       JSR     CIN				; Get key
                CMP     #ESC				; Escape?
                BNE     L4				; branch if not Esc
		
                LDX     #$00				; X=0, ESC pressed, no boot-partition
                RTS					; return
                
L4              SEC					;
                SBC     #'0'				; key = key - '0'
                TAX					
                DEX					; X = [0..3]
                JSR     CHK_PART_AVAIL			; Check if partition is available
                BCC     READ_CHAR			; branch if C=0 (error): partition not available
		
                INX					; X = [1..4]
                RTS					; return with partition-number in X

;----------------------------------------------------------------------------
; This routine checks if a partition is available.
; Input : X: [0..3], the partition to check
; Output: C=0 (Error), no partition. C=1 (OK), partition is available.
;----------------------------------------------------------------------------
CHK_PART_AVAIL  TXA					; A = partition nr. [0..3]
                CMP     #$04				;
                BCS     NO_PART_AVAIL			; branch if partition nr > 3

                ASL     				;
                ASL     				;
                ASL     				;
                ASL     				; A = 16 * partition nr
                TAY					; Y = 16 * partition nr
                LDA     PART0_START,Y			; LBA-Begin in partition description of 16 bytes
                BNE     PART_AVAIL			; branch if LSB of LBA-Begin > 0
		
                LDA     PART0_START+1,Y			; Get 2nd byte of LBA-Begin
                BNE     PART_AVAIL			; branch if > 0

                LDA     PART0_START+2,Y			; Get 3rd byte of LBA-Begin
                BNE     PART_AVAIL			; branch if > 0

                LDA     PART0_START+3,Y			; Get MSB of LBA-Begin
                BNE     PART_AVAIL			; branch if > 0

NO_PART_AVAIL   CLC					; All 4 bytes of LBA Begin are 0, no partition
                RTS					; C=0 (Error) and return
PART_AVAIL      SEC					; LBA Begin > 0, partition is available
                RTS					; C=1 (OK) and return
                
;----------------------------------------------------------------------------
; This routine prints a string to the terminal: X=LSB, Y=MSB
;----------------------------------------------------------------------------
SPRINT		STX 	PSTR	    			; LSB of text-pointer
		STY 	PSTR+1	    			; MSB of text-pointer
		JMP 	STROUT	    			; BIOS print string routine
                
MSG             .by     CR CR ' Enter boot partition (' $00
MSG2            .by	'). Press ESC to abort.' $00
MSG3            .by	CR ' Partition 1 bootable' $00

                ORG     $07BC
                .byte   $65,$02
;-------------------------------------------------------------------------------------------------------
.endl
;-------------------------------------------------------------------------------------------------------
MBRBOOT_END