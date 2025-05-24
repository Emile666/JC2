;------------------------------------------------------------------------------
; Junior Computer ][ BIOS & Monitor CONSTANTS
;------------------------------------------------------------------------------ 

; PIA Register Indices *********************************************************
PIA_PORTA       EQU     $0C             ; Port A input/output register
PIA_PORTB       EQU     $0D             ; Port B input/output register
PIA_PORTC       EQU     $0E             ; Port C input/output register
PIA_CONTROL     EQU     $0F             ; Control/Setup register

; Memory Management Unit (MMU) Register Indices ********************************
BIOS_EN		EQU	$01		; PORTB bit 0: 1 = enable BIOS at $E000
MON_EN		EQU	$02		; PORTB bit 1: 1 = enable Monitor at $1C00
BAS_EN		EQU	$80		; PORTB bit 7: 1 = enable BASIC at $B000

; VIA 1 Register Indices *******************************************************
VIA_PORTB     	EQU  	$00  		; Port B input/output register
VIA_PORTA     	EQU  	$01  		; Port A input/output register
VIA_DDRB   	EQU  	$02		; Port B data direction register
VIA_DDRA   	EQU  	$03		; Port A data direction register
VIA_T1CL   	EQU  	$04		; Timer 1 counter low byte register
VIA_T1CH   	EQU  	$05		; Timer 1 counter high byte register
VIA_T1LL   	EQU  	$06		; Timer 1 latched counter low byte register
VIA_T1LH   	EQU  	$07		; Timer 1 latched counter high byte register
VIA_T2CL   	EQU  	$08		; Timer 2 counter low byte register
VIA_T2CH   	EQU  	$09		; Timer 2 counter high byte register
VIA_SR     	EQU  	$0A		; Shift register
VIA_ACR    	EQU  	$0B		; Auxilary control register
VIA_PCR    	EQU  	$0C		; Periheral control register
VIA_IFR    	EQU  	$0D		; Interrupt flag register
VIA_IER    	EQU  	$0E		; Interrupt enable register
VIA_PANOHS 	EQU  	$0F

; VIA 2 Register Indices *******************************************************
PORTB     	EQU  	$10  		; Port B input/output register
PORTA     	EQU  	$11  		; Port A input/output register
DDRB   		EQU  	$12		; Port B data direction register
DDRA   		EQU  	$13		; Port A data direction register
T1CL   		EQU  	$14		; Timer 1 counter low byte register
T1CH   		EQU  	$15		; Timer 1 counter high byte register
T1LL   		EQU  	$16		; Timer 1 latched counter low byte register
T1LH   		EQU  	$17		; Timer 1 latched counter high byte register
T2CL   		EQU  	$18		; Timer 2 counter low byte register
T2CH   		EQU  	$19		; Timer 2 counter high byte register
SR     		EQU  	$1A		; Shift register
ACR    		EQU  	$1B		; Auxilary control register
PCR    		EQU  	$1C		; Peripheral control register
IFR    		EQU  	$1D		; Interrupt flag register
IER    		EQU  	$1E		; Interrupt enable register
PANOHS 		EQU  	$1F

CENTURY		EQU	$20		; the 20th century. change to travel in time
DATEDIV		EQU	'.'		; divider char for date string
TIMEDIV		EQU	':'		; divider char for time string
PROMPT    	EQU     '*'    		; prompt character
ADIV      	EQU     '-'    		; address divider
SOH       	EQU     $01    		; start of header
EOT       	EQU     $04    		; end of text
ACK       	EQU     $06    		; acknowledged
BS        	EQU     $08    		; backspace key
CR        	EQU     $0D    		; carriage return
LF	  	EQU     $0A    		; line feed
NAK       	EQU     $15    		; not acknowledged
CAN		EQU     $18		; Cancel
ESC       	EQU     $1B    		; ESC
SPC		EQU     $20		; space char

; Tape Reader/Writer Constants *************************************************
SYNCMARK	EQU	$2E		; synchronisation mark
NAMEMARK	EQU	$A0		; start of name mark
ADDRMARK	EQU	$A1		; start of address mark
FILEMARK	EQU	$1F		; start of file mark

RPTIME		EQU	49		; read point time   49x8uS     = 392uS
LPTIME		EQU	190		; long pulse time   190uS+50uS = 240uS
SPTIME		EQU	60		; short pulse time  60uS+50uS  = 120uS

; Device Driver Constants ******************************************************
KBD_MAGIC_NUM   EQU	129             ; magic number of keyboard ROM

COM_DEV	        EQU	$10             ; COM devices base ID
STORAGE_DEV	EQU	$20             ; STORAGE devices base ID

NULL_ID		EQU	$00             ; the NULL device

TTY1_ID		EQU	COM_DEV+0       ; TTY 1 device ID
PRINTER1_ID	EQU	COM_DEV+1       ; Printer 1 device ID
PRINTER2_ID	EQU	COM_DEV+2       ; Printer 2 device ID
KEYBD1_ID	EQU	COM_DEV+3       ; Keyboard 1 device ID (ASCII Keyboard)
KEYBD2_ID	EQU	COM_DEV+4       ; Keyboard 2 device ID (PS/2 Keyboard)
VDP1_ID         EQU     COM_DEV+5       ; Video Display Processor device ID

XMODEM1_ID	EQU	STORAGE_DEV+0   ; XModem device ID
TAPE1_ID	EQU	STORAGE_DEV+1   ; Tape 1 device ID
FDD1_ID         EQU     STORAGE_DEV+2   ; Floppy drive 1 device ID
FDD2_ID         EQU     STORAGE_DEV+3   ; Floppy drive 2 device ID
SDC1_ID		EQU	STORAGE_DEV+4   ; SD-Card device ID
HDD1_ID		EQU	STORAGE_DEV+5   ; Harddisk 1 device ID (CF-card)
HDD2_ID		EQU	STORAGE_DEV+6   ; Harddisk 2 device ID

; Device Command Constants *****************************************************
CMD_INIT	EQU	0               ; Init device
CMD_IDENTIFY	EQU	1               ; Identify device
CMD_NORMAL	EQU	2               ; Set normal text
CMD_INVERSE	EQU	3               ; Set inverse text
CMD_FLASH	EQU	4               ; Set blinking text
CMD_HOME	EQU	5               ; Set cursor to home position
CMD_CLRLINE	EQU	6               ; Clear line at cursor
CMD_CLRSCRN	EQU	7               ; Clear screen
CMD_SETCURSOR	EQU	8               ; Set cursor position X,Y

CMD_SETSTARTADR	EQU	16              ; Set start address [X:Y]
CMD_SETENDADR	EQU	17              ; Set end address [X:Y]

CMD_LOAD	EQU	32              ; Load data byte from device
CMD_SAVE	EQU	33              ; Save data byte to device
CMD_READ	EQU	34              ; Read data block from device
CMD_WRITE	EQU	35              ; Write data block to device
CMD_BOOT        EQU     36              ; Boot from device
CMD_READ_BUF	EQU	37              ; Read data block from device to standard buffer
CMD_WRITE_BUF	EQU	38              ; Write data block to device from standard buffer

; SD Command Constants *********************************************************
CMD0		EQU	$40		; Reset SD-Card
CMD1		EQU	CMD0 + 1	; Initialize MMC/SDC
CMD8		EQU	CMD0 + 8	; Check voltage range
CMD13		EQU	CMD0 + 13	;
CMD16		EQU	CMD0 + 16	; Change block size
CMD17		EQU	CMD0 + 17	; Read single block
CMD24		EQU	CMD0 + 24	; Write single block
CMD55		EQU	CMD0 + 55	; Application command prefix
CMD58		EQU	CMD0 + 58	; Get OCR
ACMD41		EQU	CMD0 + 41	; Initialize SDC

DATA_TOKEN	EQU	$FE             ; SD-Card Data Token

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

; Block Device Constants *******************************************************
MOUNT_TABLE     EQU     $0400           ; Table of mounted devices
BOOT_PART       EQU     MOUNT_TABLE     ; Boot Medium Descriptor
BLOCK_BUF	EQU	$0600           ; Block Device Block Buffer
MBR             EQU     BLOCK_BUF       ; Master Boot Block Code
PART0		EQU	$07BE		; Partition 0 start
PART0_RS	EQU	PART0 + 8 	; Partition 0 relative sector field
PART0_SIZE	EQU	PART0 + 16	; Partition 0 sector size field

BOOTBLK_TAG     EQU     $07FE           ; Address of Boot Block Tag ($55 $AA)

; Miscellaneous Constants ******************************************************
I2C_RTC_ADR	EQU	$68		; I2C address of DS1307 Real Time Clock

FGC_BASE        EQU     $1000
FGC_INFO        EQU     FGC_BASE+$10
FGC_SET_PAGE    EQU     FGC_BASE+$13
FGC_FDC_CMD     EQU     FGC_BASE+$17
FGC_VPU_CMD     EQU     FGC_BASE+$1F
FGC_VPU_OUT     EQU     FGC_BASE+$27

VPU_PORT1       EQU     FGC_BASE+$09    ; VPU Port 1
VPU_REG0        EQU     $80             ; VPU register 0
VPU_REG15       EQU     VPU_REG0+15     ; VPU status register pointer
VPU_STAT0       EQU     0               ; VPU status register 0

;------------------------------------------------------------------------------
; Junior Computer ][ BIOS & Monitor Address defines
;------------------------------------------------------------------------------ 

; Card Base Addresses **********************************************************
IOBASE		EQU	$14		; pointer to IO card base
IOBASEL		EQU	$14		; always $00
IOBASEH		EQU	$15		; K2 = $08, K3 = $0C, K4 = $10
FGCBASE	        EQU	$16		; pointer to Floppy-/Graphics-Controller card base
FGCBASEL	EQU	$16		; always $00
FGCBASEH	EQU	$17		; (K2 = $08, K3 = $0C,) should always be K4 = $10
CARD3BASE	EQU	$18		; reserved (controller base)
CARD3BASEL	EQU	$18		; always $00
CARD3BASEH	EQU	$19		;K2 = $08, K3 = $0C, K4 = $10

;-----------------------------------------------------------------------------
; Addresses up to $9C are in use by BASIC, see jc2_basic.asm
; Addresses from $A0 up to $AF are in use by DOS and boot.sys
;-----------------------------------------------------------------------------

DEVID		EQU	$C9		; 1-byte temp device id (moved from $03 to avoid Basic collision)
PRSTR		EQU	$CA		; 2-bytes replacement for PSTR
END_PTR		EQU	$CC		; ROM end-pointer (2 bytes), also used by BASIC
ROM_PTR		EQU	$CE		; ROM pointer (2 bytes)
ROM_CS		EQU	$D0		; ROM checksum counter (2 bytes)
CFLBA0          EQU     $D2		; CF-card LBA 0 [0:7]
CFLBA1          EQU     $D3		; CF-card LBA 1 [8:15]
CFLBA2          EQU     $D4		; CF-card LBA 2 [16:23]
CFLBA3          EQU     $D5		; CF-card LBA 3 [24:27]
MSEC		EQU	$D6		; msec time-out counter
RSTACT		EQU	$D7		; 1 = Reset Pending
SAVEX		EQU 	$D8		; Temp. save X register
SAVEY		EQU	$D9		; Temp. save Y register
TICKCNT         EQU     $DA             ; current tick counter
SD_TYPE		EQU	$DB		; SD Card Type: $00 Byte mode, $40 LBA mode
BLKBUF		EQU	$DC             ; pointer to block buffer
BLKBUFL		EQU	$DC             ; lower byte of block buffer pointer
CHECKSUM	EQU	$DC		; TAPE file checksum
CRCL      	EQU   	$DC      	; XModem CRC lo byte
BLKBUFH		EQU	$DD             ; upper byte of block buffer pointer
OUTBYTE		EQU	$DD		; TAPE data byte read/written from/to tape
CRCH      	EQU   	$DD     	; XModem CRC hi byte
SD_CMD		EQU	$DE		; SD-Card CMD Byte
RETRYL    	EQU   	$DE      	; XModem retry counter lo byte
PDEV		EQU	$DE		; device descriptor pointer
PDEVL    	EQU   	$DE      	; device descriptor pointer lo byte
SD_PB3      	EQU   	$DF   		; SD-Card Parameter Byte 3
RETRYH    	EQU   	$DF      	; XModem retry counter hi byte
PDEVH    	EQU   	$DF      	; device descriptor pointer hi byte

SD_PB2		EQU   	$E0   		; SD-Card Parameter Byte 2
BAUDRATE  	EQU   	$E0    		; ACIA current baud rate
OPCODE		EQU	$E0		; DISASSEM current opcode
I2C_DATA	EQU	$E0		; I2C current I2C data byte
BLKEND      	EQU     $E0		; XModem block end flag
KEY     	EQU     $E1   		; MON 1-byte Editor 
SD_PB1      	EQU   	$E1   		; SD-Card Parameter Byte 1
LINECNT		EQU	$E1		; DISASSEM number of disassembled lines
BFLAG     	EQU   	$E1      	; XModem block flag
EQUFLAG		EQU	$E1		; TAPE file name equal flag
BEGADR  	EQU     $E2   		; MON 2-byte Editor Begin Address Pointer
SD_PB0		EQU	$E2   		; SD-Card Parameter Byte 0
ADRMODE		EQU	$E2		; DISASSEM addressing mode
BLKNO     	EQU   	$E2      	; XModem block number
SD_CRC		EQU	$E3   		; SD-Card CRC Byte
IBYTES		EQU	$E3		; DISASSEM instruction byte count
ERRCNT    	EQU   	$E3      	; XModem error counter 10 is the limit
ENDADR  	EQU     $E4   		; MON 2-byte Editor End Address Pointer
VIA_STATUS 	EQU	$E4 		; VIA2 current VIA2 PortB output status
KEY_SENSE	EQU	$E5		; TAPE datasette key sense flag
CURADR  	EQU     $E6   		; MON 2-byte Editor Current Address Pointer
ASCL	  	EQU   	$E6     	; MON ASCII list start address low
PLBA		EQU	$E6		; LBA pointer
PLBAL		EQU	$E6		; LBA pointer low byte
ASCH      	EQU   	$E7     	; MON ASCII list start address high
PLBAH		EQU	$E7		; LBA pointer high byte
CENDADR 	EQU     $E8   		; MON 2-byte Editor Current End Address Pointer
STOL      	EQU   	$E8     	; MON store address Low
STOH      	EQU   	$E9     	; MON store address High
MOVADR  	EQU     $EA   		; MON 2-byte Editor 
PSTR      	EQU   	$EA      	; ACIA output string Pointer
PSTRL     	EQU   	$EA      	; ACIA lower address byte of output string pointer
PSTRH     	EQU   	$EB      	; ACIA upper address byte of output string pointer
TABLEA  	EQU     $EC   		; MON 2-byte Editor 
WBUF      	EQU   	$EC      	; ACIA character output buffer
BITCNT		EQU	$ED		; TAPE current read bit count
LABELS  	EQU     $EE   		; MON 1-byte Editor 
PSAV            EQU     $EE		; MON
PCL		EQU	$EF		; CPU program counter Low

PCH		EQU	$F0		; CPU program counter High
PREG		EQU	$F1		; CPU processor status register
SPUSER		EQU	$F2		; CPU stack pointer
DIVCHAR		EQU	$F2             ; CLOCK current divider char (. or / for date : for time)
ACC		EQU	$F3		; CPU accumulator
YREG		EQU	$F4		; CPU y-register
XREG		EQU	$F5		; CPU x-register
BYTES   	EQU     $F6   		; MON 1-byte Number of bytes to be displayed
COUNT   	EQU     $F7   		; MON 1-byte
DIG0      	EQU   	$F8     	; 10^0 digit, used by NUMOUT and DEC2STR
NUML      	EQU   	$F8     	; MON low number byte
INL     	EQU     $F8   		; MON 1-byte hex display buffer
INH     	EQU     $F9   		; MON 1-byte hex display buffer
DIG1	  	EQU   	$F9	 	; 10^1 digit, used by NUMOUT and DEC2STR
NUMH	  	EQU   	$F9	 	; MON high number byte
POINT   	EQU     $FA   		; MON 2-byte
DIG2      	EQU   	$FA     	; 10^2 digit, used by NUMOUT and DEC2STR
ADRL      	EQU   	$FA     	; MON last address Low
ADRH      	EQU   	$FB     	; MON last address High
TEMP      	EQU   	$FC     	; MON temp storage
TEMPX   	EQU     $FD   		; MON 1-byte temp. data-buffer
YSAV      	EQU   	$FD     	; MON Y register storage
NIBBLE  	EQU     $FE   		; MON 1-byte temp. data-buffer
PDBCNT    	EQU   	$FE     	; MON number of printed data bytes
MODE      	EQU   	$FF     	; MON current edit mode

; IO Base Addresses ************************************************************
K2		EQU	$0800		; Base address of IO select K2
K3		EQU	$0C00		; Base address of IO select K3
K4		EQU	$1000		; Base address of IO select K4

; Buffers **********************************************************************
STRBUF	  	EQU   	$1400    	; input string buffer at $1400
RBUFF     	EQU   	$1500    	; xmodem receive buffer at $1500

; ACIA Registers ***************************************************************
DATA_REG  	EQU   	$1600    	; ACIA Data Register
STAT_REG  	EQU   	$1601    	; ACIA Status Register
COMM_REG  	EQU   	$1602    	; ACIA Command Register
CTRL_REG  	EQU   	$1603    	; ACIA Control Register

; ATF1504 Memory Management Unit (MMU) Register ********************************
MMU		EQU	$1700		; MMU Register

;------------------------------------------------------------------------------
; $1800 - $19FF 512 Bytes, this was previously unused, with the new MMU this
; has become RAM.
;------------------------------------------------------------------------------
PPORTLOAD	EQU	$1800		; jump location for test code
PPORTSAVE	EQU	$1803		; jump location for test code
CF_LOAD_VEC	EQU	$1806		; 2-byte vector for CF-card LOAD vector
CF_SAVE_VEC	EQU	$1808		; 2-byte vector for CF-card SAVE vector
RETURN_VECT     EQU     $180A           ; 2-byte return vector to monitor caller (moved from $01 to avoid Basic collision)

; INPBUF is the BASIC Input Buffer. It is defined from $1868 - $18FF
INPBUF	  	EQU 	$1868		; change input buffer to last 151 bytes in page
ccflag		EQU	INPBUF		; BASIC CTRL-C flag, 00 = enabled, 01 = dis
ccbyte		EQU 	ccflag+1	; BASIC CTRL-C byte
ccnull		EQU 	ccbyte+1	; BASIC CTRL-C byte timeout
VEC_CC		EQU 	ccnull+1	; ctrl c check vector

; Ibuffs can now be anywhere in RAM, ensure that the max length is < $80
Ibuffs		EQU 	VEC_CC+$14	; start of input buffer after IRQ/NMI code
Ibuffe		EQU 	Ibuffs+$7F	; end of input buffer

; RIOT RAM Area ****************************************************************
IO_INFO         EQU     $1A00           ; Initialization info call for IO card

KEY_HANDLER     EQU     $1A24           ; character input handler
NKEY_HANDLER    EQU     $1A26           ; no character input handler
DEVLIST		EQU	$1A28		; start of device driver list

STDBEEP		EQU	$1A66		; current standard beep routine
DEVIN		EQU	$1A68		; current opened device input routine
DEVOUT		EQU	$1A6A		; current opened device output routine
DEVCMD		EQU	$1A6C		; current opened device command routine
STDIN		EQU	$1A6E		; current standard input routine
STDOUT		EQU	$1A70		; current standard output routine
STDCMD		EQU	$1A72		; current standard command routine
STDINDEV  	EQU   	$1A74    	; absolut standard input device id
STDOUTDEV  	EQU   	$1A75    	; absolut standard output device id
STDPRINTDEV	EQU	$1A76		; absolutstandard printer device id
STOACC		EQU	$1A77		; last accumulator before interrupt

; Interrupt Vectors ************************************************************
BRKUSR		EQU	$1A78		; address of user BREAK vector
NMIVECT		EQU     $1A7A           ; address of NMI vector
IRQUSR  	EQU   	$1A7C    	; address of user IRQ vector
IRQVECT  	EQU   	$1A7E    	; address of main IRQ vector

; Port Register ****************************************************************
PAD		EQU     $1A80		; Port A Data Register
PADD		EQU	$1A81		; Port A Data Direction Register
PBD		EQU	$1A82		; Port B Data Register
PBDD		EQU	$1A83		; Port B Data Direction Register

WRDC		EQU	$1A85		; Write = set positive edge detection, no int.
					; Read = get Edge Control Interrupt Register

; Timer Register ***************************************************************
CNTA	  	EQU	$1A94	 	; CLK/1T no interrupt
CNTB	  	EQU	$1A95	 	; CLK/8T no interrupt
CNTC	  	EQU	$1A96	 	; CLK/64T no interrupt
CNTD	  	EQU	$1A97    	; CLK/1024T no interrupt
CNTIRQ		EQU	$1A9F		; Timer Interrupt Register

;------------------------------------------------------------------------------
; MACROS for use with the MAD-Assembler
;------------------------------------------------------------------------------ 
.macro		ADCAW			; Add A to word and store in word
		CLC
		ADC	:1
		STA	:1
		LDA	:1+1
		ADC	#0
		STA	:1+1
.endm

.macro		SBCAW			; Subtract A from word and store in word
		SEC
		SBC	:1
		STA	:1
		LDA	:1+1
		SBC	#0
		STA	:1+1
.endm

.macro		SBWB	b16 b8	      	; Subtract a byte from a word
		SEC
		LDA	:b16
		SBC	:b8
		STA	:b16
		LDA	:b16+1
		SBC	#0
		STA	:b16+1
.endm
		
