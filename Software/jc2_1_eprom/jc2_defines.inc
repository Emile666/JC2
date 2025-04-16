;------------------------------------------------------------------------------
; Junior Computer ][ BIOS & Monitor address defines
;------------------------------------------------------------------------------ 
VERMAIN   	EQU     '1'    		; BIOS main version
VERPSUB    	EQU     '1'    		; BIOS primary sub version
VERSSUB		EQU	'4'		; BIOS secondary sub version

RETURN_VECT     EQU     $0001           ; return vector to monitor caller
RESET     	EQU  	$1C1D       	; original junior monitor reset vector

; Buffers **********************************************************************
STRBUF	  	EQU   	$1400    	; input string buffer at $1400
RBUFF     	EQU   	$1500    	; xmodem receive buffer at $1500

; Card Base Addresses **********************************************************
IOBASE		EQU	$14		; pointer to IO card base
IOBASEL		EQU	$14		; always $00
IOBASEH		EQU	$15		; K2 = $08, K3 = $0C, K4 = $10

FGCBASE	        EQU	$16		; pointer to Floppy-/Graphics-Controller card base
FGCBASEL	EQU	$16		; always $00
FGCBASEH	EQU	$17		; (K2 = $08, K3 = $0C,) should always K4 = $10

CARD3BASE	EQU	$18		; reserved (controller base)
CARD3BASEL	EQU	$18		; always $00
CARD3BASEH	EQU	$19		;K2 = $08, K3 = $0C, K4 = $10

; ******************************************************************************
TICKCNT         EQU     $DA             ; current tick counter

; SPI/SD-Card Reader Variables *************************************************
SD_TYPE		EQU	$DB		; SD Card Type: $00 Byte mode, $40 LBA mode

; ACIA And Terminal Variables **************************************************
PSTR      	EQU   	$EA      	; output string Pointer
PSTRL     	EQU   	$EA      	; lower address byte of output string pointer
PSTRH     	EQU   	$EB      	; upper address byte of output string pointer
WBUF      	EQU   	$EC      	; ACIA character output buffer

; ******************************************************************************
DEVID		EQU	$03		; temp device id

; Device Driver Variables ******************************************************
PDEV		EQU	$DE		; device descriptor pointer
PDEVL    	EQU   	$DE      	; device descriptor pointer lo byte
PDEVH    	EQU   	$DF      	; device descriptor pointer hi byte

; XModem Transfer Variables ****************************************************
CRCL      	EQU   	$DC      	; CRC lo byte
CRCH      	EQU   	$DD     	; CRC hi byte

RETRYL    	EQU   	$DE      	; retry counter lo byte
RETRYH    	EQU   	$DF      	; retry counter hi byte

BLKEND      	EQU     $E0		; block end flag
BFLAG     	EQU   	$E1      	; block flag
BLKNO     	EQU   	$E2      	; block number
ERRCNT    	EQU   	$E3      	; error counter 10 is the limit

; Disassembler Variables *******************************************************
OPCODE		EQU	$E0		; current opcode
LINECNT		EQU	$E1		; number of disassembled lines
ADRMODE		EQU	$E2		; addressing mode
IBYTES		EQU	$E3		; instruction byte count

; I2C Variables ****************************************************************
I2C_DATA	EQU	$E0		; current I2C data byte

; TTY Variables ****************************************************************
BAUDRATE  	EQU   	$E0    		; current baud rate

; Tape Reader/Writer Variables *************************************************
CHECKSUM	EQU	$DC		; file checksum
OUTBYTE		EQU	$DD		; data byte read/written from/to tape

EQUFLAG		EQU	$E1		; file name equal flag

KEY_SENSE	EQU	$E5		; datasette key sense flag
BITCNT		EQU	$ED		; current read bit count

; SD-Card Command Buffer *******************************************************
SD_CMD		EQU	$DE		; CMD Byte
SD_PB3      	EQU   	$DF   		; Parameter Byte 3
SD_PB2		EQU   	$E0   		; Parameter Byte 2
SD_PB1      	EQU   	$E1   		; Parameter Byte 1
SD_PB0		EQU	$E2   		; Parameter Byte 0
SD_CRC		EQU	$E3   		; CRC Byte

BLKBUF		EQU	$DC             ; pointer to block buffer
BLKBUFL		EQU	$DC             ; lower byte of block buffer pointer
BLKBUFH		EQU	$DD             ; upper byte of block buffer pointer

; Pointer to Logical Block Address *********************************************
PLBA		EQU	$E6		; LBA pointer
PLBAL		EQU	$E6		; LBA pointer low byte
PLBAH		EQU	$E7		; LBA pointer high byte

; VIA2 Variables ***************************************************************
VIA_STATUS 	EQU	$E4 		; current VIA2 PortB output status

; Hex Monitor Variables ********************************************************
ASCL	  	EQU   	$E6     	; ASCII list start address low
ASCH      	EQU   	$E7     	; ASCII list start address high

STOL      	EQU   	$E8     	; store address Low
STOH      	EQU   	$E9     	; store address High

NUML      	EQU   	$F8     	; low number byte
NUMH	  	EQU   	$F9	 	; high number byte
ADRL      	EQU   	$FA     	; last address Low
ADRH      	EQU   	$FB     	; last address High
TEMP      	EQU   	$FC     	; temp storage
YSAV      	EQU   	$FD     	; Y register storage
PDBCNT    	EQU   	$FE     	; number of printed data bytes
MODE      	EQU   	$FF     	; current edit mode

PSAV            EQU     $EE

; Number String ****************************************************************
DIG0      	EQU   	$F8     	; 10^0 digit
DIG1	  	EQU   	$F9	 	; 10^1 digit
DIG2      	EQU   	$FA     	; 10^2 digit

; CPU Register Save ************************************************************
PCL		EQU	$EF		; program counter Low
PCH		EQU	$F0		; program counter High
PREG		EQU	$F1		; processor status register
SPUSER		EQU	$F2		; stack pointer
ACC		EQU	$F3		; accumulator
YREG		EQU	$F4		; y-register
XREG		EQU	$F5		; x-register

; Clock Variables **************************************************************
DIVCHAR		EQU	$F2             ; current divider char (. or / for date : for time)

; ACIA Registers ***************************************************************
DATA_REG  	EQU   	$1600    	; ACIA Data Register
STAT_REG  	EQU   	$1601    	; ACIA Status Register
COMM_REG  	EQU   	$1602    	; ACIA Command Register
CTRL_REG  	EQU   	$1603    	; ACIA Control Register

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

; Timer Register ***************************************************************
CNTA	  	EQU	$1A94	 	; CLK/1T no interrupt
CNTB	  	EQU	$1A95	 	; CLK/8T no interrupt
CNTC	  	EQU	$1A96	 	; CLK/64T no interrupt
CNTD	  	EQU	$1A97    	; CLK/1024T no interrupt

CNTIRQ		EQU	$1A9F		; Timer Interrupt Register

; Port Register ****************************************************************
PAD		EQU     $1A80		; Port A Data Register
PADD		EQU	$1A81		; Port A Data Direction Register
PBD		EQU	$1A82		; Port B Data Register
PBDD		EQU	$1A83		; Port B Data Direction Register

WRDC		EQU	$1A85		; Write = set positive edge detection, no int.
					; Read = get Edge Control Interrupt Register

; IO Base Addresses ************************************************************
K2		EQU	$0800		; Base address of IO select K2
K3		EQU	$0C00		; Base address of IO select K3
K4		EQU	$1000		; Base address of IO select K4

; PIA Register Indices *********************************************************
PIA_PORTA       EQU     $0C             ; Port A input/output register
PIA_PORTB       EQU     $0D             ; Port B input/output register
PIA_PORTC       EQU     $0E             ; Port C input/output register
PIA_CONTROL     EQU     $0F             ; Control/Setup register

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

; Pointer To Programming Language Name *****************************************
LANGKEY		EQU     $DFF0
LANGNAME	EQU	LANGKEY+1

; Constants ********************************************************************
CENTURY		EQU	$20		; the 20th century. change to travel in time

DATEDIV		EQU	'.'		; divider char for date string
TIMEDIV		EQU	':'		; divider char for time string

PROMPT    	EQU     '*'    		; prompt character
ADIV      	EQU     '-'    		; address divider
BS        	EQU     $08    		; backspace key
CR        	EQU     $0D    		; carriage return
LF	  	EQU     $0A    		; line feed
CAN		EQU     $18		; Cancel
ESC       	EQU     $1B    		; ESC
SPC		EQU     $20		; space char

SOH       	EQU     $01    		; start of header
EOT       	EQU     $04    		; end of text
ACK       	EQU     $06    		; acknowledged
NAK       	EQU     $15    		; not acknowledged

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
HDD1_ID		EQU	STORAGE_DEV+5   ; Harddisk 1 device ID
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

; Block Device Constants *******************************************************
MOUNT_TABLE     EQU     $0400           ; Table of mounted devices
BOOT_PART       EQU     MOUNT_TABLE     ; Boot Medium Descriptor
BLOCK_BUF	EQU	$0600           ; Block Device Block Buffer
MBR             EQU     BLOCK_BUF       ; Master Boot Block Code
PART0		EQU	$07BE		; Partition 0 start
PART0_RS	EQU	PART0 + 8 	; Partition 0 relative sector field
PART0_SIZE	EQU	PART0 + 16	; Partition 0 sector size field

BOOTBLK_TAG     EQU     $07FE           ; Address of Boot Block Tag ($55 $AA)

; Miscellanious Constants ******************************************************
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

; TEMP *************************************************************************
PPORTLOAD	EQU	$2000		; jump location for test code
PPORTSAVE	EQU	$2003		; jump location for test code

