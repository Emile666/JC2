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

; MCP23017 I2C GPIO constants
MCP23017_I2C_0	EQU	$40		; I2C address of 1st MCP23017 on IO2 board
MCP23017_I2C_1	EQU	$42		; I2C address of 2nd MCP23017 on IO2 board
MCP23017_I2C_2	EQU	$44		; I2C address of 3rd MCP23017 on IO2 board
IODIRA		EQU	$00		; IO Direction register Port A (1=input, 0=output)
IODIRB		EQU	$01		; IO Direction register Port B (1=input, 0=output)
GPIOA		EQU	$12		; IO Port A, read and write
GPIOB		EQU	$13		; IO Port B, read and write

I2C_RTC_ADR	EQU	$68		; I2C address of DS1307 Real Time Clock

; VIA 1 Register Indices ********************************************************
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

; VIA 2 Register Indices (IO2 board: SD-card, sound, etc.) *************************
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
MON_PROMPT    	EQU     '*'    		; prompt character for Monitor
ADIV      	EQU     '-'    		; address divider

BSLASH          EQU     '\'             ; Backslash ASCII Code
FSLASH		EQU     '/'             ; Forward slash ASCII Code
DOT             EQU     '.'             ; Dot ASCII Code
COLON           EQU     ':'             ; Colon ASCII Code
COMMA           EQU     ','             ; Comma ASCII Code
MINUS           EQU     '-'             ; Minus ASCII Code
ULINE           EQU     '_'             ; Underline ASCII Code

; Interchangeable Chars ********************************************************
DOS_PROMPT      EQU     '>'             ; Command Line Prompt Char
NUM_SEP         EQU     DOT	        ; Thousand Seperator Char
PATH_SEP        EQU     BSLASH          ; Path Separator Char
OPT_SEP         EQU     FSLASH          ; Option Prefix Char

CRSR            EQU     $01             ; Cursor
SOH       	EQU     $01    		; start of header
EOT       	EQU     $04    		; end of text
ACK       	EQU     $06    		; acknowledged
BS        	EQU     $08    		; backspace key
TAB             EQU     $09             ; Horizontal Tab
UP              EQU     $0B             ; Up Arrow
CR        	EQU     $0D    		; carriage return
LF	  	EQU     $0A    		; line feed
RIGHT           EQU     $15             ; Right Arrow
NAK       	EQU     $15    		; not acknowledged
CAN		EQU     $18		; Cancel
ESC       	EQU     $1B    		; ESC
SPC		EQU     $20		; space char
DEL             EQU     $7F             ; Delete

; Tape Reader/Writer Constants *************************************************
RPTIME		EQU	49		; read point time   49x8uS     = 392uS

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

; File Attributes **************************************************************
FA_READONLY     EQU     $01            	; file is read only
FA_HIDDEN       EQU     $02            	; file is hidden
FA_SYSTEM       EQU     $04            	; file is a system file
FA_LABEL        EQU     $08            	; file is a volume label
FA_DIRECTORY    EQU     $10            	; file is a directory
FA_ARCHIVE      EQU     $20            	; file is modified
FA_RESERVED1    EQU     $40
FA_LINK         EQU     $80            	; file is a link

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

; FAT Types ********************************************************************
FAT12_Type      EQU     $02
FAT16_Type      EQU     $01
FAT32_Type      EQU     $00

; Directory Descriptor (32 Bytes) **********************************************
; Index of fields in a directory entry
D_FILENAME      EQU     $00             	; 8 Bytes - 8 Character Filename
D_FILEEXT       EQU     $08             	; 3 Bytes - 3 Character File Extension
D_ATTRIBUTES    EQU     $0B             	; 1 Byte  - File Attributes
D_CREATE_TIME   EQU     $0E             	; 2 Bytes - Creation Time
D_CREATE_DATE	EQU	$10			; 2 Bytes - Create Date 
D_ACCESS_DATE   EQU     $12             	; 2 Bytes - Last Accessed Date
D_START_CLSTH   EQU     $14             	; 2 Bytes - Start Cluster High Word (FAT32)
D_LAST_WR_TIME  EQU     $16             	; 2 Bytes - Last Write Time
D_LAST_WR_DATE  EQU     $18             	; 2 Bytes - Last Write Date
D_START_CLST    EQU     $1A             	; 2 Bytes - Start Cluster Low Word
D_FILE_SIZE     EQU     $1C             	; 4 Bytes - File Size

; FGC VPU Constants ************************************************************

; Mode Registers
VPU_REG0        EQU     $80           ; Mode Register 0
VPU_REG1        EQU     VPU_REG0+1    ; Mode Register 1
VPU_REG8        EQU     VPU_REG0+8    ; Mode Register 2
VPU_REG9        EQU     VPU_REG0+9    ; Mode Register 3

; Table Base Address Registers
VPU_REG2        EQU     VPU_REG0+2    ; Pattern name table base address register
VPU_REG3        EQU     VPU_REG0+3    ; Color table base address register low
VPU_REG10       EQU     VPU_REG0+10   ; Color table base address register high
VPU_REG4        EQU     VPU_REG0+4    ; Pattern generator table base address register
VPU_REG5        EQU     VPU_REG0+5    ; Sprite attribute table base address register low
VPU_REG11       EQU     VPU_REG0+11   ; Sprite attribute table base address register high
VPU_REG6        EQU     VPU_REG0+6    ; Sprite pattern generator table base address register

; Color Registers

VPU_REG7        EQU     VPU_REG0+7    ; Text color/Back drop color register
VPU_REG12       EQU     VPU_REG0+12   ; Text blink color/Back blink color register
VPU_REG13       EQU     VPU_REG0+13   ; Blinking period register
VPU_REG20       EQU     VPU_REG0+20   ; Color burst register 1
VPU_REG21       EQU     VPU_REG0+21   ; Color burst register 2
VPU_REG22       EQU     VPU_REG0+22   ; Color burst register 3

; Display Registers

VPU_REG18       EQU     VPU_REG0+18   ; Display adjust register
VPU_REG23       EQU     VPU_REG0+23   ; Display offset register
VPU_REG19       EQU     VPU_REG0+19   ; Interrupt line register

; Access Registers

VPU_REG14       EQU     VPU_REG0+14   ; VRAM Access base address register
VPU_REG15       EQU     VPU_REG0+15   ; Status register pointer
VPU_REG16       EQU     VPU_REG0+16   ; Color pallete address register
VPU_REG17       EQU     VPU_REG0+17   ; Control register pointer

; Command Registers

VPU_REG32       EQU     VPU_REG0+32   ; Source X low register
VPU_REG33       EQU     VPU_REG0+33   ; Source X high register
VPU_REG34       EQU     VPU_REG0+34   ; Source Y low register
VPU_REG35       EQU     VPU_REG0+35   ; Source Y high register

VPU_REG36       EQU     VPU_REG0+36   ; Destination X low register
VPU_REG37       EQU     VPU_REG0+37   ; Destination X high register
VPU_REG38       EQU     VPU_REG0+38   ; Destination Y low register
VPU_REG39       EQU     VPU_REG0+39   ; Destination Y high register

VPU_REG40       EQU     VPU_REG0+40   ; Number of dots X low register
VPU_REG41       EQU     VPU_REG0+41   ; Number of dots X high register
VPU_REG42       EQU     VPU_REG0+42   ; Number of dots Y low register
VPU_REG43       EQU     VPU_REG0+43   ; Number of dots Y high register

VPU_REG44       EQU     VPU_REG0+44   ; Color register
VPU_REG45       EQU     VPU_REG0+45   ; Argument register
VPU_REG46       EQU     VPU_REG0+46   ; Command register

; Status Registers

VPU_STAT0       EQU     0             ; Status register 0
VPU_STAT1       EQU     1             ; Status register 1
VPU_STAT2       EQU     2             ; Status register 2
VPU_STAT3       EQU     3             ; Status register 3
VPU_STAT4       EQU     4             ; Status register 4
VPU_STAT5       EQU     5             ; Status register 5
VPU_STAT6       EQU     6             ; Status register 6
VPU_STAT7       EQU     7             ; Status register 7
VPU_STAT8       EQU     8             ; Status register 8
VPU_STAT9       EQU     9             ; Status register 9

; Basic Mode Settings

VPU_MODE_REG0   EQU     $00           ; DG = 0, IE2 = 0, IE1 = 0, M3..5 = xxx
VPU_MODE_REG1   EQU     $42           ; BL = 1, IE0 = 0, M1..2 = xx, SI = 1, MA = 0,
VPU_MODE_REG2   EQU     $0A           ; MS = 0, LP = 0, CB = 0, VR = 1, SPD = 1, BW = 0
VPU_MODE_REG3   EQU     $02           ; LN = 0, S1 = 0, S0 = 0, IL = 0, E = 0,NT = 1, DC = 0

VPU_MODE0       EQU     $10
VPU_MODE1       EQU     $12
VPU_MODE4       EQU     $03
VPU_MODE5       EQU     $04
VPU_MODE6       EQU     $05
VPU_MODE7       EQU     $07

		
