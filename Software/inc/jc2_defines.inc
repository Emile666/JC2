;------------------------------------------------------------------------------
; Junior Computer ][ BIOS & Monitor ADDRESS DEFINES
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; $00 - $FF, Zero-Page Address Defines
; - Addresses from $00 up to $9C are in use by BASIC
; - Addresses from $A0 up to $AF are in use by DOS and boot.sys
;------------------------------------------------------------------------------ 
LAB_WARM	= 	$00		; BASIC warm start entry point
Wrmjpl 		= 	LAB_WARM+1	; BASIC warm start vector jump low byte
Wrmjph 		= 	LAB_WARM+2	; BASIC warm start vector jump high byte

FREE1		EQU	$03		; Not in use

; ------------------------------------------------------
; BASIC variables: $04 to $13, see jc2_basic_zp_vars.inc
; ------------------------------------------------------

; Card Base Addresses **********************************************************
IOBASE		EQU	$14		; pointer to IO card base
IOBASEL		EQU	$14		; always $00
IOBASEH		EQU	$15		; K2 = $08, K3 = $0C, K4 = $10
FGCBASE	        EQU	$16		; pointer to Floppy-/Graphics-Controller card base
FGCBASEL	EQU	$16		; always $00
FGCBASEH	EQU	$17		; (K2 = $08, K3 = $0C,) should always be K4 = $10
CARD3BASE	EQU	$18		; reserved (controller base)
CARD3BASEL	EQU	$18		; always $00
CARD3BASEH	EQU	$19		; K2 = $08, K3 = $0C, K4 = $10

; -------------------------------------------------------
; BASIC variables: $1A to $97, see jc2_basic_zp_vars.inc
; -------------------------------------------------------
		ICL "jc2_basic_zp_vars.inc"	; JC2 BASIC variables

; ------------------------------------------------
; DOS variables: $A0 to $A6, see boot.sys files
; ------------------------------------------------
ZP_DOS          EQU     $A0		; Reserved for DOS and boot.sys
CURR_DIR_ENTRY  EQU     ZP_DOS+$00      ; $A0..$A1 (16 bit) - pointer to current addressed directory entry
MASK            EQU     ZP_DOS+$02	; $A2 Mask for FAT table
BCNT            EQU     ZP_DOS+$03      ; $A3 Block Counter
NCNT            EQU     ZP_DOS+$04	; $A4
SCNT            EQU     ZP_DOS+$05	; $A5 Sector counter within a cluster
RES             EQU     ZP_DOS+$06	; $A6

; ------------------------------------------------
; FGC variables: $A8 to $BB, see FGC .asm files
; ------------------------------------------------
ZP_FGC          EQU     $A8             ; Zero Page Base Address
VPUMODE         EQU     ZP_FGC+$00      ; current VPU mode
MAX_X           EQU     ZP_FGC+$01      ; max characters per line
CURSOR_TICKS    EQU     ZP_FGC+$02      ; blink frequency in ticks (1/60 sec or 1/50 sec)
CURSOR          EQU     ZP_FGC+$03      ; character under the cursor
BUFFSIZE        EQU     ZP_FGC+$04      ; actual scroll buffer size
PAGECNT         EQU     ZP_FGC+$05      ; page buffer counter
TXTCOLOR        EQU     ZP_FGC+$06
COLOR           EQU     ZP_FGC+$07
POS_X           EQU     ZP_FGC+$0A      ; cursor X position (2-bytes)
POS_Y           EQU     ZP_FGC+$0C      ; cursor Y position
ADR_CHAR        EQU     ZP_FGC+$0D 	; 2-byte pointer
FGC_ADR         EQU     ZP_FGC+$0F
FGC_ADRL        EQU	ZP_FGC+$0F   	; last address Low
FGC_ADRH      	EQU   	ZP_FGC+$10  	; last address High
FGC_XREG        EQU     ZP_FGC+$11      ; current saved X register
FGC_YREG        EQU     ZP_FGC+$12      ; current saved Y register
VARSAV          EQU     ZP_FGC+$13      ; saved variable

; ------------------------------------------------
; BIOS variables: $C0 to $FF
; ------------------------------------------------
NUM32      	EQU   	$C0     	; low 32 bit number byte (moved from $F8)
SUM32           EQU     $C4             ; low 32 bit number byte (moved from $FC)
DEVID		EQU	$C9		; 1-byte temp device id (moved from $03 to avoid Basic collision)
PRSTR		EQU	$CA		; 2-bytes replacement for PSTR
END_PTR		EQU	$CC		; ROM end-pointer (2 bytes), also used by boot.sys
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
CRCL      	EQU   	$DC      	; XModem CRC lo byte
BLKBUFH		EQU	$DD             ; upper byte of block buffer pointer
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
BEGADR  	EQU     $E2   		; MON 2-byte Editor Begin Address Pointer
SD_PB0		EQU	$E2   		; SD-Card Parameter Byte 0
ADRMODE		EQU	$E2		; DISASSEM addressing mode
BLKNO     	EQU   	$E2      	; XModem block number
SD_CRC		EQU	$E3   		; SD-Card CRC Byte
IBYTES		EQU	$E3		; DISASSEM instruction byte count
ERRCNT    	EQU   	$E3      	; XModem error counter 10 is the limit
ENDADR  	EQU     $E4   		; MON 2-byte Editor End Address Pointer
VIA_STATUS 	EQU	$E4 		; VIA2 current VIA2 PortB output status

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

LABELS  	EQU     $EE   		; MON 1-byte Editor 
PSAV            EQU     $EE		; Saved Partition Number in ASCII
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

;------------------------------------------------------------------------------
; Junior Computer ][ BIOS & Monitor ADDRESS DEFINES
; $0100 - $01FF is reserved for the 6502 Stack
;------------------------------------------------------------------------------ 

;----------------------------------------------------------------------------
; Block Device / Tables and Buffer Addresses
;----------------------------------------------------------------------------
DIR_BLK_BUF     EQU     $0200           	; Directory Block Buffer
DIR_BLK_BUFL    EQU     DIR_BLK_BUF     	; Lower page of directory buffer
DIR_BLK_BUFH    EQU     DIR_BLK_BUF+$100	; upper page of directory buffer

; Current Volume Descriptor (32 Bytes) -----------------------------------------
; The BOOT routine in BIOS has already loaded the MBR into $0600, loaded the
; Volume-ID sector into $0400 and did set (most of) these variables.
; ------------------------------------------------------------------------------
CURR_VOLUME     EQU     $0400           	; Current Volume Descriptor, used by boot.sys and mkbootcf
BOOT_PART       EQU     CURR_VOLUME     	; Boot Medium Descriptor: used by bios and mkbootcf
D_PART_START	EQU	CURR_VOLUME+$00		; 4 Bytes - Volume ID sector LBA
D_ACTUAL_DIR    EQU     CURR_VOLUME+$04     	; 4 Bytes - Actual Directory cluster number
D_DEV_ID        EQU     CURR_VOLUME+$08 	; 1 Byte  - Device ID
D_MEDIUM_DESCR  EQU     CURR_VOLUME+$09 	; 1 Byte  - Medium Descriptor Byte or Partition Number in ASCII if harddisk
D_FAT_TYPE      EQU     CURR_VOLUME+$0A 	; 1 Byte  - FAT Type (0=FAT32)
D_SECT_PER_CLST EQU     CURR_VOLUME+$0B 	; 1 Byte  - Sectors/Cluster
D_NUM_OF_FAT    EQU     CURR_VOLUME+$0C 	; 1 Byte  - Number of FATs
D_NUM_ROOT_DIR  EQU     CURR_VOLUME+$0D 	; 2 Bytes - Number of Root directory Entries
D_DEV_ATTRIBS   EQU     CURR_VOLUME+$0F 	; 1 Byte  - Device Attributes
D_START_CLS_LBA EQU     CURR_VOLUME+$10 	; 4 Bytes - Start LBA of Cluster Area (= cluster_begin_lba - 2)
D_START_FAT1    EQU     CURR_VOLUME+$14 	; 4 Bytes - Start LBA of FAT1
D_START_FAT2    EQU     CURR_VOLUME+$18 	; 4 Bytes - Start LBA of FAT2
D_START_DIR     EQU     CURR_VOLUME+$1C 	; 4 Bytes - root_dir_first_cluster (usually 2), also LBA nr for FAT16/12
CURR_CLUSTER    EQU     CURR_VOLUME+$20         ; 4 Bytes - current addressed cluster in boot.sys
CURR_FAT_BLK    EQU     CURR_VOLUME+$24         ; 4 Bytes - current loaded FAT LBA in boot.sys
D_SUBDIR_NAME	EQU	CURR_VOLUME+$28		; ? Bytes - Name of entire path (root dir is \)

MOUNT_TABLE     EQU     $0500           	; Table of mounted devices, used by boot.sys

FAT_BUF		EQU	$0600           	; Buffer for FAT32 sector
BLOCK_BUF	EQU	$0600			; 1st sector of boot.sys is also stored here
MBR             EQU     BLOCK_BUF      		; Master Boot Block Code, contains a BNE to boot-code
PART0		EQU	MBR + $01BE		; Partition 0 start
PART0_RS	EQU	PART0 + 8 		; Partition 0 relative sector field
PART0_SIZE	EQU	PART0 + 16		; Partition 0 sector size field
BOOTBLK_TAG     EQU     MBR + $01FE           	; Address of Boot Block Tag ($55 $AA)

BAS_LOAD_ADDR	EQU	$2000			; .BAS Load-address
COM_RUN_ADDR	EQU	$3000			; .COM load- and run-address

; IO Base Addresses ---------------------------------------------------------
;----------------------------------------------------------------------------
K2		EQU	$0800		; Base address of IO select K2
K3		EQU	$0C00		; Base address of IO select K3 = IO2-card
K4		EQU	$1000		; Base address of IO select K4 = FGC-card

;----------------------------------------------------------------------------
; CF-IDE Hardware Registers (on stand-alone card or IO2-board)
;----------------------------------------------------------------------------
CFBASE		EQU 	K3 + $80	; Base card-address for CF card
CFREG0		EQU	CFBASE+0	; Data port
CFREG1		EQU	CFBASE+1	; Read: error-code, write: feature
CFREG2		EQU	CFBASE+2	; Number of sectors to transfer
CFREG3		EQU	CFBASE+3	; Sector address LBA 0 [0:7] (LSB)
CFREG4		EQU	CFBASE+4	; Sector address LBA 1 [8:15]
CFREG5		EQU	CFBASE+5	; Sector address LBA 2 [16:23]
CFREG6		EQU	CFBASE+6	; Sector address LBA 3 [24:27] (MSB)
CFREG7		EQU	CFBASE+7	; Read: Status, Write: Command
CFREG8		EQU	CFBASE+8	; A write with 0x00 is a HW reset

;----------------------------------------------------------------------------
; FGC / VIDEO Card Hardware Registers
;----------------------------------------------------------------------------
FGC_BASE        EQU     K4		; K4 base-address (fixed)
FGC_START_CONF  EQU     FGC_BASE+$06  	; prepare for DIP Switch reading
FGC_END_CONF    EQU     FGC_BASE+$07  	; read keyboard layout from DIP Switch settings
VPU_BASE        EQU     FGC_BASE+$08  	; VPU Register Base
VPU_PORT0       EQU     VPU_BASE      	; Data Port
VPU_PORT1       EQU     VPU_BASE+1    	; Control Port
VPU_PORT2       EQU     VPU_BASE+2    	; Color Pallete Port
VPU_PORT3       EQU     VPU_BASE+3    	; Indirect Register Port
PPI_PORTA       EQU     FGC_BASE+$0C  	; PIA Port A register
PPI_PORTC       EQU     FGC_BASE+$0E  	; PIA Port C
PPI_CONTROL     EQU     FGC_BASE+$0F  	; PIA Control register

FGC_INFO        EQU     FGC_BASE+$10
FGC_SET_PAGE    EQU     FGC_BASE+$13
FGC_FDC_CMD     EQU     FGC_BASE+$17
FGC_VPU_CMD     EQU     FGC_BASE+$1F
FGC_VPU_OUT     EQU     FGC_BASE+$27

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
; $1800 - $19FF 512 Bytes, previously unused, on JC2 v4.1 this has become RAM.
;------------------------------------------------------------------------------
PPORTLOAD	EQU	$1800		; jump location for test code
PPORTSAVE	EQU	$1803		; jump location for test code
CF_LOAD_VEC	EQU	$1806		; 2-byte vector for CF-card LOAD vector
CF_SAVE_VEC	EQU	$1808		; 2-byte vector for CF-card SAVE vector
RETURN_VECT     EQU     $180A           ; 2-byte return vector to monitor caller (moved from $01 to avoid Basic collision)
I2Cstat		EQU	$180C		; BASIC I2C read byte or ACK/NACK bit (moved from Basic $9C) 
KBD_LANG        EQU     $180D         	; Current selected keyboard language for FGC-card
FLOPPY_TYPE     EQU     $180E     	; floppy type settings for FGC-card

NEXTINDEX       EQU     $1810		; command index
TERM_CHAR       EQU     $1811
F_ATTRIBS       EQU     $1812
TERM_FLAG       EQU     $1813		; set/clear counter termination flag
LINE_CNT        EQU     $1814		; line-counter for DIR command
CURR_FILE_CNT   EQU     $1815      	; (16 bit) - total number of files in dir
CURR_DIR_CNT    EQU     $1817      	; (16 bit) - total number of directories in dir
CURR_USED_SIZE  EQU     $1819      	; (32 bit) - total number of bytes in dir
CURR_BLK_NUM    EQU     $181D      	; directory block counter
CURR_DRIVE      EQU     $181E      	; current selected drive
CURR_CMD_PARAM  EQU     $181F      	; current command parameter
FREE_CLUSTER    EQU     $1820           ; (32 bit) - last free cluster
CURR_DIR_BLK    EQU     $1824       	; (32 bit) - current directory LBA
DBG_PRINT	EQU	$1828		; 1 = Print debug info

; INPBUF is the BASIC Input Buffer. It is defined from $1868 - $18FF
; Rbytes are moved from Basic $98..$9B to free up ZP-space
Rbyte4		EQU	$1860		; extra PRNG byte
Rbyte1		EQU	$1861		; most significant PRNG byte
Rbyte2		EQU	$1862		; middle PRNG byte
Rbyte3		EQU	$1863		; least significant PRNG byte

INPBUF	  	EQU 	$1868		; change input buffer to last 151 bytes in page
ccflag		EQU	INPBUF		; BASIC CTRL-C flag, 00 = enabled, 01 = dis
ccbyte		EQU 	ccflag+1	; BASIC CTRL-C byte
ccnull		EQU 	ccbyte+1	; BASIC CTRL-C byte timeout
VEC_CC		EQU 	ccnull+1	; ctrl c check vector

; Ibuffs can now be anywhere in RAM, ensure that the max length is < $80
Ibuffs		EQU 	VEC_CC+$14	; start of input buffer after IRQ/NMI code
Ibuffe		EQU 	Ibuffs+$7F	; end of input buffer

;------------------------------------------------------------------------------
; $1A00 - $1AFF 256 Bytes, RIOT RAM area
;------------------------------------------------------------------------------
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
; $1B00 - $1BFF 256 Bytes, previously unused, on JC2 v4.1 this has become RAM.
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; $1C00 - $1FFF 1 KB, Monitor RAM area.
;------------------------------------------------------------------------------
FILE_BUFF	EQU	$1C00		; 512 byte file buffer for .exe/.com/.bas in Monitor RAM

BOOT_CODE       EQU 	$4000		; boot.sys address of 1st sector, used by mkboot and boot.sys
