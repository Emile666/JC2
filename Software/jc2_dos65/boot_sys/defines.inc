;-------------------------------------------------
; Register Definitions for BOOT.SYS file.
; Assembler: MADS-Assembler
;-------------------------------------------------

; Global Constants and Variables ***********************************************
LAB_WARM	= 	$00		; BASIC warm start entry point
Wrmjpl 		= 	LAB_WARM+1	; BASIC warm start vector jump low byte
Wrmjph 		= 	LAB_WARM+2	; BASIC warm start vector jump high byte
LAB_COLD	=	$B000		; BASIC cold START

ZP_BASE         EQU     $A0		; Reserved for DOS and boot.sys
CURR_CLUSTER    EQU     ZP_BASE         ; $A0..$A3 (32 bit) - current addressed cluster
CURR_FAT_BLK    EQU     ZP_BASE+4       ; $A4..$A7 (32 bit) - current loaded FAT LBA
CURR_DIR_ENTRY  EQU     ZP_BASE+8       ; $A8..$A9 (16 bit) - pointer to current addressed directory entry
MASK            EQU     ZP_BASE+10	; $AA Mask for FAT table
BCNT            EQU     ZP_BASE+11      ; $AB Block Counter
NCNT            EQU     ZP_BASE+12	; $AC
SCNT            EQU     ZP_BASE+13	; $AD Sector counter within a cluster
RES             EQU     ZP_BASE+14	; $AE

ssptr_l		EQU	$78		; BASIC filename pointer LSB
ssptr_h		EQU	$79		; BASIC filename pointer MSB

NUM32      	EQU   	$C0     	; low 32 bit number byte (moved from $F8)
SUM32           EQU     $C4             ; low 32 bit number byte (moved from $FC)
END_PTR		EQU	$CC		; CFC_LOAD destination pointer, also used by BIOS ROM checksum routine
SAVEX		EQU 	$D8		; Temp. save X register (same as BIOS)
SAVEY		EQU	$D9		; Temp. save Y register (same as BIOS)
BLKBUF		EQU	$DC
BLKBUFL		EQU	$DC             ; Pointer to block buffer, same in BIOS
BLKBUFH		EQU	$DD

STOL      	EQU   	$E8     	; Store address Low, same in BIOS
STOH      	EQU   	$E9     	; Store address High
PSTR            EQU     $EA		; 2-byte ACIA output pointer, same in BIOS
PSAV            EQU     $EE             ; Saved Partition Number in ASCII, same in BIOS

DIG0      	EQU   	$F8     	; 10^0 digit, used by NUMOUT and DEC2STR
DIG1	  	EQU   	$F9	 	; 10^1 digit, used by NUMOUT and DEC2STR
DIG2      	EQU   	$FA     	; 10^2 digit, used by NUMOUT and DEC2STR

STRBUF	  	EQU   	$1400    	; input string buffer at $1400 (same as BIOS)

MMU		EQU	$1700		; ATF1504 Memory Management Unit (MMU) Register

CF_LOAD_VEC	EQU	$1806		; 2-byte vector for CF-card LOAD vector
CF_SAVE_VEC	EQU	$1808		; 2-byte vector for CF-card SAVE vector
RETURN_VECT     EQU     $180A           ; return vector to monitor caller in V1.2.1 (was $0001 in V1.2.0)

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

FILE_BUFF	EQU	$1C00		; 512 byte file buffer for .exe/.com/.bas in Monitor RAM

; External Routines ************************************************************

MON_WARM_START  EQU     $E003

SWITCH_TO_ROM	EQU	$E00A			; Switch Basic to ROM Page ($B000-$DFFF)
COUT            EQU     $E052
CROUT           EQU     $E05A
SPCOUT          EQU     $E05E
CIN             EQU     $E047
STRIN           EQU     $E062
STROUT          EQU     $E083
WRSTR           EQU     $E085
HEXOUT          EQU     $E091
HEXDIG          EQU     $E09A
DEC2STR         EQU     $E0BD
CLRSCRN         EQU     $E0B5
CMDDEV          EQU     $E0BA
OPEN_DEVICE     EQU     $E1AA
READTIME        EQU     $E2DE			; BIOS v1.2.1
READDATE        EQU     $E2E2			; BIOS v1.2.1
OS_STRING_OUT	EQU	$F682			; SPRINT in BIOS v1.2.1, but same routine
SET_RAMBANK	EQU	$FD53			; BIOS v1.2.4
MON2ROM		EQU	$FD7D			; Monitor to ROM
MON2RAM		EQU	$FD86			; Monitor to RAM, enable RAM behind it

; Tables and Buffers ***********************************************************
DIR_BLK_BUF     EQU     $0200           	; Directory Block Buffer
DIR_BLK_BUFL    EQU     DIR_BLK_BUF     	; Lower page of directory buffer
DIR_BLK_BUFH    EQU     DIR_BLK_BUF+$100	; upper page of directory buffer

CURR_VOLUME     EQU     $0400           	; Current Volume Descriptor
FILE_TABLE      EQU     CURR_VOLUME+$20  	; File Descriptor Table
MOUNT_TABLE     EQU     $0500           	; Table of mounted devices
BLOCK_BUFF      EQU     $0600           	; Data Block Buffer

SIS_OFFSET	EQU	SIS_BUFF+$30		; 2-bytes: System Information Sector Offset

BAS_LOAD_ADDR	EQU	$2000			; .BAS Load-address
COM_RUN_ADDR	EQU	$3000			; .COM load- and run-address

; Device IDs *******************************************************************
NULL_ID		EQU	$00             	; the NULL device
FDD1_ID         EQU     $22             	; Floppy drive 1 device ID
FDD2_ID         EQU     $23             	; Floppy drive 2 device ID
SDC_ID		EQU     $24             	; SD-Card device ID
HDD1_ID		EQU     $25             	; CF-Card device ID

; FAT Types ********************************************************************
FAT12_Type      EQU     $02
FAT16_Type      EQU     $01
FAT32_Type      EQU     $00

; Device Driver Commands *******************************************************
CMD_READ	EQU	34              	; Read data block from device
CMD_WRITE	EQU	35              	; Write data block to device
CMD_READ_BUF	EQU	37              	; Read data block from device to standard buffer
CMD_WRITE_BUF	EQU	38              	; Write data block to device from standard buffer

; Miscellaneous Constants ******************************************************
CR              EQU     $0D             	; Carriage Return ASCII Code
SPC             EQU     $20             	; Space ASCII Code
BSLASH          EQU     '\'             	; Backslash ASCII Code
FSLASH		EQU     '/'             	; Forward slash ASCII Code
DOT             EQU     '.'             	; Dot ASCII Code
COLON           EQU     ':'             	; Colon ASCII Code
COMMA           EQU     ','             	; Comma ASCII Code
MINUS           EQU     '-'             	; Minus ASCII Code
ULINE           EQU     '_'             	; Underline ASCII Code

; Interchangeable Chars ********************************************************
PROMPT          EQU     '>'             	; Command Line Prompt Char
NUM_SEP         EQU     DOT	           	; Thousand Seperator Char
PATH_SEP        EQU     BSLASH          	; Path Seperator Char
OPT_SEP         EQU     FSLASH           	; Option Prefix Char

; File Attributes **************************************************************
FA_READONLY     EQU     $01             	; file is read only
FA_HIDDEN       EQU     $02             	; file is hidden
FA_SYSTEM       EQU     $04             	; file is a system file
FA_LABEL        EQU     $08             	; file is a volume label
FA_DIRECTORY    EQU     $10             	; file is a directory
FA_ARCHIVE      EQU     $20             	; file is modified
FA_RESERVED1    EQU     $40
FA_LINK         EQU     $80             	; file is a link

; Device Attributes ************************************************************
DA_DIRTY        EQU     $80             ; current FAT block is modified

; Current Volume Descriptor (32 Bytes) *****************************************
; The BOOT routine in BIOS has already loaded the MBR into $0600, laoded the
; Volume-ID sector into $0400 and did set (most of) these variables.
D_PART_START	EQU	CURR_VOLUME+$00		; 4 Bytes - Volume ID sector LBA
D_ACTUAL_DIR    EQU     CURR_VOLUME+$04     	; 4 Bytes - Actual Directory cluster number
D_DEV_ID        EQU     CURR_VOLUME+$08 	; 1 Byte  - Device ID
D_MEDIUM_DESCR  EQU     CURR_VOLUME+$09 	; 1 Byte  - Medium Descriptor Byte
						;           or Partition Number in ASCII if harddisk
D_FAT_TYPE      EQU     CURR_VOLUME+$0A 	; 1 Byte  - FAT Type (0=FAT32)
D_SECT_PER_CLST EQU     CURR_VOLUME+$0B 	; 1 Byte  - Sectors/Cluster
D_NUM_OF_FAT    EQU     CURR_VOLUME+$0C 	; 1 Byte  - Number of FATs
D_NUM_ROOT_DIR  EQU     CURR_VOLUME+$0D 	; 2 Bytes - Number of Root directory Entries
D_DEV_ATTRIBS   EQU     CURR_VOLUME+$0F 	; 1 Byte  - Device Attributes
D_START_CLS_LBA EQU     CURR_VOLUME+$10 	; 4 Bytes - Start LBA of Cluster Area (= cluster_begin_lba - 2)
D_START_FAT1    EQU     CURR_VOLUME+$14 	; 4 Bytes - Start LBA of FAT1
D_START_FAT2    EQU     CURR_VOLUME+$18 	; 4 Bytes - Start LBA of FAT2
D_START_DIR     EQU     CURR_VOLUME+$1C 	; 4 Bytes - root_dir_first_cluster (usually 2), also LBA nr for FAT16/12

D_SUBDIR_NAME	EQU	FILE_TABLE+$00		; 11 Bytes - Name of entire path (root dir is \)
	
; Directory Descriptor (32 Bytes) **********************************************

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


