; ******************************************************************************
;
; The Micro (Disk) Operating System for 6502 Microprocessors
;
; M/OS-65 System I, Version 0.2.1
; by Joerg Walke
;
; Developed for the Junior Computer ][
;
; First implementation 6.7.2023
; updated 20.06.2024
; by Joerg Walke
;
; Assembled with A65, Converted to MADS by Emile
;
; M/OS-65 is primarily a small Disk Operation System (DOS) based on the FAT
; filesystem. FAT12, FAT16 and FAT32 formated devices are supported.
; The system is written for the Junior Computer ][ with the IO-Board connected.
; It should support any block device useing the Junior Computer ][ driver model.
; For now it is tested on SD-Cards with up to 64GB, formatted in the appropriate
; FAT format. exFAT is and will not not be supported.
; A SD-Card can be partitioned with up to four primary partitions, formated
; in either FAT12, FAT16 or FAT32.
; Useing the MKBOOT program you can write the needed Partition Boot Block and
; the Master Boot Record which then holds a menu program to select one of the
; available partitions for booting.
;
; For now, M/OS-65 is in a very early - maybe unstable - state, which means,
; it should not be used to store any important data !!!
;
; Developing will go on, but maybe slowly. So stay tuned...
;
; This code is freely available under the Creative Commons Attribution 4.0
; International license.
; See https://creativecommons.org/licenses/by/4.0/
;
; ******************************************************************************

; TO DO:  Many...

; List Of Changes **************************************************************
;
; 11-04-25 Emile boot.asm renamed in bootcf.asm and adapted for CF-IDE interface.
;
; ******************************************************************************
		OPT h- ; do not add file header
		OPT f+ ; save as single block

; Global Constants and Variables ***********************************************

VERMAIN   	EQU     '0'    		; main version
VERPSUB    	EQU     '2'    		; primary sub version
VERSSUB		EQU	'1'		; secondary sub version

ZP_BASE         EQU     $A0
ODD_CLUSTER     EQU     ZP_BASE         ; odd cluster flag for FAT12 addressing
CURR_CLUSTER    EQU     ZP_BASE+1       ; $1C..$1F (32 bit) - current addressed cluster
CURR_FAT_BLK    EQU     ZP_BASE+5       ; $20..$23 (32 bit) - current loaded FAT block
CURR_DIR_BLK    EQU     ZP_BASE+9       ; $24..$27 (32 bit) - current directory block
CURR_DIR_ENTRY  EQU     ZP_BASE+13      ; $28..$29 (16 bit) - pointer to current addressed directory entry
CURR_FILE_CNT   EQU     ZP_BASE+15      ; $2A..$2B (16 bit) - total number of files in dir
CURR_DIR_CNT    EQU     ZP_BASE+17      ; $2C..$2D (16 bit) - total number of directories in dir
CURR_USED_SIZE  EQU     ZP_BASE+19      ; $2E..$31 (32 bit) - total number of bytes in dir

CURR_BLK_NUM    EQU     ZP_BASE+23      ; directory block counter
CURR_DRIVE      EQU     ZP_BASE+24      ; current selected drive
CURR_CMD_PARAM  EQU     ZP_BASE+25      ; current command parameter
FREE_CLUSTER    EQU     ZP_BASE+26      ; $35..$38 (32 bit) - last free cluster

PPMASK          EQU     ZP_BASE+30      ; Parameter Mask


BLKBUFL		EQU	$DC             ; Pointer to block buffer
BLKBUFH		EQU	$DD

DIR_PTR         EQU     $DE             ; Pointer to directory entry

CMDL    	EQU   	$DE      	; Command table pointer lo byte
CMDH    	EQU   	$DF      	; Command table pointer hi byte

MNT_TABLE      	EQU   	$E8     	; Pointer to mount table

STOL      	EQU   	$E8     	; Store address Low
STOH      	EQU   	$E9     	; Store address High

PSTR            EQU     $EA

NCNT            EQU     $EC
SCNT            EQU     $ED
PSAV            EQU     $EE             ; Saved Partition Number in ASCII
BCNT            EQU     $F2             ; Block Counter
RES             EQU     $EF
F_ATTRIBS       EQU     $EF
MASK            EQU     $F0
LINE_CNT        EQU     $F1
TERM_FLAG       EQU     $F2

TERM_CHAR       EQU     $ED

YEAR            EQU     $EE
MONTH           EQU     $EF
DAY             EQU     $F0

MINUTE          EQU     $EE

NUM32      	EQU   	$F8     	; low 32 bit number byte
SUM32           EQU     $FC             ; low 32 bit number byte

NEXTINDEX       EQU     $10

STRBUF	  	EQU   	$1400    	; input string buffer at $1400

; External Routines ************************************************************

RETURN_VECT     EQU     $0001           ; return vector to monitor caller
MON_WARM_START  EQU     $E003

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
READTIME        EQU     $E2B5
READDATE        EQU     $E2B9

; Tables and Buffers ***********************************************************

DIR_BLK_BUF     EQU     $0200           ; Directory Block Buffer
DIR_BLK_BUFL    EQU     DIR_BLK_BUF     ; Lower page of directory buffer
DIR_BLK_BUFH    EQU     DIR_BLK_BUF+$100; upper page of directory buffer

CURR_VOLUME     EQU     $0400           ; Current Volume Descriptor
FILE_TABLE      EQU     CURR_VOLUME+32  ; File Descriptor Table
MOUNT_TABLE     EQU     $0500           ; Table of mounted devices
BLOCK_BUFF      EQU     $0600           ; Data Block Buffer

; Device IDs *******************************************************************

NULL_ID		EQU	$00             ; the NULL device
FDD1_ID         EQU     $22             ; Floppy drive 1 device ID
FDD2_ID         EQU     $23             ; Floppy drive 2 device ID
SDC_ID		EQU     $24             ; SD-Card device ID
HDD1_ID		EQU     $25             ; CF-Card device ID

; FAT Types ********************************************************************

FAT12_Type      EQU     $02
FAT16_Type      EQU     $01
FAT32_Type      EQU     $00

; Device Driver Commands *******************************************************

CMD_READ	EQU	34              ; Read data block from device
CMD_WRITE	EQU	35              ; Write data block to device
CMD_READ_BUF	EQU	37              ; Read data block from device to standard buffer
CMD_WRITE_BUF	EQU	38              ; Write data block to device from standard buffer

; Miscellaneous Constants ******************************************************

CR              EQU     $0D             ; Carriage Return ASCII Code
SPC             EQU     $20             ; Space ASCII Code
BSLASH          EQU     '\'             ; Backslash ASCII Code
SLASH           EQU     '/'             ; Slash ASCII Code
DOT             EQU     '.'             ; Dot ASCII Code
COLON           EQU     ':'             ; Colon ASCII Code
COMMA           EQU     ','             ; Comma ASCII Code
MINUS           EQU     '-'             ; Minus ASCII Code
ULINE           EQU     '_'             ; Underline ASCII Code

; Interchangeable Chars ********************************************************

PROMPT          EQU     '>'             ; Command Line Prompt Char
NUM_SEP         EQU     COMMA           ; Thousand Seperator Char
PATH_SEP        EQU     BSLASH          ; Path Seperator Char
OPT_SEP         EQU     SLASH           ; Option Prefix Char

; File Attributes **************************************************************

FA_READONLY     EQU     $01             ; file is read only
FA_HIDDEN       EQU     $02             ; file is hidden
FA_SYSTEM       EQU     $04             ; file is a system file
FA_LABEL        EQU     $08             ; file is a volume label
FA_DIRECTORY    EQU     $10             ; file is a directory
FA_ARCHIVE      EQU     $20             ; file is modified
FA_RESERVED1    EQU     $40
FA_LINK         EQU     $80             ; file is a link

; Device Attributes ************************************************************

DA_DIRTY        EQU     $80             ; current FAT block is modified

; Current Volume Descriptor (32 Bytes) *****************************************
; The SD_BOOT routine in BIOS has already loaded the MBR into $0600 and the
; Volume-ID into $0400.
D_ACTUAL_DIR    EQU     CURR_VOLUME     ; 4 Bytes - Actual Directory
D_DEV_ID        EQU     CURR_VOLUME+$08 ; 1 Byte  - Device ID
D_MEDIUM_DESCR  EQU     CURR_VOLUME+$09 ; 1 Byte  - Medium Descriptor Byte
                                        ;           or Partition Number in ASCII if harddisk
D_FAT_TYPE      EQU     CURR_VOLUME+$0A ; 1 Byte  - FAT Type
D_SECT_PER_CLST EQU     CURR_VOLUME+$0B ; 1 Byte  - Sectors/Cluster
D_NUM_OF_FAT    EQU     CURR_VOLUME+$0C ; 1 Byte  - Number of FATs
D_NUM_ROOT_DIR  EQU     CURR_VOLUME+$0D ; 2 Bytes - Number of Root directory Entries
D_DEV_ATTRIBS   EQU     CURR_VOLUME+$0F ; 1 Byte  - Device Attributes
D_START_CLUSTER EQU     CURR_VOLUME+$10 ; 4 Bytes - Start Block of Cluster Area
D_START_FAT1    EQU     CURR_VOLUME+$14 ; 4 Bytes - Start Block of FAT1
D_START_FAT2    EQU     CURR_VOLUME+$18 ; 4 Bytes - Start Block of FAT2
D_START_DIR     EQU     CURR_VOLUME+$1C ; 4 Bytes - Start Block of Root Directory

; Directory Descriptor (32 Bytes) **********************************************

D_FILENAME      EQU     $00             ; 8 Bytes - 8 Character Filename
D_FILEEXT       EQU     $08             ; 3 Bytes - 3 Character File Extension
D_ATTRIBUTES    EQU     $0B             ; 1 Byte  - File Attributes
D_CREATION_DT   EQU     $0D             ; 5 Bytes - Creation Date/Time
D_ACCESS_DATE   EQU     $12             ; 2 Bytes - Accessed Date
D_START_CLSTH   EQU     $14             ; 2 Bytes - Start Cluster High Bytes (FAT32)
D_CREATION_T    EQU     $16             ; 2 Bytes - Creation Time
D_CREATION_D    EQU     $18             ; 2 Bytes - Creation Date
D_START_CLST    EQU     $1A             ; 2 Bytes - Start Cluster Low Bytes
D_FILE_SIZE     EQU     $1C             ; 4 Bytes - File Size

; ******************************************************************************
; Start of 1st Block of BOOT.SYS ***********************************************
; ******************************************************************************
		ORG	$2000		; the program start address
PROG_START				; Program Start Address

; ******************************************************************************
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! CRITICAL !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; !!!!!!!!!!! DO NOT CHANGE ANY CODE IN THE FIRST BLOCK OF BOOT.SYS !!!!!!!!!!!!
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; ******************************************************************************

; The boot strap loader has already injected some information about the boot
; device and loaded the first block (this one!) into memory. So any code in this
; section is critical, because it is needed to load more code of BOOT.SYS into
; memory. It's also very well balanced to fit into the first block. Adding just
; one byte of new code moves a required part out of these 512 bytes and causes
; the code to crash.
; So just let it be!!!


; BOOT.SYS Initialisation and Start Routine ************************************

; subtract two clusters from START_CLUSTER address *****************************
                
INIT            LDA     D_SECT_PER_CLST
                ASL     
                TAX
                LDA     D_START_CLUSTER
                STX     D_START_CLUSTER
                SEC
                SBC     D_START_CLUSTER
                STA     D_START_CLUSTER
                LDX     #$01
                LDY     #$03
INIT2           LDA     D_START_CLUSTER,X
                SBC     #$00
                STA     D_START_CLUSTER,X
                INX
                DEY
                BNE     INIT2
                
; initialize current FAT block value with 0 ************************************

                STY     CURR_FAT_BLK
                STY     CURR_FAT_BLK+1
                STY     CURR_FAT_BLK+2
                STY     CURR_FAT_BLK+3
                
; set first cluster of BOOT.SYS as the current cluster *************************

INIT3           LDY     #$15
                LDX     #$03
                JSR     SET_WORD            ; store cluster start byte [3:2]
                LDY     #$1B
                JSR     SET_WORD            ; store cluster start byte [1:0]
                
; calculate BOOTS.SYS file size in blocks **************************************

INIT4           SEC                         ; calculate total number of blocks
                LDA     #>BOOT_SYS_END      ; needed to load the complete
                SBC     #>PROG_START        ; BOOT.SYS file into memory
                LSR     
                STA     BCNT                ; store result into block counter
                LDA     #<BOOT_SYS_END
                BEQ     BOOT_SYS
                INC     BCNT                ; some bytes are left, so increment block counter
                
; load all blocks of BOOT.SYS file into memory *********************************

BOOT_SYS        DEC     BCNT                ; first block is already read in
                BEQ     OS_START            ; more blocks to read? No, jump to OS start
                LDA     D_SECT_PER_CLST     ; load numbers of sectors per cluster
                STA     SCNT                ; and store it in temp
LOAD_NEXT_BLK   DEC     SCNT                ; read next block in cluster
                BEQ     NEXT_CLUSTER        ; all blocks in cluster read? Yes, read next cluster
                JSR     INC_32              ; no, increment block address
LOAD_BLK        LDX     #<NUM32
                LDY     #>NUM32
                JSR     DEV_RD_LBLK         ; and read next block of BOOT.SYS
                DEC     BCNT
                BEQ     OS_START            ; more blocks to read? No, jump to OS start
                BNE     LOAD_NEXT_BLK       ; yes, read next block in cluster

; next cluster needs to be loaded considering the volume FAT type **************

NEXT_CLUSTER    JSR     GET_NEXT_CLSTR      ; get next cluster in chain
                BCS     OS_START            ; if EOF then start OS
                JSR     CLUSTER_TO_BLK      ; convert cluster address to LBA
                LDA     D_SECT_PER_CLST     ; load numbers of sectors per cluster
                STA     SCNT                ; and store it in temp
                JMP     LOAD_BLK            ; load first block of cluster
                
OS_START        JMP     OS_MAIN             ; jump to OS entry point

SET_WORD        JSR     SET_BYTE
SET_BYTE        LDA     (PSTR),Y            ; load a single byte from dir entry
                STA     CURR_CLUSTER,X      ; and store it into CURR_CLUSTER
                DEY
                DEX
                RTS
                
; **** Get Next Cluster Of Cluster Chain ***************************************
;
; Input:  CURR_CLUSTER[0:3] - LBA to current cluster
; Output: C = 0 valid cluster in CURR_CLUSTER[0:3]; C = 1 EOF
;
; ******************************************************************************

GET_NEXT_CLSTR  LDX     #<D_START_FAT1      ; load base block address of FAT into NUM32[0:3]
                LDY     #>D_START_FAT1
                JSR     LOAD_32

                LDA     D_FAT_TYPE          ; check FAT type
                BEQ     FAT32
                CMP     #FAT16_Type
                BEQ     FAT16
                
; **** Decode FAT12 Entry ******************************************************
; **** Two FAT12 entries A and B coded together as AA BA BB ********************

FAT12           LDX     CURR_CLUSTER        ; get current cluster# low byte
                LDY     CURR_CLUSTER+1      ; get current cluster# high byte
                LSR     CURR_CLUSTER+1      ; divide cluster# by 2
                ROR     CURR_CLUSTER        ; save bit 0 into carry flag
                LDA     #$00
                ROL                         ; save carry flag into Accu
                STA     ODD_CLUSTER         ; it's a odd cluster# if C = 1
                TXA
                ADC     CURR_CLUSTER        ; finally add cluster#/2 to cluster#
                STA     CURR_CLUSTER        ; CURR_CLUSTER[0] is low byte of index into FAT
                TYA
                ADC     CURR_CLUSTER+1
                STA     CURR_CLUSTER+1      ; CURR_CLUSTER[1] is high byte of index into FAT
                LSR                         ; shift one bit right to get FAT block index
                TAX
                JSR     ADD_32_8            ; add block index to FAT base block address
                JSR     LOAD_FAT_BLK        ; load this block
                LDY     CURR_CLUSTER        ; load index into the FAT block
READ_BYTE1      JSR     READ_ENTRY_BYTE     ; read first cluster byte
STORE_BYTE1     STA     CURR_CLUSTER        ; and store it as CURR_CLUSTER[0]
                INY                         ; address next byte
                BNE     READ_BYTE2          ; overflow to next page? No, just read next byte
                LDA     #$01                ; yes!
                AND     CURR_CLUSTER+1      ; check bit 9 of FAT index
                BEQ     PAGE_OVERFLOW       ; if bit 9 = 0 then it's just a page overflow
BLOCK_OVERFLOW  JSR     INC_32              ; it's a block overflow
                JSR     LOAD_FAT_BLK        ; load next block
PAGE_OVERFLOW   INC     CURR_CLUSTER+1      ; update upper byte of FAT index
READ_BYTE2      JSR     READ_ENTRY_BYTE     ; read second cluster byte
STORE_BYTE2     STA     CURR_CLUSTER+1      ; and store it as CURR_CLUSTER[1]
                LDX     ODD_CLUSTER         ; finalize result cluster#
                BEQ     EVEN_CLSTR
ODD_CLSTR       LDX     #$04                ; odd cluster# now looks like BA BB
SHIFT_LOOP      LSR                         ; rotate result 4 bits right
                ROR     CURR_CLUSTER
                DEX
                BNE     SHIFT_LOOP
                BEQ     STORE_BYTE3
EVEN_CLSTR      LDA     #$0F                ; even cluster# now looks like AA BA
                AND     CURR_CLUSTER+1      ; just clear bits[12:15]
STORE_BYTE3     STA     CURR_CLUSTER+1      ; result is AA 0A or BB 0B
CHECK_EOF       CMP     #$0F                ; check if result cluster = $FFF (EOF)
                BCC     FAT12_END
                LDA     CURR_CLUSTER
                CMP     #$FF
FAT12_END       RTS
                
; **** Decode FAT16 Entry ******************************************************
; **** a FAT16 cluster C is coded as CC CC *************************************

FAT16           LDX     CURR_CLUSTER+1      ; load byte 2 of current cluster
                JSR     ADD_32_8            ; and add it to FAT base block address
                JSR     LOAD_FAT_BLK        ; load this block
                LDA     CURR_CLUSTER        ; load index into the FAT block
                LDX     #$02                ; two bytes to read for a FAT16 entry
                JMP     (RW_FAT_ENTRY)      ; read/write FAT entry
                
; **** Decode FAT32 Entry ******************************************************
; **** a FAT32 cluster C is coded as CC CC CC 0C *******************************

FAT32           LDX     #<(CURR_CLUSTER+1)
                LDY     #>(CURR_CLUSTER+1)
                JSR     LOAD_S32            ; load CURR_CLUSTER[1:3] into SUM[0:2] = FAT block index
                LDX     #$00
                LDY     #$03
                STX     SUM32+3             ; clear garbage byte SUM[3]
                LDA     CURR_CLUSTER        ; load CURR_CLUSTER[1] = FAT entry index byte
                ASL                         ; shift bit 7 into carry flag and multiply entry index by 2
                PHA                         ; save entry index to stack
FAT32_LOOP      ROL     SUM32,X             ; shift bit 7 of entry index into bit 0 of block index
                INX
                DEY
                BNE     FAT32_LOOP
                JSR     ADD_32_32
                JSR     LOAD_FAT_BLK
                PLA                         ; restore entry index
                LDX     #$04                ; four bytes to read for a FAT32 entry
                JMP     (RW_FAT_ENTRY)      ; read/write FAT entry
                
; **** Read FAT 16 or FAT 32 Entry *********************************************
;
; INPUT : X - Length of FAT Entry in Bytes
; OUTPUT: C = 0 - Valid Cluster in CURR_CLUSTER[0:3]; C = 1 - EOF
;
; ******************************************************************************

RW_FAT_ENTRY    .word      READ_FAT_ENTRY

READ_FAT_ENTRY  LDY     #$FF                ; standard EOF mask is $FF
                STY     MASK
                ASL                         ; multiply entry index by 2
                TAY                         ; store entry index into Y
                STX     NCNT                ; store length of entry
                LDX     #$00
                STX     RES
LOOP_FAT_ENTRY  JSR     READ_ENTRY_BYTE     ; read entry byte
SET_ENTRY_BYTE  STA     CURR_CLUSTER,X      ; store byte in curr_cluster
                PHP                         ; save carry flag
                CPX     #$03                ; is this the upper byte of a FAT32 entry?
                BNE     CMP_MASK            ; no, just compare it with EOF mask
                LDX     #$0F                ; yes, we must change the mask to $0F
                STX     MASK
CMP_MASK        CMP     MASK                ; compare cluster byte with EOF mask
                BNE     READ_FAT_ENTRY1     ; if not equal just read the next byte
                STA     RES                 ; compare cluster byte is equal EOF mask
READ_FAT_ENTRY1 PLP                         ; restore carry flag
                INY
                INX
                DEC     NCNT
                BNE     LOOP_FAT_ENTRY      ; loop until all bytes copied
                LDA     RES
                CMP     #$FF                ; check if result cluster is $FFFF or $0FFFFFFF (EOF)
                RTS
                
; **** Read a Single FAT Entry Byte From Block Buffer **************************
;
; INPUT : Y - Index To FAT Entry Byte
; OUTPUT: A = Read Byte
;
; ******************************************************************************

READ_ENTRY_BYTE LDA     #$01
                AND     CURR_CLUSTER+1      ; check bit 0
                BNE     RD_UPPER_PAGE       ; if bit 0 = 1 then read byte from upper half of block
                LDA     BLOCK_BUFF,Y        ; read entry byte from lower half of block buffer
                RTS
RD_UPPER_PAGE   LDA     BLOCK_BUFF+256,Y    ; read entry byte from upper half of block buffer
                RTS
                
; **** Calculate LBA From Given Cluster Address ********************************
;
; INPUT : CURR_CLUSTER[0:3] - Current File Cluster
; OUTPUT: NUM32[0:3]        - LBA
;
; ******************************************************************************

CLUSTER_TO_BLK  LDX     #<CURR_CLUSTER
                LDY     #>CURR_CLUSTER
                
CLSTR_TO_BLK    JSR     LOAD_32
                LDA     D_SECT_PER_CLST
                LSR      
                BEQ     ADD_START_CLSTR
LOOP1           LDX     #$00
                LDY     #$04
                CLC
LOOP2           ROL     NUM32,X
                INX
                DEY
                BNE     LOOP2
                LSR      
                BNE     LOOP1
ADD_START_CLSTR LDX     #<D_START_CLUSTER
                LDY     #>D_START_CLUSTER
                JSR     LOAD_S32
                JMP     ADD_32_32

; **** Load A Block From FAT Into The Std Buffer *******************************
;
; ******************************************************************************

LOAD_FAT_BLK    LDX     #$03
                STX     RES                 ; initialize byte counter
                
; check if current FAT block and last loaded FAT block are identical ***********

CHK_FAT_BLK     LDA     NUM32,X             ; load one byte of new block pointer
                CMP     CURR_FAT_BLK,X      ; and compare it with the old one
                BNE     SET_CURR_FATBLK
                DEC     RES                 ; if equal decrement number of unequal bytes
SET_CURR_FATBLK STA     CURR_FAT_BLK,X      ; store new block pointer byte as current byte
                DEX
                BPL     CHK_FAT_BLK         ; compare and store more bytes if X >= 0
                LDA     RES                 ; RES is decremented down to -1 if all bytes equal
                BMI     LOAD_FAT_END        ; RES = $FF -> FAT block is already loaded, just exit
                
                LDA     BLKBUFL             ; save old memory pointer to stack
                PHA
                LDA     BLKBUFH
                PHA
                
                LDX     #<NUM32
                LDY     #>NUM32
                JSR     DEV_RD_LBLK_BUF     ; load FAT block into standard buffer
                
                PLA
                STA     BLKBUFH             ; restore old memory pointer from stack
                PLA
                STA     BLKBUFL
LOAD_FAT_END    RTS
                
; **** Read Logical Block To Standard Buffer ***********************************

DEV_RD_LBLK_BUF  LDA    #CMD_READ_BUF
                 JMP    CMDDEV
                
; **** Read Logical Block ******************************************************

DEV_RD_LBLK     LDA     #CMD_READ
                JMP     CMDDEV
                
; Arithmetic Functions *********************************************************
; ******************************************************************************

; **** Load a 16 Bit Value Into NUM32 ******************************************
;
; Input:  UInt16[X,Y] = 16 Bit Unsigned Integer
;
; ******************************************************************************

LOAD_16         STX     NUM32
                STY     NUM32+1
                LDX     #$00
                STX     NUM32+2
                STX     NUM32+3
                RTS
                
; **** Load a 32 Bit Value Into SUM32 ******************************************
;
; Input:  X,Y = Ptr[LO:HI] to 32 Bit Integer
;
; ******************************************************************************

LOAD_S32        STX     STOL
                LDX     #$07
                BNE     LOAD_32_1

; **** Load a 32 Bit Value Into NUM32 ******************************************
;
; Input:  X,Y = Ptr[LO:HI] to 32 Bit Integer
;
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
                
; **** Store 32 Bit Value In NUM32 To Destination At Ptr[X,Y] ******************
;
; Input:  X,Y = Ptr[LO:HI] to 32 Bit Integer
;
; ******************************************************************************

STORE_32        STX     STOL
                STY     STOH
STORE_32_D      LDY     #$03
LOOP_STORE_32   LDA     NUM32,Y
                STA     (STOL),Y
                DEY
                BPL     LOOP_STORE_32
                RTS

; **** Increment a 32 Bit Value ************************************************
;
; NUM32[0:3] = 32 Bit Integer - NUM32 = NUM32 + 1
;
; ******************************************************************************

INC_32          LDX     #$01

; **** Add a 8 Bit Value To a 32 Bit Value *************************************
;
; Input X = 8 Bit Integer, NUM32[0:3] = 32 Bit Integer - NUM32 = UInt[X] + NUM32
;
; ******************************************************************************

ADD_32_8        LDY     #$00

; **** Add a 16 Bit Value To a 32 Bit Value ************************************
;
; Input X,Y = 16 Bit Integer, NUM32[0:3] = 32 Bit Integer - NUM32 = UInt[X,Y] + NUM32
;
; ******************************************************************************

ADD_32_16       STX     SUM32
                STY     SUM32+1
                LDX     #$00
                STX     SUM32+2
                STX     SUM32+3

; **** Add a 32 Bit Value To a 32 Bit Value ************************************
;
; Input SUM32[0:3] = 32 Bit Integer, NUM32[0:3] = 32 Bit Integer  - NUM32 = SUM32 + NUM32
;
; ******************************************************************************

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
                
; ******************************************************************************
; Start of 2nd Block of BOOT.SYS ***********************************************
; ******************************************************************************

		ORG     $2200
BLOCK_2

; ******************************************************************************
; ******************************************************************************


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
                
; **** Write Logical Block From Standard Buffer ********************************

DEV_WR_LBLK_BUF  LDA     #CMD_WRITE_BUF
                 JMP     CMDDEV

; **** Write Logical Block *****************************************************

DEV_WR_LBLK     LDA     #CMD_WRITE
                JMP     CMDDEV
                
; ******************************************************************************
; *                               OS Entry Point                               *
; ******************************************************************************
                
OS_MAIN         LDX     #<MSG_BOOT       ; load boot message
                LDY     #>MSG_BOOT
                JSR     OS_STRING_OUT        ; and print it
                JSR     OS_SET_ROOT_DIR
                
; Clear Mount Table ************************************************************

                CLC
CLR_MOUNT_TABLE LDA     #NULL_ID
                STA     MOUNT_TABLE+8,X      ; set device ID to Null device
                TXA                          ; index to mount table into A
                ADC     #$20                 ; set to next entry
                TAX
                BCC     CLR_MOUNT_TABLE      ; repeat until all entries cleared
                
; Set Boot Device As Current Device ********************************************

                LDX     #$00
                LDY     #$00                 ; set index to mount table = 0
                LDA     D_DEV_ID             ; get boot device ID
CHECK_FDC1      CMP     #FDD1_ID             ; is boot drive FDD1?
                BEQ     SET_BOOT_DRV         ; yes, set current drive to A:
                INX
                LDY     #$20                 ; set index to mount table = 32
CHECK_FDC2      CMP     #FDD2_ID             ; is boot drive FDD2?
                BEQ     SET_BOOT_DRV         ; yes, set current drive to B:
                INX                          ; else set current drive (SDD or a HDD) to C:
                LDY     #$40                 ; set index to mount table = 64
SET_BOOT_DRV    STX     CURR_DRIVE

; Add Boot Device To Mount Table ***********************************************

                LDX     #$00
ADD_DEVBLK      LDA     CURR_VOLUME,X        ; read byte from current device block
                STA     MOUNT_TABLE,Y        ; and write it to mount table
                INX
                INY
                CPX     #32                  ; 32 byte to copy
                BNE     ADD_DEVBLK
                LDA     #$F8
                CMP     D_MEDIUM_DESCR       ; is boot device a harddisk?
                BNE     CHK_FAT_TYPE
                LDA     PSAV                 ; yes, load partition number
                STA     D_MEDIUM_DESCR       ; and save it as medium descriptor byte
CHK_FAT_TYPE    LDA     D_FAT_TYPE           ; get type of FAT
                BEQ     OS_SHELL_ENTRY       ; FAT32 has no static root directory blocks
                LDX     #$04
CALC_DIR_BLKS   LSR     D_NUM_ROOT_DIR+1     ; divide number of root directory entries by 16
                ROR     D_NUM_ROOT_DIR       ; to get total number of root directory blocks
                DEX
                BNE     CALC_DIR_BLKS
                
; TODO: ADD MORE DEVICES #######################################################

                
; **** Shell Entry Point *******************************************************
;
; ******************************************************************************
                
OS_SHELL_ENTRY  JMP     SH_CMD_PROMPT

; **** Read First Block Of Actual Directory ************************************
;
; ******************************************************************************

OS_FIRST_DIR_BLK
                LDA     D_FAT_TYPE           ; load type of FAT
                BEQ     SET_DIR_CLUSTER      ; is it FAT32? Yes, set cluster
                JSR     OS_IS_ROOT_DIR       ; no. Is it the root directory?
                BCC     SET_DIR_CLUSTER      ; no, set cluster
SET_DIR_BLOCK   LDX     #$03
SET_BLOCK       LDA     D_START_DIR,X
                STA     CURR_DIR_BLK,X       ; set first block of root dir as current directory block
                DEX
                BPL     SET_BLOCK            ; copy all four LBA bytes
                LDX     D_NUM_ROOT_DIR       ; set block counter
                BNE     SET_NUM_BLOCKS       ; branch always
SET_DIR_CLUSTER LDX     #$03
SET_CLUSTER     LDA     D_ACTUAL_DIR,X       ; set actual dir cluster as current cluster
                STA     CURR_CLUSTER,X
                DEX
                BPL     SET_CLUSTER
CONVERT_CLUSTER JSR     CLUSTER_TO_BLK       ; convert cluster number to block number
                LDX     #<CURR_DIR_BLK
                LDY     #>CURR_DIR_BLK
                JSR     STORE_32             ; and save result as current directory block
                LDX     D_SECT_PER_CLST      ; load number of blocks per clusters
SET_NUM_BLOCKS  STX     CURR_BLK_NUM

; **** Load Directory Block ****************************************************
;
; ******************************************************************************

OS_LOAD_DIR     LDA     #<DIR_BLK_BUF   ; set destination block buffer
                STA     BLKBUFL
                LDA     #>DIR_BLK_BUF
                STA     BLKBUFH
                LDX     #<CURR_DIR_BLK   ; block number (LBA) to be loaded
                LDY     #>CURR_DIR_BLK
                JSR     DEV_RD_LBLK          ; read directory block
                RTS

; **** Read Next Directory Block ***********************************************
;
; ******************************************************************************

OS_NEXT_DIR_BLK LDX     #$00
INC_DIR_BLK     INC     CURR_DIR_BLK,X       ; increment current directory block
                BNE     OS_LOAD_DIR
                INX                          ; overflow, increment next byte
                CMP     #$04                 ; all four bytes updated?
                BNE     INC_DIR_BLK
                BEQ     OS_LOAD_DIR          ; branch always

; **** Read Next Directory Cluster *********************************************
;
; ******************************************************************************

OS_NEXT_DIR_CLSTR
                JSR     GET_NEXT_CLSTR       ; load next directory cluster
                BCC     CONVERT_CLUSTER      ; if not EOF convert it to LBA
                RTS
                
; **** Save Directory Block ****************************************************
;
; ******************************************************************************

OS_SAVE_DIR     LDA     #<DIR_BLK_BUF   ; set source block buffer
                STA     BLKBUFL
                LDA     #>DIR_BLK_BUF
                STA     BLKBUFH
                LDX     #<CURR_DIR_BLK  ; pointer to block number (LBA) to be saved
                LDY     #>CURR_DIR_BLK
                JSR     DEV_WR_LBLK         ; write directory block
                RTS
                
; **** Create New File *********************************************************
;
; Input: FILENAME = String8_3
;        A        = File Attributes
;
; ******************************************************************************

OS_CREATE_FILE  TAX                         ; save attributes into X
                LDY     #D_FILENAME         ; set index to filename
FILL_FILENAME   LDA     FILENAME,Y          ; copy filename into current directory entry
                STA     (CURR_DIR_ENTRY),Y
                INY
                CPY     #D_ATTRIBUTES       ; all characters copied?
                BCC     FILL_FILENAME       ; no, copy more characters
                TXA                         ; yes, get attribute back to A
CLEAR_ENTRY     STA     (CURR_DIR_ENTRY),Y  ; copy attributes into current directory entry
                LDA     #$00                ; and clear all following bytes to 0
                INY
                CPY     #$20
                BCC     CLEAR_ENTRY
GET_DATE_TIME   JSR     OS_FILEDATE         ; get current date as file date
                TYA
                LDY     #D_CREATION_D+1     ; set index to creation date upper byte
                STA     (CURR_DIR_ENTRY),Y  ; store upper byte of file date
                TXA
                DEY                         ; set index to creation date lower byte
                STA     (CURR_DIR_ENTRY),Y  ; store lower byte of file date
                JSR     OS_FILETIME         ; get current time as file time
                TYA
                LDY     #D_CREATION_T+1     ; set index to creation time upper byte
                STA     (CURR_DIR_ENTRY),Y  ; store upper byte of file time
                TXA
                DEY                         ; set index to creation time lower byte
                STA     (CURR_DIR_ENTRY),Y  ; store lower byte of file time
                JSR     OS_SAVE_DIR
                RTS
                
; **** Get Current Time As File Time *******************************************
;
; Output: File Time = Word[X,Y]
;
; ******************************************************************************

OS_FILETIME     JSR     READTIME            ; read current time

; **** Convert Time To File Time ***********************************************
;
; Input:  A - HOUR 	in BCD ($00-$23)
;	  X - MINUTE 	in BCD ($00-$59)
;	  Y - SECONDS	in BCD ($00-$59)
; Output: File Time = Word[X,Y]
;
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
;
; Output: File Date = Word[X,Y]
;
; ******************************************************************************
                
OS_FILEDATE     JSR     READDATE            ; read current date

; **** Convert Date To File Date ***********************************************
;
; Input:  A - YEAR 	in BCD ($00-$99)
; 	  X - MONTH 	in BCD ($01-$12)
; 	  Y - DAY	in BCD ($01-$31)
; Output: File Date = Word[X,Y]
;
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
                STA     SUM32
                STX     SUM32+1
                STY     SUM32+2
                LDY     #$02
CONVERT_BCD     LDA     SUM32,Y
                JSR     BCD_TO_BIN
                STA     SUM32,Y
                DEY
                BPL     CONVERT_BCD
                RTS

; **** Load COM File ***********************************************************
;
; ******************************************************************************

OS_LOAD_COM     JSR     OS_FILE_EMPTY       ; check if file is empty
                BCC     OS_LOAD_COM2
                RTS                         ; file is empty, just do nothing
OS_LOAD_COM2    LDA     #$00
                STA     OS_PROG
                STA     BLKBUFL
                LDA     #$30
                STA     OS_PROG+1           ; start address of COM file is $3000
                STA     BLKBUFH
                LDX     #<CURR_CLUSTER      ; ##### LBA
                LDY     #>CURR_CLUSTER      ; ##### LBA
                JSR     CLSTR_TO_BLK        ; convert cluster number to block number
                LDA     #$00                ; get file size in blocks
                STA     BCNT
                LDY     #D_FILE_SIZE+1      ; index to file size entry
                LDA     (CURR_DIR_ENTRY),Y  ; load file size byte 1
                LSR                         ; check if bit 0 is set
                PHA
                BCS     LOAD_COM1           ; yes, add one block
                DEY
                LDA     (CURR_DIR_ENTRY),Y  ; load file size byte 0
                BEQ     LOAD_COM1           ; check if <> 0
                SEC                         ; yes, add one block
LOAD_COM1       PLA
                ADC     BCNT                ; calc used blocks
                STA     BCNT
                CMP     #89                 ; more than 88 block?
                BCS     OS_SIZE_ERR         ; yes, file is too big
                LDY     #D_FILE_SIZE+2      ; index to file size entry
                LDA     (CURR_DIR_ENTRY),Y  ; load file size byte 2
                BNE     OS_SIZE_ERR         ; if byte 2 <> 0 -> file is too big
                INY
                LDA     (CURR_DIR_ENTRY),Y  ; load file size byte 3
                BNE     OS_SIZE_ERR         ; if byte 3 <> 0 -> file is too big
                JSR     LOAD_BLK0
                LDA     #HDD1_ID   	    ; Replace by D_DEV_ID?
		JMP     OPEN_DEVICE
		
OS_SIZE_ERR     LDX     #<MSG_SIZE_ERR      ; load error message...
                LDY     #>MSG_SIZE_ERR
                JMP     OS_PRINT_ERR

; **** Read Next File Block ****************************************************
;
; ******************************************************************************

OS_LOAD_BLK     DEC     BCNT                ; first block is already read in
                BEQ     OS_EXECUTE          ; more blocks to read? No, jump to program start
                LDA     D_SECT_PER_CLST     ; load numbers of sectors per cluster
                STA     SCNT                ; and store it in temp
LOAD_NEXT_BLK0  DEC     SCNT                ; read next block in cluster
                BEQ     NEXT_CLUSTER0       ; all blocks in cluster read? Yes, read next cluster
                JSR     INC_32              ; no, increment block address
LOAD_BLK0       LDX     #<NUM32
                LDY     #>NUM32
                JSR     DEV_RD_LBLK         ; and read next block of file
                DEC     BCNT
                BEQ     OS_EXECUTE          ; more blocks to read? No, jump to program start
                BNE     LOAD_NEXT_BLK0      ; yes, read next block in cluster

; next cluster needs to be loaded considering the volume FAT type **************

NEXT_CLUSTER0   JSR     GET_NEXT_CLSTR
                BCS     OS_EXECUTE
                JSR     CLUSTER_TO_BLK
                LDA     D_SECT_PER_CLST     ; load numbers of sectors per cluster
                STA     SCNT                ; and store it in temp
                JMP     LOAD_BLK0
                
; ******************************************************************************

INIT_FREE_CLUSTER
                LDA     #$02                ; first data cluster is $000002
                STA     FREE_CLUSTER
                LDA     #$00
                STA     FREE_CLUSTER+1
                STA     FREE_CLUSTER+2
                STA     FREE_CLUSTER+3
                RTS
                
; ******************************************************************************
                
OS_NEXT_FREE_CLUSTER
                LDX     #$03
SET_CURR_CLSTR  LDA     FREE_CLUSTER,X
                STA     CURR_CLUSTER,X
                DEX
                BPL     SET_CURR_CLSTR
                JSR     GET_NEXT_CLSTR      ; get FAT entry for current cluster
                LDX     #$03
CHK_FREE        LDA     CURR_CLUSTER,X
                BNE     SET_NEXT_CLSTR      ; check if cluster is free
                DEX
                BPL     CHK_FREE
                SEC                         ; cluster is free, exit with C = 1
                RTS
SET_NEXT_CLSTR  LDX     #$00
INC_FREE_CLSTR  INC     FREE_CLUSTER,X      ; increment free cluster pointer
                BNE     CHK_MAX_FAT
                INX
                CPX     #$04
                BNE     INC_FREE_CLSTR
CHK_MAX_FAT     LDX     #03
CHK_MAX_FAT2    LDA     CURR_FAT_BLK,X
                CMP     D_START_FAT2,X      ; end of FAT1 reached?
                BCC     OS_NEXT_FREE_CLUSTER; no, expect next FAT entry
                DEX
                BPL     CHK_MAX_FAT2
                ; CURR_FAT_BLK = D_START_FAT2 ; CHECK IF END OF FAT, START OVER
                                              ; REPEAT UNTIL AT START OF FREE CLUSTER
                                              ; SEARCH
                CLC                         ; no emtpy cluster found, exit with error
                RTS

; **** Execute File ************************************************************
;
; Input:  Ptr[OS_PROG] to Start Address
; Output: A - Result Code
;
; ******************************************************************************

OS_EXECUTE      JMP     (OS_PROG)           ; call by indirect jump
OS_PROG         .word      $0000


; ******************************************************************************
; ******************************************************************************

WRITE_FAT_ENTRY LDY     #$FF                ; standard EOF mask is $FF
;                STY     MASK
;                ASL     A                   ; multiply entry index by 2
;                TAY                         ; store entry index into Y
;                STX     NCNT                ; store length of entry
;                LDX     #$00
;                STX     RES
;LOOP_FAT_ENTRY  JSR     READ_ENTRY_BYTE     ; read entry byte
;SET_ENTRY_BYTE  STA     CURR_CLUSTER,X      ; store byte in curr_cluster
;                PHP                         ; save carry flag
;                CPX     #$03                ; is this the upper byte of a FAT32 entry?
;                BNE     CMP_MASK            ; no, just compare it with EOF mask
;                LDX     #$0F                ; yes, we must change the mask to $0F
;                STX     MASK
;CMP_MASK        CMP     MASK                ; compare cluster byte with EOF mask
;                BNE     READ_FAT_ENTRY1     ; if not equal just read the next byte
;                STA     RES                 ; compare cluster byte is equal EOF mask
;READ_FAT_ENTRY1 PLP                         ; restore carry flag
;                INY
;                INX
;                DEC     NCNT
;                BNE     LOOP_FAT_ENTRY      ; loop until all bytes copied
;                LDA     RES
;                CMP     #$FF                ; check if result cluster is $FFFF or $0FFFFFFF (EOF)
                RTS

; **** Write a Single FAT Entry Byte To Block Buffer ***************************
;
; INPUT : A = Write Data
;         Y - Index To FAT Entry Byte
;
; ******************************************************************************

WRITE_ENTRY_BYTE
		TAX
                LDA     CURR_CLUSTER+1
                LSR                        	; check bit 0 of curr_cluster[1]
                TXA
                BCS     WR_UPPER_PAGE       ; if bit 0 = 1 then write byte to upper half of block
                STA     BLOCK_BUFF,Y        ; write entry byte to lower half of block buffer
                RTS
WR_UPPER_PAGE   STA     BLOCK_BUFF+256,Y    ; write entry byte to upper half of block buffer
                RTS
                
                
SET_FAT_READ    LDX     #<READ_ENTRY_BYTE
                LDY     #>READ_ENTRY_BYTE
                BNE     SET_RW_FAT
                
SET_FAT_WRITE   LDX     #<WRITE_ENTRY_BYTE
                LDY     #>WRITE_ENTRY_BYTE
SET_RW_FAT      STX     RW_FAT_ENTRY
                STY     RW_FAT_ENTRY+1
                RTS
                
; **** Add First Cluster To Empty File *****************************************
;
; ******************************************************************************
                
OS_ADD_CLUSTER  JSR     OS_NEXT_FREE_CLUSTER
                BCC     ADD_CLUSTER_END     ; no free cluster found
                JSR     SET_FAT_WRITE ; indirekte jump adresse auf WRITE_FAT_ENTRY setzen
                ; set EOF ($FFFF / $0FFFFFFF) at Ptr[curr_cluster]
                ; save curr_cluster into directory start_address
                JSR     SET_FAT_READ  ; indirekte jump adresse auf READ_FAT_ENTRY setzen
ADD_CLUSTER_END RTS

; **** Create New File *********************************************************
;
; Input:  A = File Attributes
; Output: C = 0 - Error; C = 1 - No Error
;         A = $FF - File/Directory already exists; A <> $FF Write Error Codes
;
; ******************************************************************************

OS_CREATE       STA     F_ATTRIBS           ; save attributes
                AND     #FA_DIRECTORY       ; create a directory?
                BEQ     ADD_FILE            ; no, add a file
                JSR     OS_DIR_EXISTS       ; yes, check if directory already exists
                JSR     CREATE_ENTRY        ; and try to create a dir entry
                BCC     OS_CREATE_END       ; an error occured, exit
                ; ##########################
                ; add new directory cluster
                ; and add . and .. directories
                ; ##########################
                RTS
                
ADD_FILE        JSR     OS_FILE_EXISTS      ; check if file already exists
CREATE_ENTRY    LDA     #$FF                ; error result -1 if file/dir exists
                BCS     OS_CREATE_ERR       ; file already exists, exit with error $FF
                LDA     TERM_FLAG           ; load counter termination flag
                BNE     REUSE_DIR_ENTRY     ; found a deleted directory entry?
ADD_DIR_ENTRY   LDA     #$01                ; no, add a new directory entry
                JMP     OS_CREATE_ERR       ; need to add block - TEMP show write error
;                LDA     D_FAT_TYPE
;                BEQ     ADD_SUB_DIR
;                JSR     OS_IS_ROOT_DIR
;                BCS     ADD_SUB_DIR
; ADD_ROOT_DIR   ...
;ADD_SUB_DIR     ...
REUSE_DIR_ENTRY LDY     #$05                ; deleted dir entry found. Reuse it
LOAD_CURR_DIR   LDA     SEL_DIR_BLK,Y
                STA     CURR_DIR_BLK,Y      ; set current_dir_blk and curr_dir_entry
                DEY
                BPL     LOAD_CURR_DIR
                LDA     F_ATTRIBS
                JSR     OS_CREATE_FILE      ; create the file on disk
                BCC     OS_CREATE_END
                RTS
OS_CREATE_ERR   CLC
OS_CREATE_END   RTS
                
; **** Open File ***************************************************************
;
; ******************************************************************************

OS_OPEN         RTS

; **** Close File **************************************************************
;
; ******************************************************************************

OS_CLOSE        RTS

; **** Set File Size ***********************************************************
;
; ******************************************************************************

OS_SET_SIZE     RTS

; **** Delete File *************************************************************
;
; ******************************************************************************

OS_DELETE       RTS

; **** Test If File Is Empty ************** ************************************
;
; Input:  Ptr(CURR_DIR_ENTRY)
; Output: C = 1 - File is empty; C = 0 - File not empty
;
; ******************************************************************************

OS_FILE_EMPTY   LDX     #$04
                LDY     #D_FILE_SIZE        ; index to file size
                CLC
NEXT_SIZE_BYTE  LDA     (CURR_DIR_ENTRY),Y  ; load file size byte
                BNE     OS_FILE_EMPTY2      ; if byte <> 0 then file not empty
                INY
                DEX
                BNE     NEXT_SIZE_BYTE      ; test next byte
                SEC
OS_FILE_EMPTY2  RTS

; **** Check If Directory Already Exists ***************************************
;
; ******************************************************************************

OS_DIR_EXISTS   LDX     #<CB_DIR_EXISTS
                LDY     #>CB_DIR_EXISTS
                BNE     OS_FIND
                
; **** Check If Directory Is Available *****************************************
;
; ******************************************************************************

OS_FILE_EXISTS  LDX     #<CB_FILE_EXISTS
                LDY     #>CB_FILE_EXISTS
                BNE     OS_FIND

; **** Find Directory **********************************************************
;
; ******************************************************************************

OS_FIND_PATH    LDX     #<CB_FIND_SUBDIR
                LDY     #>CB_FIND_SUBDIR
                BNE     OS_FIND

; **** Find File ***************************************************************
;
; ******************************************************************************

OS_FIND_FILE    LDX     #<CB_FIND_FILE
                LDY     #>CB_FIND_FILE
                                           ; fall through to OS_FIND
                                           
; **** Find All Files **********************************************************
;
; ******************************************************************************
                                           
OS_FIND_ALL     LDA     #$FF                ; disable total file counting
                BNE     OS_FIND2

; **** Main Find Routine *******************************************************
;
; ******************************************************************************

OS_FIND         LDA     #$00                ; enable total file counting
OS_FIND2        STX     CMD_ADDR            ; set command function address
                STY     CMD_ADDR+1
                                            ; fall through to OS_DIR_LOOP

; **** Loop Through Actual Directory *******************************************
;
; Input:  Ptr[X:Y] = Address to command specific function
;         A = $00 - Enable total file counting; A <> $00 - counting disabled
; Output: C = 1 - Found; C = 0 - Not Found
;
; ******************************************************************************

OS_DIR_LOOP     STA     TERM_FLAG           ; set/clear counter termination flag
                LDA     #$00
                STA     CURR_FILE_CNT       ; clear total file counter
                STA     CURR_FILE_CNT+1
                JSR     OS_FIRST_DIR_BLK    ; load first block of actual directory
LOWER_DIR_BLK   LDX     #<DIR_BLK_BUFL  ; set pointer to lower page of block buffer
                LDY     #>DIR_BLK_BUFL
SET_CURR_ENTRY  STX     CURR_DIR_ENTRY      ; select current directory entry
                STY     CURR_DIR_ENTRY+1
GET_CURR_ENTRY  LDY     #D_ATTRIBUTES       ; index to file attributes
                LDA     (CURR_DIR_ENTRY),Y  ; load file attributes
                TAX                         ; load attributes into X
                LDY     #D_FILENAME         ; index to filename
                LDA     (CURR_DIR_ENTRY),Y  ; load first char of filename
                BEQ     OS_DIR_LOOP_EOF     ; if NULL then last entry
                JSR     CMD_EXECUTE         ; call command routine
                BCS     OS_DIR_LOOP_END
NEXT_ITEM       LDA     TERM_FLAG           ; check if count is terminated
                BNE     END_LOOP?           ; flag still $00? No, check if end of loop
                INC     CURR_FILE_CNT       ; yes, increment total file counter
                BNE     END_LOOP?           ; overflow? No, check if end of loop
                INC     CURR_FILE_CNT+1     ; yes, increment upper counter byte
END_LOOP?       CLC
                LDA     #$20                ; index to next directory entry
                ADC     CURR_DIR_ENTRY
                STA     CURR_DIR_ENTRY
                BCC     GET_CURR_ENTRY
                LDA     #$00
                ADC     CURR_DIR_ENTRY+1
                STA     CURR_DIR_ENTRY+1
                CMP     #(> DIR_BLK_BUFH)+1
                BNE     UPPER_DIR_BLK       ; are we in the second page of the block buffer?
                DEC     CURR_BLK_NUM
                BNE     LOAD_DIR_BLK
                LDA     D_FAT_TYPE          ; get FAT type
                BEQ     LOAD_DIR_CLSTR      ; if FAT32 then load next directory cluster
                BIT     D_ATTRIBUTES        ; else check if root directory
                BCS     OS_DIR_LOOP_EOF     ; if root dir and not FAT32, all directory blocks read. Exit
LOAD_DIR_CLSTR  JSR     OS_NEXT_DIR_CLSTR   ; load next directory cluster from device
                BCC     LOWER_DIR_BLK       ; and reset read pointer to lower page of block buffer
                BCS     OS_DIR_LOOP_EOF     ; directory EOF reached. Exit
LOAD_DIR_BLK    JSR     OS_NEXT_DIR_BLK     ; load next directory block from device
                JMP     LOWER_DIR_BLK       ; and reset read pointer to lower page of block buffer
UPPER_DIR_BLK   LDX     #<DIR_BLK_BUFH      ; set pointer to upper page of block buffer
                LDY     #>DIR_BLK_BUFH
                JMP     SET_CURR_ENTRY
OS_DIR_LOOP_EOF CLC
OS_DIR_LOOP_END RTS

; **** Set Drive Command *******************************************************
;
; Input:  A = Drive Number (0..25)
; Output: C = 0 - Error
;
; ******************************************************************************

OS_SET_DRIVE    CMP     CURR_DRIVE          ; is actual drive equal current drive?
                BEQ     SET_DRIVE_END       ; yes, just exit
                CMP     #$08                ; is actual drive number exceeding max drive?
                BCS     SET_DRIVE_ERR       ; yes, show error message
                TAX                         ; save actual drive number to Y
                LDA     CURR_DRIVE          ; load current drive number into A
                JSR     SHL_A_5             ; multiply current drive number by 32
                STA     MNT_TABLE           ; and save it to indirect pointer low byte
                LDA     #> MOUNT_TABLE
                STA     MNT_TABLE+1         ; set high byte of mount table
                LDY     #$20
UPDATE_DEV_DESC LDA     CURR_VOLUME,Y       ; update descriptor in mount table
                STA     (MNT_TABLE),Y
                DEY
                BPL     UPDATE_DEV_DESC
                TXA
                JSR     SHL_A_5             ; multiply actual drive number by 32
                STA     MNT_TABLE           ; and save it to indirect pointer low byte
                LDY     #$08                ; set index to D_DEV_ID
                LDA     (MNT_TABLE),Y       ; load device ID
                BEQ     SET_DRIVE_ERR       ; if NULL device then show error message
                LDY     #$20
LOAD_DEV_DESC   LDA     (MNT_TABLE),Y
                STA     CURR_VOLUME,Y       ; load device descriptor
                DEY
                BPL     LOAD_DEV_DESC
                STX     CURR_DRIVE          ; store actual drive as current drive

; TODO: Invalidate Block Buffers ###########################################################################

                LDA     D_DEV_ID            ; get current device id
                JSR     OPEN_DEVICE         ; and open the device driver
SET_DRIVE_END   SEC
                RTS

SET_DRIVE_ERR   LDX     #<MSG_DRIVE_ERR
                LDY     #>MSG_DRIVE_ERR
                JMP     OS_PRINT_ERR

; **** Test If Root Directory **************************************************
;
; Input:
; Output: C = 1 - is root dir; C = 0 - is not root dir
;
; ******************************************************************************

OS_IS_ROOT_DIR  LDY     #$03
COMP_DIR        LDA     D_ACTUAL_DIR,Y
                CMP     D_START_DIR,Y
                BNE     OS_IS_ROOT_END
                DEY
                BPL     COMP_DIR             ; test next LBA byte
                SEC
                RTS
OS_IS_ROOT_END  CLC
                RTS

; **** Set Root Directory Command **********************************************
;
; ******************************************************************************

OS_SET_ROOT_DIR LDX     #<D_START_DIR
                LDY     #>D_START_DIR

; **** Set Directory Command ***************************************************
;
; Input : X,Y = Ptr[LO:HI] to directory LBA
;
; ******************************************************************************

OS_SET_DIR      STX     DIR_PTR
                STY     DIR_PTR+1
                LDY     #$03
                LDX     #$04
SET_DIR_ADDR    LDA     (DIR_PTR),Y          ; get LBA to directory
                BNE     SET_DIR_ADDR2        ; test if LBA is $00000000
                DEX
                BEQ     OS_SET_ROOT_DIR      ; yes, set root directory
SET_DIR_ADDR2   STA     D_ACTUAL_DIR,Y       ; store it as actual directory
                STA     CURR_DIR_BLK,Y       ; and as current directory block
                DEY
                BPL     SET_DIR_ADDR         ; copy all four LBA bytes
                RTS
                
; **** Read Input String *******************************************************
;
; Output: Null terminated string in STRBUF
;
; ******************************************************************************

OS_STRING_IN    JSR     STRIN                ; input string into string buffer
                LDA     #<STRBUF         ; set string pointer to buffer
                STA     PSTR
                LDA     #>STRBUF
                STA     PSTR+1
                LDA     #$00
                STA     STRBUF,X             ; terminate string with NULL
                RTS

; **** Print Message String ****************************************************
;
; Input:  Ptr[X:Y] = Pointer to Message String
;
; ******************************************************************************

OS_STRING_OUT   STX     PSTR
                STY     PSTR+1
                JMP     STROUT

; **** Print Error Message *****************************************************
;
; Input:  Ptr[X:Y] = Pointer to Error Message
; Output: C = 0
;
; ******************************************************************************

OS_PRINT_ERR    JSR     OS_STRING_OUT
                CLC
                RTS
                
; **** Parse Full Path String **************************************************
;
; Input:  Ptr[X:Y] to Path String
; Output: C = 0 - Error; C = 1 - No Error
;         A = $00       - End Of String
;           = $FF       - Path Not Found
;           = ?         - Wildcard Included
;           = PATH_SEP  - No Trailing Name
;
; ******************************************************************************

OS_PARSE_PATH   STX     PSTR                ; save string pointer
                STY     PSTR+1
                LDY     #$00
                STY     TERM_CHAR
                LDA     #PATH_SEP
                CMP     (PSTR),Y            ; check if first char is the path seperator
                BNE     PARSE_PATH2         ; no, just check the path
                JSR     OS_SET_ROOT_DIR     ; yes, switch to root directory
                LDY     #$00
PARSE_PATH      INY
PARSE_PATH2     LDA     (PSTR),Y            ; load next char from path string
                BEQ     PARSE_TERM          ; is it a NULL char? yes, set termination char
                CMP     #SPC                ; is it a SPACE char?
                BEQ     PARSE_TERM          ; yes, set termination char
PARSE_NAME      JSR     OS_PARSE_NAME       ; no, parse partial path name
                BCC     PARSE_PATH_END      ; filename includes forbidden chars, exit with error
                CMP     #PATH_SEP           ; is termination char the path seperator?
                BNE     PARSE_PATH_OK       ; no, it's a trailing name, exit without error
                STY     PSAV                ; save string index
                JSR     OS_FIND_PATH        ; yes, find and switch patch
                LDY     PSAV                ; restore string index
                BCC     PARSE_PATH_ERR      ; path not found, exit with error
                BCS     PARSE_PATH          ; branch always
                
PARSE_TERM      CLC
                LDA     TERM_CHAR
                BNE     PARSE_PATH_END2
                LDA     #PATH_SEP
                STA     TERM_CHAR
                BNE     PARSE_PATH_OK
                
PARSE_PATH_ERR  LDA     #$FF                ; path not found error
                BNE     PARSE_PATH_END2
PARSE_PATH_OK   SEC
PARSE_PATH_END  LDA     TERM_CHAR           ; load termination char as error status into A
PARSE_PATH_END2 RTS

; **** Parse A Partial Path Name String ****************************************
;
; Input:  A = First Char Of Partial Path
;         Y = Index Into Path String
; Output: C = 0 - Error; C = 1 - No Error
;         TERM_CHAR = NULL      - Filename
;                   = ?         - Wildcard Chars Included
;                   = PATH_SEP  - Subdirectory Name
;
; ******************************************************************************

OS_PARSE_NAME   LDX     #8
                STX     BCNT                ; set max char count to 8
                LDX     #$00                ; reset index to name string buffer
                STX     TERM_CHAR
CHK_DOT         CMP     #DOT                ; is first char a . char? (. dir?)
                BNE     GET_CHAR            ; no, get other chars
                STA     FILENAME,X          ; yes, store it
                INY
                INX
                LDA     (PSTR),Y            ; get next char from input string
                CMP     #DOT                ; is second char a . char? (.. dir?)
                BNE     GET_CHAR            ; no, get other chars
SET_CHAR        STA     FILENAME,X          ; yes, store it
NEXT_CHAR       INX                         ; point to next char of parsed name
NEXT_CHAR2      INY                         ; point to next char of input string
GET_CHAR        LDA     (PSTR),Y            ; get next char from input string
                BEQ     FILL_ALL            ; end of line? yes, terminate
                CMP     #SPC                ; is it a space char?
                BEQ     FILL_ALL            ; yes, terminate
                CMP     #PATH_SEP           ; is it a path seperator char?
                BEQ     SET_TERM_CHAR       ; yes, terminate
                CMP     #DOT                ; is it a . char?
                BEQ     FILL_NAME           ; yes, fill name with spaces
                CMP     #'*'                ; is it a * char?
                BEQ     FILL_WILDCARD       ; yes, fill name with ?
                CPX     BCNT                ; name length exceeds max length?
                BCS     NEXT_CHAR           ; yes, skip storing chars
                JSR     UPPERCASE           ; convert chars to upper case
                CMP     #'?'                ; is it a ? char?
                BNE     GET_CHAR2           ; no, check next allowed char
                STA     TERM_CHAR           ; yes, store ? as termination char
                BEQ     SET_CHAR            ; and char in name buffer
GET_CHAR2       CMP     #MINUS              ; is it a - char?
                BEQ     SET_CHAR            ; yes, store char in name buffer
                CMP     #ULINE              ; is it a _ char?
                BEQ     SET_CHAR            ; yes, store char in name buffer
                CMP     #'~'                ; is it a ~ char?
                BEQ     SET_CHAR            ; yes, store char in name buffer
                CMP     #'0'                ; is char in range 0..9?
                BCC     PARSE_NAME_END      ; no, show error message
                CMP     #':'
                BCC     SET_CHAR            ; yes, store char in name buffer
                CMP     #'A'                ; is char in range A..Z?
                BCC     PARSE_NAME_END      ; no, show error message
                CMP     #'['
                BCC     SET_CHAR            ; yes, store char in name buffer
FILL_WILDCARD   LDA     #'?'
                STA     TERM_CHAR           ; store ? as termination char
                JSR     FILL_CHAR           ; fill with ? chars
                BCS     NEXT_CHAR2          ; branch always
FILL_NAME       JSR     SET_SPC_CHAR        ; fill with space chars
                LDA     #11
                STA     BCNT                ; set max char count to 11
                BCS     NEXT_CHAR2          ; branch always
SET_TERM_CHAR   STA     TERM_CHAR           ; store termination character
FILL_ALL        LDA     #11
                STA     BCNT                ; set max char count to 11
SET_SPC_CHAR    LDA     #SPC                ; set space char as filling char
FILL_CHAR       CPX     BCNT                ; max char count exceeded?
                BCS     PARSE_NAME_END      ; yes, exit
                STA     FILENAME,X          ; no, store char in name buffer
                INX
                BCC     FILL_CHAR           ; and repeat
PARSE_NAME_END  LDA     TERM_CHAR
                RTS
                
; **** Compare File Name With Mask *********************************************
;
; Output: C = 0 - Names not equal; C = 1 - Names equal
;
; ******************************************************************************

OS_COMP_NAME    LDY     #10                 ; compare all characters
COMPARE_CHAR    LDA     FILENAME,Y          ; get char from compare mask
                CMP     (CURR_DIR_ENTRY),Y  ; compare character
                BNE     COMPARE_NEQ         ; if not equal, exit with Carry = 0
                DEY
                BPL     COMPARE_CHAR        ; more character to compare
COMPARE_EQU     SEC                         ; all characters are equal, set Carry = 1
                RTS
COMPARE_NEQ     CLC
                RTS
                
; ******************************************************************************
                
OS_FILE_DELETED CMP     #$E5                ; is file deleted?
                BNE     COMPARE_NEQ         ; no, exit
                LDY     TERM_FLAG           ; termination flag already set?
                BNE     COMPARE_EQU         ; yes, exit
                STA     TERM_FLAG           ; no, set counter termination flag
                LDY     #$05
SAVE_CURR_DIR   LDA     CURR_DIR_BLK,Y      ; save CURR_DIR_BLK and CURR_DIR_ENTRY
                STA     SEL_DIR_BLK,Y
                DEY
                BPL     SAVE_CURR_DIR
                SEC
                RTS
                
SEL_DIR_BLK     .byte $00, $00, $00, $00
SEL_DIR_ENTRY   .byte $00, $00
                
; **** Directory Loop Call Back Functions **************************************
;
; Input : A - First character of filename
;         X - File Attributes
; Output: C = 1 - File found; C = 0 - File not found
;
; ******************************************************************************

; **** Directory Exists - Call Back Routine ************************************

CB_DIR_EXISTS   JSR     OS_FILE_DELETED

; **** Find Sub Directory Name - Call Back Routine *****************************

CB_FIND_SUBDIR  CPX     #$0F                ; is it a long filename entry?
                BEQ     CB_FIND_END         ; yes, skip to next entry
                LDA     FILENAME            ; do we search for the . directory?
                CMP     #DOT
                BNE     FIND_SUBDIR
                LDA     FILENAME+1
                CMP     #SPC
                BEQ     FIND_SUBDIR_END     ; yes, do nothing, just exit.
FIND_SUBDIR     TXA
                AND     #FA_DIRECTORY       ; is it a directory?
                BEQ     CB_FIND_END         ; no, exit
                JSR     OS_COMP_NAME        ; yes, compare name with search mask
                BCC     CB_FIND_END2        ; name not equal, get next dir entry
                CLC
                LDA     CURR_DIR_ENTRY      ; load  pointer to directory start cluster into [X:Y]
                ADC     #D_START_CLST
                TAX                         ; low byte of pointer
                LDA     CURR_DIR_ENTRY+1
                ADC     #$00
                TAY                         ; high byte of pointer
                JSR     OS_SET_DIR          ; and make directory the actual directory
FIND_SUBDIR_END SEC                         ; subdirectory found
                RTS
                
; **** File Exists - Call Back Routine *****************************************
                
CB_FILE_EXISTS  JSR     OS_FILE_DELETED

; **** Find File Name - Call Back Routine **************************************

CB_FIND_FILE    CPX     #$0F                ; is it a long filename entry?
                BEQ     CB_FIND_END         ; yes, skip to next entry
                TXA
                AND     #FA_DIRECTORY       ; is it a directory?
                BNE     CB_FIND_END         ; yes, get next dir entry
                JSR     OS_COMP_NAME        ; compare name with search mask
                BCC     CB_FIND_END2        ; name not equal, get next dir entry
                LDX     #$00
                LDY     #D_START_CLST       ; get low bytes of start cluster address
GET_FILE_CLST   LDA     (CURR_DIR_ENTRY),Y  ; load address byte
                STA     CURR_CLUSTER,X      ; ##### LBA         ; and store it into current cluster
                INX
                INY
                CPX     #$02                ; first two address bytes read?
                BCC     GET_FILE_CLST       ; no, read next address byte
                LDY     #D_START_CLSTH      ; yes, get high bytes of start cluster address
                CPX     #$04                ; all four bytes read?
                BCC     GET_FILE_CLST       ; no, read next byte
                RTS                         ; yes, file found
CB_FIND_END     CLC                         ; file not found
CB_FIND_END2    RTS
                
; **** Data Area ***************************************************************
; ******************************************************************************

; Indirect Jump Pointer Of Current Command Address *****************************

CMD_ADDR        .word      $0000
                
; String Data Area *************************************************************

MSG_BOOT        .by    '...' CR CR
                .by    ' Welcome to DOS-65 System I, Version '
                .byte      VERMAIN,DOT,VERPSUB,DOT,VERSSUB,CR,CR
                .by    ' 2023/25 by Joerg Walke' CR CR $00
MSG_SIZE_ERR    .by    'Out of memory' CR $00
FILENAME        .by    '???????????'
                
                
; START OF SHELL ***************************************************************
;
; ******************************************************************************

; Main Loop Of Command Interpreter *********************************************
;
; ******************************************************************************

SH_CMD_PROMPT   LDA     #<CMD_FOUND
                STA     CMD_ADDR
                LDA     #>CMD_FOUND
                STA     CMD_ADDR+1
                JSR     PRINT_PROMPT
                JSR  	OS_STRING_IN         ; read input string
		JSR     GET_CMD              ; parse string
		BCC     CMD_EXTERNAL
                JSR     CMD_EXECUTE          ; execute internal command
                JMP     SH_CMD_PROMPT        ; endless loop
CMD_EXTERNAL    JSR     SH_RUN               ; internal command not found, load executable file from drive
                JMP     SH_CMD_PROMPT        ; endless loop
CMD_EXECUTE     JMP     (CMD_ADDR)           ; indirect jump to internal command call back function
		
; Get Command String ***********************************************************

GET_CMD         JSR     GET_CMD_STR          ; skip leading spaces
                BNE     GET_CMD0
                JMP     CMD_FOUND            ; empty command if first char is NULL
GET_CMD0        JSR     UPPERCASE	     ; uppercase chars only
                TAX
                LDY     #$00
GET_CMD1        TXA                          ; try to find command in command table
                CMP     CHARS,Y
                BEQ     GET_CMD2
                LDA     CHARS,Y
                BEQ     SH_CMD_SET_DRV
                TXA
                INY
                INY
                INY
                JMP     GET_CMD1
GET_CMD2        INY
                LDA     CHARS,Y
                STA     CMDL
                INY
                LDA     CHARS,Y
                STA     CMDH
                LDY     #$00
                STY     NEXTINDEX            ; initialize command index
GET_CMD_LOOP1   LDX     NCNT                 ; reset string index to first command char
                LDA     (CMDL),Y             ; get command length
                BEQ     SH_CMD_SET_DRV       ; if length is 0 then command not found
                CLC
                ADC     NEXTINDEX            ; set index to start of next command
                STA     NEXTINDEX
GET_CMD_LOOP2   INX                          ; point to next char in command string
                INY                          ; point to next char in command table
                CPY     NEXTINDEX            ; is y pointing to the next command?
                BEQ     GET_CMD3             ;
                LDA     STRBUF,X             ; load char from command string
                JSR     UPPERCASE	     ; and convert it to uppercase
                CMP     (CMDL),Y             ; compare char with current char in command table
                BEQ     GET_CMD_LOOP2        ; compare next char if equal
                LDY     NEXTINDEX            ; else point y to next command
                INY
                INY
                STY     NEXTINDEX
                JMP     GET_CMD_LOOP1
GET_CMD3        LDA     (CMDL),Y
                STA     CMD_ADDR
                INY
                LDA     (CMDL),Y
                STA     CMD_ADDR+1
                LDA     STRBUF,X
                BEQ     CMD_FOUND
                CMP     #SPC
                BEQ     CMD_FOUND
CMD_NOT_FOUND   CLC
                RTS
                
; Check If Set-Drive Command (A: .. Z:) ****************************************

SH_CMD_SET_DRV  LDX     NCNT                 ; get first command char
                LDA     STRBUF,X
                JSR     UPPERCASE            ; convert it to upper case
                CMP     #'A'                 ; check if char is between 'A' and 'Z'
                BCC     CMD_NOT_FOUND
                CMP     #'['
                BCS     CMD_NOT_FOUND
                TAY
                LDA     STRBUF+1,X
                CMP     #':'                 ; check if second char is ':'
                BNE     CMD_NOT_FOUND
                SEC
                TYA
                SBC     #65                  ; make a drive (0..25) number out of drive letter (A..Z)
                LDX     #<OS_SET_DRIVE   ; set call back function for set drive command
                LDY     #>OS_SET_DRIVE
                STX     CMD_ADDR
                STY     CMD_ADDR+1
CMD_FOUND       SEC
                RTS
                
; **** Return Uppercase Character **********************************************
;
; Input:  A - Character
; Output: A - Uppercase Character
;
; ******************************************************************************

UPPERCASE       CMP     #'a'
                BCC     UPPERCASE_END
                CMP     #'z'+1
                BCS     UPPERCASE_END
                AND     #$DF
UPPERCASE_END   RTS

; **** Print Two Digit Number **************************************************
;
; Input: A - Number (0..99)
;
; ******************************************************************************

NUMOUT          JSR     DEC2STR
                LDX     #$01
NEXT_NUMOUT     LDA     NUM32,X
                JSR     COUT
                DEX
                BPL     NEXT_NUMOUT
                RTS
                
; **** Print Current Drive *****************************************************
;
; ******************************************************************************

PRINT_DRIVE     LDA     CURR_DRIVE
                CLC
                ADC     #65
                JSR     COUT
                LDA     #COLON
                JSR     COUT
                RTS
                
; **** Print Current Path ******************************************************
;
; ******************************************************************************
                
PRINT_PATH      LDA     #PATH_SEP           ; temp: for now show only a single backslash
                JSR     COUT
                RTS

; **** Print Input Prompt ******************************************************
;
; ******************************************************************************

PRINT_PROMPT    JSR     CROUT
                JSR     PRINT_DRIVE
                JSR     PRINT_PATH
                LDA     #PROMPT
                JSR     COUT
                RTS
                
; **** Print Drive Label *******************************************************
;
; ******************************************************************************

PRINT_LABEL     PHA
                TYA
                PHA
                LDX     #<MSG_LABEL
                LDY     #>MSG_LABEL
                JSR     OS_STRING_OUT
                JSR     PRINT_DRIVE
                LDX     #<MSG_LABEL2
                LDY     #>MSG_LABEL2
                JSR     OS_STRING_OUT
                JSR     PRINT_FILENAME1
                JSR     CROUT
                JSR     CROUT
                PLA
                TAY
                PLA
                RTS
                
; **** Print Filename **********************************************************
;
; ******************************************************************************
                
PRINT_FILENAME  LDX     #DOT
                LDA     F_ATTRIBS           ; load attributes
                AND     #FA_DIRECTORY       ; is it a directory?
                BEQ     PRINT_FILENAME2     ; no, set divider to '.'
PRINT_FILENAME1 LDX     #SPC                ; yes, set divider to ' '
PRINT_FILENAME2 LDY     #$00
PRINT_FILENAME3 CPY     #$08
                BNE     PRINT_NEXT_CHAR     ; start of file extension?
                JSR     SPCOUT
                TXA
                JSR     COUT                ; yes, print divider char
PRINT_NEXT_CHAR LDA     (CURR_DIR_ENTRY),Y  ; load next character
                JSR     COUT                ; print character
                INY
                CPY     #D_ATTRIBUTES       ; all characters printed?
                BNE     PRINT_FILENAME3     ; no, repeat
                RTS
                
; **** Print File Info *********************************************************
;
; Input: F_ATTRIBS = File Attributes
;
; ******************************************************************************

PRINT_FILE_INFO LDA     F_ATTRIBS           ; load attributes
                AND     #FA_DIRECTORY        ; is it a directory?
                BEQ     PRINT_SIZE          ; no, print file size
                
; Print Directory Attribute ****************************************************

                LDX     #<MSG_DIR_ENTRY
                LDY     #>MSG_DIR_ENTRY
                JSR     OS_STRING_OUT       ; print <DIR>
                INC     CURR_DIR_CNT        ; increment total directory count
                BNE     NO_DCNT_CARRY
                INC     CURR_DIR_CNT+1
NO_DCNT_CARRY   JMP     PRINT_ATTRIB

; Print File Size **************************************************************

PRINT_SIZE      JSR     SPCOUT
                JSR     SPCOUT
                INC     CURR_FILE_CNT       ; increment total file count
                BNE     NO_FCNT_CARRY
                INC     CURR_FILE_CNT+1
NO_FCNT_CARRY   LDX     #$00
                LDY     #D_FILE_SIZE        ; index to file size
                CLC
                PHP
LOAD_SIZE       PLP
                LDA     (CURR_DIR_ENTRY),Y  ; load file size into NUM32
                STA     NUM32,X
                ADC     CURR_USED_SIZE,X    ; add file size to total file size
                STA     CURR_USED_SIZE,X
                PHP
                INY
                INX
                CPX     #$04
                BNE     LOAD_SIZE
                PLP
                JSR     PRINT_INT32         ; print file size
                
; Print File Attributes ********************************************************

PRINT_ATTRIB    JSR     SPCOUT
                LDA     CURR_CMD_PARAM
                AND     #%00000010          ; /A param set?
                BEQ     PRINT_DATE          ; no, skip printing attributes
                LDX     #$07
GET_ATTRIB      ASL     F_ATTRIBS           ; move attribute bit into carry
                LDA     ATTRIB_VAL,X
                BEQ     NEXT_ATTRIB         ; check if printable attribute
                BCS     SET_ATTRIB
CLEAR_ATTRIB    LDA     #'-'                ; attribute not set, print -
SET_ATTRIB      JSR     COUT                ; print attribute
NEXT_ATTRIB     DEX
                BPL     GET_ATTRIB          ; repeat until all attributes printed
                JSR     SPCOUT
                
; Print Date *******************************************************************

PRINT_DATE      LDY     #D_CREATION_D       ; index to file creation date
                LDA     (CURR_DIR_ENTRY),Y  ; load file creation date low byte
                STA     MONTH
                AND     #$1F                ; mask day value
                STA     DAY
                INY
                LDA     (CURR_DIR_ENTRY),Y  ; load file creation date high byte
                LSR                         ; year in A
                ROR     MONTH
                LSR     MONTH
                LSR     MONTH
                LSR     MONTH
                LSR     MONTH
                CLC
                ADC     #80                 ; year correction value (add 1980)
                STA     YEAR
                LDA     DAY
                JSR     NUMOUT
                LDA     #'.'
                JSR     COUT
                LDA     MONTH
                JSR     NUMOUT
                LDA     #'.'
                JSR     COUT
                LDY     YEAR
                CPY     #100
                BCC     CENTURY_19
                LDA     #20
                JSR     NUMOUT
                TYA
                SEC
                SBC     #100
                JMP     PRINT_YEAR
CENTURY_19      LDA     #19
                JSR     NUMOUT
                TYA
PRINT_YEAR      JSR     NUMOUT
                JSR     SPCOUT
                
; Print Time *******************************************************************

PRINT_TIME      LDY     #D_CREATION_T       ; index to file creation time
                LDA     (CURR_DIR_ENTRY),Y  ; load file creation time low byte
                STA     MINUTE
                INY
                LDA     (CURR_DIR_ENTRY),Y  ; load file creation time high byte
                LSR     
                ROR     MINUTE
                LSR     
                ROR     MINUTE
                LSR     
                ROR     MINUTE
                LSR     MINUTE
                LSR     MINUTE
                JSR     NUMOUT
                LDA     #':'
                JSR     COUT
                LDA     MINUTE
                JSR     NUMOUT
                RTS
                
; **** Print 16 Bit Number *****************************************************
;
; Input: X,Y = Int16
;
; ******************************************************************************

PRINT_INT16     JSR     BIN16_TO_BCD
                JMP     PRINT_NUM
                
; **** Print 32 Bit Number *****************************************************
;
; Input: NUM32[0..3] = Int32
; Output: C = 0 - Number is 0; C = 1 - Number <> 0
;
; ******************************************************************************

PRINT_INT32     JSR     BIN32_TO_BCD        ; convert NUM32 into BCD
PRINT_NUM       LDX     #$00
                LDY     #10
                CLC                         ; save status bits
PRINT_NUM1      PHP                         ; store current carry flag
                JSR     PRINT_SEPERATOR
                LDA     BCD_VAL,X           ; load two decimal digits
                PHA                         ; store A
                TYA                         ; move digit counter into A
                LSR                         ; bit one into carry
                PLA                         ; restore A
                BCC     SET_DIGIT1          ; is it a even digit?
                INX                         ; no, process digit 2
                AND     #$0F
                BPL     SET_DIGIT2
SET_DIGIT1      LSR                         ; shift upper digit of BCD into lower nibble
                LSR     
                LSR     
                LSR     
SET_DIGIT2      PLP
                BCS     PRINT_DIGIT         ; check if we processed at least one digit <> 0
                AND     #$0F
                BNE     PRINT_DIGIT         ; not a leading 0, print digit
                TYA
                CMP     #1                  ; is it the last digit
                BNE     PRINT_SPACE         ; no, just print space char
                LDA     #48                 ; yes, print 0
                JSR     HEXDIG
                CLC                         ; number is 0
                RTS
PRINT_SPACE     JSR     SPCOUT              ; print space
                CLC
                BCC     NEXT_DIGIT
PRINT_DIGIT     JSR     HEXDIG              ; print single digit
                SEC                         ; no more leading 0s
NEXT_DIGIT      DEY
                BNE     PRINT_NUM1          ; repeat if more digits
                RTS
                
; Print Thousands Separator ****************************************************
                
PRINT_SEPERATOR PHP                         ; save status bits
                CPY     #$09                ; 9th digit ?
                BEQ     PRINT_SEP           ; yes, print seperator
                CPY     #$06                ; 6th digit ?
                BEQ     PRINT_SEP           ; yes, print seperator
                CPY     #$03                ; 3rd digit ?
                BNE     NO_SEP              ; no, exit
PRINT_SEP       PLP                         ; restore status bits
                BCC     PRINT_SPC           ; leading zero, just print a space char
                LDA     #NUM_SEP
                JMP     COUT                ; print thousands seperator
PRINT_SPC       JMP     SPCOUT
NO_SEP          PLP                         ; clean up stack
                RTS
                
;**** Convert BCD Number To 8 Bit Binary ***************************************
;
; INPUT:  A = BCD Number
; Output: A = Binary Number
;
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
                
;**** Convert 16 Bit Binary Number To BCD **************************************
;
; INPUT:  Int[X:Y]      = 16 Bit Binary
; OUTPUT: BCD_VAL[4..0] = Result
;
; ******************************************************************************
                
BIN16_TO_BCD    STX     NUM32
                STY     NUM32+1
                LDX     #$00
                STX     NUM32+2
                STX     NUM32+3
                
;**** Convert 32 Bit Binary Number To BCD **************************************
;
; INPUT:  NUM32[0..3]   = 32 Bit Binary
; OUTPUT: BCD_VAL[4..0] = Result
;
;*******************************************************************************

BIN32_TO_BCD    SED                         ; set decimal mode
                LDX     #$04
                LDA     #$00                ; clear BCD result value
CLEAR_BCD       STA     BCD_VAL,X
                DEX
                BPL     CLEAR_BCD
                LDX     #$20                ; 32 source bits
CONV_BITS       ASL     NUM32               ; shift MSB of NUM32 into carry flag
                ROL     NUM32+1
                ROL     NUM32+2
                ROL     NUM32+3
                LDA     BCD_VAL+4           ; and shift carry back into BCD result
                ADC     BCD_VAL+4           ; by adding BCD_VAL = BCD_VAL+BCD_VAL+C
                STA     BCD_VAL+4
                LDA     BCD_VAL+3           ; ...
                ADC     BCD_VAL+3
                STA     BCD_VAL+3
                LDA     BCD_VAL+2           ; ...
                ADC     BCD_VAL+2
                STA     BCD_VAL+2
                LDA     BCD_VAL+1           ; ...
                ADC     BCD_VAL+1
                STA     BCD_VAL+1
                LDA     BCD_VAL             ; ...
                ADC     BCD_VAL
                STA     BCD_VAL             ; uses more code, but faster than looping
                DEX
                BNE     CONV_BITS           ; repeat until all 32 bits done
                CLD                         ; reset to binary mode
                RTS

; **** Get Command String ******************************************************
;
; ******************************************************************************
                
GET_CMD_STR     LDX     #$01
GET_CMD_CHAR    LDA     STRBUF,X
                BEQ     END_PARAM
                CMP     #SPC
                BNE     END_PARAM
SKIP_SPC_CHAR   INX
                JMP     GET_CMD_CHAR
                
; **** Get Parameter String ****************************************************
;
; Output: C = 1 Flag Parameter; C = 0 Path Parameter
;         A    = $00 : End of parameter string
;         NCNT = Index to parameter
;
; ******************************************************************************

GET_NEXT_PARAM  LDX     NCNT                ; get actual index into command line
                JSR     GET_PARM_CHAR
                CMP     #OPT_SEP
                BNE     PATH_STR
                INX
                LDA     STRBUF,X
                JMP     PARAM_STR
GET_PARM_CHAR   LDA     STRBUF,X
                BEQ     PATH_STR
                CMP     #SPC
                BEQ     SKIP_SPC_CHAR
                CMP     #OPT_SEP
                BEQ     PARAM_STR
                INX
                JMP     GET_PARM_CHAR
PATH_STR        CLC
                BCC     END_PARAM
PARAM_STR       SEC
END_PARAM       STX     NCNT
                PHA
                PLA
                RTS

; **** Internal Command Handlers ***********************************************

; **** Directory Loop Call Back Functions **************************************
;
; Input : A - First character of filename
;         X - File Attributes
; Output: C = 0 - Continue print loop, C = 1 - Break print loop
;
; ******************************************************************************

; **** Print Directory Entry - Call Back Routine *******************************

CB_PRINT_DIR    CPX     #$0F                ; is it a long filename entry?
                BEQ     CB_PRINT_CONT       ; yes, skip to next entry
                CMP     #$E5                ; is entry deleted?
                BEQ     CB_PRINT_CONT       ; yes, skip to next entry
                STX     F_ATTRIBS           ; save attributes
                TXA
                LSR     
CHK_HIDDEN      LSR                         ; check if hidden file
                BCC     CHK_SYSTEM
                LDA     CURR_CMD_PARAM
                AND     #%00000100          ; /H param set?
                BEQ     CB_PRINT_CONT       ; no, skip line counting
CHK_SYSTEM      LSR                         ; check if system file
CHK_LABEL       LSR                         ; check if disk label
                ; #### PRINT LABEL DISABLED ####################################
                BCS     CB_PRINT_CONT       ; it's a label, skip to next entry
                ;BCC     PRINT_DIR_ENTRY
                ;JSR     PRINT_LABEL         ; print disk label
                ;INC     LINE_CNT
                ;BNE     CHK_LINE_COUNT      ; branch always
                ; ##############################################################
PRINT_DIR_ENTRY LDA     TERM_CHAR           ; check the termination char
                BNE     COMP_MASK           ; if TC <> 0 then just compare file names
                JSR     CB_FIND_SUBDIR      ; TC = 0, so check if directory entry
                BCC     CB_PRINT_CONT       ; name is a file entry or includes wildcard chars, just exit
                LDA     #PATH_SEP           ; name compared equal with a directory entry
                STA     TERM_CHAR
                JSR     OS_DIR_LOOP         ; list files of sub directory
                SEC                         ; directory list finished
                RTS                         ; exit
COMP_MASK       JSR     SH_COMP_MASK
                BCC     CB_PRINT_END
PRINT_ITEM      LDA     #1
                STA     PSAV                ; set file found flag
                JSR     PRINT_FILENAME      ; print filename
                JSR     PRINT_FILE_INFO     ; print file size, date and time
                JSR     CROUT
                LDA     CURR_CMD_PARAM
                AND     #%00000001          ; /P param set?
                BEQ     CB_PRINT_CONT       ; no, skip line counting
CHK_LINE_COUNT  INC     LINE_CNT
                LDA     #25
                CMP     LINE_CNT            ; reached one screen page?
                BCS     CB_PRINT_CONT       ; no, just exit
                LDA     #$00                ; yes, reset line counter
                STA     LINE_CNT            ; reset line counter
                JSR     SH_PAUSE            ; wait for key press
                CMP     #27                 ; ESC pressed?
                BNE     CB_PRINT_CONT
                SEC                         ; break dir loop
                RTS
CB_PRINT_CONT   CLC                         ; get next next entry
CB_PRINT_END    RTS

; **** Compare File Name With Mask Useing Wildcards ****************************
;
; ******************************************************************************

SH_COMP_MASK    CMP     #PATH_SEP           ; is termination char the path seperator?
                BEQ     COMP_NAME_EQU       ; yes, skip compare
                LDY     #10                 ; compare all characters
COMP_NAME_CHAR  LDA     FILENAME,Y          ; get char from compare mask
                CMP     #'?'                ; is it a ? char?
                BEQ     SKIP_NAME_CHAR      ; yes, skip comparing this char
                CMP     (CURR_DIR_ENTRY),Y  ; compare character
                BNE     COMP_NAME_NEQ       ; if not equal, exit with Carry = 0
SKIP_NAME_CHAR  DEY
                BPL     COMP_NAME_CHAR      ; more character to compare
COMP_NAME_EQU   SEC                         ; all characters are equal, set Carry = 1
                RTS
COMP_NAME_NEQ   CLC
                RTS

; ******************************************************************************
; ******************************************************************************

SAVED_ACT_DIR   .byte      $00, $00, $00, $00
SAVED_DIR_BLK   .byte      $00, $00, $00, $00
; + DRIVE
                
SAVE_ACT_DIR    LDY     #$03
SAVE_DIR_ADDR   LDA     D_ACTUAL_DIR,Y
                STA     SAVED_ACT_DIR,Y
                LDA     CURR_DIR_BLK,Y
                STA     SAVED_DIR_BLK,Y
                DEY
                BPL     SAVE_DIR_ADDR
                RTS
                
LOAD_ACT_DIR    LDY     #$03
LOAD_DIR_ADDR   LDA     SAVED_ACT_DIR,Y
                STA     D_ACTUAL_DIR,Y
                LDA     SAVED_DIR_BLK,Y
                STA     CURR_DIR_BLK,Y
                DEY
                BPL     LOAD_DIR_ADDR
                RTS
                
; **** Get Parameters from Command Line ****************************************
;
; Input:  Ptr[X:Y] to Parameter String
; Output: C = 0 - Error; C = 1 - No Error
;
; ******************************************************************************

SH_GET_PARMS    JSR     SET_PARM_MASK
                LDA     #$00                ; set current command parameter value to 0
GET_PARM        STA     CURR_CMD_PARAM
GET_PARM2       LDY     #$01
                STY     MASK                ; reset bit mask to 00000001
                DEY                         ; param mask pointer is set to 0
                JSR     GET_NEXT_PARAM      ; find next command parameter in command line
                BEQ     GET_PARMS_END       ; reached end of command line
                BCS     PARSE_PARM          ; option parameter found? yes, parse option
                JSR     SH_GET_PATH         ; no, parse path parameter
                BCS     GET_PARM2           ; more parameters
                RTS
PARSE_PARM      JSR     UPPERCASE
                STA     F_ATTRIBS           ; no, save current parameter as F_ATTRIBS
COMP_PARM       LDA     (PPMASK),Y           ; load a char from param mask
                CMP     #SPC                ; is it a ' '?
                BEQ     PARM_ERR            ; yes, all allowed param chars are compared -> unkown param
                CMP     F_ATTRIBS           ; no, compare actual parameter char with param mask
                BNE     NEXT_MASK           ; not equal, get next char from param mask
                LDA     MASK                ; load bit mask
                ORA     CURR_CMD_PARAM      ; and set actual parameter bit
                JMP     GET_PARM            ; get next parameter from command line
NEXT_MASK       INY                         ; point to next char in param mask
                ASL     MASK                ; shift bit mask to next position
                BNE     COMP_PARM
PARM_ERR        JSR     CROUT
                LDX     #<MSG_PARAM_ERR ; load error message
                LDY     #>MSG_PARAM_ERR
                JSR     OS_PRINT_ERR        ; and print it
                LDA     F_ATTRIBS
                JSR     COUT                ; print unknown parameter char
                JSR     CROUT
                CLC
                RTS
GET_PARMS_END   SEC
                RTS
                
; ******************************************************************************

SH_GET_PATH     LDX     NCNT                ; get pointer path string into X:Y
                LDY     #> STRBUF
                JSR     OS_PARSE_PATH
                BCS     GET_PATH_END
SH_ERROR        BNE     PARSE_ERR1
                JMP     SH_NAME_ERR
PARSE_ERR1      CMP     #PATH_SEP
                BNE     PARSE_ERR2
                JMP     SH_DIR_ERR
PARSE_ERR2      CMP     #$FF
                BNE     GET_PATH_ERR
                JMP     SH_PATH_ERR
GET_PATH_ERR    CLC
GET_PATH_END    RTS

; **** Set Pointer To Parameter Mask *******************************************
;
; Input: Ptr[X:Y] = Pointer to Parameter Mask
;
; ******************************************************************************
                
SET_PARM_MASK   STX     PPMASK              ; save pointer to command param mask
                STY     PPMASK+1
                LDA     #$00
                STA     CURR_CMD_PARAM      ; set current command parameter value to 0
                LDA     #PATH_SEP
                STA     TERM_CHAR
                RTS

; Allowed Directory Options ****************************************************

DIR_PARMS       .by    'PAH '               ; param mask for DIR command
NO_PARMS        .by    ' '

; **** DIR Command *************************************************************
;
; ******************************************************************************

SH_DIR          JSR     CROUT
                JSR     SAVE_ACT_DIR        ; save actual directory LBA
                LDX     #<DIR_PARMS
                LDY     #>DIR_PARMS
                JSR     SH_GET_PARMS
                BCS     SH_DIR_START
                JMP     LOAD_ACT_DIR        ; restore actual directory LBA
SH_DIR_START    LDX     #$00
                TXA
                STA     LINE_CNT            ; reset line counter
                STA     PSAV                ; reset file found flag
CLEAR_CNT       STA     CURR_FILE_CNT,X     ; clear file count, dir count and byte count
                INX
                CPX     #$08
                BNE     CLEAR_CNT
                LDX     #<CB_PRINT_DIR  ; print call back routine
                LDY     #>CB_PRINT_DIR
                JSR     OS_FIND_ALL         ; find and print directory entries
                JSR     LOAD_ACT_DIR        ; restore actual directory LBA
                LDA     PSAV                ; any files found?
                BNE     PRINT_RESULT        ; yes, print directory result
                JMP     SH_FILE_ERR         ; no, print error
PRINT_RESULT    JSR     CROUT
                INC     LINE_CNT
                INC     LINE_CNT
                JSR     CHK_LINE_COUNT
                
; Print Total File Count

                LDX     CURR_FILE_CNT
                LDY     CURR_FILE_CNT+1
                JSR     PRINT_INT16
                LDX     #<MSG_FILE_COUNT
                LDY     #>MSG_FILE_COUNT
                JSR     OS_STRING_OUT

; Print Total Used Bytes In Directory

                LDX     #<CURR_USED_SIZE
                LDY     #>CURR_USED_SIZE
                JSR     LOAD_32
                JSR     PRINT_INT32
                LDX     #<MSG_BYTE_USED
                LDY     #>MSG_BYTE_USED
                JSR     OS_STRING_OUT

; Print Total Directory Count

                LDX     CURR_DIR_CNT
                LDY     CURR_DIR_CNT+1
                JSR     PRINT_INT16
                LDX     #<MSG_DIR_COUNT
                LDY     #>MSG_DIR_COUNT
                JMP     OS_STRING_OUT

; **** Create Directory (MKDIR) Command ****************************************
;
; ******************************************************************************

SH_MKDIR        JSR     SAVE_ACT_DIR        ; save actual directory LBA
                LDX     #<NO_PARMS      ; we don't need parameters
                LDY     #>NO_PARMS      ; #### evtl. /H hidden /S system
                JSR     SH_GET_PARMS        ; get path
                BCC     SH_MKDIR_END
                LDA     #$00 		    ;#FA_DIRECTORY       ; create directory
                JSR     OS_CREATE           ; create directory
                BCS     SH_MKDIR_END        ; if no errors, clean up and exit
MKDIR_ERR       CMP     #$FF                ; check error code
                BEQ     DIR_EXISTS_ERR      ; if error = -1 then dir already exists
                JSR     SH_WRITE_ERR        ; it was a write error
                BCC     SH_MKDIR_END        ; branch always
DIR_EXISTS_ERR  JSR     SH_D_EXIST_ERR
SH_MKDIR_END    JSR     LOAD_ACT_DIR        ; restore actual directory LBA
                RTS

; **** Change Directory Command ************************************************
;
; ******************************************************************************

SH_CD           JSR     SAVE_ACT_DIR        ; save actual directory LBA
                LDX     #<NO_PARMS
                LDY     #>NO_PARMS
                JSR     SH_GET_PARMS
                BCC     SH_CD_END
                LDA     TERM_CHAR
                BNE     SH_CD_END
                JSR     OS_FIND_PATH
                BCS     SH_CD_END
                JSR     LOAD_ACT_DIR        ; error - restore actual directory LBA
                JSR     CROUT
                JMP     SH_PATH_ERR
SH_CD_END       RTS

; **** Delete Command **********************************************************
;
; ******************************************************************************

; ########## TEMP Checking for first free cluster #################

SH_DEL          JSR INIT_FREE_CLUSTER
                JSR OS_NEXT_FREE_CLUSTER
PRINT_CURR_CLST LDA FREE_CLUSTER+3
                JSR HEXOUT
                LDA FREE_CLUSTER+2
                JSR HEXOUT
                LDA FREE_CLUSTER+1
                JSR HEXOUT
                LDA FREE_CLUSTER
                JSR HEXOUT
                RTS

; **** Clear Screen Command ****************************************************
;
; ******************************************************************************
                
SH_CLS          JMP     CLRSCRN

; **** Pause Command ***********************************************************
;
; Output: A - pressed key char
;         C = 0 ESC key pressed, C = 1 else
; ******************************************************************************

SH_PAUSE        LDX     #<MSG_PAUSE          ; load pause message
                LDY     #>MSG_PAUSE
                JSR     OS_STRING_OUT        ; and print it
                JSR     CIN                  ; wait until any key pressed
                PHA
                JSR     CROUT
                PLA
                RTS
                
; **** Echo Command ************************************************************
;
; ******************************************************************************
                
SH_ECHO         LDA     STRBUF,X
                BEQ     SH_ECHO_END
                TXA
                TAY
                INY
                JSR     WRSTR
                JSR     CROUT
SH_ECHO_END     RTS

; **** Goto Command ************************************************************
;
; ******************************************************************************

SH_GOTO         RTS

; **** If Command **************************************************************
;
; ******************************************************************************

SH_IF           RTS

; **** Rem Command *************************************************************
;
; ******************************************************************************

SH_REM          RTS
                
; **** BRUN Command ************************************************************
;
; ******************************************************************************

SH_BRUN         JSR     SAVE_ACT_DIR        ; save actual directory LBA
                LDX     #<NO_PARMS          ; no command parameters
                LDY     #>NO_PARMS
                JSR     SH_GET_PARMS
                BCC     SH_BRUN_END
                JSR     OS_FIND_FILE
                BCC     SH_BRUN_END
                ;JSR     OS_LOAD_BIN
SH_BRUN_END     JSR     LOAD_ACT_DIR        ; restore actual directory LBA
                RTS
                
; **** BLOAD Command ***********************************************************
;
; ******************************************************************************
                
SH_BLOAD        RTS

; ******************************************************************************

SH_RUN          JSR     SAVE_ACT_DIR        ; save actual directory LBA
                LDX     #<NO_PARMS          ; no command parameters
                LDY     #>NO_PARMS
                JSR     SET_PARM_MASK
                JSR     SH_GET_PATH         ; get file path
                BCC     SH_RUN_END
                LDA     FILENAME+8
                CMP     #SPC                ; check if given filename has no extension
                BNE     SH_RUN1
                LDY     #$02
SET_EXT_COM     LDA     EXT_COM,Y           ; SHOULD ALSO CHECK EXE AND BAT IN FUTURE VERSIONS ##########################
                STA     FILENAME+8,Y
                DEY
                BPL     SET_EXT_COM
SH_RUN1         LDY     #$02
CMP_EXT_COM     LDA     EXT_COM,Y           ; check if COM file
                CMP     FILENAME+8,Y
                BNE     SH_RUN_END
                DEY
                BPL     CMP_EXT_COM
                
                JSR     OS_FIND_FILE        ; check if COM file exists
                BCS     SH_RUN3             ; yes, load CPM file
                JSR     SH_SET_SYS_DIR      ; no, search in system directory
                BCC     SH_RUN_ERR          ; system directory does not exist
SH_RUN2         JSR     OS_FIND_FILE
                BCS     SH_RUN3
SH_RUN_ERR      JSR     CROUT
                JSR     SH_FILE_ERR         ; COM file dos not exists
                BCC     SH_RUN_END
SH_RUN3         JSR     OS_LOAD_COM
SH_RUN_END      JSR     LOAD_ACT_DIR        ; restore actual directory LBA
                RTS
                


; **** Set System Directory ****************************************************
;
; Output: C = 0 - Error
;
; ******************************************************************************
                
SH_SET_SYS_DIR  LDX     #10
SAVE_NAME       LDA     FILENAME,X
                STA     NAME_SAVE,X
                LDA     SYSTEM_DIR,X
                STA     FILENAME,X
                DEX
                BPL     SAVE_NAME
                JSR     OS_SET_ROOT_DIR
                LDX     #<CB_FIND_SUBDIR
                LDY     #>CB_FIND_SUBDIR
                JSR     OS_FIND             ; find subdirectory
                BCC     SH_SYS_DIR_END
                LDX     #10
RESTORE_NAME    LDA     NAME_SAVE,X
                STA     FILENAME,X
                DEX
                BPL     RESTORE_NAME
                SEC
SH_SYS_DIR_END  RTS

SH_MONITOR      LDX     #<OS_SHELL_ENTRY
                LDY     #>OS_SHELL_ENTRY
                STX     RETURN_VECT
                STY     RETURN_VECT+1
                LDX     #<MSG_MONITOR
                LDY     #>MSG_MONITOR
                JSR     OS_STRING_OUT
                JMP     MON_WARM_START
                
; **** Error Routines **********************************************************

SH_FILE_ERR     LDX     #<MSG_FILE_ERR  ; load error message...
                LDY     #>MSG_FILE_ERR
                JMP     OS_PRINT_ERR
                
SH_PATH_ERR     LDX     #<MSG_PATH_ERR  ; load error message...
                LDY     #>MSG_PATH_ERR
                JMP     OS_PRINT_ERR
                
SH_DIR_ERR      LDX     #<MSG_DIR_ERR   ; load error message
                LDY     #>MSG_DIR_ERR
                JMP     OS_PRINT_ERR        ; and print it
                
SH_NAME_ERR     LDX     #<MSG_NAME_ERR  ; load error message...
                LDY     #>MSG_NAME_ERR
                JMP     OS_PRINT_ERR        ; ...and print it
                
SH_D_EXIST_ERR  LDX     #<MSG_D_EXIST_ERR ; load error message...
                LDY     #>MSG_D_EXIST_ERR
                JMP     OS_PRINT_ERR        ; ...and print it
                
SH_WRITE_ERR    LDX     #<MSG_WRITE_ERR ; load error message...
                LDY     #>MSG_WRITE_ERR
                JMP     OS_PRINT_ERR        ; ...and print it
                
; **** Data Area ***************************************************************
; ******************************************************************************

; String Data Area *************************************************************
                
MSG_PAUSE       .by    'Press any key...' $00
MSG_LABEL       .by    'Volume in drive ' $00
MSG_LABEL2      .by    ' is ' $00
MSG_DIR_ENTRY   .by    '          <DIR>' $00
MSG_FILE_COUNT  .by    ' file(s)  ' $00
MSG_DIR_COUNT   .by    ' dir(s)  ' CR $00
MSG_BYTE_USED   .by    ' bytes' CR $00
MSG_DRIVE_ERR   .by    'Drive not found' CR $00
MSG_FILE_ERR    .by    'File not found' CR $00
MSG_PATH_ERR    .by    'Path not found' CR $00
MSG_PARAM_ERR   .by    'Unknown option ' OPT_SEP $00
MSG_NAME_ERR    .by    'Invalid file name' CR $00
MSG_DIR_ERR     .by    'Invalid directory' CR $00
MSG_D_EXIST_ERR .by    CR 'Directory already exists' CR $00
MSG_MONITOR     .by    CR 'Hex Monitor' CR $00

; ############################################
MSG_WRITE_ERR   .by    CR 'Write Error' CR $00

ATTRIB_VAL      .byte      82, 72, 83, 0, 0, 65, 0, 0

BCD_VAL         .byte      $00, $00, $00, $00, $00

SYSTEM_DIR      .by    'SYSTEM     '
NAME_SAVE       .by    '...........'

EXT_COM         .by    'COM'
                
; Command Table ****************************************************************

CHARS           .by    'B'
                .word      CMD_BLOAD
                .by    'C'
                .word      CMD_CD
                .by    'D'
                .word      CMD_DIR
                .by    'E'
                .word      CMD_ECHO
                .by    'G'
                .word      CMD_GOTO
                .by    'I'
                .word      CMD_IF
                .by    'M'
                .word      CMD_MKDIR
                .by    'P'
                .word      CMD_PAUSE
                .by    'R'
                .word      CMD_REM
                .byte      $00

CMD_BLOAD       .byte      5
                .by    'LOAD'
                .word      SH_BLOAD
CMD_BRUN        .byte      4
                .by    'RUN'
                .word      SH_BRUN
                .byte      $00
CMD_CD          .byte      2
                .by    'D'
                .word      SH_CD
CMD_CLS         .byte      3
                .by    'LS'
                .word      SH_CLS
                .byte      $00
CMD_DIR         .byte      3
                .by    'IR'
                .word      SH_DIR
CMD_DEL         .byte      3
                .by    'EL'
                .word      SH_DEL
                .byte      $00
CMD_ECHO        .byte      4
                .by    'CHO'
                .word      SH_ECHO
                .byte      $00
CMD_GOTO        .byte      4
                .by    'OTO'
                .word      SH_GOTO
                .byte      $00
CMD_IF          .byte      2
                .by    'F'
                .word      SH_IF
                .byte      $00
CMD_MKDIR       .byte      5
                .by    'KDIR'
                .word      SH_MKDIR
CMD_MON         .byte      3
                .by    'ON'
                .word      SH_MONITOR
                .byte      $00
CMD_PAUSE       .byte      5
                .by    'AUSE'
                .word      SH_PAUSE
                .byte      $00
CMD_REM         .byte      3
                .by    'EM'
                .word      SH_REM
                .byte      $00
                
; End Of Program Marker ********************************************************

BOOT_SYS_END    BRK

                END