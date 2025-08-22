;-------------------------------------------------------------------------------
; This file contains the first block of the BOOT.SYS file. This first block
; MUST fit into a single sector of 512 bytes. This first sector is already
; loaded into memory by the boot-block loader on the CF-card.
;
; The main purpose of this first block is to load the remainder of BOOT.sys
; into memory and then to execute it.
; 
; Assembler: MADS-Assembler
;-------------------------------------------------------------------------------

; subtract two clusters from START_CLUSTER address *****************************
INIT            LDA     D_SECT_PER_CLST		; Written by FAT32 boot-code
                ASL     			; A = 2 * D_SECT_PER_CLST
                TAX				; X = 2 * D_SECT_PER_CLST 
                LDA     D_START_CLS_LBA		; This is cluster_begin_lba
                STX     D_START_CLS_LBA		; D_START_CLS_LBA = 2 * D_SECT_PER_CLST
		SUB	D_START_CLS_LBA		; A = D_START_CLS_LBA - 2 * D_SECT_PER_CLST
                STA     D_START_CLS_LBA		; D_START_CLS_LBA -= 2 * D_SECT_PER_CLST
                LDX     #$01
                LDY     #$03
INIT2           LDA     D_START_CLS_LBA,X	; D_START_CLS_LBA is 32-bits
                SBC     #$00
                STA     D_START_CLS_LBA,X+
                DEY.NE	INIT2			; branch if not done yet
                
; initialize current FAT block value with 0 ************************************
                STY     CURR_FAT_BLK		; CURR_FAT_BLK = 0L
                STY     CURR_FAT_BLK+1
                STY     CURR_FAT_BLK+2
                STY     CURR_FAT_BLK+3
                
; set first cluster of BOOT.SYS as the current cluster *************************
INIT3           LDY     #$15
                LDX     #$03
                JSR     SET_WORD            	; store cluster start byte [3:2]
                LDY     #$1B
                JSR     SET_WORD            	; store cluster start byte [1:0]
                
; calculate BOOTS.SYS file size in blocks **************************************
INIT4           LDA     #>BOOT_SYS_END      	; Cal. total nr of blocks needed to load the complete
                SUB     #>PROG_START        	; BOOT.SYS file into memory
                LSR     		    	; 2 pages = 1 sector of 512 bytes
                STA     BCNT                	; store result into block counter
                LDA.EQ  #<BOOT_SYS_END BOOT_SYS	; branch if LSB is 0
                INC     BCNT                	; some bytes are left, so increment block counter
                
; load all blocks of BOOT.SYS file into memory *********************************
BOOT_SYS        DEC.EQ  BCNT OS_START          	; first block is already read in, branch if no more blocks to read
                MVA	D_SECT_PER_CLST SCNT	; SCNT = D_SECT_PER_CLST
LOAD_NEXT_BLK   DEC.EQ  SCNT NEXT_CLUSTER      	; branch if all blocks in cluster read
                JSR     INC_32              	; no, increment block address
LOAD_BLK        LDXYI   NUM32			; NUM32 contains LBA of a boot.sys cluster
                JSR     DEV_RD_LBLK         	; and read next block of BOOT.SYS
                DEC.EQ  BCNT OS_START		; branch if no more blocks to read
                BNE     LOAD_NEXT_BLK       	; yes, read next block in cluster

; next cluster needs to be loaded considering the volume FAT type **************
NEXT_CLUSTER    JSR     GET_NEXT_CLSTR      	; get next cluster in chain in CURR_CLUSTER
                BCS     OS_START            	; if EOF then start OS
		
                JSR     CLUSTER_TO_BLK      	; convert CURR_CLUSTER to LBA number in NUM32
                MVA	D_SECT_PER_CLST SCNT	; SCNT = D_SECT_PER_CLST
                JMP     LOAD_BLK            	; load first block of cluster
                
OS_START        MVA	#0 DBG_PRINT		; 0 = No debug print info
		JMP     OS_MAIN             	; jump to OS entry point

;-------------------------------------------------------------------------------
; Copy a word, that is part of a cluster nr for boot.sys, into CURR_CLUSTER.
; PSTR is already set by the FAT boot-block routine.
;-------------------------------------------------------------------------------
SET_WORD        JSR     SET_BYTE
SET_BYTE        MVA	(PSTR),Y- CURR_CLUSTER,X- 	; load a byte from dir entry and store in CURR_CLUSTER
                RTS
		
; **** Get Next Cluster Of Cluster Chain ***************************************
; Input:  CURR_CLUSTER[0:3] - current cluster
; Output: C = 0 valid cluster in CURR_CLUSTER[0:3]; C = 1 EOF
; ******************************************************************************
GET_NEXT_CLSTR  LDXYI   D_START_FAT1      	; load base block address of FAT into NUM32[0:3]
                JSR     LOAD_32		    	; NUM32 = LBA nr. of FAT

                LDA.EQ  D_FAT_TYPE  FAT32      	; check FAT type and branch if FAT32 (0)
		CMP.EQ	#FAT16_Type FAT16	; branch if FAT16
                
; **** Decode FAT12 Entry ******************************************************
; **** Two FAT12 entries A and B coded together as AA BA BB ********************
FAT12           RTS				; Removed for DEBUG

; **** Decode FAT16 Entry ******************************************************
; **** a FAT16 cluster C is coded as CC CC *************************************
FAT16           LDX     CURR_CLUSTER+1      	; load byte 2 of current cluster
                JSR     ADD_32_8            	; and add it to FAT base block address
                JSR     LOAD_FAT_BLK        	; load this block
                LDA     CURR_CLUSTER        	; load index into the FAT block
                LDX     #$02                	; two bytes to read for a FAT16 entry
		JMP	READ_FAT_ENTRY
		
; **** Decode FAT32 Entry ******************************************************
; **** a FAT32 cluster C is coded as CC CC CC 0C *******************************
FAT32           LDXYI	(CURR_CLUSTER+1)		; Get CURR_CLUSTER+1 into SUM32
		JSR     LOAD_S32            	; load CURR_CLUSTER[1:3] into SUM[0:2] = FAT block index
		MVX	#$00 SUM32+3		; clear garbage byte SUM[3]: SUM32 = CURR_CLUSTER / 256
                LDY     #$03
                LDA     CURR_CLUSTER        	; load CURR_CLUSTER[0] = FAT entry index byte
                ASL                         	; shift bit 7 into carry flag and multiply entry index by 2
                PHA                         	; save entry index to stack
FAT32_LOOP      ROL     SUM32,X+             	; shift bit 7 of entry index into bit 0 of block index
                DEY.NE  FAT32_LOOP	    	; branch if not done yet
		
		; A FAT entry is 4 bytes and there are 128 FAT entries in one FAT-sector
                JSR     ADD_32_32	    	; NUM32 = START_FAT1 + CURR_CLUSTER / 128
                JSR     LOAD_FAT_BLK	    	; Load FAT sector into standard buffer ($600)
                PLA                         	; restore entry index
                LDX     #$04                	; four bytes to read for a FAT32 entry
;               JMP     (RW_FAT_ENTRY)      	; Fall-through to READ_FAT_ENTRY
                
; **** Read FAT 16 or FAT 32 Entry *********************************************
; INPUT : X - Length of FAT Entry in Bytes
; OUTPUT: C = 0: Valid Cluster in CURR_CLUSTER[0:3]; C = 1: EOF
; example: 08000000 => next cluster = 8: A=RES=$00, C=0
;          FFFFFF0F => EOF: A=RES=$FF, C=1 
; ******************************************************************************
;RW_FAT_ENTRY    .word      READ_FAT_ENTRY
READ_FAT_ENTRY  
		MVY     #$FF MASK              	; standard EOF mask is $FF
                ASL                         	; multiply entry index by 2 (4 in total now)
                TAY                         	; store entry index into Y
                STX     NCNT                	; store length of entry
                MVX     #$00 RES		;
LOOP_FAT_ENTRY  JSR     READ_ENTRY_BYTE     	; read entry byte
SET_ENTRY_BYTE  STA     CURR_CLUSTER,X      	; store byte in curr_cluster
                PHP                         	; save carry flag
                CPX.NE  #$03 CMP_MASK          	; branch if not the upper byte of a FAT32 entry
                MVX     #$0F MASK              	; yes, we must change the mask to $0F
CMP_MASK        CMP.NE 	MASK READ_FAT_ENTRY1   	; cluster byte = EOF mask? Read next byte if not
                STA     RES                 	; compare cluster byte is equal EOF mask
READ_FAT_ENTRY1 PLP                         	; restore carry flag
                INY
                INX
                DEC.NE  NCNT LOOP_FAT_ENTRY   	; loop until all bytes copied
                LDA     RES
                CMP     #$FF                	; check if result cluster is $FFFF or $0FFFFFFF (EOF)
                RTS
                
; **** Read a Single FAT Entry Byte From Block Buffer **************************
; INPUT : Y - Index To FAT Entry Byte
; OUTPUT: A = Read Byte from FAT table
; ******************************************************************************
READ_ENTRY_BYTE BCS	RD_UPPER_PAGE				; if $40..$7F then read byte from upper half of block

                LDA     FAT_BUF,Y        			; read entry byte from lower half of block buffer
                RTS			    			; return
RD_UPPER_PAGE   LDA     FAT_BUF+256,Y    			; read entry byte from upper half of block buffer
                RTS			    			; return
                
; **** Calculate LBA From Given Cluster Address ********************************
; At the start of boot.sys, 2 clusters were already subtracted from D_START_CLS_LBA.
; Used formula: lba_addr = cluster_begin_lba + (cluster_number-2) * sectors_per_cluster.
;
; INPUT : CURR_CLUSTER[0:3] - Current File Cluster number
; OUTPUT: NUM32[0:3]        - LBA of current file cluster number
; ******************************************************************************
CLUSTER_TO_BLK  LDXYI   CURR_CLUSTER
CLSTR_TO_BLK    JSR     LOAD_32		     	; NUM32 = CURR_CLUSTER
CLSTR_TO_LBA    LDA     D_SECT_PER_CLST
                LSR      		     	; A = D_SECT_PER_CLST / 2
                BEQ     ADD_START_CLSTR	     	; branch if D_SECT_PER_CLST = 1
		
LOOP1           LDX     #$00		     	; NUM32 = CURR_CLUSTER * D_SECT_PER_CLST
                LDY     #$04
                CLC
LOOP2           ROL     NUM32,X+	     	; NUM32 <<= 1
                DEY.NE	LOOP2		     	; branch if not done yet
		
                LSR      		     	; A = D_SECT_PER_CLST >>= 1
                BNE     LOOP1		     	; 
		
ADD_START_CLSTR LDXYI   D_START_CLS_LBA    	; D_START_CLS_LBA = Cluster_begin_lba
                JSR     LOAD_S32	     	; NUM32 = Cluster_begin_lba
                JMP     ADD_32_32	     	; NUM32 = Cluster_begin_lba + CURR_CLUSTER * D_SECT_PER_CLST

; **** Load A Block From FAT Into The Std Buffer ($600) ************************
; Input: NUM32: LBA number of FAT sector to load
; ******************************************************************************
LOAD_FAT_BLK    LDX     #$03
                STX     RES                 	; initialize byte counter
                
; check if current FAT block and last loaded FAT block are identical ***********
CHK_FAT_BLK     LDA     NUM32,X             		; load one byte of new block pointer
                CMP.NE 	CURR_FAT_BLK,X SET_CURR_FATBLK	; branch if not the same as the old one
		
                DEC     RES                 	; if equal decrement number of unequal bytes
SET_CURR_FATBLK STA     CURR_FAT_BLK,X-      	; store new block pointer byte as current byte
                BPL     CHK_FAT_BLK         	; compare and store more bytes if X >= 0
		
                LDA     RES                 	; RES is decremented down to -1 if all bytes equal
                BMI     LOAD_FAT_END        	; RES = $FF -> FAT block is already loaded, just exit
                
                PHW	BLKBUF			; save old memory pointer to stack
                LDXYI   NUM32
                JSR     DEV_RD_LBLK_BUF     	; load FAT block into standard buffer ($0600)
		PLW	BLKBUF			; restore old memory pointer from stack
LOAD_FAT_END    RTS
                
; **** Read Logical Block To Standard Buffer ($0600) ***************************
; Input: [X,Y] points to 32-bit LBA
; ******************************************************************************
DEV_RD_LBLK_BUF  LDA    #CMD_READ_BUF	        ; Read sector with LBA nr into $0600
                 JMP    CMDDEV			; Call Device-driver Read routine
                
; **** Read Logical Block ******************************************************
; Input: [X,Y] points to 32-bit LBA
;        BLKBUF,BLKBUFH = 16 Bit Destination Address
; ******************************************************************************
DEV_RD_LBLK     LDA     #CMD_READ	        ; Read sector with LBA nr into BLKBUF
                JMP     CMDDEV			; Call Device-driver Read routine
                
; Arithmetic Functions *********************************************************
; ******************************************************************************

; **** Load a 16 Bit Value Into NUM32 ******************************************
; Input:  UInt16[X,Y] = 16 Bit Unsigned Integer
; ******************************************************************************
LOAD_16         STXY	NUM32			; Load LSB in NUM32 and MSB in NUM32+1
		MWX	#0 NUM32+2		; Zero upper 16-bits
                RTS			   	; return
                
; **** Load a 32 Bit Value Into SUM32 ******************************************
; Input:  X,Y = Ptr[LO:HI] to 32 Bit Integer
; ******************************************************************************
LOAD_S32        STX     STOL		   	; store pointer LSB
                LDX     #$07		   	; SUM32 is 4 bytes above NUM32 in memory
                BNE     LOAD_32_1	   	; branch always

; **** Load a 32 Bit Value Into NUM32 ******************************************
; Input:  X,Y = Ptr[LO:HI] to 32 Bit Integer
; ******************************************************************************
LOAD_32         STX     STOL		   	; store pointer LSB
                LDX     #$03
LOAD_32_1       STY     STOH		   	; store pointer MSB
                LDY     #$03
LOOP_LOAD_32    MVA 	(STOL),Y NUM32,X-	; get byte and store in NUM32 (or SUM32)
                DEY.PL  LOOP_LOAD_32	   	; branch if not done yet
                RTS			   	; return
                
; **** Store 32 Bit Value In NUM32 To Destination At Ptr[X,Y] ******************
; Input:  X,Y = Ptr[LO:HI] to 32 Bit Integer
; ******************************************************************************
STORE_32        STXY	STOL			; store pointer LSB and MSB
STORE_32_D      LDY     #$03		   	; 1 DWORD = 4 bytes
LOOP_STORE_32   MVA     NUM32,Y (STOL),Y-   	; get number and store it
                BPL     LOOP_STORE_32	   	; branch if not done yet
                RTS			   	; return

; **** Increment a 32 Bit Value ************************************************
; NUM32[0:3] = 32 Bit Integer - NUM32 = NUM32 + 1
; ******************************************************************************
INC_32          LDX     #$01

; **** Add a 8 Bit Value To a 32 Bit Value *************************************
; Input X = 8 Bit Integer, NUM32[0:3] = 32 Bit Integer - NUM32 = UInt[X] + NUM32
; ******************************************************************************
ADD_32_8        LDY     #$00

; **** Add a 16 Bit Value To a 32 Bit Value ************************************
; Input X,Y = 16 Bit Integer, NUM32[0:3] = 32 Bit Integer - NUM32 = UInt[X,Y] + NUM32
; ******************************************************************************
ADD_32_16       STXY	SUM32			; Store byte or word in lower word
		MWX	#0 SUM32+2		; Clear upper 16-bits of DWORD

; **** Add a 32 Bit Value To a 32 Bit Value ************************************
; Input SUM32[0:3] = 32 Bit Integer, NUM32[0:3] = 32 Bit Integer  - NUM32 = SUM32 + NUM32
; ******************************************************************************
ADD_32_32       CLC			   	; clear carry flag
                LDX     #$00		   	; start with byte 0
                PHP			   	; save carry flag
ADD_LOOP        PLP			   	; get carry flag back
                LDA     SUM32,X		   	; Get SUM32 byte
                ADC:STA NUM32,X		   	; num32 = num32 + sum32, save in num32
                PHP			   	; save carry flag
                INX			   	; next byte
                CPX.NE  #$04 ADD_LOOP		; branch if not all bytes done yet
                PLP			   	; restore stack
                RTS			   	; return
