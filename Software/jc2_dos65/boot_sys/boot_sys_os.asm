;-------------------------------------------------------------------------------
; This file contains the OS portion of the BOOT.SYS file.
; 
; Assembler: MADS-Assembler
; V0.1: 22-05-25, Emile first version
;-------------------------------------------------------------------------------

; ******************************************************************************
; NUM32 shl X
; ******************************************************************************
SHL_32          ASL32	NUM32		  	; SHL with C=0
                DEX.NE	SHL_32		  	; decrement #shifts, branch if not done yet
                RTS			  	; return

; ******************************************************************************
; NUM32 shr X
; ******************************************************************************
SHR_32          LSR32	NUM32			; SHR with C=0
                DEX.NE  SHR_32		  	; decrement #shifts, branch if not done yet
                RTS			  	; return
                
; **** Write Logical Block From Standard Buffer ********************************
; Input: [X,Y] points to 32-bit LBA
DEV_WR_LBLK_BUF  LDA    #CMD_WRITE_BUF	  	; Call Device-driver Write routine
                 JMP    CMDDEV

; **** Write Logical Block *****************************************************
; Input: [X,Y] points to 32-bit destination LBA
;        BLKBUF,BLKBUFH = 16 Bit Source Address
DEV_WR_LBLK      LDA     #CMD_WRITE		; Call Device-driver Write routine
                 JMP     CMDDEV

; ******************************************************************************
; *                               OS Entry Point                               *
; ******************************************************************************
OS_MAIN         PRSTR	MSG_BOOT			; Print boot-message
                JSR     OS_SET_ROOT_DIR	     		; set D_ACTUAL_DIR and CURR_DIR_BLK to root-dir
                
; Init CFC LOAD and SAVE VECTORS ***********************************************
		MWA	#CFC_LOAD CF_LOAD_VEC		; macro CF_LOAD_VEC = CFC_LOAD, Used by BASIC with CMD_LOAD
		MWA	#CFC_SAVE CF_SAVE_VEC		; macro CF_SAVE_VEC = CFC_SAVE, Used by BASIC with CMD_SAVE
		MWA	#OS_SHELL_ENTRY RETURN_VECT	; Return-vector for Monitor and BASIC
		MWX	#0 Wrmjpl			; Reset BASIC warm-start vector, so that a reboot is also a BASIC cold-start
                
; Clear Mount Table ************************************************************
                CLC
CLR_MOUNT_TABLE MVA	#NULL_ID MOUNT_TABLE+8,X	; set device ID to NULL device
		TXA                          		; index to mount table into A
                ADC     #$20                 		; set to next entry
                TAX
                BCC     CLR_MOUNT_TABLE      		; repeat until all entries cleared
                
; Set Boot Device As Current Device ********************************************
                LDX     #$00		     	; X=0
                LDY     #$00                 	; set index to mount table = 0
                LDA     D_DEV_ID             	; get boot device ID
CHECK_FDC1      CMP.EQ  #FDD1_ID SET_BOOT_DRV	; If boot drive is FDD1, set current drive to A:
                INX		     	     	; X=2
                LDY     #$20                 	; set index to mount table = 32
CHECK_FDC2      CMP.EQ  #FDD2_ID SET_BOOT_DRV	; If boot drive is FDD2, set current drive to B:
                INX                          	; else set current drive (SDD or a HDD) to C:
                LDY     #$40                 	; set index to mount table = 64
SET_BOOT_DRV    STX     CURR_DRIVE	     	; 0=FDD1, 1=FDD2, 2=SD/CF card

; Add Boot Device To Mount Table ***********************************************
                LDX     #$00
ADD_DEVBLK      MVA 	CURR_VOLUME,X+ MOUNT_TABLE,Y+	; read from current device block and write to mount table
                CPX.NE  #32 ADD_DEVBLK         		; 32 bytes to copy
                LDA     #$F8
                CMP.NE  D_MEDIUM_DESCR CHK_FAT_TYPE	; Branch if boot device is not a harddisk
                MVA     PSAV D_MEDIUM_DESCR  		; yes, load partition number (ASCII) and save it as medium descr. byte
CHK_FAT_TYPE    LDA.EQ  D_FAT_TYPE OS_SHELL_ENTRY 	; get type of FAT, branch if FAT32 (0), has no static root directory blocks
                LDX     #$04		     		; D_NUM_ROOT_DIR is 0 for FAT32
CALC_DIR_BLKS   LSR16	D_NUM_ROOT_DIR	     		; divide #root-dir entries by 16 to get total #blocks
		DEX.NE	CALC_DIR_BLKS	     		; branch if not done yet
                
; TODO: ADD MORE DEVICES #######################################################

; **** Shell Entry Point *******************************************************
; ******************************************************************************
OS_SHELL_ENTRY  JMP     SH_CMD_PROMPT

; **** Read First Block Of Actual Directory ************************************
; The first block of D_ACTUAL_DIR cluster is read into DIR_BLK_BUF
; ******************************************************************************
OS_FIRST_DIR_BLK
                LDA.EQ  D_FAT_TYPE SET_DIR_CLUSTER  	; load type of FAT, branch if FAT32 (set cluster)
                JSR     OS_IS_ROOT_DIR       		; No FAT32, is it the root directory?
                BCC     SET_DIR_CLUSTER      		; no, set cluster

		MVAX	4 D_START_DIR CURR_DIR_BLK	; CURR_DIR_BLK = LBA root dir., FAT16/FAT12 only
                LDX     D_NUM_ROOT_DIR       		; set block counter
                BNE     SET_NUM_BLOCKS       		; branch always

SET_DIR_CLUSTER MVAX	4 D_ACTUAL_DIR CURR_CLUSTER	; CURR_CLUSTER = D_ACTUAL_DIR, FAT32 and FAT16/FAT12 subdir
CONVERT_CLUSTER JSR     CLUSTER_TO_BLK       		; convert CURR_CLUSTER to LBA number in NUM32
                LDXYI   CURR_DIR_BLK
                JSR     STORE_32             		; and save result as current directory block
                LDX     D_SECT_PER_CLST      		; load number of blocks per clusters
SET_NUM_BLOCKS  STX     CURR_BLK_NUM	     		; CURR_BLK_NUM = D_SECT_PER_CLST

; **** Load Directory Block ****************************************************
; This routine reads a directory block into DIR_BLK_BUF.
; It is called from OS_NEXT_DIR_BLK.
; ******************************************************************************
OS_LOAD_DIR     MWA	#DIR_BLK_BUF BLKBUF  ; BLKBUF now points to dir block buffer
		LDXYI	CURR_DIR_BLK         ; block number (LBA) to be loaded
                JMP     DEV_RD_LBLK          ; read directory block into DIR_BLK_BUF and return

; **** Read Next Directory Block ***********************************************
; The LBA nr in CURR_DIR_BLK is incremented by 1. It is called from OS_DIR_LOOP
; when CURR_BLK_NUM is between D_SECT_PER_CLST and 1.
; ******************************************************************************
OS_NEXT_DIR_BLK LDX     #$00
INC_DIR_BLK     INC.NE  CURR_DIR_BLK,X OS_LOAD_DIR	; increment current directory block and branch if no overflow
                INX                          		; overflow, increment next byte
                CMP.NE  #$04 INC_DIR_BLK     		; increment next byte if not all four bytes updated
                BEQ     OS_LOAD_DIR          		; branch always

; **** Read Next Directory Cluster *********************************************
; Reads a first sector of a directory cluster into DIR_BLK_BUF. It is called
; from OS_DIR_LOOP and resets CURR_BLK_NUM to D_SECT_PER_CLST.
; ******************************************************************************
OS_NEXT_DIR_CLSTR
                JSR     GET_NEXT_CLSTR       ; load next directory cluster nr from FAT
                BCC     CONVERT_CLUSTER      ; if not EOF convert it to LBA + load from disk
                RTS			     ; return
                
; **** Save Directory Block ****************************************************
; Write directory in DIR_BLK_BUF to disk with LBA nr in X,Y.
; Input: X,Y = Pointer to LBA nr.
; ******************************************************************************
OS_SAVE_DIR     STX	SAVEX
		STY	SAVEY
		MWA	#DIR_BLK_BUF BLKBUF	; set source block buffer to DIR_BLK_BUF
                LDX     SAVEX       		; pointer to block number (LBA) to be saved
                LDY     SAVEY
                JMP     DEV_WR_LBLK          	; write directory block in DIR_BLK_BUF to LBA in NUM32 and return
                
; **** Create New File on Disk *************************************************
; Input: FILENAME = String8_3
;        A        = File Attributes
;        FREE_CLUSTER: contains cluster nr for new File/Dir
; An empty dir. entry is written with: Filename, Attribute, Date, Time and Cluster Nr.
; ******************************************************************************
OS_CREATE_FILE  TAX                         		; save attributes into X
                LDY     #D_FILENAME         		; set index to filename
FILL_FILENAME   MVA     FILENAME,Y (CURR_DIR_ENTRY),Y+	; copy filename into current directory entry
                CPY.CC  #D_ATTRIBUTES FILL_FILENAME 	; branch if not all characters copied?
		
		; Write Attribute
                TXA                         	; yes, get attribute back to A
CLEAR_ENTRY     STA     (CURR_DIR_ENTRY),Y  	; copy attributes into current directory entry
                LDA     #$00                	; and clear all following bytes to 0
                INY
                CPY.CC  #$20 CLEAR_ENTRY	; branch if not everything cleared yet
		
		; Write create-Date and Last-write Date
GET_DATE_TIME   JSR     OS_FILEDATE         	; get current date as file date
                TYA				; Y = MSB
                LDY     #D_LAST_WR_DATE+1     	; set index to last write date MSB
                STA     (CURR_DIR_ENTRY),Y  	; store MSB of file last write date
                LDY     #D_CREATE_DATE+1     	; set index to create date MSB
                STA     (CURR_DIR_ENTRY),Y-  	; store MSB of file create date
                TXA				; X = LSB
                STA     (CURR_DIR_ENTRY),Y  	; store LSB of file create date
                LDY     #D_LAST_WR_DATE     	; set index to last write date LSB
                STA     (CURR_DIR_ENTRY),Y  	; store LSB of file date

		; Write create-Time and last-write Time
                JSR     OS_FILETIME         	; get current time as file time
                TYA				; Y = MSB
                LDY     #D_LAST_WR_TIME+1     	; set index to last write time MSB
                STA     (CURR_DIR_ENTRY),Y  	; store MSB of file last write time
                LDY     #D_CREATE_TIME+1     	; set index to create time MSB
                STA     (CURR_DIR_ENTRY),Y-  	; store MSB of file create date
                TXA				; X = LSB
                STA     (CURR_DIR_ENTRY),Y  	; store LSB byte of file create time
                LDY     #D_LAST_WR_TIME     	; set index to last write time LSB
                STA     (CURR_DIR_ENTRY),Y  	; store LSB of file last write time

		; Write File Cluster Nr High and Low
		LDY	#D_START_CLSTH+1			; index of MSB of 1st_cluster_high
		MVA	FREE_CLUSTER+3  (CURR_DIR_ENTRY),Y-	; MSB of new cluster nr
		MVA	FREE_CLUSTER+2  (CURR_DIR_ENTRY),Y	; LSB of 1st_cluster_high
		LDY	#D_START_CLST+1				; index of MSB of 1st_cluster_low
		MVA	FREE_CLUSTER+1  (CURR_DIR_ENTRY),Y-	; store in MSB of 1st_cluster_low
		MVA	FREE_CLUSTER    (CURR_DIR_ENTRY),Y	; store in LSB of 1st_cluster_low
		
		LDXYI	CURR_DIR_BLK		; Write new subdir entry in current dir (CURR_DIR_BLK is LBA of current dir block)
                JMP     OS_SAVE_DIR	    	; write this dir entry back to disk and return
                
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
		MVA	#$00 NUM32+1	    ; clear upper byte of result
                LDX     #$06
                JSR     SHL_32              ; shift left NUM32 by 6 bits
                LDA     SUM32+1             ; load MINUTE into A
                ORA:STA NUM32               ; and add value into result
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
                ADD     #20                 ; file date starts from 1980, so we have to add 20 to our year 2000 based RTC date
                STA     NUM32               ; store YEAR into lower byte of Word[NUM32:NUM32+1]
                MVA     #$00 NUM32+1	    ; clear upper byte of result
                LDX     #$04
                JSR     SHL_32              ; shift left NUM32 by 4 bits
                LDA     SUM32+1             ; load MONTH into A
                ORA:STA NUM32               ; and add value into result
                LDX     #$05
                JSR     SHL_32              ; shift left NUM32 by 5 bits
                LDA     SUM32+2             ; load DAY into A
                ORA     NUM32               ; and add value into result
                TAX
                LDY     NUM32+1             ; result is in Word[X,Y]
                RTS
                
; **** Convert BCD Date Or Time Values Into Unpacked Binary ********************
CONVERT_DATETIME
                STA     SUM32		    ; Year or Hour
		STXY	SUM32+1		    ; X=Month/Minutes, Y=Day/Seconds
                LDY     #$02		    ; 3 bytes to convert
CONVERT_BCD     LDA     SUM32,Y		    ; Get byte
                JSR     BCD_TO_BIN	    ; Convert BCD to 8-bit binary
                STA     SUM32,Y-	    ; Store result back
                BPL     CONVERT_BCD	    ; branch if not done yet
                RTS			    ; return

; **** Copy First block of File to memory **************************************
; Input:
; ******************************************************************************
COPY_BLK0_DEST	JSR	INIT_FBUF_PTR		; PSTR = Ptr(FILE_BUFF)
		LDA.EQ	FTYPE BASFILE		; 0 = .BAS, 1=.COM, 2=.EXE, branch if .BAS file
		CMP.EQ	#1 COMFILE		; branch if a .COM file
		
		; .EXE file (FTYPE=2)
		MVA	FILE_BUFF OS_PROG	; load- and run-address LSB = FILE_BUFF LSB
		STA	STOL			; destination address LSB
		MVA	FILE_BUFF+1 OS_PROG+1	; load- and run-address MSB = FILE_BUFF MSB
		STA	STOH			; destination address MSB
		MVA	#<FILE_BUFF+2 PSTR	; load-address offset: start-address = 2nd byte in FILE_BUFF
		BNE	COPY_BLK_DEST		; branch always
		
		; .COM file  (FTYPE=1)
COMFILE		MVA	#<COM_RUN_ADDR OS_PROG	 ; Load- and run-address LSB
		STA	STOL			 ; destination address LSB
		MVA	#>COM_RUN_ADDR OS_PROG+1	 ; load- and run-address MSB
		STA	STOH			 ; destination address MSB
		BNE	COPY_BLK_DEST		 ; branch always
		
		; .BAS file (FTYPE=0)
BASFILE		MWA	#BAS_LOAD_ADDR STOL	; destination = BAS_LOAD_ADDR

; **** Copy Second and other blocks of File to Memory **************************
; Input:
; ******************************************************************************
COPY_BLK_DEST	LDY	#0
CP_BLK0_LP	MVA	(PSTR),Y (STOL),Y		; Get byte from buffer and store in destination
		INW	STOL				; Increment destination pointer (macro)
		INW	PSTR				; Increment buffer pointer (macro)
		LDA	PSTR+1				; MSB of buffer pointer
		CMP.NE	#>FILE_BUFF+2 CP_BLK0_LP		; branch if not 2 pages (512 bytes) increased yet
		RTS					; return
		
FTYPE		.byte  	$00				; 0 = .BAS, 1=.COM, 2=.EXE

; **** Init File Buffer ********************************************************
; This routine sets BLKBUFL/BLKBUFH to FILE_BUFF
; ******************************************************************************
INIT_FILE_BUFF	MWA	#FILE_BUFF BLKBUFL	; macro BLKBUF = FILE_BUFF
		RTS				; return
		
; **** Init File-buffer pointer ************************************************
; This routine sets PSTR to FILE_BUFF
; ******************************************************************************
INIT_FBUF_PTR	MWA	#FILE_BUFF PSTR		; macro PSTR = FILE_BUFF
		RTS				; return

; **** Load BAS/COM/EXE File ***************************************************
; Input: CURR_CLUSTER: cluster nr of file to load
; ******************************************************************************
OS_LOAD_FILE    JSR     OS_FILE_EMPTY       	; check if filesize is 0
                BCC     OS_LOAD_COM2		; branch if file is not empty
		
                RTS                         	; filesize is 0, just do nothing
		
OS_LOAD_COM2    LDXYI   CURR_CLUSTER      	; current cluster nr
                JSR     CLSTR_TO_BLK        	; convert cluster number to LBA number in NUM32
                MVA     #$00 BCNT              	; init. block counter
		; Emile: This was apparently an error: SCNT was not initialized, now added here
                MVA     D_SECT_PER_CLST SCNT   	; SCNT = numbers of sectors per cluster
                LDY     #D_FILE_SIZE+1      	; index to file size in dir. entry
                LDA     (CURR_DIR_ENTRY),Y  	; load file size byte 1
                LSR                         	; check if bit 0 is set (bytes 256-511 of buffer)
                PHA			    	; save byte: now contains file-size in blocks of 512 bytes
                BCS     LOAD_COM1           	; yes, add one block
		
                DEY					; now points to D_FILE_SIZE LSB
                LDA.EQ  (CURR_DIR_ENTRY),Y LOAD_COM1	; load file size byte 0, branch if 0
                SEC                         		; yes, add one block
LOAD_COM1       PLA
                ADC:STA BCNT                		; calc used blocks, BCNT now contains number of blocks
                CMP.CS  #89 OS_SIZE_ERR         	; branch if file is too big (>88 blocks, 44 KB, $3000-$E000)
		
                LDY     #D_FILE_SIZE+2      		; index to file size entry
                LDA.NE  (CURR_DIR_ENTRY),Y OS_SIZE_ERR	; load file size byte 2, branch if > 0 -> file is too big
                INY					; D_FILE_SIZE+3
                LDA.NE  (CURR_DIR_ENTRY),Y OS_SIZE_ERR 	; load D_FILE_SIZE+3, branch if > 0 -> file is too big
		
		; Read first part of file into FILE_BUFF
		JSR	INIT_FILE_BUFF		; Set BLKBUF pointer to FILE_BUFF
                LDXYI   NUM32		    	; NUM32 contains LBA of cluster to read
                JSR     DEV_RD_LBLK         	; Read first block of file into FILE_BUFF
		DEC     SCNT                	; First block is already read in
		DEC	BCNT			; Number of blocks to read
		JSR	COPY_BLK0_DEST		; Copy first block to destination
		
                JSR     LOAD_NEXT_BLKS	    	; Load next blocks of file and execute it
                LDA     #HDD1_ID   	    	; Replace by D_DEV_ID?
		JMP     OPEN_DEVICE		; Init. device driver again and return
		
OS_SIZE_ERR     LDXYI   MSG_SIZE_ERR      	; load error message...
                JMP     OS_PRINT_ERR		; Print it

; **** Read Next File Blocks ***************************************************
; ******************************************************************************
LOAD_NEXT_BLKS  JSR     INC_32              	; Increment LBA block address in NUM32
LOAD_BLK0       JSR	INIT_FILE_BUFF		; Set BLKBUF pointer to FILE_BUFF
		LDXYI   NUM32		    	; NUM32 contains LBA of cluster to read
                JSR     DEV_RD_LBLK         	; and read next block of file into FILE_BUFF
		JSR	INIT_FBUF_PTR		; reset file-buffer pointer PSTR to FILE_BUFF again
		JSR	COPY_BLK_DEST	    	; Copy block to destination
                DEC.EQ  BCNT OS_EXEC_CHK      	; branch if no more blocks to read
		DEC.NE  SCNT LOAD_NEXT_BLKS    	; branch if more blocks in cluster to read

; next cluster needs to be loaded considering the volume FAT type **************
NEXT_CLUSTER0   JSR     GET_NEXT_CLSTR	   	; Get next cluster from FAT table in CURR_CLUSTER
                BCS     OS_EXEC_CHK	    	; C=1, EOF, go execute File
			
                JSR     CLUSTER_TO_BLK	    	; convert CURR_CLUSTER to LBA number in NUM32
                MVA     D_SECT_PER_CLST SCNT   	; SCNT = numbers of sectors per cluster
                JMP     LOAD_BLK0	    	; branch always
		
; Run file if needed ***********************************************************
; ******************************************************************************
OS_EXEC_CHK	LDA.NE	FTYPE OS_EXECUTE	; 0 = .BAS, 1=.COM, 2=.EXE, branch if an executable file
		RTS				; return in case of a .BAS file

; **** Execute File ************************************************************
; Input:  Ptr[OS_PROG] to Start Address
; Output: A - Result Code
; ******************************************************************************
OS_EXECUTE      JMP     (OS_PROG)           	; run .com or .exe file
OS_PROG         .word      $0000
                
; ******************************************************************************
INIT_FREE_CLUSTER
		MVA     #$02 FREE_CLUSTER       ; first data cluster is $000002
                MVA     #$00 FREE_CLUSTER+1
		STA     FREE_CLUSTER+2
                STA     FREE_CLUSTER+3
                RTS
                
; ******************************************************************************
; Input: FREE_CLUSTER: number of possible free cluster
; Output: C=1: cluster is free ; C=0: 
; ******************************************************************************
OS_NEXT_FREE_CLUSTER
                MVAX	4 FREE_CLUSTER CURR_CLUSTER	; CURR_CLUSTER = FREE_CLUSTER
                JSR     GET_NEXT_CLSTR      		; get FAT entry for current cluster
                LDX     #$03
CHK_FREE        LDA.NE  CURR_CLUSTER,X SET_NEXT_CLSTR	; branch if cluster is not free
                DEX.PL	CHK_FREE	    		; branch if not all bytes checked
                SEC                         		; cluster is free, exit with C = 1
                RTS

; ******************************************************************************
; This routines increments FREE_CLUSTER by 1, called from OS_NEXT_FREE_CLUSTER.
; ******************************************************************************
SET_NEXT_CLSTR  LDX     #$00
INC_FREE_CLSTR  INC.NE  FREE_CLUSTER,X CHK_MAX_FAT 		; increment FREE_CLUSTER[0:3], branch if no overflow
                INX						; next byte of FREE_CLUSTER
                CPX.NE  #$04 INC_FREE_CLSTR	 		; branch if not done yet
		
CHK_MAX_FAT     LDX     #03					; 3 bytes to check
CHK_MAX_FAT2    LDA     CURR_FAT_BLK,X	    			; LBA of current FAT block
                CMP.CC  D_START_FAT2,X OS_NEXT_FREE_CLUSTER	; end of FAT1 reached? No, check next FAT entry
                DEX.PL  CHK_MAX_FAT2	      			; branch if not done yet
                CLC                         			; no emtpy cluster found, exit with error
                RTS

; ******************************************************************************
; This routine sets a DWORD in the FAT sector from free to allocated, it is 
; called from OS_ADD_CLUSTER. The correct FAT page has already been loaded into 
; BLOCK_BUFF ($600) by OS_NEXT_FREE_CLUSTER -> GET_NEXT_CLUSTER. So the cluster nr
; in CURR_CLUSTER needs to be allocated in the FAT table.
; ******************************************************************************
WRITE_FAT_ENTRY	PRCH	'['			; Print [
		PRHEX16	CURR_CLUSTER		; Print CURR_CLUSTER
		PRCH	']'
		LDA	CURR_CLUSTER		; get LSB of cluster nr
	:2	ASL				; SHL2, DWORD index in FAT page
		TAY				; Y = DWORD byte 0 in FAT page
	.rept 3					; Write 3 x $FF into FAT entry
		LDA	#$FF			
		JSR	WRITE_ENTRY_BYTE	; Write 1st $FF
		INY				; Next byte in DWORD
	.endr	
		LDA	#$0F			; write end marker
		JSR	WRITE_ENTRY_BYTE	; Write 4th byte ($0F) and return
		LDXYI	CURR_FAT_BLK		; LBA of current FAT block
		JMP	DEV_WR_LBLK_BUF		; write FAT block back to disk and return
		
; **** Write a Single FAT Entry Byte To Block Buffer ***************************
; INPUT : A = Write Data
;         Y - Index To FAT Entry Byte
; ******************************************************************************
WRITE_ENTRY_BYTE
		TAX
                LDA     CURR_CLUSTER+1
                LSR				; check bit 0 of free_cluster[1]
                TXA
                BCS     WR_UPPER_PAGE       	; if bit 0 = 1 then write byte to upper half of block
		
                STA     BLOCK_BUFF,Y		; write entry byte to lower half of block buffer
		RTS				
		
WR_UPPER_PAGE   STA     BLOCK_BUFF+256,Y	; write entry byte to upper half of block buffer
		RTS
                
; **** Add Date and Time to subdir entry ***************************************
; ******************************************************************************
ADD_DATE_TIME	; Write create-Date and Last-write Date
		JSR     OS_FILEDATE         	; get current date as file date
                TYA				; Y = MSB
                LDY     #D_LAST_WR_DATE+1     	; set index to last write date MSB
                STA     (BLKBUF),Y  		; store MSB of file last write date
                LDY     #D_CREATE_DATE+1     	; set index to create date MSB
                STA     (BLKBUF),Y-  		; store MSB of file create date
                TXA				; X = LSB
                STA     (BLKBUF),Y  		; store LSB of file create date
                LDY     #D_LAST_WR_DATE     	; set index to last write date LSB
                STA     (BLKBUF),Y  		; store LSB of file date

		; Write create-Time and last-write Time
                JSR     OS_FILETIME         	; get current time as file time
                TYA				; Y = MSB
                LDY     #D_LAST_WR_TIME+1     	; set index to last write time MSB
                STA     (BLKBUF),Y  		; store MSB of file last write time
                LDY     #D_CREATE_TIME+1     	; set index to create time MSB
                STA     (BLKBUF),Y-  		; store MSB of file create date
                TXA				; X = LSB
                STA     (BLKBUF),Y  		; store LSB byte of file create time
                LDY     #D_LAST_WR_TIME     	; set index to last write time LSB
                STA     (BLKBUF),Y  		; store LSB of file last write time
		RTS
		
; **** Add new subdirectory entry to dir buffer ********************************
; Input: X=1: . subdir, X=2: .. subdir
; ******************************************************************************
ADD_NEW_SUBDIR	TXA					; . or .. subdir
		PHA					; save it
		LDY	#D_FILENAME			; Filename entry
ANS_FILL_FN	MVA     #' ' (BLKBUF),Y+			; Fill filename entry with spaces
                CPY.CC  #D_ATTRIBUTES ANS_FILL_FN	; branch if not all chars copied

		MVA	#FA_DIRECTORY (BLKBUF),Y		; set directory attribute
		LDY	#D_FILENAME			; Filename entry
		LDA	#'.'				; 1 or 2 dots for subdir name
ANS_LP1		STA	(BLKBUF),Y+
		DEX.NE	ANS_LP1				; branch if name not done yet

		JSR	ADD_DATE_TIME			; Add date and time to subdir entry
		PLA
		CMP.NE	#1 PARENT_DIR			; branch if .. Subdir
	
		; Current subdir .: Write File Cluster Nr High and Low
		LDY	#D_START_CLSTH+1		; index of MSB of 1st_cluster_high
		MVA	FREE_CLUSTER+3 (BLKBUF),Y-	; store in MSB of 1st_cluster_high
		MVA	FREE_CLUSTER+2 (BLKBUF),Y	; store in LSB of 1st_cluster_high
		LDY	#D_START_CLST+1			; index of MSB of 1st_cluster_low
		MVA	FREE_CLUSTER+1 (BLKBUF),Y-	; store in MSB of 1st_cluster_low
		MVA	FREE_CLUSTER   (BLKBUF),Y	; store in LSB of 1st_cluster_low
		RTS					; return
		
		; Parent subdir ..: Write File Cluster Nr High and Low
PARENT_DIR	LDY	#D_START_CLSTH+1		; index of MSB of 1st_cluster_high
		MVA	D_ACTUAL_DIR+3 (BLKBUF),Y-	; store in MSB of 1st_cluster_high
		MVA	D_ACTUAL_DIR+2 (BLKBUF),Y	; store in LSB of 1st_cluster_high
		LDY	#D_START_CLST+1			; index of MSB of 1st_cluster_low
		MVA	D_ACTUAL_DIR+1 (BLKBUF),Y-	; store in MSB of 1st_cluster_low
		MVA	D_ACTUAL_DIR   (BLKBUF),Y	; store in LSB of 1st_cluster_low
		RTS
		
; **** Add new Directory Cluster to Disk ***************************************
; Create a new Directory Cluster in DIR_BLK_BUF and write . and .. subdirs in it.
; ******************************************************************************
ADD_NEW_DIR_CLST
		MWA	#DIR_BLK_BUF BLKBUF	; BLKBUF now points to dir block buffer
		LDX	#1
		LDA	#0
ANDIR_LP1	STA.NE	(BLKBUF),Y+ ANDIR_LP1	; Clear dir block buffer (512 bytes) and loop
		INC	BLKBUF+1		; next page
		DEX.PL	ANDIR_LP1		; branch always
		
		MVA	#>DIR_BLK_BUF BLKBUF+1	; Set to begin of buffer again
		LDX	#1			; 1 = . subdir entry
		JSR	ADD_NEW_SUBDIR		; add subdir . (current dir)
		ADB	BLKBUFL #$20		; BLKBUF += $20, next dir entry
		LDX	#2			; 2 = .. subdir entry
		JSR	ADD_NEW_SUBDIR		; add subdir .. (parent dir)

		; and write it to disk
.if	DBG_PRINT = 1
		PRSTR	TXT_CURR_CLST3		; Print 'ADD_NEW_DIR, FREE=$'
		PRHEX32	FREE_CLUSTER		; print FREE_CLUSTER in hex
		JSR	CROUT			; Print CR
.endif
		LDXYI	FREE_CLUSTER		; Write new subdir in cluster with FREE_CLUSTER nr
		JSR	CLSTR_TO_BLK		; Convert FREE_CLUSTER nr to LBA nr in num32
		LDXYI	NUM32			; LBA nr
		JMP	OS_SAVE_DIR		; Save new subdir to disk and return
		
TXT_CURR_CLST3	.by	'ADD_NEW_DIR_CLST, Free=$' $00

; **** Add First Cluster To Empty File *****************************************
; ******************************************************************************
OS_ADD_CLUSTER  JSR	INIT_FREE_CLUSTER	; FREE_CLUSTER = 0L
		JSR     OS_NEXT_FREE_CLUSTER	; Get free cluster in FREE_CLUSTER
                BCC     ADD_CLUSTER_END     	; no free cluster found
		
                JSR     WRITE_FAT_ENTRY	    	; Allocate CURR_CLUSTER, also writes FAT block back to disk
ADD_CLUSTER_END RTS

; **** Create New File *********************************************************
; Input:  A = File Attributes
; Output: C = 0 - Error; C = 1 - No Error
;         A = $FF - File/Directory already exists; A <> $FF Write Error Codes
; ******************************************************************************
OS_CREATE       STA     F_ATTRIBS           	; save attributes
                AND.EQ  #FA_DIRECTORY ADD_FILE 	; create a directory? Branch if not (add a file)
                JSR     OS_DIR_EXISTS       	; C=1: dirname already exists
		BCC	OS_CREATE_CONT		; branch if dirname does not exist yet
		BCS	OS_CREATE_ERR		; branch on error

ADD_FILE        JSR     OS_FILE_EXISTS      	; check if file already exists
OS_CREATE_CONT	
		LDA     #$FF                	; error result -1 if file/dir exists
                BCS     OS_CREATE_ERR       	; file already exists, exit with error $FF

		JSR	OS_ADD_CLUSTER		; Return free cluster in CURR_CLUSTER and write FAT
		
		; Find a Free dir. entry and fill it with file info and save it to disk
		JSR 	OS_FIND_FREE		; Find a free directory entry in the current directory
.if	DBG_PRINT = 1
		PRSTR	TXT_FFREE1		; DEBUG
		PRHEX16	CURR_DIR_ENTRY
		JSR	CROUT			; Print CR
.endif
                LDA     F_ATTRIBS		; Get file/dir attributes
		JSR	OS_CREATE_FILE		; create the file/dir on disk
		
		JSR	ADD_NEW_DIR_CLST	; Add new dir cluster with . and .. and save it to disk
		SEC				; C=1, OK
                RTS
OS_CREATE_ERR   CLC				; C=0, error
OS_CREATE_END   RTS				; return
                
TXT_FFREE1	.by	'Free Dir Entry $' $00

; **** Open File ***************************************************************
; ******************************************************************************
OS_OPEN         RTS

; **** Close File **************************************************************
; ******************************************************************************
OS_CLOSE        RTS

; **** Set File Size ***********************************************************
; ******************************************************************************
OS_SET_SIZE     RTS

; **** Delete File *************************************************************
; ******************************************************************************
OS_DELETE       RTS

; **** Test If File Is Empty ************** ************************************
; Input:  Ptr(CURR_DIR_ENTRY)
; Output: C = 1 - File is empty; C = 0 - File not empty
; ******************************************************************************
OS_FILE_EMPTY   LDX     #$04
                LDY     #D_FILE_SIZE        			; index to file size
                CLC
NEXT_SIZE_BYTE  LDA.NE  (CURR_DIR_ENTRY),Y OS_FILE_EMPTY2	; branch if file-size byte > 0 (file is not empty)
                INY
                DEX.NE	NEXT_SIZE_BYTE      			; test next byte
                SEC			    			; C=1: File is empty
OS_FILE_EMPTY2  RTS			    			; return

; **** Check If a directory entry is free or deleted ***************************
; ******************************************************************************
OS_FIND_FREE   	LDXYI	CB_FIND_FREE_DIR_ENTRY
                BNE     OS_FIND			; branch always

; **** Check If Directory Already Exists ***************************************
; ******************************************************************************
OS_DIR_EXISTS   LDXYI   CB_DIR_EXISTS
                BNE     OS_FIND			; branch always
                
; **** Check If Directory Is Available *****************************************
; ******************************************************************************
OS_FILE_EXISTS  LDXYI   CB_FILE_EXISTS
                BNE     OS_FIND			; branch always

; **** Find Directory **********************************************************
; ******************************************************************************
OS_FIND_PATH    LDXYI   CB_FIND_SUBDIR
                BNE     OS_FIND			; branch always

; **** Find File ***************************************************************
; ******************************************************************************
OS_FIND_FILE    LDXYI   CB_FIND_FILE
                                           ; fall through to OS_FIND
                                           
; **** Find All Files **********************************************************
; ******************************************************************************
OS_FIND_ALL     LDA     #$FF                ; disable total file counting
                BNE     OS_FIND2

; **** Main Find Routine *******************************************************
; ******************************************************************************
OS_FIND         LDA     #$00                ; enable total file counting
OS_FIND2        STXY    CMD_ADDR            ; set command function address
                                            ; fall through to OS_DIR_LOOP

; **** Loop Through Actual Directory *******************************************
; Input:  Ptr[X:Y] = Address to command specific function
;         A = $00 - Enable total file counting; A <> $00 - counting disabled
; Output: C = 1 - Found; C = 0 - Not Found
; ******************************************************************************
OS_DIR_LOOP     STA     TERM_FLAG           			; set/clear counter termination flag
                MWA	#0 CURR_FILE_CNT    			; clear total file counter
                JSR     OS_FIRST_DIR_BLK    			; load first block of actual directory
LOWER_DIR_BLK   LDXYI   DIR_BLK_BUFL        			; set pointer to lower page of block buffer
SET_CURR_ENTRY  STXY	CURR_DIR_ENTRY	    			; select current directory entry
GET_CURR_ENTRY  LDY     #D_ATTRIBUTES       			; index to file attributes
                LDA     (CURR_DIR_ENTRY),Y  			; load file attributes
                TAX                         			; load attributes into X
                LDY     #D_FILENAME         			; index to filename
                LDA.EQ  (CURR_DIR_ENTRY),Y OS_DIR_LOOP_EOF	; load first char of filename and branch if NULL (= last entry)
                JSR     CMD_EXECUTE         			; call command routine
                BCS     OS_DIR_LOOP_END	    			; C=1: OK and return
		
NEXT_ITEM       LDA.NE  TERM_FLAG END_LOOP_CHK           	; check if count is terminated, branch if flag > 0
		INW	CURR_FILE_CNT	    			; yes, increment total file counter
END_LOOP_CHK    ADB	CURR_DIR_ENTRY #$20 			; CURR_DIR_ENTRY += $20, next dir. entry
                BCC     GET_CURR_ENTRY	    			; LSB overflow?
		
                LDA     #$00
                ADC:STA	CURR_DIR_ENTRY+1    			; MSB +1
                CMP.NE  #(> DIR_BLK_BUFH)+1 UPPER_DIR_BLK	; branch if we are in 2nd page above 512 B dir buffer

                DEC.NE  CURR_BLK_NUM LOAD_DIR_BLK	    	; CURR_BLK_NUM counts from D_SECT_PER_CLST down to 0, load next block if > 0
                LDA.EQ  D_FAT_TYPE LOAD_DIR_CLSTR          	; get FAT type, branch if FAT32 (load next dir cluster)
                BIT     D_ATTRIBUTES        			; else check if root directory
                BCS     OS_DIR_LOOP_EOF     			; if root dir and not FAT32, all directory blocks read. Exit

LOAD_DIR_CLSTR  JSR     OS_NEXT_DIR_CLSTR   ; load next directory cluster from device
                BCC     LOWER_DIR_BLK       ; and reset read pointer to lower page of block buffer
                BCS     OS_DIR_LOOP_EOF     ; directory EOF reached. Exit

LOAD_DIR_BLK    JSR     OS_NEXT_DIR_BLK     ; load next directory block from device
                JMP     LOWER_DIR_BLK       ; and reset read pointer to lower page of block buffer

UPPER_DIR_BLK   LDXYI   DIR_BLK_BUFH        ; set pointer to upper page of block buffer
                JMP     SET_CURR_ENTRY	    ; branch to begin of loop

OS_DIR_LOOP_EOF CLC			    ; C=1: not found
OS_DIR_LOOP_END RTS			    ; return

; **** Set Drive Command *******************************************************
; Input:  A = Drive Number (0..25)
; Output: C = 0 - Error
; ******************************************************************************
OS_SET_DRIVE    CMP.EQ  CURR_DRIVE SET_DRIVE_END	; Branch (just exit) if actual drive equals current drive
                CMP.CS  #$08       SET_DRIVE_ERR  	; show error message if actual drive number exceeds max drive?
                TAX                         		; save actual drive number to Y
                LDA     CURR_DRIVE          		; load current drive number into A
        :5      ASL             	    		; multiply current drive number by 32
                STA     STOL           	    		; and save it to indirect pointer low byte
                MVA	#>MOUNT_TABLE STOH  		; set high byte of mount table
		MVAY	$20 CURR_VOLUME (STOL)		; update device descriptor in mount table
                TXA
        :5      ASL             	    		; multiply current drive number by 32
                STA     STOL                		; and save it to indirect pointer low byte
                LDY     #$08                		; set index to D_DEV_ID
                LDA.EQ  (STOL),Y SET_DRIVE_ERR		; load device ID, if NULL then show error message

		MVAY    $20 (STOL) CURR_VOLUME	; load device descriptor into CURR_VOLUME
                STX     CURR_DRIVE          	; store actual drive as current drive

; TODO: Invalidate Block Buffers ###########################################################################

                LDA     D_DEV_ID            ; get current device id
                JSR     OPEN_DEVICE         ; and open the device driver
SET_DRIVE_END   SEC			    ; C=1: no error
                RTS

SET_DRIVE_ERR   LDXYI   MSG_DRIVE_ERR	
                JMP     OS_PRINT_ERR	    ; Print 'Drive not found'

; **** Test If Root Directory **************************************************
; Input:
; Output: C = 1 - is root dir; C = 0 - is not root dir
; ******************************************************************************
OS_IS_ROOT_DIR  LDY     #$03
COMP_DIR        LDA     D_ACTUAL_DIR,Y	     		; contains cluster nr of actual dir.
                CMP.NE  D_START_DIR,Y OS_IS_ROOT_END	; branch if not a root dir.
                DEY.PL	COMP_DIR             		; test next cluster byte
		
                SEC			     		; C=1 => is root dir.
                RTS
OS_IS_ROOT_END  CLC			     		; C=0 => not a root dir.
                RTS

; **** Set Root Directory Command **********************************************
; ******************************************************************************
OS_SET_ROOT_DIR LDX	#3				; 4 byte to copy
OS_SET_RDIR_LP	MVA 	D_START_DIR,X D_ACTUAL_DIR,X	; D_ACTUAL_DIR = root dir cluster nr
		STA	CURR_DIR_BLK,X-			; Save in CURR_DIR_BLK
		BPL	OS_SET_RDIR_LP			; branch if not done yet
		
		MWA	#BSLASH	D_SUBDIR_NAME		; Add '\'and '\0' to D_SUBDIR_NAME
		RTS					; return

; **** Set Directory Command ***************************************************
; Input : CURR_DIR_ENTRY: Pointer into current directory block
; Copy the directory cluster number to D_ACTUAL_DIR and CURR_DIR_BLK.
; A cluster number cannot be < 2. If a cluster number is 0, then it is 
; considered to be the root-dir and cluster number is set to 2.
; ******************************************************************************
OS_SET_DIR      PRHEX16	CURR_DIR_ENTRY
		PRCH	','
		PRCLW	D_START_CLSTH CURR_DIR_ENTRY	; Print 1st cluster HIGH word
		PRCLW	D_START_CLST  CURR_DIR_ENTRY	; Print 1st cluster LOW  word
		PRCH	']'
		LDY     #D_START_CLSTH+1		; MSB of 1st cluster HIGH word
                LDX     #$04				; Copy 4 bytes
		STX	OS_DWORD0			; Flag for zero all 4 bytes 
		DEX					; Copy bytes 3..0
		JSR	OS_SET_WORD			; Save 1st cluster HIGH word
		LDY	#D_START_CLST+1			; MSB of 1st cluster LOW word
		JSR	OS_SET_WORD			; Save 1st cluster LOW word
		LDA.EQ	OS_DWORD0 OS_SET_ROOT_DIR	; branch if flag is 0 (all 4 bytes are 0), branch to set actual dir to D_START_DIR
                RTS			     		; return
                
;-------------------------------------------------------------------------------------------------
; Used by OS_SET_DIR to copy a cluster nr in a dir. entry to D_ACTUAL_DIR and CURR_DIR_BLK
; A cluster nr is stored in two words in a dir. entry: a High word in $14 and a Low word in $1A.
; A flag (OS_DWORD0) is used to check if all 4 bytes are zero.
;-------------------------------------------------------------------------------------------------
OS_SET_WORD	JSR	OS_SET_BYTE				; Store word in D_ACTUAL_DIR and CURR_DIR_BLK
OS_SET_BYTE	MVA	(CURR_DIR_ENTRY),Y D_ACTUAL_DIR,X	; Get byte from D_START_CLSTH or D_START_CLST and save in D_ACTUAL_DIR
		STA.NE	CURR_DIR_BLK,X SET_BYTE_NOT0		; Save in CURR_DIR_BLK and branch if > 0
		DEC	OS_DWORD0				; decrement flag for zero all 4 bytes
SET_BYTE_NOT0	DEY				
		DEX
		RTS				; return
OS_DWORD0	.byte	$00

; **** Read Input String *******************************************************
; Output: Null terminated string in STRBUF
; ******************************************************************************
OS_STRING_IN    JSR     STRIN                ; input string into string buffer
                MWA	#STRBUF PSTR	     ; set string pointer to buffer
                MVA     #$00    STRBUF,X     ; terminate string with NULL
                RTS

; **** Print Error Message *****************************************************
; Input:  Ptr[X:Y] = Pointer to Error Message
; Output: C = 0
; ******************************************************************************
OS_PRINT_ERR    JSR     OS_STRING_OUT
                CLC
                RTS
                
; **** Parse Full Path String **************************************************
; Input:  Ptr[X:Y] to Path String
; Output: C = 0 - Error; C = 1 - No Error
;         A = $00       - End Of String
;           = $FF       - Path Not Found
;           = ?         - Wildcard Included
;           = PATH_SEP  - No Trailing Name
; ******************************************************************************
OS_PARSE_PATH   STXY    PSTR                	; save string pointer
                LDY     #$00
                STY     TERM_CHAR
                LDA     #PATH_SEP
                CMP.NE  (PSTR),Y PARSE_PATH2	; check if first char is the path seperator. Branch if not, just check the path
                JSR     OS_SET_ROOT_DIR     	; yes, switch to root directory
                LDY     #$00
PARSE_PATH      INY
PARSE_PATH2     LDA.EQ  (PSTR),Y PARSE_TERM     ; load next char from path string, branch if NULL (set termination char)
                CMP.EQ  #SPC     PARSE_TERM     ; Set termination char if it is a SPACE char
		
PARSE_NAME      JSR     OS_PARSE_NAME       ; no, parse partial path name
                BCC     PARSE_PATH_END      ; filename includes forbidden chars, exit with error
		
                CMP.NE  #PATH_SEP PARSE_PATH_OK	; is termination char the path seperator? No, trailing name, exit withour error
                STY     PSAV                ; save string index
                JSR     OS_FIND_PATH        ; yes, find and switch patch
                LDY     PSAV                ; restore string index
                BCC     PARSE_PATH_ERR      ; path not found, exit with error
                BCS     PARSE_PATH          ; branch always
                
PARSE_TERM      CLC
                LDA.NE  TERM_CHAR PARSE_PATH_END2	; branch if TERM_CHAR > 0
                MVA     #PATH_SEP TERM_CHAR		; TERM_CHAR = PATH_SEP
                BNE     PARSE_PATH_OK			; branch always
                
PARSE_PATH_ERR  LDA.NE  #$FF PARSE_PATH_END2    ; path not found error
PARSE_PATH_OK   SEC
PARSE_PATH_END  LDA     TERM_CHAR           	; load termination char as error status into A
PARSE_PATH_END2 RTS

; **** Parse A Partial Path Name String ****************************************
; Input:  A = First Char Of Partial Path
;         Y = Index Into Path String
; Output: C = 0 - Error; C = 1 - No Error
;         TERM_CHAR = NULL      - Filename
;                   = ?         - Wildcard Chars Included
;                   = PATH_SEP  - Subdirectory Name
; ******************************************************************************
OS_PARSE_NAME   LDX     #8
                STX     BCNT                	; set max char count to 8
                LDX     #$00                	; reset index to name string buffer
                STX     TERM_CHAR
CHK_DOT         CMP.NE  #DOT GET_CHAR       	; is first char a . char? (. dir?), branch if not
                STA     FILENAME,X+         	; yes, store it
                INY
                LDA     (PSTR),Y            	; get next char from input string
                CMP.NE  #DOT GET_CHAR       	; is second char a . char? (.. dir?), branch if not
SET_CHAR        STA     FILENAME,X          	; yes, store it
NEXT_CHAR       INX                         	; point to next char of parsed name
NEXT_CHAR2      INY                         	; point to next char of input string
GET_CHAR        LDA.EQ  (PSTR),Y  FILL_ALL   	; get next char from input string, branch if end of line.
                CMP.EQ  #SPC      FILL_ALL     	; Terminate if it is a space char
                CMP.EQ  #PATH_SEP SET_TERM_CHAR	; is it a path seperator char? If so, terminate
                CMP.EQ  #DOT      FILL_NAME     ; is it a . char? If so, fill name with spaces
                CMP.EQ  #'*'      FILL_WILDCARD ; is it a * char? If so, fill name with '?'

                CPX.CS  BCNT NEXT_CHAR          ; branch if name length exceeds max length
                JSR     UPPERCASE           	; convert chars to upper case
                CMP.NE  #'?' GET_CHAR2      	; is it a ? char? Branch if not, check next allowed char
                STA     TERM_CHAR           	; yes, store ? as termination char
                BEQ     SET_CHAR            	; and char in name buffer

GET_CHAR2       CMP.EQ  #MINUS SET_CHAR     ; is it a - char? Yes, store char in name buffer
                CMP.EQ  #ULINE SET_CHAR     ; is it a _ char? Yes, store char in name buffer
		CMP.EQ	#'~' SET_CHAR       ; is it a ~ char? Yes, store char in name buffer
		CMP.CC  #'0' PARSE_NAME_END ; is char in range 0..9? Branch if not, show error message
                CMP.CC  #':' SET_CHAR	    ; yes, store char in name buffer
                CMP.CC  #'A' PARSE_NAME_END ; is char in range A..Z? Branch if not, show error message
                CMP.CC  #'[' SET_CHAR	    ; yes, store char in name buffer

FILL_WILDCARD   MVA     #'?' TERM_CHAR      ; store ? as termination char
                JSR     FILL_CHAR           ; fill with ? chars
                BCS     NEXT_CHAR2          ; branch always

FILL_NAME       JSR     SET_SPC_CHAR        ; fill with space chars
                MVA     #11 BCNT            ; set max char count to 11
                BCS     NEXT_CHAR2          ; branch always

SET_TERM_CHAR   STA     TERM_CHAR           ; store termination character
FILL_ALL        MVA     #11 BCNT            ; set max char count to 11
SET_SPC_CHAR    LDA     #SPC                ; set space char as filling char
FILL_CHAR       CPX.CS  BCNT PARSE_NAME_END ; branch if max char count exceeded
                STA     FILENAME,X+         ; no, store char in name buffer
                BCC     FILL_CHAR           ; and repeat

PARSE_NAME_END  LDA     TERM_CHAR
                RTS
                
; **** Compare File Name With Mask *********************************************
; Output: C = 0 - Names not equal; C = 1 - Names equal
; ******************************************************************************
OS_COMP_NAME    LDY     #10                 		; compare all characters (0..10)
COMPARE_CHAR    LDA     FILENAME,Y          		; get char from compare mask
                CMP.NE  (CURR_DIR_ENTRY),Y COMPARE_NEQ	; compare character
                DEY.PL	COMPARE_CHAR        		; branch if more characters to compare
		
COMPARE_EQU     SEC                         		; C=1, all characters are equal
                RTS
COMPARE_NEQ     CLC					; C=0, filename is different
                RTS
                
; **** Delete File *************************************************************
; Input : A - First character of filename
; Output: C = 0 - File is not deleted; C = 1 - Names equal
; ******************************************************************************
OS_FILE_DELETED CMP.NE  #$E5 COMPARE_NEQ       		; is file deleted? Branch if not, C=0 and exit
                LDY     TERM_FLAG           		; termination flag already set?
                BNE     COMPARE_EQU         		; yes, C=1 and exit

                STA     TERM_FLAG           		; no, set counter termination flag
		MVAY	6 CURR_DIR_BLK SEL_DIR_BLK	; SEL_DIR_BLK = CURR_DIR_BLK
                SEC
                RTS
                
SEL_DIR_BLK     .byte $00, $00, $00, $00
SEL_DIR_ENTRY   .byte $00, $00
                
; **** Directory Loop Call Back Functions **************************************
; Input : A - First character of filename
;         X - File Attributes
; Output: C = 1 - File found; C = 0 - File not found
; ******************************************************************************

; **** Directory Exists - Call Back Routine ************************************
CB_DIR_EXISTS   JSR     OS_FILE_DELETED

; **** Find Sub Directory Name - Call Back Routine *****************************
CB_FIND_SUBDIR  CPX.EQ  #$0F CB_FIND_END       	; branch if a long filename entry
                LDA     FILENAME            	; do we search for the . directory?
                CMP.NE  #DOT FIND_SUBDIR	; branch if not . or ..
		
                LDA     FILENAME+1		; get next byte
                CMP.EQ  #SPC FIND_SUBDIR_END   	; Space? Yes, do nothing, dir = '.', just exit.
		
FIND_SUBDIR     TXA
                AND.EQ  #FA_DIRECTORY CB_FIND_END	; is it a directory we are looking for? Branch if not, exit
                JSR     OS_COMP_NAME        		; yes, compare name with search mask
                BCC     CB_FIND_END2        		; branch if C=0: name not equal, get next dir entry, just RTS

		; Emile: two errors here (corrected here and OS_SET_DIR is changed as well):
		; 1) only D_START_CLST ($1A) was used to get dir cluster nr, D_START_CLSTH ($14) also needs to be used 
		; 2) OS_SET_DIR expected 4 consecutive bytes and copied $1A, $1B, $1C & $1D into D_ACTUAL_DIR and CURR_DIR_BLK,
		;    with $1C & $1D not being cluster nr bytes.
                JSR     OS_SET_DIR          	; make this directory the actual directory
FIND_SUBDIR_END SEC                         	; C=1: subdirectory found
                RTS				; return
                
; **** File Exists - Call Back Routine *****************************************
CB_FILE_EXISTS  JSR     OS_FILE_DELETED

; **** Find File Name - Call Back Routine **************************************
CB_FIND_FILE    CPX.EQ  #$0F CB_FIND_END        		; branch if a long filename entry
                TXA
                AND.NE  #FA_DIRECTORY CB_FIND_END 		; is it a directory? Branch if so, get next dir entry
                JSR     OS_COMP_NAME        			; compare name with search mask
                BCC     CB_FIND_END2        			; name not equal, get next dir entry
		
                LDX     #$00					; init. index in CURR_CLUSTER
                LDY     #D_START_CLST       			; get low bytes of start cluster address
GET_FILE_CLST   MVA     (CURR_DIR_ENTRY),Y+  CURR_CLUSTER,X+	; and store it into current cluster
                CPX.CC  #$02 GET_FILE_CLST     			; branch if not first two address bytes read
                LDY     #D_START_CLSTH      			; yes, get high bytes of start cluster address
                CPX.CC  #$04 GET_FILE_CLST     			; branch if not all four bytes read
                RTS                         			; yes, file found

CB_FIND_END     CLC                         			; file not found
CB_FIND_END2    RTS
                
; **** Find Free or Deleted Dir. Entry **************************************
CB_FIND_FREE_DIR_ENTRY
		LDY	#D_FILENAME			; set index to filename
		LDA	(CURR_DIR_ENTRY),Y		; 1st char of filename
		CMP.EQ	#$E5 FND_EMPTY			; Branch if deleted entry found
FFD_LP1		LDA.NE	(CURR_DIR_ENTRY),Y NOT_EMPTY	; branch if dir. entry is in use
		INY
                CPY.CC  #D_ATTRIBUTES FFD_LP1  		; branch if not all characters copied
FND_EMPTY	LDA	#$00				; empty rest of subdir, just to be sure
FFD_LP2		STA	(CURR_DIR_ENTRY),Y+
		CPY.NE	#$20 FFD_LP2			; branch if not all 32 bytes cleared yet
		
FF_EMPTY	SEC					; C=1, dir. entry is free or deleted
                RTS                         		; return
NOT_EMPTY     	CLC                         		; C=0, dir. entry is not empty
		RTS					; return

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
FILENAME        .ds 	12
