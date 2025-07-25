;-------------------------------------------------------------------------------
; This file contains the SHELL portion of the BOOT.SYS file.
; 
; Assembler: MADS-Assembler
; V0.1: 22-05-25, Emile first version
;-------------------------------------------------------------------------------

; ******************************************************************************
; Main Loop Of Command Interpreter *********************************************
; ******************************************************************************
SH_CMD_PROMPT   MWA	#CMD_FOUND CMD_ADDR  ; CMD_ADDR = CMD_FOUND
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
GET_CMD1        TXA                          	; try to find command in command table
		CMP.EQ	CHARS,Y GET_CMD2     	; branch if equal
                LDA.EQ  CHARS,Y SH_CMD_SET_DRV	; get [X,Y] pointer, branch if at end-of-string
                TXA
        :3      INY			     	; Y += 3
                JMP     GET_CMD1	     	; branch always

GET_CMD2        INY
                MVA     CHARS,Y+ STOL        	; store in pointer LSB
                MVA     CHARS,Y  STOH	     	; store in pointer MSB
		MVY	#$00 NEXTINDEX	     	; init. command index
GET_CMD_LOOP1   LDX     NCNT                 	; reset string index to first command char
                LDA.EQ  (STOL),Y SH_CMD_SET_DRV	; branch if command length is 0 (command not found)
                ADD:STA NEXTINDEX		; set index to start of next command
GET_CMD_LOOP2   INX                          	; point to next char in command string
                INY                          	; point to next char in command table
                CPY.EQ  NEXTINDEX GET_CMD3   	; branch if y is pointing to the next command

                LDA     STRBUF,X             	; load char from command string
                JSR     UPPERCASE	     	; and convert it to uppercase
		CMP.EQ	(STOL),Y  GET_CMD_LOOP2	; compare char with char in command table and branch if equal

                LDY     NEXTINDEX            	; else point y to next command
        :2      INY			     	; Y += 2
                STY     NEXTINDEX
                JMP     GET_CMD_LOOP1	     	; branch always

GET_CMD3        MWA	(STOL),Y CMD_ADDR    	; CMD_ADDR = (STOL),Y (STOL),Y+1
                LDA.EQ  STRBUF,X CMD_FOUND	; branch if STRBUF,X contains end-of-string
                CMP.EQ	#SPC     CMD_FOUND	; branch if space found
CMD_NOT_FOUND   CLC
                RTS
                
; Check If Set-Drive Command (A: .. Z:) ****************************************
SH_CMD_SET_DRV  LDX     NCNT                 ; get first command char
                LDA     STRBUF,X
                JSR     UPPERCASE            ; convert it to upper case
                CMP.CC  #'A' CMD_NOT_FOUND   ; branch if char is not between 'A' and 'Z'
                CMP.CS  #'[' CMD_NOT_FOUND

                TAY
                LDA     STRBUF+1,X
                CMP.NE  #':' CMD_NOT_FOUND   ; branch if second char is not a ':'
                SEC
                TYA
                SBC     #'A'                 ; make a drive (0..25) number out of drive letter (A..Z)
                LDXYI   OS_SET_DRIVE         ; set call back function for set drive command
                STXY    CMD_ADDR
CMD_FOUND       SEC
                RTS
                
; **** Return Uppercase Character **********************************************
; Input:  A - Character
; Output: A - Uppercase Character
; ******************************************************************************
UPPERCASE       CMP.CC  #'a'   UPPERCASE_END	; exit if A < 'a'
                CMP.CS  #'z'+1 UPPERCASE_END	; exit if A > 'z'
                AND     #$DF			; make lower-case if 'a'..'z'
UPPERCASE_END   RTS				; return

; **** Print Two Digit Number **************************************************
; Input: A - Number (0..99)
; ******************************************************************************
NUMOUT          JSR     DEC2STR			; Converts into DIG0, DIG1 and DIG2
                LDX     #$01
NEXT_NUMOUT     LDA     DIG0,X			
                JSR     COUT
		DEX.PL	NEXT_NUMOUT		; branch if not done yet
                RTS				; return
                
; **** Print Current Drive *****************************************************
;
; ******************************************************************************
PRINT_DRIVE     LDA     CURR_DRIVE	   ; 0=FDD1, 1=FDD2, @=SD/CF card
		ADD	#'A'		   ; 'A', 'B', 'C', ...
                JSR     COUT		   ; print char
                LDA     #COLON		   ; ':'
                JMP     COUT		   ; print char and return
                
; **** Print Current Path ******************************************************
;
; ******************************************************************************
PRINT_PATH      MVX	#0 SAVEX		; SAVEX = 0
PR_PATH_LP	LDX	SAVEX
		LDA     D_SUBDIR_NAME,X		; 
		PHA
                JSR     COUT		    	; print char
		PLA
		BEQ	PR_PATH_DN		; End-of-String?
		
		INC.NE	SAVEX PR_PATH_LP	; Get next char, branch if not done yet
PR_PATH_DN      RTS				; return

; **** Print Drive and Path Prompt *********************************************
;
; ******************************************************************************
PRINT_PROMPT    JSR     CROUT		   ; CR
                JSR     PRINT_DRIVE	   ; e.g. 'C:'
                JSR     PRINT_PATH	   ; '\'
                LDA     #PROMPT		   ; '>'
                JMP     COUT		   ; print char and return
                
; **** Print Drive Label *******************************************************
;
; ******************************************************************************
PRINT_LABEL     PHA			   ; save A
                PHY			   ; save Y
                PRSTR   MSG_LABEL	   ; 'Volume in drive '
                JSR     PRINT_DRIVE	   ; e.g. 'C'
                PRSTR   MSG_LABEL2	   ; ' is '
                JSR     PRINT_FILENAME1    ; print volume-label
                JSR     CROUT		   ; CR 2x
                JSR     CROUT
		PLY			   ; restore Y
                PLA			   ; restore A
                RTS
                
; **** Print Filename **********************************************************
;
; ******************************************************************************
PRINT_FILENAME  LDX     #DOT					; Check if directory
		AND.EQ	F_ATTRIBS #FA_DIRECTORY PRINT_FILENAME2	; branch if F_ATTRIBS & FA_DIRECTORY = 0  (not a dir.)
PRINT_FILENAME1 LDX     #SPC                			; yes, set divider to ' '
PRINT_FILENAME2 LDY     #$00
PRINT_FILENAME3 CPY.NE  #$08 PRINT_NEXT_CHAR			; branch if not at start of file extension
                JSR     SPCOUT		    		; print space
                TXA
                JSR     COUT                		; yes, print divider char
PRINT_NEXT_CHAR LDA     (CURR_DIR_ENTRY),Y  		; load next character
                JSR     COUT                		; print character (does not affect Y)
                INY
                CPY.NE  #D_ATTRIBUTES PRINT_FILENAME3	; repeat if not all characters printed
                RTS			    		; return
                
; **** Print File Info *********************************************************
; Input: F_ATTRIBS = File Attributes
; ******************************************************************************
PRINT_FILE_INFO AND.EQ	F_ATTRIBS #FA_DIRECTORY PRINT_SIZE	; branch if F_ATTRIBS & FA_DIRECTORY = 0 (not a dir.)
                
; Print Directory Attribute ****************************************************
                PRSTR   MSG_DIR_ENTRY	    ; print <DIR>
		INW	CURR_DIR_CNT	    ; increment total dir count
		JMP     PRINT_ATTRIB

; Print File Size **************************************************************
PRINT_SIZE      JSR     SPCOUT
                JSR     SPCOUT
                INW  	CURR_FILE_CNT       ; increment total file count (word)
NO_FCNT_CARRY   LDX     #$00
                LDY     #D_FILE_SIZE        ; index to file size
                CLC
                PHP
LOAD_SIZE       PLP
                MVA     (CURR_DIR_ENTRY),Y NUM32,X	; load file size into NUM32
                ADC:STA CURR_USED_SIZE,X    		; add file size to total file size
                PHP
                INY
                INX
		CPX.NE	#$04 LOAD_SIZE			; branch if not done yet
                PLP
                JSR     PRINT_INT32         		; print file size
                
; Print File Attributes ********************************************************
PRINT_ATTRIB    JSR     SPCOUT
		BTST	CURR_CMD_PARAM 1 PRINT_DATE	; skip printing attributes is /A param is set
                LDX     #$07
GET_ATTRIB      ASL     F_ATTRIBS           		; move attribute bit into carry
                LDA.EQ  ATTRIB_VAL,X NEXT_ATTRIB	; branch if attribute is not printable
                BCS     SET_ATTRIB			; branch if printable

CLEAR_ATTRIB    LDA     #'-'                		; attribute not set, print -
SET_ATTRIB      JSR     COUT                		; print attribute
NEXT_ATTRIB     DEX.PL	GET_ATTRIB          		; repeat until all attributes printed
                JSR     SPCOUT
                
; Print Date *******************************************************************
; Date Format: 15-09 Years from 1980 (0-127 -> 1980-2107)
;              08-05 Month of year (1-12)
;              04-00 Day of month (1-32)
; ******************************************************************************
PRINT_DATE      LDY     #D_LAST_WR_DATE     		; index to file last write date
                MVA     (CURR_DIR_ENTRY),Y MONTH	; load file creation date low byte
                AND     #$1F                		; mask day value
                STA     DAY
                INY
                LDA     (CURR_DIR_ENTRY),Y  ; load file creation date high byte
                LSR                         ; year in A
                ROR     MONTH
        :4      LSR     MONTH		    ; LSR 4
		ADD	#80		    ; same as CLC + ADC, year correction value (add 1980)
                STA     YEAR
                LDA     DAY
                JSR     NUMOUT
                PRCH    '.'
                LDA     MONTH
                JSR     NUMOUT
                PRCH    '.'
                LDY     YEAR
                CPY.CC  #100 CENTURY_19	    ; branch if < 100

                LDA     #20
                JSR     NUMOUT
                TYA
		SUB	#100		    ; same as SEC + SBC
                JMP     PRINT_YEAR

CENTURY_19      LDA     #19
                JSR     NUMOUT
                TYA
PRINT_YEAR      JSR     NUMOUT
                JSR     SPCOUT
                
; Print Time *******************************************************************
; Time Format: 15-11 Hours (0-23)
;              10-05 Minutes (0-59)
;              04-00 Seconds (0-29), 2-second intervals, so 29 gives 58 seconds.
; ******************************************************************************
PRINT_TIME      LDY     #D_LAST_WR_TIME     		; index to file Last write time
                MVA     (CURR_DIR_ENTRY),Y MINUTE	; load file last write time low byte
                INY
                LDA     (CURR_DIR_ENTRY),Y  		; load file last write time high byte
        .rept 3
		LSR     
                ROR     MINUTE
	.endr	
        :2      LSR     MINUTE
                JSR     NUMOUT
                PRCH     ':'		    ; Print :
                LDA     MINUTE
                JSR     NUMOUT		    ; print minutes

; Print Cluster Number *********************************************************
; Only with /C parameter: Prints Cluster number AND LBA number
; ******************************************************************************
		BTST	CURR_CMD_PARAM 3 PRINT_EXIT		; Skip printing cluster nr if /C parameter is set
                JSR	SPCOUT		    			; Print space
		PRCLW	D_START_CLSTH CURR_DIR_ENTRY NUM32+2	; Print cluster nr (32-bit) and save it in NUM32
		PRCLW	D_START_CLST  CURR_DIR_ENTRY NUM32
		JSR	SPCOUT
		JSR	CLSTR_TO_LBA	   			; Convert NUM32 Cluster nr into NUM32 LBA
		PRHEX32	NUM32			        	; and print as 32-bit hex number
PRINT_EXIT	RTS

YEAR		.byte 	$00
MONTH		.byte 	$00
DAY 		.byte	$00
MINUTE		.byte 	$00
                
; **** Print 16 Bit Number *****************************************************
; Input: X,Y = Int16
; ******************************************************************************
PRINT_INT16     JSR     BIN16_TO_BCD
                JMP     PRINT_NUM
                
; **** Print 32 Bit Number *****************************************************
; Input: NUM32[0..3] = Int32
; Output: C = 0 - Number is 0; C = 1 - Number <> 0
; ******************************************************************************
PRINT_INT32     JSR     BIN32_TO_BCD        ; convert NUM32 into BCD
PRINT_NUM       LDX     #$00
                LDY     #10
                CLC                         ; save status bits
PRINT_NUM1      PHP                         ; store current carry flag
                JSR     PRINT_SEPARATOR
                LDA     BCD_VAL,X           ; load two decimal digits
                PHA                         ; store A
                TYA                         ; move digit counter into A
                LSR                         ; bit one into carry
                PLA                         ; restore A
                BCC     SET_DIGIT1          ; is it a even digit?
		
                INX                         ; no, process digit 2
                AND     #$0F
                BPL     SET_DIGIT2
SET_DIGIT1 :4   LSR                         ; LSR4, shift upper digit of BCD into lower nibble
SET_DIGIT2      PLP
                BCS     PRINT_DIGIT         ; check if we processed at least one digit <> 0
		
                AND.NE  #$0F PRINT_DIGIT    ; branch if not a leading 0, print digit
                TYA
                CMP.NE  #1 PRINT_SPACE      ; is it the last digit? Branch if not, just print space char
                LDA     #48                 ; yes, print 0
                JSR     HEXDIG
                CLC                         ; number is 0
                RTS
		
PRINT_SPACE     JSR     SPCOUT              ; print space
                CLC
                BCC     NEXT_DIGIT	    ; branch always
		
PRINT_DIGIT     JSR     HEXDIG              ; print single digit
                SEC                         ; no more leading 0s
NEXT_DIGIT      DEY.NE	PRINT_NUM1          ; repeat if more digits
                RTS
                
; Print Thousands Separator ****************************************************
PRINT_SEPARATOR PHP                         ; save status bits
                CPY.EQ  #$09 PRINT_SEP      ; branch if at 9th digit, print separator
                CPY.EQ  #$06 PRINT_SEP      ; branch if at 6th digit, print separator
                CPY.NE  #$03 NO_SEP         ; branch if not at 3rd digit, exit
		
PRINT_SEP       PLP                         ; restore status bits
                BCC     PRINT_SPC           ; leading zero, just print a space char
		
                LDA     #NUM_SEP
                JMP     COUT                ; print thousands seperator
		
PRINT_SPC       JMP     SPCOUT
NO_SEP          PLP                         ; clean up stack
                RTS
                
;**** Convert BCD Number To 8 Bit Binary ***************************************
; INPUT:  A = BCD Number
; Output: A = Binary Number
; ******************************************************************************
BCD_TO_BIN      STA     NUM32               ; save BCD number
                AND     #$F0                ; and clear ones digit in A
                LSR                         ; calc tens digit * 8
                STA     NUM32+1             ; and store result
        :2      LSR     		    ; calc tens digit * 2
		ADD:STA	NUM32+1		    ; add it with tens digit * 8 and store result
                LDA     NUM32               ; reload BCD number int A
                AND     #$0F                ; and clear tens digit in A
                ADC     NUM32+1             ; finally add both result
                RTS			    ; return
                
;**** Convert 16 Bit Binary Number To BCD **************************************
; INPUT:  Int[X:Y]      = 16 Bit Binary
; OUTPUT: BCD_VAL[4..0] = Result
; ******************************************************************************
BIN16_TO_BCD    STXY	NUM32		    ; Store [X:Y] in lower word
		MWX	#$00 NUM32+2	    ; clear upper word
                
;**** Convert 32 Bit Binary Number To BCD **************************************
; INPUT:  NUM32[0..3]   = 32 Bit Binary
; OUTPUT: BCD_VAL[4..0] = Result
;*******************************************************************************
BIN32_TO_BCD    SED                         ; set decimal mode
                LDX     #$04
                LDA     #$00                ; clear BCD result value
CLEAR_BCD       STA     BCD_VAL,X-
                BPL     CLEAR_BCD

                LDX     #$20                ; 32 source bits
CONV_BITS       ASL32	NUM32		    ; shift MSB of NUM32 into carry flag
		LDA:ADC:STA BCD_VAL+4	    ; and shift carry back into BCD result
		LDA:ADC:STA BCD_VAL+3	    ; by adding BCD_VAL = BCD_VAL + BCD_VAL + C
		LDA:ADC:STA BCD_VAL+2	    ; ...
		LDA:ADC:STA BCD_VAL+1	    ; ...
		LDA:ADC:STA BCD_VAL	    ; uses more code, but faster than looping
                DEX.NE	CONV_BITS           ; repeat until all 32 bits done
                CLD                         ; reset to binary mode
                RTS

; **** Get Command String ******************************************************
;
; ******************************************************************************
GET_CMD_STR     LDX     #$01
GET_CMD_CHAR    LDA.EQ  STRBUF,X END_PARAM	; get char, branch if end-of-string
                CMP.NE	#SPC     END_PARAM	; branch if not a space
SKIP_SPC_CHAR   INX
                JMP     GET_CMD_CHAR
                
; **** Get Parameter String ****************************************************
; Output: C = 1: Parameter; C = 0: Path
;         A    = $00 : End of parameter string
;         NCNT = Index to parameter
; ******************************************************************************
GET_NEXT_PARAM  LDX     NCNT                ; get actual index into command line
                JSR     GET_PARM_CHAR
		CMP.NE	#OPT_SEP PATH_STR   ; branch if not a '/'
                INX			    ; next char
                LDA     STRBUF,X	    ; load parameter
                JMP     PARAM_STR	    ; OK and return

GET_PARM_CHAR   LDA.EQ  STRBUF,X PATH_STR   	; get char, branch if end-of-string
		CMP.EQ	#SPC     SKIP_SPC_CHAR	; branch if a space
		CMP.EQ	#OPT_SEP PARAM_STR	; branch if a '/'
                INX			    
                JMP     GET_PARM_CHAR

PATH_STR        CLC			    ; C=0, it is a path
                BCC     END_PARAM	    ; branch always

PARAM_STR       SEC			    ; C=1, it is a parameter
END_PARAM       STX     NCNT		    ; save index in parameter string
                PHA
                PLA
                RTS			    ; return

; **** Internal Command Handlers ***********************************************

; **** Directory Loop Call Back Functions **************************************
; Input : A - First character of filename
;         X - File Attributes
; Output: C = 0 - Continue print loop, C = 1 - Break print loop
; ******************************************************************************

; **** Print Directory Entry - Call Back Routine *******************************
CB_PRINT_DIR    CPX.EQ  #$0F CB_PRINT_CONT		; skip to next entry if long filename entry
		CMP.EQ  #$E5 CB_PRINT_CONT  		; branch (skip to next entry) if entry is deleted
                STX     F_ATTRIBS           		; save attributes
                TXA
                LSR     
CHK_HIDDEN      LSR                         		; check if hidden file
                BCC     CHK_SYSTEM	    		; branch if not a hidden file

                BTST	CURR_CMD_PARAM 2 CB_PRINT_CONT	; Skip line counting if dir /H (display hidden files) is set
CHK_SYSTEM      LSR                         		; check if system file
CHK_LABEL       LSR                         		; check if disk label
                ; #### PRINT LABEL DISABLED ####################################
                BCS     CB_PRINT_CONT       		; it's a label, skip to next entry

                ;BCC     PRINT_DIR_ENTRY
                ;JSR     PRINT_LABEL         		; print disk label
                ;INC     LINE_CNT
                ;BNE     CHK_LINE_COUNT     		 ; branch always
                ; ##############################################################
PRINT_DIR_ENTRY LDA.NE  TERM_CHAR COMP_MASK		; check the termination char, if >0 then just compare file names
                JSR     CB_FIND_SUBDIR      		; TC = 0, so check if directory entry
                BCC     CB_PRINT_CONT       		; name is a file entry or includes wildcard chars, just exit

                MVA     #PATH_SEP TERM_CHAR 		; TERM_CHAR = name compared equal with a directory entry
                JSR     OS_DIR_LOOP         		; list files of sub directory
                SEC                         		; directory list finished
                RTS                         		; exit

COMP_MASK       JSR     SH_COMP_MASK
                BCC     CB_PRINT_END

PRINT_ITEM      MVA     #1 PSAV		    		; set file found flag
                JSR     PRINT_FILENAME      		; print filename
                JSR     PRINT_FILE_INFO     		; print file size, date and time
                JSR     CROUT
                BTST    CURR_CMD_PARAM 0 CB_PRINT_CONT	; Skip line counting if dir /P parameter is set
CHK_LINE_COUNT  INC     LINE_CNT
                LDA     #25
		CMP.CS	LINE_CNT CB_PRINT_CONT		; reached one screen page? Branch if not, just exit
                MVA     #$00 LINE_CNT       		; reset line counter
                JSR     SH_PAUSE            		; wait for key press
                CMP.NE	#27 CB_PRINT_CONT   		; ESC pressed? Branch if not
                SEC                         		; break dir loop
                RTS
CB_PRINT_CONT   CLC                         		; get next next entry
CB_PRINT_END    RTS

; **** Compare File Name With Mask Using Wildcards *****************************
;
; ******************************************************************************
SH_COMP_MASK    CMP.EQ	#PATH_SEP COMP_NAME_EQU			; Termination char = path separator? Branch if it is, skip compare
                LDY     #10                 			; compare all characters
COMP_NAME_CHAR  LDA     FILENAME,Y          			; get char from compare mask
		CMP.EQ	#'?' SKIP_NAME_CHAR			; Skip comparing with a '?' char
		CMP.NE	(CURR_DIR_ENTRY),Y COMP_NAME_NEQ	; Compare char, if not equal, exit with C=0
		
SKIP_NAME_CHAR  DEY.PL	COMP_NAME_CHAR      			; more character to compare
COMP_NAME_EQU   SEC                         			; all characters are equal, set Carry = 1
                RTS
COMP_NAME_NEQ   CLC
                RTS

; ******************************************************************************
; ******************************************************************************
SAVED_ACT_DIR   .byte      $00, $00, $00, $00
SAVED_DIR_BLK   .byte      $00, $00, $00, $00
; + DRIVE
                
SAVE_ACT_DIR    LDY     #$03		   		; Save D_ACTUAL_DIR and CURR_DIR_BLK
SAVE_DIR_ADDR   MVA	D_ACTUAL_DIR,Y SAVED_ACT_DIR,Y	; SAVED_ACT_DIR = D_ACTUAL_DIR
		MVA	CURR_DIR_BLK,Y SAVED_DIR_BLK,Y-	; SAVED_DIR_BLK = CURR_DIR_BLK
                BPL     SAVE_DIR_ADDR			; branch if not finished
                RTS					; return
                
LOAD_ACT_DIR    LDY     #$03
LOAD_DIR_ADDR   MVA	SAVED_ACT_DIR,Y D_ACTUAL_DIR,Y	; D_ACTUAL_DIR = SAVED_ACTUAL_DIR
		MVA	SAVED_DIR_BLK,Y CURR_DIR_BLK,Y-	; CURR_DIR_BLK = SAVED_DIR_BLK
                BPL     LOAD_DIR_ADDR			; branch if not finished
                RTS					; return
                
; **** Get Parameters from Command Line ****************************************
; Input:  Ptr[X:Y] to Parameter String
; Output: C = 0 - Error; C = 1 - No Error
; ******************************************************************************
SH_GET_PARMS    JSR     SET_PARM_MASK	    ; set pointer to parameter mask
                LDA     #$00                ; set current command parameter value to 0
GET_PARM        STA     CURR_CMD_PARAM
GET_PARM2       MVY     #$01 MASK           ; reset bit mask to 00000001
                DEY                         ; param mask pointer is set to 0
                JSR     GET_NEXT_PARAM      ; find next command parameter in command line
                BEQ     GET_PARMS_END       ; reached end of command line
                BCS     PARSE_PARM          ; C=1: parameter found? branch if parameter
		
                JSR     SH_GET_PATH         ; no, parse path parameter
                BCS     GET_PARM2           ; more parameters

                RTS			    ; return
		
PARSE_PARM      JSR     UPPERCASE	    ; convert to uppercase
                STA     F_ATTRIBS           ; no, save current parameter as F_ATTRIBS
COMP_PARM       LDA     (STOL),Y            ; load a char from param mask (STOL = Ptr to param mask)
                CMP.EQ	#SPC PARM_ERR	    ; is it a ' '? Branch if all allowed param chars are compared -> unknown param
		CMP.NE  F_ATTRIBS NEXT_MASK ; Compare actual parameter char with param mask. Branch if not equal, get next char from param mask
                LDA     MASK                ; load bit mask
                ORA     CURR_CMD_PARAM      ; and set actual parameter bit
                JMP     GET_PARM            ; get next parameter from command line

NEXT_MASK       INY                         ; point to next char in param mask
                ASL     MASK                ; shift bit mask to next position
                BNE     COMP_PARM	    ; branch if more to do

PARM_ERR        JSR     CROUT		    ; print CR
                LDXYI   MSG_PARAM_ERR       ; load error message
                JSR     OS_PRINT_ERR        ; and print it
                LDA     F_ATTRIBS
                JSR     COUT                ; print unknown parameter char
                JSR     CROUT		    ; print CR
                CLC			    ; C=0: error
                RTS
GET_PARMS_END   SEC			    ; C=1: OK
                RTS
                
; ******************************************************************************
SH_GET_PATH     LDX     NCNT                ; get pointer path string into X:Y
                LDY     #> STRBUF
                JSR     OS_PARSE_PATH
                BCS     GET_PATH_END
SH_ERROR        BNE     PARSE_ERR1

                JMP     SH_NAME_ERR

PARSE_ERR1      CMP.NE  #PATH_SEP PARSE_ERR2
                JMP     SH_DIR_ERR

PARSE_ERR2      CMP.NE  #$FF GET_PATH_ERR
                JMP     SH_PATH_ERR
GET_PATH_ERR    CLC
GET_PATH_END    RTS

; **** Set Pointer To Parameter Mask *******************************************
; Input: Ptr[X:Y] = Pointer to Parameter Mask
; ******************************************************************************
SET_PARM_MASK   STXY	STOL			; save pointer to command param mask
		MVA	#0 CURR_CMD_PARAM	; set current command parameter value to 0
		MVA	#PATH_SEP TERM_CHAR	; TERM_CHAR = PATH_SEP
                RTS

; Allowed Directory Options ****************************************************
DIR_PARMS       .by    'PAHC '               	; param mask for DIR command
NO_PARMS        .by    ' '

; **** DIR Command *************************************************************
; Prints a directory. The following optional parameters can be used:
; /P : 
; /A : Show attributes
; /H : Show hidden files
; /C : Show cluster number and LBA number
; ******************************************************************************
SH_DIR          JSR     CROUT			; print CR
                JSR     SAVE_ACT_DIR        	; save actual-dir cluster nr
                LDXYI   DIR_PARMS
                JSR     SH_GET_PARMS		; Get parameters from command-line
                BCS     SH_DIR_START		; branch if parameter found

                JMP     LOAD_ACT_DIR        	; restore actual directory LBA

SH_DIR_START    LDX     #$00
                TXA
                STA     LINE_CNT            	; reset line counter
                STA     PSAV                	; reset file found flag
CLEAR_CNT       STA     CURR_FILE_CNT,X+     	; clear file count, dir count and byte count
                CPX.NE  #$08 CLEAR_CNT		; branch if not 8 bytes cleared yet
                LDXYI   CB_PRINT_DIR  		; print call-back routine
                JSR     OS_FIND_ALL         	; find and print directory entries
                JSR     LOAD_ACT_DIR        	; restore actual directory LBA
                LDA.NE  PSAV PRINT_RESULT      	; Branch if any files found, print dir. result
                JMP     SH_FILE_ERR         	; no, print error

PRINT_RESULT    JSR     CROUT			; Print CR
        :2      INC     LINE_CNT		; LINE_CNT += 2
                JSR     CHK_LINE_COUNT		; if LINE_CNT > 25 then SH_PAUSE
                
; Print Total File Count
                LDXY	CURR_FILE_CNT		; [X,Y] = Current nr of files
                JSR     PRINT_INT16		; print as word
                PRSTR   MSG_FILE_COUNT		; print 'file(s) '

; Print Total Used Bytes In Directory
                LDXYI   CURR_USED_SIZE		; Total filesize
                JSR     LOAD_32			; NUM32 = total filesize
                JSR     PRINT_INT32		; print as int32
                PRSTR   MSG_BYTE_USED		; print ' bytes'

; Print Total Directory Count
		LDXY	CURR_DIR_CNT		; nr of directories
                JSR     PRINT_INT16		; Print as word
                PRSTR   MSG_DIR_COUNT		; print ' dir(s)'
		MVAX	4 FREE_KB NUM32		; NUM32 = #Free KB
		JSR	PRINT_INT32		; Print as decimal number
		PRSTR	TXT_KB			; Print ' KB free'
		RTS				; return
		
; **** Create Directory (MKDIR) Command ****************************************
;
; ******************************************************************************
SH_MKDIR        JSR     SAVE_ACT_DIR        	; save actual directory LBA
                LDXYI   NO_PARMS          	; we don't need parameters, evtl. /H hidden /S system
                JSR     SH_GET_PARMS        	; get path
                BCC     SH_MKDIR_END	    	; branch if no dirname was given

                LDA     #FA_DIRECTORY		; create directory
                JSR     OS_CREATE           	; create directory
                BCS     SH_MKDIR_END        	; if no errors, clean up and exit

MKDIR_ERR       CMP.EQ  #$FF DIR_EXISTS_ERR    	; if error code = -1 then dir already exists

                JSR     SH_WRITE_ERR        	; it was a write error
                BCC     SH_MKDIR_END        	; branch always

DIR_EXISTS_ERR  JSR     SH_D_EXIST_ERR		; Print 'Dir already exists'
SH_MKDIR_END    JMP     LOAD_ACT_DIR        	; restore actual directory LBA and return

; **** Check if Current Dir. ***************************************************
; Check if directory entered is current dir (..).
; Output: C=0: not a current dir, C=1: is current dir.
; ******************************************************************************
IS_FNAME_CURDIR	LDY	#0
		LDA	FILENAME,Y
		CMP.NE	#'.' NOT_FNAME
		INY
		LDA.NE	FILENAME,Y NOT_FNAME	; Load filename char, branch if not a current dir (.)
		BEQ	IS_FNAME		; branch always if it is a current dir (.)

; **** Check if Parent Dir. ************************************************
; Check if current directory is parent dir (..).
; Output: C=0: not a parent dir, C=1: is parent dir.
; ******************************************************************************
IS_FNAME_PARENT	LDY	#0
		LDA	FILENAME,Y
		CMP.NE	#'.' NOT_FNAME
		INY
		LDA	FILENAME,Y
		CMP.NE	#'.' NOT_FNAME
		INY
		LDA.NE	FILENAME,Y NOT_FNAME	; get filename char, branch if not end-of-string

IS_FNAME	SEC				; C=1: is parent (..) directory
		RTS
NOT_FNAME	CLC				; The filename is not . or ..
		RTS
		
; **** Convert filename to FN83 type filename **********************************
; INPUT: ssptr_l/ssptr_h pointer to filename
; OUTPUT: converted filename in FN83
; ******************************************************************************
FNAME2FN83	LDY	#0
CPFN83_1	LDA	(ssptr_l),Y		; BASIC pointer to filename
		CMP.EQ	#'.' FN83_DOTFND	; branch if a dot is found
		
CPFNCNT		INY
		CPY.NE	#D_ATTRIBUTES CPFN83_1	; branch if not at end-of-filename yet
		BEQ	EXT_DN			; branch always if no dot was found

FN83_DOTFND	; Found a dot
		LDX	#8
		INY				; points to char next to '.'
CPFN83_2	LDA	(ssptr_l),Y		; Get char of extension
		JSR	UPPERCASE		; Convert to upper-case (only affects A)
		STA.EQ	FILENAME,X EXT_DN	; Store in extension, branch if '\0' (done)
		INY
		INX
		CPX.NE	#D_ATTRIBUTES CPFN83_2	; branch if not done with extension
		
		MVA	#0 FILENAME,X		; Add '\0' to filename
EXT_DN		LDY	#0
CPFN83_3	LDA	(ssptr_l),Y		; Get char of Filename
		JSR	UPPERCASE		; Convert to upper-case (only affects A)
		STA	FILENAME,Y		; Store in result
		CMP.EQ	#'.' CPFN83_4		; branch if filename copied
		INY
		CPY.NE	#8 CPFN83_3		; branch if not all chars copied
		RTS				; return
		
CPFN83_4	MVA	#' ' FILENAME,Y+	; fill remainder with spaces
		CPY.NE	#8   CPFN83_4		; branch if not all chars copied
CPFN_DN		RTS				; return
		
; **** CFC_LOAD routine for CF-IDE driver **************************************
; Called with a CMD_LOAD from the CFC Device-driver through a JMP CFC_LOAD_VEC.
; ******************************************************************************
CFC_LOAD	STXY	FNAME_PTR
		PRSTR	TXT_LOAD		; Print 'CFC_LOAD'
		JSR	FNAME2FN83		; Convert filename to FN83 filename
		PRCH	'['
		LDXYI	FILENAME		; 
		JSR	OS_STRING_OUT		; Print FN83 filename
		PRCH	']'
		JSR     SAVE_ACT_DIR        	; save actual directory LBA
		JSR	SH_LOAD_BAS		; Load .bas file in memory
		SEC				; C=1: OK
		RTS				; return

TXT_LOAD	.by	'CFC_LOAD: ' $00
FNAME_PTR	.word 	$0000

; **** CFC_SAVE routine for CF-IDE driver **************************************
; Called with a CMD_SAVE from the CFC Device-driver through a JMP CFC_SAVE_VEC.
; ******************************************************************************
CFC_SAVE	STXY	FNAME_PTR
		PRSTR	TXT_SAVE		; Print 'CFC_SAVE'
		PRHEX16	$2000			; Print end-address
		JSR	SPCOUT		
		JSR	FNAME2FN83		; Convert filename to FN83 filename
		PRCH	'['
		LDXYI	FILENAME		; 
		JSR	OS_STRING_OUT		; Print FN83 filename
		PRCH	']'
		SBW	$2000 #$2000		; Get net file-size
		LSR	$2001			; $2000 now contains #sectors of 512 bytes needed
		LDA	D_SECT_PER_CLST		; Convert to #clusters
DIV_SPC		LSR	$2001			; $2000 = $2000 / D_SECT_PER_CLST
		LSR
		BNE	DIV_SPC			; branch if not done with shifting
		
		LDA.EQ	$2000 NO_ADD_SEC
		INC 	$2001			; add 1 if last part of file is not empty
NO_ADD_SEC	PRSTR	TXT_CLND		; Print 'Clusters needed: '
		LDA	$2001			; Get cluster count
		JSR	NUMOUT			; Print #clusters needed
		JSR	CROUT
		SEC				; C=1: OK
		RTS				; return

TXT_SAVE	.by	'CFC_SAVE: $' $00
TXT_CLND	.by	', clusters needed: ' $00

; **** Add subdir name to D_SUBDIR_NAME ****************************************
; Check if directory entered is current dir (..).
; Output: C=0: not a current dir, C=1: is current dir.
; ******************************************************************************
ADD_SUBDIR_NAME	LDY	#0
FIND_EOS	LDA.EQ	D_SUBDIR_NAME,Y SUBDIR_EOS	; get char of subdir name, branch if end-of-string
		INY
		BNE	FIND_EOS			; branch always
		RTS

SUBDIR_EOS	CPY.EQ	#1 CP_INIT		; branch if root-dir
		MVA	#BSLASH D_SUBDIR_NAME,Y	; add '\' to subdir name
		INY
CP_INIT		LDX	#0
CP_FNAME	MVA	FILENAME,X D_SUBDIR_NAME,Y
		BEQ	SUBDIR_X		; exit if $00 found
		
		INY				; index in D_SUBDIR_NAME
		INX				; index in FILENAME
		CPX.NE	#D_ATTRIBUTES CP_FNAME	; branch if not at max filename yet
SUBDIR_X	RTS				; return if done
		
; **** Del subdir name from D_SUBDIR_NAME **************************************
; ******************************************************************************
DEL_SUBDIR_NAME	LDY	#0
FIND_EOS2	LDA.EQ	D_SUBDIR_NAME,Y SUBDIR_EOS2	; get char of subdir name, branch if end-of-string
		INY
		BNE	FIND_EOS2			; branch always
DEL_SUBDIR_X	RTS

SUBDIR_EOS2	DEY				; goto last char before $00
SUBDIR_LP1	LDA	D_SUBDIR_NAME,Y		; get char from D_SUBDIR_NAME
		CMP.EQ	#BSLASH BSLASH_FND	; branch if filename separator found
		DEY.NE	SUBDIR_LP1		; branch always

BSLASH_FND	CPY.EQ	#0 DSNM_X		; branch if root-dir
		MVA	#0 D_SUBDIR_NAME,Y	; replace '\' with '\0' in D_SUBDIR_NAME
DSNM_X		RTS				; and return

; **** Change Directory Command ************************************************
;
; ******************************************************************************
SH_CD           JSR     SAVE_ACT_DIR        	; save actual directory LBA
                LDXYI   NO_PARMS
                JSR     SH_GET_PARMS		; get subdir name
                BCC     SH_CD_END		; C=0: no name entered

                LDA.NE  TERM_CHAR SH_CD_END	; branch if termination char > 0
                JSR     OS_FIND_PATH		; loop through dir to find path
                BCC     SH_CD_ERR		; branch if subdir name was not found
		
		LDY	#0			; add '\0' to string
TERM_FNAME_LP	LDA	FILENAME,Y		; find end-of-string first
		CMP.EQ	#' ' TERM_FNAME		; branch if a space found
		INY
		CPY.NE	#D_ATTRIBUTES TERM_FNAME_LP	; branch if not at max. len of filename
		
TERM_FNAME	MVA	#$00 FILENAME,Y		; terminate string with \0
		JSR	IS_FNAME_PARENT		; is dir .. ?
		BCC	TST_CURDIR		; branch if not . or .. entered
		
		JSR	DEL_SUBDIR_NAME		; remove last subdir from D_SUBDIR_NAME
SH_CD_EXIT_OK	SEC				; C=1: OK
SH_CD_END	RTS

TST_CURDIR	JSR	IS_FNAME_CURDIR		; is dir . ?
		BCS	SH_CD_EXIT_OK		; branch if dir is . (current dir)
		
		JSR	ADD_SUBDIR_NAME		; Normal subdir name, add to D_SUBDIR_NAME
		JMP	SH_CD_EXIT_OK		; branch always
		
SH_CD_ERR       JSR     LOAD_ACT_DIR        	; error - restore actual directory LBA
                JSR     CROUT			; print CR
                JMP     SH_PATH_ERR		; Print 'Path not found'

; **** Release the FAT clusters for a file  ************************************
; Algorithm:
;       SCNT = 1;
; loop: CURR_CLUSTER = FAT[CURR_CLUSTER];
;       FAT[CURR_CLUSTER] = 0L; // 0L = free entry
;       if (CURR_CLUSTER == 0FFFFFFF) SCNT++; goto loop;
; ******************************************************************************
CLR_FAT32_FILE 	MVX 	#0 SCNT				; #clusters cleared
CLR_FAT32_LP1	LDXYI   D_START_FAT1      		; load base block address of FAT into NUM32[0:3]
                JSR     LOAD_32		    		; NUM32 = LBA nr. of FAT
		LDXYI	(CURR_CLUSTER+1)			; SUM32 = CURR_CLUSTER into SUM32
		JSR     LOAD_S32            		; load CURR_CLUSTER[1:3] into SUM[0:2] = FAT block index
		JSR	PRTST1				; DEBUG
		MVX	#$00 SUM32+3			; clear garbage byte SUM[3]: SUM32 = CURR_CLUSTER / 256
                LDY     #$03
                LDA     CURR_CLUSTER        		; load CURR_CLUSTER[0] = FAT entry index byte
                ASL                         		; shift bit 7 into carry flag and multiply entry index by 2
                PHA                         		; save entry index to stack
CLR32_LP        ROL     SUM32,X+             		; shift bit 7 of entry index into bit 0 of block index
                DEY.NE  CLR32_LP	    		; branch if not done yet
		
		; A FAT entry is 4 bytes and there are 128 FAT entries in one FAT-sector
                JSR     ADD_32_32	    		; NUM32 = START_FAT1 + CURR_CLUSTER / 128
                JSR     LOAD_FAT_BLK	    		; Load FAT sector into standard buffer ($600)
                PLA                         		; restore entry index
                LDX     #$04                		; four bytes to read for a FAT32 entry
                ASL                         		; multiply entry index by 2 (4 in total now)
                TAY                         		; store entry index into Y
		JSR	HEXOUT				; DEBUG
                MVX	#4 NCNT				; 1 FAT entry = 4 bytes
		LDX	#0				; init. CURR_CLUSTER index
LP_FAT_ENTRY    JSR     READ_ENTRY_BYTE     		; read entry byte
		STA     TEMP_CLUSTER,X      		; store byte in TEMP_CLUSTER to follow link
		JSR	WR_ENTRY_BYTE			; FAT[CURR_CLUSTER] = 0
		INY					; Increment FAT index counter
                INX					; Increment CURR_CLUSTER counter
                DEC.NE  NCNT LP_FAT_ENTRY   		; loop until all bytes copied
		MVAX	4 TEMP_CLUSTER CURR_CLUSTER	; CURR_CLUSTER = TEMP_CLUSTER
		INC	SCNT				; #clusters cleared + 1
		CPD	#$0FFFFFFF CURR_CLUSTER		; CURR_CLUSTER == $0FFFFFFF ?
		BNE	CLR_FAT32_LP1			; branch if file has more clusters to clear
		
		LDXYI	CURR_FAT_BLK			; LBA of current FAT block
		JMP	DEV_WR_LBLK_BUF			; write FAT block back to disk and return

TEMP_CLUSTER	.dword	$00000000
		
PRTST1		PRCH	'<'
		PRHEX32	CURR_CLUSTER
		PRCH	'>'
		RTS

; **** Write a Single FAT Entry Byte From Block Buffer *************************
; INPUT : Y - Index To FAT Entry Byte
; OUTPUT: A = Read Byte from FAT table
; ******************************************************************************
WR_ENTRY_BYTE 	AND.NE  #$01 CURR_CLUSTER+1 CLR_UPPER_PAGE	; check bit 0 (= bit 7 of CURR_CLUSTER[0:3] because of ASL in FAT32 routine)
                MVA	#0 BLOCK_BUFF,Y				; write entry byte from lower half of block buffer
                RTS			    			; return
CLR_UPPER_PAGE  MVA     #0 BLOCK_BUFF+256,Y    			; write entry byte from upper half of block buffer
                RTS			    			; return

; **** Delete Command **********************************************************
;
; ******************************************************************************
SH_DEL          JSR     SAVE_ACT_DIR        			; save actual directory LBA
                LDXYI   NO_PARMS          			; we don't need parameters, just the filename
                JSR     SH_GET_PARMS        			; get path
                BCC     SH_DEL_X	    			; branch if no filename was given

                JSR     OS_FIND_FILE				; Now find file to delete
                BCC     SH_DEL_ERR				; branch if file not found

		; OS_FIND_FILE did already set CURR_CLUSTER to the file starting-cluster
		LDY	#D_FILENAME
		MVA	#$E5 (CURR_DIR_ENTRY),Y			; $E5 first char. is a deleted file
		LDY	#D_START_CLSTH
		MWA	#$00 (CURR_DIR_ENTRY),Y			; delete high word of file-size
		PRHEX16	CURR_DIR_BLK
		LDXYI	CURR_DIR_BLK				; CURR_DIR_BLK is LBA of current dir block
                JSR     OS_SAVE_DIR	    			; write this dir entry back to disk
		
		JSR	CLR_FAT32_FILE				; Set all FAT entries for this file to 00000000 (free)
		JSR	INIT_FREE_CLUSTER			; FREE_CLUSTER = 2L
		JSR     OS_NEXT_FREE_CLUSTER			; Get first free cluster in FREE_CLUSTER

		; Update SIS with #clusters freed and first-free cluster nr
		JSR	WRITE_SIS				; Update SIS and write back
SH_DEL_X        JMP     LOAD_ACT_DIR        			; error - restore actual directory LBA and return

SH_DEL_ERR	JMP	SH_FILE_ERR				; Print 'File not found' and return

; **** Clear Screen Command ****************************************************
;
; ******************************************************************************
SH_CLS          JMP     CLRSCRN

; **** Pause Command ***********************************************************
; Output: A - pressed key char
;         C = 0 ESC key pressed, C = 1 else
; ******************************************************************************
SH_PAUSE        PRSTR   MSG_PAUSE          	; print pause message
                JSR     CIN                  	; wait until any key pressed
                PHA
                JSR     CROUT
                PLA
                RTS
                
; **** Echo Command ************************************************************
;
; ******************************************************************************
SH_ECHO         LDA.EQ  STRBUF,X SH_ECHO_END	; get char, branch if end-of-string
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
SH_IF           JSR 	INIT_FREE_CLUSTER	; FREE_CLUSTER = 2L
                JSR 	OS_NEXT_FREE_CLUSTER
		LDY 	#3
PRINT_CURR_CLST PHY				; save Y
		LDA 	FREE_CLUSTER,Y		; 
                JSR 	HEXOUT			; Print FREE_CLUSTER
		PLY				; restore Y
		DEY.PL	PRINT_CURR_CLST		; print if not done yet
		RTS

; **** Rem Command *************************************************************
;
; ******************************************************************************
SH_REM          PHW	BLKBUF			; DEBUG: For testing SIS routines
		JSR	GET_SIS
		PLW	BLKBUF
SH_REM_X	RTS
                
; **** BASIC Command ***********************************************************
; Executes Basic in ROM. Return with 'DOS' command. 
; ******************************************************************************
SH_BASIC        JSR	SWITCH_TO_ROM		; Make sure BASIC ROM is enabled
		LDA	Wrmjph	    	    	; Is BASIC Warm-start vector already set?
		CMP.NE	#$B1 SH_BCOLD 	    	; If not in this range, branch and do a BASIC cold start
		
		JMP	(Wrmjpl)	    	; Basic Warm-start
SH_BCOLD	JMP	LAB_COLD	    	; Basic Cold-start

; **** BRUN Command ************************************************************
;
; ******************************************************************************
SH_BRUN         JSR     SAVE_ACT_DIR        	; save actual directory LBA
                LDXYI   NO_PARMS            	; no command parameters
                JSR     SH_GET_PARMS
                BCC     SH_BRUN_END

                JSR     OS_FIND_FILE
                BCC     SH_BRUN_END		; branch if file not found

                ;JSR     OS_LOAD_BIN
SH_BRUN_END     JSR     LOAD_ACT_DIR        	; restore actual directory LBA
                RTS
                
; **** BLOAD Command ***********************************************************
;
; ******************************************************************************
SH_BLOAD        RTS

; ******************************************************************************
SH_RUN          JSR     SAVE_ACT_DIR        ; save actual directory LBA
                LDXYI   NO_PARMS            ; no command parameters
                JSR     SET_PARM_MASK
                JSR     SH_GET_PATH         ; get file path
                BCC     SH_RUN_END

		; This is the entry-point for loading a .BAS file into memory
SH_LOAD_BAS     LDA     FILENAME+8
                CMP.NE  #SPC SH_RUN1        	; branch if given filename has no extension
		MVAY	3 EXT_COM FILENAME+8	; copy .COM extension to FILENAME

		; Check for .COM file
SH_RUN1         LDY     #$02
CMP_EXT_COM     LDA     EXT_COM,Y           	; check if COM file
		CMP.NE	FILENAME+8,Y CHK_BASF	; Not a .COM file, check .BAS file next
                DEY.PL	CMP_EXT_COM	    	; branch if not done yet
                
		MVA	#1 FTYPE	    	; 1 = .COM file
		BNE	SH_RUN_FF	    	; branch always, check if file exists

CHK_BASF	LDY     #$02
CMP_EXT_BAS     LDA     EXT_BAS,Y           	; check if .BAS file
                CMP.NE  FILENAME+8,Y CHK_EXEF	; Not a .BAS file, check .EXE file next
                DEY.PL  CMP_EXT_BAS	    	; branch if not done yet
                
		MVA	#0 FTYPE	    	; 0 = .BAS file
		BEQ	SH_RUN_FF	    	; branch always, check if file exists

CHK_EXEF	LDY     #$02
CMP_EXT_EXE     LDA     EXT_EXE,Y           	; check if .EXE file
                CMP.NE  FILENAME+8,Y SH_RUN_END	; all 3 extensions do not exist
                DEY.PL	CMP_EXT_EXE	    	; branch if not done yet
                
		MVA	#2 FTYPE	    	; 2 = .EXE file
SH_RUN_FF       JSR     OS_FIND_FILE        	; check if file with this extension exists
                BCS     SH_RUN3             	; yes, load file

                JSR     SH_SET_SYS_DIR      	; no, search in system directory
                BCC     SH_RUN_ERR          	; system directory does not exist

SH_RUN2         JSR     OS_FIND_FILE	    	; find file with this extension in root-dir
                BCS     SH_RUN3		    	; branch if found

SH_RUN_ERR      JSR     CROUT		    	; print CR
                JSR     SH_FILE_ERR         	; file does not exist
                BCC     SH_RUN_END	    	; branch always

SH_RUN3         JSR     OS_LOAD_FILE	    	; Load .bas file or load/run .com/.exe file
SH_RUN_END      JMP     LOAD_ACT_DIR        	; restore actual directory LBA and return
                
; **** Set System Directory ****************************************************
; Output: C = 0 - Error
; ******************************************************************************
SH_SET_SYS_DIR  LDX     #10
SAVE_NAME       MVA	FILENAME,X   NAME_SAVE,X	; NAME_SAVE = FILENAME
		MVA	SYSTEM_DIR,X FILENAME,X-	; FILENAME = SYSTEM_DIR
                BPL     SAVE_NAME			; branch if not done

                JSR     OS_SET_ROOT_DIR
		LDXYI	CB_FIND_SUBDIR
                JSR     OS_FIND             		; find subdirectory
                BCC     SH_SYS_DIR_END

                MVAX	11 NAME_SAVE FILENAME		; FILENAME = NAME_SAVE
                SEC
SH_SYS_DIR_END  RTS

; **** Monitor call-back Routine ************************************************
SH_MONITOR      PRSTR   MSG_MONITOR
                JMP     MON_WARM_START
                
.macro	PR_ERR	msg
		LDXYI	:msg
		JMP	OS_PRINT_ERR
.endm
; **** Error Routines **********************************************************
SH_FILE_ERR     PR_ERR	MSG_FILE_ERR		; 'File not Found' error message
SH_PATH_ERR     PR_ERR  MSG_PATH_ERR  		; 'Path not Found' error message
SH_DIR_ERR      PR_ERR  MSG_DIR_ERR  		; 'Invalid Directory' error message
SH_NAME_ERR     PR_ERR  MSG_NAME_ERR  		; 'Invalid Filename' error message...
SH_D_EXIST_ERR  PR_ERR  MSG_D_EXIST_ERR 	; 'Directory already exists' error message...
SH_WRITE_ERR    PR_ERR  MSG_WRITE_ERR 		; 'Write error' message
                
; Inits BLKBUF to SIS_BUFF **************************************
; Prepare for DEV_RD_LBLK and DEV_WR_LBLK routines
; ***************************************************************
INIT_SIS_BUF	MWA	#SIS_BUFF BLKBUF	; macro BLKBUF = SIS_BUF
		RTS

;-------------------------------------------------------------------------------
; Convert #Clusters to KB in FREE_KB
; D_SECT_PER_CLST = 1: 2 CL =  2 SEC = 1 KB: SHR 1
;                   2: 2 CL =  4 SEC = 2 KB: -
;		    4: 2 CL =  8 SEC = 4 KB: SHL 1
;		    8: 2 CL = 16 SEC = 8 KB: SHL 2 etcetera
;-------------------------------------------------------------------------------
CL2KB		LDA	D_SECT_PER_CLST			; #sectors per cluster
		CMP.EQ	#2 CL2KB_X			; 2 sec/cl, just exit
		CMP.EQ	#1 CL2KB_1			; 1 sec/cl, SHR 1
	:2	LSR					; init nr of shifts		
CL2KB_SHL	ASL32	FREE_KB				; SHL 1 of FREE_KB
		LSR
		BNE	CL2KB_SHL			; branch if not done with shifting
		RTS					; return
		
CL2KB_1		LSR32	FREE_KB				; SHR 1 of FREE_KB
CL2KB_X		RTS					; return
		
; Get Info from System Information Sector **************************************
GET_SIS		JSR	INIT_SIS_BUF			; Init SIS Buffer for CMD_READ command
		LDXYI	D_PART_START			; macro Ptr(X,Y) = D_PART_START
		JSR 	DEV_RD_LBLK           		; Read Volume ID again
		MVAX	4 D_PART_START SYS_INFO_LBA	; SYS_INFO_LBA = D_PART_START
		ADW	SYS_INFO_LBA SIS_OFFSET		; SYS_INFO_LBA = D_PART_START + *SIS_OFFSET
		LDA	SYS_INFO_LBA+2			; update high word of SYS_INFO_LBA
		ADC	#0
		STA	SYS_INFO_LBA+2
		LDA	SYS_INFO_LBA+3
		ADC	#0
		STA	SYS_INFO_LBA+3
		
		JSR	INIT_SIS_BUF			; Init SIS Buffer for CMD_READ command
		LDXYI	SYS_INFO_LBA			; Read Sys. Info. Sector into SIS-buffer
		JSR 	DEV_RD_LBLK           		; Read SIS sector
		PRSTR	TXT_FFREE_CLST			; print 'First Free Cluster:$'
		PRHEX32	SIS_BUFF+$01EC
		JSR	CROUT
FREE_KB_UPDATE	MVAX	4 SIS_BUFF+$01E8 FREE_KB	
		JSR	CL2KB				; Convert #clusters to KB and store in FREE_KB
		RTS
		
SYS_INFO_LBA	.dword	$00000000
FREE_KB		.dword	$00000000
TXT_FFREE_CLST	.by	'First free cluster:$' $00
TXT_KB		.by	' KB free' CR $00

; Write Info back to System Information Sector **************************************
WRITE_SIS	ADB	SIS_BUFF+$01E8 SCNT		; add SCNT to #free clusters in SIS-buffer
		SCC					; 'skip if C is clear' macro
		INC	SIS_BUFF+$01E9
		SCC	
		INC	SIS_BUFF+$01EA
		SCC	
		INC	SIS_BUFF+$01EB
		MVAX	4 FREE_CLUSTER SIS_BUFF+$01EC	; SIS First free cluster = FREE_CLUSTER
		JSR	INIT_SIS_BUF			; Init SIS Buffer for CMD_WRITE command
		LDXYI	SYS_INFO_LBA 			; Sys. Info. Sector LBA
		JSR 	DEV_WR_LBLK           		; Write SIS to disk
		JMP	FREE_KB_UPDATE			; Update FREE_KB and return

; **** Data Area ***************************************************************
; ******************************************************************************

; String Data Area *************************************************************
MSG_PAUSE       .by    'Press any key...' $00
MSG_LABEL       .by    'Volume in drive ' $00
MSG_LABEL2      .by    ' is ' $00
MSG_DIR_ENTRY   .by    '          <DIR>' $00
MSG_FILE_COUNT  .by    ' File(s)  ' $00
MSG_DIR_COUNT   .by    ' Dir(s)   ' $00
MSG_BYTE_USED   .by    ' bytes' CR $00
MSG_DRIVE_ERR   .by    'Drive not found' CR $00
MSG_FILE_ERR    .by    'File not found' CR $00
MSG_PATH_ERR    .by    'Path not found' CR $00
MSG_PARAM_ERR   .by    'Unknown option ' OPT_SEP $00
MSG_NAME_ERR    .by    'Invalid filename' CR $00
MSG_DIR_ERR     .by    'Invalid directory' CR $00
MSG_D_EXIST_ERR .by    CR 'Directory already exists' CR $00
MSG_MONITOR     .by    CR 'Hex Monitor' CR $00
MSG_WRITE_ERR   .by    CR 'Write Error' CR $00

; ############################################
ATTRIB_VAL      .byte      82, 72, 83, 0, 0, 65, 0, 0
BCD_VAL         .byte      $00, $00, $00, $00, $00
SYSTEM_DIR      .by    'SYSTEM     '
NAME_SAVE       .by    '...........'
EXT_COM         .by    'COM'
EXT_EXE		.by    'EXE'
EXT_BAS		.by    'BAS'
                
; Command Table ****************************************************************
CHARS		dta	'B' , a(CMD_BASIC)		; byte, word
		dta	'C' , a(CMD_CD)		
		dta	'D' , a(CMD_DIR)		
		dta	'E' , a(CMD_ECHO)		
		dta	'G' , a(CMD_GOTO)		
		dta	'I' , a(CMD_IF)		
		dta	'M' , a(CMD_MKDIR)		
		dta	'P' , a(CMD_PAUSE)		
		dta	'R' , a(CMD_REM)		
		.byte 	$00

CMD_BASIC	dta	5, c'ASIC', a(SH_BASIC)		; byte, string, word, EOT
CMD_BLOAD	dta	5, c'LOAD', a(SH_BLOAD)		; 
CMD_BRUN	dta	4, c'RUN' , a(SH_BRUN)   , $00	; 
CMD_CD		dta	2, c'D'   , a(SH_CD) 		; 
CMD_CLS		dta	3, c'LS'  , a(SH_CLS)    , $00	; 
CMD_DIR		dta	3, c'IR'  , a(SH_DIR) 		; 
CMD_DEL		dta	3, c'EL'  , a(SH_DEL)    , $00	; 
CMD_ECHO	dta	4, c'CHO' , a(SH_ECHO)   , $00	; 
CMD_GOTO	dta	4, c'OTO' , a(SH_GOTO)   , $00	; 
CMD_IF		dta	2, c'F'   , a(SH_IF)     , $00	; 
CMD_MKDIR	dta	5, c'KDIR', a(SH_MKDIR)		; 
CMD_MON		dta	3, c'ON'  , a(SH_MONITOR), $00	; 
CMD_PAUSE	dta	5, c'AUSE', a(SH_PAUSE)  , $00	; 
CMD_REM		dta	3, c'EM'  , a(SH_REM)    , $00	; 
