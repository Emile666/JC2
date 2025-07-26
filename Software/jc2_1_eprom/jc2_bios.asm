; ******************************************************************************
; Junior Computer ][ BIOS Version 1.2.0 by Joerg Walke
;
; first implementation 28.12.2021
; updated 16.11.2024 by Joerg Walke
;
; Assembled With A65
;
; 20.03.2023 A bug in the disassembler code was fixed by the German Classic 
; Computing forum user jet2bue. See version history
;
; 01.08.2023 A bug in SD_WR_BLK was fixed. The bug was found by German Classic 
; Computing forum user Dietrich Lausberg. See version history
;
; 29.04.2024 SPI & IRQ optimization by Dietrich Lausberg
;
; 26.05.24 Changes in Fast SPI by Dietrich Lausberg
;
; 21.04.24 Integration with CF-IDE drivers into 1 32K eprom
; ******************************************************************************

MON_COLD_START	JMP  	MAINSTART	; jump to monitor cold start
MON_WARM_START	JMP	MONINP		; jump to monitor warm start

; **** Switch BASIC To RAM Page (B000..DFFF) ***********************************
; ******************************************************************************
SWITCH_TO_RAM	JSR	BAS2RAM		; Set MMU bit 7 to 0, enable BASIC RAM
		RTS			; return

; **** Switch BASIC To ROM Page (B000..DFFF) ***********************************
; ******************************************************************************
SWITCH_TO_ROM	JSR	BAS2ROM		; Set MMU bit 7 to 1, enable BASIC ROM
SWITCH		NOP			; maintain compatibility with v1.1.4
		RTS

; **** Set Standard In/Out Routine ID ******************************************
; Input: A - ID Of Standard IO Device
; ******************************************************************************
SET_STDIOID	JSR	DEV_OPEN

; **** Set Standard In/Out Routine *********************************************
; Input: X - Low Byte Of Standard Device Descriptor
;	 Y - High Byte Of Standard Device Descriptor
; ******************************************************************************
SET_STDIO	JSR	SET_STDIN
		JMP	SET_STDOUT0

; **** Set Standard Out Routine ID *********************************************
; Input: A - ID Of Standard Output Device
; ******************************************************************************
SET_STDOUTID	JSR	DEV_OPEN

; **** Set Standard Out Routine ************************************************
; Input: X - Low Byte Of Standard Out Device Descriptor
;	 Y - High Byte Of Standard Out Device Descriptor
; ******************************************************************************
SET_STDOUT	STX	PDEVL
		STY	PDEVH
SET_STDOUT0	LDY	#$04
		LDX	#$00
SET_STDOUT1	LDA	(PDEV),Y
		STA	STDOUT,X
		INY
		INX
		CPX	#$04
		BNE	SET_STDOUT1
		RTS

; **** Set Standard In Routine ID **********************************************
; Input: A - ID Of Standard Input Device
; ******************************************************************************
SET_STDINID	JSR	DEV_OPEN

; **** Set Standard In Routine *************************************************
; Input: X - Low Byte Of Standard In Device Descriptor
;	 Y - High Byte Of Standard In Device Descriptor
; ******************************************************************************
SET_STDIN	STX	PDEVL
		STY	PDEVH
		LDY	#$02
		LDA	(PDEV),Y
		STA	STDIN
		INY
		LDA	(PDEV),Y
		STA	STDIN+1
         	RTS

; **** Write Binary Routine ****************************************************
; Input: A - Output Byte to Standard Out
; ******************************************************************************
BOUT		JMP	(STDOUT)

; **** Read Character Routine **************************************************
; Output: A - character read from standard in
; ******************************************************************************
CIN		JSR	CGET		; call standard in. Character available?
		BCC	CIN		; no, repeat
		RTS

; **** Get Character (no wait) Routine *****************************************
; Output: A - character read from standard in
;         C - 1 char get, 0 no char get
; ******************************************************************************
CGET            JMP     CHAR_GET

; **** Write LF Character Routine **********************************************
; ******************************************************************************
LFOUT		LDA  	#LF        	; write a LF
					; fall through to COUT

; **** Write Character Routine *************************************************
; Input: A - character to write
; ******************************************************************************
COUT 		JSR     BOUT
                CMP  	#CR        	; character was a CR?
		BEQ  	LFOUT      	; yes, also write LF
		RTS

; **** Write CR/LF To Terminal *************************************************
; ******************************************************************************
CROUT		LDA	#CR		; write a CR
		BNE	COUT

; **** Write Single Space Char To Terminal *************************************
; ******************************************************************************
SPCOUT		LDA	#$20		; write a Space char
		BNE	BOUT

; **** Read String Routine *****************************************************
; Output:  (PSTRL, PSTRH) - pointer to CR terminated string data
; ******************************************************************************
STRIN           LDX  	#$02		; initialize character index
BACKSPACE       DEX
		BEQ  	STRIN		; if line empty, restart
NEXTCHR         JSR  	CIN        	; get next character from input buffer
		STA  	STRBUF,X   	; store character in string buffer
		CMP  	#CR		; is it a CR?
                BEQ  	ENDSTRIN	; yes, exit
                JSR  	COUT		; echo character
		CMP  	#BS        	; backspace key?
		BEQ  	BACKSPACE  	; yes
                INX             	; advance string index
                BNE  	NEXTCHR    	; more then 255 characters? No, read next char
                LDA  	#CR        	; yes, auto new line and stop reading
		STA  	STRBUF,X   	; store CR in string buffer
ENDSTRIN        JMP  	COUT       	; send CR

; **** Write String Routine ****************************************************
; Input:  (PSTRL, PSTRH) - pointer to null terminated string data
; ******************************************************************************
STROUT		LDY  	#$00       	; index y is 0

; **** Write String From Index Routine *****************************************
; Input:  (PSTRL, PSTRH) - pointer to null terminated string data
;	  Y              - start index into string data
; ******************************************************************************
WRSTR		LDA  	(PSTR),Y   	; load char at string pos y
		BEQ  	ENDSTROUT  	; exit, if NULL char
		JSR  	COUT       	; write character
		INY             	; next index
		JMP  	WRSTR
ENDSTROUT	RTS

; **** Print A Byte In Hexadecimal *********************************************
; Input: A - data byte to print in hex
; ******************************************************************************
HEXOUT          PHA             	; save A for lower hex digit
                LSR  	
                LSR  	
                LSR  	
                LSR  	
                JSR  	HEXDIG		; write upper hex digit
                PLA             	; write lower hex digit
					; fall through to HEXDIG

; **** Print A Single Hexadecimal Digit ****************************************
; Input: A - data nibble in Bit 0-3 to print in hex
; ******************************************************************************
HEXDIG          AND     #$0F    	; mask lower digit
                ORA     #'0'    	; add 48
                CMP     #'9'+1  	; decimal digit?
                BCC     PRHEX   	; yes, print it
                ADC     #6      	; add offset for letter digit
PRHEX		JMP     BOUT

; **** Print A Byte As Decimal Number ******************************************
; Input: A - number 00..FF (0..255)
; ******************************************************************************
NUMOUT		JSR	DEC2STR
		LDX	#2
NEXTNUMOUT	LDA	DIG0,X
		JSR	BOUT
		DEX
		BPL	NEXTNUMOUT
		RTS

; **** Clear Screen Routine ****************************************************
; ******************************************************************************
CLRSCRN		LDA	#CMD_CLRSCRN

; **** Call Standard Print Command Routine *************************************
; Input : A - command byte
;         X - command data byte low
;         Y - command data byte high
; ******************************************************************************
CMDPRINT	JMP	(STDCMD)

; **** Call Opened Device Command Routine **************************************
; Input : A - command byte
;         X - command data byte low
;         Y - command data byte high
; ******************************************************************************
CMDDEV		JMP	(DEVCMD)

; **** Convert a Byte To Decimal String ****************************************
; Input:  A - number 00..FF (0..255)
; Output; DIG0 (10^0), DIG1 (10^1), DIG2 (10^2)
; ******************************************************************************
DEC2STR		LDX	#48
		STX	DIG0		; initialize digit counter 0 to '0'
		STX	DIG1		; initialize digit counter 1 to '0'
		STX	DIG2		; initialize digit counter 2 to '0'
GETDIG2		CMP	#100		; is A >= 100?
		BCC	GETDIG1		; no, convert next digit
		SBC	#100		; yes, subract 100 from A
		INC	DIG2		; and increment digit counter 2
		BNE	GETDIG2		; branch always
GETDIG1		CMP	#10		; is A >= 10?
		BCC	GETDIG0		; no, convert next digit
		SBC	#10		; yes, subract 10 from A
		INC	DIG1		; and increment digit counter 1
		BNE	GETDIG1		; branch always
GETDIG0		ADC	DIG0		; add digit counter 0 to remainder in A
		STA	DIG0		; and store it back to digit counter 0
		RTS

; **** Print Tab Routine *******************************************************
; Input: A - number of space characters to print
; ******************************************************************************
TAB		STA  	TEMP
		LDA  	#SPC		; load SPC char
PRINTTAB	JSR  	COUT		; write SPC
		DEC   	TEMP
                BNE   	PRINTTAB   	; all spaces written? No, repeat
		RTS

; **** Read Hex Number From Input String ***************************************
; Input:  Y - current input string position to read from
; Output: (NUML, NUMH) - last 8 digits of read hex number
; ******************************************************************************
HEXINPUT	LDX   	#$00
		STX   	NUML       	; clear input value
		STX   	NUMH
NEXTDIGIT       LDA   	STRBUF,Y   	; get next input char
		CMP   	#'0'
		BCC   	NOTHEX     	; char < '0'? Yes, no hex digit
		CMP   	#':'
		BCC   	NUMDIGIT   	; char is in '0'..'9'
		AND   	#$DF	 	; uppercase chars only
HEXDIGIT	CMP   	#'A'
		BCC   	NOTHEX     	; char < 'A'? Yes, no hex digit
		CMP   	#'G'
		BCS   	NOTHEX     	; char > 'F'? Yes, no hex digit
		SBC   	#'A'-11	 	; char 'A'..'F' to value 10..16
NUMDIGIT        ASL   	
		ASL   	
		ASL   	
		ASL   			; digit shifted to upper nibble
		LDX   	#$04      	; load shift loop counter
SHIFT		ASL   			; shift msb in C
		ROL   	NUML
		ROL   	NUMH
		DEX
		BNE   	SHIFT	 	; 4 times shifted? No, repeat
		INY		 	; increment string index
		BNE   	NEXTDIGIT	; branch always
NOTHEX		RTS

; **** Read String From Input Buffer *******************************************
; Input:  Y - current input string position to read from
; Output: C = 1, string found C = 0, string not found
;         PSTRL = low byte of string pointer
;	  PSTRH = high byte of string pointer
; ******************************************************************************
STRINPUT	CLC
NEXTSTRCHAR	LDA  	STRBUF,Y   	; get next input char
		INY
		CMP  	#' '
		BEQ  	NEXTSTRCHAR 	; ignore spaces
		CMP  	#CR
		BEQ  	ENDSTRING 	; end of input line, no filename found
		CMP	#'"'
		BNE	ENDSTRING
		LDX	#$00
		SEC
READSTRING	LDA  	STRBUF,Y   	; get next input char
		CMP	#CR
		BEQ	ENDSTRING
		CMP	#'"'
		BEQ	ENDSTRING
		STA	RBUFF,X		; char to buffer
		INY
		INX
		BNE	READSTRING	; read next char of filename
ENDSTRING	LDA	#$00
		STA	RBUFF,X		; terminate filename string with NULL

; **** Set String Pointer To Read Buffer ***************************************
; Output: X - low byte of string pointer
;	  Y - high byte of string pointer
; ******************************************************************************

SETSTRBUFF	LDX	#< RBUFF	; set string pointer to filename buffer
		LDY	#> RBUFF
SETSTRBUFF0	STX	PSTRL
		STY	PSTRH
		RTS

; **** Delay Routine ***********************************************************
; Input: A - milliseconds to wait
; ******************************************************************************
DELAY           STA  	CNTD
		BNE	LOOPDELAY

; **** Short Delay Routine *****************************************************
; Input: A - microseconds to wait
; ******************************************************************************
SHORTDELAY	TAY
SHORTDELAY1	STY  	CNTA		; set counter
LOOPDELAY	BIT  	CNTIRQ		; check if counter reached 0
		BPL  	LOOPDELAY	; no, check again
		RTS

; **** Check ESC Routine *******************************************************
; Output: C = 1 ESC pressed, 0 ESC not pressed
; Beep if ESC pressed
; ******************************************************************************
CHKESC		JSR     CGET            ; key pressed?
		BCC	NOTESC		; no
		CMP     #ESC		; ESC pressed?
		BEQ	BEEP		; yes, exit and beep.
		CLC			; no, clear carry flag
NOTESC		RTS

; **** System Beep Routine *****************************************************
; ******************************************************************************
BEEP		JMP	(STDBEEP)	; call standard BEEP routine

; **** Simple Beep Routine *****************************************************
; ******************************************************************************
DOBEEP		LDA	PBDD		; save port b data direction register
		PHA
		LDX     #$60		; repeat 60 times
		LDA     #$21
		TAY
		STA     PBDD		; turn speaker on
BEEPLOOP	STY     PBD		; set PB0 high
		LDA     #$01
		JSR     DELAY		; delay of ~1ms
		DEY
		STY	PBD		; set PB0 low
		LDA	#$01
		JSR	DELAY		; delay of ~1ms
		LDY	#$21
		DEX
		BNE	BEEPLOOP	; not finished, repeat
		PLA
		STA	PBDD		; restore port b data direction register
		SEC
		RTS

; ******************************************************************************
; REAL TIME CLOCK ROUTINES ALIASES
; ******************************************************************************

; **** Print Date And Time *****************************************************

; ******************************************************************************

PRINT_DATETIME	JMP	PRINTDATETIME

; **** Print Time *************************************************************

; ******************************************************************************

PRINT_TIME	JMP	PRINTTIME

; **** Print Date **************************************************************

; ******************************************************************************

PRINT_DATE	JMP	PRINTDATE

; **** Print Date Including Day Of Week ****************************************

; ******************************************************************************

PRINT_FULLDATE	JMP	PRINTFULLDATE

; **** Set Date And Time *******************************************************

; ******************************************************************************

SET_DATETIME	JMP	SETDATETIME

; **** Set Time ****************************************************************

; ******************************************************************************

SET_TIME	JMP	SETTIME

; **** Set Date ****************************************************************

; ******************************************************************************

SET_DATE	JMP	SETDATE

; **** Add Storage Device ******************************************************

; Input:   X - device descriptor pointer low
;          Y - device descriptor pointer high
; Output - C = 1 Success, C = 0 Error
;          A = Device ID (0F = Too Many Devices, FF = Unknown Device Type)

; ******************************************************************************

ADD_DEVICE	JMP	DEV_ADD

; **** Open Device For Read/Write **********************************************

; Input:  A - device id
; Output: C = 1 Success, C = 0 Error
;         X - device descriptor pointer low
;         Y - device descriptor pointer high

; ******************************************************************************

OPEN_DEVICE	JMP	DEV_OPEN

; **** Reset Standard I/O To First Screen Device *******************************

; ******************************************************************************

RESET_STDIO	LDA	STDINDEV	; open base In device
		JSR	SET_STDINID	; and set it as standard input
		LDA	STDOUTDEV	; open base Out device
		JMP	SET_STDOUTID	; and set it as standard output

; **** Read Joystick Port ******************************************************

; Output: A - button state (Bit 0 = Button 1, Bit 1 = Button 2, Bit 2 = Button 3)
;         X - vertical joystick position 0 = Center, -1 ($FF) = Left, 1 = Right
;         Y - horizontal joystick position 0 = Center, -1 ($FF) = Up, 1 = Down
;         C = 0 - No joystick port available; C = 1 - Joystickport available

; ******************************************************************************

READ_JOYSTICK   JMP     READ_JOY_PORT

; **** Decode Joystick Data ****************************************************

; Output: A - button state (Bit 0 = Button 1, Bit 1 = Button 2, Bit 2 = Button 3)
;         X - horizontal joystick position 0 = Center, -1 ($FF) = Left, 1 = Right
;         Y - vertical joystick position 0 = Center, -1 ($FF) = Up, 1 = Down

; ******************************************************************************

DECODE_JOYSTICK JMP     DECODE_JOY_PORT

; **** Mute All Sound Chip Channels ********************************************

; ******************************************************************************

SOUND_MUTE_ALL  JMP     SOUND_MUTEALL

; **** Mute A Sound Chip Channel ***********************************************

; Input: A - Channel # (0..3)

; ******************************************************************************

SOUND_MUTE_CHAN JMP     SOUND_MUTE

; **** Set Attenuation For A Sound Chip Channel ********************************

; Input: A - Channel # (0..3)
; 	 X - Attenuation Level 0..15 (0dB, 2dB, 4dB ... OFF)

; ******************************************************************************

SOUND_SET_ATN   JMP     SOUND_SETATN

; **** Set Periodic Noise ******************************************************

; Input: X - Noise Shift Rate

; ******************************************************************************

SOUND_P_NOISE   JMP     SOUND_PNOISE

; **** Set White Noise *********************************************************

; Input: X - Noise Shift Rate

; ******************************************************************************

SOUND_W_NOISE   JMP     SOUND_WNOISE

; **** Set Noise ***************************************************************

; Input: A - 0 = Periodic Noise  1 = White Noise
;	 X - Noise Shift Rate

; ******************************************************************************

SOUND_SET_NOISE JMP     SOUND_SETNOISE

; **** Set Sound Frequency in HZ ***********************************************

; Input: A - Channel (0..2)
;	 X - Frequency Low Bits 7..0
;	 Y - Frequency High Bits 9..8

; ******************************************************************************

SOUND_SET_FREQ  JMP     SOUND_SETFREQ

; ******************************************************************************
; INTERNAL
; ******************************************************************************

; **** Extended Read-Character Handler *****************************************

CHAR_GET        JSR     READ_STD_IN
                BCC     NO_CHAR_GET
                JMP     (KEY_HANDLER)
NO_CHAR_GET     JMP     (NKEY_HANDLER)
READ_STD_IN     JMP	(STDIN)

; ******************************************************************************
; TTY DEVICE DRIVER
; ******************************************************************************

; **** Terminal Command Routine ************************************************

; Input : A - command byte
;         X - command data byte low
;         Y - command data byte high

; ******************************************************************************

TTY_CMD		CMP	#9
		BCS	END_TTY_CMD
		STY	YSAV
		ASL	
		TAY
		LDA	TTY_CMD_TABLE,Y
		STA	PSTRL
		LDA	TTY_CMD_TABLE+1,Y
		STA	PSTRH
		LDY	YSAV
		JMP     (PSTR)
END_TTY_CMD	RTS

TTY_CMD_TABLE	.word	TTY_INIT,TTY_IDENTIFY,TTY_NORMAL,TTY_INVERSE,TTY_FLASH
		.word	TTY_HOME,TTY_CLRLINE,TTY_CLRSCRN,TTY_SETCURSOR

; **** Initialize TTY Device ***************************************************

; ******************************************************************************

TTY_INIT	LDA  	#$00
		STA  	BAUDRATE   	; initialize baud rate variable
		LDA  	#$0B       	; set ACIA to
         	STA  	COMM_REG	; no parity, no receiver echo, RTS low, no IRQ, DTR low
		LDX  	#$19		; start with 1 stop bit, 8 data bits, 2400 bps as the current baud rate
NEXTBAUD	STX  	CTRL_REG	; set the baud rate
		JSR  	TTY_IDENTIFY	; send identify string to terminal
		LDA  	#40
		JSR  	DELAY		; wait for ~64ms
		LDA  	STAT_REG
		AND  	#$08		; ACIA input register full?
		BEQ  	NOESC	 	; no, go on
		LDA  	DATA_REG    	; read data register from ACIA
		CMP  	#ESC	 	; is it a ESC char
		BNE  	NOESC	 	; no, go on
		STX  	BAUDRATE    	; and store it
		LDX  	#$1F	 	; detection finished
NOESC           INX  			; try next baud rate
		CPX  	#$20
		BCC  	NEXTBAUD   	; tried all baud rates?
		RTS

; **** Identify TTY Device *****************************************************

; ******************************************************************************

TTY_IDENTIFY    LDY  	#ESCGID-STRINGP	; load sequence index
		BNE  	PRINTESC   	; jump always

; **** Set Normal Text *********************************************************

; ******************************************************************************

TTY_NORMAL      LDY  	#ESCNORM-STRINGP; load sequence index
		BNE  	PRINTESC   	; jump always

; **** Set Inverse Text ********************************************************

; ******************************************************************************

TTY_INVERSE     LDY  	#ESCINV-STRINGP ; load sequence index
		BNE  	PRINTESC   	; jump always

; **** Set Blinking Text *******************************************************

; ******************************************************************************

TTY_FLASH       LDY  	#ESCBLNK-STRINGP; load sequence index
		BNE  	PRINTESC   	; jump always

; **** Set Cursor To Home Position *********************************************

; ******************************************************************************

TTY_HOME        LDY  	#ESCHOME-STRINGP; load sequence index
		BNE  	PRINTESC   	; jump always

; **** Clear Line **************************************************************

; ******************************************************************************

TTY_CLRLINE     LDA	#$0D
		JSR	BOUT
		LDY  	#ESCCLL-STRINGP	; load sequence index
		BNE  	PRINTESC   	; jump always

; **** Clear Screen And Set Cursor To Home Position ****************************

; ******************************************************************************

TTY_CLRSCRN     LDY  	#ESCCLS-STRINGP	; load sequence index
					; fall through to PRINTESC

; **** VT100 ESC Sequence Loader ***********************************************

; ******************************************************************************

PRINTESC        JSR	TTY_ESCCODE
		JSR	LOADSTRING
		JMP  	WRSTR

; **** VT100 ESC Start Code ****************************************************

; ******************************************************************************

TTY_ESCCODE	LDA	#$1B
		JSR	BOUT
		LDA	#'['
		JMP	BOUT

; **** Set Cursor Location *****************************************************

; Input: X - x position of cursor.  Y - y position of cursor

; ******************************************************************************

TTY_SETCURSOR	TXA
		PHA
		JSR	TTY_ESCCODE
		TYA
		JSR	NUMOUT
		LDA	#';'
		JSR	BOUT
		PLA
		JSR	NUMOUT
		LDA	#'H'
		JMP	BOUT

; ******************************************************************************
; LOW LEVEL REAL TIME CLOCK CODE
; ******************************************************************************

; **** Set Day Of Week *********************************************************

; Input: A - Day Of Week 1 (MON) - 7 (SUN)

; ******************************************************************************

WRITEDOW	STA	ACC
		LDA	#$03
		JSR	SETRTCADR
		LDA	ACC
		JSR	I2C_SEND	; set day of week
		JMP	I2C_STOP

; **** Get Day Of Week *********************************************************

; Output: A - Day Of Week 1 (MON) - 7 (SUN)

; ******************************************************************************

READDOW		LDA	#$03
		JSR	READCLOCK
		TYA
		RTS

; **** Write Time **************************************************************

; Input: A - HOUR 	in BCD ($00-$23)
;	 X - MINUTE 	in BCD ($00-$59)
;	 Y - SECOND	in BCD ($00-$59)

; ******************************************************************************

WRITETIME	STA	ACC
		STX	XREG
		STY	YREG
WRITETIME2	LDA	#$00		; start at register 0
		JSR	WRITECLOCK	; write time bytes to clock registers
		LDA	#$08		; set address pointer to ram
		JSR	SETRTCADR
		LDA	#$65		; time set mark
		JSR	I2C_SEND
		JMP	I2C_STOP

; **** Write Date **************************************************************

; Input: A - YEAR 	in BCD ($00-$99)
;	 X - MONTH 	in BCD ($01-$12)
;	 Y - DAY	in BCD ($01-$31)

; ******************************************************************************

WRITEDATE	STA	ACC
		STX	XREG
		STY	YREG
WRITEDATE2	LDA	#$04		; start at register 4
		JSR	WRITECLOCK	; write date bytes to clock register
		LDA	#$09		; set address pointer to ram
		JSR	SETRTCADR
		LDA	#$02		; date set mark
		JSR	I2C_SEND
		JMP	I2C_STOP

; **** Write Data To Clock *****************************************************

; ******************************************************************************

WRITECLOCK	JSR	SETRTCADR
		LDA	YREG
		JSR	I2C_SEND	; set second or day
		LDA	XREG
		JSR	I2C_SEND	; set minute or month
		LDA	ACC
		JSR	I2C_SEND	; set hour or year
		JMP	I2C_STOP

; **** Read Time ***************************************************************

; Output: A - HOUR 	in BCD ($00-$23)
;	  X - MINUTE 	in BCD ($00-$59)
;	  Y - SECOND	in BCD ($00-$59)

; ******************************************************************************

READTIME	LDA	#$00
		BEQ	READCLOCK

; **** Read Date ***************************************************************

; Output: A - YEAR 	in BCD ($00-$99)
; 	  X - MONTH 	in BCD ($01-$12)
; 	  Y - DAY	in BCD ($01-$31)


; ******************************************************************************

READDATE	LDA	#$04
		BNE	READCLOCK

; **** Read Data From Clock ****************************************************

; ******************************************************************************

READCLOCK	JSR	SETRTCADR	; set read pointer
		JSR	I2C_START	; send start condition
		LDA	#I2C_RTC_ADR	; the I2C address
		JSR	I2C_READ_DEV	; send device id and set read mode
		JSR	I2C_RCV		; receive first data byte
		STA	YREG		; and store it
		JSR	I2C_ACK		; send acknowlege
		JSR	I2C_RCV		; receive second data byte
		STA	XREG		; and store it
		JSR	I2C_ACK		; send acknowlege
		JSR	I2C_RCV		; receive third data byte
		STA	ACC		; and store it
		JSR	I2C_NACK	; no more data
		JSR	I2C_STOP	; stop communication
		LDA	ACC		; load third data byte into A
		LDX	XREG		; load second data byte into X
		LDY	YREG		; load first data byte into Y
		RTS

; **** Set RTC Address Read/Write Pointer **************************************

; Input: A - Register Address

; ******************************************************************************

SETRTCADR	PHA			; save register address onto stack
		JSR	I2C_START	; send start condition
		LDA	#I2C_RTC_ADR	; the I2C device address
		JSR	I2C_WRITE_DEV	; send device address and write bit
		PLA			; restore register address
		JMP	I2C_SEND	; send register address

		; 25-07-25 Emile: WRITE_VIA and READ_VIA removed
		ORG	*+6		; Maintain compatibility with v1.1.4
		
; ******************************************************************************
; START OF I2C CODE
; ******************************************************************************

; **** Send I2C Start Condition ************************************************

; ******************************************************************************

I2C_START	LDY	#DDRB
		LDA	#%01011110	; SDA = 1; SCL = 1
		STA	(IOBASE),Y
		LDA	#%11011110	; SDA = 0; SCL = 1
		STA	(IOBASE),Y
		LDA	#%11011111	; SDA = 0; SCL = 0
		STA	(IOBASE),Y
		RTS

; **** Send I2C Stop Condition *************************************************

; ******************************************************************************

I2C_STOP	LDY	#DDRB
		LDA	#%11011111	; SDA = 0; SCL = 0
		STA	(IOBASE),Y
		LDA	#%11011110	; SDA = 0; SCL = 1
		STA	(IOBASE),Y
		LDA	#%01011110	; SDA = 1; SCL = 1
		STA	(IOBASE),Y
		RTS

; **** Send I2C Acknowledged ***************************************************

; ******************************************************************************

I2C_ACK		LDY	#DDRB
		LDA	#%11011111	; SDA = 0; SCL = 0
		STA	(IOBASE),Y
		LDA	#%11011110	; SDA = 0; SCL = 1
		STA	(IOBASE),Y
		LDA	#%11011111	; SDA = 0; SCL = 0
		STA	(IOBASE),Y
		RTS

; **** Send I2C Not Acknowledged ***********************************************

; ******************************************************************************

I2C_NACK	LDY	#DDRB
		LDA	#%01011111	; SDA = 1; SCL = 0
		STA	(IOBASE),Y
		LDA	#%01011110	; SDA = 1; SCL = 1
		STA	(IOBASE),Y
		LDA	#%01011111	; SDA = 1; SCL = 0
		STA	(IOBASE),Y
		RTS

; **** Read I2C Device *********************************************************

; Input:  A - Device Address
; Output: C - 0 = not acknowledged, 1 = acknowledged

; ******************************************************************************

I2C_READ_DEV	SEC			; set carry flag
		ROL			; shift device address one bit left and rotate C in LSB. LSB = 1 = read
		BNE	I2C_SEND	; and send it

; **** Write I2C Device ********************************************************

; Input:  A - Device Address
; Output: C - 0 = not acknowledged, 1 = acknowledged

; ******************************************************************************

I2C_WRITE_DEV	ASL			; shift device address one bit left. LSB is now 0 = write
					; directly fallthrough to I2C_SEND

; **** Send a Byte to I2C Device ***********************************************

; Input:  A - Data Byte
; Output: C - 0 = not acknowledged, 1 = acknowledged

; ******************************************************************************

I2C_SEND	STA	I2C_DATA
		LDX	#$08		; send 8 bits
		LDY	#DDRB
SENDLOOP	ASL	I2C_DATA	; get next bit into C flag
		BCS	MSENDH		; is it a 1 bit?
		LDA	#%11011111	; no, SDA = 0; SCL = 0
		BNE	SETBIT		; branch always
MSENDH		LDA	#%01011111	; yes, SDA = 1; SCL = 0
SETBIT		STA	(IOBASE),Y
		AND	#%11111110	; SDA = X; SCL = 1
		STA	(IOBASE),Y
		ORA	#%00000001	; SDA = X, SCL = 0
		STA	(IOBASE),Y
		DEX
		BNE	SENDLOOP

I2C_ACK?	LDY	#DDRB

		LDA	#%01011111	; SDA = 1; SCL = 0
		STA	(IOBASE),Y

		LDA	#%01011110	; SDA = 1; SCL = 1
		STA	(IOBASE),Y
		LDY	#PORTB
		LDA	(IOBASE),Y	; get SDA
		BPL	ISACK		; SDA = 1 ?
		CLC			; no, not acknowledeged
		BCC	CLKDOWN
ISACK		SEC			; yes, acknowledeged
CLKDOWN		LDY	#DDRB
		LDA	#%01011111	; SCL = 0
		STA	(IOBASE),Y
		RTS

; **** Receive a Byte from I2C Device ******************************************

; Output: A - Data Byte

; ******************************************************************************

I2C_RCV		LDX	#$09
RCVLOOP		LDY	#DDRB
		LDA	#%01011111	; SDA = 1; SCL = 0
		STA	(IOBASE),Y
		DEX
		BEQ	RCVEND		; all eight bits received?
		LDA	#%01011110	; SDA = 1; SCL = 1
		STA	(IOBASE),Y
		LDY	#PORTB
		LDA	(IOBASE),Y	; get SDA
		ASL			; and shift it into C
		ROL	I2C_DATA	; shift byte buffer one bit left. C goes into LSB
		JMP	RCVLOOP
RCVEND		LDA	I2C_DATA	; load data into A
		RTS

; ******************************************************************************
; START OF SOUND GENERATOR CODE
; ******************************************************************************

; **** Send A Command Byte To The Sound Chip ***********************************

; Input: A - Data Byte

; ******************************************************************************

SOUND_SENDBYTE	STY	YSAV		; save current Y register
		LDY	#PORTA
		STA	(IOBASE),Y	; set data
		LDY	#PORTB
		LDA	#%11111101	; Set sound WE low
		AND	VIA_STATUS
		STA	(IOBASE),Y	; enable sound data write
		LDA	VIA_STATUS	; set sound WE high
		STA	(IOBASE),Y	; disable sound data write
		LDY	YSAV		; restore Y register
		RTS

; **** Mute All Sound Chip Channels ********************************************

; ******************************************************************************

SOUND_MUTEALL	LDY	#$03		; channels 0..3 to mute
NEXTCHANNEL	TYA
		JSR	SOUND_MUTE	; mute current channel
		DEY			; next channel
		BPL	NEXTCHANNEL	; loop if not all four channels done
		RTS

; **** Mute A Sound Chip Channel ***********************************************

; Input: A - Channel # (0..3)

; ******************************************************************************

SOUND_MUTE	LDX	#$0F		; set attenuation level to maximum
					; fall through to set attenuation level

; **** Set Attenuation For A Sound Chip Channel ********************************

; Input: A - Channel # (0..3)
; 	 X - Attenuation Level 0..15 (0dB, 2dB, 4dB ... OFF)

; ******************************************************************************

SOUND_SETATN	STX	TEMP		; store attenuation level in TEMP variable
		CLC			; clear carry flag
		ROR			; and rotate channel number to bit 5 and 6
		ROR	
		ROR	
		ROR	
		ORA	TEMP		; combine channel number with attenuation value
		ORA	#$90		; and also set bit 7 and 4
		JMP	SOUND_SENDBYTE	; send complete command byte to the sound chip

; **** Set Periodic Noise ******************************************************

; Input: X - Noise Shift Rate

; ******************************************************************************

SOUND_PNOISE	LDA	#$00
		BEQ	SET_NOISE

; **** Set White Noise *********************************************************

; Input: X - Noise Shift Rate

; ******************************************************************************

SOUND_WNOISE	LDA	#$01

; **** Set Noise ***************************************************************

; Input: A - 0 = Periodic Noise  1 = White Noise
;	 X - Noise Shift Rate [0..3]

; ******************************************************************************

SOUND_SETNOISE	ASL	
		ASL	
SET_NOISE	STX	TEMP
		ORA	TEMP
		ORA	#$E0		; Emile: 23-07-25, corrected, was $F0 (noise-att.), $E0 is noise-control register
		JMP	SOUND_SENDBYTE	; send complete command byte to the sound chip

; **** Set Sound Frequency in HZ ***********************************************

; Input: A - Channel (0..2)
;	 X - Frequency Low Bits 7..0
;	 Y - Frequency High Bits 9..8

; *****************************************************************************

SOUND_SETFREQ	CLC			; clear carry flag
		ROR			; and rotate channel number to bit 5 and 6
		ROR	
		ROR	
		ROR	
		ORA	#$80		; set high bit
		STA	TEMP		; and store it in TEMP variable
		TXA			; load frequency low bits into A
		AND	#$0F		; we first want to send the lower 4 bits
		ORA	TEMP		; combined it with the channel number
		JSR	SOUND_SENDBYTE	; send complete first command byte to the sound chip
		TYA			; load frequency high bits into A
		STX	TEMP		; store frequency low bits to TEMP variable
		LDX	#$04		; we need four bits shifted
LOOP_NXT	ASL	TEMP		; shift highest bit of low frequency to Carry flag
		ROL			; and shift it into the high frequency bits
		DEX			; decrement counter
		BNE	LOOP_NXT	; do we need more shifts?
		JMP	SOUND_SENDBYTE	; send complete second command byte to the sound chip

; ******************************************************************************
; ***************************** MAIN MONITOR ***********************************
; ******************************************************************************

; **** Auto Terminal And Baud Rate Detection Routine ***************************

; ******************************************************************************

INITVECT        LDX	#< NMI		; set NMI service routine
                LDY	#> NMI
		STX	NMIVECT
		STY	NMIVECT+1
		STX	IRQUSR
		STY	IRQUSR+1
		STX	BRKUSR
                STY	BRKUSR+1
		LDX	#< IRQ
		LDY	#> IRQ
		JMP	SETIRQVECT	; Set IRQ vector and return

		ORG	*+8		; maintain compatibility with v1.1.4
		
MAINSTART       SEI			; disable Interrupts
                LDX     #$FF
		TXS			; initialize stack pointer
		CLD			; set binary mode

                LDA     #< _HANDLER_  ; low address to empty event handler (RTS)
                STA     KEY_HANDLER     ; init character input handler low address
                STA     NKEY_HANDLER    ; init no character input handler low address
                LDA     #> _HANDLER_ ; high address to empty event handler (RTS)
                STA     KEY_HANDLER+1   ; init character input handler high address
                STA     NKEY_HANDLER+1  ; init no character input handler high address

                JSR     INITVECT

INITRESET       LDX     #< MON_WARM_START
		LDY     #> MON_WARM_START
		JSR	SET_RETURN_VECT		; set entry point for monitor warm start
		NOP				; maintain compatibility with v1.1.4

		LDA     #$80
		JSR  	DELAY		; wait for ~128ms after reset
		JSR	INITIO		; find and initialize IO cards
VTDETECT	JSR  	DELAY		; wait for ~128ms after reset
		STA  	STAT_REG   	; reset ACIA

; ******************************************************************************
; Set Fixed Baud Rate Patch
; ******************************************************************************

		LDA	#$3E
		STA	PBDD
		LDA	#$06		; set keyboard decoder Q4 to low
		STA	PBD		; write value to RIOT port B
		LDA	$FFF9		; load standard baud rate value
		STA	BAUDRATE	; and store it in detected baud rate variable
		LDA  	#$0B       	; set ACIA to
         	STA  	COMM_REG	; no parity, no receiver echo, RTS low, no IRQ, DTR low
		LDA	PBD		; read RIOT port B
		LDX	#$0F		; set all keyboard decoder outputs to high
		STX	PBD		; write value to RIOT port B
		ROR			; rotate bit 0 into Carry
		BCC	INIT		; if Carry = 0 then skip autodetection

; ******************************************************************************

		LDA	#CMD_INIT
		JSR	CMDPRINT	; try to detect connected terminal

; **** Main Initialization Routine *********************************************

; ******************************************************************************

INIT            JSR     BEEP		; give some feedback
		LDA  	BAUDRATE   	; load selected baud rate
		BNE     SETBAUDRATE	; terminal detected or fixed baud rate?
		LDA	$FFF9		; no, load standard baud rate value
		STA  	CTRL_REG	; set baud rate
		LDA     DEVID
		CMP     #TTY1_ID        ; is TTY still the standard output device?
		BNE     SET_CRTDEV      ; no, CRT controller is installed. Continue initialization
		JMP  	JCRESET		; TTY ist still sdtoutdev, but not connected. Jump to junior monitor

SET_CRTDEV      STA     STDOUTDEV       ; make CRT controller the standard output device
                JSR     SET_STDOUTID
                LDA	#CMD_INIT
		JSR	CMDPRINT	; initialize standard output device

SETBAUDRATE     STA  	CTRL_REG	; set detected baud rate

		CLI			; enable interrupts

; **** Main Program Loop *******************************************************

; ******************************************************************************

MAIN		JSR	CGET		; clear input buffer
		JSR  	CLRLOADSTR    	; clear screen and load pointer to string table
		LDA  	#$1F
		JSR  	TAB		; send some space chars to center title
        	LDY  	#TITLE-STRINGP 	; load title string
		JSR  	WRSTR		; and write it
		JSR     INIT_CFC	; init. CFC-driver

CHK_IO_CARD     LDA	IOBASEH		; language card available?
		BEQ	TRY_BOOT	; no, try to boot from CF-device
		
		LDY	#IOCARD-STRINGP ; load detect message
		JSR	WRSTR		; and write it
		LDA	IOBASEH
		JSR	HEXOUT
		LDA	#$00
		JSR	HEXOUT
		LDA	STDINDEV
		CMP	#KEYBD1_ID	; is ASCII keyboard the standard input device?
		BNE	SHOW_CLOCK 	; no, show clock
		
		LDY	#KBDSTR-STRINGP	; yes, load detect message
		JSR	WRSTR		; and write it
		JSR	SETPPORTIN
SHOW_CLOCK	JSR	CLOCKSTART	; call clock

TRY_BOOT        JSR     SYS_BOOT        ; try to boot from CF or SD device
                BCC     NO_BOOT_DEV     ; no boot device found, show menu
		
                JMP     BLOCK_BUF       ; jump to boot code in Volume-ID with C=1

NO_BOOT_DEV     LDA	IOBASEH		; language card available?
		BEQ	SHOWMON		; no, just start monitor

		LDA	#$00
                LDY	#ACR		; select auxilary control register
		STA	(IOBASE),Y	; disable shift operation
                JSR     LOADSTRING
		LDY	#SPACE-STRINGP
		JSR	WRSTR		; write spacer lines
		LDA	#$1E		; send some space chars to center menu
		JSR  	TAB
		LDY	#MENU-STRINGP   ; load menu string
		JSR	WRSTR		; and write it
		LDX	#< LANGNAME	; load language name
		LDY	#> LANGNAME
		JSR	SPRINT		; and write it
		LDA	#SPC
		JSR	COUT
		LDA	#'?'
		JSR	COUT
MLOOP		JSR  	CIN		; main menu loop
        	AND  	#$DF		; convert the input to uppercase char
        	CMP  	#'M'		; (M)onitor choosen?
		BNE	MNEXT1

STARTMON	JMP  	MONITOR		; yes, start monitor
MNEXT1		CMP	LANGKEY		; compare with language key char
		BNE	MNEXT

		JSR  	CLRSCRN    	; clear screen
		JMP	$B000		; jump to language start
MNEXT		JMP  	MLOOP		; no valid input choosen, try again
SHOWMON		JMP	MONRESET

; Load String Pointer **********************************************************

CLRLOADSTR      JSR     CLRSCRN
LOADSTRING	LDA  	#< STRINGP 	; load string pointer 1
		STA  	PSTRL
		LDA  	#> STRINGP
		STA  	PSTRH
		RTS
		
LOADSTRING2	LDA  	#< STRINGP2 	; load string pointer 2
		STA  	PSTRL
		LDA  	#> STRINGP2
		STA  	PSTRH
		RTS

WRITE_IO_INFO   LDA     #$10
		STA     IO_INFO
CHK_IO_0	LDA     FGCBASEH        ; controller card 0 available?
		BEQ     CHK_IO_1        ; no, check next card
		STA     IO_INFO+1
		JSR     CALL_INFO
CHK_IO_1        LDA     CARD3BASEH      ; controller card 1 available?
                BEQ     IO_INFO_END     ; no, exit
		
                STA     IO_INFO+1
		JSR     CALL_INFO
IO_INFO_END	RTS

CALL_INFO       JMP     (IO_INFO)

; ******************************************************************************
; MONITOR COMMAND EXECUTOR ROUTINES
; ******************************************************************************

; print command ****************************************************************

PRINTOUT	STY	YSAV		; save y register
		LDA	STDPRINTDEV	; get standard printer
		JSR	SET_STDOUTID    ; and make it the current output device
		LDY	YSAV		; restore y register
		INY
		LDA  	STRBUF,Y   	; get next input char
		AND  	#$DF		; uppercase chars only
		CMP  	#'D'		; print mem dump?
		BEQ	PRINTDUMP
		JSR	DISASSEM
		JMP	ENDINP
PRINTDUMP	JSR	MEMDUMP		; print memory dump
		BCC	ENDINP 		; normal termination?
		JSR	BEEP		; no, ESC pressed. Beep
ENDINP		JSR	CROUT		; send CR/LF to print last line
		LDA	STDOUTDEV	; get standard output device
		JSR	SET_STDOUTID    ; and make it the current output device
		JMP	MONINP		; get next command line

; XMODEM load/save command *****************************************************

XMODEM		DEX			; check read/write mode
		BEQ	XMODEML		; read mode?
		LDA	MODE		; no, test if valid address mode
		BEQ	NOTVALID	; not valid, get next input
		JSR	XModemSnd	; call xmodem send
		BEQ	XMODEME
XMODEML		JSR	XModemRcv	; yes, call xmodem receive
XMODEME		JMP	MONINP		; get next command line

; parallel load/save command ***************************************************

PARALLEL	DEX			; check read/write mode
		BEQ	PARALLELL	; read mode?
		LDA	MODE		; no, test if valid address mode
		BEQ	NOTVALID	; not valid, get next input
		JSR	PPORTSAVE	; call save pport ### not implemented yet
		JMP	MONINP
PARALLELL	JSR	PPORTLOAD	; call load pport ### not implemented yet
		JMP	MONINP		; get next command line

; Removed 20-07-'25 (+34 bytes): tape load/save command **********************************

;----------------------------------------------------------------------------------		
; This routine sets the return vector that is used by both Monitor and boot.sys.
;----------------------------------------------------------------------------------		
SET_RETURN_VECT	STX     RETURN_VECT     	; set entry point for monitor warm start
                STY     RETURN_VECT+1
		RTS

		ORG	$E5E7		; maintain compatibility with previous BIOS versions
; load/save command ************************************************************

LOADSAVE	LDA	MODE		; check address mode
		ASL	
		BMI	CHKNEXTCMD	; mode = $C0 (block mode)?
		LDA	#$FF		; no, set end address to $ffff
		STA	NUML
		STA	NUMH
CHKNEXTCMD	INY
		LDA  	STRBUF,Y   	; get next input char
		AND  	#$DF		; uppercase chars only
		CMP  	#'M'		; load/save via xmodem
		BEQ  	XMODEM
		CMP  	#'P'		; load/save via parallel port
		BEQ  	PARALLEL
	:4	NOP			; REMOVED load/save via tape
		CMP	#'0'
		BCS	NOTVALID
		DEX			; check load/save mode
		BNE	NOTVALID	; if save mode, just get next input char
		JSR	DISASSEM	; last command was L, so call disassembler
		JMP	MONINP		; we are finnished, get next input line
NOTVALID	LDA	#$00		; no valid command, so restore registers
		TAX
		JMP  	SETMODE2   	; and get next input char

; save command *****************************************************************

SAVE		INX

; load command *****************************************************************

LOAD		INX
		JMP	LOADSAVE

; print command ****************************************************************

PRINT		JMP	PRINTOUT

; call a program ***************************************************************

RUN		JSR	PRADDRESS
		LDA	#'R'		; print R to signal run mode
		JSR	COUT
		JSR	CROUT
		JSR     EXECPROG
		JMP  	MONINP		; jump back from program call
EXECPROG	JMP  	(ADRL)     	; jump to program address; execute program

; **** Start Of Hex Monitor ****************************************************

; ******************************************************************************
MONITOR 	JSR  	CLRLOADSTR    	; clear screen and load pointer to string table
		LDY  	#MONSTR-STRINGP
        	JSR  	WRSTR		; show monitor title
MONRESET	JSR	SETPPORTIN	; initialize RIOT
MONINP		JSR  	CROUT
		LDA  	#PROMPT
		JSR  	COUT		; show monitor prompt
		JSR  	STRIN      	; read input string
		LDY  	#$00       	; reset string index
		TYA			; mode = 0 (none)

MONINIT		TAX
SETADRMODE	STA  	MODE
SKIPCMDCHR      INY			; increment string index
NEXTCMDCHR	LDA  	STRBUF,Y   	; get next input char
		CMP  	#CR
		BEQ  	ENDCMD 		; end of input line, return to reader routine
		CMP  	#' '
		BEQ  	SKIPCMDCHR 	; ignore spaces
		CMP  	#'.'
		BEQ  	SETBLKMODE 	; block mode
		CMP	#':'
		BEQ	SETADRMODE
		AND  	#$DF		; uppercase chars only
		CMP  	#'L'		; LOAD/LIST command
		BEQ	LOAD 		; load or list data
		CMP  	#'S'		; SAVE command
		BEQ	SAVE		; save data
		CMP	#'P'		; PRINT command
		BEQ	PRINT		; print data
		CMP  	#'G'		; GO command
		BEQ  	RUN		; call program
		CMP  	#'M'		; JUNIOR MONITOR command
		BNE  	NEXTCMD
		JMP	JCRESET		; execute original junior computer monitor
NEXTCMD		CMP  	#'Q'		; QUIT command
		BEQ  	MONEND		; exit monitor program
		LDA	MODE		; test if list command pending
		BMI	DUMP		; if mode = $80, dump last line
		STY  	YSAV		; save Y
		JSR  	HEXINPUT   	; read hex number
	 	CPY  	YSAV		; min 1 hex digit entered?
		BEQ  	SKIPCMDCHR     	; no, read next command
		LDA  	MODE
		BNE	SETMODE
STOREADR	LDA  	NUML       	; yes, copy input value to last address
                STA  	ADRL
                LDA  	NUMH
                STA  	ADRH
		LDA	#$00		; line list mode
SETMODE		CMP	#':'		; is it store mode?
		BEQ	STOREDATA	; yes, store data
		ORA	#$80
SETMODE2	STA	MODE
		BNE	NEXTCMDCHR	; branch always
SETBLKMODE	LDA	#$40		; set block list mode
		BNE	SETADRMODE
ENDCMD		LDA	MODE		; test if list command pending
		BMI	DUMP		; yes, dump last line
CMDEND		JMP	MONINP		; read next command line
MONEND		JMP     (RETURN_VECT)   ; return to monitor caller

; store data *******************************************************************

STOREDATA	LDA  	NUML       	; load lower byte of number
                STA  	(ADRL,X)   	; store current store address (X=0)
                INC  	ADRL       	; increment lower store index.
                BNE  	NEXTITEM    	; no overflow
                INC  	ADRH       	; add carry to upper store index
NEXTITEM        JMP  	NEXTCMDCHR    	; get next command string

; call memory dump *************************************************************

DUMP		JSR	MEMDUMP
		BCC	NEXTCMDCHR   	; get next input
		BCS	CMDEND		; yes, stop printing memory dump

; print memory dump ************************************************************

MEMDUMP		LDX	#$00
		STX  	PDBCNT		; printed data byte count = 0
		JSR	CHKESC		; ESC pressed?
		BCC	PRADR		; no, go on dumping
		RTS			; yes, exit leaving carry flag set
PRADR		JSR	PRADDRESS	; print current address

; print current data byte ******************************************************

PRDATA		JSR  	SPCOUT		; print space
                LDA  	(ADRL,X)   	; get data from address (X=0)
                JSR  	HEXOUT     	; print data in hex format
		INC  	PDBCNT     	; increment data counter

; examine next address *********************************************************

ADRNEXT		JSR	CMPADDR		; see if there's more to print
                BCS  	FINISHED?  	; no more data to output

		JSR	INCADR          ; increment list index
DIVCHK          LDA  	PDBCNT
		CMP  	#$08
		BNE  	MOD16CHK	; do we need a divider?
		JSR  	SPCOUT		; yes, print single SPC as block divider
MOD16CHK	LDA  	ADRL       	; if address MOD 16 = 0 start new line
                AND  	#$0F
		BNE  	PRDATA
		JSR  	PRASCII
		JMP  	MEMDUMP		; print next line

; check if line print completed ************************************************

FINISHED?	LDA  	MODE		; examine last mode
		STX  	MODE       	; set mode 0
		ASL			; mode = $D0?
		BPL  	ENDDUMP		; no, get next input
		JSR  	PRASCII		; yes, we are not finished, print ASCII output for last address
ENDDUMP		CLC			; normal exit, so clear carry flag
		RTS

; print a column with ASCII representation of data *****************************

PRASCII         STY  	YSAV       	; store Y
		SEC			; no carry to subtract
		LDA  	#52		; max tabs
		SBC  	PDBCNT		; calc tab count to print ASCII column
		SBC  	PDBCNT		; tab = 52-3*printed_data_bytes_count
		SBC  	PDBCNT
		LDY  	PDBCNT
		CPY  	#9		; more than 8 bytes viewed?
		BCS  	NOADJUST	; no
		ADC  	#1		; yes, adjust by one char for block divider
NOADJUST	JSR  	TAB		; print tab spaces

		LDY  	#$00
NEXTASC		LDA  	(ASCL),Y   	; get data from address
		CMP	#$7F
		BCS  	NOASC      	; char >= ASCII 127? yes, print '.'
		CMP  	#' '
		BCS  	ASCOUT		; printable character?
NOASC		LDA  	#'.'       	; no, print '.'
ASCOUT		JSR  	COUT
		INY
		CPY  	PDBCNT
		BNE  	NEXTASC
		LDY  	YSAV       	; restore Y
		RTS

; Prompt new line with current address *****************************************

;*******************************************************************************

PRADDRESS	JSR  	CROUT
		LDA  	ADRH
                STA  	ASCH		; store current print address high-order byte
		JSR  	HEXOUT		; print high-order byte of address
                LDA  	ADRL
		STA  	ASCL		; store current print address low-order byte
                JSR  	HEXOUT		; print low-order byte of address
		LDA  	#ADIV      	; print '-'
                JMP  	COUT

; Compare if start address ADR is greater end address NUM **********************

;*******************************************************************************

CMPADDR		LDA  	ADRL       	; see if there's more to print
                CMP  	NUML
                LDA  	ADRH
                SBC  	NUMH
		RTS

; Jump to original Junior Computer reset vector ********************************

;*******************************************************************************
JCRESET		LDA	#$06			; set PB5 = L (WRITE)
		STA	PBD
		SEI
		JSR     INITVECT
                JSR     VIA2IRQ_OFF
		CLI
		JMP	MONITOR_BLOCK.RESET	; jump to Junior Computer reset routine

; ******************************************************************************
; String Data Section
; ******************************************************************************

MAGIC0		.byte	$00,$00,$00,$00                 ; Removed by Emile
MAGIC1          .byte   $18,$90,$00,$90                 ; clc bcc 00 bcc

PSSTR		.by	'PSAYX'				; processor status string

STRINGP							; *** string base pointer ***
ESCCLS  	.byte   $32,$4A,$1B,$5B                 ; VT100 clear screen sequence
ESCHOME        	.byte   $48,$00                 	; VT100 cursor home sequence
ESCCLL        	.byte   $32,$4B,$00         		; VT100 clear line sequence
ESCGID		.byte   $30,$63,$00	        	; VT100 get ID sequence
ESCNORM		.byte   $6D,$00	        		; VT100 set normal text mode
ESCINV		.byte   $37,$6D,$00	        	; VT100 set inverse text mode
ESCBLNK		.byte   $35,$6D,$00	        	; VT100 set blinking text mode

TITLE		.by   	'Junior Computer ][ ' CR,CR,CR
		.by   	' BIOS V'
        	.byte   VERMAIN,$2E,VERPSUB,$2E,VERSSUB,CR
        	.by   	' 2020/24 by Joerg Walke, 2025 by Emile' CR CR $00
IOCARD		.by	' IO-Card at $' $00
KBDSTR		.by	CR ' ASCII Keyboard connected' $00
SPACE    	.byte  	CR, CR, CR, CR, $00
MENU		.by   	'(M)onitor  ' $00
MONSTR		.by	CR 'Hex Monitor' CR $00

DT_NOT_SET	.by	CR CR ' Date/Time not set' CR $00
DATEINPUT	.by	CR ' Date: DD' DATEDIV 'MM' DATEDIV 'YY'
		.byte	8,8,8,8,8,8,8,8,$00
TIMEINPUT	.by	CR ' Time: HH' TIMEDIV 'MM' TIMEDIV 'SS'
		.byte	8,8,8,8,8,8,8,8,$00

STRINGP2
DAYS		.by	'Mon' $00
		.by	'Tue' $00
		.by	'Wed' $00
		.by	'Thu' $00
		.by	'Fri' $00
		.by	'Sat' $00
		.by	'Sun' $00
OSID            .by    'JCOS'
BOOTDEV         .by    CR ' Booting from ' $00
NOBOOTDEV       .by    CR ' No Boot Disk ' $00
CFCDEV          .by    'CFC1' $00
SDCDEV          .by    'SDC1' $00

; ******************************************************************************
; START OF DISASSEMBLER
; ******************************************************************************

DISASSEM	LDA	MODE
		ASL	
		BPL	SHOW1PAGE	; mode <> $C0 (block mode)?
NEXTLINE1	JSR     CHKESC          ; ESC pressed?
		BCS	ENDDISASSEM	; yes, quit disassembling
NEXTOP		JSR	LOADOPCODE	; load current opcode
MORE?		JSR	CMPADDR		; see if there's more to print
                BCC  	NEXTLINE1  	; no more data to output
		RTS
SHOW1PAGE	LDA	#23		; show 23 disassembled lines
		STA	LINECNT
NEXTLINE2	JSR	LOADOPCODE	; load current opcode
		DEC	LINECNT
		BNE	NEXTLINE2	; more lines to show?
ENDDISASSEM	RTS			; no, jump back to monitor

; load next opcode

LOADOPCODE	JSR	PRADDRESS	; print current address
		LDY	#$00
		LDA	(ADRL),Y	; load opcode
		TAY			; store opcode in Y
		AND	#$03
		CMP	#$03		; is it a unused opcode?
		BNE	DECODE  	; no, decode it
		LDY	#$02		; Y points to unused opcode $02 to print '???'
		TYA			; and we also need it in A
		BNE	DECODEMNEM	; branch always

; decode opcode index into compressed opcode instruction table

DECODE		TYA			; reload opcode into A
		LSR			; every fourth column in the opcode table is a (opcode) gap
		LSR			; so we have to adjust the index because these columns are
					; stripped off in our indirect index table
		STA	TEMP		; store number of bytes to subtract
		TYA			; reload opcode again
		SEC
		SBC	TEMP		; and subtract value in TEMP from the original opcode.
DECODEMNEM	STY	OPCODE		; store opcode
		TAY			; Y holds now the actual index to the stripped opcode table
		LDA	OPCODES,Y	; load packed mnemonic_index/instr._bytes from opcode table
		TAY			; and save to Y
		AND	#$03		; the lower two bits are the number of instruction bytes
		STA	IBYTES		; store it in IBYTES var
		TYA			; reload packed index
		LSR			; and strip the lower two bits off
		LSR	
		TAY			; Y holds now the index to the mnemonics table
		LDA   	MNEMONICSH,Y    ; load first packed byte of mnemonic string
		STA   	ASCH            ; and store it as left byte of mnemonic
		LDA   	MNEMONICSL,Y    ; load second packed byte of mnemonic string
		STA   	ASCL            ; and store it as right byte of mnemonic
		JSR	SHOWHEX		; first print out all instruction bytes as hex numbers
		LDX   	#$03		; we have to unpack three chars
NEXTMCHR        LDA   	#$00		; clear A
		LDY   	#$05            ; shift 5 bits into A
NEXTSHIFT       ASL   	ASCL           	; MSBit of ASCL in C
		ROL   	ASCH		; C in LSBit of ASCH and MSBit of ASCH in C
		ROL   			; C in A
		DEY
		BNE   	NEXTSHIFT
		ADC   	#'?'		; add offset to result, to make it an ASCII char
		JSR     COUT		; print one character of mnemonic
		DEX
		BNE     NEXTMCHR	; more chars to decode?
		LDA	#$02		; print two space chars
		JSR	TAB

; decode address mode and print left part of mode string ('#', '(' or 'A')

		LDX     #$A0            ; default address mode is implied
		LDY	#$FF
NEXTMASK	INY
		CPY	#$0F		; all masks tested?
		BEQ	ENDMASK		; yes, finish. Address mode is implied
		LDA	ADRMODEM,Y	; load mask
		AND	OPCODE		; mask opcode
		CMP	ADRMODEC,Y	; is it the mask result?
		BNE	NEXTMASK	; no, try next mask
		LDX	ADRMODER,Y	; yes, load the resulting address mode
ENDMASK		STX	ADRMODE		; save address mode
		TXA			; address mode is in A
		AND	#$0F		; A holds left mode string index
		TAY
		LDA	ADRMSTRL,Y      ; load left mode string
		BEQ	PRINTVAL	; is it a NULL char? Then there is nothing to print
		JSR	COUT		; else print character

; print either one or two operand value bytes

PRINTVAL	JSR	INCADR		; increment current address
		LDX	IBYTES		; load number of instruction bytes
		DEX			; more than one IBs?
		BEQ	ENDINC		; no, just finish
		LDA	#'$'		; yes, print operant value
		JSR	COUT		; first print out '$' as hex number indicator
		LDY	#$01
		DEX			; more than two IBs?
		BEQ	HEX1		; no, just print one byte
		LDA	(ADRL),Y	; load high byte
		JSR	HEXOUT		; and print it as hex number
HEX1		DEY
		LDA	(ADRL),Y	; load low byte
		LDX	ADRMODE
		CPX	#$A4		; is it a branch opcode?
		BEQ	CALCADR		; yes, calculate branch destination address
		JSR	HEXOUT		; no, print byte as hex number

; print right part of mode string. (',X', ',Y', ',X)', '),Y' or ')' )

		TXA			; load address mode in A
		LSR			; upper nibble is index to right address mode string
		LSR			; so we have to shift it right by four bits
		LSR	
		LSR	
		TAY
NEXTAMCHR	LDA	ADRMSTRR,Y	; load one char of right mode string
		BEQ     ENDMODE		; if a NULL char then we are finished
		JSR	COUT		; else print char
		INY
		BNE	NEXTAMCHR	; branch always

; finish current instruction

ENDMODE		LDA	IBYTES		; how many address increments left?
		CMP	#$03
		BNE	INCADR		; just one?
		JSR	INCADR		; no, two increments needed

; increment current address

INCADR  	INC  	ADRL    	; increment current address
        	BNE  	ENDINC  	; no carry!
        	INC  	ADRH
ENDINC		RTS			; end of disassembly

; calculate destination address for branch instructions

CALCADR		SEC
		TAY			; transfer A to Y for sign test
		BPL	ADDOFS		; is the branch offset positiv?
		EOR	#$FF		; no, subtract offset from current address
		STA	STOL
		LDA	ADRL
		SBC	STOL		; subtract branch offset from LSB current address
		TAY			; store low byte of address to Y
		LDA	ADRH
		SBC	#$00		; substract carry from MSB of address
		JMP	PRINTOFFS
ADDOFS		ADC	ADRL		; add branch offset to LSB of current address
		TAY			; store low byte of address to Y
		LDA	ADRH
		ADC	#$00		; add carry to MSB of address
PRINTOFFS	JSR	HEXOUT		; print high byte of branch address
		TYA
		JSR	HEXOUT		; print low byte of branch address
		JMP	INCADR		; and increment current address by one

; show instruction bytes as hex values and trailing variable number of space chars

SHOWHEX		LDY	#$00
NEXTBYTE	JSR	SPCOUT		; print leading space char
		LDA	(ADRL),Y	; load data byte
		JSR	HEXOUT		; and print it
		INY
		CPY	IBYTES		; all data bytes printed?
		BNE	NEXTBYTE	; no, print next byte
		LDA	#$0C		; tab size is 12
		SEC
CALCTAB		SBC	#$03		; reduce tab space by 3 for every data byte
		DEY
		BNE	CALCTAB		; all data bytes considered?
		JMP	TAB

; Address Mode Decode Tables ***************************************************

; Mask, Mask Result and Mode tables. If Opcode and Mask = Mask Result then Mode
; each Mode holds two indices (4 bits R | 4 bits L) to the mode string parts

; ******************************************************************************

ADRMODEM	.byte	$FF,$FF,$FF,$1F,$1F,$1F,$1F,$1F,$9F,$9F,$1C,$1C,$DF,$1C,$1C ; mask bits

ADRMODEC	.byte	$6C,$A2,$BE,$01,$09,$10,$11,$19,$0A,$80,$04,$0C,$96,$14,$1C ; mask result bits

ADRMODER	.byte	$52,$A1,$80,$32,$A1,$A4,$72,$80,$A3,$A1,$A0,$A0,$80,$00,$00 ; packed mode bits

; Address Mode Strings *********************************************************

ADRMSTRL	.byte	$00,$23,$28,$41,$00
		;	 0   #   (   A   0

ADRMSTRR	.byte	$2C,$58,$00,$2C,$58,$29,$00,$29,$2C,$59,$00
		;	 ,   X   0   ,   X   )   0   )   ,   Y   0

; Mnemonics Table **************************************************************

; three characters packed in two bytes. Each character uses 5 bits, last bit is
; unused

; ******************************************************************************

; low bytes of table

MNEMONICSL	.byte	$48, $CA, $1A, $08, $28, $A4, $AA, $94
     		.byte	$CC, $5A, $D8, $C8, $E8, $48, $4A, $54
     		.byte	$6E, $A2, $72, $74, $88, $B2, $B4, $26
     		.byte	$C8, $F2, $F4, $A2, $26, $44, $72, $74
     		.byte	$26, $22, $C4, $44, $62, $44, $62, $1A
     		.byte	$26, $54, $68, $C8, $88, $8A, $94, $44
     		.byte	$72, $74, $B2, $B4, $32, $44, $68, $84, $00

; high bytes of table

MNEMONICSH	.byte	$11, $13, $15, $19, $19, $19, $1A, $1B
     		.byte	$1B, $1C, $1C, $1D, $1D, $23, $23, $23
     		.byte	$23, $23, $24, $24, $29, $29, $29, $34
     		.byte	$53, $53, $53, $5B, $5D, $69, $69, $69
     		.byte	$6D, $7C, $84, $8A, $8A, $8B, $8B, $9C
     		.byte	$9C, $9D, $9D, $A0, $A1, $A1, $A1, $A5
     		.byte	$A5, $A5, $A8, $A8, $AD, $AE, $AE, $AE, $00

; Compressed Opcode Table ******************************************************

; each byte holds a 6 bit index to the mnemonic table and 2 bits instruction
; byte count
; empty opcode table columns (3,7,B,F) are stripped out

; ******************************************************************************

OPCODES         .byte	$29, $8A, $E1, $E1, $8A, $0A, $91, $8A, $09, $E1, $8B, $0B
     		.byte	$26, $8A, $E1, $E1, $8A, $0A, $35, $8B, $E1, $E1, $8B, $0B
     		.byte	$73, $06, $E1, $1A, $06, $9E, $99, $06, $9D, $1B, $07, $9F
     		.byte	$1E, $06, $E1, $E1, $06, $9E, $B1, $07, $E1, $E1, $07, $9F
     		.byte	$A5, $5E, $E1, $E1, $5E, $82, $8D, $5E, $81, $6F, $5F, $83
     		.byte	$2E, $5E, $E1, $E1, $5E, $82, $3D, $5F, $E1, $E1, $5F, $83
     		.byte	$A9, $02, $E1, $E1, $02, $A2, $95, $02, $A1, $6F, $03, $A3
     		.byte	$32, $02, $E1, $E1, $02, $A2, $B9, $03, $E1, $E1, $03, $A3
     		.byte	$E1, $BE, $E1, $C6, $BE, $C2, $59, $E1, $D5, $C7, $BF, $C3
     		.byte	$0E, $BE, $E1, $C6, $BE, $C2, $DD, $BF, $D9, $E1, $BF, $E1
     		.byte	$7E, $76, $7A, $7E, $76, $7A, $CD, $76, $C9, $7F, $77, $7B
     		.byte	$12, $76, $E1, $7E, $76, $7A, $41, $77, $D1, $7F, $77, $7B
     		.byte	$4E, $46, $E1, $4E, $46, $52, $69, $46, $55, $4F, $47, $53
     		.byte	$22, $46, $E1, $E1, $46, $52, $39, $47, $E1, $E1, $47, $53
     		.byte	$4A, $AE, $E1, $4A, $AE, $62, $65, $AE, $85, $4B, $AF, $63
     		.byte	$16, $AE, $E1, $E1, $AE, $62, $B5, $AF, $E1, $E1, $AF, $63

; ******************************************************************************
; START OF HIGH LEVEL REAL TIME CLOCK CODE
; ******************************************************************************

; **** Check If Date/Time Is Set And Show Date/Time ****************************

CLOCKSTART	JSR	CHECKDATETIME
		JSR	CROUT
		JSR	CROUT
		JSR	SPCOUT
		JMP	PRINTDATETIME

; **** Check If Date/Time Is Set ***********************************************

; ******************************************************************************

CHECKDATETIME	LDA	#$08
		JSR	READCLOCK
		CPY	#$65
		BNE	DATETIMELOST
		CPX	#$02
		BNE	DATETIMELOST
		RTS

DATETIMELOST	JSR	LOADSTRING
		LDY	#DT_NOT_SET-STRINGP
		JSR	WRSTR

SETDATETIME	JSR	SETTIME

SETDATE		JSR	LOADSTRING
		LDA	#'.'
		STA	DIVCHAR
		LDY	#DATEINPUT-STRINGP
		JSR	WRSTR
		LDY	#$31
		JSR	GETDIGIT
		STA	YREG
		JSR	PRINTDIVCHAR
		LDY	#$12
		JSR	GETDIGIT
		STA	XREG
		JSR	PRINTDIVCHAR
		LDY	#$99
		JSR	GETDIGIT
		STA	ACC
		JMP	WRITEDATE2

SETTIME		JSR	LOADSTRING
		LDA	#':'
		STA	DIVCHAR
		LDY	#TIMEINPUT-STRINGP
		JSR	WRSTR
		LDY	#$23
		JSR	GETDIGIT
		STA	ACC
		JSR	PRINTDIVCHAR
		LDY	#$59
		JSR	GETDIGIT
		STA	XREG
		JSR	PRINTDIVCHAR
		LDY	#$59
		JSR	GETDIGIT
		STA	YREG
		JMP	WRITETIME2

GETDIGIT	INY
		STY	YSAV
GETDIGIT1	JSR	NUMINPUT
		BCC	GETDIGIT1
		TAX
		SBC	#48
		ASL	
		ASL	
		ASL	
		ASL	
		CMP	YSAV
		BCS	GETDIGIT1
		STA	TEMP
		TXA
		JSR	COUT
GETDIGIT2	JSR	NUMINPUT
		BCC	GETDIGIT2
		TAX
		SBC	#48
		ORA	TEMP
		CMP	YSAV
		BCS	GETDIGIT2
		STA	TEMP
		LDA	#'.'
		CMP	DIVCHAR
		BNE	GETDIGITEND
		LDA	TEMP
		BEQ	GETDIGIT2
GETDIGITEND	TXA
		JSR	COUT
		LDA	TEMP
		RTS


NUMINPUT	JSR	CIN
		CMP	#'0'
		BCC	NOTNUM
		CMP	#':'
		BCC	ISNUM
NOTNUM		CLC
		RTS
ISNUM		SEC
		RTS

; **** Print Date And Time *****************************************************

; ******************************************************************************

PRINTDATETIME	JSR	PRINTDATE	; print current date	; PRINTFULLDATE
		JSR	SPCOUT
		JSR	PRINTTIME	; print current time
		JMP	CROUT

; **** Print Time **************************************************************

; ******************************************************************************

PRINTTIME	LDA	#':'
		STA	DIVCHAR
		JSR	READTIME
		JSR	PRINTDIGIT
		TXA
		JSR	PRINTDIGIT
		TYA
		JMP	HEXOUT

; **** Print Date And Day Of Week **********************************************

; ******************************************************************************

PRINTFULLDATE	JSR	READDOW
		ASL	
		ASL	
		TAY
		LDA  	#< (DAYS-4)
		STA  	PSTRL
		LDA  	#> (DAYS-4)
		STA  	PSTRH
		JSR	WRSTR
		JSR	SPCOUT

; **** Print Date **************************************************************

; ******************************************************************************

PRINTDATE	LDA	#DATEDIV	; load divider char
		STA	DIVCHAR
		JSR	READDATE	; read current date
		STA	TEMP		; store year value in TEMP
		TYA
		JSR	PRINTDIGIT	; print day
		TXA
		JSR	PRINTDIGIT	; print month
		LDA	#CENTURY
		JSR	HEXOUT		; print century
		LDA	TEMP
		JMP	HEXOUT		; print year

PRINTDIGIT	JSR	HEXOUT		; print digit
PRINTDIVCHAR	LDA	DIVCHAR		; print divider char
		JMP	COUT

; ******************************************************************************
; START OF LOW LEVEL ROUTINES
; ******************************************************************************

; **** Print Processor Status **************************************************

; ******************************************************************************

PRSTATUS	JSR	PRADDRESS	; print current program counter
		LDX	#$04
NXTREG		JSR	SPCOUT		; print space char
		LDA	PSSTR,X		; load register label
		JSR	COUT		; and print it
		LDA	#'='
		JSR	COUT		; print =
		LDA	PREG,X
		JSR	HEXOUT
		DEX
		BPL	NXTREG
		RTS

; **** Initialize IO Devices ***************************************************

; ******************************************************************************

INITIO          LDA     #$00
                LDX     #$06
INITIO1         STA     IOBASE,X        ; clear K2,K3 and K4 base address pointers
                DEX
                BNE     INITIO1

                JSR     DEV_INIT        ; initialize driver list
SET_TTY_DEV     LDX     #<  TTY_DEV
                LDY     #> TTY_DEV
                JSR     DEV_ADD         ; add terminal driver
		STA	STDINDEV        ; and initially set TTY as standard IO
		STA	STDOUTDEV
		STA     DEVID

SET_XMODEM_DEV  LDX     #<  XMODEM_DEV
                LDY     #> XMODEM_DEV
                JSR     DEV_ADD         ; add xmodem driver

SET_PRINTER_DEV LDX     #<  PPRINT_DEV
                LDY     #> PPRINT_DEV
                JSR     DEV_ADD         ; add parallel printer driver
                STA	STDPRINTDEV     ; and initially set parallel port printer as standard printer

		JSR     DETECT_IO       ; detect IO cards

; TEMP #### future: Set std beep only if no language card found

INIT_BEEP	LDA	#< DOBEEP	; load low byte of address of system beep
		STA	STDBEEP
		LDA	#> DOBEEP	; load high byte of address of system beep
		STA	STDBEEP+1

                JMP     RESET_STDIO     ; reset to standard IO devices

IO_INITIALIZE   JMP     (ADRL)

; **** Scan Bus And Detect IO Cards ********************************************
;
; ******************************************************************************

DETECT_IO       LDA     #$FB
                STA     ADRL            ; set pointer to init routine
                LDA     #$07
                STA     ADRH
                LDA     #$03            ; search on three slot base addresses
                STA     PDBCNT
DETECT_LOOP     CLC
                LDA     ADRH
                TAX
                INX                     ; X holds slot base address high byte
                ADC     #$04
                STA     ADRH            ; set high byte to init routine
                LDY	#$03            ; test byte string in card ROM against magic number
COMP_LOOP	LDA	MAGIC1,Y        ; get one byte of magic number
		CMP	(ADRL),Y        ; and compare it with ROM content.
		BNE	NO_MATCH        ; byte does not match, exit inner detection loop
		DEY                     ; byte matched magic number, try next one
		BPL	COMP_LOOP       ; more bytes to compare?
		JSR     IO_INITIALIZE   ; IO card detected. Call init routine
NO_MATCH        DEC     PDBCNT
                BNE     DETECT_LOOP     ; try next card base address
                                        ; fall through to IO/Language Card detection

; **** Try To Detect IO/Language Card ******************************************
;
; ******************************************************************************

DETECT_IOL_CARD	LDA	#$00
		STA	IOBASEL		; set low byte of IO base pointer to $00
		LDA	#$08		; try 1st IO-card at $0800
		STA	IOBASEH		; set high byte of IO base pointer to $00
		
DETECT_LP	LDY	#VIA_DDRA	; input & output regs are the same
		LDA	#$55		; DDRA = $55
		STA	(IOBASE),Y	; DDRA = 0101 0101 (0=input, 1=output)
		LDY	#VIA_PORTA	; 
		LDA	#0
		STA	(IOBASE),Y	; Write all zeros to PORTA	
		LDA	(IOBASE),Y	; Get PORTA value
		CMP	#$AA		; input bits should be 1, output bits should be 0
		BEQ	INIT_IOCARD	; branch if a card is found
		
		LDA	IOBASEH		; MSB of IO base pointer
		CLC
		ADC	#4		; next IO base-address
		STA	IOBASEH		; next card-address
		CMP	#$14
		BCC	DETECT_LP	; branch if MSB base-address < $14
		
		LDA	#0
		STA	IOBASEH		; 0 = no card found
NOCARD          RTS                     ; no card found

		ORG	$ED01		; maintain compatibility with v1.1.4
		
; **** Initialize The IO/Language Card *****************************************
INIT_IOCARD     LDA     STDINDEV
                CMP     #TTY1_ID
                BNE     INIT_VIA        ; is standard input device still TTY?
                JSR	DETECT_ASCIIKBD	; yes, check if ASCII keyboard available, else skip it
INIT_VIA	LDY	#PORTB
		LDA	#%01001110	; SDA=0,/CAS_MOT=1,CAS_SENSE=0,CAS_WR=0,/SPI_LOAD=1,SPI_CS=1,/SND_WE=1,SCL=0
		STA	VIA_STATUS	; store current PortB output status
		STA	(IOBASE),Y	; set SDA as input to pull it high, set SCL as input to pull it high
		LDY	#DDRB		; initialize data direction of port B
		LDA	#%01011110	; SDA,/CAS_MOT,/CAS_SENSE,CAS_WR,/SPI_LOAD,SPI_CS,/SND_WE,SCL
		STA	(IOBASE),Y
		LDY	#DDRA		; initialize data direction of port A
		LDA	#$FF		; all pins of port A are outputs
		STA	(IOBASE),Y
		JSR	SOUND_MUTEALL	; mute sound output
INIT_SDCARD     JSR     SPI_INIT        ; initialize SPI
                LDX     #< SDC_DEV
                LDY     #> SDC_DEV
                JSR     DEV_ADD         ; add sd-card driver
INIT_TAPE       LDA	#$00
                LDY	#ACR		; select auxilary control register
		STA	(IOBASE),Y	; set one shot timer mode
		LDY	#PCR		; select peripheral control register
		STA	(IOBASE),Y	; set interrupt on falling edge of CA1
		JSR	VIA2IRQ_ON	; Enable VIA2 Timer interrupt
		JSR	RESET_TIMER2	; set Timer2 to 1/60 second
		LDX	#< VIA2IRQ 	; set low address of clock interrupt routine
		LDY	#> VIA2IRQ   	; set high address of clock interrupt routine
SETIRQVECT	STX	IRQVECT
		STY	IRQVECT+1
		RTS

; ******************************************************************************
; TAPE READ/WRITE ROUTINES removed 24-07-'25 Emile
; ******************************************************************************

; **** VIA2 IRQ Routine ********************************************************
VIA2IRQ		PHA			; save accumulator
		TYA
		PHA			; save Y register
		LDY	#IFR		; select interrupt flag register
		LDA	(IOBASE),Y
		BPL	NOVIA2IRQ	; check if it was a VIA2 interrupt
		
		AND	#$02		; yes, CA1 interrupt occured?
		BEQ	CHECKKEY	; no, check key status
		
CHECKBIT	LDY	#PORTA		; VIA2 PORTA
		LDA	(IOBASE),Y	; clear CA1 interrupt flag
		LDA	CNTIRQ		; load timer IRQ status
		LDA	#RPTIME
		STA	CNTB		; set RIOT timer B to Read-Point-Time
		BNE	ENDVIA2IRQ	; and exit IRQ routine
		
CHECKKEY	JSR	RESET_TIMER2	; reset Timer2 and interrupt flags
		LDA     TICKCNT         ; load the tick counter
		BEQ     ENDVIA2IRQ      ; is it 0?
		
		DEC     TICKCNT         ; no, decrement tick counter
ENDVIA2IRQ	PLA
		TAY			; restore Y register
		PLA			; restore accumulator
		RTI

NOVIA2IRQ	PLA
		TAY			; restore Y register
	        PLA			; restore accumulator
USRIRQ		JMP	IRQ		; call user interrupt routine

; **** Reset Timer2 Routine ****************************************************
RESET_TIMER2	LDY	#T2CL		; select Timer2 lower byte register
		LDA	#$4B		; reset Timer2
		STA	(IOBASE),Y	; store timer low value
		LDA	#$41		; $411A = 16666 => 1/60 second
		INY			; select Timer2 higher byte register
VIA2IRQ_W	STA	(IOBASE),Y	; store timer high value
VIA2IRQ_X	RTS			; return

; **** VIA2 IRQ Off ************************************************************
VIA2IRQ_OFF     LDY	#IER		; select interrupt enable register
                LDA     IOBASEH
                BEQ     VIA2IRQ_X    	; IO card available? No, just exit
		LDA	#$7F		; Disable all VIA2 interrupts
		BNE	VIA2IRQ_W	; branch always

; **** VIA2 IRQ On *************************************************************
VIA2IRQ_ON	JSR	VIA2IRQ_OFF	; disable all VIA2 interrupts
                LDA     IOBASEH
                BEQ     VIA2IRQ_X    	; IO card available? No, just exit
		LDA	#$A0		; Enable interrupt for Timer2
		BNE	VIA2IRQ_W	; branch always
		
; ------------------------------------------------------------------------------
; Writes one data byte to a specific register of the MCP23017.
; Inputs: A: I2C-address of MCP23017 ($40, $42 or $44).
;         X: the register to write to
;         Y: the databyte to write into the register
; Output: C=0: Error writing byte
;         C=1: OK
; ------------------------------------------------------------------------------
MCP23017_WRITE	STX	SAVEX		; Save register address
		STY	SAVEY		; Save register data
		TAX			; X = I2C-address
		JSR	I2C_START	; Send I2C start condition (affects A and Y)
		TXA			; A now contains the address of the MCP23017 to write to
		AND	#$FE		; Make sure it is an I2C write address
		JSR	I2C_SEND	; I2C-write MCP23017 address
		BCC	MCP_WRX		; branch if C=0 (NACK)
		
		LDA	SAVEX		; Get register address
		JSR	I2C_SEND	; I2C-write register address
		BCC	MCP_WRX		; branch if C=0 (NACK)

		LDA	SAVEY		; Get databyte to write
		JSR	I2C_SEND	; I2C-write register address
MCP_WRX		JMP	I2C_STOP	; send I2C-stop and return

; ------------------------------------------------------------------------------
; Reads one data byte from a specific register of the MCP23017.
; Inputs: A: I2C-address of MCP23017 ($40, $42 or $44).
;         X: the register to read from
; Output: C=0: Error reading byte
;         C=1: A = byte read
; ------------------------------------------------------------------------------
MCP23017_READ	AND	#$FE		; Make sure it is an I2C write address
		STA	SAVEY		; Save I2C write address in SAVEY
		STX	SAVEX		; SAVEX = register address
		TAX			; X = I2C write-address
		JSR	I2C_START	; Send I2C start condition (affects A and Y)
		TXA			; A now contains the address of the MCP23017 to write to
		JSR	I2C_SEND	; I2C-write MCP23017 address
		BCC	MCP_WRX		; branch if C=0 (NACK)
		
		LDA	SAVEX		; Get register address
		JSR	I2C_SEND	; I2C-write register address
		BCC	MCP_WRX		; branch if C=0 (NACK)
		
		JSR	I2C_START	; Send I2C repeated start condition
		LDA	SAVEY		; Get I2C write address back
		ORA	#$01		; Make it an I2C read-address
		JSR	I2C_SEND	; I2C-write MCP23017 read-address
		BCC	MCP_WRX		; branch if C=0 (NACK)

		JSR	I2C_RCV		; I2C-read: receive byte 
		PHA			; Save for now
		JSR	I2C_NACK	; Send NACK (done reading)
		JSR	I2C_STOP	; send I2C-stop and return
		PLA			; Get byte read back
		SEC			; C=1: ok
		RTS			; and return

		ORG 	$F015		; maintain compatibility with previous BIOS versions

; ******************************************************************************
; START OF XMODEM CODE
; ******************************************************************************
;
; XMODEM/CRC Sender/Receiver for the 6502
;
; By Daryl Rictor Aug 2002
;
; A simple file transfer program to allow transfers between the SBC and a
; console device utilizing the x-modem/CRC transfer protocol.
;
;*******************************************************************************
; This implementation of XMODEM/CRC does NOT conform strictly to the
; XMODEM protocol standard in that it (1) does not accurately time character
; reception or (2) fall back to the Checksum mode.

; (1) For timing, it uses a crude timing loop to provide approximate
; delays.  These have been calibrated against a 1MHz CPU clock.  I have
; found that CPU clock speed of up to 5MHz also work but may not in
; every case.  Windows HyperTerminal worked quite well at both speeds!
;
; (2) Most modern terminal programs support XMODEM/CRC which can detect a
; wider range of transmission errors so the fallback to the simple checksum
; calculation was not implemented to save space.
;*******************************************************************************
;
; Files transferred via XMODEM-CRC will have the load address contained in
; the first two bytes in little-endian format:
;  FIRST BLOCK
;     offset(0) = lo(load start address),
;     offset(1) = hi(load start address)
;     offset(2) = data byte (0)
;     offset(n) = data byte (n-2)
;
; Subsequent blocks
;     offset(n) = data byte (n)
;
; One note, XMODEM send 128 byte blocks.  If the block of memory that
; you wish to save is smaller than the 128 byte block boundary, then
; the last block will be padded with zeros.  Upon reloading, the
; data will be written back to the original location.  In addition, the
; padded zeros WILL also be written into RAM, which could overwrite other
; data.
;
;*******************************************************************************
;
; Code extensions 2022 by Joerg Walke
;
; Included: CAN command in addition to ESC to cancel sending and receiving data.
; Included: EOT command to signal end of transmition.
; Included: address range for received data, to override the start address in
;           the first data block and to prevent overwriting of data by
;	    trailing zeros.

; XMODEM Receive Routine *******************************************************

XModemRcv       JSR     PrintXStart
		STA	BLKEND		; set flag to false
                LDA     #$01
                STA     BLKNO           ; set block # to 1
                STA	BFLAG           ; set flag to get address from block 1
StartRcv        LDA     #'C'            ; "C" start with CRC mode
                JSR     SOUT	     	; send it
                LDA     #$FF
                STA     RETRYH          ; set loop counter for ~3 sec delay
                LDA     #$00
                STA     CRCL
                STA     CRCH            ; init CRC value
                JSR     GetByte         ; wait for input
		BCS     GotByte         ; byte received, process it
		JMP     StartRcv
StartBlk        LDA     #$FF
                STA     RETRYH          ; set loop counter for ~3 sec delay
                JSR     GetByte         ; get first byte of block
                BCC     StartBlk        ; timed out, keep waiting...
GotByte         CMP     #ESC            ; quitting?
                BEQ     GotESC          ; yes
		CMP	#CAN		; cancel?
		BNE     GotByte1	; no
GotESC          JMP     PrintXErr       ; print error and return
GotByte1        CMP     #SOH            ; start of block?
                BEQ     BegBlk          ; yes
                CMP     #EOT            ;
                BNE     BadCRC          ; Not SOH or EOT, so flush buffer & send NAK
                JMP     RDone           ; EOT - all done!
BegBlk          LDX     #$00
GetBlk          LDA     #$FF            ; 3 sec window to receive characters
                STA     RETRYH
GetBlk1         JSR     GetData         ; get next character
                BCC     BadCRC          ; chr rcv error, flush and send NAK
GetBlk2         STA     RBUFF,x         ; good char, save it in the rcv buffer
                INX                     ; inc buffer pointer
                CPX     #$84            ; <01> <FE> <128 bytes> <CRCH> <CRCL>
                BNE     GetBlk          ; get 132 characters
                LDX     #$00
                LDA     RBUFF,x         ; get block # from buffer
                CMP     BLKNO           ; compare to expected block #
                BEQ     GoodBlk1        ; matched!
                jsr     PrintXErr       ; Unexpected block number - abort
                JMP     Flush           ; mismatched - flush buffer and return
GoodBlk1        EOR     #$FF            ; 1's comp of block #
                INX                     ;
                CMP     RBUFF,x         ; compare with expected 1's comp of block #
                BEQ     GoodBlk2        ; matched!
                JSR     PrintXErr       ; Unexpected block number - abort
                JMP     Flush           ; mismatched - flush buffer and return
GoodBlk2        JSR     CalcCRC         ; calc CRC
                LDA     RBUFF,y         ; get hi CRC from buffer
                CMP     CRCH            ; compare to calculated hi CRC
                BNE     BadCRC          ; bad crc, send NAK
                INY                     ;
                LDA     RBUFF,y         ; get lo CRC from buffer
                CMP     CRCL            ; compare to calculated lo CRC
                BEQ     GoodCRC         ; good CRC
BadCRC          JSR     Flush           ; flush the input port
                LDA     #NAK            ;
                JSR     SOUT            ; send NAK to resend block
                JMP     StartBlk        ; start over, get the block again
GoodCRC         LDX     #$02            ;
                LDA     BLKNO           ; get the block number
                CMP     #$01            ; 1st block?
                BNE     CopyBlk         ; no, copy all 128 bytes
                LDA     BFLAG           ; is it really block 1, not block 257, 513 etc.
                BEQ     CopyBlk         ; no, copy all 128 bytes
		LDA     MODE		; address mode = 0?
		BEQ	READADR         ; yes, read start address from data stream
                INX
		BNE     READDATA	; branch always
READADR         LDA     RBUFF,x         ; get target address from 1st 2 bytes of blk 1
		STA     ADRL            ; save lo address
                INX
                LDA     RBUFF,x         ; get hi address
                STA     ADRH            ; save it
READDATA        LDA	ADRL
		STA	STOL		; save start address low byte
		LDA	ADRH
		STA	STOH		; save start address high byte
		INX                     ; point to first byte of data
                DEC     BFLAG           ; set the flag so we won't get another address
CopyBlk         LDY     #$00            ; set offset to zero
CopyBlk3        LDA     BLKEND		; block end flag set?
		BNE     CopyBlk5	; yes, skip reading data
		LDA     RBUFF,x         ; get data byte from buffer
		STA     (STOL),y        ; save to target
		SEC
                LDA     NUML
                SBC     STOL            ; are we at the last address?
                BNE     CopyBlk5  	; no, inc pointer and continue
                LDA     NUMH
                SBC     STOH
                BNE     CopyBlk5
                INC     BLKEND		; yes, set last byte flag
CopyBlk5	INC     STOL            ; point to next address
                BNE     CopyBlk4        ; did it step over page boundary?
                INC     STOH            ; adjust high address for page crossing
CopyBlk4        INX                     ; point to next data byte
                CPX     #$82            ; is it the last byte
                BNE     CopyBlk3        ; no, get the next one
IncBlk          INC     BLKNO           ; done.  Inc the block #
                LDA     #ACK            ; send ACK
                JSR     SOUT
                JMP     StartBlk        ; get next block
RDone           LDA     #ACK            ; last block, send ACK and exit.
                JSR     SOUT
                JSR     Flush           ; get leftover characters, if any
                JMP     PrintXSucc

; XMODEM Send Routine **********************************************************

XModemSnd       JSR     PrintXStart
		STA     ERRCNT          ; error counter set to 0
		STA     BLKEND          ; set flag to false
		LDA     #$01
                STA     BLKNO           ; set block # to 1
Wait4CRC        LDA     #$FF            ; 3 seconds
                STA     RETRYH
                JSR     GetByte
                BCC     Wait4CRC        ; wait for something to come in...
                CMP     #'C'            ; is it the "C" to start a CRC xfer?
                BEQ     SetStoAddr      ; yes
                CMP     #ESC            ; is it a cancel? <Esc> Key
                BEQ     DoCancel        ; No, wait for another character
		CMP     #CAN            ; is it a cancel?
                BNE     Wait4CRC        ; No, wait for another character
DoCancel        JMP     PrtAbort        ; Print abort msg and exit
SetStoAddr	LDA     #$01            ; manually load blk number
                STA     RBUFF           ; into 1st byte
                LDA     #$FE            ; load 1's comp of block #
                STA     RBUFF+1         ; into 2nd byte
                LDA     ADRL            ; load low byte of start address
                STA     RBUFF+2         ; into 3rd byte
                LDA     ADRH            ; load hi byte of start address
                STA     RBUFF+3         ; into 4th byte
		LDX     #$04            ; preload X to receive buffer
		LDY     #$00            ; init data block offset to 0
                BEQ     LdBuff1         ; jump into buffer load routine
LdBuffer        LDA     BLKEND          ; was the last block sent?
                BEQ     LdBuff0         ; no, send the next one
                JMP     SDone           ; yes, we're done
LdBuff0         LDX     #$02            ; init pointers
                LDY     #$00
                INC     BLKNO           ; inc block counter
                LDA     BLKNO
                STA     RBUFF           ; save in 1st byte of buffer
                EOR     #$FF
                STA     RBUFF+1         ; save 1's comp of blkno next
LdBuff1         LDA     (ADRL),y        ; save 128 bytes of data
                STA     RBUFF,x
LdBuff2         SEC
                LDA     NUML
                SBC     ADRL            ; are we at the last address?
                BNE     LdBuff4         ; no, inc pointer and continue
                LDA     NUMH
                SBC     ADRH
                BNE     LdBuff4
                INC     BLKEND          ; yes, set last byte flag
LdBuff3         INX
                CPX     #$82            ; are we at the end of the 128 byte block?
                BEQ     SCalcCRC        ; yes, calc CRC
                LDA     #$00            ; fill rest of 128 bytes with $00
                STA     RBUFF,x
                BEQ     LdBuff3         ; branch always
LdBuff4         INC     ADRL            ; inc address pointer
                BNE     LdBuff5
                INC     ADRH
LdBuff5         INX
                CPX     #$82            ; last byte in block?
                BNE     LdBuff1         ; no, get the next
SCalcCRC        JSR     CalcCRC
                LDA     CRCH            ; save hi byte of CRC to buffer
                STA     RBUFF,y
                INY
                LDA     CRCL            ; save lo byte of CRC to buffer
                STA     RBUFF,y
Resend          LDX     #$00
                LDA     #SOH
                JSR     SOUT            ; send SOH
SendBlk         LDA     RBUFF,x         ; send 132 bytes in buffer to the console
                JSR     SOUT
                INX
                CPX     #$84            ; last byte?
                BNE     SendBlk         ; no, get next
                LDA     #$FF            ; yes, set 3 second delay
                STA     RETRYH          ; and
                JSR     GetByte         ; wait for ACK/NACK
                BCC     SetError        ; no char received after 3 seconds, resend
                CMP     #ACK            ; char received... is it:
                BEQ     LdBuffer        ; ACK, send next block
                CMP     #NAK
                BEQ     SetError        ; NAK, inc errors and resend
                CMP     #ESC
                BEQ     PrtAbort        ; ESC pressed to abort
		CMP	#CAN
		BEQ     PrtAbort	; CANCEL send
					; fall through to error counter
SetError        INC     ERRCNT          ; inc error counter
                LDA     ERRCNT
                CMP     #$0A            ; are there 10 errors? (Xmodem spec for failure)
                BNE     Resend          ; no, resend block

PrtAbort        JSR     Flush           ; yes, too many errors, flush buffer,
                JMP     PrintXErr       ; print error msg and exit
SDone           JMP     PrintXSucc   	; All Done..Print msg and exit

; Get Data From Serial Port ****************************************************

GetData		LDA     #$00            ; wait for chr input and cycle timing loop
                STA     RETRYL          ; set low value of timing loop
LoopGetData     JSR     SIN        	; get chr from serial port, don't wait
                BCS     EndGetData      ; got one, so exit
                DEC     RETRYL          ; no character received, so dec counter
                BNE     LoopGetData
                DEC     RETRYH          ; dec hi byte of counter
                BNE     LoopGetData     ; look for character again
                CLC                     ; if loop times out, CLC, else SEC and return
EndGetData      RTS                     ; with character in A

; Get Byte From Serial Port. Check if ESC pressed ******************************

GetByte		LDA     #$00            ; wait for chr input and cycle timing loop
                STA     RETRYL          ; set low value of timing loop
LoopGetByte     LDA     #< SIN        ; check low byte of serial in address
		CMP	STDIN	        ; is Low(stdin) = Low(SIN)?
                BNE     GetChar         ; no, use standard Get Char Routine
                LDA     #> SIN       ; yes, check high byte of serial in address
                CMP     STDIN+1         ; is High(stdin) = High(SIN)?
                BEQ	ReadByte	; yes, just read input stream
GetChar		JSR	CGET
		BCC	ReadByte
		CMP	#ESC
		BNE	ReadByte
		SEC
		BCS	EndGetByte
;		JSR	CHKESC		; no, check stdin if ESC key pressed
;		BCC	ReadByte	; no ESC pressed, read data byte from serial port
;		LDA	#ESC
;		BNE     EndGetByte      ; ESC pressed, so exit
ReadByte	JSR     SIN        	; get chr from serial port, don't wait
                BCS     EndGetByte      ; got one, so exit
                DEC     RETRYL          ; no character received, so dec counter
                BNE     LoopGetByte
                DEC     RETRYH          ; dec hi byte of counter
                BNE     LoopGetByte     ; look for character again
                CLC                     ; if loop times out, CLC, else SEC and return
EndGetByte      RTS                     ; with character in A

; Empty Buffer *****************************************************************

Flush           LDA     #$1C            ; flush receive buffer
                STA     RETRYH          ; flush until empty for ~1/4 sec.
Flush1          JSR     GetData         ; read the port
                BCS     Flush           ; if char received, wait for another
                RTS

; Calculate CRC ****************************************************************

CalcCRC		LDA	#$00		; calculate the CRC for the 128 bytes
		STA	CRCL
		STA	CRCH
		LDY	#$02
CalcCRC1	LDA	RBUFF,y
		EOR 	CRCH 		; Quick CRC computation with lookup tables
       		TAX		 	; updates the two bytes at crc & crc+1
       		LDA 	CRCL		; with the byte send in the "A" register
       		EOR 	CRCHI,x
       		STA 	CRCH
      	 	LDA 	CRCLO,x
       		STA 	CRCL
		INY
		CPY	#$82		; done yet?
		BNE	CalcCRC1	; no, get next
		RTS			; y=82 on exit

; Print XModem Messages ********************************************************

PrintXStart     SEI			; disable interrupts during XModem transfer
		JSR	Flush		; clear buffer
		LDY     #$00		; load start message
		BEQ	PrintXMsg

PrintXErr       JSR	BEEP
PrintXError	LDY     #(ERRX-MSGX)	; load error message
		CLC
		BNE     PrintXEnd

PrintXSucc      LDY     #(SUCCX-MSGX)	; load success message
		SEC
PrintXEnd	CLI			; enable interrupts

PrintXMsg	LDA     #$00
		ROL			; save carry
		PHA
PrintXMsg1	LDA  	MSGX,Y   	; load char at string pos y
		BEQ  	EndXMsg  	; exit, if NULL char
		JSR  	COUT       	; write character
		INY             	; next index
		BNE  	PrintXMsg1
EndXMsg		PLA
		LSR			; restore carry and leave A = 0
		RTS

; ******************************************************************************
; String Data Section
; ******************************************************************************
MSGX            .by     CR 'Begin data transfer, <ESC> to cancel. ' $00
ERRX		.by	CR 'Transfer Error' CR $00
SUCCX           .byte	EOT,EOT,EOT
MSG_OK		.by	CR 'OK' CR $00
					; Tape Messages removed 25-07-'25 Emile
		ORG	*+120		; maintain compatibility with v1.1.4
		
; **** IRQ, NMI and BREAK Service Routines *************************************

; ******************************************************************************

IRQ		STA	STOACC		; save current accumulator
		PLA			; get current processor status in A
		PHA			; and push it back to stack
		AND	#$10		; mask break flag
		BNE	USRBREAK	; if break flag set, jump to user break handler
		LDA	STOACC
		JMP	(IRQUSR)	; else jump to clock IRQ routine

USRBREAK	LDA	STOACC
		JMP	(BRKUSR)

NMI		STA	ACC		; save current accumulator

BREAK					; default IRQUSR & BRKUSR entry
		PLA			; get current processor status in A
		STA	PREG		; save it
		PHA			; and push it back to stack
		STX	XREG		; save x-register
		STY	YREG		; save y-register
		JSR	RESET_STDIO	; always reset to standard I/O
		PLP			; get last processor status
		PLA			; get last program counter low byte
		STA	PCL		; and store it
		STA	ADRL
		PLA			; get last program counter high byte
		STA	PCH		; and store it
		STA	ADRH
		TSX			; get current stack pointer
		STX	SPUSER		; and store it
		CLD			; set binary mode
		JSR	BEEP		; error beep
		JSR	PRSTATUS	; print user program status
		LDX     #$FF
		TXS			; initialize stack pointer
		CLI			; enable interrupts
		JMP	MONRESET	; and return to monitor

					; GETMAGIC removed 25-07-'25 Emile
		ORG	*+15		; maintain compatibility with v1.1.4

; **** Write To Serial Routine *************************************************

; Input: A - Output Byte to RS232

; ******************************************************************************

SOUT
SERIALOUT	PHP			; save processor status
		SEI			; disable interrupts
		PHA			; save character
		LDA  	#$10
EMPTY?		BIT  	STAT_REG	; ACIA output register empty?
		BEQ  	EMPTY?		; no, check again.
		PLA			; restore character
		STA  	DATA_REG   	; write character to ACIA
		PLP			; restore processor status
		RTS

; **** Read From Serial Routine ************************************************

; Output: A - Input Byte from RS232
;         C - 1 char get, 0 no char get

; ******************************************************************************

SIN
SERIALIN	CLC              	; set to no chr present
		LDA	STAT_REG
		AND	#$08		; ACIA input register full?
		BEQ	SERIALEND	; no, just exit
		LDA	DATA_REG	; yes, read character
		SEC		 	; and set C = 1, char present
SERIALEND	RTS

; **** Read From ASCII Keyboard Routine ****************************************

; Output: A - Input Byte from Keyboard
;         C - 1 char get, 0 no char get

; ******************************************************************************

ASCIIKBD	LDA	PADD		; are we in read mode?
		BEQ	READMODE	; yes, check if data available
		JSR	SETPPORTIN	; no, first set parallel port as an input
READMODE	CLC			; set to no char present
		BIT	WRDC		; test PA7 (DATA_AVAIL)
		BVC	NODATA		; no new data, just exit with C = 0
		LDA	WRDC		; clear PA7 flag
		LDA	PAD		; load keyboard ASCII code from port A
		AND	#%01111111	; clear MSB
DATA_AVAIL	SEC			; and set C = 1, char present
NODATA		RTS

; **** PS2 Keyboard Driver Routine *********************************************

; Output: A - Input Byte from Keyboard
;         C - 1 char get, 0 no char get

; ******************************************************************************

PS2KBD          CLC                     ; set to no char present
                STY     PREG            ; save current Y register
                LDY     #PIA_PORTC
                LDA     (FGCBASE),Y     ; load data from Port C
                AND     #$20            ; and check Strobe line
                BEQ     PS2_NODATA      ; no data received, just exit with C = 0
                LDY     #PIA_PORTA
                LDA     (FGCBASE),Y     ; data received, load it from Port A
                BNE     PS2_DATA_AVAIL
                LDY     #PIA_PORTC      ; NULL Byte received, check for second byte
PS2_CHECK       LDA     (FGCBASE),Y     ; load data from Port C
                AND     #$20            ; and check Strobe line
                BEQ     PS2_CHECK       ; no data received, repeat
                LDY     #PIA_PORTA
                LDA     (FGCBASE),Y     ; data received, load it from Port A
                ORA     #$80            ; set bit 7
PS2_DATA_AVAIL  SEC			; and set C = 1, char present
PS2_NODATA      LDY     PREG            ; restore Y register
                RTS

; **** Detect ASCII Keyboard Routine *******************************************

; ******************************************************************************

DETECT_ASCIIKBD JSR	SETPPORTIN	; set parallel port as an input
		LDA	PAD		; read parallel port
		CMP	#$FF		; is there anything connected?
		BEQ	NOKBD		; no, just exit
                LDX     #<  KEYBD_DEV
                LDY     #> KEYBD_DEV
                JSR     DEV_ADD         ; add ASCII keyboard driver
		STA	STDINDEV	; make it the standard input device
NOKBD		RTS

; **** Write To Parallel Port Routine ******************************************

; Input: A - Output Byte to parallel port

; ******************************************************************************

PPORTOUT	PHA			; save character
		LDA	#$BE		; initialize handshake line I/O on port b
		CMP	PBDD		; already initialized?
		BEQ	SETHSK		; yes, just set output values
		STA	PBDD		; no, PB7 = /strobe, PB6 = busy, PB5 = r/w, PB0 = speaker off
SETHSK		LDA	#$86		; set handshake lines to their initial values
		STA	PBD		; r/w = L, strobe = H, PB1,PB2 = H -> hex-kbd disabled; speaker = H
		LDA	#$FF		; all port A lines are outputs
		STA	PADD
		PLA			; reload character in A
		PHA
		STA	PAD		; set output data
PPORTBSY?	BIT	PBD		; bussy line is high?
		BVS	PPORTBSY?	; yes, check bussy line again
		LDA	#$06		; generate strobe pulse
		STA	PBD		; set strobe line low
		LDA	#$86
		STA	PBD		; set strobe line high
		PLA			; restore character
		RTS

; **** Read From Parallel Port Routine *****************************************

; Output: A - Input Byte from parallel port
;         C - 1 char get, 0 no char get

; ******************************************************************************

PPORTIN		JSR	SETPPORTIN	; set parallel port as input
		CLC
		BIT	PBD		; check if /STROBE = 0
		BMI	NOSTROBE	; no, just exit with C = 0
STROBE?		BIT	PBD		; yes, wait for strobe to come high again
		BMI	STROBE?
		LDA	PAD		; load data from port A
		SEC			; and set C = 1, data present
NOSTROBE	RTS

; **** Switch Parallel Port To Data Input **************************************

; ******************************************************************************

SETPPORTIN	LDA	#$00		; initialize port A as input
		STA	PADD
		LDA	#$3E		; initialize port B bits for read operation
		STA	PBDD
		LDA	#$26		; set PB5 = H (READ)
		STA	PBD
		STA	WRDC		; set PA7 raising edge detection, no interrupt
		LDA	WRDC		; clear interrupt flag
		RTS

; ******************************************************************************
; SPI Driver
; ******************************************************************************

; ******************************************************************************
; Initialize SPI Interface
; ******************************************************************************

SPI_INIT					;fall trough to SPI_SLOW

; ******************************************************************************
; Set SPI to Slow Mode (250KHz)
; ******************************************************************************

SPI_SLOW        LDA	#$04
		LDY	#ACR
		STA	(IOBASE),Y		; set VIA mode "shift in under T2 control"
		LDA	#$00			; reset Timer2
		LDY	#T2CL
		STA	(IOBASE),Y		; store timer low value
		JSR	SPI_RESET		; flush shift register
		RTS				; Clock is set to 250 kHz
		
; ******************************************************************************
; Set SPI to Fast Mode (500KHz)
; ******************************************************************************

SPI_FAST	LDA	#$08
		LDY	#ACR
		STA	(IOBASE),Y		; set VIA mode "shift in under phi2 control"
		RTS				; Clock is set to 500 kHz

; ******************************************************************************
; Write a Single Byte to the SPI Interface
;
; Input: A = Byte to Send
; ******************************************************************************

SPI_WRITE	STY	YSAV
		LDY	#PORTA
		STA	(IOBASE),Y		; output data to shift register
		LDY	#IFR
SPI_WRITE1	LDA	#$04			; set bit mask for data available flag
		AND	(IOBASE),Y		; shift register full?
		BEQ	SPI_WRITE1		; no, check again
		LDY	#PORTB
		LDA	#$42 			; SPI_CS = L; LOAD_DATA = 0
		STA	(IOBASE),Y		; load data into shift register
		LDA	#$4A 			; SPI_CS = L; LOAD_DATA = 1
		STA	(IOBASE),Y		; data is now in shift register
		BNE	SPI_RESET               ; branch always

; ******************************************************************************
; Read a Single Byte from the SPI Interface
;
; Output: A = Received Byte
; ******************************************************************************

SPI_READ	STY	YSAV
		LDY	#IFR
SPI_READ1	LDA	#$04			; set bit mask for data available flag
		AND	(IOBASE),Y		; shift register full?
		BEQ	SPI_READ1		; no, check again
SPI_RESET	LDY	#SR
		LDA	(IOBASE),Y		; start next shifting, clear data available flag
		LDY	YSAV
		RTS

; ******************************************************************************
; SD-Card Driver Routines
; ******************************************************************************

; ******************************************************************************
; Initialize SD-Card
; Output: C = 1 Init OK, C = 0 Error
; ******************************************************************************

SD_INIT		SEI                             ; disable interrupts
                LDA	#$00
		STA	SD_TYPE
		JSR	SD_RESET		; reset SD-Card
		CMP	#$01			; SD-Card present?
		BNE	SDC_NOT_FOUND		; invalid response, no usable card found
		JSR	SD_GET_VERS		; get SD-Card version
		CMP	#$05			; seems to be a version 1 card
		BEQ	INIT_SD0		; so just try to initialize it
		CMP	#$AA			; version 2 cards should response with $(01)AA
		BNE	SDC_NOT_FOUND		; invalid response, no usable card found
		LDA	#$40			; try ACMD41($40000000) init (SD Ver. 2+)
		BNE	INIT_SD1
INIT_SD0	LDA	#$00			; try ACMD41($00000000) init (SD Ver. 1)
INIT_SD1	JSR	SD_CLEAR_CMD		; prepare for new command
		STA	SD_PB3
INIT_SD2	LDA	#CMD55			; send prefix CMD55 (application cmd)
		JSR	SD_SEND_CMD
		CMP	#$01
		BNE	SDC_NOT_FOUND		; invalid response, no usable card found
		LDA	#ACMD41			; send ACMD41 (initialize)
		JSR	SD_SEND_CMD
		BEQ	INIT_SD3		; response = 0 means card waked up,
		CMP	#$01			; card still idle?
		BEQ	INIT_SD2		; yes, try again
		BNE	SDC_NOT_FOUND		; no, invalid response, no usable card found
INIT_SD3	LDA	SD_PB3			; Ver. 2+ Card?
		BEQ	INIT_SD4		; no, just set block size
		JSR	SD_CLEAR_CMD		; prepare for new command
		LDA	#CMD58			; send CMD58 (get OCR)
		JSR	SD_SEND_CMD
		BNE	SDC_NOT_FOUND		; invalid response, no usable card found
		JSR	SD_WAIT_RESP3		; wait for OCR response
		LDA	SD_PB3			; Test Bit 30
		AND	#$40			; 1 if SDHC/SDXC card, 0 else
		STA	SD_TYPE			; set type $00 Byte mode, $40 LBA mode
INIT_SD4	JSR	SD_CLEAR_CMD		; prepare for new command
		LDA	#$02			; set blocksize to 512 byte
		STA	SD_PB1
		LDA	#CMD16			; send CMD16 (set block size)
		JSR	SD_SEND_CMD
		BNE	SDC_NOT_FOUND		; invalid response, no usable card found
		JSR	SPI_FAST		; and switch to SPI fast mode (500kHz)
		CLI                             ; reenable interrupts
		SEC				; everything gone well, set carry
		RTS
SDC_NOT_FOUND	LDA	#$80
                CLI                             ; reenable interrupts
		CLC				; something went wrong, clear carry
		RTS				; to signal error

; ******************************************************************************
; Get SD-Card Version
; ******************************************************************************

SD_GET_VERS	LDA	#$01			; set parameter byte 1
		STA	SD_PB1
		LDA	#$AA			; set parameter byte 0
		STA	SD_PB0
		LDA	#$87			; set crc
		STA	SD_CRC
		LDA	#CMD8			; send CMD8($000001AA) (get version)
		JSR	SD_SEND_CMD		; response should be $01
		CMP	#$01			; SD-Card present?
		BNE	END_GET_VERS		; no, exit with result <> $01
						; yes, fall through to sd_wait_resp

; ******************************************************************************
; Wait for a 32 Bit Command R3 Response from SD-Card
; ******************************************************************************

SD_WAIT_RESP3	LDY	#$00
READ_RESP3	JSR	SD_WAIT_RESP		; yes, receive 4 response bytes
		STA	SD_PB3,Y		; store response bytes in PB0..3
		INY
		CPY	#$04
		BNE	READ_RESP3
END_GET_VERS	RTS

; ******************************************************************************
; Clear SD-Card Command Parameters
; ******************************************************************************

SD_CLEAR_CMD	LDA	#$00
		LDY	#$04			; 4 parameter bytes to clear
NEXT_PARAM	STA	SD_CMD,Y		; clear parameter byte
		DEY
		BNE	NEXT_PARAM		; more to clear?
		LDA	#$FF
		STA	SD_CRC			; no, finally set CRC byte to $FF
		RTS

; ******************************************************************************
; Send Command to SD-Card
; Input: A = Command Index
; ******************************************************************************

SD_SEND_CMD	STA	SD_CMD
		JSR	SPI_READ		; send one dummy
		LDX	#$00
SEND_BYTE	LDA	SD_CMD,X		; get one command byte
		JSR	SPI_WRITE		; and send it
		INX
		CPX	#$06			; all 6 cmd bytes send?
		BNE	SEND_BYTE		; no, send more bytes
						; yes, fall through to sd_wait_resp

; ******************************************************************************
; Wait for a 8 Bit Command R1 Response from SD-Card
; Output: A = Response Byte
; ******************************************************************************

SD_WAIT_RESP	LDX	#$08			; wait for max 8 cycles
READ_RESP1	JSR	SPI_READ		; receive data
		CMP	#$FF			; is it a $FF?
		BNE	RESPONSE		; no, card did response
		DEX				; yes, try again
		BNE	READ_RESP1		; check for timeout
RESPONSE	TAX
		TXA				; set proper status flags for A
		RTS

; ******************************************************************************
; Wait for a Special Token Response from SD-Card
; Input:  A = Token Byte
; Output: A = Response Byte
; ******************************************************************************

SD_WAIT_TOKEN	STA	TEMP			; store token into TEMP variable
		LDY	#$FF			; load low byte of time out counter
		LDX	#$0A			; load high byte of time out counter
WAIT_RESP	JSR	SPI_READ		; read byte from SPI
		DEY				; decrement wait counter
		BNE	WAIT_RESP0
		DEX
		BEQ	WAIT_RESP_END		; wait counter is 0 -> time out
WAIT_RESP0	CMP	TEMP			; did we read the token we are waiting for?
		BNE	WAIT_RESP		; no, read next byte
WAIT_RESP_END	RTS

; ******************************************************************************
; Read Single Data Block to Std. Block Buffer
; Input:  SD_PB3..SD_PB0 = 32 Bit Command Block Source Address
; Output: C = 0 Error, C = 1 Read OK
;	  A = Error Code
; ******************************************************************************

SD_RD_BLK_BUF	JSR	INIT_BLKBUF		; set pointer to block buffer
		BEQ	SD_RD_BLK

; ******************************************************************************
; Read Single Data Block from Logical Address to Std. Block Buffer
; Input:  X,Y = Ptr[LO:HI] to 32 Bit LBA Source Address
; Output: C = 0 Error, C = 1 Data OK
;	  A = Error Code
; ******************************************************************************

SD_RD_LBLK_BUF	JSR	INIT_BLKBUF		; set pointer to block buffer
						; fall through to sd_rd_lblk

; ******************************************************************************
; Read Single Data Block from Logical Address
; Input:  X,Y = Ptr[LO:HI] to 32 Bit LBA Source Address
;	  BLKBUF,BLKBUFH = 16 Bit Destination Address
; Output: C = 0 Error, C = 1 Data OK
;	  A = Error Code
; ******************************************************************************

SD_RD_LBLK	JSR	LOAD_LBA_SD		; convert LBA CMD ADR
						; fall through to sd_rd_blk

; ******************************************************************************
; Read Single Data Block
; Input:  SD_PB3..SD_PB0 = 32 Bit Command Block Source Address
;         BLKBUF,BLKBUFH = 16 Bit Destination Address
; Output: C = 0 Error, C = 1 Read OK
;	  A = Error Code
; ******************************************************************************

SD_RD_BLK	LDA	#CMD17			; send CMD17 (blk read)
		JSR	SD_SEND_BLK_CMD
		JSR	SD_WAIT_TOKEN		; wait for data token $FE
		CMP	#$FE			; is card ready for block read?
		CLC
		BNE	SD_RD_END		; did not receive data token, exit with C = 0
		LDX	#$01			; initialize page counter
		LDY	#$00			; initialize byte counter
SD_RD_BLK0	STY	YSAV			; read a byte
		LDY 	#SR
		LDA	(IOBASE),Y
		LDY	YSAV
		STA	(BLKBUF),Y		; and store it into the block buffer
		INY				; increment destination pointer
		BNE	SD_RD_BLK0		; pointer overflow? No, read next byte
		INC	BLKBUFH			; yes, increment block buffer page
		DEX
		BPL	SD_RD_BLK0		; two pages read? no, read next byte
SD_RD_BLK1	JSR	SPI_READ		; yes, read 3 more bytes (CRC H, CRC L, dummy)
		INY
		CPY	#$03			; all 3 bytes read?
		BNE	SD_RD_BLK1		; no, read next byte
		SEC				; yes, all data read, set C = 1
SD_RD_END	RTS

; ******************************************************************************
; Write Single Data Block from Std. Block Buffer
; Input:  SD_PB3..SD_PB0 = 32 Bit Command Block Destination Address
; Output: C = 0 Error, C = 1 Read OK
;	  A = Error Code
; ******************************************************************************

SD_WR_BLK_BUF	JSR	INIT_BLKBUF		; set pointer to block buffer
		BEQ	SD_WR_BLK

; ******************************************************************************
; Write Single Data Block from Std. Block Buffer to Logical Address
; Input:  X,Y = Ptr[LO:HI] to 32 Bit LBA Destination Address
; Output: C = 0 Error, C = 1 Data OK
;	  A = Error Code
; ******************************************************************************

SD_WR_LBLK_BUF	JSR	INIT_BLKBUF		; set pointer to block buffer
						; fall through to sd_rd_lblk

; ******************************************************************************
; Write Single Data Block to Logical Address
; Input:  X,Y = Ptr[LO:HI] to 32 Bit LBA Destination Address
;	  BLKBUF,BLKBUFH = 16 Bit Source Address
; Output: C = 0 Error, C = 1 Data OK
;	  A = Error Code
; ******************************************************************************

SD_WR_LBLK	JSR	LOAD_LBA_SD		; convert LBA CMD ADR
						; fall through to sd_rd_blk

; ******************************************************************************
; Write Single Data Block
; Input:  SD_PB3..SD_PB0 = 32 Bit CommandBlock Destination Address
;	  BLKBUF,BLKBUFH = 16 Bit Source Address
; Output: C = 0 Error, C = 1 Write OK
;	  A = Error Code
; ******************************************************************************

SD_WR_BLK	LDA	#CMD24			; send CMD24 (blk write)
		JSR	SD_SEND_BLK_CMD
		JSR	SPI_WRITE		; write data token
		LDX	#1			; initialize page counter
		STX	YSAV
		DEX				; initialize byte counter
SD_WR_BLK0	TXA
		TAY
		LDA	(BLKBUF),Y		; read next byte from buffer
		LDY	#PORTA			; and write it to the card
		STA	(IOBASE),Y		; output data to shift register
		DEY				; set for PORTB
		LDA	#$42 			; SPI_CS = L; LOAD_DATA = 0
		STA	(IOBASE),Y		; load data into shift register
		LDA	#$4A 			; SPI_CS = L; LOAD_DATA = 1
		STA	(IOBASE),Y		; data is now in shift register
		LDY 	#SR
		LDA	(IOBASE),Y		; and start clk'ing
		INX				; increment source pointer
		BNE	SD_WR_BLK0		; pointer overflow? No, write next byte
		INC	BLKBUFH			; yes, increment block buffer page
		DEC	YSAV
		BPL	SD_WR_BLK0		; two pages written? no, write next byte
		JSR	SPI_READ		; yes, send a (dummy) CRC ($FFFF)
		JSR	SPI_READ
		JSR	SPI_READ		; read one dummy byte
		JSR	SPI_READ		; read response byte
                PHA                             ; and save it onto the stack
SD_WR_BUSY?	JSR	SPI_READ		; read next byte
		CMP	#0
		BEQ	SD_WR_BUSY?		; check if busy ($00)
		PLA
		AND	#$1F			; mask result bits
		CMP	#$05			; data accepted?
		CLC
		BNE	SD_WR_END		; no, exit with C = 0
		SEC				; yes, exit with C = 1
SD_WR_END	RTS

; ******************************************************************************
; Send Block Read or Write Command
; Input :  A = Command (CMD17,CMD24)
; Output : A = Data Token
; ******************************************************************************

SD_SEND_BLK_CMD	JSR	SD_SEND_CMD
		BNE	SD_RESP_ERR		; response <> 0 check error type
		LDA	#DATA_TOKEN
		RTS

; ******************************************************************************
; Check Error
; ******************************************************************************

SD_RESP_ERR	AND	#$01			; is card in idle mode?
		BEQ	SD_DISK_RW		; no, print error
		JSR	SPI_SLOW		; set SPI slow mode
		JSR	SD_INIT			; yes, maybe card changed, reset
		BCS	SD_DISK_CHNG
SD_NO_DISK	LDA	#$80
		RTS
SD_DISK_RW	LDA	#$81
		CLC
		RTS
SD_DISK_CHNG	LDA	#$82
		CLC
		RTS

; ******************************************************************************
; Reset SD-Card
; ******************************************************************************

SD_RESET	JSR	SD_CLEAR_CMD		; clear command parameters
		LDA	#$95
		STA	SD_CRC			; and set crc to $95 for CMD0
		JSR	SD_PREPARE		; send dummy sequence to SD-Card
		BNE	RESET_SDC		; is MISO line high?
		LDA	#CMD0			; no, send CMD0 (reset) to SD-Card
		JSR	SD_SEND_CMD
		JSR	SD_PREPARE		; send init dummy sequence again
		BEQ	END_SD_RESET		; MISO still low? Exit with A = $FF
RESET_SDC	LDA	#CMD0			; send CMD0 (reset) to SD-Card
		JMP	SD_SEND_CMD		; response should be $01

END_SD_RESET	LDA	#$FF			; reset failed
		RTS

; **** Prepare SD-Card for Communication ***************************************
;
; ******************************************************************************

SD_PREPARE	JSR	SPI_SLOW		; set SPI slow mode
		LDY	#PORTB			; initialize VIA Port B
		LDA	#$4E			; set /SPI_CS = H and /SPI_LOAD = H
		STA	(IOBASE),Y
		LDX	#10			; first send 80 clocks to SD-Card
SEND_CLOCK	JSR	SPI_READ		; send 8 clock cycles
		DEX
		BNE	SEND_CLOCK		; send more clock cycles
		TAX
		LDY	#IFR
SD_PREPARE1	LDA	#$04
		AND	(IOBASE),Y
		BEQ	SD_PREPARE1
		LDY	#PORTB
		LDA	#$4A			; set /SPI_CS = L and /SPI_LOAD = H
		STA	(IOBASE),Y
		TXA				; set proper status flags
SD_END		RTS

; **** SD-Card Boot Routine ****************************************************
;
; ******************************************************************************
SD_BOOT         JSR	SD_CLEAR_CMD
		JSR	SD_RD_BLK_BUF           ; read MBR
                BCC     SD_END                  ; error reading MBR. Exit

		JSR	LOAD_RUN_PART		; Load MBR and Volume ID
		BCC	SD_END			; branch if error
		
		LDY     #SDCDEV-STRINGP2        ; load pointer to device name
                JMP	DISP_DVC		; display device-name and return
	
;----------------------------------------------------------------------------------		
; This routine is the same for both the CF and SD cards. It does the following:
; - Load the MBR (sector 0) and does check for $55 $AA and $65 $02
; - It runs the boot-menu routine in the MBR
; - It loads the begin-LBA of the selected partition
; - It loads the Volume ID (first sector) of the partition
; Exit: C=0: Error, C=1: OK
;----------------------------------------------------------------------------------		
LOAD_RUN_PART   JSR     SYS_MBR_ID              ; check boot block ID tag
                BCC     SD_END                  ; error, wrong ID. Exit
                LDA     PART0-2                 ; check if partition ID1 is $65
                CMP     #$65
                BNE     LOAD_PART0              ; no, just load partition 0
                LDA     PART0-1                 ; check if partition ID2 is $02
                CMP     #$02
                BNE     LOAD_PART0              ; no, just load partition 0
                JSR     MBR                     ; partition ID $65 $02 found. Call MBR code
                BNE     LOAD_PART1              ; is boot menu result 1,2,3, or 4 ?
                CLC                             ; no, ESC pressed or no valid partition found
LRP_END         RTS                             ; abort booting from SD-Card

LOAD_PART1      DEX                             ; set result to 0,1,2 or 3
                TXA                             ; transfer result to Accu
                TAY                             ; and to Y-Register
                ASL                            ; multiply result by 16
                ASL     
                ASL     
                ASL     
                ORA     #$08                    ; and add 8
                TAX                             ; move partition table index into X
                TYA
                CLC
                ADC     #49                     ; convert partition number to ASCII char (+1)
                STA     PSAV                    ; and store it to PSAV
                BNE     LOAD_PART               ; branch always
LOAD_PART0      LDX     #$08                    ; for partition 0 the table index is 8
                LDA     #'1'                    ; partition 0 number as ASCII char (+1)
                STA     PSAV                    ; store it in PSAV
                LDA     PART0                   ; read boot indicator
                BEQ     SYS_MSG_ERR             ; if $00 then exit
LOAD_PART       LDY     #$08
SD_BOOT1        LDA     PART0_RS,X              ; load partition start and length
                STA     BOOT_PART,Y             ; and save it to boot device descriptor
                DEX
                DEY
                BPL     SD_BOOT1
                LDX	#< BOOT_PART            ; read partition boot blk ptr
		LDY	#> BOOT_PART
		JSR     SYS_LD_BOOTBLK          ; load partition boot block
                BCC     LRP_END                 ; block not found. Exit
                JMP     SYS_CHECK_OS            ; check OS OEM string C=0: wrong OEM string. And return
		
;----------------------------------------------------------------------------
; This routine prints a string to the terminal: A=LSB, Y=MSB
;----------------------------------------------------------------------------
SPRINT		STX 	PRSTR	    	; LSB of text-pointer
		STY 	PRSTR+1	    	; MSB of text-pointer
		JSR 	SPROUT	    	; BIOS print string routine
		RTS
		NOP			; maintain v1.1.4 compatibility
		
; ******************************************************************************
; Initialize Block Buffer Pointer, it must return with A = 0.
; ******************************************************************************

INIT_BLKBUF	LDA	#> BLOCK_BUF         ; set pointer to standard block buffer
		STA	BLKBUFH
		LDA	#$00
		STA	BLKBUF
		RTS

; ******************************************************************************
; Load Logical Block Address into SD-card Command Address.
; Swap Endian and Shift Bits if Desired
; Input:  X,Y = Ptr[LO:HI] to 32 Bit LBA Address
; Output: ADR in SD_PB3..SD_PB0
; ******************************************************************************

LOAD_LBA_SD	STX	PLBAL
		STY	PLBAH
		LDX	#$04
		LDY	#$00
		LDA	SD_TYPE
		BNE	BLK_MODE
		CLC
		TYA
                STA	SD_CMD,X
		DEX
BIT_MODE	LDA	(PLBA),Y
		ROL	
                STA	SD_CMD,X
		INY
		DEX
		BNE	BIT_MODE
		RTS
BLK_MODE	LDA	(PLBA),Y
		STA	SD_CMD,X
		INY
		DEX
		BNE	BLK_MODE
		RTS

; ******************************************************************************
; Boot Routines
; ******************************************************************************

; **** Main Boot Routine *******************************************************
;
; Find first bootable device
; Output : C = 0 No Boot Device Found
;          C = 1 Boot Device Found. Boot Code at $0600 Available
;
; ******************************************************************************

SYS_BOOT        LDY     #STORAGE_DEV            ; boot from storage device only
SYS_BOOT1       STY     YREG
                TYA
                JSR     DEV_OPEN                ; open device descriptor
                BCC     SYS_BOOT2               ; device not found, try next one
                LDA     #CMD_INIT
                JSR     CMDDEV                  ; initialize device
                BCC     SYS_BOOT2               ; could not initialize, try next one
                LDA     #CMD_BOOT
                JSR     CMDDEV                  ; can we boot from device?
                BCS     SYS_BOOT_END            ; yes, exit
SYS_BOOT2       LDY     YREG                    ; no, try next device
                INY
                CPY     #$2F                    ; all devices checked?
                BNE     SYS_BOOT1               ; no, try next one
                LDY     #NOBOOTDEV-STRINGP2     ; yes, no boot device found

; ***** Show System Message ****************************************************
;
; Input:  Y - Index To Message String
; Output: C = 0
;
; ******************************************************************************

SYS_MSG         JSR     LOADSTRING2
                JSR  	WRSTR                   ; show error message
SYS_MSG_ERR     CLC
SYS_MSG_END     RTS

; ***** Finalize Boot Procedure ************************************************

SYS_BOOT_END    LDA     #$B0                    ; boot block could be loaded
                STA     $0600                   ; modify jump opcode in boot block into BCS
                RTS

; ***** Load Boot Block From Device ********************************************
;
; Input:  X - Pointer to Boot Block Low Address
;         Y - Pointer to Boot Block High Address
; Output: C = 0 No Boot Block Found
;         C = 1 Boot Block Loaded at $0600
;
; ******************************************************************************

SYS_LD_BOOTBLK  LDA     #CMD_READ_BUF
                JSR     CMDDEV                   ; load master boot block
                BCC     SYS_TAG_ERR

; ***** Check Boot Block ID Tag ($55 $AA) **************************************
;
; Output: C = 0 No Boot Block Tag Found
;         C = 1 Boot Block Tag Found
;
; ******************************************************************************

SYS_MBR_ID      LDA     BOOTBLK_TAG             ; check boot block ID tag
                CMP     #$55
                BNE     SYS_TAG_ERR
                LDA     BOOTBLK_TAG+1
                CMP     #$AA
                BNE     SYS_TAG_ERR
                SEC
                RTS
SYS_TAG_ERR     CLC
                RTS

; ***** Check OS OEM String ****************************************************
;
; Output: C = 0 OS OEM String Not Found
;         C = 1 OS OEM String Found
;
; ******************************************************************************

SYS_CHECK_OS    LDX     #04                     ; check four characters of OEM string
SYS_ID_LOOP     LDA     OSID-1,X
                CMP     BLOCK_BUF+2,X
                CLC
                BNE     SYS_CHECK_END           ; wrong OEM string
                DEX
                BNE     SYS_ID_LOOP             ; more charactrs to check
                LDY     #BOOTDEV-STRINGP2
SYS_BOOTMSG     JSR     SYS_MSG                 ; write boot message
                SEC
SYS_CHECK_END   RTS

; ******************************************************************************
; Miscellanious Routines
; ******************************************************************************

; **** Read Joystick Port ******************************************************

; Output: A - button state (Bit 0 = Button 1, Bit 1 = Button 2, Bit 2 = Button 3)
;         X - vertical joystick position 0 = Center, -1 ($FF) = Left, 1 = Right
;         Y - horizontal joystick position 0 = Center, -1 ($FF) = Up, 1 = Down

; ******************************************************************************

READ_JOY_PORT   LDA     FGCBASEH
                BEQ     NO_JOY_PORT             ; check if Floppy-/Graphisc-Controller installed
                LDY     #PIA_PORTB
                LDA     (FGCBASE),Y             ; yes, read joystick port
DECODE_JOY_PORT LDX     #$00                    ; preset x position to CENTER
                LDY     #$00                    ; preset y position to CENTER
                STX     TEMP                    ; clear temp value
JP_UP           LSR                             ; get /UP flag
                BCS     JP_DOWN                 ; not set, check DOWN position
                LDY     #$FF                    ; set y position to -1 (UP)
                LSR                             ; skip DOWN bit
                JMP     JP_LEFT                 ; and test x position
JP_DOWN         LSR                             ; get /DOWN flag
                BCS     JP_LEFT                 ; not set, test x position
                LDY     #$01                    ; set y position to 1 (DOWN)
JP_LEFT         LSR                             ; get /LEFT flag
                BCS     JP_RIGHT                ; not set, check RIGHT position
                LDX     #$FF                    ; set x position to -1 (UP)
                LSR                             ; skip RIGHT bit
                JMP     JP_BUTTON3              ; and test button 3
JP_RIGHT        LSR                             ; get /RIGHT flag
                BCS     JP_BUTTON3              ; not set, test button 3
                LDX     #$01                    ; set x position to 1 (RIGHT)
JP_BUTTON3      LSR                             ; get /BUTTON3 flag
                BCS     JP_BUTTON1              ; not set, test button 1
                PHA                             ; save joystick port value
                LDA     #$04
                STA     TEMP                    ; set bit 2 of temp button result
                PLA                             ; restore joystick port value
JP_BUTTON1      LSR                             ; get /BUTTON1 flag
                BCS     JP_BUTTON2              ; not set, test button 2
                PHA                             ; save joystick port value
                LDA     #$01
                ORA     TEMP
                STA     TEMP                    ; set bit 0 of temp button result
                PLA                             ; restore joystick port value
JP_BUTTON2      LSR                             ; get /BUTTON2 flag
                BCS     END_JOY_PORT            ; not set, exit
                LDA     #$02
                ORA     TEMP
                STA     TEMP                    ; set bit 1 of temp button result
END_JOY_PORT    LDA     TEMP                    ; load temp button result into A
                SEC                             ; data valid
                RTS
NO_JOY_PORT     TAX                             ; no joystick port available, clear X
                TAY                             ; and Y
                CLC                             ; no joystick port available, data invalid
                RTS

; ******************************************************************************
; Device Driver Routines
; ******************************************************************************

; **** Initialize Device Driver List *******************************************
;
; ******************************************************************************

DEV_INIT	LDY	#$3E                    ; clear entire list
                LDA     #$00                    ; and fill it with zeros
DEV_INIT1       STA     DEVLIST,Y
                DEY
		BPL	DEV_INIT1
END_DEV_INIT	RTS

; **** Add Device Driver *******************************************************
;
; Input  - X : Driver Descriptor Address Low Byte
;          Y : Driver Descriptor Address High Byte
; Output - C = 1 Success, C = 0 Error
;          A = Device ID (0F = Too Many Devices, FF = Unknown Device Type)
;
; ******************************************************************************
DEV_ADD		STX	PDEVL			; LSB of device driver descriptor
		STY	PDEVH			; MSB of device driver descriptor
		LDY	#$00
		LDA	(PDEV),Y                ; load device ID into A
		STA     TEMP			; TEMP = Device ID
                JSR     DEV_CHECK		; Check device ID
                BCC     END_DEV_ADD		; Exit if device ID error
		
                LSR     			; A = Device ID again
                AND     #$0F			; Get ID-nr from Device ID
                CMP     #$0F			; Device 15 ?
                BNE     ADD_DEV			; branch if not full yet
		
FIND_FREE_DEV   TYA				; A now contains 2 * Device ID = $5E			
                AND     #$E0			; A = $50
                TAY				; Y = $50
                LDX     #$00			; init. X
FIND_NEXT_DEV   LDA     DEVLIST-$20,Y		; Check if Device List entry is empty
                BEQ     ADD_DEV1		; branch if it is empty
		
                INY				; Next entry in Device List
                INY
                INX				; Next device
                CPX     #$0F			; 15 device entries checked?
                BCC     FIND_NEXT_DEV		; branch if not at end of list yet
		
                CLC				; error
                RTS				; return
		
ADD_DEV         TAX				; X = nr. of Device ID
ADD_DEV1        LDA     PDEVL			; LSB of device driver descriptor
                STA     DEVLIST-$20,Y		; Start of device driver list in RIOT memory
                LDA     PDEVH			; MSB of device driver descriptor
                STA     DEVLIST-$1F,Y		;
                LDA     TEMP			; Get Device ID back
                AND     #$F0			; A = $20
                STX     TEMP			; Temp = nr. of Device ID
                ORA     TEMP			; Original Device ID again
                SEC				; No error
END_DEV_ADD     RTS				; Return

DEV_ERR         LDX     #$FF			; Error, unknown Device Type
                CLC
               	RTS

;----------------------------------------------------------------------
; This functions checks if the Device ID is correct
;----------------------------------------------------------------------
DEV_CHECK       CMP     #STORAGE_DEV+$10	; Not too many devices?
		BCS     DEV_ERR			; branch if so
		
		CMP     #COM_DEV		; Device ID too small?
                BCC     DEV_ERR			; branch if so
		
                ASL     			; Why?
                TAY				; y = ID * 2
                SEC				; No error
                RTS				; return

; **** Open Device Driver ******************************************************
;
; Input  - A : Device ID
; Output - C = 1 Success, C = 0 Error
;          X : Descriptor Address Low Byte
;          Y : Descriptor Address High Byte
;
; ******************************************************************************
DEV_OPEN        JSR     DEV_CHECK		; Check Device ID
                BCC     END_DEV_OPEN		; exit if ID error
		
                LDA     DEVLIST-$20,Y		; DEVLIST = $1A28, Y=2*Device ID, $1A48 ???
                BNE     DEV_OPEN1		
		
                LDA	#< NULL_DEV   		; no device found use NULL device
DEV_OPEN1       STA     PDEVL
                LDA     DEVLIST-$1F,Y		; Get MSB
                BNE     DEV_OPEN2
		
                LDA	#> NULL_DEV  		; no device found use NULL device
DEV_OPEN2       STA     PDEVH
		LDY	#$02
		LDX	#$00
DEV_OPEN3	LDA	(PDEV),Y		; Start Input vector LSB
		STA	DEVIN,X			
		INY
		INX
		CPX	#$06			; Copy Input, Output & Command vector
		BNE	DEV_OPEN3		; branch if not done yet
		
		LDX	PDEVL			; X = LSB of Descriptor Address
		LDY	PDEVH			; Y = MSB of Descriptor Address
		SEC				; No error
END_DEV_OPEN    RTS				; Return

; ******************************************************************************
; Standard Driver Command Routines
; ******************************************************************************

; ******************************************************************************
; XModem Command Interpreter
; ******************************************************************************

XMODEM_CMD	CMP	#CMD_LOAD
		BNE	XM_SAVE
		JMP	XModemRcv
XM_SAVE  	CMP	#CMD_SAVE
		BNE     COM_CMD
		JMP	XModemSnd

; ******************************************************************************
; Tape Device Command Interpreter (14 bytes, removed 24-07-25 Emile)
; ******************************************************************************

		ORG	$F806		; maintain compatibilitywith v1.1.4
; ******************************************************************************
; XSD_Card Command Interpreter
; ******************************************************************************

SDC_CMD         CMP     #CMD_INIT
                BNE     SDC_READ
                JMP     SD_INIT
SDC_READ        CMP     #CMD_READ
                BNE     SDC_WRITE
                JMP     SD_RD_LBLK
SDC_WRITE       CMP     #CMD_WRITE
                BNE     SDC_RD_BUF
                JMP     SD_WR_LBLK
SDC_RD_BUF      CMP     #CMD_READ_BUF
                BNE     SDC_WR_BUF
                JMP     SD_RD_LBLK_BUF
SDC_WR_BUF      CMP     #CMD_WRITE_BUF
                BNE     SDC_SETADR
                JMP     SD_WR_LBLK_BUF
SDC_SETADR      CMP     #CMD_SETSTARTADR
                BNE     SDC_BOOT
                STX     BLKBUFL
                STY     BLKBUFH
                SEC
                RTS
SDC_BOOT        CMP     #CMD_BOOT
                BNE     _EMPTY_
                JMP     SD_BOOT

; ******************************************************************************
; Common Command Interpreter
; ******************************************************************************

COM_CMD	        CMP	#CMD_SETSTARTADR
		BNE     COM_SETENDADR
		STX	ADRL
		STY	ADRH
		SEC
		RTS
COM_SETENDADR	CMP	#CMD_SETENDADR
                BNE     _EMPTY_
		STX	NUML
		STY	NUMH
		SEC
		RTS

; EMPTY Command Handler ********************************************************
_EMPTY_         CLC
_HANDLER_       RTS

; Command Handler For Floppy Drive 2 *******************************************
FGC_FDC_CMD2    ORA     #$80            ; set bit 7 of command byte (drive 2 operation)
                JMP     FGC_FDC_CMD     ; call command handler

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
                BNE     CFC_LOAD
                JMP     CF_WR_LBLK_BUF
CFC_LOAD	CMP	#CMD_LOAD
		BNE	CFC_SAVE
		JMP	(CF_LOAD_VEC)	; Filled in by boot.sys
CFC_SAVE	CMP	#CMD_SAVE
		BNE	CFC_BOOT
		JMP	(CF_SAVE_VEC)	; Filled in by boot.sys
CFC_BOOT        CMP     #CMD_BOOT
                BNE     _EMPTY_
                JMP     CF_BOOT

; ******************************************************************************
; CF-Card Driver Routines
; ******************************************************************************
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
		
		JMP CF_INFO		; Print CF-Card Info, returns with C=1 (OK)

;----------------------------------------------------------------------------
; This routine waits until the CF-card is ready.
;----------------------------------------------------------------------------
CFWAIT		LDA #0
		STA MSEC		; msec counter
CFWLP		LDA RSTACT		; 1 = Reset pending
		BEQ NO_DLY10		; branch if no 10 msec. delay needed
		
		LDA #10			; delay = 10 msec.
		JSR DELAY		; delay 10 msec.
NO_DLY10	INC MSEC		; msec-counter
		LDA MSEC
		BEQ CFWLPTO		; branch after 2550 msec. and no reset
		
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
CF_END		RTS			; return

;-------------------------------------------------------------------------------
; CF-Card Boot Routine
;-------------------------------------------------------------------------------
CF_BOOT         JSR	INIT_LBA		; CFLBA0..CFLBA3 = 0 (MBR) and load into CF-card
		JSR	CF_RD_BLK_BUF		; Read MBR and store in BLOCK_BUF ($0600)
		BCC     CF_END                  ; error reading MBR. Exit
		
		JSR	LOAD_RUN_PART		; Load MBR and Volume ID
		BCC	CF_END			; branch if error
		
		LDY     #CFCDEV-STRINGP2        ; load pointer to device name
                JMP	DISP_DVC		; display device-name and return

;-------------------------------------------------------------------------------
; Init CFC-card, this is called from within MAIN loop.
; The first instruction comes from the beginning of the MAIN routine and 
; ensures that addresses do not change in a new firmware version
;-------------------------------------------------------------------------------
INIT_CFC	JSR     WRITE_IO_INFO		; instruction from MAIN routine
		JSR	CHECK_ROMS		; Check ROM checksum
		LDX     #<CFC_DEV
                LDY     #>CFC_DEV
                JMP     DEV_ADD         	; add CF-card driver and return

;----------------------------------------------------------------------------------		
; This routine is the same for both the CF and SD cards. It displays the device ID
;----------------------------------------------------------------------------------		
DISP_DVC        JSR     SYS_MSG                 ; print device name to screen
                LDA     #'_'
                JSR     COUT
                LDA     PSAV                    ; add partition number to name (_1.._4)
                JSR     COUT
                SEC                             ; normal boot, set carry flag
                RTS

TXT_RSTOK       .by     CR ' CF Reset: ' $00
TXT_MSEC	.by	'0 msec.' CR $00
TXT_HWERR       .by     'No CF Reset, Status=$' $00

; **** VPU IRQ Routine *********************************************************
; ******************************************************************************
                ORG     $F960

VPU_IRQ         PHA
                LDA     #VPU_STAT0
                STA     VPU_PORT1
                LDA     #VPU_REG15
                STA     VPU_PORT1
                LDA     VPU_PORT1       ; is it a line interrupt?
                BPL     NO_VPU_IRQ      ; no, exit
                LDA     TICKCNT         ; yes, load the tick counter
		BEQ     IRQ_END         ; is it 0?
		DEC     TICKCNT         ; no, decrement tick counter
IRQ_END         PLA
                RTI
NO_VPU_IRQ	PLA			; restore accumulator
		JMP	IRQ		; call user interrupt routine

; ******************************************************************************
; Standard Driver Descriptors
; ******************************************************************************

                ORG     $FA00-16*8

NULL_DEV	.byte	NULL_ID, $00     ; Null Device Driver Descriptor
		.word	_EMPTY_
		.word	_EMPTY_
		.word	_EMPTY_

TTY_DEV		.byte	TTY1_ID, $00     ; Terminal Driver Descriptor
		.word	SERIALIN
		.word	SERIALOUT
		.word	TTY_CMD

PPRINT_DEV	.byte	PRINTER1_ID, $00 ; Parallel Printer Driver Descriptor
		.word	_EMPTY_
		.word	PPORTOUT
		.word	_EMPTY_

KEYBD_DEV       .byte	KEYBD1_ID, $00   ; ASCII Keyboard Driver Descriptor
		.word	ASCIIKBD
		.word	_EMPTY_
		.word	_EMPTY_

VDP_DEV         .byte	VDP1_ID, $00     ; Video Display Processor Driver Descriptor
		.word   PS2KBD
                .word   FGC_VPU_OUT
		.word   FGC_VPU_CMD

XMODEM_DEV	.byte	XMODEM1_ID, $00  ; XModem Device Driver Descriptor
		.word	SERIALIN
		.word	SERIALOUT
		.word	XMODEM_CMD

TAPE_DEV	.byte	TAPE1_ID, $00    ; Tape Device Driver Descriptor
		.word	_EMPTY_
		.word	_EMPTY_
		.word	_EMPTY_

SDC_DEV	        .byte	SDC1_ID, $00     ; SD-Card Driver Descriptor
		.word	_EMPTY_
		.word	_EMPTY_
		.word   SDC_CMD

FDD1_DEV	.byte	FDD1_ID, $00     ; Floppy Disk Drive 1 Driver Descriptor
		.word	_EMPTY_
		.word	_EMPTY_
		.word   FGC_FDC_CMD

FDD2_DEV	.byte	FDD2_ID, $00     ; Floppy Disk Drive 2 Driver Descriptor
		.word	_EMPTY_
		.word	_EMPTY_
		.word   FGC_FDC_CMD2

CFC_DEV	        .byte	HDD1_ID, $00     ; CF-Card Driver Descriptor
		.word	_EMPTY_
		.word	_EMPTY_
		.word   CFC_CMD		 ; CF-card driver descriptor

; ******************************************************************************
; Low Byte CRC Lookup Table (XMODEM)
; ******************************************************************************

                ORG 	$FA00
CRCLO
 		.byte 	$00,$21,$42,$63,$84,$A5,$C6,$E7,$08,$29,$4A,$6B,$8C,$AD,$CE,$EF
 		.byte 	$31,$10,$73,$52,$B5,$94,$F7,$D6,$39,$18,$7B,$5A,$BD,$9C,$FF,$DE
 		.byte 	$62,$43,$20,$01,$E6,$C7,$A4,$85,$6A,$4B,$28,$09,$EE,$CF,$AC,$8D
 		.byte 	$53,$72,$11,$30,$D7,$F6,$95,$B4,$5B,$7A,$19,$38,$DF,$FE,$9D,$BC
 		.byte 	$C4,$E5,$86,$A7,$40,$61,$02,$23,$CC,$ED,$8E,$AF,$48,$69,$0A,$2B
 		.byte 	$F5,$D4,$B7,$96,$71,$50,$33,$12,$FD,$DC,$BF,$9E,$79,$58,$3B,$1A
 		.byte 	$A6,$87,$E4,$C5,$22,$03,$60,$41,$AE,$8F,$EC,$CD,$2A,$0B,$68,$49
 		.byte 	$97,$B6,$D5,$F4,$13,$32,$51,$70,$9F,$BE,$DD,$FC,$1B,$3A,$59,$78
 		.byte 	$88,$A9,$CA,$EB,$0C,$2D,$4E,$6F,$80,$A1,$C2,$E3,$04,$25,$46,$67
 		.byte 	$B9,$98,$FB,$DA,$3D,$1C,$7F,$5E,$B1,$90,$F3,$D2,$35,$14,$77,$56
 		.byte 	$EA,$CB,$A8,$89,$6E,$4F,$2C,$0D,$E2,$C3,$A0,$81,$66,$47,$24,$05
 		.byte 	$DB,$FA,$99,$B8,$5F,$7E,$1D,$3C,$D3,$F2,$91,$B0,$57,$76,$15,$34
 		.byte 	$4C,$6D,$0E,$2F,$C8,$E9,$8A,$AB,$44,$65,$06,$27,$C0,$E1,$82,$A3
 		.byte 	$7D,$5C,$3F,$1E,$F9,$D8,$BB,$9A,$75,$54,$37,$16,$F1,$D0,$B3,$92
 		.byte 	$2E,$0F,$6C,$4D,$AA,$8B,$E8,$C9,$26,$07,$64,$45,$A2,$83,$E0,$C1
 		.byte 	$1F,$3E,$5D,$7C,$9B,$BA,$D9,$F8,$17,$36,$55,$74,$93,$B2,$D1,$F0

; ******************************************************************************
; Hi Byte CRC Lookup Table (XMODEM)
; ******************************************************************************

                ORG 	$FB00
CRCHI
 		.byte 	$00,$10,$20,$30,$40,$50,$60,$70,$81,$91,$A1,$B1,$C1,$D1,$E1,$F1
 		.byte 	$12,$02,$32,$22,$52,$42,$72,$62,$93,$83,$B3,$A3,$D3,$C3,$F3,$E3
 		.byte 	$24,$34,$04,$14,$64,$74,$44,$54,$A5,$B5,$85,$95,$E5,$F5,$C5,$D5
 		.byte 	$36,$26,$16,$06,$76,$66,$56,$46,$B7,$A7,$97,$87,$F7,$E7,$D7,$C7
 		.byte 	$48,$58,$68,$78,$08,$18,$28,$38,$C9,$D9,$E9,$F9,$89,$99,$A9,$B9
 		.byte 	$5A,$4A,$7A,$6A,$1A,$0A,$3A,$2A,$DB,$CB,$FB,$EB,$9B,$8B,$BB,$AB
 		.byte 	$6C,$7C,$4C,$5C,$2C,$3C,$0C,$1C,$ED,$FD,$CD,$DD,$AD,$BD,$8D,$9D
 		.byte 	$7E,$6E,$5E,$4E,$3E,$2E,$1E,$0E,$FF,$EF,$DF,$CF,$BF,$AF,$9F,$8F
 		.byte 	$91,$81,$B1,$A1,$D1,$C1,$F1,$E1,$10,$00,$30,$20,$50,$40,$70,$60
 		.byte 	$83,$93,$A3,$B3,$C3,$D3,$E3,$F3,$02,$12,$22,$32,$42,$52,$62,$72
 		.byte 	$B5,$A5,$95,$85,$F5,$E5,$D5,$C5,$34,$24,$14,$04,$74,$64,$54,$44
 		.byte 	$A7,$B7,$87,$97,$E7,$F7,$C7,$D7,$26,$36,$06,$16,$66,$76,$46,$56
 		.byte 	$D9,$C9,$F9,$E9,$99,$89,$B9,$A9,$58,$48,$78,$68,$18,$08,$38,$28
 		.byte 	$CB,$DB,$EB,$FB,$8B,$9B,$AB,$BB,$4A,$5A,$6A,$7A,$0A,$1A,$2A,$3A
 		.byte 	$FD,$ED,$DD,$CD,$BD,$AD,$9D,$8D,$7C,$6C,$5C,$4C,$3C,$2C,$1C,$0C
 		.byte 	$EF,$FF,$CF,$DF,$AF,$BF,$8F,$9F,$6E,$7E,$4E,$5E,$2E,$3E,$0E,$1E
