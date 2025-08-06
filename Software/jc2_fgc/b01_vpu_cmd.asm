; ------------------------------------------------------------------------------
; Floppy/Graphics-Controller BIOS for the Junior Computer ][, by Joerg Walke.
;
; Driver Call Entry Table, part of the Floppy-/Graphics-Controller (FGC) ROM
; ------------------------------------------------------------------------------

B01_ADDR	; FGC ROM at $0400-$07FF and $4400-$47FF, size 1024 Bytes
B01		.local, FGC_BASE

FGC_REGISTERS	.byte	$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF  ; 16 bytes reserved space
 	        .byte	$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF  ; for FGC IO addresses
 	        
FGC_INFO        JMP     $0300

FGC_SET_PAGE    STA     PIA_PORTC       ; switch ROM page and call code
                RTS
                
FGC_FDC_CMD     PHA                     ; save Accumulator
                LDA     #$02
                STA     PIA_PORTC       ; switch to ROM page 2. Code continues in page 2
                NOP                     ; place holder nops
                NOP
                
FGC_VPU_CMD     PHA                     ; save Accumulator
                LDA     #$01
                STA     PIA_PORTC       ; switch to ROM page 1
                BNE     VPU_CMD         ; branch to VPU command routine

FGC_VPU_OUT     PHA                     ; save Accumulator
                LDA     #$00
                STA     PIA_PORTC       ; switch to ROM page 0. Code continues in page 0
                NOP                     ; place holder nops
                NOP
                
_BLINK_HANDLER_ LDA     #$00
                STA     PIA_PORTC       ; switch to ROM page 0. Code continues on page 0

; **** DRIVER START ************************************************************
;
; ******************************************************************************

; **** VPU Driver Command Routine **********************************************

; Input : A - command byte
;         X - command data byte low
;         Y - command data byte high

; ******************************************************************************

VPU_CMD		PLA
                PHA
VPU_TXT_CMD     CMP	#16
		BCS     VPU_GRAPH_CMD
VPU_CMD2	STY	YSAV
		ASL	
		TAY
		LDA	VPU_CMD_TABLE,Y
		STA	PSTRL
		LDA	VPU_CMD_TABLE+1,Y
		STA	PSTRH
		PLA
		LDY	YSAV
		JMP     (PSTR)
VPU_GRAPH_CMD 
	:4  	LSR
                CLC
                ADC     #11
                CMP     #20
                BCC     VPU_CMD2
                PLA
VPU_CMD_END	RTS

VPU_CMD_TABLE	.word	VPU_INIT,VPU_IDENTIFY,VPU_CMD_END,VPU_CMD_END,VPU_CMD_END,VPU_HOME
		.word	B01_0.VPU_CLRLINE,VPU_CLRSCRN,VPU_GOTO_XY,VPU_CURSOR_ON_OFF,VPU_COLOR
		.word   VPU_TEXTCOLOR,VPU_BACKCOLOR,VPU_STD_COLOR,VPU_CMD_END,VPU_SET_MODE
		.word   VPU_PIXEL,VPU_FRAME_RECT,VPU_LINE,VPU_FILL_RECT
                
; **** Write To VPU Register ***************************************************
;
; Input : A - Data
;         X - Register Number
;
; ******************************************************************************

VPU_SET_REG     STA     VPU_PORT1
                STX     VPU_PORT1
VPU_WAIT        RTS

; **** Set VRAM Write Address **************************************************
;
; Input : X - A0..A7
;         Y - A8..A13
;         A - A14..A16
;
; ******************************************************************************

VPU_SET_WR_ADR  SEC

; **** Set VRAM Address ********************************************************
;
; Input : X - A0..A7
;         Y - A8..A13
;         A - A14..A16
;         C - Read = 0; Write = 1
;
; ******************************************************************************

VPU_SET_ADR     STA     VPU_PORT1       ; set A14..A16
                LDA     #VPU_REG14      ; to VRAM access base address
                STA     VPU_PORT1
                STX     VPU_PORT1       ; set A0..A7
                TYA                     ; move A8..A13 to Accu
                BCC     VPU_SET_ADR1
                ORA     #$40            ; set Write flag
VPU_SET_ADR1    STA     VPU_PORT1       ; set A8..A13 and Read/Write flag
                RTS
                
; **** Set Standard Grayscale Palette ******************************************
;
; ******************************************************************************

VPU_STD_GRAY    LDX     #< B01_0.COLOR_PALETTE4
                LDY     #> B01_0.COLOR_PALETTE4
                BNE     VPU_LD_COLPAL
                
; **** Set Standard Color Pallete **********************************************
;
; ******************************************************************************

VPU_STD_COLOR   LDX     #< B01_0.COLOR_PALETTE
                LDY     #> B01_0.COLOR_PALETTE
                
; **** Load Color Palette ******************************************************
;
; Input : X - Pointer To Color Palette Low Byte
;         Y - Pointer To Color Palette High Byte
;
; ******************************************************************************
                
VPU_LD_COLPAL   STX     ADRL
                STY     ADRH
                LDY     #$00
VPU_LD_COLPAL1  TYA
                LSR     
                LDX     #VPU_REG16
                JSR     VPU_SET_REG
                LDA     (ADR),Y
                STA     VPU_PORT2
                INY
                LDA     (ADR),Y
                STA     VPU_PORT2
                INY
                CPY     #$20
                BNE     VPU_LD_COLPAL1
                RTS

; **** Set Font Bitmap *********************************************************
;
; Input : A,Y,X - Ptr24[A:Y:X] to Font Pattern Table
;
; ******************************************************************************

VPU_SET_FONT    JSR     VPU_SET_WR_ADR  ; set VRAM address to $AAYYXX
                LDA     #$04            ; we need to switch to ROM page 4
                JMP     FGC_SET_PAGE    ; call set font routine in page 4
                ;RTS
                
; **** Set Video Mode By Number ************************************************
;
; Input : X - Mode Number
;
; ******************************************************************************

VPU_SET_MODE    TXA
                LDX     #$00
                STX     VARSAV          ; reset IE0 mask
                CMP     #$05            ; mode 5?
                BNE     VPU_SET_MODES   ; no, set standard color palette
                JSR     VPU_STD_GRAY    ; yes, set standard grayscale palette
                JMP     VPU_SET_MODE5   ; 5 = 512 x 212 pixel, 4 colors
VPU_SET_MODES   PHA
                JSR     VPU_STD_COLOR   ; set standard color palette
                PLA
                CMP     #$01
                BEQ     VPU_SET_MODE1   ; 1 = 80 columns, 24 lines, b/w
                CMP     #$02
                BEQ     VPU_SET_MODE2   ; 2 = 40 columns, 24 lines, color
                CMP     #$03
                BEQ     VPU_SET_MODE3   ; 3 = 80 columns, 24 lines, color
                CMP     #$04
                BEQ     VPU_SET_MODE4   ; 4 = 256 x 212 pixel, 16 colors
                CMP     #$06
                BEQ     VPU_SET_MODE6   ; 6 = 512 x 212 pixel, 16 colors
                CMP     #$07
                BEQ     VPU_SET_MODE7   ; 7 = 256 x 212 pixel, 256 colors

                ; else                  ; 0 = 40 columns, 24 lines, b/w

; **** Set Text Mode 1 (40 x 24 chars) *****************************************
;
; ******************************************************************************

VPU_SET_MODE0   SEC                     ; black/white mode
                BCS     VPU_TEXT_MODE1
VPU_SET_MODE2   CLC                     ; color mode;
VPU_TEXT_MODE1  LDA     #VPU_MODE0      ; MODE0 = 40 columns text mode
                JSR     VPU_SET_TXTMODE
                LDX     #40
                STX     MAX_X           ; MAX_X = 40 characters
                LDA     #$04
                STA     PAGECNT         ; 4 buffer pages (of 240 bytes)
                LDA     #$00            ; #$00 for 40 columns
                PHA
                LDY     #$08            ; address 1 byte of font bitmap pointer
                LDA     #$01            ; #$01 for 40 columns
VPU_TEXT_MODE   LDX     #VPU_REG4       ; set Pattern Generator Table base address
                JSR     VPU_SET_REG
                LDX     #$00            ; address byte 0 of font bitmap pointer
                TXA                     ; address byte 2 of font bitmap pointer
                JSR     VPU_SET_FONT    ; set font bitmap
                PLA
                LDX     #VPU_REG2       ; set Pattern Name Table base address
                JSR     VPU_SET_REG
                LDA     #30             ; standard cursor blink time 30ms on/off
                JSR     VPU_CURSOR_ON
                JMP     VPU_CLRSCRN

; **** Set Text Mode 2 (80 x 24 chars) *****************************************
;
; ******************************************************************************

VPU_SET_MODE1   SEC                     ; black/white mode
                BCS     VPU_TEXT_MODE2
VPU_SET_MODE3   CLC                     ; color mode;
VPU_TEXT_MODE2  LDA     #VPU_MODE1      ; MODE1 = 80 columns
                JSR     VPU_SET_TXTMODE
                LDX     #80
                STX     MAX_X           ; MAX_X = 80 characters
                LDA     #$08
                STA     PAGECNT         ; 8 buffer pages (of 240 bytes)
                LDA     #$F0
                LDX     #VPU_REG12      ; set blink Text/Back Color
                JSR     VPU_SET_REG
                LDA     #$00
                LDX     #VPU_REG13      ; set blink on/off time
                JSR     VPU_SET_REG
                LDA     #$2F
                LDX     #VPU_REG3
                JSR     VPU_SET_REG     ; set Color Table base low address
                LDA     #$00
                LDX     #VPU_REG10
                JSR     VPU_SET_REG     ; set Color Table base high address
                LDA     #$03            ; #$03 for 80 columns
                PHA
                LDY     #$10            ; address byte 1 of font bitmap pointer
                LDA     #$02            ; #$02 for 80 columns
                BNE     VPU_TEXT_MODE

; **** Set Graphic Mode 4 (256 x 212 pixel, 16 colors) *************************
;
; ******************************************************************************

VPU_SET_MODE4   LDA     #VPU_MODE4
                BNE     VPU_SET_GMODE

; **** Set Graphic Mode 5 (512 x 212 pixel, 4 colors) **************************
;
; ******************************************************************************

VPU_SET_MODE5   LDA     #VPU_MODE5
                BNE     VPU_SET_GMODE

; **** Set Graphic Mode 6 (512 x 212 pixel, 16 colors) *************************
;
; ******************************************************************************

VPU_SET_MODE6   LDA     #VPU_MODE6
                BNE     VPU_SET_GMODE

; **** Set Graphic Mode 7 (256 x 212 pixel, 256 colors) ************************
;
; ******************************************************************************

VPU_SET_MODE7   LDA     #VPU_MODE7
VPU_SET_GMODE   CLC                     ; CLC = color; SEC = black/white mode
                JSR     VPU_SET_VMODE
                LDA     #$1F
                LDX     #VPU_REG2       ; set Pattern Name Table base address
                JSR     VPU_SET_REG
                LDA     #$00
                LDX     #VPU_REG7
                JSR     VPU_SET_REG     ; set Back Color
                LDA     #< _NO_HANDLER_
                STA     NKEY_HANDLER
                LDA     #> _NO_HANDLER_
                STA     NKEY_HANDLER+1
                JMP     VPU_CLRSCRN

; **** Set Video Mode **********************************************************
;
; Input : A - Mode
;         C = 0 Color Mode; C = 1 Black/White Mode
;
; ******************************************************************************

VPU_SET_TXTMODE LDX     IOBASE          ; IO board installed?
                BNE     VPU_SET_VMODE   ; yes, normal setup
                LDX     #$20            ; no, set VPU line interrupt as tick interrupt
                STX     VARSAV

VPU_SET_VMODE   STA     VPUMODE
                PHP                     ; save carry flag (color) onto stack
                PHP                     ; save carry flag (color) onto stack (again)
                PHA                     ; save accu (mode) onto stack
                ASL     
                AND     #$0F            ; mask mode flags
                ORA     #VPU_MODE_REG0  ; set mode flags M3..M5
                LDX     #VPU_REG0
                JSR     VPU_SET_REG     ; set Mode Register 0
                PLA                     ; restore mode into accu
                AND     #$18            ; mask mode flags
                ORA     #VPU_MODE_REG1  ; set mode flags M1..M2
                ORA     VARSAV          ; set IE0 flag if needed
                LDX     #VPU_REG1
                JSR     VPU_SET_REG     ; set Mode Register 1
                LDA     #VPU_MODE_REG2
                ROR     
                PLP
                ROL                     ; get carry flag into BW bit
                LDX     #VPU_REG8
                JSR     VPU_SET_REG     ; set Mode Register 2
                LDA     #VPU_MODE_REG3
                LDY     VPUMODE
                CPY     #VPU_MODE0
                BCS     VPU_TEXTMODE
                ORA     #$80            ; set 212 lines graphics mode
VPU_TEXTMODE    LDX     #VPU_REG9
                JSR     VPU_SET_REG     ; set Mode Register 3
                LDA     #$00
                LDX     #VPU_REG45
                JSR     VPU_SET_REG     ; switch to VPU video RAM
                PLP                     ; restore carry flag
                BCC     VPU_COLOR_MODE
                LDA     #$F0            ; Text Color = White; Background Color = Black
                BNE     VPU_SET_COLOR
VPU_COLOR_MODE  LDA     #$D2            ; Text Color = Yellow; Background Color = Dark Blue

; **** Set Text And Background Color *******************************************
;
; Input: AH = Text Color Index; AL = Background Color Index
;
; ******************************************************************************

VPU_SET_COLOR   STA     TXTCOLOR
                STA     VPU_PORT1
                LDA     #VPU_REG7
                STA     VPU_PORT1
                RTS
                
; **** Set Text Color **********************************************************
;
; Input: X = Text Color Index
;
; ******************************************************************************

VPU_TEXTCOLOR   TXA
        :4      ASL                     ; clip to 16 colors and shift to upper nibble
                PHA                     ; and save accu
                LDA     #$0F            ; set text color mask
                BNE     VPU_SAVE_COLOR

; **** Set Background Color ****************************************************
;
; Input: X = Background Color Index
;
; ******************************************************************************

VPU_BACKCOLOR   TXA
                AND     #$0F            ; clip to 16 colors
                PHA                     ; and save accu
                LDA     #$F0            ; set background color mask
VPU_SAVE_COLOR  AND     TXTCOLOR        ; clear masked color part
                STA     TXTCOLOR
                PLA                     ; restore accu
                ORA     TXTCOLOR        ; combine text and background color
                JMP     VPU_SET_COLOR
                
; **** Set Color ***************************************************************
;
; Input: X = 4/16 Color Index / 256 RGB Color (RRRGGGBB)
;
; ******************************************************************************

VPU_COLOR       STX     COLOR
                STX     VPU_PORT1
                LDA     #VPU_REG44
                STA     VPU_PORT1
                RTS
                
; **** Set Graphics Pen or Cursor Position *************************************
;
; Input: X = X-Coordinate  Bit(7..0)
;        Y = Y-Coordinate  Bit(7..0)
;
; ******************************************************************************
                
VPU_GOTO_XY     LDA     VPUMODE
                CMP     #$10
                BCC     VPU_MOVE_TO
                JMP     B01_0.VPU_SETCURSOR
                
; **** Set Graphics Pen Position ***********************************************
;
; Input: X = X-Coordinate  Bit(7..0)
;        Y = Y-Coordinate  Bit(7..0)
;
; ******************************************************************************

VPU_MOVE_TO     LDA     #$00
                STA     POS_X+1
                STX     POS_X
                STY     POS_Y
                RTS
                
VPU_SET_XY      JSR     VPU_INIT_R36
                LDA     POS_X
                STA     VPU_PORT3       ; R36 - set X low byte
                LDA     POS_X+1
                STA     VPU_PORT3       ; R37 - set X high byte
                LDA     POS_Y
                STA     VPU_PORT3       ; R38 - set Y low byte
                LDA     #$00
                STA     VPU_PORT3       ; R39 - set Y high byte to 0
                RTS
                
; **** Set Indirect Register Addressing From Register #36 **********************
                
VPU_INIT_R36    LDA     #36             ; indirect access register #36
                STA     VPU_PORT1
                LDA     #VPU_REG17
                STA     VPU_PORT1
                RTS
                
; **** Wait For VPU To Come Ready **********************************************

WAIT_READY      LDA     #VPU_STAT2
                STA     VPU_PORT1
                LDA     #VPU_REG15
                STA     VPU_PORT1
                LDA     VPU_PORT1
                AND     #$01
                BNE     WAIT_READY
                RTS

; **** Draw Filled Rectangle ***************************************************
;
; Input: A = Logical Operation
;        X = Width
;        Y = Height
;
; ******************************************************************************

VPU_FILL_RECT   ;STA     VARSAV
;VPU_RECT_LINE   LDA     VARSAV
;                JSR     VPU_HLINE
;                DEY
;                BEQ     VPU_END_RECT
;                INC     POS_Y
;                BNE     VPU_RECT_LINE
VPU_END_RECT    RTS

; **** Draw Framed Rectangle ***************************************************
;
; Input: A = Logical Operation
;        X = Width
;        Y = Height
;
; ******************************************************************************

VPU_FRAME_RECT  STA     VARSAV
                JSR     VPU_HLINE
                LDA     VARSAV
                JSR     VPU_VLINE
                LDA     POS_X
                STA     XREG
                TXA
                CLC
                ADC     POS_X
                BCS     FRAME_RECT1
                STA     POS_X
                LDA     VARSAV
                JSR     VPU_VLINE
FRAME_RECT1     TYA
                CLC
                ADC     POS_Y
                BCC     FRAME_RECT2
                RTS
FRAME_RECT2     STA     POS_Y
                LDA     XREG
                STA     POS_X
                LDA     VARSAV

; **** Draw Horizontal Line ****************************************************
;
; Input: A = Logical Operation
;        X = Length
;
; ******************************************************************************

VPU_HLINE       PHA
                JSR     WAIT_READY
                JSR     VPU_SET_XY
                STX     VPU_PORT3       ; R40 - set DX low byte
                STA     VPU_PORT3       ; R41 - set DX high byte to 0
                STA     VPU_PORT3       ; R42 - set DY low byte to 0
                STA     VPU_PORT3       ; R43 - set DY high byte to 0
                LDA     COLOR
                STA     VPU_PORT3       ; R44 - set color
                LDA     #$00
                STA     VPU_PORT3       ; R45 - set arguments
                BEQ     VPU_LINE_CMD

; **** Draw Vertical Line ******************************************************
;
; Input: A = Logical Operation
;        Y = Y-Coordinate  Bit(7..0)
;
; ******************************************************************************

VPU_VLINE       PHA
                JSR     WAIT_READY
                JSR     VPU_SET_XY
                STY     VPU_PORT3       ; R40 - set DY low byte to 0
                STA     VPU_PORT3       ; R41 - set DY high byte to 0
                STA     VPU_PORT3       ; R42 - set DX low byte
                STA     VPU_PORT3       ; R43 - set DX high byte to 0
                LDA     COLOR
                STA     VPU_PORT3       ; R44 - set color
                LDA     #$01
                STA     VPU_PORT3       ; R45 - set arguments
VPU_LINE_CMD    PLA
                ORA     #$70
                STA     VPU_PORT3       ; R46 - set command
                RTS

; **** Draw Line ***************************************************************
;
; Input: A = Logical Operation
;        X = X-Coordinate  Bit(7..0)
;        Y = Y-Coordinate  Bit(7..0)
;
; ******************************************************************************

VPU_LINE        PHA
                STX     XREG
                STY     YREG
                CPX     POS_X
                BCC     VPU_NEG_X
                TXA
                SBC     POS_X
                TAX
                LDA     #$00
                BEQ     VPU_LINE1
VPU_NEG_X       LDA     POS_X
                SEC
                SBC     XREG
                TAX
                LDA     #$04
VPU_LINE1       STA     VARSAV
                CPY     POS_Y
                BCC     VPU_NEG_Y
                TYA
                SBC     POS_Y
                TAY
                LDA     #$00
                BEQ     VPU_LINE2
VPU_NEG_Y       LDA     POS_Y
                SEC
                SBC     YREG
                TAY
                LDA     #$08
VPU_LINE2       ORA     VARSAV
                LSR     
                PHA
                STX     VARSAV
                CPY     VARSAV
                BCC     VPU_LINE3
                TYA
                PHA
                TXA
                TAY
                PLA
                TAX
VPU_LINE3       PLA
                ROL     
                STA     VARSAV
                JSR     WAIT_READY
                JSR     VPU_SET_XY
                STX     VPU_PORT3       ; R40 - set DX low byte to 0
                STA     VPU_PORT3       ; R41 - set DX high byte to 0
                STY     VPU_PORT3       ; R42 - set DY low byte
                STA     VPU_PORT3       ; R43 - set DY high byte to 0
                LDA     COLOR
                STA     VPU_PORT3       ; R44 - set color
                LDA     VARSAV
                STA     VPU_PORT3       ; R45 - set arguments
                PLA
                ORA     #$70
                STA     VPU_PORT3       ; R46 - set command
                LDX     XREG
                LDY     YREG
                STX     POS_X
                STY     POS_Y
                RTS
                
; **** Draw Pixel **************************************************************
;
; Input: A = Logical Operation
;        X = X-Coordinate
;        Y = Y-Coordinate
;
; ******************************************************************************

VPU_PIXEL       PHA
                JSR     VPU_INIT_R36
                LDA     #$00
                STX     VPU_PORT3       ; set X low byte
                STA     VPU_PORT3       ; set X high byte
                STY     VPU_PORT3       ; set Y low byte
                STA     VPU_PORT3       ; set Y high byte to 0
                PLA
                ORA     #$50            ; send command PSET / LogOp
                STA     VPU_PORT1
                LDA     #VPU_REG46
                STA     VPU_PORT1
                RTS

; ******************************************************************************
                
; ******************************************************************************

VPU_IDENTIFY    LDA     #$99
                RTS
                
; **** Initialize Video Processor **********************************************
;
; ******************************************************************************

VPU_INIT        LDX     #$01
                JSR     VPU_SET_MODE    ; set 80 column B/W text mode

; **** Clear Screen ************************************************************
;
; ******************************************************************************

VPU_CLRSCRN     JSR     VPU_HOME        ; set VRAM address to 0 and store x/y registers
                LDX     VPUMODE         ; current screen mode
                LDA     #SPC
CLS_MODE0       CPX     #VPU_MODE0      ; 40 columns text mode?
                BCC     CLS_MODE6_7     ; graphics modes?
                BNE     CLS_MODE1       ; 80 columns text mode?
                LDY     #$03
                LDX     #$C0
CLS_MODE1       LDY     #$07
                LDX     #$80
                BNE     CLS_LOOP
CLS_MODE6_7     LDA     #$6A
                CPX     #VPU_MODE6
                BCC     CLS_MODE_4_5
                ASL     
CLS_MODE_4_5    TAY
CLS_SET_ADRL    LDX     #$00
                TXA
CLS_LOOP        STA     VPU_PORT0
                DEX
                CPX     #$FF
                BNE     CLS_LOOP
                DEY
                CPY     #$FF
                BNE     CLS_LOOP
                BEQ     VPU_HOME2       ; set cursor to home position
                
; **** Set Cursor Position to X(0), Y(0) ***************************************
;
; ******************************************************************************

VPU_HOME        STX     XREG            ; save X register
                STY     YREG            ; save Y register
VPU_HOME2       LDA     #$00
                TAX
                STX     POS_X
                TAY
                STY     POS_Y
                JSR     VPU_SET_WR_ADR
                LDX     XREG            ; restore x register
                LDY     YREG            ; restore y register
                RTS
                
VPU_CURSOR_ON_OFF
                TXA
                BEQ     VPU_CURSOR_OFF

; **** Turn Cursor On **********************************************************
;
; Input: A = Blink Rate in Milliseconds
;
; ******************************************************************************

VPU_CURSOR_ON   STA     CURSOR_TICKS
                LDA     #CRSR
                STA     CURSOR
                LDA     #< _BLINK_HANDLER_
                STA     NKEY_HANDLER
                LDA     #> _BLINK_HANDLER_
                STA     NKEY_HANDLER+1
                RTS
                
; **** Turn Cursor Off *********************************************************
;
; ******************************************************************************
                
VPU_CURSOR_OFF  LDA     #< _NO_HANDLER_
                STA     NKEY_HANDLER
                LDA     #> _NO_HANDLER_
                STA     NKEY_HANDLER+1
                
; **** Hide Cursor *************************************************************

VPU_HIDE_CURSOR LDA     CURSOR          ; check if cursor is visible
                CMP     #CRSR
                BEQ     VPU_HIDE_END    ; no, just exit
                STA     VPU_PORT0       ; yes, write current saved char back to screen
VPU_HIDE_END    SEC                     ; set carry for easy branches
                RTS
                
 		.endl
		ORG	B01_0.VPU_CLRLINE - FGC_BASE + B01_ADDR	; address in eeprom
B01_0		.local,	$13CB

VPU_CLRLINE     LDA     #$00
                STA     PIA_PORTC  	; switch to ROM page 0. Code continues in page 0
                NOP                	; place holder nops
                NOP
                NOP

; ******************************************************************************

; **** Set Text Cursor To X/Y Position *****************************************
;
; Input: X = Cursor X Position (0..39/79) ; Y = Cursor Y Position (0..23)
;
; ******************************************************************************

VPU_SETCURSOR   LDA     #$00
                STA     PIA_PORTC  ; switch to ROM page 0. Code continues in page 2
                
; **** Color Palettes **********************************************************

COLOR_PALETTE4  .byte 	$00, $00        ; 0 Black
                .byte 	$33, $03        ; 5 Dark Gray
                .byte 	$55, $05        ; A Light Gray
                .byte 	$77, $07        ; 3 White
                
COLOR_PALETTE   .byte 	$00, $00        ; 0 Black
                .byte 	$60, $00        ; 1 Dark Red
                .byte 	$04, $00        ; 2 Dark Blue
                .byte 	$75, $00        ; 3 Purple
                .byte 	$00, $03        ; 4 Dark Green
                .byte 	$33, $03        ; 5 Dark Gray
                .byte 	$06, $00        ; 6 Blue
                .byte 	$07, $05        ; 7 Light Blue
                .byte 	$41, $02        ; 8 Brown
                .byte 	$70, $04        ; 9 Orange
                .byte 	$55, $05        ; A Light Gray
                .byte 	$75, $05        ; B Pink
                .byte 	$00, $07        ; C Green
                .byte 	$70, $07        ; D Yellow
                .byte 	$06, $07        ; E Aquamarine
                .byte 	$77, $07        ; F White

		.endl
		