; ------------------------------------------------------------------------------
; Floppy/Graphics-Controller BIOS for the Junior Computer ][, by Joerg Walke.
; ------------------------------------------------------------------------------

;**** DRIVER CALL ENTRY TABLE **************************************************
;*******************************************************************************
B00_ADDR	; FGC ROM at $0000-$03FF and $4000-$43FF
B00		.local,	FGC_BASE

FGC_REGISTERS	.byte	$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF  ; 16 bytes reserved space
 	        .byte	$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF  ; for FGC IO addresses
 	        
FGC_INFO        JMP     $0300

FGC_SET_PAGE    STA     PPI_PORTC       ; switch ROM page and call code
                RTS
                
FGC_FDC_CMD     PHA                     ; save Accumulator
                LDA     #$02
                STA     PPI_PORTC       ; switch to ROM page 2. Code continues in page 2
                NOP                     ; place holder nops
                NOP
                
FGC_VPU_CMD     PHA                     ; save Accumulator
                LDA     #$01
                STA     PPI_PORTC       ; switch to ROM page 1. Code continues in page 1
                NOP                     ; place holder nops
                NOP
                
FGC_VPU_OUT     PHA                     ; save Accumulator
                LDA     #$00
                STA     PPI_PORTC       ; switch to ROM page 0
                BEQ     VPU_OUT         ; branch to VPU output routine
                
_BLINK_HANDLER_ LDA     #$00
                STA     PPI_PORTC       ; switch to ROM page 0
                
; **** DRIVER START ************************************************************

; **** Cursor Blink Handler ****************************************************
;
; ******************************************************************************
                
                LDA     TICKCNT
                BNE     NO_HANDLER
                STX     FGC_XREG
                STY     FGC_YREG
                LDA     CURSOR_TICKS
                STA     TICKCNT
                JSR     VPU_XY_TO_ADR
                JSR     VPU_SET_RD_ADR
                LDA     VPU_PORT0
                PHA
                JSR     VPU_TEXT_WR_ADR
                LDA     CURSOR
                STA     VPU_PORT0
                JSR     VPU_TEXT_WR_ADR
                PLA
                STA     CURSOR
HNDLR0          LDX     FGC_XREG
                LDY     FGC_YREG
                CLC
NO_HANDLER      RTS
;
; ******************************************************************************

; **** VPU Driver Character Output Routine *************************************
;
; ******************************************************************************

VPU_OUT         LDA     VPUMODE
                CMP     #VPU_MODE0      ; is current mode a text mode?
                BCS     VPU_OUT2        ; yes, print character
                PLA                     ; no, restore accu and exit
                RTS
                
VPU_OUT2        PLA                     ; get character to print on screen
                PHA
                STX     FGC_XREG        ; save X register
                STY     FGC_YREG        ; save Y register
                CMP     #SPC            ; is it a control character?
                BCS     VPU_CHAR        ; no, handle printable chars
VPU_CR          CMP     #CR             ; Carriage Return char?
                BNE     VPU_BELL        ; no, check next char
                JSR     B00_1.VPU_HIDE_CURSOR
                LDX     #0
                STX     POS_X           ; reset cursor x position to 0
                BEQ     VPU_CURR_ADR    ; branch always
VPU_BELL        CMP	#$07		; is it the BELL character?
		BNE	VPU_LF          ; no, check next char
		JSR	BEEP		; chime bell
                JMP     VPU_OUT_END
VPU_LF          CMP     #LF             ; Line Feed / Down key (ASCII 10) ?
                BNE     VPU_BS          ; no, check next char
                JSR     B00_1.VPU_HIDE_CURSOR
                BCS     VPU_DOWN
VPU_BS          CMP     #BS             ; Backspace / Left key (ASCII 8) ?
                BNE     VPU_RIGHT       ; no, check next char
                JSR     B00_1.VPU_HIDE_CURSOR
VPU_LEFT        DEC     POS_X           ; move cursor one char left
                BPL     VPU_CURR_ADR    ; line break? No, set VRAM address
                LDX     #0              ; yes
                LDY     POS_Y           ; first line?
                BEQ     VPU_SET_X       ; yes, set cursor x position to 0
                DEC     POS_Y           ; no, move one line up
                LDX     MAX_X           ; and set cursor y position to
                DEX                     ; max_x-1
VPU_SET_X       STX     POS_X
                BPL     VPU_CURR_ADR    ; branch always
VPU_RIGHT       CMP     #RIGHT          ; Right key (ASCII 21) ?
                BEQ     VPU_HIDE_CRSR
VPU_UP          CMP     #UP             ; Up key (ASCII 11) ?
                BNE     VPU_TAB
                LDY     POS_Y
                BEQ     VPU_UP1
                DEC     POS_Y
VPU_UP1         JSR     B00_1.VPU_HIDE_CURSOR
                BCS     VPU_CURR_ADR
VPU_TAB         CMP     #TAB            ; Horizontal Tab (ASCII 9) ?
                BNE     VPU_CHAR
                LDA     #8
                INC     POS_X
VPU_TAB1        CMP     POS_X
                BCS     VPU_TAB2
                ADC     #8
                CMP     MAX_X
                BCC     VPU_TAB1
                LDA     MAX_X
VPU_TAB2        STA     POS_X
VPU_HIDE_CRSR   JSR     B00_1.VPU_HIDE_CURSOR
                BCS     VPU_INC_X
VPU_CHAR        STA     VPU_PORT0       ; printable character
VPU_INC_X       INC     POS_X
VPU_LINE_BREAK  LDX     POS_X
                CPX     MAX_X           ; line break?
                BCC     VPU_OUT_END     ; no, finish
                LDX     #0              ; carriage return
                STX     POS_X           ; reset cursor x position to 0
VPU_DOWN        INC     POS_Y           ; increment cursor y position
                LDY     POS_Y
                CPY     #24             ; max lines reached?
                BCC     VPU_CURR_ADR    ; no, just calculate current VRAM address
                DEC     POS_Y           ; yes
                JSR     VPU_SCROLL      ; scroll screen one line up
VPU_CURR_ADR    JSR     VPU_XY_TO_ADR   ; calculate VRAM address from cursor x-y position
                JSR     VPU_SET_WR_ADR  ; set VRAM write address pointer
VPU_OUT_END     LDX     #CRSR           ; load cursor character
                STX     TICKCNT         ; set tick counter to 1
                STX     CURSOR          ; set character at cursor position to cursor char
                LDX     FGC_XREG        ; restore x register
                LDY     FGC_YREG        ; restore y register
                PLA
                RTS

; **** Set VRAM Read Address ***************************************************
;
; Input : X - A0..A7
;         Y - A8..A13
;         A - A14..A16
;
; ******************************************************************************

VPU_SET_RD_ADR  CLC
                BCC     VPU_SET_ADR

; **** Set Text Mode VRAM Write Address ****************************************

VPU_TEXT_WR_ADR LDA     #$00

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
                
; **** Scroll Text Screen One Line Up ******************************************
;
; ******************************************************************************

VPU_SCROLL      LDA     PAGECNT
                STA     VARSAV
                LDY     #240
                STY     BUFFSIZE
                LDX     MAX_X
                LDY     #$00
VPU_SCRL_LOOP   STX     FGC_ADRL
                STY     FGC_ADRH
                LDA     #$00
                JSR     VPU_SET_RD_ADR
                LDX     BUFFSIZE
VPU_RD_CHAR0    LDA     VPU_PORT0
                STA     RBUFF,X
                DEX
                BNE     VPU_RD_CHAR0
                SEC
                LDA     FGC_ADRL
                SBC     MAX_X
                TAX
                LDA     FGC_ADRH
                SBC     #$00
                TAY
                LDA     #$00
                JSR     VPU_SET_WR_ADR
                LDX     BUFFSIZE
VPU_WR_CHAR0    LDA     RBUFF,X
                STA     VPU_PORT0
                DEX
                BNE     VPU_WR_CHAR0
                CLC
                LDA     #240
                ADC     FGC_ADRL
                TAX
                LDA     #$00
                ADC     FGC_ADRH
                TAY
                DEC     VARSAV
                BEQ     VPU_SCRL_END
                LDA     VARSAV
                CMP     #$01
                BNE     VPU_SCRL_LOOP
                SEC
                LDA     BUFFSIZE
                SBC     MAX_X
                STA     BUFFSIZE
                BNE     VPU_SCRL_LOOP
VPU_SCRL_END    LDX     MAX_X
                LDA     #' '
VPU_WR_CHAR1    STA     VPU_PORT0
                DEX
                BNE     VPU_WR_CHAR1
                RTS
                
; **** Calculate VRAM Address from Cursor X/Y Position *************************
;
; ******************************************************************************
                
VPU_XY_TO_ADR   LDY     POS_Y           ; cursor y position is index into table
                LDA     VPU_ADR_START_H,Y
                STA     VARSAV
                LDA     VPU_ADR_START_L,Y
                LDX     VPUMODE
                CPX     #VPU_MODE0      ; 40 column mode?
                BNE     VPU_80COL       ; no, skip next step
                LSR     VARSAV          ; yes, devide start address by two
                ROR     
VPU_80COL       CLC
                ADC     POS_X           ; add cursor x position
                TAX                     ; save low byte to X
                LDA     #$00
                ADC     VARSAV          ; add carry to high byte
                TAY                     ; save high byte to Y
                LDA     #$00            ; most significant byte is 0
                RTS
                
VPU_ADR_START_L .byte	$00,$50,$A0,$F0,$40,$90,$E0,$30,$80,$D0,$20,$70
                .byte	$C0,$10,$60,$B0,$00,$50,$A0,$F0,$40,$90,$E0,$30
VPU_ADR_START_H .byte	$00,$00,$00,$00,$01,$01,$01,$02,$02,$02,$03,$03
                .byte	$03,$04,$04,$04,$05,$05,$05,$05,$06,$06,$06,$07
                
; ******************************************************************************
                
VPU_SET_STR     ;STX     FGC_ADRL
                ;STY     FGC_ADRH
                
                RTS

; ******************************************************************************
		.endl
		ORG	$03D0		; address in EPROM
B00_0		.local,	$13D0
VPU_CLRLINE     JMP     B00.VPU_SET_STR
                
; **** Set Text Cursor To X/Y Position *****************************************
;
; Input: X = Cursor X Position (0..39/79) ; Y = Cursor Y Position (0..23)
;
; ******************************************************************************
		.endl
		ORG	$03D8		; address in EPROM
B00_1		.local,	$13D8

VPU_SETCURSOR   JSR     B00_1.VPU_HIDE_CURSOR
                CPX     MAX_X
                BCS     VPU_SETCRSR_END
                CPY     #24
                BCS     VPU_SETCRSR_END
                STX     POS_X
                STY     POS_Y
                JSR     B00.VPU_XY_TO_ADR
                JMP     B00.VPU_SET_WR_ADR
VPU_SETCRSR_END RTS

; **** Hide Cursor *************************************************************

VPU_HIDE_CURSOR LDA     CURSOR          ; check if cursor is visible
                CMP     #CRSR
                BEQ     VPU_HIDE_END    ; no, just exit
                STA     VPU_PORT0       ; yes, write current saved char back to screen
VPU_HIDE_END    SEC                     ; set carry for easy branches
                RTS

      		.endl
		ORG	$03FF		; address in EPROM
		.local,	$13FF
                BRK
		.endl
