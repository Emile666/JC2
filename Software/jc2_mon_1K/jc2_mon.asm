;----------------------------------------------------------------------------
; SOURCE LISTING OF ELEKTOR'S JUNIOR COMPUTER
;
; Written by: A. NACHTMANN
; Date      :  7 FEB. 1980
;
; The features of Junior's monitor program are:
; - Hex address data display (entry via RST)
; - Hex editor (start address is $1CB5)
; - Hex assembler (start address is $1F51)
;----------------------------------------------------------------------------
; Original code restored and adapted for the 
; TASM (Telemark Assembler) by : A.J. Prosman, October 26, 2019
; MADS (MAD-Assembler) by : E. v.d. Logt,      December 2024
;----------------------------------------------------------------------------
		OPT h- ; do not add file header
		OPT f+ ; save as single block

; Editor's pointers and temps in page zero
        KEY     EQU     $00E1   ;1-byte
        BEGADR  EQU     $00E2   ;2-byte Begin Address Pointer
        ENDADR  EQU     $00E4   ;2-byte End Address Pointer
        CURADR  EQU     $00E6   ;2-byte Current Address Pointer
        CENDADR EQU     $00E8   ;2-byte Current End Address Pointer
        MOVADR  EQU     $00EA   ;2-byte 
        TABLEA  EQU     $00EC   ;2-byte
        LABELS  EQU     $00EE   ;1-byte

;       MPU registers in page zero
        PCL     EQU     $00EF   ;1-byte Program-counter low
        PCH     EQU     $00F0   ;1-byte Program-counter high
        PREG    EQU     $00F1   ;1-byte Flags
        SPUSER  EQU     $00F2   ;1-byte 
        ACC     EQU     $00F3   ;1-byte 
        YREG    EQU     $00F4   ;1-byte 
        XREG    EQU     $00F5   ;1-byte 

        BYTES   EQU     $00F6   ;1-byte Number of bytes to be displayed
        COUNT   EQU     $00F7   ;1-byte

;       Hex display buffers in page zero
        INL     EQU     $00F8   ;1-byte
        INH     EQU     $00F9   ;1-byte
        POINT   EQU     $00FA   ;2-byte
        
;       Temporary data-buffers in page zero
        TEMP    EQU     $00FC   ;1-byte
        TEMPX   EQU     $00FD   ;1-byte
        NIBBLE  EQU     $00FE   ;1-byte
        MODE    EQU     $00FF   ;1-byte

;------------------------------------------------------------------------------
;       6532 RIOT IC memory locations
;------------------------------------------------------------------------------
        PAD     EQU     $1A80   ; Port A Data register 
        PADD    EQU     $1A81   ; Port A Data direction register 
        PBD     EQU     $1A82   ; Port B Data register
        PBDD    EQU     $1A83   ; Port B Data direction register

;       Write edge detect control
        EDETA   EQU     $1AE4   ; Negative edge detection disable PA7-IRQ
        EDETB   EQU     $1AE5   ; Positive edge detection disable PA7-IRQ
        WDETC   EQU     $1AE6   ; Negative edge detection enable PA7-IRQ
        EDETD   EQU     $1AE7   ; Positive edge detection enable PA7-IRQ

;       Read flag register and clear timer & IRQ flag
        RDFLAG  EQU     $1AD5   ; BIT6=PA7-FLAG; BIT7=TIMER-FLAG

;       Write count into timer, disable timer-IRQ
        CNTA    EQU     $1AF4   ; CLK1T
        CNTB    EQU     $1AF5   ; CLK8T
        CNTC    EQU     $1AF6   ; CLK64T
        CNTD    EQU     $1AF7   ; CLK1024T

;       Write count into timer, enable timer-IRQ
        CNTE    EQU     $1AFC   ; CLK1T
        CNTF    EQU     $1AFD   ; CLK8T
        CNTG    EQU     $1AFE   ; CLK64T
        CNTH    EQU     $1AFF   ; CLK1024T

;       Interrupt vectors: IRQ & NMI vectors should be loaded into the
;       following memory locations for proper system operation.
        NMIL    EQU     $1A7A   ; NMI LOWER BYTE
        NMIH    EQU     $1A7B   ; NMI HIGHER BYTE
        IRQL    EQU     $1A7E   ; IRQ LOWER BYTE
        IRQH    EQU     $1A7F   ; IRQ HIGHER BYTE

;       Beginners may load these locations
;       $1C00 for step by step modus and BRK command

;----------------------------------------------------------------------------
;        JUNIOR'S MAIN ROUTINES
;----------------------------------------------------------------------------
        ORG $1C00, $FC00

SAVE    STA     ACC         ; Save ACCU
        PLA                 ; Get current P-Register
        STA     PREG        ; Save P-Register

SAVEA   PLA                 ; Get current PCL
        STA		PCL         ; Save current PCL
        STA     POINT       ; PCL to display buffer
        PLA                 ; Get current PCH
        STA     PCH         ; Save current PCH
        STA     POINT+1     ; PCH to display buffer

SAVEB   STY     YREG        ; Save current Y-Register
        STX     XREG        ; Save current X-Register 
        TSX                 ; Get current SP
        STX     SPUSER      ; Save current SP
        LDX     #$01        ; Set AD-Mode
        STX     MODE
        JMP     START

; RESET vector 6502
RESET   LDA     #$1E        ; PB1---PB4
        STA     PBDD        ; IS output
        LDA     #$04        ; Reset P-Register
        STA     PREG
        LDA     #$03
        STA     MODE        ; Set AD-Mode
        STA     BYTES       ; Display POINT, INH
        LDX     #$FF
        TXS 
        STX     SPUSER
        CLD 
        SEI 

START   JSR     SCAND       ; Display data specified by POINT
        BNE     START       ; Wait until key is released

STARA   JSR     SCAND       ; Display data specified by point
        BEQ     STARA       ;  Any key pressed
        JSR     SCAND       ;  Debounce key
        BEQ     STARA       ; Any key still pressed
        JSR     GETKEY      ; If Yes, decode key, return with key in ACC

GOEXEC  CMP     #$13        ; GO-Key ?
        BNE     ADMODE
        LDX     SPUSER      ; Get current SP
        TXS 
        LDA     POINT+1     ; Start execution at POINT
        PHA 
        LDA     POINT
        PHA 
        LDA     PREG        ; Restore current P register
        PHA 
        LDX     XREG
        LDY     YREG
        LDA     ACC
        RTI                 ; Execute program

ADMODE  CMP     #$10        ; AD-Key ?
        BNE     DAMODE
        LDA     #$03        ; Set AD-Mode
        STA     MODE
        BNE     STEPA       ; Always
        
DAMODE  CMP     #$11        ; DA-Key ?
        BNE     STEP
        LDA     #$00        ; Set DA-Mode
        STA     MODE
        BEQ     STEPA

STEP    CMP     #$12        ; PLUS-Key ?
        BNE     PCKEY
        INC     POINT
        BNE     STEPA
        INC     POINT+1

STEPA   JMP     START

PCKEY   CMP     #$14        ; PC-Key
        BNE     ILLKEY
        LDA     PCL
        STA     POINT      ; Last PC to display buffer
        LDA     PCH
        STA     POINT+1
        JMP     STEPA
        
ILLKEY  CMP     #$15        ; Illegal key?
        BPL     STEPA       ; If Yes, ignore it

DATA    STA     KEY         ; Save key
        LDY     MODE        ; Y=0 Is data mode, else address mode
        BNE     ADDRESS
        LDA     (POINT),Y   ; Get Data specified
        ASL                 ; by point
        ASL                 ; shift low order
        ASL                 ; nibble into high order nibble
        ASL      
        ORA     KEY         ; Data with key
        STA     (POINT),Y   ; Restore data
        JMP     STEPA

ADDRESS LDX     #$04        ; 4 Shifts
ADLOOP  ASL     POINT       ; POINT+1, POINT 4 Positions to the left
        ROL     POINT+1
        DEX 
        BNE     ADLOOP
        LDA     POINT
        ORA     KEY         ; Restore address
        STA     POINT
        JMP     STEPA

;-------------------------------------------------------------------------------
;       JUNIOR'S HEX EDITOR
;
;       FOLLOWING COMMANDS ARE VALID:
;       "INSERT": INSERT A NEW LINE JUST BEFORE DISPLAYED LINE
;       "INPUT": INSERT A NEW LINE JUST BEHIND THE DISPLAYED LINE
;       "SEARCH": SEARCH IN WORKSPACE FOR A GIVEN 2BYTE PATTERN
;       "SKIP": SKIP TO NEXT INSTRUCTION
;       "DELETE": DELETE CURRENT DISPLAYED INSTRUCTION
;
;       AN ERROR IS INDICATED, IF THE INSTRUCTION POINTER CURAD IS OUT OF RANGE
;-------------------------------------------------------------------------------
EDITOR  JSR     BEGIN       ; CURAD := BEGAD
        LDY     BEGADR+1
        LDX     BEGADR
        INX 
        BNE     EDIT
        INY 

EDIT    STX     CENDADR     ; CEND := BEGAD + 1
        STY     CENDADR+1
        LDA     #$77        ; Display "77"
        LDY     #$00
        STA     (CURADR),Y
CMND    JSR     SCAN        ; Display current instruction,
                            ; wait for a key
SEARCH  CMP     #$14        ; Search command ?
        BNE     INSERT
        JSR     GETBYT      ; Read 1st byte
        BPL     SEARCH      ; COM. Key ?
        STA     POINT+1     ; Discard data
        JSR     GETBYT      ; Read 2nd byte
        BPL     SEARCH      ; COM. Key ?
        STA     POINT       ; Discard data
        JSR     BEGIN       ; CURAD := BEGAD

SELOOP  LDY     #$00
        LDA     (CURADR),Y  ; Compare instruction
        CMP     POINT+1     ; against data to be searched
        BNE     SEARA       ; Skip to the next instruction, if not equal
        INY 
        LDA     (CURADR),Y
        CMP     POINT
        BEQ     CMND        ; Return if 2byte pattern is found

SEARA   JSR     OPLEN       ; Get length of the current instruction
        JSR     NEXT        ; Skip to the next instruction
        BMI     SELOOP      ; Search again, if CURAD is less than CEND
        BPL     ERRA

INSERT  CMP     #$10        ; Insert command ?
        BNE     INPUT
        JSR     RDINST      ; Read instruction and compute length
        BPL     SEARCH      ; COM. key?
        JSR     FILLWS      ; Move data in WS downward by the amount in bytes
        BEQ     CMND        ; Return to display the inserted instruction

INPUT   CMP     #$13        ; Input command ?
        BNE     SKIP
        JSR     RDINST      ; Read instruction and compute length
        BPL     SEARCH      ; COM. key ?
        JSR     OPLEN       ; Length of the current instruction
        JSR     NEXT        ; Return with N=1, if CURAD is less than CEND
        LDA     TEMPX       ; Length of instr. to be inserted
        STA     BYTES
        JSR     FILLWS      ; Move data in ws downward by the amount in bytes
        BEQ     CMND        ; Return to display the inserted data

SKIP    CMP     #$12        ; Skip command ?
        BNE     DELETE
        JSR     NEXT        ; Skip to next instruction. CURAD less than CEND?
        BMI     CMND
        BPL     ERRA

DELETE  CMP     #$11        ; Delete command ?
        BNE     ERRA
        JSR     UP          ; Delete current instruction by moving up the WS
        JSR     RECEND      ; Adjust current end address
        JMP     CMND

ERRA    LDA     #$EE
        STA     POINT+1
        STA     POINT
        STA     INH
        LDA     #$03
        STA     BYTES

ERRB    JSR     SCANDS      ; Display "EEEEEE" until key is released
        BNE     ERRB
        JMP     CMND

;----------------------------------------------------------------------------
;       EDITOR'S SUBROUTINES
;
;       SCAN is a subroutine, filling up the display-buffer determined by
;       CURADR. Then the display is scanned depending on the length of the  
;       instruction pointed to by CURADR if a keypress is detected.
;----------------------------------------------------------------------------

;       SCAN RETURNS WITH VALUE IN A
SCAN    LDX     #$02        ; Fill up the display buffer
        LDY     #$00

FILBUF  LDA     (CURADR),Y  ; Start filling at OPCode
        STA     INH,X
        INY 
        DEX 
        BPL     FILBUF
        JSR     OPLEN       ; Store instruction length in bytes

SCANA   JSR     SCANDS      ; Display current instruction
        BNE     SCANA       ; Key released ?

SCANB   JSR     SCANDS      ; Display current instruction
        BEQ     SCANB       ; Any key pressed
        JSR     SCANDS      ; Display current instruction
        BEQ     SCANB       ; Any key still pressed ?
        JSR     GETKEY      ; If yes, return with key in ACC
        RTS 

;----------------------------------------------------------------------------
;       GETBYT reads 2 hex-keys and composes their values in the A register.
;       If only hex-keys were pressed, it returns with N=1. If a command-key
;       was pressed, it returns with N=0;
;----------------------------------------------------------------------------
GETBYT  JSR     SCANA       ; Read high order nibble
        CMP     #$10
        BPL     BYTEND      ; Command key ?
        ASL     
        ASL                 ; If not, save high order nibble
        ASL     
        ASL     
        STA     NIBBLE
        JSR     SCANA       ; Read low order nibble
        CMP     #$10
        BPL     BYTEND      ; Command key ?
        ORA     NIBBLE      ; If not, compose byte
        LDX     #$FF        ; Set N=1
BYTEND  RTS 

;----------------------------------------------------------------------------
;       SCAND is a subroutine showing data specified by POINT.
;       SCANDS is a subroutine showing the contents of the display-buffer as
;       a function of BYTES.
;       The next subroutine AK scans the keyboard, it returns with A=0 if no
;       key was pressed and with A > 0 if a key was pressed.
;       When SCAND or SCANDS are exit, PA0..PA7 are set to input.
;----------------------------------------------------------------------------
SCAND
        LDY     #$00
        LDA     (POINT),Y   ; Get data specified by point
        STA     INH

SCANDS  LDA     #$7F
        STA     PADD        ; PA0..PA6 is output
        LDX     #$08        ; Enable display
        LDY     BYTES       ; Fetch length from bytes

SCDSA   LDA     POINT+1     ; Output 1st byte
        JSR     SHOW
        DEY 
        BEQ     SCDSB       ; More bytes ?
        LDA     POINT
        JSR     SHOW        ; If yes, output 2nd byte
        DEY 
        BEQ     SCDSB       ; More bytes ?
        LDA     INH
        JSR     SHOW        ; If yes, output 3rd byte

SCDSB   LDA     #$00
        STA     PADD        ; PA0..PA7 is input
AK      LDY     #$03        ; Scan 3 rows
        LDX     #$00        ; Reset row counter

ONEKEY  LDA     #$FF
AKA     STX     PBD         ; Output row number
        INX                 ; Enable next row
        INX 
        AND     PAD         ; Input row pattern
        DEY                 ; All rows scanned ?
        BNE     AKA
        LDY     #$06        ; Turn display off
        STY     PBD
        ORA     #$80        ; Set BIT7=1
        EOR     #$FF        ; Invert key pattern
        RTS 

;----------------------------------------------------------------------------
;       Subroutine SHOW copies the contents of a display-buffer to the display.
;       The X-register is used as a scan-counter. It determines if POINT+1,
;       POINT or INH is transported to the displays.
;----------------------------------------------------------------------------
SHOW
        PHA                 ; Save display 
        STY     TEMP        ; Save Y register
        LSR     
        LSR                 ; Get high order nibble
        LSR     
        LSR     
        JSR     CONVD       ; Output high order nibble
        PLA                 ; Get display again
        AND     #$0F        ; Mask off high order nibble
        JSR     CONVD       ; Output low order nibble
        LDY     TEMP
        RTS 

;----------------------------------------------------------------------------
;       Subroutine CONVD controls the display scanning. It converts the
;       contents of the display-buffer to be displayed into a segment pattern.
;----------------------------------------------------------------------------
CONVD   TAY                 ; Use nibble as index
        LDA     LOOK,Y      ; Fetch segment pattern
        STA     PAD         ; Output segment pattern
        STX     PBD         ; Output digit enable
        LDY     #$7F

DELAY   DEY                 ; Delay 500uS approx
        BPL     DELAY
        STY     PAD         ; Turns segments off
        LDY     #$06
        STY     PBD         ; Turn display off
        INX                 ; Enable next digit
        INX 
        RTS 

;----------------------------------------------------------------------------
;       GETKEY converts a key-press into a hex number. It returns with the
;       key value in A. 
;       If an invalid key was pressed ?
;----------------------------------------------------------------------------
GETKEY  LDX     #$21        ; Start at row 0
GETKEA  LDY     #$01        ; Get one row
        JSR     ONEKEY      ; A=0, No key pressed
        BNE     KEYIN
        CPX     #$27
        BNE     GETKEA      ; Each row scanned ?
        LDA     #$15        ; Return if invalid key
        RTS 

KEYIN   LDY     #$FF
KEYINA  ASL                 ; Shift left until Y=Key number
        BCS     KEYINB
        INY 
        BPL     KEYINA

KEYINB  TXA 
        AND     #$0F        ; Mask MSD
        LSR                 ; Divide by 2
        TAX 
        TYA 
        BPL     KEYIND

KEYINC  CLC 
        ADC     #$07        ; Add row offset
KEYIND  DEX 
        BNE     KEYINC
        RTS 

;----------------------------------------------------------------------------
;       RDINST transfers an instruction from the keyboard to the display-buffer. 
;       It returns with N=0 if a command-key was pressed. Once the entire
;       instruction is read, RDINST returns with N=1;
;----------------------------------------------------------------------------
RDINST  JSR     GETBYT      ; Read OPCode
        BPL     RDB         ; Return if it is the command key
        STA     POINT+1     ; Store OP cod in the display buffer
        JSR     LENACC      ; Calculate instruction length
        STY     COUNT
        STY     TEMPX
        DEC     COUNT
        BEQ     RDA         ; 1 Byte instruction ?
        JSR     GETBYT      ; If not, read first operand
        BPL     RDB         ; Return if it is the command key
        
        STA     POINT       ; Store 1st operand in the display buffer
        DEC     COUNT
        BEQ     RDA         ; 2 Byte instruction ?
        
        JSR     GETBYT      ; If not, read second operand
        BPL     RDB         ; Return if it is the command key
        
        STA     INH         ; Store 2nd operand in the display buffer
RDA     LDX     #$FF        ; N=1
RDB     RTS 

;----------------------------------------------------------------------------
;       WILLWS transfers data from the display to the workspace. It always
;       returns with Z=1.
;----------------------------------------------------------------------------
FILLWS  JSR     DOWN        ; Move data down by the amount in bytes
        JSR     ADCEND      ; Adjust current end address
        LDX     #$02
        LDY     #$00
WS      LDA     INH,X       ; Fetch data from display buffer
        STA     (CURADR),Y  ; Insert data into the data field
        DEX 
        INY 
        CPY     BYTES       ; All inserted ?
        BNE     WS          ; If not, continue
        RTS 

;----------------------------------------------------------------------------
;       OPLEN calculates the length of a 6502 instruction.
;       Instruction length is saved in BYTES.
;----------------------------------------------------------------------------
OPLEN   LDY     #$00
        LDA     (CURADR),Y  ; Fetch OPCode from WS
LENACC  LDY     #$01        ; Length of the OPCode is 1 byte
        CMP     #$00
        BEQ     LENEND      ; BRK Instruction ?
        
        CMP     #$40
        BEQ     LENEND      ; TRI Instruction ?
        
        CMP     #$60
        BEQ     LENEND      ; RTS Instruction ?
        
        LDY     #$03
        CMP     #$20
        BEQ     LENEND      ; JSR Instruction ?
        
        AND     #$1F        ; Strip to 5 bits
        CMP     #$19
        BEQ     LENEND      ; Any ABS,Y instruction ?
        
        AND     #$0F        ; Strip to 4 bits
        TAX                 ; Use nibble as index
        LDY     LEN,X       ; Fetch length from LEN
LENEND  STY     BYTES       ; Discard length in bytes
        RTS 

;----------------------------------------------------------------------------
;       UP moves a data-field between CURADR and CENDADR upwards by the
;       amount in BYTES.
;----------------------------------------------------------------------------
UP      LDA     CURADR
        STA     MOVADR
        LDA     CURADR+1    ; MOVAD := CURADR
        STA     MOVADR+1
UPLOOP  LDY     BYTES
        LDA     (MOVADR),Y  ; Move upward by the number of bytes
        LDY     #$00
        STA     (MOVADR),Y
        INC     MOVADR
        BNE     UPA
        
        INC     MOVADR+1    ; MOVADR+1 := MOVADR+1 + 1
UPA     LDA     MOVADR
        CMP     CENDADR
        BNE     UPLOOP      ; All data moved ?
        LDA     MOVADR+1    ; If not continue
        CMP     CENDADR+1
        BNE     UPLOOP
        RTS 

;----------------------------------------------------------------------------
;       DOWN moves a data-field between CURADR and CENDADR downwards by the
;       amount in BYTES.
;----------------------------------------------------------------------------
DOWN    LDA     CENDADR
        STA     MOVADR      ; MOVAD := CEND
        LDA     CENDADR+1
        STA     MOVADR+1
DNLOOP  LDY     #$00
        LDA     (MOVADR),Y  ; Move downward by the number of bytes
        LDY     BYTES
        STA     (MOVADR),Y
        LDA     MOVADR
        CMP     CURADR
        BNE     DNA         ; All data moved ?
        
        LDA     MOVADR+1    ; If not, continue
        CMP     CURADR+1
        BEQ     DNEND
DNA     SEC 
        LDA     MOVADR
        SBC     #$01
        STA     MOVADR
        LDA     MOVADR+1    ; MOVAD := MOVAD - 1
        SBC     #$00
        STA     MOVADR+1
        JMP     DNLOOP
DNEND   RTS 

;----------------------------------------------------------------------------
;       BEGIN sets CURADR TO BEGADR
;----------------------------------------------------------------------------
BEGIN
        LDA     BEGADR
        STA     CURADR
        LDA     BEGADR+1    ; CURAD := BEGADR
        STA     CURADR+1
        RTS 

;----------------------------------------------------------------------------
;       ADCEND increases the current end-address by the number in BYTES.
;----------------------------------------------------------------------------
ADCEND  CLC 
        LDA     CENDADR
        ADC     BYTES       ; CEND := CEND + BYTES
        STA     CENDADR
        LDA     CENDADR+1
        ADC     #$00
        STA     CENDADR+1
        RTS 

;----------------------------------------------------------------------------
;       RECEND decreases the current end-address by the number in BYTES.
;----------------------------------------------------------------------------
RECEND  SEC 
        LDA     CENDADR
        SBC     BYTES       ; CEND := CEND - BYTES
        STA     CENDADR
        LDA     CENDADR+1
        SBC     #$00
        STA     CENDADR+1
        RTS 

;----------------------------------------------------------------------------
;       NEXT increases the current displayed address by the number in BYTES.
;----------------------------------------------------------------------------
NEXT    CLC 
        LDA     CURADR
        ADC     BYTES       ; CURAD := CURAD + BYTES
        STA     CURADR
        LDA     CURADR+1
        ADC     #$00
        STA     CURADR+1
        SEC 
        LDA     CURADR
        SBC     CENDADR
        LDA     CURADR+1
        SBC     CENDADR+1
        RTS 

;----------------------------------------------------------------------------
;       Lookup table "LOOK"" is used to convert a hex number into a pattern
;       for the seven-segment displays. 
;       Lookup table "LEN" is used to convert an instruction into an 
;       instruction length.
;----------------------------------------------------------------------------
LOOK    .byte   $40         ; "0"
        .byte   $79         ; "1"
        .byte   $24         ; "2"
        .byte   $30         ; "3"
        .byte   $19         ; "4"
        .byte   $12         ; "5"
        .byte   $02         ; "6"
        .byte   $78         ; "7"
        .byte   $00         ; "8"
        .byte   $10         ; "9"
        .byte   $08         ; "A"
        .byte   $03         ; "B"
        .byte   $46         ; "C"
        .byte   $21         ; "D"
        .byte   $06         ; "E"
        .byte   $0E         ; "F"

LEN     .byte   $02
        .byte   $02
        .byte   $02
        .byte   $01
        .byte   $02
        .byte   $02
        .byte   $02
        .byte   $01
        .byte   $01
        .byte   $02
        .byte   $01
        .byte   $01
        .byte   $03
        .byte   $03
        .byte   $03
        .byte   $03

NMI     JMP     (NMIL)      ; Jump to a user selectable NMI vector
IRQ     JMP     (IRQL)      ; Jump to a user selectable IRQ vector

;----------------------------------------------------------------------------
;       GETLBL is an assembler subroutine. It searches for labels on the
;       symbol pseudo stack. If this stack contains a valid label, it returns
;       with the high-order label address in X and the low-order label
;       address in A. If no valid label is found, it returns with Z=1.
;----------------------------------------------------------------------------
GETLBL
        LDA     (CURADR),Y  ; Fetch current label number from WS
        LDY     #$FF        ; Reset pseudo stack
SYMA    CPY     LABELS      ; Upper most symbol table address ?
        BEQ     SYMB        ; If yes, return, no label on pseudo stack
        
        CMP     (TABLEA),Y  ; Label Nr. in WS = Label Nr. on pseudo stack
        BNE     SYMNXT
        
        DEY                 ; If yes, get high order address
        LDA     (TABLEA),Y
        TAX                 ; Discard high order, add in X
        DEY 
        LDA     (TABLEA),Y  ; Get low order add
        LDY     #$01        ; Prepare Y register
SYMB    RTS 

SYMNXT  DEY                 ; *********   *********
        DEY                 ; * X=ADH *   * A=ADL *
        DEY                 ; *********   *********
        BNE     SYMA
        RTS 

;----------------------------------------------------------------------------
;       ASSEMBLER main routine.
;
;       The following instructions are assembled:
;       - JSR instruction
;       - JMP instruction 
;       - BRANCH instructions
;----------------------------------------------------------------------------
ASSEMB  SEC 
        LDA     ENDADR
        SBC     #$FF
        STA     TABLEA      ; TABLE := ENDAD - $FF
        LDA     ENDADR+1
        SBC     #$00
        STA     TABLEA+1
        LDA     #$FF
        STA     LABELS
        JSR     BEGIN       ; CURAD := BEGAD

PASSA   JSR     OPLEN       ; Start pass one, get current instruction
        LDY     #$00
        LDA     (CURADR),Y  ; Fetch current instruction
        CMP     #$FF        ; Is the current instruction a label ? 
        BNE     NXTINS
        
        INY 
        LDA     (CURADR),Y  ; If yes, fetch label number
        LDY     LABELS
        STA     (TABLEA),Y  ; Push label number on symbol stack
        DEY 
        LDA     CURADR+1    ; Get high order address
        STA     (TABLEA),Y  ; Push on symbol stack
        DEY 
        LDA     CURADR      ; Get high order address
        STA     (TABLEA),Y  ; Push on symbol stack
        DEY 
        STY     LABELS      ; Adjust pseudo stack pointer
        JSR     UP          ; Delete current label in ws
        JSR     RECEND      ; Adjust current end address
        JMP     PASSA       ; Look for more labels

NXTINS  JSR     NEXT        ; If no label skip to the next instruction
        BMI     PASSA       ; All labels in WS collected ?
        JSR     BEGIN       ; Start pass 2
        
PASSB   JSR     OPLEN       ; Get length of the current instruction
        LDY     #$00
        LDA     (CURADR),Y  ; Fetch current instruction
        CMP     #$4C        ; JMP instruction ?
        BEQ     JUMPS
        
        CMP     #$20        ; JSR Instruction ?
        BEQ     JUMPS
        
        AND     #$1F        ; Strip to 5 bits
        CMP     #$10        ; Any branch instruction ?
        BEQ     BRINST
        
PB      JSR     NEXT        ; If not, return
        BMI     PASSB       ; All labels between CURAD and ENDAD assembled ?
        
        LDA     #$03        ; Enable 3 display buffers
        STA     BYTES
        JMP     START       ; Exit here

JUMPS   INY                 ; Set pointer to label number
        JSR     GETLBL      ; Get label address
        BEQ     PB          ; Return if not found
        
        STA     (CURADR),Y  ; Store low order address
        TXA 
        INY 
        STA     (CURADR),Y  ; Store high order address
        BNE     PB

BRINST  INY                 ; Set pointer to label number
        JSR     GETLBL      ; Get label address
        BEQ     PB          ; Return if label not found
        
        SEC 
        SBC     CURADR      ; Calculate branch offset
        SEC 
        SBC     #$02        ; DESTINATION - SOURCE - 2 = OFFSET
        STA     (CURADR),Y  ; Insert branch offset in WS
        JMP     PB

;----------------------------------------------------------------------------
;       BRANCH calculates the offset of branch instructions. The 2 right-hand
;       displays show the calculated offset defined by the 4 left-hand
;       displays. The program must be stopped by the RESET key.
;----------------------------------------------------------------------------
BRANCH
        CLD 
        LDA     #$00        ; Reset display buffer
        STA     POINT+1
        STA     POINT
        STA     INH
BR      JSR     GETBYT      ; Read source
        BPL     BRANCH      ; Command key ?
        
        STA     POINT+1    ; Save source in buffer
        JSR     GETBYT      ; Read destination
        BPL     BRANCH      ; Command key ?

        STA     POINT       ; Save destination in buffer
        CLC 
        LDA     POINT       ; Fetch destination
        SBC     POINT+1     ; Substract source
        STA     INH
        DEC     INH         ; Equalize and save offset in buffer
        JMP     BR

;----------------------------------------------------------------------------
;       VECTORS AT THE END OF THE MEMORY
;----------------------------------------------------------------------------
        ORG    $1FFA, $FFFA
NMI_VECTOR
        .word  NMI          ; $1F2F in Junior Computer Monitor program

RESET_VECTOR
        .word  RESET        ; $1C1D in Junior Computer Monitor program

IRQ_BRK_VECTOR
        .word  IRQ          ; $1F32 in Junior Computer Monitor program

;----------------------------------------------------------------------------
;       END OF JUNIOR'S MONITOR
;----------------------------------------------------------------------------
