; ------------------------------------------------------------------------------
; Floppy/Graphics-Controller BIOS for the Junior Computer ][, by Joerg Walke.
;
; Character Generator Table, part of the Floppy-/Graphics-Controller (FGC) ROM
; ------------------------------------------------------------------------------

B04_ADDR	; FGC ROM PAGE 4 at $1000-$1FFF and $5000-$5FFF, size 4096 bytes = 4 x 1KB ROM
B04		.local, FGC_BASE

         	.byte	$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF  ; 16 bytes reserved space
 	        .byte   $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF  ; for FGC IO addresses

                RTS
                NOP
                NOP
                
FGC_SET_PAGE_5  STA     PIA_PORTC       ; switch ROM page
                LDY     #$00
                STY     ADR_CHAR
                LDA     #> B04_1.PAGE_START
                STA     ADR_CHAR+1
FONT_LOOP01     LDA     (ADR_CHAR),Y
                STA     VPU_PORT0
                INY
                BNE     FONT_LOOP01
                INC     ADR_CHAR+1
FONT_LOOP02     LDA     (ADR_CHAR),Y
                STA     VPU_PORT0
                INY
                BNE     FONT_LOOP02
                LDA     #$05
                BNE     FGC_SET_PAGE_5  ; switch to ROM page 5

; **** Character Table Part 0 **************************************************
		.endl
		ORG	B04_1.PAGE_START - FGC_BASE + B04_ADDR	; address in eeprom
B04_1		.local,	$1200		; 512 bytes offset from page start

PAGE_START
                ; Control Code Chars

                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char (0)
                .byte	 $F8, $F8, $F8, $F8, $F8, $F8, $F8, $00      ; code for char (1) CURSOR1
                .byte	 $00, $00, $00, $F0, $10, $10, $10, $10      ; code for char (2)
                .byte	 $10, $10, $10, $1C, $00, $00, $00, $00      ; code for char (3)
                .byte	 $10, $10, $10, $F0, $00, $00, $00, $00      ; code for char (4)
                .byte	 $10, $10, $10, $10, $10, $10, $10, $10      ; code for char (5)
                .byte	 $00, $00, $00, $FC, $00, $00, $00, $00      ; code for char (6)
                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char (7)
                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char (8)
                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char (9)
                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char (10)
                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char (11)
                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char (12)
                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char (13)
                .byte	 $00, $00, $00, $1C, $10, $10, $10, $10      ; code for char (14)
                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char (15)
                .byte	 $10, $10, $10, $FC, $10, $10, $10, $10      ; code for char (16)
                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char (17)
                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char (18)
                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char (19)
                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char (20)
                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char (21)
                .byte	 $00, $00, $00, $FC, $10, $10, $10, $10      ; code for char (22)
                .byte	 $10, $10, $10, $F0, $10, $10, $10, $10      ; code for char (23)
                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char (24)
                .byte	 $10, $10, $10, $1C, $10, $10, $10, $10      ; code for char (25)
                .byte	 $10, $10, $10, $FC, $00, $00, $00, $00      ; code for char (26)
                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char (27)
                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char (28)
                .byte	 $28, $28, $EC, $00, $FC, $00, $00, $00      ; code for char (29)
                .byte	 $00, $00, $FC, $00, $EC, $28, $28, $28      ; code for char (30)
                .byte	 $28, $28, $EC, $00, $EC, $28, $28, $28      ; code for char (31)

                ; Normal Text Chars


                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char SPACE
                .byte	 $20, $20, $20, $20, $20, $00, $20, $00      ; code for char !
                .byte	 $50, $50, $50, $00, $00, $00, $00, $00      ; code for char "
                .byte	 $50, $50, $F8, $50, $F8, $50, $50, $00      ; code for char #
                .byte	 $20, $78, $A0, $70, $28, $F0, $20, $00      ; code for char $
                .byte	 $C0, $C8, $10, $20, $40, $98, $18, $00      ; code for char %
                .byte	 $40, $A0, $A0, $40, $A8, $90, $68, $00      ; code for char &
                .byte	 $20, $20, $20, $00, $00, $00, $00, $00      ; code for char '
                .byte	 $10, $20, $40, $40, $40, $20, $10, $00      ; code for char (
                .byte	 $20, $10, $08, $08, $08, $10, $20, $00      ; code for char )
                .byte	 $20, $A8, $70, $20, $70, $A8, $20, $00      ; code for char *
                .byte	 $00, $20, $20, $F8, $20, $20, $00, $00      ; code for char +
                .byte	 $00, $00, $00, $00, $20, $20, $40, $00      ; code for char ,
                .byte	 $00, $00, $00, $F8, $00, $00, $00, $00      ; code for char -
                .byte	 $00, $00, $00, $00, $00, $00, $20, $00      ; code for char .
                .byte	 $00, $08, $10, $20, $40, $80, $00, $00      ; code for char /
                .byte	 $70, $88, $98, $A8, $C8, $88, $70, $00      ; code for char 0
                .byte	 $20, $60, $20, $20, $20, $20, $70, $00      ; code for char 1
                .byte	 $70, $88, $08, $30, $40, $80, $F8, $00      ; code for char 2
                .byte	 $70, $88, $08, $30, $08, $88, $70, $00      ; code for char 3
                .byte	 $10, $30, $50, $90, $F8, $10, $10, $00      ; code for char 4
                .byte	 $F8, $80, $F0, $08, $08, $88, $70, $00      ; code for char 5
                .byte	 $30, $40, $80, $F0, $88, $88, $70, $00      ; code for char 6
                .byte	 $F8, $08, $10, $20, $40, $40, $40, $00      ; code for char 7
                .byte	 $70, $88, $88, $70, $88, $88, $70, $00      ; code for char 8
                .byte	 $70, $88, $88, $78, $08, $10, $60, $00      ; code for char 9
                .byte	 $00, $00, $20, $00, $20, $00, $00, $00      ; code for char :
                .byte	 $00, $00, $20, $00, $20, $20, $40, $00      ; code for char ;
                .byte	 $10, $20, $40, $80, $40, $20, $10, $00      ; code for char <
                .byte	 $00, $00, $F8, $00, $F8, $00, $00, $00      ; code for char =
                .byte	 $40, $20, $10, $08, $10, $20, $40, $00      ; code for char >
                .byte	 $70, $88, $08, $10, $20, $00, $20, $00      ; code for char ?
                
; ROM PAGE 5 ($1400 - $17FF and $5400 - $57FF) *********************************
		.endl
		ORG	B04_2.START - FGC_BASE + B04_ADDR	; address in eeprom
B04_2		.local,	$1400					; 1 KB offset from page start

START         	.byte	      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF  ; 16 bytes reserved space
 	        .byte	      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF  ; for FGC IO addresses
 	        
 	        RTS
                NOP
                NOP

FGC_SET_PAGE_6  STA     PIA_PORTC       ; switch ROM page
                LDY     #$00
                STY     ADR_CHAR
                LDA     #> B04_1.PAGE_START
                STA     ADR_CHAR+1
FONT_LOOP11     LDA     (ADR_CHAR),Y
                STA     VPU_PORT0
                INY
                BNE     FONT_LOOP11
                INC     ADR_CHAR+1
FONT_LOOP12     LDA     (ADR_CHAR),Y
                STA     VPU_PORT0
                INY
                BNE     FONT_LOOP12
                LDA     #$06
                BNE     FGC_SET_PAGE_6  ; switch to ROM page 6
                
; **** Character Table Part 1 **************************************************

		.endl
		ORG	B04_3.START - FGC_BASE + B04_ADDR	; address in eeprom
B04_3		.local,	$1600 					; 1536 bytes offset from page start
                
START           .byte	 $70, $88, $A8, $B8, $B0, $80, $78, $00      ; code for char @
                .byte	 $70, $88, $88, $F8, $88, $88, $88, $00      ; code for char A
                .byte	 $F0, $88, $88, $F0, $88, $88, $F0, $00      ; code for char B
                .byte	 $70, $88, $80, $80, $80, $88, $70, $00      ; code for char C
                .byte	 $F0, $88, $88, $88, $88, $88, $F0, $00      ; code for char D
                .byte	 $F8, $80, $80, $F0, $80, $80, $F8, $00      ; code for char E
                .byte	 $F8, $80, $80, $F0, $80, $80, $80, $00      ; code for char F
                .byte	 $70, $88, $80, $80, $98, $88, $78, $00      ; code for char G
                .byte	 $88, $88, $88, $F8, $88, $88, $88, $00      ; code for char H
                .byte	 $70, $20, $20, $20, $20, $20, $70, $00      ; code for char I
                .byte	 $08, $08, $08, $08, $88, $88, $70, $00      ; code for char J
                .byte	 $88, $90, $A0, $C0, $A0, $90, $88, $00      ; code for char K
                .byte	 $80, $80, $80, $80, $80, $80, $F8, $00      ; code for char L
                .byte	 $88, $D8, $A8, $A8, $88, $88, $88, $00      ; code for char M
                .byte	 $88, $88, $C8, $A8, $98, $88, $88, $00      ; code for char N
                .byte	 $70, $88, $88, $88, $88, $88, $70, $00      ; code for char O
                .byte	 $F0, $88, $88, $F0, $80, $80, $80, $00      ; code for char P
                .byte	 $70, $88, $88, $88, $A8, $90, $68, $00      ; code for char Q
                .byte	 $F0, $88, $88, $F0, $A0, $90, $88, $00      ; code for char R
                .byte	 $70, $88, $80, $70, $08, $88, $70, $00      ; code for char S
                .byte	 $F8, $20, $20, $20, $20, $20, $20, $00      ; code for char T
                .byte	 $88, $88, $88, $88, $88, $88, $70, $00      ; code for char U
                .byte	 $88, $88, $88, $50, $50, $20, $20, $00      ; code for char V
                .byte	 $88, $88, $88, $A8, $A8, $D8, $88, $00      ; code for char W
                .byte	 $88, $88, $50, $20, $50, $88, $88, $00      ; code for char X
                .byte	 $88, $88, $50, $20, $20, $20, $20, $00      ; code for char Y
                .byte	 $F8, $08, $10, $20, $40, $80, $F8, $00      ; code for char Z
                .byte	 $78, $60, $60, $60, $60, $60, $78, $00      ; code for char [
                .byte	 $00, $80, $40, $20, $10, $08, $00, $00      ; code for char \
                .byte	 $78, $18, $18, $18, $18, $18, $78, $00      ; code for char ]
                .byte	 $00, $20, $50, $88, $00, $00, $00, $00      ; code for char ^
                .byte	 $00, $00, $00, $00, $00, $00, $F8, $00      ; code for char _
                .byte	 $40, $20, $10, $00, $00, $00, $00, $00      ; code for char `
                .byte	 $00, $00, $70, $08, $78, $88, $78, $00      ; code for char a
                .byte	 $80, $80, $F0, $88, $88, $88, $F0, $00      ; code for char b
                .byte	 $00, $00, $78, $80, $80, $80, $78, $00      ; code for char c
                .byte	 $08, $08, $78, $88, $88, $88, $78, $00      ; code for char d
                .byte	 $00, $00, $70, $88, $F8, $80, $78, $00      ; code for char e
                .byte	 $10, $28, $20, $70, $20, $20, $20, $00      ; code for char f
                .byte	 $00, $00, $70, $88, $88, $78, $08, $70      ; code for char g
                .byte	 $80, $80, $F0, $88, $88, $88, $88, $00      ; code for char h
                .byte	 $20, $00, $60, $20, $20, $20, $70, $00      ; code for char i
                .byte	 $10, $00, $30, $10, $10, $10, $90, $60      ; code for char j
                .byte	 $80, $80, $90, $A0, $C0, $A0, $90, $00      ; code for char k
                .byte	 $60, $20, $20, $20, $20, $20, $70, $00      ; code for char l
                .byte	 $00, $00, $D0, $A8, $A8, $88, $88, $00      ; code for char m
                .byte	 $00, $00, $F0, $88, $88, $88, $88, $00      ; code for char n
                .byte	 $00, $00, $70, $88, $88, $88, $70, $00      ; code for char o
                .byte	 $00, $00, $F0, $88, $88, $F0, $80, $80      ; code for char p
                .byte	 $00, $00, $78, $88, $88, $78, $08, $08      ; code for char q
                .byte	 $00, $00, $B8, $C0, $80, $80, $80, $00      ; code for char r
                .byte	 $00, $00, $78, $80, $70, $08, $F0, $00      ; code for char s
                .byte	 $20, $20, $70, $20, $20, $28, $10, $00      ; code for char t
                .byte	 $00, $00, $88, $88, $88, $88, $78, $00      ; code for char u
                .byte	 $00, $00, $88, $88, $88, $50, $20, $00      ; code for char v
                .byte	 $00, $00, $88, $88, $A8, $A8, $50, $00      ; code for char w
                .byte	 $00, $00, $88, $50, $20, $50, $88, $00      ; code for char x
                .byte	 $00, $00, $88, $88, $88, $78, $08, $70      ; code for char y
                .byte	 $00, $00, $F8, $10, $20, $40, $F8, $00      ; code for char z
                .byte	 $38, $60, $60, $C0, $60, $60, $38, $00      ; code for char {
                .byte	 $20, $20, $20, $20, $20, $20, $20, $00      ; code for char |
                .byte	 $E0, $30, $30, $18, $30, $30, $E0, $00      ; code for char }
                .byte	 $00, $00, $00, $68, $B0, $00, $00, $00      ; code for char ~
                .byte	 $00, $00, $00, $00, $00, $00, $00, $00      ; code for char DEL
                
; ROM PAGE 6 ($1800 - $1BFF and $5800 - $5BFF) *********************************

		.endl
		ORG	B04_4.START - FGC_BASE + B04_ADDR	; address in eeprom
B04_4		.local,	$1800

START         	.byte	      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF  ; 16 bytes reserved space
 	        .byte	      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF  ; for FGC IO addresses
 	        
 	        RTS
                NOP
                NOP

FGC_SET_PAGE_7  STA     PIA_PORTC       ; switch ROM page
                LDY     #$00
                STY     ADR_CHAR
                LDA     #> B04_1.PAGE_START
                STA     ADR_CHAR+1
FONT_LOOP21     LDA     (ADR_CHAR),Y
                STA     VPU_PORT0
                INY
                BNE     FONT_LOOP21
                INC     ADR_CHAR+1
FONT_LOOP22     LDA     (ADR_CHAR),Y
                STA     VPU_PORT0
                INY
                BNE     FONT_LOOP22
                LDA     #$07
                BNE     FGC_SET_PAGE_7  ; switch to ROM page 7

; **** Character Table Part 2 **************************************************
                
		.endl
		ORG	B04_5.START - FGC_BASE + B04_ADDR	; address in eeprom
B04_5		.local,	$1A00 	; 2560 bytes offset from page start
                
START           .byte	 $30, $48, $E0, $40, $E0, $48, $30, $00      ; code for char €
                .byte	 $A8, $50, $A8, $50, $A8, $50, $A8, $00      ; code for char CURSOR2
                .byte	 $00, $00, $00, $00, $60, $20, $40, $00      ; code for char ‚
                .byte	 $18, $20, $20, $70, $20, $20, $C0, $00      ; code for char ƒ
                .byte	 $00, $00, $00, $00, $50, $50, $A0, $00      ; code for char „
                .byte	 $00, $00, $00, $00, $00, $A8, $00, $00      ; code for char …
                .byte	 $20, $20, $70, $20, $20, $20, $20, $00      ; code for char †
                .byte	 $20, $20, $70, $20, $70, $20, $20, $00      ; code for char ‡
                .byte	 $20, $50, $00, $00, $00, $00, $00, $00      ; code for char ˆ
                .byte	 $00, $48, $10, $20, $40, $A8, $00, $00      ; code for char ‰
                .byte	 $50, $20, $78, $80, $70, $08, $F0, $00      ; code for char Š
                .byte	 $00, $10, $20, $40, $20, $10, $00, $00      ; code for char ‹
                .byte	 $78, $A0, $A0, $B8, $A0, $A0, $78, $00      ; code for char Œ
                .byte	 $00, $00, $F8, $08, $E8, $28, $28, $28      ; code for char 
                .byte	 $50, $20, $F8, $10, $20, $40, $F8, $00      ; code for char Ž
                .byte	 $00, $00, $3C, $20, $2C, $28, $28, $28      ; code for char 
                .byte	 $28, $28, $2C, $20, $3C, $00, $00, $00      ; code for char 
                .byte	 $10, $20, $30, $00, $00, $00, $00, $00      ; code for char ‘
                .byte	 $00, $00, $00, $00, $60, $20, $40, $00      ; code for char ’
                .byte	 $28, $50, $50, $00, $00, $00, $00, $00      ; code for char “
                .byte	 $00, $00, $00, $00, $50, $50, $A0, $00      ; code for char ”
                .byte	 $00, $00, $30, $78, $78, $30, $00, $00      ; code for char •
                .byte	 $00, $00, $00, $00, $70, $00, $00, $00      ; code for char –
                .byte	 $00, $00, $00, $00, $FC, $00, $00, $00      ; code for char —
                .byte	 $50, $A0, $00, $00, $00, $00, $00, $00      ; code for char ˜
                .byte	 $00, $00, $FC, $5C, $54, $00, $00, $00      ; code for char ™
                .byte	 $50, $20, $78, $80, $70, $08, $F0, $00      ; code for char š
                .byte	 $00, $40, $20, $10, $20, $40, $00, $00      ; code for char ›
                .byte	 $00, $00, $50, $A8, $B8, $A0, $58, $00      ; code for char œ
                .byte	 $28, $28, $E8, $08, $F8, $00, $00, $00      ; code for char 
                .byte	 $50, $20, $F8, $10, $20, $40, $F8, $00      ; code for char ž
                .byte	 $88, $00, $88, $50, $20, $20, $20, $00      ; code for char Ÿ
                .byte	 $28, $28, $E8, $08, $E8, $28, $28, $28      ; code for char  
                .byte	 $20, $00, $20, $20, $20, $20, $20, $00      ; code for char ¡
                .byte	 $00, $10, $38, $40, $40, $40, $38, $10      ; code for char ¢
                .byte	 $30, $48, $40, $E0, $40, $60, $98, $00      ; code for char £
                .byte	 $00, $48, $30, $48, $48, $30, $48, $00      ; code for char ¤
                .byte	 $88, $50, $20, $70, $20, $70, $20, $00      ; code for char ¥
                .byte	 $20, $20, $20, $00, $20, $20, $20, $00      ; code for char ¦
                .byte	 $78, $80, $70, $88, $70, $08, $F0, $00      ; code for char §
                .byte	 $00, $50, $00, $00, $00, $00, $00, $00      ; code for char ¨
                .byte	 $78, $84, $B4, $C4, $C4, $B4, $84, $78      ; code for char ©
                .byte	 $38, $08, $38, $38, $00, $00, $00, $00      ; code for char ª
                .byte	 $00, $28, $50, $A0, $50, $28, $00, $00      ; code for char «
                .byte	 $00, $00, $00, $F0, $10, $00, $00, $00      ; code for char ¬
                .byte	 $00, $00, $00, $00, $60, $00, $00, $00      ; code for char ­
                .byte	 $78, $84, $E4, $D4, $E4, $D4, $84, $78      ; code for char ®
                .byte	 $00, $F8, $00, $00, $00, $00, $00, $00      ; code for char ¯
                .byte	 $30, $48, $48, $30, $00, $00, $00, $00      ; code for char °
                .byte	 $00, $20, $70, $20, $00, $70, $00, $00      ; code for char ±
                .byte	 $60, $10, $20, $40, $70, $00, $00, $00      ; code for char ²
                .byte	 $70, $10, $20, $10, $60, $00, $00, $00      ; code for char ³
                .byte	 $10, $20, $00, $00, $00, $00, $00, $00      ; code for char ´
                .byte	 $00, $00, $00, $90, $90, $90, $E8, $80      ; code for char µ
                .byte	 $00, $78, $D0, $D0, $50, $50, $50, $00      ; code for char ¶
                .byte	 $00, $00, $00, $20, $00, $00, $00, $00      ; code for char ·
                .byte	 $00, $00, $00, $00, $00, $60, $10, $70      ; code for char ¸
                .byte	 $20, $60, $20, $20, $20, $00, $00, $00      ; code for char ¹
                .byte	 $30, $48, $30, $00, $78, $00, $00, $00      ; code for char º
                .byte	 $00, $50, $28, $14, $28, $50, $00, $00      ; code for char »
                .byte	 $00, $00, $FC, $00, $FC, $00, $00, $00      ; code for char ¼
                .byte	 $28, $28, $28, $28, $28, $28, $28, $28      ; code for char ½
                .byte	 $28, $28, $2C, $20, $2C, $28, $28, $28      ; code for char ¾
                .byte	 $20, $00, $20, $40, $80, $88, $70, $00      ; code for char ¿
                
; ROM PAGE 7 ($1C00 - $1FFF and $5C00 - $5FFF) *********************************

		.endl
		ORG	B04_6.START - FGC_BASE + B04_ADDR	; address in eeprom
B04_6		.local,	$1C00

START         	.byte	      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF  ; 16 bytes reserved space
 	        .byte	      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF  ; for FGC IO addresses
 	        
 	        RTS
                NOP
                NOP

FGC_SET_PAGE_1  STA     PIA_PORTC       ; switch ROM page
                LDY     #$00
                STY     ADR_CHAR
                LDA     #> B04_1.PAGE_START
                STA     ADR_CHAR+1
FONT_LOOP31     LDA     (ADR_CHAR),Y
                STA     VPU_PORT0
                INY
                BNE     FONT_LOOP31
                INC     ADR_CHAR+1
FONT_LOOP32     LDA     (ADR_CHAR),Y
                STA     VPU_PORT0
                INY
                BNE     FONT_LOOP32
                LDA     #$01
                BNE     FGC_SET_PAGE_1  ; switch to ROM page 1

; **** Character Table Part 3 **************************************************
                
		.endl
		ORG	B04_7.START - FGC_BASE + B04_ADDR	; address in eeprom
B04_7		.local,	$1E00 					; $E00 bytes offset from page start
                
START           .byte	 $40, $20, $70, $88, $F8, $88, $88, $00      ; code for char À
                .byte	 $10, $20, $70, $88, $F8, $88, $88, $00      ; code for char Á
                .byte	 $20, $50, $70, $88, $F8, $88, $88, $00      ; code for char Â
                .byte	 $50, $A0, $70, $88, $F8, $88, $88, $00      ; code for char Ã
                .byte	 $88, $00, $70, $88, $F8, $88, $88, $00      ; code for char Ä
                .byte	 $70, $50, $70, $88, $F8, $88, $88, $00      ; code for char Å
                .byte	 $78, $A0, $A0, $F8, $A0, $A0, $B8, $00      ; code for char Æ
                .byte	 $70, $88, $80, $80, $88, $70, $20, $30      ; code for char Ç
                .byte	 $40, $20, $F8, $80, $F0, $80, $F8, $00      ; code for char È
                .byte	 $10, $20, $F8, $80, $F0, $80, $F8, $00      ; code for char É
                .byte	 $20, $50, $F8, $80, $F0, $80, $F8, $00      ; code for char Ê
                .byte	 $88, $00, $F8, $80, $F0, $80, $F8, $00      ; code for char Ë
                .byte	 $40, $20, $70, $20, $20, $20, $70, $00      ; code for char Ì
                .byte	 $10, $20, $70, $20, $20, $20, $70, $00      ; code for char Í
                .byte	 $20, $50, $70, $20, $20, $20, $70, $00      ; code for char Î
                .byte	 $50, $00, $70, $20, $20, $20, $70, $00      ; code for char Ï
                .byte	 $70, $48, $48, $E8, $48, $48, $70, $00      ; code for char Ð
                .byte	 $28, $50, $88, $C8, $A8, $98, $88, $00      ; code for char Ñ
                .byte	 $40, $20, $70, $88, $88, $88, $70, $00      ; code for char Ò
                .byte	 $10, $20, $70, $88, $88, $88, $70, $00      ; code for char Ó
                .byte	 $20, $50, $70, $88, $88, $88, $70, $00      ; code for char Ô
                .byte	 $50, $A0, $70, $88, $88, $88, $70, $00      ; code for char Õ
                .byte	 $88, $70, $88, $88, $88, $88, $70, $00      ; code for char Ö
                .byte	 $00, $88, $50, $20, $50, $88, $00, $00      ; code for char ×
                .byte	 $68, $90, $98, $A8, $C8, $48, $B0, $00      ; code for char Ø
                .byte	 $40, $20, $88, $88, $88, $88, $70, $00      ; code for char Ù
                .byte	 $10, $20, $88, $88, $88, $88, $70, $00      ; code for char Ú
                .byte	 $20, $50, $00, $88, $88, $88, $70, $00      ; code for char Û
                .byte	 $88, $00, $88, $88, $88, $88, $70, $00      ; code for char Ü
                .byte	 $10, $20, $88, $50, $20, $20, $20, $00      ; code for char Ý
                .byte	 $00, $40, $70, $48, $70, $40, $00, $00      ; code for char Þ
                .byte	 $F0, $88, $88, $F0, $88, $88, $B0, $80      ; code for char ß
                .byte	 $40, $20, $70, $08, $78, $88, $78, $00      ; code for char à
                .byte	 $10, $20, $70, $08, $78, $88, $78, $00      ; code for char á
                .byte	 $20, $50, $70, $08, $78, $88, $78, $00      ; code for char â
                .byte	 $50, $A0, $70, $08, $78, $88, $78, $00      ; code for char ã
                .byte	 $00, $88, $70, $08, $78, $88, $78, $00      ; code for char ä
                .byte	 $30, $30, $70, $08, $78, $88, $78, $00      ; code for char å
                .byte	 $00, $00, $F0, $28, $78, $A0, $78, $00      ; code for char æ
                .byte	 $00, $00, $78, $80, $80, $78, $20, $30      ; code for char ç
                .byte	 $40, $20, $70, $88, $F8, $80, $78, $00      ; code for char è
                .byte	 $10, $20, $70, $88, $F8, $80, $78, $00      ; code for char é
                .byte	 $20, $50, $70, $88, $F8, $80, $78, $00      ; code for char ê
                .byte	 $00, $88, $70, $88, $F8, $80, $78, $00      ; code for char ë
                .byte	 $40, $20, $00, $60, $20, $20, $70, $00      ; code for char ì
                .byte	 $10, $20, $00, $60, $20, $20, $70, $00      ; code for char í
                .byte	 $20, $50, $00, $60, $20, $20, $70, $00      ; code for char î
                .byte	 $00, $50, $00, $60, $20, $20, $70, $00      ; code for char ï
                .byte	 $28, $10, $28, $78, $88, $88, $70, $00      ; code for char ð
                .byte	 $28, $50, $00, $F0, $88, $88, $88, $00      ; code for char ñ
                .byte	 $40, $20, $00, $70, $88, $88, $70, $00      ; code for char ò
                .byte	 $10, $20, $00, $70, $88, $88, $70, $00      ; code for char ó
                .byte	 $20, $50, $00, $70, $88, $88, $70, $00      ; code for char ô
                .byte	 $50, $A0, $00, $70, $88, $88, $70, $00      ; code for char õ
                .byte	 $00, $88, $70, $88, $88, $88, $70, $00      ; code for char ö
                .byte	 $00, $20, $00, $F8, $00, $20, $00, $00      ; code for char ÷
                .byte	 $00, $00, $68, $90, $A8, $48, $B0, $00      ; code for char ø
                .byte	 $40, $20, $00, $88, $88, $88, $78, $00      ; code for char ù
                .byte	 $10, $20, $00, $88, $88, $88, $78, $00      ; code for char ú
                .byte	 $20, $50, $00, $88, $88, $88, $78, $00      ; code for char û
                .byte	 $00, $88, $00, $88, $88, $88, $78, $00      ; code for char ü
                .byte	 $10, $20, $00, $88, $88, $78, $08, $70      ; code for char ý
                .byte	 $80, $80, $F0, $88, $88, $F0, $80, $80      ; code for char þ
                .byte	 $00, $88, $00, $88, $88, $78, $08, $70      ; code for char ÿ

		.endl
		