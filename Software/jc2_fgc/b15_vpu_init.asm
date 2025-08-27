; ------------------------------------------------------------------------------
; Floppy/Graphics-Controller BIOS for the Junior Computer ][, by Joerg Walke.
;
; Main Driver Initialization, part of the Floppy-/Graphics-Controller (FGC) ROM
; ------------------------------------------------------------------------------

B15_ADDR	; FGC ROM PAGE 15 at $3C00 - $3CFF and $7C00 - $7CFF, size 1 KB
B15		.local, PROG_START		; in RAM

FGC_REGS	.byte	$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF  ; 16 bytes reserved space
 	        .byte	$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF  ; for FGC IO addresses

; **** Main Initialisation Of The Floppy-/Graphics-Controller *******************

FGC_INIT        SEI			; JC2 address in ROM: $1010
                LDX     #< VPU_IRQ   	; install interrupt handler
                STX     IRQVECT
                LDX     #> VPU_IRQ
                STX     IRQVECT+1
                CLI
                LDX     #< VDP_DEV
                LDY     #> VDP_DEV
                JSR     ADD_DEVICE      ; add VDP and PS/2 Keyboard driver
                STA     DEVID           ; store device ID
                LDX     #< B15_1.FGC_PRINT_INFO
                LDY     #> B15_1.FGC_PRINT_INFO
                STX     IO_INFO      	; set pointer to card info routine
                STY     IO_INFO+1
FGC_READ_DIP_SW LDA     FGC_START_CONF  ; release microcontroller data port for reading
                JSR     FGC_WAIT        ; and wait for some microseconds
                LDA     #$92            ; set Port A = Input; Port B = Input; Port C = Output
                STA     PPI_CONTROL
                LDA     #$BF            ; set Port C pin 6 = L to enable DIP switch reading
                STA     PPI_PORTC
                LDA     PPI_PORTA       ; load dip switch settings
                TAX                     ; save DIP switch settings to X
                LDA     FGC_END_CONF    ; read keyboard layout and reset microcontroller data port for writing
                JSR     FGC_WAIT        ; and wait for some microseconds
                LDA     #$B2            ; set Port A to handshake input mode and Port B to input mode
                STA     PPI_CONTROL
                LDA     #$0D            ; set Port C pin 6 = H to disable DIP switch reading
                STA     PPI_CONTROL
                TXA                     ; reload DIP switch settings from X to A
                AND     #$0F            ; mask out floppy type bits
                STA     FLOPPY_TYPE     ; save lower nibble as FLOPPY_TYPE
                TXA                     ; reload DIP switch settings from X to A
        :4      LSR                     ; and shift keymap bits to lower nibble
                TAX
                LDA     KBD_MAP_ID,X    ; ...decode DIP switch settings to Map ID
                STA     KBD_LANG        ; and store result to KBD_LANG
                
; **** PS2 Keyboard Initialization *********************************************
                
                CMP     #PS2NO_KBD      ; any PS2 keyboard type selected?
                BEQ     FGC_INIT_END    ; no, just exit.
                LDA     DEVID           ; yes, load VDP Device ID
		STA	STDINDEV	; and make it the standard input device
FGC_INIT_END    RTS

; **** Wait For Some Microseconds **********************************************

FGC_WAIT        LDY     $06
FGC_WAIT2       NOP
                DEY
                BNE     FGC_WAIT2
                RTS
                
; **** DIP-Switch to Language Conversion Table **********************************

KBD_MAP_ID      .byte	$06, $03, $05, $01, $06, $02, $04, $00
                
; **** Print Controller Info ***************************************************
		.endl
		ORG	B15_1.FGC_PRINT_INFO - PROG_START + B15_ADDR	; address in eeprom
B15_1		.local,	$0300		; JC2 address $1100 in ROM, $0300 in RAM

FGC_PRINT_INFO  LDX     #< FGC_INFO_STR1
                LDY     #> FGC_INFO_STR1
                JSR     FGC_STROUT
                JSR     FGC_ADDROUT
FGC_PRINT_INFO2 LDA     KBD_LANG
                CMP     #PS2NO_KBD      ; any PS2 keyboard type selected?
                BEQ     FGC_INFO_END    ; no, just exit
                ASL     
                TAY
                JSR     SPCOUT
                LDA     KBD_LANG_NAME,Y
                JSR     COUT
                LDA     KBD_LANG_NAME+1,Y
                JSR     COUT
                LDX     #< KBD_INFO_STR
                LDY     #> KBD_INFO_STR
FGC_STROUT      LDA     PSTRL
                PHA
                LDA     PSTRH
                PHA
                STX     PSTRL
                STY     PSTRH
                JSR     STROUT
                PLA
                STA     PSTRH
                PLA
                STA     PSTRL
                RTS
FGC_ADDROUT     LDA	FGCBASEH
		JSR	HEXOUT
		LDA	#$00
		JSR	HEXOUT
		JSR     CROUT
FGC_INFO_END    RTS

; **** Strings *****************************************************************

FGC_INFO_STR1   .by    ' FGC BIOS V'
                .byte  VERMAIN,$2E,VERPSUB,$2E,VERSSUB
	        .by    ' by Emile' CR $00
   		.by    ' V9938 VPU at $' $00
                
KBD_LANG_NAME   .by    'US' 'DE' 'FR' 'ES' 'IT' 'UK'

KBD_INFO_STR    .by	' type PS/2 Keyboard enabled' CR $00
                
; **** Main Initialization Routine *********************************************

		.endl
		ORG	B15_2.MAIN_START - PROG_START + B15_ADDR	; address in eeprom
B15_2		.local,	$05D0		; JC2 address $13D0
                
; ******************************************************************************

MAIN_START      STX     FGCBASEH        ; set FGC base address high byte
                STX     TEMP            ; and save a copy to TEMP
                LDA     #< B15.FGC_REGS
                STA     PSTRL
                LDA     #> B15.FGC_REGS	; set destination address
                STA     PSTRH
                LDX     #$04            ; four pages to copy
                LDY     #$10            ; start at offset $10
LOOP1           LDA     (FGCBASE),Y     ; load ROM content
                STA     (PSTR),Y        ; and copy it to destination address
                INY                     ; next byte
                BNE     LOOP1           ; all bytes in page copied? No copy more
                INC     FGCBASEH        ; yes, increment ROM base address by $100
                INC     PSTRH           ; and also increment destination address by $100
                DEX                     ; next page
                BNE     LOOP1           ; all pages copied? No copy more
                LDA     TEMP            ; yes, restore original FGC base address high byte
                STA     FGCBASEH
                JMP     B15.FGC_INIT    ; jump to FGC initialization routine in RAM

; **** Main Initialization Entry Point *****************************************

		.endl
		ORG	B15_3.START - PROG_START + B15_ADDR	; address in eeprom
B15_3		.local,	$05FB			; JC2 address $13FB
                
START           CLC                     	; short jump code to MAIN_START and
                BCC     NEXT            	; also MAGIC NUMBER $18 $90 $00 $90
NEXT            BCC     B15_2.MAIN_START	; to detect card
                
		.endl
		