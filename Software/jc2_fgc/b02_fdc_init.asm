; ------------------------------------------------------------------------------
; Floppy/Graphics-Controller BIOS for the Junior Computer ][, by Joerg Walke.
;
; Floppy Disc Controller, part of the Floppy-/Graphics-Controller (FGC) ROM.
; Not implemented yet.
; ------------------------------------------------------------------------------

B02_ADDR	; ROM PAGE 2 ($0800 - $0BFF and $4800 - $4BFF), size 1 KB
B02		.local, FGC_BASE

         	.byte	$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF  ; 16 bytes reserved space
 	        .byte   $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF  ; for FGC IO addresses

                RTS
                NOP
                NOP
                
FGC_SET_PAGE_2  STA     PIA_PORTC       ; switch ROM page
                RTS
                
FGC_FDC_CMD     PHA                     ; save Accumulator
                LDA     #$02
                STA     PIA_PORTC       ; switch to ROM page 2. Code continues in page 2
                BNE	PAGE2_X		; branch always
                
FGC_VPU_CMD     PHA                     ; save Accumulator
                LDA     #$01
                STA     PIA_PORTC       ; switch to ROM page 1. Code continues in page 1
                BEQ	PAGE2_X
                
FGC_VPU_OUT     PHA                     ; save Accumulator
                LDA     #$00
                STA     PIA_PORTC       ; switch to ROM page 0
                BEQ	PAGE2_X
                
_BLINK_HANDLER_ LDA     #$00
                STA     PIA_PORTC       ; switch to ROM page 0
PAGE2_X		PLA
		RTS

		.endl

		ORG	B02_ADDR + $0400	; Page 3
B03_ADDR	; ROM PAGE 3 ($0C00 - $0FFF and $4C00 - $4CFF), size 1 KB	
B03		.local,	FGC_BASE

START         	.byte	      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF  ; 16 bytes reserved space
 	        .byte	      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF  ; for FGC IO addresses
 	        
                RTS
                NOP
                NOP
                
FGC_SET_PAGE_3  STA     PIA_PORTC       ; switch ROM page
                RTS
                
FGC_FDC_CMD     PHA                     ; save Accumulator
                LDA     #$02
                STA     PIA_PORTC       ; switch to ROM page 2. Code continues in page 2
                BNE	PAGE3_X		; branch always
                
FGC_VPU_CMD     PHA                     ; save Accumulator
                LDA     #$01
                STA     PIA_PORTC       ; switch to ROM page 1. Code continues in page 1
                BEQ	PAGE3_X
                
FGC_VPU_OUT     PHA                     ; save Accumulator
                LDA     #$00
                STA     PIA_PORTC       ; switch to ROM page 0
                BEQ	PAGE3_X
                
_BLINK_HANDLER_ LDA     #$00
                STA     PIA_PORTC       ; switch to ROM page 0
PAGE3_X		PLA
		RTS

       		.endl
