; ------------------------------------------------------------------------------
; Junior Computer ][ BIOS Version 1.1.4
; by Joerg Walke
;
; 14-04-25: Emile, Integration into 1 eprom
; ------------------------------------------------------------------------------
        	OPT h-                          ; no DOS file-header
        	OPT f+                          ; save as single block

        	icl 	"jc2_defines.inc"  	; all address defines for the JC-II

		;ORG	$8000
		;.byte	$FF			; Fill entire eprom
		
		ORG 	$B000       		; start address of BASIC
RAM_TOP		icl 	"jc2_basic.asm"		; end of user RAM+1 (set as needed, should be page aligned)
		icl	"jc2_cf_ide.asm"	; cf-ide driver to be placed in free BASIC ROM area
		
		ORG 	$DFF0
LANGKEY		.byte	'B'
LANGNAME 	.by	'(B)asic' $00
ROM_CS16	.word	$0000			; 12K ROM checksum, to be filled in by jc2_csum.exe
		.byte	$00,$65,$22,$65,$22

		ORG 	$E000       		; start address of BIOS
		icl 	"jc2_bios.asm"

		ORG	$FC00			; start address of Monitor
;-------------------------------------------------------------------------------------------------------
MONITOR_BLOCK	.local, $1C00
;-------------------------------------------------------------------------------------------------------
		icl 	"jc2_mon.asm"
;----------------------------------------------------------------------------
MONITOR_END	
		.endl
;----------------------------------------------------------------------------

		ORG	$FFFA
;----------------------------------------------------------------------------
;       VECTORS AT THE END OF THE ROM AREA
;----------------------------------------------------------------------------
NMI_VECTOR	.word	MONITOR_BLOCK.NMI	; $1F2F in Junior Computer Monitor program
RESET_VECTOR	.word  	MON_COLD_START		; $E000 (was $1C1D in Junior Computer Monitor program)
IRQ_BRK_VECTOR	.word	MONITOR_BLOCK.IRQ	; $1F32 in Junior Computer Monitor program

		END