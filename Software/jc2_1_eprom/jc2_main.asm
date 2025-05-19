; ------------------------------------------------------------------------------
; Junior Computer ][ BIOS Version 1.2.2 by Joerg Walke
;
; 21-04-25: v1.2.0 Emile, Integration into 1 eprom
; 08-05-25: v1.2.1 -Basic update (v2.22 patches + I2COUT, I2CIN and DOS commands)
;                  -CF_LOAD_VEC/CF_SAVE_VEC added, RETURN_VECT moved to $180A 
;                  -Address corrections to maintain V1.1.4 compatibility (broken in V1.2.0)
; 19-05-25: v1.2.2 -ZP addresses reorganised to avoid conflicts between Basic, BIOS & boot.sys.
;		   -NMI & IRQ JMP vectors moved from Monitor to here to prepare for Monitor ROM switching
; ------------------------------------------------------------------------------
VERMAIN   	EQU     '1'    			; BIOS main version
VERPSUB    	EQU     '2'    			; BIOS primary sub version
VERSSUB		EQU	'2'			; BIOS secondary sub version

        	OPT h-                          ; no DOS file-header
        	OPT f+                          ; save as single block

        	icl 	"jc2_defines.inc"  	; all address defines for the JC-II

		ORG	$8000
		.byte	$FF			; Fill entire eprom
		
		ORG	$9C00			; start address of 1K Monitor in 32K combined EPROM
;-------------------------------------------------------------------------------------------------------
MONITOR_BLOCK	.local, $1C00
;-------------------------------------------------------------------------------------------------------
		icl 	"jc2_mon.asm"		; Monitor source-file
;-------------------------------------------------------------------------------------------------------
MONITOR_END	
		.endl
;-------------------------------------------------------------------------------------------------------

		ORG 	$B000       		; start address of BASIC (12K)
RAM_TOP		icl 	"jc2_basic.asm"		; end of user RAM+1 (set as needed, should be page aligned)
		
		ORG	$DFD8
;----------------------------------------------------------------------------
; This function enables BIOS-ROM at $E000-$FFFF and disables the RAM behind it.
;----------------------------------------------------------------------------
BIOS2ROM	LDA	MMU		; MMU-register
		ORA	#BIOS_EN	; 1 = enable BIOS ROM
		STA	MMU		; Activate BIOS ROM
		RTS			; return
		
;----------------------------------------------------------------------------
; This function Enables BIOS-RAM at $E000-$FFFF and disables BIOS-ROM.
;----------------------------------------------------------------------------
BIOS2RAM	LDA	MMU		; MMU-register
		AND	#~BIOS_EN	; 0 = enable BIOS RAM, disable ROM
		STA	MMU		; Activate BIOS RAM
		RTS			; return

		ORG 	$DFF0
LANGKEY		.byte	'B'
LANGNAME 	.by	'(B)asic' $00

		ORG 	$E000       		; start address of BIOS (8K)
		icl 	"jc2_bios.asm"

		ORG	$FC00			; start address of CF-IDE and MMU routines
		icl	"jc2_cf_ide.asm"	; cf-ide and MMU routines
		
;----------------------------------------------------------------------------
;       VECTORS AT THE END OF THE ROM AREA
;----------------------------------------------------------------------------
		ORG	$FFEE
NMI_JMP_VEC    	JMP     (NMIVECT)		; Jump to a user selectable NMI vector (moved from Monitor $1F2F)
IRQ_JMP_VEC    	JMP     (IRQVECT)		; Jump to a user selectable IRQ vector (moved from Monitor $1F32)
MON_CS16	.word	$000			;  1 KB Monitor ROM checksum
BAS_CS16	.word	$000			; 12 KB BASIC ROM checksum
ROM_CS16	.word	$000			;  8 KB BIOS ROM checksum
NMI_VECTOR	.word	NMI_JMP_VEC		; This was $1F2F in Junior Computer Monitor program
RESET_VECTOR	.word  	MON_COLD_START		; $E000 (was $1C1D in Junior Computer Monitor program)
IRQ_BRK_VECTOR	.word	IRQ_JMP_VEC		; This was $1F32 in Junior Computer Monitor program

		END