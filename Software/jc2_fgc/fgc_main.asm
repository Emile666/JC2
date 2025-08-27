; ------------------------------------------------------------------------------
; Floppy/Graphics-Controller BIOS V0.2.2 for the Junior Computer ][, by Emile
; V0.2.0 by Joerg Walke.
;
; EPROM code is divided into 16 pages of 1K, called BANK 00..31. The lower 16K
; of the FGC - BANK 00..BANK 15 - are mirrored to the higher 16K - BANK 16..BANK 31.
; JP4 now selects which 16 KB ROM area will be used:
; 1-2 (27C256) = lower 16 KB ROM area, 2-3 (28C256) = higher 16 KB ROM area
;
; FGC_VPU      : BANK 00   $0000-$03FF
;                BANK 16   $4000-$43FF (Mirror)
;
; FGC_VPU_CMD  : BANK 01   $0400-$07FF
;                BANK 17   $4400-$47FF (Mirror)
;                
; FGC_FLOPPY     BANK 02 - 03	$0800-$0FFF (not yet implemented)
; FGC_FLOPPY     BANK 18 - 19	$4800-$4CFF (Mirror)
;
; FGC_CHAR_ROM : BANK 04 - 07	$1000-$1FFF
;                BANK 20 - 23	$5000-$5FFF  (Mirror)
;               
; EMPTY          BANK 08..14
; EMPTY          BANK 24..30
;
; FGC_INIT_ROM : BANK 15   $3C00-$3FFF
;                BANK 31   $7C00-$7FFF  (Mirror)
; ------------------------------------------------------------------------------
VERMAIN   	EQU     '0'    			; FGC main version
VERPSUB    	EQU     '2'    			; FGC primary sub version
VERSSUB		EQU	'2'			; FGC secondary sub version

; List Of Changes **************************************************************
; V0.2  : Original version from Joerg Walke, May 14th, 2025
; V0.2.1: Adapted for MADS by Emile, eprom binary is now identical to Joerg's version ($0000-$3FFF)
; V0.2.2: source include files reorganised, one system wide definition of ZP vars
; ******************************************************************************
		OPT h- ; do not add file header
		OPT f+ ; save as single block

		ICL "jc2_macros.inc"		; JC2 macro definitions for MADS
		ICL "jc2_defines.inc"		; JC2 system-defines
		ICL "jc2_constants.inc"		; JC2 constants
		ICL "jc2_bios_calls.inc"	; JC2 BIOS routines
		
PROG_START	EQU	$0200	      		; Program Start Address in RAM
PS2NO_KBD       EQU     $06           		; No keyboard selected

; ------------------------------------------------------------------------------
; EPROM Code Size is from $0000 - $3FFF (lowest 16 KB) and from
; $4000 - $7FFF for the highest 16 KB. Every 1 KB page is assembed to 
; base-address FGC_BASE ($1000). The active page is selected by the 
; 8255 PIA PORTC PC7,PC2,PC1,PC0 ($100E).
; ------------------------------------------------------------------------------
		ORG	$0000			; Page 0, Eprom Start address
		ICL 	"b00_vpu.asm"		; VPU Driver

		ORG	$0400			; Page 1
		ICL 	"b01_vpu_cmd.asm"	; VPU Command Driver

		; Pages 2 & 3: Floppy Disc Controller (FDC)
		ORG	$0800			; Pages 2 & 3
		ICL 	"b02_fdc_init.asm"	; FDC, not implemented yet

		ORG	$1000			; Page 4
		ICL 	"b04_char_tbl.asm"	; VPU Character table
		
		ORG	$3C00			; Page 15
		ICL 	"b15_vpu_init.asm"	; VPU Init
		
		ORG	$7FFF
		.byte	$FF			; Make .bin file 32K
		END