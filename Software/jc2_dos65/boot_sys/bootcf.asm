; ******************************************************************************
; The Disk Operating System for 6502 Microprocessors
;
; JC2 DOS65, Version 0.2.9 by Emile, original design (V0.2.0) by Joerg Walke
;
; Developed for the Junior Computer ][
;
; First implementation 6.7.2023, updated 20.06.2024, by Joerg Walke
; Assembled with A65, Converted to MADS by Emile
;
; DOS65 is primarily a small Disk Operation System (DOS) based on the FAT
; filesystem. This version is for the CF-IDE interface with FAT32 only.
; A CF-Card can be partitioned with up to four primary partitions.
; Using the MKBOOTCF program you can write the needed Partition Boot Block and
; the Master Boot Record which then holds a menu program to select one of the
; available partitions for booting.
;
; This code is freely available under the Creative Commons Attribution 4.0
; International license, see https://creativecommons.org/licenses/by/4.0/
; ******************************************************************************

VERMAIN   	EQU     '0'    		; main version
VERPSUB    	EQU     '2'    		; primary sub version
VERSSUB		EQU	'9'		; secondary sub version

; List Of Changes **************************************************************
; V0.2.1: 11-04-25 Emile boot.asm renamed in bootcf.asm and adapted for CF-IDE interface.
; V0.2.2: 19-05-25 ZP-vars reorganised, too much overlap with BIOS
; V0.2.3: - OS_LOAD_COM renamed in OS_LOAD_FILE, now loads .bas, .com and .exe files
;         - Macro's added, start of code-reorganisation.
;	  - File (3350 lines) split in separate smaller files.
; V0.2.4: - .BAS LOAD file now works, filename bug-fix and NUM32 bug-fix
;	  - Bug-fix MKDIR run after DIR command
;	  - SIS added + total KB printed with DIR command
; V0.2.5: - DEL command added.
; V0.2.6: - SAVE from BASIC added, CFC_SAVE, OS_CREATE, OS_CREATE_FILE changed,
;           OS_SAVE_FILE and LINK_FAT_ENTRY added.
;         - VER command added.
;         - Bug-fixes OS_LOAD_FILE when 1) #clusters = 1 2) sectors/cluster = 1
; V0.2.7: Couple of bug-fixes with path-name string and CD command.
; V0.2.8: Enabling RAM-BANK 4 for BASIC and RAM-BANK 0 for DOS. Now Basic programs
;         can load all the way up to $AFFF.
; V0.2.9: BASIC related DOS functions are now copied into Monitor RAM area.
; ******************************************************************************
		OPT h- ; do not add file header
		OPT f+ ; save as single block

;------------------------------------------------------------------------------------------------
; TERMINOLOGY AND FORMULAS USED:
; 
; Sector                : A sector consists of 512 bytes.
; Cluster               : Consists of one or more sectors as defined by D_SECT_PER_CLST.
;                         This is a 24-bit number relative to the cluster_begin_lba number.
; LBA                   : Logical Block Address, a 32-bit address for a sector on the CF/SD-card.
; Cluster_nr            : number of a cluster relative to the cluster_begin_lba Number
; root_dir_1st_cluster  : the 1st cluster in a partition, contains the root directory
; 
; fat_begin_lba         : D_START_FAT1 = partition_lba_begin + nr_of_reserved_sectors
; cluster_begin_lba     : D_START_CLS_LBA = fat_begin_lba + (number_of_FATs * Sectors_per_FAT)
; Sectors_per_Cluster   : D_SECT_PER_CLST (written by bootcode.fat32 during boot)
; root_dir_first_cluster: D_START_DIR, this is usually 2
; lba_addr = cluster_begin_lba + (cluster_nr - 2) * sectors_per_cluster
;------------------------------------------------------------------------------------------------
		ICL "macros.inc"		; boot.sys macro definitions for MADS
		ICL "defines.inc"		; boot.sys defines
		
.if	USE_XMODEM = 1
	.word	PROG_START			; Needed for XMODEM lm command loading .bin files
.endif

; ******************************************************************************
; Start of 1st Block of BOOT.SYS ***********************************************
; ******************************************************************************
LOAD_ADDRESS	EQU	$4000
		ORG	LOAD_ADDRESS		; the program start address
PROG_START					; Program Start Address
		ICL 	"boot_sys_block1.asm"	; boot.sys first block (< 512 bytes)
               
; ******************************************************************************
; Start of remaining BOOT.SYS blocks *******************************************
; ******************************************************************************
		ORG     LOAD_ADDRESS + $0200
BLOCK_2
		ICL 	"boot_sys_os.asm"	; OS portion of boot.sys
		ICL 	"boot_sys_sh.asm"	; Shell portion of boot.sys
                
; End Of Program Marker ********************************************************
BOOT_SYS_END    BRK
		ORG ((*/256)+1)*256		; next free page
SIS_BUFF	.ds 	512			; SIS Buffer 
		END