;-------------------------------------------------
; JC2 BIOS Calls, checked for: V1.2.5
; Assembler: MADS-Assembler
;-------------------------------------------------

LAB_COLD	=	$B000			; BASIC cold START
MON_COLD_START  EQU     $E000
MON_WARM_START  EQU     $E003
SWITCH_TO_RAM	EQU	$E006			; Switch Basic to RAM Page ($B000-$DFFF)
SWITCH_TO_ROM	EQU	$E00A			; Switch Basic to ROM Page ($B000-$DFFF)
_NO_HANDLER_	EQU	$E00E			; Just RTS
CIN             EQU     $E047
COUT            EQU     $E052
CROUT           EQU     $E05A
SPCOUT          EQU     $E05E
STRIN           EQU     $E062
STROUT          EQU     $E083
WRSTR           EQU     $E085
HEXOUT          EQU     $E091
HEXDIG          EQU     $E09A
CLRSCRN         EQU     $E0B5
DEC2STR         EQU     $E0BD
CMDDEV          EQU     $E0BA
ADD_DEVICE	EQU	$E1A7
DELAY		EQU	$E14D
BEEP            EQU     $E167
OPEN_DEVICE     EQU     $E1AA
READTIME        EQU     $E2DE			; BIOS v1.2.1
READDATE        EQU     $E2E2			; BIOS v1.2.1

OS_STRING_OUT	EQU	$F682			; SPRINT in BIOS v1.2.1, but same routine
INIT_BLKBUF	EQU	$F68B
CFWAIT		EQU	$F8B5
VPU_IRQ		EQU	$F960
VDP_DEV		EQU	$F9A0
LOAD_LBA_CF	EQU	$FC85
SET_RAMBANK	EQU	$FD53			; BIOS v1.2.4
MON2ROM		EQU	$FD7D			; Monitor to ROM
MON2RAM		EQU	$FD86			; Monitor to RAM, enable RAM behind it


