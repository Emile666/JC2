;-------------------------------------------------
; Register Definitions for the FGC BIOS.
; Assembler: MADS-Assembler
;-------------------------------------------------

PS2NO_KBD       EQU     $06           ; No keyboard selected

FGC_BASE        EQU     $1000

FGC_START_CONF  EQU     FGC_BASE+$06  ; prepare for DIP Switch reading
FGC_END_CONF    EQU     FGC_BASE+$07  ; read keyboard layout from DIP Switch settings
VPU_BASE        EQU     FGC_BASE+$08  ; VPU Register Base
PIA_PORTA       EQU     FGC_BASE+$0C  ; PIA Port A register
PIA_PORTC       EQU     FGC_BASE+$0E  ; PIA Port C
PIA_CONTROL     EQU     FGC_BASE+$0F  ; PIA Control register

IOBASE          EQU     $15           ; IO indirect pointer

; Device Descriptor Pointers ***************************************************

VDP_DEV         EQU     $F9A0
FDD1_DEV        EQU     $F9C0
FDD2_DEV        EQU     $F9C8

; **** System BIOS Routines ****************************************************

COUT            EQU     $E052
CROUT           EQU     $E05A
SPCOUT          EQU     $E05E
STROUT          EQU     $E083
HEXOUT          EQU     $E091
ADD_DEVICE      EQU     $E1A7
VPU_IRQ         EQU     $F948         ; VPU interrupt handler

CARD0_INFO      EQU     $1A00
STDIN		EQU	$1A6E
STDINDEV  	EQU   	$1A74
IRQVECT  	EQU   	$1A7E         ; main interrupt vector

; Video Processing Unit Ports

VPU_PORT0       EQU     VPU_BASE      ; Data Port
VPU_PORT1       EQU     VPU_BASE+1    ; Control Port
VPU_PORT2       EQU     VPU_BASE+2    ; Color Pallete Port
VPU_PORT3       EQU     VPU_BASE+3    ; Indirect Register Port

; Mode Registers

VPU_REG0        EQU     $80           ; Mode Register 0
VPU_REG1        EQU     VPU_REG0+1    ; Mode Register 1
VPU_REG8        EQU     VPU_REG0+8    ; Mode Register 2
VPU_REG9        EQU     VPU_REG0+9    ; Mode Register 3

; Table Base Address Registers

VPU_REG2        EQU     VPU_REG0+2    ; Pattern name table base address register
VPU_REG3        EQU     VPU_REG0+3    ; Color table base address register low
VPU_REG10       EQU     VPU_REG0+10   ; Color table base address register high
VPU_REG4        EQU     VPU_REG0+4    ; Pattern generator table base address register
VPU_REG5        EQU     VPU_REG0+5    ; Sprite attribute table base address register low
VPU_REG11       EQU     VPU_REG0+11   ; Sprite attribute table base address register high
VPU_REG6        EQU     VPU_REG0+6    ; Sprite pattern generator table base address register

; Color Registers

VPU_REG7        EQU     VPU_REG0+7    ; Text color/Back drop color register
VPU_REG12       EQU     VPU_REG0+12   ; Text blink color/Back blink color register
VPU_REG13       EQU     VPU_REG0+13   ; Blinking period register
VPU_REG20       EQU     VPU_REG0+20   ; Color burst register 1
VPU_REG21       EQU     VPU_REG0+21   ; Color burst register 2
VPU_REG22       EQU     VPU_REG0+22   ; Color burst register 3

; Display Registers

VPU_REG18       EQU     VPU_REG0+18   ; Display adjust register
VPU_REG23       EQU     VPU_REG0+23   ; Display offset register
VPU_REG19       EQU     VPU_REG0+19   ; Interrupt line register

; Access Registers

VPU_REG14       EQU     VPU_REG0+14   ; VRAM Access base address register
VPU_REG15       EQU     VPU_REG0+15   ; Status register pointer
VPU_REG16       EQU     VPU_REG0+16   ; Color pallete address register
VPU_REG17       EQU     VPU_REG0+17   ; Control register pointer

; Command Registers

VPU_REG32       EQU     VPU_REG0+32   ; Source X low register
VPU_REG33       EQU     VPU_REG0+33   ; Source X high register
VPU_REG34       EQU     VPU_REG0+34   ; Source Y low register
VPU_REG35       EQU     VPU_REG0+35   ; Source Y high register

VPU_REG36       EQU     VPU_REG0+36   ; Destination X low register
VPU_REG37       EQU     VPU_REG0+37   ; Destination X high register
VPU_REG38       EQU     VPU_REG0+38   ; Destination Y low register
VPU_REG39       EQU     VPU_REG0+39   ; Destination Y high register

VPU_REG40       EQU     VPU_REG0+40   ; Number of dots X low register
VPU_REG41       EQU     VPU_REG0+41   ; Number of dots X high register
VPU_REG42       EQU     VPU_REG0+42   ; Number of dots Y low register
VPU_REG43       EQU     VPU_REG0+43   ; Number of dots Y high register

VPU_REG44       EQU     VPU_REG0+44   ; Color register
VPU_REG45       EQU     VPU_REG0+45   ; Argument register
VPU_REG46       EQU     VPU_REG0+46   ; Command register

; Status Registers

VPU_STAT0       EQU     0             ; Status register 0
VPU_STAT1       EQU     1             ; Status register 1
VPU_STAT2       EQU     2             ; Status register 2
VPU_STAT3       EQU     3             ; Status register 3
VPU_STAT4       EQU     4             ; Status register 4
VPU_STAT5       EQU     5             ; Status register 5
VPU_STAT6       EQU     6             ; Status register 6
VPU_STAT7       EQU     7             ; Status register 7
VPU_STAT8       EQU     8             ; Status register 8
VPU_STAT9       EQU     9             ; Status register 9

; Basic Mode Settings

VPU_MODE_REG0   EQU     $00           ; DG = 0, IE2 = 0, IE1 = 0, M3..5 = xxx
VPU_MODE_REG1   EQU     $42           ; BL = 1, IE0 = 0, M1..2 = xx, SI = 1, MA = 0,
VPU_MODE_REG2   EQU     $0A           ; MS = 0, LP = 0, CB = 0, VR = 1, SPD = 1, BW = 0
VPU_MODE_REG3   EQU     $02           ; LN = 0, S1 = 0, S0 = 0, IL = 0, E = 0,NT = 1, DC = 0

VPU_MODE0       EQU     $10
VPU_MODE1       EQU     $12
VPU_MODE4       EQU     $03
VPU_MODE5       EQU     $04
VPU_MODE6       EQU     $05
VPU_MODE7       EQU     $07

; Character Codes

CRSR            EQU     1               ; Cursor
BS              EQU     8               ; Backspace / Left Arrow
TAB             EQU     9               ; Horizontal Tab
LF              EQU     10              ; Line Feed / Down Arrow
UP              EQU     11              ; Up Arrow
CR              EQU     13              ; Carriage Return
RIGHT           EQU     21              ; Right Arrow
SPC             EQU     32              ; Space
DEL             EQU     127             ; Delete

VDP1_ID         EQU     $15

; **** Variables ***************************************************************

ZP_BASE         EQU     $C8             ; Zero Page Base Address
KBD_LANG        EQU     ZP_BASE         ; the current selected keyboard language
FLOPPY_TYPE     EQU     ZP_BASE+1       ; floppy type settings
VPUMODE         EQU     ZP_BASE+2       ; current VPU mode
MAX_X           EQU     ZP_BASE+3       ; max characters per line
CURSOR_TICKS    EQU     ZP_BASE+4       ; blink frequency in ticks (1/60 sec or 1/50 sec)
CURSOR          EQU     ZP_BASE+5       ; character under the cursor

BUFFSIZE        EQU     ZP_BASE+6       ; actual scroll buffer size
PAGECNT         EQU     ZP_BASE+7       ; page buffer counter
TXTCOLOR        EQU     ZP_BASE+8
COLOR           EQU     ZP_BASE+9

POS_X           EQU     ZP_BASE+10       ; cursor X position (16 bit)
POS_Y           EQU     ZP_BASE+12       ; cursor Y position

ADR_CHAR        EQU     $08 		 ; ######################### Wat is dit nu weer?

ADR             EQU     ZP_BASE+13
ADRL            EQU	ZP_BASE+13   	; last address Low
ADRH      	EQU   	ZP_BASE+14  	; last address High

XREG            EQU     ZP_BASE+15      ; current saved X register
YREG            EQU     ZP_BASE+16      ; current saved Y register
VARSAV          EQU     ZP_BASE+17      ; saved variable

FGCBASE	        EQU	$16	      ; pointer to Floppy-/Graphics-Controller card base
FGCBASEL	EQU	$16	      ; always $00
FGCBASEH	EQU	$17	      ; (K2 = $08, K3 = $0C,) should always K4 = $10

DEVID		EQU	$03	      ; current device id

TICKCNT         EQU     $DA             ; current tick counter

PSTR      	EQU   	$EA      	; output string Pointer
PSTRL     	EQU   	$EA      	; lower address byte of output string pointer
PSTRH     	EQU   	$EB      	; upper address byte of output string pointer

TEMP      	EQU   	$FC           ; temp storage
YSAV      	EQU   	$FD

; **** System BIOS Routines And Static Addresses *******************************

RBUFF     	EQU   	$1500           ; scroll buffer
NKEY_HANDLER    EQU     $1A26           ; vector to no-key-pressed keyboard handler
_NO_HANDLER_    EQU     $E00E

BEEP            EQU     $E167

PROG_START	EQU	$0200	      ; Program Start Address in RAM
