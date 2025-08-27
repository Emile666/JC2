;-------------------------------------------------
; JC2 BASIC Zero-Page variables.
; Assembler: MADS-Assembler
;-------------------------------------------------

Decss			= $04		; BASIC number to decimal string start LSB (moved from $EF)
Decssp1			= $05		; BASIC number to decimal string start MSB (moved from $F0)
					; $06..$09 also reserved for Decss
					
Usrjmp			= $0A		; USR function JMP address
Usrjpl			= Usrjmp+1	; USR function JMP vector low byte
Usrjph			= Usrjmp+2	; USR function JMP vector high byte
Nullct			= $0D		; nulls output after each line
TPos			= $0E		; BASIC terminal position byte
TWidth			= $0F		; BASIC terminal width byte
Iclim			= $10		; input column limit
Itempl			= $11		; temporary integer low byte
Itemph			= Itempl+1	; temporary integer high byte

nums_1			= Itempl	; number to bin/hex string convert MSB
nums_2			= nums_1+1	; number to bin/hex string convert
nums_3			= nums_1+2	; number to bin/hex string convert LSB

; *** free space **********************************************************
;			= $14		; *used by JC2 system
;			= $15		; *used by JC2 system
;			= $16		; *used by JC2 system
;			= $17		; *used by JC2 system
;			= $18		; *used by JC2 system
;			= $19		; *used by JC2 system 
; *************************************************************************

Temp1			= $1A		; temporary byte for free use

Srchc			= $1B		; search character
Temp3			= Srchc		; temp byte used in number routines
Scnquo			= $1C		; scan-between-quotes flag
Asrch			= Scnquo	; alt search character

XOAw_l			= Srchc		; eXclusive OR, OR and AND word low byte
XOAw_h			= Scnquo	; eXclusive OR, OR and AND word high byte

Ibptr			= $1D		; input buffer pointer
Dimcnt			= Ibptr		; # of dimensions

Defdim			= $1E		; default DIM flag
Dtypef			= $1F		; data type flag, $FF=string, $00=numeric
Oquote			= $20		; open quote flag (b7) (Flag: DATA scan; LIST quote; memory)
Gclctd			= $20		; garbage collected flag
Sufnxf			= $21		; subscript/FNX flag, 1xxx xxx = FN(0xxx xxx)
Imode			= $22		; input mode flag, $00=INPUT, $80=READ

Cflag			= $23		; comparison evaluation flag

TabSiz			= $24		; TAB step size (was input flag)

next_s			= $25		; next descriptor stack address

					; these two bytes form a word pointer to the item
					; currently on top of the descriptor stack

last_sl			= $26		; last descriptor stack address low byte
last_sh			= $27		; last descriptor stack address high byte (always $00)

des_sk			= $28		; descriptor stack start address (temp strings)

;			= $30		; End of descriptor stack

ut1_pl			= $31		; utility pointer 1 low byte
ut1_ph			= ut1_pl+1	; utility pointer 1 high byte
ut2_pl			= $33		; utility pointer 2 low byte
ut2_ph			= ut2_pl+1	; utility pointer 2 high byte

FACt_1			= $35		; FAC temp mantissa1
FACt_2			= FACt_1+1	; FAC temp mantissa2
FACt_3			= FACt_2+1	; FAC temp mantissa3

dims_l			= FACt_2	; array dimension size low byte
dims_h			= FACt_3	; array dimension size high byte

TempB			= $38		; temp page 0 byte

Smeml			= $39		; start of mem low byte		(Start-of-Basic)
Smemh			= Smeml+1	; start of mem high byte	(Start-of-Basic)
Svarl			= $3B		; start of vars low byte	(Start-of-Variables)
Svarh			= Svarl+1	; start of vars high byte	(Start-of-Variables)
Sarryl			= $3D		; var mem end low byte		(Start-of-Arrays)
Sarryh			= Sarryl+1	; var mem end high byte		(Start-of-Arrays)
Earryl			= $3F		; array mem end low byte	(End-of-Arrays)
Earryh			= Earryl+1	; array mem end high byte	(End-of-Arrays)
Sstorl			= $41		; string storage low byte	(String storage (moving down))
Sstorh			= Sstorl+1	; string storage high byte	(String storage (moving down))
Sutill			= $43		; string utility ptr low byte
Sutilh			= Sutill+1	; string utility ptr high byte
Ememl			= $45		; end of mem low byte		(Limit-of-memory)
Ememh			= Ememl+1	; end of mem high byte		(Limit-of-memory)
Clinel			= $47		; current line low byte		(Basic line number)
Clineh			= Clinel+1	; current line high byte	(Basic line number)
Blinel			= $49		; break line low byte		(Previous Basic line number)
Blineh			= Blinel+1	; break line high byte		(Previous Basic line number)

Cpntrl			= $4B		; continue pointer low byte
Cpntrh			= Cpntrl+1	; continue pointer high byte

Dlinel			= $4D		; current DATA line low byte
Dlineh			= Dlinel+1	; current DATA line high byte

Dptrl			= $4F		; DATA pointer low byte
Dptrh			= Dptrl+1	; DATA pointer high byte

Rdptrl			= $51		; read pointer low byte
Rdptrh			= Rdptrl+1	; read pointer high byte

Varnm1			= $53		; current var name 1st byte
Varnm2			= Varnm1+1	; current var name 2nd byte

Cvaral			= $55		; current var address low byte
Cvarah			= Cvaral+1	; current var address high byte

Frnxtl			= $57		; var pointer for FOR/NEXT low byte
Frnxth			= Frnxtl+1	; var pointer for FOR/NEXT high byte

Tidx1			= Frnxtl	; temp line index

Lvarpl			= Frnxtl	; let var pointer low byte
Lvarph			= Frnxth	; let var pointer high byte

prstk			= $59		; precedence stacked flag

comp_f			= $5B		; compare function flag, bits 0,1 and 2 used
					; bit 2 set if >
					; bit 1 set if =
					; bit 0 set if <

func_l			= $5C		; function pointer low byte
func_h			= func_l+1	; function pointer high byte

garb_l			= func_l	; garbage collection working pointer low byte
garb_h			= func_h	; garbage collection working pointer high byte

des_2l			= $5E		; string descriptor_2 pointer low byte
des_2h			= des_2l+1	; string descriptor_2 pointer high byte

g_step			= $60		; garbage collect step size

Fnxjmp			= $61		; jump vector for functions
Fnxjpl			= Fnxjmp+1	; functions jump vector low byte
Fnxjph			= Fnxjmp+2	; functions jump vector high byte

g_indx			= Fnxjpl	; garbage collect temp index
	
FAC2_r			= $63		; FAC2 rounding byte

Adatal			= $64		; array data pointer low byte
Adatah			= Adatal+1	; array data pointer high  byte

Nbendl			= Adatal	; new block end pointer low byte
Nbendh			= Adatah	; new block end pointer high  byte

Obendl			= $66		; old block end pointer low byte
Obendh			= Obendl+1	; old block end pointer high  byte

numexp			= $68		; string to float number exponent count
expcnt			= $69		; string to float exponent count

numbit			= numexp	; bit count for array element calculations

numdpf			= $6A		; string to float decimal point flag
expneg			= $6B		; string to float eval exponent -ve flag

Astrtl			= numdpf	; array start pointer low byte
Astrth			= expneg	; array start pointer high  byte

Histrl			= numdpf	; highest string low byte
Histrh			= expneg	; highest string high  byte

Baslnl			= numdpf	; BASIC search line pointer low byte
Baslnh			= expneg	; BASIC search line pointer high  byte

Fvar_l			= numdpf	; find/found variable pointer low byte
Fvar_h			= expneg	; find/found variable pointer high  byte

Ostrtl			= numdpf	; old block start pointer low byte
Ostrth			= expneg	; old block start pointer high  byte

Vrschl			= numdpf	; variable search pointer low byte
Vrschh			= expneg	; variable search pointer high  byte

FAC1_e			= $6C		; FAC1 exponent
FAC1_1			= FAC1_e+1	; FAC1 mantissa1
FAC1_2			= FAC1_e+2	; FAC1 mantissa2
FAC1_3			= FAC1_e+3	; FAC1 mantissa3
FAC1_s			= FAC1_e+4	; FAC1 sign (b7)

str_ln			= FAC1_e	; string length
str_pl			= FAC1_1	; string pointer low byte
str_ph			= FAC1_2	; string pointer high byte

des_pl			= FAC1_2	; string descriptor pointer low byte
des_ph			= FAC1_3	; string descriptor pointer high byte

mids_l			= FAC1_3	; MID$ string temp length byte

negnum			= $71		; string to float eval -ve flag
numcon			= $71		; series evaluation constant count

FAC1_o			= $72		; FAC1 overflow byte

FAC2_e			= $73		; FAC2 exponent
FAC2_1			= FAC2_e+1	; FAC2 mantissa1
FAC2_2			= FAC2_e+2	; FAC2 mantissa2
FAC2_3			= FAC2_e+3	; FAC2 mantissa3
FAC2_s			= FAC2_e+4	; FAC2 sign (b7)

FAC_sc			= $78		; FAC sign comparison, Acc#1 vs #2
FAC1_r			= $79		; FAC1 rounding byte

ssptr_l			= FAC_sc	; string start pointer low byte
ssptr_h			= FAC1_r	; string start pointer high byte

sdescr			= FAC_sc	; string descriptor pointer

csidx			= $7A		; line crunch save index
Asptl			= csidx		; array size/pointer low byte
Aspth			= $7B		; array size/pointer high byte

Btmpl			= Asptl		; BASIC pointer temp low byte
Btmph			= Aspth		; BASIC pointer temp low byte

Cptrl			= Asptl		; BASIC pointer temp low byte
Cptrh			= Aspth		; BASIC pointer temp low byte

Sendl			= Asptl		; BASIC pointer temp low byte
Sendh			= Aspth		; BASIC pointer temp low byte

LAB_IGBY		= $7C		; get next BASIC byte subroutine

LAB_GBYT		= $82		; get current BASIC byte subroutine
Bpntrl			= $83		; BASIC execute (get byte) pointer low byte
Bpntrh			= Bpntrl+1	; BASIC execute (get byte) pointer high byte

;			= $97		; end of get BASIC char subroutine

; Rbyte4,1,2,3 are moved to $1860 to free up ZP-use
; I2Cstat is moved to $180F
