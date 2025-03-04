;---------------------------------------------------------------------------
; Memory-Test for Junior Computer ][
; v1: first version of mem-Test
; v2: table-driven version of mem-Test
; v3: PIA-detection + RAM-Banks test added
; v3.1: 628128 RAM IC test + Printing subroutines added
; v3.2: BASIC-ROM test added + IOL-card detection
; v3.3: MMU-banks detection improved
; v3.4: ATF1504 MMU detection added
;---------------------------------------------------------------------------
        OPT h- ; do not add file header
        OPT f+ ; save as single block

; Defines and Constants ********************************************************
CR        	EQU     $0D    		; carriage return
LF	  	EQU     $0A    		; line feed

BIOS_EN		EQU	$01		; PORTB bit 0: 1 = enable BIOS at $E000
MON_EN		EQU	$02		; PORTB bit 1: 1 = enable Monitor at $1C00

; Defines for RAM-table
TNO		EQU	$00		; No test
TRAM		EQU     $01		; RAM test only
TROM		EQU	$02		; ROM test and RAM test behind ROM
TPIA		EQU	$03		; 6520 PIA test
TRIOT		EQU	$80		; 6532 RIOT 128 B RAM test 

NO_RAMBANK	EQU	$03		; No RAM-banks selected, both ROMs enabled

ROM_CS_MON	EQU     $0E65		; Checksum JC Monitor program (1K)
ROM_CS_BIOS	EQU	$65C3		; Checksum BIOS v1.1.4 (8K)
ROM_CS_BASIC	EQU	$804C		; Checksum BASIC ROM v2.25 (12K)

IOBASE		EQU	$14		; LSB of IO/ROM card if found (0 if not present)
IOBASEH		EQU	$15		; MSB of IO/ROM card if found (0 if not present)

PIA_PRESENT	EQU	$C7		; 1-byte, 0=no PIA, $80=PIA present
BCNTR		EQU     $C8		; 1-byte RAM-bank counter
MMU_BANKS	EQU     $C9		; 1-byte, #RAM-banks found, 0 = no RAM-banks found
KB		EQU	$CB		; 1-byte Size in KB
ROM_PTR		EQU	$CC		; 2-byte ROM pointer
ROM_CS 		EQU 	$CE		; 2-byte ROM checksum
SAVEX		EQU	$D0		; X-register back-up
SAVEY		EQU	$D1		; Y-register back-up
PPSTR		EQU	$D2		; 2-byte Post Print pointer
PRBMSB          EQU     $D4		; MSB of begin-address to print
PREMSB          EQU     $D5		; MSB of end-address to print
CHK_MODE        EQU     $D6             ; 0=no RAM-check, 1=RAM-check 2=ROM checksum,
                                        ; 80=start halfway a memory page
TBL_IDX		EQU     $D7		; 1-byte index in memory table
PGCNTR		EQU     $D8		; 2-byte Memory end address
RAMSAV1         EQU     $DA             ; 1-byte memory save
RAMSAV2         EQU     $DB             ; 1-byte memory save

PSTR      	EQU   	$EA      	; 2-byte BIOS output string Pointer

DIG0      	EQU   	$F8     	; 10^0 digit
DIG1	  	EQU   	$F9	 	; 10^1 digit
DIG2      	EQU   	$FA     	; 10^2 digit

COMM_REG  	EQU   	$1602    	; ACIA Command Register

PORTA		EQU	$1700		; CRA-2=1: PIA PORTA register
DDRA            EQU     $1700           ; CRA-2=0: PIA data-direction A register 
CRA             EQU     $1701           ; PIA control A register
PORTB		EQU	$1702		; CRB-2=1: PIA PORTB register
DDRB            EQU     $1702           ; CRB-2=0: PIA data-direction B register 
CRB             EQU     $1703           ; PIA control B register

; BIOS v1.1.4 Routines *********************************************************

STROUT		EQU     $E083           ; string write routine v1.1.4 bios
HEXOUT          EQU     $E091           ; print 2-byte hex number
NUMOUT          EQU     $E0A7           ; print 3-byte decimal number
DEC2STR		EQU	$E0BD		; convert decimal number to string
BOUT		EQU     $E044           ; print 1 byte
COUT		EQU     $E052           ; print 1 character
MON_WARM_START	EQU  	$E003       	; BIOS monitor warm start
DETECT_IOL_CARD EQU	$ECD1		; Detect IO / Basic card

		.word RUN_ADR		; Needed for lm command (XMODEM load program)

		ORG $2000
	
;----------------------------------------------------------------------------
; This is the main program for the JC2 Memory Tester.
;----------------------------------------------------------------------------
RUN_ADR 	JSR PR_TITLE        	; Print Program Title text
		LDA #$00            	; Init. table index
                STA TBL_IDX
		STA PIA_PRESENT		; 0 = No PIA
		STA MMU_BANKS		; Init. nr. of RAM-banks found
		
		JSR DETECT_IOL_CARD	; Check if IO-Basic ROM card is present
		JSR PR_IOL_CARD		; Print status of IOL-card
		JSR PIA_DETECT	    	; check if PIA or ATF1504 MMU is present
		BCS CHECK_BASIC	    	; branch if no PIA or MMU is present
		
		; A 6520 PIA or MMU is present, check number of RAM-banks
		JSR RAM_BANKS_DET   	; Get #RAM-banks present

CHECK_BASIC	LDA IOBASEH		; 0=not present
		BEQ MAIN_LOOP		; branch if no IO-card detected
		
		JSR COPY_B0_C0		; update RAM-table with BASIC ROM info
		
;----------------------------------------------------------------------------
; This is the main loop of the memory-test program, 
; it tests all memory-areas defined in RAM_TABLE.
;----------------------------------------------------------------------------
MAIN_LOOP	JSR READ_TABLE	    	; Read values from memory-table
		BCS MAIN_EXIT	    	; branch if all entries are Read
		
		JSR PR_INFO	    	; Print line of text to screen
		LDA CHK_MODE	    	; check mode
		CMP #TRAM              	; Test RAM
		BNE MLP1	    	; branch if no RAM-Test

		; Option 1: Check RAM
		JSR CHK_RAM_MAIN	; Now check memory for this entry
		JMP MCONT	    	; branch always
	
MLP1		CMP #TROM	    	; Test ROM
		BNE MLP4	    	; branch if no ROM-Test

		; Option 2: Check ROM and RAM behind it
		JSR CHK_ROM_MAIN	; Test ROM
		LDY #0			; init page index
		LDA PGCNTR+1		; get original page-counter
		CMP #$1C		; $1C00-$1FFF, Monitor ROM
		BNE MLP2
		
		JSR MON_RAM_TEST	; test RAM behind monitor ROM
		JMP MCONT		; branch always
		
MLP2		CMP #$E0		; $E000-$FFFF, BIOS ROM
		BNE MLP3		
		
		JSR BIOS_RAM_TEST	; test RAM behind BIOS ROM
		JMP MCONT
		
MLP3		CMP #$B0		; test RAM behind BASIC ROM
		BNE MLP4
		
		JSR BASIC_RAM_TEST	; Test RAM
		JMP MCONT		; branch always

MLP4		CMP #TPIA		; 6520 PIA?
		BNE MCONT		; branch if not PIA entry

		JSR PR_PIA_RESULT	; Print PIA result
		;JMP MCONT
		
MCONT		INC TBL_IDX	    	; next entry
		BNE MAIN_LOOP	    	; branch always

MAIN_EXIT	LDA MMU_BANKS		; Nr. of RAM-banks found
		BEQ EXIT_MON	    	; branch if no RAM-banks to Test
		
		JSR RAM_BANKS_TEST  	; test all memory in RAM-banks
EXIT_MON	JMP MON_WARM_START  	; exit program, jump to JC monitor again

;----------------------------------------------------------------------------
; This routine is the entry-point for the check RAM function.
;----------------------------------------------------------------------------
CHK_RAM_MAIN	JSR CHECK_RAM	    	; Now check memory for this entry
		BCS MAERR	    	; branch if error
		
		JMP PR_RAM_OK	    	; Print RAM OK message and return
	
MAERR	    	LDA #<TXT_RAM_NOK   	; Print RAM error
		LDY #>TXT_RAM_NOK
		JMP SPRINT          	; print and return
		
;----------------------------------------------------------------------------
; This routine is the entry-point for the check ROM function.
;----------------------------------------------------------------------------
CHK_ROM_MAIN	LDY #$30		; IOBASE+$30 selects BASIC ROM
		LDA (IOBASE),Y		; Switch to BASIC ROM area
		
		JSR CHECK_ROM	    	; Now check memory for this entry
		BCS MOERR	    	; branch if error
		
		LDA #<TXT_ROM_OK    	; Print ROM OK message
		LDY #>TXT_ROM_OK
		JMP SPRINT          	; print and return
	
MOERR	    	JMP PR_ROM_ERR         	; print and return
		
;----------------------------------------------------------------------------
; This routine checks a RAM area for errors.
; PGCNTR should be set to the RAM begin-address to check.
; X-register: Number of pages - 1 (1 page = 256 B) to check: 3 = 1K block.
; Y-register: Number of bytes to check (0 = 256 bytes) on the first page.
; Returns   : C=0: RAM-check ok
;             C=1: RAM error
;----------------------------------------------------------------------------
CHECK_RAM	LDX SAVEX
		DEX		    	; X = 3 => 4 pages to check
		LDY SAVEY
CHECK_RAM_LP	LDA (PGCNTR),Y
		STA RAMSAV1         	; save value
		LDA #$55            	; 0101 0101
		STA (PGCNTR),Y
		INY
		LDA (PGCNTR),Y
		STA RAMSAV2	    	; save value
		LDA #$AA	    	; 1010 1010
		STA (PGCNTR),Y
		DEY		    	; Go back to previous RAM byte
		LDA (PGCNTR),Y
		CMP #$55
		BNE RAM_ERR	    	; branch on RAM error
		
		LDA RAMSAV1
		STA (PGCNTR),Y      	; restore original value
		INY
		LDA (PGCNTR),Y
		CMP #$AA
		BNE RAM_ERR	    	; branch on RAM error
		
		LDA RAMSAV2
		STA (PGCNTR),Y      	; restore original value
		
		INY		    	; next 2 bytes
		BNE CHECK_RAM_LP	; continue checking this page
		
		; 1 page done, no errors
		STY PGCNTR          	; in case PGCNTR started halfway a page
		INC PGCNTR+1	    	; Next page
		DEX		    	; number of pages to check - 1
		BPL CHECK_RAM_LP      	; new page, continue checking
		
		; Entire RAM-area checked, no errors
		CLC		    	; C=0, no error
		RTS
		
RAM_ERR		TYA	
		STA SAVEY		; save Y-value
		AND #$01	    	; Odd or Even?
		TAY		    	; Y=0 (RAMSAV1) or 1 (RAMSAV2)
		LDA RAMSAV1,Y       	; restore value of memory byte
		PHA			; save restored value
		LDA SAVEY		; 
		TAY			; get original Y value back
		PLA			; get restored value back again
                STA (PGCNTR),Y      	; not really need in case of error, just to be sure
		SEC		    	; C=1, error!
		RTS

;----------------------------------------------------------------------------
; This routine calculates the checksum of the ROM area starting in PRBMSB and
; ends in PREMSB. The result is stored in ROM_CS.
;----------------------------------------------------------------------------
CHECK_ROM    	LDX SAVEX
		DEX		    	; X = 3 => 4 pages to check
		LDY #0   		
		STY ROM_CS
		STY ROM_CS+1
		STY ROM_PTR
		LDA PRBMSB	    	; MSB of ROM begin-addres to check
		STA ROM_PTR+1
ROM_LP1		LDA (ROM_PTR),Y	    	; get byte from ROM
		CLC
		ADC ROM_CS	    	; add to LSB checksum
		STA ROM_CS
		BCC ROM_LP2
		
		INC ROM_CS+1        	; increment MSB
ROM_LP2		INY
		BNE ROM_LP1

		INC ROM_PTR+1	    	; next ROM page
		DEX
		BPL ROM_LP1         	; branch if not done yet

		LDA ROM_PTR+1
		BEQ ROM_BIOS_CHK	; BIOS end-address = $FFFF + 1
		
		CMP #$20	    	; end address JC Monitor program
		BNE ROM_BASIC_CHK	; branch if not Monitor ROM
		
		; Compare checksum with JC Monitor ROM checksum
ROM_MON_CHK	LDA ROM_CS
		CMP #<ROM_CS_MON
		BNE ROM_ERR	    	; branch on error
		
		LDA ROM_CS+1
		CMP #>ROM_CS_MON
		BNE ROM_ERR	    	; branch on error
		BEQ ROM_NOERR	    	; branch always

		; Compare checksum with BASIC ROM checksum
ROM_BASIC_CHK	LDA ROM_CS
		CMP #<ROM_CS_BASIC
		BNE ROM_ERR		; branch on error
		
		LDA ROM_CS+1
		CMP #>ROM_CS_BASIC
		BNE ROM_ERR		; branch on error
		BEQ ROM_NOERR		; branch always
		
		; Compare checksum with JC2 BIOS ROM checksum
ROM_BIOS_CHK	LDA ROM_CS
		CMP #<ROM_CS_BIOS
		BNE ROM_ERR	    	; branch on error
		
		LDA ROM_CS+1
		CMP #>ROM_CS_BIOS
		BNE ROM_ERR	    	; branch on error

ROM_NOERR	CLC		    	; ROM checksum OK
		RTS		    	; return

ROM_ERR		SEC		    	; ROM checksum error
		RTS		    	; return

;----------------------------------------------------------------------------
; This routine reads an entry from the memory-table.
; TBL_IDX is the index number and should be set to the RAM begin-address 
; to check. This routine updates the following:
; PGCNTR    : MSB is set to the begin-area in RAM to check
; CHK_MODE  : The mode selected (NO, RAM or ROM check)
; X-register: is set to the number of pages (256 B) to check
; Y-register: is set to the number of bytes to check on the 1st page (0 = all)
; Returns   : C=0: ok, C=1: TBL_IDX = TBL_MAX
;----------------------------------------------------------------------------
READ_TABLE	LDA #0
		STA SAVEY           	; 0 = default value (full page)
		STA PGCNTR		; Init. page-counter to full page
		LDX TBL_IDX         	; get current index
		CPX #TBL_MAX	    	; max. entries in memory table
		BCC TBL1	    	; branch if TBL_IDX < TBL1
			
		RTS		    	; error (C=1), return
		
TBL1		CPX #0
		BEQ TBL2	    	; X=0? then exit multiply loop
		
TBL1A		CLC
		ADC #TBL1_SZ	    	;
		DEX
		BNE TBL1A           	; branch always

 		TAX		    	; X = TBL1_SZ * TBL_IDX

TBL2		LDA RAM_TABLE,X	    	; RAM_TABLE[TBL_IDX].mode
		CMP #TRIOT	    	; half a page to check?
		BNE TBL_FP         	; branch if not $80
		
		STA SAVEY	    	; Y = $80 (half page)
		LDA #TRAM	    	; 1 = normal RAM-check
		
TBL_FP		STA CHK_MODE	    
		INX		    	; next entry in table (print begin-address)
		LDA RAM_TABLE,X	    	; print begin-address MSB
		STA PRBMSB
		INX		    	; next entry in table (RAM check begin-address)
		LDA RAM_TABLE,X	    	; RAM-check begin-address MSB
		STA PGCNTR+1        	; begin-address MSB for RAM-check
		INX		    	; next entry in table (pointer to print text)
		LDA RAM_TABLE,X	    	; pointer to print-text LSB
		STA PPSTR	    	; init. screen pointer LSB
		INX 
		LDA RAM_TABLE,X	    	; pointer to print-text MSB
		STA PPSTR+1         	; pointer to print-text MSB
		
		LDA TBL_IDX
		CMP #TBL_MAX-1
		BNE TBL_NEND	    	; branch if not the last entry in the table
		
		;-------------------------------------------------------------
		; This is the last entry in the memory table.
		; Set Print end-address to $FF and number of
		; pages to check to one-complement of PGCNTR+1
		;-------------------------------------------------------------
		LDA #$FF
		STA PREMSB	    	; Print end-address
		LDA PGCNTR+1	    	; MSB begin-address for RAM-check
		EOR #$FF	    	; one-complement
		CLC
		ADC #1		    	; two-complement
		TAX	 	    	; X = #pages till $FFFF
		BNE TBL_EXIT	    	; branch always

		;-------------------------------------------------------------
		; This is NOT the last entry in the memory table.
		; Set Print end-address to begin-address of next entry - 1 
		; and number of pages to begin-address next entry - begin-address
		; for RAM-check in the current entry.
		;-------------------------------------------------------------
TBL_NEND	INX		    	; now at mode next entry in memory-table
		INX		    	; now at MSB of print begin-address next entry
		LDA RAM_TABLE,X	    	; also the MSB end-address of the current entry
		STA PREMSB	    	; save print end-address
		PHA                 	; save A
		SEC
		SBC PRBMSB	    	; size in KB
		STA KB              	; save size
		DEC PREMSB	    	; end-address = next begin-address - 1
		PLA                 	; get value back
		SEC
		SBC PGCNTR+1
		TAX 		    	; X = #pages to check
		
		; Now check for 6532 128 bytes only RAM-check
		LDY SAVEY	    	; this is 0 (full-page) or $80 (6532 half-page)
		CPY #TRIOT
		BNE TBL_EXIT	    	; branch if normal area
		
		LDX #1		    	; 1 (half) page to check
		DEC PGCNTR+1	    	; PGCNTR -= 128
		STY PGCNTR	    	; 
		STY KB              	; 128 B size
		
TBL_EXIT	STX SAVEX	    	; save X-register
		CLC		    	; C=0, no errors
		RTS		    	; and return
		
;----------------------------------------------------------------------------
; This routine checks if the 6520 PIA from the MMU is present.
;----------------------------------------------------------------------------
PIA_DETECT	LDA COMM_REG       	; ACIA command register at $1602
		PHA		   	; save value
		EOR #$FF 	   	; invert all bits
		STA DDRB	   	; PIA DDRB register at $1702 or MMU-register
		CMP COMM_REG	   	; If ACIA and (PIA or MMU) are present, value is different 
		BNE PIA_DET2	   	; branch if PIA or MMU are present
		
		; no PIA or MMU present
		PLA		   	; restore value
		STA COMM_REG	   	;
NO_PIA_DET	SEC		   	; C=1, error, no PIA
		RTS
		
PIA_DET2	PLA		   	; restore previous value
		STA COMM_REG	   	;
		
		; Area at $1700-$17FF either contains nothing, a PIA or the MMU-register
		JSR PIA_INIT		; Init. the 6520 PIA output ports
		LDA PORTB		; PIA PORTB or MMU-register
		CMP #NO_RAMBANK		; PORTB init. value
		BNE NO_PIA_DET		; if not, no PIA or MMU-register present

		; CRB bit 2 = 1 => PIA present
		; CRB bit 2 = 0 => MMU-register present
		LDA CRB			; PIA CRB register (or MMU-register)
		ORA #4			; test bit 2
		BEQ MMU_REG		; branch if no PIA present
		
		LDA #$80		; $80 = PIA present
		BNE PIA_DET_END		; branch always

MMU_REG		LDA #<TXT_MMU		; MMU text instead of PIA text
		STA TBL_PIA_TXT		; LSB of text-pointer
		LDA #>TXT_MMU		;
		STA TBL_PIA_TXT+1	; MSB of text-pointer
		LDA #$C0		; $C0 = MMU register (ATF1504) present
		
PIA_DET_END	STA PIA_PRESENT		; $80 = PIA present, $C0 = ATF1504 MMU present
		CLC		   	; C=0, PIA or MMU is present
		RTS

;----------------------------------------------------------------------------
; This routine initializes the 6520 PIA.
;----------------------------------------------------------------------------
PIA_INIT	LDA CRB            	; PIA control register B
		AND #$FB	   	; CRB-2=0: select DDRB (MMU: MA14=0)
		STA CRB
		LDA #$7F           	; PB6..PB0 are all output (MMU: MA17..14=1)
		STA DDRB
		LDA CRB
		ORA #$04	   	; CRB-2=1: select PORTB (MMU: MA14=1)
		STA CRB
		LDA #NO_RAMBANK	   	; BIOS and Monitor ROM are enabled
		STA PORTB	   	; Start with RAM-bank R1 in main-64K
		RTS			; and return
		
;----------------------------------------------------------------------------
; This routine checks how many RAM-banks are present. The result is
; stored in MMU_BANKS.
;----------------------------------------------------------------------------
RAM_BANKS_DET	JSR DET_628128		; start with detection of 628128 SRAM
		BCC NORMAL_RAM		; C=0, normal RAM
		
		LDA #<TXT_628128    	; Print 628128 RAM IC found
		LDY #>TXT_628128
		JSR SPRINT	   	; print

		LDA #4			; 4 RAM-banks present
		BNE RAMB_DONE		; branch always
		
NORMAL_RAM	LDA #0			; 
		STA $4000          	; Init. first byte in RAM-bank
		TAX 	   		; First RAM-bank
RAMB_LP1	LDA RAMB_TABLE,X   	; get value for PORTB
		STA PORTB	   	; select RAM-bank
		STX $4000	   	; First byte in RAM-bank
		INX
		CPX #28		   	; table done?
		BNE RAMB_LP1	   	; branch if not done yet
		
		LDA #NO_RAMBANK	   	; Main-bank, no RAM-bank
		STA PORTB
		
		; Load the value in the main RAM-bank again.
		; 25=none, 21=4 banks (64K), 13=12 banks, 0 = 28 banks
		LDA $4000 	   
		BNE RAMB_LP2
		
		LDA #28		   	; 28 RAM-banks found
		BNE RAMB_DONE		; branch always
		
RAMB_LP2	LDA #25
		SEC
		SBC $4000
RAMB_DONE	STA MMU_BANKS	    	; A = 25-X	
		RTS

;----------------------------------------------------------------------------
; This routine detects the presence of a 628128 SRAM (128Kx8). This is the
; default RAM for the JC2. If used in the 512K PCB, !MA17 is connected to
; CS2. So if MA17=1, the entire RAM is deselected. Any other RAM just 
; selects a RAM-bank.
;----------------------------------------------------------------------------
DET_628128	LDA $3FFF		; This is always RAM in every JC2 configuration
		PHA			; save for now
		LDA #$55
		STA $3FFF		; No RAM-bank, just normal RAM
		LDA #$23		; MA17=1, RAM-bank enabled or RAM-deselected
		STA PORTB		; This is either PORTB or the MMU-register
		LDX $3FFF		; Now find out
		
		LDA #NO_RAMBANK		; select normal RAM again
		STA PORTB
		PLA			; get saved value
		STA $3FFF		; and restore it
		
		CPX #$55		; Same number again?
		BNE NO_RAM		; branch if 628128 detected
		
		CLC			; C=0, normal RAM detected
		RTS			; and return
		
NO_RAM		SEC			; C=1, 628128 RAM detected
		RTS			; and return
		
;----------------------------------------------------------------------------
; This routine tests the memory of every RAM-bank present.
;----------------------------------------------------------------------------
RAM_BANKS_TEST	LDA #CR			; empty line
		JSR COUT
		LDA #0
		STA BCNTR          	; init ram-bank counter

RB_TST1		TAX		   	; ram-bank counter
		LDA RAMB_TABLE,X   	; get value for PORTB
		STA PORTB	   	; enable RAM-bank
		
		LDA #<TXT_RAMB1    	; Print RAM-bank number
		LDY #>TXT_RAMB1
		JSR SPRINT	   	; print
		LDA BCNTR	   	;  
		JSR NUMOUT2	   	; print current ram-bank number
		LDA #':'
		JSR COUT
		LDA #' '
		JSR COUT
		
		LDY #0	           	; 0 = full-page
		STY PGCNTR		; Zero LSB of PGCNTR
		LDX #$40	   	; 
		STX PGCNTR+1	   	; RAM-bank starts at $4000
		DEX                     ; X=$3F => $40 pages = 16K
		JSR CHECK_RAM_LP   	; test memory in RAM-bank

		BCS RBTST_ERR	   	; branch if error
		
		LDA #<TXT_RAMB_OK  	; Print RAM-bank OK
		LDY #>TXT_RAMB_OK
		JSR SPRINT	   	; Print RAM OK message
		JMP RBTST_CNT
	
RBTST_ERR    	LDA #<TXT_RAMB_NOK 	; Print RAM-bank error
		LDY #>TXT_RAMB_NOK
		JSR SPRINT	   	; Print error text
		
RBTST_CNT	INC BCNTR	   	; increment RAM-bank counter
		LDA BCNTR
		CMP MMU_BANKS      	; all banks done?
		BNE RB_TST1        	; branch if not all banks done
		
		LDA #NO_RAMBANK	   	; BIOS and Monitor ROM are enabled
		STA PORTB	   	; Disable RAM-banks
		RTS		   	; return

;----------------------------------------------------------------------------
; This routine tests the memory behind the Monitor ROM.
;----------------------------------------------------------------------------
MON_RAM_TEST	LDA #<TXT_MONRAM1  	;
		LDY #>TXT_MONRAM1
		JSR SPRINT	   	; print
		
		SEI		   	; disable interrupts
		LDA PORTB
		AND #~MON_EN	   
		STA PORTB	   	; disable monitor ROM, enable RAM behind it
		
		LDX #$03   	   	; 03 = 4 pages = 1K
		LDY #0	           	; 0 = full-page
		JSR CHECK_RAM_LP   	; test memory in RAM-bank
		PHP		   	; save processor status
		
		LDA PORTB
		ORA #MON_EN	   	; enable monitor ROM again
		STA PORTB
		CLI		   	; enable interrupts again
		
		PLP		   	; get processor status again
		BCS MONRAM_ERR	   	; branch if error
		
		LDA #<TXT_MONRAM_OK 	; RAM-bank OK
		LDY #>TXT_MONRAM_OK
		JMP SPRINT          	; Print Monitor RAM OK message and return
	
MONRAM_ERR    	LDA #<TXT_MONRAM_NOK 	; Print RAM-bank error
		LDY #>TXT_MONRAM_NOK
		JMP SPRINT           	; Print error text and return

;----------------------------------------------------------------------------
; This routine tests the memory behind the BIOS ROM.
;----------------------------------------------------------------------------
BIOS_RAM_TEST	LDA #<TXT_BIOSRAM1 	; First part of text
		LDY #>TXT_BIOSRAM1
		JSR SPRINT	   	; print
		
		SEI		   	; disable interrupts
		LDA PORTB
		AND #~BIOS_EN	   
		STA PORTB	   	; disable BIOS ROM, enable RAM behind it
		
		LDX #$1F   	   	; $1F = $20 pages = 8K
		LDY #0	           	; 0 = full-page
		JSR CHECK_RAM_LP   	; test memory in RAM-bank
		PHP		   	; save processor status
		
		LDA PORTB
		ORA #BIOS_EN	   	; enable BIOS ROM again
		STA PORTB
		CLI		   	; enable interrupts again
		
		PLP		   	; get processor status again
		BCS BIOSRAM_ERR	   	; branch if error
		
		LDA #<TXT_BIOSRAM_OK  	; Print RAM OK
		LDY #>TXT_BIOSRAM_OK
		JMP SPRINT            	; Print BIOS RAM OK message and return
	
BIOSRAM_ERR    	LDA #<TXT_BIOSRAM_NOK	; Print RAM error
		LDY #>TXT_BIOSRAM_NOK
		JMP SPRINT            	; Print error text and return

;----------------------------------------------------------------------------
; This routine tests the memory behind the BASIC ROM.
;----------------------------------------------------------------------------
BASIC_RAM_TEST	LDA #<TXT_BASICRAM1 	; First part of text
		LDY #>TXT_BASICRAM1
		JSR SPRINT	   	; print
		
		LDY #$20		; IOBASE + $20 selects RAM area
		LDA (IOBASE),Y		; switch to RAM	   
		
		LDX #$2F   	   	; $2F = $30 pages = 12K
		LDY #0	           	; 0 = full-page
		JSR CHECK_RAM_LP   	; test memory in RAM-bank
		PHP		   	; save processor status
		
		LDY #$30		; IOBASE + $30 selects ROM area
		LDA (IOBASE),Y		; switch to ROM again	   
		
		PLP		   	; get processor status again
		BCS BASICRAM_ERR   	; branch if error
		
		LDA #<TXT_BASICRAM_OK  	; Print RAM OK
		LDY #>TXT_BASICRAM_OK
		JMP SPRINT            	; Print BIOS RAM OK message and return
	
BASICRAM_ERR   	LDA #<TXT_BASICRAM_NOK	; Print RAM error
		LDY #>TXT_BASICRAM_NOK
		JMP SPRINT            	; Print error text and return

;----------------------------------------------------------------------------
; This routine prints the title text and detects the CPU-type.
;----------------------------------------------------------------------------
PR_TITLE	LDA #<TXT_TITLE     	; Print block nr
		LDY #>TXT_TITLE
		JSR SPRINT	    	; print
		
		LDA #$99	    	; test for NMOS or CMOS
		CLC
		SED
		ADC #$01
		CLD
		BEQ CPU_CMOS	    	; 0 = CMOS

		LDA #<TXT_6502      	; Print 6502 NMOS found
		LDY #>TXT_6502
		JMP SPRINT	    	; print and return

		OPT C+		    	; enable 65816 instructions
CPU_CMOS	LDA #0
		rep #%00000010	    	; set Z-bit again
		BNE CPU_816
		
		LDA #<TXT_65C02     	; Print block nr
		LDY #>TXT_65C02
		JMP SPRINT	    	; print and return

CPU_816		LDA #<TXT_65816     	; Print block nr
		LDY #>TXT_65816
		JMP SPRINT	    	; print and return
		OPT C-		    	; disable 65816 instructions
		
;----------------------------------------------------------------------------
; This routine prints a line of text for a block. The text to
; print is already stored in PSTR by the READ_TABLE routine.
;----------------------------------------------------------------------------
PR_INFO		LDA #<TXT1          	; Print block nr
		LDY #>TXT1
		JSR SPRINT
		LDA TBL_IDX	    	; use table index as block number
		JSR HEXOUT          	; print block nr = page nr/4
		LDA #<TXT1A          
		LDY #>TXT1A
		JSR SPRINT 		
		
		LDA PRBMSB
		JSR HEXOUT          	; print block begin-address
		LDA #<TXT1B          
		LDY #>TXT1B
		JSR SPRINT 
		
		LDA PREMSB
		JSR HEXOUT          	; print block end-address
		LDA #<TXT1C          
		LDY #>TXT1C
		JSR SPRINT
		
		LDA TBL_IDX	    	; Check for K2, K3 or K4 IO-Area
		BEQ PP1		    	; branch if TBL_IDX = 0
		
		CMP #4
		BCS PP1	    	    	; branch if TBL_IDX > 3
		
		; K2, K3 or K4 area
		LDA #'K'
		JSR COUT
		LDA TBL_IDX
		CLC
		ADC #'1'	    	; TBL_IDX=1,2,3 => K2,K3,K4
		JSR COUT	    	; Print K number
		
PP1		LDA PPSTR	    	; Pointer to 2nd part text to print
		LDY PPSTR+1
		JMP SPRINT	    	; Print last part and return

; **** Print A Byte As Decimal Number ******************************************
; Input: A - number 00..63 (0..99)
; ******************************************************************************
NUMOUT2		JSR	DEC2STR
	        LDA 	DIG1
		CMP 	#'0'
		BEQ	NU20		; no -leading-zero

		JSR	BOUT
NU20:		LDA	DIG0
		JMP	BOUT		; print and return

;----------------------------------------------------------------------------
; This routine prints a RAM OK message.
;----------------------------------------------------------------------------
PR_RAM_OK       LDA KB		    	; size in KB or B
		CMP #100
		BCC PRKB            	; branch if size in KB
		
		JSR NUMOUT	    	; print size in bytes
PR_RAM1		LDA #<TXT_RAM_OK    	; Print ' bytes RAM OK' message
		LDY #>TXT_RAM_OK
		JMP SPRINT          	; print and return
		
PRKB		CMP #4
		BCC PR512
		
		LSR
		LSR
		JSR NUMOUT2	    	; print size in KB
		LDA #<TXT_RAMKB_OK    	; Print RAM OK message
		LDY #>TXT_RAMKB_OK
		JMP SPRINT          	; print and return

PR512		LDA #'5'	    	; 2 pages = 512 bytes
		JSR COUT
		LDA #'1'
		JSR COUT
		LDA #'2'
		JSR COUT
		JMP PR_RAM1         	; print '512 bytes RAM OK'
		
;----------------------------------------------------------------------------
; This routine prints a ROM ERROR message.
;----------------------------------------------------------------------------
PR_ROM_ERR      LDA #<TXT_ROM_NOK   	; Print ROM error
		LDY #>TXT_ROM_NOK
		JSR SPRINT
		LDA ROM_CS+1
		JSR HEXOUT
		LDA ROM_CS
		JSR HEXOUT	    
		LDA #CR
		JMP COUT	    	; print and return
		
;----------------------------------------------------------------------------
; This routine prints a string to the terminal
;----------------------------------------------------------------------------
SPRINT		STA PSTR	    	; LSB of text-pointer
		STY PSTR+1	    	; MSB of text-pointer
		JMP STROUT	    	; BIOS print string routine
		
;----------------------------------------------------------------------------
; This routine copies the entry for the BASIC ROM aread into the RAM-table
;----------------------------------------------------------------------------
COPY_B0_C0	LDX #4
COPY_LP1	LDA RAM_TABLE_B0,X	; Get info for $B000-$DFFF area
		STA RAM_TABLE_C0,X	; and overwrite $C000-$DFFF area
		DEX
		BPL COPY_LP1		; branch if not done yet
		RTS			; return
		
;----------------------------------------------------------------------------
; This routine prints the result of the BASIC-IO card detection, 
; including the number of RAM-banks found.
;----------------------------------------------------------------------------
PR_IOL_CARD	LDA #<TXT_IOL1      	; Print 1st part of message
		LDY #>TXT_IOL1
		JSR SPRINT          	; print and return
		LDA IOBASEH
		BEQ IOL_NOK		; 0 = no card detected
		
		LDA #<TXT_IOL_OK      	; Print 2nd part of message
		LDY #>TXT_IOL_OK
		JSR SPRINT
		LDA IOBASEH
		JSR HEXOUT		; print base-address
		LDA #$00
		JSR HEXOUT
		LDA #CR			; print 2xCR and return
		JSR COUT
		LDA #CR
		JMP COUT

IOL_NOK		LDA #<TXT_IOL_NOK      	; Print 2nd part of message
		LDY #>TXT_IOL_NOK
		JMP SPRINT		; print and return
		
;----------------------------------------------------------------------------
; This routine prints the results of the PIA detection.
;----------------------------------------------------------------------------
PR_PIA_RESULT	LDA PIA_PRESENT
		BNE PR_PIA_PRES		; branch if PIA present

		LDA #<TXT_PIA_NOT
		LDY #>TXT_PIA_NOT
		JSR SPRINT		; print 'not'
		
PR_PIA_PRES	LDA #<TXT_PIA_DET
		LDY #>TXT_PIA_DET
		JSR SPRINT		; print 'detected'
		
		LDA MMU_BANKS	    	; #RAM-banks found
		CMP #10
		BCC PR_PIA_LT10		; branch if 0-9 RAM-banks found
		
		JSR NUMOUT2	    	; >= 10, print 2-digit number
		JMP PR_PIA_RB1
		
PR_PIA_LT10	CLC		    	; < 10, print 1-digit number
		ADC #'0'
		JSR COUT
		
PR_PIA_RB1	LDA #<TXT_PIA2      	; Print 2nd part of message
		LDY #>TXT_PIA2
		JMP SPRINT          	; print and return

;--------------------------------------------------------------------------
; This table defines all PORTB values for the RAM-banks in the JC2 system.
; RAM-banks are numbered from R4..R28.
;--------------------------------------------------------------------------
RAMB_TABLE	.byte $13,$17,$1B,$1F,$23,$27,$2B,$2F
                .byte $33,$37,$3B,$3F,$43,$47,$4B,$4F
                .byte $53,$57,$5B,$5F,$63,$67,$6B,$6F
                .byte $73,$77,$7B,$7F
		
;--------------------------------------------------------------------------
; This table defines all memory areas of the JC2 computer system
; Entries are: Mode, Block begin MSB, Check begin MSB, text pointer
;--------------------------------------------------------------------------
RAM_TABLE	.byte $01, $00, $02     ; $0000 - $07FF, check from $0200
		.word TXT0
TBL1_SZ = (*-RAM_TABLE)		
		.byte TNO, $08, $08     ; $0800 - $0BFF, K2 IO-area, no RAM-check  
		.word TXT_KX	
		.byte TNO, $0C, $0C     ; $0C00 - $0FFF, K3 IO-area, no RAM-check
		.word TXT_KX   	
		.byte TNO, $10, $10     ; $1000 - $13FF, K4 IO-area, no RAM-check
		.word TXT_KX   	
		.byte TRAM, $14, $14    ; $1400 - $15FF, check all
		.word TXT0   	
		.byte TNO, $16, $16     ; $1600 - $16FF, 6551 ACIA, no RAM-check
		.word TXT_ACIA   	
		.byte TPIA, $17, $17    ; $1700 - $17FF, 6520 PIA, no RAM-check
TBL_PIA_TXT	.word TXT_PIA   	
		.byte TRAM, $18, $18    ; $1800 - $19FF, check all
		.word TXT0   	
		.byte TRIOT, $1A, $1A   ; $1800 - $1AFF, 6532 RIOT
		.word TXT_RIOT 		; 6532 RIOT, check RAM (128 B) at $1A00-$1A7F
		.byte TRAM, $1B, $1B    ; $1B00 - $1BFF, check all
		.word TXT0   	
		.byte TROM, $1C, $1C    ; $1C00 - $1FFF, JC Monitor ROM
		.word TXT_MON   	
		.byte TRAM, $20, $27    ; $2000 - $3FFF, check from $2700 (this program!)
		.word TXT0   	
		.byte TRAM, $40, $40    ; $4000 - $7FFF, check all
		.word TXT0   	
		.byte TRAM, $80, $80    ; $8000 - $9FFF, check all
		.word TXT_USER_RAM   	
		.byte TRAM, $A0, $A0    ; $A000 - $BFFF, check all
		.word TXT_USER_RAM   	
RAM_TABLE_C0	.byte TRAM, $C0, $C0    ; $C000 - $DFFF, check all
		.word TXT_USER_RAM  	
		.byte TROM, $E0, $E0    ; $E000 - $FFFF, JC2 BIOS ROM, ROM-checksum only
		.word TXT_BIOS   	
TBL_MAX = (*-RAM_TABLE)/TBL1_SZ

; If the IO card with BASIC ROM is present, this entry is copied
; over the existing $C000-$DFFF entry.
RAM_TABLE_B0	.byte TROM, $B0, $B0    ; $B000 - $DFFF, BASIC ROM
		.word TXT_USER_RAM  	

TXT_TITLE	.by     'JC ][ Memory-Tester (ROM/RAM) v0.34 by Emile' CR
		.by     'CPU-type     : ' $00
TXT0		.byte	$00
TXT1		.by 	'Block $' $00
TXT1A		.by 	': $' $00
TXT1B		.by 	'00-$' $00
TXT1C		.by 	'FF, ' $00
TXT_ACIA	.by     '6551 ACIA' CR $00
TXT_PIA		.by     '6520 PIA (MMU) ' $00
TXT_MMU		.by     'ATF1504 MMU ' $00
TXT_RIOT	.by     '6532 RIOT, ' $00
TXT_MON		.by	'JC Monitor ROM 1 KB, ' $00
TXT_BIOS	.by 	'JC2 BIOS ROM 8 KB, ' $00
TXT_USER_RAM	.by 	'User RAM or IO-area, ' $00
TXT_RAMKB_OK	.by 	' KB RAM OK' CR $00
TXT_RAM_OK	.by 	' bytes RAM OK' CR $00
TXT_RAM_NOK	.by 	'RAM error or not present' CR $00
TXT_ROM_OK	.by 	'ROM checksum OK' CR $00
TXT_ROM_NOK	.by 	'ROM checksum error: $' $00
TXT_KX		.by 	' IO-area, no RAM/ROM present' CR $00
TXT_6502	.by     'NMOS 6502'     CR $00
TXT_65C02	.by     'CMOS 65C02'    CR $00
TXT_65816	.by     'CMOS 65C816 16-bit' CR $00 
TXT_PIA2        .by     ' RAM-banks found' CR $00
TXT_PIA_NOT	.by 	'not ' $00
TXT_PIA_DET	.by	'present, ' $00
TXT_RAMB1	.by 	'RAM-BANK ' $00
TXT_RAMB_OK	.by 	'16 KB RAM OK' CR $00
TXT_RAMB_NOK	.by 	'RAM-bank memory error' CR $00
TXT_MONRAM1	.by	'           $1C00-$1FFF, RAM behind Monitor ROM, ' $00
TXT_MONRAM_OK	.by     '1 KB RAM OK' CR $00
TXT_BASICRAM_NOK
TXT_BIOSRAM_NOK
TXT_MONRAM_NOK  .by	'RAM memory error' CR $00
TXT_BIOSRAM1	.by	'           $E000-$FFFF, RAM behind BIOS ROM, ' $00
TXT_BIOSRAM_OK	.by     '8 KB RAM OK' CR $00
TXT_BASICRAM1	.by	'           $B000-$DFFF, RAM behind BASIC ROM, ' $00
TXT_BASICRAM_OK	.by     '12 KB RAM OK' CR $00
TXT_628128	.by	'SRAM 628128 IC detected' CR $00
TXT_IOL1        .by     'BASIC-IO card: ' $00
TXT_IOL_OK	.by	'detected at $' $00
TXT_IOL_NOK	.by	'not present' CR CR $00
MEM_TEST_END = *
