# Junior Computer 2 (JC ][)
Junior-Computer-2 Hardware and Software projects.

# History
The Junior computer design was published by Elektuur/Elektor from 1980 on. It was developed by Loys Nachtmann. The Junior computer is more or less a copy of the original KIM-1 single board computer.

Fourty years later, Joerg Walke created the Junior-Computer 2, which is an expanded version of the original Junior Computer. It contained 32 KB RAM, 8KB ROM and an onboard RS232 serial interface. The development of the Junior Computer 2 is still in progress and you can read all about it on the web-site of Joerg: https://old-computer-tech.net/junior-computer/

Up until now Joerg created the JC2-board (v3.1b), the IO-Basic card, the JC2 backplane card and the Floppy-Graphics (FGC) card.

I became enthusiastic about all the excellent hardware and software Joerg developed and decided to built these boards and then to create add-ons, both hardware and software.

This page serves as a placeholder for all hardware and software projects that I created. It is all open-source, so enjoy!

# Hardware projects

Hardware projects are (mostly) created with KiCad 8 and I publish both the KiCad project-files as the Gerber files. The Gerber files can be sent directly to a PCB manufacturer.

Since these projects nearly always contain a GAL or CPLD, the .pld files describing the programmable logic is also included. I typically use an ATF16V8, an ATF22V10 or an ATF1504 (plcc44 and plcc84). They are programmed with WinCUPL and the .pld file can be loaded directly into WinCUPL. The resulting .jed file is either programmed into the GAL / CPLD directly with the Afterburner programmer (ATF16V8 and ATF22V10), or further processed by ATMISP (to create a .svf file from the .jed file) and svf2xsvf502 (to create a .xsvf file from the .svf file) and then programmed by the Afterburner programmer into the device (ATF1504). A detailed walk-through for an ATF1504 is found [here](./atf1504_program_aftb.md).

- [RAM-bank add-on](./Hardware/rambank): I created a 512K add-on board for the JC2 which contains a AS6C4008 512Kx8 SRAM and a ATF1504 plcc44 GAL. It contains a memory management unit (MMU) register that can switch Monitor ROM, BIOS ROM and the different RAM-banks (28 banks of 16K each). Current version is made with Eagle, never revisions are planned to be made with KiCad.
- [Compact-Flash IDE interface](./Hardware/cf_ide): The idea originated from the Atari XL CF-IDE interface. I changed it for the JC2. Most of the TTL ICs were removed and put into a GAL (an ATF16V8).
- [Backplane](./Hardware/backplane/): I created an update of Joerg's original backplane and added a flat-cable connector to it, so that you can have multiple backplanes connected together.
- [Main-Board](./Hardware/main_board/): I created an update of Joerg's original v3.1b main-board, most notable changes are: 1) one integrated eprom combining Monitor, Basic and BIOS 2) ATF1504 MMU added that supports 512K RAM with 28 RAM-banks of 16K each 3) Monitor, BIOS and Basic ROM are now switchable from MMU, enabling RAM behind it 4) Additional 6522 VIA 5) UART-to-USB bridge added.
- [IO2-Board](./Hardware/IO2/): I created an update of Joerg's original rev. 1D IO-board, most notable changes are: 1) BASIC eprom moved to JC2 main-board and removed all diode-based logic 2) removed papertape reader, Datasette interface and ASCII-keyboard 3) Added 3 x 16-bits IO with MCP23017 ICs 4) Added SPI RAM (1024 Kb)  and I2C eeprom (512 Kb) 5) Added PCF8591 8-bit ADC and DAC 6) Added One-Wire Interface with DS2482 7) Added CF-IDE Interface 8) Level-shifting with BSS138 board instead of 74LVC245.

# Software projects

Software projects are written in 6502 assembly language and are assembled with the Mad-Assembler (MADS), https://mads.atari8.info/. I call MADS from the Windows command-line with mads <filename.asm> -l:<filename.lst> -o:<filename.bin>. This makes sure that you get a decent list-file and the .bin file that can be uploaded to the JC2 using the XMODEM protocol.

- [Memory-test](./Software/memtest): The memory-test program scans the hardware present and detects if a MMU is present. It shows and tests all available RAM and ROM memory, displays the number of RAM-banks found and tests each one of them.
- [Monitor](./Software/jc2_mon_1K): This is the original monitor program written by A. Nachtmann (1980), which was restored and adapted for TASM by A.J. Prosman (2019). I adapted the source for MADS (2024). Included here for reference.
- [JC2-DOS65](./Software/jc2_dos65): Joerg made a v0.2.0 version that supported the SD-card. This version here is a further development and now supports the CF-IDE Interface.
- [JC2-1-EPROM](./Software/jc2_1_eprom): This software integrates BASIC, BIOS and Monitor ROM into a single EPROM version. It already includes the CF-IDE drivers and is supported by a RAM-loader for testing without directly creating a new EPROM version.




