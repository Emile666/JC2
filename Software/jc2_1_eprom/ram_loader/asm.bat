@echo off
echo This is the assembly for the BIOS RAM Loader.
mads ram_load.asm -l:ram_load.lst -o:ram_load.bin -fv:$FF -t:ram_load.txt

