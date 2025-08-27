@echo off
echo This is the assembly for the 32K JC2 EPROM.
mads jc2_main.asm -i:"..\inc\" -l:jc2_main.lst -o:jc2_main.bin -fv:$FF -t:jc2_main.txt
jc2_csum jc2_main.bin
jc2_defines jc2_main.txt ..\inc\jc2_bios_calls.inc

