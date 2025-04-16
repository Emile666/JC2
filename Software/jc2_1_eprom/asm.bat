@echo off
echo This is the assembly for the 32K JC2 EPROM.
mads jc2_main.asm -l:jc2_main.lst -o:jc2_main.bin -fv:$FF -t:jc2_main.txt

