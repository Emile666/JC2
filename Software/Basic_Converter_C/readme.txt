This is a PC-application that converts a .txt Basic Program written on the PC into
a .bas Basic Program that can be loaded into the JC2.
It also works from .bas to .txt, e.g. when you have a .bas file loaded from the JC2
and you want to convert it into readable .txt.

Compiled with Code::Blocks.

The jc2_basic.l is the lexical analyzer source and needs to be converted into a lex.yy.c
file with the following command: flex jc2_basic.l
The jc2_basic.tab.h file is a required file for flex and contains all the Basic keywords.

