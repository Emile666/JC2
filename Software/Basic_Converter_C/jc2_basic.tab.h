
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
END		= 0x280,
FOR		,
NEXT	,
DATA	,
INPUT	,
DIM		,
READ	,
LET		,
DEC		,
GOTO	,
RUN		,
IF		,
RESTORE	,
GOSUB	,
SOUND	,
RES2	,
RETURN	,
REM		,
STOP	,
ON		,
_NULL	,
INC		,
WAIT	,
LOAD	,
SAVE	,
DEF		,
POKE	,
DOKE	,
CALL	,
DO		,
LOOP	,
PRINT	,
CONT	,
LIST	,
CLEAR	,
NEW		,
WIDTH	,
GET		,
SWAP	,
BITSET	,
BITCLR	,
RES3		,
RES4		,
BEEP	,
PLIST   ,
HOME	,
CLS		,
NORMAL	,
INVERSE	,
FLASH	,
LOCATE	,
IN_	    ,
PR_	    ,
PORTIO	,
PORTOUT	,
SCREEN	,
PIXEL	,
LINE	,
OVAL	,
RECT	,
COLOR	,
DELAY	,
I2COUT	,
DOS	,
TAB		,
ELSE	,
TO		,
FN		,
SPC		,
THEN	,
NOT		,
STEP	,
UNTIL	,
WHILE	,
OFF		,
PLUS	,
MINUS	,
MUL		,
DIV		,
MOD		,
POWER	,
AND		,
EOR		,
OR		,
SHR 	,
SHL 	,
GT		,
EQUAL	,
LT		,
SGN		,
INT		,
ABS		,
USR		,
FRE		,
POS		,
SQR		,
RND		,
LOG		,
EXP		,
COS		,
SIN		,
TAN		,
ATN		,
PEEK	,
DEEK	,
SADD	,
LEN		,
STR		,
VAL		,
ASC		,
UCASE	,
LCASE	,
CHR		,
HEXSTR	,
BINSTR	,
BITTST	,
MAX		,
MIN		,
PI		,
TWOPI	,
VARPTR	,
LEFT	,
RIGHT	,
MID		,
PORTIN	,
I2CIN	,
RES5	,
RES6	,
LINE_NUMBER
   };
#endif

typedef union YYSTYPE
{
  double d;
  int i;
  char *s;
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1

extern YYSTYPE yylval;



