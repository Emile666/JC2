#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <ctype.h>
#include "jc2_basic.tab.h"

extern int   yylex(void); // lexer function
extern FILE *yyin;        // input file for lexer

#define MAX_LINES (1000)

struct {
    uint16_t line_num; /* Basic line-number */
    char     s[512];   /* Hex-codes of Basic line */
} line[MAX_LINES];
uint16_t line_cntr = 0;
uint8_t  obj_out[65500];

const char token[128][9] = {"END"    ,"FOR"  ,"NEXT"  ,"DATA"   ,"INPUT","DIM"   ,"READ" ,"LET"   ,"DEC"    ,"GOTO"  ,"RUN"    ,"IF"   ,"RESTORE","GOSUB"   ,"SOUND"   ,"RES2"    ,
                  /* $90 */ "RETURN" ,"REM"  ,"STOP"  ,"ON"     ,"NULL" ,"INC"   ,"WAIT" ,"LOAD"  ,"SAVE"   ,"DEF"   ,"POKE"   ,"DOKE" ,"CALL"   ,"DO"      ,"LOOP"    ,"PRINT"   ,
                  /* $A0 */ "CONT"   ,"LIST" ,"CLEAR" ,"NEW"    ,"WIDTH","GET"   ,"SWAP" ,"BITSET","BITCLR" ,"RES3"  ,"RES4"   ,"BEEP" ,"PLIST"  ,"HOME"    ,"CLS"     ,"NORMAL"  ,
                  /* $B0 */ "INVERSE","FLASH","LOCATE","IN#"    ,"PR#"  ,"PORTIO","PORT" ,"SCREEN","PIXEL"  ,"LINE"  ,"OVAL"   ,"RECT" ,"COLOR"  ,"DELAY"   ,"I2Cout"  ,"DOS"     ,
                  /* $C0 */ "TAB("   ,"ELSE" ,"TO"    ,"FN"     ,"SPC(" ,"THEN"  ,"NOT"  ,"STEP"  ,"UNTIL"  ,"WHILE" ,"OFF"    ,"+"    ,"-"      ,"*"       ,"/"       ,"MOD_#"   ,
                  /* $D0 */ "^"      ,"AND"  ,"EOR"   ,"OR"     ,">>"   ,"<<"    ,">"    ,"="     ,"<"      ,"SGN("  ,"INT("   ,"ABS(" ,"USR("   ,"FRE("    ,"POS("    ,"SQR("    ,
                  /* $E0 */ "RND("   ,"LOG(" ,"EXP("  ,"COS("   ,"SIN(" ,"TAN("  ,"ATN(" ,"PEEK(" ,"DEEK("  ,"SADD(" ,"LEN("   ,"STR$(","VAL"    ,"ASC("    ,"UCASE$(" ,"LCASE$(" ,
                  /* $F0 */ "CHR$("  ,"HEX$(","BIN$(" ,"BITTST(","MAX(" ,"MIN("  ,"PI"   ,"TWOPI" ,"VARPTR(","LEFT$(","RIGHT$(","MID$(","PORT("  ,"I2Cin("  ,"RES5("   ,"RES6("   };

char *get_fname_ext(char *filename)
{
    char *dot = strrchr(filename, '.');
    if (!dot || dot == filename) return "";
    return dot + 1;
} // get fname_ext()

bool check_fname_ext(char *fname, bool *bas)
{
    *bas = !strcmp("bas",get_fname_ext(fname));
    return *bas || !strcmp("txt",get_fname_ext(fname));
} // check_fname_ext()

void process_token(uint8_t c, FILE *f)
{
    printf("%s",token[c]);
    fprintf(f,"%s",token[c]);
} // process_token()

// Convert char to hex number, e.g. 'C' -> 12
uint8_t hex1(char bt)
{
    char ch = toupper(bt);

    if ((ch >= '0') && (ch <= '9'))
        return (uint8_t)(ch - '0');
    else if ((ch >= 'A') && (ch <= 'F'))
        return (uint8_t)(ch - 'A' + 10);
    return 0;
} // hex()

// Convert char to hex number, e.g. 'C' -> 12
uint8_t hex(char c1, char c2)
{
    return (hex1(c1)<<4) + hex1(c2);
} // hex()

void create_output_file(FILE *f)
{
    uint16_t j, len, i=0, cntr=4;
    uint16_t addr_begin = 0x2000; // Basic-file in 6502 memory
    uint16_t eol;

    obj_out[0] = addr_begin & 0x00FF; // LSB
    obj_out[1] = addr_begin >> 8;     // MSB

    while (line[i].line_num > 0)
    {   // Basic-line
        len = strlen(line[i].s)>>1; // #bytes of line
        eol = addr_begin + cntr + len + 1; //  end-address of this line
        printf("len=%d,eol=%d,cntr=%d,",len,eol,cntr);
        obj_out[cntr++] = eol & 0x00FF; // LSB
        obj_out[cntr++] = eol >> 8;     // MSB
        for (j = 0; j < strlen(line[i].s + 1); j += 2)
        {   // convert all hex-codes in Basic-line to bytes
            obj_out[cntr++] = hex(line[i].s[j],line[i].s[j+1]);
        } // for j
        obj_out[cntr++] = 0x00; // marks beginning of next-line
        i++; // next Basic-line
    } // while
    obj_out[2] = (addr_begin + cntr) & 0x00FF; // LSB
    obj_out[3] = (addr_begin + cntr) >> 8;     // MSB
    obj_out[cntr++] = 0x00;
    obj_out[cntr++] = 0x00; // marks end-of-file
    obj_out[cntr++] = 0x6E; // 'n', also in original .bas files
    printf("cntr=%d",cntr);
    for (i = 0; i < 80; i++)
    {   // fill-up with zeros
        obj_out[cntr++] = 0x00;
    } // for i
    printf("\n");
    for (i = 0; i < cntr; i++)
    {
        if (((i % 8) == 0) && ((i % 16) > 0))
             printf("|");
        else printf(" ");
        if ((i % 16) == 0) printf("\n%04X: ",i+0x2000);
        printf("%02X",obj_out[i]);
    } // for i
    printf("\n");
    fwrite(obj_out,1,cntr,f); // write to output-file
} // create_output_file()

// This function takes a readable Basic text-file and converts it into a coded JC2 Basic file
// The flex yyin file is already opened by the calling program
void txt_to_bas(FILE *g)
{
    char s[512];
    int  yychar; /* The lookahead symbol.  */

   	for (uint16_t i = 0; i < MAX_LINES; i++)
    {   // Init. line-struct
        line[i].line_num = 0;
    } // for i
   while ((yychar = yylex()) > 0)
   {
        if (yychar == LINE_NUMBER)
        {
            sprintf(s,"%02X%02X",yylval.i&0xFF,yylval.i>>8);
            printf("%s",s);
            strcpy(line[line_cntr].s,s);
            line[line_cntr].line_num = yylval.i;
            //printf("{line_cntr=%d,line_num=%d}",line_cntr,line[line_cntr].line_num);
        } // if
//        else if (yychar == REM)
//        {   // REM + text till EOL
//            sprintf(s,"%02X",yychar-0x200);
//            printf("%s",s);
//            strcat(line[line_cntr].s,s);
//            //printf("{REM len=%d}",strlen(yylval.s));
//            for (int i = 0; i < strlen(yylval.s); i++)
//            {
//                if (yylval.s[i] != '\r')
//                {
//                    sprintf(s,"%02X",yylval.s[i]);
//                    printf("%s",s);
//                    strcat(line[line_cntr].s,s);
//                } // if
//            } // for i
//            //line_cntr++;
//        } // else if
        else if (yychar >= 0x200)
        {   // A Basic token
            sprintf(s,"%02X",yychar-0x200);
            printf("%s",s);
            strcat(line[line_cntr].s,s);
        } // else if
        else if (yychar == '\n')
        {   // End of Line
            sprintf(s,"\n");
            printf("%s",s);
            strcat(line[line_cntr++].s,s); // next line
        } // else if
        else
        {   // a char, just copy it
            sprintf(s,"%02X",yychar);
            printf("%s",s);
            strcat(line[line_cntr].s,s);
        } // else
   } ; // while
   printf("#lines: %d\n",line_cntr);
   create_output_file(g);
} // txt_to_bas()

// This function takes a coded JC2 Basic file and converts it into readable text
void bas_to_txt(FILE *f, FILE *g)
{
    uint8_t  bas_buf[32768];
    uint16_t memb, meme, bol, eol, brd, lnum;
    uint16_t i;
    bool     err = false;
    uint8_t  ch;

    brd  = fread(bas_buf, 1, 6, f);
    memb = bas_buf[0]+256*bas_buf[1];
    meme = bas_buf[2]+256*bas_buf[3];
    eol  = bas_buf[4]+256*bas_buf[5]; // points to last char. on current line
    bol  = memb + 6;                  // points to 1st byte of line-number
    brd  = fread(&bas_buf[6], 1, meme - memb + 1 - 6, f);
    if (brd != meme - memb - 5) printf("Read-error: %d bytes to read, %d bytes read\n",meme - memb - 5, brd);

    printf("File-size in Memory: $%04X - $%04X, Size: %d bytes\n\n",memb, meme, meme-memb);

	while ((eol > memb) && (eol <= meme) && !err)
    {
        i    = bol - memb; // index in buffer
        lnum = bas_buf[i] + 256 * bas_buf[i+1];
        err  = false;
        i   += 2; // now at start of code in this line
        printf("%d ",lnum);
        fprintf(g,"%d ",lnum); // write to output file
        while (i < eol + 1 - memb)
        {
            ch = bas_buf[i];
            if (ch < 0x80)
            {
                printf("%c",(char)ch);
                fprintf(g,"%c",(char)ch);
            } // if
            else process_token((uint8_t)(ch-0x80),g);
            i++; // next char. in line
        } // while
        //printf("i=0x%02X(0x%04X),bas_buf[i]=%02X ",i,i+memb,bas_buf[i]); // should be 0
        err  = (bas_buf[i] != 0x00);
        bol  = i + 3 + memb;                  // points to 1st byte of line-number
        eol  = bas_buf[i+1]+256*bas_buf[i+2]; // points to last char. on current line
        //printf("bol=%04X,eol=%04X\n",bol,eol);
        printf("\n");
        fprintf(g,"\n");
    } // while
    if (err) printf("Error in Basic file: memb=%04X, meme=%04X, bol=%04X, eol=%04X\n",memb,meme,bol,eol);
} // bas_to_txt()

int main(int argc, char *argv[])
{
    FILE     *f; // input file
    FILE     *g; // output file
    char     fin[100];
    char     fout[100];
    bool     finbas  = false;
    bool     foutbas = false;

    if (argc != 3)
    {
        printf("Usage: JC2_Basic filename_in filename_out\n");
        printf("       If .bas .txt then dis-assemble from JC2 .bas to .txt\n");
        printf("       If .txt .bas then assemble from .txt to JC2 .bas\n");
        return 0;
    } // if
    strcpy(fin,argv[1]);
    strcpy(fout,argv[2]);
    if (!check_fname_ext(fin, &finbas))
    {
        printf("%s: not a .bas or .txt file\n",fin);
        return 0;
    } // if
    if (!check_fname_ext(fout, &foutbas))
    {
        printf("%s: not a .bas or .txt file\n",fin);
        return 0;
    } // if
    if (finbas == foutbas)
    {
        printf("Only one .bas file and one .txt file is allowed!\n");
        return 0;
    } // if

    // Open output file
	if ((g = fopen(fout,"wb")) == NULL)
	{
		printf("Could not open %s for writing\n",fout);
		exit(0);
	} // if
	else printf("Opened %s for writing\n",fout);

    if (finbas)
    {   // Convert JC2 coded .bas file to readable .txt file
        if ((f = fopen(fin,"rb")) == NULL)
        {
            printf("Could not open %s\n",fin);
            exit(0);
        } // if
        bas_to_txt(f,g);
        fclose(f);
    } // if
    else
    {   // Convert readable Basic .txt file to a coded JC2 .bas file
        if ((yyin = fopen(fin,"rb")) == NULL)
        {   // yyin is file for lexer
            printf("Could not open %s\n",fin);
            exit(0);
        } // if
        txt_to_bas(g); // Use lexer to write coded JC2 .bas file
        fclose(yyin);
    } // else
    fclose(g);
    printf("Ready!\n");
    return 0;
}
