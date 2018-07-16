%{

static char rcsid[] = "$Id:$";

#define EXT extern 
#ifdef ECHO
#undef ECHO
#endif
#include "defs.h"
#include "manage.h"
#include "qpaths.h"
#include "keyconfig_y.h"
char *yystrdup();

int yyfd= -1;
int yyupchar= -1;
#undef input
#undef unput
extern int yyqcflineno;
static int input()
{	
	int tmp;
	char buf[1];
        extern char *fileread;
	if (yyupchar != -1)
	{
		tmp=yyupchar;
		yyupchar = -1;
		return tmp;
	}
	if (yyfd==-1) yyfd=open(fileread=keydeffile,O_RDONLY);
	if (yyfd==-1) { return 0; }
	if (read(yyfd,buf,1)==1) { return buf[0]; }
	else { close(yyfd); yyfd= -1; yyupchar= -1; return 0; }
}
static int unput(ch)
{
	if (yyupchar == -1) { yyupchar=ch; return; }
	else return;
}
%}

ident [A-Za-z_][A-Za-z0-9_]*
whitesp [ \t]+
eq =
number [0-9]+
comma \,
nl \n
colon \:
stuff \".*\"
%%
{whitesp}	;
{comma}         { return COMMA; }
{eq}		{ return EQUAL; }
{ident}		{ 
			if (yylval.kw=yykeywd(yytext)){ 
				return yylval.kw; }
			yylval.string = yystrdup(yytext); 
			return VAR;
		}
{nl}		{
			++yylineno;
			return '\n';
		}
{stuff}	        { 
			yylval.string = yystrdup(yytext);
			return STUFF;
		}
{number}        { yylval.num = atoi(yytext); return NUMBER; }
.               {	return yytext[0]; }
