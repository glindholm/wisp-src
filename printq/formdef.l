%{

static char rcsid[] = "$Id:$";

#define EXT extern 
#ifdef ECHO
#undef ECHO
#endif
#include "daemon.h"
#include "formdef_y.h"
char *yystrdup();

int yyfd= -1;
int yyupchar= -1;
#undef input
#undef unput
extern int yyfdlineno;
static int input()
{	
	int tmp;
        extern char *fileread;
	char buf[1];
	if (yyupchar != -1)
	{
		tmp=yyupchar;
		yyupchar = -1;
		return tmp;
	}
	if (yyfd==-1) yyfd=open(fileread=FORMDEF,O_RDONLY);
	if (yyfd==-1) { perror("Warning: could not open formdef file"); return 0; }
	if (read(yyfd,buf,1)==1) { return buf[0]; }
	else { close(yyfd); yyfd= -1; yyupchar= -1; return 0; }
}
static int unput(ch)
{
	if (yyupchar == -1) { yyupchar=ch; return; }
	else return;
}
%}

ident [A-Za-z0-9]+
whitesp [ \t]+
eq =
number [0-9]+
colon :
nl \n
stuff \".*\"
comment #.*


%%
{comment}       ;
\({ident}\)     { yylval.string = yystrdup(yytext);
                  return MODEL;
                }
{whitesp}	;
{number}	{ sscanf(yytext,"%d",&yylval.num); return NUMBER; }
{eq}		{ return EQUAL; }
{colon}		{ return COLON; }
{ident}		{ 
			if (yylval.kw=yykeywd(yytext))
				return yylval.kw;
			yylval.string = yystrdup(yytext);
			return VAR;
		}
{stuff}	        { 
			yylval.string = yystrdup(yytext);
			return STUFF;
		}
{nl}		{
			++yyfdlineno;
			return '\n';
		}
.		{	return yytext[0]; }
