%{

static char rcsid[] = "$Id:$";

#define EXT extern 
#ifdef ECHO
#undef ECHO
#endif
#include "daemon.h"
#include "iprintcap_y.h"
char *yystrdup();

int yyfd= -1;
int yyupchar= -1;
#undef input
#undef unput
extern int yypclineno;
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
	if (yyfd==-1) yyfd=open(fileread=PRINTCAP,O_RDONLY);
	if (yyfd==-1) yyfd=open(fileread=OLDPRINTCAP,O_RDONLY); /* for compatibility */
	if (yyfd==-1) {  perror("Warning: could not open prtdef file"); return 0; }
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
unixpath [a-zA-Z0-9/._-]+
stuff \".*\"
comment #.*
%%
{comment}       ;
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
{unixpath}	{ 
			yylval.string = yystrdup(yytext);
			return PATH;
		}
{stuff}	        { 
			yylval.string = yystrdup(yytext);
			return STUFF;
		}
{nl}		{
			++yypclineno;
			return '\n';
		}
.		{	return yytext[0]; }
