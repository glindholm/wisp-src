%{

static char rcsid[] = "$id: iprintcap.l,v 1.5 1992/05/20 23:17:32 jockc v2_18 jockc $";

#define EXT extern 
#ifdef echo
#undef echo
#endif
#ifdef ECHO
#undef ECHO
#endif
#include "daemon.h"
#include "iprintcap_y.h"
char *yystrdup();

char yylbuff[1024] = {0};
int yylposs=0;
int yynewlnn=FALSE;

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
	if (yyfd==-1) 
	{
		yyfd=open(fileread=prtdef,O_RDONLY);
	}
	if (yyfd==-1) 
	{
		yyfd=open(fileread=printcap,O_RDONLY); /* for compatibility */
	}
	if (yyfd==-1) 
	{  
		perror("Warning: could not open prtdef file"); 
		return 0; 
	}
	if (read(yyfd,buf,1)==1) 
        { 
		if (yynewlnn)
		{
			yylposs=0;
			yynewlnn=FALSE;
		}
		if (buf[0]=='\n')
		{
			++yynewlnn;
		}
		else
		{
			yylbuff[yylposs++]=buf[0];
		}
                return buf[0]; 
        }
	else 
	{ 
		close(yyfd); 
		yyfd= -1; 
		yyupchar= -1; 
		return 0; 
	}
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
{number}	{
#ifdef __STDC__
	sscanf((CONST char *)yytext,"%d",&yylval.num); return NUMBER;
#else
	sscanf(yytext,"%d",&yylval.num); return NUMBER;
#endif
 		}
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
