%{

static char rcsid[]="$Id:$";

#define EXT extern 
#include "daemon.h"

extern CLASSINFO class_info;
int yyclassindex;
int yycdlineno;
extern char yylbuf[];
extern int yylpos;
extern int yynewln;

%}

%union {
    int kw;
    int num;
    char *string;
}

%token <num> EQUAL COLON COMMA
%token <string> WORD
%token <kw> BANNER FF BEFORE AFTER BOTH NEITHER YES NO BETWEEN

%%


file:	
	| file record	
	;

record: '\n'
        | name fields   
	;

fields: 
	| fields field
	;

name:	WORD COLON 	
{ 
	char tmp;
	tmp = $1[0];
	yyclassindex = tmp == ' '?0: toupper(tmp) - '@';
}
	;

field:	'\n'
        | BANNER EQUAL YES  {
		class_info.classlist[yyclassindex].header = 1;
		
		}
        | BANNER EQUAL NO  {
		class_info.classlist[yyclassindex].header = 0;
		}

        | FF EQUAL BEFORE  {
		class_info.classlist[yyclassindex].ffbefore = 1;
		
		}
        | FF EQUAL AFTER  {
		class_info.classlist[yyclassindex].ffafter = 1;

		}
        | FF EQUAL NEITHER  {
		class_info.classlist[yyclassindex].ffbefore = 0;
		class_info.classlist[yyclassindex].ffafter = 0;
		}
        | FF EQUAL BOTH  {
		class_info.classlist[yyclassindex].ffbefore = 1;
		class_info.classlist[yyclassindex].ffafter = 1;
		}
        | FF EQUAL BETWEEN  {
		class_info.classlist[yyclassindex].ffbetween = 1;
		}
        ;
%%

char *yystrdup(p)
char *p;
{
	char *tmp;
	char *gmem();
	int l;

	l=strlen(p);
	tmp=gmem(l+1,1);
	memcpy(tmp,p,l);
	*(tmp+l)=(char)0;
	return tmp;
}
static struct yykeyword
{
	char *name;
	int value;
} yykeywords[] =
{
   { "formfeed", FF },
   { "ff", FF },
   { "banner", BANNER },
   { "yes", YES },
   { "on", YES },
   { "no", NO },
   { "off", NO },
   { "before", BEFORE },
   { "after", AFTER },
   { "both", BOTH },
   { "between", BETWEEN },
   { "neither", NEITHER },
   { NULL, 0 }
};
int yykeywd(string)
char *string;
{
	struct yykeyword *ptr = yykeywords;
	while (ptr->name)
	{
		if (strcmp(ptr->name,string)==0) return ptr->value;
		else ++ptr;
	}	
	return 0;
}
static int yyfirst=TRUE;
loadclassdef()
{
	extern int yyupchar;

	yycdlineno=0;
	memset(&class_info,0,sizeof(class_info));
	yyupchar = -1;
	yyparse();
	yyfirst=FALSE;

}
static int yyerror()
{
	char yyerrbuf[200];
	extern char *fileread;
	char *repchar();
	extern int fileerrs;
	
	fileerrs |= CLDEFERR;
	
	sprintf(yyerrbuf,"Syntax error on %s line %d:\n",fileread,yycdlineno+1);
	logerr(yyerrbuf,0);
	if (yyfirst)
	{
		fprintf(stderr,yyerrbuf);
	}
	yylbuf[yylpos]='\0';
	sprintf(yyerrbuf,"%s\n",yylbuf);
	logerr(yyerrbuf,0);
	if (yyfirst)
	{
		fprintf(stderr,yyerrbuf);
	}
	sprintf(yyerrbuf,"%s^ error near this position\n",repchar(yylpos,(int)' '));

	logerr(yyerrbuf,0);
	if (yyfirst)
	{
		fprintf(stderr,yyerrbuf);
	}
}
int yywrap()
{ 
	return 1;
}
