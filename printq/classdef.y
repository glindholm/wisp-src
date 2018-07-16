%{

static char rcsid[]="$Id:$";

#define EXT extern 
#include "daemon.h"

extern CLASSINFO class_info;
int yyclassindex;
int yycdlineno;

%}

%union {
    int kw;
    int num;
    char *string;
}

%token <num> EQUAL COLON COMMA
%token <string> WORD
%token <kw> BANNER FF BEFORE AFTER BOTH NEITHER YES NO 

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
        ;
%%

char *yystrdup(p)
char *p;
{
#ifdef __STDC__
	char *tmp;
	void *malloc();
#else
	char *tmp,*malloc();
#endif
	int l;

	l=strlen(p);
	tmp=(char*)malloc(l+1);
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

loadclassdef()
{
	extern int yyupchar;

	yycdlineno=0;
	memset(&class_info,0,sizeof(class_info));
	yyupchar = -1;
	yyparse();
}
static int yyerror()
{
	char yyerrbuf[200];
	extern char *fileread;
	
	sprintf(yyerrbuf,"syntax error on %s line %d\n",fileread,yycdlineno);
	logerr(yyerrbuf,0);
}
int yywrap()
{ 
	return 1;
}
