%{

static char rcsid[]="$Id:$";

#define EXT extern 
#include "daemon.h"

extern MASTCONFIG qconfig;
int yylineno;

%}

%union {
    int kw;
    int num;
    char *string;
}

%token <num> EQUAL COMMA LETTER NUMBER
%token <string>  VAR
%token <kw> OPL DEFPRT DEFFORM DEFCLASS

%%


file:	
	| file fields
	;

ops: 
	| ops op
	;

op:	
	| VAR
	{
		new_op(&qconfig,$1);
	}
	| VAR COMMA
	{
		new_op(&qconfig,$1);
	}


fields:	'\n'
  	| OPL EQUAL ops '\n'
		{
			;
		}

	| DEFPRT EQUAL VAR '\n'
		{
			strcpy(qconfig.default_printer,$3);
			free($3);
		}
	| DEFFORM EQUAL VAR '\n'
		{
			strcpy(qconfig.default_form,$3);
			free($3);
		}
	| DEFFORM EQUAL NUMBER '\n'
		{
			sprintf(qconfig.default_form,"%03d",$3);
		}
	| DEFCLASS EQUAL VAR '\n'
		{
			qconfig.default_class = $3[0];			
		}
        ;
%%

char *yystrdup(p)
char *p;
{
	char *tmp,*malloc();
	int l;

	l=strlen(p);
	tmp=malloc(l+1);
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
   { "op", OPL },
   { "operators", OPL },
   { "defprt", DEFPRT },
   { "default_printer", DEFPRT },
   { "defform", DEFFORM },
   { "default_form", DEFFORM },
   { "defclass", DEFCLASS },
   { "default_class", DEFCLASS },
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

loadqconfig()
{
	extern int yyupchar;

	yylineno=0;
	memset(&qconfig,0,sizeof(qconfig));
	yyupchar = -1;
	yyparse();
}
static int yyerror()
{
	char yyerrbuf[200];
	extern char *fileread;
	
	sprintf(yyerrbuf,"syntax error on %s line %d\n",fileread,yylineno);
	logerr(yyerrbuf,0);
}
int yywrap()
{ 
	return 1;
}
