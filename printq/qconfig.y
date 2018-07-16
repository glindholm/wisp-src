%{

static char rcsid[]="$Id:$";

#define EXT extern 
#include "daemon.h"

extern MASTCONFIG qconfig;
int yylineno;
extern char yylbuf[];
extern int yylpos;
extern int yynewln;

%}

%union {
    int kw;
    int num;
    char *string;
}

%token <num> EQUAL COMMA LETTER NUMBER
%token <string>  VAR
%token <kw> OPL DEFPRT DEFFORM DEFCLASS INET_ADDRESS
%token <kw> DEFMODE MODEWANG MODEUNIQUE RUNPRI

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
        | INET_ADDRESS EQUAL NUMBER '\n'
                {
			qconfig.inet_address = $3;
		}
        | RUNPRI EQUAL NUMBER '\n'
                {
			qconfig.priority = $3;
		}
        | DEFMODE EQUAL MODEWANG '\n'
                {
			qconfig.default_scrmode = SCRMODE_WANG;
 	        }
        | DEFMODE EQUAL MODEUNIQUE '\n'
                {
			qconfig.default_scrmode = SCRMODE_UNIQUE;
 	        }
        ;
%%

char *yystrdup(p)
char *p;
{
	char *tmp,*gmem();
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
   { "op", OPL },
   { "operators", OPL },
   { "defprt", DEFPRT },
   { "default_printer", DEFPRT },
   { "defform", DEFFORM },
   { "default_form", DEFFORM },
   { "defclass", DEFCLASS },
   { "default_class", DEFCLASS },
   { "ip_port",INET_ADDRESS },	  
   { "defmode", DEFMODE },
   { "default_mode", DEFMODE },
   { "wang", MODEWANG },
   { "unique", MODEUNIQUE },
   { "normal", MODEUNIQUE },
   { "priority", RUNPRI },
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

loadqconfig()
{
	extern int yyupchar;

	yylineno=0;
	memset(&qconfig,0,sizeof(qconfig));
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
	
	fileerrs |= CONFERR;
	
	sprintf(yyerrbuf,"Syntax error on %s line %d:\n",fileread,yylineno+1);
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
