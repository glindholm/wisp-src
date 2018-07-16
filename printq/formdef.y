%{

static char rcsid[]="$Id:$";

#define EXT extern 
#include "daemon.h"

FITEM yythe_form, *yythe_formp, *findfitem();
int yydummy;
int yyfdlineno;
char initbuf[INITSIZE];
char *yystrdup(),*yystrduplen();
%}

%union {
    int kw;
    int num;
    char *string;
}

%token <num> NUMBER
%token <num> EQUAL COLON
%token <string> VAR STUFF MODEL
%token <kw> LPP STOCK INIT FILTER BINARY

%%


file:	
	| file record	
	;

record: '\n'
        | name fields   	{
	extern char *fileread;
	
	yythe_form.time = filetime(fileread);
	yythe_formp = findfitem(yythe_form.form_name);
	if (!yythe_formp) 
	{
		FITEM *newitem;
		char *gmem();
		newitem=(FITEM*)gmem(sizeof(FITEM));
		memcpy(newitem,&yythe_form,sizeof(FITEM));
		addfitem(newitem);
	}
	else updatefitem(yythe_formp,&yythe_form);
	memset(&yythe_form,0,sizeof(yythe_form));
			}
	;

fields: 
	| fields field
	;

name:	VAR COLON 	{ 
        	strcpy(yythe_form.form_name,$1); free($1);
			}
	| NUMBER COLON  {
		sprintf(yythe_form.form_name,"%03d",$1);
	}
	;
field:	'\n'
	| LPP EQUAL NUMBER '\n' {
		yythe_form.lpp = $3;
		}
        | STOCK EQUAL STUFF '\n' {
		decode(yythe_form.stock,$3,&yydummy);
		free($3);
		}
        | INIT EQUAL STUFF '\n' {
		yythe_form.initlen = decode(initbuf,$3,&yydummy);
		yythe_form.initdata = yystrduplen(initbuf,yythe_form.initlen);
		yythe_form.idatatype=INITDSTR;
		free($3);
		}
        | INIT MODEL EQUAL STUFF '\n' {
		char *model, *cutparen(); /* cutparen is below */
		init_s *init_p, *findinititem(), *newinititem(); /* findinititem is in d_main */
		model=cutparen($2);
		init_p = findinititem(&yythe_form,model); /* get pointer to model or new if not found */
		if (!init_p) init_p=newinititem(&yythe_form,model);
		strcpy(init_p->model,model);
		init_p->initlen=decode(initbuf,$4,&yydummy);
		init_p->initdata=yystrduplen(initbuf,init_p->initlen);
		init_p->idatatype=INITDSTR;
		free($4); free($2);
	        }
        | INIT EQUAL VAR '\n' {
		yythe_form.initlen = decode(initbuf,$3,&yydummy);
		yythe_form.initdata = yystrduplen(initbuf,yythe_form.initlen);
		yythe_form.idatatype=INITDFILE;
		free($3);
		}
        | INIT MODEL EQUAL VAR '\n' {
		char *model, *cutparen(); /* cutparen is below */
		init_s *init_p, *findinititem(), *newinititem(); /* findinititem is in d_main */
		model=cutparen($2);
		init_p = findinititem(&yythe_form,model); /* get pointer to model or new if not found */
		if (!init_p) init_p=newinititem(&yythe_form,model);
		strcpy(init_p->model,model);
		init_p->initlen=decode(initbuf,$4,&yydummy);
		init_p->initdata=yystrduplen(initbuf,init_p->initlen);
		init_p->idatatype=INITDFILE;
		free($4); free($2);
	        }
        | FILTER EQUAL STUFF '\n' {
		decode(yythe_form.filterstr,$3,&yydummy);
		free($3);
		}
	| FILTER MODEL EQUAL STUFF '\n' {
		char *model, *cutparen(), *cutquote();  /*  cutquote() is in iprintcap.y  */
		filt_s *filt_p, *findfiltitem(), *newfiltitem();
		model = cutparen($2);
		filt_p = findfiltitem(&yythe_form, model);
		if (!filt_p) filt_p = newfiltitem(&yythe_form,model);
		strcpy(filt_p->model,model);
		filt_p->filter=yystrdup(cutquote($4));
		free($4); free($2);
		}
        | BINARY '\n' {
		yythe_form.binary = 1;
		}
        ;
%%

char *yystrdup(p)
char *p;
{
	return yystrduplen(p,strlen(p));
}
char *yystrduplen(p,l)
char *p;
int l;
{
#ifdef __STDC__
	char *tmp;
	void *malloc();
#else
	char *tmp,*malloc();
#endif
	tmp = (char*)malloc(l+1);
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
   { "lpp", LPP }, 
   { "linespp", LPP }, 
   { "stock", STOCK }, 
   { "init", INIT },
   { "filter", FILTER },
   { "binary", BINARY },
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

loadformdef()
{
	extern int yyupchar;

	yyfdlineno=0;
	
	memset(&yythe_form,0,sizeof(yythe_form));
	yyupchar = -1;
	yyparse();
}
static int yyerror()
{
	char yyerrbuf[200];
	extern char *fileread;
	
	sprintf(yyerrbuf,"syntax error on %s line %d\n",fileread,yyfdlineno);
	logerr(yyerrbuf,0);
}
int yywrap()
{ 
	return 1;
}
static char *cutparen(ptr)
char *ptr;
{
	char *p, *e, *strchr();
	static char buf[64];
	p=ptr;
	
	if (*p == '(') 
	{
		++p;
	}
	if (e=strchr(p,')'))
	{
		*e = (char)0;
	}
	strcpy(buf,p);
	return buf;
}





