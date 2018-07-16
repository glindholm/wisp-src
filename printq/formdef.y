
%{

static char rcsid[]="$Id:$";

#define EXT extern 
#include "daemon.h"

FITEM yythe_form, *yythe_formp, *findfitem();
int yydummy;
int yyfdlineno;
char initbuf[INITSIZE];
char *yystrdup(),*yystrduplen();
extern char yylbuf[];
extern int yylpos;
extern int yynewln;
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
#ifdef DEBUGCODE
	D(DBGRELOADFORM)
	{
		sprintf(errmsg,"   NEW ==> %d  Form[%s]  stock[%s]  init[%s]  filt[%s]\n",
			yythe_form.time,
			yythe_form.form_name,
			yythe_form.stock,
			yythe_form.initdata,
			yythe_form.filterstr);
		logerr(errmsg,0);
		if (yythe_form.p_init)
		{
			sprintf(errmsg,"               1st init[%s/%s]\n",yythe_form.p_init->model,yythe_form.p_init->initdata);
			logerr(errmsg,0);
		}
	}
#endif
	yythe_formp = findfitem(yythe_form.form_name);
#ifdef DEBUGCODE
	D(DBGRELOADFORM)
	{
		if (yythe_formp)
		{
			sprintf(errmsg,"   OLD ==> %d  Form[%s]  stock[%s]  init[%s]  filt[%s]\n",
				yythe_formp->time,
				yythe_formp->form_name,
				yythe_formp->stock,
				yythe_formp->initdata,
				yythe_formp->filterstr);
			logerr(errmsg,0);
			if (yythe_formp->p_init)
			{
				sprintf(errmsg,"               1st init[%s/%s]\n",yythe_formp->p_init->model,yythe_formp->p_init->initdata);
				logerr(errmsg,0);
			}
		}
	}
#endif
	if (!yythe_formp) 
	{
		FITEM *newitem;
		char *gmem();
		newitem=(FITEM*)gmem(1,sizeof(FITEM));
		memcpy(newitem,&yythe_form,sizeof(FITEM));
		addfitem(newitem);
#ifdef DEBUGCODE
		D(DBGRELOADFORM)
		{
			logerr("    Form added\n",0);
		}
#endif
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
	char *tmp;
	char *gmem();
	
	tmp = (char*)gmem(l+1,1);
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

static int yyfirst=TRUE;

loadformdef()
{
	extern int yyupchar;

	yyfdlineno=0;
	
	memset(&yythe_form,0,sizeof(yythe_form));
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
	
	fileerrs |= FORMDEFERR;
	
	sprintf(yyerrbuf,"Syntax error on %s line %d:\n",fileread,yyfdlineno+1);
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





