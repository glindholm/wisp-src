%{

static char rcsid[]="$Id:$";

#define EXT extern 
#include "defs.h"
#include "manage.h"
#include "qpaths.h"

int yylineno;

%}

%union {
    int kw;
    int num;
    char *string;
}

%token <num> EQUAL COMMA LETTER NUMBER
%token <string>  VAR STUFF
%token <kw> DELKEY REMKEY ENDISKEY EDKEY QPKEY OTHERKEY EXITKEY HOLDKEY 
%token <kw> ALIGNKEY TOPKEY BOTTOMKEY PGDNKEY PGUPKEY DISPKEY STOPKEY ALLKEY 

%%


file:	
	| file fields
	;

fields:	'\n'
        | DELKEY   EQUAL VAR COMMA STUFF '\n'
                {
			read_keydef(&keydefs[DELKEY_DEF],$3,$5);
                }
        | REMKEY EQUAL VAR COMMA STUFF '\n'
                {
			read_keydef(&keydefs[REMKEY_DEF],$3,$5);
                }
        | ENDISKEY EQUAL VAR COMMA STUFF '\n'
                {
			read_keydef(&keydefs[ENDISKEY_DEF],$3,$5);
                }
        | EDKEY  EQUAL VAR COMMA STUFF '\n'
                {
			read_keydef(&keydefs[EDKEY_DEF],$3,$5);
                }
        | QPKEY  EQUAL VAR COMMA STUFF '\n'
                {
			read_keydef(&keydefs[QPKEY_DEF],$3,$5);
                }
        | OTHERKEY  EQUAL VAR COMMA STUFF '\n'
                {
			read_keydef(&keydefs[OTHERKEY_DEF],$3,$5);
                }
        | EXITKEY  EQUAL VAR COMMA STUFF '\n'
                {
			read_keydef(&keydefs[EXITKEY_DEF],$3,$5);
                }
        | HOLDKEY  EQUAL VAR COMMA STUFF '\n'
                {
			read_keydef(&keydefs[HOLDKEY_DEF],$3,$5);
                }
        | ALIGNKEY EQUAL VAR COMMA STUFF '\n'
                {
			read_keydef(&keydefs[ALIGNKEY_DEF],$3,$5);
                }
        | TOPKEY  EQUAL VAR COMMA STUFF '\n'
                {
			read_keydef(&keydefs[TOPKEY_DEF],$3,$5);
                }
        | BOTTOMKEY  EQUAL VAR COMMA STUFF '\n'
                {
			read_keydef(&keydefs[BOTTOMKEY_DEF],$3,$5);
                }
        | PGDNKEY  EQUAL VAR COMMA STUFF '\n'
                {
			read_keydef(&keydefs[PGDNKEY_DEF],$3,$5);
                }
        | PGUPKEY  EQUAL VAR COMMA STUFF '\n'
                {
			read_keydef(&keydefs[PGUPKEY_DEF],$3,$5);
                }
        | DISPKEY  EQUAL VAR COMMA STUFF '\n'
                {
			read_keydef(&keydefs[DISPKEY_DEF],$3,$5);
                }
        | STOPKEY  EQUAL VAR COMMA STUFF '\n'
                {
			read_keydef(&keydefs[STOPKEY_DEF],$3,$5);
                }
        | ALLKEY  EQUAL VAR COMMA STUFF '\n'
                {
			read_keydef(&keydefs[ALLKEY_DEF],$3,$5);
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
   { "delete_item", DELKEY },
   { "remove_item", REMKEY },
   { "enable_disable", ENDISKEY },
   { "edit_item", EDKEY },
   { "queue_printer", QPKEY },
   { "other_info", OTHERKEY },
   { "exit_key", EXITKEY },
   { "hold_release", HOLDKEY },
   { "align", ALIGNKEY },
   { "top", TOPKEY },
   { "bottom", BOTTOMKEY },
   { "page_down", PGDNKEY },
   { "page_up", PGUPKEY },
   { "display_item", DISPKEY },
   { "stop_start", STOPKEY },
   { "all_mine", ALLKEY },
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

loadkeys()
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
	
	sprintf(yyerrbuf,"syntax error on %s line %d\n",fileread,yylineno+1);
	fprintf(stderr,yyerrbuf,0);
}
int yywrap()
{ 
	return 1;
}
read_keydef(kp,value,label)
struct manage_key_defs *kp;
char *value;
char *label;
{
	char *llabel,*cutquote();
	char *scanidx;
	int gotnum;
	
	yylower(value);
	if (strncmp(value,"esc",3))
	{
		kp->esc_or_fn = KEY_ESC;
	}
	else if (value[0]=='f')
	{
		kp->esc_or_fn = KEY_FN;
	}
	else
	{
		kp->esc_or_fn = KEY_ESC;
	}
	llabel = cutquote(label);
	strcpy(kp->label,llabel);
	for (gotnum=FALSE,scanidx=value; ;++scanidx)
	{
		if (*scanidx >= '0' && *scanidx <='9')
		{
			++gotnum;
			break;
		}
	}
	if (gotnum)
	{
		kp->keydef = atoi(scanidx);
	}
	free(label);
	free(value);
}
yylower(p)
char *p;
{
	while (*p)
	{
		*p = tolower(*p);
		++p;
	}
}
static char *cutquote(ptr)
char *ptr;
{
	char *p, *e, *strchr();
	static char buf[64];
	p=ptr;
	
	if (*p == '\"') 
	{
		++p;
	}
	if (e=strchr(p,'"'))
	{
		*e = (char)0;
	}
	strcpy(buf,p);
	return buf;
}

