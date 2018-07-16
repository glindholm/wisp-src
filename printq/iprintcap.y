%{

static char rcsid[]="$Id:$";

#define EXT extern 
#include "daemon.h"

/*******************************************************/
/* Code added to protect warn when user specifies two  */
/* printers with same dev                              */

#include "jlist.h" /* jock's list management functions */
LIST *pdevlst;
struct pdevstr 
{
	char name[32];
	char dev[200];
};
static struct pdevstr *newpdevstr();
	
/*******************************************************/

extern struct ipcapstruct printers[],ipcap,*pcapptr;
char *fileread;
int pnum_nospec = TRUE; /* true unless pnum has been specified */
int printer_count;
int filter_count;
struct fstruct
{
	char filter[128];
};
struct fstruct filters[MAXFILTERS];

int yypclineno=0;
char *yythe_printer;

%}

%union {
    int kw;
    int num;
    char *string;
}

%token <num> NUMBER
%token <num> EQUAL COLON
%token <string> VAR PATH STUFF
%token <kw> BR FO FF FQ LF LP MC PL PW RM RP SD FM SP NM OF CL PN MODEL TYPE DB SB PA

%%


file:	
	| file record	
	;

record: '\n'
        | name fields   	{
                                if (pnum_nospec) /* not specified, so set it to -1 */
                                  ipcap.prtnum = -1;
				memcpy(&printers[printer_count],&ipcap,sizeof(struct ipcapstruct));
				check_pdev(ipcap.printer,ipcap.device,ipcap.type);
				memcpy(printers[printer_count].filters,filters,sizeof(struct fstruct)*filter_count);
				memset(&ipcap,0,sizeof(struct ipcapstruct));
                                pnum_nospec=TRUE;
				++printer_count;
			}
	;

fields: 
	| fields field
	;

name:	VAR COLON 	{ 
				strcpy(ipcap.printer,$1); free($1); 
				filter_count=0;
			}
	;

field:	'\n'
	|	BR EQUAL NUMBER '\n' {
			ipcap.baud = $3;
		}
	|	CL EQUAL VAR '\n' {
			strncpy(ipcap.class,$3,26);
			free($3);
		}
	|	FF EQUAL VAR  '\n' {
			strncpy(ipcap.ffstr,$3,8);
			free($3);
		}
	|	LF EQUAL PATH  '\n' {
			strncpy(ipcap.logfile,$3,128);
			free($3);
		}
	|	LP EQUAL PATH '\n' {
			strncpy(ipcap.device,$3,128);
			free($3);
		}
	|	LP EQUAL STUFF '\n' {
			strncpy(ipcap.device,cutquote($3),128);
			free($3);
		}
	|	LP EQUAL VAR '\n' {
			strncpy(ipcap.device,$3,128);
			free($3);
		}
	|	PL EQUAL NUMBER '\n' {
			ipcap.pagelen = $3;
		}
	|	PW EQUAL NUMBER '\n' {
			ipcap.pagewid = $3;
		}
	|	SP '\n' { 
			ipcap.sizepri = TRUE;
		}
	|	RM EQUAL VAR '\n' {
			strncpy(ipcap.rmachine,$3,8);
			free($3);
		}
	|	RP EQUAL VAR '\n' {
			strncpy(ipcap.rprinter,$3,8);
			free($3);
		}
	|	SD EQUAL VAR '\n' {
			strncpy(ipcap.sdir,$3,128);
			free($3);
		}
	|	NM EQUAL VAR '\n' {
			strncpy(ipcap.printer,$3,PRTNAMEMAX-1);
			free($3);
		}
	|	MODEL EQUAL VAR '\n' {
			strncpy(ipcap.model,$3,MODNAMEMAX-1);
			free($3);
		}
	|	FM EQUAL VAR '\n' {
			strncpy(ipcap.form,$3,FORMNAMEMAX-1);
			free($3);
		}
	|	FM EQUAL NUMBER '\n' {
			sprintf(ipcap.form,"%03d",$3);
		}
	|	PN EQUAL NUMBER '\n' {
			ipcap.prtnum = $3;
                        pnum_nospec = FALSE;
		}
	|	TYPE EQUAL VAR '\n' {
                        strncpy(ipcap.type,$3,20);
		}
 	|       SB EQUAL NUMBER '\n' {
			ipcap.stopbits = $3;
		}
	|	DB EQUAL NUMBER '\n' {
			ipcap.databits = $3;
		}
	|	PA EQUAL VAR '\n' {
			lowerstring($3);
			if (!strncmp($3,"even",4))
			{
				ipcap.parity = PAR_EVEN;
			}
			else if (!strncmp($3,"odd",4))
			{
				ipcap.parity = PAR_ODD;
			}
			else
			  ipcap.parity = PAR_NONE;	
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
   { "br", BR }, 
   { "fo", FO },
   { "ff", FF },
   { "fq", FQ },
   { "lf", LF },
   { "lp", LP },
   { "mc", MC },
   { "of", OF },
   { "pl", PL },
   { "pw", PW },
   { "rm", RM },
   { "rp", RP },
   { "sd", SD },
   { "fm", FM },
   { "sp", SP },
   { "nm", NM },
   { "cl", CL },
   { "pn", PN },
   { "baud", BR },
   { "class", CL },
   { "ffopen", FO },
   { "ffstr", FF },
   { "ffquit", FQ }, 
   { "logfile", LF }, 
   { "device", LP }, 
   { "filter", OF }, 
   { "maxcopies", MC }, 
   { "pagelen", PL }, 
   { "pagewid", PW }, 
   { "rmachine", RM }, 
   { "rprinter", RP }, 
   { "spooldir", SD }, 
   { "form", FM }, 
   { "sizepri", SP },
   { "name", NM },
   { "printnum", PN },
   { "pnum", PN },
   { "model", MODEL },
   { "type", TYPE },
   { "databits", DB },
   { "stopbits", SB },
   { "parity", PA },
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

loadpcap()
{
	extern int yyupchar;

	pdevlst=NULL;
	yypclineno=0;
	memset(printers,0,sizeof(struct ipcapstruct) * (MAXPRTRS+1));
	memset(&ipcap,0,sizeof(struct ipcapstruct));
	printer_count=0;
	yyupchar = -1;
	yyparse();
	kill_list(&pdevlst,1);
}
static int yyerror()
{
	char yyerrbuf[200];
	extern char *fileread;
	
	sprintf(yyerrbuf,"syntax error on %s line %d\n",fileread,yypclineno);
	logerr(yyerrbuf,0);
	kill_list(&pdevlst,1);
}
int yywrap()
{ 
	return 1;
}
char *cutquote(ptr)
char *ptr;
{
	char *p, *e, *strchr();
	static char buf[64];
	p=ptr;
	
	if (*p == '"') 
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

static struct pdevstr *newpdevstr(name,dev)
char *name,*dev;
{
#ifdef __STDC__
	void *calloc();
#else
	char *calloc();
#endif
	struct pdevstr *tmp;
	
	tmp = (struct pdevstr *)calloc(1,sizeof(struct pdevstr));
	strcpy(tmp->name,name);
	strcpy(tmp->dev,dev);
	return tmp;
}
check_pdev(name,dev,type)
char *name,*dev;
char *type;
{
	LIST *ptr;
	char errmsg[200];

	if (pdevlst)
	for (ptr=pdevlst->next; ptr; ptr = ptr->next)
	{
		if (!strcmp(((struct pdevstr *)(ptr->data))->name,name))
		{ 
			sprintf(errmsg,"Warning: duplicate printer name (%s) found in prtdef file on line %d\n",
				name,yypclineno);				
			logerr(errmsg,0);
			logerr("    prior version will be superceded\n");
		}
		if (strcmp(type,PRT_TYPE_PROGRAM) && !strcmp(((struct pdevstr *)(ptr->data))->dev,dev))
		{
			sprintf(errmsg,"Warning: duplicate printer device/lp (%s) found in prtdef file on line %d\n",
				dev,yypclineno);				
			logerr(errmsg,0);
			logerr("    this may result in unpredictable behavior,\n",0);
			logerr("    including jobs printing simultaneously to the same printer\n",0);
		}
	}
	add_node(&pdevlst,newpdevstr(name,dev));
}
lowerstring(p)
char *p;
{	
	while (*p)
	{
		*p = tolower(*p);
		++p;
	}
}
