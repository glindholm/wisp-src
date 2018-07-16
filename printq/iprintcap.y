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
	char name[PRTNAMEMAX];
	char dev[PIPEMAX];
};
static struct pdevstr *newpdevstr();
	
/*******************************************************/

#define YYDEBUG 1
extern struct ipcapstruct printers[],ipcap,*pcapptr;
char *fileread;
int pnum_nospec = TRUE; /* true unless pnum has been specified */
int printer_count;
int filter_count;
struct fstruct
{
	char filter[PIPEMAX];
};
struct fstruct filters[MAXFILTERS];

int yypclineno=0;
char *yythe_printer;

extern char yylbuff[];
extern int yylposs;
extern int yynewlnn;

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
%token <kw> IN ST FI PAR SER

%%


file:	
	| file record	
	;

record: '\n'
        | name fields   	{
                                PITEM *findpitem(), *pptr;

                                if (pnum_nospec) /* not specified, so set it to -1 */
                                  ipcap.prtnum = -1;
				check_pdev(ipcap.printer,ipcap.device,ipcap.type);
                                if ((pptr=findpitem(ipcap.printer)))
                                {
					pptr->baud = ipcap.baud;
					pptr->databits = ipcap.databits;
					pptr->stopbits = ipcap.stopbits;
					pptr->parity = ipcap.parity;
					pptr->prtnum = ipcap.prtnum;
					strcpy(pptr->printer_dev,ipcap.device);
					strcpy(pptr->default_form,ipcap.form);
					strcpy(pptr->printer_model,ipcap.model);
					if (ipcap.sizepri) pptr->mode |= SIZEPRI;
					else pptr->mode &= ~SIZEPRI;
					pptr->mode |= ipcap.devtype;
					if (strlen(ipcap.type)==0) pptr->mode |= PMD_DEVICE;
					else
					{
						if (strcmp(ipcap.type,PRT_TYPE_DEVICE)==0)
						{
							pptr->mode |= PMD_DEVICE;
						}
						else if(strcmp(ipcap.type,PRT_TYPE_PROGRAM)==0) 
						{
							pptr->mode |= PMD_PROGRAM;
							pptr->mode |= 
							  ((ipcap.inputtype==PMD_PROGFILE) ? ipcap.inputtype : PMD_PROGSTDIN);	
						}
						else
						{
							pptr->mode &= ~PMD_PROGRAM;
						}
					}
					if (pptr->unspooler)
					{
						tag_unsp_obsolete(pptr);
					}
				}
				else
				{
					addprtr(&ipcap);
				}
				memset(&ipcap,0,sizeof(struct ipcapstruct));
                                pnum_nospec=TRUE;
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
                        upperstring($3);
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
			strncpy(ipcap.device,$3,PIPEMAX);
			free($3);
		}
	|	LP EQUAL STUFF '\n' {
			strncpy(ipcap.device,cutquote($3),PIPEMAX);
			free($3);
		}
	|	LP EQUAL VAR '\n' {
			strncpy(ipcap.device,$3,PIPEMAX);
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
	|       IN EQUAL ST '\n' {
			ipcap.inputtype = PMD_PROGSTDIN;
		}
	|       IN EQUAL FI '\n' {
			ipcap.inputtype = PMD_PROGFILE;
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
        |       PAR '\n'  {
                        ipcap.devtype = PMD_PARALLEL;
	        }
	|       SER '\n'  {
                        ipcap.devtype = 0;
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
	tmp=(char*)gmem(l+1,1);
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
   { "input", IN }, 
   { "stdin", ST }, 
   { "file", FI }, 
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
   { "parallel", PAR },
   { "serial", SER },
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
loadpcap()
{
	extern int yyupchar;

	pdevlst=NULL;
	yypclineno=0;
#ifdef OLD
	memset(printers,0,sizeof(struct ipcapstruct) * (MAXPRTRS+1));
#endif
	memset(&ipcap,0,sizeof(struct ipcapstruct));
	printer_count=0;
	yyupchar = -1;
	yyparse();
	kill_list(&pdevlst,1);
	yyfirst=FALSE;
}
static int yyerror()
{
	char yyerrbuf[200];
	extern char *fileread;
	char *repchar();
	extern int fileerrs;

	fileerrs |= PRTDEFERR;

	sprintf(yyerrbuf,"Syntax error on %s line %d:\n",fileread,yypclineno+1);
	logerr(yyerrbuf,0);

	if (yyfirst)
	{
		fprintf(stderr,yyerrbuf);
		fflush(stderr);
	}
	yylbuff[yylposs]='\0';
	sprintf(yyerrbuf,"%s\n",yylbuff);
	logerr(yyerrbuf,0);
	if (yyfirst)
	{
		fprintf(stderr,yyerrbuf);
		fflush(stderr);
	}
	sprintf(yyerrbuf,"%s^ error near this position\n",repchar(yylposs,(int)' '));

	logerr(yyerrbuf,0);
	if (yyfirst)
	{
		fprintf(stderr,yyerrbuf);
		fflush(stderr);
	}

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
	char *gmem();

	struct pdevstr *tmp;
	
	tmp = (struct pdevstr *)gmem(1,sizeof(struct pdevstr));
	strcpy(tmp->name,name);
	strcpy(tmp->dev,dev);
	return tmp;
}
check_pdev(name,dev,type)
char *name,*dev;
char *type;
{
	LIST *ptr;

	if (pdevlst)
	for (ptr=pdevlst->next; ptr; ptr = ptr->next)
	{
		if (!strcmp(((struct pdevstr *)(ptr->data))->name,name))
		{ 
			sprintf(errmsg,"Warning: duplicate printer name (%s) found in prtdef file on line %d\n",
				name,yypclineno);				
			logerr(errmsg,0);
			logerr("    prior version will be superceded\n",0);
		}
		if (strcmp(type,PRT_TYPE_PROGRAM) && !strcmp(((struct pdevstr *)(ptr->data))->dev,dev) &&
		    strcmp(((struct pdevstr *)(ptr->data))->dev,"/dev/null"))
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
upperstring(p)
char *p;
{	
	while (*p)
	{
		*p = toupper(*p);
		++p;
	}
}
