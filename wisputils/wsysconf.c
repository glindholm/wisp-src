static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#ifdef unix
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <ctype.h>
#include <termio.h>
#include <fcntl.h>

#include "wperson.h"
#define EXT_FILEXT
#include "filext.h"
#include "wdefines.h"
#include "wsysconf.h"

char *regex();
char *regcmp();
char *gmem();
static char *nextfile();

/*
** The following was in wsysconf.h
*/

int devnum;
int fcount;
char curnam[16];
char tmpold[64];
char tmpnew[64];
/*
**	NOTE: The devclass for DISKS was "dk" and the lines were
**	being generated without a devname field, using the volume
**	name as the devtype.
**
**	OLD:	200 dk VOL100
**	NEW:	200 dv disk VOL100
*/
static char *classes[] = { "ws", "lp", "mt", "dv", 0 };
static int   classvals[] = { 192, 97, 45, 50, 0 };
static char *classnames[] = { "workstation", "line printer", "magtape", "disk", 0 };


#define DEV_ENTRY struct devent
DEV_ENTRY {
	DEV_ENTRY *next;
	char specfile[64];
	int devnum;
	char *class;
	char *type;
};

DEV_ENTRY *devhead, *devp;


/*
**	**** NOTE ****	All the following xxNAME and xxPATH defines are Regular Expressions
*/

/*
**	WSPATH/WSNAME		Workstation path and name mask
**	WSPATH2/WSNAME2		Workstation path and name mask  (secondary)
**	DEFTTY			Default workstation type
*/

#ifdef ATT3B2
#define DEFTTY "att605"
#endif

#ifdef AIX
#define WSNAME2 "pty..*"
#define WSPATH2	"/dev"
#define DEFTTY "ibm3151"
#endif

#ifdef ULTRIX
#define WSNAME2	"pty..*"
#define WSPATH2	"/dev"
#define DEFTTY	"vt220"
#endif

#ifdef OSF1_ALPHA
#define WSNAME2	"pty..*"
#define WSPATH2	"/dev"
#define DEFTTY	"vt220"
#endif

#ifdef SCO
#define WSNAME2 "pty..*"
#define WSPATH2 "/dev"
#endif

#ifdef NCR486
#define WSNAME2 "..."
#define WSPATH2 "/dev/nty"
#endif

#ifdef HPUX
#define WSNAME2 "tty..*"
#define WSPATH2 "/dev/pty"
#endif

#ifdef MSDOS
#define DEFTTY "ansidos"
#endif

#ifdef ICL
#define WSNAME  ".*"
#define WSPATH  "/dev/pts"
#endif

#ifdef UNISYS
#define WSNAME	".*"
#define WSPATH	"/dev/pts"
#define WSNAME2	".*"
#define WSPATH2	"/dev/term"
#endif

#ifdef SOLARIS
#define WSNAME2 ".*"
#define WSPATH2 "/dev/pts"
#endif

#ifdef MOTOROLA
#define WSNAME2 "pty..*"
#define WSPATH2	"/dev"
#endif

#ifndef WSNAME
#define WSNAME "tty..*"
#endif
#ifndef WSPATH
#define WSPATH "/dev"
#endif
#ifndef DEFTTY
#define DEFTTY	"ansi"
#endif


/*
**	MTPATH/MTNAME	Magtape path and name mask
**	DEFMAGTAPE	The default type of mag tape.
*/

#ifdef AIX
#define DEFMAGTAPE "8mm"
#endif

#ifdef ULTRIX 
#define DEFMAGTAPE "CompacTape"
#endif

#ifdef OSF1_ALPHA 
#define DEFMAGTAPE "4mmDat"
#endif

#ifdef HPUX
#define MTNAME "[0-9].*"
#define MTPATH "/dev/rmt"
#define DEFMAGTAPE "4mmDat"
#endif

#ifdef ATT3B2
#define MTNAME "ctape.*"
#define MTPATH "/dev/rSA"
#endif

#ifdef DGUX
#define MTNAME "[0-9].*"
#define MTPATH "/dev/rmt"
#endif

#ifdef ICL
#define MTNAME "[0-9].*"
#define MTPATH "/dev/rmt"
#endif

#ifdef NCR486
#define MTNAME "c0*"
#define MTPATH "/dev/rmt"
#endif

#ifndef MTNAME
#define MTNAME "rmt[0-9].*"
#endif
#ifndef MTPATH
#define MTPATH "/dev"
#endif
#ifndef DEFMAGTAPE
#define DEFMAGTAPE "Cartridge"
#endif


char cfgdir[100];

#define CFGDIR cfgdir

#ifndef MSDOS									/* struct termio is NOT defined for MSDOS	*/
struct termio ttyraw,ttynorm;
#endif

int prompt;

char devtype[10];

/*
** End of wsysconf.h stuff
*/

static int	autoconfig;							/* Use automatic configuring			*/

/*
 *	KLUDGE Warning: There is a problem if more than 1000 devices are found.  Cobol expects only 3 digits. (0-255)
 *
 *	If devnum reaches DEVNUM_MAX, devnum is reset to DEVNUM_RESET.  There are potential problems if
 *	multiple devices have the same number, but the odds are that will not happen...  Hopefully anyway.
 */

static int	DEVNUM_MAX	= 254;
static int	DEVNUM_RESET 	= 100;

main(argc,argv)
int argc;
char *argv[];
{
	FILE *cfg;
	char cfgpath[256];
	char cfgpath_new[256];
	char cfgpath_old[256];
	char buff[256];

	process_args(argc,argv);
	init_stuff();

	printf("\n\n");
	printf("        *** WISP SYSTEM CONFIGURATION TOOL ***\n");
	printf("Copyright (c) 1992-1996 NeoMedia Migrations Incorporated\n\n");
	printf("This program will create the file \"$WISPCONFIG/wsysconfig\".\n");
	printf("It contains hardware and logical volume configuration information.\n");
	printf("Wsysconf will prompt you for certain configuation information as it\n");
	printf("runs.  See the WISP Manual for further explanations.\n\n\n");

	if (getenv("WISPCONFIG")) 
	{
		strcpy(cfgdir,getenv("WISPCONFIG"));
	}
	else
	{
		printf("%%WSYSCONF-F-WISPCONFIG WISPCONFIG is undefined. \n");
		exit(0);
	}

	sprintf(cfgpath,"%s/%s",CFGDIR,CFGFNAME);
	sprintf(cfgpath_new,"%s/%s.New",CFGDIR,CFGFNAME);
	sprintf(cfgpath_old,"%s/%s.Old",CFGDIR,CFGFNAME);

	if (0==access(cfgpath,00))
	{
		printf("Running this program will replace the previous version of\n");
		printf("file %s .\n\n",cfgpath);

		if (!getyn("Do you wish to continue (y/n) : "))	
		{
			printf("No modifications have been made.\n");
			exit(0);
		}
	}

	if (getyn("Use automatic configuring (y/n) : ")) autoconfig=TRUE;
	else autoconfig=FALSE;

	devnum=0;
	DEVNUM_MAX=199;
	DEVNUM_RESET=100;

	build_ttys();									/* Terminals 0-199			*/

	devnum=200;
	DEVNUM_MAX=254;
	DEVNUM_RESET=200;								/* Devices 200-254			*/

	build_tapes();
	build_disks();
	build_lprs();	

	cfg=fopen(cfgpath_new,"w");
	if (!cfg)
	{
		sprintf(buff,"Unable to create file %s",cfgpath_new);
		panic(buff);
	}

	fprintf(cfg,"#   %s - WISP SYSTEM CONFIGURATION\n#\n",CFGFNAME);
	fprintf(cfg,"#   DEVICE NUMBER MAP:\n#\n");
	fprintf(cfg,"#Num Class Type Name\n");
	for (devp=devhead, fcount=0; devp; devp=devp->next)
	{
		++fcount;
		if (strlen(devp->specfile))
		{
			if (0==strcmp("dv",devp->class) || 0==access(devp->specfile,0))
			{
				/*
				**	If the specfile is found or we have a disk volume then write it.
				**	(For dv we use the volume name which isn't a special file.)
				*/
				fprintf(cfg,RECFMT,devp->devnum,devp->class,devp->type,devp->specfile);
				
			}
			else
			{
				fprintf(cfg,RECFMT,devp->devnum,devp->class,devp->type,"");
			}
		}
		else
		{
				fprintf(cfg,RECFMT,devp->devnum,devp->class,devp->type,"");
		}
	}
	fprintf(cfg,"#\n#   DEVICE TYPE MAP:\n#\n");
	fclose(cfg);
	build_dtyp_list();

	if (0==access(cfgpath,00))							/* If wsysconfig exists			*/
	{
		if (0==access(cfgpath_old,00))						/* If wsysconfig.Old exists		*/
		{
			if (0 != unlink(cfgpath_old))					/* Remove wsysconfig.Old		*/
			{
				fprintf(stderr,"Unable to delete file %s\n",cfgpath_old);
			}
		}

		if (0 != link(cfgpath,cfgpath_old))					/* move wsysconfig to old		*/
		{
			fprintf(stderr,"Unable to move file %s to %s\n",cfgpath,cfgpath_old);
		}

		if ( 0 != unlink(cfgpath) )						/* remove wsysconfig			*/
		{
			fprintf(stderr,"Unable to remove existing file %s\n\n");
			fprintf(stderr,"wsysconfig has NOT been updated!!\n");
			fprintf(stderr,"wsysconf terminating ...\n");
			exit(3);
		}
	}

	if (0 != link(cfgpath_new,cfgpath))						/* move new to wsysconfig		*/
	{
		fprintf(stderr,"Unable to move file %s to %s\n",cfgpath_new,cfgpath);
		fprintf(stderr,"wsysconfig has NOT been updated!!\n");
		fprintf(stderr,"wsysconf terminating ...\n");
		exit(4);
	}

	unlink(cfgpath_new);								/* Remove new				*/

	printf("wsysconf has successfully created %s .\n",cfgpath);
	exit(0);
}

build_ttys()
{
	char *devclass = "ws";

	ask(classnames[0],DEFTTY);

#ifdef AIX
	build_list("/dev","hft",CFGDIR,devclass,devtype);
#endif
	build_list("/dev","console",CFGDIR,devclass,devtype);
	build_list("/dev","contty",CFGDIR,devclass,devtype);

	build_list(WSPATH,WSNAME,CFGDIR,devclass,devtype);
#ifdef WSNAME2
	build_list(WSPATH2,WSNAME2,CFGDIR,devclass,devtype);
#endif
}

build_tapes()
{
	char *devclass = "mt";

	ask(classnames[2],DEFMAGTAPE);

	build_list(MTPATH,MTNAME,CFGDIR,devclass,devtype);
}

build_list(srcpath,srcpat,cfgdir,devclass,devtype)
char *srcpath;										/* where to look for special files 	*/
char *srcpat;										/* pattern to match special file names	*/
char *cfgdir;										/* our config dir			*/
char *devclass;										/* class (tty/printer/disk/etc)		*/
char *devtype;										/* type (att605/oki92/etc)		*/
{
	DIR *cur;
	char *filename, *nextfile();
	char dtyp[10];
	char *srcname;

	cur = NULL;

	srcname = regcmp(srcpat,NULL);
	while (filename=nextfile(srcpath,&cur,srcname)) 
	{
		sprintf(tmpold,"%s/%s",srcpath,filename);
		if (prompt)
		{
			printf("Enter type for device %s [%s] : ",filename,devtype);
			fgets(dtyp,9,stdin);
			dtyp[strlen(dtyp)-1] = (char)0;
			if (strlen(dtyp)==0) strcpy(dtyp,devtype);
		}
		else
			strcpy(dtyp,devtype);
		sprintf(tmpnew,cfgdir,curnam);
		add(tmpold,devnum,devclass,dtyp);
		increment_devnum();
	}
	free(srcname);
}

build_disks()										/* special case */
{
	char *devclass = "dv";
	logical_id *p;

	for (p=get_logical_list(); p; p=(logical_id *)p->next)
	{
		if (strcmp(p->logical,"."))
		{
			add(p->logical,devnum,devclass,"disk");
			increment_devnum();
		}
		else 
		  continue;
	}
}

build_lprs()										/* also a special case */
{
	char *tmp, *p, *e;
	FILE *tmpfile;
	char buf[100];
	char lpclass[16];
	char lpid[64];
	char *devclass = "lp";

#if !defined(AIX) && !defined(ULTRIX) && !defined(sun) && !defined(OSF1_ALPHA)
	memset(lpclass,(char)0,sizeof(lpclass));
	memset(lpid,(char)0,sizeof(lpid));

	tmp = tmpnam(NULL);
	sprintf(buf,"lpstat -c >%s",tmp);
	system(buf);
			
	tmpfile = fopen(tmp,"r");
	while (fgets(buf,sizeof(buf),tmpfile))
	{
		if (!strncmp(buf,"members",7))
		{
			e=strrchr(buf,':');
			*e = (char)0;
			p=strrchr(buf,' ')+1;
			strcpy(lpclass,p);
		}
		else
		{
			if (buf[0] != '\t') panic("reading lpstat output");	
			
			p = buf+1;
			strcpy(lpid,p);
			sprintf(tmpnew,CFGDIR,curnam);
			add(lpid,devnum,devclass,lpclass);
			increment_devnum();
		}
	}
	unlink(tmp);
#endif
}

add(specfile,num,class,type)
char *specfile, *class, *type;
int num;
{

	if (devhead==NULL)
	{
		devp = devhead = (DEV_ENTRY *)gmem(1,sizeof(DEV_ENTRY));
	}
	else
	{
		devp->next = (DEV_ENTRY *)gmem(1,sizeof(DEV_ENTRY));	
		devp = devp->next;
	}
	if (specfile) strcpy(devp->specfile,specfile);
	else devp->specfile[0] = (char)0;
#ifdef AIX
	if (!strncmp(class,"ws",2))
	  ttyxlat(devp->specfile);
#endif
	devp->devnum=num;
	devp->class=class;
	devp->type=gmem(strlen(type)+1,sizeof(char));
	strcpy(devp->type,type);
}

process_args(argc,argv)
int argc;
char *argv[];
{
										                /* no possible args yet 	*/
}

char *gmem(nelem,size)
unsigned nelem, size;
{
	static long	total = 0;
	static char *tmp;

	total += nelem*size;

	tmp = calloc(nelem,size);
	if (tmp==NULL)
	{
		fprintf(stderr,"wsysconf: Process out of memory. [total=%dl]\n",total);
		exit(2);
	}
	return(tmp);
}

static char *nextfile(path,curdir,pattern)
char *path;
DIR **curdir;
char *pattern;
{
	struct dirent *dp;

	if ( ! *curdir )
	{
		if (!(*curdir = opendir(path))) 
		{
			return(NULL);
		}
		dp=readdir(*curdir);						/* read entry for . */
		dp=readdir(*curdir);						/* read entry for .. */
	}
	for (;;)
	{
		if ((dp=readdir(*curdir))==NULL)
		{
			closedir(*curdir);
			return NULL;
		}	
		if (regex(pattern,dp->d_name)) return dp->d_name; 
	}
}
getyn(pr)
char *pr;
{
	char ch;

	printf(pr);
	rawmode();
	do
	{
		ch = getchar();	
	} while (!strchr("YNyn",ch));
	norawmode();
	printf("\n");
	return ch=='Y'||ch=='y'?TRUE:FALSE;
}
rawmode()
{
	ioctl(0,TCSETA,&ttyraw);
}
norawmode()
{
	ioctl(0,TCSETA,&ttynorm);
}
init_stuff()
{
	ioctl(0,TCGETA,&ttynorm);
	ioctl(0,TCGETA,&ttyraw);
	ttyraw.c_lflag &= ~ICANON;
	ttyraw.c_cc[4] = 1;
	ttyraw.c_cc[5] = 10;
}
ask(dtyp,def)
char *dtyp, *def;
{
	char pr[64];

	if (autoconfig)
	{
		strcpy(devtype,def);
		prompt=FALSE;
	}
	else
	{
		printf("\nBuilding %s list...\nEnter default %s type [%s] : ",dtyp,dtyp,def);
		gets(devtype);
		if (strlen(devtype)==0) strcpy(devtype,def);
		sprintf(pr,"prompt for each %s type? (y/n) : ",dtyp);
		if (getyn(pr)) prompt=TRUE;
		else prompt=FALSE;
	}
}
panic(p)
char *p;
{
	fprintf(stderr,"fatal error: %s\n",p);
	exit(2);
}
build_dtyp_list()
{
	FILE *dtyp;
	char dtnam[256], *fname;
        char *cpat,*spc;
	char *getclass();
	DIR *cur;
	char **typlst;
	int i=0;
	int (*fn)(), cmpare();
	char last[16];
	char buf[256];
	
	if (!autoconfig)
	{
		printf("\nBuilding device type table...\n");
	}

	cur = NULL;
	fn = cmpare;
	typlst=(char **)gmem(fcount,sizeof(char *));

	sprintf(dtnam,"%s/%s.New",CFGDIR,CFGFNAME);
	dtyp=fopen(dtnam,"r");
	if (!dtyp)
	{
		sprintf(buf,"Unable to open file %s",dtnam);
		panic(buf);
	}

	while (fgets(buf,sizeof(buf),dtyp))
	{
		int	rc;
		int	dev_num;
		char	dev_class[10];
		char	dev_type[20];
		char	dev_name[80];

		if (buf[0] == '#') continue;						/* Skip over comments			*/
		rc = sscanf(buf,"%d %2s %20s %80s",&dev_num, dev_class, dev_type, dev_name);
		if (rc < 4) dev_name[0] = (char)0;
		if (rc < 3) dev_type[0] = (char)0;
		if (rc < 2) continue;							/* Skip Badly formed line.		*/

		*(typlst+i) = gmem(sizeof(dev_class)+sizeof(dev_type)+2,sizeof(char));
		strcpy(*(typlst+i),dev_class);						/* Copy in the type "ws" "mt"		*/
		strcat(*(typlst+i)," ");
		strcat(*(typlst+i),dev_type);
		++i;
	}
	fclose(dtyp);
	qsort((char *)typlst,fcount,sizeof(char *),fn);

	dtyp = fopen(dtnam,"a");
	if (!dtyp) 
	{
		sprintf(buf,"Unable to append file %s",dtnam);
		panic(buf);
	}

	for (i=0, memset(last,(char)0,sizeof(last)); i<fcount; ++i)
	{
		int type;

		if (strcmp(last,*(typlst+i)))
		{
			int k;
			type = 0;
			for (k=0; classes[k]; ++k)
			{
				if (!strncmp(classes[k],*(typlst+i),2))
				{
					type = classvals[k];
				}
			}

			if (!autoconfig)
			{
				printf("Enter (integer) device type for %s type \"%s\" [%d] : ",
					getclass(*(typlst+i)),*(typlst+i)+3,type);
				gets(buf);
				if (strlen(buf)!=0)
				{
					sscanf(buf,"%d",&type);
				}
			}

			fprintf(dtyp,"%s=%d\n",*(typlst+i)+3,type);
			strcpy(last,*(typlst+i));
		}
	}
	fclose(dtyp);
}
char *getclass(p)
char *p;
{
	int i;

	for (i=0; classes[i]; ++i)
	{
		if (!strncmp(classes[i],p,2)) return classnames[i];
	}
	return 0;
}
cmpare(s1,s2)
char **s1, **s2;
{
	return strcmp(*s1,*s2);
}
#ifdef AIX
ttyxlat(p)										/* kludge to xlate aix ttyp? and ptyp? */
char *p;										/* to ptc/? and pts/? */
{
	static char tmpbuf[100];
	char *ptr;
	
	ptr=strrchr(p,'/');
	strcpy(tmpbuf,++ptr);
	switch (tmpbuf[0])
	{
	      case 'p':
		sprintf(ptr,"ptc/%d",hextoi(&tmpbuf[4]));
		break;
	      case 't':
		if (tmpbuf[3]=='p')
		  sprintf(ptr,"pts/%d",hextoi(&tmpbuf[4]));
		break;
	}
}
hextoi(p)
char *p;
{
	int res;

	for (res=0; *p && isxdigit(*p); ++p)
	{
		res *= 16;
		res += isdigit(*p)?*p-0x30:toupper(*p)-'A'+10;
	}
	return res;
}
#endif	/* AIX	*/

increment_devnum()
{
	if( devnum < DEVNUM_MAX )
	{
		++devnum;
	}
	else
	{
		devnum = DEVNUM_RESET;
	}
}

#include "wutils.h"

#endif
/*
**	History:
**	$Log: wsysconf.c,v $
**	Revision 1.12  1998-01-19 15:45:45-05  gsl
**	Fix autoconfig for SOLARIS
**	bye
**
**	Revision 1.11  1996-12-12 13:16:26-05  gsl
**	Devtech -> NeoMedia
**
**	Revision 1.10  1996-10-08 17:49:29-07  gsl
**	fix getenv()'s
**
**	Revision 1.9  1996-07-23 11:13:12-07  gsl
**	drcs update
**
**
**
*/
