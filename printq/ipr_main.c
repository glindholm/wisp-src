/*
 * Module:  ipr_main
 * Program: IDSIprint ipr
 *
 * $Log: ipr_main.c,v $
 * Revision 1.16  1992/07/30  22:25:11  jockc
 * added quiet flag and spool of stdin if no files specified
 *
 * Revision 1.15  1992/04/30  19:22:39  jockc
 * cut page compute
 *
 * Revision 1.14  1992/04/30  19:10:04  jockc
 * get file size before sending packet
 *
 * Revision 1.13  1992/04/30  18:57:06  jockc
 * back to pnum=0, change for make_vers_date(), remove
 * code to count lines (moved to screen manage area)
 * init class array to 0
 *
 * Revision 1.12  1992/03/27  23:27:26  jockc
 * took out stupid reference to the old forms layout
 *
 * Revision 1.11  1992/03/27  23:12:24  jockc
 * changed to pass in printer number -1, create src
 * list to support multiple file printing
 *
 * Revision 1.10  1991/10/14  18:48:00  jockc
 * fixup usage stuff
 *
 * Revision 1.9  1991/10/11  00:32:04  jockc
 * add size to send_daemon call
 *
 * Revision 1.8  1991/10/01  17:09:09  jockc
 * support new comm scheme, changed version printing stuff
 *
 * Revision 1.7  1991/08/03  00:04:39  jockc
 * fixed crash on no arg
 *
 * Revision 1.6  1991/05/22  17:07:49  jockc
 * added a check of the ret code from init_tcp in case of failure
 *
 * Revision 1.5  1991/05/10  21:55:53  jockc
 * changed flags to be more like lp/lpr
 *
 * Revision 1.4  1991/04/25  21:26:34  jockc
 * receive qid back from daemon and print it
 *
 * Revision 1.3  1991/04/19  00:47:27  jockc
 * *** empty log message ***
 *
 * Revision 1.2  1991/04/19  00:22:40  jockc
 * fixed version stuff
 *
 * Revision 1.1  1991/04/18  23:50:26  jockc
 * Initial revision
 *
 *
 */
static char copyright[] = "Copyright 1991 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

static char VERSION[5];
static char MODDATE[20];

#include <stdio.h>
#include <sys/types.h>
#include <signal.h>
#include "defs.h"

#include "c_comm.h"

#define OPTSTR "d:p:r:f:n:C:P:M:h?q"

char printer[64];
char form[16];
int stpg=1,endpg=99999;
char *srcfiles[200];
int scnt=0;
int pri=0,copies=1;
char class[16]={0};
int mode=0;
int quiet_mode=0;

main(argc,argv)
int argc;
char *argv[];
{
	int qid;
	int st,i;
	char *type;
	
	make_vers_date(rcsid,VERSION,MODDATE);
#ifdef SOCKCODE
	type="<BSD IPC>";
#endif
#ifdef MQCODE
	type="<SYSV IPC>";
#endif
 	parseopts(argc,argv);

	if (!quiet_mode)
	  fprintf(stderr,"%s Release V%s (c)IDSI %s %s\n",argv[0],VERSION,MODDATE,type);

	if (!lockfile(LK_STAT,DAEMON_RUNNING))
	{
		fprintf(stderr,"IDSI scheduler [idaemon] not running\n");
		exit(1);
	}
#ifdef SOCKCODE
	if (!lockfile(LK_STAT,SOCKVERSION))
	{
		fprintf(stderr,"Incompatible version.  This is socket version of ilp\n");
		fprintf(stderr,"The currently running idaemon is a message queue version\n");
		exit(1);
	}
#endif	
#ifdef MQCODE
	if (!lockfile(LK_STAT,MQVERSION))
	{
		fprintf(stderr,"Incompatible version.  This is message queue version of ilp\n");
		fprintf(stderr,"The currently running idaemon is a socket version\n");
		exit(1);
	}
#endif


	init_me();
	st=init_comm();
	if (st!=INIT_OK)
	{
		if (st == IERR_NODAEMON)
		  fprintf(stderr,"IDSI scheduler [idaemon] not running\n");
		else
		  fprintf(stderr,"Error contacting daemon: %d\n",st);
		exit(st);
	}
	if (scnt)
	{
		for (i=0; i<scnt; ++i)
		{
			if (access(srcfiles[i],R_OK))
			{
				fprintf(stderr,"%s: Cannot read %s\n",argv[0],srcfiles[i]);
			}
			qid=queue_job(srcfiles[i],printer,form,stpg,endpg,pri,copies,class,mode);
		}
	}
	else
	{
		char *tmppath, *capture_stdin();

		if (!quiet_mode)
		  fprintf(stderr,"No files specified.  Spooling <stdin>.\n");
		tmppath = capture_stdin();
		if (tmppath)
		{
			if (!quiet_mode)
			  fprintf(stderr,"<stdin> captured to temporary file %s\n",
				  tmppath);
			qid=queue_job(tmppath,printer,form,stpg,endpg,pri,copies,class,mode|QMD_DELETEAFTER);
		}
	}
	shut_comm();
	

}
parseopts(cnt,v)
int cnt;
char *v[];
{
	int c;
	extern int optind,opterr;
	extern char *optarg;

	if (cnt==1)
	{
		usage();
	}

      top:
	
	while ((c=getopt(cnt,v,OPTSTR))!=EOF)
	{
		switch (c)
		{
		      case 'p':
		      case 'd':
			strcpy(printer,optarg);
			break;
		      case 'r':
			getrange(optarg,&stpg,&endpg);
			break;
		      case 'f':
			strcpy(form,optarg);
			break;
		      case 'C':
			strcpy(class,optarg);
			break;
		      case 'n':
			copies=atoi(optarg);
			break;
		      case 'P':
			pri=atoi(optarg);
			break;
		      case 'M':
			mode |= getmode(optarg); 
			break;
		      case 'q':
			++quiet_mode;
			break;
		      case 'h':
		      case '?':
 		      default:
			usage();
			break;
		}
	}
	if (optind < cnt) add_srcfile(v[optind++]);
	if (optind < cnt) goto top;
	
}
getrange(arg,st,end)
char *arg;
int *st,*end;
{
	char *p,*strchr();
	
	p=strchr(arg,':');
	if (p)
	{
		*p++ = '\0';
		*end = atoi(p);
	}
	else
	{
		*end = 99999;		
	}
	
	*st = atoi(arg);
	if (*st == 0) *st=1;
}
getmode(arg)
char *arg;
{
	char *opt,*getmodesub();
	int pos,mymode;
	
	pos=0;
	mymode=0;
	
	while (opt=getmodesub(arg,&pos))
	{
		if (!strcmp(opt,"hold")) mymode |= QMD_HOLD;
		else if (!strcmp(opt,"del")) mymode |= QMD_DELETEAFTER;
		else if (!strcmp(opt,"mail")) mymode |= QMD_INFORM;
		else if (!strcmp(opt,"re")) mymode |= QMD_RESPOOLHOLD;
		else if (!strcmp(opt,"nb")) mymode |= QMD_NOBANNER;
		else printf("bad mode %s\n",opt);
	}
	return mymode;
}
char *getmodesub(arg,pos)
char *arg;
int *pos;
{
	static char opt[5];
	int x;
	
	if (arg[*pos] == '\0') return NULL;
	
	memset(opt,0,sizeof(opt));
	
	x=0;
	while (arg[*pos]!=',' && arg[*pos]) opt[x++] = arg[(*pos)++];
	if (arg[*pos]==',') (*pos)++;
	return opt;
}      
init_me()
{
}
usage()
{
	fprintf(stderr,"\nilp V%s (c)IDSI  %s\n",VERSION,MODDATE);
	fprintf(stderr,"usage: ilp [options] inputfile\n");
	fprintf(stderr,"options:\n");
	fprintf(stderr,"         -d printer         job destination\n");
	fprintf(stderr,"         -r start:end       range of pages to print\n");
	fprintf(stderr,"         -f form            form to use\n");
	fprintf(stderr,"         -C class           printer class\n");
	fprintf(stderr,"         -n copies          specify number of copies\n");
	fprintf(stderr,"         -q                 quiet mode\n");
	fprintf(stderr,"         -M mode[,mode...]  specify job mode:\n");
	fprintf(stderr,"                            hold  - queue file but hold\n");
	fprintf(stderr,"                            del   - delete after print\n");
	fprintf(stderr,"                            re    - respool and hold after\n");
	fprintf(stderr,"                                    completion\n");
	exit(0);
	
}
queue_job(srcfile,ptr,form,st,end,pri,copies,class,mode)
char *srcfile,*ptr,*form,*class;
int st,end,pri,copies,mode;
{
	QITEM the_item;
	char path[200];
	char formpath[200],buf[200];
	FILE *in;
	int plen=66;
	char *p,*lines;
	int qid;
	struct stat sbuf;
	
	memset(&the_item,0,sizeof(QITEM));
	
	strcpy(the_item.orig_path,srcfile);
	if (srcfile[0] != '/')
	{
		char tmpbuf[200];
		
		getcwd(path,198);
		sprintf(tmpbuf,"%s/%s",path,srcfile);
		fixuppath(tmpbuf,the_item.real_path);
	}
	else
	  strcpy(the_item.real_path,srcfile);
	
	strcpy(the_item.requested_prtr,ptr);
	strcpy(the_item.form,form);

	stat(srcfile,&sbuf);
	the_item.size=sbuf.st_size;

	the_item.stpage=st;
	the_item.endpage=end;
	the_item.copies=copies;
	the_item.class=class[0];
	the_item.mode=mode;
	the_item.prtnum= 0;
	
	send_daemon(M_QUEUE,0,&the_item,sizeof(the_item));

}
fixuppath(src,dest)   /* cut out /./'s and /../'s in a path */
char *src,*dest;
{
	char *p, *strchr(), *backup;
	char *x=dest;
	
	do
	{
		p=strchr(src,'/');
		if (!p) continue;
		else if (!strncmp(p,"/./",3))
		{
			memcpy(dest,src,p-src+1);
			dest += p-src+1;
			src = p+3;
			continue;
		}
		else if (!strncmp(p,"/../",4))
		{
			src = p+4;

			continue;
		}
		memcpy(dest,src,p-src+1);
		dest += p-src+1;
		src=p+1; 

	} while (p);
	memcpy(dest,src,strlen(src));
	
}

add_srcfile(path)
char *path;
{
	char *calloc();
	
	srcfiles[scnt]=calloc(strlen(path)+1,1);
	strcpy(srcfiles[scnt],path);
	++scnt;
}
char *capture_stdin()
{
	static char tmppath[256];
	char buffer[512];
	int icnt,ocnt,outfd;
	extern int errno;
	extern char *sys_errlist[];
	
	sprintf(tmppath,"/usr/tmp/stdin.%d",getpid());
	outfd = open(tmppath,O_WRONLY|O_CREAT|O_TRUNC|0600);
	if (outfd<0)
	{
		fprintf(stderr,"Error opening temporary file %s: %s\n",
			tmppath,sys_errlist[errno]);
		fprintf(stderr,"Unable to spool <stdin>\n");
		return 0;
	}
	while (icnt = read(0,buffer,512))
	{
		ocnt = write(outfd,buffer,icnt);
		if (ocnt < 0)
		{
			fprintf(stderr,"Error writing temporary file %s: %s\n",
				tmppath,sys_errlist[errno]);
			fprintf(stderr,"Unable to spool <stdin>\n");
			unlink(tmppath);
			return 0;
		}			
	}
	close(outfd);
	chmod(tmppath,0600);
	return tmppath;
}
