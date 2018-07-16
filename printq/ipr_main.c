/*
 * Module:  ipr_main
 * Program: IDSIprint ipr
 *
 */
static char copyright[] = "Copyright 1991,1992,1993 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

static char VERSION[5];
static char MODDATE[20];

#include "defs.h"
#include "qpaths.h"
#include "c_comm.h"

#define OPTSTR "d:p:r:f:n:C:P:M:h?qcNs"

char printer[64];
char form[16];
int stpg=1,endpg=99999;
char *srcfiles[8192];
int scnt=0;
int pnum=0,copies=1;
char class[16]={0};
int mode=0;
int quiet_mode=0;
MASTCONFIG qconfig;
extern char *sys_errlist[];
extern int comm_status;		
int make_copy=FALSE;
int dont_copy=FALSE;

main(argc,argv)
int argc;
char *argv[];
{
	int qid;
	unsigned int st,i;
	
	make_vers_date(rcsid,VERSION,MODDATE);
 	parseopts(argc,argv);

	if (!quiet_mode)
	  fprintf(stderr,"%s Release V%s (c)IDSI %s\n",LPNAME,VERSION,MODDATE);

	if (initpaths() == -1)
	{
		fprintf(stderr,"%s: can't access spool dir: %s.\n",LPNAME,
			spooldir);
		exit(10);
	}
	st=isrunning();
	if (st == IERR_NODAEMON)
	{
		fprintf(stderr,"IDSI scheduler [%s] not running.\n",DAEMONNAME);
		exit(1);
	}
	else
	  if (st == IERR_SEMAPHORE)
	  {
		  fprintf(stderr,"Error accessing master semaphore.\nContact Customer Support.\n");
		  exit(2);
	  }
	waitready();
#ifdef SOCKCODE
	if (!issocket())
	{
		fprintf(stderr,"Incompatible version.  This is socket version of %s.\n",LPNAME);
		fprintf(stderr,"The currently running %s is a message queue version.\n",DAEMONNAME);
		exit(3);
	}
#endif	
#ifdef MQCODE
	if (!ismqcode())
	{
		fprintf(stderr,"Incompatible version.  This is message queue version of %s.\n",LPNAME);
		fprintf(stderr,"The currently running %s is a socket version.\n",DAEMONNAME);
		exit(4);
	}
#endif
	if (load_qconfig()<0)
	  exit(1);
	st=init_comm();
	if (st!=INIT_OK)
	{
		int ret;
		
		switch(st&0x00ff)
		{
		      case IERR_NODAEMON:
			fprintf(stderr,"IDSI scheduler [%s] not running\n",DAEMONNAME);
			ret=1;
			break;
		      case IERR_SOCKET:
			fprintf(stderr,"Error contacting %s [socket]: %s\n",DAEMONNAME,sys_errlist[(st&0xff00)>>8]);
			ret=5;
			break;
		      case IERR_BIND:
			fprintf(stderr,"Error contacting %s [connect]: %s\n",DAEMONNAME,sys_errlist[(st&0xff00)>>8]);
			ret=6;
			break;
		}
		exit(ret);
	}
	if (scnt)
	{
		for (i=0; i<scnt; ++i)
		{
			if (fcanread(srcfiles[i])==FALSE)
			{
				if (!quiet_mode)
				{
					fprintf(stderr,"%s: Cannot read %s\n",LPNAME,srcfiles[i]);
				}
			}
			else
			{
				if (uisnormal(srcfiles[i])==FALSE)
				{
					if (!quiet_mode)
					{
						fprintf(stderr,"%s: %s is not an ordinary file\n",
							LPNAME,srcfiles[i]);
					}
				}
				else
				{
					qid=queue_job(srcfiles[i],printer,form,stpg,endpg,pnum,copies,class,mode);
					if (comm_status!=COMM_OK)
					{
						fprintf(stderr,"Error talking to %s: %s\n",DAEMONNAME,sys_errlist[comm_status]);
						if (comm_status == EPIPE)
						  fprintf(stderr,"The Master process appears to have aborted prematurely\n");
						fprintf(stderr,"Contact Customer Support\n");
						exit(7);
					}
				}
			}
			if (scnt>10 && !quiet_mode)
			{
				fprintf(stderr,"\r%d files remaining.  File %s queued.            ",
					scnt-i,srcfiles[i]);
			}
		}
		if (scnt>10 && !quiet_mode)
		{
			fprintf(stderr,"\n");
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
			qid=queue_job(tmppath,printer,form,stpg,endpg,pnum,copies,class,mode|QMD_DELETEAFTER);
		}
	}
	if (comm_status!=COMM_OK)
	{
		fprintf(stderr,"Error talking to %s: %s\n",DAEMONNAME,sys_errlist[comm_status]);
		if (comm_status == EPIPE)
		  fprintf(stderr,"The %s process appears to have aborted prematurely\n",DAEMONNAME);
		fprintf(stderr,"Contact Customer Support\n");
	}
	shut_comm();
	
	exit(0);
}
parseopts(cnt,v)
int cnt;
char *v[];
{
	int c;
	extern int optind,opterr;
	extern char *optarg;
#if 0
	if (cnt==1)
	{
		usage();
	}
#endif

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
			pnum=atoi(optarg);
			break;
		      case 'M':
			mode |= getmode(optarg); 
			break;
		      case 's':
		      case 'q':
			++quiet_mode;
			break;
		      case 'c':
			++make_copy;
			break;
		      case 'N':
			++dont_copy;
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
	fprintf(stderr,"\n%s V%s (c)IDSI  %s\n",LPNAME,VERSION,MODDATE);
	fprintf(stderr,"usage: %s [options] inputfile\n",LPNAME);
	fprintf(stderr,"options:\n");
	fprintf(stderr,"         -d printer         job destination (printer name)\n");
	fprintf(stderr,"         -P printer number  job destination (printer number)\n");
	fprintf(stderr,"         -r start:end       range of pages to print\n");
	fprintf(stderr,"         -f form            form to use\n");
	fprintf(stderr,"         -C class           printer class\n");
	fprintf(stderr,"         -n copies          specify number of copies\n");
	fprintf(stderr,"         -q                 quiet mode\n");
	fprintf(stderr,"         -c                 make a copy of the file to print\n");
	fprintf(stderr,"         -M mode[,mode...]  specify job mode:\n");
	fprintf(stderr,"                            hold  - queue file but hold\n");
	fprintf(stderr,"                            del   - delete after print\n");
	fprintf(stderr,"                            re    - respool and hold after\n");
	fprintf(stderr,"                                    completion\n");
	exit(0);
	
}
queue_job(srcfile,ptr,form,st,end,pnum,copies,class,mode)
char *srcfile,*ptr,*form,*class;
int st,end,pnum,copies,mode;
{
	QITEM the_item;
	char path[2000];
	char formpath[2000],buf[2000];
	FILE *in;
	int plen=66;
	char *p,*lines;
	int qid;
	struct stat sbuf;
	time_t time();
	
	memset(&the_item,0,sizeof(QITEM));
	
	strcpy(the_item.orig_path,srcfile);
	if (srcfile[0] != '/')
	{
		char tmpbuf[2000];
		
		getcwd(path,1999);
		sprintf(tmpbuf,"%s/%s",path,srcfile);
		fixuppath(tmpbuf,the_item.real_path);
	}
	else
	  strcpy(the_item.real_path,srcfile);

	if ((qconfig.inet_address && !dont_copy) || make_copy)
	{
		char cmdbuf[2000],tmpname[2000];
		char *p, filename[2000], *strrchr();
		
		p = strrchr(the_item.real_path,'/');
		if (p)
		{
			sprintf(tmpname,"%s.%d",++p,getpid());
			sprintf(cmdbuf,"cp %s %s/%s",the_item.real_path,pfiledir,tmpname);
			system(cmdbuf);
			the_item.mode |= QMD_DELETEAFTER;
			sprintf(the_item.real_path,"%s/%s",pfiledir,tmpname);
		}
	}
	strcpy(the_item.requested_prtr,ptr);
	strcpy(the_item.form,form);

	stat(srcfile,&sbuf);
	the_item.size=sbuf.st_size;

	the_item.stpage=st;
	the_item.endpage=end;
	the_item.copies=copies;
	the_item.class=class[0];
	the_item.mode=mode;
	the_item.prtnum= pnum;

	send_daemon(M_QUEUE,0,&the_item,sizeof(the_item),the_item.time_stamp = time((time_t*)NULL));


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
	char *gmem();
	
	srcfiles[scnt]=gmem(strlen(path)+1,1);
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
	outfd = open(tmppath,O_WRONLY|O_CREAT|O_TRUNC,0600);
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
load_qconfig()
{
	int conf,sz;
	MASTCONFIG newconfig;

/*	
	if (chk_vers(CVERS)==INCOMPAT_VERS)
	{
		fprintf(stderr,"Warning: incompatible %s version\n",DAEMONNAME);
		return -1;
	}
*/
	if ((conf=open(cdumpfile,O_RDONLY))<0)
	{
		fprintf(stderr,"Warning: cannot read config file %s\n",cdumpfile);
		return -1;
	}
	lseek(conf,8,0);
	sz = read(conf,(char*)&newconfig,sizeof(MASTCONFIG));
	if (sz == sizeof(MASTCONFIG))
	  qconfig = newconfig;
	close(conf);
}     
uisnormal(path)
char *path;
{
	struct stat stinfo;
	int st;
	
	st=stat(path,&stinfo);
	if (st<0)
	{
		return FALSE;
	}
	if ((stinfo.st_mode & S_IFMT)==S_IFREG)
	{
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}
/*
 * $Log: ipr_main.c,v $
 * Revision 1.35  1993/11/10  22:06:18  jockc
 * took out stupid includes that are also in defs.h
 *
 * Revision 1.34  1993/11/10  17:38:36  jockc
 * added -s switch which is same as -q for lp compatibility
 *
 * Revision 1.33  1993/10/01  17:23:34  jockc
 * replaced reps to argv[0] with LPNAME
 *
 * Revision 1.32  1993/10/01  16:42:45  jockc
 * don't usage on no args, spool stdin
 *
 * Revision 1.31  1993/09/30  23:19:23  jockc
 * misspelled DAEMONNAME
 *
 * Revision 1.30  1993/09/30  23:09:31  jockc
 * fixed prog names
 *
 * Revision 1.29  1993/09/30  21:00:58  jockc
 * don't let people print directories with delete flag
 *
 * Revision 1.28  1993/09/15  00:31:15  jockc
 * remove BSD/SYSV IPC message
 *
 * Revision 1.27  1993/09/13  15:28:20  jockc
 * 499
 * 503
 *
 * Revision 1.27  1993/09/10  18:32:06  jockc
 * for copy of file, add unique pid to filename
 *
 * Revision 1.25  1993/08/13  18:57:46  jockc
 * added time stamp  and print by printer #, and meaningful return codes
 *
 * Revision 1.24  1993/05/28  21:45:36  jockc
 * changed ref to calloc to use gmem
 *
 * Revision 1.23  1993/05/14  23:07:09  jockc
 * changed to use fcanread() instead of access.. also, the code used to
 * print the message "cannot read the file" and spool it anyway.  This has
 * been corrected
 *
 * Revision 1.22  1993/03/26  06:36:57  jockc
 * the code which checked retcode from init_comm was wrong.. needed to
 * mask out the upper 8 bits..
 *
 * Revision 1.21  1993/03/26  00:11:29  jockc
 * check err status on looped queues and print progress info for user if
 * file count > 10
 *
 * Revision 1.20  1993/01/12  02:08:21  jockc
 * patched in the new lockfile (semaphore) mechanism, and massaged
 * the error handling a bit.
 *
 * Revision 1.19  1992/12/31  23:47:21  jockc
 * qpaths/ISPOOL.  copy of file to spool area for -c or
 * internet mode.. -N overrides the copy in internet mode
 *
 * Revision 1.18  1992/10/27  23:32:26  jockc
 * improved daemon version check and add support for
 * internet addressing
 *
 * Revision 1.17  1992/10/09  20:18:05  jockc
 * had to update for new lock const names
 *
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
