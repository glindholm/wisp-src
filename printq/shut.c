/*
 * Module:  shut.c
 * Program: IDSIprint
 * Purpose: main code for scheduler
 *
 */
static char copyright[] = "Copyright 1991 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

static char VERSION[5];
static char MODDATE[20];

#include "defs.h"
#include "qpaths.h"

MASTCONFIG qconfig;
main(c,v)
char *v[];
{
	FILE *pid;
	char buf[10];
	int ipid;
	int stat;
	int verchk;
	
	make_vers_date(rcsid,VERSION,MODDATE);
	fprintf(stderr,"%s V%s  (C) IDSI %s\n",v[0],VERSION,MODDATE);
	if (initpaths() == -1)
	{
		fprintf(stderr,"%s: can't access spool dir: %s\n",v[0],
			spooldir);
		exit(1);
	}

	verchk = chk_vers(CVERS);
	if (verchk ==INCOMPAT_VERS)
	{
		fprintf(stderr,"Incompatible version of %s.\nMake sure you are running the\n",SHUTNAME);
		fprintf(stderr,"most recent version of %s.\n",SHUTNAME);
		exit(0);
	}
	if (verchk ==LOCK_TIMEOUT)
	{
		fprintf(stderr,"There is a problem reading the configuration file\n");
		fprintf(stderr,"Contact Customer Support\n");
		exit(0);
	}
	load_qconfig();
	if (!isoperator(getuid())) 
	{
		fprintf(stderr,"You must be an operator (specified in %s),\n(or root) to run %s\n",
		       ilpdef,v[0]);
		exit(0);
	}
	pid=fopen(daemonpid,"r");
	if (!pid) exit(0);
	fgets(buf,9,pid);
	ipid=atoi(buf);
	if (!ipid) exit(0);
	
	stat=kill(ipid,SIGUSR2);
	if (stat== -1)
	{
		fprintf(stderr,"Error shutting down %s: %s (errno=%d)\n",
			DAEMONNAME,sys_errlist[errno],errno);
	}		
	else
	{
		fprintf(stderr,"Successful shutdown of %s.\n",DAEMONNAME);
	}
}
load_qconfig()
{
	int conf,sz;
	
	if ((conf=open(cdumpfile,O_RDONLY))<0)
	{
		memset(&qconfig,0,sizeof(qconfig));
		qconfig.opcount = 1;
		qconfig.operators[0]=0;
		return -1;
	}
	lseek(conf,8,0);
	sz = read(conf,(char*)&qconfig,sizeof(MASTCONFIG));
	close(conf);
}     
isoperator( id )
int id;
{
	int i;
	
	for (i=0; i<qconfig.opcount; ++i)
	  if (qconfig.operators[i] == id) 
	    return TRUE;

	return FALSE;
}
chk_vers(type)
int type;
{
	int ret;
	
	if (type&QVERS && (ret=chk_qvers())<0) 
	  return ret;
	if (type&PVERS && (ret=chk_pvers())<0) 
	  return ret;
	if (type&CVERS && (ret=chk_cvers())<0) 
	  return ret;
	return 0;
}
chk_qvers()
{
	return chk_dump(qdumpfile);
}
chk_pvers()
{
	return chk_dump(pdumpfile);
}
chk_cvers()
{
	return chk_dump(cdumpfile);
}
chk_dump(path)
char *path;
{
	int dump,ret=0,count;
	unsigned int magic, version;
	extern char *sys_errlist[];
	dump=open(path,O_RDWR);
	if (dump<0)
	{
		ret = INCOMPAT_VERS;
		goto exit_chkdump;
	}
	if (read(dump,(char*)&magic,sizeof(magic))<0)
	  ret = INCOMPAT_VERS;
	if (read(dump,(char*)&version,sizeof(version))<0)
	  ret = INCOMPAT_VERS;
	if ((magic & 0xffffff00) != (QDUMPMAGIC & 0xffffff00))
	  ret = INCOMPAT_VERS;
	if (version != DUMPVERSION)
	  ret = INCOMPAT_VERS;

      exit_chkdump:
	if (dump>=0) close(dump);
	return ret;
}
char *gmem(cnt,sz)
int sz,cnt;
{
	char *calloc();
	static char *tmp;
	
	tmp=calloc(cnt,sz);
	if (tmp==NULL)
	{
		fprintf(stderr,"Out of memory (%d bytes requested)\n",sz*cnt);
		fprintf(stderr,"ishut failed.\n",0);
		exit(1);
	}
	else
	  return tmp;
}
/*
 * $Log: shut.c,v $
 * Revision 1.11  1993/09/30  23:05:37  jockc
 * fix paths and prognames
 *
 * Revision 1.10  1993/08/13  20:14:00  jockc
 * using int instead of long
 *
 * Revision 1.9  1993/05/28  21:46:18  jockc
 * slight change to shut.c's gmem()
 *
 * Revision 1.8  1993/01/12  02:09:47  jockc
 * implemented new semaphore thing
 *
 * Revision 1.7  1992/12/31  23:50:42  jockc
 * qpaths/ISPOOL
 *
 * Revision 1.6  1992/10/09  20:19:16  jockc
 * changed to support new lock scheme, including a new condition:
 * timeout...
 *
 * Revision 1.5  1992/05/20  23:18:36  jockc
 * support for version checking. changed #define for config dump
 * file name.  changed printf to fprintf stderr
 *
 * Revision 1.4  1992/04/30  18:59:00  jockc
 * allow any operator to ishut, fixup version printing,
 * nicen up message a bit
 *
 * Revision 1.3  1991/04/19  00:47:34  jockc
 * *** empty log message ***
 *
 * Revision 1.2  1991/04/19  00:23:00  jockc
 * *** empty log message ***
 *
 * Revision 1.1  1991/04/18  23:52:13  jockc
 * Initial revision
 *
 *
 */
