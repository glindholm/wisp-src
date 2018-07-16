/*
 * Module:  shut.c
 * Program: IDSIprint
 * Purpose: main code for scheduler
 *
 * $Log: shut.c,v $
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
static char copyright[] = "Copyright 1991 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

static char VERSION[5];
static char MODDATE[20];

#include "defs.h"
MASTCONFIG qconfig;
main(c,v)
char *v[];
{
	FILE *pid;
	char buf[10];
	int ipid;
	int stat;

	make_vers_date(rcsid,VERSION,MODDATE);
	fprintf(stderr,"%s V%s  (C) IDSI %s\n",v[0],VERSION,MODDATE);

	if (chk_vers(CVERS)==INCOMPAT_VERS)
	{
		fprintf(stderr,"Incompatible version of ishut.\nMake sure you are running the\n");
		fprintf(stderr,"most recent version of ishut.\n");
		exit(0);
	}
	load_qconfig();
	if (!isoperator(getuid())) 
	{
		fprintf(stderr,"You must be an operator (specified in %s),\n(or root) to run %s\n",
		       ILPDEF,v[0]);
		exit(0);
	}
	pid=fopen(DAEMONPID,"r");
	if (!pid) exit(0);
	fgets(buf,9,pid);
	ipid=atoi(buf);
	if (!ipid) exit(0);
	
	stat=kill(ipid,SIGUSR2);
	if (stat== -1)
	  perror("Error shutting down idaemon");
	else
	  printf("Successful shutdown of idaemon.\n");
}
load_qconfig()
{
	int conf,sz;
	
	while(lockfile(LK_STAT,DUMPING));
	if ((conf=open(CDUMPFILE,O_RDONLY))<0)
	{
		memset(&qconfig,0,sizeof(qconfig));
		qconfig.opcount = 1;
		qconfig.operators[0]=0;
		return -1;
	}
	lseek(conf,8,0);
	sz = read(conf,&qconfig,sizeof(MASTCONFIG));
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
