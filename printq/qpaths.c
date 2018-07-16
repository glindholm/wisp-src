/*
 * Module:  queue_paths
 * Program: IDSIprint
 * Purpose: routines to manage the working pathnames
 *
 */
static char copyright[] = "Copyright 1991,1992,1993 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

#define EXT extern
#include "defs.h"

char pfiledir[128];
char spooldir[128];
char initdir[128];
char dumpdir[128];
char qdumpfile[128];
char pdumpfile[128];
char cdumpfile[128];
char oldqdump[128];
char pstatusd[128];
char printcap[128];
char prtdef[128];
char formdef[128];
char classdef[128];
char ilpdef[128];
char dispprog[128];
char remdir[128];

char keydeffile[128];

char *shmkeypaths[4];
char semkeyfile[128];
char prtkeyfile[128];

char errlogfile[128];
char olderrlogfile[128];
char daemonpid[128];

char oldlockfile[128];
char oldlocktmp[128];
char lockfilep[128];

#ifdef MQCODE
char mqpath[128];
#endif
#ifdef SOCKCODE
char socketpath[128];
#endif

int initpaths(void)
{
	char *envvar, *getenv(CONST char *);
	static int pathsinit = -1;
	int idx;
	
	if (pathsinit == 1)
	{
		return 1;
	}
	else
	{
		pathsinit = 1;
	}
	if (envvar=getenv(SPOOLVAR))
	{
		strcpy(spooldir,envvar);
	}
	else
	{
		strcpy(spooldir,DEFSPOOLDIR);
	}
	if (access(spooldir,R_OK|X_OK))
	{
		return -1;
	}
	sprintf(initdir,INITDIRF,spooldir);
	sprintf(dumpdir,DUMPDIRF,spooldir);
	sprintf(remdir,REMDIRF,spooldir);
	sprintf(qdumpfile,QDUMPFILEF,dumpdir);
	sprintf(pdumpfile,PDUMPFILEF,dumpdir);
	sprintf(cdumpfile,CDUMPFILEF,dumpdir);
	strcpy(oldqdump,OLDQDUMPFILEF);
	sprintf(pstatusd,PSTATUSLOGSF,spooldir);
	sprintf(pfiledir,PFILEDIRF,spooldir);
	strcpy(printcap,OLDPRINTCAPF);
	sprintf(prtdef,PRINTCAPF,spooldir);
	sprintf(formdef,FORMDEFF,spooldir);
	sprintf(classdef,CLASSDEFF,spooldir);
	sprintf(ilpdef,ILPDEFF,spooldir);
#ifdef UNIQUE
	sprintf(errlogfile,DEFERRLOGF,spooldir);
	sprintf(olderrlogfile,OLDERRLOGF,spooldir);
#else
	sprintf(errlogfile,ERRLOGF,spooldir);
#endif
	sprintf(daemonpid,DAEMONPIDF,spooldir);
	sprintf(oldlockfile,OLDLOCKFILEF,spooldir);
	sprintf(oldlocktmp,OLDLOCKTMPF,spooldir);
	sprintf(lockfilep,OLDLOCKFILEF,spooldir);


	sprintf(semkeyfile,SEMFILE,dumpdir);
	sprintf(prtkeyfile,PRTKFILE,dumpdir);
	for (idx = 0; idx < QDFILECNT; ++idx)
	{
		extern char *gmem(int cnt, int sz);
		
		shmkeypaths[idx] = gmem(128,1);
		sprintf(shmkeypaths[idx],QDKFILE,dumpdir,idx);
	}
	
#ifdef MQCODE
	sprintf(mqpath,MQPATHF,spooldir);
#endif
#ifdef SOCKCODE
	sprintf(socketpath,SOCKETPATHF,spooldir);
#endif	

	return 1;
}

