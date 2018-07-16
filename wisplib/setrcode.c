			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	NAME:	setretcode.c
*/

/* subroutine: setretcode													*/
/* called by COBOL to pass PIC 999 status code back to calling shell								*/

#ifdef unix
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <stdio.h>
#include <errno.h>
#include "werrlog.h"
#include "wdefines.h"

extern char WISPRETURNCODE[3];
extern long LINKRETCODE;
extern char *sys_errlist[];
extern int PGRPID;									/* Process Group ID.			*/

#define		ROUTINE		59000
setretcode(wispreturncode)
char *wispreturncode;
{
	FILE *tmp;
	key_t key, wftok();
	int msgid;
	char wrc[4];
	struct {
		long mtype;
		char mtext[3];
	} msg;

	werrlog(ERRORCODE(1),wispreturncode,0,0,0,0,0,0,0);

	memcpy(WISPRETURNCODE,wispreturncode,3);					/* Copy cobol data area to C data area	*/

	memcpy(wrc,WISPRETURNCODE,3);
	wrc[3] = '\0';
	LINKRETCODE = atol(wrc);						/* Convert to long				*/

	if (0==access(WISP_RETCOD_FILE,0))					/* If WISP_RETCOD_FILE exists...		*/
	{
		key = wftok(WISP_RETCOD_FILE);
		if (key == (key_t) -1)
		{
			char	buf[80];
			sprintf(buf,"Cannot access %s",WISP_RETCOD_FILE);
			werrlog(ERRORCODE(2),buf,"wftok",0,0,0,0,0,0);
			msgid = -1;
		}
		else
		{
			msgid = msgget(key,0);
			if (msgid== -1)						/* If MESSAGE QUEUE doesn't exist		*/
			{
				/*
				** 	This used to be a (2) error but because it was happening so often
				**	and didn't actually cause a problem I reduced it to a warning.
				*/
				werrlog(ERRORCODE(3),"Cannot access msg queue",sys_errlist[errno],0,0,0,0,0,0);
			}
		}
	}                  
	else 									/* If WISP_RETCOD_FILE doesn't exist ...	*/
	{
		tmp = fopen(WISP_RETCOD_FILE,"w");				/* Create it					*/
		if (tmp==NULL)
		{
			werrlog(ERRORCODE(2),"Cannot create msg queue key file",sys_errlist[errno],0,0,0,0,0,0);
			return;
		}
		fprintf(tmp,"\n");
		fclose(tmp);
		chmod(WISP_RETCOD_FILE,0666);
		key = wftok(WISP_RETCOD_FILE);
		msgid = -1;
	}

	if ( msgid == -1 )							/* If msgid < 0 (QUEUE doesn't exist)...	*/
	{
		if (key == (key_t) -1)
		{
			werrlog(ERRORCODE(2),"Cannot create msg queue","key == -1",0,0,0,0,0,0);
			return;
		}
		msgid = msgget(key,IPC_CREAT|0777);				/* create it					*/
		if (msgid == -1)
		{
			werrlog(ERRORCODE(2),"Cannot create msg queue",sys_errlist[errno],0,0,0,0,0,0);
			return;
		}
	}
	
	msg.mtype = PGRPID;
	msgrcv(msgid,&msg,3,PGRPID,IPC_NOWAIT);					/* discard previous ret code		*/
	strncpy(msg.mtext,WISPRETURNCODE,3);
	if (msgsnd(msgid,&msg,3,IPC_NOWAIT)<0)
		werrlog(ERRORCODE(2),"Message queue error",sys_errlist[errno],0,0,0,0,0,0);
	
}

#endif	/* unix */

				/* The MSDOS section of this routine has NOT been properly coded and will NOT work until it is.	*/
				/* What is here is the remnants of the unix version which will compile under MSDOS.		*/
				/* The logic, however, is questionable at best and needs some major attention.			*/
#ifdef MSDOS

#include <errno.h>
#include <io.h>
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "werrlog.h"
#include "wdefines.h"

extern char WISPRETURNCODE[3];
extern long LINKRETCODE;
extern int PGRPID;									/* Process Group ID.			*/

#define		ROUTINE		59000
void setretcode(wispreturncode)
char *wispreturncode;
{
	FILE *tmp;
	int msgid;
	char wrc[4];
	struct {
		long mtype;
		char mtext[3];
	} msg;

	werrlog(ERRORCODE(1),wispreturncode,0,0,0,0,0,0,0);

	memcpy(WISPRETURNCODE,wispreturncode,3);					/* Copy cobol data area to C data area	*/

	memcpy(wrc,WISPRETURNCODE,3);
	wrc[3] = '\0';
	LINKRETCODE = atol(wrc);						/* Convert to long				*/

	if (0==access(WISP_RETCOD_FILE,0))					/* If WISP_RETCOD_FILE exists...		*/
	{
		{
			if (msgid== -1)						/* If MESSAGE QUEUE doesn't exist		*/
			{
				werrlog(ERRORCODE(2),"Cannot access msg queue",sys_errlist[errno],0,0,0,0,0,0);
			}
		}
	}                  
	else 									/* If WISP_RETCOD_FILE doesn't exist ...	*/
	{
		tmp = fopen(WISP_RETCOD_FILE,"w");				/* Create it					*/
		if (tmp==NULL)
		{
			werrlog(ERRORCODE(2),"Cannot create msg queue key file",sys_errlist[errno],0,0,0,0,0,0);
			return;
		}
		fprintf(tmp,"\n");
		fclose(tmp);
		chmod(WISP_RETCOD_FILE,0666);
	}

	if ( msgid == -1 )							/* If msgid < 0 (QUEUE doesn't exist)...	*/
	{
		if (msgid == -1)
		{
			werrlog(ERRORCODE(2),"Cannot create msg queue",sys_errlist[errno],0,0,0,0,0,0);
			return;
		}
	}
	
	msg.mtype = PGRPID;
	strncpy(msg.mtext,WISPRETURNCODE,3);
}

#endif	/* MSDOS */

#ifdef VMS
#include "werrlog.h"

#define 	LIB$K_CLI_GLOBAL_SYM	2
#define		ROUTINE		59000
setretcode(rc)
char *rc;  /* rc[3] */
{
	long	size;

	werrlog(ERRORCODE(1),rc,0,0,0,0,0,0,0);

	size=3;
	if (*rc == '0')								/* Remove leading zeros				*/
	{
		rc++;
		size--;
	}
	if (*rc == '0')
	{
		rc++;
		size--;
	}

	setsymb("$W_RETURN_CODE",rc,size,LIB$K_CLI_GLOBAL_SYM);
}
#endif


