			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	NAME:	retcode.c
*/

/* module: retcode														*/
/* called by COBOL to get 3 digit status code back from linked COBOL programs							*/ 
#ifdef unix
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <stdio.h>
#include <errno.h>
#include "werrlog.h"
#include "wdefines.h"

extern int PGRPID;									/* Process Group ID.			*/

extern char *sys_errlist[];

#define		ROUTINE		54000
RETCODE(code)
char *code;
{
	key_t key, wftok();
	int msgid;
	struct {
		long mtype;
		char mtext[3];
	} msg;
	char mtxt[4];
	int retval;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	key = wftok(WISP_RETCOD_FILE);
	if (key == (key_t) -1)
	{
		memcpy(code,"000",3);
		return;
	}
	msgid = msgget(key,0);
	if (msgid == -1)
	{
		werrlog(ERRORCODE(3),"Cannot access message queue",sys_errlist[errno],0,0,0,0,0,0);
		memcpy(code,"000",3);
		return;
	}
	
	memset(&msg,0,sizeof(msg));
	memset(mtxt,0,sizeof(mtxt));
	strncpy(mtxt,"000",3);
	do 
	{
		if (msgrcv(msgid,&msg,3,PGRPID,IPC_NOWAIT)>0)
			strncpy(mtxt,msg.mtext,3);

	} while (errno != ENOMSG);

	strncpy(code,msg.mtext,3);
}
#endif

