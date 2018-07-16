/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

/*
**	File:	        WIN32MSG.h
**
**	Project:	WISPLIB
**
**	RCS:		$Source:$
**
**	Purpose:	some defines for the msgxxx() routines
**
*/

#ifndef WIN32MSG_H
#define WIN32MSG_H
/*
**	Includes
*/
/*
**	Structures and Defines
*/
#define IPC_EXCL 1
#define IPC_CREAT 2

#define IPC_STAT 1
#define IPC_RMID 2

#define IPC_NOWAIT 1

#define FILE_SHARE_EXCL 0
typedef long key_t;

struct msqid_ds {
	long	   msg_qnum;	   /* # of messages on q */
};


/*
**	Function Prototypes
*/

int  WL_win32msgget(key_t thekey, int flag);
int  WL_win32msgsnd(int id, void *msg, size_t msglen, int flag);
int  WL_win32msgrcv(int id, void *msg, size_t msglen, int msgtype, int flag);
int  WL_win32msgctl(int id, int function, struct msqid_ds *mctlbuf);
void WL_win32msgtimeout(int secs_to_wait);
int  WL_win32move(char *src, char *dest);

#endif /* WIN32MSG_H */

/*
**	History:
**	$Log: win32msg.h,v $
**	Revision 1.3  2003/01/31 19:18:00  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.2  2002/07/10 21:05:33  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.1  1996/10/15 23:09:12  jockc
**	Initial revision
**	
**
*/
