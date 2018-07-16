/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
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

int win32msgget(key_t thekey, int flag);
int win32msgsnd(int id, void *msg, size_t msglen, int flag);
int win32msgrcv(int id, void *msg, size_t msglen, int msgtype, int flag);
int win32msgctl(int id, int function, struct msqid_ds *mctlbuf);
void win32msgtimeout(int secs_to_wait);
int win32move(char *src, char *dest);

#endif /* WIN32MSG_H */

/*
**	History:
**	$Log: win32msg.h,v $
**	Revision 1.1  1996/10/15 23:09:12  jockc
**	Initial revision
**	
**
*/
