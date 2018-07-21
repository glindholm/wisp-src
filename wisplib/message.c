/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
*/


/*
**	File:		message.c
**
**	Project:	WISPLIB
**
**	Purpose:	VSSUB MESSAGE
**
*/


#include <stdio.h>
#include <string.h>
#include <stdarg.h>


#ifdef unix
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <termio.h>
#include <unistd.h>
#endif

#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <time.h>

#ifdef WIN32
#include <signal.h>
#include <direct.h>
#include <io.h>
#include "wperson.h"
#include "win32msg.h"
#include "wispnt.h"
#include "wispcfg.h"

#define msgget WL_win32msgget
#define msgsnd WL_win32msgsnd
#define msgrcv WL_win32msgrcv
#define msgctl WL_win32msgctl
#endif


#include "idsistd.h"
#include "werrlog.h"
#include "wdefines.h"
#include "vwang.h"
#include "wmalloc.h"
#include "wperson.h"
#include "wisplib.h"
#include "osddefs.h"

#define PORTNAME_SIZE   4

/* Message port buffer defn's */
#define MIN_MSG_BUFSIZE 1
#define MAX_MSG_BUFSIZE 2014
#define DEF_MSG_BUFSIZE 2014

#define MIN_ARGS  3


#define M_CREATE 0
#define M_DELETE 1
#define M_XMIT  2
#define M_XMTWT 3
#define M_CHECK 4
#define M_INV -1

#define SUCCESS 	0
#define FAILURE 	64

#define CR_PORT_BUSY 	4
#define CR_PORT_OURS 	8
#define CR_NO_SPACE	24

#define DE_MSG_LOST 	4
#define DE_NO_PORT 	8

#define XM_NO_PORT 	4
#define XM_PORT_FULL 	8
#define XM_PORT_PRIV 	12

#define CH_PORT_TIMEOUT 8
#define CH_NO_PORT 	16
#define CH_PORT_FKEYLOCK 12

#define MSGTYPE 1L

#ifdef unix
extern	key_t 	WL_wftok(const char *file);
extern	int	WL_woperator(void);
#endif

static int timeout;

static struct {
	long mtype;
	char mtext[MAX_MSG_BUFSIZE];
} msg;
static struct msqid_ds mctlbuf;

static int create_message_port(char port_name[PORTNAME_SIZE], int4 buf_size, char keep_flag);
static int destroy_message_port(char port_name[PORTNAME_SIZE]);
static int transmit_message(char portname[PORTNAME_SIZE], char* mtext, int4 mlen, int4 wait);
static int check_message(char port_name[PORTNAME_SIZE],
			 char check_t,
			 int4 time_int,
			 int4 *mess_len,
			 char *receiver);

static key_t keyval(char p[PORTNAME_SIZE], int create);
static void makemsgkeyfile(char port[PORTNAME_SIZE], char* path);
static int portownergid(char port[PORTNAME_SIZE]);
static int functype(char* func);

static void signal_timed_out(int sig);

extern void WSXIO(char*, ...);

struct port_s
{
	struct port_s	*next;
	char		port_name[5];
};
static struct port_s	*port_list = NULL;

/*
	MESSAGE	("CR", Port, [Bufsize, [Keep,]] Retcode)
		("DE", Port, Retcode)
		("XM", Port, Message, Messlen, Retcode)
		("XW", Port, Message, Messlen, Retcode)
		("CH", Port, Chtype, Time, Message, Messlen, [Nohelp,] Retcode)

	Func	Alpha(2)	
			"CR"	Create message port
			"DE"	Destroy message port
			"XM"	Transmit a message
			"XW"	Transmit a message (wait if port full)
			"CH"	Check for message

	Port	Alpha(4)
			Port name

	Bufsize	Int(4)
			Max bytes port can hold until checked

	Keep	Alpha(1)
			Keep port on unlink
			"Y"	Yes
			"N"	No
			" "	Default (No)

	Message	Alpha(var)
	MessLen	Int(4)
	Chtype	Alpha(1)
			Type of check
			"W"	Wait until message received
			"T"	Wait until message received or time-out
			"K"	Wait until message received or PFkey pressed
			"B"	Wait until message received or time-out or PFkey pressed

	Time	Int(4)
			Time to wait in hundreds of seconds

	Nohelp	Alpha(1)
			Disable help
	Retcode	Int(4)
			Return code
			"CR"
				0	port created
				4	Another task is using this port
				8	This task is using this port
			"DE"
				0	Port destroyed
				4	Port destroyed (messages lost)
				8	No such port was created by this task
			"XM", "XW"
				0	Message queued
				4	Port not created
				8	Buffer full ("XM" only)
				12	Port is privileged
			"CH"
				0	Message received
				8	Time-out
				12	Keyboard locked
				16	No such port was created by this task
*/

void MESSAGE( char* arg1_func, char* arg2_port, ... )
{
	va_list arg_list;
	int 	arg_count, original_arg_count;
	char 	*chptr;
	char 	funcstr[3], portname[5];
	int4 	wait, ourret, *retcode;
	int	i=0;

	wait = 0;

	va_start(arg_list, arg2_port);
	arg_count = WL_va_count();

	if (arg_count < MIN_ARGS)
	{
		werrlog(WERRCODE(35002),arg_count,0,0,0,0,0,0,0);
		va_end(arg_list);
		return;
	}

	WL_wtrace("MESSAGE","ENTRY","Function=[%2.2s] Port=[%4.4s] args=%d", arg1_func, arg2_port, arg_count);
	
	original_arg_count = arg_count;

	/*
	**	Get the Function
	*/
	chptr = arg1_func;
	funcstr[0] = chptr[0];
	funcstr[1] = chptr[1];
	funcstr[2] = (char)0;
	arg_count -= 1;

	/*
	**	Get the port name and correct any unusable characters
	*/
	chptr = arg2_port;
	memcpy(portname, chptr, PORTNAME_SIZE);
	portname[PORTNAME_SIZE] = (char)0;
	arg_count -= 1;

#ifdef unix
	for(i=0; i<PORTNAME_SIZE; i++)
	{
		if (' ' == chptr[i] || '/' == chptr[i] || (char)0 == chptr[i])
		{
			/*
			**	SPACE, NULL and SLASH are invalid characters for a portname, 
			**	replace with '#'.
			*/
			portname[i] = '#';
		}
	}
#endif


	switch(functype(funcstr))
	{
	case M_CREATE:
		{
			int4	l_bufsize;
			char	l_keep;
			int4	*buffer_size;

			if (original_arg_count > 5)
			{
				WL_wtrace("MESSAGE","ARGS","Too many arguments [%d], maximum is 5", original_arg_count);
			}

			l_bufsize = DEF_MSG_BUFSIZE;
			l_keep = 'N';

			if (arg_count > 1)
			{
				buffer_size = va_arg(arg_list, int4 *);
				arg_count -= 1;
				l_bufsize = WL_get_swap(buffer_size);

				if (l_bufsize < MIN_MSG_BUFSIZE || l_bufsize >MAX_MSG_BUFSIZE)
				{
					WL_wtrace("MESSAGE","CREATE","Invalid Bufsize=[%d] using default Bufsize=[%d]", 
					       l_bufsize, DEF_MSG_BUFSIZE);
					l_bufsize = DEF_MSG_BUFSIZE;
				}
			}

			if (arg_count > 1)
			{
				l_keep = *(va_arg(arg_list, char *));
				arg_count -= 1;

				if (l_keep != 'Y')
				{
					l_keep = 'N';
				}
			}

			ourret	= create_message_port(portname,l_bufsize,l_keep);
		}
		break;

	case M_DELETE:
		if (original_arg_count != 3)
		{
			WL_wtrace("MESSAGE","ARGS","Invalid number of arguments [%d], expected is 3", original_arg_count);
		}
		ourret	= destroy_message_port(portname);			/* dispatch to handler			*/
		break;

	case M_XMTWT:
		wait = TRUE;							/* set wait flag, then do normal xmit	*/
	case M_XMIT:
		{
			char 	*mtext;
			int4 	mess_len, *mess_len_p;

			if (original_arg_count < 5)
			{
				werrlog(WERRCODE(35002),original_arg_count,0,0,0,0,0,0,0);
				va_end(arg_list);
				return;
			}
			if (original_arg_count > 5)
			{
				WL_wtrace("MESSAGE","ARGS","Invalid number of arguments [%d], expected is 5", original_arg_count);
			}

			mtext = va_arg(arg_list,char *); 			/* get text pointer			*/
			arg_count--;
			
			mess_len_p = va_arg(arg_list,int4 *);			/* and length				*/
			arg_count--;
			mess_len = WL_get_swap(mess_len_p);

			ourret = transmit_message(portname,mtext,mess_len,wait);/* call handler for message send op	*/
		}
		break;	

	case M_CHECK:
		{
			char	check_t;
			char 	*receiver;
			int4 	time_int, *time_int_p;
			int4 	mess_len, *mess_len_p;

			if (original_arg_count < 7)
			{
				werrlog(WERRCODE(35002),original_arg_count,0,0,0,0,0,0,0);
				va_end(arg_list);
				return;
			}
	
			if (original_arg_count > 8)
			{
				WL_wtrace("MESSAGE","ARGS","Too many arguments [%d], maximum is 8", original_arg_count);
			}

			check_t    = *va_arg(arg_list,char*);			/* get check type			*/
			arg_count--;
			time_int_p = va_arg(arg_list,int4 *);			/* and timeout value			*/
			arg_count--;
			time_int   = WL_get_swap(time_int_p);
			receiver   = va_arg(arg_list,char*);			/* pointer to receiving area		*/
			arg_count--;
			mess_len_p = va_arg(arg_list,int4 *); 			/* pointer to message len		*/
			arg_count--;
			mess_len   = WL_get_swap(mess_len_p);
			if (arg_count > 1) 
			{
				chptr = va_arg(arg_list,char*);			/* if two args left ignore first	*/
				arg_count--;
			}
			
			ourret     = check_message(portname,check_t,time_int,&mess_len,receiver);	/* get the message	*/

			if (SUCCESS == ourret)
			{
				WL_put_swap( mess_len_p, mess_len );
			}
		}
		break;

	case M_INV:
	default:
		werrlog(WERRCODE(35004),funcstr,0,0,0,0,0,0,0);
		va_end(arg_list);
		return;	
	}

	WL_wtrace("MESSAGE","RETURN","Return code = [%d]", ourret);
	
	if (arg_count > 1)
	{
		WL_wtrace("MESSAGE","ARGS", "Too many args remaining, expecting 1 (retcode) found [%d]", arg_count);
	}
	else if (arg_count < 1)
	{
		WL_wtrace("MESSAGE","ARGS", "Too few args, retcode not found");
		va_end(arg_list);
		return;
	}

	retcode	= va_arg(arg_list,int4 *);
	va_end(arg_list);

	WL_put_swap( retcode, ourret );
}

static int create_message_port(char port_name[PORTNAME_SIZE], int4 buf_size, char keep_flag)
{
	key_t 	port_key;
	char	keyfilepath[256];
	int    	msqid;

	WL_wtrace("MESSAGE","CREATE","Port=[%4.4s] bufsize=[%d] keep=[%c]", port_name, buf_size, keep_flag);
	
try_again:
	makemsgkeyfile(port_name,keyfilepath);
	if ( fexists(keyfilepath) )
	{
		if (WL_wgetpgrp() == portownergid(port_name))
		{
			return CR_PORT_OURS;
		}
		else
		{
			return CR_PORT_BUSY;
		}
	}

	port_key = keyval(port_name,1);
	if (port_key == (key_t) -1)
	{
		wisp_unlink(keyfilepath);
		return FAILURE;
	}
	msqid = msgget(port_key,IPC_EXCL|IPC_CREAT|0777);				/* get and create		*/
	if (msqid == -1)								/* system call failed		*/
	{
		if (EEXIST==errno)
		{
			/*
			**	We hit a dangling message queue. 
			**	Don't mess with it!
			**	Just rename the keyfile so no one else uses this inode,
			**	then we try again (with a new inode/key).
			*/

#ifdef unix			
			char	newbuf[80];

			sprintf(newbuf,"%s/i%ld", wispprbdir(NULL), WL_inode(keyfilepath));
			if (-1 == link(keyfilepath,newbuf))
			{
				return CR_PORT_BUSY;
			}
			if (-1 == wisp_unlink(keyfilepath))
			{
				return CR_PORT_BUSY;
			}
#endif
			goto try_again;
		}
		else if (ENOSPC==errno)
		{
			wisp_unlink(keyfilepath);
			return CR_NO_SPACE;
		}
		else
		{
			wisp_unlink(keyfilepath);
			return FAILURE;
		}
	}

	if ('Y' != keep_flag)
	{
		/*
		**	Add the port to the list of ports to be deleted.
		*/
		struct port_s	*port_ptr;

		port_ptr = (struct port_s *)wisp_malloc(sizeof(struct port_s));
		port_ptr->next = port_list;
		memcpy(port_ptr->port_name,port_name,5);
		port_list = port_ptr;
	}

	return SUCCESS;
}

static int destroy_message_port(char port_name[PORTNAME_SIZE])
{
	key_t 	port_key;
	char 	keyfilepath[256];
	int 	msqid;

	WL_wtrace("MESSAGE","DELETE","Delete port=[%4.4s]", port_name);

	makemsgkeyfile(port_name,keyfilepath);
	if ( !fexists(keyfilepath) )
	{
		return DE_NO_PORT;
	}
#ifdef unix	
	else if (WL_woperator())
	{
		/* Allow operator to delete port */
	}
#endif	
	else if (WL_wgetpgrp() != portownergid(port_name))
	{
		return DE_NO_PORT;
	}

	{
		/*
		**	Remove this port from the delete list.
		*/
		struct port_s	*port_ptr;

		port_ptr = port_list;
		while(port_ptr)
		{
			if (0==strcmp(port_ptr->port_name,port_name))
			{
				/*
				**	Found port in the list.
				**	"Remove" it by nulling the name.
				**	We don't muck with the list as message_unlink() may be doing this.
				*/
				port_ptr->port_name[0] = (char)0;
				break;
			}
			port_ptr = port_ptr->next;
		}
	}

	port_key = keyval(port_name,0);							/* get key for this ipc structure	*/
	if (port_key == (key_t) -1)
	{
		/*
		**	There is a problem, but if we can delete the keyfile
		**	assume were OK else report failure.
		*/
		if (0==wisp_unlink(keyfilepath)) return SUCCESS;
		return FAILURE;
	}

	msqid = msgget(port_key,0);							/* get identifier for queue		*/
	if (msqid == -1) 
	{
		/*
		**	There is a problem, but if we can delete the keyfile
		**	assume were OK else report failure.
		*/
		if (0==wisp_unlink(keyfilepath)) return SUCCESS;
		return FAILURE;								/* queue not exist			*/
	}

	if (msgctl(msqid,IPC_STAT,&mctlbuf) == -1)					/* cannot get status, not owner		*/
	{
		/*
		**	There is a problem, but if we can delete the keyfile
		**	assume were OK else report failure.
		*/
		if (0==wisp_unlink(keyfilepath)) return SUCCESS;
		return FAILURE;
	}

	if (msgctl(msqid,IPC_RMID,0) == -1) 						/* only the owner can delete.		*/
	{
#ifdef unix		
		char	newbuf[80];

		/*
		**	We couldn't remove the message queue so we will 
		**	rename the keyfile (same inode) so no one else
		**	will run into a dangling message queue.
		*/
		sprintf(newbuf,"%s/i%ld", wispprbdir(NULL), WL_inode(keyfilepath));
		link(keyfilepath,newbuf);
#endif
	}

	if (0!=wisp_unlink(keyfilepath)) return FAILURE;
	if (mctlbuf.msg_qnum) return DE_MSG_LOST;					/* deleted, but msgs on queue lost	*/
	else return SUCCESS;								/* normal deletion			*/
}

static int transmit_message(char portname[PORTNAME_SIZE], char* mtext, int4 mlen, int4 wait)
{
	key_t 	port_key;
	int 	msqid;

	if (WL_wtracing())
	{
		char message[24];
		
		if (mlen > sizeof(message)-4)
		{
			memcpy(message, mtext, sizeof(message)-4);
			message[sizeof(message)-4] = '\0';
			strcat(message,"...");
		}
		else
		{
			memcpy(message, mtext, mlen);
			message[mlen] = '\0';
		}
		
		WL_wtrace("MESSAGE","TRANSMIT","Port=[%4.4s] Message=[%s] Messlen=[%d] %s",
		       portname, message, mlen, (wait)?"WAIT":"NO WAIT"); 
	}

	port_key = keyval(portname,0);
	if (port_key == (key_t) -1)
	{
		return XM_NO_PORT;
	}
	msqid = msgget(port_key,0);							/* get identifier for queue		*/
	if (msqid == -1) 
	{
		return (errno==EACCES)?XM_PORT_PRIV:XM_NO_PORT;				/* queue not exist or not privileged	*/
	}

	memset(&msg,0,sizeof(msg));							/* zed message struct			*/
	mlen = (mlen > MAX_MSG_BUFSIZE) ? MAX_MSG_BUFSIZE : mlen;			/* Trim the length to max		*/
	memcpy(msg.mtext,mtext,mlen);							/* copy callers message into our struct	*/
	msg.mtype = (long)MSGTYPE;							/* assign arbitrary type		*/

	if (msgsnd(msqid,&msg,(size_t)mlen,wait?0:IPC_NOWAIT)<0)			/* call msg send routine		*/
	{
		if (!wait && errno==EAGAIN) 
		{
			return XM_PORT_FULL;						/* call failed, queue must be full	*/
		}
	}

	if (wait)									/* now wait for queue to empty		*/
	{
		do
		{
			WL_hpause(10);     /* sleep a 1/10 of a second */
			msgctl(msqid,IPC_STAT,&mctlbuf); 

		} while (mctlbuf.msg_qnum); 
	}
	return SUCCESS;
}

static int check_message(char port_name[PORTNAME_SIZE],
			 char check_t,
			 int4 time_int,
			 int4 *mess_len,
			 char *receiver)
{
	key_t 	port_key;
	int 	wait_time, wait_key, gotmsg, gotlen, gotkey;
	int	rc;
	int4	secs_to_wait;
	char	keyfilepath[256];
	int msqid;

	WL_wtrace("MESSAGE","CHECK", "Port=[%4.4s] Checktype=[%c], Time=[%d]", port_name, check_t, time_int);
	
	timeout = gotkey = gotmsg = 0;

	makemsgkeyfile(port_name,keyfilepath);
	if ( !fexists(keyfilepath) )
	{
		/*
		**	Port doesn't exist.
		*/
		return CH_NO_PORT;
	}
	else if (WL_wgetpgrp() != portownergid(port_name))
	{
		/*
		**	Port exists but not created by this task.
		*/
		return CH_NO_PORT;
	}

	port_key = keyval(port_name,0);
	if (port_key == (key_t) -1)
	{
		/*
		**	Port is corrupted.
		*/
		wisp_unlink(keyfilepath);
		return FAILURE;
	}

	msqid = msgget(port_key,0);							/* get identifier for queue		*/
	if (msqid == -1)
	{
		/*
		**	Associated message queue not found.
		**	Port is corrupted.
		*/
		wisp_unlink(keyfilepath);
		return FAILURE;								/* queue not exist			*/
	}

	memset(&msg,0,sizeof(msg));							/* zed message struct			*/

	switch (check_t)
	{
	case 'T': 
		wait_time = 1;
		wait_key = 0;
		break;	
	case 'K': 	
		wait_time = 0;
		wait_key = 1;
		break;
	case 'B': 
		wait_time = 1;
		wait_key = 1;
		break;
	case 'W': 
	default:
		wait_time = 0;
		wait_key = 0;
		break;
	}

	/*
	**	Keyboard interaction is not supported with native screens
	*/
	if (wait_key && wisp_nativescreens())
	{
		WL_werrlog_error(WERRCODE(35000),"MESSAGE", "NATIVE", 
			"MESSAGE check type '%c' not supported with Native Screens", 
			check_t);
		wait_key = 0;
		wait_time = 1;
		time_int = 0;
	}

	if (wait_time) 
	{
		/*
		**	Calc the number of seconds to wait. 
		**	If less the 1/2 then don't wait.
		*/
		if (time_int < 50)
		{
			secs_to_wait = 0;
			timeout = 1;
		}
		else
		{
			/*
			**	Round up to a minimum of 1 sec.
			*/
			secs_to_wait = (time_int+50) / 100;
		}
	}

	if (wait_key)
	{
		/*
		**	Loop until a message received
		**		or a aid-key is pressed
		**		[or a timeout occurs]
		*/

		time_t	curr_time, stop_time;

		if (wait_time)
		{
			/*
			**	Calculate the stop time for a timeout.
			*/
			time(&curr_time);
			stop_time = curr_time + secs_to_wait;
		}

		for(;;)
		{
			/*
			**	Check for a message (nodelay)
			*/
			rc = msgrcv(msqid,&msg,MAX_MSG_BUFSIZE,MSGTYPE,IPC_NOWAIT); 	/* call with nodelay on			*/

			if (rc >= 0)
			{
				/*
				**	Got a message
				*/
				gotlen = rc;
				gotmsg = 1;
				timeout = 0;
				break;
			}

			if (wait_time)
			{
				time(&curr_time);
				if (curr_time >= stop_time)
				{
					/*
					**	A timeout occurred
					*/
					timeout = 1;
					break;
				}
			}

			/*
			**	We are waiting for a AID key to be pressed.
			**	The workstation MUST be unlocked.
			**	This WSXIO/WAIT call will do a 1 sec timed read.
			**	This will allow the user to also enter regular data on the screen.
			*/
			{
				int4	one_sec = 100;
				char	iosw[8];

				WL_wswap(&one_sec);
				memset(iosw,' ',8);

				WL_set_va_count(4);
				WSXIO("W", 0, &one_sec, iosw);

				if (0 != memcmp(iosw,"        ",8))
				{
					/*
					**	A AID key was pressed
					*/
					gotkey = 1;
					break;
				}
			}
		} 
	}
	else if (wait_time && 0==secs_to_wait)
	{
		/*
		**	Check for a message with nowait
		*/
		rc = msgrcv(msqid,&msg,MAX_MSG_BUFSIZE,MSGTYPE,IPC_NOWAIT);
		if (rc >= 0)
		{
			gotlen = rc;
			gotmsg = 1;
			timeout = 0;
		}
	}
	else
	{
		/*
		**	Check for a message and wait for it to arrive.
		**	(may be interrupted by a timeout)
		*/

		if (wait_time && secs_to_wait)
		{
#ifdef unix
			signal(SIGALRM,signal_timed_out);				/* setup to catch alarm signal		*/
			alarm(secs_to_wait);						/* turn on alarm			*/
#endif
#ifdef WIN32
			WL_win32msgtimeout(secs_to_wait);
#endif
		}

		rc = msgrcv(msqid,&msg,MAX_MSG_BUFSIZE,MSGTYPE,0);
		if (rc >= 0)
		{
			gotlen = rc;
			gotmsg = 1;
			timeout = 0;
		}
#ifdef WIN32
		/*
		** WIN32 does not support the SIGALRM, so we can't rely on the sighandler
		** to set this flag for us.  However a -1 return from msgrcv on a timed read
		** means the read timed out (or an actual fatal error occurred, in which
		** case it probably no longer matters if we misinterperet it as a timeout)
		*/
		if (rc == -1 )
		{
			timeout = 1;
		}
#endif

		if (wait_time && secs_to_wait)
		{
#ifdef unix			
			alarm(0);							/* kill any pending alarm		*/
			signal(SIGALRM,SIG_DFL);					/* and reset sig table			*/
#endif			
		}
	}

	if (gotmsg)
	{
		if (gotlen < *mess_len)
		{
			*mess_len = gotlen;						/* get actual length			*/
		}

		memcpy(receiver,msg.mtext,*mess_len);					/* copy message to receiver		*/

		return SUCCESS;
	}
	else if (gotkey) 
	{
		return CH_PORT_FKEYLOCK;
	}
	else if (timeout) 
	{
		return CH_PORT_TIMEOUT;
	}

	return FAILURE;
}

/*
**	Routine:	WL_message_unlink()
**
**	Function:	Delete all the message ports created by this task.
**
**	Description:	Step thru the port_list and delete each port and free the list.
**
**	Arguments:	None
**
**	Globals:
**	port_list	The pointer to the list of ports to be deleted.
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	11/30/93	Written by GSL
**
*/
int WL_message_unlink(void)
{
	struct port_s	*port_ptr;

	while(port_list)
	{
		port_ptr = port_list;
		if (port_ptr->port_name[0])
		{
			char	port_name[PORTNAME_SIZE+1];

			/*
			**	Move port_name to a tmp variable because destroy_message_port
			**	will manipulate the port_list, and null the port_name.
			*/
			memcpy(port_name,port_ptr->port_name,PORTNAME_SIZE+1);
			destroy_message_port(port_name);
		}
		port_list = port_ptr->next;
		free(port_ptr);
	}
	return 0;
}

#ifdef unix
static void signal_timed_out(int sig)
{
	timeout=1;
}
#endif

static key_t keyval(char p[PORTNAME_SIZE], int create)
{
	char 	keypath[256];
	FILE 	*keyfile;
	key_t	return_key;
#ifdef WIN32
	int4   thekey;
#endif

	return_key = (key_t) -1;
		
	makemsgkeyfile(p,keypath);
	if (!create && !fexists(keypath))
	{
		WL_wtrace("MESSAGE","KEYVAL", "Keypath [%s] doesn't exists for Port=[%4.4s]  Keyval=-1", keypath, p);
		return (key_t) -1;
	}
	
	if (create)
	{
		keyfile=fopen(keypath,FOPEN_WRITE_TEXT);
		if( !keyfile )
		{
			werrlog(WERRCODE(35016),"create",keypath,0,0,0,0,0,0);
			return (key_t) -1;
		}
		fprintf(keyfile,"%d\n",WL_wgetpgrp());
		fclose(keyfile);
#ifdef unix
		chmod(keypath,0666);
#endif
	}
#ifdef unix
	return_key =  WL_wftok(keypath);
#endif
#ifdef WIN32
	memcpy((void *)&thekey,(void *)p,sizeof(thekey));
	return_key = (key_t)thekey;
#endif

	WL_wtrace("MESSAGE","KEYVAL", "Port=[%4.4s] Keyval=[0x%08X]", p, (int)return_key);

	return return_key;
}

static void makemsgkeyfile(char port[PORTNAME_SIZE], char* path)
{
#ifdef unix
	const char *msgdir = wispprbdir(NULL);
	char tmpbuf[PORTNAME_SIZE+1];

	memcpy(tmpbuf,port,PORTNAME_SIZE);
	tmpbuf[PORTNAME_SIZE]=(char)0;
	sprintf(path,"%s/MSG_%s",msgdir,tmpbuf);
#endif	
#ifdef WIN32
	const char *msgdir = wispmsgsharedir(NULL);
	long portval;

	memcpy(&portval, port, PORTNAME_SIZE);
	sprintf(path,"%s\\M_%08X.mkey",msgdir,portval);
#endif
	if (!fexists(msgdir))
	{
		/* 
		**	If the PRB directory doesn't exist then create it here.
		**	(This is kind of sloppy but any error will be picked up later when we go to use it.)
		*/
		makepath(path);
	}
	
	WL_wtrace("MESSAGE","KEYFILE","Port=[%4.4s] Keyfile=[%s]",port,path);
}

/*
**	Routine:	portownergid()
**
**	Function:	Get the gid of the owner of the port.
**
**	Description:	This routine reads the PORT key file which contains
**			the GID of the owner of the port and returns it.
**
**	Arguments:
**	port		The 4 char port name.
**
**	Globals:	None
**
**	Return:		The GID from the port key file (Owner's gid) or
**			-1 if an error.
**
**	Warnings:	None
**
**	History:	
**	11/30/93	Written by GSL
**
*/
static int portownergid(char port[PORTNAME_SIZE])
{
	char	path[256];
	FILE	*keyfile;
	int	gid;

	makemsgkeyfile(port,path);

	gid = -1;

	if ( fexists(path) )
	{
		keyfile=fopen(path,FOPEN_READ_TEXT);
		if( !keyfile )
		{
			return( -1 );
		}
		fscanf(keyfile,"%d\n",&gid);
		fclose(keyfile);
		return gid;
	}
	return -1;
}

static int functype(char* func)
{
	if      (0==strcmp(func,"CR")) return M_CREATE;
	else if (0==strcmp(func,"DE")) return M_DELETE;
	else if (0==strcmp(func,"XM")) return M_XMIT;
	else if (0==strcmp(func,"XW")) return M_XMTWT;
	else if (0==strcmp(func,"CH")) return M_CHECK;
	else			       return M_INV;
}


/*
**	History:
**	$Log: message.c,v $
**	Revision 1.44  2009/10/18 20:45:57  gsl
**	fix windows warnings
**	
**	Revision 1.43  2003/01/31 18:25:18  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.42  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.41  2003/01/21 14:46:14  gsl
**	Change to use stdarg.h
**	
**	Revision 1.40  2002/12/11 17:03:06  gsl
**	use wisp_unlink()
**	
**	Revision 1.39  2002/12/10 20:54:13  gsl
**	use WERRCODE()
**	
**	Revision 1.38  2002/12/09 19:15:32  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.37  2002/10/04 21:00:55  gsl
**	Change to use WL_stat_xxx() routines
**	
**	Revision 1.36  2002/07/16 16:24:54  gsl
**	Globals
**	
**	Revision 1.35  2002/07/12 19:10:13  gsl
**	Global unique WL_ changes
**	
**	Revision 1.34  2002/07/12 17:00:58  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.33  2002/07/11 20:29:11  gsl
**	Fix WL_ globals
**	
**	Revision 1.32  2002/07/10 21:05:20  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.31  2002/07/02 21:15:26  gsl
**	Rename wstrdup
**	
**	Revision 1.30  2001/10/31 20:31:19  gsl
**	replace mkdir() with makepath()
**	
**	Revision 1.29  2001-10-02 13:13:42-04  gsl
**	NOTE: Message still checks by the second.
**	Does a one second WSXIO() for a timed check.
**	Fixing would require vrawtimeout() of finer then one second
**
**	Revision 1.28  2001-09-27 11:06:26-04  gsl
**	Change to check for message every 1/100th of a second
**
**	Revision 1.27  2001-09-27 10:07:33-04  gsl
**	Remove VMS & MSDOS
**
**	Revision 1.26  1998-12-09 09:39:31-05  gsl
**	Use FOPEN mode defines
**
**	Revision 1.25  1998-10-28 11:18:21-05  gsl
**	Added argument error checking and tracing
**
**	Revision 1.24  1997-12-04 18:12:59-05  gsl
**	changed to wispnt.h
**
**	Revision 1.23  1997-10-17 17:00:10-04  gsl
**	Add nativescreens() logic
**	Replace the GETBIN()s with WL_get_swap()
**
**	Revision 1.22  1997-05-21 08:30:03-04  gsl
**	fix a warning message
**
**	Revision 1.21  1997-05-19 13:58:21-04  gsl
**	Fix for WIN32
**	Prototyped all the routines.
**	Add WL_wtrace() call
**	Fix to use #defines instead of hardcoded numbers
**	Fix the creation of the key file for WIN32
**
**	Revision 1.20  1997-02-17 10:40:12-05  gsl
**	Fix initialization of const var msgdir
**
**	Revision 1.19  1996-11-11 17:48:14-05  jockc
**	a few small changes for message.c
**
**	Revision 1.18  1996-11-11 09:04:25-08  jockc
**	changed WIN32 to use wispmsgsharedir() for message file dir
**
**	Revision 1.17  1996-11-04 17:08:25-08  gsl
**	Fix the order of the include files and move them all
**	to the top of the file
**
**	Revision 1.16  1996-10-30 15:11:40-08  jockc
**	change mkdir call back to unix style.. redef is in win32std.h
**
**	Revision 1.15  1996-10-15 16:12:10-07  jockc
**	support for windows 32.. reused as much existing code as
**	possible by emulating the unix message queues with win32msg.c ..
**
**	Revision 1.14  1996-08-23 14:02:41-07  gsl
**	Changed to use wispprbdir()
**
**	Revision 1.13  1996-08-19 15:32:31-07  gsl
**	drcs update
**
**
**
*/
