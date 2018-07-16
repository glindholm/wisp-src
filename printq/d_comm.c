/*
 * Module:  d_comm
 * Program: IDSIprint
 * Purpose: handle communication between scheduler daemon and client progs
 *
 */
static char copyright[] = "Copyright 1991,1992,1993 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

#define DAEMON_IO
#define EXT extern 
#include "daemon.h"

#ifdef SOCKCODE
static int master_d = -1;
static struct sockaddr_un my_addr_u,from_u;
static struct sockaddr_in my_addr_i,from_i;
#endif

#ifdef MQCODE
static int msgid = -1;
#endif

/*#define DBGREADSOCK*/
/*#define DBGCLIENT*/


extern int ring_add (ring_struct *ring_ptr, int position, char *element);
extern int ring_remove (ring_struct *ring_ptr, int position, char *element);

int receive_rq(int *pending)
{
#ifdef SOCKCODE
	struct timeval to;
	static fd_set rfds;
	int newsd,alen;
	struct sockaddr *addr;
	char *gmem(int cnt, int sz);	
	int findcli(int *d1, int *d2);
	int clipos,clicnt,value,srchcnt;
	static int last_client_read = 0;
	int savepending;
#endif
#ifdef MQCODE
	if (msgid == -1)
	  init_comm();

	if (msgselect(msgid))
	{
		readmq(msgid,(char*)packetrq,PACKET_SIZE);
		return msgid;
	}
	sleep(2);
#endif

	
#ifdef SOCKCODE

	savepending = *pending;

	ring_count(clients,&clicnt);
		
# ifdef DBGCLIENT
	if (!clients)
	{
		logerr("no clients\n");
	}
	else
	{
		for (clipos=0;clipos<clicnt;++clipos)
		{
			ring_get(clients,clipos,&value);
			sprintf(errmsg,"client #%d: socket descriptor %d\n",clipos,value);
			logerr(errmsg,0);
		}
	}
# endif
	if (master_d == -1)				/* need to init communications? */
	{
		init_comm();					/* get a generic unidirectional channel */
	}

	if (*pending <= 0)
	{
		int maxfd;
		
		build_rfds(&rfds,&maxfd);
		to.tv_sec = 2;					/* setup for timeout */
		to.tv_usec = 0;

		/* FDSETCAST defined in defs.h */
		*pending = select(maxfd+1,FDSETCAST &rfds,NULL,NULL,&to);    /* check for incoming business. */
# ifdef DBGCLIENT
		logerr("select returned %d\n",*pending);
# endif
		if (*pending == -1)		/* ouch */
		{
			if (errno != EINTR)
			  logerr("receive_rq: select: errno %d\n",errno);
			
			return -1;
		}
	}
# ifdef DBGCLIENT
	else
	{
		logerr("in receive_rq.. %d fd's remaining\n",*pending);
	}
# endif	

	if (*pending && FD_ISSET(master_d,&rfds))
	{
		if (qconfig.inet_address==0)
		{
			addr=(struct sockaddr *)&from_u;
			alen=sizeof(from_u);
		}
		else
		{
			addr=(struct sockaddr *)&from_i;
			alen=sizeof(from_i);
		}
# ifdef DBGCLIENT
		logerr("reading master at %d\n",master_d);
# endif
		newsd = accept(master_d, addr, &alen);
		if (newsd== -1)
		{
			logerr("receive_rq: accept: errno %d\n",errno);
			return -1;
		}
# ifdef DBGCLIENT
		logerr("got new client at %d\n",newsd);
# endif
		ring_add(clients,-1,(char*)&newsd);
		FD_CLR(master_d,&rfds);
		--(*pending);
	}
# ifdef DBGCLIENT
	logerr("clicnt is %d\n",clicnt);
# endif
	if (*pending && clicnt)
	{
		clipos = last_client_read + 1;
		if (clipos >= clicnt)
		{
			clipos=0;
		}
		for ( srchcnt=clicnt; srchcnt ; --srchcnt)
		{
			ring_get(clients,clipos,(char*)&value);
# ifdef DBGCLIENT
			logerr("checking client %d\n",value);
# endif
			if (FD_ISSET(value,&rfds))
			{
				FD_CLR(value,&rfds);
				--(*pending);
# ifdef DBGCLIENT				
				logerr("client %d has data\n",value);
# endif				
				last_client_read = clipos;

				if (readsocket(value,(char*)packetrq,PACKET_SIZE)>0)
				{
					return value;
				}
				else
				{
					close(value);
# ifdef DBGCLIENT
					logerr("client chopped\n");
# endif
					ring_remove(clients,clipos,NULL);
					--clicnt;
					return -1;
				}
			}
			++clipos;
			if (clipos >= clicnt)
			{
				clipos = 0;
			}
		}
	}
	if (*pending && *pending == savepending)
	{
		*pending=0;
	}
#endif
	return -1;
}
int findcli(int *d1, int *d2)
{
	return (*d2)-(*d1);
}

void init_comm(void)
{
#ifdef MQCODE
	key_t msgkey, myftok();
	int fd;
#endif
#ifdef SOCKCODE
	int on=1;
	
	if (master_d == -1)
	{
		if (qconfig.inet_address==0)
		{
			if ((master_d=socket(AF_UNIX,SOCK_STREAM,0))== -1)
			{
				logerr("init_comm: socket: errno %d\n",errno);
				daemon_shutdown(1);
			}
			my_addr_u.sun_family=AF_UNIX;
			strcpy(my_addr_u.sun_path,socketpath);
			if (bind(master_d,(struct sockaddr*)&my_addr_u,
				 SUN_LEN(&my_addr_u))<0)
			{
				logerr("init_comm: bind: errno %d\n",errno);
				daemon_shutdown(1);
			}				
			chmod(socketpath,0777);
			listen(master_d,5);
		}
		else
		{
			if ((master_d=socket(AF_INET,SOCK_STREAM,0))== -1)
			{
				logerr("init_comm: socket: errno %d\n",errno);
				daemon_shutdown(1);
			}
			memset(&my_addr_i,0,sizeof(my_addr_i));
			my_addr_i.sin_family      = AF_INET;
			my_addr_i.sin_port        = htons(qconfig.inet_address);
			my_addr_i.sin_addr.s_addr = htonl(INADDR_ANY);
			if (bind(master_d,(struct sockaddr *)&my_addr_i,sizeof(my_addr_i))<0)
			{
				logerr("init_socket: bind: errno %d\n",errno);
				daemon_shutdown(1);
			}				
			listen(master_d,5);
		}
		if (ioctl(master_d,FIONBIO,&on)<0)
		{
			logerr("init_comm: ioctl [socket]: errno %d\n",errno);
			daemon_shutdown(1);
		}
		
		
	}
#endif
#ifdef MQCODE
	if (msgid == -1)
	{
		if (!access(mqpath,0))
		{
			msgkey = myftok(mqpath,KEYMOD);
			msgid = msgget(msgkey,0);
			if (msgid != -1) if (msgctl(msgid,IPC_RMID,0)<0) perror("msgctl");
		}
		if ((fd=creat(mqpath,0777))<0)
		{
			logerr("error creating key file %d\n",errno);
			daemon_shutdown(1);
		}
		else close(fd); /* close the fd as we don't need it */
		chmod(mqpath,0777);
		msgkey = myftok(mqpath,KEYMOD);
		if ((msgid = msgget(msgkey,IPC_CREAT|0666))<0)
		{
			logerr("error getting msgid %d\n",msgid);
			daemon_shutdown(1);
		}
	}
#endif	
}
void shut_comm(void)
{
#ifdef SOCKCODE
	close(master_d);
#endif
#ifdef MQCODE
	msgctl(msgid,IPC_RMID,0);
#endif
}

#ifdef SOCKCODE
int readsock_to=0;
void readsock_timeout(int dummy)
{
	++readsock_to;
}
int readsocket(int fd, char *ptr, int size)
{
	int cnt,tot;
	int fromlen;
	extern char *sys_errlist[];
	
	signal(SIGALRM,readsock_timeout);
	alarm(TIMEOUT);
	readsock_to = tot = 0;

	while (tot < size)
	{
		cnt = read(fd, ptr+tot, size-tot);
#ifdef DBGREADSOCK
		logerr("read %d bytes\n",cnt);
#endif
		if (cnt== -1)
		{
			logerr("error reading socket: %s\n",sys_errlist[errno]);
			alarm(0);
			return -1;
		}
		if (cnt==0)
		{
			alarm(0);
			return -1;
		}
		tot+=cnt;
	}
	alarm(0);

	if (readsock_to) 
	{ 
		logerr("timeout reading packet on socket. wanted %d bytes\n",size); 
		readsock_to=0; 
	}

	return tot;
}
void build_rfds(fd_set *fds, int *fdmax)
{
	int clipos, clicnt,value,max= -1;
	FD_ZERO(fds);
	FD_SET(master_d,fds);
	if (master_d>max)
	{
		max=master_d;
	}
	ring_count(clients,&clicnt);
	
	if (clicnt)
	{
		for (clipos = 0; clipos < clicnt; ++clipos)
		{
			ring_get(clients,clipos,(char*)&value);
			
			FD_SET( value, fds );
			if (value > max)
			{
				max=value;
			}
		}
	}
	*fdmax=max;
}
#endif

#ifdef MQCODE
int readmq(msgid,buf,size)
int msgid,size;
char *buf;
{
	static struct 
	{ 
		int type; 
		char data[PACKET_SIZE]; 
	} msgbuff;
	int recsz;
	
	recsz = msgrcv(msgid,&msgbuff,size,1,0);
	if (recsz>0)
	  memcpy(buf,msgbuff.data,recsz);
	return msgbuff.type;
}
int msgselect(desc) /* select for msg queues */
int desc;
{
	struct msqid_ds msgbuf;
	
	msgctl(desc,IPC_STAT,&msgbuf);
	if (msgbuf.msg_qnum) return TRUE;
	else return FALSE;
}
#endif

/*
 * $Log: d_comm.c,v $
 * Revision 1.28  1993/11/11  23:32:39  jockc
 * fix var mispelling
 *
 * Revision 1.27  1993/11/11  23:20:59  jockc
 * added kludge fix for when DG select returns bogus values
 *
 * Revision 1.26  1993/08/13  20:42:30  jockc
 * cast fdsets to int * for HPUX
 *
 * Revision 1.25  1993/08/13  18:34:22  jockc
 * changes for compilation under c89
 *
 * Revision 1.24  1993/06/02  23:34:39  root
 * change to allow fd > 20 in select statement
 *
 * Revision 1.23  1993/05/28  21:43:19  jockc
 * reworked client list scheme.  uses ring.c now
 *
 * Revision 1.22  1993/05/14  23:05:37  jockc
 * clear out the alarm signal if the read(socket) returns 0 (client shut down)
 *
 * Revision 1.21  1993/04/27  17:22:42  jockc
 * fixed buggy client processing
 *
 * Revision 1.20  1993/03/26  06:35:39  jockc
 * mod the debug code slightly
 *
 * Revision 1.19  1993/03/25  23:46:01  jockc
 * improved client handling and request processing.
 *
 * Revision 1.18  1993/01/12  02:02:54  jockc
 * reverted to connectioned protocol for flow control
 * purposes.  went back to client list, although it's much
 * simpler now.
 *
 * Revision 1.17  1992/12/31  23:36:28  jockc
 * change for qpath.  added SUN_LEN for compat with
 * BSD 4.2 and 4.3 ...
 *
 * Revision 1.16  1992/10/27  23:28:38  jockc
 * added internet addressing stuff
 *
 * Revision 1.15  1992/05/20  23:14:08  jockc
 * small change to a comment
 *
 * Revision 1.14  1992/04/30  18:51:01  jockc
 * cast 2nd arg of signal (gcc gave warning)
 *
 * Revision 1.13  1992/01/06  19:37:02  jockc
 * add sleep(2) to mqcode main loop to lessen cpu usage
 *
 * Revision 1.12  1991/10/01  17:03:41  jockc
 * removed unspooler processing from receive_rq function.
 * removed all client processing junk.  simplified
 * communications, and support mq or socket transport
 *
 * Revision 1.11  1991/09/12  03:23:17  jockc
 * fixed error printer un-hold still printer phenomenon
 *
 * Revision 1.10  1991/08/23  18:50:24  jockc
 * re added printer error stuff.  when errmsg came from unsp process,
 * handler code was lost.
 *
 * Revision 1.9  1991/08/03  00:03:04  jockc
 * fixed bogus wait on client shutdown, fixed lost client list if head node gets cut
 *
 * Revision 1.8  1991/07/18  00:05:35  jockc
 * fixed unix domain addr size in bind call
 *
 * Revision 1.7  1991/06/27  18:23:59  jockc
 * add support for UNIX DOMAIN sockets
 * add readio layer to socket read to allow for
 * partial reads
 *
 * Revision 1.6  1991/06/04  15:25:59  jockc
 * front ended read() to allow for slow network incomplete reads
 *
 * Revision 1.5  1991/05/02  19:30:41  jockc
 * check for errno==EINTR in case the select() call is interrupted
 * for some reason.  Actually, this was done to allow SIGCLD to
 * be caught when children exited.  The scheme for child waiting
 * was changed, but this code might as well stay
 *
 * Revision 1.4  1991/04/30  23:30:23  jockc
 * misc stuff:
 * daemon correctly handles form specs in iprintcap that are
 * numeric (yacc grammar expected alpha).  No delay in wisp
 * screen manager at startup. also reentry into ilpwisp routines
 * are now ok
 *
 * Revision 1.3  1991/04/30  17:43:56  jockc
 * misc cleanup
 *
 * Revision 1.2  1991/04/19  00:47:14  jockc
 * *** empty log message ***
 *
 * Revision 1.1  1991/04/18  23:49:26  jockc
 * Initial revision
 *
 *
 */
