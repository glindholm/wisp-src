/*
 * Module:  d_comm
 * Program: IDSIprint
 * Purpose: handle communication between scheduler daemon and client progs
 *
 * $Log: d_comm.c,v $
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
static char rcsid[] = "$Id:$";

#define DAEMON_IO
#define EXT extern 
#include "daemon.h"

receive_rq()
{
#ifdef SOCKCODE
	struct timeval to;
	fd_set rfds;
	int cnt;
#endif
	
#ifdef MQCODE
	if (msgid == -1)
	  init_comm();

	if (msgselect(msgid))
	{
		readmq(msgid,messbuf,PACKET_SIZE);
		return msgid;
	}
	sleep(2);
#endif

#ifdef SOCKCODE
	if (master_d == -1)				/* need to init communications? */
	  init_comm();					/* get a generic unidirectional channel */

	FD_ZERO(&rfds);
	FD_SET(master_d,&rfds);
	to.tv_sec = 2;					/* setup for timeout */
	to.tv_usec = 0;

	cnt=select(20,&rfds,NULL,NULL,&to);    /* check for incoming business */
	if (cnt== -1 && errno!=EINTR)		/* ouch */
	{
		logerr("receive_rq: select: errno %d\n",errno);
		daemon_shutdown(1);
	}
	
	if (cnt && FD_ISSET(master_d,&rfds))
	{
		readsocket(master_d,messbuf,PACKET_SIZE);
		return master_d;
	}
#endif
	return -1;
}
init_comm()
{
#ifdef MQCODE
	key_t msgkey, myftok();
	int fd;
#endif
#ifdef SOCKCODE
	int on=1;
	
	if (master_d == -1)
	{
		if ((master_d=socket(AF_UNIX,SOCK_DGRAM,0))== -1)
		{
			logerr("init_comm: socket: errno %d\n",errno);
			daemon_shutdown(1);
		}
		my_addr.sun_family=AF_UNIX;
		strcpy(my_addr.sun_path,SOCKETPATH);
		if (bind(master_d,(struct sockaddr*)&my_addr,sizeof(my_addr.sun_family)+strlen(SOCKETPATH))<0)
		{
			logerr("init_comm: bind: errno %d\n",errno);
			daemon_shutdown(1);
		}				
		chmod(SOCKETPATH,0777);
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
		if (!access(MQPATH,0))
		{
			msgkey = myftok(MQPATH,KEYMOD);
			msgid = msgget(msgkey,0);
			if (msgid != -1) if (msgctl(msgid,IPC_RMID,0)<0) perror("msgctl");
		}
		if ((fd=creat(MQPATH,0777))<0)
		{
			logerr("error creating key file %d\n",errno);
			daemon_shutdown(1);
		}
		else close(fd); /* close the fd as we don't need it */
		chmod(MQPATH,0777);
		msgkey = myftok(MQPATH,KEYMOD);
		if ((msgid = msgget(msgkey,IPC_CREAT|0666))<0)
		{
			logerr("error getting msgid %d\n",msgid);
			daemon_shutdown(1);
		}
	}
#endif	
}
shut_comm()
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
readsock_timeout()
{
	++readsock_to;
}
readsocket(fd,ptr,size)
int fd,size;
char *ptr;
{
	int cnt,tot;
	struct sockaddr_un from;
	int fromlen;
	
	signal(SIGALRM,(void*)readsock_timeout);
	alarm(TIMEOUT);
	readsock_to = 0;
	fromlen=sizeof(from);
	
	for (tot=0,cnt=0; tot < size && !readsock_to; )
	{
		cnt=recvfrom(fd,ptr+tot,size-tot,0,&from,&fromlen);
		tot+= cnt;
	}
	alarm(0);
	
	if (readsock_to) { logerr("timeout reading packet on socket. wanted %d bytes\n",size); readsock_to=0; }

	return tot;
}
#endif

#ifdef MQCODE
readmq(msgid,buf,size)
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
msgselect(desc) /* select for msg queues */
int desc;
{
	struct msqid_ds msgbuf;
	
	msgctl(desc,IPC_STAT,&msgbuf);
	if (msgbuf.msg_qnum) return TRUE;
	else return FALSE;
}
#endif

