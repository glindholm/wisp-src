/*
 * Module:  c_comm.c
 * Program: IDSIprint
 * Purpose: handle communication between scheduler daemon and client progs
 * 
 */
static char copyright[] = "Copyright 1991,1992,1993 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

#if 0
#define DO_ACKS
#endif
#define EXT extern 
#include "c_comm.h"
#include "qpaths.h"

int broken_pipe = FALSE;


int init_comm (void);
int write_d (int descriptor, char *buf, int size);
extern int issocket (void);

send_daemon(int type, int what, char *ptr, int size, time_t time_stamp)							/* send message to daemon */
         
         
          
         
                  
{
	int len,wrtcnt,to;
	int total_sent;
	time_t time(time_t *);
	
	char *gmem(int cnt, int sz);
	
	if (messbuf==NULL)
	{
		messbuf=gmem(1,PACKET_SIZE);
		packet = (PACKET *)messbuf;
	}
	if (daemon_d == -1)
	{
		init_comm();
	}		     

	packet->magic = (int4)PACKETMAGIC;
	packet->version = (int4)PACKETVERSION;

	packet->functype   = type;
	packet->sender_pid = mypid;
	packet->sender_uid = myuid;
	packet->data.q.time_stamp = time_stamp;
	packet->aux = what;
	len=PACKET_SIZE;
	if (ptr) 
	{
		memcpy(&(packet->data),ptr,(size_t)size);
		len = PACKET_SIZE;
	}
	to=0;
	total_sent=0;
	for (total_sent=0;  total_sent<len; total_sent += wrtcnt)
	{
		wrtcnt=write_d(daemon_d,messbuf+total_sent,len-total_sent);
		if (wrtcnt<0) return;
	}

}
init_comm(void)				/* initialize ipc connection to */
{					/* host contained in daemon_host[] */
	int status;

#ifdef MQCODE
	key_t msgkey, myftok();
#endif

#ifdef SOCKCODE
	int retry_cnt, connect_status;
	extern struct sockaddr_un daemon_addr_u;
	extern struct sockaddr_in daemon_addr_i,cli_addr_i;
#endif

#ifdef SOCKCODE
	if (!issocket())
	  return IERR_INCOMPATDS;
#endif	
#ifdef MQCODE
	if (!ismqcode())
	  return IERR_INCOMPATDM;
#endif

	if (daemon_d>=0) return INIT_OK;
	if (mypid == -1) mypid = getpid();
	if (myuid == -1) myuid = getuid();

#ifdef SOCKCODE
	if (qconfig.inet_address==0)
	{
		if ((daemon_d=socket(AF_UNIX,SOCK_STREAM,0))== -1)
		{
			return IERR_SOCKET | errno<<8;
		}
		daemon_addr_u.sun_family = AF_UNIX;
		strcpy(daemon_addr_u.sun_path,socketpath);

		for (retry_cnt=RETRIES, connect_status= -1; retry_cnt && connect_status== -1; )
		{
			connect_status = connect(daemon_d,(struct sockaddr *)&daemon_addr_u,
						 SUN_LEN(&daemon_addr_u));
			if (connect_status == -1 && errno != ECONNREFUSED)
			{
				close(daemon_d);
				return IERR_BIND | errno<<8;
			}
			else if (connect_status == -1 && errno == ECONNREFUSED)
			{
				--retry_cnt;
				sleep(SLEEP_INTERVAL);
			}
		}
		if (connect_status == -1)
		{
			close(daemon_d);
			return IERR_BIND | ECONNREFUSED<<8;
		}
	}
	else
	{
		if ((daemon_d=socket(AF_INET,SOCK_STREAM,0))== -1)
		{
			return IERR_SOCKET | errno<<8;
		}
		memset(&daemon_addr_i,0,sizeof(daemon_addr_i));
		daemon_addr_i.sin_family      = AF_INET;
		daemon_addr_i.sin_port        = htons(qconfig.inet_address);
		daemon_addr_i.sin_addr.s_addr = htonl(INADDR_ANY);
		cli_addr_i.sin_family      = AF_INET;
		cli_addr_i.sin_port        = htons(0);
		cli_addr_i.sin_addr.s_addr = htonl(INADDR_ANY);

		for (retry_cnt=RETRIES, connect_status= -1; retry_cnt && connect_status== -1; )
		{
			connect_status = connect(daemon_d,(struct sockaddr *)&daemon_addr_i,
						 sizeof(daemon_addr_i));
			if (connect_status == -1 && errno != ECONNREFUSED)
			{
				close(daemon_d);
				return IERR_BIND | errno<<8;
			}
			else if (connect_status == -1 && errno == ECONNREFUSED)
			{
				--retry_cnt;
				sleep(SLEEP_INTERVAL);
			}
		}
		if (connect_status == -1)
		{
			close(daemon_d);
			return IERR_BIND | ECONNREFUSED<<8;
		}
#ifdef OLD
		if (bind(daemon_d,(struct sockaddr *)&cli_addr_i,sizeof(cli_addr_i))<0)
		{
			return IERR_BIND | errno<<8;
		}				
#endif
	}
	broken_pipe = FALSE;
	comm_status = 0;
	return INIT_OK;
#endif /*SOCKCODE*/
#ifdef MQCODE

	msgkey=myftok(mqpath,KEYMOD);
	
	if ((daemon_d = msgget(msgkey,0))<0)
	{
		perror("msgget");
		
		return errno;
	}
	else return INIT_OK;
#endif
}
shut_comm(void)
{
#ifdef SOCKCODE
	close(daemon_d);
	daemon_d = -1;
#endif
#ifdef MQCODE
	/* probably nothing needed here */
#endif
}
write_d(int descriptor, char *buf, int size)
{
#ifdef SOCKCODE
	int cnt,tot;

	tot=cnt=0;
	while (tot < size)
	{
		cnt = write(descriptor, buf+tot, size-tot);
		if (cnt== -1)
		{
			comm_status=errno;
			return cnt;
		}
		tot+=cnt;
	}
	comm_status=0;
	return tot;
#endif
#ifdef MQCODE
	extern struct msgstruct msgbuff;
	int cnt;
	
	memcpy(msgbuff.data,buf,size);
	msgbuff.type = 1;
	cnt=msgsnd(descriptor,&msgbuff,size,0);
	if (cnt<0)
	  comm_status=errno;
	else
	  comm_status=COMM_OK;
	return cnt==0? size: 0;
#endif
}
void link_shutdown(int dummy)
{
	broken_pipe=TRUE;
}
#if 0
ck_daemon_pid()
{
	FILE *pid;
	int stat,ipid;
	char buf[20];
	
	pid=fopen(DAEMONPID,"r");
	if (!pid) return 0;
	fgets(buf,9,pid);
	ipid=atoi(buf);
	if (!ipid) return 0;
	stat=kill(ipid,0);
	if (stat == -1) return 0;
	else return 1;
}
#endif
/*
 * $Log: c_comm.c,v $
 * Revision 1.28  1993/09/13  15:28:10  jockc
 * 499
 * 503
 *
 * Revision 1.28  1993/09/10  18:44:12  jockc
 * clear of comm_status
 *
 * Revision 1.27  1993/08/13  18:32:25  jockc
 * protoize and start to improve comm handling.. still need to relink
 * if daemon stops and starts
 *
 * Revision 1.26  1993/05/28  21:41:45  jockc
 * need to change gmem args slightly
 *
 * Revision 1.25  1993/03/26  06:34:37  jockc
 * implement retry mechanism for "connection refused" on connect()
 * which is caused by too many pending connect requests.. which is
 * caused (hopefully) by the system being too busy.
 *
 * Revision 1.24  1993/01/12  02:01:32  jockc
 * went back to connection oriented comm.  needed the flow
 * control; using connectionless, packets were sometimes lost.
 * also using the new semaphore instead of the old lockfile
 *
 * Revision 1.23  1992/12/31  23:35:50  jockc
 * changed to support new qpath scheme
 *
 * Revision 1.22  1992/10/27  23:26:16  jockc
 * added support for internet addressing
 *
 * Revision 1.21  1992/10/09  18:22:25  jockc
 * changed the names of the locking #defines
 *
 * Revision 1.20  1992/05/20  23:13:40  jockc
 * set magic num and version on packet send
 *
 * Revision 1.19  1991/10/11  23:28:04  jockc
 * took out ck_daemon_pid
 *
 * Revision 1.18  1991/10/11  22:42:27  jockc
 * write_d not returning count fixed.  accidentally chopped
 * on last delta
 *
 * Revision 1.17  1991/10/11  21:59:15  jockc
 * add new check for daemon
 *
 * Revision 1.16  1991/10/11  19:22:47  jockc
 * set global if comm fails instead of exit with message
 *
 * Revision 1.15  1991/10/11  00:31:35  jockc
 * fixup crash on memcpy in send_daemon.. pitem smaller than qunion
 * screwed us up
 *
 * Revision 1.14  1991/10/10  17:15:04  jockc
 * fixed mispelled incomptdm
 *
 * Revision 1.13  1991/10/04  20:47:52  jockc
 * fixup ret code if daemon not running or incompatible
 *
 * Revision 1.12  1991/10/01  17:03:03  jockc
 * changes to simplify communications and
 * support for Message queues or sockets
 *
 * Revision 1.11  1991/07/18  00:05:55  jockc
 * fixed unix domain addr size in connect cal
 *
 * Revision 1.10  1991/06/27  18:23:31  jockc
 * add support for UNIX DOMAIN sockets
 *
 * Revision 1.9  1991/06/03  22:59:08  jockc
 * fix rcv_daemon partial read bug
 *
 * Revision 1.7  1991/05/02  19:29:19  jockc
 * change COMMUNICATION lock (it meant that communication was taking
 * place) to RDY4RQ.. which means that the dameon is ready to
 * accept a request, ie is in the select system call
 *
 * Revision 1.6  1991/04/30  23:30:21  jockc
 * misc stuff:
 * daemon correctly handles form specs in iprintcap that are
 * numeric (yacc grammar expected alpha).  No delay in wisp
 * screen manager at startup. also reentry into ilpwisp routines
 * are now ok
 *
 * Revision 1.5  1991/04/30  17:43:54  jockc
 * misc cleanup
 *
 * Revision 1.4  1991/04/23  22:10:03  jockc
 * init_tcp now returns status instead of printing errmsg and exit
 *
 * Revision 1.3  1991/04/19  00:47:11  jockc
 * *** empty log message ***
 *
 * Revision 1.2  1991/04/19  00:24:44  jockc
 * *** empty log message ***
 *
 * Revision 1.1  1991/04/18  23:49:07  jockc
 * Initial revision
 *
 *
 */
