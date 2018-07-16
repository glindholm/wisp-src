/*
 * Module:  c_comm.c
 * Program: IDSIprint
 * Purpose: handle communication between scheduler daemon and client progs
 * 
 * $Log: c_comm.c,v $
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
static char copyright[] = "Copyright 1991 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

#if 0
#define DO_ACKS
#endif
#define EXT extern 
#include "c_comm.h"

send_daemon(type,what,ptr,size)								/* send message to daemon */
int type;
int what;
char *ptr;
int size;
{
	int len,wrtcnt,to;
	int total_sent;
	
	char *gmem();
	
	if (messbuf==NULL)
	{
		messbuf=gmem(PACKET_SIZE);
		packet = (PACKET *)messbuf;
	}
	if (daemon_d == -1)
	{
		init_comm();
	}		     

	packet->magic = PACKETMAGIC;
	packet->version = PACKETVERSION;

	packet->functype   = type;
	packet->sender_pid = mypid;
	packet->sender_uid = myuid;
	packet->sender_station = ttyslot();
	packet->aux = what;
	len=PACKET_SIZE;
	if (ptr) 
	{
		memcpy(&(packet->data),ptr,size);
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
init_comm()				/* initialize ipc connection to */
{					/* host contained in daemon_host[] */
#ifdef MQCODE
	key_t msgkey, myftok();
#endif
#ifdef SOCKCODE
	extern struct sockaddr_un daemon_addr;
#endif

	if (!lockfile(LK_STAT,DAEMON_RUNNING))
	  return IERR_NODAEMON;
#if 0
	if (!ck_daemon_pid()) 
	  return IERR_NODAEMON;
#endif
#ifdef SOCKCODE
	if (!lockfile(LK_STAT,SOCKVERSION))
	  return IERR_INCOMPATDS;
#endif	
#ifdef MQCODE
	if (!lockfile(LK_STAT,MQVERSION))
	  return IERR_INCOMPATDM;
#endif

	if (daemon_d>=0) return INIT_OK;
	if (mypid == -1) mypid = getpid();
	if (myuid == -1) myuid = getuid();

#ifdef SOCKCODE
	if ((daemon_d=socket(AF_UNIX,SOCK_DGRAM,0))== -1)
	{
		return IERR_SOCKET | errno<<8;
	}
	daemon_addr.sun_family = AF_UNIX;
	strcpy(daemon_addr.sun_path,SOCKETPATH);

	return INIT_OK;
#endif /*SOCKCODE*/
#ifdef MQCODE

	msgkey=myftok(MQPATH,KEYMOD);
	
	if ((daemon_d = msgget(msgkey,0))<0)
	{
		perror("msgget");
		
		return errno;
	}
	else return INIT_OK;
#endif
}
shut_comm()
{
#ifdef SOCKCODE
	close(daemon_d);
	daemon_d = -1;
#endif
#ifdef MQCODE
	/* probably nothing needed here */
#endif
}
write_d(descriptor,buf,size)
int descriptor,size;
char *buf;
{
#ifdef SOCKCODE
	int st;
	extern struct sockaddr_un daemon_addr;
	
	st = sendto(descriptor,buf,size,0, 
		      (struct sockaddr*)&daemon_addr,
		      sizeof(daemon_addr.sun_family)+strlen(SOCKETPATH));
	if (st<0) 
	  comm_status=errno;
	else
	  comm_status=COMM_OK;
	return st;
	
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
