/*
 * Header:  c_comm.h
 * Program: IDSIprint ipman
 * Purpose: defines for clients communication stuff
 *      Id: $Id:$
 *
 * $Log: c_comm.h,v $
 * Revision 1.8  1992/06/18  23:48:21  jockc
 *  remove decl of timedout
 *
 * Revision 1.7  1991/10/11  19:52:40  jockc
 * defined comm_status here
 *
 * Revision 1.6  1991/10/04  20:48:07  jockc
 * fixup ret code if daemon not running or incompatible
 *
 * Revision 1.5  1991/10/01  17:03:23  jockc
 * changes to simplify communications and
 * support for Message queues or sockets
 *
 * Revision 1.4  1991/05/22  17:04:50  jockc
 * new error code for bad hostname
 *
 * Revision 1.3  1991/04/23  22:10:40  jockc
 * added #defines for init error status return codes
 *
 * Revision 1.2  1991/04/19  00:47:38  jockc
 * *** empty log message ***
 *
 * Revision 1.1  1991/04/18  23:49:16  jockc
 * Initial revision
 *
 *
 */
#ifndef PQCLIENT
#define PQCLIENT

#include "defs.h"

#define IERR_SOCKET 1
#define IERR_SERVUNDEF 2
#define IERR_NODAEMON 3
#define IERR_CONNECT 4
#define IERR_HOSTADDR 5
#define IERR_INCOMPATDS 6
#define IERR_INCOMPATDM 7

#ifdef MQCODE
struct msgstruct
{
	int type;
	char data[PACKET_SIZE];
};
#endif

#define INIT_OK 0

#ifdef EXT
EXT PACKET *packet;
EXT char *messbuf;

EXT int daemon_d;

EXT char daemon_host[];
EXT int mypid;
EXT int myuid;
EXT int comm_status;
#ifdef MQCODE
EXT struct msgstruct msgbuff;
EXT int msg_data;


#ifdef SOCKCODE
EXT struct sockaddr_un daemon_addr;
#endif
#endif

#else
#ifdef SOCKCODE
struct sockaddr_un daemon_addr;
#endif

PACKET *packet;
char *messbuf= NULL;
int daemon_d= -1;
char daemon_host[32] = { 0 };
int mypid = -1;
int myuid = -1;
int comm_status=COMM_OK;
#ifdef MQCODE
struct msgstruct msgbuff;
int msg_data;
#endif
#endif


#endif
