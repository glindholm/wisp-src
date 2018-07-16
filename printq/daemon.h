/*
 * Header:  daemon.h
 * Program: IDSIprint
 * Purpose: primary header for scheduler
 *      Id: $Id:$
 *
 * $Log: daemon.h,v $
 * Revision 1.15  1992/08/06  18:17:38  jockc
 * added global pagecount
 *
 * Revision 1.14  1992/06/25  18:21:23  jockc
 * added formfeed_enc floag
 *
 * Revision 1.13  1992/06/18  23:48:11  jockc
 * remove decl of timedout
 *
 * Revision 1.12  1992/06/09  00:05:05  jockc
 * *** empty log message ***
 *
 * Revision 1.11  1992/04/30  18:54:26  jockc
 * added dump flag for cfg, cfg struct,  progname ptr for NICEPS
 * ..
 *
 * Revision 1.10  1992/03/27  22:54:20  jockc
 * add last_pvalidate for printer validation stuff, changed
 * last_validate to last_qvalidate.. should probably combine
 * these two vars
 *
 * Revision 1.9  1992/02/13  23:33:03  jockc
 * added classdef and formdef timestamp variables
 *
 * Revision 1.8  1991/12/17  22:37:34  jockc
 * validate stuff
 *
 * Revision 1.7  1991/10/08  23:32:23  jockc
 * add unspooling flag
 *
 * Revision 1.6  1991/10/01  17:06:42  jockc
 * support class, form stuff. printer dumping.
 * simpler comm and client handling
 *
 * Revision 1.5  1991/08/07  18:36:19  jockc
 * added flag to indicate a queue change requires a dump of the queue
 * "need_to_dump"
 *
 * Revision 1.4  1991/06/27  18:25:36  jockc
 * add struct decls for sockaddr_un
 *
 * Revision 1.3  1991/04/23  19:08:39  jockc
 * lite cleanup
 *
 * Revision 1.2  1991/04/19  00:47:40  jockc
 * *** empty log message ***
 *
 * Revision 1.1  1991/04/18  23:49:59  jockc
 * Initial revision
 *
 *
 */
#ifndef PQDAEMON
#define PQDAEMON

#define DEBUG

#include "defs.h"

#define MONITOR 0x01

#ifdef _DAEMON_MAIN_
int init_mq=FALSE;
int init_cfg=FALSE;
int init_paths=FALSE;

time_t last_qvalidate=0;
time_t last_pvalidate=0;
#define VALIDATE_GRANULARITY 30

unsigned int queue_id;

char printcap_path[256];
int found_printcap=FALSE;
int loaded_printcap=FALSE;
time_t printcap_time=0;
time_t formdef_time=0;
time_t classdef_time=0;
time_t ilpdef_time=0;
time_t q_file_time=0;

int status;
int unspooler;

int need_to_dump=FALSE, need_to_dump_p=FALSE, need_to_dump_cfg=FALSE;

char *errmsg[]=
{
	"ok",
	"error during msgget for deletion",
	"error deleting leftover message queue",
	"error creating message queue: systemwide limit exceeded",
	"error creating message queue",
	NULL
};


PLIST *p_head, *p_ptr;
QLIST *q_head, *q_tail, *q_ptr, *ready_head, *r_ptr;
FLIST *f_head, *f_ptr;
ULIST *u_head, *u_ptr;

QITEM Queue[QMAXSIZE],*top_item, *qitem_ptr;

char forms_cfg_path[128];
char prt_cfg_path[128];
char spool_dir[128];

FILE *debug=NULL;
int timedout;
FILE *errlog=NULL;

#ifdef SOCKCODE
int master_d = -1;
struct sockaddr_un my_addr,from;
#endif

#ifdef MQCODE
int msgid = -1;
#endif

char *messbuf=NULL;

QITEM *qitemrq;
PITEM *prtrq,*prtsnd;
FITEM *fitemp;
PACKET *packetrq;

CLASSINFO class_info;
MASTCONFIG qconfig;

struct ipcapstruct printers[MAXPRTRS+1],ipcap,*pcapptr;

/* signal flags*/
int broken_pipe;
int unspooling;
int pagecount;

char *progname_ptr;

#else

extern int pagecount;
extern MASTCONFIG qconfig;
extern char *progname_ptr;

extern int unspooling;

extern int init_mq;
extern int init_cfg;
extern int init_paths;

extern char printcap_path[];
extern int found_printcap;
extern int loaded_printcap;
extern time_t printcap_time;
extern time_t formdef_time;
extern time_t classdef_time;
extern time_t ilpdef_time;

extern int need_to_dump, need_to_dump_p, need_to_dump_cfg;

extern time_t q_file_time;

extern int status;
extern int unspooler;

extern char *errmsg[];

extern PLIST *p_head, *p_ptr;
extern QLIST *q_head, *q_tail, *q_ptr, *ready_head, *r_ptr;
extern FLIST *f_head, *f_ptr;
extern ULIST *u_head, *u_ptr;

extern QITEM Queue[],*top_item;
extern char forms_cfg_path[];
extern char prt_cfg_path[];
extern char spool_dir[];
extern FILE *debug;


extern FILE *errlog;
extern char *messbuf;

extern QITEM *qitemrq;
extern PITEM *prtrq;
extern FITEM *fitemp;
extern PACKET *packetrq;

#ifdef SOCKCODE
extern int master_d;
extern struct sockaddr_un my_addr,from;
#endif

#ifdef MQCODE
extern int msgid;
#endif

extern CLASSINFO class_info;

#endif


#endif
