/*
 * Header:  daemon.h
 * Program: IDSIprint
 * Purpose: primary header for scheduler
 *      Id: $Id:$
 *
 */
#ifndef PQDAEMON
#define PQDAEMON

#define DEBUG

#include "defs.h"
#include "qpaths.h"


/* funcs in d_main */
EXFUN( int,              main,                  (int, char**));      
EXFUN( void,		 daemon_shutdown,       (int));
EXFUN( void,		 exitbug,               (int));
EXFUN( void,		 daemon,                (void));
EXFUN( void,		 queue_modified,        (void));
EXFUN( void,		 pstatus_changed,       (void));
EXFUN( void,		 service_rq,            (void));
EXFUN( void,		 reorder_q,             (void));
EXFUN( void,		 check_config_files,    (void));
EXFUN( void,		 validate_q,            (void));
EXFUN( void,		 validate_printers,     (void));
EXFUN( int,		 validate_qitem,	(struct q_item *));
EXFUN( int,		 validate_pitem,	(struct p_item *));
EXFUN( void,		 change,                (void));
EXFUN( void,		 queue,                 (void));
EXFUN( void,		 exception,             (int));
EXFUN( int,		 q_depth,               (void));
EXFUN( void,		 get_class_chars,       (struct q_item *));
EXFUN( void,		 allcaps,               (char *));
EXFUN( int,		 haspriv,               (int));

EXFUN( void,		 move_item,             (char *, struct q_item *, int));
EXFUN( void,		 addready,              (struct q_item *));
EXFUN( struct q_item *,  findqitem,             (int));
EXFUN( struct q_item *,  findritem,             (int));
EXFUN( struct q_item *,  findrdyqitem,          (char, char*, int));
EXFUN( int,		 findqueueitem,         (struct q_item **,struct q_item **));
EXFUN( void,		 removereadyqitem,      (struct q_item *));
EXFUN( void,		 removeqitem,           (struct q_item *));

EXFUN( struct p_item *,  findpitem,             (char *));
#if 0
EXFUN( struct p_item *,  findrdyprinter,        (char));
#endif
EXFUN( int,		 findprinter,           (struct p_item **,struct p_item **));
EXFUN( void,             addprtr,               (struct ipcapstruct *));

EXFUN( init_s *,         findinititem,          (struct f_item *, char *));
EXFUN( init_s *,         newinititem,           (struct f_item *, char *));
EXFUN( void,		 freeinits,             (init_s *));

EXFUN( void,	         addfitem,              (struct f_item *));
EXFUN( struct f_item *,	 findfitem,             (char *));
EXFUN( void,	         updatefitem,           (struct f_item *,struct f_item *));

EXFUN( void,		 freefilts,             (filt_s *));
EXFUN( filt_s *,         findfiltitem,          (struct f_item *, char *));
EXFUN( filt_s *,         newfiltitem,           (struct f_item *, char *));

EXFUN( int,		 getnuid,               (char *));
EXFUN( void,             new_op,                (struct master_config *, char*));
EXFUN( void,             check_class,           (char, int *, int *, int *, int *));
EXFUN( void,             sigpipe,               (int));

/* in d_misc.c */
EXFUN( time_t,           filetime,              (char *));
EXFUN( void,             reallogerr,            (char *));
EXFUN( char*,            gmem,                  (int, int));
EXFUN( int,              setflag,               (int, int));
EXFUN( int,              init_semaphore,        (void));
EXFUN( key_t,            getipckey,             (char *, char *, unsigned char));
/*key_t getipckey(char *path, char *type, unsigned char mod);*/

EXFUN( int,              chk_vers,              (int));
EXFUN( int,              chk_qvers,             (void));
EXFUN( int,              chk_pvers,             (void));
EXFUN( int,              chk_cvers,             (void));
EXFUN( int,              chk_dump,              (char *));
EXFUN( char*,            dosubst,               (char *, struct q_item *, int));
EXFUN( char*,            d_username,            (int));

/* in d_dump.c */
EXFUN( void,             dump_stuff,            (void));
EXFUN( void,             dump_q,                (void));
EXFUN( void,             dump_p,                (void));
EXFUN( void,             dump_conf,             (void));
EXFUN( void,             load_qdump,            (void));
EXFUN( void,             load_pdump,            (void));
EXFUN( void,             write_old_qdump_file,  (void));
EXFUN( int,              init_shmem,            (void));
EXFUN( char*,            getshseg,              (int, key_t));
EXFUN( void,             dump_q_shmem,          (void));
EXFUN( void,             dump_p_shmem,          (void));
EXFUN( void,             shut_shmem,            (void));

/* in d_comm.c */
EXFUN( int,              receive_rq,            (int *));
EXFUN( int,		 findcli,               (int *, int *));
EXFUN( void,             init_comm,             (void));
EXFUN( void,             shut_comm,             (void));
#ifdef SOCKCODE
EXFUN( void,             readsock_timeout,      (int));
EXFUN( int,              readsocket,            (int, char*, int));
EXFUN( void,             build_rfds,            (fd_set *, int *));
#endif

#ifdef MQCODE
EXFUN( int,              readmq,                (int, char*, int));
EXFUN( int,              msgselect,             (int));
#endif

/* in d_unspool.c */
EXFUN( void,             unspool,               (void));
EXFUN( void,             opento,                (int));
EXFUN( int,              q_unspool,             (struct q_item *));
EXFUN( void,             master_shutdown,       (int));
EXFUN( void,             abortinfo,             (struct q_item *, int));
EXFUN( void,             addulist,              (int, int, int, struct p_item *, struct q_item *));
EXFUN( char**,           banner,                (struct q_item *));
EXFUN( void,             build_filtercmd,       (char *, char *, struct q_item *, int));
EXFUN( ULIST *,          find_unspooler,        (char *));
EXFUN( void,             check_unspoolers,      (void));
EXFUN( void,             finished_unspooler,    (struct unspooler_list *));
EXFUN( int,              getinitstr,            (struct f_item *, struct p_item *, char **, int *, int *));
EXFUN( int,              myfgets,               (char *, int, FILE *, int, int));

/* in utils.c */
EXFUN( key_t,            myftok,                (char *, char));
EXFUN( void,             make_vers_date,        (char *, char*, char *));

/* in qpaths.c */
EXFUN( int,              initpaths,             (void));

#define ring_struct char
extern EXFUN( int,	 ring_open,	        (char **, int, int, int, int (*)(), int));
extern EXFUN( int,	 ring_close,		(ring_struct *));
extern EXFUN( int,	 ring_que,		(ring_struct *, char *));
extern EXFUN( int,	 ring_unque,		(ring_struct *, char *));
extern EXFUN( int,	 ring_front,		(ring_struct *, char *));
extern EXFUN( int,	 ring_back,		(ring_struct *, char *));
extern EXFUN( int,	 ring_count,		(ring_struct *, int *));
extern EXFUN( int,	 ring_get,              (ring_struct *, int, char *));

#define logerr(x,y) \
{\
 extern char errmsg[];\
 sprintf(errmsg,(x),(y));\
 reallogerr(errmsg);\
}

#define PRTDEFERR 0x1
#define FORMDEFERR 0x2
#define CLDEFERR 0x4
#define CONFERR 0x8

#define DBGCHKUNSP    0x00000001	      /* debug the check unspooler logic */
#define DBGUNSPOOL    0x00000002	      /* debug the unspoolers */
#define DBGUNSPOOLP   0x00000004            /* debug the new unspooler persistance logic */
#define DBGFILT       0x00000008            /* debug the filter logic */
#define DBGCHILD      0x00000010           /* debug general child behavior */
#define DBGCHILD2     0x00000020           /* debug child read/write loop */
#define DBGMAINPACK   0x00000040           /* debug master process packet handling */
#define DBGSIGCORE    0x00000080           /* don't trap signals, make a core file */
#define DBGVALIDATE   0x00000100
#define DBGREMOVEQ    0x00000200
#define DBGCLASSINFO  0x00000400
#define DBGFINDRDYQ   0x00000800
#define DBGRELOADFORM 0x00001000

#define DEBUGCODE
#define D(x) if (debug_flag & (x)) 

#ifdef OLD
struct optstruct
{
	char *opt;
	int type;
	char *optdest;
	int optval;
};

#define OPT_INT 1
#define OPT_STRING 2
#define OPT_BAD -1
#endif

#ifdef _DAEMON_MAIN_
char *dbgdesc[]=
{
	"0x0001     Check unspooler logic",
	"0x0002     Unspooler logic",
	"0x0004     Unspooler persist logic",
	"0x0008     Filter setup logic",
	"0x0010     General child logic",
	"0x0020     Child I/O logic",
	"0x0040     General master packet i/o logic",
	"0x0080     Generate core file on signals",
	"0x0100     Validate logic",
	"0x0200     Remove queue item logic",
	"0x0400     Class information logic",
	"0x0800     Find ready queue item logic",
	"0x1000     Forms reload logic",
	NULL
};

#ifdef OLD
int dinf=0;
char dpristr[100]={0};
char bogus[100];

struct optstruct optlist[]=
{
	{ "-dinf", OPT_INT, (char *)&dinf, TRUE },
	{ "-p", OPT_STRING, dpristr, 0 },
	{ "-B", OPT_STRING, bogus, 0 },
	{ NULL, 0, NULL, 0 }
};
#endif

time_t last_qvalidate=0;
time_t last_pvalidate=0;
#define VALIDATE_GRANULARITY 30

unsigned int queue_id;

time_t printcap_time=0;
time_t formdef_time=0;
time_t classdef_time=0;
time_t ilpdef_time=0;

int need_to_dump=FALSE, need_to_dump_p=FALSE, need_to_dump_cfg=FALSE;

char errmsg[1024] = {0};

#if 0
PLIST *p_head, *p_ptr;
QLIST *q_head, *q_tail, *q_ptr, *ready_head, *r_ptr;
ULIST *u_head, *u_ptr;
#endif
FLIST *f_head, *f_ptr;

char *clients=NULL;
char *unsplist=NULL;
char *readylist=NULL;
char *queuelist=NULL;
char *printerlist=NULL;
char *formlist=NULL;

FITEM *fitemp;
PACKET *packetrq=NULL;

CLASSINFO class_info;
MASTCONFIG qconfig;

#ifdef OLD
struct ipcapstruct printers[MAXPRTRS+1],ipcap,*pcapptr;
#endif
struct ipcapstruct yyprinter,ipcap,*pcapptr;

/* signal flags*/
int broken_upipe;
int unspooling;
int pagecount;
int fileerrs=0;

char *progname_ptr;

char *unspcmds[]=
{
	"INVALID--ZERO",
        "Done Printing",
	"Printer Error",
	"Form changed",
	"Align",
	"New Form",
	"Abort Print",
	"Stop Print",
	"Start Print",
	"Un Unspool",
	"Hold",
	"Print",
	"Unspooler Exiting",
	"Shutdown",
	NULL
};
char *auxstr[]=
{
	"INVALID--ZERO",
	"Align Printer",
	"Change Form",
	"Qitem Update",
	"Stop Print",
	"Enable Printer",
	"Disable Printer",
	"Start Print",
	"Printer Update",
	"Hold Item",
	"Unhold Item",
	"Delete Item",
	"Remove Item",
	"Clear Error",
	NULL
};

int4 debug_flag;

#else

extern char *dbgdesc[];

extern int4 debug_flag;

extern char *unspcmds[];
extern char *auxstr[];

extern int pagecount;
extern MASTCONFIG qconfig;
extern char *progname_ptr;

extern int unspooling;

extern time_t printcap_time;
extern time_t formdef_time;
extern time_t classdef_time;
extern time_t ilpdef_time;

extern char errmsg[];
extern int queue_id;

extern int need_to_dump, need_to_dump_p, need_to_dump_cfg;

#if 0
extern PLIST *p_head, *p_ptr;
extern QLIST *q_head, *q_tail, *q_ptr, *ready_head, *r_ptr;
extern ULIST *u_head, *u_ptr;
#endif

extern FLIST *f_head, *f_ptr;
extern int fileerrs;
extern char *clients;
extern char *unsplist;
extern char *readylist;
extern char *queuelist;
extern char *printerlist;
extern char *formlist;

extern FITEM *fitemp;
extern PACKET *packetrq;

extern CLASSINFO class_info;

#endif


#endif
/*
 * $Log: daemon.h,v $
 * Revision 1.25  1993/11/10  21:45:06  jockc
 * slight change to proto for ring_open.. sco didn't like ecompare
 *
 * Revision 1.24  1993/11/10  17:40:23  jockc
 * null change.
 *
 * Revision 1.23  1993/09/13  15:28:16  jockc
 * 499
 * 503
 *
 * Revision 1.23  1993/09/10  18:39:46  jockc
 * added debug flag descriptions, and new debug
 * flags.  added fileerrs flag for reporting errs
 * in configuration.
 *
 * Revision 1.22  1993/08/13  20:43:24  jockc
 * change parm list of getshset to use key_t instead of int
 *
 * Revision 1.21  1993/08/13  18:46:10  jockc
 * add debugging stuff and prototypes
 *
 * Revision 1.20  1993/06/01  23:21:56  root
 * change "table" load of iprintcap to use less memory
 *
 * Revision 1.19  1993/05/28  21:44:40  jockc
 * changed list items for use with ring.c
 *
 * Revision 1.18  1993/01/12  02:06:42  jockc
 * chopped out some dead variables, and moved others to
 * appropriate modules
 *
 * Revision 1.17  1992/12/31  23:43:08  jockc
 * added include of qpaths.h
 *
 * Revision 1.16  1992/10/27  23:29:08  jockc
 * added socket addresses for internet
 *
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
