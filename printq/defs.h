/*
 * Header:  defs.h
 * Program: IDSIprint
 * Purpose: std includes, constant defines, and struct definitions
 *      Id: $Id:$
 *
 * $Log: defs.h,v $
 * Revision 1.38  1992/08/04  00:21:35  jockc
 * added stuff for parity/stop/databits
 *
 * Revision 1.37  1992/07/07  23:07:43  jockc
 * added new (temporary) lockfile
 *
 * Revision 1.36  1992/06/26  22:21:35  jockc
 * added sys/lpio.h include for AIX
 *
 * Revision 1.35  1992/06/18  23:34:49  jockc
 * add default commtype/waittype back
 *
 * Revision 1.34  1992/06/18  23:31:03  jockc
 * remove conditional stuff for commtype/waittype
 *
 * Revision 1.33  1992/05/22  21:07:50  jockc
 * sco can't use niceps
 *
 * Revision 1.32  1992/05/20  23:16:32  jockc
 * changed locations of dump files, added support for
 * magic and version numbers, support for dummy iqueue.dat
 *
 * Revision 1.31  1992/05/04  20:47:21  jockc
 * change all form/prt names to 32 chars
 *
 * Revision 1.30  1992/04/30  20:21:08  jockc
 * no niceps for hp
 *
 * Revision 1.29  1992/04/30  19:14:15  jockc
 * change lines to size in the qitem
 *
 * Revision 1.27  1992/04/29  21:07:54  jockc
 * code for filters-per-model
 *
 * Revision 1.26  1992/04/16  21:06:37  jockc
 * increase timeout to 20 seconds for caelus
 *
 * Revision 1.25  1992/03/27  23:09:17  jockc
 * removed some unused path defines, changed initstr layout
 * to support init files, added busy_other bit to printer
 * status, added aux to umsg,  changed model size in ipcapstruct
 * to 33 bytes..
 *
 * Revision 1.24  1992/03/24  01:02:13  jockc
 * some 3b2 stuff
 *
 * Revision 1.23  1992/03/13  21:35:53  jockc
 * add bits in qitem mode for ffbefore,after,and banner. add type
 * to pitem, bits to pitem mode for device/program.. add int flag
 * to fitem for binary
 *
 * Revision 1.22  1992/02/26  22:38:52  jockc
 * move include sys/time to inside ifndef i386
 *
 * Revision 1.21  1992/01/06  19:36:25  jockc
 * default SOCKCODE and BSDWAIT if not defined
 * also default for hpux
 *
 * Revision 1.20  1991/11/20  19:54:12  jockc
 * some include changes to allow mq compile on sco
 *
 * Revision 1.19  1991/10/11  19:23:04  jockc
 * define comm_ok
 *
 * Revision 1.18  1991/10/08  23:26:57  jockc
 * struct changes to support prt models
 *
 * Revision 1.17  1991/10/07  18:26:00  jockc
 * added define for umsg_hold
 *
 * Revision 1.16  1991/10/01  17:08:09  jockc
 * massive systemwide changes.  some structures changed, some
 * eliminated.
 *
 * Revision 1.15  1991/08/07  18:36:05  jockc
 * added qdumpfile
 *
 * Revision 1.14  1991/08/05  23:02:25  jockc
 * added printer num to qitem and pitem
 *
 * Revision 1.13  1991/08/02  23:51:03  jockc
 * added CST_CLOSING, lengthened TIMEOUT
 *
 * Revision 1.12  1991/07/22  23:02:05  jockc
 * added defs for printer error stuff
 *
 * Revision 1.11  1991/06/27  18:27:06  jockc
 * unixdomain socket adds
 *
 * Revision 1.10  1991/05/22  17:07:00  jockc
 * added include fcntl for sco
 *
 * Revision 1.9  1991/05/03  17:49:40  jockc
 * added CLASSDIR for classes
 *
 * Revision 1.8  1991/05/02  19:48:05  jockc
 * changed communication lock to rdy4rq
 *
 * Revision 1.7  1991/04/30  17:43:59  jockc
 * misc cleanup
 *
 * Revision 1.6  1991/04/25  21:25:57  jockc
 * some prelime changes for v1.2 stuff.  forms struct changes
 *
 * Revision 1.5  1991/04/23  19:20:04  jockc
 * *** empty log message ***
 *
 * Revision 1.4  1991/04/23  19:13:13  jockc
 * zaphod needs sys/select.h
 *
 * Revision 1.3  1991/04/23  19:08:01  jockc
 * maxpath already defined on aix
 *
 * Revision 1.2  1991/04/19  00:47:42  jockc
 * *** empty log message ***
 *
 * Revision 1.1  1991/04/18  23:50:06  jockc
 * Initial revision
 *
 *
 */

#ifndef PQDEF
#define PQDEF

/* define NICEPS if you want the spooler to modify its args so that */
/* its entries in the PS list will be more informative .  (doesn't)*/
/* work on HP, so don't try it there */
#if !defined(hpux) && !defined(NCR) && !defined(i386)
#define NICEPS
#endif

#if !defined(SOCKCODE) && !defined(MQCODE)
#define SOCKCODE
#endif

#if !defined(SYSVWAIT) && !defined(BSDWAIT)
#define BSDWAIT
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#ifdef SOCKCODE
# include <sys/socket.h>
#endif
#ifdef u3b2
# include <time.h>
# include <fcntl.h>
#endif
#ifndef i386
# include <sys/time.h>
# ifndef u3b2
#  include <sys/resource.h>
# endif
#endif
#ifndef u3b2
# include <sys/wait.h>
# include <sys/file.h>
#endif
#include <sys/stat.h>
#include <sys/termio.h>
#if defined (_AIX) || defined(AIX)
# include <sys/select.h>
# include <time.h>
# include <sys/lpio.h>
#endif
#include <unistd.h>
#ifdef SOCKCODE
# include <sys/un.h>
# include <netdb.h>
#endif
#include <signal.h>
#include <errno.h>
#include <pwd.h>
#include <dirent.h>
#ifdef i386
# include <fcntl.h>
# include <sys/times.h>
# include <sys/select.h>
# define bzero(x,y) memset(x,0,y)
#endif
#ifdef u3b2
# define bzero(x,y) memset(x,0,y)
#endif
#ifdef MQCODE
#include <sys/ipc.h>
#include <sys/msg.h>
/* KEYMOD is used as modifier in ftok call */
#define KEYMOD 0xd4 
#endif

#define PACKET struct packet
#define QITEM struct q_item
#define QLIST struct q_list
#define PITEM struct p_item
#define PLIST struct p_list
#define FITEM struct f_item
#define FLIST struct f_list
#define BITEM struct b_item
#define BLIST struct b_list
#define MASTCONFIG struct master_config
/*#define CLIST struct c_list*/
#define CLASS struct class
#define CLASSINFO struct classinfo
#define ULIST struct unspooler_list
#define RANGE struct range
#define UMSG struct unspooler_msg

#define OLDQITEM struct old_q_item

#define QDUMPMAGIC 0xd5deaa00 /* magic value for queue dump file */
#define PDUMPMAGIC 0xd5deaa01 /* magic value for printer dump file */
#define CDUMPMAGIC 0xd5deaa02 /* magic value for config dump file */
#define DUMPVERSION 1         /* version number: change if structures (qitem, pitem, etc) change */

#define PACKETMAGIC 0xd5deaa10 /* magic value for packets */
#define PACKETVERSION 1

#define QVERS 1 /* function codes for checking versions of dump files (check function is in utils.c) */
#define PVERS 2
#define CVERS 4
#define ALLVERS QVERS|PVERS|CVERS
#define INCOMPAT_VERS -1

#define RETRIES 5
#define SLEEP_INTERVAL 1
#define IPC_FAIL -1
#define QMAXSIZE 256
#ifndef MAXPATH
#define MAXPATH 128+1
#endif
#define MAXPRTRS 32
#define MAXFILTERS 8
#define MAXNAME 14+1
#define QNAMEMAX 32+1
#define FORMNAMEMAX 32+1
#define PRTNAMEMAX 32+1
#define MODNAMEMAX 32+1
#define TIMEOUT 20

#define NEXT(x) ((x) = (x)->next)

UMSG
{
	int status;
	int aux;
	char message[80];
};
#define UMSG_DONE 1
#define UMSG_ERR 2
#define UMSG_FORMCHANGED 3
#define UMSG_ALIGN 4
#define UMSG_NEWFORM 5
#define UMSG_ABORT 6
#define UMSG_STOP 7
#define UMSG_START 8
#define UMSG_UNUNSPOOL 9
#define UMSG_HOLD 10

ULIST
{
	ULIST *next;
	int pid, rfd, wfd;
	PITEM *printer;
	QITEM *qitem;
};

#if 0
CLIST
{
	CLIST *next,*prev;
	int pid,uid,station,status;
	int needs_queue,needs_plist;
	iod io_d;
};	      
#define CST_CONNECTED 0x0001
#define CST_MONITORING 0x0002
#define CST_CLOSING 0x0004
#endif

RANGE
{
	int st,end;
	int type;
};

#define T_QLIST 1
#define T_QITEM 2
#define T_PLIST 3
#define T_PITEM 4

#define PACKET_SIZE (sizeof(PACKET))

QITEM
{
	int q_pos;
	int q_id;
	int q_type;
	
	char orig_path[MAXPATH];
	char real_path[MAXPATH];

	int owner_uid;
	int owner_pid;
	int owner_station;

	int status;

	char requested_prtr[QNAMEMAX];
	char actual_prtr[QNAMEMAX];
	
	PITEM *printer;

	char form[FORMNAMEMAX];
	FITEM *fitem;
	char class;
	
	int copies, mode;
	int stpage, endpage;
	int banner, prtnum;
	
	int size, pages;
	char **filters;
};
/* status */
#define QST_UNSPOOLED 0x0001
#define QST_HOLDING 0x0002
#define QST_READY 0x0004
#define QST_CHKPRT 0x0008
#define QST_DELETE 0x0010
#define QST_REMOVE 0x0020
#define QST_STOPPED 0x0040
/* modes */
#define QMD_RESPOOLHOLD 0x0001
#define QMD_DELETEAFTER 0x0002
#define QMD_INFORM 0x0004
#define QMD_HOLD 0x0008
#define QMD_NOBANNER 0x0010
#define QMD_FFBEFORE 0x0020
#define QMD_FFAFTER 0x0040

#define ISHOLDING(x) ((x)->status & QST_HOLDING)
#define ISREADY(x)  ((x)->status & QST_READY)
#define ISPRTRERR(x) ((x)->status & QST_CHKPRT)
#define ISUNSPOOLED(x) ((x)->status & QST_UNSPOOLED)
#define ISQSTOPPED(x) ((x)->status & QST_STOPPED)
#define WANTBANNER(x) (!((x)->mode & QMD_NOBANNER))
#define FFBEFORE(x) ((x)->mode & QMD_FFBEFORE)
#define FFAFTER(x) ((x)->mode & QMD_FFAFTER)

QLIST
{
	QLIST *prev,*next;
	QITEM *data;
};

OLDQITEM
{
	int q_pos;
	int q_id;
	int q_type;
	
	char orig_path[129];
	char real_path[129];

	int owner_uid;
	int owner_pid;
	int owner_station;

	int status;

	char requested_prtr[33];
	char actual_prtr[33];
	
	PITEM *printer;

	char form[17];
	FITEM *fitem;
	char class;
	
	int copies, mode;
	int stpage, endpage;
	int banner, prtnum;
	
	int size, pages;
	char **filters;
	
};

#define PAR_NONE 0
#define PAR_ODD 1
#define PAR_EVEN 2

struct ipcapstruct
{
	int baud,stopbits,databits,parity,pagelen,pagewid,
	sizepri,prtnum;

	char ffstr[9],logfile[128],device[128],rmachine[32],rprinter[9],
	sdir[128],form[FORMNAMEMAX],printer[PRTNAMEMAX],class[27],model[MODNAMEMAX],type[20];
	char filters[8][128];
};

PITEM
{
	int printer_id;
	int baud,stopbits,databits,parity,pagelen,pagewid;
	char printer_name[PRTNAMEMAX];
	char printer_dev[100];
	char printer_model[MODNAMEMAX];
	
	char hostname[33];
	char remote_prtr[QNAMEMAX];
	
	char  default_form[FORMNAMEMAX];
	char  current_form[FORMNAMEMAX];
	char class[26];
	
	int status;									/* status defined below */
	int mode,flags;									/* modes defined below */
	int waitpid;                                                                    /* process using this printer */
	int qid;									/* job being printed  */
	ULIST *unspooler;
	int prtnum;
	
	char **filters;
};
/* modes */
#define SIZEPRI 0x0001 /* not used */
#define PMD_DEVICE 0x0002
#define PMD_PROGRAM 0x0004
#define PRT_TYPE_DEVICE "device"
#define PRT_TYPE_PROGRAM "program"
/*status*/
#define PST_BUSY 0x0001
#define PST_ENABLED 0x0002
#define PST_FORMCHANGE 0x0004
#define PST_STOPPED 0x0008
#define PST_ERROR 0x0010
#define PST_OTHER_BUSY 0x0020

#define ISIDLE(x) (!((x)->status & PST_BUSY))
#define ISBUSY(x) (((x)->status & PST_BUSY))
#define ISINUSEBYOTHER(x) ((x)->status & PST_OTHER_BUSY)
#define ISENABLED(x) ((x)->status & PST_ENABLED)
#define ISPSTOPPED(x) ((x)->status & PST_STOPPED)
#define WRONGFORM(x) ((x)->status & PST_FORMCHANGE)
#define ISERROR(x) ((x)->status & PST_ERROR)

PLIST
{
	PLIST *next;
	PITEM *data;
};

typedef struct init_struct
{
	struct init_struct *next;
	
	char model[MODNAMEMAX];
	char *initdata;
	int idatatype;
#define INITSIZE 1024
#define INITDFILE 1
#define INITDSTR 2
	int initlen;
} init_s;
typedef struct filt_struct
{
	struct filt_struct *next;
	
	char model[MODNAMEMAX];
	char *filter;
} filt_s;
FITEM
{
	int form_id;
	char form_name[FORMNAMEMAX];
	int cpi, lpi, lpp;
	int lm,rm,tm,bm;
	char filterstr[200]; /* this is the default filter */
	char stock[33];
	char *initdata;
	int idatatype;
	int initlen;
	init_s *p_init;
	filt_s *filts; /* this is a list of filters per model */
	time_t time;
	int binary;
};

FLIST
{
	FLIST *next;
	FITEM *data;
};

CLASS
{
	int ffbefore, ffafter, header;
};
CLASSINFO
{
	CLASS classlist[27]; /* A-Z and ' ' */
	time_t time;
};

BITEM
{
	int batch_id;
	int batch_q_name[33];
	int batch_days;
	int batch_st_time,batch_end_time;
	int batch_nice;
	int batch_flags;
};

#define MAXOPS 256
MASTCONFIG 
{
	char default_class;
	char default_form[FORMNAMEMAX];
	char default_printer[FORMNAMEMAX];
	int operators[MAXOPS];
	int opcount;
};

union q_union
{
	QITEM q;
	PITEM p;
	FITEM f;
	BITEM b;
};
PACKET
{
	int magic; /* magic must be PACKETMAGIC */
	int version; /* version must match PACKETVERSION */
	
	int functype;
	int sender_pid;	
	int sender_uid;
	int sender_station;
	
	int aux;

	union q_union data;
};

#define INFORM_MAIL 0z01
#define INFORM_WRITE 0x02
#define INFORM_MESSAGE 0x04

#define M_CONNECT    1
#define M_DISCONNECT 2
#define M_MONITOR    3
#define M_UNMONITOR  4
#define M_QUERY	     5
#define M_CHANGE     6
#define M_QUEUE	     7
#define M_INFO	     8
#define M_EXCEPTION  9
#define M_CFGUPDATE 10
#define M_ACKNOWLEDGE 11
#define M_RANGE 12

#ifdef EXT
EXT char *mtypes[];
#else
char *mtypes[]=
{
	"",
	"CONNECT",
	"DISCON",
	"MONITOR",
	"UNMON",
	"QUERY",
	"CHANGE",
	"QUEUE",
	"INFO",
	"ERROR",
	"CFGUP",
	"ACK",
	"START",
	"END",
	NULL
};
#endif

/* these go in the aux field for types query and change, respectively */
#define Q_GETQUEUE 1
#define Q_GETITEM 2
#define Q_GETPRTR 3
#define Q_GETFORM 4

#define C_ALIGN 1
#define C_FORM 2
#define C_QITEM 3
#define C_STOPQ 4
#define C_ENABLE 5
#define C_DISABLE 6
#define C_STARTQ 7
#define C_PRTR 8
#define C_HOLD 9
#define C_UNHOLD 10
#define C_DELETE 11
#define C_REMOVE 12
#define C_CLRERR 13

#define DAEMON_MSG 1

#define BADTYPE 1

#ifdef FALSE
#undef FALSE
#endif
#ifdef TRUE
#undef TRUE
#endif
#define FALSE 0
#define TRUE !FALSE

#ifdef ON
#undef ON
#endif
#ifdef OFF
#undef OFF
#endif
#define ON 1
#define OFF 0

#define PANIC(x) panic_routine(__FILE__,__LINE__,x?errmsg[x-1]:NULL,errno)

#define DEFERRLOG "/usr/spool/iprint/errs"
#define DAEMONPID "/usr/spool/iprint/daemon_pid"

#define LK_STAT 1
#define LK_SET 2
#define LK_CLEAR 3

#define DAEMON_RUNNING 0x0001
#define RDY4RQ 0x0002
#define DUMPING 0x0004
#define MQVERSION 0x0008
#define SOCKVERSION 0x0010
#define LOCKFILE "/usr/spool/iprint/LOCKFILE"
#define LOCKTMP  "/usr/spool/iprint/.daemon"
#define COMM_OK 0

#define DEFSPOOLDIR "/usr/spool/iprint"

#define INITDIR	    "/usr/spool/iprint/init"
#define DUMPDIR     "/usr/spool/iprint/data"
#define QDUMPFILE   "/usr/spool/iprint/data/iqueue.dat"
#define PDUMPFILE   "/usr/spool/iprint/data/iprinters.dat"
#define CDUMPFILE   "/usr/spool/iprint/data/ilpconfig.dat"
#define OLDQDUMPFILE   "/usr/spool/iprint/iqueue.dat"
#define PSTATUSLOGS "/usr/spool/iprint/pstatus"
#define OLDPRINTCAP "/etc/iprintcap"
#define PRINTCAP    "/usr/spool/iprint/prtdef"
#define FORMDEF	    "/usr/spool/iprint/formdef"
#define CLASSDEF    "/usr/spool/iprint/classdef"
#define ILPDEF	    "/usr/spool/iprint/ilpdef"
#define ILPUSER	    "ilp"

#ifdef MQCODE
#define MQPATH "/usr/spool/iprint/ILP_mq"
#endif
#ifdef SOCKCODE
#define SOCKETPATH "/usr/spool/iprint/ILP_sock"
#endif

#ifdef NICEPS
#define FAKEARG   "xxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
#define FAKEARGLEN strlen(FAKEARG)
#define DMAINNAME "(master process)             "
#define DUNSPNAME "(slave process)              "
#endif
#endif
