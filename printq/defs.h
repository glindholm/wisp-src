/*
 * Header:  defs.h
 * Program: IDSIprint/UNIQUE
 * Purpose: std includes, constant defines, and struct definitions
 *      Id: $Id:$
 *
 */

#ifndef PQDEF
#define PQDEF

/* the following macros allow easier prototyping for compatibility with non-ANSI C compilers */

#ifdef __STDC__
# define EXFUN(type, name, proto)          type    name proto
# define DEFUN(type, name, args, arglist)  type name(arglist)
# define AND ,
# define END 
# define CONST const
#else
# define EXFUN(type, name, proto)          type    name()
# define DEFUN(type, name, args, arglist)  type name args arglist
# define AND ;
# define END ;
# define CONST
#endif

#include "intdef.h"

/* define NICEPS if you want the spooler to modify its args so that */
/* its entries in the PS list will be more informative .  (doesn't)*/
/* work on HP, so don't try it there */
#if !defined(HPUX) && !defined(NCR) && !defined(i386) && !defined(ICL)
#define NICEPS
#endif

#if !defined(SOCKCODE) && !defined(MQCODE)
#define SOCKCODE
#endif

#if !defined(SYSVWAIT) && !defined(BSDWAIT)
#define BSDWAIT
#endif

#define DUMP_SHMEM  /* define for experimental shared mem maps */

#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <signal.h>
#include <errno.h>
#include <pwd.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/termio.h>
#include <unistd.h>
#include <string.h>
#if defined (SUNOS)
# include <sys/filio.h>
#endif
#if defined (DGUX)
# include <sys/ioctl.h>
#endif

#if defined(MOTOROLA) || defined(ATT3B2) || defined (SCO) || defined(ICL) ||\
  defined (AIX) || defined (SOLARIS) || defined(NCR486) || defined(SEQUENT)
# if defined(SCO)
#  include <sys/times.h>
#  include <ctype.h>
#  include <sys/wait.h>
# else
#  include <time.h>
# endif
# include <fcntl.h>
# define bzero(x,y) memset(x,0,y)
#else
# include <sys/time.h>
# include <sys/wait.h>
# include <sys/resource.h>
# include <sys/file.h>
#endif

#if defined(AIX)
# include <sys/resource.h>
# include <sys/select.h>
# include <sys/lpio.h>
# include <sys/wait.h>
#endif

#if defined(SOLARIS)
# include <sys/filio.h>
#endif

#ifdef SOCKCODE
# include <sys/socket.h>
# include <sys/un.h>
# include <netdb.h>
# include <netinet/in.h>
# ifndef SUN_LEN
#  define SUN_LEN(su) \
	(sizeof(*(su)) - sizeof((su)->sun_path) + strlen((su)->sun_path))
# endif
#endif

#if defined(HPUX)
#define INTFDSET
#endif

#ifdef INTFDSET
#define FDSETCAST (int*)
#else
#define FDSETCAST
#endif

#ifdef MQCODE
#include <sys/msg.h>
#define KEYMOD 0xd4 /* KEYMOD is used as modifier in ftok call */
#endif

extern char *sys_errlist[];

#define PACKET struct packet
#define QITEM struct q_item
#define QITEM3 struct q_item_v3
#define QLIST struct q_list
#define PITEM struct p_item
#define PLIST struct p_list
#define FITEM struct f_item
#define FLIST struct f_list
#define BITEM struct b_item
#define BLIST struct b_list
#define MASTCONFIG struct master_config
#define CLASS struct class
#define CLASSINFO struct classinfo
#define ULIST struct unspooler_list
#define UITEM struct unspooler_list
#define RANGE struct range
#define UMSG struct unspooler_msg

#define OLDQITEM struct old_q_item

#define SEMRUNNING 0
#define SEMREADING 1
#define SEMWRITING 2
#define SEMCOMMTYPE 3
#define SEMWAIT 4

#define FLAG_SET 1
#define FLAG_CLEAR 2
#define FLAG_CHECK 3

#define TYPESOCKET 1
#define TYPEMQCODE 2

#define SCRMODE_WANG 1
#define SCRMODE_UNIQUE 2

#define IPCKEYMOD 0xd4

#define QDUMPMAGIC 0xd5deaa00 /* magic value for queue dump file */
#define PDUMPMAGIC 0xd5deaa01 /* magic value for printer dump file */
#define CDUMPMAGIC 0xd5deaa02 /* magic value for config dump file */
#define SHMMAGIC   0xd5deaa03
#define DUMPVERSION 3         /* version number: change if structures (qitem, pitem, etc) change */

#define PACKETMAGIC 0xd5deaa10 /* magic value for packets */
#define PACKETVERSION 2

#define QVERS 1 /* function codes for checking versions of dump files (check function is in utils.c) */
#define PVERS 2
#define CVERS 4
#define ALLVERS QVERS|PVERS|CVERS

#define INCOMPAT_VERS -1
#define LOCK_TIMEOUT -2
#define LOCKTIMEOUTVALUE 30

#define RETRIES 20
#define SLEEP_INTERVAL 1
#define IPC_FAIL -1

#define FILEDUMP_FREQ 60

#define QMAXSIZE 1024
#define QSEGSIZE 256

#define QMAXPATH 128+1
#define MAXPRTRS 1024
#define MAXFILTERS 1024
#define MAXNAME 14+1
#define QNAMEMAX 32+1
#define FORMNAMEMAX 32+1
#define PRTNAMEMAX 32+1
#define MODNAMEMAX 32+1
#define STOCKNAMEMAX 32+1
#define PIPEMAX 1024+1

#define TIMEOUT 20

#define NEXT(x) ((x) = (x)->next)

UMSG
{
	int status;
	int aux;
	char message[512];
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
#define UMSG_PRINT 11
#define UMSG_EXITING 12
#define UMSG_SHUTDOWN 13 

#define UNSPOOLER_PERSISTANCE 600

ULIST
{
	ULIST *next;
	int pid, rfd, wfd;
	PITEM *printer;
	QITEM *qitem;
	int status;
	time_t idlesince;
};
#define UST_OBSOLETE 0x0001
#define UST_WAITINGSHUT 0x0002

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
	
	char orig_path[QMAXPATH];
	char real_path[QMAXPATH];

	int owner_uid;
	int owner_pid;
	time_t time_stamp;
	
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
#define QST_OLD 0x0080
/* modes */
#define QMD_RESPOOLHOLD 0x0001
#define QMD_DELETEAFTER 0x0002
#define QMD_INFORM 0x0004
#define QMD_HOLD 0x0008
#define QMD_NOBANNER 0x0010
#define QMD_FFBEFORE 0x0020
#define QMD_FFAFTER 0x0040
#define QMD_FFBETWEEN 0x0080

#define ISHOLDING(x) ((x)->status & QST_HOLDING)
#define ISREADY(x)  ((x)->status & QST_READY)
#define ISPRTRERR(x) ((x)->status & QST_CHKPRT)
#define ISUNSPOOLED(x) ((x)->status & QST_UNSPOOLED)
#define ISQSTOPPED(x) ((x)->status & QST_STOPPED)
#define WANTBANNER(x) (!((x)->mode & QMD_NOBANNER))
#define FFBEFORE(x) ((x)->mode & QMD_FFBEFORE)
#define FFAFTER(x) ((x)->mode & QMD_FFAFTER)
#define FFBETWEEN(x) ((x)->mode & QMD_FFBETWEEN)

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

	char ffstr[9],logfile[128],device[PIPEMAX],rmachine[32],rprinter[9],
	sdir[128],form[FORMNAMEMAX],printer[PRTNAMEMAX],class[27],model[MODNAMEMAX],type[20];
	char filters[8][128]; 
	int inputtype; int devtype;
};

PITEM
{
	int printer_id;
	int baud,stopbits,databits,parity,pagelen,pagewid;
	char printer_name[PRTNAMEMAX];
	char printer_dev[PIPEMAX];
	char printer_model[MODNAMEMAX];
	
	time_t time_stamp;
	
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
#define PMD_PROGSTDIN 0x0008
#define PMD_PROGFILE 0x0010
#define PMD_PARALLEL 0x0020
#define PRT_TYPE_DEVICE "device"
#define PRT_TYPE_PROGRAM "program"
/*status*/
#define PST_BUSY 0x0001
#define PST_ENABLED 0x0002
#define PST_FORMCHANGE 0x0004
#define PST_STOPPED 0x0008
#define PST_ERROR 0x0010
#define PST_OTHER_BUSY 0x0020
#define PST_OLD 0x0040

#define ISIDLE(x) (!((x)->status & PST_BUSY))
#define ISBUSY(x) (((x)->status & PST_BUSY))
#define ISINUSEBYOTHER(x) ((x)->status & PST_OTHER_BUSY)
#define ISENABLED(x) ((x)->status & PST_ENABLED)
#define ISPSTOPPED(x) ((x)->status & PST_STOPPED)
#define WRONGFORM(x) ((x)->status & PST_FORMCHANGE)
#define ISERROR(x) ((x)->status & PST_ERROR)
#define ISWAITSHUT(x) ((x)->unspooler? ((x)->unspooler->status & UST_WAITINGSHUT) : 0)

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
#define INITSIZE 32767
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
	char filterstr[PIPEMAX]; /* this is the default filter */
	char stock[STOCKNAMEMAX];
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
	int ffbefore, ffafter, ffbetween, header;
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
	int inet_address;  /* if 0, use AF_UNIX, else use AF_INET */
	int default_scrmode;
	int priority;
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

struct shmmapheader
{
	int4 magic;
	int4 version;
	int4 type;
	int4 sequence;
	int4 item_cnt;
};

#define SHMTYP_P 1
#define SHMTYP_Q 2
#define MAXSHMPRTRS 64
#define MAXSHMQ 256
#define PRTSHMSZ (sizeof(PITEM)*MAXSHMPRTRS+sizeof(struct shmmapheader))
#define QSHMSZ (sizeof(QITEM)*MAXSHMQ+sizeof(struct shmmapheader))

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

#ifdef UNIQUE
# define DEFERRLOGF "%s/udaemon.log"
# define OLDERRLOGF "%s/errs"
#else
# define ERRLOGF "%s/errs"
#endif

#define DAEMONPIDF "%s/udaemon.pid"

#define IERR_OK 0
#define IERR_SOCKET 1
#define IERR_SERVUNDEF 2
#define IERR_NODAEMON 3
#define IERR_BIND 4     
#define IERR_HOSTADDR 5
#define IERR_INCOMPATDS 6
#define IERR_INCOMPATDM 7
#define IERR_RUNNING 8
#define IERR_SEMAPHORE 9
#define IERR_SHMEM 10
#define IERR_NODIR 11
#define IERR_DUMPVERS 12
#define IERR_DUMPMAG  13
#define IERR_DUMPFMT  14

#define OLDOLDLOCKFILEF "%s/LOCKFILE" 
#define OLDLOCKTMPF  "%s/.daemon"
#define OLDLOCKFILEF    "%s/.lockfile"

#define COMM_OK 0

#ifdef UNIQUE
# define SPOOLVAR "USPOOL"
#else
# define SPOOLVAR "ISPOOL"
#endif

#define PFILEDIRF   "%s/printfiles"
#define INITDIRF    "%s/init"
#define DUMPDIRF    "%s/data"
#define REMDIRF     "%s/remote"

#ifdef UNIQUE
#define DEFSPOOLDIR "/usr/spool/uprint"
# define QDUMPFILEF  "%s/uqueue.dat"
# define PDUMPFILEF   "%s/uprinters.dat"
# define CDUMPFILEF  "%s/uconfig.dat"
#else
#define DEFSPOOLDIR "/usr/spool/iprint"
# define QDUMPFILEF  "%s/iqueue.dat"
# define PDUMPFILEF   "%s/iprinters.dat"
# define CDUMPFILEF  "%s/ilpconfig.dat"
#endif

#define OLDQDUMPFILEF "/usr/spool/iprint/iqueue.dat"
#define PSTATUSLOGSF "%s/pstatus"
#define OLDPRINTCAPF "/etc/iprintcap"
#define PRINTCAPF    "%s/prtdef"
#define FORMDEFF	    "%s/formdef"
#define CLASSDEFF    "%s/classdef"

#ifdef UNIQUE
#define ILPDEFF	    "%s/ulpdef"
#define ILPUSER	    "ulp"
#else
#define ILPDEFF	    "%s/ilpdef"
#define ILPUSER	    "ilp"
#endif

#define QDFILECNT 4

#ifdef UNIQUE
# define SEMFILE "%s/UNI_semaphore"
# define QDKFILE "%s/UNI_queue.%d"
# define PRTKFILE "%s/UNI_printers"
# ifdef MQCODE
#  define MQPATHF "%s/UNI_mq"
# endif
# ifdef SOCKCODE
#  define SOCKETPATHF "%s/UNI_sock"
# endif
#else
# define SEMFILE "%s/ILP_semaphore"
# define QDKFILE "%s/ILP_queue.%d"
# define PRTKFILE "%s/ILP_printers"
# ifdef MQCODE
#  define MQPATHF "%s/ILP_mq"
# endif
# ifdef SOCKCODE
#  define SOCKETPATHF "%s/ILP_sock"
# endif
#endif

#define IDEBUG "IDEBUG"
#define UDEBUG "UDEBUG"

#ifdef UNIQUE
#define DAEMONNAME "udaemon"
#define MANNAME    "unique"
#define LPNAME     "ulp"
#define SHUTNAME   "ushut"
#else
#define DAEMONNAME "idaemon"             /* make sure this agrees with Makefile */
#define MANNAME    "ilpman"
#define LPNAME     "ilp"
#define SHUTNAME   "ishut"
#endif

#define DEFDISPPROGW "display"
#define DEFDISPPROGN "pg"

#define DISPVAR "UNIDISPLAY"
#define WANGVAR "UNIWANGMODE"

#ifdef NICEPS
#define FAKEARG   "initializing..................................................."
#ifdef __STD__
#define FAKEARGLEN strlen((CONST char *)FAKEARG)
#else
#define FAKEARGLEN strlen(FAKEARG)
#endif
#define DMAINNAME "V%-5.5s (master process)                                       "
#define DUNSPNAME "V%-5.5s (slave: printer %s)                                    "
#endif
#endif

/*
 * $Log: defs.h,v $
 * Revision 1.72  1993/11/12  00:19:33  jockc
 * need sys/wait for AIX
 *
 * Revision 1.71  1993/11/10  23:40:20  jockc
 * add new remote dir path
 *
 * Revision 1.70  1993/11/10  22:37:43  jockc
 * shorten key file names for stupid SCO box
 *
 * Revision 1.69  1993/11/10  21:50:49  jockc
 * include sys/wait for sco
 *
 * Revision 1.68  1993/11/10  17:39:04  jockc
 * added priority to config struct and parallel /devtype to ipcapstruct
 *
 * Revision 1.67  1993/09/17  17:24:48  jockc
 * default /usr/spool/uprint for unique, /usr/spool/iprint for old style
 *
 * Revision 1.66  1993/09/13  23:02:01  jockc
 * added screen mode for qconfig.. change printf format string for slave unspoolers.. add default disp prog for wang and normal mode..use lowercase names for
 * #defined stuff
 *
 * Revision 1.65  1993/09/13  17:26:26  jockc
 * change ilpdef, uid ilp to ulpdef ulp
 *
 * Revision 1.64  1993/09/13  15:28:17  jockc
 * 499
 * 503
 *
 * Revision 1.64  1993/09/10  18:34:06  jockc
 * added default display prog, vars for WANG mode and display
 *  prog override.  #defs for program names. changed some data
 * files to more appropriate names. added new printer mode,
 *  input file or input stdin.
 *
 * Revision 1.63  1993/08/13  22:08:19  jockc
 * CONST replaces const
 *
 * Revision 1.62  1993/08/13  21:49:24  jockc
 * small change reintegrated from SCS
 *
 * Revision 1.61  1993/08/13  20:43:59  jockc
 * add CAST of fdset on HPUX to int *, change ifdef hpux to HPUX
 *
 * Revision 1.60  1993/08/13  18:56:50  jockc
 * change paths to u instead of i, small changes
 *
 * Revision 1.59  1993/06/03  17:45:10  jockc
 * don't use MAXPATH if the system has defined it.  we need
 * the QITEM to be less than 512 bytes
 *
 * Revision 1.58  1993/06/02  23:41:01  jockc
 * added a few new unspooler messages
 *
 * Revision 1.57  1993/05/15  00:05:48  jockc
 * change MAXPRTRS and MAXFILTERS to 1024 to allow more to be loaded. (these
 * values were only used during the yacc load stuff
 *
 * Revision 1.56  1993/05/14  23:06:11  jockc
 * added def for EXFUN() prototyper
 *
 * Revision 1.55  1993/04/27  17:27:59  jockc
 * small add for SOLARIS
 *
 * Revision 1.54  1993/03/26  18:19:37  jockc
 * increase retries to 20
 *
 * Revision 1.53  1993/03/26  06:36:00  jockc
 * bump the reties (now being used by c_comm.c) to 10.. on really
 * loaded systems 10 may even be too few.. note that this
 * constant is only (currently) used by the connect() loop in
 * c_comm.c:init_comm
 *
 * Revision 1.52  1993/03/11  19:36:17  jockc
 * add error code for no dir on sem create
 *
 * Revision 1.51  1993/03/01  23:33:45  jockc
 * needed ctype apparently for sco to get these macros
 *
 * Revision 1.50  1993/01/12  02:07:53  jockc
 * chopped out some dead stuff, added defines for semaphore ops and
 * share memory maps
 *
 * Revision 1.49  1993/01/06  19:02:09  jockc
 * move include of netinet/in.h into sockcode
 * include ioctl for DGUX
 *
 * Revision 1.48  1993/01/04  19:36:28  jockc
 * slight revision for SUNOS includes and FAKEARG
 *
 * Revision 1.47  1993/01/01  00:34:54  jockc
 * *** empty log message ***
 *
 * Revision 1.46  1992/12/31  23:45:38  jockc
 * cleaned up the include mess.  added qmd_between and related
 * stuff.  added adjustable path stuff aka qpath/ISPOOL.
 * increased size of message buffer in MSG struct. added
 * version stamp to NICEPS.  defined SUN_LEN here for systems
 * that don't have it (pre bsd4.3)
 *
 * Revision 1.45  1992/11/13  21:30:37  jockc
 * adjusted some includes for motorola
 *
 * Revision 1.44  1992/10/28  18:59:00  root
 * increased size of device member in ipcapstruct to PIPEMAX chars
 *
 * Revision 1.43  1992/10/27  23:30:18  jockc
 * increased max len for pipe and filters
 * added inet_address to MASTCONFIG
 * new version for config files
 *
 * Revision 1.42  1992/10/09  21:11:41  jockc
 * increased the initsize (max size for init file contents)
 *
 * Revision 1.41  1992/10/09  20:17:23  jockc
 * added #defines for IDEBUG, new lock stuff, changed
 * old lock names
 *
 * Revision 1.40  1992/08/10  20:43:03  jockc
 * added NEED_BZERO for ICL
 *
 * Revision 1.39  1992/08/10  20:38:43  jockc
 * added fcntl for ICL
 *
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
 */
