/* Copyright (c) 1995-2003 by Acucorp, Inc.  All rights reserved.	*/
/* Users of the ACUCOBOL-GT runtime may freely modify and distribute	*/
/* this file as they see fit in order to support an ACUCOBOL-GT based	*/
/* application.								*/

/* THIS FILE IS #INCLUDED FROM sub.c.  BECAUSE SYSTEM HEADER FILES	*/
/* SHOULD BE INCLUDED BEFORE sub.h, AND BECAUSE THIS FILE IS INCLUDED	*/
/* AFTER sub.h, YOU REALLY SHOULDN'T INCLUDE ANY SYSTEM HEADER FILES	*/
/* FROM THIS FILE.							*/

/* Sometimes it is desirable to create smaller versions of the runtime	*/
/* system.  This file allows you to remove selected pieces of the 	*/
/* runtime.  By removing pieces of the runtime, you can make it 	*/
/* smaller.  Of course, you will not be able to use the piece that you	*/
/* remove.  You can make the runtime smaller if memory space on the	*/
/* machine is very tight, or if you want to minimize the overhead	*/
/* associated with the runtime.						*/

/* To reconfigure the runtime, you will need the 'C' compiler for the	*/
/* target machine.  See Appendix C for the list of which 'C' compiler	*/
/* to use for a particular machine.  Once you have selected the 	*/
/* configuration options that you want to use, recreate the runtime	*/
/* using the instructions listed in that Appendix.			*/


/* You can remove the runtime source debugger by setting the following	*/
/* value to 1.  This will save 17+ Kbytes depending on your machine.	*/
/* One idea is to leave the debugger in the version of the runtime you	*/
/* develop with, but to remove it from the runtime you send to your	*/
/* users.  This way, you can still take advantage of the debugger while	*/
/* using less space on the user's machine.				*/

#ifndef	NO_DEBUGGER
#define	NO_DEBUGGER	0
#endif	/* NO_DEBUGGER */


/* Set the following to 1 to remove support for Floating-Point numbers.	*/
/* Under MS-DOS and OS/2, this will save about 18 Kbytes.		*/

#ifndef	NO_FLOAT
#define	NO_FLOAT	0
#endif	/* NO_FLOAT */


/* If you wish to remove the SORT/MERGE module, you can set the next	*/
/* value to 1.  This will remove about 10 Kbytes.  You will not be	*/
/* able to use the SORT, MERGE, RELEASE or RETURN verbs if you set	*/
/* this value to 1.							*/

#ifndef	NO_SORT
#define	NO_SORT	        0
#endif	/* NO_SORT */


/* If you set INT_POWER to 1, then only integer exponents will be 	*/
/* supported.  This prevents you from taking roots, but it saves a fair	*/
/* amount of code in the runtime.  In particular, it removes code that	*/
/* computes natural logs and code that computes e^x.  This saves 	*/
/* almost 2 Kbytes.							*/

#ifndef	INT_POWER
#define	INT_POWER	0
#endif	/* INT_POWER */


/* If you set NO_SCRN_SECTION to 1, then the Screen Section field	*/
/* manager will be removed from the runtime system.  This will prevent	*/
/* you from using any programs that contain a Screen Section.  You will	*/
/* have to use only field-level ACCEPT and DISPLAY statements.  This	*/
/* saves about 3 Kbytes of code.					*/

#ifndef	NO_SCRN_SECTION
#define	NO_SCRN_SECTION		0
#endif	/* NO_SCRN_SECTION */


/* If you set NO_CHARTS to 1, then the charting capability will be	*/
/* removed from the runtime system.  This will prevent you from 	*/
/* printing any charts that your programs try to draw.  Otherwise, the	*/
/* programs will run normally.  You will save about 7K by doing this.	*/

#ifndef	NO_CHARTS
#define	NO_CHARTS		0
#endif	/* NO_CHARTS */


/* If you set NO_CLIENT to 1, then the client library will not be	*/
/* necessary, and you will not be able to access remote files.		*/

#ifndef	NO_CLIENT
#define	NO_CLIENT		0
#endif	/* NO_CLIENT */


/* If you set NO_SOCKETS to 1, then the runtime will not need to link	*/
/* with any sockets libraries.  Note that CLIENT and ACUCONNECT both	*/
/* require sockets, so setting this to 1 will automatically imply	*/
/* NO_ACUCONNECT and NO_CLIENT.						*/

#ifndef	NO_SOCKETS
#define	NO_SOCKETS		0
#endif	/* NO_SOCKETS */


/* If you set NO_ACUCONNECT to 1, then the AcuConnect librares will	*/
/* not be necessary, and you will not be able to access remote		*/
/* programs using the runtime or create the acuconnect server.		*/

#ifndef	NO_ACUCONNECT
#define	NO_ACUCONNECT		0
#endif	/* NO_ACUCONNECT */


/* If you set NO_ACUSQL to 1, then the ACUSQL libraries will not be	*/
/* necessary, and you will not be able to execute embedded SQL via	*/
/* AcuSQL.								*/

#ifndef	NO_ACUSQL
#define	NO_ACUSQL		1
#endif	/* NO_ACUSQL */


/* If you set NO_MQSERIES to 1, then the MQSERIES libraries will not be	*/
/* necessary, and you will not be able to execute MQ SERIES calls.      */

#ifndef	NO_MQSERIES
#define	NO_MQSERIES		1
#endif	/* NO_MQSERIES */


/* NO_CONTROLS option removed in 3.2 - screen controls are now used	*/
/* by various runtime functions (such as the debugger), not just	*/
/* COBOL programs.							*/


/* The rest of this file implements the configuration options.  You	*/
/* will not need to modify this.					*/

/*  ! NO_CLIENT implies ! NO_SOCKETS.	*/
#if	!NO_CLIENT && NO_SOCKETS
#undef	NO_SOCKETS
#define	NO_SOCKETS		0
#endif	/* !NO_CLIENT && NO_SOCKETS */

/*  ! NO_ACUCONNECT implies ! NO_SOCKETS.	*/
#if	!NO_ACUCONNECT && NO_SOCKETS
#undef	NO_SOCKETS
#define	NO_SOCKETS		0
#endif	/* !NO_ACUCONNECT && NO_SOCKETS */

/*  NO_SOCKETS implies NO_CLIENT and NO_ACUCONNECT.	*/
#if	NO_SOCKETS
#if	!NO_ACUCONNECT
#undef	NO_ACUCONNECT
#define	NO_ACUCONNECT	1
#endif	/* !NO_ACUCONNECT */
#if	!NO_CLIENT
#undef	NO_CLIENT
#define	NO_CLIENT	1
#endif	/* !NO_CLIENT */
#endif	/* NO_SOCKETS */

extern void stop_runtime P_((int, int, char *));

/* The following is extracted from "message/message.h" */

char *message_get P_((int, int, char *));

#define	MESSAGE_NO_DEFAULTS	0

#if	MESSAGE_NO_DEFAULTS
#define Agetmsg(n, s) 	(message_get(n, 0, NULL))
#else	/* MESSAGE_NO_DEFAULTS */
#define Agetmsg(n, s) 	(message_get(n, 0, s))
#endif	/* MESSAGE_NO_DEFAULTS */

#define	Msg_SorryTheDebuggerHasBeenRem		Agetmsg(1, \
	"Sorry, the debugger has been removed from this runtime!\n")
#define	Msg_RuntimeAbortedN		Agetmsg(2, \
	"*** Runtime Aborted ***\n")
#define	Msg_FloatingPointModuleRemoved		Agetmsg(3, \
	"Floating-Point module removed!\n")
#define	Msg_SORTMERGEModuleRemovedn		Agetmsg(4, \
	"SORT/MERGE module removed!\n")
#define	Msg_ScreenSectionSupportRemove		Agetmsg(5, \
	"Screen Section support removed!\n")
#define	Msg_ChartSupportRemovedn		Agetmsg(6, \
	"Chart support removed!\n")
#define	Msg_MQSERIESModuleRemoved		Agetmsg(1784, \
	"MQ SERIES module removed!\n")

#ifdef	_MSC_VER
#ifndef	_WINDOWS
 #pragma	check_stack(off)
#endif	/* _WINDOWS */
#endif	/* _MSC_VER */


#if	NO_DEBUGGER

int A_in_debugger = 0;
unsigned int Adbg_canvas = 0;

void
ensure_A_srcfile()
{
	return;
}

int
Aload_line_info()
{
	return 0;
}

int
Atranslate_address(pd_addr, pFilename, pLine)
unsigned int pd_addr;
char **pFilename;
long *pLine;
{
	return 0;
}

void
AdbgEnableTool(toolNum, enabled)
int toolNum;
int enabled;
{
	return;
}

void
AdbgEnableToolbar(enabled)
int enabled;
{
	return;
}

int
pgh_trace()
{
	return 0;
}

int
init_debug()
{
	eprintf(Msg_SorryTheDebuggerHasBeenRem);
	eprintf(Msg_RuntimeAbortedN);
	return 0;
}

void
DBGSetMonitor(p)
char *p;
{
	return;
}

int
debugger()
{
	return 0;
}

void
Aload_dbg()
{
	return;
}

void
DBGclose_windows()
{
	return;
}

char *
Adbgalloc()
{
	return NULL;
}

long
A_FSEEK()
{
	return 0L;
}

int
A_FILBUF()
{
	return 0;
}

void
A_FREAD()
{
	return;
}

int
AconditionTrue()
{
	return 0;
}

void
Aload_src_table()
{
	return;
}

void
Ado_help()
{
	return;
}

void
AsaveDebuggerSettings()
{
	return;
}

void
AloadDebuggerSettings()
{
	return;
}

void
AcreateDebugToolbar()
{
	return;
}

int
Atranslate_line(p1, p2, l)
int *p1;
char *p2;
long l;
{
	return -1;
}

int
DBGBreakpointCommand(p, q)
char *p;
int q;
{
	return -1;
}

#endif	/* NO_DEBUGGER */


#ifdef	NOFLOAT
#undef	NO_FLOAT
#define	NO_FLOAT	1
#endif	/* NOFLOAT */

#if	NO_FLOAT

int
float_convert()
{
	stop_runtime(255, 1, Msg_FloatingPointModuleRemoved);
	return 0;
}

void
float_to_num()
{
	float_convert();
}

void
float_store()
{
	float_convert();
}

void
float_to_float()
{
	float_convert();
}

void
float_op_to_num()
{
	float_convert();
}

void
float_add()
{
	float_convert();
}

void
float_sub()
{
	float_convert();
}

void
float_mult()
{
	float_convert();
}

void
float_div()
{
	float_convert();
}

void
float_negate()
{
	float_convert();
}

void
float_pow()
{
	float_convert();
}

void
float_cmp()
{
	float_convert();
}

void
float_set_remainder()
{
	float_convert();
}

void
float_move()
{
	float_convert();
}

long
float_cvt_long()
{
	float_convert();
	return 0L;
}

int
float_scan()
{
	float_convert();
	return 0;
}

void
float_inc()
{
	float_convert();
}

void
exfloat_to_num()
{
	float_convert();
}

void
move_uefp_to_efp()
{
	float_convert();
}

void
exfloat_op_to_num()
{
	float_convert();
}

void
exfloat_to_float()
{
	float_convert();
}

void
move_ncs_to_efp()
{
	float_convert();
}

void
move_fp_to_efp()
{
	float_convert();
}

void
exfloat_add()
{
	float_convert();
}

void
exfloat_multiply()
{
	float_convert();
}

void
exfloat_divide()
{
	float_convert();
}

void
num_to_exfloat()
{
	float_convert();
}

int
exfloat_normalize()
{
	float_convert();
}

int
exfloat_scan()
{
	float_convert();
}

int
exfloat_compare()
{
	float_convert();
}

int
exfloat_power()
{
	float_convert();
}

void
float_move_check()
{
	float_convert();
}

static void
float_check_size()
{
	float_convert();
}

#endif	/* NO_FLOAT */


#if	NO_SORT

int *sort_pgm;

static int
sort_error()
{
	stop_runtime(255, 1, Msg_SORTMERGEModuleRemovedn);
	return 0;
}

int
init_sort()
{
	sort_error();
	return 0;
}

int
sort_input()
{
	return 0;
}

int
sort_output()
{
	return 0;
}

int
do_sort()
{
	return 0;
}

int
end_sort_merge()
{
	return 0;
}

int
init_merge()
{
	sort_error();
	return 0;
}

int
prime_merge()
{
	return 0;
}

int
merge_input()
{
	return 0;
}

int
do_release()
{
	sort_error();
	return 0;
}

int
do_return()
{
	sort_error();
	return 0;
}

#endif	/* NO_SORT */



#if	INT_POWER

void
do_real_power()
{
	do_int_power();
}

#endif	/* INT_POWER */



#if	NO_SCRN_SECTION

void
Ascr_destroy()
{
	stop_runtime(255, 1, Msg_ScreenSectionSupportRemove);
}

int
Ascr_display()
{
	stop_runtime(255, 1, Msg_ScreenSectionSupportRemove);
	return 0;
}

void
AContAcceptLoop()
{
	return;
}

int
Ascr_accept()
{
	stop_runtime(255, 1, Msg_ScreenSectionSupportRemove);
	return 0;
}

int
Ascr_handle()
{
	stop_runtime(255, 1, Msg_ScreenSectionSupportRemove);
	return 0;
}

#endif	/* NO_SCRN_SECTION */


#if	NO_CHARTS

int	AV_errno;
short	AV_error;

int
CheckAVLicense()
{
	return 0;
}

void
AVinit_cfg()
{
	return;
}

int
AVinit(i)
short i;
{
	stop_runtime(255, 1, Msg_ChartSupportRemovedn);
	return 0;
}

int
AVparm (i)
short i;
{
	return 0;
}

int
AVdata(i, j)
short i;
short j;
{
	return 0;
}

int
AVdraw(i)
short i;
{
	return 0;
}

int
AVcancel(i)
short i;
{
	return 0;
}

int
AVfinish(i)
short i;
{
	return 0;
}

int
clean_all_charts()
{
	return 0;
}

#endif	/* NO_CHARTS */


#if	NO_CLIENT

void
AClientRegisterRemoteVisionFunctions(p)
void *p;
{
	return;
}

void
AClientDeregisterRemoteVisionFunctions(p)
void *p;
{
	return;
}

void
AClientRegisterRemoteFileFunctions(p)
void *p;
{
	return;
}

void
AClientDeregisterRemoteFileFunctions(p)
void *p;
{
	return;
}

#endif	/* NO_CLIENT */


#if	NO_ACUCONNECT

int
AcuConnectCallRemote(n, c, p)
char *n;
int c;
Argument *p;
{
	return -1;
}

int
AcuConnectCancelRemote(n)
char *n;
{
	return -1;
}

void
AcuConnectStartup()
{
	return;
}

void
AcuConnectShutdown()
{
	return;
}

int
AcuConnectCallASync(n, c, a, i)
char *n;
int c;
Argument *a;
int i;
{
	return 0;
}

#endif	/* NO_ACUCONNECT */


#if	NO_CLIENT && NO_ACUCONNECT

void *
AClCoSetGetPwdFunc(p)
void *p;
{
	return NULL;
}

char *
AClCoPassword()
{
	static char p[9] = "        ";
	return p;
}

void *
AClCoRegisterProcedure(n, f)
char *n;
void *f;
{
	return NULL;
}

#endif	/* NO_CLIENT && NO_ACUCONNECT */


#if	NO_SOCKETS

int
AGS_startup()
{
	return 0;
}

int
AGS_shutdown()
{
	return 0;
}

int
AGS_gethostname(p, l)
char *p;
int l;
{
	return -1;
}

unsigned
AGS_srv_make_conn(p, lpfn)
int p;
void *lpfn;
{
	return 0;
}

int
AGS_get_socket_port(h)
unsigned h;
{
	return -1;
}

void
*AGS_set_read_ready_func(h, p)
unsigned h;
void *p;
{
	return NULL;
}

unsigned
AGS_accept_conn(h)
unsigned h;
{
	return 0;
}

unsigned
AGS_clnt_make_conn(p, s, lpfn)
int p;
char *s;
void *lpfn;
{
	return 0;
}

void
AGS_set_user_data(h, p)
unsigned h;
void *p;
{
	return;
}

void *
AGS_get_user_data(h)
unsigned h;
{
	return NULL;
}

int
AGS_close_socket(h)
unsigned h;
{
	return 1;
}

int
AGS_write(h, b, l, f)
unsigned h;
char *b;
int l, f;
{
	return -1;
}

int AGS_read(h, b, l, f)
unsigned h;
char *b;
int l, f;
{
	return -1;
}

int
AGS_flush(h, ip1, ip2)
unsigned h;
int *ip1, *ip2;
{
	return -1;
}

int
AGS_run(h, t)
unsigned h;
long t;
{
	return -1;
}

int
AGS_control(h, i, p)
unsigned h;
int i;
void *p;
{
	return 0;
}

void
AGS_trash(h, l)
unsigned h;
int l;
{
	return;
}

void
AGS_sockethost(h, b, l)
unsigned h;
char *b;
int l;
{
	return;
}

int
AGS_get_last_error(h)
unsigned h;
{
	return 10093;
}

int
AGS_input(i)
int i;
{
	return 0;
}

unsigned int
AGS_create_from_socket(a, lpfn)
int a;
void *lpfn;
{
	return 0;
}

#ifdef	WINNT
FARPROC
AGS_set_blocking_hook(lpfn)
FARPROC	lpfn;
{
	return (FARPROC)NULL;
}

int
AGS_unhook_blocking_hook()
{
	return 0;
}
#endif	/* WINNT */

#endif	/* NO_SOCKETS */


#if	NO_ACUSQL

void ESQL_Initialize P_((char *));

void
ESQL_Initialize(junk)
char *junk;
{
	stop_runtime(255, 1, "No AcuSQL support");
}

void
ESQL_Shutdown()
{
	return;
}

int ESQL_CBL_Dispatch P_((char *, int, Argument [], int));

int
ESQL_CBL_Dispatch(name, nargs, args, initial)
char *name;
int nargs;
Argument args[];
int initial;
{
	stop_runtime(255, 1, "No AcuSQL support");
	return 0;
}

void ESQL_GetVersion P_((char *));

void
ESQL_GetVersion(ver)
char *ver;
{
	*ver = '\0';
}

void ESQL_RegisterOneProcedure P_((char *, void *));

void
ESQL_RegisterOneProcedure( p, pf )
char *p;
void *pf;
{
    return;
}

#endif	/* NO_ACUSQL */


#if	NO_MQSERIES

#ifndef	MQC_INCLUDED                  /* File not yet included? */
#define MQC_INCLUDED                  /* Show file now included */
#if	defined(__cplusplus)
extern "C" {
#endif	/* defined(__cplusplus) */

/* Function Entry-Point and Pointer Attributes */
#define MQENTRY
#define MQPOINTER *

/* Other Datatypes */
typedef char MQCHAR;
typedef MQCHAR MQPOINTER PMQCHAR;
typedef long MQLONG;
typedef MQLONG MQPOINTER PMQLONG;
typedef MQLONG MQHCONN;
typedef MQHCONN MQPOINTER PMQHCONN;
typedef MQLONG MQHOBJ;
typedef MQHOBJ MQPOINTER PMQHOBJ;
typedef void MQPOINTER MQPTR;
typedef MQPTR MQPOINTER PMQPTR;
typedef void MQPOINTER PMQVOID;
typedef PMQVOID MQPOINTER PPMQVOID;

#if	defined(__STDC__) || defined(__cplusplus)

void MQENTRY MQBACK(MQHCONN, PMQLONG, PMQLONG);
void MQENTRY MQCLOSE(MQHCONN, PMQHOBJ, MQLONG, PMQLONG, PMQLONG);
void MQENTRY MQCMIT(MQHCONN, PMQLONG, PMQLONG);
void MQENTRY MQCONN(PMQCHAR, PMQHCONN, PMQLONG, PMQLONG);
void MQENTRY MQDISC(PMQHCONN, PMQLONG, PMQLONG);
void MQENTRY MQGET(MQHCONN, MQHOBJ, PMQVOID, PMQVOID, MQLONG, PMQVOID,
	PMQLONG, PMQLONG, PMQLONG);
void MQENTRY MQINQ(MQHCONN, MQHOBJ, MQLONG, PMQLONG, MQLONG, PMQLONG,
	MQLONG, PMQCHAR, PMQLONG, PMQLONG);
void MQENTRY MQOPEN(MQHCONN, PMQVOID, MQLONG, PMQHOBJ, PMQLONG, PMQLONG);
void MQENTRY MQPUT(MQHCONN, MQHOBJ, PMQVOID, PMQVOID, MQLONG, PMQVOID,
	PMQLONG, PMQLONG);
void MQENTRY MQPUT1(MQHCONN, PMQVOID, PMQVOID, PMQVOID, MQLONG, PMQVOID,
	PMQLONG, PMQLONG);
void MQENTRY MQSET(MQHCONN, MQHOBJ, MQLONG, PMQLONG, MQLONG, PMQLONG, MQLONG,
	PMQCHAR, PMQLONG, PMQLONG);

void MQENTRY
MQBACK(
MQHCONN  Hconn,      /* Connection handle */
PMQLONG  pCompCode,  /* Completion code */
PMQLONG  pReason)    /* Reason code qualifying CompCode */
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQCLOSE(
MQHCONN  Hconn,      /* Connection handle */
PMQHOBJ  pHobj,      /* Object handle */
MQLONG   Options,    /* Options that control the action of MQCLOSE */
PMQLONG  pCompCode,  /* Completion code */
PMQLONG  pReason)    /* Reason code qualifying CompCode */
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQCMIT(
MQHCONN  Hconn,      /* Connection handle */
PMQLONG  pCompCode,  /* Completion code */
PMQLONG  pReason)    /* Reason code qualifying CompCode */
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQCONN(
PMQCHAR   pName,      /* Name of queue manager */
PMQHCONN  pHconn,     /* Connection handle */
PMQLONG   pCompCode,  /* Completion code */
PMQLONG   pReason)    /* Reason code qualifying CompCode */
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQDISC(
PMQHCONN  pHconn,     /* Connection handle */
PMQLONG   pCompCode,  /* Completion code */
PMQLONG   pReason)    /* Reason code qualifying CompCode */
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQGET(
MQHCONN  Hconn,         /* Connection handle */
MQHOBJ   Hobj,          /* Object handle */
PMQVOID  pMsgDesc,      /* Message descriptor */
PMQVOID  pGetMsgOpts,   /* Options that control the action of MQGET */
MQLONG   BufferLength,  /* Length in bytes of the Buffer area */
PMQVOID  pBuffer,       /* Area to contain the message data */
PMQLONG  pDataLength,   /* Length of the message */
PMQLONG  pCompCode,     /* Completion code */
PMQLONG  pReason)       /* Reason code qualifying CompCode */
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQINQ(
MQHCONN  Hconn,           /* Connection handle */
MQHOBJ   Hobj,            /* Object handle */
MQLONG   SelectorCount,   /* Count of selectors */
PMQLONG  pSelectors,      /* Array of attribute selectors */
MQLONG   IntAttrCount,    /* Count of integer attributes */
PMQLONG  pIntAttrs,       /* Array of integer attributes */
MQLONG   CharAttrLength,  /* Length of character attributes buffer */
PMQCHAR  pCharAttrs,      /* Character attributes */
PMQLONG  pCompCode,       /* Completion code */
PMQLONG  pReason)         /* Reason code qualifying CompCode */
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQOPEN(
MQHCONN  Hconn,      /* Connection handle */
PMQVOID  pObjDesc,   /* Object descriptor */
MQLONG   Options,    /* Options that control the action of MQOPEN */
PMQHOBJ  pHobj,      /* Object handle */
PMQLONG  pCompCode,  /* Completion code */
PMQLONG  pReason)    /* Reason code qualifying CompCode */
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQPUT(
MQHCONN  Hconn,         /* Connection handle */
MQHOBJ   Hobj,          /* Object handle */
PMQVOID  pMsgDesc,      /* Message descriptor */
PMQVOID  pPutMsgOpts,   /* Options that control the action of MQPUT */
MQLONG   BufferLength,  /* Length of the message in Buffer */
PMQVOID  pBuffer,       /* Message data */
PMQLONG  pCompCode,     /* Completion code */
PMQLONG  pReason)       /* Reason code qualifying CompCode */
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQPUT1(
MQHCONN  Hconn,         /* Connection handle */
PMQVOID  pObjDesc,      /* Object descriptor */
PMQVOID  pMsgDesc,      /* Message descriptor */
PMQVOID  pPutMsgOpts,   /* Options that control the action of MQPUT1 */
MQLONG   BufferLength,  /* Length of the message in Buffer */
PMQVOID  pBuffer,       /* Message data */
PMQLONG  pCompCode,     /* Completion code */
PMQLONG  pReason)       /* Reason code qualifying CompCode */
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY MQSET (
MQHCONN  Hconn,           /* Connection handle */
MQHOBJ   Hobj,            /* Object handle */
MQLONG   SelectorCount,   /* Count of selectors */
PMQLONG  pSelectors,      /* Array of attribute selectors */
MQLONG   IntAttrCount,    /* Count of integer attributes */
PMQLONG  pIntAttrs,       /* Array of integer attributes */
MQLONG   CharAttrLength,  /* Length of character attributes buffer */
PMQCHAR  pCharAttrs,      /* Character attributes */
PMQLONG  pCompCode,       /* Completion code */
PMQLONG  pReason)         /* Reason code qualifying CompCode */
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

#else	/* defined(__STDC__) || defined(__cplusplus) */

void MQENTRY
MQBACK()
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQCLOSE()
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQCMIT()
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQCONN()
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQDISC()
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQGET()
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQINQ()
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQOPEN()
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQPUT()
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQPUT1()
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

void MQENTRY
MQSET()
{
	stop_runtime(255, 1, Msg_MQSERIESModuleRemoved);
}

#endif	/* defined(__STDC__) || defined(__cplusplus) */

#if	defined(__cplusplus)
}	/* extern "C" */
#endif	/* defined(__cplusplus) */

#endif	/* MQC_INCLUDED */

#endif	/* NO_MQSERIES */

/* that's all! */
