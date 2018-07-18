/* Copyright (c) 1995-2007 by Acucorp, Inc.  All rights reserved.	*/
/* Users of the ACUCOBOL-GT runtime may freely modify and distribute	*/
/* this file as they see fit in order to support an ACUCOBOL-GT based	*/
/* application.								*/

/* THIS FILE IS #INCLUDED FROM sub.c.  BECAUSE SYSTEM HEADER FILES	*/
/* SHOULD BE INCLUDED BEFORE sub.h, AND BECAUSE THIS FILE IS INCLUDED	*/
/* AFTER sub.h, YOU REALLY SHOULDN'T INCLUDE ANY SYSTEM HEADER FILES	*/
/* FROM THIS FILE.							*/

/* Sometimes it is desirable to create smaller versions of the runtime	*/
/* system.  This file allows you to remove selected pieces of the	*/
/* runtime.  By removing pieces of the runtime, you can make it		*/
/* smaller.  Of course, you will not be able to use the piece that you	*/
/* remove.  You can make the runtime smaller if memory space on the	*/
/* machine is very tight, or if you want to minimize the overhead	*/
/* associated with the runtime.						*/

/* To reconfigure the runtime, you will need the 'C' compiler for the	*/
/* target machine.  See Appendix C for the list of which 'C' compiler	*/
/* to use for a particular machine.  Once you have selected the		*/
/* configuration options that you want to use, recreate the runtime	*/
/* using the instructions listed in that Appendix.			*/


/* You can remove the runtime source debugger by setting the following	*/
/* value to 1.  This will save 17+ Kbytes depending on your machine.	*/
/* One idea is to leave the debugger in the version of the runtime you	*/
/* develop with, but to remove it from the runtime you send to your	*/
/* users.  This way, you can still take advantage of the debugger while	*/
/* using less space on the user's machine.				*/

#ifdef	ACU_SOURCE_FILENAME
#undef	ACU_SOURCE_FILENAME
#endif	/* ACU_SOURCE_FILENAME */
#define	ACU_SOURCE_FILENAME	"lib/config85.c"
const char what_lib_config85_c_str[] = "@(#) " ACU_SOURCE_FILENAME " $Date:$$Rev: 59605 $";

#ifndef	NO_DEBUGGER
#define	NO_DEBUGGER	0
#endif	/* NO_DEBUGGER */


/* Set the following to 1 to remove support for Floating-Point numbers.	*/

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


/* If you set USE_EXTSM to 1, then an external SORT library will be	*/
/* necessary, and you will be be able to use an external SORT		*/
/* routine by using the CALL verb and setting the USE_EXTSM config	*/
/* variable.								*/

#ifndef	USE_EXTSM
#define	USE_EXTSM	0
#endif	/* USE_EXTSM */


/* If you set INT_POWER to 1, then only integer exponents will be	*/
/* supported.  This prevents you from taking roots, but it saves a fair	*/
/* amount of code in the runtime.  In particular, it removes code that	*/
/* computes natural logs and code that computes e^x.  This saves	*/
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


/* If you set NO_JAVA to 1, then the java will be removed from the	*/
/* runtime system.  This will prevent					*/
/* you from using any programs that use java interoperability		*/
/* This saves about 80 Kbytes of code.					*/

#ifndef	NO_JAVA
#define	NO_JAVA		0
#endif	/* NO_JAVA */

/* If you set NO_CHARTS to 1, then the charting capability will be	*/
/* removed from the runtime system.  This will prevent you from		*/
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


/* If you set NO_CICS to 1, then the CICS libraries will not be		*/
/* necessary, and you will not be able to execute CICS calls.		*/

#ifndef	NO_CICS
#define	NO_CICS			1
#endif	/* NO_CICS */


/* If you set NO_ZLIB to 1, then stub functions will be used instead of	*/
/* the compression library, meaning no compression will be available.	*/
/* Set this if your port was built with the compression library, but	*/
/* that compression library is not available on your machine.  (The	*/
/* compression library is libz.a or libz.so or something similar.)	*/

#ifndef	NO_ZLIB
#define	NO_ZLIB			0
#endif	/* NO_ZLIB */

/************************************************************************/
/************************************************************************/
/*									*/
/* The rest of this file implements the configuration options.		*/
/* You will not need to modify anything below.				*/
/*									*/
/************************************************************************/

/* NO_SORT implies !USE_EXTSM */
#if	NO_SORT && USE_EXTSM
#undef	USE_EXTSM
#define	USE_EXTSM	0
#endif	/* NO_SORT && USE_EXTSM */

extern void stop_runtime(int, int, char *);

/* The following section is extracted from "message/message.h" */

extern char *message_get(int, int, char *);

#define Agetmsg(n,s)	(message_get((n), 0, (s)))

#define	Msg_SorryTheDebuggerHasBeenRem		Agetmsg(1, \
	"Sorry, the debugger has been removed from this runtime!\n")
#define	Msg_RuntimeAbortedN			Agetmsg(2, \
	"*** Runtime Aborted ***\n")
#define	Msg_FloatingPointModuleRemoved		Agetmsg(3, \
	"Floating-Point module removed!\n")
#define	Msg_SORTMERGEModuleRemovedn		Agetmsg(4, \
	"SORT/MERGE module removed!\n")
#define	Msg_ScreenSectionSupportRemove		Agetmsg(5, \
	"Screen Section support removed!\n")
#define	Msg_ChartSupportRemovedn		Agetmsg(6, \
	"Chart support removed!\n")
#define	Msg_NoExternalSORTRoutineLinkedIn	Agetmsg(1794, \
	"No EXTSM routine linked in!\n")

/* end "message/message.h" */

#ifdef	_MSC_VER
#ifndef	_WINDOWS
#pragma	check_stack(off)
#endif	/* _WINDOWS */
#endif	/* _MSC_VER */

/*
** deal with warnings about unused parameters
*/
#ifdef	HAVE_ATTRIBUTE
#define	A_UNUSED	__attribute__((unused))
#else	/* HAVE_ATTRIBUTE */
#define	A_UNUSED
#endif	/* HAVE_ATTRIBUTE */

#if	NO_DEBUGGER

extern void eprintf(const char *, ...);

void ensure_A_srcfile(void);
int Aload_line_info(void);
int Atranslate_address(unsigned int, char **, long *);
void AdbgEnableTool(int, int);
void AdbgEnableToolbar(int);
int pgh_trace(void);
int init_debug(void);
void shutdown_debug(void);
void DBGSetMonitor(char *);
int debugger(void);
void Aload_dbg(void);
void DBGclose_windows(void);
char *Adbgalloc(void);
long A_FSEEK(void);
int A_FILBUF(void);
void A_FREAD(void);
int AconditionTrue(void);
void Aload_src_table(void);
void Ado_help(void);
void AsaveDebuggerSettings(void);
void AloadDebuggerSettings(void);
void AcreateDebugToolbar(void);
int Atranslate_line(int *, char *, long);
int DBGBreakpointCommand(char *, int);
void AcountParaEntry(void);
void AprofileTickOccurred(void *);
int AGetProfileFilename(char *, size_t, int);
void AprintProfile(void *);
void AprintProfileHeader(void);
void AprintProfileTrailer(void);
int AdumpRuntimeState(char *, char *);
void AfinishProfile(void);
void AXExit(void);
void *Aset_trace_vars(const char *, unsigned int, long, unsigned);

int A_in_debugger = 0;
unsigned int Adbg_canvas = 0;
char Adbg_ready = 0;

void
ensure_A_srcfile(void)
{
    return;
} /* ensure_A_srcfile */

int
Aload_line_info(void)
{
    return 0;
} /* Aload_line_info */

int
Atranslate_address(unsigned int pd_addr A_UNUSED, char **pFilename A_UNUSED,
	long *pLine A_UNUSED)
{
    return 0;
} /* Atranslate_address */

void
AdbgEnableTool(int toolNum A_UNUSED, int enabled A_UNUSED)
{
    return;
} /* AdbgEnableTool */

void
AdbgEnableToolbar(int enabled A_UNUSED)
{
    return;
} /* AdbgEnableToolbar */

int
pgh_trace(void)
{
    return 0;
} /* pgh_trace */

int
init_debug(void)
{
    eprintf(Msg_SorryTheDebuggerHasBeenRem);
    eprintf(Msg_RuntimeAbortedN);
    return 0;
} /* init_debug */

void
shutdown_debug(void)
{
    return;
} /* shutdown_debug */

void
DBGSetMonitor(char *p A_UNUSED)
{
    return;
} /* DBGSetMonitor */

int
debugger(void)
{
    return 0;
} /* debugger */

void
Aload_dbg(void)
{
    return;
} /* Aload_dbg */

void
DBGclose_windows(void)
{
    return;
} /* DBGClose_windows */

char *
Adbgalloc(void)
{
    return NULL;
} /* Adbgalloc */

long
A_FSEEK(void)
{
    return 0L;
} /* A_FSEEK */

int
A_FILBUF(void)
{
    return 0;
} /* A_FILBUF */

void
A_FREAD(void)
{
    return;
} /* A_FREAD */

int
AconditionTrue(void)
{
    return 0;
} /* AconditionTrue */

void
Aload_src_table(void)
{
    return;
} /* Aload_src_table */

void
Ado_help(void)
{
    return;
} /* Ado_help */

void
AsaveDebuggerSettings(void)
{
    return;
} /* AsaveDebuggerSettings */

void
AloadDebuggerSettings(void)
{
    return;
} /* AloadDebuggerSettings */

void
AcreateDebugToolbar(void)
{
    return;
} /* AcreateDebugToolbar */

int
Atranslate_line(int *p1 A_UNUSED, char *p2 A_UNUSED, long l A_UNUSED)
{
    return -1;
} /* Atranslate_line */

int
DBGBreakpointCommand(char *p A_UNUSED, int q A_UNUSED)
{
    return -1;
} /* DBGBreakpointCommand */

void
AcountParaEntry(void)
{
    return;
} /* AcountParaEntry */

void
AprofileTickOccurred(void *pData A_UNUSED)
{
    return;
} /* AprofileTickOccurred */

int
AGetProfileFilename(char *n A_UNUSED, size_t l A_UNUSED, int b A_UNUSED)
{
    return (0);
} /* AGetProfileFilename */

void
AprintProfile(void *pPgm A_UNUSED)
{
    return;
} /* AprintProfile */

void
AprintProfileHeader(void)
{
    return;
} /* AprintProfileHeader */

void
AprintProfileTrailer(void)
{
    return;
} /* AprintProfileTrailer */

int
AdumpRuntimeState(char *reasonText A_UNUSED, char *dumpFileName A_UNUSED)
{
    return 0;
} /* AdumpRuntimeState */

void
AfinishProfile(void)
{
    return;
} /* AfinishProfile */

void
AXExit(void)
{
    return;
} /* AXExit */

void *
Aset_trace_vars(const char *var A_UNUSED, unsigned int hval A_UNUSED,
	long lval A_UNUSED, unsigned show A_UNUSED)
{
    return NULL;
} /* Aset_trace_vars */

#endif	/* NO_DEBUGGER */


#ifdef	NOFLOAT
#undef	NO_FLOAT
#define	NO_FLOAT	1
#endif	/* NOFLOAT */

#if	NO_FLOAT

int float_convert(void);
void float_to_num(void);
void float_store(void);
void float_to_float(void);
void float_op_to_num(void);
void float_add(void);
void float_sub(void);
void float_mult(void);
int float_div(void);
void float_negate(void);
void float_pow(void);
void float_cmp(void);
void float_set_remainder(void);
void float_move(void);
long float_cvt_long(void);
int float_scan(void);
void float_inc(void);
void exfloat_to_num(void);
void move_uefp_to_efp(void);
void exfloat_op_to_num(void);
void exfloat_to_float(void);
void move_ncs_to_efp(void);
void move_fp_to_efp(void);
void exfloat_add(void);
void exfloat_multiply(void);
void exfloat_divide(void);
void num_to_exfloat(void);
int exfloat_normalize(void);
int exfloat_scan(void);
int exfloat_compare(void);
int exfloat_power(void);
void float_move_check(void);

int
float_convert(void)
{
    stop_runtime(255, 1, Msg_FloatingPointModuleRemoved);
    return 0;
} /* float_convert */

void
float_to_num(void)
{
    float_convert();
} /* float_to_num */

void
float_store(void)
{
    float_convert();
} /* float_store */

void
float_to_float(void)
{
    float_convert();
} /* float_to_float */

void
float_op_to_num(void)
{
    float_convert();
} /* float_op_to_num */

void
float_add(void)
{
    float_convert();
} /* float_add */

void
float_sub(void)
{
    float_convert();
} /* float_sub */

void
float_mult(void)
{
    float_convert();
} /* float_mult */

int
float_div(void)
{
    float_convert();
    return 1;
} /* float_div */

void
float_negate(void)
{
    float_convert();
} /* float_convert */

void
float_pow(void)
{
    float_convert();
} /* float_pow */

void
float_cmp(void)
{
    float_convert();
} /* float_pow */

void
float_set_remainder(void)
{
    float_convert();
} /* float_set_remainder */

void
float_move(void)
{
    float_convert();
} /* float_move */

long
float_cvt_long(void)
{
    float_convert();
    return 0L;
} /* float_cvt_long */

int
float_scan(void)
{
    float_convert();
    return 0;
} /* float_scan */

void
float_inc(void)
{
    float_convert();
} /* float_inc */

void
exfloat_to_num(void)
{
    float_convert();
} /* exfloat_to_num */

void
move_uefp_to_efp(void)
{
    float_convert();
} /* move_uefp_to_efp */

void
exfloat_op_to_num(void)
{
    float_convert();
} /* exfloat_op_to_num */

void
exfloat_to_float(void)
{
    float_convert();
} /* exfloat_to_float */

void
move_ncs_to_efp(void)
{
    float_convert();
} /* move_ncs_to_efp */

void
move_fp_to_efp(void)
{
    float_convert();
} /* move_fp_to_efp */

void
exfloat_add(void)
{
    float_convert();
} /* exfloat_add */

void
exfloat_multiply(void)
{
    float_convert();
} /* exfloat_multiply */

void
exfloat_divide(void)
{
    float_convert();
} /* exfloat_divide */

void
num_to_exfloat(void)
{
    float_convert();
} /* num_to_exfloat */

int
exfloat_normalize(void)
{
    float_convert();
    return 0;
} /* exfloat_normalize */

int
exfloat_scan(void)
{
    float_convert();
    return 0;
} /* exfloat_scan */

int
exfloat_compare(void)
{
    float_convert();
    return 0;
} /* exfloat_compare */

int
exfloat_power(void)
{
    float_convert();
    return 0;
} /* exfloat_power */

void
float_move_check(void)
{
    float_convert();
} /* float_move_check */

#endif	/* NO_FLOAT */


#if	NO_SORT

static void no_sort_error(void);

static void
no_sort_error(void)
{
    stop_runtime(255, 1, Msg_SORTMERGEModuleRemovedn);
} /* no_sort_error */

/* sort.c */

void Asort_init_sort(int);
int Asort_sort_input(void);
void Asort_do_sort(void);
int Asort_sort_output(void);
void Asort_end(void);
void Asort_do_release(void);
void Asort_do_return(void);
void Asort_init_merge(int);
int Asort_merge_input(void);
int Asort_prime_merge(void);
int Asort_get_phase(void);
void *Asort_get_program(void);
int SortTableCompare(char *, char *);
int c_sort_table(char *, int, Argument *, int);

void
Asort_init_sort(int pgmAddr A_UNUSED)
{
    no_sort_error();
} /* Asort_init_sort */

int
Asort_sort_input(void)
{
    return 0;
} /* Asort_sort_input */

void
Asort_do_sort(void)
{
} /* Asort_do_sort */

int
Asort_sort_output(void)
{
    return 0;
} /* Asort_sort_output */

void
Asort_end(void)
{
} /* Asort_end */

void
Asort_do_release(void)
{
    no_sort_error();
} /* Asort_do_release */

void
Asort_do_return(void)
{
    no_sort_error();
} /* Asort_do_return */

void
Asort_init_merge(int pgmAddr A_UNUSED)
{
    no_sort_error();
} /* Asort_init_merge */

int
Asort_merge_input(void)
{
    return 0;
} /* Asort_merge_input */

int
Asort_prime_merge(void)
{
    return 0;
} /* Asort_prime_merge */

int
Asort_get_phase(void)
{
    return 0;
} /* Asort_get_phase */

void *
Asort_get_program(void)
{
    return (void *)0;
} /* Asort_get_program */

int
SortTableCompare(char *a A_UNUSED, char *b A_UNUSED)
{
    return 0;
} /* SortTableCompare */

int
c_sort_table(char *name A_UNUSED, int num_args A_UNUSED,
	Argument *args A_UNUSED, int initial A_UNUSED)
{
    no_sort_error();
    return -1;
} /* c_sort_table */


/* acusm.c */

void Aacusm_init_sort(void *);
int Aacusm_sort_input(void);
void Aacusm_do_sort(void *);
int Aacusm_sort_output(void);
void Aacusm_end(void);
void Aacusm_do_release(void *);
void Aacusm_do_return(void *);
void Aacusm_init_merge(void *);
int Aacusm_merge_input(void);
int Aacusm_prime_merge(void);

void
Aacusm_init_sort(void *ptr A_UNUSED)
{
    no_sort_error();
} /* Aacusm_init_sort */

int
Aacusm_sort_input(void)
{
    return 0;
} /* Aacusm_sort_input */

void
Aacusm_do_sort(void *ptr A_UNUSED)
{
    return;
} /* Aacusm_do_sort */

int
Aacusm_sort_output(void)
{
    return 0;
} /* Aacusm_sort_output */

void
Aacusm_end(void)
{
    return;
} /* Aacusm_end */

void
Aacusm_do_release(void *ptr A_UNUSED)
{
    no_sort_error();
} /* Aacusm_do_release */

void
Aacusm_do_return(void *ptr A_UNUSED)
{
    no_sort_error();
} /* Aacusm_do_return */

void
Aacusm_init_merge(void *ptr A_UNUSED)
{
    no_sort_error();
} /* Aacusm_init_merge */

int
Aacusm_merge_input(void)
{
    return 0;
} /* Aacusm_merge_input */

int
Aacusm_prime_merge(void)
{
    return 0;
} /* Aacusm_prime_merge */

#endif	/* NO_SORT */


#if	!USE_EXTSM

static void use_extsm_error(void);

static void
use_extsm_error(void)
{
    stop_runtime(255, 1, Msg_NoExternalSORTRoutineLinkedIn);
} /* use_extsm_error */

/* extsm.c */

void Aextsm_init_sort(void *);
int Aextsm_sort_input(void);
void Aextsm_do_sort(void *);
int Aextsm_sort_output(void);
void Aextsm_end(void);
void Aextsm_do_release(void *);
void Aextsm_do_return(void *);
void Aextsm_init_merge(void *);
int Aextsm_merge_input(void);
int Aextsm_prime_merge(void);
void EXTSM(void *, void *);

int EXTFH_initialized;

void
Aextsm_init_sort(void *ptr A_UNUSED)
{
    use_extsm_error();
} /* Aextsm_init_sort */

int
Aextsm_sort_input(void)
{
    return 0;
} /* Aextsm_sort_input */

void
Aextsm_do_sort(void *ptr A_UNUSED)
{
    return;
} /* Aextsm_do_sort */

int
Aextsm_sort_output(void)
{
    return 0;
} /* Aextsm_sort_output */

void
Aextsm_end(void)
{
    return;
} /* Aextsm_end */

void
Aextsm_do_release(void *ptr A_UNUSED)
{
    use_extsm_error();
} /* Aextsm_do_release */

void
Aextsm_do_return(void *ptr A_UNUSED)
{
    use_extsm_error();
} /* Aextsm_do_return */

void
Aextsm_init_merge(void *ptr A_UNUSED)
{
    use_extsm_error();
} /* Aextsm_init_merge */

int
Aextsm_merge_input(void)
{
    return 0;
} /* Aextsm_merge_input */

int
Aextsm_prime_merge(void)
{
    return 0;
} /* Aextsm_prime_merge */

void
EXTSM(void *p1 A_UNUSED, void *p2 A_UNUSED)
{
    return;
} /* EXTSM */

#endif	/* !USE_EXTSM */


#if	INT_POWER

extern void do_int_power(void);

void do_real_power(void);

void
do_real_power(void)
{
    do_int_power();
} /* do_real_power */

#endif	/* INT_POWER */



#if	NO_SCRN_SECTION

void Ascr_destroy(void);
int Ascr_display(void);
void AContAcceptLoop(void);
int Ascr_accept(void);
int Ascr_handle(void);

void
Ascr_destroy(void)
{
    stop_runtime(255, 1, Msg_ScreenSectionSupportRemove);
} /* Ascr_destroy */

int
Ascr_display(void)
{
    stop_runtime(255, 1, Msg_ScreenSectionSupportRemove);
    return 0;
} /* Ascr_display */

void
AContAcceptLoop(void)
{
    return;
} /* AContAcceptLoop */

int
Ascr_accept(void)
{
    stop_runtime(255, 1, Msg_ScreenSectionSupportRemove);
    return 0;
} /* Ascr_accept */

int
Ascr_handle(void)
{
    stop_runtime(255, 1, Msg_ScreenSectionSupportRemove);
    return 0;
} /* Ascr_handle */

#endif	/* NO_SCRN_SECTION */


#if	NO_JAVA

void AInitJavaRuntime(void);
int ACallJavaMethod(char *, int, Argument []);
void AUnloadJavaRuntime(void);

void
AInitJavaRuntime(void)
{
    return;
} /* AInitJavaRuntime */

int
ACallJavaMethod(char *name A_UNUSED, int num_args A_UNUSED,
	Argument args[] A_UNUSED)
{
    return 0;
} /* ACallJavaMethod */

void
AUnloadJavaRuntime(void)
{
    return;
} /* AUnloadJavaRuntime */

#endif	/* NO_JAVA */

#if	NO_CHARTS

int CheckAVLicense(void);
void AVinit_cfg(void);
int AVinit(short);
int AVparm(short);
int AVdata(short, short);
int AVdraw(short);
int AVcancel(short);
int AVfinish(short);
int clean_all_charts(void);

int	AV_errno;
short	AV_error;

int
CheckAVLicense(void)
{
    return 0;
} /* CheckAVLicense */

void
AVinit_cfg(void)
{
    return;
} /* AVinit_cfg */

int
AVinit(short i A_UNUSED)
{
    stop_runtime(255, 1, Msg_ChartSupportRemovedn);
    return 0;
} /* AVinit */

int
AVparm(short i A_UNUSED)
{
    return 0;
} /* AVparm */

int
AVdata(short i A_UNUSED, short j A_UNUSED)
{
    return 0;
} /* AVdata */

int
AVdraw(short i A_UNUSED)
{
    return 0;
} /* AVdraw */

int
AVcancel(short i A_UNUSED)
{
    return 0;
} /* AVcancel */

int
AVfinish(short i A_UNUSED)
{
    return 0;
} /* AVfinish */

int
clean_all_charts(void)
{
    return 0;
} /* clean_all_charts */

#endif	/* NO_CHARTS */


#if	NO_CLIENT

void AClientRegisterRemoteVisionFunctions(void *);
void AClientDeregisterRemoteVisionFunctions(void *);
void AClientRegisterRemoteFileFunctions(void *);
void AClientDeregisterRemoteFileFunctions(void *);
void Aremote_stderr_close(void);

void *client_mutex;

void
AClientRegisterRemoteVisionFunctions(void *p A_UNUSED)
{
    return;
} /* AClientRegisterRemoteVisionFunctions */

void
AClientDeregisterRemoteVisionFunctions(void *p A_UNUSED)
{
    return;
} /* AClientDeregisterRemoteVisionFunctions */

void
AClientRegisterRemoteFileFunctions(void *p A_UNUSED)
{
    return;
} /* AClientRegisterRemoteFileFunctions */

void
AClientDeregisterRemoteFileFunctions(void *p A_UNUSED)
{
    return;
} /* AClientDeregisterRemoteFileFunctions */

void
Aremote_stderr_close(void)
{
    return;
} /* Aremote_stderr_close */


#endif	/* NO_CLIENT */


#if	NO_ACUCONNECT

int AcuConnectCallRemote(char *, int, Argument *);
int AcuConnectCancelRemote(char *);
void AcuConnectStartup(void);
void AcuConnectShutdown(void);
int AcuConnectCallASync(char *, int, Argument *, int);
unsigned AClientPTRTConnect(short);
unsigned AClientPTRTSend(const char *, size_t);
size_t AClientPTRTRecv(char *, size_t);
unsigned AClientPTRTStatus(const char *, size_t, int);
unsigned AClientPTRTStop(const char *, size_t);
void AClientPTRTClose(void);


int
AcuConnectCallRemote(char *n A_UNUSED, int c A_UNUSED, Argument *p A_UNUSED)
{
    return -1;
} /* AcuConnectCallRemote */

int
AcuConnectCancelRemote(char *n A_UNUSED)
{
    return -1;
} /* AcuConnectCancelRemote */

void
AcuConnectStartup(void)
{
    return;
} /* AcuConnectStartup */

void
AcuConnectShutdown(void)
{
    return;
} /* AcuConnectShutdown */

int
AcuConnectCallASync(char *n A_UNUSED, int c A_UNUSED, Argument *a A_UNUSED,
	int i A_UNUSED)
{
    return 0;
} /* AcuConnectCallASync */

unsigned
AClientPTRTConnect(short port A_UNUSED)
{
    return 0;
} /* AClientPTRTConnect */

unsigned
AClientPTRTSend(const char *data A_UNUSED, size_t len A_UNUSED)
{
    return 0;
} /* AClientPTRTSend */

size_t
AClientPTRTRecv(char *data A_UNUSED, size_t len A_UNUSED)
{
    return 0;
} /* AClientPTRTRecv */

unsigned
AClientPTRTStatus(const char *data A_UNUSED, size_t len A_UNUSED,
	int over A_UNUSED)
{
    return 0;
} /* AClientPTRTStatus */

unsigned
AClientPTRTStop(const char *data A_UNUSED, size_t len A_UNUSED)
{
    return 0;
} /* AClientPTRTStop */

void
AClientPTRTClose(void)
{
    return;
} /* AClientPTRTClose */

#endif	/* NO_ACUCONNECT */


#if	NO_CLIENT && NO_ACUCONNECT

void *AClientRegisterProcedure(char *, void *);
int AClientDisconnectFromServer(char *, int);
void *AClientSetGetPwdFunc(void *);
char *AClientPassword(void);
void *AClCoSetGetPwdFunc(void *);
char *AClCoPassword(void);
void *AClCoRegisterProcedure(char *, void *);

void *
AClientRegisterProcedure(char *n A_UNUSED, void *f A_UNUSED)
{
    return NULL;
} /* AClientRegisterProcedure */

int
AClientDisconnectFromServer(char *n A_UNUSED, int i A_UNUSED)
{
    return 0;
} /* AClientDisconnectFromServer */

void *
AClientSetGetPwdFunc(void *pf A_UNUSED)
{
    return NULL;
} /* AClientSetGetPwdFunc */

char *
AClientPassword(void)
{
    return NULL;
} /* AClientPassword */

void *
AClCoSetGetPwdFunc(void *p A_UNUSED)
{
    return NULL;
} /* AClCoSetGetPwdFunc */

char *
AClCoPassword(void)
{
    static char p[9] = "        ";

    return p;
} /* AClCoPassword */

void *
AClCoRegisterProcedure(char *n A_UNUSED, void *f A_UNUSED)
{
    return NULL;
} /* AClCoRegisterProcedure */

#endif	/* NO_CLIENT && NO_ACUCONNECT */


#if	NO_ACUSQL

ESQL_CBL_DispatchProc	ESQL_CBL_DispatchFunc = NULL;
ESQL_InitializeProc	ESQL_InitializeFunc = NULL;
ESQL_ShutdownProc	ESQL_ShutdownFunc = NULL;
ESQL_GetVersionProc	ESQL_GetVersionFunc = NULL;
ESQL_RegisterOneProcedureProc ESQL_RegisterOneProcedureFunc = NULL;

#else	/* NO_ACUSQL */

ESQL_CBL_DispatchProc	ESQL_CBL_DispatchFunc = ESQL_CBL_Dispatch;
ESQL_InitializeProc	ESQL_InitializeFunc = ESQL_Initialize;
ESQL_ShutdownProc	ESQL_ShutdownFunc = ESQL_Shutdown;
ESQL_GetVersionProc	ESQL_GetVersionFunc = ESQL_GetVersion;
ESQL_RegisterOneProcedureProc ESQL_RegisterOneProcedureFunc = ESQL_RegisterOneProcedure;

#endif	/* NO_ACUSQL */

#if	NO_MQSERIES

MQBEGINProc	MQBEGINFunc = NULL;
MQBACKProc	MQBACKFunc = NULL;
MQCLOSEProc	MQCLOSEFunc = NULL;
MQCMITProc	MQCMITFunc = NULL;
#ifdef	WIN32
MQCONNXProc	MQCONNXFunc = NULL;
#endif	/* WIN32 */
MQCONNProc	MQCONNFunc = NULL;
MQDISCProc	MQDISCFunc = NULL;
MQGETProc	MQGETFunc = NULL;
MQINQProc	MQINQFunc = NULL;
MQOPENProc	MQOPENFunc = NULL;
MQPUT1Proc	MQPUT1Func = NULL;
MQPUTProc	MQPUTFunc = NULL;
MQSETProc	MQSETFunc = NULL;

#else	/* NO_MQSERIES */

MQBEGINProc	MQBEGINFunc = MQBEGIN;
MQBACKProc	MQBACKFunc = MQBACK;
MQCLOSEProc	MQCLOSEFunc = MQCLOSE;
MQCMITProc	MQCMITFunc = MQCMIT;
#ifdef	WIN32
MQCONNXProc	MQCONNXFunc = MQCONNX;
#endif	/* WIN32 */
MQCONNProc	MQCONNFunc = MQCONN;
MQDISCProc	MQDISCFunc = MQDISC;
MQGETProc	MQGETFunc = MQGET;
MQINQProc	MQINQFunc = MQINQ;
MQOPENProc	MQOPENFunc = MQOPEN;
MQPUT1Proc	MQPUT1Func = MQPUT1;
MQPUTProc	MQPUTFunc = MQPUT;
MQSETProc	MQSETFunc = MQSET;

#endif	/* NO_MQSERIES */

#if	NO_CICS

CICS_DispatchProc	CICS_DispatchFunc = NULL;
CICS_ListDispatchProc	CICS_ListDispatchFunc = NULL;

#else	/* NO_CICS */

/* If your CICS libraries don't have CICS_EciListSystems, set */
/* the following value to NULL instead of CICS_EciListSystems */
CICS_DispatchProc	CICS_DispatchFunc = CICS_ExternalCall;
CICS_ListDispatchProc	CICS_ListDispatchFunc = CICS_EciListSystems;

#endif	/* NO_CICS */


#if	NO_ZLIB

void *gzdopen(int, const char *);
int gzread(void *, void *, unsigned);
int gzwrite(void *, void *, unsigned);
int gzprintf(void *, const char *, ...);
int gzputs(void *, const char *);
char *gzgets(void *, char *, int);
int gzputc(void *, int);
int gzgetc(void *);
int gzflush(void *, int);
long gzseek(void *, long, int);
long gztell(void *);
int gzclose(void *);
const char * compress(char *, unsigned long *, const char *, unsigned long);
int uncompress(char *, unsigned long *, char *, unsigned long);

void
*gzdopen(int i A_UNUSED, const char *f A_UNUSED)
{
    return NULL;
} /* gzdopen */

int
gzread(void *f A_UNUSED, void *b A_UNUSED, unsigned u A_UNUSED)
{
    return -1;
} /* gzread */

int
gzwrite(void *f A_UNUSED, void *b A_UNUSED, unsigned u A_UNUSED)
{
    return 0;
} /* gzwrite */

int
gzprintf(void *f A_UNUSED, const char *fmt A_UNUSED, ...)
{
    return 0;
} /* gzprintf */

int
gzputs(void *f A_UNUSED, const char *b A_UNUSED)
{
    return -1;
} /* gzputs */

char
*gzgets(void *f A_UNUSED, char *b A_UNUSED, int s A_UNUSED)
{
    return NULL;
} /* gzgets */

int
gzputc(void *f A_UNUSED, int c A_UNUSED)
{
    return -1;
} /* gzputc */

int
gzgetc(void *f A_UNUSED)
{
    return -1;
} /* gzgetc */

int
gzflush(void *f A_UNUSED, int i A_UNUSED)
{
    return -1;
} /* gzflush */

long
gzseek(void *f A_UNUSED, long off A_UNUSED, int w A_UNUSED)
{
    return -1;
} /* gzseek */

long
gztell(void *f A_UNUSED)
{
    return -1;
} /* gztell */

int
gzclose(void *f A_UNUSED)
{
    return -1;
} /* gzclose */

const char *
compress(char *dest A_UNUSED, unsigned long *destlen A_UNUSED,
	const char *src A_UNUSED, unsigned long srclen A_UNUSED)
{
    return NULL;
} /* compress */

int
uncompress(char *dest A_UNUSED, unsigned long *destlen A_UNUSED,
	char *src A_UNUSED, unsigned long srclen A_UNUSED)
{
    return -1;
} /* uncompress */

#endif	/* NO_ZLIB */

/* that's all! */
