/* sub.h - header file to be included in 'C' subroutines  */

/* Copyright (c) 1995-2006 by Acucorp, Inc.  All rights reserved.	*/
/* Users of the ACUCOBOL-GT runtime may freely modify and distribute	*/
/* this file as they see fit in order to support an ACUCOBOL-GT based	*/
/* application.  */

/* This file contains definitions used by the sub.c file.		*/
/* It provides the basic interface structures and definitions.		*/
/* This file is equivalent to the file "rmc85call.h" provided with 	*/
/* Unix versions of RM/COBOL-85 and may be used in place of that file.	*/

#ifndef	ACU_LIB_SUB_H
#define	ACU_LIB_SUB_H

#ifdef	__cplusplus
extern	"C"	{
#endif	/* __cplusplus */

/* PROCTABLE is a structure that relates a CALL name to its 'C' routine	*/

struct	PROCTABLE {
  char	*name;                 /* subroutine name */
#ifdef	A_DOT_NET
  int	(*routine)(char *, ...); /* pointer to function to call */
#else	/* A_DOT_NET */
  int	(*routine)();            /* pointer to function to call */
#endif	/* A_DOT_NET */
};


struct	DIRECTTABLE {
	char	*name;			/* subroutine name */
	int	(*routine)();		/* pointer to routine */
	short	return_type;		/* routine's return type */
};


struct	FORTRANTABLE {
	char	*name;			/* subroutine name */
	int	(*routine)();		/* pointer to routine */
	short	return_type;		/* routine's return type */
};

/* The following are valid values for "return_type" */

#define	C_int		0
#define	C_unsigned	1
#define	C_short		2
#define	C_char		3
#define	C_long		4
#define	C_pointer	5
#define	C_void		6

#define	FUNC	(int(*)())	/* useful cast for DIRECTTABLE entries */


/* EXTRNTABLE is a structure that relates external 'C' variables to 	*/
/* their COBOL external names  */

struct	EXTRNTABLE {
	char	*name;			/* COBOL data name */
	char	*addr;			/* address of data item */
};


/* COMMONTABLE is a structure that relates FORTRAN COMMON to 	*/
/* their COBOL external names  */

struct	COMMONTABLE {
	char	*name;			/* COBOL data name */
	char	*addr;			/* address of data item */
};


/* ARGUMENT_ENTRY is a structure that is passed to the 'C' routine for	*/
/* each USING argument.  It defines the layout of that argument. 	*/

struct	ARGUMENT_ENTRY {
	char	*a_address;		/* pointer to actual data */
	long	a_length;		/* size of data item (in bytes) */
	short	a_type;			/* COBOL data type, see below */
	char	a_digits;		/* # of digits for numeric items */
	char	a_scale;		/* implied decimal point offset */
	char	*a_picture;		/* unused - always NULL */
};

typedef	struct ARGUMENT_ENTRY	Argument;

#define	a_size(arg)	(unsigned)(arg).a_length
#define	Scale(s)	(((s) & 0x80) ? ((-1 << 8) | (s)) : (s))


/* The "a_type" value can be one of the following.  */

#define	NumEdited	0	/* Numeric edited */
#define	NumUnsigned	1	/* Unsigned numeric */
#define	NumSignSep	2	/* Signed numeric (trailing separate) */
#define	NumSigned	3	/* Signed numeric (trailing combined) */
#define	NumSepLead	4	/* Signed numeric (leading separate) */
#define	NumLeading	5	/* Signed numeric (leading combined) */
#define	CompSigned	6	/* Signed computational */
#define	CompUnsigned	7	/* Unsigned computational */
#define	PackedPositive	8	/* Positive packed-decimal */
#define	PackedSigned	9	/* Signed packed-decimal */
#define	PackedUnsigned	10	/* Computational-6 */
#define	BinarySigned	11	/* Signed binary */
#define	BinaryUnsigned	12	/* Unsigned binary */
#define	NativeSigned	13	/* Signed native-order binary */
#define	NativeUnsigned	14	/* Unsigned native-order binary */
#define	Alphanum	16	/* Alphanumeric */
#define	JustAN		17	/* Alphanumeric (justified) */
#define	Alphabetic	18	/* Alphabetic */
#define	JustAlpha	19	/* Alphabetic (justified) */
#define	AlphaEdited	20	/* Alphanumeric Edited */
#define	Group		22	/* Group */
#define	Float		23	/* Float or Double */
#define	National	24	/* National */
#define	JustNat		25	/* National (justified) */
#define	NatEdited	26	/* National edited */
#define	Wide		27	/* Wide */
#define	JustWide	28	/* Wide (justified) */
#define	WideEdited	29	/* Wide edited */
/*  The following 2 data types should never be used by programs  */
/*  passing data to the runtime.  Instead, you should use NativeSigned  */
/*  and NativeUnsigned with the correct size.  */
/*  These values show up primarily in XFD files.  */
#define	NativeVSigned	30	/* Signed variable-size native-order binary */
#define	NativeVUnsigned	31	/* Unsigned variable-size native-order binary */

#define	ExternalFloat	32	/* External Floating-Point */
#define	BigFloat	33	/* bigfloat (extended floating-point) */

#define	InternalTemp	34	/* Compiler's internal temporary format */

#define	LargestTypeVal	33

#define	Numeric(x)	(((x) >= NumUnsigned && (x) <= NativeUnsigned ) || \
				(x) == NativeVSigned || \
				(x) == NativeVUnsigned || \
				(x) == Float || \
				(x) == BigFloat || \
				(x) == ExternalFloat)


/* The following can be used as return codes from the 'C' routines */

#define	Okay		0	/* routine found - no errors */
#define	Halt		1	/* routine requests STOP RUN */
#define	NotFound	-1	/* routine not found, continue searching */


/* The following definitions are provided for RM/COBOL-85 compatibility */

#define	RM_NSE		NumEdited
#define	RM_NSU		NumUnsigned
#define	RM_NTS		NumSignSep
#define	RM_NTC		NumSigned
#define	RM_NLS		NumSepLead
#define	RM_NLC		NumLeading
#define	RM_NCS		CompSigned
#define	RM_NCU		CompUnsigned
#define	RM_NPP		PackedPositive
#define	RM_NPS		PackedSigned
#define	RM_NPU		PackedUnsigned
#define	RM_NBS		BinarySigned
#define	RM_NBU		BinaryUnsigned
#define	RM_ANS		Alphanum
#define	RM_ANSR		JustAN
#define	RM_ABS		Alphabetic
#define	RM_ABSR		JustAlpha
#define	RM_ANSE		AlphaEdited
#define	RM_ABSE		21		/* Unused */
#define	RM_GRPF		Group

#define	RM_FND		Okay
#define	RM_STOP		Halt
#define	RM_NFND		NotFound


/* The following two routines are used to set the terminal state to its	*/
/* default operating state (resetunit) and to the state it needs to be	*/
/* in to run ACUCOBOL (setunit).  */

extern	void		w_set_term(void), w_reset_term(void);

#define	setunit()	w_set_term()
#define	resetunit()	w_reset_term()


/* Declare the "cobol()" routine if not already declared */

#ifndef	A_COBOL_DECLARED
#define	A_COBOL_DECLARED

typedef	enum	tag_ACUCOBOL_DEBUG_METHODS {
	ADM_NONE,
	ADM_XTERM,
	ADM_TERMINAL,
	ADM_THINCLIENT
} ACUCOBOL_DEBUG_METHOD, ADM_t;

struct	a_cobol_info {
	size_t		a_cobol_info_size;
	char		*pgm_name;
	int		num_params;
	Argument	*params;
	int		exit_code;
	const char	*exit_msg;
	int		signal_number;
	int		call_error;
	long		cobol_return_code;
	unsigned	no_stop:1;
	unsigned	cache:1;
	ADM_t		debug_method;
	char		*debug_method_string;
};

typedef	struct a_cobol_info	ACUCOBOLINFO;

#ifdef	_WIN32
#define	ASTDCALL	__stdcall
#define	ACDECL		__cdecl
#else	/* _WIN32 */
#define	ASTDCALL
#define	ACDECL
#endif	/* _WIN32 */
extern	int	ASTDCALL cobol(char *name, int num_params, Argument *params);
extern	int	ASTDCALL cobol_no_stop(char *name, int num_params, Argument *params);
extern	void	ASTDCALL acu_abend(int signal_number);
extern	void	ASTDCALL acu_cancel(char *name);
extern	void	ASTDCALL acu_cancel_all(void);
extern	int	ASTDCALL acu_cobol(ACUCOBOLINFO *data);
extern	int	ASTDCALL acu_initv(int argc, char **argv);
extern	int	ASTDCALL acu_reg_sig_handlers(void);
extern	void	ASTDCALL acu_runmain(int argc, char **argv, int pgm_arg);
extern	void	ASTDCALL acu_shutdown(int place_cursor);
extern	void	ASTDCALL acu_unload(char *name, int called_programs);
extern	void	ASTDCALL acu_unload_all(void);
extern	int	ASTDCALL acu_unreg_sig_handlers(void);
extern	void	ASTDCALL acu_convert(Argument *, Argument *, int);
typedef	int	(ACDECL ACU_SUB_FUNC)(int argc, char **argv);
extern	ACU_SUB_FUNC	ACDECL *acu_register_sub(ACU_SUB_FUNC *func);
#endif	/* A_COBOL_DECLARED */


/* cobol_exit_code() declaration and values */

#define	COBOL_EXIT_PROGRAM	1
#define	COBOL_REMOTE_CALL	2
#define	COBOL_STOP_RUN		3
#define	COBOL_CALL_ERROR	4
#define	COBOL_SIGNAL		5
#define	COBOL_FATAL_ERROR	6
#define	COBOL_NONFATAL_ERROR	7
#define	COBOL_DEBUGGER		8

/* COBOL_CALL_ERROR a_cobol_info.call_error values */

#ifndef	CS_SUCCESSFUL /* If already defined then assume the rest are also */
#define	CS_SUCCESSFUL		0
#define	CS_MISSING		1
#define	CS_NOT_COBOL		2
#define	CS_INTERNAL		3
#define	CS_MEMORY		4
#define	CS_VERSION		5
#define	CS_RECURSIVE		6
#define	CS_EXTERNAL		7
#define	CS_LARGE_MODEL		8
#define	CS_JAPANESE		14
#define	CS_MULTITHREADED	22
#define	CS_AUTHORIZATION	23
#define	CS_CONNECT_REFUSED	25
#define	CS_MISMATCHED_CPU	27
#define	CS_SERIAL_NUMBER	28
#define	CS_USER_COUNT_EXCEEDED	29
#define	CS_LICENSE		30
#define	CS_CONT			-1
#define	CS_STOP_RUN		-2
#ifdef	_WIN32
#define	CS_WIN_INIT_FAILED	-3
#define	CS_MULTIPLE_OS_THREADS	-4
#endif	/* _WIN32 */
#endif	/* ifndef CS_SUCCESSFUL */

#ifdef	_WIN32
extern	int	__stdcall cobol_exit_code(char *msg, int msg_size);
#else	/* _WIN32 */
extern	int	cobol_exit_code(char *msg, int msg_size);
#endif	/* _WIN32 */


/*  The following are no longer variables, but rather functions.  With  */
/*  the following definitions, they can be used just like variables.  */
/*  And they are multi-thread safe, if you are using a multi-thread save  */
/*  version of the appropriate libraries.  The functions are in acme.dll  */
/*  (on Windows) and in libstdlib.a (on UNIX.)  */

#ifndef	f_errmsg
extern	char		**Astdlib_f_errmsg(void);
#define	f_errmsg	(*Astdlib_f_errmsg())
#endif	/* f_errmsg */

#ifndef	f_errno
extern	short		*Astdlib_f_errno(void);
#define	f_errno		(*Astdlib_f_errno())
#endif	/* f_errno */

#ifndef	f_int_errno
extern	long		*Astdlib_f_int_errno(void);
#define	f_int_errno	(*Astdlib_f_int_errno())
#endif	/* f_int_errno */

#ifndef	f_int2_errno
extern	long		*Astdlib_f_int2_errno(void);
#define	f_int2_errno	(*Astdlib_f_int2_errno())
#endif	/* f_int2_errno */

#ifndef	f_log_errno
extern	short		*Astdlib_f_log_errno(void);
#define	f_log_errno	(*Astdlib_f_log_errno())
#endif	/* f_log_errno */

#ifndef	f_no_lock
extern	short		*Astdlib_f_no_lock(void);
#define	f_no_lock	(*Astdlib_f_no_lock())
#endif	/* f_no_lock */

#ifndef	f_upgrade_rlock
extern	short		*Astdlib_f_upgrade_rlock(void);
#define	f_upgrade_rlock	(*Astdlib_f_upgrade_rlock())
#endif	/* f_upgrade_rlock */

#ifndef	f_extra_info
extern	void		**Astdlib_f_extra_info(void);
#define	f_extra_info	(*Astdlib_f_extra_info())
#endif	/* f_extra_info */

#ifndef	return_code
#ifdef	_WIN64
extern	INT_PTR		*Astdlib_return_code(void);
#else	/* _WIN64 */
extern	long		*Astdlib_return_code(void);
#endif	/* _WIN64 */
#define	return_code	(*Astdlib_return_code())
#endif	/* return_code */

/*  Generic proc for registering functions  */
#ifndef	ACU_GENERIC_PROC_DEFINED
typedef	int		(*GenericProc)();
#define	ACU_GENERIC_PROC_DEFINED
#endif	/* ACU_GENERIC_PROC_DEFINED */

/* Some macros and types for accessing AcuSQL runtime libraries */
struct tagSQLCA;
typedef int	(*ESQL_CBL_DispatchProc)(char *, int,  Argument *, int);
typedef void	(*ESQL_InitializeProc)(struct tagSQLCA *);
typedef void	(*ESQL_ShutdownProc)(void);
typedef	void	(*ESQL_GetVersionProc)(char *);
typedef GenericProc (*ESQL_RegisterOneProcedureProc)(char *, GenericProc);
extern	int	ESQL_CBL_Dispatch(char *, int,  Argument *, int);
extern	void	ESQL_Initialize(struct tagSQLCA *);
extern	void	ESQL_Shutdown(void);
extern	void	ESQL_GetVersion(char *);
extern	GenericProc ESQL_RegisterOneProcedure(char *, GenericProc);

/* Some macros and types for accessing IBM MQSeries functions */
#ifdef	WIN32
#define MQENTRY	__cdecl
#else	/* WIN32 */
#define MQENTRY
#endif	/* WIN32 */
#ifndef	MQC_INCLUDED
typedef char MQCHAR;
typedef MQCHAR *PMQCHAR;
typedef long MQLONG;
typedef MQLONG *PMQLONG;
typedef MQLONG MQHCONN;
typedef MQHCONN *PMQHCONN;
typedef MQLONG MQHOBJ;
typedef MQHOBJ *PMQHOBJ;
typedef void *MQPTR;
typedef MQPTR *PMQPTR;
typedef void *PMQVOID;
typedef PMQVOID *PPMQVOID;
#ifdef	WIN32
typedef	MQCHAR MQCHAR4[4];
typedef struct tagMQCNO {
  MQCHAR4  StrucId;  /* Structure identifier */
  MQLONG   Version;  /* Structure version number */
  MQLONG   Options;  /* Options that control the action of MQCONNX */
} MQCNO;
typedef MQCNO *PMQCNO;
#endif	/* WIN32 */
#define	MQC_INCLUDED
#endif	/* MQC_INCLUDED */
typedef	void (MQENTRY *MQBACKProc)(MQHCONN, PMQLONG, PMQLONG);
typedef	void (MQENTRY *MQBEGINProc)(MQHCONN, PMQVOID, PMQLONG, PMQLONG);
typedef	void (MQENTRY *MQCLOSEProc)(MQHCONN, PMQHOBJ, MQLONG, PMQLONG, PMQLONG);
typedef	void (MQENTRY *MQCMITProc)(MQHCONN, PMQLONG, PMQLONG);
#ifdef	WIN32
typedef	void (MQENTRY *MQCONNXProc)(PMQCHAR, PMQCNO, PMQHCONN, PMQLONG, PMQLONG);
#endif	/* WIN32 */
typedef	void (MQENTRY *MQCONNProc)(PMQCHAR, PMQHCONN, PMQLONG, PMQLONG);
typedef	void (MQENTRY *MQDISCProc)(PMQHCONN, PMQLONG, PMQLONG);
typedef	void (MQENTRY *MQGETProc)(MQHCONN, MQHOBJ, PMQVOID, PMQVOID, MQLONG, PMQVOID, PMQLONG, PMQLONG, PMQLONG);
typedef	void (MQENTRY *MQINQProc)(MQHCONN, MQHOBJ, MQLONG, PMQLONG, MQLONG, PMQLONG, MQLONG, PMQCHAR, PMQLONG, PMQLONG);
typedef	void (MQENTRY *MQOPENProc)(MQHCONN, PMQVOID, MQLONG, PMQHOBJ, PMQLONG, PMQLONG);
typedef	void (MQENTRY *MQPUT1Proc)(MQHCONN, PMQVOID, PMQVOID, PMQVOID, MQLONG, PMQVOID, PMQLONG, PMQLONG);
typedef	void (MQENTRY *MQPUTProc)(MQHCONN, MQHOBJ, PMQVOID, PMQVOID, MQLONG, PMQVOID, PMQLONG, PMQLONG);
typedef	void (MQENTRY *MQSETProc)(MQHCONN, MQHOBJ, MQLONG, PMQLONG, MQLONG, PMQLONG, MQLONG, PMQCHAR, PMQLONG, PMQLONG);
extern	void MQENTRY MQBACK(MQHCONN, PMQLONG, PMQLONG);
extern	void MQENTRY MQBEGIN(MQHCONN, PMQVOID, PMQLONG, PMQLONG);
extern	void MQENTRY MQCLOSE(MQHCONN, PMQHOBJ, MQLONG, PMQLONG, PMQLONG);
extern	void MQENTRY MQCMIT(MQHCONN, PMQLONG, PMQLONG);
#ifdef	WIN32
extern	void MQENTRY MQCONNX(PMQCHAR, PMQCNO, PMQHCONN, PMQLONG, PMQLONG);
#endif	/* WIN32 */
extern	void MQENTRY MQCONN(PMQCHAR, PMQHCONN, PMQLONG, PMQLONG);
extern	void MQENTRY MQDISC(PMQHCONN, PMQLONG, PMQLONG);
extern	void MQENTRY MQGET(MQHCONN, MQHOBJ, PMQVOID, PMQVOID, MQLONG, PMQVOID, PMQLONG, PMQLONG, PMQLONG);
extern	void MQENTRY MQINQ(MQHCONN, MQHOBJ, MQLONG, PMQLONG, MQLONG, PMQLONG, MQLONG, PMQCHAR, PMQLONG, PMQLONG);
extern	void MQENTRY MQOPEN(MQHCONN, PMQVOID, MQLONG, PMQHOBJ, PMQLONG, PMQLONG);
extern	void MQENTRY MQPUT1(MQHCONN, PMQVOID, PMQVOID, PMQVOID, MQLONG, PMQVOID, PMQLONG, PMQLONG);
extern	void MQENTRY MQPUT(MQHCONN, MQHOBJ, PMQVOID, PMQVOID, MQLONG, PMQVOID, PMQLONG, PMQLONG);
extern	void MQENTRY MQSET(MQHCONN, MQHOBJ, MQLONG, PMQLONG, MQLONG, PMQLONG, MQLONG, PMQCHAR, PMQLONG, PMQLONG);
extern	MQBEGINProc	MQBEGINFunc;
extern	MQBACKProc	MQBACKFunc;
extern	MQCLOSEProc	MQCLOSEFunc;
extern	MQCMITProc	MQCMITFunc;
#ifdef	WIN32
extern	MQCONNXProc	MQCONNXFunc;
#endif	/* WIN32 */
extern	MQCONNProc	MQCONNFunc;
extern	MQDISCProc	MQDISCFunc;
extern	MQGETProc	MQGETFunc;
extern	MQINQProc	MQINQFunc;
extern	MQOPENProc	MQOPENFunc;
extern	MQPUT1Proc	MQPUT1Func;
extern	MQPUTProc	MQPUTFunc;
extern	MQSETProc	MQSETFunc;

/* Some macros and types for accessing IBM CICS functions */
struct CICS_EciSystem_t;
typedef	int	(*CICS_DispatchProc)(struct CICS_EciSystem_t *);
typedef	int	(*CICS_ListDispatchProc)(void *, unsigned short *,
					 struct CICS_EciSystem_t *);
extern	int	CICS_Dispatch(struct CICS_EciSystem_t *);
extern	int	CICS_ListDispatch(void *, unsigned short *,
				  struct CICS_EciSystem_t *);
extern	CICS_DispatchProc	CICS_DispatchFunc;
extern	CICS_ListDispatchProc	CICS_ListDispatchFunc;

#ifdef	__cplusplus
}
#endif	/* __cplusplus */

#endif	/* ACU_LIB_SUB_H */

/* */
