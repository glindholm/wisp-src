/* sub.h - header file to be included in 'C' subroutines  */

/* Copyright (C) 1993-1995,1997-2011 Micro Focus.  All rights reserved. */

/* Users of the ACUCOBOL-GT runtime may freely modify and distribute	*/
/* this file as they see fit in order to support an ACUCOBOL-GT based	*/
/* application.  */

/* This file contains definitions used by the sub.c file.		*/
/* It provides the basic interface structures and definitions.		*/
/* This file is equivalent to the file "rmc85cal.h" provided with      */
/* Unix versions of RM/COBOL-85 and may be used in place of that file.	*/

#ifndef	ACU_LIB_SUB_H
#define	ACU_LIB_SUB_H

#ifdef	__cplusplus
extern	"C"	{
#endif	/* __cplusplus */

/*
** try to avoid warnings about unused parameters
*/
#ifdef	__GNUC__
#define	A_UNUSED	__attribute__((unused))
#else   /* __GNUC__ */
#define	A_UNUSED
#endif	/* __GNUC__ */


#define	CURRENT_MFACU_TABLES_VERSION	1
typedef	struct	mfacu_tables {
	int	version;
	void	*sub85_table;
	void	*direct_table;
	void	*extern_table;
	void	*extern2_table;
} MFACU_TABLES_t;

/* PROCTABLE is a structure that relates a CALL name to its 'C' routine	*/
typedef	struct Argument	Argument;
typedef	int	(*pfSub85Intf)(char *name, int num_args, Argument *args, int initial, ...);
struct	PROCTABLE {
	char		*name;                  /* subroutine name */
	pfSub85Intf	routine;                /* pointer to function to call */
	void  		*hSharedLib;            /* H_SHARELIB */
};

typedef	int	(*pfDirectIntf)();
struct	DIRECTTABLE {
	char		*name;			/* subroutine name */
	pfDirectIntf	routine;		/* pointer to routine */
	short		return_type;		/* routine's return type */
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

/* EXTRNTABLE2 is a structure that relates external 'C' variables to 	*/
/* their COBOL external names, but which assumes that the variable is   */
/* really a function that returns an address.  In that case we also     */
/* the size of the variable  */

typedef	void	*(*pfEXTRNDATA)(void);
struct	EXTRNTABLE2 {
	char		*name;
	pfEXTRNDATA	addr_func;
	size_t		len;
};


/* COMMONTABLE is a structure that relates FORTRAN COMMON to 	*/
/* their COBOL external names  */

struct	COMMONTABLE {
	char	*name;			/* COBOL data name */
	char	*addr;			/* address of data item */
};

/* Argument is a structure that is passed to the 'C' routine for	*/
/* each USING argument.  It defines the layout of that argument. 	*/

struct	Argument {
	char	*a_address;		/* pointer to actual data */
	long	a_length;		/* size of data item (in bytes) */
	short	a_type;			/* COBOL data type, see below */
	char	a_digits;		/* # of digits for numeric items */
	char	a_scale;		/* implied decimal point offset */
	union	{
		char	*a_picture;	/* unused */
		short	a_pass_type;	/* 0 = BY REFERENCE */
		                        /* 1 = BY CONTENT */
		                        /* 2 = BY VALUE */
	}	u;
};

#define	a_size(arg)	(unsigned)(arg).a_length
#define	Scale(s)	(((s) & 0x80) ? ((-1 << 8) | (s)) : (s))

/*
 * ENTRYTABLE structure definition (added to support RM Dlls)
 */
typedef struct RMEntryTable
{
    char	*EntryPointCobolName;	/* name of subroutine as in call  */
    int 	(*EntryPointAddress)(); /* entry point address */
    char	*EntryPointName;	/* name of entry point in the object */

}   RMENTRYTABLE;

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

#define	UNumUnsigned	35	/* Usage national Unsigned numeric */
#define	UNumSigned	36	/* Usage nat Signed num */
#define	UNumLeading	37	/* Usage nat Signed num (leading combined) */
#define	UNumSignSep	38	/* Usage nat Signed num (trailing separate) */
#define	UNumSepLead	39	/* Usage nat Signed num (leading separate) */
#define	UNational	40	/* National usage national */
#define	UJustNat	41	/* National usage national (justified) */
#define	UNumEdited	42	/* Usage national Numeric edited */
#define	UNatEdited	43	/* National usage national edited */
#define	UNatNuMedZwB	44	/* Usage nat nat numeric edit, blank w zero */

#define	LargestTypeVal	45

#define	Numeric(x)	(((x) >= NumUnsigned && (x) <= NativeUnsigned ) || \
				(x) == NativeVSigned || \
				(x) == NativeVUnsigned || \
				(x) == Float || \
				(x) == BigFloat || \
				(x) == ExternalFloat || \
				((x) >= UNumUnsigned && (x) <= UNumSepLead))
#define	IsNational(x)	((x) >= National && (x) <= WideEdited)
#define	IsUnicode(x)	((x) >= UNumUnsigned && (x) <= UNatNuMedZwB)


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
#define RM_GRPV 	23		/* RM: Group variable length */
#define RM_POINTER      25		/* RM: Pointer (macro conflicts with an Extend macro) */
#define RM_NBSN 	26		/* RM: Binary signed native (COMP-5) */
#define RM_NBUN 	27		/* RM: Binary unsigned native (COMP-5) */
#define RM_OMITTED	32		/* RM: omitted parameter */

#define	RM_FND		Okay
#define	RM_STOP		Halt
#define	RM_NFND		NotFound

/* The following two routines are used to set the terminal state to its	*/
/* default operating state (resetunit) and to the state it needs to be	*/
/* in to run ACUCOBOL (setunit).  */

extern	void		w_set_term(void), w_reset_term(void);

#define	setunit()	w_set_term()
#define	resetunit()	w_reset_term()

extern long		Acall_direct(long (*func)(), int num_params,
				void *params[]);


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
	void		*internal; /* internal use only - this should be NULL */
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
extern  void    ASTDCALL acu_async_shutdown(void);
extern	int	ASTDCALL acu_set_int_variable(const char *varname, int value);

#define AINITV_SUCCESS			0
#define AINITV_LICENSE_EXPIRED		-1
#define AINITV_LICENSE_NOT_FOUND	-2
#define AINITV_LICENSE_EXCEEDED		-3
#define AINITV_INIT_LIBRARY_FAILED	-4
#define AINITV_LOAD_LIBRARY_FAILED	-5
#define AINITV_CFG_FAILED		-6
#define AINITV_DEBUG_FAILED		-7
#define AINITV_FILE_FAILED		-8
#define AINITV_MEM_FAILED		-9
#define AINITV_RMT_FILE_FAILED		-10
#define AINITV_STARTUP_FAILED		-11
#define AINITV_PROFILE_FAILED		-12
#define AINITV_MISMATCHED_VERSIONS	-13
#define AINITV_EXCEPTION		-14
#define AINITV_LICENSE_BIS_NOT_WAN	-15
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

/* This must match the order above */
#define COBOL_EXIT_CODE_TEXT_MULTIZ  "?\0Exit Program\0Remote Call\0Stop Run\0Call Error\0Signal\0FatalError\0Nonfatal Error\0Debugger\0"

/* COBOL_CALL_ERROR a_cobol_info.call_error values */

#ifndef	CS_SUCCESSFUL /* If already defined then assume the rest are also */
#define	CS_SUCCESSFUL		0	/* The call completed with no errors */
#define	CS_MISSING		1	/* Program missing or inaccessible */
#define	CS_NOT_COBOL		2	/* Not a COBOL program */
#define	CS_INTERNAL		3	/* Corrupted program */
#define	CS_MEMORY		4	/* Inadequate memory available */
#define	CS_VERSION		5	/* Unsupported version of object code */
#define	CS_RECURSIVE		6	/* Program already in use */
#define	CS_EXTERNAL		7	/* Too many external segments */
#define	CS_LARGE_MODEL		8	/* Large-model program not supported */
#define	CS_JAPANESE		14	/* Japanese extensions not supported */
#define	CS_MULTITHREADED	22	/* Multithreaded CALL RUN illegal */
#define	CS_AUTHORIZATION	23	/* Access denied */
#define	CS_CONNECT_REFUSED	25	/* Connection refused - user count 
					 * exceeded on remote server */
#define	CS_MISMATCHED_CPU	27	/* Program contains object code for a 
					 * different processor */
#define	CS_SERIAL_NUMBER	28	/* Incorrect serial number */
#define	CS_USER_COUNT_EXCEEDED	29	/* Connection refused - user count 
					 * exceeded on remote server */
#define	CS_LICENSE		30	/* License error */
#define	CS_XCD_MISSING		32	/* XCD not found */
#define	CS_XCD_MISMATCH		33	/* Input does not match XCD */
#define	CS_CONT			-1	/* Continue normal processing */
#define	CS_STOP_RUN		-2	/* STOP RUN requested */
#ifdef	_WIN32
#define	CS_WIN_INIT_FAILED	-3	/* acu_initv() call failed */
#define	CS_MULTIPLE_OS_THREADS	-4	/* Multiple OS threads not allowed */
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
