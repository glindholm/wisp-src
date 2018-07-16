/* sub.h - header file to be included in 'C' subroutines  */

/* Copyright (c) 1995-2003 by Acucorp, Inc.  All rights reserved.	*/
/* Users of the ACUCOBOL-GT runtime may freely modify and distribute	*/
/* this file as they see fit in order to support an ACUCOBOL-GT based	*/
/* application.  */

/* This file contains definitions used by the sub.c file.		*/
/* It provides the basic interface structures and definitions.		*/
/* This file is equivalent to the file "rmc85call.h" provided with 	*/
/* Unix versions of RM/COBOL-85 and may be used in place of that file.	*/

#ifndef	ACU_LIB_SUB_H
#define	ACU_LIB_SUB_H

#ifndef	P_
#if	defined(__STDC__) || defined(_MSC_VER)
#define	P_(x)	x
#else	/* defined(__STDC__) || defined(_MSC_VER) */
#define	P_(x)	()
#endif	/* defined(__STDC__) || defined(_MSC_VER) */
#endif	/* P_ */

#ifdef	__cplusplus
extern	"C"	{
#endif	/* __cplusplus */

#ifdef	__WATCOMC__
 #pragma pack(4)
#endif	/* __WATCOMC__ */

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

#ifdef	__WATCOMC__
 #pragma pack()
#endif	/* __WATCOMC__ */

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
#define Float		23	/* Float or Double */
#define National	24	/* National */
#define JustNat		25	/* National (justified) */
#define NatEdited	26	/* National edited */
#define Wide		27	/* Wide */
#define JustWide	28	/* Wide (justified) */
#define WideEdited	29	/* Wide edited */
/*  The following 2 data types should never be used by programs  */
/*  passing data to the runtime.  Instead, you should use NativeSigned  */
/*  and NativeUnsigned with the correct size.  */
/*  These values show up primarily in XFD files.  */
#define	NativeVSigned	30	/* Signed variable-size native-order binary */
#define	NativeVUnsigned	31	/* Unsigned variable-size native-order binary */

#define ExternalFloat	32 /* External Floating-Point */
#define BigFloat	33 /* bigfloat (extended floating-point) */

#define	InternalTemp	34	/* Compiler's internal temporary format */

#define	LargestTypeVal	33

#define	Numeric(x)	( ( (x) > NumEdited && (x) < Alphanum ) || \
			    (x) == NativeVSigned || (x) == NativeVUnsigned || \
			    (x) == Float || (x) == BigFloat || \
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

extern	void		w_set_term P_((void)), w_reset_term P_((void));

#define	setunit()	w_set_term()
#define	resetunit()	w_reset_term()


/* Declare the "cobol()" routine if not already declared */

#ifndef	A_COBOL_DECLARED
#define	A_COBOL_DECLARED
#ifdef	_WINDOWS
extern	int	__stdcall cobol(char *name, int num_params, Argument *params);
extern	int	__stdcall cobol_no_stop(char *name, int num_params, Argument *params);
#else	/* _WINDOWS */
extern	int	cobol();
extern	int	cobol_no_stop();
#endif	/* _WINDOWS */
#endif	/* A_COBOL_DECLARED */


/* cobol_exit_code() declaration and values */

#define	COBOL_EXIT_PROGRAM	1
#define	COBOL_REMOTE_CALL	2
#define	COBOL_STOP_RUN		3
#define	COBOL_CALL_ERROR	4
#define	COBOL_SIGNAL		5
#define	COBOL_FATAL_ERROR	6

#ifdef	_WINDOWS
#ifdef	WIN32
extern	int	__stdcall cobol_exit_code(char *msg, int msg_size);
#else	/* WIN32 */
extern	int	cobol_exit_code(char *msg, int msg_size);
#endif	/* WIN32 */
#else	/* _WINDOWS */
extern	int	cobol_exit_code();
#endif	/* _WINDOWS */


/*  The following are no longer variables, but rather functions.  With  */
/*  the following definitions, they can be used just like variables.  */
/*  And they are multi-thread safe, if you are using a multi-thread save  */
/*  version of the appropriate libraries.  The functions are in acme.dll  */
/*  (on Windows) and in libstdlib.a (on UNIX.)  */

#ifndef	f_errmsg
extern	char		**Astdlib_f_errmsg P_((void));
#define	f_errmsg	(*Astdlib_f_errmsg())
#endif	/* f_errmsg */

#ifndef	f_errno
extern	short		*Astdlib_f_errno P_((void));
#define	f_errno		(*Astdlib_f_errno())
#endif	/* f_errno */

#ifndef	f_int_errno
extern	long		*Astdlib_f_int_errno P_((void));
#define	f_int_errno	(*Astdlib_f_int_errno())
#endif	/* f_int_errno */

#ifndef	f_int2_errno
extern	long		*Astdlib_f_int2_errno P_((void));
#define	f_int2_errno	(*Astdlib_f_int2_errno())
#endif	/* f_int2_errno */

#ifndef	f_log_errno
extern	short		*Astdlib_f_log_errno P_((void));
#define	f_log_errno	(*Astdlib_f_log_errno())
#endif	/* f_log_errno */

#ifndef	f_no_lock
extern	short		*Astdlib_f_no_lock P_((void));
#define	f_no_lock	(*Astdlib_f_no_lock())
#endif	/* f_no_lock */

#ifndef	f_upgrade_rlock
extern	short		*Astdlib_f_upgrade_rlock P_((void));
#define	f_upgrade_rlock	(*Astdlib_f_upgrade_rlock())
#endif	/* f_upgrade_rlock */

#ifndef	return_code
extern	long		*Astdlib_return_code P_((void));
#define	return_code	(*Astdlib_return_code())
#endif	/* return_code */

#ifdef	__cplusplus
}
#endif	/* __cplusplus */

#endif	/* ACU_LIB_SUB_H */

/* */
