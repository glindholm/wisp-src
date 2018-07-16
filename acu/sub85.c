/*
	sub85.c		

	This is the WISP compatable version of sub85.c for use with 
	Acucobol 5.2 or later.

	If you are using Acucobol 5.1 or earlier you will need to 
	use the file sub85_acu51.c instead.  Rename this file 
	to sub85_acu52.c then rename sub85_acu51.c to sub85.c.

	All of the WISP added code is enclosed in
	"#ifdef WISP" statements.
*/
#define WISP

/* sub85.c - RM/COBOL-85 compatible 'C' routine interface */

/* Copyright (c) 1995-2001 by Acucorp, Inc.  All rights reserved.	*/
/* Users of the ACUCOBOL-GT runtime may freely modify and distribute	*/
/* this file as they see fit in order to support an ACUCOBOL-GT based	*/
/* application.  */


/* THIS FILE IS #INCLUDED FROM sub.c.  BECAUSE SYSTEM HEADER FILES	*/
/* SHOULD BE INCLUDED BEFORE sub.h, AND BECAUSE THIS FILE IS INCLUDED	*/
/* AFTER sub.h, YOU REALLY SHOULDN'T INCLUDE ANY SYSTEM HEADER FILES	*/
/* FROM THIS FILE.  */

/* The following LIBTABLE should be modified to contain the names and	*/
/* function addresses of 'C' routines you wish to link into the runtime	*/
/* system.  This table is searched for each CALL statement to see if a	*/
/* matching routine name is found.  If so, then the corresponding 	*/
/* 'C' function is called.  Note that the table must be terminated by	*/
/* NULL pointers and that the routine names should be all upper case.	*/

/* Each 'C' routine receives 4 parameters.  The first is a pointer to 	*/
/* the name it was called by.  The second is the number of USING 	*/
/* arguments the CALL statement contained.  The third is a pointer to	*/
/* an array of ARGUMENT_ENTRY structures (see "sub.h").  Each array	*/
/* element describes one of the USING arguments.  The final parameter	*/
/* is 1 if this routine is being called for the first time or has been	*/
/* CANCELLED since its last CALL.  Otherwise this parameter is zero.	*/

#ifdef WISP
/************************************************************************/
static char wisp_copyright[]="Copyright (c) 1989-2002 NeoMedia Technologies, All rights reserved.";
static char wisp_rcsid[]="$Id:$";
/*
	To reduce the size of the RTS you can remove any of the following
	"#define" statements and the corresponding files will not be
	included.

		ACP	 The Wang ACP routines.
		NETCAP	 The Netron Cap routines.
		EDE	 The EDE routines.
		CRID	 Control Report Inquiry Datentry (by default they are not included) 
*/

#include <string.h>

typedef short 		 int2; 
typedef int  		 int4;
typedef unsigned short  uint2;  
typedef unsigned int   	uint4;

#ifdef unix
#define ACP
#endif

#define NETCAP
#define EDE

char	WISPFILEXT[39];			/* Define the file extension variable.	*/
char	WISPRETURNCODE[3];		/* Define the return code field.	*/

extern int va_set();
extern void reversebytes();
extern int bytenormal();
extern void werrlog();
extern void wexit();
extern void set_isdebug_true();
extern void set_isdebug_false();
extern char *upper_string();
extern void setdbfile();
extern int nativescreens();

int 	wfrontend();
int	wfrontend2();

/*
**  Include the CRID header files
*/
#ifdef CRID
#include "crid.h"
#endif /* CRID */

/************************************************************************/
#endif /* WISP */


int	call_system P_((char *, int, Argument[], int));
int	call_cbl_error_proc P_((char *, int, Argument[], int));
int	call_cbl_exit_proc P_((char *, int, Argument[], int));
int	call_cbl_get_exit_info P_((char *, int, Argument[], int));
void	wstoasc P_((Argument *, char *));


struct	PROCTABLE LIBTABLE[] = {
	{ "SYSTEM", 	call_system },
	{ "CBL_ERROR_PROC", 	call_cbl_error_proc },
	{ "CBL_EXIT_PROC", 	call_cbl_exit_proc },
	{ "CBL_GET_EXIT_INFO", 	call_cbl_get_exit_info },

#ifdef WISP
/************************************************************************/
/*
	The first entry must be UPPERCASE.
*/
	{ "ACUGARGS", 	wfrontend },
	{ "ACUNARGS", 	wfrontend },
	{ "ACUPARGS", 	wfrontend },
	{ "BELL", 	wfrontend },
	{ "BITPACK",	wfrontend },
	{ "BITUNPK",	wfrontend },
	{ "BIT_OFF",	wfrontend },
	{ "BIT_ON",	wfrontend },
	{ "BIT_TEST",	wfrontend },
	{ "CANCEL",	wfrontend },
	{ "CEXIT",	wfrontend },
	{ "COBLINK",	wfrontend },
	{ "DATE",	wfrontend },
	{ "DATE2",	wfrontend },
	{ "DATE4",	wfrontend },
	{ "DAY",	wfrontend },
	{ "EXTRACT",	wfrontend },
	{ "FILECOPY",	wfrontend },
	{ "FIND",	wfrontend },
	{ "FXZONE",	wfrontend },
	{ "GETPARM",	wfrontend },
	{ "GETPARMBUILD",wfrontend },
	{ "GETWFILEXT", wfrontend },
	{ "GRECLEN",	wfrontend },
	{ "HEXPACK",	wfrontend },
	{ "HEXUNPK",	wfrontend },
	{ "INITWISP",	wfrontend },
	{ "INITWISP2",	wfrontend },
	{ "ISDBFILE",	wfrontend2 },
	{ "LBIT_OFF",	wfrontend },
	{ "LBIT_ON",	wfrontend },
	{ "LINK",	wfrontend2 },
	{ "LINKPROC",	wfrontend },
	{ "LOGOFF",	wfrontend },
	{ "MESSAGE",	wfrontend },
	{ "MWCONV",	wfrontend },
	{ "NOHELP",	wfrontend },
	{ "ONHELP",	wfrontend },
	{ "PRINT",	wfrontend },
	{ "PUTPARM",	wfrontend },
	{ "READFDR",	wfrontend },
	{ "READFDR4",	wfrontend },
	{ "READVTOC",	wfrontend },
	{ "RETCODE",	wfrontend },
	{ "SCRATCH",	wfrontend },
	{ "SCREEN",	wfrontend },
	{ "SEARCH",	wfrontend },
	{ "SET",	wfrontend },
	{ "SET8BIT",	wfrontend },
	{ "SETFILE",	wfrontend },
	{ "SETPROGID",  wfrontend },
	{ "SETRETCODE", wfrontend },
	{ "SETRUNNAME", wfrontend },
	{ "SETSUBMIT",  wfrontend },
	{ "SETTRIGPROG",  wfrontend },
	{ "SETWFILEXT", wfrontend },
	{ "SETWISPFILEXT", wfrontend },
	{ "SORT",	wfrontend },
	{ "SORTCALL",	wfrontend },
	{ "SORTINFO",	wfrontend },
	{ "SORTLINK",	wfrontend },
	{ "STRING",	wfrontend },		
	{ "SUBMIT",	wfrontend },
	{ "UPDATFDR",	wfrontend },
	{ "UPPER",	wfrontend },
	{ "USEHARDLINK",wfrontend },
	{ "USESOFTLINK",wfrontend },
	{ "VEXIT",      wfrontend },
	{ "VWANG",	wfrontend },
	{ "W2ROWCOL",	wfrontend },
	{ "W4WAPI",	wfrontend },
	{ "WACCEPT",	wfrontend },
	{ "WANSI2WANG",	wfrontend },
	{ "WCHAIN",	wfrontend },
	{ "WDISPLAY",	wfrontend },
	{ "WEXITH",     wfrontend },
	{ "WFCLOSE",	wfrontend },
	{ "WFILECHK",   wfrontend },
	{ "WFILECHK2",  wfrontend },
	{ "WFNAME",     wfrontend },
	{ "WFOPEN",	wfrontend },
	{ "WFOPEN2",	wfrontend },
	{ "WFOPEN3",	wfrontend },
	{ "WFSWAIT",	wfrontend },
	{ "WFWAIT",	wfrontend },
	{ "WISPEXIT",	wfrontend },
	{ "WISPHELP",	wfrontend },
	{ "WISPPLAT",	wfrontend },
	{ "WISPSHUT",	wfrontend },
	{ "WISPSORT",	wfrontend },
	{ "WISPSYNC",	wfrontend },
	{ "WMEMCPY",	wfrontend },
	{ "WPAUSE",	wfrontend },
	{ "WRENAME",	wfrontend },
	{ "WS132",	wfrontend },
	{ "WS80",	wfrontend },
	{ "WSCREEN",	wfrontend },
	{ "WSETSTAT",	wfrontend },
	{ "WSTOP",	wfrontend },
	{ "WSXIO",	wfrontend },
	{ "WTITLE",	wfrontend },
	{ "WVASET",     wfrontend },
	{ "WWANG2ANSI",	wfrontend },
	{ "X4DBFILE",	wfrontend2 },
	{ "XX2BYTE",	wfrontend },

/*
** The following are NETRON CAP specific routines
*/
#ifdef NETCAP
	{ "WSCLOSE",	wfrontend },
	{ "WSFNM",	wfrontend },
	{ "WSFNS",	wfrontend },
#endif
/*
** The following are ACP routines 
*/
#ifdef ACP
	{ "BREAKACP",	wfrontend },
	{ "CHECKACP",	wfrontend },
	{ "CLOSEACP",	wfrontend },
	{ "GETACP",	wfrontend },
	{ "OPENACP",	wfrontend },
	{ "READACP",	wfrontend },
	{ "SETACP",	wfrontend },
	{ "WRITEACP",	wfrontend },
#endif

/*
** The following are EDE routines
*/
#ifdef EDE
	{ "A_WSLINK",	wfrontend },
	{ "DYLINK",	wfrontend },
	{ "DYUNLINK",	wfrontend },
	{ "EDCLRSCR",	wfrontend },
	{ "EDDRKSCR",	wfrontend },
	{ "EDEXIT",	wfrontend },
	{ "EDLOAD",	wfrontend },
	{ "EDLTESCR",	wfrontend },
	{ "EDNARSCR",	wfrontend },
	{ "EDWIDSCR",	wfrontend },
	{ "GCALC",	wfrontend },
	{ "GCALEND",	wfrontend },
	{ "GCLOCK",	wfrontend },
	{ "GENVEC",	wfrontend },
	{ "GNOTEPAD",	wfrontend },
	{ "GPUZZLE",	wfrontend },
	{ "MENUCONT",	wfrontend },
	{ "MENUEXIT",	wfrontend },
	{ "MENUGO",	wfrontend },
	{ "MENUINFO",	wfrontend },
	{ "MENUITEM",	wfrontend },
	{ "MENUKILL",	wfrontend },
	{ "MENULOAD",	wfrontend },
	{ "MENUMODE",	wfrontend },
	{ "MENUREST",	wfrontend },
	{ "MENUSAVE",	wfrontend },
	{ "NOPFKEYS",	wfrontend },
	{ "PFKEYSON",	wfrontend },
	{ "POPAREA",	wfrontend },
	{ "PUSHAREA",   wfrontend },
	{ "PUSHSCRN",	wfrontend },
	{ "RETRACE",	wfrontend },
	{ "TRACEEND",	wfrontend },
	{ "TRACEGO",	wfrontend },
	{ "VIDLINE",	wfrontend },
	{ "VIDMODE",	wfrontend },
	{ "VIDMOVE",	wfrontend },
	{ "VIDTEXT",	wfrontend },
#endif /* EDE */

/*
** The OLD WISP menuing system (obsolete)
*/
#ifdef ORIGMENU
	{ "MENU",	wfrontend },
#endif /* ORIGMENU */

/*
** This includes the CRID utility routines
*/
#ifdef CRID
#include "cridtbl.c"
#endif /* CRID */

/*
** Terminate with a NULL
*/
/************************************************************************/
#endif /* WISP */
	{ NULL,		NULL }
	};


/* Implementation of SYSTEM routine */

/* The following structure accesses the COLOR-MAP configuration	value	*/
/* maintained by the runtime system.  We pull the EXIT value out of 	*/
/* this table to set the default colors to be used by the SYSTEM call.	*/
/* The "w_set_fgbg" function is used to communicate the chosen colors   */
/* to the window manager prior to setting the terminal to its standard  */
/* operating mode.  */

typedef struct {
	char	foregrnd;
	char	backgrnd;
} COLORMAP;

extern	COLORMAP	colormap[19];

extern	int		Asystem();
extern	void		A_moveleft();
extern	void		w_set_fgbg P_((int, int));


#define	EXIT_COLOR	4
#define	MAXCMD		256

int
call_system( name, num_args, args, initial )
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
	char		command[ MAXCMD+1 ];
	unsigned	size;

	/* Check to see that we received reasonable parameters */

	if ( ( num_args != 1 && num_args != 2 ) || Numeric( args[0].a_type ) )
		return Halt;

	/* load USING parameter into local buffer and NULL terminate */

	size = a_size( args[0] ) > MAXCMD ? MAXCMD : a_size( args[0] );
	A_moveleft( command, args[0].a_address, size );
	command[ size ] = 0;

	/* set terminal to normal mode (unless two arguments used) */

	if ( num_args == 1 ) {
		w_set_fgbg( colormap[ EXIT_COLOR ].foregrnd,
			colormap[ EXIT_COLOR ].backgrnd );
		resetunit();
	}

	/* execute command and set return code to exit status */

	return_code = Asystem( command );

	/* set terminal back to COBOL state and return */

#ifdef	ACU_ALWAYS_INIT
	w_set_fgbg( 0, 0 );
	setunit();
#else	/* ACU_ALWAYS_INIT */
	if ( num_args == 1 ) {
		w_set_fgbg( 0, 0 );
		setunit();
	}
#endif	/* ACU_ALWAYS_INIT */
	return Okay;

}   /* call_system */



/* wstoasc - this routine simply takes an Argument and copies to a 'C'	*/
/* string, adding a NULL terminator.  It is provided for RM/COBOL-85 	*/
/* compatibility.  */

void
wstoasc( arg, dest )
Argument	*arg;
char		*dest;
{
	register char		*src;
	register unsigned	count;

	count = (unsigned) arg->a_length;
	for( src = arg->a_address;
			count-- && *src >= ' ' && *src <= '~';
			*dest++ = *src++ );
	*dest = 0;

}   /* wstoasc */

/*----------------------------------------------------------------------------
This table is used to extract the sign and digit from a single character that
combines both of them. The MS bit of the table entry is one if the sign is
negative. The 4 ls bits contain the digit.
----------------------------------------------------------------------------*/

static unsigned char COMBINED_SIGN_TABLE['}'-'0'+1] = {
    0,1,2,3,4,5,6,7,8,9,                               /* '0' - '9' */
    0,0,0,0,0,0,0,                                     /* punctuation */
    1,2,3,4,5,6,7,8,9,                                 /* 'A' - 'I' */
    0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,      /* 'J' - 'R' */
    0,0,0,0,0,0,0,0,                                   /* 'S' - 'Z' */
    0,0,0,0,0,0,                                       /* punctuation */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,                     /* 'a' - 'o' */
    0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89, /* 'p' - 'y' */
    0,                                                 /* 'z' */
    0,                                                 /* '{' */
    0,                                                 /* '_' */
    0x80                                               /* '}' */
};

/*----------------------------------------------------------------------------
This function returns a nonzero (true) value if the character c includes a
combined minus sign; otherwise, it returns zero (false).
----------------------------------------------------------------------------*/

static int
negative_from_combined(c)
int c;
{
    c &= 0xFF;
    return '0' <= c && c <= '}' ?
      COMBINED_SIGN_TABLE[c-'0'] & 0x80 : 0;
}

/*----------------------------------------------------------------------------
This function returns a number in the range 0-9 representing the digit
combined with a sign in the character c. It returns zero if the character c
does not have a combined digit.
----------------------------------------------------------------------------*/

static int
digit_from_combined(c)
int c;
{
    c &= 0xFF;
    return '0' <= c && c <= '}' ? COMBINED_SIGN_TABLE[c-'0'] & 0x0F : 0;
}

/*----------------------------------------------------------------------------
This function returns the integer part of the value of a string of ASCII or
binary digits.
----------------------------------------------------------------------------*/

static long numeric_value_of_string(address, digits, scale)
char *address;
int digits;
int scale;
{
    long n = 0;
    int ns = scale < 0 ? -scale : 0;
    while (--digits >= ns)
	n = 10 * n + (*address++ & 0xF);
    while (--scale >= 0)
	n *= 10;
    return n;
}

/*----------------------------------------------------------------------------
This function extracts packed digits from t and returns the integer part
of their value.
----------------------------------------------------------------------------*/

static long
numeric_value_of_packed(t, count, scale)
char *t;
int count;  /* number of digits */
int scale;
{
    long n = 0;
    int ns = scale < 0 ? -scale : 0;
    while (count >= 2+ns) {
	n = 10 * n + (*t >> 4 & 0xF);
	n = 10 * n + (*t++ & 0xF);
	count -= 2;
    }
    if (count == 1+ns)
	n = 10 * n + (*t >> 4 & 0xF);
    while (--scale >= 0)
	n *= 10;
    return n;
}

/*----------------------------------------------------------------------------
This function returns a true (nonzero) value if run on a little-endian machine.
----------------------------------------------------------------------------*/

static int
little_endian()
{
    union {
	char c[2];
	short s;
    } x;
    x.s = 1;
    return x.c[0];
}

/*----------------------------------------------------------------------------
This function gets the integral part of the value of a binary argument
containing count bytes starting at address.
----------------------------------------------------------------------------*/

static long
numeric_value_of_binary(address, count, is_signed, native, scale)
char *address;
int count;
int is_signed;  /* true if argument is signed */
int native;     /* true if argument is in native byte order */
int scale;
{
    unsigned char buffer[18];
    int i, x;
    int negative;
    long n;

    if (count > (int)sizeof(buffer))
	return 0;
    /* move to buffer, converting to big endian order if necessary */
    if (native && little_endian()) {
	for (i = 0; i < count; i++)
	    buffer[count-1-i] = address[i];
    } else {
	for (i = 0; i < count; i++)
	    buffer[i] = address[i];
    }
    /* save sign if necessary */
    if (is_signed && buffer[0] & 0x80) {
	i = count-1;
	while (i >= 0 && buffer[i] == 0)
	    i--;
	if (i >= 0) {
	    buffer[i] = 0x100 - buffer[i];
	    i--;
	}
	while (i >= 0) {
	    buffer[i] = 0xFF - buffer[i];
	    i--;
	}
	negative = 1;
    } else
	negative = 0;
    /* apply scaling */
    while (scale > 0) {
	x = 0;
	for (i = count-1; i >= 0; i--) {
	    x += 10 * buffer[i];
	    buffer[i] = x;
	    x >>= 8;
	}
	scale--;
    }
    while (scale < 0) {
	x = 0;
	for (i = 0; i < count; i++) {
	    x += buffer[i];
	    buffer[i] = x / 10;
	    x = x % 10 << 8;
	}
	scale++;
    }
    /* gather result */
    n = 0;
    for (i = 0; i < count; i++)
	n = n << 8 | buffer[i];
    return negative ? -n : n;
}

/*----------------------------------------------------------------------------
This function gets the integral part of the value of a numeric argument. It
returns zero if the argument is not numeric.
----------------------------------------------------------------------------*/

static long 
integral_part_of_value(p)
Argument *p;
{
    int s, d;
    long n = 0;
    switch (p->a_type) {
	case NumUnsigned:    /* Unsigned numeric DISPLAY */
	    n = numeric_value_of_string(p->a_address, p->a_digits,
	      Scale(p->a_scale));
	    break;
	case NumSignSep:     /* Signed numeric DISPLAY (trailing separate) */
	    n = numeric_value_of_string(p->a_address, p->a_digits,
	      Scale(p->a_scale));
	    if (p->a_address[p->a_digits] == '-')
		n = -n;
	    break;
	case NumSigned:      /* Signed numeric DISPLAY (trailing combined) */
	    s = Scale(p->a_scale);
	    n = numeric_value_of_string(p->a_address, p->a_digits-1, s+1);
	    if (s >= 0) {
		d = digit_from_combined(p->a_address[p->a_digits-1]);
		while (--s >= 0)
		    d = 10 * d;
		n += d;
	    }
	    if (negative_from_combined(p->a_address[p->a_digits-1]))
		n = -n;
	    break;
	case NumSepLead:     /* Signed numeric DISPLAY (leading separate) */
	    n = numeric_value_of_string(p->a_address+1, p->a_digits,
	      Scale(p->a_scale));
	    if (p->a_address[0] == '-')
		n = -n;
	    break;
	case NumLeading:     /* Signed numeric DISPLAY (leading combined) */
	    s = Scale(p->a_scale);
	    n = numeric_value_of_string(p->a_address+1, p->a_digits-1, s);
	    if ((int) p->a_digits + s >= 0) {
		d = digit_from_combined(p->a_address[0]);
		while ((int) p->a_digits + --s >= 0)
		    d = 10 * d;
		n += d;
	    }
	    if (negative_from_combined(p->a_address[0]))
		n = -n;
	    break;
	case CompSigned:     /* Signed COMP-2 */
	    n = numeric_value_of_string(p->a_address, p->a_digits,
	      Scale(p->a_scale));
	    if (p->a_address[p->a_digits] == 0x0D)
		n = - n;
	    break;
	case CompUnsigned:   /* Unsigned COMP-2 */
	    n = numeric_value_of_string(p->a_address, p->a_digits,
	      Scale(p->a_scale));
	    break;
	case PackedPositive: /* Positive packed-decimal COMP-3 */
	    n = numeric_value_of_packed(p->a_address, p->a_length*2-1,
	      Scale(p->a_scale));
	    break;
	case PackedSigned:   /* Signed packed-decimal COMP-3 */
	    n = numeric_value_of_packed(p->a_address, p->a_length*2-1,
	      Scale(p->a_scale));
	    if ((p->a_address[p->a_length-1] & 0xF) == 0xD)
		n = - n;
	    break;
	case PackedUnsigned: /* COMP-6 */
	    n = numeric_value_of_packed(p->a_address, p->a_length*2,
	      Scale(p->a_scale));
	    break;
	case BinarySigned:   /* Signed binary */
	    n = numeric_value_of_binary(p->a_address, p->a_length, 1, 0,
	      Scale(p->a_scale));
	    break;
	case BinaryUnsigned: /* Unsigned binary */
	    n = numeric_value_of_binary(p->a_address, p->a_length, 0, 0,
	      Scale(p->a_scale));
	    break;
	case NativeSigned:   /* Signed native-order binary */
	    n = numeric_value_of_binary(p->a_address, p->a_length, 1, 1,
	      Scale(p->a_scale));
	    break;
	case NativeUnsigned: /* Unsigned native-order binary */
	    n = numeric_value_of_binary(p->a_address, p->a_length, 0, 1,
	      Scale(p->a_scale));
	    break;
	case Float:          /* Float or Double */
	    switch (p->a_length) {
		case sizeof(float):
		    n = (long) * (float *) (p->a_address);
		    break;
		case sizeof(double):
		    n = (long) * (double *) (p->a_address);
		    break;
	    }
	    break;
    }
    return n;
}

#if 0		/* currently not used anywhere... */
/*----------------------------------------------------------------------------
This function returns a true (nonzero) value if a string of ASCII or binary 
digits is not all zeros.
----------------------------------------------------------------------------*/

static int 
nonzero_string(address, digits)
char *address;
int digits;
{
    while (--digits >= 0) {
	if (*address++ & 0xF)
	    return 1;
    }
    return 0;
}


/*----------------------------------------------------------------------------
This function returns a true (nonzero) value if the value of a numeric 
argument is nonzero. It returns zero if the argument is not numeric.
----------------------------------------------------------------------------*/

static int
value_is_nonzero(p)
Argument *p;
{
    int n = 0;
    switch (p->a_type) {
	case NumUnsigned:    /* Unsigned numeric DISPLAY */
	case NumSignSep:     /* Signed numeric DISPLAY (trailing separate) */
	case CompSigned:     /* Signed COMP-2 */
	case CompUnsigned:   /* Unsigned COMP-2 */
	    n = nonzero_string(p->a_address, p->a_digits);
	    break;
	case NumSigned:      /* Signed numeric DISPLAY (trailing combined) */
	    n = nonzero_string(p->a_address, p->a_digits-1) ||
	      digit_from_combined(p->a_address[p->a_digits-1]);
	    break;
	case NumSepLead:     /* Signed numeric DISPLAY (leading separate) */
	    n = nonzero_string(p->a_address+1, p->a_digits);
	    break;
	case NumLeading:     /* Signed numeric DISPLAY (leading combined) */
	    n = nonzero_string(p->a_address+1, p->a_digits-1) ||
	      digit_from_combined(p->a_address[0]);
	    break;
	case PackedPositive: /* Positive packed-decimal COMP-3 */
	case PackedSigned:   /* Signed packed-decimal COMP-3 */
	    n = nonzero_string(p->a_address, p->a_length-1) ||
		p->a_address[p->a_length-1] & 0xF0;
	    break;
	case PackedUnsigned: /* COMP-6 */
	    n = nonzero_string(p->a_address, p->a_length);
	    break;
	case BinarySigned:   /* Signed binary */
	case BinaryUnsigned: /* Unsigned binary */
	case NativeUnsigned: /* Unsigned native-order binary */
	case NativeSigned: { /* Signed native-order binary */
	    int i;
	    for (i = 0; i < p->a_length; i++) {
		if (p->a_address[i] != 0) {
		    n = 1;
		    break;
		}
	    }
	    break;
	}
	case Float:          /* Float or Double */
	    switch (p->a_length) {
		case sizeof(float):
		    n = * (float *) (p->a_address) != 0.0;
		    break;
		case sizeof(double):
		    n = * (double *) (p->a_address) != 0.0;
		    break;
	    }
	    break;
    }
    return n;
}
#endif	/* 0 */

extern long cbl_exit_proc();
extern long cbl_error_proc();
extern long cbl_get_exit_info();

int
call_cbl_exit_proc( name, num_args, args, initial )
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
    if (num_args == 2 && Numeric(args[0].a_type) && 
      args[1].a_type == Alphanum)
	return_code = cbl_exit_proc(integral_part_of_value(&args[0]),
	  args[1].a_address, args[1].a_length);
    return Okay;
}

int
call_cbl_error_proc( name, num_args, args, initial )
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
    if (num_args == 2 && Numeric(args[0].a_type) && 
      args[1].a_type == Alphanum)
	return_code = cbl_error_proc(integral_part_of_value(&args[0]),
	  args[1].a_address, args[1].a_length);
    return Okay;
}

int
call_cbl_get_exit_info( name, num_args, args, initial )
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
    if (num_args == 1 && args[0].a_type == Group && args[0].a_length >= 16)
	return_code = cbl_get_exit_info(args[0].a_address);
    return Okay;
}

#ifdef WISP
/************************************************************************/
extern ACUGARGS();
extern ACUNARGS();
extern ACUPARGS();
extern BELL();
extern BITPACK();
extern BITUNPK();
extern CANCEL();
extern CEXIT();
extern COBLINK();
extern DATE2();
extern DATE4();
extern DAY();
extern EXTRACT();
extern FILECOPY();
extern FIND();
extern FXZONE();
extern GETPARM();
extern HEXPACK();
extern HEXUNPK();
extern ISDBFILE();
extern LINK2();
extern LINKPROC();
extern LOGOFF();
extern MESSAGE();
extern NOHELP();
extern ONHELP();
extern PRINT();
extern PUTPARM();
extern READFDR();
extern READFDR4();
extern READVTOC();
extern RETCODE();
extern SCRATCH();
extern SCREEN();
extern SEARCH();
extern SET();
extern SET8BIT();
extern SETFILE();
extern SETSUBMIT();
extern SETTRIGPROG();
extern SORT();
extern SORTCALL();
extern SORTINFO();
extern SORTLINK();
extern STRING();
extern SUBMIT();
extern UPDATFDR();
extern UPPER();
extern USEHARDLINK();
extern USESOFTLINK();
extern W4WAPI();
extern WACCEPT();
extern WANSI2WANG();
extern WCHAIN();
extern WDISPLAY();
extern WISPDATE();
extern WISPEXIT();
extern WISPHELP();
extern WISPPLAT();
extern WISPSHUT();
extern WISPSORT();
extern WISPSYNC();
extern WSTOP();
extern WSXIO();
extern WTITLE();
extern WWANG2ANSI();
extern bit_off();
extern bit_on();
extern bit_test();
extern getparmbuild();
extern getwfilext();
extern greclen();
extern initwisp();
extern initwisp2();
extern lbit_off();
extern lbit_on();
extern mwconv();
extern setprogid();
extern setretcode();
extern setrunname();
extern setwfilext();
extern setwispfilext();
extern vexit();
extern vwang();
extern w2rowcol();
extern wexith();
extern wfclose();
extern wfilechk();
extern wfilechk2();
extern wfname();
extern wfopen();
extern wfopen2();
extern wfopen3();
extern wfswait();
extern wfwait();
extern wmemcpy();
extern wpause();
extern wrename();
extern ws132();
extern ws80();
extern wscreen();
extern wsetstat();
extern wvaset();
extern x4dbfile();
extern xx2byte();

/*
** The following are NETRON CAP specific routines
*/
#ifdef NETCAP
extern WSCLOSE();
extern WSFNM();
extern WSFNS();
#endif

/*
** The following are ACP routines
*/
#ifdef ACP
extern OPENACP();
extern CLOSEACP();
extern READACP();
extern WRITEACP();
extern BREAKACP();
extern CHECKACP();
extern GETACP();
extern SETACP();
#endif

/*
** The following are EDE routines
*/
#ifdef EDE
extern A_WSLINK();
extern DYLINK();
extern DYUNLINK();
extern EDCLRSCR();
extern EDDRKSCR();
extern EDEXIT();
extern EDLOAD();
extern EDLTESCR();
extern EDNARSCR();
extern EDWIDSCR();
extern GENVEC();
extern MENUCONT();
extern MENUEXIT();
extern MENUGO();
extern MENUINFO();
extern MENUITEM();
extern MENUKILL();
extern MENULOAD();
extern MENUMODE();
extern MENUREST();
extern MENUSAVE();
extern NOPFKEYS();
extern PFKEYSON();
extern POPAREA();
extern PUSHAREA();
extern PUSHSCRN();
extern RETRACE();
extern TRACEEND();
extern TRACEGO();
extern VIDLINE();
extern VIDMODE();
extern VIDMOVE();
extern VIDTEXT();
extern gcalc();
extern gcalend();
extern gclock();
extern gen_ncpfkey();
extern gnotepad();
extern gpuzzle();
extern nc_pop_menu();
extern ws_bar_menu();
#endif /* EDE */

#ifdef ORIGMENU
extern menu();
#endif /* ORIGMENU */

/*
	The first entry is UPPERCASE.
	The second is case sensitive.
*/
struct	PROCTABLE WISPTABLE[] = {
	{ "ACUGARGS",	ACUGARGS },
	{ "ACUNARGS",	ACUNARGS },
	{ "ACUPARGS",	ACUPARGS },
	{ "BELL",	BELL },
	{ "BITPACK",	BITPACK },
	{ "BITUNPK",	BITUNPK },
	{ "BIT_OFF",	bit_off }, 
	{ "BIT_ON",	bit_on },
	{ "BIT_TEST",	bit_test },
	{ "CANCEL",	CANCEL },	
	{ "CEXIT",	CEXIT },	
	{ "COBLINK",	COBLINK },	
	{ "DATE",	WISPDATE },
	{ "DATE2",	DATE2 },
	{ "DATE4",	DATE4 },
	{ "DAY",	DAY },
	{ "EXTRACT",	EXTRACT },	
	{ "FILECOPY",	FILECOPY },
	{ "FIND",	FIND },
	{ "FXZONE",	FXZONE },
	{ "GETPARM",	GETPARM },
	{ "GETPARMBUILD",getparmbuild },
	{ "GETWFILEXT", getwfilext },
	{ "GRECLEN",    greclen },
	{ "HEXPACK",	HEXPACK },
	{ "HEXUNPK",	HEXUNPK },
	{ "INITWISP",	initwisp },
	{ "INITWISP2",	initwisp2 },
	{ "ISDBFILE",	ISDBFILE },
	{ "LBIT_OFF",	lbit_off },
	{ "LBIT_ON",	lbit_on },
	{ "LINK",	LINK2 },	/* NOTE: Translate LINK ==> LINK2 */
	{ "LINKPROC",	LINKPROC },
	{ "LOGOFF",	LOGOFF },
	{ "MESSAGE",	MESSAGE },
	{ "MWCONV",	mwconv },
	{ "NOHELP",	NOHELP },
	{ "ONHELP",	ONHELP },
	{ "PRINT",	PRINT },
	{ "PUTPARM",	PUTPARM },
	{ "READFDR",	READFDR },
	{ "READFDR4",	READFDR4 },
	{ "READVTOC",	READVTOC },
	{ "RETCODE",	RETCODE },
	{ "SCRATCH",	SCRATCH },
	{ "SCREEN",	SCREEN },
	{ "SEARCH",	SEARCH },
	{ "SET",	SET },
	{ "SET8BIT",	SET8BIT },
	{ "SETFILE",	SETFILE},
	{ "SETPROGID",  setprogid },
	{ "SETRETCODE", setretcode },
	{ "SETRUNNAME", setrunname },
	{ "SETSUBMIT",	SETSUBMIT },
	{ "SETTRIGPROG",SETTRIGPROG },
	{ "SETWFILEXT", setwfilext },
	{ "SETWISPFILEXT", setwispfilext},
	{ "SORT",	SORT },
	{ "SORTCALL",	SORTCALL },
	{ "SORTINFO",	SORTINFO },
	{ "SORTLINK",	SORTLINK },
	{ "STRING",	STRING },
	{ "SUBMIT",	SUBMIT },
	{ "UPDATFDR",	UPDATFDR },
	{ "UPPER",	UPPER },
	{ "USEHARDLINK",USEHARDLINK },
	{ "USESOFTLINK",USESOFTLINK },
	{ "VEXIT",      vexit },
	{ "VWANG",	vwang },
	{ "W2ROWCOL",	w2rowcol },
	{ "W4WAPI",	W4WAPI },
	{ "WACCEPT",	WACCEPT },
	{ "WANSI2WANG", WANSI2WANG },	
	{ "WCHAIN",	WCHAIN },
	{ "WDISPLAY",	WDISPLAY },
	{ "WEXITH",     wexith },
	{ "WFCLOSE",	wfclose },
	{ "WFILECHK",   wfilechk },
	{ "WFILECHK2",  wfilechk2 },
	{ "WFNAME",     wfname },
	{ "WFOPEN",	wfopen },
	{ "WFOPEN2",	wfopen2 },
	{ "WFOPEN3",	wfopen3 },
	{ "WFSWAIT",    wfswait },
	{ "WFWAIT",	wfwait },
	{ "WISPEXIT",	WISPEXIT },
	{ "WISPHELP",	WISPHELP },
	{ "WISPPLAT",	WISPPLAT },
	{ "WISPSHUT",	WISPSHUT },
	{ "WISPSORT",	WISPSORT },
	{ "WISPSYNC",	WISPSYNC },
	{ "WMEMCPY",    wmemcpy },
	{ "WPAUSE",     wpause },
	{ "WRENAME",	wrename },
	{ "WS132",	ws132 },
	{ "WS80",	ws80 },
	{ "WSCREEN",	wscreen },
	{ "WSETSTAT",	wsetstat },
	{ "WSTOP",	WSTOP },
	{ "WSXIO",	WSXIO },
	{ "WTITLE",	WTITLE },
	{ "WVASET", 	wvaset },
	{ "WWANG2ANSI", WWANG2ANSI },	
	{ "X4DBFILE",	x4dbfile },
	{ "XX2BYTE",	xx2byte },

/*
** The following are NETRON CAP specific routines
*/
#ifdef NETCAP
	{ "WSCLOSE",	WSCLOSE },
	{ "WSFNM",	WSFNM },
	{ "WSFNS",	WSFNS },
#endif
/*
** The following are ACP routines 
*/
#ifdef ACP
	{ "BREAKACP",	BREAKACP },
	{ "CHECKACP",	CHECKACP },
	{ "CLOSEACP",	CLOSEACP },
	{ "GETACP",	GETACP   },
	{ "OPENACP",	OPENACP  },
	{ "READACP",	READACP  },
	{ "SETACP",	SETACP   },
	{ "WRITEACP",	WRITEACP },
#endif
/*
** The following are EDE routines
*/
#ifdef EDE
	{ "A_WSLINK",	A_WSLINK },
	{ "DYLINK",	DYLINK 	},
	{ "DYUNLINK",	DYUNLINK },
	{ "EDCLRSCR",	EDCLRSCR },
	{ "EDDRKSCR",	EDDRKSCR },
	{ "EDEXIT",	EDEXIT 	},
	{ "EDLOAD",	EDLOAD 	},
	{ "EDLTESCR",	EDLTESCR },
	{ "EDNARSCR",	EDNARSCR },
	{ "EDWIDSCR",	EDWIDSCR },
	{ "GCALC",	gcalc },
	{ "GCALEND",	gcalend },
	{ "GCLOCK",	gclock },
	{ "GENVEC",	GENVEC },
	{ "GEN_NCPFKEY",gen_ncpfkey},
	{ "GNOTEPAD",	gnotepad },
	{ "GPUZZLE",	gpuzzle },
	{ "MENUCONT",	MENUCONT },
	{ "MENUEXIT",	MENUEXIT },
	{ "MENUGO",	MENUGO 	},
	{ "MENUINFO",	MENUINFO },
	{ "MENUITEM",	MENUITEM },
	{ "MENUKILL",	MENUKILL },
	{ "MENULOAD",	MENULOAD },
	{ "MENUMODE",	MENUMODE },
	{ "MENUREST",	MENUREST },
	{ "MENUSAVE",	MENUSAVE },
	{ "NC_POP_MENU",nc_pop_menu},
	{ "NOPFKEYS",	NOPFKEYS },
	{ "PFKEYSON",	PFKEYSON },
	{ "POPAREA",	POPAREA },
	{ "PUSHAREA",	PUSHAREA },
	{ "PUSHSCRN",	PUSHSCRN },
	{ "RETRACE",	RETRACE },
	{ "TRACEEND",	TRACEEND },
	{ "TRACEGO",	TRACEGO },
	{ "VIDLINE",	VIDLINE },
	{ "VIDMODE",	VIDMODE },
	{ "VIDMOVE",	VIDMOVE },
	{ "VIDTEXT",	VIDTEXT },
	{ "WS_BAR_MENU",ws_bar_menu},
#endif /* EDE */
/*
** The OLD wisp menu system (obsolete)
*/
#ifdef ORIGMENU
	{ "MENU", 	menu },
#endif /* ORIGMENU */

/*
** Terminate with a NULL
*/
	{ NULL,		NULL }
};


wfrontend( name, num_args, args, initial )
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
#define MAXARGS 128
	int i;
	int (*fn)();
	struct PROCTABLE *p;

	for (p = WISPTABLE; p->name && 0 != strcmp(p->name,name); ++p);
	if (p->name && 0 == strcmp(p->name,name))
	{
		fn = p->routine;
	}
 	else
	{
		return 1;
	}

	if (num_args > MAXARGS)
	{
		char mess[80];
		
		sprintf(mess,"%%SUB85-E-WFRONTEND MAXARGS exceeded [num_args = %d]",num_args);
		werrlog(104,mess,0,0,0,0,0,0,0);
		
		num_args = MAXARGS;
	}

	wvaset(&num_args);

	if (num_args > 0)
	{
		long	tempbin[MAXARGS];					/* Place to store binary values to reverse bytes*/
		void 	*arglist[MAXARGS]; 

		memset((char *)arglist, '\0', sizeof(arglist));

		for (i=0; i < num_args; ++i)
		{
			if ( !bytenormal() && ( args[i].a_type == BinarySigned || args[i].a_type == BinaryUnsigned ) )
			{
				memcpy(&tempbin[i], args[i].a_address, args[i].a_length);	/* Store bin in temp area	*/
				reversebytes(&tempbin[i], args[i].a_length);			/* Reverse the bytes		*/
				arglist[i] = (void *)&tempbin[i];				/* Put addr of temp in arglist	*/
			}
			else
			{
				arglist[i] = (void *)args[i].a_address;
			}
		}

		/*
		**	Call the function.
		*/
		(*fn)(
		      arglist[0],      arglist[1],	arglist[2],      arglist[3],
		      arglist[4],      arglist[5],    	arglist[6],      arglist[7],
		      arglist[8],      arglist[9],  	arglist[10],     arglist[11],
		      arglist[12],     arglist[13],   	arglist[14],     arglist[15],
		      arglist[16],     arglist[17],   	arglist[18],     arglist[19],
		      arglist[20],     arglist[21],   	arglist[22],     arglist[23],
		      arglist[24],     arglist[25],	arglist[26],     arglist[27],
		      arglist[28],     arglist[29],	arglist[30],     arglist[31],
		      arglist[32],     arglist[33],	arglist[34],     arglist[35],
		      arglist[36],     arglist[37],	arglist[38],     arglist[39],
		      arglist[40],     arglist[41],	arglist[42],     arglist[43],
		      arglist[44],     arglist[45],	arglist[46],     arglist[47],
		      arglist[48],     arglist[49],	arglist[50],     arglist[51],
		      arglist[52],     arglist[53],	arglist[54],     arglist[55],
		      arglist[56],     arglist[57],	arglist[58],     arglist[59],
		      arglist[60],     arglist[61],	arglist[62],     arglist[63],
		      arglist[64],     arglist[65],	arglist[66],     arglist[67],
		      arglist[68],     arglist[69],	arglist[70],     arglist[71],
		      arglist[72],     arglist[73],	arglist[74],     arglist[75],
		      arglist[76],     arglist[77],	arglist[78],     arglist[79],
		      arglist[80],     arglist[81],	arglist[82],     arglist[83],
		      arglist[84],     arglist[85],	arglist[86],     arglist[87],
		      arglist[88],     arglist[89],	arglist[90],     arglist[91],
		      arglist[92],     arglist[93],	arglist[94],     arglist[95],
		      arglist[96],     arglist[97],	arglist[98],     arglist[99],
		      arglist[100],    arglist[101],	arglist[102],    arglist[103],
		      arglist[104],    arglist[105],	arglist[106],    arglist[107],
		      arglist[108],    arglist[109],	arglist[110],    arglist[111],
		      arglist[112],    arglist[113],	arglist[114],    arglist[115],
		      arglist[116],    arglist[117],	arglist[118],    arglist[119],
		      arglist[120],    arglist[121],	arglist[122],    arglist[123],
		      arglist[124],    arglist[125],	arglist[126],    arglist[127]);

		if ( !bytenormal() )
		{
			for (i=0; i < num_args; ++i)
			{
				if ( args[i].a_type == BinarySigned ||
				     args[i].a_type == BinaryUnsigned )
				{
					reversebytes(&tempbin[i], args[i].a_length);		/* Undo the byte reverse	*/
					memcpy(args[i].a_address, &tempbin[i], args[i].a_length);	/* Restore argument	*/
				}
			}
		}
	}
	else 
	{
		/*
		**	Call the function with no arguments
		*/
		(*fn)();
	}	
	return 0;
}

/*
	wfrontend2 - Acts just like wfrontend except it puts both the address and the length on the stack. So for each arg
		     there 2 entries on the stack.
*/	
wfrontend2( name, num_args, args, initial )
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
#define MAXARGS2 64
	int i,l;
	int (*fn)();
	struct PROCTABLE *p;

	for (p = WISPTABLE; p->name && 0 != strcmp(p->name,name); ++p);
	if (p->name && 0 == strcmp(p->name,name))
	{
		fn = p->routine;
	}
 	else
	{
		return 1;
	}

	if (num_args > MAXARGS2)
	{
		char mess[80];
		
		sprintf(mess,"%%SUB85-E-WFRONTEND2 MAXARGS2 exceeded [num_args = %d]",num_args);
		werrlog(104,mess,0,0,0,0,0,0,0);

		num_args = MAXARGS2;
	}

	wvaset(&num_args);

	if (num_args > 0)
	{
		long	tempbin[MAXARGS2];					/* Place to store binary values to reverse bytes*/
		void 	*arglist[MAXARGS2*2]; 

		memset((char *)arglist, '\0', sizeof(arglist));

		for (i=0,l=0; i < num_args; ++i)
		{
			if ( !bytenormal() && ( args[i].a_type == BinarySigned || args[i].a_type == BinaryUnsigned ) )
			{
				memcpy(&tempbin[i], args[i].a_address, args[i].a_length);	/* Store bin in temp area	*/
				reversebytes(&tempbin[i], args[i].a_length);			/* Reverse the bytes		*/
				arglist[l++] = (void *)&tempbin[i];				/* Put addr of temp in arglist	*/
			}
			else
			{
				arglist[l++] = (void *)args[i].a_address; 
			}

			arglist[l++] = (void *)args[i].a_length;				/* Put the length on the stack	*/
		}

		/*
		**	Call the function.
		*/
		(*fn)(
		      arglist[0],	arglist[1],	arglist[2],      arglist[3],
		      arglist[4],	arglist[5],   	arglist[6],      arglist[7],
		      arglist[8],	arglist[9],   	arglist[10],     arglist[11],
		      arglist[12],	arglist[13],  	arglist[14],     arglist[15],
		      arglist[16],	arglist[17],  	arglist[18],     arglist[19],
		      arglist[20],	arglist[21],  	arglist[22],     arglist[23],
		      arglist[24],	arglist[25],	arglist[26],     arglist[27],
		      arglist[28],	arglist[29],	arglist[30],     arglist[31],
		      arglist[32],	arglist[33],	arglist[34],     arglist[35],
		      arglist[36],	arglist[37],	arglist[38],     arglist[39],
		      arglist[40],	arglist[41],	arglist[42],     arglist[43],
		      arglist[44],	arglist[45],	arglist[46],     arglist[47],
		      arglist[48],	arglist[49],	arglist[50],     arglist[51],
		      arglist[52],	arglist[53],	arglist[54],     arglist[55],
		      arglist[56],	arglist[57],	arglist[58],     arglist[59],
		      arglist[60],	arglist[61],	arglist[62],     arglist[63],
		      arglist[64],	arglist[65],	arglist[66],     arglist[67],
		      arglist[68],	arglist[69],	arglist[70],     arglist[71],
		      arglist[72],	arglist[73],	arglist[74],     arglist[75],
		      arglist[76],	arglist[77],	arglist[78],     arglist[79],
		      arglist[80],	arglist[81],	arglist[82],     arglist[83],
		      arglist[84],	arglist[85],	arglist[86],     arglist[87],
		      arglist[88],	arglist[89],	arglist[90],     arglist[91],
		      arglist[92],	arglist[93],	arglist[94],     arglist[95],
		      arglist[96],	arglist[97],	arglist[98],     arglist[99],
		      arglist[100],	arglist[101],	arglist[102],    arglist[103],
		      arglist[104],	arglist[105],	arglist[106],    arglist[107],
		      arglist[108],	arglist[109],	arglist[110],    arglist[111],
		      arglist[112],	arglist[113],	arglist[114],    arglist[115],
		      arglist[116],	arglist[117],	arglist[118],    arglist[119],
		      arglist[120],	arglist[121],	arglist[122],    arglist[123],
		      arglist[124],	arglist[125],	arglist[126],    arglist[127]);


		if ( !bytenormal() )
		{
			for (i=0; i < num_args; ++i)
			{
				if ( args[i].a_type == BinarySigned ||
				     args[i].a_type == BinaryUnsigned )
				{
					reversebytes(&tempbin[i], args[i].a_length);		/* Undo the byte reverse	*/
					memcpy(args[i].a_address, &tempbin[i], args[i].a_length);	/* Restore argument	*/
				}
			}
		}
	}
	else 
	{
		/*
		**	Call the function with no arguments
		*/
		(*fn)();
	}	

	return 0;
}

/*
**	Routine:	call_acucobol()
**
**	Function:	To "call" an Acucobol COBOL routine from 'C'.
**
**	Description:	This routine builds the correct calling sequence to call
**			an Acucobol COBOL routine from 'C'.
**			Currently this is used for the MSDOS implementation of LINK.
**
**	Arguments:
**	filespec	The full file spec of the COBOL routine to call.
**	parmcnt		The number of parameters to pass.
**	parms		Array of pointers to the parms.
**	lens		Array of lengths of the parms
**	rc		The return code from the "cobol" routine (A_call_err). 
**
**	Globals:
**	A_call_err	The error code from Acucobol
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	12/22/92	Written by GSL
**
*/
void call_acucobol( filespec, parmcnt, parms, lens, rc )
char *filespec;
int4 parmcnt;
char *parms[];
int4 lens[];
int *rc;
{
#define MAX_CALL_ARGS	32
/* extern	short A_call_err; */

#ifdef WIN32
#ifdef ACUCANCEL42
extern void acu_cancel(char *);
#else
extern void PASCAL acu_cancel(char *);
#endif
#else
extern void acu_cancel();
#endif
	Argument args[MAX_CALL_ARGS];
	int	argcnt, i;

	*rc = 0;
	if (parmcnt > MAX_CALL_ARGS)
	{
		*rc = -1;
		return;
	}

	argcnt = (int)parmcnt;

	for(i=0; i<argcnt; i++)
	{
		args[i].a_address = parms[i];
		args[i].a_length  = (int)lens[i];
	}

	if (cobol(filespec, argcnt, args))
	{
		*rc = -1; /* (int)A_call_err; */
	}

	/*
	**	Cancel the program
	*/
	acu_cancel(filespec);

	return;
}

static int wisp_wexit = 0;

void shutexitcobol(exit_code)					/* Called by wisp from wexit()  */
int exit_code;
{
	extern void stop_runtime();
	
	/*
	**	If already processing WISP exit logic then just return.
	*/
	if (wisp_wexit)
	{
		return;
	}

	/*
	**	This prevents a COBOL and WISP exit loop.
	*/
	wisp_wexit = 1;

	/*
	**	Call stop_runtime() to have cobol stop the runtime.
	*/
	stop_runtime(exit_code,0,0,0,0,0);			/* Acucobol routine to shutdown */
}

void start_cobol_screen_handler()
{
	w_set_term();
}

void shutdown_cobol_screen_handler()
{
	w_reset_term();
}

/*
	AShutdown is called by ACUCOBOL at shutdown.
	If error_halt==0 then a normal STOP RUN otherwise error.

	Shutdown() was renamed to AShutdown() for version 2.4.x
*/
void
AShutdown( error_halt )
int	error_halt;
{
	/*
	**	If a normal exit from the runtime then just return.
	*/
	if (0==error_halt)
	{
		return;
	}
	
	/*
	**	This is an abnormal exit (0 != error_halt).
	**
	**	If we are not already processing wexit() then go thur the WISP exit logic.
	*/
	if (0 == wisp_wexit)
	{
		if (!nativescreens())
		{
			
			werrlog(104,"Terminating on an error detected by ACUCOBOL.",0,0,0,0,0,0,0);
#ifdef unix
			werrlog(104,"Use the -e runtime option to capture the error message.",0,0,0,0,0,0,0);
#endif
		}
		
		wisp_wexit = 1;

		wexit(error_halt);
	}
}
void Shutdown(error_halt)
int error_halt;
{
	AShutdown(error_halt);
}

#define Shutdown OLD_Shutdown
#define AShutdown OLD_Shutdown

int
exam_args( argc, argv )
int argc;
char *argv[];
{
	int idx;
	int gotdashd = 0;

	/* Start looking at argv[1] the second arg */
	for (idx=1; idx<argc; idx++)
	{
		if (argv[idx] && 0==strcmp(argv[idx],"-d"))
		{
			gotdashd = 1;
		}
	}
	if (gotdashd)
	{
		set_isdebug_true();
	}
	else
	{
		set_isdebug_false();
	}
	return 0;
}

#define exam_args OLD_exam_args

/*
**	Routine:	ISDBFILE()
**
**	Function:	Check if a file is a Database file
**
**	Description:	This routine uses Agetenv() to check the ACUCONFIG file
**			to see if this file is a database file.
**
**	Arguments:
**	select_name	The COBOL select name of the file. It should be terminated
**			by a space or a period or by length.
**	answer		The answer flag 
**				'Y' yes
**				'N' no
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	04/06/94	Written by GSL
**
*/
int ISDBFILE(select_name, l1, answer, l2)
char *select_name;
int4 l1;
char *answer;
int4 l2;
{
	static	int first = 1;
	static	char	default_host[40];
	int	i;
	char	*ptr;
	char	test_host[80];
	char	buff[80];
	char	*Agetenv();

	if (first)
	{
		/*
		**	The first time thru get the default host, if not set assume VISION.
		*/
		if (ptr = Agetenv("DEFAULT_HOST"))
		{
			strcpy(default_host,ptr);
			upper_string(default_host);
		}
		else
		{
			strcpy(default_host,"VISION");
		}
		first = 0;
	}

	for(i=0; i < l1; i++)
	{
		if (select_name[i] <= ' ' ||
		    select_name[i] == '.' ||
		    select_name[i] >  'z'   ) break;

		test_host[i] = select_name[i];
		if (test_host[i] == '-') test_host[i] = '_';
	}
	test_host[i] = (char)0;
	strcat(test_host,"_HOST");
	upper_string(test_host);

	if (ptr = Agetenv(test_host))
	{
		strcpy(buff,ptr);
		upper_string(buff);
	}
	else
	{
		strcpy(buff,default_host);
	}

	if (	(0 == strcmp(buff,"INFORMIX"))	||
		(0 == strcmp(buff,"ORACLE"))	||
		(0 == strcmp(buff,"SYBASE"))	  )
	{
		*answer = 'Y';
	}
	else
	{
		*answer = 'N';
	}
	return 0;
}

/*
**	Routine:	x4dbfile()
**
**	Function:	To set the IS_DBFILE flag in a files status word.
**
**	Description:	This routine uses ISDBFILE() to check the ACUCONFIG file
**			to see if this file is a database file if it is it will set
**			flag in the status word.
**
**	Arguments:
**	select_name	The COBOL select name of the file. It should be terminated
**			by a space or a period or by length.
**	select_status	The 4 byte status for the file.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	04/06/94	Written by GSL
**
*/
int x4dbfile(select_name, l1, select_status, l2)
char *select_name;
int4 l1;
int *select_status;
int4 l2;
{
	char	answer;
	int4	one = 1;

	ISDBFILE(select_name, l1, &answer, one);

	if ('Y' == answer)
	{
		setdbfile(select_status,1);
	}
	else
	{
		setdbfile(select_status,0);
	}
	return 0;
}

/*
** Include CRID interface routines
*/
#ifdef CRID
#include "crid85.c"
#endif /* CRID */

/************************************************************************/
#endif /* WISP */


