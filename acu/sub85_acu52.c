/*
**	Id:	$Id:$
**	File:	sub85_acu52.c		
**
**	This is the WISP compatable version of sub85.c for use with 
**	Acucobol 5.2 or later.
**
**	If you are using Acucobol 5.2 you will need to copy
**	and rename this file to sub85.c.
**
**	All of the WISP added code is enclosed in
**	"#ifdef WISP" statements.
*/
#define WISP
#define WISP_ACU52


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
/*
**  Include the WISP header info
*/
#define WISP_SUB85_HEADER
#include "wisp_sub85_inc.c"
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
** This includes the WISP LIBTABLE entries
*/
#define WISP_SUB85_LIBTABLE
#include "wisp_sub85_inc.c"
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
/*
** Include WISP interface routines
*/
#define WISP_SUB85_ROUTINES
#include "wisp_sub85_inc.c"
/************************************************************************/
#endif /* WISP */


