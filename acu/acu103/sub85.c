/*
**	File:	acu103/sub85.c
**
**	This is the WISP compatible version of sub85.c for use with 
**	Acucobol 10.3 
**
**	All of the WISP added code is enclosed in "#ifdef WISP" statements.
*/
#define WISP
#define WISP_ACU103

/* sub85.c - RM/COBOL-85 compatible 'C' routine interface */

/*
** Copyright (C) 1993,1995,1997,1999-2003,2007-2008,2010,2015-2016 Micro Focus.
** All rights reserved.
*/

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


#ifdef	ACU_SOURCE_FILENAME
#undef	ACU_SOURCE_FILENAME
#endif	/* ACU_SOURCE_FILENAME */
#define	ACU_SOURCE_FILENAME	"lib/sub85.c"
const char what_lib_sub85_c_str[] = "@(#) " ACU_SOURCE_FILENAME " $Date: 2017-12-22 17:24:40 +0000 (Fri, 22 Dec 2017) $$Rev: 72641 $";

int call_system(char *, int, Argument [], int);
void wstoasc(Argument *, char *);


struct	PROCTABLE LIBTABLE[] = {
	{ "SYSTEM", (pfSub85Intf)call_system, NULL },

#ifdef WISP
/************************************************************************/
/*
** This includes the WISP LIBTABLE entries
*/
#define WISP_SUB85_LIBTABLE
#include "wisp_sub85_inc.c"
/************************************************************************/
#endif /* WISP */

	{ NULL,	NULL, NULL }
};


/* Implementation of SYSTEM routine */

/* The following structure accesses the COLOR-MAP configuration	value	*/
/* maintained by the runtime system.  We pull the EXIT value out of 	*/
/* this table to set the default colors to be used by the SYSTEM call.	*/
/* The "w_set_fgbg" function is used to communicate the chosen colors   */
/* to the window manager prior to setting the terminal to its standard  */
/* operating mode.  */

typedef struct {
    char foregrnd;
    char backgrnd;
} COLORMAP;

extern COLORMAP colormap[19];

extern int Asystem(char *);
extern void w_set_fgbg(int, int);


#define	EXIT_COLOR	4
#define	MAXCMD		256

int
call_system(char *name A_UNUSED, int num_args, Argument args[],
	int initial A_UNUSED)
{
	char		command[ MAXCMD+1 ];
	unsigned	size;

	/* Check to see that we received reasonable parameters */

	if ( ( num_args != 1 && num_args != 2 ) || Numeric( args[0].a_type ) )
		return Halt;

	/* load USING parameter into local buffer and NULL terminate */

	size = a_size( args[0] ) > MAXCMD ? MAXCMD : a_size( args[0] );
	memcpy( command, args[0].a_address, size );
	command[ size ] = 0;

	/* set terminal to normal mode (unless two arguments used) */

#ifndef	_WINDOWS
	if ( num_args == 1 ) {
		w_set_fgbg( colormap[ EXIT_COLOR ].foregrnd,
			colormap[ EXIT_COLOR ].backgrnd );
		resetunit();
	}
#endif	/* _WINDOWS */

	/* execute command and set return code to exit status */

	return_code = Asystem( command );

	/* set terminal back to COBOL state and return */

#ifndef	_WINDOWS
#ifdef	ACU_ALWAYS_INIT
	w_set_fgbg( 0, 0 );
	setunit();
#else	/* ACU_ALWAYS_INIT */
	if ( num_args == 1 ) {
		w_set_fgbg( 0, 0 );
		setunit();
	}
#endif	/* ACU_ALWAYS_INIT */
#endif	/* _WINDOWS */
	return Okay;

}   /* call_system */



/* wstoasc - this routine simply takes an Argument and copies to a 'C'	*/
/* string, adding a NULL terminator.  It is provided for RM/COBOL-85 	*/
/* compatibility.  */

void
wstoasc(Argument *arg, char *dest)
{
	register char		*src;
	register unsigned	count;

	count = (unsigned) arg->a_length;
	for( src = arg->a_address;
			count-- && *src >= ' ' && *src <= '~';
			*dest++ = *src++ );
	*dest = 0;

}   /* wstoasc */

#ifdef WISP
/************************************************************************/
/*
** Include WISP interface routines
*/
#define WISP_SUB85_ROUTINES
#include "wisp_sub85_inc.c"
/************************************************************************/
#endif /* WISP */
