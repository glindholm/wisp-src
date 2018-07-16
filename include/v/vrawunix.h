/*
 * File:	vrawunix.h
 *
 * Author:	david s.a. stine
 *
 * Purpose:	This is the header file for the System V implementation
 *		of the vraw package.
 *
 * Notes:	The client of this package should not have to issue
 *		a call to vraw_getc() -- all calls to this routine
 *		should happen via the vraw_input() and vraw_check() calls.
 *		The access to the vraw_getc() routine is provided for low-level
 *		access to the single-character routine.
 *
 * Usage:	#include "vrawunix.h"
 *
 * Edits:
 * ----------------------------------------------------------------------------
 * 89.05.21	dsa	Created.
 * 89.06.28	dsa	Changed filename to vrawuinx.h. Made input routines
 *			return 'chars' rather than 'int's.
 * 89.07.01	dsa	Removed routine entry points that were not used
 *			by higher layers. Also removed all '_' characters
 *			from the entry point names.
 *
 */
#ifndef _vrawunix_
#define _vrawunix_

extern char vrawinput();		/* Get a character, blocking. */
extern char vrawcheck();		/* Get a character, non-blocking. */
extern int vrawexit();			/* Perform vraw-layer rundown. */
extern int vrawputc();			/* Raw output of a single character. */
extern int vrawprint();			/* Raw output of a C-string buffer. */
extern int vrawexit();			/* Cleanup from the raw layer. */
extern int vrawsigset();		/* Set interrupt signal function. */
extern int vrawerrno();			/* Get vraw layer error. */


#define DOLLAR '$'
#define OPENBR '<'
#define CLOSBR '>'

#define E_DOL  0
#define E_OPEN 1
#define E_CLOSE 2
#define E_DIG 3
#define E_ANY 4
#define PEVENTCNT 5

#ifdef VDBGPADST
char *evstr[]=
{
	"DOLLAR $",
	"OPEN   <",
	"CLOSE  >",
	"DIG  0-9",
	"ANY     ",
	NULL
};
#endif

#define S_NORMAL 0
#define S_GOTDOL 1
#define S_GOTOP 2
#define S_RNUMS 3
#define S_EMIT 4
#define S_ABORT 5
#define PSTCNT 6

#ifdef VDBGPADST
char *ststr[]=
{
	"NORMAL",
	"GOTDOL",
	"GOTOP ",
	"RNUMS ",
	"EMIT  ",
	"ABORT ",
	NULL
};
#endif

static int padstatetab[PEVENTCNT][PSTCNT] = 
{        /* NORMAL    GOTDOL   GOTOP     RNUMS      EMIT       ABORT   */
/*DOL*/ { S_GOTDOL,  S_ABORT, S_ABORT,   S_ABORT,   S_NORMAL,  S_NORMAL },  
/*OP */ { S_NORMAL,  S_GOTOP, S_ABORT,   S_ABORT,   S_NORMAL,  S_NORMAL },  
/*CLO*/ { S_NORMAL,  S_ABORT, S_ABORT,   S_EMIT,    S_NORMAL,  S_NORMAL },  
/*DIG*/ { S_NORMAL,  S_ABORT, S_RNUMS,   S_RNUMS,   S_NORMAL,  S_NORMAL },  
/*ANY*/ { S_NORMAL,  S_ABORT, S_ABORT,   S_ABORT,   S_NORMAL,  S_NORMAL }  
};

#endif

