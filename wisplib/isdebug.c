#ifdef Obsolete
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		isdebug.c
**
**	Purpose:	To determine if running with a COBOL debugger.
**
**	Routines:	
**	isdebug()		Report if a COBOL debugger is running.
**	set_isdebug()		Determine if a COBOL debugger is running.
**	set_debug_true()	Force the debug flag to be true.
**	set_debug_false()	Force the debug flag to br false.
**
*/

#ifdef unix
#include <stdio.h>
#include <termio.h>

#include "idsistd.h"
#include "cobrun.h"

static int	is_debug = -1;			/* Default to no debugger running. */

/*
**	Routine:	isdebug()
**
**	Function:	To report if we are running with a COBOL debugger.
**
**	Description:	Return the value set by set_isdebug().
**			This can be overridden by the enc var WISPISDEBUG=ON/OFF.
**			This was added as a safety measure to give control incase
**			the set_isdebug() returned the wrong value.
**
**	Arguments:	None
**
**	Globals:
**	is_debug	Flag if a debugger is running.
**
**	Return:	
**	1 (true)	A debugger is running
**	0 (false)	Debugger is not running
**
**	Warnings:	See set_isdebug().
**			If set_isdebug() is not called then it defaults to no debugger.
**
**	History:	
**	04/20/93	Written by GSL
**	06/17/93	Added the WISPISDEBUG override logic. GSL
**
*/
int isdebug()
{
	static	int	first = 1;
	static	int	override = 0;
	char	*ptr;

	if (first)
	{
		first = 0;
		if (ptr = getenv("WISPISDEBUG"))
		{
			if (0==strcmp(ptr,"off") || 0==strcmp(ptr,"OFF"))
			{
				override = 2;
			}
			else if (0==strcmp(ptr,"on") || 0==strcmp(ptr,"ON"))
			{
				override = 1;
			}
		}
	}

	if (override)
	{
		return( (1==override) ? 1: 0);
	}

	return ((is_debug== -1) ? 0 : is_debug);
}

/*
**	Routine:	set_isdebug()
**
**	Function:	To determine if a COBOL debugger is running.
**
**	Description:	This routine determines if a COBOL debugger is running.
**			It must only be called once and from initwisp() before any video actions are done.
**			It checks the TTY to see what mode it is in, and if it is in a "raw" mode (-icanon)
**			then it assumes a COBOL debugger (MF animator or ACUCOBOL debugger) is already
**			active.
**			For Micro Focus it also checks if COBSW contains "+A" because the above doesn't
**			work if the first program of a linklevel was not compiled for debug.
**
**	Arguments:	None
**
**	Globals:
**	is_debug	Flag if a COBOL debugger is running.
**
**	Return:		None
**
**	Warnings:	This must be called before the first call to any video routine as video
**			will put the TTY into raw mode.
**
**			If for some unbeknownst reason the ICANON is not set and a COBOL debugger is
**			not running then it will incorrectly set is_debug.
**
**
**	History:	
**	04/20/93	Written by GSL
**	04/30/93	Added MF test of COBSW for +A. GSL
**
*/
void set_isdebug()
{
	struct termio debug_termio;						/* The termio struct for debugger. 		*/

	if (is_debug != -1)
	  return;
	
	is_debug = 0;								/* Default to no COBOL debugger			*/
	if (-1 != ioctl(fileno(stdin), TCGETA, &debug_termio))			/* Get terminal info				*/
	{
		if (!(debug_termio.c_lflag & ICANON))				/* No ICANON == raw == debugger 		*/
		{
			is_debug = 1;
		}
	}

	if (mf_cobol)								/* For Micro Focus check the COBSW for "+A"	*/
	{
		char	*ptr;

		if (ptr = getenv("COBSW"))
		{
			if (strpos(ptr,"+A") >= 0)
			{
				is_debug = 1;
			}
		}
	}
	return;
}

/*
**	Routine:	set_debug_true()
**
**	Function:	To set the debug flag true
**
**	Description:	This routine is called at startup time from exam_args() (in ACUCOBOL sub85.c).
**                      exam_args looks for the -d flag to determine if the debugger is running. This routine
**                      sets the flag to say debug is active.  No further checks are needed.
**
**	Arguments:	None
**
**	Globals:
**	is_debug	Flag if a COBOL debugger is running.
**
**	Return:		None
**
**	Warnings:	This must be called before initwisp or set_isdebug.  set_isdebug may incorrectly
**                      decide that we are in debug if the -b flag is not being used.  Once this routine
**                      sets the flag, set_isdebug will not attempt to.
**
**	History:	
**	05/28/93	Written by JEC.
**
*/
void set_debug_true()
{
	is_debug = 1;
	return;
}

/*
**	Routine:	set_debug_false()
**
**	Function:	To set the debug flag false
**
**	Description:	See description for set_debug_true().  This routine is called if exam_args()
**                      does not find a -d flag
**
**/
void set_debug_false()
{
	is_debug = 0;
	return;
}



#endif /* unix */

#endif /* Obsolete */
