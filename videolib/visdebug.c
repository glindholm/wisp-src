static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	               VIDEO VIEW Screen Management.			*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Header file references.								*/


#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "video.h"

/*						      Local data.								*/

static int is_debug = -1;								/* Default to no debugger running.	*/
static int is_sharedscreen = 0;

/*					Report if we are running with a debugger.						*/

int isdebug(void)									/* Report if a debugger is running.	*/
{
	static int first = TRUE;							/* Set the first time control flag.	*/
	static int override = 0;							/* Assume not in override mode.		*/
	char *ptr;									/* Working pointer.			*/

	if (first)									/* Is this the first time in?		*/
	{
		first = FALSE;								/* No longer the first time.		*/
		set_isdebug();								/* Load the debugger state.		*/

		if (ptr = getenv("DEBUGACTIVE"))					/* Is the override flag set.		*/
		{
			if (0==strcmp(ptr,"off") || 0==strcmp(ptr,"OFF")) override = 2;			/* Force it off?	*/
			else if (0==strcmp(ptr,"on") || 0==strcmp(ptr,"ON")) override = 1;		/* Force it on?		*/
		}
	}

	if (override) return((1==override) ? 1: 0);					/* Is there an override in effect?	*/
	return ((is_debug== -1) ? 0 : is_debug);					/* No, then return the flag value.	*/
}

void set_isdebug(void)									/* Determine if debugger is running.	*/
{
	char *ptr;									/* Working pointer.			*/

	if (is_debug != -1) return;							/* Already set, then return.		*/
	
	is_debug = 0;									/* Default to no COBOL debugger.	*/

	if (ptr = getenv("COBSW"))							/* Is Microfocus COBOL running.		*/
	{
		if (strstr(ptr,"+A") != NULL) is_debug = 1;				/* Is there an animate switch?		*/
	}
}

void set_isdebug_true(void)								/* Set the debugging control flag on.	*/
{
	is_debug = TRUE;								/* Set the flag.			*/
	return;
}

void set_isdebug_false(void)								/* Set the control flag off.		*/
{
	is_debug = FALSE;								/* Clear the flag.			*/
	return;
}

/*
**	ROUTINE:	vsharedscreen()
**
**	FUNCTION:	Report if screen is being shared.
**
**	DESCRIPTION:	The screen is shared when the debugger is running or with native screens.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		
**	1		Screen is shared by VIDEO and something else.
**	0		Screen is only control by VIDEO
**
**	WARNINGS:	None
**
*/
int vsharedscreen(void)
{
	if (isdebug())
	{
		return 1;
	}
	else
	{
		return is_sharedscreen;
	}
}
void set_vsharedscreen_true(void)
{
	is_sharedscreen = 1;
}


/*
**	History:
**	$Log: visdebug.c,v $
**	Revision 1.9  1997-09-22 12:28:16-04  gsl
**	Change the set_debug routines to set_isdebug for consistent naming.
**	Add vsharedscreen() routines
**
**	Revision 1.8  1997-07-08 17:02:51-04  gsl
**	Generalize so works on WIN32
**
**	Revision 1.7  1997-01-08 16:38:24-05  gsl
**	Change strpos() to strstr()
**
**	Revision 1.6  1996-10-11 15:16:06-07  gsl
**	drcs update
**
**
**
*/
