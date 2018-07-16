			/************************************************************************/
			/*	               VIDEO VIEW Screen Management.			*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Header file references.								*/

#include "video.h"
#include <stdio.h>
#ifdef unix
#include <termio.h>
#endif

/*						      Local data.								*/

static int is_debug = -1;								/* Default to no debugger running.	*/
void set_isdebug();									/* Determine if debugger is running.	*/
char *getenv();										/* Define getenv as a char pointer.	*/

/*					Report if we are running with a debugger.						*/

int isdebug()										/* Report if a debugger is running.	*/
{
	static int first = TRUE;							/* Set the first time control flag.	*/
	static int override = 0;							/* Assume not in override mode.		*/
	char *ptr;									/* Working pointer.			*/
	int i;										/* Working integer.			*/

	if (first)									/* Is this the first time in?		*/
	{
		first = FALSE;								/* No longer the first time.		*/
		set_isdebug();								/* Load the debugger state.		*/
#ifdef unix
		if (ptr = getenv("DEBUGACTIVE"))					/* Is the override flag set.		*/
		{
			if (0==strcmp(ptr,"off") || 0==strcmp(ptr,"OFF")) override = 2;			/* Force it off?	*/
			else if (0==strcmp(ptr,"on") || 0==strcmp(ptr,"ON")) override = 1;		/* Force it on?		*/
		}
#endif
	}

	if (override) return((1==override) ? 1: 0);					/* Is there an override in effect?	*/
	return ((is_debug== -1) ? 0 : is_debug);					/* No, then return the flag value.	*/
}

void set_isdebug()									/* Determine if debugger is running.	*/
{
	char *ptr;									/* Working pointer.			*/

	if (is_debug != -1) return;							/* Already set, then return.		*/
	
	is_debug = 0;									/* Default to no COBOL debugger.	*/

#ifdef unix
	if (ptr = getenv("COBSW"))							/* Is Microfocus COBOL running.		*/
	{
		if (strpos(ptr,"+A") >= 0) is_debug = 1;				/* Is there an animate switch?		*/
	}
#endif
	return;
}

void set_debug_true()									/* Set the debugging control flag on.	*/
{
	is_debug = TRUE;								/* Set the flag.			*/
	return;
}

void set_debug_false()									/* Set the control flag off.		*/
{
	is_debug = FALSE;								/* Clear the flag.			*/
	return;
}
