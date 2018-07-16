static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#include "wisplib.h"

/*
**	ROUTINE:	WISPEXIT()
**
**	FUNCTION:	Preform Pre-exit logic
**
**	DESCRIPTION:	Called before STOP RUN or highest EXIT PROGRAM for unix.
**			Do the exit handler stuff.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void WISPEXIT(void)
{
	wexith();							/* Do Exit handler stuff.				*/
}


/*
**	History:
**	$Log: wispexit.c,v $
**	Revision 1.11  1996-08-26 20:09:42-04  gsl
**	Documented and moved pre-exit logic to wexith()
**
**	Revision 1.10  1996-08-19 15:33:18-07  gsl
**	drcs update
**
**
**
*/
