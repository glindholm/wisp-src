static char copyright[]="Copyright (c) 1995-1998 NeoMedia Technologies, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wispexit.c
**
**	Project:	WISP/LIB
**
**	RCS:		$Source:$
**
**	Purpose:	WISPEXIT()
*/


#include "wisplib.h"
#include "werrlog.h"

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
	wtrace_timestamp("WISPEXIT");
	wexith();							/* Do Exit handler stuff.				*/
}


/*
**	History:
**	$Log: wispexit.c,v $
**	Revision 1.12  1998/05/18 18:43:32  gsl
**	Add trace
**	
**	Revision 1.11  1996-08-26 20:09:42-04  gsl
**	Documented and moved pre-exit logic to wexith()
**
**	Revision 1.10  1996-08-19 15:33:18-07  gsl
**	drcs update
**
**
**
*/
