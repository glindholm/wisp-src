/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/

/*
**	File:		wispexit.c
**
**	Project:	WISP/LIB
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
	WL_wtrace_timestamp("WISPEXIT");
	WL_wexith();							/* Do Exit handler stuff.				*/
}


/*
**	History:
**	$Log: wispexit.c,v $
**	Revision 1.15  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.14  2002/07/19 22:07:14  gsl
**	Renaming cobol api routines for global uniqueness
**	
**	Revision 1.13  2002/07/10 21:05:34  gsl
**	Fix globals WL_ to make unique
**	
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
