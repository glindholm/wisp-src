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

/*

	wvaset(argcount)	This routine is called from unix before a "vararg" call to simualate the va_count on VMS.
				All it does is set WVACOUNT which is returned on the next va_count() call.

*/
#include "idsistd.h"

static int WVACOUNT;

void wvaset(x)
int4 *x;
{
	WVACOUNT = *x;
}

#ifndef VMS
#include <varargs.h>

int va_count(va_alist)
va_dcl
{
	return(WVACOUNT);
}
#endif

/*
**	History:
**	$Log: wvaset.c,v $
**	Revision 1.9  1996/08/19 22:33:26  gsl
**	drcs update
**	
**
**
*/
