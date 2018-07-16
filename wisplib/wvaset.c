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

int wvaset(x)
int4 *x;
{
	WVACOUNT = (int4)*x;
}

#ifndef VMS
#include <varargs.h>

int va_count(va_alist)
va_dcl
{
	return(WVACOUNT);
}
#endif

