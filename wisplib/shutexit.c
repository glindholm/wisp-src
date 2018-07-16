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
	shutexitcobol:	This is the "default" version of shutexitcobol() just in case no defines one for the specific cobol.
			It is called from wexit() to shutdown cobol and exit().  This one will occur in WISPLIB so the real
			one needs to be linked in ahead of the lib.

			*** Don't add cobol specific code to this file. ***
*/

#include "idsistd.h"

void shutexitcobol(int exit_code)			
{
	exit(exit_code);
}

/*
**	History:
**	$Log: shutexit.c,v $
**	Revision 1.9  1996-08-19 18:32:56-04  gsl
**	drcs update
**
**
**
*/
