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

shutexitcobol(exit_code)			
int exit_code;
{
	exit(exit_code);
}

