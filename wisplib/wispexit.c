			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	WISPEXIT	Called before STOP RUN or highest EXIT PROGRAM for unix.

			Do the exit handler stuff.
*/

#include "cobrun.h"

WISPEXIT()
{
#ifdef unix
	restoreprogdefs();						/* Do PROGLIB and PROGVOL cleanup			*/
#endif

#ifndef MSDOS
	if (acu_cobol) ACUPARGS();					/* Do LINK cleanup.					*/
	else           LINKPARG();
#endif

	wexith();							/* Do Exit handler stuff.				*/
	vexit();							/* Shutdown Video.					*/
}


