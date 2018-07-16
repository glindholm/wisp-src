			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		acustubs.c
**
**	Purpose:	To hold stub routines that are normally found in the ACUCOBOL rts.
**			This is to allow linking to resolve undefines that are only used
**			when then ACUCOBOL rts is included. 
**			I.e. It is used to build utilities that don't include runcbl.a or sub85.c etc.
**
**	Routines:	
**	call_acucobol()		Stub used in LINK normally found in sub85.c
**
**
**	History:
**	12/28/92	Written by GSL
**
*/

#ifdef DACU
#include "idsistd.h"

void call_acucobol()
{
	werrlog(102,"STUB: call_acucobol()",0,0,0,0,0,0,0);
	return;
}
#else /* DACU */
static int dummy_acustub;  /* to prevent compiler warning */
#endif
