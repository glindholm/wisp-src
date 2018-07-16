			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/*
**	initglbs.c
*/

#include "idsistd.h"
#include "wglobals.h"
#include "filext.h"
#include "cobrun.h"

initglbs(wisprunname)									/* Init GLOBALS				*/
char	*wisprunname;
											/* Call this routine from NON-COBOL	*/
											/* utilities that use WISPLIB.		*/
{
	memcpy(WISPRUNNAME,wisprunname,8);
	setprogid(wisprunname);
	memset(WISPFILEXT,' ',39);							/* Set file extension to spaces.	*/
	memset(WISPRETURNCODE,'0',3);							/* Set RETURN-CODE to zero.		*/
	strcpy(WISPTRANVER,"INITGLBS");							/* Set the translator version		*/
	wpload();
}
