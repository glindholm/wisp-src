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
**	wfileext.c
*/

#include <string.h>

#include "idsistd.h"
#include "filext.h"

void setwispfilext(char* wispfilext)							/* Init the complet string (39 chars)	*/
{
	int i, len;

	for(len=0;len<39;len++)								/* Figure out len			*/
	{
		if ( wispfilext[len] == '\0' || wispfilext[len] == ' ' ) break;
	}
	memcpy(WISPFILEXT,wispfilext,len);
	for (i = len; i < 39; i++) WISPFILEXT[i] = ' ';					/* Pad with spaces.			*/
}

void getwfilext(char* ptr)
{
	memcpy(ptr,WISPFILEXT,39);
}

void setwfilext(char* ptr)
{
	memcpy(WISPFILEXT,ptr,39);
}

/*
**	History:
**	$Log: wfileext.c,v $
**	Revision 1.9  1996-08-19 18:33:15-04  gsl
**	drcs update
**
**
**
*/
