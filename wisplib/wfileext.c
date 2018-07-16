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
#include "idsisubs.h"

static int	initialized = 0;
static char	WISPFILEXT[WISP_FILE_EXT_SIZE];						/* Define the file extension variable.	*/

void WSETFILEXT(const char* wispfilext)	/* setwfilext() setwispfilext() */		/* Init the complet string (39 chars)	*/
{
	int i, len;

	initialized = 1;

	for(len=0;len<WISP_FILE_EXT_SIZE;len++)						/* Figure out len			*/
	{
		if ( wispfilext[len] == '\0' || wispfilext[len] == ' ' ) break;
	}
	memcpy(WISPFILEXT,wispfilext,len);
	for (i = len; i < WISP_FILE_EXT_SIZE; i++) WISPFILEXT[i] = ' ';			/* Pad with spaces.			*/
}
void WGETFILEXT(char* ptr)	/* getwfilext() */
{
	if (!initialized)
	{
		WSETFILEXT(" ");
	}
	memcpy(ptr,WISPFILEXT,WISP_FILE_EXT_SIZE);
}

/*
** Old interfaces
*/
void setwfilext(const char* wispfilext)
{
	WSETFILEXT(wispfilext);
}
void setwispfilext(const char* wispfilext)
{
	WSETFILEXT(wispfilext);
}
void getwfilext(char* ptr)
{
	 WGETFILEXT(ptr);
}

/*
**	History:
**	$Log: wfileext.c,v $
**	Revision 1.9.2.1  2002/11/14 21:12:27  gsl
**	Replace WISPFILEXT and WISPRETURNCODE with set/get calls
**	
**	Revision 1.9  1996/08/19 22:33:15  gsl
**	drcs update
**	
**
**
*/
