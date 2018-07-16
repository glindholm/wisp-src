/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/


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
**	History:
**	$Log: wfileext.c,v $
**	Revision 1.14  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.13  2002/10/15 17:58:44  gsl
**	Ensure file extension is initialized
**	
**	Revision 1.12  2002/07/29 15:46:49  gsl
**	getwfilext -> WGETFILEXT
**	setwfilext -> WSETFILEXT
**	setwispfilext -> WSETFILEXT
**	
**	Revision 1.11  2002/07/12 17:01:03  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.10  2002/06/25 17:46:05  gsl
**	Remove WISPFILEXT as a global, now must go thru set/get routines
**	
**	Revision 1.9  1996/08/19 22:33:15  gsl
**	drcs update
**	
**
**
*/
