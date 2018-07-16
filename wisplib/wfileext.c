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

#include "idsistd.h"
#include "filext.h"

setwispfilext(wispfilext)								/* Init the complet string (39 chars)	*/
char	*wispfilext;
{
	int i, len;

	for(len=0;len<39;len++)								/* Figure out len			*/
	{
		if ( wispfilext[len] == '\0' || wispfilext[len] == ' ' ) break;
	}
	memcpy(WISPFILEXT,wispfilext,len);
	for (i = len; i < 39; i++) WISPFILEXT[i] = ' ';					/* Pad with spaces.			*/
}
getwfilext(ptr)
char *ptr;
{
	memcpy(ptr,WISPFILEXT,39);
}
setwfilext(ptr)
char *ptr;
{
	memcpy(WISPFILEXT,ptr,39);
}

