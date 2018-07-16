static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/* UPPER ... convert a string from lower case to upper case									*/

#include <ctype.h>
#include "idsistd.h"
#include "movebin.h"
#include "wisplib.h"

void UPPER(char *line, int4 *len)
{
	int 	i;
	int4	length;

	GETBIN(&length,len,4);
	wswap(&length);

	for (i=0; i < length; i++)
	{
		line[i] = toupper(line[i]);
	}
}
/*
**	History:
**	$Log: upper.c,v $
**	Revision 1.10  1996/08/19 22:33:02  gsl
**	drcs update
**	
**
**
*/
