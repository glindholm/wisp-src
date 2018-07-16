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

#include <stdio.h>
#include <string.h>

#include "idsistd.h"
#include "cobrun.h"
#include "wglobals.h"
#include "wisplib.h"
#include "wexit.h"


static int	BYTESWAP = -1;			/* Is this a byte swap machine (-1 == uninitialized)				*/

void wswap(void *lword)				/* swap the order of the words in a longword item (for WANG routines to use)	*/
{
	int2 *swords;
	int2 temp;									/* used for the swap			*/

	if (bytenormal()) return;
	if (noswap_words) return;							/* Not supposed to do it.		*/

	if (acu_cobol) 
	{
		reversebytes(lword,4);
		return;
	}

	swords = (int2 *)lword;

	temp = swords[0];								/* get high order word			*/
	swords[0] = swords[1];								/* move low to high			*/
	swords[1] = temp;								/* move high to low			*/
}

void reversebytes(void *ptr, int len) 							/* Reverse the bytes.			*/
{
	char	temp[80];
	char   *lptr = (char*)ptr;
	
	memcpy(temp,lptr,len);

	while(len>0)
	{
		*lptr++ = temp[--len];
	}			
}	

int bytenormal(void)
{
	static int first = 1;
	static int normal;
	int4	l;
	char	*p;

	if (first)
	{
		first = 0;
		l = (int4) 0x01020304;
		p = (char *)&l;
		if      ( *p == (char) 0x01 )
		{
			normal = 1;
		}
		else if ( *p == (char) 0x04 )
		{
			normal = 0;
		}
		else
		{
			printf( "\n\r UNKNOWN BYTE-ORDER <%8.8x> = <%2.2x> <%2.2x> <%2.2x> <%2.2x> \n\r",
				l, p[0], p[1], p[2], p[3] );
			wexit(1L);
		}
	}

	return( normal );
}

int4 get_swap(int4 *src)
{
	int4	temp;

	memcpy((char*)&temp, (char*)src, 4);
	wswap(&temp);

	return temp;
}

void put_swap(int4 *dest, int4 value)
{
	wswap(&value);
	memcpy((char*)dest, (char*)&value, 4);
}


/*
**	History:
**	$Log: wswap.c,v $
**	Revision 1.13  1997-10-17 11:41:18-04  gsl
**	Add get_swap() and put_swap().
**	These are replacements for GETBIN()/PUTBIN() and swap used together.
**
**	Revision 1.12  1996-08-19 18:33:25-04  gsl
**	drcs update
**
**
**
*/
