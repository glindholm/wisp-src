static char copyright[]="Copyright (c) 1989-2002 NeoMedia Technologies Inc., All rights reserved.";
static char rcsid[]="$Id:$";

#include <stdio.h>
#include <string.h>

#include "idsistd.h"
#include "cobrun.h"
#include "wglobals.h"
#include "wisplib.h"
#include "wexit.h"


void wswap(void *lword)				/* swap the order of the words in a longword item (for WANG routines to use)	*/
{
	int2 *swords;
	int2 temp;									/* used for the swap			*/

	if (bytenormal()) return;
	if (noswap_words) return;							/* Not supposed to do it.		*/

	if (acu_cobol) 
	{
		/*
		**	Acucobol keeps all binaries in big-endian order so 
		**	simply reverse to make into little-endian.
		**
		**	Note: a 2+2 will come thru sub85.c frontend() unchanged because type is char.
		*/
		reversebytes(lword,4);
	}
	else
	{
		swords = (int2 *)lword;

		temp = swords[0];							/* get high order word			*/
		swords[0] = swords[1];							/* move low to high			*/
		swords[1] = temp;							/* move high to low			*/
	}
	
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

int4 get_swap(const int4 *src)
{
	int4	temp;

	memcpy((char*)&temp, (const char*)src, 4);
	wswap(&temp);

	return temp;
}

void put_swap(int4 *dest, int4 value)
{
	wswap(&value);
	memcpy((char*)dest, (char*)&value, 4);
}


/*
**	ROUTINE:	WBB2B4()
**
**	FUNCTION:	Convert a Wang Style int(2)+int(2) to int(4)
**
**	DESCRIPTION:	On a bytenormal machines do nothing.
**
**			On Acucobol all binary fields are stored in
**			big-endian order so do nothing.
**			
**			On a little-endian machines with MF:
**
**			0x01020304 will be stored as 02,01,04,03
**			flip the half words to be    04,03,02,01
**
**	ARGUMENTS:	
**	bb		Wang Style 2*int(2)
**			01 BB.
**			   05 B1 BINARY.
**			   05 B2 BINARY.
**
*/
void WBB2B4(char* bb)
{
	if (!bytenormal() && !acu_cobol)
	{
		char b4[4];
		
		b4[0] = bb[2];
		b4[1] = bb[3];
		b4[2] = bb[0];
		b4[3] = bb[1];

		memcpy(bb, b4, 4);
	}
}


/*
**	History:
**	$Log: wswap.c,v $
**	Revision 1.15  2002-03-28 15:43:14-05  gsl
**	Add WBB2B4 to do a wswap() from cobol
**
**	Revision 1.14  1998-11-04 10:04:49-05  gsl
**	change get_swap() arg to const
**
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
