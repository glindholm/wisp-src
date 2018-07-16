			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#include <stdio.h>
#include "idsistd.h"
#include "cobrun.h"
#include "wglobals.h"


int	BYTESWAP = -1;				/* Is this a byte swap machine (-1 == uninitialized)				*/

wswap(lword)					/* swap the order of the words in a longword item (for WANG routines to use)	*/
int4 *lword;									/* Argument must be pointer to int4	*/
{
	int2 *swords;
	int2 temp;									/* used for the swap			*/

	swords = (int2 *)lword;

	if (bytenormal()) return(0);
	if (noswap_words) return(0);							/* Not supposed to do it.		*/

	if (acu_cobol) 
	{
		reversebytes(swords,4);
		return(0);
	}

	temp = swords[0];								/* get high order word			*/
	swords[0] = swords[1];								/* move low to high			*/
	swords[1] = temp;								/* move high to low			*/

}

reversebytes(ptr,len)									/* Reverse the bytes.			*/
char	*ptr;										/* Char array; really short or int4.	*/
int	len;
{
	char	temp[80];

	memcpy(temp,ptr,len);

	while(len>0)
	{
		*ptr++ = temp[--len];
	}			
}

#ifdef OLD
wbyteorder()
{
	int4	l;
	char	*p;

	if (BYTESWAP == -1)
	{
		l = (int4) 0x01020304;
		p = (char *)&l;
		if      ( *p == (char) 0x01 )
		{
			BYTESWAP = 0;
		}
		else if ( *p == (char) 0x04 )
		{
			BYTESWAP = 1;
		}
		else
		{
			printf( "\n\r UNKNOWN BYTE-ORDER <%8.8x> = <%2.2x> <%2.2x> <%2.2x> <%2.2x> \n\r",
				l, p[0], p[1], p[2], p[3] );
			wexit(1L);
		}
	}

	return (BYTESWAP);		
}
#endif
	

int bytenormal()
{
	static int first = 1;
	static int normal;
	int4	l;
	char	*p;
	char	messstr[80];

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
