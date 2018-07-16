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


#include <stdio.h>
#include <string.h>

#include "idsistd.h"
#include "cobrun.h"
#include "wglobals.h"
#include "wisplib.h"
#include "wexit.h"

/*
**	Routine:	WL_wswap()
**
**	Function:	swap the order of the half-words in a long-word item (for WANG routines to use)
**
**	Description:	Wang COBOL-74 used 2 x 2-byte Integer (BINARY) fields 
**			in order to make up a 4-byte Integer field.
**
**			01 MY-4-BYTE-INT.
**			   05  HIGH-2-BYTES  USAGE IS BINARY.
**			   05  LOW-2-BYTES   USAGE IS BINARY.
**			
**			On Little-Endian machines the bytes within the word 
**			need to be rearranged to form a native 32-bit integer.
**			On Big-Endian machines this routine does nothing.
**
**	Arguments:
**	lword		A Wang COBOL INT(4) field. 
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
*/
void WSWAP(void *lword)
{
	WL_wswap(lword);
}

void WL_wswap(void *lword)
{
	int2 *swords;
	int2 temp;									/* used for the swap			*/

	if (WL_bytenormal()) return;
	if (wisp_get_noswap()) return;							/* Not supposed to do it.		*/

	if (wisp_acu_cobol()) 
	{
		/*
		**	Acucobol keeps all binaries in big-endian order so 
		**	simply reverse to make into little-endian.
		**
		**	Note: a 2+2 will come thru sub85.c frontend() unchanged because type is char.
		*/
		WL_reversebytes(lword,4);
	}
	else
	{
		swords = (int2 *)lword;

		temp = swords[0];							/* get high order word			*/
		swords[0] = swords[1];							/* move low to high			*/
		swords[1] = temp;							/* move high to low			*/
	}
	
}

void WL_reversebytes(void *ptr, int len) 							/* Reverse the bytes.			*/
{
	char	temp[80];
	char   *lptr = (char*)ptr;
	
	memcpy(temp,lptr,len);

	while(len>0)
	{
		*lptr++ = temp[--len];
	}			
}	

int WL_bytenormal(void)
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

int4 WL_get_swap(const int4 *src)		/* Replacement for GETBIN(); wswap() */
{
	int4	temp;

	memcpy((char*)&temp, (const char*)src, 4);
	WL_wswap(&temp);

	return temp;
}

void WL_put_swap(void *dest, int4 value)	/* Replacement for wswap(); PUTBIN() */
{
	WL_wswap(&value);
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
	if (!WL_bytenormal() && !wisp_acu_cobol())
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
**	Revision 1.22  2003/07/11 16:58:31  gsl
**	Add WSWAP() as a replacement for wswap()
**	
**	Revision 1.21  2003/04/21 14:39:54  gsl
**	comments
**	
**	Revision 1.20  2003/01/31 19:08:36  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.19  2002/07/12 17:01:07  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.18  2002/07/10 21:05:38  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.17  2002/07/02 04:00:36  gsl
**	change acu_cobol and mf_cobol to wisp_acu_cobol() and wisp_mf_cobol()
**	
**	Revision 1.16  2002/07/01 04:02:45  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.15  2002/03/28 20:43:14  gsl
**	Add WBB2B4 to do a wswap() from cobol
**	
**	Revision 1.14  1998-11-04 10:04:49-05  gsl
**	change WL_get_swap() arg to const
**
**	Revision 1.13  1997-10-17 11:41:18-04  gsl
**	Add WL_get_swap() and WL_put_swap().
**	These are replacements for GETBIN()/PUTBIN() and swap used together.
**
**	Revision 1.12  1996-08-19 18:33:25-04  gsl
**	drcs update
**
**
**
*/
