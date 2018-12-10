/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/

/*
**	File:		hexunpk.c
**
**	Project:	WISPLIB
**
**	Purpose:	vssubs HEXPACK and HEXUNPK
**
**	Routines:	
**	HEXUNPK()	Unpacks each character in a string into two hex chars
**	HEXPACK()	Packs each pair of hex chars into a single char
*/

/*
**	Includes
*/

#include <string.h>

#include "idsistd.h"
#include "werrlog.h"
#include "wisplib.h"
#include "hexunpk.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/


/*
**	ROUTINE:	HEXUNPK()
**
**	FUNCTION:	Unpacks each character in a string into two hex chars
**
**	DESCRIPTION:	Each char is converted into 2 chars. 
**			Example: "ABC" would become "414243".
**
**	ARGUMENTS:	
**	source		The source string of characters to unpack.
**	target		The resulting string of hex characters.
**	tlen		The length of the source string.
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	Target must be twice as long as source.
**
*/
void HEXUNPK( char* source,  char* target, int4 *tlen )
{
	int4	len;
	char	mask[17];
	char	*src, *trg;

	WL_wtrace("HEXUNPK","ENTRY","Entry into HEXUNPK");

	src = source;
	trg = target;
	strcpy(mask, "0123456789ABCDEF");

	len = WL_get_swap(tlen);

	while( len-- > 0 )
	{
		*trg++ = mask[ (*src  & 0xF0) >> 4 ];
		*trg++ = mask[ (*src++ & 0x0F) ];
	}
}

/*
**	ROUTINE:	HEXPACK()
**
**	FUNCTION:	Packs each pair of hex chars into a single char.
**
**	DESCRIPTION:	Each two hex chars is converted into a single chars. 
**			Example: "414243" would become "ABC".
**
**	ARGUMENTS:	
**	source		The source string of pairs of hex characters to pack.
**	target		The resulting string of packed characters.
**	tlen		The length of the source string.
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	Target must be half as long as source.
**			If source in odd length then the last char is ignored.
**			An invalid char in source is treated as '0'.
**
*/
void HEXPACK(  char* source,  char* target, int4* tlen )
{
	int4	len, p1, p2;
	char	mask[17], mask2[17];
	char	s[2], *src, *trg;

	WL_wtrace("HEXPACK","ENTRY","Entry into HEXPACK");

	strcpy(mask,  "0123456789ABCDEF" );					/* Source may be upper or lowercase.		*/
	strcpy(mask2, "0123456789abcdef" );
	s[1] = 0;								/* Pre-Null terminate 1 char string.		*/

	len = WL_get_swap(tlen);

	src = source;
	trg = target;

	if ( len % 2 == 1 ) len--;						/* If len is odd ignore last byte.		*/
	
	while( len > 0 )
	{
		*s = *src++;
		p1 = strcspn( mask, s);						/* Translate first char.			*/
		if ( p1 < 0 || p1 > 15 ) p1 = strcspn( mask2, s );
		if ( p1 < 0 || p1 > 15 ) p1 = 0;				/* If invalid then set to 0.			*/
		p1 = p1 << 4;							/* Shift to high nibble.			*/

		*s = *src++;
		p2 = strcspn( mask, s);						/* Translate 2nd char.				*/
		if ( p2 < 0 || p2 > 15 ) p2 = strcspn( mask2, s );
		if ( p2 < 0 || p2 > 15 ) p2 = 0;
		p1 = p1 | p2;							/* Or the pair together.			*/
		*trg++ = p1;
		len -= 2;
	}
}
/*
**	History:
**	$Log: hexunpk.c,v $
**	Revision 1.13  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.12  2002/12/09 21:09:28  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.11  2002/07/12 17:00:56  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.10  1997/02/20 14:39:20  gsl
**	Document and add header file
**	
**	Revision 1.9  1996-08-19 18:32:23-04  gsl
**	drcs update
**
**
**
*/
