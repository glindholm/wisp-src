static char copyright[]="Copyright (c) 1995-1997 NeoMedia Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		hexunpk.c
**
**	Project:	WISPLIB
**
**	RCS:		$Source:$
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
#include "movebin.h"
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
#define		ROUTINE		23500

	int4	len;
	char	mask[17];
	char	*src, *trg;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	src = source;
	trg = target;
	strcpy(mask, "0123456789ABCDEF");

	GETBIN(&len,tlen,4);

	wswap(&len);

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
#undef		ROUTINE
#define		ROUTINE		23000

	int4	len, p1, p2;
	char	mask[17], mask2[17];
	char	s[2], *src, *trg;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	strcpy(mask,  "0123456789ABCDEF" );					/* Source may be upper or lowercase.		*/
	strcpy(mask2, "0123456789abcdef" );
	s[1] = 0;								/* Pre-Null terminate 1 char string.		*/

	GETBIN(&len,tlen,4);

	wswap(&len);

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
**	Revision 1.10  1997-02-20 09:39:20-05  gsl
**	Document and add header file
**
**	Revision 1.9  1996-08-19 18:32:23-04  gsl
**	drcs update
**
**
**
*/
