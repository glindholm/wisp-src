#include "movebin.h"
#include "werrlog.h"

HEXUNPK( source, target, tlen )							/* WANG HEXUNPK routine.			*/
char	*source;
char	*target;
long	*tlen;
{
#define		ROUTINE		23500

	long	len;
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

HEXPACK( source, target, tlen )							/* WANG HEXPACK routine.			*/
char	*source;
char	*target;
long	*tlen;
{
#undef		ROUTINE
#define		ROUTINE		23000

	long	len, p1, p2;
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
