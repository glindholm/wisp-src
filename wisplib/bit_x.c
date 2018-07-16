/*
	bit_x.c		A collection of bit handling routines.
*/

#include <string.h>
#ifndef VMS	/* unix or MSDOS */
#include <memory.h>
#endif

#include "idsistd.h"
#include "cobrun.h"

wsetstat(smask,cmask,src)							/* Used to set/clear bits in a status field.	*/
uint4 *smask,*cmask,*src;
{
	*src = *src | *smask;							/* Set bits with smask.				*/
	*src = *src & (~(*cmask));						/* Clear bits with cmask.			*/
}

										/* turn off the bits in src represented by mask	*/

bit_off(mask,src)
unsigned char *mask,*src;
{
	*src = *src & (~(*mask));						/* complement the mask and AND it with the src	*/
}
lbit_off(mask,src)
uint4 *mask,*src;
{
	*src = *src & (~(*mask));						/* complement the mask and AND it with the src	*/
}


										/* turn on the bits in src represented by mask	*/

bit_on(mask,src)
unsigned char *mask,*src;
{
	*src = *src | *mask;							/* OR the mask with the src			*/
}
lbit_on(mask,src)
uint4 *mask,*src;
{
	*src = *src | *mask;							/* OR the mask with the src			*/
}

							/* test the bits in src with those in mask and give result of Y or N */

bit_test(mask,src,value)
unsigned char *mask,*src,*value;
{
	if (*src & *mask)				/* AND the mask with the src						*/
	{
		*value = 'Y';				/* bits did match							*/
	}
	else
	{
		*value = 'N';				/* bits did not match							*/
	}
}
lbit_test(mask,src,value)
unsigned char *value;
uint4 *mask, *src;
{
	if (*src & *mask)				/* AND the mask with the src						*/
	{
		*value = 'Y';				/* bits did match							*/
	}
	else
	{
		*value = 'N';				/* bits did not match							*/
	}
}



/* convert a PIC 9(2) to a PIC X(1)												*/

xx2byte(src,dst)
char *src;
char *dst;
{
	*dst = (char)(10 * (src[0] - '0')) + (src[1] - '0');
}


/* convert a word with the a row and col in it to 2 words of row and col							*/

w2rowcol(mybytes,mywords)
unsigned char mybytes[];
short    int  mywords[];
{
									/* This will work with any byte order.			*/
									/* The max size of mybyte is 80 or 0x50 so we don't 	*/
									/* need to worry about sign extention.			*/
	mywords[0] = mybytes[0];
	mywords[1] = mybytes[1];
	if (acu_cobol && !bytenormal()) 
	{
		reversebytes((char *)&mywords[0],2);
		reversebytes((char *)&mywords[1],2);
	}
}

wmemcpy(dst,src,len)							/* COBOL call able memcpy				*/
char	*src, *dst;
short	*len;
{
	memcpy(dst,src,*len);
}





