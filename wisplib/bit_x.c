static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	bit_x.c		A collection of bit handling routines.
*/

#include <string.h>

#include "idsistd.h"
#include "cobrun.h"
#include "wisplib.h"

void wsetstat(uint4 *smask, uint4 *cmask, uint4 *src)							/* Used to set/clear bits in a status field.	*/
{
	*src = *src | *smask;							/* Set bits with smask.				*/
	*src = *src & (~(*cmask));						/* Clear bits with cmask.			*/
}

										/* turn off the bits in src represented by mask	*/

void bit_off(unsigned char *mask, unsigned char *src)
{
	*src = *src & (~(*mask));						/* complement the mask and AND it with the src	*/
}
void lbit_off(uint4 *mask, uint4 *src)
{
	*src = *src & (~(*mask));						/* complement the mask and AND it with the src	*/
}


										/* turn on the bits in src represented by mask	*/

void bit_on(unsigned char *mask,unsigned char *src)
{
	*src = *src | *mask;							/* OR the mask with the src			*/
}
void lbit_on(uint4 *mask,uint4 *src)
{
	*src = *src | *mask;							/* OR the mask with the src			*/
}


/*
**	Routine:	bit_test()
**
**	Function:	Test if bits in mask are all on in src.
**
**	Description:	Test if all the bits that are set on in mask are
**			also on in src, if so set value to 'Y'.
**
**	Arguments:
**	mask		The test bit mask.
**	src		The field to test against mask.
**	value		The return field
**			'Y' = All the bits in mask are on
**			'N' = The mask is not on
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	12/13/93	Fixed to work correct if multiple bits are set in mask. GSL
**
*/
void bit_test(mask,src,value)
unsigned char *mask,*src;
char	*value;
{
	*value = ((*src & *mask) == *mask) ? 'Y':'N';
}

void lbit_test(mask,src,value)
char 	*value;
uint4 	*mask, *src;
{
	*value = ((*src & *mask) == *mask) ? 'Y':'N';
}



/* convert a PIC 9(2) to a PIC X(1)												*/

void xx2byte(char *src,char *dst)
{
	*dst = (char)(10 * (src[0] - '0')) + (src[1] - '0');
}


/* convert a word with the a row and col in it to 2 words of row and col							*/

void w2rowcol(unsigned char mybytes[],short int mywords[])
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

void wmemcpy(char *dst,char *src,short *len)							/* COBOL call able memcpy				*/
{
	memcpy(dst,src,*len);
}

/*
**	History:
**	$Log: bit_x.c,v $
**	Revision 1.10  1996-08-19 18:32:09-04  gsl
**	drcs update
**
**
**
*/
