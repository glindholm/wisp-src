			/************************************************************************/
			/*									*/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*									*/
			/*			    Copyright (c) 1987				*/
			/*									*/
			/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
			/*									*/
			/************************************************************************/


/*						Include standard header files.							*/

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include local definitions.		*/


/*						Subroutine entry point.								*/

int vloadchar(cnum,bits)								/* Load a definable char into VT220.	*/
	char cnum;									/* The ascii chararacter to load.	*/
	char *bits;									/* The array of bits.			*/
{
	char outstr[80];								/* String to hold output.		*/
	char mask;									/* Decoding mask.			*/
	char sixel[18];									/* A place to put the SIXEL values.	*/
	register int i,j,k;								/* Working registers.			*/

	if ((cnum > '~') || (cnum < '!')) return(FAILURE);				/* Character not in range.		*/

	sixel[17] = 0;									/* Make sure sizel array is terminated.	*/
	mask = 1;									/* Set mask to first bit.		*/

	for (i = 7; i >= 0; i--)							/* For each output character.		*/
	{
		sixel[i] = '\0';							/* Initialize to zero.			*/
		for (j = 0; j < 6; j++)							/* Scan each input bit.			*/
		{
			if (bits[j] & mask)  sixel[i] |=  ('\01' << j);			/* Change bits to sixel representation.	*/
		}
		mask = mask << 1;							/* Shift the mask.			*/
		sixel[i] += '?';							/* Add in "zero" bias.			*/
	}

	mask = 1;									/* Reset mask to first bit.		*/
	sixel[8] = '/';
	for (i=16; i>8; i--)								/* For each output character.		*/
	{
		sixel[i] = '\0';							/* Initialize to zero.			*/
		for (j=0; j<4; j++)							/* Scan each input bit.			*/
		{
			if (bits[j+6] & mask)  sixel[i] |= ('\01' << j);		/* Change bits to sixel representation.	*/
		}
		mask = mask << 1;							/* Shift the mask.			*/
		sixel[i] += '?';							/* Bias into excess "?" format.		*/
	}

	sprintf(outstr,"\033P1;%d;1{ @%s\033\\",cnum-' ',sixel);			/* Encode into a string.		*/
	vcontrol(outstr);								/* Output the data.			*/
	return(SUCCESS);								/* Success is guaranteed.		*/
}
