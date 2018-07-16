/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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

/*
	bit_x.c		A collection of bit handling routines.

	bit_off		BIT_OFF		- SET figcon IN FAC OF xxx OFF, 
	bit_on		BIT_ON		- SET figcon IN FAC OF xxx ON, 
	bit_test	BIT_TEST	- Fac Altered/Protected & IF figcon ON/OFF
	lbit_off	LBIT_OFF	- Unused
	lbit_on		LBIT_ON		- Turn on DECLARATIVES bit

*/

#include <string.h>

#include "idsistd.h"
#include "cobrun.h"
#include "wisplib.h"
#include "scnfacs.h"

void WSETSTAT(const uint4 *smask, const uint4 *cmask, uint4 *src)		/* Used to set/clear bits in a status field.	*/
{
	*src = *src | *smask;							/* Set bits with smask.				*/
	*src = *src & (~(*cmask));						/* Clear bits with cmask.			*/
}

										/* turn off the bits in src represented by mask	*/

void BIT_OFF(const unsigned char *mask, unsigned char *src)	/* was bit_off() */
{
	*src = *src & (~(*mask));						/* complement the mask and AND it with the src	*/
}
void LBIT_OFF(const uint4 *mask, uint4 *src)			/* was lbit_off() */
{
	*src = *src & (~(*mask));						/* complement the mask and AND it with the src	*/
}


										/* turn on the bits in src represented by mask	*/

void BIT_ON(const unsigned char *mask, unsigned char *src)	/* was bit_on() */
{
	*src = *src | *mask;							/* OR the mask with the src			*/
}
void LBIT_ON(const uint4 *mask,uint4 *src)			/* was lbit_on() */
{
	*src = *src | *mask;							/* OR the mask with the src			*/
}

/*
**	Routine:	WSETFACBLINK()
**
**	Function:	Set the fac to BLINK rendition
**
**	Description:	Set to BLINK  xxx10xxx
**
**	Arguments:
**	fac		The FAC
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
*/
void WSETFACBLINK(unsigned char *fac)
{
	*fac = FAC_SET_BLINK(*fac);
}

/*
**	Routine:	BIT_TEST()
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
void BIT_TEST(const unsigned char *mask, const unsigned char *src, char *value) /* was bit_test() */
{
	*value = ((*src & *mask) == *mask) ? 'Y':'N';
}




/* 
**  convert a PIC 9(2) to a PIC X(1)
*/
void W99TOX(const char *src, char *dst) /* was xx2byte */
{
	*dst = (char)(10 * (src[0] - '0')) + (src[1] - '0');
}


/*  
**  WGETCURPOS()
**  w2rowcol()
**		convert a word with the a row and col in it to 2 words of row and col
**	arg1	- pointer to 3rd byte of order-area (col, row)
**	arg2	- pointer to cursor-position 
**			01  CURSOR-POSITION.
**			    05  CURSOR-COL  BINARY.
**			    05  CURSOR-ROL  BINARY.
**
*/
void WGETCURPOS(const unsigned char mybytes[],short int mywords[])
{
									/* This will work with any byte order.			*/
									/* The max size of mybyte is 80 or 0x50 so we don't 	*/
									/* need to worry about sign extention.			*/
	mywords[0] = mybytes[0]; /* col */
	mywords[1] = mybytes[1]; /* row */
	if (wisp_acu_cobol() && !WL_bytenormal()) 
	{
		WL_reversebytes((char *)&mywords[0],2);
		WL_reversebytes((char *)&mywords[1],2);
	}
}

void WMEMCPY(char *dst, const char *src, const short *len)		/* COBOL call able memcpy				*/
{
	memcpy(dst,src,*len);
}

/*
**	History:
**	$Log: bit_x.c,v $
**	Revision 1.21  2003/01/31 17:23:49  gsl
**	Fix  copyright header
**	
**	Revision 1.20  2002/07/31 20:24:29  gsl
**	globals
**	
**	Revision 1.19  2002/07/30 22:00:35  gsl
**	WSETFACBLINK
**	
**	Revision 1.18  2002/07/29 14:47:20  gsl
**	wfopen2 ->WFOPEN2
**	wfopen3 ->WFOPEN3
**	
**	Revision 1.17  2002/07/26 19:20:47  gsl
**	bit routine globals (bit_on -> BIT_ON) etc.
**	
**	Revision 1.16  2002/07/20 00:31:14  gsl
**	remove unused lbit_test
**	
**	Revision 1.15  2002/07/19 22:07:13  gsl
**	Renaming cobol api routines for global uniqueness
**	
**	Revision 1.14  2002/07/16 16:24:57  gsl
**	Globals
**	
**	Revision 1.13  2002/07/10 21:05:14  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.12  2002/07/10 01:36:01  gsl
**	fix wmemcpy to WMEMCPY
**	
**	Revision 1.11  2002/07/02 04:00:39  gsl
**	change acu_cobol and mf_cobol to wisp_acu_cobol() and wisp_mf_cobol()
**	
**	Revision 1.10  1996/08/19 22:32:09  gsl
**	drcs update
**	
**
**
*/
