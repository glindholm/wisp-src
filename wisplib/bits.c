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


#include <string.h>

#include "idsistd.h"
#include "werrlog.h"
#include "wisplib.h"
#include "vssubs.h"
                 
void BITPACK(unsigned char *in_ptr,unsigned char *out_ptr, int4 *in_len)
{      
	unsigned char	*l_in_ptr, *l_out_ptr;						/* Local copies of the arg. pointer.	*/
	int4	llen;
	int4	ilen;
	int4	x;									/* Working variables.			*/
	unsigned char	test_bit = 0x80;						/* Used in setting the bytes.		*/
 
	WL_wtrace_entry("BITPACK");

	l_in_ptr = in_ptr;  								/* Get passed value.			*/
	l_out_ptr = out_ptr;								/* Get passed value.			*/

	llen = WL_get_swap(in_len);							/* Get passed value.			*/
	ilen = llen / 8; 								/* ilen = number of bytes to set.	*/
	memset(out_ptr,0,(size_t)ilen);							/* Initialize the output bytes.		*/

	while (ilen)									/* Do the computed number of bytes.	*/
	{      
		test_bit = 0x80;							/* Initialize it for the FOR loop.	*/
		for (x = 1; x < 9; x++)							/* x starts at 1, goes to 8 -- 8 bits. 	*/
		{
			if (*l_in_ptr == '1')						/* Is the input character a '1'		*/
                      	{
				*l_out_ptr |= test_bit;					/* Set the corresponding bit.		*/
			}
			test_bit >>= 1;							/* Shift left for the next bit.		*/
			l_in_ptr++;							/* Next character in the arg.		*/
		}
		l_out_ptr++;								/* Next byte to be set.			*/
		ilen--;									/* One less byte to proccess.		*/
	}
}

void BITUNPK(unsigned char *in_ptr, unsigned char *out_ptr, int4 *in_len)
{      
	unsigned char	*l_in_ptr, *l_out_ptr;						/* Local copies of the passed args.	*/
	int4	llen;
	int4	ilen;
	int4	x;									/* Working variables. 			*/
	unsigned char	test_bit = 0x80;						/* Byte used to test the bits.		*/
 
	WL_wtrace_entry("BITUNPK");

	l_in_ptr = in_ptr;								/* Get passed value.			*/
	l_out_ptr = out_ptr;								/* Get passed value.			*/

	llen = WL_get_swap(in_len);							/* How many bytes to be proccessed ?	*/
	ilen = llen;
      	memset(l_out_ptr,' ',(size_t)ilen*8);						/* Initialize the output array.		*/

	while (ilen)									/* i = number of bytes to proccess.	*/
	{           
 		test_bit = 0x80;							/* Set the test_bit mask.		*/
		for (x = 1; x < 9; ++x)							/* x starts at 1, goes to 8 -- 8 bits.	*/
		{
			if (*l_in_ptr & test_bit)					/* Is this bit set ?			*/
			{
				*l_out_ptr = '1';					/* Output gets  1 if the bit is set.	*/
			}
			else								/* otherwise...				*/
			{
				*l_out_ptr = '0';					/* Output gets 0 if the bit isn't set.	*/
		 	}
	 		l_out_ptr++;							/* Point to the next char in output.	*/
		    	test_bit >>= 1;							/* Shift right to test the next bit.	*/
	     	}   
		l_in_ptr++;                   						/* Point to the next byte to be tested.	*/
		ilen--;									/* One less byte to process.		*/
	}       
}
/*
**	History:
**	$Log: bits.c,v $
**	Revision 1.16  2003/01/31 17:23:49  gsl
**	Fix  copyright header
**	
**	Revision 1.15  2003/01/29 21:50:08  gsl
**	Switch to use vssubs.h
**	
**	Revision 1.14  2002/12/10 17:09:20  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.13  2002/12/09 21:09:26  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.12  2002/07/12 17:00:54  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.11  2002/06/26 01:42:45  gsl
**	Remove VMS code
**	
**	Revision 1.10  1996/08/19 22:32:10  gsl
**	drcs update
**	
**
**
*/
