			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/* BITS ... A VAX/VMS implementation of two WANG-VS subroutines.  By...Steve Guilford.						*/

#include <string.h>
#ifndef VMS	/* unix or MSDOS */
#include <memory.h>
#endif

#include "idsistd.h"
#include "werrlog.h"
                 
BITPACK(in_ptr,out_ptr,in_len)
unsigned char *in_ptr, *out_ptr;				                        /* Pointers to parameters.		*/
int4 *in_len;										/* Input length.			*/

{      
#define		ROUTINE		3000

	unsigned char	*l_in_ptr, *l_out_ptr;						/* Local copies of the arg. pointer.	*/
	int4	llen;
	int4	ilen;
	int4	x;									/* Working variables.			*/
	unsigned char	test_bit = 0x80;						/* Used in setting the bytes.		*/
 
	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Log the entry.			*/

	l_in_ptr = in_ptr;  								/* Get passed value.			*/
	l_out_ptr = out_ptr;								/* Get passed value.			*/

	llen = *in_len;									/* Get passed value.			*/
	wswap(&llen);									/* Swap the order of the words.		*/
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

BITUNPK(in_ptr,out_ptr,in_len)
unsigned char *in_ptr, *out_ptr;							/* Pointers to parameters.		*/
int4 *in_len;										/* Input length.			*/

{      
#undef		ROUTINE
#define		ROUTINE		3500

	unsigned char	*l_in_ptr, *l_out_ptr;						/* Local copies of the passed args.	*/
	int4	llen;
	int4	ilen;
	int4	x;									/* Working variables. 			*/
	unsigned char	test_bit = 0x80;						/* Byte used to test the bits.		*/
 
	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	l_in_ptr = in_ptr;								/* Get passed value.			*/
	l_out_ptr = out_ptr;								/* Get passed value.			*/

	llen = *in_len;									/* How many bytes to be proccessed ?	*/
	wswap(&llen);									/* Swap the order of the words.		*/
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
