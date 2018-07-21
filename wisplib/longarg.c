/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
*/



/*
	WL_longargtest	This routine test if the arg from a variable arg list is a int4.

			Returns		 1  is a int4
					 0  is not a int4
					-1  invalid size for sigbytes
*/

/*
 We are testing to see if arg is a int4. Arg is a 4 byte binary.

					byteorder  	1-byte	test	2-bytes	test	3-bytes test
    (1)	Big-Endian    (byte-normal)	1 2 3 4    	0 0 0 n	 1	0 0 n n	 1	0 n n n  1
    (2)	Little-Endian (byte-swap)	2 1 4 3    	0 0 n 0  1	0 0 n n	 1	n 0 n n	 2	(2+2)
    (3)	Little-Endian (byte-swap)	4 3 2 1	   	n 0 0 0	 2	n n 0 0  3	n n n 0	 4	(4 byte bin + noswap)

   NOTE: This test can fail if the arg is not a int4 and we are testing other then the first byte and the following arg 
	 is smaller then the postion of the byte we are testing.  I.e. if we are testing byte 2 and the following are is only
	 one byte int4.
*/

#include "idsistd.h"
#include "wglobals.h"
#include "wisplib.h"

int	WL_longargtest(char *ptr, int sigbytes)
	/* ptr		pointer to the argument.								*/
	/* sigbytes	the number of significant bytes (1,2 or 3) it can't tell if all 4 bytes are significant	*/
{

	if (sigbytes < 1 || sigbytes > 3) return(-1);

	if ( WL_bytenormal() || (!wisp_get_noswap() && sigbytes<3) )
	{
		if (ptr[0] == '\0')						/* Test first byte				*/
			return(1);
		else
			return(0);
	}

	/* The following are all Little-Endian (byte-swap) cases */

	if ((wisp_get_noswap() && sigbytes==1) || (!wisp_get_noswap() && sigbytes==3))
	{
		if (ptr[1] == '\0')						/* Test second byte				*/
			return(1);
		else
			return(0);
	}

	/* The following are noswap cases */

	if (sigbytes==2)
	{
		if (ptr[2] == '\0')						/* Test third byte				*/
			return(1);
		else
			return(0);
	}
	else /* sigbytes==3 */
	{
		if (ptr[3] == '\0')						/* Test forth byte				*/
			return(1);
		else
			return(0);
	}
	
}



/*
**	History:
**	$Log: longarg.c,v $
**	Revision 1.12  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.11  2002/07/10 21:05:19  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.10  2002/07/01 04:02:39  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.9  1996/08/19 22:32:28  gsl
**	drcs update
**	
**
**
*/
