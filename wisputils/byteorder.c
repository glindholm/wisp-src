/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
**
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
** CVS
**
**
**
**
******************************************************************************
*/

#include <stdio.h>
main()
{
	long	l;
	char	*ptr;

	l = 0x12345678;
	ptr = (char *)&l;

	printf( "\nBYTE-ORDER <%8.8x> = <%2.2x> <%2.2x> <%2.2x> <%2.2x> \n",
		l, ptr[0], ptr[1], ptr[2], ptr[3] );
}
	
/*
**	History:
**	$Log: byteorder.c,v $
**	Revision 1.8  2003/02/04 18:50:26  gsl
**	fix copyright header
**	
**	Revision 1.7  1996/07/23 18:12:50  gsl
**	drcs update
**	
**
**
*/
