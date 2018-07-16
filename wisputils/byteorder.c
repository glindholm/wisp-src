static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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
**	Revision 1.7  1996-07-23 14:12:50-04  gsl
**	drcs update
**
**
**
*/
