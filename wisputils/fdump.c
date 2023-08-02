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
**
******************************************************************************
*/

#include <stdio.h>
#include <errno.h>

main(argc,argv)
int	argc;
char	*argv[];
{
	FILE	*fh;
	int 	c;

	if (argc < 2)
	{
		printf("Usage: fdump {filename}\n");
		exit(0);
	}

	printf("Opening file %s\n",argv[1]);

	fh = fopen(argv[1],"r");

	if (!fh)
	{
		printf("Open failed errno = %d\n",errno);
		exit(0);
	}

	while( ((c = fgetc(fh)) != EOF ) )
	{
		if      ( c == 0x0A ) printf("<NL>\n");
		else if ( c == 0x0D ) printf("<CR>");
		else if ( c == 0x09 ) printf("<TB>");
		else if ( c == 0x0C ) printf("<FF>\n");
		else if ( c < 20  || c > 128 ) printf("<%02x>",c);
		else printf("%c",c);
	}
	
	exit(0);
}



/*
**	History:
**	$Log: fdump.c,v $
**	Revision 1.8  2003/02/04 18:50:26  gsl
**	fix copyright header
**	
**	Revision 1.7  1996/07/23 18:12:53  gsl
**	drcs update
**	
**
**
*/
