static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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
**	Revision 1.7  1996-07-23 14:12:53-04  gsl
**	drcs update
**
**
**
*/
