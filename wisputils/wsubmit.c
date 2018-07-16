			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*	Submit a file from the unix shell			*/
/*								*/
/*	wsubmit         file library volume			*/

#ifdef unix

#include <stdio.h>

#define EXT_FILEXT
#include "filext.h"

main(argc,argv)
int	argc;
char	*argv[];
{
	long	argcount, retcode;
	char	file[20], lib[20], vol[20];

	initglbs("WSUBMIT ");

	if (argc !=2 && argc != 4) badusage();

	strcpy(file, argv[1]);
	if ( strlen(file)>8 ) badusage();

	if (argc == 4)
	{
		strcpy(lib, argv[2]);
		strcpy(vol, argv[3]);
		if ( strlen(lib)>8 || strlen(vol)>6 ) badusage();
	}

	 
 
	retcode = 0;

	if (argc == 2)
	{
		argcount = 2;
		wvaset( &argcount );
		SUBMIT( file, &retcode);
	}
	else
	{
		argcount = 4;
		wvaset( &argcount );
		SUBMIT( file, lib, vol, &retcode);
	}

	wswap(&retcode);
	exit(retcode);
}

badusage()
{
	printf("\n");
	printf("Usage: wsubmit         file [library volume]\n");
	printf("\n");
	exit(-1);
}

#include "wutils.h"

#endif

