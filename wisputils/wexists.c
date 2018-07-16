			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	NAME:	wexists.c
*/

/*	Check for wEXISTS of a file/lib/vol from unix shell		*/
/*									*/
/*	exists FILE    file library volume				*/
/*	exists LIBRARY      library volume				*/
/*	exists VOLUME               volume				*/

#include <stdio.h>

#define EXT_FILEXT
#include "filext.h"


main(argc,argv)
int	argc;
char	*argv[];
{
	int	argcount, start, cnt, fcnt, i;
	char	recv[22], type[1], key[20], file[20], lib[20], vol[20];

	initglbs("WEXISTS ");

	if (argc < 3) badusage();

	for(i=0; argv[1][i]; i++) key[i] = toupper( argv[1][i] );
	key[i] = '\0';

	if ( 0 == strcmp( "FILE", key ) )
	{
		if (argc != 5) badusage();
		strcpy(file,argv[2]);
		strcpy(lib, argv[3]);
		strcpy(vol, argv[4]);
	}
	else if ( 0 == strcmp( "LIBRARY", key ) )
	{
		if (argc != 4) badusage();
		strcpy(file, "        ");
		strcpy(lib, argv[2]);
		strcpy(vol, argv[3]);
	}
	else if ( 0 == strcmp( "VOLUME", key ) )
	{
		if (argc != 3) badusage();
		strcpy(file, "        ");
		strcpy(lib,  "        ");
		strcpy(vol, argv[2]);
	}
	else badusage();

	if ( strlen(file)>8 || strlen(lib)>8 || strlen(vol)>6 ) badusage();

	start = 1;
	cnt = 1;
	fcnt = 0;
	type[0] = 'A';
	argcount = 8;
	wvaset( &argcount );
	FIND(file,lib,vol,&start,&cnt,recv,&fcnt,type);

	if ( fcnt > 0 )
		exit(0);
	else
		exit(1);
}

badusage()
{
	printf("\n");
	printf("Usage: wexists FILE    file library volume\n");
	printf("       wexists LIBRARY      library volume\n");
	printf("       wexists VOLUME               volume\n");
	printf("\n");
	printf("Returns: 0 = EXISTS  1 = NOT FOUND\n");
	printf("\n");
	exit(-1);
}

#ifdef unix
#include "wutils.h"
#endif


