static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	Scratch a file from the unix shell

	wscratch         file library volume
	wscratch LIBRARY      library volume
*/
#ifdef unix

#include <stdio.h>

#define EXT_FILEXT
#include "filext.h"

main(argc,argv)
int	argc;
char	*argv[];
{
	int	argcount, retcode;
	char	type[1], check[1], access[1];
	char	key[20], file[20], lib[20], vol[20];

	initglbs("WSCRATCH");

	if (argc != 4) badusage();

	strcpy( key, argv[1] );
	upper_string( key );

	if ( 0 == strcmp( "LIBRARY", key ) )
	{
		type[0] = 'L';
		strcpy(file, "        ");
	}
	else
	{
		type[0] = 'F';
		strcpy(file, argv[1]);
	}
	strcpy(lib, argv[2]);
	strcpy(vol, argv[3]);
	 
	if ( strlen(file)>8 || strlen(lib)>8 || strlen(vol)>6 ) badusage();
 
	check[0] = 'B';
	access[0] = ' ';
	retcode = 0;

	argcount = 7;
	wvaset( &argcount );
	SCRATCH( type, file, lib, vol, check, access, &retcode);

	wswap(&retcode);
	exit(retcode);
}

badusage()
{
	printf("\n");
	printf("Usage: wscratch         file library volume\n");
	printf("       wscratch LIBRARY      library volume\n");
	printf("\n");
	exit(-1);
}

#include "wutils.h"

#endif

/*
**	History:
**	$Log: wscratch.c,v $
**	Revision 1.7  1996-07-23 14:13:12-04  gsl
**	drcs update
**
**
**
*/
