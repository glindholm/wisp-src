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
	wfind	test the FIND usersub
*/

#ifdef unix

#include <stdio.h>
#include <string.h>

#define EXT_FILEXT
#include "filext.h"

main(argc,argv)
int	argc;
char	*argv[];
{
	int	argcount, retcode;
	char	type[1], check[1], access[1];
	char	key[20], file[20], lib[20], vol[20];
	int	starter, counter, filecount;
	char	rtype[1], receiver[2200];
	char	*ptr;

	initglbs("WFIND   ");

	if (argc != 4) badusage();

	strcpy(file, argv[1]);
	strcpy(lib, argv[2]);
	strcpy(vol, argv[3]);

	if (!strcmp(lib,".")) strcpy(lib,"        ");
	if (!strcmp(vol,".")) strcpy(vol,"      ");
	 
	if ( strlen(file)>8 || strlen(lib)>8 || strlen(vol)>6 ) badusage();
 
	retcode = 0;

	argcount = 8;
	wvaset( &argcount );
	starter=1;
	wswap(&starter);
	counter=100;
	wswap(&counter);
	rtype[0] = 'A';
	FIND( file, lib, vol, &starter, &counter, receiver, &filecount, rtype);
	wswap(&counter);
	wswap(&filecount);
	printf("\n");
	printf("FIND file(%s) lib(%s) vol(%s)\n", file, lib, vol);
	printf("     counter=%d filecount=%d\n\n",counter,filecount);
	for(ptr=receiver; filecount > 0; filecount--, ptr+=22)
	{
		printf("     %22.22s\n",ptr);
	}
	exit(retcode);
}

badusage()
{
	printf("\n");
	printf("Usage: wfind         file library volume\n");
	printf("\n");
	exit(-1);
}

#include "wutils.h"

#endif

/*
**	History:
**	$Log: wfind.c,v $
**	Revision 1.7  1996-07-23 14:13:09-04  gsl
**	drcs update
**
**
**
*/
