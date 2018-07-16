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

/*	Rename a file from the unix shell				*/
/*									*/
/*	wrename         oldfile oldlib oldvol newfile newlib		*/
/*	wrename LIBRARY         oldlib oldvol         newlib		*/

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
	char	type[1];
	char	key[20], oldfile[20], oldlib[20], oldvol[20];
	char	newlib[20], newfile[20];

	initglbs("WRENAME  ");

	if (argc < 5) badusage();

	strcpy( key, argv[1] );
	upper_string( key );

	if ( 0 == strcmp( "LIBRARY", key ) )
	{
		if (argc != 5) badusage();
		strcpy(oldfile, "        ");
		strcpy(oldlib,  argv[2]);
		strcpy(oldvol,  argv[3]);
		strcpy(newfile, "        ");
		strcpy(newlib,  argv[4]);
	}
	else
	{
		if (argc != 6) badusage();
		strcpy(oldfile, argv[1]);
		strcpy(oldlib,  argv[2]);
		strcpy(oldvol,  argv[3]);
		strcpy(newfile, argv[4]);
		strcpy(newlib,  argv[5]);
	}
	 
	if ( strlen(oldfile)>8 ||
	     strlen(oldlib)>8  ||
	     strlen(oldvol)>6  ||
	     strlen(newfile)>8 ||
	     strlen(newlib)>8   ) badusage();
 
	type[0] = 'G';
	retcode = 0;

	argcount = 7;
	wvaset( &argcount );
	wrename(type, oldfile, oldlib, oldvol,
                      newfile, newlib,         &retcode);

	wswap(&retcode);
	exit(retcode);
}

badusage()
{
	printf("\n");
	printf("Usage: wrename         oldfile oldlib oldvol newfile newlib\n");
	printf("       wrename LIBRARY         oldlib oldvol         newlib\n");
	printf("\n");
	exit(-1);
}

#include "wutils.h"

#endif

/*
**	History:
**	$Log: wrename.c,v $
**	Revision 1.7.2.1  2002/08/16 21:50:52  gsl
**	Alpha Port 4402f
**	
**	Revision 1.7  1996-07-23 14:13:11-04  gsl
**	drcs update
**
**
**
*/
