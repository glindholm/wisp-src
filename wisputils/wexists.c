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


/*
**	NAME:	wexists.c
*/

/*	Check for wEXISTS of a file/lib/vol from unix shell		*/
/*									*/
/*	exists FILE    file library volume				*/
/*	exists LIBRARY      library volume				*/
/*	exists VOLUME               volume				*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#ifdef unix
#include <unistd.h>
#endif

#include "wisplib.h"
#include "filext.h"
#include "vssubs.h"

static void badusage();


int main(argc,argv)
int	argc;
char	*argv[];
{
	int	start, cnt, fcnt, i;
	char	recv[22], type[1], key[20], file[20], lib[20], vol[20];

	WL_initglbs("WEXISTS ");

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
	WL_set_va_count( 8);
	FIND(file,lib,vol,&start,&cnt,recv,&fcnt,type);

	if ( fcnt > 0 )
		exit(0);
	else
		exit(1);
}

static void badusage()
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


/*
**	History:
**	$Log: wexists.c,v $
**	Revision 1.14  2003/02/04 21:05:36  gsl
**	fix -Wall warnings
**	
**	Revision 1.13  2003/02/04 20:42:49  gsl
**	fix -Wall warnings
**	
**	Revision 1.12  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.11  2002/07/12 17:17:04  gsl
**	Global unique WL_ changes
**	
**	Revision 1.10  2002/07/10 21:06:30  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.9  2002/07/09 19:58:15  gsl
**	Add missing include string.h
**	
**	Revision 1.8  2002/06/25 18:18:35  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.7  1996/07/23 18:13:08  gsl
**	drcs update
**	
**
**
*/
