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


/*
	Scratch a file from the unix shell

	wscratch         file library volume
	wscratch LIBRARY      library volume
*/
#ifdef unix

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "wisplib.h"
#include "vssubs.h"
#include "filext.h"
#include "idsisubs.h"

static void badusage();


int main(argc,argv)
int	argc;
char	*argv[];
{
	int	retcode;
	char	type[1], check[1], access[1];
	char	key[20], file[20], lib[20], vol[20];

	WL_initglbs("WSCRATCH");

	if (argc != 4) badusage();

	strcpy( key, argv[1] );
	WL_upper_string( key );

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

	WL_set_va_count( 7 );
	SCRATCH( type, file, lib, vol, check, access, &retcode);

	WL_wswap(&retcode);
	exit(retcode);
}

static void badusage()
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
**	Revision 1.16  2003/02/04 21:23:44  gsl
**	fix -Wall warnings
**	
**	Revision 1.15  2003/02/04 21:05:36  gsl
**	fix -Wall warnings
**	
**	Revision 1.14  2003/02/04 20:42:49  gsl
**	fix -Wall warnings
**	
**	Revision 1.13  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.12  2002/07/12 17:17:04  gsl
**	Global unique WL_ changes
**	
**	Revision 1.11  2002/07/11 14:33:55  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.10  2002/07/10 21:06:32  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.9  2002/07/09 19:58:15  gsl
**	Add missing include string.h
**	
**	Revision 1.8  2002/06/25 18:18:37  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.7  1996/07/23 18:13:12  gsl
**	drcs update
**	
**
**
*/
