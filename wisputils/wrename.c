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


/*	Rename a file from the unix shell				*/
/*									*/
/*	wrename         oldfile oldlib oldvol newfile newlib		*/
/*	wrename LIBRARY         oldlib oldvol         newlib		*/

#ifdef unix
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "filext.h"
#include "wisplib.h"
#include "vssubs.h"
#include "idsisubs.h"

static void badusage();


int main(argc,argv)
int	argc;
char	*argv[];
{
	int	retcode;
	char	type[1];
	char	key[20], oldfile[20], oldlib[20], oldvol[20];
	char	newlib[20], newfile[20];

	WL_initglbs("WRENAME  ");

	if (argc < 5) badusage();

	strcpy( key, argv[1] );
	WL_upper_string( key );

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

	WL_set_va_count( 7 );
	RENAME(type, oldfile, oldlib, oldvol,
                      newfile, newlib,         &retcode);

	WL_wswap(&retcode);
	exit(retcode);
}

static void badusage()
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
**	Revision 1.17  2003/02/04 21:05:36  gsl
**	fix -Wall warnings
**	
**	Revision 1.16  2003/02/04 20:42:49  gsl
**	fix -Wall warnings
**	
**	Revision 1.15  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.14  2003/01/30 21:11:22  gsl
**	Change RENAME to use stdarg.h
**	
**	Revision 1.13  2002/07/23 21:24:54  gsl
**	wrename -> RENAME
**	
**	Revision 1.12  2002/07/12 17:17:04  gsl
**	Global unique WL_ changes
**	
**	Revision 1.11  2002/07/11 14:33:56  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.10  2002/07/10 21:06:31  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.9  2002/07/09 19:58:15  gsl
**	Add missing include string.h
**	
**	Revision 1.8  2002/06/25 18:18:36  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.7  1996/07/23 18:13:11  gsl
**	drcs update
**	
**
**
*/
