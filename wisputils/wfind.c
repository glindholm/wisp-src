/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
**
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
**
** $Author: gsl $
**
**
******************************************************************************
*/


/*
	wfind	test the FIND usersub
*/

#ifdef unix

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "wisplib.h"
#include "vssubs.h"
#include "filext.h"

static void badusage();

int main(argc,argv)
int	argc;
char	*argv[];
{
	int	retcode;
	char	file[20], lib[20], vol[20];
	int	starter, counter, filecount;
	char	rtype[1], receiver[2200];
	char	*ptr;

	WL_initglbs("WFIND   ");

	if (argc != 4) badusage();

	strcpy(file, argv[1]);
	strcpy(lib, argv[2]);
	strcpy(vol, argv[3]);

	if (!strcmp(lib,".")) strcpy(lib,"        ");
	if (!strcmp(vol,".")) strcpy(vol,"      ");
	 
	if ( strlen(file)>8 || strlen(lib)>8 || strlen(vol)>6 ) badusage();
 
	retcode = 0;

	WL_set_va_count( 8 );
	starter=1;
	WL_wswap(&starter);
	counter=100;
	WL_wswap(&counter);
	rtype[0] = 'A';
	FIND( file, lib, vol, &starter, &counter, receiver, &filecount, rtype);
	WL_wswap(&counter);
	WL_wswap(&filecount);
	printf("\n");
	printf("FIND file(%s) lib(%s) vol(%s)\n", file, lib, vol);
	printf("     counter=%d filecount=%d\n\n",counter,filecount);
	for(ptr=receiver; filecount > 0; filecount--, ptr+=22)
	{
		printf("     %22.22s\n",ptr);
	}
	exit(retcode);
}

static void badusage()
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
**	Revision 1.13  2003/02/04 20:42:49  gsl
**	fix -Wall warnings
**	
**	Revision 1.12  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.11  2002/07/12 17:17:04  gsl
**	Global unique WL_ changes
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
**	Revision 1.7  1996/07/23 18:13:09  gsl
**	drcs update
**	
**
**
*/
