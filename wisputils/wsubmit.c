/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id: wsubmit.c,v 1.18 2003/02/04 21:05:36 gsl Exp $
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
** $Date: 2003/02/04 21:05:36 $
** $Revision: 1.18 $
******************************************************************************
*/

/*
**	File:		wsubmit.c
**
**	Project:	wisp/utils
**
**
**
**	Purpose:	Emulation of Wang Proc SUBMIT statement
**
**	Routines:	main()		The main routine
**
*/

#ifdef unix

/*
**	Includes
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "wisplib.h"
#include "vssubs.h"
#include "idsisubs.h"
#include "wispvers.h"
#include "intdef.h"

#include "filext.h"

/*
**	Static data
*/
static char rcs_revision[] = "$Revision: 1.18 $";


static void badusage();

/*
**	Usage:	$ wsubmit file [library [volume]] [USING parms1 - parm8]
*/

int main(argc,argv)
int	argc;
char	*argv[];
{
	int4	retcode;
	int	using;
	char	file[20], lib[20], vol[20];
	char	buff[80];

	WL_initglbs("WSUBMIT ");

	if (argc < 2) badusage();

	strcpy(buff, argv[1]);
	WL_upper_string(buff);
	if ( strlen(buff)>8 ) badusage();

	WL_loadpad(file,buff,8);

	using = 0;
	strcpy(lib,"        ");
	strcpy(vol,"      ");

	if (argc > 2)
	{
		strcpy(buff,argv[2]);
		WL_upper_string(buff);
		if (0==strcmp(buff,"USING")) 
		{
			using = 3;
		}
		else
		{
			WL_loadpad(lib, buff, 8);
			if ( strlen(lib)>8 ) badusage();
		}
	}

	if (argc > 3 && !using)
	{
		strcpy(buff,argv[3]);
		WL_upper_string(buff);
		if (0==strcmp(buff,"USING")) 
		{
			using = 4;
		}
		else
		{
			WL_loadpad(vol, buff, 6);
			if ( strlen(vol)>6 ) badusage();
		}
	}

	if (argc > 4 && !using)
	{
		strcpy(buff,argv[4]);
		WL_upper_string(buff);
		if (0==strcmp(buff,"USING")) 
		{
			using = 5;
		}
		else
		{
			badusage();
		}
	}

	if (using)
	{
		int	i;
		short	cnt;
		short	len[8];
		char	*parm[8];

		cnt = argc - using;
		if (cnt > 8 || cnt < 1) badusage();

#ifdef TESTING
printf("SETSUBMIT cnt=%d\n",(int)cnt);
#endif

		for(i=0; i<cnt ; i++)
		{
			parm[i] = argv[using+i];
			len[i] = strlen(parm[i]);
#ifdef TESTING
printf("len[%d]=%d, parm[%d]=%s\n",i,(int)len[i],i,parm[i]);
#endif
		}
		SETSUBMIT(&cnt, &len[0],parm[0], &len[1],parm[1], &len[2],parm[2], &len[3],parm[3],
				&len[4],parm[4], &len[5],parm[5], &len[6],parm[6], &len[7],parm[7]);
	}
 
	retcode = 0;

#ifdef TESTING
printf("SUBMIT file[%8.8s] lib[%8.8s] vol[%6.6s]\n",file,lib,vol);
#endif
	WL_set_va_count(4);
	SUBMIT( file, lib, vol, &retcode);

	WL_wswap(&retcode);
	exit(retcode);
}

static void badusage()
{

	printf("\n");
	printf("wsubmit: %s (WL=%s)\n", rcs_revision, wisp_version());
	printf("Usage: wsubmit file [library [volume]] [USING parm1 - parm8]\n");
	printf("\n");
	exit(-1);
}

#include "wutils.h"

#endif

/*
**	History:
**	$Log: wsubmit.c,v $
**	Revision 1.18  2003/02/04 21:05:36  gsl
**	fix -Wall warnings
**	
**	Revision 1.17  2003/02/04 20:42:49  gsl
**	fix -Wall warnings
**	
**	Revision 1.16  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.15  2002/07/12 17:17:05  gsl
**	Global unique WL_ changes
**	
**	Revision 1.14  2002/07/11 14:52:52  gsl
**	Fix WL_ globals
**	
**	Revision 1.13  2002/07/11 14:33:55  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.12  2002/07/10 21:06:32  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.11  2002/06/25 18:18:37  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.10  1997/06/10 19:48:54  scass
**	Changed long to int4 for portability.
**	
**	Revision 1.9  1996-01-02 11:33:57-05  gsl
**	Changed ifdef DEBUG to TESTING
**
 * Revision 1.8  1995/07/10  13:37:05  gsl
 * fixed verison printout
 *
 * Revision 1.7  1995/07/10  13:34:38  gsl
 * Add standard headers plus printing of version number
 *
**
**
**	09/14/92	Added parameter passing. GSL
**
*/
