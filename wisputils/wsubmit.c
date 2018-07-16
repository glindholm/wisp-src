static char copyright[]="Copyright (c) 1988-1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wsubmit.c
**
**	Project:	wisp/utils
**
**	RCS:		$Source:$
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

#include "wispvers.h"
#include "intdef.h"

#define EXT_FILEXT
#include "filext.h"

/*
**	Static data
*/
static char rcs_revision[] = "$Revision:$";

/*
**	Usage:	$ wsubmit file [library [volume]] [USING parms1 - parm8]
*/

main(argc,argv)
int	argc;
char	*argv[];
{
	int4	argcount, retcode;
	int	using;
	char	file[20], lib[20], vol[20];
	char	buff[80];

	initglbs("WSUBMIT ");

	if (argc < 2) badusage();

	strcpy(buff, argv[1]);
	upper_string(buff);
	if ( strlen(buff)>8 ) badusage();

	loadpad(file,buff,8);

	using = 0;
	strcpy(lib,"        ");
	strcpy(vol,"      ");

	if (argc > 2)
	{
		strcpy(buff,argv[2]);
		upper_string(buff);
		if (0==strcmp(buff,"USING")) 
		{
			using = 3;
		}
		else
		{
			loadpad(lib, buff, 8);
			if ( strlen(lib)>8 ) badusage();
		}
	}

	if (argc > 3 && !using)
	{
		strcpy(buff,argv[3]);
		upper_string(buff);
		if (0==strcmp(buff,"USING")) 
		{
			using = 4;
		}
		else
		{
			loadpad(vol, buff, 6);
			if ( strlen(vol)>6 ) badusage();
		}
	}

	if (argc > 4 && !using)
	{
		strcpy(buff,argv[4]);
		upper_string(buff);
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
	argcount = 4;
	wvaset( &argcount );
	SUBMIT( file, lib, vol, &retcode);

	wswap(&retcode);
	exit(retcode);
}

badusage()
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
