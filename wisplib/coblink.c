			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


#include "idsistd.h"
#include "cobrun.h"

COBLINK(progname)
char	*progname;									/* 8 char file name			*/
{
	int4	four=4L;								/* 4 byte int4 holding value "4".	*/
	int4	compcode,retcode;

	compcode=0;
	retcode=0;

	wvaset(&four);
	LINK2(progname,8, " ",1, &compcode,4, &retcode,4);

	wswap(&compcode);
	wswap(&retcode);

	if ( compcode == 8 && retcode == 20 )					/* If not found then try TYPE = S		*/
	{
		wvaset(&four);
		LINK2(progname,8, "S",1, &compcode,4, &retcode,4);
	}
}

