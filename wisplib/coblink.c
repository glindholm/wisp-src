static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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
#include "wisplib.h"

void COBLINK(const char *progname)
{
	int4	four=4L;								/* 4 byte int4 holding value "4".	*/
	int4	compcode,retcode;

	compcode=0;
	retcode=0;

	wvaset(&four);
	LINK2(progname, (int4) 8, " ", (int4) 1, &compcode, (int4) 4, &retcode, (int4) 4);

	wswap(&compcode);
	wswap(&retcode);

	if ( compcode == 8 && retcode == 20 )					/* If not found then try TYPE = S		*/
	{
		wvaset(&four);
		LINK2(progname, (int4) 8, "S", (int4) 1, &compcode, (int4) 4, &retcode, (int4) 4);
	}
}

/*
**	History:
**	$Log: coblink.c,v $
**	Revision 1.11  1997/10/23 20:06:16  gsl
**	Make progname a "const"
**	
**	Revision 1.10  1996-08-19 18:32:13-04  gsl
**	drcs update
**
**
**
*/
