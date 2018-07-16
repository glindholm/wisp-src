static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	werrpath.c	Initialize werrlog_path.
*/

#include "idsistd.h"
#include "wdefines.h"
#include "wglobals.h"
#include "idsisubs.h"
#include "wperson.h"
#include "wmalloc.h"
#include "wispcfg.h"

const char* werrpath(void)
{
	static char *the_path=NULL;

	if (!the_path)
	{
		char buff[128];

		buildfilepath( buff, wisphomedir(NULL), WISP_ERROR_FILE );
		the_path = wstrdup(buff);
	}
	return the_path;
}

/*
**	History:
**	$Log: werrpath.c,v $
**	Revision 1.11  1996/10/09 00:28:18  gsl
**	Add include wispcfg.h
**	
**	Revision 1.10  1996-08-23 14:06:19-07  gsl
**	Changed to use wisphomedir()
**
**	Revision 1.9  1996-08-19 15:33:11-07  gsl
**	drcs update
**
**
**
*/
