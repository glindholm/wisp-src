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

werrpath()
{
	static	int	first=1;
	char	*path;

	if (first)
	{
		first=0;
#ifdef VMS
		path = "sys$login:";
#else /* !VMS */
		path = (char *)getenv(WISP_HOME_ENV);
#endif /* !VMS */

		buildfilepath( werrlog_path, path, WISP_ERROR_FILE );
	}
}

