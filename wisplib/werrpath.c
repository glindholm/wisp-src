			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	werrpath.c	Initialize werrlog_path.
*/

#include "wdefines.h"
#include "wglobals.h"

werrpath()
{
	static	int	first=1;
	char	*ptr;

	if (first)
	{
		first=0;
#ifdef unix
		if ( ptr = (char *)getenv( WISP_HOME_ENV ) )
		{
			strcpy( werrlog_path, ptr );
			strcat( werrlog_path, "/" );
		}
		strcat( werrlog_path, WISP_ERROR_FILE );
#endif
#ifdef VMS
		strcpy( werrlog_path, "sys$login:" );
		strcat( werrlog_path, WISP_ERROR_FILE );
#endif
	}
}

