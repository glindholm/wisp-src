			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	wgetpgrp	Returns the process group id. 
			It will first check the env variable PGRPID and use it if set.
			This is to solve problem of using ksh or csh.
*/

#ifdef MSDOS
#include <stdlib.h>
#endif

#include "idsistd.h"
#include "wdefines.h"

int wgetpgrp()
{
	static	int	gid = -1;
	int	rc;
	char	*ptr;

	if (gid < 0)								/* First time thru				*/
	{
		if (ptr=(char *)getenv(WISP_GID_ENV))				/* Use the WISPGID variable if available	*/
		{
			rc = sscanf(ptr,"%d",&gid);
			if (rc != 1 || gid < 1 ) gid = -1;
		}

		if (gid < 1)
		{
#ifdef unix
#ifndef OSF1_ALPHA
			gid = getpgrp(0);
#else
			gid = getpgrp();
#endif			
#else
			gid = 1;						/* For MSDOS default to a group id of "1"	*/
#endif
		}
	}

	return( gid );
}

