			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


#ifdef unix

/*
	wgetpgrp	Returns the process group id. 
			It will first check the env variable PGRPID and use it if set.
			This is to solve problem of using ksh or csh.
*/

#include "wdefines.h"

int wgetpgrp()
{
	int	gid, rc;
	char	*ptr;

	gid = -1;

	if (ptr=(char *)getenv(WISP_GID_ENV))
	{
		rc = sscanf(ptr,"%d",&gid);
		if (rc != 1 || gid < 1 ) gid = -1;
	}

	if (gid < 1)
	{
		gid = getpgrp(0);
	}

	return( gid );
}
#endif	/* unix */

#ifdef MSDOS

/*
	wgetpgrp	Returns the process group id. 
			It will first check the env variable PGRPID and use it if set.
			This is to solve problem of using ksh or csh.
*/

#include <stdlib.h>
#include "wdefines.h"

int wgetpgrp()
{
	int	gid, rc;
	char	*ptr;

	gid = -1;

	if (ptr=(char *)getenv(WISP_GID_ENV))
	{
		rc = sscanf(ptr,"%d",&gid);
		if (rc != 1 || gid < 1 ) gid = -1;
	}

	if (gid < 1)
	{
		gid = 1;									/* Default to a group id of "1"	*/
	}

	return( gid );
}
#endif	/* MSDOS */

