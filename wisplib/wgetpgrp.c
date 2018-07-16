static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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

#include <stdio.h>
#include <stdlib.h>

#ifdef WIN32
#include <process.h>
#endif
#ifdef VMS
#include <unixlib.h>
#endif

#include "idsistd.h"
#include "wdefines.h"
#include "werrlog.h"
#include "wexit.h"
#include "wisplib.h"

int wgetpgrp(void)
{
	static	int	gid = -1;
	int	rc;
	char	*ptr;

	if (-1 == gid)
	{
		/*
		**	First time thru set gid.
		*/

		if (ptr=getenv(WISP_GID_ENV))					/* Use the WISPGID variable if available	*/
		{
			rc = sscanf(ptr,"%d",&gid);
			if (rc != 1) gid = -1;
			wtrace("WGETPGRP","GID","Environment variable [%s=%s] GID=[%d]",WISP_GID_ENV,ptr,gid);
		}

		if (-1 == gid)
		{
#ifdef unix
#ifndef OSF1_ALPHA
			gid = getpgrp(0);
#else
			gid = getpgrp();
#endif			
#endif /* unix */
#if defined(MSDOS) || defined(VMS) || defined(WIN32)

			/*
			**	These systems do not support getpgrp().
			**	Since wgetpgrp() is used to get a unique number
			**	for the process group we will use getpid() and 
			**	ensure that this routine is called very early
			**	in every process so gid gets set.
			*/
			gid = getpid();
			{
				/*
				**	Now that we have picked a gid - set it
				**	in the environment so linked-to processes
				**	will inherit it.
				*/
				char	envstring[40];
				sprintf(envstring,"%s=%d",WISP_GID_ENV,gid);
				if (setenvstr(envstring))
				{
					werrlog(102,"%%wgetpgrp-F-ERROR setenvstr() Failed",0,0,0,0,0,0,0);
					wexit(102);
				}
			}
#endif /* MSDOS || VMS || WIN32 */

			ptr=getenv(WISP_GID_ENV);
			wtrace("WGETPGRP","GID","Environment variable [%s=%s] GID=[%d]",WISP_GID_ENV,(ptr)?ptr:"(nil)",gid);
		}
	}

	return( gid );
}

/*
**	History:
**	$Log: wgetpgrp.c,v $
**	Revision 1.11  1997/07/16 19:09:57  gsl
**	Add wtrace() for GID
**	
**	Revision 1.10  1996-08-22 20:22:43-04  gsl
**	Faked a getpgrp() for systems that don't have one - namely WIN32 MSDOS and VMS
**
**	Revision 1.9  1996-08-19 15:33:17-07  gsl
**	drcs update
**
**
**
*/
