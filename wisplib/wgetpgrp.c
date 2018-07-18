/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/


/*
	WL_wgetpgrp	Returns the process group id. 
			It will first check the env variable PGRPID and use it if set.
			This is to solve problem of using ksh or csh.
*/

#include <stdio.h>
#include <stdlib.h>

#ifdef unix
#include <unistd.h>
#endif

#ifdef WIN32
#include <process.h>
#endif

#include "idsistd.h"
#include "wdefines.h"
#include "werrlog.h"
#include "wexit.h"
#include "wisplib.h"

int WL_wgetpgrp(void)
{
	static	int	gid = -1;
	int	rc;
	char	*ptr;

	if (-1 == gid)
	{
		/*
		**	First time thru set gid.
		*/

		if ((ptr=getenv(WISP_GID_ENV)))					/* Use the WISPGID variable if available	*/
		{
			rc = sscanf(ptr,"%d",&gid);
			if (rc != 1) gid = -1;
			wtrace("WGETPGRP","GID","Environment variable [%s=%s] GID=[%d]",WISP_GID_ENV,ptr,gid);
		}

		if (-1 == gid)
		{
#ifdef unix
			gid = getpgrp();
#endif /* unix */
#if defined(WIN32)

			/*
			**	These systems do not support getpgrp().
			**	Since WL_wgetpgrp() is used to get a unique number
			**	for the process group we will use getpid() and 
			**	ensure that this routine is called very early
			**	in every process so gid gets set.
			*/
			gid = _getpid();
			{
				/*
				**	Now that we have picked a gid - set it
				**	in the environment so linked-to processes
				**	will inherit it.
				*/
				char	envstring[40];
				sprintf(envstring,"%s=%d",WISP_GID_ENV,gid);
				if (WL_setenvstr(envstring))
				{
					wexit(102);
				}
			}
#endif /*  WIN32 */

			ptr=getenv(WISP_GID_ENV);
			wtrace("WGETPGRP","GID","Environment variable [%s=%s] GID=[%d]",WISP_GID_ENV,(ptr)?ptr:"(nil)",gid);
		}
	}

	return( gid );
}

/*
**	History:
**	$Log: wgetpgrp.c,v $
**	Revision 1.19  2011/09/04 21:12:53  gsl
**	fix win32 IOS C++ warnings
**	
**	Revision 1.18  2003/02/04 17:05:01  gsl
**	Fix -Wall warnings
**	
**	Revision 1.17  2003/02/04 16:02:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.16  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.15  2002/12/09 19:15:36  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.14  2002/07/10 21:05:32  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.13  2002/07/09 04:13:53  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.12  2002/06/21 03:10:45  gsl
**	Remove VMS & MSDOS
**	
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
