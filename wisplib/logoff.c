/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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

/********************************************************************************************************************************
 *	WANG USERSUBS "LOGOFF"													*
 *	function:	Log off user immediately										*
 ********************************************************************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include "idsistd.h"
#include "wexit.h"
#include "wglobals.h"
#include "werrlog.h"
#include "vwang.h"
#include "wdefines.h"
#include "wisplib.h"


#ifdef unix
#include <sys/types.h>
#include <signal.h>
#include <unistd.h>

void LOGOFF(void)
{
	int 	gpid, kpid, rc;
	char	*ptr;


	WL_wtrace("LOGOFF","ENTRY","PID=%d WISPGID=%d GID=%d", 
		(int)getpid(), WL_wgetpgrp(), (int)getpgrp());

	wisp_set_LOGOFFFLAG(1);							/* We've hit a logoff				*/
	if ( wisp_get_CANEXITFLAG() )						/* Cancel exit was set so don't logoff just	*/
	{									/* bubble up to the next link-level.		*/
		WL_wtrace("LOGOFF","CANCELEXIT","LOGOFF with CANEXITFLAG=%d",wisp_get_CANEXITFLAG());
		wexit(32);
	}

	vwang_shut();

	/*
	**  Ignore hangup signals 
	**  When the parent process is killed this one may get an SIGHUP
	*/
	signal( SIGHUP,  SIG_IGN );

	ptr = getenv(WISP_CANCELEXIT_ENV);
	if ( ptr && *ptr && 0!=strcmp(ptr,"0"))					/* Cancel exit was set BUT we went thru a script*/
	{									/* or some other non-kosher route to get here	*/
										/* and we don't have a contiguous path back up	*/
										/* to the cancel-exit point. So we are going to	*/
										/* kill the process immediately below the cancel*/
										/* exit point and hope we get back correctly.	*/

		WL_wtrace("LOGOFF","CANCELEXIT","LOGOFF with [%s=%s]",WISP_CANCELEXIT_ENV,ptr);

		rc = sscanf(ptr,"%d",&kpid);					/* - Get the pid to kill.			*/
		if ( rc != 1 )
		{
			WL_wtrace("LOGOFF","CANCELEXIT","Mis-formed kill process id");
			wexit(32);						/* This should not happen -- Just exit		*/
		}

		if ( kpid != 0 )						/* If cancel-exit not turned off by SUBMIT	*/
		{
			if (kpid != getpid())					/* - If not current process then kill it.	*/
			{
				WL_wtrace("LOGOFF","CANCELEXIT","KILL pid=%d signal=%d (SIGKILL)",
					kpid,(int)SIGKILL);
				kill((pid_t)kpid,SIGKILL);			/* Kill the process below the cancel-exit point	*/
			}
			wexit(32);						/* - This process is probably dead already but	*/
										/*   just in case lets do an exit.		*/
		}
	}

										/* No cancel-exit so really LOGOFF		*/
	gpid=WL_wgetpgrp();							/* Get process group id 			*/
	WL_wtrace("LOGOFF","KILL","GROUP WISPGID=%d signal=%d (SIGKILL)", -gpid, (int)SIGKILL);
	kill((pid_t)-gpid,SIGKILL);						/* Kill all procs in our group			*/

	/*
	**	If WISPGID was set for a Bourne shell then the above
	**	might not work so try again with the *REAL* group id.
	*/
	gpid=getpgrp();								/* Get the *REAL* process group id 		*/
	WL_wtrace("LOGOFF","KILL","GROUP gid=%d signal=%d (SIGKILL)", -gpid, (int)SIGKILL);
	kill((pid_t)-gpid,SIGKILL);						/* Kill all procs in our group			*/

	wexit(32);
}

#endif


#ifdef WIN32
void LOGOFF(void)
{
	WL_wtrace_entry("LOGOFF");

	wisp_set_LOGOFFFLAG(1);							/* We've hit a logoff				*/
	if ( wisp_get_CANEXITFLAG() )						/* Cancel exit was set so don't logoff just	*/
	{									/* bubble up to the next link-level.		*/
		wexit(32);
	}

	/* ExitWindows(0,0); */
	wexit(32);
}
#endif


/*
**	History:
**	$Log: logoff.c,v $
**	Revision 1.23  2003/07/22 19:02:21  gsl
**	ignore SIGHUP during a logoff
**	
**	Revision 1.22  2003/07/22 18:06:19  gsl
**	add tracing for UNIX
**	
**	Revision 1.21  2003/07/22 17:48:58  gsl
**	add tracing for UNIX
**	
**	Revision 1.20  2003/01/31 21:40:59  gsl
**	Fix -Wall warnings
**	
**	Revision 1.19  2003/01/31 17:23:48  gsl
**	Fix  copyright header
**	
**	Revision 1.18  2002/12/10 17:09:18  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.17  2002/12/09 21:09:29  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.16  2002/07/10 21:05:19  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.15  2002/07/01 04:02:39  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.14  2002/06/21 03:10:37  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.13  1996/08/29 02:38:25  gsl
**	Fix check of CANCEL-EXIT to not count if value is ")'
**	thats "0".
**	
**	Revision 1.12  1996-08-22 14:40:55-07  gsl
**	For NT remove call to ExitWindows() - it doesn't do what's needed
**
**	Revision 1.11  1996-08-19 15:32:28-07  gsl
**	drcs update
**
**
**
*/
