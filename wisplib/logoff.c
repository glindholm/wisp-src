static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	     Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/********************************************************************************************************************************
 *	WANG USERSUBS "LOGOFF"													*
 *	function:	Log off user immediately										*
 ********************************************************************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include "idsistd.h"
#include "wexit.h"

extern int4 LOGOFFFLAG;
extern int4 CANEXITFLAG;


#ifdef VMS
#include <descrip.h>

static $DESCRIPTOR(logoff_command,"logoutnow");		/*parameter is descriptor*/


void LOGOFF(void)
{
	LOGOFFFLAG = 1;									/* We've hit a logoff			*/
	if ( CANEXITFLAG )
	{
		wexit(32L);								/* Don't logoff just terminate		*/
	}

	lib$do_command(&logoff_command);
}
#endif

#ifdef unix
#include <sys/types.h>
#include <signal.h>
#include "idsistd.h"
#include "wdefines.h"

void LOGOFF(void)
{
	int 	gpid, kpid, rc;
	char	*ptr;

	LOGOFFFLAG = 1;								/* We've hit a logoff				*/
	if ( CANEXITFLAG )							/* Cancel exit was set so don't logoff just	*/
	{									/* bubble up to the next link-level.		*/
		wexit(32);
	}

	vwang_shut();

	ptr = getenv(WISP_CANCELEXIT_ENV);
	if ( ptr && *ptr && 0!=strcmp(ptr,"0"))								/* Cancel exit was set BUT we went thru a script*/
	{									/* or some other non-kosher route to get here	*/
										/* and we don't have a contiguous path back up	*/
										/* to the cancel-exit point. So we are going to	*/
										/* kill the process immediately below the cancel*/
										/* exit point and hope we get back correctly.	*/
		rc = sscanf(ptr,"%d",&kpid);					/* - Get the pid to kill.			*/
		if ( rc != 1 )
		{
			wexit(32);						/* This should not happen -- Just exit		*/
		}

		if ( kpid != 0 )						/* If cancel-exit not turned off by SUBMIT	*/
		{
			if (kpid != getpid())					/* - If not current process then kill it.	*/
			{
				kill((pid_t)kpid,SIGKILL);			/* Kill the process below the cancel-exit point	*/
			}
			wexit(32);						/* - This process is probably dead already but	*/
										/*   just in case lets do an exit.		*/
		}
	}

										/* No cancel-exit so really LOGOFF		*/
	gpid=wgetpgrp();							/* Get process group id 			*/
	kill((pid_t)-gpid,SIGKILL);						/* Kill all procs in our group			*/

	/*
	**	If WISPGID was set for a Bourne shell then the above
	**	might not work so try again with the *REAL* group id.
	*/
	gpid=getpgrp();								/* Get the *REAL* process group id 		*/
	kill((pid_t)-gpid,SIGKILL);						/* Kill all procs in our group			*/

	wexit(32);
}

#endif

#ifdef MSDOS
LOGOFF()
{
	/*
	**	No equivalent to LOGOFF on MSDOS so we just do a full exit.
	*/
	wexit(32L);
}
#endif /* MSDOS */

#ifdef WIN32
void LOGOFF(void)
{

	LOGOFFFLAG = 1;								/* We've hit a logoff				*/
	if ( CANEXITFLAG )							/* Cancel exit was set so don't logoff just	*/
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
**	Revision 1.13  1996-08-28 22:38:25-04  gsl
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
