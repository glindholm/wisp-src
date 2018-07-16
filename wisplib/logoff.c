			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/********************************************************************************************************************************
 *	WANG USERSUBS "LOGOFF"													*
 *	function:	Log off user immediately										*
 ********************************************************************************************************************************/

extern long LOGOFFFLAG;
extern long CANEXITFLAG;


#ifdef VMS
#include <descrip.h>

static $DESCRIPTOR(logoff_command,"logoutnow");		/*parameter is descriptor*/


LOGOFF()
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
#include <signal.h>
#include "wdefines.h"

extern int PGRPID;									/* Process Group ID.			*/

LOGOFF()
{
	int 	gpid, kpid, rc;
	char	*ptr;

	LOGOFFFLAG = 1;								/* We've hit a logoff				*/
	if ( CANEXITFLAG )							/* Cancel exit was set so don't logoff just	*/
	{									/* bubble up to the next link-level.		*/
		wexit(32L);
	}

	vexit();

	ptr = (char *)getenv(WISP_CANCELEXIT_ENV);
	if ( ptr )								/* Cancel exit was set BUT we went thru a script*/
	{									/* or some other non-kosher route to get here	*/
										/* and we don't have a contiguous path back up	*/
										/* to the cancel-exit point. So we are going to	*/
										/* kill the process immediately below the cancel*/
										/* exit point and hope we get back correctly.	*/
		rc = sscanf(ptr,"%d",&kpid);					/* - Get the pid to kill.			*/
		if ( rc != 1 )
		{
			wexit(32L);						/* This should not happen -- Just exit		*/
		}

		if ( kpid != 0 )						/* If cancel-exit not turned off by SUBMIT	*/
		{
			if (kpid != getpid())					/* - If not current process then kill it.	*/
			{
				kill(kpid,SIGKILL);				/* Kill the process below the cancel-exit point	*/
			}
			wexit(32L);						/* - This process is probably dead already but	*/
										/*   just in case lets do an exit.		*/
		}
	}

										/* No cancel-exit so really LOGOFF		*/
	gpid=PGRPID;								/* Get process group id 			*/
	kill(-gpid,SIGKILL);							/* Kill all procs in our group			*/
}

#endif

