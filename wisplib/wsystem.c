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


#include "wisplib.h"
#include "werrlog.h"

/*
**	Routine:	wsystem()
**
**	Function:	To replace system().
**
**	Description:	Does a system() call but first handled SIGCLD signals.
**			The problem with system() is that if SIGCLD signals are being ignored
**			then it returns a bogus exit status.
**
**	Arguments:
**		cmd	The shell command string.
**
**	Return:		Exit status from the shell (from system()).
**
**	Warnings:	none
**
**	History:	07/14/92	Written by GSL
**
*/

#ifdef unix
#include <signal.h>

int wsystem(char* cmd)
{
	int 	rc;
	void	(*save_sig)();

	wtrace("WSYSTEM","COMMAND","%s", cmd);
	
	save_sig = signal(SIGCLD,SIG_DFL);
	rc = system(cmd);
	signal(SIGCLD,save_sig);
	return rc;
}
#endif /* unix */

#if defined(MSDOS)
int wsystem(char* cmd)
{
	wtrace("WSYSTEM","COMMAND","%s", cmd);

	return system(cmd);
}
#endif /* MSDOS */

#if defined(WIN32)
/*
**	ROUTINE:	wsystem()
**
**	FUNCTION:	Emulate the system() routine (as on unix)
**
**	DESCRIPTION:	The system() routine with WIN32 only runs "commands" (eg DIR, TYPE, etc) 
**			so we will parse the command string and issue a spawn instead.
**
**	ARGUMENTS:	
**	cmd		The command line
**
**	GLOBALS:	none
**
**	RETURN:		
**	-1		Failed
**
**	WARNINGS:	Only 20 command line args
**
*/
#include <stdio.h>
#include "win32spn.h"

int wsystem(char* cmd)
{
	return win32spawnlp(NULL, cmd, SPN_WAIT_FOR_CHILD);
}
#endif /* WIN32 */
/*
**	History:
**	$Log: wsystem.c,v $
**	Revision 1.14  1997-07-29 15:40:31-04  gsl
**	Add a wtrace()
**
**	Revision 1.13  1997-07-12 19:02:03-04  gsl
**	Change WIN32 to use our special win32spawnlp() instead of the
**	generic spawnvp().
**
**	Revision 1.12  1997-05-02 22:02:40-04  gsl
**	Removed _flushall()
**
**	Revision 1.11  1996-08-30 21:46:13-04  gsl
**	Add the required _flushall() before the spawn
**
**	Revision 1.10  1996-08-21 17:32:35-07  gsl
**	Add a WIN32 version which uses spawn() to create a new process
**
**	Revision 1.9  1996-08-21 16:35:29-07  gsl
**	Fix typo
**
**	Revision 1.8  1996-08-21 16:25:19-07  gsl
**	Add vwang_shut()/vwang_synch() around call to system
**
**	Revision 1.7  1996-08-19 15:33:25-07  gsl
**	drcs update
**
**
**
*/
