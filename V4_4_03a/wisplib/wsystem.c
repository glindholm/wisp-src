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

#include <stdio.h>
#include <string.h>
#include "wisplib.h"
#include "werrlog.h"

#ifdef unix
#include <signal.h>

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

int wsystem(const char* cmd)
{
	int 	rc;
	void	(*save_sig)();

	wtrace("WSYSTEM","COMMAND","%s", cmd);
	
	save_sig = signal(SIGCLD,SIG_DFL);
	rc = system(cmd);
	signal(SIGCLD,save_sig);
	return rc;
}


/*
**	Routine:	run_unixcommand_silent()
**
**	Function:	Run a unix command silently.
**
**	Description:	Run a unix command thru a pipe and trace the output.
**
**	Arguments:
**	command		The unix command string. (without output redirection)
**
**	Return:		Exit status from the command or -1 if the command fails.
**
**
*/
int run_unixcommand_silent(const char* command)
{
	int 	rc;
	void	(*save_sig)();
	FILE    *pipe;
	char	unixcommand[1024];

	sprintf(unixcommand, "%s 2>&1", command);
	
	wtrace("UNIXCOMMAND","RUN","%s",unixcommand);

	save_sig = signal(SIGCLD,SIG_DFL);

	if ((pipe = popen(unixcommand,"r")) != NULL)
	{
		char	buff[1024];
		while (fgets(buff, sizeof(buff), pipe) != NULL)
		{
			size_t i;
			i = strlen(buff);
			if (i > 0 && '\n' == buff[i-1]) 
			{
				buff[i-1] = '\0'; /* Remove trailing newline */
			}
			
			wtrace("UNIXCOMMAND","OUTPUT","%s",buff);
		}

		rc =  pclose(pipe);
	}
	else
	{
		rc = -1;
	}

	signal(SIGCLD,save_sig);

	return rc;
}

#endif /* unix */

#ifdef WIN32
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

int wsystem(const char* cmd)
{
	return win32spawnlp(NULL, cmd, SPN_WAIT_FOR_CHILD);
}

int wsystem_standalone(const char* cmd)
{
	return win32spawnlp(NULL, cmd, SPN_STANDALONE_CHILD);
}
#endif /* WIN32 */
/*
**	History:
**	$Log: wsystem.c,v $
**	Revision 1.19.2.1  2002/08/16 21:47:04  gsl
**	Alpha Port 4402f
**	
**	Revision 1.19  2001-11-01 10:43:05-05  gsl
**	In run_unixcommand_silent() remove the trailing NL
**
**	Revision 1.18  2001-10-29 10:37:04-05  gsl
**	Add include stdio.h
**
**	Revision 1.17  2001-10-26 15:43:15-04  gsl
**	Add run_unixcommand_silent()
**	Remove MSDOS
**
**	Revision 1.16  1998-05-05 13:15:54-04  gsl
**	add wsystem_standalone() to support UTILSWINDOWS
**
**	Revision 1.15  1998-03-13 17:59:20-05  gsl
**	Make const
**
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
