			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

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

int wsystem(cmd)
char *cmd;
{
	int 	rc;
	void	(*save_sig)();

	save_sig = signal(SIGCLD,SIG_DFL);
	rc = system(cmd);
	signal(SIGCLD,save_sig);
	return rc;
}
