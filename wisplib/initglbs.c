static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		initglbs.c
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
**
**	Purpose:	Initialize wisp runtime routines for non-COBOL runtimes
**
**	Routines:	
**	initglbs()
*/

/*
**	Includes
*/

#include <stdlib.h>
#include <string.h>

#if defined(unix) || defined(WIN32)
#include <signal.h>
#include <sys/types.h>
#endif

#include "idsistd.h"
#include "wglobals.h"
#include "werrlog.h"
#include "filext.h"
#include "cobrun.h"
#include "wexit.h"
#include "wisplib.h"
#include "wperson.h"
#include "level.h"
#include "vwang.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/

int initglbs(char *wisprunname)								/* Init GLOBALS				*/
											/* Call this routine from NON-COBOL	*/
											/* utilities that use WISPLIB.		*/
{
	vwang_set_videocap();
	
#ifdef WATCOM
	init_watcom();
#endif

	werrset();									/* get runtime w_err_flag override.	*/

	time(&WSTARTTIME);

	newlevel();									/* Increment the link-level		*/
	wgetpgrp();									/* Call to Set the Process Group ID.	*/

	wtrace("INITGLBS","ENTRY","RUNNAME=[%8.8s] LINKLEVEL=[%d] GID=[%d][0x%08X]",
	       wisprunname, linklevel(), wgetpgrp(), wgetpgrp());

	memcpy(WISPRUNNAME,wisprunname,8);
	setprogid(wisprunname);
	memset(WISPFILEXT,' ',39);							/* Set file extension to spaces.	*/
	memset(WISPRETURNCODE,'0',3);							/* Set RETURN-CODE to zero.		*/
	strcpy(WISPTRANVER,"INITGLBS");							/* Set the translator version		*/

	load_options();									/* Load the opt_xxxx variables		*/

	if (opt_errflag_found)
	{
		w_err_flag = opt_errflag;
	}
	werrset();									/* get runtime w_err_flag override.	*/

	return 0;
}


#ifdef WATCOM
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int init_watcom(void)
{
#ifdef HOLD_FOR_HOW
	/*
	**	For WATCOM set the default mode for open() and fopen() to be BINARY
	*/
	_fmode = O_BINARY;
#endif
	return 0;
}
#endif

#if defined(unix) || defined(WIN32)

/*
** WINDOWS/NT Doesn't really handle signals well so we have to fake 
** a couple of them.
**
**  NOTE: WINDOWS/NT will never generate SIGTERM, SIGILL, or SIGSEGV
*/

#ifdef WIN32
#define SIGQUIT SIGTERM
#define SIGCLD	SIGILL
#define SIGEMT	SIGILL
#define SIGBUS	SIGSEGV
#define SIGHUP	SIGTERM
#endif

void wexitint(int sig)
{
#undef		ROUTINE
#define		ROUTINE		74400

	signal( SIGINT,  SIG_IGN );
	signal( SIGQUIT, SIG_IGN );
	werrlog(ERRORCODE(2),sig,0,0,0,0,0,0,0);					/* User signalled interrupt.		*/
	wexit(ERRORCODE(2));
}

void wexitbug(int sig)
{
#undef		ROUTINE
#define		ROUTINE		74500
	signal( SIGINT,  SIG_IGN );
	signal( SIGQUIT, SIG_IGN );
	werrlog(ERRORCODE(2),sig,0,0,0,0,0,0,0);					/* Fatal signalled interrupt.		*/
	wexit(ERRORCODE(2));
}

void wexitsig(int sig)
{
#undef		ROUTINE
#define		ROUTINE		74400
	/*
	**	This routine is only called when we want to terminate without further user interaction.
	**	We do however what to log the signal error so we change the w_err_flag to only write to wisperr.log
	*/
	w_err_flag = ENABLE_LOGGING + LOG_LOGFILE;
	werrlog(ERRORCODE(2),sig,0,0,0,0,0,0,0);					/* User signalled interrupt.		*/
	wexit(ERRORCODE(2));
}

void wisp_signal_handler(void)
{
	/*
	**	Ignore DEATH OF CHILD signals (from submitted processes).
	**	This is to stop the <DEFUNCT> zombie processes from occuring because of no "wait".
	**	SIGCLD will need to be reset to SIG_DFL around a fork-exec-wait (LINK) so that the wait will work correctly.
	*/
	signal( SIGCLD, SIG_IGN );

	load_options();									/* Load the opt_xxxx variables		*/

	if (opt_signalsoff)
	{
		signal( SIGINT,  SIG_DFL );
		signal( SIGQUIT, SIG_DFL );
		signal( SIGILL,  SIG_DFL );
		signal( SIGEMT,  SIG_DFL );
		signal( SIGBUS,  SIG_DFL );
		signal( SIGSEGV, SIG_DFL );
	}
	else
	{
		signal( SIGINT,  wexitint );
		signal( SIGQUIT, wexitint );
		signal( SIGILL,  wexitbug );
		signal( SIGEMT,  wexitbug );
		signal( SIGBUS,  wexitbug );
		signal( SIGSEGV, wexitbug );
	}

	if (!wbackground())
	{
		/*
		**	We need to catch SIGHUP because it does not properly kill the process.
		**	It the user closes a window the process was being sent a SIGHUP but this
		**	was NOT killing the process, it only terminated the terminal-read with
		**	an error.  The process then tried to report the error with a werrlog()
		**	"Press and key to continue." box which hung the process waiting for
		**	a response that would never come.
		*/
		signal( SIGHUP,  wexitsig );
	}
}

#endif

/*
**	History:
**	$Log: initglbs.c,v $
**	Revision 1.17  1997-07-16 15:07:05-04  gsl
**	Change to match initwisp2() order
**	Add wtrace()
**
**	Revision 1.16  1997-05-02 20:03:18-04  gsl
**	Add a trace
**
**	Revision 1.15  1996-11-04 18:56:07-05  gsl
**	Add call to vwang_set_videocap();
**
**	Revision 1.14  1996-09-04 17:02:44-07  gsl
**	missing include file
**
**	Revision 1.13  1996-08-29 17:08:22-07  gsl
**	Added the initialization of WSTARTTIME and newlevel()
**
**	Revision 1.12  1996-08-22 17:24:51-07  gsl
**	Call wgetpgrp() at start to init gid
**
**	Revision 1.11  1996-06-28 09:04:04-07  gsl
**	add missing includes and combine the unix and NT signal handling code
**
**	Revision 1.10  1995-08-25 04:29:28-07  gsl
**	New routine wisp_signal_handler() created from the signal logic in
**	initwisp.c.
**	Added standard headers
**
**
**
*/
