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
**	WL_initglbs()
*/

/*
**	Includes
*/

#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <signal.h>
#include <sys/types.h>

#include "idsistd.h"
#include "wglobals.h"
#include "werrlog.h"
#include "filext.h"
#include "wexit.h"
#include "wisplib.h"
#include "wperson.h"
#include "level.h"
#include "vwang.h"
#include "wispvers.h"
#include "wrunconf.h"
#include "platsubs.h"

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

/*
**	Call WL_initglbs() to setup for a WISP utility:
**	- Record start time
**	- Set the link level
**	- Set the the process group id
**	- Set the Run Name
**	- Set the Program Id
**	- trace the entry
*/
int WL_initglbs(const char *wisprunname)						/* Init GLOBALS				*/
											/* Call this routine from NON-COBOL	*/
											/* utilities that use WISPLIB.		*/
{
	struct wruncfg cfg;

	wisp_set_WSTARTTIME();

	WL_werr_override();								/* Check for error logging override.	*/
	WL_load_options();								/* Load the opt_xxxx variables		*/

	WL_newlevel();									/* Increment the link-level		*/
	WL_wgetpgrp();									/* Call to Set the Process Group ID.	*/

	wtrace("INITGLBS","ENTRY","RUNNAME=[%8.8s] VERSION=[%s] PLATFORM=[%s] LINKLEVEL=[%d] GID=[%d][0x%08X]",
	       wisprunname, wisp_version(), WL_platform_name(), WL_linklevel(), WL_wgetpgrp(), WL_wgetpgrp());

	wisp_set_runname(wisprunname);

	vwang_init_video();

	WL_setprogid(wisprunname);
	WSETFILEXT(" ");								/* Set file extension to spaces.	*/
	WL_set_internal_retcode(0);							/* Set RETURN-CODE to zero.		*/


	if (0 == WL_wrunconfig(&cfg))
	{
		if (0==strcmp(cfg.wrun_cobtype, WRUNCOBTYPE_ACU))
		{
			/* ACU */
		}
		else if  (0==strcmp(cfg.wrun_cobtype, WRUNCOBTYPE_MF))
		{
			/* MF */
		}
		else
		{
			wtrace("INITGLBS","COBOL","Invalid COBOL type [%s]", cfg.wrun_cobtype);
		}
	}
	

	WL_wtrace_timestamp("INITGLBS");

	return 0;
}


/*
** WINDOWS/NT Doesn't really handle signals well so we have to fake 
** a couple of them.
**
**  NOTE: WINDOWS/NT will never generate SIGTERM, SIGILL, or SIGSEGV
*/

#ifdef WIN32
#define SIGQUIT SIGTERM
#define SIGCLD	SIGILL
#define SIGBUS	SIGSEGV
#define SIGHUP	SIGTERM
#endif

static int wexit_handler_depth = 0;	/* Keep track of re-entering signal handlers */

void WL_wexitint(int sig)
{
	wexit_handler_depth++;	
	if (wexit_handler_depth > 1)
	{
		/*
		** If re-entered then we are in a loop so abort
		*/
		abort();
	}

	signal( SIGINT,  SIG_IGN );
	signal( SIGQUIT, SIG_IGN );
	werrlog(WERRCODE(74402),sig,0,0,0,0,0,0,0);					/* User signalled interrupt.		*/
	wexit(WERRCODE(74402));
}

void WL_wexitbug(int sig)
{
	wexit_handler_depth++;	
	if (wexit_handler_depth > 1)
	{
		/*
		** If re-entered then we are in a loop so abort
		*/
		abort();
	}

	signal( SIGINT,  SIG_IGN );
	signal( SIGQUIT, SIG_IGN );
	werrlog(WERRCODE(74502),sig,0,0,0,0,0,0,0);					/* Fatal signalled interrupt.		*/
	wexit(WERRCODE(74502));
}

void WL_wexitsig(int sig)
{
	wexit_handler_depth++;	
	if (wexit_handler_depth > 1)
	{
		/*
		** If re-entered then we are in a loop so abort
		*/
		abort();
	}

	/*
	**	This routine is only called when we want to terminate without further user interaction.
	**	We do however what to log the signal error so we change the w_err_flag to only write to wisperr.log
	*/
	WL_set_wispdebug(WISPDEBUG_FULL);
	werrlog(WERRCODE(74402),sig,0,0,0,0,0,0,0);					/* User signalled interrupt.		*/
	wexit(WERRCODE(74402));
}

void wisp_signal_handler(void)
{
	/*
	**	Ignore DEATH OF CHILD signals (from submitted processes).
	**	This is to stop the <DEFUNCT> zombie processes from occuring because of no "wait".
	**	SIGCLD will need to be reset to SIG_DFL around a fork-exec-wait (LINK) so that the wait will work correctly.
	*/
	signal( SIGCLD, SIG_IGN );

	WL_load_options();								/* Load the opt_xxxx variables		*/

	if (OPTION_SIGNALSOFF)
	{
		signal( SIGINT,  SIG_DFL );
		signal( SIGQUIT, SIG_DFL );
		signal( SIGILL,  SIG_DFL );
#ifdef SIGEMT
		signal( SIGEMT,  SIG_DFL );
#endif
		signal( SIGBUS,  SIG_DFL );
		signal( SIGSEGV, SIG_DFL );
	}
	else
	{
		signal( SIGINT,  WL_wexitint );
		signal( SIGQUIT, WL_wexitint );
		signal( SIGILL,  WL_wexitbug );
#ifdef SIGEMT
		signal( SIGEMT,  WL_wexitbug );
#endif
		signal( SIGBUS,  WL_wexitbug );
		signal( SIGSEGV, WL_wexitbug );
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
		signal( SIGHUP,  WL_wexitsig );
	}
}


/*
**	History:
**	$Log: initglbs.c,v $
**	Revision 1.41  2003/04/24 17:56:13  gsl
**	Comments
**	
**	Revision 1.40  2003/04/21 20:02:44  gsl
**	WL_initglbs() takes a const char*
**	
**	Revision 1.39  2003/02/07 20:45:14  gsl
**	Add platform to version display
**	
**	Revision 1.38  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.37  2002/12/10 20:54:15  gsl
**	use WERRCODE()
**	
**	Revision 1.36  2002/12/04 15:30:41  gsl
**	Cleanup ordering for first time in.
**	Load OPTIONS earlier to set WISPDEBUG earlier
**	Mark OBSOLETE args
**	
**	Revision 1.35  2002/12/03 22:15:15  gsl
**	Replace the w_err_flag bitmask with wispdebug mode that can be set to "FULL"
**	"ERRORS" or "NONE" to simplify.
**	
**	Revision 1.34  2002/11/14 13:45:34  gsl
**	Prevent recursive calls to the exit handlers routines
**	
**	Revision 1.33  2002/10/18 19:14:11  gsl
**	Cleanup
**	
**	Revision 1.32  2002/10/11 20:39:50  gsl
**	Detect runtime Cobol type without needing INITWISP call.
**	For ACU set in sub85.c,
**	For utils set via WRUNCONFIG
**	Default to MF on UNIX
**	
**	Revision 1.31  2002/09/04 18:10:46  gsl
**	LINUX SIGEMT
**	
**	Revision 1.30  2002/07/29 15:46:50  gsl
**	getwfilext -> WGETFILEXT
**	setwfilext -> WSETFILEXT
**	setwispfilext -> WSETFILEXT
**	
**	Revision 1.29  2002/07/12 19:10:11  gsl
**	Global unique WL_ changes
**	
**	Revision 1.28  2002/07/12 17:00:56  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.27  2002/07/11 20:29:09  gsl
**	Fix WL_ globals
**	
**	Revision 1.26  2002/07/11 15:21:41  gsl
**	Fix WL_ globals
**	
**	Revision 1.25  2002/07/10 21:05:17  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.24  2002/07/01 04:02:38  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.23  2002/06/25 18:18:39  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.22  2002/06/25 17:46:04  gsl
**	Remove WISPFILEXT as a global, now must go thru set/get routines
**	
**	Revision 1.21  2001/10/15 13:52:44  gsl
**	Change vwang_set_videocap() to vwang_init_video()
**	Remove WATCOM
**	
**	Revision 1.20  1998-05-14 15:04:36-04  gsl
**	Add version to trace
**
**	Revision 1.19  1998-05-12 10:53:12-04  gsl
**	change to use wtrace_timestamp()
**
**	Revision 1.18  1998-05-05 11:14:23-04  gsl
**	Add datestamp to trace message
**
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
**	Added the initialization of WSTARTTIME and WL_newlevel()
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
