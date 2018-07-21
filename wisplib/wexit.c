/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
*/

/*
**	File:		wexit.c
**
**	Project:	WISP/LIB
**
**	Purpose:	Comment exit point
**
**	Routines:	
**	WL_wexit()	Perform cleanup then terminal process
*/

/*
**	Includes
*/

#include "idsistd.h"
#include "werrlog.h"
#include "wglobals.h"
#include "wexit.h"
#include "cobrun.h"
#include "vwang.h"
#include "wisplib.h"
#include "level.h"

extern void WL_shutexitcobol(int exit_code);

/*
**	ROUTINE:	WL_wexit()
**
**	FUNCTION:	Preform cleanup then terminate the process
**
**	DESCRIPTION:	If coming from a COBOL app then this would
**			be an abnormal termination.
**			If called from a utility this is a normal exit.
**			Set LINKCOMPCODE=16 since we are bailing
**			Clean up the exit code 
**			Call wexith() to do pre-exit processing
**			Exit
**
**	ARGUMENTS:	
**	num		The exit code
**
**	GLOBALS:	
**	run_cobol	Flag if running from a cobol program
**	LINKCOMPCODE	The link completion code
**
**	RETURN:		Doesn't return
**
**	WARNINGS:	None
**
*/
void WL_wexit(int4 exit_code)
{
	WL_wtrace("WEXIT", "ENTRY", "wexit(%ld) linklevel=%d", (long)exit_code, WL_linklevel()); 

	if (0 == exit_code)
	{
		wisp_set_LINKCOMPCODE(0);						/* Non-error termination of LINK	*/
	}
	else
	{
		wisp_set_LINKCOMPCODE(16);						/* Abnormal termination of LINK		*/
	}
	

	WISPEXIT();
	WL_wtrace_timestamp("WEXIT");

	if (wisp_acu_cobol() || wisp_mf_cobol())
	{
		WL_shutexitcobol(exit_code);	/* this is the COBOL specific routine to shutdown cobol and exit		*/
	}
	else
	{
		exit(exit_code);
	}
}

/*
**	History:
**	$Log: wexit.c,v $
**	Revision 1.28  2011/08/25 23:39:30  gsl
**	tracing
**	
**	Revision 1.27  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.26  2002/12/10 17:09:14  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.25  2002/12/09 21:09:33  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.24  2002/10/18 19:14:07  gsl
**	Cleanup
**	
**	Revision 1.23  2002/07/19 22:07:14  gsl
**	Renaming cobol api routines for global uniqueness
**	
**	Revision 1.22  2002/07/12 19:10:18  gsl
**	Global unique WL_ changes
**	
**	Revision 1.21  2002/07/11 15:21:44  gsl
**	Fix WL_ globals
**	
**	Revision 1.20  2002/07/10 21:05:30  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.19  2002/07/09 04:13:54  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.18  2002/07/02 04:00:37  gsl
**	change acu_cobol and mf_cobol to wisp_acu_cobol() and wisp_mf_cobol()
**	
**	Revision 1.17  2002/07/01 04:02:42  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.16  2002/06/21 03:10:44  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.15  1998/05/12 14:53:36  gsl
**	Added wtrace_timestamp()
**	
**	Revision 1.14  1997-05-02 20:03:32-04  gsl
**	Add wtrace()
**
**	Revision 1.13  1996-11-11 19:39:47-05  gsl
**	If exit number is zero then is not a abnormal termination
**
**	Revision 1.12  1996-08-26 17:10:15-07  gsl
**	Documented and moved pre-exit logic to wexith()
**
**	Revision 1.11  1996-08-19 15:33:11-07  gsl
**	drcs update
**
*/
