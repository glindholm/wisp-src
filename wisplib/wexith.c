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
**	File:		wexith.c
**
**	Project:	WISP/LIB
**
**	Purpose:	WISP exit handler routines
**
**	Routines:	
**	wexith()	WISP exit handler
**	wispexit_cleanup() Normal exit cleanup code. (Safe to call directly)
*/

/*
**	Includes
*/

#ifdef WIN32
#include <process.h>
#endif

#include "idsistd.h"
#include "wperson.h"
#include "wfiles.h"
#include "wglobals.h"
#include "filext.h"
#include "level.h"
#include "vwang.h"
#include "sharemem.h"
#include "wisplib.h"
#include "vssubs.h"
#include "idsisubs.h"
#include "werrlog.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/
extern int WL_message_unlink();
int WL_delete_worklib(void);

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/



/*
**	ROUTINE:	WL_wexith()
**
**	FUNCTION:	Exit handler - Pre-exit cleanup
**
**	DESCRIPTION:	Preform all the pre-exit cleanup logic.
**			This process is terminating.
**
**			- Close (wfclose) any open files.
**			- Fix link levels
**			- MESSAGE exit logic
**			- Usage constants
**			- return codes
**			- LINK parameters
**			- vwang() shutdown
**
**	ARGUMENTS:	None
**
**	GLOBALS:	?
**
**	RETURN:		None
**
**	WARNINGS:	Can only be called once so we had better 
**			really exit after called.
**
*/
void WL_wexith(void)									/* This is the WISP exit handler.	*/
{											/* It is installed by WFOPEN, and refed	*/
	static int already = 0;								/* Already done?			*/
	wisp_fstruct *temp_file;

	if (already) return;								/* Already been here.			*/
	else	already = 1;

	WL_wtrace("WEXITH", "ENTRY", "Performing pre-exit cleanup"); 

	/*
	**	Delete all IS_SCRATCH files
	*/
	while(WL_g_temp_file_list)
	{
		int4 	rc;
	
		/*
		**	Take this item of the list first before processing because
		**	SCRATCH calls wfname() which can alter the list.
		*/
		temp_file = WL_g_temp_file_list;
		WL_g_temp_file_list = WL_g_temp_file_list->nextfile;

		WL_set_va_count(5);
		SCRATCH("F", temp_file->file, temp_file->lib, temp_file->vol, &rc);
		free(temp_file);
	}

	WFCLOSE("*");									/* Ask close routine to spool everything.*/


	/*
	**	This changes the link-level so place logic before or after this
	**	point if it needs to know link-level.
	*/
	WL_oldlevel();									/* Decrement the link-level		*/
	WL_ppunlink(WL_linklevel());								/* Putparm UNLINK			*/


	/* 
		Do PROGLIB and PROGVOL cleanup
	*/
	WL_restore_progdefs();

	if (wisp_get_LINKPARM())
	{
		/* 
			If came in from a LINK then restore args		
		*/

		LINKPARG();

		/* 
			If came in from a LINK then don't clear	
		*/
		vwang_noclear_on_shut();
	}

	/*
	**	Cleanup
	*/
	wispexit_cleanup();

	/*
	**	Shut down vwang
	*/
	vwang_shut();
}

/*
**	ROUTINE:	wispexit_cleanup()
**
**	FUNCTION:	Normal exiting cleanup code (Safe to call directly)
**
**	DESCRIPTION:	Called before process exits to cleanup.
**			MUST DECREMENT LINK-LEVEL BEFORE CALLING.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	The link level
**
**	RETURN:		None
**
**	WARNINGS:	MUST DECREMENT LINK-LEVEL BEFORE CALLING.
**
*/
void wispexit_cleanup(void)
{

	/* 
	**	Delete message ports	
	*/
	WL_message_unlink();

	/*
	**	When exiting the highest link level then delete the worklib
	**	(unless the KEEPWORKLIB option is set).
	*/
	if (WL_linklevel() < 1)
	{
		if (!WL_get_wisp_option("KEEPWORKLIB"))
		{
			WL_delete_worklib();
		}
	}

#ifdef WIN32
	/*
	**	If this is the top level process then delete
	**	the temp files.
	*/
	if (_getpid() == WL_wgetpgrp()) /* If TOP level process */
	{
		WL_delete_defaults_temp();
		WL_delete_retcode();
	}
#endif
}

/*
**	ROUTINE:	WL_delete_worklib()
**
**	FUNCTION:	Delete the worklib directory
**
**	DESCRIPTION:	Call SCRATCH to delete the worklib directory.
**			This would normally only be called when exiting the
**			the highest level process.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:	       
**	0		The WORKLIB was deleted or didn't exist.
**	1		The WORKLIB still exists.
**
**	WARNINGS:	None
**
*/
int WL_delete_worklib(void)
{
	char	workvol[7], worklib[9], workfile[9];
	int4	retcode;

	WL_get_defs(DEFAULTS_WV,workvol);
	WL_get_defs(DEFAULTS_WL,worklib);
	memset(workfile,' ',sizeof(workfile));

	WL_set_va_count(5);
	SCRATCH("L", workfile, worklib, workvol, &retcode);
	WL_wswap(&retcode);
	switch(retcode)
	{
	case 0:
	case 4:
	case 16:
		return 0;
	default:
		return 1;
	}
}

/*
**	History:
**	$Log: wexith.c,v $
**	Revision 1.37  2011/09/04 21:12:54  gsl
**	fix win32 IOS C++ warnings
**	
**	Revision 1.36  2011/08/26 00:57:02  gsl
**	tracing
**	
**	Revision 1.35  2011/08/25 23:40:00  gsl
**	tracing
**	
**	Revision 1.34  2003/02/17 22:07:17  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.33  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.32  2002/12/11 20:33:37  gsl
**	Enhance tracing of runtype
**	
**	Revision 1.31  2002/07/23 02:57:51  gsl
**	wfclose -> WFCLOSE
**	
**	Revision 1.30  2002/07/19 22:07:14  gsl
**	Renaming cobol api routines for global uniqueness
**	
**	Revision 1.29  2002/07/12 19:10:18  gsl
**	Global unique WL_ changes
**	
**	Revision 1.28  2002/07/12 17:01:02  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.27  2002/07/11 20:29:16  gsl
**	Fix WL_ globals
**	
**	Revision 1.26  2002/07/11 15:21:44  gsl
**	Fix WL_ globals
**	
**	Revision 1.25  2002/07/10 21:05:30  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.24  2002/07/09 04:13:54  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.23  2002/07/01 04:02:42  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.22  2001/11/27 20:46:31  gsl
**	Remove VMS
**	
**	Revision 1.21  1999-01-29 19:06:32-05  gsl
**	Change the logic that deletes IS_SCRATCH files to use SCRATCH instead
**	of unlink() to ensure that two-part files are deleted (x.dat + x.idx).
**	Add logic to wispext_cleanup() to delete the WORKLIB when exiting the
**	highest link-level unless the KEEPWORKLIB option is set.
**
**	Revision 1.20  1998-10-22 14:08:17-04  gsl
**	Fix the g_temp_file_list processing so the list gets freed and nulled.
**
**	Revision 1.19  1998-07-31 15:50:06-04  gsl
**	Change NAME_LENGTH to COB_FILEPATH_LEN
**
**	Revision 1.18  1996-12-11 16:26:18-05  gsl
**	Add delete_worklib() routine which automatically gets called
**	on an exit from WIN32
**
**	Revision 1.17  1996-11-11 15:30:06-08  gsl
**	Declare message_unlink()
**
**	Revision 1.16  1996-11-11 15:19:29-08  gsl
**	Restored lost changes from 1.13 plus add wispexit_cleanup()
**
**	Revision 1.13  1996-08-26 17:10:45-07  gsl
**	Documented and centralized all the pre-exit logic
**
**	Revision 1.12  1996-08-19 15:33:12-07  gsl
**	drcs update
**
**
**
*/
