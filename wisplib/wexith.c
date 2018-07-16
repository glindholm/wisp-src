static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wexith.c
**
**	Project:	WISP/LIB
**
**	RCS:		$Source:$
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
#include "que_jobs.h"
#include "wperson.h"
#include "wfiles.h"
#include "wglobals.h"
#include "filext.h"
#include "level.h"
#include "vwang.h"
#include "sharemem.h"
#include "wisplib.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/
#if defined(unix) || defined(WIN32)
extern int message_unlink();
int delete_worklib(void);
#endif

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/



/*
**	ROUTINE:	wexith()
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
void wexith(void)									/* This is the WISP exit handler.	*/
{											/* It is installed by WFOPEN, and refed	*/
	static int already = 0;								/* Already done?			*/
	int i;										/* vuserexit.				*/
	char fname[NAME_LENGTH+10];

	if (already) return;								/* Already been here.			*/
	else	already = 1;

	/*
	**	Delete all scratch files
	*/
	if (flist)									/* if any files were opened for scratch	*/
	{
		flptr = flist;
		do
		{
			i = 0;
			do
			{
				fname[i] = flptr->name[i];				/* copy the file name			*/
				i++;
			} while ((flptr->name[i] != ' ') && (i < NAME_LENGTH));
			fname[i] = '\0';
			strcat(fname,";");						/* add a semicolon			*/
			unlink(fname);							/* try to delete it			*/
			flptr = (fstruct *)flptr->nextfile;
		} while (flptr);
	}

	wfclose("*");									/* Ask close routine to spool everything.*/

	oldlevel();									/* Decrement the link-level		*/
	ppunlink(linklevel());								/* Putparm UNLINK			*/


	/* 
		Do PROGLIB and PROGVOL cleanup
	*/
	restoreprogdefs();

#ifdef VMS
	setretcode(WISPRETURNCODE);
#endif

	if (LINKPARM)
	{
		/* 
			If came in from a LINK then restore args		
		*/

#if defined(unix) || defined(WIN32)
		LINKPARG();
#endif
#ifdef VMS
		VMSPARGS();
#endif
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
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void wispexit_cleanup(void)
{

#if defined(unix) || defined(WIN32)
	/* 
		Delete message ports	
	*/
	message_unlink();
#endif

#ifdef WIN32
	/*
	**	If this is the top level process then delete
	**	the temp files.
	*/
	if (getpid() == wgetpgrp()) /* If TOP level process */
	{
		delete_defaults_temp();
		delete_retcode();
		delete_worklib();
	}
#endif

#ifdef VMS
	/* 
		Cleanup the Share mem file.		
	*/
	cleanup_shrfil();
#endif

}

/*
**	ROUTINE:	delete_worklib()
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
int delete_worklib(void)
{
	char	workvol[7], worklib[9], workfile[9];
	int4	retcode;
	int4	args;
	
	get_defs(DEFAULTS_WV,workvol);
	get_defs(DEFAULTS_WL,worklib);

	args=5;
	wvaset(&args);
	SCRATCH("L", workfile, worklib, workvol, &retcode);
	wswap(&retcode);
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
