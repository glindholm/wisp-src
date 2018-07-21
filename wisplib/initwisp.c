/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
**	File:		initwisp.c
**
**	Project:	wisp/lib
**
**	Purpose:	Initialize the WISP runtime
**
**	Routines:	
**	INITWISP3()	The COBOL called routine to do initialization.
*/

#if !(defined(unix) || defined(WIN32))

#error	'This will not compile as unix || WIN32 are not defined!!!'
#error	'This will not compile as unix || WIN32 are not defined!!!'
#error	'This will not compile as unix || WIN32 are not defined!!!'

#endif

/*
**	Includes
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <sys/types.h>

#include "idsistd.h"
#include "werrlog.h"
#include "wdefines.h"
#include "wcommon.h"

#include "vwang.h"
#include "wglobals.h"
#include "filext.h"
#include "wperson.h"
#include "wisplib.h"
#include "wexit.h"
#include "level.h"
#include "machid.h"
#include "wsb.h"
#include "wispvers.h"
#include "platsubs.h"
#include "wanguid.h"

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

static int license_checked(int set);
void WL_license_warning(void);


/*
	INITWISP3() - called as the first action in COBOL

	- Sets the error logging level
	- Sets the runname
	- Sets the PROGRAM-ID
	- Sets the Start time
	- Sets the link-level
	- Sets the noswap option
	- Checks the license
	- Installs the signal handler

*/
	
void INITWISP3(	char	wisp_tran_version[WISP_VERSION_SIZE],	/* The TRANSLATOR version		*/
		char	wisp_application_name[8],	/* The name of the program		*/
		char	wisprunname[8],			/* The first appl name this run unit	*/
		int4	*swap_on)			/* Swap_on == 0 forces swaping off	*/
{
	static  int first = 1;

	if (first)
	{
		first = 0;

		wisp_set_WSTARTTIME();

#if defined(DEBUG) || defined(_DEBUG)
#ifdef WIN32
		if (getenv("WISPDEBUGBREAK"))
		{
			extern void vrawDebugBreak(void);

			vrawDebugBreak();
		}
#endif /* WIN32 */
#endif /* DEBUG */


		WL_werr_override();	/* Check for error logging override.	*/
		WL_load_options();

		WL_newlevel();		/* Increment the link-level		*/
		WL_wgetpgrp();		/* Call to Set the Process Group ID.	*/

		wtrace("INITWISP3","FIRST","APPNAME=[%8.8s] LINKLEVEL=[%d] GID=[%d][0x%08X] COBOL=[%s%s] PLAT=[%s] USER=[%s]",
		       wisp_application_name, WL_linklevel(), WL_wgetpgrp(), WL_wgetpgrp(),
		       (wisp_acu_cobol()?"ACU":""),(wisp_mf_cobol()?"MF":""), 
		       WL_platform_name(), WL_longuid());
		WL_wtrace_timestamp("INITWISP3");

		wisp_set_runname(wisp_application_name);

		if (!WL_wbackground())
		{
			vwang_pre_init();
		}

		WSETFILEXT(" ");								/* Set file extension to spaces.	*/
		WL_set_internal_retcode(0);							/* Set RETURN-CODE to zero.		*/
		wisp_set_progname("");
		wisp_set_screenname("");

		if (wisp_get_noswap() && (*swap_on == 1))					/* Swapping was off, now they want it on*/
		{
			werrlog(WERRCODE(24002),0,0,0,0,0,0,0,0);
		}
		wisp_set_noswap( ! *swap_on );							/* Set/reset the flags.			*/

		WL_license_warning();								/* Check the WISP license		*/

		WL_save_progdefs();								/* Save PROGLIB/PROGVOL values		*/

		/*
		**	Set up the standard wisp signal handling
		*/
		wisp_signal_handler();
	}

	/*=========================================================================*/

	if( memcmp(wisp_get_runname(),"ACULINK ",WISP_RUNNAME_SIZE) == 0 ||	/* Reset RUNNAME only if ACULINK	*/
	    memcmp(wisp_get_runname(),"ACUUSING",WISP_RUNNAME_SIZE) == 0    )	/* 		      or ACUUSING	*/
	{
		wisp_set_runname(wisp_application_name);			/* Set the RUN name in C.		*/
	}
	memcpy(wisprunname,wisp_get_runname(),WISP_RUNNAME_SIZE);		/* Set the COBOL wisprunname		*/
	WL_setprogid(wisp_application_name);					/* Set the program id in C.		*/

	wtrace("INITWISP3","ENTRY",
	       "APPNAME=[%8.8s] RUNNAME=[%8.8s] LEVEL=[%d] GID=[%d][0x%08X] VER=[%s] TRAN=[%20.20s]",
	       wisp_application_name, wisp_get_runname(), WL_linklevel(), WL_wgetpgrp(), WL_wgetpgrp(),
	       wisp_version(), wisp_tran_version);

	/*=========================================================================*/

}

void INITWISP2(	char	wisp_tran_version[20],		/* The TRANSLATOR version		*/
		char	wisp_lib_version[1],		/* The LIBRARY version	(OBSOLETE)	*/
		char	cobol_type[3],			/* The type of COBOL	(OBSOLETE)	*/
		char	wisp_application_name[8],	/* The name of the program		*/
		char	wisprunname[8],			/* The first appl name this run unit	*/
		int4	*swap_on,			/* Swap_on == 0 forces swaping off	*/
		int4	*err_flag)			/* (OBSOLETE)				*/
{
	INITWISP3(wisp_tran_version, wisp_application_name, wisprunname, swap_on);
}


/*
**	Routine:	WL_license_warning()
**
**	Function:	To check the WISP license and issue warning messages if not valid.
**
**	Description:	This routine calls WLIC_validate_license() to check if the WISP license has been installed
**			and is valid.  If not installed it issues a warning.  If timed out or invalid it issues
**			an error and exits.
**			No checking is done if in background.
**
**	Input:		None
**			
**
**	Output:		None
**			
**
**	Return:		None
**
**	Warnings:	This routine will not return if the license is invalid or timed out.
**
**	History:	05/27/92	Written by GSL
**			05/28/92	Added the license_checked() routine GSL
**
*/

#include "wlicense.h"

void WL_license_warning(void)
{
	int pfkey, currow, curcol;
	HWSB	hWsb;
	int	code;
	char	version_mess[80];
	char	platform_mess[80];
	char	*contact_message;
	int	row;
	char	*license_check_mess = "UNKNOWN";

	if (wbackground()) return;						/* don't check in background			*/

	if (license_checked(0)) return;						/* If license already checked then return	*/

	code = WLIC_validate_license();						/* Validate the license file			*/

	if (code == LICENSE_CHECK_OK) 
	{
		license_checked(1);						/* Mark the license as checked.			*/
		return;
	}

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	wsb_add_text(hWsb,1,0,"****  WISP License Information  ****");
	wsb_add_text(hWsb,3,0,"Copyright (c) 1989-" WISP_COPYRIGHT_YEAR_STR " Shell Stream Software LLC");
	wsb_add_text(hWsb,4,0,"Web: " WISP_WEBSITE "  Email: " WISP_EMAIL);

	sprintf(version_mess ,"WISP Version %s", wisp_version());
	sprintf(platform_mess,"%s (%2.2s - %s)", 
		WL_platform_name(), WL_platform_code(), WL_platform_define());
	wsb_add_text(hWsb,6,0,version_mess);
	wsb_add_text(hWsb,7,0,platform_mess);

	contact_message = "Please contact your WISP vendor for assistance.";

	row = 12;

	switch (code)
	{
	case LICENSE_CHECK_MISSING:
		license_check_mess = "MISSING";
		wsb_add_text(hWsb,row++,0,"****  WARNING  ****");
		row++;
		wsb_add_text(hWsb,row++,0,"This machine has not been licensed to use the WISP runtime system.");
		wsb_add_text(hWsb,row++,0,"Please run the wlicense program to install the WISP license.");
		row++;
		wsb_add_text(hWsb,row++,0,"****  WARNING  ****");
		wsb_add_text(hWsb,24,0,"Press (ENTER) to continue.");
		break;
	case LICENSE_CHECK_TIMEDOUT:
		license_check_mess = "TIMEDOUT";
		wsb_add_text(hWsb,row++,0,"****  TIMED OUT  ****");
		row++;
		wsb_add_text(hWsb,row++,0,"The WISP demo license for this machine has timed out.");
		wsb_add_text(hWsb,row++,0,contact_message);
		row++;
		wsb_add_text(hWsb,row++,0,"****  TIMED OUT  ****");
		wsb_add_text(hWsb,24,0,"Press (ENTER) to EXIT.");
		break;
	case LICENSE_CHECK_VERSION:
		license_check_mess = "VERSION";
		wsb_add_text(hWsb,row++,0,"****  INVALID LICENSE VERSION  ****");
		row++;
		wsb_add_text(hWsb,row++,0,"The WISP license version is invalid for this release.");
		wsb_add_text(hWsb,row++,0,contact_message);
		row++;
		wsb_add_text(hWsb,row++,0,"****  INVALID LICENSE VERSION  ****");
		wsb_add_text(hWsb,24,0,"Press (ENTER) to EXIT.");
		break;
	case LICENSE_CHECK_UNLIMITED:
		license_check_mess = "UNSUPPORTED";
		wsb_add_text(hWsb,row++,0,"****  UNSUPPORTED LICENSE  ****");
		row++;
		wsb_add_text(hWsb,row++,0,"An unsupported WISP license type (UNLIMITED) has been detected.");
		wsb_add_text(hWsb,row++,0,contact_message);
		row++;
		wsb_add_text(hWsb,row++,0,"****  UNSUPPORTED LICENSE  ****");
		wsb_add_text(hWsb,24,0,"Press (ENTER) to EXIT.");
		break;
	case LICENSE_CHECK_INVALID:
	case LICENSE_CHECK_UNKNOWN:
	default:
		license_check_mess = "INVALID";
		wsb_add_text(hWsb,row++,0,"****  INVALID LICENSE  ****");
		row++;
		wsb_add_text(hWsb,row++,0,"An invalid WISP runtime license has been installed on this machine.");
		wsb_add_text(hWsb,row++,0,"Please run the wlicense program to install the correct WISP license.");
		row++;
		wsb_add_text(hWsb,row++,0,"****  INVALID LICENSE  ****");
		wsb_add_text(hWsb,24,0,"Press (ENTER) to EXIT.");
		break;
	}

	WL_wtrace("LICENSE", "ERROR", "WISP License check failed [%s]",license_check_mess);
	
	wsb_display_and_read(hWsb, "001216X", &pfkey, &currow, &curcol);
	wsb_delete(hWsb);

	if (code == LICENSE_CHECK_MISSING) 					/* only a warning				*/
	{
		license_checked(1);						/* Mark the license as checked			*/
		return;
	}

	wexit(0);								/* INVALID WISP LICENSE - EXIT			*/
}

/*
**	Routine:	license_checked()
**
**	Function:	To mark that the license has been checked.
**			To test if the license has been checked.
**
**	Description:	If "set" is true then this routine will set a shell 
**			variable WISPLICENSE equal to a "munged" Group Id.  This
**			indicates that the license has been checked.
**			If "set" is false then it will check if the license has 
**			been checked by examining the shell var and seeing if the 
**			munged group id matches.
**
**			The MSDOS version uses the Machine Id instead of the GID.
**
**	Input:		set		flag to set the shell variable
**			
**
**	Output:		WISPLICENSE - shell variable
**			
**
**	Return:		0 - license has not been checked
**			1 - license has been checked
**
**	Warnings:	If the user is not using the Bourne Shell and doesn't have WISPGID set then this will not work and 
**			the WISP license file will be read and checked at every link level.
**
**	History:	
**	05/28/92	Written by GSL
**	07/07/93	Added MSDOS version. GSL
**
*/

static int license_checked(int set)
{
	char	*ptr;
	char	buff[128];
	char	mid[80];

	{
		int	gid;
		int4	mask;
		
		gid = WL_wgetpgrp();	/* get the Process Group ID.			*/

		/*
		 *	Mung the GID so it isn't recognizable.
		 *	Create a mask of 9's and subtract the gid.
		 *	The mask has leading digits based on it's size.
		 */
		if (gid < 0)
		{
			gid = -gid;
		}

		if (gid < 1000)
		{
			mask = 123456999;
		}
		else if (gid < 10000)
		{
			mask = 123459999;
		}
		else if (gid < 100000)
		{
			mask = 123499999;
		}
		else if (gid < 1000000)
		{
			mask = 123999999;
		}
		else if (gid < 10000000)
		{
			mask = 129999999;
		}
		else if (gid < 100000000)
		{
			mask = 199999999;
		}
		else
		{
			mask = 999999999;
			while (gid > mask)
			{
				gid = gid / 10;
			}
		}

		sprintf(mid,"%d",mask-gid);
	}

	if (set)
	{
		sprintf(buff,"WISPLICENSE=%s",mid);				/* set env variable - message was displayed	*/
		WL_setenvstr(buff);
		return(1);
	}
	else
	{
		if ((ptr = getenv("WISPLICENSE")))				/* see if license already checked 		*/
		{
			if (0==strcmp(ptr,mid)) return(1);			/* If WISPLICENSE is equal to machine id - OK	*/
		}
		return(0);							/* License has not been checked			*/
	}
}


/*
**	History:
**	$Log: initwisp.c,v $
**	Revision 1.67  2009/10/18 20:45:31  gsl
**	Copyright
**	
**	Revision 1.66  2003/06/13 17:36:12  gsl
**	ENTERPRISE License
**	
**	Revision 1.65  2003/06/12 20:54:29  gsl
**	Add support for ENTERPRISE licenses with a version number and remove
**	support for UNLIMITED license.
**	
**	Revision 1.64  2003/04/28 15:31:09  gsl
**	Add version and platform to license warning screen
**	
**	Revision 1.63  2003/04/04 20:05:01  gsl
**	missing include
**	
**	Revision 1.62  2003/04/04 19:41:43  gsl
**	Streamline the traceing
**	
**	Revision 1.61  2003/03/07 20:11:37  gsl
**	Use defines for all the field sizes passed from Cobol to C
**	
**	Revision 1.60  2003/02/07 20:45:14  gsl
**	Add platform to version display
**	
**	Revision 1.59  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.58  2003/01/31 21:40:59  gsl
**	Fix -Wall warnings
**	
**	Revision 1.57  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.56  2002/12/12 19:57:00  gsl
**	INITWISP3
**	
**	Revision 1.55  2002/12/10 20:54:14  gsl
**	use WERRCODE()
**	
**	Revision 1.54  2002/12/04 15:30:41  gsl
**	Cleanup ordering for first time in.
**	Load OPTIONS earlier to set WISPDEBUG earlier
**	Mark OBSOLETE args
**	
**	Revision 1.53  2002/12/04 14:39:26  gsl
**	Cleanup ordering for first time in.
**	Load OPTIONS earlier to set WISPDEBUG earlier
**	Mark OBSOLETE args
**	
**	Revision 1.52  2002/12/03 22:15:14  gsl
**	Replace the w_err_flag bitmask with wispdebug mode that can be set to "FULL"
**	"ERRORS" or "NONE" to simplify.
**	
**	Revision 1.51  2002/11/06 20:41:44  gsl
**	Change address to Suite 402
**	
**	Revision 1.50  2002/10/18 19:14:10  gsl
**	Cleanup
**	
**	Revision 1.49  2002/10/11 20:39:51  gsl
**	Detect runtime Cobol type without needing INITWISP call.
**	For ACU set in sub85.c,
**	For utils set via WRUNCONFIG
**	Default to MF on UNIX
**	
**	Revision 1.48  2002/07/31 21:00:30  gsl
**	globals
**	
**	Revision 1.47  2002/07/29 15:46:50  gsl
**	getwfilext -> WGETFILEXT
**	setwfilext -> WSETFILEXT
**	setwispfilext -> WSETFILEXT
**	
**	Revision 1.46  2002/07/12 19:10:12  gsl
**	Global unique WL_ changes
**	
**	Revision 1.45  2002/07/12 17:00:57  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.44  2002/07/11 20:29:09  gsl
**	Fix WL_ globals
**	
**	Revision 1.43  2002/07/11 15:21:41  gsl
**	Fix WL_ globals
**	
**	Revision 1.42  2002/07/10 21:05:17  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.41  2002/07/09 04:14:01  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.40  2002/07/02 21:15:25  gsl
**	Rename wstrdup
**	
**	Revision 1.39  2002/07/02 04:08:03  gsl
**	Cleanup cobol types
**	
**	Revision 1.38  2002/07/01 04:02:38  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.37  2002/06/26 20:52:16  gsl
**	Fix phone number
**	
**	Revision 1.36  2002/06/25 18:18:39  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.35  2002/06/25 17:46:04  gsl
**	Remove WISPFILEXT as a global, now must go thru set/get routines
**	
**	Revision 1.34  2002/06/21 20:49:28  gsl
**	Rework the IS_xxx bit flags and the WFOPEN_mode flags
**	
**	Revision 1.33  2002/06/21 03:49:29  gsl
**	Remove LPI & DMF & softlock
**	
**	Revision 1.32  2002/06/21 03:10:36  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.31  2002/03/28 14:44:57  gsl
**	Use define for copyright year
**	
**	Revision 1.30  2002-03-26 16:33:34-05  gsl
**	(C) 2002
**	Remove VMS and MSDOS
**
**	Revision 1.29  2001-09-05 14:43:09-04  gsl
**	Change copyright date
**
**	Revision 1.28  2000-03-16 10:25:36-05  gsl
**	2000
**
**	Revision 1.27  1999-09-13 15:47:49-04  gsl
**	update copyright
**
**	Revision 1.26  1999-08-23 09:45:26-04  gsl
**	Fix a potential security problem byt munging the WISPLICENSE variable.
**
**	Revision 1.25  1998-05-14 15:04:17-04  gsl
**	Add version to trace
**
**	Revision 1.24  1998-05-12 10:52:51-04  gsl
**	Change to use wtrace_timestamp()
**
**	Revision 1.23  1998-05-08 15:03:53-04  gsl
**	Add WIN32 debugging break
**
**	Revision 1.22  1997-10-29 11:57:09-05  gsl
**	fix cursor col position
**
**	Revision 1.21  1997-10-29 11:55:39-05  gsl
**	Changed license_warning() to use the WSB generic screen handler
**
**	Revision 1.20  1997-10-23 16:41:00-04  gsl
**	FIx (ENTER) key tags
**
**	Revision 1.19  1997-07-16 15:06:04-04  gsl
**	Change the ENTRY logging into wtrace() and include more vital info
**	including the GID and the LINKLEVEL
**
**	Revision 1.18  1997-03-21 10:23:35-05  gsl
**	Changed the WISPLICENSE logic for WIN32 to be the same as UNIX instead of DOC
**	thats MSDOS
**
**	Revision 1.17  1997-03-20 16:46:21-05  gsl
**	Make license_warning() external so it can be called from wshell
**
**	Revision 1.16  1997-03-17 13:22:47-05  gsl
**	Change to NeoMedia Technologies
**
**	Revision 1.15  1997-02-17 16:37:40-05  gsl
**	Change address
**
**	Revision 1.14  1996-12-12 12:57:50-05  gsl
**
**	Revision 1.13  1996-08-22 17:24:03-07  gsl
**	Call wgetpgrp() at start to initialize gid
**
**	Revision 1.12  1996-06-28 16:36:41-07  gsl
**	Fix includes and prototypes for NT.
**	Combine the msdos and unix code for use with NT.
**	Update copyrights for 96
**
**	Revision 1.11  1995-08-25 04:26:44-07  gsl
**	Moved the unix signal handling logic to a new routine wisp_signal_handler()
**	and moved it into initglbs().
**	Added the standard headers and protoized everything.
**
**
**
*/
