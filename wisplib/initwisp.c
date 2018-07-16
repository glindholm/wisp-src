static char copyright[]="Copyright (c) 1995-1997 NeoMedia Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		initwisp.c
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
**
**	Purpose:	Initialize the WISP runtime
**
**	Routines:	
**	initwisp2()	The COBOL called routine to do initialization.
*/

#if !(defined(unix) || defined(VMS) || defined(MSDOS) || defined(WIN32))

#error	'This will not compile as unix || VMS || MSDOS || WIN32 are not defined!!!'
#error	'This will not compile as unix || VMS || MSDOS || WIN32 are not defined!!!'
#error	'This will not compile as unix || VMS || MSDOS || WIN32 are not defined!!!'

#endif

/*
**	Includes
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <varargs.h>
#include <string.h>

#ifdef VMS
#include <ssdef.h>
#endif

#if defined(unix) || defined(_MSC_VER)
#include <sys/types.h>
#endif

#ifdef MSDOS
#include <process.h>
#include <DOS.H>
#ifdef DMF
#define EXT_FILEXT
#endif /* DMF */
#endif /* MSDOS */

#include "idsistd.h"
#include "werrlog.h"
#include "wdefines.h"
#include "wcommon.h"

#include "vwang.h"
#include "cobrun.h"
#include "wglobals.h"
#include "filext.h"
#include "wperson.h"
#include "wisplib.h"
#include "wexit.h"
#include "level.h"
#include "machid.h"
#include "wsb.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/
#ifdef VMS
static struct {
		int4 flink;								/* The forward link (VMS only)		*/
		char *exproc;								/* The pointer to the proc.		*/
		int4 argcnt;								/* The arg count (0)			*/
		int4 *condit;								/* Pointer to the condition value.	*/
	      } exit_handler;

static int4 conval;									/* A place to hold the condition value	*/
static uint4 status;
#endif	/* VMS */

/*
**	Static Function Prototypes
*/

#if defined(unix) || defined(MSDOS) || defined(WIN32)
static int license_checked(int set);
void license_warning(void);
#endif



/*
	INITWISP is OLD and has been replaced by initwisp2()

=================================================================
	INITWISP 	Perform initialization of wisped software.

	Parameters	WISP-APPLICATION-NAME	char[8]
			Word-Swap-Flag		int
			W-Error-Flag		int
*/

void initwisp(void)
{
	printf("\n\r %%WISP-F-INITWISP \n\r");
	printf(" The routine \"initwisp\" is no longer in use and has \n\r");
	printf(" been replaced by \"initwisp2\" please re-WISP.\n\r\n\r");
	exit(0);
}

void setrunname(void)
{
	printf("\n\r %%WISP-F-SETRUNNAME \n\r");
	printf(" The routine \"setrunname\" is no longer in use and has \n\r");
	printf(" been replaced by \"initwisp2\" please re-WISP.\n\r\n\r");
	exit(0);
}
	
void initwisp2(	char	wisp_tran_version[20],						/* The TRANSLATOR version		*/
		char	wisp_lib_version[1],						/* The LIBRARY version			*/
		char	cobol_type[3],							/* The type of COBOL			*/
		char	wisp_application_name[8],					/* The name of the program		*/
		char	wisprunname[8],							/* The first appl name this run unit	*/
		int4	*swap_on,							/* Swap_on == 0 forces swaping off	*/
		int4	*err_flag)							/* Err_flag != 0 Changes error logging	*/
{
#define		ROUTINE		24000

	static  int 	already = 0;							/* Have we already done it?		*/
	time_t	clock;
	char	datestamp[40];

#ifdef WATCOM
	init_watcom();
#endif

	werrset();									/* get runtime w_err_flag override.	*/

	clock = time(0);
	strcpy(datestamp, ctime(&clock));
	datestamp[strlen(datestamp) - 1] = '\0';					/* Remove the trailing newline 		*/

	if ( *wisp_lib_version != 20 &&							/* Accept 20 (2.0a-2.0f,3x)		*/
	     *wisp_lib_version != LIBRARY_VERSION )
	{
		werrlog(ERRORCODE(6),(int) *wisp_lib_version, LIBRARY_VERSION,0,0,0,0,0,0);
		wexit(1L);
	}

	/*=========================================================================*/

	if (already) 									/* Already did it!			*/
	{
		if( memcmp(WISPRUNNAME,"ACULINK ",8) == 0 ||				/* Reset RUNNAME only if ACULINK	*/
		    memcmp(WISPRUNNAME,"ACUUSING",8) == 0    )				/* 		      or ACUUSING	*/
		{
			memcpy(WISPRUNNAME,wisp_application_name,8);			/* Set the RUN name in C.		*/
		}
		memcpy(wisprunname,WISPRUNNAME,8);					/* Set the COBOL wisprunname		*/
		setprogid(wisp_application_name);					/* Set the program id in C.		*/

		wtrace("INITWISP2","ENTRY","APPNAME=[%8.8s] LINKLEVEL=[%d] GID=[%d][0x%08X] TIME=[%s]",
		       wisp_application_name, linklevel(), wgetpgrp(), wgetpgrp(), datestamp);

		return;
	}

	/*=========================================================================*/

	already = 1;

	time(&WSTARTTIME);

	newlevel();									/* Increment the link-level		*/
	wgetpgrp();									/* Call to Set the Process Group ID.	*/

	wtrace("INITWISP2","FIRST","APPNAME=[%8.8s] LINKLEVEL=[%d] GID=[%d][0x%08X] TIME=[%s]",
	       wisp_application_name, linklevel(), wgetpgrp(), wgetpgrp(), datestamp);


	memcpy(WISPTRANVER,wisp_tran_version,20);					/* Get the translator version		*/
	WISPTRANVER[20] = NULL_CHAR;

	memcpy(WISPRUNNAME,wisp_application_name,8);					/* Set the RUN name in C.		*/
	memcpy(wisprunname,wisp_application_name,8);					/* Set the RUN name in COBOL.		*/
	setprogid(wisp_application_name);						/* Set the program id name in C.	*/

	if (!wbackground())
	{
		vwang_pre_init();							/* Determine if COBOL debugger running	*/
	}


	if      ( !memcmp(cobol_type,"VAX",3) )
	{
		vax_cobol = TRUE;
		run_cobol = COBOL_VAX;
		memcpy(filelock,"91",2);
		memcpy(hardlock,"92",2);
		memcpy(softlock,"90",2);
	}
	else if ( !memcmp(cobol_type,"LPI",3) )
	{
		lpi_cobol = TRUE;
		run_cobol = COBOL_LPI;
		memcpy(filelock,"93",2);
		memcpy(hardlock,"99",2);
		memcpy(softlock,"99",2);
	}
	else if ( !memcmp(cobol_type,"ACU",3) )
	{
		acu_cobol = TRUE;
		run_cobol = COBOL_ACU;
		memcpy(filelock,"93",2);
		memcpy(hardlock,"99",2);
		memcpy(softlock,"99",2);
	}
	else if ( !memcmp(cobol_type,"AIX",3) )
	{
		aix_cobol = TRUE;
		run_cobol = COBOL_AIX;
		memcpy(filelock,"9A",2);
		memcpy(hardlock,"9D",2);
		memcpy(softlock,"9D",2);
	}
	else if ( !memcmp(cobol_type,"MF ",3) )
	{
		mf_cobol  = TRUE;
		run_cobol = COBOL_MF;
		memcpy(filelock,"9A",2);
		memcpy(hardlock,"9D",2);
		memcpy(softlock,"9D",2);
	}
	else if ( !memcmp(cobol_type,"DMF",3) )
	{
		mf_cobol  = TRUE;
		run_cobol = COBOL_MF;
		memcpy(filelock,"9A",2);
		memcpy(hardlock,"9D",2);
		memcpy(softlock,"9D",2);
	}
	else
	{
		run_cobol = COBOL_OTHER;
		memcpy(filelock,"93",2);
		memcpy(hardlock,"99",2);
		memcpy(softlock,"99",2);
	}

	memset(WISPFILEXT,' ',39);							/* Set file extension to spaces.	*/
	memset(WISPRETURNCODE,'0',3);							/* Set RETURN-CODE to zero.		*/
	wisp_progname[0] = '\0';
	wisp_screen[0] = '\0';

	if (noswap_words && (*swap_on == 1))						/* Swapping was off, now they want it on*/
	{
		werrlog(ERRORCODE(2),0,0,0,0,0,0,0,0);
	}
	noswap_words = ! *swap_on;							/* Set/reset the flags.			*/

	if (*err_flag)									/* Is the w_err_flag being set?		*/
	{
		w_err_flag = *err_flag;							/* Get the flag.			*/
	}

	load_options();									/* Load the opt_xxxx variables		*/

	if (opt_errflag_found)
	{
		w_err_flag = opt_errflag;
	}
	werrset();									/* get runtime w_err_flag override.	*/

#ifdef VMS
	exit_handler.exproc = (char *)wexith;						/* Install the exit handler on VMS.	*/
	exit_handler.argcnt = 0;
	exit_handler.condit = &conval;
	status = sys$dclexh(&exit_handler);
	if (status != SS$_NORMAL) werrlog(ERRORCODE(4),0,0,0,0,0,0,0,0);		/* Error installing exit handler (VMS)	*/
#endif

#ifdef MSDOS
	license_warning();
#endif

#if defined(unix) || defined(WIN32)


	license_warning();								/* Check the WISP license		*/

	saveprogdefs();									/* Save PROGLIB/PROGVOL values		*/

	/*
	**	Set up the standard wisp signal handling
	*/
	wisp_signal_handler();
#endif

}


#if defined(unix) || defined(MSDOS) || defined(WIN32)
/*
**	Routine:	license_warning()
**
**	Function:	To check the WISP license and issue warning messages if not valid.
**
**	Description:	This routine calls validate_license() to check if the WISP license has been installed
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

void license_warning(void)
{
	int pfkey, currow, curcol;
	HWSB	hWsb;
	int	code;

	if (wbackground()) return;						/* don't check in background			*/

	if (license_checked(0)) return;						/* If license already checked then return	*/

	code = validate_license();						/* Validate the license file			*/

	if (code == LICENSE_OK) 
	{
		license_checked(1);						/* Mark the license as checked.			*/
		return;
	}

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	wsb_add_text(hWsb,1,0,"****  WISP License Information  ****");
	wsb_add_text(hWsb,3,0,"Copyright (c) 1988-1997 NeoMedia Technologies Inc.");
	wsb_add_text(hWsb,4,0,"2201 2nd Street Suite 600, Fort Myers FL 33901 (941) 337-3434");

	switch (code)
	{
	case LICENSE_MISSING:
		wsb_add_text(hWsb,10,0,"****  WARNING  ****");
		wsb_add_text(hWsb,12,0,"This machine has not been licensed to use the WISP runtime system.");
		wsb_add_text(hWsb,13,0,"Please run the wlicense program to install the WISP license.");
		wsb_add_text(hWsb,15,0,"****  WARNING  ****");
		wsb_add_text(hWsb,24,0,"Press (ENTER) to continue.");
		break;
	case LICENSE_TIMEDOUT:
		wsb_add_text(hWsb,10,0,"****  TIMED OUT  ****");
		wsb_add_text(hWsb,12,0,"The WISP demo license for this machine has timed out.");
		wsb_add_text(hWsb,13,0,"Please contact NeoMedia at the above number for assistance.");
		wsb_add_text(hWsb,15,0,"****  TIMED OUT  ****");
		wsb_add_text(hWsb,24,0,"Press (ENTER) to EXIT.");
		break;
	case LICENSE_INVALID:
	case LICENSE_UNKNOWN:
	default:
		wsb_add_text(hWsb,10,0,"****  INVALID LICENSE  ****");
		wsb_add_text(hWsb,12,0,"An invalid WISP runtime license has been installed on this machine.");
		wsb_add_text(hWsb,13,0,"Please run the wlicense program to install the correct WISP license.");
		wsb_add_text(hWsb,15,0,"****  INVALID LICENSE  ****");
		wsb_add_text(hWsb,24,0,"Press (ENTER) to EXIT.");
		break;
	}
	
	wsb_display_and_read(hWsb, "001216X", &pfkey, &currow, &curcol);
	wsb_delete(hWsb);

	if (code == LICENSE_MISSING) 						/* only a warning				*/
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
**	Description:	If "set" is true then this routine will set a shell variable WISPLICENSE equal to the Group Id.  This
**			indicates that the license has been checked.
**			If "set" is false then it will check if the license has been checked by examining the shell var and
**			seeing if the group id matches.
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

#if defined(unix) || defined(WIN32)
	{
		int	gid;
		gid = wgetpgrp();	/* get the Process Group ID.			*/
		sprintf(mid,"%d",gid);
	}
#endif
#if defined(MSDOS)
	getmachineid(mid);							/* Get the machine id				*/

	{
		int	i, len;
		/*
		**	Mung the machine id so can't be easily figured out by a hacker.
		*/
		len = strlen(mid);
		for(i=0; i<len; i++)
		{
			mid[i] += 96;
		}
	}
#endif

	if (set)
	{
		sprintf(buff,"WISPLICENSE=%s",mid);				/* set env variable - message was displayed	*/
		setenvstr(buff);
		return(1);
	}
	else
	{
		if (ptr = getenv("WISPLICENSE"))				/* see if license already checked 		*/
		{
			if (0==strcmp(ptr,mid)) return(1);			/* If WISPLICENSE is equal to machine id - OK	*/
		}
		return(0);							/* License has not been checked			*/
	}
}

#endif /* unix || MSDOS || WIN32 */

/*
**	History:
**	$Log: initwisp.c,v $
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
**	Changed DevTech to NeoMedia
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
