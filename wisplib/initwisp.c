			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#ifndef unix
#ifndef VMS
#ifndef MSDOS
		Error:  This will not compile as unix, VMS and MSDOS are not defined!!! 
#endif
#endif
#endif


#ifdef unix
#include <signal.h>
#include <sys/types.h>
void wexitsig();
void wexitint();
void wexitbug();
static int license_checked();
#endif

#ifdef MSDOS
#include <process.h>
#include <stdlib.h>
#include <DOS.H>
#ifdef DMF
#define EXT_FILEXT
#endif /* DMF */
#endif /* MSDOS */

#include <stdio.h>
#include <time.h>
#include <varargs.h>

#include "idsistd.h"
#include "werrlog.h"
#include "wdefines.h"
#include "wcommon.h"

#include "cobrun.h"
#include "wglobals.h"
#include "filext.h"
#include "wperson.h"

static void license_warning();
static int centload();

#ifdef VMS
#include <ssdef.h>
int wexith();										/* Declare the exit handler routine.	*/

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
	INITWISP is OLD and has been replaced by initwisp2()

=================================================================
	INITWISP 	Perform initialization of wisped software.

	Parameters	WISP-APPLICATION-NAME	char[8]
			Word-Swap-Flag		int
			W-Error-Flag		int
*/

initwisp()
{
	printf("\n\r %%WISP-F-INITWISP \n\r");
	printf(" The routine \"initwisp\" is no longer in use and has \n\r");
	printf(" been replaced by \"initwisp2\" please re-WISP.\n\r\n\r");
	exit(0);
}

setrunname( )
{
	printf("\n\r %%WISP-F-SETRUNNAME \n\r");
	printf(" The routine \"setrunname\" is no longer in use and has \n\r");
	printf(" been replaced by \"initwisp2\" please re-WISP.\n\r\n\r");
	exit(0);
}
	
void initwisp2(wisp_tran_version, wisp_lib_version, cobol_type, wisp_application_name, wisprunname, swap_on, err_flag)
char	wisp_tran_version[20];								/* The TRANSLATOR version		*/
char	wisp_lib_version[1];								/* The LIBRARY version			*/
char	cobol_type[3];									/* The type of COBOL			*/
char	wisp_application_name[8];							/* The name of the program		*/
char	wisprunname[8];									/* The first appl name this run unit	*/
int4	*swap_on;									/* Swap_on == 0 forces swaping off	*/
int4	*err_flag;									/* Err_flag != 0 Changes error logging	*/
{
#define		ROUTINE		24000

static  int 	already = 0;								/* Have we already done it?		*/
	int 	i;
	time_t	clock;

	werrset();									/* get runtime w_err_flag override.	*/

	clock = time(0);
	werrlog(101,wisp_application_name,ctime(&clock),0,0,0,0,0,0);			/* Timestamp with application name.	*/


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

		return;
	}

	/*=========================================================================*/

	already = 1;

	time(&WSTARTTIME);

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say hello.				*/

	newlevel();									/* Increment the link-level		*/

	memcpy(WISPTRANVER,wisp_tran_version,20);					/* Get the translator version		*/
	WISPTRANVER[20] = NULL_CHAR;

	memcpy(WISPRUNNAME,wisp_application_name,8);					/* Set the RUN name in C.		*/
	memcpy(wisprunname,wisp_application_name,8);					/* Set the RUN name in COBOL.		*/
	setprogid(wisp_application_name);						/* Set the program id name in C.	*/


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

	wpload();

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

#ifdef unix
	if (!wbackground())
	{
		set_isdebug();								/* Determine if COBOL debugger running	*/
	}

	PGRPID = wgetpgrp();								/* Set the Process Group ID.		*/

	license_warning();								/* Check the WISP license		*/

	saveprogdefs();									/* Save PROGLIB/PROGVOL values		*/

	/*
	**	Ignore DEATH OF CHILD signals (from submitted processes).
	**	This is to stop the <DEFUNCT> zombie processes from occuring because of no "wait".
	**	SIGCLD will need to be reset to SIG_DFL around a fork-exec-wait (LINK) so that the wait will work correctly.
	*/
	signal( SIGCLD, SIG_IGN );

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
#endif

}

#ifdef unix
void wexitint(sig)
int sig;
{
#undef		ROUTINE
#define		ROUTINE		74400

	signal( SIGINT,  SIG_IGN );
	signal( SIGQUIT, SIG_IGN );
	werrlog(ERRORCODE(2),sig,0,0,0,0,0,0,0);					/* User signalled interrupt.		*/
	wexit(ERRORCODE(2));
}
void wexitbug(sig)
int sig;
{
#undef		ROUTINE
#define		ROUTINE		74500
	signal( SIGINT,  SIG_IGN );
	signal( SIGQUIT, SIG_IGN );
	werrlog(ERRORCODE(2),sig,0,0,0,0,0,0,0);					/* Fatal signalled interrupt.		*/
	wexit(ERRORCODE(2));
}
void wexitsig(sig)
int sig;
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
#endif

#if defined(unix) || defined(MSDOS)
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

static void license_warning()
{
	int	code;
	char	sbuff[1924];
	char	func;
	char	lines;
	char	termkey[2];
	char	status[2];

	if (wbackground()) return;						/* don't check in background			*/

	if (license_checked(0)) return;						/* If license already checked then return	*/

	code = validate_license();						/* Validate the license file			*/

	if (code == LICENSE_OK) 
	{
		license_checked(1);						/* Mark the license as checked.			*/
		return;
	}

	memset(sbuff,' ',1924);							/* Perpare message screen			*/
	sbuff[0] = 1;
	sbuff[1] = 0xc0;
	sbuff[2] = 1;
	sbuff[3] = 1;

	centload(sbuff,1,"****  WISP License Information  ****");
	centload(sbuff,3,"Copyright (c) 1988,89,90,91,92,93 International Digital Scientific Inc.");
	centload(sbuff,4,"28460 Avenue Stanford Suite 100, Valencia CA 91355  (805) 295-1155");

	switch (code)
	{
	case LICENSE_MISSING:
		centload(sbuff,10,"****  WARNING  ****");
		centload(sbuff,12,"This machine has not been licensed to use the WISP runtime system.");
		centload(sbuff,13,"Please run the wlicense program to install the WISP license.");
		centload(sbuff,15,"****  WARNING  ****");
		centload(sbuff,24,"Press ENTER to continue.");
		break;
	case LICENSE_TIMEDOUT:
		centload(sbuff,10,"****  TIMED OUT  ****");
		centload(sbuff,12,"The WISP demo license for this machine has timed out.");
		centload(sbuff,13,"Please contact I.D.S.I. at the above number for assistance.");
		centload(sbuff,15,"****  TIMED OUT  ****");
		centload(sbuff,24,"Press ENTER to EXIT.");
		break;
	case LICENSE_INVALID:
	case LICENSE_UNKNOWN:
	default:
		centload(sbuff,10,"****  INVALID LICENSE  ****");
		centload(sbuff,12,"An invalid WISP runtime license has been installed on this machine.");
		centload(sbuff,13,"Please run the wlicense program to install the correct WISP license.");
		centload(sbuff,15,"****  INVALID LICENSE  ****");
		centload(sbuff,24,"Press ENTER to EXIT.");
		break;
	}
	func = 3;
	lines = 24;
	vwang(&func,sbuff,lines,"A",termkey,status);				/* Display the message				*/

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

#ifdef unix
static int license_checked(set)
int	set;
{
	char	*ptr;
	int	gid, gid_x;
	char	buff[128];

	gid = wgetpgrp();							/* get the Process Group ID.			*/

	if (set)
	{
		sprintf(buff,"WISPLICENSE=%d",gid);				/* set env variable - message was displayed	*/
		setenvstr(buff);
		return(1);
	}
	else
	{
		if (ptr = getenv("WISPLICENSE"))				/* see if license already checked 		*/
		{
			sscanf(ptr,"%d",&gid_x);				/* If WISPLICENSE env var is equal to the GID	*/
			if (gid_x == gid) return(1);				/* then license has already been checked	*/
		}
		return(0);							/* License has not been checked			*/
	}
}
#endif /* unix */

#ifdef MSDOS
static int license_checked(set)
int	set;
{
	char	*ptr;
	char	mid[80];
	char	buff[128];
	int	i, len;

	getmachineid(mid);							/* Get the machine id				*/

	/*
	**	Mung the machine id so can't be easily figured out by a hacker.
	*/
	len = strlen(mid);
	for(i=0; i<len; i++)
	{
		mid[i] += 96;
	}

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
#endif /* MSDOS */

/*
**	Routine:	centload()
**
**	Function:	To load a string into a screen map and center it on a given row.
**
**	Description:	
**
**	Input:		buff		the screen buffer
**			row		the row number to load at
**			mess		the message string to load
**
**	Output:		buff		the modified screen buffer
**			
**
**	Return:		None
**
**	Warnings:	No error checking is done.
**
**	History:	05/27/92	Written by GSL
**
*/

static centload(buff,row,mess)
char	buff[1924];
int	row;
char	*mess;
{
	int	col,len;

	len = strlen(mess);
	col = 40 - len/2;

	memcpy(&buff[4+(row-1)*80+col],mess,len);
}
#endif /* unix || MSDOS */