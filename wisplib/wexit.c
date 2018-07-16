static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wexit.c
**
**	Project:	WISP/LIB
**
**	RCS:		$Source:$
**
**	Purpose:	Comment exit point
**
**	Routines:	
**	wexit()		Perform cleanup then terminal process
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

#define		ROUTINE		65400

extern int shutexitcobol(int exit_code);

/*
**	ROUTINE:	wexit()
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
void wexit(int4 num)
{
	int	exit_code;

	wtrace("WEXIT", "ENTER", "wexit(%ld) linklevel=%d", (long)num, linklevel()); 

	if (0 == num)
	{
		LINKCOMPCODE = 0;							/* Non-error termination of LINK	*/
	}
	else
	{
		LINKCOMPCODE = 16;							/* Abnormal termination of LINK		*/
	}
	

#ifdef VMS
	if ( num == 0 ) 
	{
		exit_code = 1;								/* Change to VMS style exit code	*/
	}
	else
	{
		/*
		**	WISP coded return codes == (RC * 10000 + 1)
		**	the "+ 1" is to make them VMS warnings/informational codes.
		**	LINK knows how to decode.
		*/
		exit_code = num * 10000 + 1;
	}
#else					/* unix and MSDOS */
	if ( 2 == sizeof(int) )								/* int == short (16 bits; like MSDOS).	*/
	{
		exit_code = (int)( num % 10000L );					/* Preserve the last 4 decimal digits.	*/
	}
	else										/* int == int4 (32 bits; unix or VMS).	*/
	{
		exit_code = num;
	}

	wexith();
#endif

	if (run_cobol)
	{
		shutexitcobol(exit_code);	/* this is the COBOL specific routine to shutdown cobol and exit		*/
	}
	else
	{
		exit(exit_code);
	}
}

/*
**	History:
**	$Log: wexit.c,v $
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
