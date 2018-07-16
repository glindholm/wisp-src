static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	backgrnd.c
*/

#include "idsistd.h"
#include "wperson.h"								/* get the structure definitions		*/
#include "werrlog.h"
#include "wdefines.h"

#ifdef VMS
int wbackground()									/* Return true if in background.	*/
{
	char mode;
	static	int	first = 1;
	static	int	rc;

	if (first)
	{
		first = 0;
		osd_mode(&mode);
		if (mode == 'B') rc = TRUE;						/* Is in background mode.		*/
		else		 rc = FALSE;						/* Is in foreground.			*/
	}
	return( rc );
}
#endif	/* VMS */

#if defined(unix) || defined(WIN32)
int wbackground()									/* Return true if in background.	*/
{
	static	int	first = 1;
	static	int	rc;

	if (first)
	{
		first = 0;
#ifdef unix		
		rc = (getenv( WISP_BACKGROUND_ENV )) ? TRUE : FALSE;
#endif
#ifdef WIN32
		if (getenv( WISP_BACKGROUND_ENV ) ||
		    getenv( AQM_COMPUTER_NAME ) ||
		    getenv( AQM_JOB_FILE ) ||
		    getenv( AQM_JOB_NAME ) ||
		    getenv( AQM_JOB_NUMBER) ||
		    getenv( AQM_LOG_FILE ) ||
		    getenv( AQM_QUEUE ) )
		{
			rc = TRUE;
		}	
		else
		{
			rc = FALSE;
		}
#endif		
	}
	return(rc);
}
#endif	/* unix */

#ifdef MSDOS
										/* On MSDOS, wbackground() is always false.	*/
										/* A process can only be "background" if it is	*/
										/* started by a "submit()" call which is not	*/
										/* available on MSDOS.				*/
int wbackground()
{
	return(FALSE);
}
#endif	/* MSDOS */
/*
**	History:
**	$Log: backgrnd.c,v $
**	Revision 1.10  1996-12-06 18:41:13-05  jockc
**	separated unix and win32 code.. win32 also checks for
**	AQM (argent queue manager) variables
**
**	Revision 1.9  1996-08-19 15:32:08-07  gsl
**	drcs update
**
**
**
*/
