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

#include <stdio.h>
#include <stdlib.h>

#ifdef unix
#include <unistd.h>
#endif

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
	static	int	rc = FALSE;

	if (first)
	{
		const char *ptr=NULL;
		char szMessBuff[256];
		char *pszMess = "Default to foreground.";
		
		first = 0;

		if (ptr=getenv( WISP_BACKGROUND_ENV ))
		{
			if ('t'==*ptr || 'T'==*ptr) /* WISPBACKGRD=true or TRUE */
			{
				rc = TRUE;
			}
			sprintf(szMessBuff,"Detected variable %s=%s",WISP_BACKGROUND_ENV,ptr);
			pszMess = szMessBuff;
		}
#ifdef unix		
		else if (0==isatty(fileno(stdin)))
		{
			/*
			 * Stdin is not a tty so assume we are in background.
			 */
			rc = TRUE;
			pszMess = "Special file (stdin) is not a tty.";
		}
#endif
#ifdef WIN32
		else if (getenv( ptr=AQM_COMPUTER_NAME ) ||
			 getenv( ptr=AQM_JOB_FILE ) ||
			 getenv( ptr=AQM_JOB_NAME ) ||
			 getenv( ptr=AQM_JOB_NUMBER) ||
			 getenv( ptr=AQM_LOG_FILE ) ||
			 getenv( ptr=AQM_QUEUE ) )
		{
			rc = TRUE;
			sprintf(szMessBuff,"Detected AQM variable %s",ptr);
			pszMess = szMessBuff;
		}	
#endif		

		wtrace("WBACKGROUND",(rc)?"TRUE":"FALSE","%s",pszMess);
		
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
**	Revision 1.12  1999-05-26 10:59:47-04  gsl
**	fix const warning
**
**	Revision 1.11  1999-05-20 15:08:20-04  gsl
**	Enhanced the detection of background.
**	On unix if stdin is not a tty then assume background.
**	Add a trace to tell how result was determined.
**
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
