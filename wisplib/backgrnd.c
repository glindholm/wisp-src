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


int WL_wbackground()									/* Return true if in background.	*/
{
	static	int	first = 1;
	static	int	rc = FALSE;

	if (first)
	{
		const char *ptr=NULL;
		char szMessBuff[256];
		char *pszMess = "Default to foreground.";
		
		first = 0;

		if ((ptr=getenv( WISP_BACKGROUND_ENV )))
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

/*
**	History:
**	$Log: backgrnd.c,v $
**	Revision 1.16  2003/01/31 17:23:49  gsl
**	Fix  copyright header
**	
**	Revision 1.15  2003/01/29 19:42:50  gsl
**	Fix -Wall warnings
**	
**	Revision 1.14  2002/07/10 21:05:14  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.13  2002/06/21 03:10:34  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.12  1999/05/26 14:59:47  gsl
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
