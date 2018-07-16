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

#ifdef unix
int wbackground()									/* Return true if in background.	*/
{
	static	int	first = 1;
	static	int	rc;

	if (first)
	{
		first = 0;
		rc = (getenv( WISP_BACKGROUND_ENV )) ? TRUE : FALSE;
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
