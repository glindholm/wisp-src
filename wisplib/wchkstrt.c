			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

#include <v/video.h>
#include "idsistd.h"
#include "werrlog.h"

wchkstrt(timeval,stat,fname)						/* Wait for a START stetement to time out.		*/
short *timeval;
char *stat;
char *fname;
{
#define		ROUTINE		71000
	int i;
	char tstr[40];

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

#ifdef VMS
											/* If file status = hard lock		*/
	if (stat[0] == '9' && stat[1] == '2' && (*timeval < 1))				/* And time has run out...		*/
#endif
#ifdef unix
	if (stat[0] == '9' && stat[1] == '9' && (*timeval < 1))				/* Use 99 for unix.			*/
#endif
	{										/* We have timed out.			*/
		*timeval = 15;								/* Reset the timer.			*/
		i = strpos(fname," ");
		memcpy(tstr,fname,i);
		tstr[i] = 0;
		werrlog(ERRORCODE(3),tstr,0,0,0,0,0,0,0);				/* Timeout - record in use.		*/
	}
}
