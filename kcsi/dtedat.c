static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

#include <sys/types.h>
#include <time.h>
#include "kcsifunc.h"

static char sccsid[]="@(#)dtedat.c	1.2 1/22/93";


void DTEDAT(char *ymd,char *mdy,char *tim,char *jul)
{
	struct tm *ltm;
	time_t systime;
	char work[101];

	time(&systime);
	ltm = localtime(&systime);

	sprintf(work,"%02d%02d%02d",ltm->tm_year%100,ltm->tm_mon+1,ltm->tm_mday);
	memcpy(ymd,work,6);
	sprintf(work,"%02d%02d%02d",ltm->tm_mon+1,ltm->tm_mday,ltm->tm_year%100);
	memcpy(mdy,work,6);
	sprintf(work,"%02d%03d",ltm->tm_year%100,ltm->tm_yday);
	memcpy(jul,work,5);
	sprintf(work,"%02d%02d%02d00",ltm->tm_hour,ltm->tm_min,ltm->tm_sec);
	memcpy(tim,work,8);
}
	

/*
**	History:
**	$Log: dtedat.c,v $
**	Revision 1.4.2.1  2002/11/12 15:56:24  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.5  2002/10/17 17:17:17  gsl
**	Removed VAX VMS code
**	
**	Revision 1.4  1997/10/03 17:30:55  gsl
**	YEAR2000 fix to mod 100 the year
**	
**	Revision 1.3  1996-09-17 19:45:36-04  gsl
**	drcs update
**
**
**
*/
