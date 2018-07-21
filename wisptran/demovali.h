/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/


#ifdef unix
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define DEMODAYS 65
#define DEMOSYSLOCK "/tmp/._wlocked"
#define DEMOUSRLOCK "._wlock"

static int demovalidate()							/* 1-Success 0-Failure				*/
{
	char	keyfile[80];
	char	*ptr;
	int	fh;
	time_t	now, then, demotime, mod_time;

	time(&now);

	ptr = wisphomedir();						/* Build lock file name				*/
	if (!ptr) ptr = ".";
	sprintf(keyfile,"%s/%s",ptr,DEMOUSRLOCK);

	if ( 0 == access(DEMOSYSLOCK,0) ) return(0);				/* System lock					*/

	if ( 0 != access(keyfile,0))						/* First time - file doesn't exist		*/
	{
		fh = open(keyfile, O_WRONLY | O_CREAT, 0444);
		if ( fh == -1 ) return(1);
		write(fh,&now,sizeof(now));
		close(fh);
		chmod(keyfile, 0444);
		return(1);							/* Success - First time 			*/
	}
	
	WL_stat_mtime(keyfile,&mod_time);
	then = mod_time;						/* Then is the mod-time				*/

	demotime = DEMODAYS * 24 * 60 * 60;					/* Alive time in seconds			*/

	if ( now < then ) return(0);						/* They messed with system time 		*/

	if ( now-then > demotime )						/* Time has elapsed				*/
	{
		fh = open(DEMOSYSLOCK, O_WRONLY | O_CREAT, 0444);		/* Write system lock				*/
		if ( fh == -1 ) return(0);
		write(fh,&now,sizeof(now));
		close(fh);
		chmod(keyfile, 0444);
		return(0); 							/* Failure - Demo has timed out 		*/
	}
	return(1);
}

#else
static int demovalidate() { return(1); }
#endif



static demoexit()
{
		printf("Your EVALUATION copy of WISP has timed out.\n\n");
		printf("For assistance contact:\n\n");
		printf("                       Shell Stream Software LLC\n");
		printf("                          %s,\n", WISP_ADDRESS_STREET);
		printf("                              %s\n", WISP_ADDRESS_CITY_STATE_ZIP);
		printf("                            Phone: %s\n", WISP_PHONE_NUMBER);

		exit(0);
}



/*
**	History:
**	$Log: demovali.h,v $
**	Revision 1.16  2005/07/11 15:10:34  gsl
**	Moved to Suite 600
**	
**	Revision 1.15  2003/02/04 17:33:20  gsl
**	fix copyright header
**	
**	Revision 1.14  2002/11/06 20:41:46  gsl
**	Change address to Suite 402
**	
**	Revision 1.13  2002/10/04 21:00:56  gsl
**	Change to use WL_stat_xxx() routines
**	
**	Revision 1.12  2002/06/26 20:52:17  gsl
**	Fix phone number
**	
**	Revision 1.11  1997/02/18 13:46:13  gsl
**	Change address
**	
**	Revision 1.10  1996-10-09 12:29:01-04  gsl
**	replace getenv() with wisphomedir()
**
**	Revision 1.9  1996-10-08 10:24:32-07  gsl
**	Move from wisp/common to wisp/tran
**
**	Revision 1.8  1996-07-23 11:17:46-07  gsl
**	drcs update
**
**
**
*/
