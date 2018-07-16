/* 
	Copyright (c) 1995-1997 NeoMedia Migrations, All rights reserved.
	$Id:$
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
	time_t	now, then, demotime;
	struct stat statbuf;

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
	
	stat(keyfile,&statbuf);
	then = statbuf.st_mtime;						/* Then is the mod-time				*/

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
		printf("                       NeoMedia Migrations Incorporated\n");
		printf("                          2201 2nd Street, Suite 600,\n");
		printf("                              Fort Myers FL 33901\n");
		printf("                            Phone: (941) 337-3434\n");
		printf("                            Fax:   (941) 337-3668\n\n");

		exit(0);
}



/*
**	History:
**	$Log: demovali.h,v $
**	Revision 1.11  1997-02-18 08:46:13-05  gsl
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
