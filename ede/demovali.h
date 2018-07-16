			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


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
	long	now, then, demotime;
	struct stat statbuf;

	time(&now);

	ptr = (char *) getenv("HOME");						/* Build lock file name				*/
	if (!ptr) ptr = ".";
	sprintf(keyfile,"%s/%s",ptr,DEMOUSRLOCK);

	if ( 0 == access(DEMOSYSLOCK,0) ) return(0);				/* System lock					*/

	if ( 0 != access(keyfile,0))						/* First time - file doesn't exist		*/
	{
		fh = open(keyfile, O_WRONLY | O_CREAT, 0444);
		if ( fh == -1 ) return(1);
		write(fh,&now,4);
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
		write(fh,&now,4);
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
		printf("                 International Digital Scientific Incorporated\n");
		printf("                        25050 Avenue Kearny, Suite 203,\n");
		printf("                              Valencia CA 91355\n");
		printf("                            Phone: (805) 295-1155\n");
		printf("                            Fax:   (805) 295-8755\n\n");

		exit(0);
}



