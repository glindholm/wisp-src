static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		fcopy.c
**
**	Purpose:	File copy routines
**
**	Routines:	
**	fcopy()		Copy a file.
**
**
**	History:
**	02/15/93	Written by GSL.
**
*/

#define _POSIX_SOURCE 1

#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#if defined(MSDOS) || defined(_MSC_VER)
#include <io.h>
#endif

#include "idsistd.h"
#include "wisplib.h"
#include "fcopy.h"

/*
**	Routine:	fcopy()
**
**	Function:	To copy a file.
**
**	Description:	This routine copies a file.
**			It behaves like the unix shell "cp" command:
**				- the directory path must exist
**				- the protections are based on srcfile
**				- the owner and group are set to the user
**				- the mode is taken from srcfile
**				- if dstfile exists mode and owner are unchanged
**
**	Arguments:
**	srcfile		The source file name.
**	dstfile		The destination file name.
**
**	Globals:	None
**
**	Return:
**	0		Successful
**	errno		Failure
**
**	Warnings:	None
**
**	History:	
**	02/15/93	Written by GSL
**
*/
int fcopy(char* srcfile, char* dstfile)
{
	int	src,dst;
	char	buff[1024];
	int	rc,cnt;
	struct stat statbuf;
	int	dstexists;

#ifdef O_BINARY
	src = open(srcfile, O_RDONLY | O_BINARY);				/* Open the source file				*/
#else
	src = open(srcfile, O_RDONLY);						/* Open the source file				*/
#endif
	if (src == -1)
	{
		return(errno);
	}

	dstexists = fexists(dstfile);						/* Check if dstfile exists			*/

#ifdef O_BINARY
	dst = open(dstfile, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, 0666);	/* Open the destination file			*/
#else
	dst = open(dstfile, O_WRONLY | O_CREAT | O_TRUNC, 0666);		/* Open the destination file			*/
#endif
	if (dst == -1)
	{
		rc = errno;
		close(src);
		return(rc);
	}

	while((rc = read(src,buff,sizeof(buff))) > 0)				/* Read the source file until done		*/
	{
		cnt = rc;
		if ((rc = write(dst,buff,cnt)) != cnt)				/* Write the destination file until done	*/
		{
			if (rc != -1)						/* If we had a short write then			*/
			{
				rc = -1;
				errno = ENOSPC;					/* Assume Out of space.				*/
			}
			break;
		}
	}

	if (rc == -1)								/* If error occurred then return errno		*/
	{
		rc = errno;
		close(src);
		close(dst);
		return(rc);
	}

	close(src);
	rc = close(dst);							/* Make sure the close of destination succeeds	*/
	if (rc == -1)
	{
		return(errno);
	}

	if (!dstexists && 0 == stat(srcfile,&statbuf))				/* If dstfile didn't exist then			*/
	{
		chmod(dstfile,statbuf.st_mode);					/* change the mode to match srcfile		*/
	}

	return 0;
}

#ifdef MAIN
main(argc,argv)
int argc;
char *argv[];
{
	int	rc;

	if (argc != 3)
	{
		fprintf(stderr,"Usage: fcopy <srcfile> <dstfile>\n");
		exit(0);
	}

#ifdef __STDC__
	printf("__STDC__ defined\n");
#endif
	printf("fcopy %s %s\n",argv[1],argv[2]);
	rc = fcopy(argv[1],argv[2]);
	printf("rc = %d\n",rc);
	exit(0);
}
#endif /* MAIN */
/*
**	History:
**	$Log: fcopy.c,v $
**	Revision 1.7  1996-08-19 18:32:19-04  gsl
**	drcs update
**
**
**
*/
