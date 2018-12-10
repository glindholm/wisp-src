/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
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

#if defined(AIX) || defined(HPUX) || defined(SOLARIS) || defined(LINUX)
#define _LARGEFILE64_SOURCE
#define USE_FILE64
#endif

#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#ifdef unix
#include <unistd.h>
#endif

#if defined(WIN32)
#include <io.h>
#endif

#include "idsistd.h"
#include "wisplib.h"
#include "fcopy.h"
#include "werrlog.h"

#ifdef WIN32
#include "isonames.h"
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_LARGEFILE
#define O_LARGEFILE 0
#endif

/*
**	Routine:	wisp_fcopy()
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
int wisp_fcopy(const char* srcfile, const char* dstfile)
{
	int	src,dst;
	char	buff[1024];
	int	rc,cnt;
	int	dstexists;
	int	retcode = 0;

	src = open(srcfile, O_RDONLY | O_BINARY |O_LARGEFILE, 0);			/* Open the source file				*/
	if (src == -1)
	{
		retcode = errno;
		goto fcopy_return;
	}

	dstexists = fexists(dstfile);						/* Check if dstfile exists			*/

	dst = open(dstfile, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY | O_LARGEFILE, 0666);	/* Open the destination file			*/
	if (dst == -1)
	{
		retcode = errno;
		close(src);
		goto fcopy_return;
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
		retcode = errno;
		close(src);
		close(dst);
		goto fcopy_return;
	}

	close(src);
	rc = close(dst);							/* Make sure the close of destination succeeds	*/
	if (rc == -1)
	{
		retcode = errno;
		goto fcopy_return;
	}

	if (!dstexists)
	{
		mode_t mode;
		if (0 == WL_stat_mode(srcfile, &mode))				/* If dstfile didn't exist then			*/
		{
			chmod(dstfile, mode);					/* change the mode to match srcfile		*/
		}
	}

fcopy_return:
	wtrace("WISP","FCOPY","srcfile=[%s] dstfile=[%s] rc=[%d]", srcfile, dstfile, retcode);

	return retcode;
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

	printf("fcopy %s %s\n",argv[1],argv[2]);
	rc = wisp_fcopy(argv[1],argv[2]);
	printf("rc = %d\n",rc);
	exit(0);
}
#endif /* MAIN */
/*
**	History:
**	$Log: fcopy.c,v $
**	Revision 1.20  2011/10/29 20:09:14  gsl
**	Fix ISO routine name warnins on WIN32
**	
**	Revision 1.19  2003/01/31 17:23:48  gsl
**	Fix  copyright header
**	
**	Revision 1.18  2003/01/29 19:42:50  gsl
**	Fix -Wall warnings
**	
**	Revision 1.17  2002/11/19 16:28:45  gsl
**	Define O_LARGEFILE for ALPHA and SCO
**	
**	Revision 1.16  2002/10/07 21:08:00  gsl
**	Huge file support
**	
**	Revision 1.15  2002/10/07 20:54:48  gsl
**	Huge file support
**	
**	Revision 1.14  2002/10/07 19:27:58  gsl
**	Add O_LARGEFILE to open() options
**	
**	Revision 1.13  2002/10/04 21:00:55  gsl
**	Change to use WL_stat_xxx() routines
**	
**	Revision 1.12  2002/10/03 21:28:53  gsl
**	Start the Huge file support
**	
**	Revision 1.11  2002/10/01 18:54:01  gsl
**	Define O_BINARY for windows to simplify
**	
**	Revision 1.10  2002/06/26 01:41:12  gsl
**	Change fcopy() to wisp_fcopy()
**	
**	Revision 1.9  2002/06/21 03:10:35  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.8  1998/05/15 21:41:21  gsl
**	Add trace logic
**	
**	Revision 1.7  1996-08-19 18:32:19-04  gsl
**	drcs update
**
**
**
*/
