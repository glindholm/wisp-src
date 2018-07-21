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
**      filesize.c
*/


#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include "wisplib.h"
#include "idsistd.h"
#include "werrlog.h"


/*
**	ROUTINE:	WL_filesize()
**
**	FUNCTION:	Get the file size in bytes
**
**	DESCRIPTION:	Call stat() to get the file size
**
**	ARGUMENTS:	
**	path		The file path
**
**	RETURN:		The file size in bytes or -1 on error.
**
**	WARNINGS:	Files above 2gig will fail as size cannot be returned
**			in a long.
**			
**	NOTE:		This is only used in printing to check for zero byte files.
**
*/
long WL_filesize(const char* path)     /* Return the length of the file in bytes.                      */
{
	long stat_size;

	if ( 0 != WL_stat_size_long(path, &stat_size) )
	{
		WL_werrlog_error(WERRCODE(65300),"FILESIZE", "STAT", 
			"stat() failed path=[%s] errno=[%d] msg=[%s]", 
			path, errno, WL_strerror(errno));
		return( -1 );
	}

	WL_wtrace("FILESIZE","SIZE","path=[%s] size=[%ld]", path, stat_size);
	return( stat_size );
}

/*
**	History:
**	$Log: filesize.c,v $
**	Revision 1.18  2003/01/31 17:33:56  gsl
**	Fix  copyright header
**	
**	Revision 1.17  2002/12/10 17:09:20  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.16  2002/12/09 19:15:31  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.15  2002/10/04 21:00:55  gsl
**	Change to use WL_stat_xxx() routines
**	
**	Revision 1.14  2002/10/01 19:06:32  gsl
**	Fix tracing and error messages
**	
**	Revision 1.13  2002/07/10 04:27:38  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.12  2002/06/26 01:42:45  gsl
**	Remove VMS code
**	
**	Revision 1.11  2002/06/21 03:10:35  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.10  1996/09/10 15:42:45  gsl
**	fix some of the ifdefs
**	
**	Revision 1.9  1996-08-19 15:32:20-07  gsl
**	drcs update
**
**
**
*/
