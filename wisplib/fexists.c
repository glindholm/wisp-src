/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** $Id:$
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
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/


/*
**	File:		fexists.c
**
**	Purpose:	To hold routines that test the existence of files.
**
**	Routines:	
**	WL_fexists()	Test if a file exists
**	WL_fcanread()	Test for read access to a file.
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

#ifdef WIN32
#include <io.h>
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_LARGEFILE
#define O_LARGEFILE 0
#endif

#include "idsistd.h"
#include "wisplib.h"
#include "werrlog.h"

#ifndef S_ISDIR
#define S_ISFIFO(m)	(((m)&(_S_IFMT)) == (_S_IFIFO))
#define S_ISDIR(m)	(((m)&(_S_IFMT)) == (_S_IFDIR))
#define S_ISCHR(m)	(((m)&(_S_IFMT)) == (_S_IFCHR))
#define S_ISBLK(m)	(((m)&(_S_IFMT)) == (_S_IFBLK))
#define S_ISREG(m)	(((m)&(_S_IFMT)) == (_S_IFREG))
#endif

/*
**	Routine:	fexists()
**
**	Function:	To test if a file or directory exists
**
**	Description:	Routine stat() is used to check if the file exists.
**			This will use the effective UID not the real UID.
**
**	Arguments:
**	name		The file name.
**
**	Globals:	None
**
**	Return:
**	1		The file exists
**	0		The file does not exist
**
**	Warnings:	None
**
*/

int WL_fexists(const char* name)
{
#ifdef unix
#ifdef USE_FILE64
	struct stat64 buf;

	if (0==stat64(name,&buf))
	{
		return 1;		/* File exists */
	}

#else /* !USE_FILE64 */
	struct stat buf;

	if (0==stat(name,&buf))
	{
		return 1;		/* File exists */
	}
#endif /* !USE_FILE64 */
#endif /* unix */

#ifdef WIN32
	/*
	 *	Use access() because stat() fails on a share name like "\\machine\share".
	 */

	if (0==_access(name,000))
	{
		return 1;
	}
#endif

#ifdef EOVERFLOW
	if (EOVERFLOW == errno)		/* Large file exists */
	{
		return 1;
	}
#endif /* EOVERFLOW */

	/*
	**	Report unexpected error only
	*/
	if (ENOENT != errno)
	{
		WL_wtrace("FEXISTS","NOTFOUND","File=[%s] errno=[%d]", name, errno);
	}
	return 0;
}

/*
**	ROUTINE:	WL_isafile()
**
**	FUNCTION:	Checks if a file exists (and is a regular file)
**
**	DESCRIPTION:	Call stat() to see if a file exists then check the
***			mode to see that it is a regular file.
**
**	ARGUMENTS:	
**	name		The file path
**
**	GLOBALS:	None
**
**	RETURN:		
**	1		It is a file and does exist
**	0		It doesn't exist or is not a file.
**
**	WARNINGS:	None
**
*/
int WL_isafile(const char* name)
{
#ifdef USE_FILE64
	struct stat64 buf;
	if ( 0 != stat64(name,&buf) )
	{
		return 0;
	}
#else
	struct stat buf;
	if ( 0 != stat(name,&buf) )
	{
		return 0;
	}
#endif

	if (S_ISREG(buf.st_mode))
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

/*
**	ROUTINE:	WL_isadir()
**
**	FUNCTION:	Checks if a directory exists
**
**	DESCRIPTION:	Call stat() to see if a file exists then check the
***			mode to see that it is a directory.
**
**	ARGUMENTS:	
**	name		The directory path
**
**	GLOBALS:	None
**
**	RETURN:		
**	1		It is a directory and does exist
**	0		It doesn't exist or is not a directory.
**
**	WARNINGS:	None
**
*/
int WL_isadir(const char* name)
{
#ifdef USE_FILE64
	struct stat64 buf;
	if ( 0 != stat64(name,&buf) )
	{
		return 0;
	}
#else
	struct stat buf;
	if ( 0 != stat(name,&buf) )
	{
		return 0;
	}
#endif

	if (S_ISDIR(buf.st_mode))
	{
		return 1;
	}
	else
	{
		return 0;
	}
}


/*
**	ROUTINE:	WL_inode()
**
**	FUNCTION:	Get the file inode number
**
**	DESCRIPTION:	Use stat() or stat64()
**
**	ARGUMENTS:	
**	name		The directory path
**
**
**	RETURN:		The inode or 0 if error
**
**	WARNINGS:	None
**
*/
#ifdef unix
long WL_inode(const char* name)
{
#ifdef USE_FILE64
	struct stat64 buf;
	if ( 0 != stat64(name,&buf) )
	{
		return 0;
	}
#else
	struct stat buf;
	if ( 0 != stat(name,&buf) )
	{
		return 0;
	}
#endif

	return buf.st_ino;
}
#endif

/*
**	ROUTINE:	WL_stat_ctime()
**
**	FUNCTION:	Get the file create timestamp
**
**	DESCRIPTION:	Use stat() or stat64()
**
**	ARGUMENTS:	
**	name		The directory path
**	create_time	The create time
**
**	RETURN:		
**	0		Sucess
**	-1		stat failed
**
**	WARNINGS:	None
**
*/
int WL_stat_ctime(const char* name, time_t *create_time)
{
#ifdef USE_FILE64
	struct stat64 buf;
	if ( 0 != stat64(name,&buf) )
	{
		return -1;
	}
#else
	struct stat buf;
	if ( 0 != stat(name,&buf) )
	{
		return -1;
	}
#endif

	*create_time = buf.st_ctime;
	return 0;
}

/*
**	ROUTINE:	WL_stat_mtime()
**
**	FUNCTION:	Get the file modify timestamp
**
**	DESCRIPTION:	Use stat() or stat64()
**
**	ARGUMENTS:	
**	name		The directory path
**	mod_time	The modify time
**
**	RETURN:		
**	0		Sucess
**	-1		stat failed
**
**	WARNINGS:	None
**
*/
int WL_stat_mtime(const char* name, time_t *mod_time)
{
#ifdef USE_FILE64
	struct stat64 buf;
	if ( 0 != stat64(name,&buf) )
	{
		return -1;
	}
#else
	struct stat buf;
	if ( 0 != stat(name,&buf) )
	{
		return -1;
	}
#endif

	*mod_time = buf.st_mtime;
	return 0;
}


/*
**	ROUTINE:	WL_stat_mode()
**
**	FUNCTION:	Get the file modify timestamp
**
**	DESCRIPTION:	Use stat() or stat64()
**
**	ARGUMENTS:	
**	name		The directory path
**	mode		The returned mode
**
**	RETURN:		
**	0		Sucess
**	-1		stat failed
**
**	WARNINGS:	None
**
*/
int WL_stat_mode(const char* name, mode_t *mode)
{
#ifdef USE_FILE64
	struct stat64 buf;
	if ( 0 != stat64(name,&buf) )
	{
		WL_wtrace("STAT64","FAILED","File=[%s] errno=[%d]", name, errno);
		return -1;
	}
#else
	struct stat buf;
	if ( 0 != stat(name,&buf) )
	{
		WL_wtrace("STAT","FAILED","File=[%s] errno=[%d]", name, errno);
		return -1;
	}
#endif

	*mode = buf.st_mode;
	return 0;
}

#ifdef INT64_DEFINED
/*
**	ROUTINE:	WL_stat_size_int8()
**
**	FUNCTION:	Get the file size as an INT64 (64-bit)
**
**	DESCRIPTION:	Use stat() or stat64()
**
**	ARGUMENTS:	
**	name		The directory path
**	size		The returned size
**
**	RETURN:		
**	0		Sucess
**	-1		stat failed
**
**	WARNINGS:	None
**
*/
int WL_stat_size_int8(const char* name, INT64 *size)
{
#ifdef USE_FILE64
	struct stat64 buf;
	if ( 0 != stat64(name,&buf) )
	{
		return -1;
	}
#else
	struct stat buf;
	if ( 0 != stat(name,&buf) )
	{
		return -1;
	}
#endif

	*size = buf.st_size;
	return 0;
}
#endif

/*
**	ROUTINE:	WL_stat_size_long()
**
**	FUNCTION:	Get the file size as a long
**
**	DESCRIPTION:	Use stat() 
**
**	ARGUMENTS:	
**	name		The directory path
**	size		The returned size
**
**	RETURN:		
**	0		Sucess
**	-1		stat failed
**
**	WARNINGS:	None
**
*/
int WL_stat_size_long(const char* name, long *size)
{
	struct stat buf;
	if ( 0 != stat(name,&buf) )
	{
		return -1;
	}

	*size = buf.st_size;
	return 0;
}

/*
**	Routine:	WL_fcanread()
**
**	Function:	To check if we have read access to this file or directory.
**
**	Description:	Try to open the file up to see if we can read it with out effective UID.
**			For MSDOS we use access() because you can't "open" a directory.
**
**	Arguments:
**	name		The file name to check
**
**	Globals:	None
**
**	Return:
**	0		Can't read
**	1		Can read
**
**	Warnings:	There is a lot of overhead on most systems with this so don't over use it.
**
**	History:	
**	04/26/93	Written by GSL
**	05/07/93	Change MSDOS to use access(). GSL
**
*/

int WL_fcanread(const char* name)
{
#ifdef unix
	int 	fh;

	fh = open( name, O_RDONLY | O_BINARY | O_LARGEFILE );
	if ( -1 == fh)
	{
		return 0;
	}
	close(fh);
	return 1;
#endif /* unix */

#ifdef WIN32
	return (0==_access(name,004)) ? 1 : 0;
#endif
}
/*
**	History:
**	$Log: fexists.c,v $
**	Revision 1.26  2011/10/29 20:09:14  gsl
**	Fix ISO routine name warnins on WIN32
**	
**	Revision 1.25  2007/07/31 16:51:07  gsl
**	Change INT8 to INT64 to avoid conflicts on WIN32
**	
**	Revision 1.24  2003/01/31 17:23:48  gsl
**	Fix  copyright header
**	
**	Revision 1.23  2003/01/29 19:42:50  gsl
**	Fix -Wall warnings
**	
**	Revision 1.22  2002/12/11 21:09:19  gsl
**	Only trace "odd" errors in fexists()
**	
**	Revision 1.21  2002/12/11 20:58:40  gsl
**	Enhance tracing of runtype
**	
**	Revision 1.20  2002/11/19 16:28:44  gsl
**	Define O_LARGEFILE for ALPHA and SCO
**	
**	Revision 1.19  2002/10/08 15:44:39  gsl
**	Change int8 to INT8 to avoid conficts
**	
**	Revision 1.18  2002/10/07 21:08:00  gsl
**	Huge file support
**	
**	Revision 1.17  2002/10/07 20:59:08  gsl
**	Huge file support
**	
**	Revision 1.16  2002/10/07 20:54:48  gsl
**	Huge file support
**	
**	Revision 1.15  2002/10/04 21:00:55  gsl
**	Change to use WL_stat_xxx() routines
**	
**	Revision 1.14  2002/10/03 21:14:07  gsl
**	Change to use stat64() to support Hugh files.
**	
**	Revision 1.13  2002/10/01 19:10:34  gsl
**	in fexists() check for EOVERFLOW on stat()
**	
**	Revision 1.12  2002/07/10 04:27:38  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.11  2001/11/13 20:46:30  gsl
**	On WIN32 use access() instead of stat()
**	
**	Revision 1.10  1996-12-11 16:31:10-05  gsl
**	Added isafile() and isadir()
**
**	Revision 1.9  1996-09-10 08:41:55-07  gsl
**	move idsistd.h to after systemincludes
**
**	Revision 1.8  1996-08-23 13:58:01-07  gsl
**	filename parameter is now const
**
**	Revision 1.7  1996-08-19 15:32:19-07  gsl
**	drcs update
**
**
**
*/
