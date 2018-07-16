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

#ifdef WIN32
#include <io.h>
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif

#if defined(WIN32)
#define O_LARGEFILE 0
#endif

#include "idsistd.h"
#include "wisplib.h"

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

	if (EOVERFLOW == errno)		/* Error but file does exist */
	{
		return 1;
	}

	return 0;
#else
	struct stat buf;

	if (0==stat(name,&buf))
	{
		return 1;		/* File exists */
	}

#ifdef EOVERFLOW
	if (EOVERFLOW == errno)		/* Large file exists */
	{
		return 1;
	}
#endif

	return 0;
#endif
#endif

#ifdef WIN32
	/*
	 *	Use access() because stat() fails on a share name like "\\machine\share".
	 */

	return (0==access(name,000)) ? 1 : 0;
#endif
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
		return -1;
	}
#else
	struct stat buf;
	if ( 0 != stat(name,&buf) )
	{
		return -1;
	}
#endif

	*mode = buf.st_mode;
	return 0;
}

#ifdef INT8_DEFINED
/*
**	ROUTINE:	WL_stat_size_int8()
**
**	FUNCTION:	Get the file size as an INT8 (64-bit)
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
int WL_stat_size_int8(const char* name, INT8 *size)
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
	return (0==access(name,004)) ? 1 : 0;
#endif
}
/*
**	History:
**	$Log: fexists.c,v $
**	Revision 1.11.2.1  2002/10/09 19:20:29  gsl
**	Update fexists.c to match HEAD
**	Rename routines WL_xxx for uniqueness
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
