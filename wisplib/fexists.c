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
**	fexists()	Test if a file exists
**	fcanread()	Test for read access to a file.
**
**
**	History:
**	04/26/93	Written by GSL
**
*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef WIN32
#include <io.h>
#endif

#include "idsistd.h"

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

int fexists(const char* name)
{
#ifdef unix
	struct stat buf;

	return((0==stat(name,&buf)) ? 1:0);
#endif

#ifdef WIN32
	/*
	 *	Use access() because stat() fails on a share name like "\\machine\share".
	 */

	return (0==access(name,000)) ? 1 : 0;
#endif
}

/*
**	ROUTINE:	isafile()
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
int isafile(const char* name)
{
	struct stat buf;

	if ( 0 != stat(name,&buf) )
	{
		return 0;
	}

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
**	ROUTINE:	isadir()
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
int isadir(const char* name)
{
	struct stat buf;

	if ( 0 != stat(name,&buf) )
	{
		return 0;
	}

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
**	Routine:	fcanread()
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

int fcanread(const char* name)
{
#ifdef unix
	FILE 	*fh;
	int	rc;

	if (fh = fopen(name,"r"))
	{
		fclose(fh);
		return 1;
	}
	return 0;
#endif /* unix */

#ifdef WIN32
	return (0==access(name,004)) ? 1 : 0;
#endif
}
/*
**	History:
**	$Log: fexists.c,v $
**	Revision 1.11  2001-11-13 15:46:30-05  gsl
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
