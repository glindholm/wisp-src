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
#include "idsistd.h"
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
**	History:	
**	04/26/93	Written by GSL
**
*/

int fexists(name)
char	*name;
{
#ifdef VMS
#include <stat.h>
#else /* !VMS */
#include <sys/types.h>
#include <sys/stat.h>
#endif /* !VMS */

	struct stat buf;

	return((0==stat(name,&buf)) ? 1:0);
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

int fcanread(name)
char *name;
{
	int	rc;

	rc = 0;
#if defined(unix) || defined(VMS)
	{
		FILE 	*fh;
		if (fh = fopen(name,"r"))
		{
			rc = 1;
			fclose(fh);
		}
	}
#endif /* unix || VMS */
#if defined(MSDOS)
	rc = (0==access(name,004)) ? 1 : 0;
#endif /* MSDOS */
	return(rc);
}
