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
**	File:		paths.c
**
**	Purpose:	Routines to search paths
**
**	Routines:	
**	WL_env_path_seg()	Get segment of the env PATH
**	WL_link_path_seg()	Get segment of the LINK PATH
**	WL_osd_ext()		Get the extension of a file
**	WL_whichenvpath()	Find a file on the $PATH
**	WL_whichlinkpath()	Find a file on the LINK path
*/



#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#include <io.h>
#endif

#ifdef unix
#include <unistd.h>
#endif

#include "idsisubs.h"
#include "paths.h"
#include "wispcfg.h"
#include "wmalloc.h"

#ifdef WIN32
#include "isonames.h"
#endif

/*
**	Routine:	WL_link_path_seg()
**
**	Function:	Return a directory on the path.
**
**	Description:	Split the PATH into null terminated segments and return
**			the segment requested.
**
**	Arguments:
**	pathnum		The segment of the $PATH requested
**			0  = Whole $PATH
**			n  = The requested segment.
**
**			Example:
**				PATH = .:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/etc
**				       1 2              3        4    5         6
**
**	Globals:	None
**
**	Return:		The null terminated segment or NULL.
**
**	Warnings:	None
**
**	History:	
**	08/03/94	Documented. GSL
**
*/
char *WL_link_path_seg(int pathnum)
{
	static	int	first=1;
	static	int	totalcnt=1;							/* Number of dirs in $PATH		*/
	static  char	*fullpath;							/* The $PATH				*/
	static  char	*nullpath;							/* $PATH with ':' replaced by '\0'.	*/
	int	i, cnt;

	if ( first)
	{
		const char *ptr = wisplinkpath();
		if (!ptr)
		{
			return NULL;
		}
		
		fullpath = wisp_strdup(ptr);
		nullpath = wisp_strdup(ptr);

		for( i=0; fullpath[i]; i++ )
		{
			if ( nullpath[i] == PATH_SEPARATOR )
			{
				nullpath[i] = '\0';					/* Replace ':' with '\0'.		*/
				totalcnt += 1;
			}
		}

		first = 0;
	}

	if ( pathnum == 0 ) return( fullpath );
	if ( pathnum == 1 ) return( nullpath );
	if ( pathnum > totalcnt ) return( NULL );
	if ( pathnum < 0 )
	{
		return( NULL );
	}

	cnt = 1;
	for( i=0; fullpath[i]; i++ )
	{
		if ( fullpath[i] == PATH_SEPARATOR )
		{
			cnt += 1;
			if ( cnt == pathnum ) return( &nullpath[i+1] );
		}
	}

	return( NULL );									/* This should never occur.		*/
}
char *WL_env_path_seg(int pathnum)
{
	static	int	first=1;
	static	int	totalcnt=1;							/* Number of dirs in $PATH		*/
	static  char	*fullpath;							/* The $PATH				*/
	static  char	*nullpath;							/* $PATH with ':' replaced by '\0'.	*/
	int	i, cnt;

	if ( first)
	{
		const char *ptr = wispenvpath();
		if (!ptr)
		{
			return NULL;
		}
		
		fullpath = wisp_strdup(ptr);
		nullpath = wisp_strdup(ptr);

		for( i=0; fullpath[i]; i++ )
		{
			if ( nullpath[i] == PATH_SEPARATOR )
			{
				nullpath[i] = '\0';					/* Replace ':' with '\0'.		*/
				totalcnt += 1;
			}
		}

		first = 0;
	}

	if ( pathnum == 0 ) return( fullpath );
	if ( pathnum == 1 ) return( nullpath );
	if ( pathnum > totalcnt ) return( NULL );
	if ( pathnum < 0 )
	{
		return( NULL );
	}

	cnt = 1;
	for( i=0; fullpath[i]; i++ )
	{
		if ( fullpath[i] == PATH_SEPARATOR )
		{
			cnt += 1;
			if ( cnt == pathnum ) return( &nullpath[i+1] );
		}
	}

	return( NULL );									/* This should never occur.		*/
}

/*
**	Routine:	WL_osd_ext()
**
**	Function:	Find the extension within a filename.
**
**	Description:	Returns a pointer to the first char that follows the last
**			period after the last directory separator.
**
**	Arguments:
**	filepath	The filepath to examine.
**
**	Globals:	None
**
**	Return:		Pointer to the extension within filepath or NULL if no ext.
**
**	Warnings:	None
**
**	History:	
**	old		Written by GSL
**	08/03/94	Documented and moved to paths.c. GSL
**
*/
char *WL_osd_ext(char *filepath)
{
	int	k;

	for( k=strlen(filepath); k>=0; k-- )
	{
		if ( filepath[k] == DIR_SEPARATOR ) return( NULL );

		if ( filepath[k] == '.' ) return( &filepath[k+1] );
	}

	return( NULL );
}

/*
**	Routine:	WL_whichenvpath()
**			WL_whichlinkpath()
**
**	Function:	Find the path of a program by searching the env PATH or LINK PATH
**
**	Description:	{Full detailed description}...
**
**	Arguments:
**	filename	The simple file name to find eg. "gcc"
**	fullpath	The returned full path to the file based on PATH eg "/usr/local/bin/gcc"
**
**	Globals:	None
**
**	Return:
**	0		Success
**	1		Not found on PATH
**
**	Warnings:	None
**
*/
int WL_whichenvpath(char *filename, char *fullpath)
{
	char	testname[256];
	int	pathcnt;
	int	not_found;

	not_found = 1;
	for(pathcnt=1; not_found && WL_env_path_seg(pathcnt); pathcnt++)	/* while not found & more paths		*/
	{
		/*
		**	Build test filepath
		*/
		buildfilepath( testname, WL_env_path_seg(pathcnt), filename);
		if (0 == access(testname,0))
		{
			not_found = 0;
			strcpy(fullpath,testname);
		}
	}
	return(not_found);

}
int WL_whichlinkpath(char *filename, char *fullpath)
{
	char	testname[256];
	int	pathcnt;
	int	not_found;

	not_found = 1;
	for(pathcnt=1; not_found && WL_link_path_seg(pathcnt); pathcnt++)	/* while not found & more paths		*/
	{
		/*
		**	Build test filepath
		*/
		buildfilepath( testname, WL_link_path_seg(pathcnt), filename);
		if (0 == access(testname,0))
		{
			not_found = 0;
			strcpy(fullpath,testname);
		}
	}
	return(not_found);

}


/*
**	History:
**	$Log: paths.c,v $
**	Revision 1.15  2011/10/29 20:09:14  gsl
**	Fix ISO routine name warnins on WIN32
**	
**	Revision 1.14  2003/01/31 18:48:36  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.13  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.12  2002/07/18 21:04:27  gsl
**	Remove MSDOS code
**	
**	Revision 1.11  2002/07/11 14:33:57  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.10  2002/07/09 04:13:59  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.9  2002/07/02 21:15:27  gsl
**	Rename wstrdup
**	
**	Revision 1.8  1997/12/04 23:09:50  gsl
**	Split the path routines into two; one for the env PATH and
**	on for the LINK PATH.
**	On unix these are the same but different on NT/95
**	
**	Revision 1.7  1996-10-08 20:24:04-04  gsl
**	replace getenv() with wispenvpath() calls
**
**	Revision 1.6  1996-08-19 15:32:39-07  gsl
**	drcs update
**
**
**
*/
