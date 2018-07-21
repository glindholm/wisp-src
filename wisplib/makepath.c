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
**	makepath.c	was createpath.c and creatpath() before MSDOS port.  Name shortened to < 8 characters. 
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef WIN32
#include <io.h>
#include <direct.h>
#endif
#ifdef unix
#include <unistd.h>
#endif

#include "idsistd.h"
#include "paths.h"
#include "wisplib.h"

#include "idsisubs.h"
#include "wdefines.h"
#include "assert.h"



#define DS DIR_SEPARATOR			/* Directory Separator	*/

#ifdef WIN32
#define PS 3					/* Path Start (Skip this many chars in front of path to bypass root "E:\path")	*/

#else	/* unix */
#define PS 1					/* Path Start (Skip this many chars in front of path to bypass root "/path")	*/

#endif

/*
**	ROUTINE:	WL_makepath( )
**
**	FUNCTION:	Makes the path up to but not including the filename.
**
**	DESCRIPTION:	Makepath is passed a full path and makes all the intermediate directories.
**			The received path MUST be a cstr and can NEVER BE a COBOL PIC X.
**
**	ARGUMENTS:	(I)	fullpath
**
**	GLOBALS:	?
**
**	RETURN:		0       if the path now exists.
**			errno   if it was not able to make the path.
**
**	WARNINGS:	There is no possible way to ASSERT that the received path is a cstr.
**
*/
int	WL_makepath(const char* fullpath )
{
	char 	buff[WISP_FILEPATH_LEN];
	int	i,rc;
	int	ps=PS;
	char	*ptr;

	ASSERT(WISP_FILEPATH_LEN > strlen(fullpath));					/* weak test for is it a c str		*/

	strncpy(buff, fullpath, WISP_FILEPATH_LEN - 1 );				/* get a local copy of fullpath		*/
	buff[WISP_FILEPATH_LEN - 1] = '\0';						/* prevent a loop if fullpath is bad	*/

#ifdef WIN32
	/*
	**	Calculate the skip chars needed to get to the DIRPATH based on the name format.
	**
	**	\\SERVER\SHARE\DIRPATH\FILE
	**	C:\DIRPATH\FILE
	**	C:DIRPATH\FILE
	**	\DIRPATH\FILE
	**	DIRPATH\FILE
	*/
	if (0 == memcmp(buff,"\\\\",2))  /* If a UNC name \\SERVER\SHARE\DIR */
	{
		/* Skip the SERVER name */
		if (ptr = strchr(&buff[2],DS))
		{
			ps = ptr - buff + 1;
			/* Skip the SHARE name */
			if (ptr = strchr(&buff[ps],DS))
			{
				ps = ptr - buff + 1;
			}
		}
		else
		{
			ps = 2;
		}
	}
	else
	{
		/* Skip the drive */
		if (ptr = strchr(buff,':'))
		{
			ps = ptr - buff + 1;
			if (DS == buff[ps])
			{
				/* Skip the leading DS */
				ps++;
			}
		}
		else
		{
			ps = 1;
		}
	}
#endif /* WIN32 */

	/*
	**	First try the simple case: Strip off the filename and
	**	see if the directory exists.
	*/
	if ((ptr = strrchr(buff,DS)))
	{
		/* Find the last directory separator and null it out */
		*ptr = '\0';
		if (fexists(buff))
		{
			return(0);
		}
		*ptr = DS;
	}
										/* The dir-path does not exist so begin at the	*/
										/* top and make sure each dir exists or create	*/
										/* the directories.				*/

	for( i = ps; i < (int)strlen(buff); i++ )				/* Start at the PS position and search forward	*/
	{
		if ((ptr = strchr(&buff[i],DS)))
		{
			*ptr = '\0';
			if (!fexists(buff))					/* See if the directory exists, If it doesn't	*/
			{
#ifdef unix
				rc = mkdir( buff, 00777 );			/* Try to create it. (with mode)		*/
#endif
#ifdef WIN32
				rc = _mkdir(buff);				/* Try to create it. 		*/
#endif
				if ( rc < 0 )					/* If can't create the dir then			*/
				{
					return( errno );			/* Give up.					*/
				}

#ifdef unix
				chmod( buff, 0777);
#endif
			}
			*ptr = DS;						/* Reset directory separator for next loop	*/
			i = ptr - buff;
		}
		else
		{
			break;
		}
	}
	return( 0 );								/* The path now exists.				*/
}


/*	End of makepath.c source code.	*/
/*
**	History:
**	$Log: makepath.c,v $
**	Revision 1.22  2009/10/18 20:37:18  gsl
**	fix windows warnings
**	
**	Revision 1.21  2003/01/31 21:40:59  gsl
**	Fix -Wall warnings
**	
**	Revision 1.20  2003/01/31 18:48:36  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.19  2003/01/31 18:25:18  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.18  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.17  2002/07/10 04:27:37  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.16  2001/10/31 20:37:44  gsl
**	Removed VMS
**	Added chmod(0777) after a mkdir()
**	
**	Revision 1.15  1998-08-03 16:54:58-04  jlima
**	Support Logical Volume Translations to long file names containing eventual embedded blanks.
**
**	Revision 1.14  1998-07-08 15:58:11-04  gsl
**	Fix loading buff[] from fullpath[] loop termination condition.
**
**	Revision 1.13  1997-05-04 12:25:52-04  gsl
**	Fix of WIN32 to properly handle UNC names and relative paths.
**
**	Revision 1.12  1996-09-10 11:45:18-04  gsl
**	move system include before wisp includes,
**	combine WIN32 and unix code for mkdir()
**
**	Revision 1.11  1996-08-23 14:01:55-07  gsl
**	filepath parm isnow const
**
**	Revision 1.10  1996-08-19 15:32:29-07  gsl
**	drcs update
**
**
**
*/
