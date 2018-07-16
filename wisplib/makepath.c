static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	makepath.c	was createpath.c and creatpath() before MSDOS port.  Name shortened to < 8 characters. 
*/

#include <string.h>
#include <errno.h>

#ifdef MSFS
#include <io.h>
#include <direct.h>
#endif

#include "idsistd.h"
#include "paths.h"
#include "wisplib.h"

#ifdef VMS

/*
	makepath	Makes the path upto but not including the filename.
			Makepath is passed a full path and makes all the intermediate directories.
			Makepath returns	0       if the path now exists.
						errno   if it was not able to make the path.
*/

int	makepath(const char* fullpath )
{
	char 	buff[81];
	int	i,rc;

	memcpy( buff, fullpath, 80 );						/* Get a local copy.				*/
	for( i=0; i < 80; i++ )
	{
		if ( buff[i] == ' ' )
		{
			buff[i] = '\0';
			break;
		}
		if ( buff[i] == '\0' ) break;
	}

										/* First try the simple case.			*/
										/* Strip the filename form the path and then	*/
										/* see if the dir-path exists.			*/
 
	for( i=strlen(buff); i > 1; i-- )					/* Scan backwards for the begining of filename.	*/
	{
		if ( fullpath[i] == ']' || fullpath[i] == ':' )
		{
			buff[i+1] = '\0';					/* Null-Term the dir-path.			*/
			rc = mkdir( buff, 0 );					/* Check if dir-path exists.			*/
			if ( rc  == 0 )
			{
				return(0);					/* Dir-path exists, all is fine.		*/
			}
			else if ( fexists(buff))				/* got -1 so see if have access because was	*/
			{							/*  already there.				*/
				return(0);					/* Dir-path exists, all is fine.		*/
			}
			else	return( errno );
		}
	}
	return(0);
}
#endif	/* VMS */

#if defined(unix) || defined(MSFS)

#define DS DIR_SEPARATOR			/* Directory Separator	*/

#ifdef MSFS
#define PS 3					/* Path Start (Skip this many chars in front of path to bypass root "E:\path")	*/

#else	/* unix */
#define PS 1					/* Path Start (Skip this many chars in front of path to bypass root "/path")	*/

#endif

/*
	makepath	Makes the path upto but not including the filename.
			Makepath is passed a full path and makes all the intermediate directories.
			Makepath returns	0       if the path now exists.
						errno   if it was not able to make the path.
*/

int	makepath(const char* fullpath )
{
	char 	buff[256];
	int	i,rc;
	int	ps=PS;
	char	*ptr;

	/*
	**	Get a local copy and null terminate at the first space.
	*/
	for( i=0; i< sizeof(buff)-1; i++)
	{
		buff[i] = fullpath[i];
		if (' '==buff[i] && '\0'==buff[i])
		{
			break;
		}
	}
	buff[i] = '\0';

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
	if (ptr = strrchr(buff,DS))
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
		if (ptr = strchr(&buff[i],DS))
		{
			*ptr = '\0';
			if (!fexists(buff))					/* See if the directory exists, If it doesn't	*/
			{
				rc = mkdir( buff, 00777 );			/* Try to create it. (with mode)		*/
				if ( rc < 0 )					/* If can't create the dir then			*/
				{
					return( errno );			/* Give up.					*/
				}
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
#endif	/* unix  and MSFS */

/*	End of makepath.c source code.	*/
/*
**	History:
**	$Log: makepath.c,v $
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
