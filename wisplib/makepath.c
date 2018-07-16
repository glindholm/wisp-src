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
#include "idsistd.h"

#ifndef VMS	/* unix or MSDOS */
#include <memory.h>
#endif

#ifdef MSDOS
#include <memory.h>
#include <io.h>
#include <direct.h>
#endif

#ifdef VMS

/*
	makepath	Makes the path upto but not including the filename.
			Makepath is passed a full path and makes all the intermediate directories.
			Makepath returns	0       if the path now exists.
						errno   if it was not able to make the path.
*/

int	makepath( fullpath )
char	*fullpath;
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

#ifndef VMS	/* unix and MSDOS */

#ifdef MSDOS
#define DS '\\'					/* Directory Separator	*/
#define PS 3					/* Path Start (Skip this many chars in front of path to bypass root "E:\path")	*/

#else	/* unix */
#define DS '/'					/* Directory Separator	*/
#define PS 1					/* Path Start (Skip this many chars in front of path to bypass root "/path")	*/

#endif

/*
	makepath	Makes the path upto but not including the filename.
			Makepath is passed a full path and makes all the intermediate directories.
			Makepath returns	0       if the path now exists.
						errno   if it was not able to make the path.
*/

int	makepath( fullpath )
char	*fullpath;
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
										/* Strip the filename from the path and then	*/
										/* see if the directory path exists.		*/
 
	for( i=strlen(buff); i > PS; i-- )					/* Scan backwards for the begining of filename.	*/
	{
		if ( fullpath[i] == DS )
		{
			buff[i] = '\0';						/* Null-Term the dir-path.			*/
			if ( fexists(buff) )					/* Check if dir-path exists.			*/
			{
				return(0);					/* Dir-path exists, all is fine.		*/
			}
			buff[i] = DS;						/* Put back the directory separator		*/
			break;
		}
	}
										/* The dir-path does not exist so begin at the	*/
										/* top and make sure each dir exists or create	*/
										/* the directories.				*/

	for( i = PS; i < (int)strlen(buff); i++ )				/* Start at the PS position and search forward	*/
	{
		if ( fullpath[i] == DS )					/* If you find a directory separator then	*/
		{
			buff[i] = '\0';
			if (!fexists(buff))					/* See if the directory exists, If it doesn't	*/
			{
#ifdef MSDOS
				rc = mkdir( buff );				/* Try to create it.				*/
#else	/* unix or VMS */
				rc = mkdir( buff, 00777 );			/* Try to create it. (with mode)		*/
#endif
				if ( rc < 0 )					/* If can't create the dir then			*/
				{
					return( errno );			/* Give up.					*/
				}
			}
			buff[i] = DS;						/* Reset directory separator for next loop	*/
		}
	}
	return( 0 );								/* The path now exists.				*/
}
#endif	/* unix  and MSDOS */

/*	End of makepath.c source code.	*/
