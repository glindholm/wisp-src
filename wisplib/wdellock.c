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
	wdellock:	Delete Lock file.

			This routine is called from COBOL after a successful OPEN to delete the lock file create when
			a temporary ## file is created.

			This is a VMS only routine.
*/

#ifdef VMS

#include "idsistd.h"
#include "wcommon.h"

void wdellock(mode,file)
int4	*mode;
char	*file;
{
	char	buff[100];
	char	*ptr, *strchr();

	if (!(*mode & IS_TEMP))								/* If not a temp file return.		*/
	{
		return;
	}

	if (file[0] == ' ' || file[0] == (char)0)
	{
		return;
	}

	memcpy(buff,file,80);
	if (ptr = strchr(buff,' '))
	{
		*ptr = (char)0;								/* Null terminate a blank padded string	*/
	}

	strcat(buff,";1");								/* Append a ";1" version number		*/

	unlink(buff);									/* Delete the lock file.		*/
}

#else
static int dummy_wdellock;
#endif
	
/*
**	History:
**	$Log: wdellock.c,v $
**	Revision 1.10  1996-08-19 18:33:09-04  gsl
**	drcs update
**
**
**
*/
