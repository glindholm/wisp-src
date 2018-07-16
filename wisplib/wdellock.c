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

#include "wcommon.h"

wdellock(mode,file)
long	*mode;
char	*file;
{
	char	buff[100];
	char	*ptr, *strchr();

	if (!(*mode & IS_TEMP))								/* If not a temp file return.		*/
	{
		return;
	}

	memcpy(buff,file,80);
	ptr = strchr(buff,' ');
	if ( !ptr ) return;								/* If messed up then return.		*/

	*ptr++	= ';';									/* Append a ";1" version number		*/
	*ptr++	= '1';
	*ptr	= '\0';

	delete(buff);									/* Delete the lock file.		*/
}

#endif
	
