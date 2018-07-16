			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/*
	findexts:	This routine takes a file basename and trys to find the complete filename by adding extensions.
			It returns 0 if the file was found and 1 if not found.  If the file was found it loads the found
			filename with possible extension into base_ext.
*/

#ifndef VMS	/* unix or MSDOS */

#include <stdio.h>
#include <string.h>
#include "idsistd.h"
#include "wdefines.h"
#include "idsisubs.h"

extern char *g_exts[];								/* Possible extensions (from wfname)		*/

int findexts(basename,base_ext)
char 	*basename;
char 	*base_ext;
{
	int 	i;
	char	base[100];
	char 	tmp[100];
	char	*ptr;
	int	not_found;

	not_found = 1;
	unloadpad(base,basename,80);
	strcpy(tmp,splitext(base));
	if (*tmp)									/* If there is an extension remove it	*/
	{
		ptr = strrchr(base,'.');
		*ptr = (char)0;
	}

	if ( fexists(base) )								/* Look for basename with no extension.	*/
	{
		strcpy(base_ext,base);
		not_found = 0;
	}
	else
	{
		for (i=0; g_exts[i] != NULL ; ++i)					/* For each extension.			*/
		{
			sprintf(tmp,"%s%s",base,g_exts[i]);				/* Put basename and extension into tmp.	*/
			if ( fexists(tmp) )						/* Does tmp file exist?			*/
			{								/* If it does,				*/
				strcpy(base_ext,tmp);					/* Move the path (tmp) to base_ext.	*/
				not_found = 0;
				break;							/* Break out of this "for" loop.	*/
			}
		}
	}

	return(not_found);								/* return 1 if not found, 0 if found.	*/
}

#endif		/* #ifndef VMS ( unix or MSDOS ) */
