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
			filename with possible extension into base_ext. If type is not null it will return the type of
			extension that was added to basename.
*/

#ifndef VMS	/* unix or MSDOS */

#include <stdio.h>
#include "wdefines.h"

#ifdef unix
static 	char 	*exts[] = { ".exe", ".com", ".sh",  ".gnt", ".int", NULL };		/* possible UNIX extensions		*/
#endif	/* #ifdef unix */

#ifdef MSDOS
static 	char 	*exts[] = { ".exe", ".com", ".bat", ".gnt", ".int", NULL };		/* possible MSDOS extensions		*/
#endif	/* #ifdef MSDOS */


int findexts(basename,base_ext,type)
char 	*basename;
char 	*base_ext;
int	*type;
{
	int 	i;
	char	base[100];
	char 	tmp[100];
	int	l_type;

	l_type = NOTFOUND;

	for( i = 0 ; basename[i] != ' ' && basename[i] != '\0' ; ++i )			/* Move basename to base until space.	*/
	{
		base[i] = basename[i];
	}
	base[i] = '\0';									/* Terminate with null char.		*/


#ifdef unix
	if ( access(base, 0) == 0 )							/* Look for basename with no extension.	*/
	{
		strcpy(base_ext,base);
		l_type = NOEXT;
	}

	if ( l_type == NOTFOUND && ! osd_ext(base) )
	{
#endif	/* #ifdef unix */								/* unix and MSDOS section.	*/

		for (i=0; exts[i] != NULL ; ++i)					/* For each extension.			*/
		{
			sprintf(tmp,"%s%s",base,exts[i]);				/* Put basename and extension into tmp.	*/
			if ( access(tmp, 0) == 0 )					/* Does tmp file exist?			*/
			{								/* If it does,				*/
				strcpy(base_ext,tmp);					/* Move the path (tmp) to base_ext.	*/
				l_type = i;						/* Set l_type to the extension index.	*/
				break;							/* Break out of this "for" loop.	*/
			}
		}

#ifdef unix
	}
#endif

	if ( type ) *type = l_type;							/* If type is a ptr, put l_type in it.	*/

	if ( l_type == NOTFOUND ) return(1);						/* return 1 if not found, 0 if found.	*/
	else			  return(0);
}

#endif		/* #ifndef VMS ( unix or MSDOS ) */
