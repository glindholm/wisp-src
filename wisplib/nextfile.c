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
**	nextfile.c
*/


/*
	WL_nextfile()	This routine returns a pointer to the next entery in a directory (sorted).

			Context is a ptr to a ptr the point to a malloced struct that stores all the pointer and counter
			associated with a given path.

			The first time you call nextfile you must set *context=0;
			When you are finished reading a given path call nextfile with path=0, this frees up everything.

			When it is first called it reads the whole dir into memory and sorts it. The memory is freed
			when nextfile is called after the last entry is read or by calling with a path=0.

			If you change paths without the memory being freed no one knows what will happen. (Actually nextfile
			will continue to give you entries from the previous dir until exhausted.)


			Limit of 32 char filenames.
*/

#define SLOT_CHUNK	100

struct context_struct
	{
		int diropen_flag;						/* Flag that say if a dir is open.		*/
		int slots_used;							/* Number of memory slots used.			*/
		char *slots;							/* Ptr to alloced memory			*/
		int slots_returned;						/* Number of slots returned to caller		*/
		char *ret_ptr;							/* ptr to next slot to return.			*/
	};

#define CT ((struct context_struct *)(*context))


#ifdef unix

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include "idsistd.h"
#include "wdefines.h"
#include "wmalloc.h"

#ifdef NOCONST
#define const
#endif

int WL_nexfile_q_strcmp(const void *str1, const void *str2)
{
	return(strcmp((const char *)str1,(const char *)str2));
}

char *WL_nextfile(char* path, char** context)
{
	DIR 	*curdir;
	char	*ptr;								/* temp ptr					*/

#define FILENAMESIZE 32


	if ( *context == NULL )							/* IF NULL the alloc a CONTEXT			*/
	{
		*context = (char *)wisp_malloc( sizeof( struct context_struct ) );
		if ( ! *context )
		{
			return(NULL);
		}
		CT->diropen_flag   = FALSE;
		CT->slots_used     = 0;
		CT->slots          = NULL;
		CT->slots_returned = 0;
		CT->ret_ptr        = NULL;
	}

	if ( ! path )								/* If path=0 do all the cleanup		       	*/
	{
		if ( CT->diropen_flag )
		{
			free( CT->slots );
		}
		free( *context );
		*context = NULL;
		return(NULL);
	}

	if ( ! CT->diropen_flag )						/* Open the dir, Read into mem & Sort.		*/
	{
		int slots_alloced;						/* Number of memory slots available.		*/

		if (!(curdir = opendir(path))) 
		{
			free( *context );
			*context = NULL;
			return(NULL);						/* unable to open.				*/
		}
		CT->diropen_flag = TRUE;					/* Dir was opened.				*/

		CT->slots = (char *)wisp_malloc( SLOT_CHUNK * FILENAMESIZE );	/* alloc  slots.				*/
		if ( ! CT->slots )
		{
			closedir(curdir);
			free( *context );
			*context = NULL;
			return(NULL);						/* Unable to alloc memory			*/
		}
		slots_alloced 	= SLOT_CHUNK;
		CT->slots_used	= 0;

		ptr = CT->slots;
		for (;;)							/* read the whole dir into memory		*/
		{
			struct dirent *dp;					/* Pointer to a dir enter			*/

			if ((dp=readdir(curdir))==NULL)				/* Read the dir.				*/
			{
				closedir(curdir);
				break;
			}	

			if ( CT->slots_used >= slots_alloced )
			{
				char	*tslots;

				slots_alloced += SLOT_CHUNK;			/* Alloc another chuck of slots			*/
				tslots = (char *)realloc( CT->slots, slots_alloced * FILENAMESIZE );
				if ( ! tslots )
				{
					closedir(curdir);
					free( CT->slots );
					free( *context );
					*context = NULL;
					return(NULL);				/* Can't get the space				*/
				}
				CT->slots = tslots;
				ptr = &(CT->slots[ CT->slots_used * FILENAMESIZE ]); /* Reset ptr				*/

			}

			/* 
			** Only add the file name if it fits.
			** (Previously truncated file names)
			*/
			if (strlen(dp->d_name) < FILENAMESIZE)
			{
				strncpy(ptr, dp->d_name, FILENAMESIZE);
				ptr[FILENAMESIZE-1] = (char)0;

				ptr += FILENAMESIZE;
				CT->slots_used += 1;
			}
		}
										/* Sort the table of dir entries		*/
		qsort(CT->slots, CT->slots_used, FILENAMESIZE, WL_nexfile_q_strcmp);

		CT->slots_returned = 0;
		CT->ret_ptr = CT->slots;					/* Setup ret_ptr to begin returning		*/
	}

	if ( CT->slots_returned >= CT->slots_used )				/* All entries returned.			*/
	{
		free(CT->slots);
		free( *context );
		*context = NULL;
		return(NULL);
	}

	ptr = CT->ret_ptr;
	CT->ret_ptr += FILENAMESIZE;
	CT->slots_returned += 1;

	return(ptr);
}
#endif	/* unix */

#ifdef unix
char *WL_s_nextfile(const char* path, int* first)
{
	static DIR *curdir;
	static struct dirent *dp;

	if ( *first )
	{
		*first = 0;

		if (!(curdir = opendir(path))) 
		{
			return(NULL);
		}
	}

	for (;;)
	{
		if ((dp=readdir(curdir))==NULL)
		{
			closedir(curdir);
			return(NULL);
		}

		if (0 != strcmp(dp->d_name,".") &&				/* skip entry for . */
		    0 != strcmp(dp->d_name,"..")   )				/* skip entry for .. */
		{
			return dp->d_name; 
		}
	}
}
#endif	/* unix */


#ifdef WIN32

#include <windows.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include "wdefines.h"
#include "paths.h"
#include "wmalloc.h"

char *WL_nextfile(char* path, char** context)
{
	char	*ptr;								/* temp ptr					*/

#define FILENAMESIZE 32


	if ( *context == NULL )							/* IF NULL the alloc a CONTEXT			*/
	{
		*context = (char *)wisp_malloc( sizeof( struct context_struct ) );
		if ( ! *context )
		{
			return(NULL);
		}
		CT->diropen_flag   = FALSE;
		CT->slots_used     = 0;
		CT->slots          = NULL;
		CT->slots_returned = 0;
		CT->ret_ptr        = NULL;
	}

	if ( ! path )								/* If path=0 do all the cleanup		       	*/
	{
		if ( CT->diropen_flag )
		{
			free( CT->slots );
		}
		free( *context );
		*context = NULL;
		return(NULL);
	}

	if ( ! CT->diropen_flag )						/* Open the dir, Read into mem & Sort.		*/
	{
		char wildpath[256];						/* path + wild card (*) for directory search.	*/
		int slots_alloced;						/* Number of memory slots available.		*/
		int	len;

		WIN32_FIND_DATA fi;
		HANDLE fh;

		strcpy( wildpath, path );
		len = strlen(wildpath);
		if (len > 0 && wildpath[len-1] != DIR_SEPARATOR)
		{
			strcat( wildpath, DIR_SEPARATOR_STR );
		}
		strcat( wildpath, "*" );

		fh = FindFirstFile(wildpath, &fi);
		if (INVALID_HANDLE_VALUE == fh)
		{
			free( *context );
			*context = NULL;
			return(NULL);						/* unable to open.				*/
		}
		CT->diropen_flag = TRUE;					/* Dir was opened.				*/

		CT->slots = (char *)wisp_malloc( SLOT_CHUNK * FILENAMESIZE );	/* alloc  slots.				*/
		if ( ! CT->slots )
		{
			free( *context );
			*context = NULL;
			FindClose(fh);
			return(NULL);						/* Unable to alloc memory			*/
		}
		slots_alloced 	= SLOT_CHUNK;
		CT->slots_used	= 0;

		ptr = CT->slots;
		for (;;)							/* read the whole dir into memory		*/
		{
			if ( CT->slots_used >= slots_alloced )
			{
				char	*tslots;

				slots_alloced += SLOT_CHUNK;			/* Alloc another 100 slots			*/
				tslots = (char *)realloc( CT->slots, slots_alloced * FILENAMESIZE );
				if ( ! tslots )
				{
					free( CT->slots );
					free( *context );
					*context = NULL;

					FindClose(fh);

					return(NULL);				/* Can't get the space				*/
				}
				CT->slots = tslots;
				ptr = &(CT->slots[ CT->slots_used * FILENAMESIZE ]); /* Reset ptr				*/

			}

			/* 
			** Only add the file name if it fits.
			** (Previously truncated file names)
			*/
			if (strlen(fi.cFileName) < FILENAMESIZE)
			{
				strncpy(ptr, fi.cFileName, FILENAMESIZE);
				ptr[FILENAMESIZE-1] = (char)0;

				ptr += FILENAMESIZE;
				CT->slots_used += 1;
			}

			/* Read the next directory entry. */
			if (FALSE == FindNextFile(fh, &fi))
			{
				FindClose(fh);
				
				break;
			}	
		}

		qsort(CT->slots, CT->slots_used, FILENAMESIZE, (int(*)(const void*,const void*))(strcmp));
										/* Sort the table of dir entries		*/

		CT->slots_returned = 0;
		CT->ret_ptr = CT->slots;					/* Setup ret_ptr to begin returning		*/
	}

	if ( CT->slots_returned >= CT->slots_used )				/* All entries returned.			*/
	{
		free(CT->slots);
		free( *context );
		*context = NULL;
		return(NULL);
	}

	ptr = CT->ret_ptr;
	CT->ret_ptr += FILENAMESIZE;
	CT->slots_returned += 1;

	return(ptr);
}

char *WL_s_nextfile(const char* path, int* first)
{
	char	wildpath[MAX_PATH];
	static	WIN32_FIND_DATA fileinfo;
	static	HANDLE fh;

getnextfile:

	if ( *first )
	{
		*first = 0;

		if (strlen(path) > sizeof(wildpath)-4)
		{
			return(NULL);
		}

		strcpy( wildpath, path );
		strcat( wildpath, "\\*" );

		fh = FindFirstFile(wildpath, &fileinfo);
		if (INVALID_HANDLE_VALUE == fh)
		{
			return(NULL);						/* unable to open.				*/
		}
	}
	else
	{
		if (FALSE == FindNextFile(fh, &fileinfo))
		{
			FindClose(fh);
			return(NULL);
		}
	}

	if (0==strcmp(fileinfo.cFileName,".") || 
	    0==strcmp(fileinfo.cFileName,"..")   )
	{
		/*
		**	Skip "." and ".."
		*/
		goto getnextfile;
	}

	return( fileinfo.cFileName );
}
#endif	/* WIN32 */
/*
**	History:
**	$Log: nextfile.c,v $
**	Revision 1.24  2003/08/04 15:10:16  gsl
**	Filenames that are too long are not return.
**	previously they were truncated.
**	
**	Revision 1.23  2003/08/01 20:43:04  gsl
**	fixed so doesn't truncate the file name, either returns full name or ignores it
**	
**	Revision 1.22  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.21  2002/07/12 19:10:14  gsl
**	Global unique WL_ changes
**	
**	Revision 1.20  2002/07/11 16:11:45  gsl
**	Fix warnings
**	
**	Revision 1.19  2002/07/10 04:27:36  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.18  2002/07/02 21:15:27  gsl
**	Rename wstrdup
**	
**	Revision 1.17  2002/06/25 15:21:53  gsl
**	Change to use wmalloc()
**	
**	Revision 1.16  2002/06/21 03:10:38  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.15  1996/09/03 21:43:32  gsl
**	For NT don't return files "." or ".." (just like on unix)
**	
**	Revision 1.14  1996-08-19 15:32:35-07  gsl
**	drcs update
**
**
**
*/
