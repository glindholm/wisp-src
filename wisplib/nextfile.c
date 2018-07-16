			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	nextfile.c
*/


/*
	nextfile()	This routine returns a pointer to the next entery in a directory (sorted).

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

#ifdef unix

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include "idsistd.h"
#include "wdefines.h"


#ifdef strcmp
#undef strcmp
#endif /* strcmp */
int strcmp();

char *nextfile(path,context)
char *path;
char **context;
{
	DIR 	*curdir;
	char	*ptr;								/* temp ptr					*/

	struct context_struct
	{
		int diropen_flag;						/* Flag that say if a dir is open.		*/
		int slots_used;							/* Number of memory slots used.			*/
		char *slots;							/* Ptr to alloced memory			*/
		int slots_returned;						/* Number of slots returned to caller		*/
		char *ret_ptr;							/* ptr to next slot to return.			*/
	};

#define CT ((struct context_struct *)(*context))
#define FILENAMESIZE 32


	if ( *context == NULL )							/* IF NULL the alloc a CONTEXT			*/
	{
		*context = (char *)malloc( sizeof( struct context_struct ) );
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

		CT->slots = (char *)malloc( 100 * FILENAMESIZE );		/* alloc 100 slots.				*/
		if ( ! CT->slots )
		{
			closedir(curdir);
			free( *context );
			*context = NULL;
			return(NULL);						/* Unable to alloc memory			*/
		}
		slots_alloced 	= 100;
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

				slots_alloced += 100;				/* Alloc another 100 slots			*/
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

			if (strlen(dp->d_name) < FILENAMESIZE)
			{
				strcpy(ptr,dp->d_name);
			}
			else							/* Filename is too large			*/
			{
				memcpy(ptr,dp->d_name,FILENAMESIZE-1);
				ptr[FILENAMESIZE-1] = (char)0;
			}

			ptr += FILENAMESIZE;
			CT->slots_used += 1;
		}

#ifdef OSF1_ALPHA
		qsort(CT->slots, CT->slots_used, FILENAMESIZE,			/* Sort the table of dir entries		*/
		      ( int (*) (const void *,const void *) )strcmp);
#else
		qsort(CT->slots, CT->slots_used, FILENAMESIZE, strcmp);		/* Sort the table of dir entries		*/
#endif

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

#ifdef MSDOS					/* Please note: this MSDOS section is different from the unix section above.	*/

#include <dos.h>
#include <errno.h>
#include <malloc.h>
#include <search.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include "wdefines.h"

#ifdef strcmp
#undef strcmp
#endif /* strcmp */
int strcmp();

				/*	#ifdef MSDOS	*/
char *nextfile(path,context)
char *path;
char **context;
{
	char	*ptr;								/* temp ptr					*/

	struct context_struct
	{
		int diropen_flag;						/* Flag that say if a dir is open.		*/
		int slots_used;							/* Number of memory slots used.			*/
		char *slots;							/* Ptr to alloced memory			*/
		int slots_returned;						/* Number of slots returned to caller		*/
		char *ret_ptr;							/* ptr to next slot to return.			*/
	};

#define CT ((struct context_struct *)(*context))
#define FILENAMESIZE 13								/* NAME[8] + DOT[1] + EXTENSION[3] + NULL[1]	*/


	if ( *context == NULL )							/* IF NULL the alloc a CONTEXT			*/
	{
		*context = (char *)malloc( sizeof( struct context_struct ) );
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
		char wildpath[256];						/* path + wild card (*.*) for directory search.	*/
		struct find_t fi;						/* MSDOS Directory file info structure.		*/
		int slots_alloced;						/* Number of memory slots available.		*/
		int	len;

										/* This MSDOS specific function starts a read	*/
										/* of a directory.  Use an (OR)ing of these:	*/
										/*						*/
										/* Attribute	Description			*/
										/*						*/
										/* _A_NORMAL	Normal file without any read or	*/
										/*		write restrictions.		*/
										/* _A_RDONLY	File cannot be opened for write	*/
										/*		operations.			*/
										/* _A_HIDDEN	File will not show up on	*/
										/*		directory search.		*/
										/* _A_SYSTEM	File is marked as a system file	*/
										/*		and will be excluded from	*/
										/*		normal directory searches.	*/
										/* _A_VOLID	Volume name; can exist only in	*/
										/*		root directory.			*/
										/* _A_SUBDIR	Subdirectory name (meaning the	*/
										/*		file is a subdirectory).	*/
										/* _A_ARCH	If set, file will be archived	*/
										/*		by MS-DOS BACKUP command.  This	*/
										/*		attribute is set after any	*/
										/*		changes to the file.		*/
		strcpy( wildpath, path );
		len = strlen(wildpath);
		if (len > 0 && wildpath[len-1] != '\\')
		{
			strcat( wildpath, "\\" );
		}
		strcat( wildpath, "*.*" );
										/* Turbo C and C++ function name is "findfirst"	*/
		if ( 0 != ( _dos_findfirst ( wildpath, _A_NORMAL | _A_RDONLY | _A_SUBDIR, &fi ) ) )
		{
			free( *context );
			*context = NULL;
			return(NULL);						/* unable to open.				*/
		}
		CT->diropen_flag = TRUE;					/* Dir was opened.				*/

		CT->slots = (char *)malloc( 100 * FILENAMESIZE );		/* alloc 100 slots.				*/
		if ( ! CT->slots )
		{
			free( *context );
			*context = NULL;
			return(NULL);						/* Unable to alloc memory			*/
		}
		slots_alloced 	= 100;
		CT->slots_used	= 0;

		ptr = CT->slots;
		for (;;)							/* read the whole dir into memory		*/
		{
			if ( CT->slots_used >= slots_alloced )
			{
				char	*tslots;

				slots_alloced += 100;				/* Alloc another 100 slots			*/
				tslots = (char *)realloc( CT->slots, slots_alloced * FILENAMESIZE );
				if ( ! tslots )
				{
					free( CT->slots );
					free( *context );
					*context = NULL;
					return(NULL);				/* Can't get the space				*/
				}
				CT->slots = tslots;
				ptr = &(CT->slots[ CT->slots_used * FILENAMESIZE ]); /* Reset ptr				*/

			}

			strcpy(ptr,fi.name);
			ptr += FILENAMESIZE;
			CT->slots_used += 1;
										/* Turbo C and C++ function name is "findnext"	*/
			if ( 0 != _dos_findnext ( &fi ) )			/* Read the next directory entry.		*/
			{
				break;
			}	
		}

		qsort(CT->slots, CT->slots_used, FILENAMESIZE, (int(*)(const void*,const void*))strcmp);
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
#endif	/* MSDOS */
