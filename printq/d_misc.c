/*
 * Module:  daemon_misc
 * Program: IDSIprint
 * Purpose: misc routines for daemon
 *
 * $Log: d_misc.c,v $
 * Revision 1.5  1992/06/09  23:14:41  jockc
 * posix fix
 *
 * Revision 1.4  1992/06/08  23:55:47  jockc
 * polish code to remove some warnings
 *
 * Revision 1.3  1991/10/01  17:06:09  jockc
 * open errlog in logerr if not already open
 *
 * Revision 1.2  1991/04/19  00:47:20  jockc
 * *** empty log message ***
 *
 * Revision 1.1  1991/04/18  23:49:38  jockc
 * Initial revision
 *
 *
 */
static char rcsid[] = "$Id:$";

#define EXT extern 
#include "defs.h"
#include "daemon.h"

/*
	nextfile()	This routine returns a pointer to the next enter in a directory (sorted).
                        Borrowed from wisplib.
	
			Context is a ptr to a ptr the point to a malloced struct that stores all the pointer and counter
			associated with a given path.

			The first time you call nextfile you must set *context=0;
			When you are finished reading a given path call nextfile with path=0, this frees up everything.

			When it is first called it reads the whole dir into memory and sorts it. The memory is freed
			when nextfile is called after the last entry is read or by calling with a path=0.

			If you change paths without the memory being freed no one knows what will happen. (Actually nextfile
			will continue to give you entries from the previous dir until exhausted.)
*/

char *nextfile(path,context)
char *path;
char **context;
{
	DIR 	*curdir;
	char	*ptr;								/* temp ptr					*/
	int strcmp();
	
	struct context_struct
	{
		int diropen_flag;						/* Flag that say if a dir is open.		*/
		int slots_used;							/* Number of memory slots used.			*/
		char *slots;							/* Ptr to alloced memory			*/
		int slots_returned;						/* Number of slots returned to caller		*/
		char *ret_ptr;							/* ptr to next slot to return.			*/
	};

#define CT ((struct context_struct *)(*context))
#define FILENAMESIZE 16


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

	if ( ! path )								/* If path=0 do all the cleanup				*/
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

			strcpy(ptr,dp->d_name);
			ptr += FILENAMESIZE;
			CT->slots_used += 1;
		}

		qsort(CT->slots, CT->slots_used, FILENAMESIZE, strcmp);		/* Sort the table of dir entries		*/

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
time_t filetime(path)
char *path;
{
	struct stat buf;
	
	stat(path,&buf);
	return buf.st_ctime;
}
logerr(fmt,errno)
char *fmt;
int errno;
{
	static char errbuf[200];
	char tmpbuf[100];
	char ctbuf[40];
	time_t t,time();
	
	if (errlog==NULL)
	  errlog=fopen(DEFERRLOG,"a");

	t=time((time_t*)0);
	strcpy(ctbuf,ctime(&t));
	if (ctbuf[strlen(ctbuf)-1]=='\n') ctbuf[strlen(ctbuf)-1]=(char)0;
	sprintf(tmpbuf,fmt,errno);
	
	sprintf(errbuf,"%s:%s",ctbuf,tmpbuf);
	fprintf(errlog,errbuf);
	fflush(errlog);
}
