			/************************************************************************/
			/*                                                                      */
			/*              WISP - Wang Interchange Source Pre-processor            */
			/*                     Copyright (c) 1988, 1989, 1990, 1991             */
			/*       An unpublished work of International Digital Scientific Inc.   */
			/*                          All rights reserved.                        */
			/*                                                                      */
			/************************************************************************/

/*
**      filesize.c
*/

#ifndef VMS

#ifdef unix
#include <sys/types.h>
#include <sys/stat.h>
#endif
#ifdef MSDOS
#include <sys\types.h>
#include <sys\stat.h>
#endif
#include <errno.h>
#include "idsistd.h"
#include "werrlog.h"

int4 filesize(path)                                             /* Return the length of the file in bytes.                      */
char    *path;
{
#undef          ROUTINE
#define         ROUTINE         65300
	struct stat buf;

	werrlog(ERRORCODE(1),path,0,0,0,0,0,0,0);

	if ( stat(path,&buf) )
	{
		werrlog(ERRORCODE(2),errno,path,0,0,0,0,0,0);
		return( -1 );
	}

	return( buf.st_size );
}

#endif
