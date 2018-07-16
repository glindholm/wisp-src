			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	filesize.c
*/

#ifdef unix

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "werrlog.h"

long filesize(path)						/* Return the length of the file in bytes.			*/
char	*path;
{
#undef          ROUTINE
#define		ROUTINE		65300
	struct stat buf;

	werrlog(ERRORCODE(1),errno,0,0,0,0,0,0,0);

	if ( stat(path,&buf) )
	{
		werrlog(ERRORCODE(2),errno,path,0,0,0,0,0,0);
		return( -1 );
	}

	return( buf.st_size );
}

#endif
