static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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

#if defined(unix) || defined(WIN32) || defined(MSDOS)

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include "idsistd.h"
#include "werrlog.h"

int4 filesize(char* path)                                             /* Return the length of the file in bytes.                      */
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
/*
**	History:
**	$Log: filesize.c,v $
**	Revision 1.10  1996-09-10 11:42:45-04  gsl
**	fix some of the ifdefs
**
**	Revision 1.9  1996-08-19 15:32:20-07  gsl
**	drcs update
**
**
**
*/
