#include "idsistd.h"
#ifndef VMS	/* unix or MSDOS */

#include <stdio.h>
#include <errno.h>

#ifdef unix
delete( path )
char *path;
{
	return( unlink( path ) );
}
#endif	/* unix */

#ifdef MSDOS
#include <io.h>

delete( path )
char *path;
{
	return( unlink( path ) );
}
#endif	/* MSDOS*/

#endif	/* unix or MSDOS */

