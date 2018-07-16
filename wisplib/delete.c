static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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

/*
**	History:
**	$Log: delete.c,v $
**	Revision 1.8  1996-08-19 18:32:16-04  gsl
**	drcs update
**
**
**
*/
