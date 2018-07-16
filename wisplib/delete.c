static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#ifdef MSDOS

#include <stdio.h>
#include <errno.h>
#include <io.h>

delete( path )
char *path;
{
	return( unlink( path ) );
}
#endif	/* MSDOS*/


/*
**	History:
**	$Log: delete.c,v $
**	Revision 1.9  1999-01-29 14:30:03-05  gsl
**	This is obsolete and is MSDOS only
**
**	Revision 1.8  1996-08-19 18:32:16-04  gsl
**	drcs update
**
**
**
*/
