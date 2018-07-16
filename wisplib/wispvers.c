static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wispvers.c
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
**
**	Purpose:	WISP version numbers
**
**	Routines:	
**	wisp_version()	Return the WISP version string.
*/

/*
**	Includes
*/
#include "wcommon.h"
#include "wispvers.h"

char *wisp_version(void)
{
	static char the_version[] = WISP_VERSION;
	return the_version;
}

/*
**	History:
**	$Log: wispvers.c,v $
**	Revision 1.1  1995-06-14 11:05:53-04  gsl
**	Initial revision
**
**
**
*/
