static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		setenvst.c
**
**	Project:	WISP
**
**	RCS:		$Source:$
**
**	Purpose:	Simplify setting an environment string
**
**	Routines:	
**	setenvstr()
*/

#include <stdlib.h>

#include "setenvst.h"
#include "wmalloc.h"

/*
**	ROUTINE:	setenvstr()
**
**	FUNCTION:	Set a environment string
**
**	DESCRIPTION:	This is a frontend for putenv() which mallocs a duplicate of the string.
**
**	ARGUMENTS:	
**	envstring	The envirnment string of the form "variable=value" eg. "WISPTERM=vt100"
**
**	GLOBALS:	none
**
**	RETURN:		
**	0		Success
**	2		putenv failed
**
**	WARNINGS:	None
**
*/
int setenvstr(const char* envstring)
{
	if ( putenv( wstrdup(envstring) ) )
	{
		return(2);
	}

	return(0);
}

/*
**	History:
**	$Log: setenvst.c,v $
**	Revision 1.10  1996-10-14 12:25:34-04  gsl
**	rewrote to use wstrdup()
**
**	Revision 1.9  1996-08-19 15:32:53-07  gsl
**	drcs update
**
**
**
*/
