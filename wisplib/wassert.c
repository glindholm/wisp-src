static char copyright[]="Copyright (c) 1995-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wassert.c
**
**	Project:	WISP
**
**	RCS:		$Source:$
**
**	Purpose:	Define wisp_assert()
**
**	Routines:	
**	wisp_assert()	Issue the assertion error and exit.
*/

/*
**	Includes
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "assert.h"
#include "werrlog.h"
#include "wexit.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/

/*
**	ROUTINE:	wisp_assert()
**
**	FUNCTION:	Issue the assertion error and exit
**
**	DESCRIPTION:	
**
**	ARGUMENTS:
**	cond		The condition that failed
**	file		The source file name
**	line		The line number
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void wisp_assert(char *cond, char *file, int line)
{
	char	buf[128];
	
	sprintf(buf,"ASSERT(%s) Failed: File=%s Line=%d",cond, file, line);
	werrlog(104,buf,0,0,0,0,0,0,0);
	wexit(0);
}

/*
**	History:
**	$Log: wassert.c,v $
**	Revision 1.2  1996/07/10 23:55:08  gsl
**	change to use include assert.h
**	
**	Revision 1.1  1996-01-02 08:23:38-08  gsl
**	Initial revision
**
**
**
*/
