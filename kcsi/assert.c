static char copyright[]="Copyright (c) 1997 NeoMedia Technologies, Inc., All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		assert.c
**
**	Project:	WISP/KCSI/COMMON
**
**	RCS:		$Source:$
**
**	Purpose:	Define ASSERT()
**
**	Routines:	
**	kcsi_assert()	Issue the assertion error and exit.
*/

/*
**	Includes
*/
#include <stdlib.h>

#include "assert.h"
#include "kcsit.h"

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
**	ROUTINE:	kcsi_assert()
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
void kcsi_assert(char *cond, char *file, int line)
{
	kcsitrace(4,"KCSI","ASSERT","ASSERT(%s) failed File=%s Line=%d",cond, file, line);
	kcsi_exit(1);
}

/*
**	History:
**	$Log: assert.c,v $
**	Revision 1.1.2.1  2002/11/12 15:56:18  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.2  2002/10/17 21:22:40  gsl
**	cleanup
**	
**	Revision 1.1  1997/08/01 15:54:24  scass
**	Initial revision
**	
**
**
*/
