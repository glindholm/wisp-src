/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/

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
	WL_werrlog_error(WERRCODE(104),"ASSERT", "FAILED", 
		"ASSERT(%s) Failed: File=%s Line=%d", cond, file, line);
	wexit(0);
}

/*
**	History:
**	$Log: wassert.c,v $
**	Revision 1.4  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.3  2002/12/09 19:15:35  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.2  1996/07/10 23:55:08  gsl
**	change to use include assert.h
**	
**	Revision 1.1  1996-01-02 08:23:38-08  gsl
**	Initial revision
**
**
**
*/
