/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
**
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
**
** $Author: gsl $
**
**
******************************************************************************
*/

/*
**	File:		assert.c
**
**	Project:	WISP/KCSI/COMMON
**
**
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
**	Revision 1.3  2003/02/04 19:19:09  gsl
**	fix header
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
