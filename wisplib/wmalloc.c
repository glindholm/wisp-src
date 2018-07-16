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
**	File:		wmalloc.c
**
**	Project:	WISP
**
**	RCS:		$Source:$
**
**	Purpose:	WISP version of malloc()
**
**	Routines:	
**	wisp_malloc()
**	wmalloc()
**	wcalloc()
**	wstrdup()
*/

/*
**	Includes
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "werrlog.h"
#include "wexit.h"

#include "wmalloc.h"

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
**	ROUTINE:	wisp_malloc()
**
**	FUNCTION:	WISP malloc plus error checking
**
**	DESCRIPTION:	This is a wrapper around malloc().
**			It issues the malloc() the checks if an error.
**			If the malloc() failed it report an error and exits.
**
**	ARGUMENTS:	
**	size		The size to malloc()
**
**	GLOBALS:	None
**
**	RETURN:		Pointer to malloced data.
**
*/
void *wisp_malloc(size_t size)
{
	void	*ptr;
	
	ptr = malloc(size);
	if (ptr) return ptr;
	
	WL_werrlog_error(WERRCODE(104),"MALLOC", "FAILED", 
		"malloc(%d) failed.", size);
	wexit(1);
	
	return NULL;
}

void *wisp_calloc(size_t nelem, size_t elsize)
{
	void	*ptr;
	
	ptr = calloc(nelem, elsize);
	if (ptr) return ptr;
	
	WL_werrlog_error(WERRCODE(104),"CALLOC", "FAILED", 
		"calloc(%d , %d) failed.", nelem, elsize);
	wexit(1);
	
	return NULL;
}

char *wisp_strdup(const char *string)
{
	if (NULL == string)
	{
		return NULL;
	}
	
	return strcpy(wisp_malloc(strlen(string)+1), string);
}
	
/*
**	History:
**	$Log: wmalloc.c,v $
**	Revision 1.8  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.7  2002/12/09 19:15:37  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.6  2002/07/02 21:15:34  gsl
**	Rename wstrdup
**	
**	Revision 1.5  1998/10/21 12:42:42  gsl
**	In wstrdup() if a NULL is passed then a NULL will be returned.
**	
**	Revision 1.4  1996-07-17 17:55:25-04  gsl
**	Changed wisp_malloc() to wmalloc() and added wcalloc() and wdupstr()
**	These routines ensure that memory is allocated or they exit with
**	and a werrlog() message
**
**	Revision 1.3  1996-07-11 16:23:26-07  gsl
**	fix missing includes for NT
**
**	Revision 1.2  1996-01-03 03:39:07-08  gsl
**	Finished
**
 * Revision 1.1  1996/01/03  11:06:02  gsl
 * Initial revision
 *
**
**
*/
