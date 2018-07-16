static char copyright[]="Copyright (c) 1996-1998 NeoMedia Technologies, All rights reserved.";
static char rcsid[]="$Id:$";
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
**	ROUTINE:	wmalloc()
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
void *wmalloc(size_t size)
{
	void	*ptr;
	
	ptr = malloc(size);
	if (ptr) return ptr;
	
	werrlog(104,"%%WMALLOC-F-FAILED malloc failed", 0, 0, 0, 0, 0, 0, 0);
	wexit(1);
	
	return NULL;
}
void *wisp_malloc(size_t size)
{
	return wmalloc(size);
}

void *wcalloc(size_t nelem, size_t elsize)
{
	void	*ptr;
	
	ptr = calloc(nelem, elsize);
	if (ptr) return ptr;
	
	werrlog(104,"%%WCALLOC-F-FAILED calloc failed", 0, 0, 0, 0, 0, 0, 0);
	wexit(1);
	
	return NULL;
}

char *wstrdup(const char *string)
{
	if (NULL == string)
	{
		return NULL;
	}
	
	return strcpy(wmalloc(strlen(string)+1), string);
}
	
/*
**	History:
**	$Log: wmalloc.c,v $
**	Revision 1.5  1998-10-21 08:42:42-04  gsl
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
