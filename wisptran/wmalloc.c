/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/


/*
**	File:		wmalloc.c
**
**	Purpose:	To ...
**
**	Routines:	
**
*/

#include <stdlib.h>
#include <string.h>

#include "wmalloc.h"
#include "output.h"
#include "proto.h"

void *wmalloc(int size)
{
	char	*ptr;

	ptr = malloc(size);

	if (!ptr)
	{
		write_log("WISP",'F',"MALLOC","Failed to malloc %d bytes",size);
		exit_with_err();
	}
	return( (void *)ptr );
}

void wfree(void *ptr)
{
	free(ptr);
}

void *wdupstr(const char *str)
{
	char	*new;

	if (str)
	{
		new = wmalloc(strlen(str)+1);
		strcpy(new,str);
	}
	else
	{
		new = NULL;
	}
	return( (void *)new );
}

void *wrealloc(void *ptr, int size)
{
	ptr = realloc(ptr, size);

	if (!ptr)
	{
		write_log("WISP",'F',"REALLOC","Failed to realloc %d bytes",size);
		exit_with_err();
	}
	return( (void *)ptr );
}

/*
**	History:
**	$Log: wmalloc.c,v $
**	Revision 1.9  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.8  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.7  1998/03/20 22:39:10  gsl
**	Add wrealloc() and make const args const
**	
**	Revision 1.6  1996-08-30 21:56:12-04  gsl
**	drcs update
**
**
**
*/
