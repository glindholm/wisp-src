static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		wmalloc.c
**
**	Purpose:	To ...
**
**	Routines:	
**
**
**	History:
**	mm/dd/yy	Written by ...
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
**	Revision 1.7  1998/03/20 22:39:10  gsl
**	Add wrealloc() and make const args const
**	
**	Revision 1.6  1996-08-30 21:56:12-04  gsl
**	drcs update
**
**
**
*/
