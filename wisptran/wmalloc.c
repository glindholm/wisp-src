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

#include "wmalloc.h"

void *wmalloc(size)
int size;
{
	char	*ptr;
	char	*malloc();

	ptr = malloc(size);

	if (!ptr)
	{
		write_log("WISP",'F',"MALLOC","Failed to malloc %d bytes",size);
		exit_with_err();
	}
	return( (void *)ptr );
}

void wfree(ptr)
char *ptr;
{
	free(ptr);
}

void *wdupstr(str)
char *str;
{
	char	*new;

	if (str)
	{
		new = wmalloc(strlen(str)+1);
		strcpy(new,str);
	}
	else
	{
		new = str;
	}
	return( (void *)new );
}
