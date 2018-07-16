/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		wmalloc.h
**
**	Purpose:	To ...
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#ifndef wmalloc_H
#define wmalloc_H

void *wmalloc(int size);
void wfree(void *ptr);
void *wdupstr(const char *str);
void *wrealloc(void *ptr, int size);

#endif /* wmalloc_H */
/*
**	History:
**	$Log: wmalloc.h,v $
**	Revision 1.7  1998-03-20 17:39:42-05  gsl
**	update
**
**	Revision 1.6  1996-08-30 21:56:13-04  gsl
**	drcs update
**
**
**
*/
