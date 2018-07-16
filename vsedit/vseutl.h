/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		vseutl.h
**
**	Purpose:	To ...
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#ifndef vseutl_H
#define vseutl_H

void trunc(char *str);
void untrunc(char *s, int len);
int isblankstr(char *str, int len);
int exists(char *name);
void vse_untabify(char *str, int size);

#endif /* vseutl_H */
/*
**	History:
**	$Log: vseutl.h,v $
**	Revision 1.5  1996-09-03 18:24:13-04  gsl
**	drcs update
**
**
**
*/
