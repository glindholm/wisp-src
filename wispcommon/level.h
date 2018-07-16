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
**	File:		level.h
**
**	Purpose:	Header for level.c
**
**
**	History:
**	08/19/94	Written by GSL
**
*/

#ifndef LEVEL_H
#define LEVEL_H

extern int linklevel(void);
extern int newlevel(void);
extern int oldlevel(void);
extern int zerolevel(void);
extern int setlevel(int level);

#endif /* LEVEL_H */
/*
**	History:
**	$Log: level.h,v $
**	Revision 1.5  1996-07-23 14:17:48-04  gsl
**	drcs update
**
**
**
*/
