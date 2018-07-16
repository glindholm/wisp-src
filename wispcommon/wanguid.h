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
**	File:		wanguid.h
**
**	Purpose:	Function declarations for wanguid.c
**
**
**	History:
**	07/19/93	Written by GSL
**
*/

#ifndef WANGUID_H
#define WANGUID_H

const char *wanguid3(void);
const char *numuid3(void);
const char *longuid(void);

#endif /* WANGUID_H */
/*
**	History:
**	$Log: wanguid.h,v $
**	Revision 1.8  2001-11-27 16:40:23-05  gsl
**	Remove reset uid
**
**	Revision 1.7  1996-10-25 17:06:24-04  gsl
**	Fix wanguid3(), numuid3(),and longuid() to return a const ptr.
**
**	Revision 1.6  1996-07-23 11:17:54-07  gsl
**	drcs update
**
**
**
*/
