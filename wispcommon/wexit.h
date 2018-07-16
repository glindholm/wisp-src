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
**	File:		wexit.h
**
**	Purpose:	Header for wexit.c
**
**
**	History:
**	08/19/94	Written by GSL
**
*/

#ifndef WEXIT_H
#define WEXIT_H

#include "intdef.h"

#define wexit	WL_wexit

void WL_wexit(int4 num);


#endif /* WEXIT_H */
/*
**	History:
**	$Log: wexit.h,v $
**	Revision 1.6.2.1  2002/11/14 15:23:38  gsl
**	Change wexit() to WL_wexit()
**	
**	Revision 1.6  1996/07/23 18:17:56  gsl
**	drcs update
**	
**
**
*/
