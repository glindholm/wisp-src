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
**	File:		reduce.h
**
**	Purpose:	To ...
**
**
**	History:
**	09/20/93	Written by GSL
**
*/

#ifndef REDUCE_H
#define REDUCE_H

#include "node.h"

extern NODE reduce_data_item(NODE start);
extern NODE reduce_parens(NODE start);
extern NODE reduce_one(NODE start);


#endif /* REDUCE_H */
/*
**	History:
**	$Log: reduce.h,v $
**	Revision 1.6  1996-08-30 21:56:09-04  gsl
**	drcs update
**
**
**
*/
