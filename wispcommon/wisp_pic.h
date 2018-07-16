/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		wisp_pic.h
**
**	Project:	WISP/COMMON
**
**	RCS:		$Source:$
**
**	Purpose:	Wang COBOL picture clauses
**
*/

#ifndef wisp_pic_H
#define wisp_pic_H
/*
**	Includes
*/
#include "intdef.h"

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
int pic_size(const char *the_pic);
int pic_dp(const char* the_pic);
int pic_fac(const char* the_pic);
uint4 pic_edit(const char* the_pic);
uint4 pic_zmask(const char* the_pic);

#endif /* wisp_pic_H */

/*
**	History:
**	$Log: wisp_pic.h,v $
**	Revision 1.2  1997-12-17 15:30:28-05  gsl
**	Fix prototypes to be const
**
**	Revision 1.1  1996-06-28 12:19:12-04  gsl
**	Initial revision
**
**
**
*/
