/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/

	/************************************************************************/
	/*									*/
	/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
	/*	 An unpublished work of International Digital Scientific Inc.	*/
	/*			    All rights reserved.			*/
	/*									*/
	/************************************************************************/

/*
**	File:		idsistd.h
**
**	Purpose:	This header can and should be included in all IDSI C files.
**			It includes the INT typedefs plus standard headers.
**
**			Only add things to this that a truely GLOBAL to absolutely
**			everything.
**
**
**	History:
**	07/16/93	Written by JEC.
**	07/17/93	Added comments and hfile.x. GSL
**	07/20/93	Added the size_t typedef. GSL
**
*/

#ifndef IDSISTD_H
#define IDSISTD_H

#include "intdef.h"

#ifndef NOSTDLIB
#include <stdlib.h>
#endif

#ifdef NOSIZE_T
typedef unsigned int size_t;
#endif

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE !FALSE
#endif

#ifdef WIN32
#include "win32std.h"
#endif

#endif /* IDSISTD_H */
/*
**	History:
**	$Log: idsistd.h,v $
**	Revision 1.7  1996/09/10 15:43:49  gsl
**	Add include of wisp32std.h in ifdef WIN32 code
**	
**	Revision 1.6  1996-07-23 11:17:47-07  gsl
**	drcs update
**
**
**
*/
