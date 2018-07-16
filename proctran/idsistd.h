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

#endif /* IDSISTD_H */
