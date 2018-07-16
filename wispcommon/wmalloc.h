/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		wmalloc.h
**
**	Project:	WISP
**
**	RCS:		$Source:$
**
**	Purpose:	WISP malloc
**
*/

#ifndef wmalloc_H
#define wmalloc_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/
#define wmalloc wisp_malloc
#define wcalloc wisp_calloc
#define wstrdup wisp_strdup

/*
**	Function Prototypes
*/
extern void *wisp_malloc(size_t size);
extern void *wisp_calloc(size_t nelem, size_t elsize);
extern char *wisp_strdup(const char *string);

#endif /* wmalloc_H */

/*
**	History:
**	$Log: wmalloc.h,v $
**	Revision 1.4.2.1  2002/10/03 13:55:11  gsl
**	Change wstrdup() -> wisp_strdup()
**	
**	Revision 1.4  1996/11/06 17:31:46  jockc
**	Move from wisp/lib to wisp/common
**	
**	Revision 1.3  1996-07-17 14:57:18-07  gsl
**	Add wmalloc() wcalloc() and wstrdup() prototrypes
**	and removed the define malloc() macro
**
**	Revision 1.2  1996-01-03 03:39:34-08  gsl
**	finished
**
 * Revision 1.1  1996/01/03  10:51:55  gsl
 * Initial revision
 *
**
*/
