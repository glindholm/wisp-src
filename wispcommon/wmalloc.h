/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

/*
**	File:		wmalloc.h
**
**	Project:	WISP
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
**	Revision 1.7  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.6  2002/07/10 21:06:36  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.5  2002/07/02 21:15:39  gsl
**	Rename wstrdup
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
