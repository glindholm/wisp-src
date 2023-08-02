/*
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
*/


/*
**	File:		wmalloc.h
**
**
*/

#ifndef wmalloc_H
#define wmalloc_H

void *wmalloc(int size);
void wfree(void *ptr);
void *wdupstr(const char *str);
void *wrealloc(void *ptr, int size);

#endif /* wmalloc_H */
/*
**	History:
**	$Log: wmalloc.h,v $
**	Revision 1.9  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.8  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.7  1998/03/20 22:39:42  gsl
**	update
**	
**	Revision 1.6  1996-08-30 21:56:13-04  gsl
**	drcs update
**
**
**
*/
