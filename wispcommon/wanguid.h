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

const char *WL_wanguid3(void);
const char *WL_numuid3(void);
const char *WL_longuid(void);

#endif /* WANGUID_H */
/*
**	History:
**	$Log: wanguid.h,v $
**	Revision 1.11  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.10  2002/07/10 21:06:34  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.9  2002/07/09 04:14:03  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.8  2001/11/27 21:40:23  gsl
**	Remove reset uid
**	
**	Revision 1.7  1996-10-25 17:06:24-04  gsl
**	Fix wanguid3(), WL_numuid3(),and longuid() to return a const ptr.
**
**	Revision 1.6  1996-07-23 11:17:54-07  gsl
**	drcs update
**
**
**
*/
