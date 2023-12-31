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
**	File:		linkvect.h
**
**	Purpose:	To handle vectoring call "LINK"s into call sub's for internal routines.
**
**	History:
**	04/27/94	Written by GSL
**
*/

#ifndef LINKVECT_H
#define LINKVECT_H

int WL_islinkvector();
int WL_linkvector();

#endif /* LINKVECT_H */
/*
**	History:
**	$Log: linkvect.h,v $
**	Revision 1.7  2003/01/31 19:18:00  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.6  2002/07/12 19:10:13  gsl
**	Global unique WL_ changes
**	
**	Revision 1.5  1996/08/19 22:32:27  gsl
**	drcs update
**	
**
**
*/
