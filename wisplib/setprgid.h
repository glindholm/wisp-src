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
**	File:		setprgid.h
**
*/

#ifndef setprgid_H
#define setprgid_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
void WL_setprogid(const char *wisp_application_name);
const char* WL_getprogid(void);

#endif /* setprgid_H */

/*
**	History:
**	$Log: setprgid.h,v $
**	Revision 1.4  2003/01/31 19:18:00  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.3  2002/07/12 19:10:16  gsl
**	Global unique WL_ changes
**	
**	Revision 1.2  2002/07/10 21:05:24  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.1  1997/10/21 14:17:07  gsl
**	Initial revision
**	
**
**
*/
