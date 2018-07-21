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
**	File:		wisp_pic.h
**
**	Project:	WISP/COMMON
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
int WL_pic_size(const char *the_pic);
int WL_pic_dp(const char* the_pic);
int WL_pic_fac(const char* the_pic);
uint4 WL_pic_edit(const char* the_pic);
uint4 WL_pic_zmask(const char* the_pic);

#endif /* wisp_pic_H */

/*
**	History:
**	$Log: wisp_pic.h,v $
**	Revision 1.4  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.3  2002/07/10 21:06:35  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.2  1997/12/17 20:30:28  gsl
**	Fix prototypes to be const
**	
**	Revision 1.1  1996-06-28 12:19:12-04  gsl
**	Initial revision
**
**
**
*/
