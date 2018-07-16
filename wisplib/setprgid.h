/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

/*
**	File:		setprgid.h
**
**	Project:	???
**
**	RCS:		$Source:$
**
**	Purpose:	???
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
