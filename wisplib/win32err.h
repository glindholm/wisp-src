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
**	File:		win32err.h
**
**	Project:	WISPLIB
**
**	RCS:		$Source:$
**
**	Purpose:	prototypes for win32err routines
**
*/

#ifndef WIN32ERR_H
#define WIN32ERR_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
char *WL_GetWin32Error(char *szText);
int   WL_win32_message_box(const char *instr);

#endif /* WIN32ERR_H */

/*
**	History:
**	$Log: win32err.h,v $
**	Revision 1.5  2003/01/31 19:18:00  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.4  2002/12/06 22:53:12  gsl
**	WL_win32_message_box(const char *instr);
**	
**	Revision 1.3  2002/07/10 21:05:32  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.2  1997/07/14 12:31:55  gsl
**	Add win32_message_box()
**	
**	Revision 1.1  1996-12-06 18:35:55-05  jockc
**	Initial revision
**
**
**
*/
