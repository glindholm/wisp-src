/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
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
char *GetWin32Error(char *szText);
int win32_message_box(char *instr);

#endif /* WIN32ERR_H */

/*
**	History:
**	$Log: win32err.h,v $
**	Revision 1.2  1997-07-14 08:31:55-04  gsl
**	Add win32_message_box()
**
**	Revision 1.1  1996-12-06 18:35:55-05  jockc
**	Initial revision
**
**
**
*/
