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
**	File:		win32err.c
**
**	Project:	WISPLIB
**
**	Purpose:	format error string for last error
**
**	Routines:	
*/
#ifdef WIN32
/*
**	Includes
*/
#include <windows.h>
#include <stdio.h>
#include "win32err.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

extern HWND vraw_get_console_hWnd(void);

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/
char *WL_GetWin32Error(char *szText)
{
	DWORD dwError, nBytes;
	static char *savebuff = NULL;
	LPTSTR lpMsgBuf = NULL;
 
	dwError = GetLastError();
	nBytes = FormatMessage( 
		FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
		NULL,
		dwError,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
		(LPTSTR) &lpMsgBuf,
		0,
		NULL );
	if (savebuff) free(savebuff);
	savebuff = malloc(nBytes + strlen(szText) + 10);
	if (nBytes <= 0) lpMsgBuf = NULL;
	sprintf(savebuff,"%s: %s",szText, lpMsgBuf ? lpMsgBuf:"(no message)");

	LocalFree( lpMsgBuf );
	return savebuff;
}


/*
**	Routine:	WL_win32_message_box()
**
**	Function:	Display a message in a message box .
**
**	Description:	This is a WIN32 replacement for WL_werr_message_box().
**
**	Arguments:
**	instr		The message string to display.
**
**	Globals:	None
**
**	Return:		
**	1		Cancel was requested
**	0		Countinue was requested
**
**	Warnings:	None
**
*/
int WL_win32_message_box(const char *instr)
{
	int	rc;
	rc = MessageBox(vraw_get_console_hWnd(), instr, "WISP Runtime Error", 
		MB_OKCANCEL | MB_ICONEXCLAMATION | 
		MB_SETFOREGROUND | MB_TOPMOST | MB_DEFBUTTON1 );

	return (IDCANCEL == rc);
}

#endif /* WIN32 */
/*
**	History:
**	$Log: win32err.c,v $
**	Revision 1.7  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.6  2002/12/06 22:53:13  gsl
**	WL_win32_message_box(const char *instr);
**	
**	Revision 1.5  2002/07/10 21:05:32  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.4  1997/07/14 12:31:27  gsl
**	Move WL_win32_message_box() from werrvre.c
**	
**	Revision 1.3  1997-05-16 16:12:38-04  gsl
**	Fix formating of message
**
**	Revision 1.2  1996-12-06 18:35:38-05  jockc
**	added header for prototypes
**
**	Revision 1.1  1996-09-13 12:01:56-07  jockc
**	Initial revision
**
**
**
*/


