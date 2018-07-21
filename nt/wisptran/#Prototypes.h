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

#if !defined  PROTOTYPES_H
#define PROTOTYPES_H

//////////////////////////////////////////////////////////////////////////
//		This file contains the function prototypes for the application

LRESULT CALLBACK ShellWndProc ( HWND hShellWnd, UINT msg, WPARAM wp, LPARAM lp );
LRESULT CALLBACK ClockWndProc ( HWND hClockWnd, UINT msg, WPARAM wp, LPARAM lp );
BOOL CALLBACK WISPShtDlgProc ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp );
BOOL CALLBACK COBOLShtDlgProc ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp );

DWORD ThreadManager ( );

#endif

/*
**	History:
**	$Log: #Prototypes.h,v $
**	Revision 1.3  2003/06/18 16:43:07  gsl
**	Add CVS header and history
**	
**
*/
