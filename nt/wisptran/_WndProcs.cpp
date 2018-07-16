/*
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
*/

#include "#Defines.h"
#include "#Classes.h"
#include "#Prototypes.h"
#include "#Externs.h"
#include "resource.h"

//////////////////////////////////////////////////////////////////////////
//	ShellWndProc - Window procedure for the main application window
LRESULT CALLBACK ShellWndProc ( HWND hShellWnd, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;
	SECURITY_ATTRIBUTES sa;

	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	char ch_FileName[_MAX_PATH];

	switch ( msg ) {
		case WM_SYSCOLORCHANGE:
			//Ctl3dColorChange ( );
			break;
		case WM_CLOSE:
			cApp.CloseApp ( 1001, 0 );												//	Call cApp.CloseApp
			return 0;
			break;
		case WM_SIZE:
		case WM_SIZING:
			cWnd.SizeAppWindow ( );
			return ( DefWindowProc ( hShellWnd, msg, wp, lp ));
			break;
		case WM_NOTIFY:
			NMHDR *nms;
			nms = (NMHDR *)lp;
			switch ( nms->code ) {
				case TCN_SELCHANGE:
					cWnd.OptionWnd.TabClicked ( );
					break;
			}
			break;
		case WM_COMMAND:
			switch ( wpHi ) {														//	Get the high order byte of wp (is the notification message sent from a control to the parent window
				case BN_CLICKED:													//	If Execute button was clicked
					cWnd.OptionWnd.BClicked ( lp );
				break;
			}
			//	Process menu commands
			switch ( wpLo ) {														//	Loword of WPARAM conatins the control ID that sent the message
				case mnu_Exit:														//	Menu - File|Exit
					cApp.CloseApp ( 1002, 2 );										//	Call CloseApp
					break;
				case mnu_ClearOutput:											//	Menu - File|Clear Output
					if ( SendMessage ( cWnd.hWnd.hTabWnd, 
							TCM_GETCURSEL, 0, 0 ) == 0 ) {
						SendMessage ( cWnd.hWnd.hWOutput,
										WM_SETTEXT, 0, (LPARAM) "" );			//	Send message to the output edit control to clear it's buffer
					}
					else {
						SendMessage ( cWnd.hWnd.hCOutput,
										WM_SETTEXT, 0, (LPARAM) "" );			//	Send message to the output edit control to clear it's buffer
					}
					EnableMenuItem ( cWnd.hAppMenu, 
						mnu_Print, MF_BYCOMMAND | MF_GRAYED );
					EnableMenuItem ( cWnd.hAppMenu, 
						mnu_ClearOutput, MF_BYCOMMAND | MF_GRAYED );
					break;
				case mnu_Set_Save_Dir:
					cWnd.OptionWnd.SetWISPOptions ( FALSE );
					cApp.FSettings.SetMaster ( );
					break;
				case mnu_Set_Save_File:
					GetDlgItemText ( cWnd.hWnd.hWISPOpWnd,
						GetDlgCtrlID ( cWnd.OptionWnd.WCtls._TargetFile ),
						ch_FileName, _MAX_PATH );
					cWnd.OptionWnd.SetWISPOptions ( FALSE );
					cApp.FSettings.SetSingle ( ch_FileName );
					cApp.FSettings.ChkOptFileExist ( ch_FileName );
					break;
				case mnu_Set_Ret_Dir:
					cApp.FSettings.GetMaster ( );
					break;
				case mnu_Set_Ret_File:
					GetDlgItemText ( cWnd.hWnd.hWISPOpWnd,
						GetDlgCtrlID ( cWnd.OptionWnd.
						WCtls._TargetFile ), ch_FileName, _MAX_PATH );
					cApp.FSettings.GetSingle ( ch_FileName );
					break;
				case mnu_Print:
					cWnd.PrintOutputWnd ( );
					break;
				case mnu_Translate:
					HANDLE hBldCmdLine;
					char ch_ButtonText[26];
					DWORD thrdID;
					//	Set members of SECURITY_ATTRIBUTES struct for new thread
					sa.nLength = sizeof ( SECURITY_ATTRIBUTES );
					sa.lpSecurityDescriptor = NULL;
					sa.bInheritHandle = FALSE;
					GetWindowText ( cWnd.hWnd.hExecuteBtn,
						ch_ButtonText, 25 );
					cWnd.OptionWnd.SetWISPOptions ( TRUE );
					//	Create seperate thread to run the translation and compilation in
					WaitForSingleObject ( 
						cApp.Thread.hPreEntrySmphr, INFINITE );
					cApp.Thread.Param = 1;
					hBldCmdLine = CreateThread ( &sa, 0, 
						( LPTHREAD_START_ROUTINE ) ThreadManager, 
						NULL, 0, &thrdID );
					break;
			}
		case WM_TIMER:

			break;
		default:
			return ( DefWindowProc ( hShellWnd, msg, wp, lp ));
			break;
	}
	return ( DefWindowProc ( hShellWnd, msg, wp, lp ));
}
//////////////////////////////////////////////////////////////////////////
//	WISPShtDlgProc
BOOL CALLBACK WISPShtDlgProc ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;

	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	switch ( msg ) {
		case WM_COMMAND:
			wpHi = HIWORD (wp);
			wpLo = LOWORD (wp);
			switch ( wpHi ) {
				case EN_UPDATE:
					if ( (HWND) lp == cWnd.hWnd.hWCmdLine ) break;
					cWnd.OptionWnd.SetWISPOptions ( FALSE );
					cApp.CmdLine.Build ( FALSE, TC_Translate );
					break;
				case BN_CLICKED:
					cWnd.OptionWnd.WBClicked ( lp );
					break;
				case CBN_SELCHANGE:
					cWnd.OptionWnd.SetWISPOptions ( FALSE );
					cApp.CmdLine.Build ( FALSE, TC_Translate );
					break;
				case EN_KILLFOCUS:
					cApp.FSettings.SetMaster ( );
					break;
			}
			break;
		case WM_CLOSE:
			cApp.FSettings.SetMaster ( );
			break;
		default:
			return 0;
			break;
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//	COBOLShtDlgProc
BOOL CALLBACK COBOLShtDlgProc ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;
	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	switch ( msg ) {
		case WM_COMMAND:
			wpHi = HIWORD (wp);
			wpLo = LOWORD (wp);
			switch ( wpHi ) {
				case EN_UPDATE:
					if ( (HWND) lp == cWnd.hWnd.hCCmdLine ) break;
					cWnd.OptionWnd.SetCOBOLOptions ( FALSE );
					cApp.CmdLine.Build ( FALSE, TC_Compile );
					break;
				case BN_CLICKED:
					cWnd.OptionWnd.CBClicked ( lp );
					break;
			}
		default:
			return 0;
			break;
	}
	return 0;
}

/*
**	History:
**	$Log: _WndProcs.cpp,v $
**	Revision 1.7  2007/08/02 13:22:51  gsl
**	Remove CTL3Dxxxx stuff, obsolete.
**	
**	Revision 1.6  2003/06/18 16:43:07  gsl
**	Add CVS header and history
**	
**
*/
