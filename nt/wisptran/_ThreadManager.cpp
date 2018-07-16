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


//	Includes
#include "#Defines.h"
#include "#Classes.h"
#include "#Prototypes.h"
#include "#Externs.h"
#include "resource.h"

//************************************************************************
//	_ThreadManager - Provides entry points for new threads

DWORD ThreadManager ( )
{
	ReleaseSemaphore ( cApp.Thread.hPreEntrySmphr, 1, NULL );
	//	If translate was selected
	if ( cApp.Thread.Param == 1 ) {
		//	disable controls that souldn't be accessable while translating
		EnableWindow ( cWnd.OptionWnd.WCtls._TargetBrs, FALSE );

		SetWindowText ( cWnd.hWnd.hExecuteBtn, CancelBtnTxt );
		GlbFlags.isTranslating = TRUE;
		cApp.CmdLine.Build ( TRUE, TC_Translate );
		GlbFlags.isTranslating = FALSE;
		if ( SendMessage ( cWnd.hWnd.hTabWnd, TCM_GETCURSEL, 0, 0 ) == 0 )
			SetWindowText ( cWnd.hWnd.hExecuteBtn, TranslateBtnTxt );
		else SetWindowText ( cWnd.hWnd.hExecuteBtn, CompileBtnTxt );
		//	Enable controls that were disabled above
		EnableWindow ( cWnd.OptionWnd.WCtls._TargetBrs, TRUE );
		MessageBeep ( MB_ICONASTERISK );

		ExitThread ( 1 );
	}
	//	If Compile was selected
	if ( cApp.Thread.Param == 2 ) {
		SendMessage ( cWnd.hWnd.hCOutput, 
			WM_SETTEXT, 0,  ( LPARAM ) "" );
		SetWindowText ( cWnd.hWnd.hExecuteBtn, CancelBtnTxt );
		GlbFlags.isCompiling = TRUE;
		cApp.CmdLine.Build ( TRUE, TC_Compile );
		GlbFlags.isCompiling = FALSE;
		if ( SendMessage ( cWnd.hWnd.hTabWnd, TCM_GETCURSEL, 0, 0 ) == 0 )
			SetWindowText ( cWnd.hWnd.hExecuteBtn, TranslateBtnTxt );
		else SetWindowText ( cWnd.hWnd.hExecuteBtn, CompileBtnTxt );
		MessageBeep ( MB_ICONASTERISK );
	}
	//	If Translate & Compile was selected
	if ( cApp.Thread.Param == 3 ) {
		SendMessage ( cWnd.hWnd.hWOutput, 
			WM_SETTEXT, 0,  ( LPARAM ) "" );
		SendMessage ( cWnd.hWnd.hCOutput, 
			WM_SETTEXT, 0,  ( LPARAM ) "" );
		SetWindowText ( cWnd.hWnd.hExecuteBtn, CancelBtnTxt );
		GlbFlags.isTranslating = TRUE;
		GlbFlags.isCompiling = TRUE;
		cApp.CmdLine.Build ( TRUE, TC_TranComp );
		GlbFlags.isTranslating = FALSE;
		GlbFlags.isCompiling = FALSE;
		if ( SendMessage ( cWnd.hWnd.hTabWnd, TCM_GETCURSEL, 0, 0 ) == 0 )
			SetWindowText ( cWnd.hWnd.hExecuteBtn, TranslateBtnTxt );
		else SetWindowText ( cWnd.hWnd.hExecuteBtn, CompileBtnTxt );
		MessageBeep ( MB_ICONASTERISK );
	}
	ExitThread ( 0 );
	return 0;
}

//		Param Values:
//			1	- Translate button was clicked; call AppManager::CmdLineBuild
//			2	- Compile button was clicked; call AppManager::CmdLineBuild
//			3	- Translate and Compile button was clicked; call AppManager::CmdLineBuild
//

/*
**	History:
**	$Log: _ThreadManager.cpp,v $
**	Revision 1.3  2003/06/18 16:43:07  gsl
**	Add CVS header and history
**	
**
*/

