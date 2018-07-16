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
#include "rControls.h"
#include "rResources.h"
#include "rPrototypes.h"
//////////////////////////////////////////////////////////////////////////
//
//	clsWindowManager::_Dialogs::_SelectDirDlg::Create
//		Creates the Select Directory dialog based on the
//		arguments passed by the application
//
rControls::_Dialogs::_SelectDirDlg::Create ( HWND hParent, char *StartDir,
						  BOOL MultiSelect, int Type, HWND Caller, char *Title )
{
	//	Type refers to whether the user needs the directory listing or the server listing
	//	0 = directories, 1 = servers
	CustCtrl.Dialogs.SelectDirDlg.Type = Type;
	CustCtrl.Dialogs.SelectDirDlg.MultiSelect = MultiSelect;
	CustCtrl.Dialogs.SelectDirDlg.Caller = Caller;

	strcpy ( CustCtrl.Dialogs.SelectDirDlg.sTitle, Title );
	if ( (Type == 0) && (StartDir != NULL) )
		memcpy ( CustCtrl.Ctrl.tvList.stv_StartDir, StartDir, _MAX_PATH );
	if ( MultiSelect == TRUE )
		DialogBox ( CustCtrl.hInst, MAKEINTRESOURCE (DlgSelectDir),
			hParent, (DLGPROC) SelectDirDlgProc );
	else 
		DialogBox ( CustCtrl.hInst, MAKEINTRESOURCE (dlgSelectSingleDir),
			hParent, (DLGPROC) SelectSingleDirDlgProc );
	
	return 0;
}	
//////////////////////////////////////////////////////////////////////////
//
//	clsWindowManager::_Dialogs::_SelectDirDlg::Initialize
//		Initializes the dialog
//
rControls::_Dialogs::_SelectDirDlg::Initialize ( HWND hDlg, HINSTANCE hInst )
{
//	HBITMAP hAddBmp, hRemoveBmp, hUpBmp, hDownBmp;
	HWND hTVAvailDirs/*,
		hAddBtn, hRemoveBtn, hUpBtn, hDownBtn*/;

	SetWindowText ( hDlg, sTitle );

	//	Initialize the tree view control
	hTVAvailDirs = GetDlgItem ( hDlg, TV_AvailDirs );		//	Get the handle of the tree view control
	CustCtrl.Ctrl.tvList.Initialize ( hDlg, hTVAvailDirs );
	//	If is multiselect dialog
	if ( MultiSelect == TRUE ) {
		//	Place bitmaps on buttons
/*		hAddBmp = LoadBitmap ( CustCtrl.hInst, MAKEINTRESOURCE(bmpRight) );
		hAddBtn = GetDlgItem ( hDlg, ctlAddBtn );
		SendMessage ( hAddBtn, BM_SETIMAGE, IMAGE_BITMAP, (LPARAM) hAddBmp );

		hRemoveBmp = LoadBitmap ( CustCtrl.hInst, MAKEINTRESOURCE(bmpLeft) );
		hRemoveBtn = GetDlgItem ( hDlg, ctlRemoveBtn );
		SendMessage ( hRemoveBtn, BM_SETIMAGE, IMAGE_BITMAP, (LPARAM) hRemoveBmp );

		hUpBmp = LoadBitmap ( CustCtrl.hInst, MAKEINTRESOURCE(bmpUp) );
		hUpBtn = GetDlgItem ( hDlg, ctlMUpBtn );
		SendMessage ( hUpBtn, BM_SETIMAGE, IMAGE_BITMAP, (LPARAM) hUpBmp );

		hDownBmp = LoadBitmap ( CustCtrl.hInst, MAKEINTRESOURCE(bmpDown) );
		hDownBtn = GetDlgItem ( hDlg, ctlMDownBtn );
		SendMessage ( hDownBtn, BM_SETIMAGE, IMAGE_BITMAP, (LPARAM) hDownBmp );*/
		//	Initialize the list box
		char CurList[2048], Single[_MAX_PATH];
		HWND hList;
		ZeroMemory ( CurList, sizeof ( CurList ));
		ZeroMemory ( Single, sizeof ( Single ));
		GetWindowText ( Caller, CurList, _MAX_PATH );
		hList = GetDlgItem ( hDlg, LB_SelectedDirs );
		int StartStr = 0, EndStr = 0;
		while ( CurList[EndStr] != '\0' ) {
			while ( CurList[EndStr] != ';' ) {
				if ( CurList[EndStr] == '\0' ) break;
				EndStr++;
			}
			memcpy ( Single, &CurList[StartStr], EndStr - StartStr );
			SendMessage ( hList, LB_ADDSTRING, 0, (LPARAM) Single );
			StartStr = EndStr+1;
			EndStr++;
			ZeroMemory ( Single, sizeof ( Single ));
		}
	}
	else {
		HWND /*hOKBtn, hCancelBtn, hDirTree, */hRetPath;
//		hOKBtn = GetDlgItem ( hDlg, IDOK );
//		hCancelBtn = GetDlgItem ( hDlg, IDCANCEL );
//		hDirTree = GetDlgItem ( hDlg, TV_AvailDirs );
		hRetPath = GetDlgItem ( hDlg, ctlRetPath );
		//	Hide controls that aren't used
//		ShowWindow ( GetDlgItem ( hDlg, ctlAddBtn ), SW_HIDE );
//		ShowWindow ( GetDlgItem ( hDlg, ctlRemoveBtn ), SW_HIDE );
//		ShowWindow ( GetDlgItem ( hDlg, ctlMUpBtn ), SW_HIDE );
//		ShowWindow ( GetDlgItem ( hDlg, ctlMDownBtn ), SW_HIDE );
//		ShowWindow ( GetDlgItem ( hDlg, LB_SelectedDirs ), SW_HIDE );
//		ShowWindow ( GetDlgItem ( hDlg, StaticText1 ), SW_HIDE );

//		SetWindowPos ( hDlg, NULL, NULL, NULL, 368, 295,
//			SWP_NOMOVE | SWP_NOZORDER );
//		SetWindowPos ( hDirTree, NULL, NULL, NULL, 341, 180,
//			SWP_NOMOVE | SWP_NOZORDER );
//		SetWindowPos ( hOKBtn, NULL, 107, 242, 0, 0,
//			SWP_NOZORDER | SWP_NOSIZE );
//		SetWindowPos ( hCancelBtn, NULL, 186, 242, 0, 0, // 117, 217, 0, 0,
//			SWP_NOZORDER | SWP_NOSIZE );
		SetWindowPos ( hRetPath, NULL, NULL, NULL, 341, 13,
			SWP_NOMOVE | SWP_NOZORDER | SWP_SHOWWINDOW );
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	clsWindowManager::_Dialogs::_SelectDirDlg::AddClicked
//		Adds a directory to the selected dirs list
//
rControls::_Dialogs::_SelectDirDlg::AddClicked ( HWND hDlg )
{
	HWND hTVWnd,
		hLBWnd;
	TV_ITEM tvCurSel;
	int cnt = 0;
	char ch_FullPath[MaxPath+MaxFileLen];

	hTVWnd = GetDlgItem ( hDlg, TV_AvailDirs );
	hLBWnd = GetDlgItem ( hDlg, LB_SelectedDirs );

	//	Get the currently selected TV item
	if ( (tvCurSel.hItem = TreeView_GetSelection ( hTVWnd )) == NULL ) 
		return 0;

	char ch_PathTok[50][256];
	TV_ITEM tviPrnt;
	tviPrnt = tvCurSel;

	tviPrnt.mask = TVIF_TEXT;
	tviPrnt.cchTextMax = MaxFileLen;
	tviPrnt.pszText = (char *) malloc ( MaxFileLen );
	SendMessage ( hTVWnd, TVM_GETITEM, 0, (LPARAM) &tviPrnt );

	ZeroMemory ( ch_PathTok, sizeof ( ch_PathTok ));
	strcpy ( ch_PathTok[0], tviPrnt.pszText );
	cnt = 1;
	//	Get the full path to the selected dir
	while ( TRUE ) {
		if (( tviPrnt.hItem = 
			TreeView_GetParent (hTVWnd, tviPrnt.hItem )) == NULL ) break;
		SendMessage ( hTVWnd, TVM_GETITEM, 0, (LPARAM) &tviPrnt );
		strcpy ( ch_PathTok[cnt], tviPrnt.pszText );
		cnt++;
	}
	cnt = 0;
	while ( strlen ( ch_PathTok[cnt] ) > 0 ) cnt++;
	BOOL isLocal = FALSE;
	if ( strcmp ( ch_PathTok[cnt-1], "My Computer" ) == 0 ) isLocal = TRUE;
	ZeroMemory ( ch_FullPath, sizeof ( ch_FullPath ));
	if ( isLocal == TRUE ) {
		cnt = cnt - 2;
		ZeroMemory ( ch_FullPath, sizeof ( ch_FullPath ));
		memcpy ( ch_FullPath, &ch_PathTok[cnt][strlen ( ch_PathTok[cnt] )-3], 2 );
		ch_FullPath[2] = '\\';
	}
	else {
		cnt = cnt - 3;
	}
	cnt--;
	while ( cnt >= 0 ) {
		strcat ( ch_FullPath, ch_PathTok[cnt] );
		if ( cnt > 0 ) strcat ( ch_FullPath, "\\" );
		cnt--;
	}
	if ( SendMessage ( hLBWnd, LB_FINDSTRINGEXACT, 0, (LPARAM) ch_FullPath ) == LB_ERR )
		SendMessage ( hLBWnd, LB_ADDSTRING, 0, (LPARAM) ch_FullPath );

	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	clsWindowManager::_Dialogs::_SelectDirDlg::RemoveClicked
//		Removes a dir from the selected dirs list
//
rControls::_Dialogs::_SelectDirDlg::RemoveClicked ( HWND hDlg )
{
	HWND hLBWnd;

	hLBWnd = GetDlgItem ( hDlg, LB_SelectedDirs );

	SendMessage ( hLBWnd, LB_DELETESTRING, 
		SendMessage ( hLBWnd, LB_GETCURSEL, 0, 0 ), 0 );

	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	clsWindowManager::_Dialogs::_SelectDirDlg::MUpClicked
//		Moves the selected dir up one in the list of selected
//		dirs
//
rControls::_Dialogs::_SelectDirDlg::MUpClicked ( HWND hDlg )
{
	HWND hLBWnd;
	int index;
	char ch_Text[MaxPath];

	hLBWnd = GetDlgItem ( hDlg, LB_SelectedDirs );
	index = SendMessage ( hLBWnd, LB_GETCURSEL, 0, 0 );
	if ( index != 0 ) {
		SendMessage ( hLBWnd, LB_GETTEXT, index, (LPARAM) ch_Text );
		SendMessage ( hLBWnd, LB_DELETESTRING, index, 0 );
		SendMessage ( hLBWnd, LB_INSERTSTRING, index-1, (LPARAM) ch_Text );
		SendMessage ( hLBWnd, LB_SETCURSEL, index-1, 0 );
	}

	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	clsWindowManager::_Dialogs::_SelectDirDlg::MDownClicked
//		Moves the selected dir down one in the list of selected
//		dirs
//
rControls::_Dialogs::_SelectDirDlg::MDownClicked ( HWND hDlg )
{
	HWND hLBWnd;
	int index, MaxCnt;
	char ch_Text[MaxPath];

	hLBWnd = GetDlgItem ( hDlg, LB_SelectedDirs );
	index = SendMessage ( hLBWnd, LB_GETCURSEL, 0, 0 );
	MaxCnt = SendMessage ( hLBWnd, LB_GETCOUNT, 0, 0 );
	if ( index != MaxCnt-1 ) {
		SendMessage ( hLBWnd, LB_GETTEXT, index, (LPARAM) ch_Text );
		SendMessage ( hLBWnd, LB_DELETESTRING, index, 0 );
		SendMessage ( hLBWnd, LB_INSERTSTRING, index+1, (LPARAM) ch_Text );
		SendMessage ( hLBWnd, LB_SETCURSEL, index+1, 0 );
	}


	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	clsWindowManager::_Dialogs::_SelectDirDlg::OKClicked
//		Puts the list of selected dirs into the edit control
//		that is specified as the Caller in the Create member func
//
rControls::_Dialogs::_SelectDirDlg::OKClicked ( HWND hDlg )
{
	char *ch_DirList, ch_SingDir[MaxFileLen];

	if ( MultiSelect == TRUE ) {
		HWND hLBWnd;
		int cnt = 0, NumItems = 0;

		hLBWnd = GetDlgItem ( hDlg, LB_SelectedDirs );

		ch_DirList = (char *) malloc ( MaxFileLen );
		ZeroMemory ( ch_DirList, _msize ( ch_DirList ));

		NumItems = SendMessage ( hLBWnd, LB_GETCOUNT, 0, 0 );
		cnt = 0;
		while ( cnt < NumItems ) {
			SendMessage ( hLBWnd, LB_GETTEXT, cnt, (LPARAM) ch_SingDir );
			if ( (strlen ( ch_SingDir ) + strlen ( ch_DirList )) >
				_msize ( ch_DirList ) )
				ch_DirList = (char *) realloc ( ch_DirList, MaxFileLen );
			if ( strlen ( ch_DirList ) != 0 ) strcat ( ch_DirList, ";" );
			strcat ( ch_DirList, ch_SingDir );
			cnt++;
		}
		SetWindowText ( Caller, ch_DirList );
	}
	else {
		int cnt = 0;
		char sRetPath[_MAX_PATH]/*, sTemp[_MAX_PATH]*/;
		ZeroMemory ( sRetPath, _MAX_PATH );
		GetDlgItemText ( hDlg, ctlRetPath, sRetPath, _MAX_PATH );
		if ( Type == TVL_SERVERS ) {
			char sTemp[_MAX_PATH];
			int cnt1 = 0;
			ZeroMemory ( sTemp, sizeof ( sTemp ));
			while ( sRetPath[cnt1] == '\\' ) cnt1++;
			memcpy ( sTemp, &sRetPath[cnt1], strlen ( sRetPath ) - cnt1 );
			strcpy ( sRetPath, sTemp );
		}
		SetWindowText ( Caller, sRetPath );
	}
	EndDialog ( hDlg, 1 );		//	Successful
	return 0;
}

/*
**	History:
**	$Log: rDialogs.cpp,v $
**	Revision 1.9  2003/06/18 16:43:07  gsl
**	Add CVS header and history
**	
**
*/
