//////////////////////////////////////////////////////////////////////////
//
//		This file contains the class member functions for the _OptionWindow
//			class

//	Includes
#include "#Classes.h"
#include "#Externs.h"
#include "resource.h"

//////////////////////////////////////////////////////////////////////////
//
//		_OptionWindow - Controls the window that contains the option window
//

//////////////////////////////////////////////////////////////////////////
//
//	cWindows::_OptionWindow::TabClicked
//		Selects an option sheet when a tab is clicked
//
cWindows::_OptionWindow::TabClicked ( )
{
	int index;
	index = SendMessage ( cWnd.hWnd.hTabWnd, TCM_GETCURSEL, 0, 0 );
	//	If WISP is now the current tab
	if ( index == 0 ) {
		//	Show and hide the windows as required
		ShowWindow ( cWnd.hWnd.hCOBOLOpWnd, SW_HIDE );
		ShowWindow ( cWnd.hWnd.hCOBOLOutWnd, SW_HIDE );
		ShowWindow ( cWnd.hWnd.hWISPOpWnd, SW_SHOW );
		ShowWindow ( cWnd.hWnd.hWISPOutWnd, SW_SHOW );
		//	If the app is not translating
		if ( GlbFlags.isTranslating == FALSE ) 
			SendMessage ( cWnd.hWnd.hExecuteBtn,
				WM_SETTEXT, 0, ( LPARAM ) TranslateBtnTxt );			//	Print Translate on the Execute button
		//	If the app is translating
		if ( GlbFlags.isTranslating == TRUE ) 
			SendMessage ( cWnd.hWnd.hExecuteBtn, 
				WM_SETTEXT, 0,  ( LPARAM ) CancelBtnTxt );			//	Print Translate on the Execute button
		cApp.CmdLine.Build ( FALSE, TC_Translate );
		int isBlank = SendMessage ( 
			cWnd.OptionWnd.WCtls._TargetFile, 
				CB_GETCOUNT, 0, 0 );
		//	If there are no target files selected
		if ( isBlank == 0 ) {
			EnableWindow (
				cWnd.hWnd.hExecuteBtn, FALSE );
			EnableWindow (
				cWnd.hWnd.hExecCompBtn, FALSE );
		}
		//	If there are target files selected
		else { 
			EnableWindow (
				cWnd.hWnd.hExecuteBtn, TRUE );
			EnableWindow (
				cWnd.hWnd.hExecCompBtn, TRUE );
		}

		int text_len = GetWindowTextLength(cWnd.hWnd.hWOutput);
		//	If there is anything in the output window
		if ( text_len > 0 ) {
			EnableMenuItem ( cWnd.hAppMenu, 
				mnu_Print, MF_BYCOMMAND | MF_ENABLED );
			EnableMenuItem ( cWnd.hAppMenu, 
				mnu_ClearOutput, MF_BYCOMMAND | MF_ENABLED );
		}
		//	If there is nothing in the output window
		else {
			EnableMenuItem ( cWnd.hAppMenu, 
				mnu_Print, MF_BYCOMMAND | MF_GRAYED );
			EnableMenuItem ( cWnd.hAppMenu, 
				mnu_ClearOutput, MF_BYCOMMAND | MF_GRAYED );
		}
	}
	else {
		//	Show and hide the windows as necissary
		ShowWindow ( cWnd.hWnd.hWISPOpWnd, SW_HIDE );
		ShowWindow ( cWnd.hWnd.hWISPOutWnd, SW_HIDE );
		ShowWindow ( cWnd.hWnd.hCOBOLOpWnd, SW_SHOW );
		ShowWindow ( cWnd.hWnd.hCOBOLOutWnd, SW_SHOW );
		SendMessage ( cWnd.hWnd.hExecuteBtn, 
			WM_SETTEXT, 0,  ( LPARAM ) CompileBtnTxt );						//	Print Compile on the Execute button
		cWnd.OptionWnd.SetCOBOLOptions ( FALSE );
		cApp.CmdLine.Build ( FALSE, TC_Compile );
		int isBlank = SendMessage ( 
			cWnd.OptionWnd.CCtls._TargetFile, 
			CB_GETCOUNT, 0, 0 );
		//	If there are no target files selected
		if ( isBlank == 0 ) 
			EnableWindow ( 
				cWnd.hWnd.hExecuteBtn, FALSE );
		//	If there are target files selected
		else EnableWindow ( 
				cWnd.hWnd.hExecuteBtn, TRUE );
		EnableWindow ( 
			cWnd.hWnd.hExecCompBtn, FALSE );
		cApp.Env->WLastFocus = GetFocus ( );
		SetFocus ( cApp.Env->CLastFocus );

		int text_len = GetWindowTextLength(cWnd.hWnd.hCOutput);
		//	If there is anything in the output window
		if ( text_len > 0 ) {
			EnableMenuItem ( cWnd.hAppMenu, 
				mnu_Print, MF_BYCOMMAND | MF_ENABLED );
			EnableMenuItem ( cWnd.hAppMenu, 
				mnu_ClearOutput, MF_BYCOMMAND | MF_ENABLED );
		}
		//	If there is nothing in the output window
		else {
			EnableMenuItem ( cWnd.hAppMenu, 
				mnu_Print, MF_BYCOMMAND | MF_GRAYED );
			EnableMenuItem ( cWnd.hAppMenu, 
				mnu_ClearOutput, MF_BYCOMMAND | MF_GRAYED );
		}
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	cWindows::_OptionWindow::BClicked
//		Responds when a translate or compile button is clicked
//
cWindows::_OptionWindow::BClicked ( LPARAM lp ) 
{
	SECURITY_ATTRIBUTES sa;
	DWORD thrdID;
	HANDLE hBldCmdLine;

	sa.nLength = sizeof ( SECURITY_ATTRIBUTES );
	sa.lpSecurityDescriptor = NULL;
	sa.bInheritHandle = FALSE;

	if (  ( HWND ) lp == cWnd.hWnd.hExecuteBtn ) {
		char ch_ButtonText[26];
		//	Set members of SECURITY_ATTRIBUTES struct for new thread
		GetWindowText ( cWnd.hWnd.hExecuteBtn, ch_ButtonText, 25 );
		//	If the Execute Btn says Cancel
		if ( strcmp ( ch_ButtonText, CancelBtnTxt )  == 0 )
		{
			cApp.CmdLine.Break = TRUE;
		}

		//	If the Execute Btn says Translate
		if ( strcmp ( ch_ButtonText, TranslateBtnTxt )  == 0 ) {
			WIN32_FIND_DATA FindData;
			ZeroMemory ( &FindData, sizeof ( FindData ));
			if (( strlen ( cApp.Env->RegInfo.ch_WPath ) == 0 ) ||	//	If WISP.EXE isn't specified
				( FindFirstFile ( cApp.Env->RegInfo.ch_WPath,
				&FindData ) == INVALID_HANDLE_VALUE )) {				//	or if it isn't valid
				MessageBox ( (HWND) lp,
					"The WISP executable is not valid.\n"
					"You must run the WISP Configuration Utility\n"
					"to set this option.", "WISP.EXE not found", MB_OK );
				return 0;
			}
			SendMessage ( cWnd.hWnd.hWOutput,
				WM_SETTEXT, 0, (LPARAM) "" );
			cWnd.OptionWnd.SetWISPOptions ( TRUE );
			//	Create seperate thread to run the translation and compilation in
			WaitForSingleObject ( cApp.Thread.hPreEntrySmphr, INFINITE );
			cApp.Thread.Param = 1;
			hBldCmdLine = CreateThread ( &sa, 0, 
				 ( LPTHREAD_START_ROUTINE ) ThreadManager, 
				NULL, 0, &thrdID );
			CloseHandle(hBldCmdLine);
		}

		//	If the Execute Btn says Compile
		if ( strcmp ( ch_ButtonText, CompileBtnTxt )  == 0 ) {
			char sCOBOLEXE[_MAX_PATH];
			ZeroMemory ( sCOBOLEXE, sizeof ( sCOBOLEXE ));
			GetWindowText ( cWnd.OptionWnd.CCtls._PathToComp, sCOBOLEXE, _MAX_PATH );
			WIN32_FIND_DATA FindData;
			ZeroMemory ( &FindData, sizeof ( FindData ));
			if (( strlen ( sCOBOLEXE ) == 0 ) ||					//	If the COBOL EXE isn't specified
				( FindFirstFile ( sCOBOLEXE, &FindData ) ==
				INVALID_HANDLE_VALUE )) {							//	or if it isn't valid
				MessageBox ( (HWND) lp,
					"The COBOL compiler is not valid.\n"
					"You must select a COBOL compiler before you can\n"
					"compile any source files.", "COBOL Compiler not found", MB_OK );
				return 0;
			}
			SendMessage ( cWnd.hWnd.hCOutput,
				WM_SETTEXT, 0, (LPARAM) "" );
			cWnd.OptionWnd.SetCOBOLOptions ( TRUE );
			WaitForSingleObject ( cApp.Thread.hPreEntrySmphr, INFINITE );
			cApp.Thread.Param = 2;
			hBldCmdLine = CreateThread ( &sa, 0, 
				 ( LPTHREAD_START_ROUTINE ) ThreadManager, 
				NULL, 0, &thrdID );
			CloseHandle(hBldCmdLine);
		}
	}

	//	If translate & compile was clicked
	if (  ( HWND ) lp == cWnd.hWnd.hExecCompBtn ) {
		WIN32_FIND_DATA FindData;
		ZeroMemory ( &FindData, sizeof ( FindData ));
		if (( strlen ( cApp.Env->RegInfo.ch_WPath ) == 0 ) ||	//	If WISP.EXE isn't specified
			( FindFirstFile ( cApp.Env->RegInfo.ch_WPath,
			&FindData ) == INVALID_HANDLE_VALUE )) {				//	or if it isn't valid
			MessageBox ( (HWND) lp,
				"The WISP executable is not valid.\n"
				"You must run the WISP Configuration Utility\n"
				"to set this option.", "WISP.EXE not found", MB_OK );
			return 0;
		}
		char sCOBOLEXE[_MAX_PATH];
		ZeroMemory ( sCOBOLEXE, sizeof ( sCOBOLEXE ));
		ZeroMemory ( &FindData, sizeof ( FindData ));
		GetWindowText ( cWnd.OptionWnd.CCtls._PathToComp,
			sCOBOLEXE, _MAX_PATH );
		if (( strlen ( sCOBOLEXE ) == 0 ) ||					//	If the COBOL EXE isn't specified
			( FindFirstFile ( sCOBOLEXE, &FindData ) ==
			INVALID_HANDLE_VALUE )) {							//	or if it isn't valid
			MessageBox ( (HWND) lp,
				"The COBOL compiler is not valid.\n"
				"You must select a COBOL compiler before you can\n"
				"compile any source files.", "COBOL Compiler not found", MB_OK );
			return 0;
		}
		SendMessage ( cWnd.hWnd.hWOutput, WM_SETTEXT, 0, (LPARAM) "" );
		SendMessage ( cWnd.hWnd.hCOutput, WM_SETTEXT, 0, (LPARAM) "" );
		cWnd.OptionWnd.SetWISPOptions ( TRUE );
		cWnd.OptionWnd.SetCOBOLOptions ( TRUE );
		WaitForSingleObject ( cApp.Thread.hPreEntrySmphr, INFINITE );
		cApp.Thread.Param = 3;
		hBldCmdLine = CreateThread ( &sa, 0, 
			 ( LPTHREAD_START_ROUTINE ) ThreadManager, 
			NULL, 0, &thrdID );
		CloseHandle ( hBldCmdLine );
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	WIPSOps - Controls the WISP option window
//

//////////////////////////////////////////////////////////////////////////
//
//	cWindows::_OptionWindow::UnDimWOptions
//		Enables the WISP controls
//
cWindows::_OptionWindow::UnDimWOptions ( )
{
	//	Get window handles for static controls 
	WCtls.l_FromDir = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_FromDir );
	WCtls.l_I = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctll_I );
	WCtls.l_K = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctll_K );
	WCtls.l_O = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctll_O );
	WCtls.l_P = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctll_P );
	WCtls.l_V = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctll_V );
	WCtls.l_W = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctll_W );
    WCtls.l_TargetFile = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctll_TargetFile );

	EnableWindow ( WCtls._DirPath, TRUE );
	EnableWindow ( WCtls.l_FromDir, TRUE );
	EnableWindow ( WCtls._1, TRUE );
	EnableWindow ( WCtls._4, TRUE );
	EnableWindow ( WCtls._C, TRUE );
	EnableWindow ( WCtls._D, TRUE );
	EnableWindow ( WCtls._e, TRUE );
	EnableWindow ( WCtls._f, TRUE );
	EnableWindow ( WCtls.l_I, TRUE );
	EnableWindow ( WCtls._I, TRUE );
	EnableWindow ( WCtls._IBrs, TRUE );
	EnableWindow ( WCtls.l_K, TRUE );
	EnableWindow ( WCtls._K, TRUE );
	EnableWindow ( WCtls._KBrs, TRUE );
	EnableWindow ( WCtls._l, TRUE );
	EnableWindow ( WCtls._L, TRUE );
	EnableWindow ( WCtls._m, TRUE );
	EnableWindow ( WCtls._M, TRUE );
	EnableWindow ( WCtls.l_O, TRUE );
	EnableWindow ( WCtls._O, TRUE );
	EnableWindow ( WCtls._OBrs, TRUE );
	EnableWindow ( WCtls.l_P, TRUE );
	EnableWindow ( WCtls._P, TRUE );
	EnableWindow ( WCtls._PBrs, TRUE );
	EnableWindow ( WCtls._q, TRUE );
	EnableWindow ( WCtls._R, TRUE );
	EnableWindow ( WCtls._S, TRUE );
	EnableWindow ( WCtls._T, TRUE );
	EnableWindow ( WCtls.l_V, TRUE );
	EnableWindow ( WCtls._V, TRUE );
	EnableWindow ( WCtls._w, TRUE );
	EnableWindow ( WCtls.l_W, TRUE );
	EnableWindow ( WCtls._W, TRUE );
	EnableWindow ( WCtls._WBrs, TRUE );
	EnableWindow ( WCtls._x, TRUE );
	EnableWindow ( WCtls._X, TRUE );
	EnableWindow ( WCtls._TargetFile, TRUE );
	EnableWindow ( WCtls.l_TargetFile, TRUE );

	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	cWindows::_OptionWindow::AddTargets
//		Adds the target files to the combo boxes
//
cWindows::_OptionWindow::AddTargets ( char *ch_FileSpecs )
{
	char ch_SingleFName[256];
	int cnt, 
		StrStart, 
		StrEnd, 
		FileSpecLen;
	BOOL MultiSelect;
	int LoopCnt;
	//	If the WISP tab is active
	int CurTab;
	CurTab = SendMessage ( cWnd.hWnd.hTabWnd, TCM_GETCURSEL, 0, 0 );
	//	If WISP tab is active
	if ( CurTab == 0 ) {
		//	Enable menu options
		EnableMenuItem ( cWnd.hAppMenu, 
			mnu_Set_Save_Dir, MF_BYCOMMAND | MF_ENABLED );
		EnableMenuItem ( cWnd.hAppMenu, 
			mnu_Set_Save_File, MF_BYCOMMAND | MF_ENABLED );
		EnableMenuItem ( cWnd.hAppMenu, 
			mnu_Set_Ret_Dir, MF_BYCOMMAND | MF_ENABLED );
		EnableMenuItem ( cWnd.hAppMenu, 
			mnu_Translate, MF_BYCOMMAND | MF_ENABLED );
		LoopCnt = 1;
		//	Remove any files that are currently in the combo box
		cnt = SendMessage (
			cWnd.OptionWnd.WCtls._TargetFile, CB_GETCOUNT, 0, 0 );
		while ( cnt >= 0 ) {
			SendMessage (
				cWnd.OptionWnd.WCtls._TargetFile, CB_DELETESTRING,  cnt, 0 );
			cnt--;
		}
		cnt = SendMessage (
			cWnd.OptionWnd.CCtls._TargetFile, CB_GETCOUNT, 0, 0 );
		while ( cnt >= 0 ) {
			SendMessage (
				cWnd.OptionWnd.CCtls._TargetFile, CB_DELETESTRING,  cnt, 0 );
			cnt--;
		}
		MultiSelect = FALSE;
		cnt = FileSpecLen = strlen ( ch_FileSpecs );
		//	Check for a space in the list of files returned - space means mutliple files selected
		while ( ch_FileSpecs[cnt] != '\\' ) {
			if ( ch_FileSpecs[cnt] == ' ' ) MultiSelect = TRUE;
			if ( cnt == 0 ) break;
			cnt--;
		}
		StrStart = 0;
		StrEnd = cnt + 1;
		//	Copy the dir path to a local var
		if ( MultiSelect == FALSE )
			memcpy ( ch_DirPath, &ch_FileSpecs[0], StrEnd - StrStart );
		else {
			while ( ch_FileSpecs[cnt] != ' ' ) cnt++;
			StrEnd = cnt;
			memcpy ( ch_DirPath, &ch_FileSpecs[0], StrEnd - StrStart );
			ch_DirPath[StrEnd-StrStart] = '\0';
		}
		// Ensure the dir ends with a backslash
		int DirLen = strlen(ch_DirPath);
		if (DirLen > 0 &&
			ch_DirPath[DirLen-1] != '\\')
		{
			strcat( ch_DirPath, "\\" );
		}

		//	Put the dir path in the edit control of the WISP sheet
		SendMessage ( 
			cWnd.OptionWnd.WCtls._DirPath,
			WM_SETTEXT, 0, (LPARAM) ch_DirPath );
		SendMessage ( 
			cWnd.OptionWnd.CCtls._DirPath,
			WM_SETTEXT, 0, (LPARAM) ch_DirPath );
		//	Get the names of the files selected
		while ( ch_FileSpecs[cnt] != '\0' ) {
			if ( MultiSelect == FALSE ) cnt = StrStart = StrEnd;
			else cnt = StrStart = StrEnd + 1;
			while ( ch_FileSpecs[cnt] != '\0' ) {
				if ( ch_FileSpecs[cnt] != ' ' ) cnt++;
				else break;
			}
			StrEnd = cnt;
			//	Copy the file name to a local var
			memcpy ( ch_SingleFName, 
				&ch_FileSpecs[StrStart], StrEnd - StrStart );
			ch_SingleFName[StrEnd - StrStart] = '\0';
			int cnt1 = 0;
			//	Add the file name to the combo box
			SendMessage ( cWnd.OptionWnd.WCtls._TargetFile, 
				CB_ADDSTRING, 0, (LPARAM) ch_SingleFName );
		}
		//	Put the first file name in the combo box into the edit control part
		SendMessage ( cWnd.OptionWnd.WCtls._TargetFile, CB_SETCURSEL, 0, 0 );
		GetDlgItemText ( cWnd.hWnd.hWISPOpWnd,
			GetDlgCtrlID ( cWnd.OptionWnd.WCtls._TargetFile ),
			ch_FileSpecs, _MAX_PATH );
		//	Check if the file in the edit part of the combo box has a corresponding settings file
		cApp.FSettings.ChkOptFileExist ( ch_FileSpecs );

		//	Add files to COBOL list
		int Index = 0;
		char FName[_MAX_PATH];
		//	while we are not at the last file in the combo box
		while ( Index < SendMessage (
				cWnd.OptionWnd.WCtls._TargetFile,
				CB_GETCOUNT, 0, 0 ) ) {
			//	Get the file name
			SendMessage ( cWnd.OptionWnd.WCtls._TargetFile,
				CB_GETLBTEXT, Index, (LPARAM) FName );
			cnt = 0;
			//	Find the end of the file name or the beginning of the extension, whichever comes first
			while ( FName[cnt] != '\0' ) {
				if ( FName[cnt] == '.' ) break;
				cnt++;
			}
			//	change the extension to ".cob"
			FName[cnt] = '\0';
			strcat ( FName, ".cob" );
			//	Add the new file name to the COBOL file name combo box
			SendMessage ( cWnd.OptionWnd.CCtls._TargetFile,
				CB_ADDSTRING, 0, (LPARAM) FName );
			Index++;
		}
		//	put the first file name in the edit box part of the combo box
		SendMessage ( cWnd.OptionWnd.CCtls._TargetFile,
			CB_SETCURSEL, 0, 0 );
		cWnd.OptionWnd.UnDimWOptions ( );
		cWnd.OptionWnd.UnDimCOptions ( );
	}
	//	If the COBOL sheet is active
	else {
		char ch_SingleFName[256];
		int cnt, 
			StrStart, 
			StrEnd, 
			FileSpecLen;
		BOOL MultiSelect;
		//	Get the number of files previously selected
		cnt = SendMessage ( cWnd.OptionWnd.CCtls._TargetFile, CB_GETCOUNT, 0, 0 );
		//	while there is still something in the edit control, delete it
		while ( cnt >= 0 ) {
			SendMessage ( cWnd.OptionWnd.CCtls._TargetFile, CB_DELETESTRING, cnt, 0 );
			cnt--;
		}
		MultiSelect = FALSE;
		//	Get the length of the file name list as returned from the open dlg proc
		cnt = FileSpecLen = strlen ( ch_FileSpecs );
		//	Starting from the right of the list of file names look for a backslach and a space.  Backslash is the end of the file list, spaces sepereate files in the list
		while ( ch_FileSpecs[cnt] != '\\' ) {
			if ( ch_FileSpecs[cnt] == ' ' ) MultiSelect = TRUE;
			if ( cnt == 0 ) break;
			cnt--;
		}
		StrStart = 0;																		//	Set to start of di path
		StrEnd = cnt + 1;																	//	Set to one character passed the last backslash in the string
		//	Copy the dir path to a local var
		if ( MultiSelect == FALSE ) 
			memcpy ( ch_DirPath, &ch_FileSpecs[0], StrEnd - StrStart );
		else {
			while ( ch_FileSpecs[cnt] != ' ' ) cnt++;
			StrEnd = cnt;
			memcpy ( ch_DirPath, &ch_FileSpecs[0], StrEnd - StrStart );
			ch_DirPath[StrEnd-StrStart] = '\0';
		}
		// Ensure the dir ends with a backslash
		int DirLen = strlen(ch_DirPath);
		if (DirLen > 0 &&
			ch_DirPath[DirLen-1] != '\\')
		{
			strcat( ch_DirPath, "\\" );
		}

		//	Display dir path in edit control in the window
		SendMessage ( 
			cWnd.OptionWnd.CCtls._DirPath,
			WM_SETTEXT, 0, (LPARAM) ch_DirPath );
		//	Until you reach the end of the string
		while ( ch_FileSpecs[cnt] != '\0' ) {
			//	Set them to the first character of the first file name in the list
			if ( MultiSelect == FALSE ) cnt = StrStart = StrEnd;
			else cnt = StrStart = StrEnd + 1;
			//	Find the end of the file name
			while ( ch_FileSpecs[cnt] != '\0' ) {
				if ( ch_FileSpecs[cnt] != ' ' ) cnt++;
				else break;
			}
			StrEnd = cnt;
			//	Copy the file name to another buffer
			memcpy ( ch_SingleFName, 
				&ch_FileSpecs[StrStart], StrEnd - StrStart );
			ch_SingleFName[StrEnd - StrStart] = '\0';
			//	Add the name to the combo box
			SendMessage ( cWnd.OptionWnd.CCtls._TargetFile, 
				CB_ADDSTRING, 0, (LPARAM) ch_SingleFName );
		}
		//	Select the first name in the combo box
		SendMessage ( cWnd.OptionWnd.CCtls._TargetFile, CB_SETCURSEL, 0, 0 );
		cWnd.OptionWnd.UnDimCOptions ( );
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	cWindows::_OptionWindow::CreateWCtls
//		Creates the WISP controls
//
cWindows::_OptionWindow::CreateWCtls ( )
{
	WCtls._TargetFile = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_TargetFile );
	WCtls._DirPath = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_DirPath );
	WCtls._TargetBrs = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_TargetBrs );
	WCtls._1 = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_1 );
	WCtls._C = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_C );
	WCtls._x = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_x );
	WCtls._X = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_X2 );
	WCtls._q = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_q );
	WCtls._S = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_S );
	WCtls._D = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_D );
	WCtls._m = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_m );
	WCtls._M = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_M2 );
	WCtls._f = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_f );
	WCtls._w = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl__w );
	WCtls._I = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_I );
	WCtls._IBrs = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_IBrs );
	WCtls._K = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_K );
	WCtls._KBrs = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_KBrs );
	WCtls._V = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_V );
	WCtls._L = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_L );
	WCtls._l = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl__l );
	WCtls._O = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_O );
	WCtls._OBrs = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_OBrs );
	WCtls._R = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_R );
	WCtls._T = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_T );
	WCtls._e = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_e );
	WCtls._W = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_W );
	WCtls._WBrs = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_WBrs );
	WCtls._4 = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_4 );
	WCtls._P = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_P );
	WCtls._PBrs = GetDlgItem ( cWnd.hWnd.hWISPOpWnd, ctl_PBrs );

	int yPos, xPos;
	yPos = 10;
	xPos = 10;
	int OrderCnt = 0;

	char CurDir[_MAX_PATH];
	GetCurrentDirectory ( _MAX_PATH, CurDir );
	SetWindowText ( WCtls._DirPath, CurDir );

	//	Add items to language list
	SendMessage ( WCtls._V, CB_ADDSTRING, 0, (LPARAM) "ACN" );
	SendMessage ( WCtls._V, CB_ADDSTRING, 0, (LPARAM) "ACU" );
	SendMessage ( WCtls._V, CB_ADDSTRING, 0, (LPARAM) "MF" );
	//SendMessage ( WCtls._V, CB_ADDSTRING, 0, (LPARAM) "VAX" );
	SendMessage ( WCtls._V, CB_SETCURSEL, 1, 0 );

	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	cWindows::_OptionWindow::SetWISPOptions
//		Saves the current WISP options to glaobal vars
//
cWindows::_OptionWindow::SetWISPOptions ( BOOL Exec )
{
	//	Put the contents of the edit fields into struct in cApp.CmdLine
	GetDlgItemText ( cWnd.hWnd.hWISPOpWnd, 
		GetDlgCtrlID ( WCtls._I ),							//	Get the Control ID
		cApp.CmdLine.WOptions->ch_I, MaxSwitch );													//	Place the text in Options->_I
	GetDlgItemText ( cWnd.hWnd.hWISPOpWnd, 
		GetDlgCtrlID ( WCtls._K ),							//	Get the Control ID
		cApp.CmdLine.WOptions->ch_K, MaxSwitch );													//	Place the text in Options->_K
	GetDlgItemText ( cWnd.hWnd.hWISPOpWnd, 
		GetDlgCtrlID ( WCtls._O ),							//	Get the Control ID
		cApp.CmdLine.WOptions->ch_O, MaxSwitch );													//	Place the text in Options->_O
	GetDlgItemText ( cWnd.hWnd.hWISPOpWnd, 
		GetDlgCtrlID ( WCtls._P ),							//	Get the Control ID
		cApp.CmdLine.WOptions->ch_P, MaxSwitch );													//	Place the text in Options->_P
	GetDlgItemText ( cWnd.hWnd.hWISPOpWnd, 
		GetDlgCtrlID ( WCtls._W ),							//	Get the Control ID
		cApp.CmdLine.WOptions->ch_W, MaxSwitch );													//	Place the text in Options->_W
	GetDlgItemText ( cWnd.hWnd.hWISPOpWnd, 
		GetDlgCtrlID ( WCtls._V ),							//	Get the Control ID
		cApp.CmdLine.WOptions->ch_V, 4 );													//	Get the currently selected language
	//	Get the check button states - if the button is checked then set to TRUE, else will be false
	if ( SendMessage ( 
			WCtls._1, BM_GETCHECK, 0, 0 ) == BST_CHECKED ) 
		cApp.CmdLine.WOptions->_1 = TRUE;
	else cApp.CmdLine.WOptions->_1 = FALSE;
	if ( SendMessage ( 
			WCtls._4, BM_GETCHECK, 0, 0 ) == BST_CHECKED ) 
		cApp.CmdLine.WOptions->_4 = TRUE;
	else cApp.CmdLine.WOptions->_4 = FALSE;
	if ( SendMessage ( 
			WCtls._C, BM_GETCHECK, 0, 0 ) == BST_CHECKED ) 
		cApp.CmdLine.WOptions->_C = TRUE;
	else cApp.CmdLine.WOptions->_C = FALSE;
	if ( SendMessage ( 
			WCtls._D, BM_GETCHECK, 0, 0 ) == BST_CHECKED ) 
		cApp.CmdLine.WOptions->_D = TRUE;
	else cApp.CmdLine.WOptions->_D = FALSE;
	if ( SendMessage ( 
			WCtls._e, BM_GETCHECK, 0, 0 ) == BST_CHECKED ) 
		cApp.CmdLine.WOptions->_e = TRUE;
	else cApp.CmdLine.WOptions->_e = FALSE;
	if ( SendMessage ( 
			WCtls._f, BM_GETCHECK, 0, 0 ) == BST_CHECKED ) 
		cApp.CmdLine.WOptions->_f = TRUE;
	else cApp.CmdLine.WOptions->_f = FALSE;
	if ( SendMessage ( 
			WCtls._l, BM_GETCHECK, 0, 0 ) == BST_CHECKED ) 
		cApp.CmdLine.WOptions->_l = TRUE;
	else cApp.CmdLine.WOptions->_l = FALSE;
	if ( SendMessage ( 
			WCtls._L, BM_GETCHECK, 0, 0 ) == BST_CHECKED ) 
		cApp.CmdLine.WOptions->_L = TRUE;
	else cApp.CmdLine.WOptions->_L = FALSE;
	if ( SendMessage ( 
			WCtls._m, BM_GETCHECK, 0, 0 ) == BST_CHECKED ) 
		cApp.CmdLine.WOptions->_m = TRUE;
	else cApp.CmdLine.WOptions->_m = FALSE;
	if ( SendMessage ( 
			WCtls._M, BM_GETCHECK, 0, 0 ) == BST_CHECKED ) 
		cApp.CmdLine.WOptions->_M = TRUE;
	else cApp.CmdLine.WOptions->_M = FALSE;
	if ( SendMessage ( 
			WCtls._q, BM_GETCHECK, 0, 0 ) == BST_CHECKED ) 
		cApp.CmdLine.WOptions->_q = TRUE;
	else cApp.CmdLine.WOptions->_q = FALSE;
	if ( SendMessage ( 
			WCtls._R, BM_GETCHECK, 0, 0 ) == BST_CHECKED ) 
		cApp.CmdLine.WOptions->_R = TRUE;
	else cApp.CmdLine.WOptions->_R = FALSE;
	if ( SendMessage ( 
			WCtls._S, BM_GETCHECK, 0, 0 ) == BST_CHECKED ) 
		cApp.CmdLine.WOptions->_S = TRUE;
	else cApp.CmdLine.WOptions->_S = FALSE;
	if ( SendMessage ( 
			WCtls._T, BM_GETCHECK, 0, 0 ) == BST_CHECKED ) 
		cApp.CmdLine.WOptions->_T = TRUE;
	else cApp.CmdLine.WOptions->_T = FALSE;
	if ( SendMessage ( 
			WCtls._w, BM_GETCHECK, 0, 0 ) == BST_CHECKED ) 
		cApp.CmdLine.WOptions->_w = TRUE;
	else cApp.CmdLine.WOptions->_w = FALSE;
	if ( SendMessage ( 
			WCtls._x, BM_GETCHECK, 0, 0 ) == BST_CHECKED ) 
		cApp.CmdLine.WOptions->_x = TRUE;
	else cApp.CmdLine.WOptions->_x = FALSE;
	if ( SendMessage ( 
			WCtls._X, BM_GETCHECK, 0, 0 ) == BST_CHECKED ) 
		cApp.CmdLine.WOptions->_X = TRUE;
	else cApp.CmdLine.WOptions->_X = FALSE;
	//	Test the target file's validity
	//		If the field is empty
	if ( Exec == TRUE ) {
		if ( GetDlgItemText ( 
			cWnd.hWnd.hWISPOpWnd, GetDlgCtrlID ( WCtls._TargetFile ),							//	Get the edit control's ID
			cApp.CmdLine.WOptions->ch_TargetFile, MaxSwitch ) == 0 ) {							//	If the field is empty...
			MessageBox ( cWnd.hWnd.hShellWnd, 
				"You must enter a file name.",
				"Input Error", MB_OK );																	//	Show an error box
			SetFocus ( WCtls._TargetFile );							//	Set the focus back to the Target Name edit field
			return 0;																						//	Return from the function without further validity checking
		}
		//		If the field is not empty - as it souldn't be
		else {
			GetDlgItemText ( cWnd.hWnd.hWISPOpWnd,
				GetDlgCtrlID ( WCtls._TargetFile ),						//	Get Target File's control ID
				cApp.CmdLine.WOptions->ch_TargetFile, MaxSwitch );									//	If the field is not empty...
		}
	}
	else {
		GetDlgItemText ( cWnd.hWnd.hWISPOpWnd,
			GetDlgCtrlID ( WCtls._TargetFile ),							//	Get Target File's control ID
			cApp.CmdLine.WOptions->ch_TargetFile, MaxSwitch );										//	If the field is not empty...
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	cWindows::_OptionWindow::WBClicked
//		Responds to a button being clicked on the WISP sheet
//
cWindows::_OptionWindow::WBClicked ( LPARAM lp )
{
	OPENFILENAME OFN;			//	structure to hold information for the common open dialog box, used with browse buttons
	char ch_FileName[1024];		//	Holds the name of the currently selected file

	ZeroMemory ( &OFN, sizeof (OPENFILENAME) );
	ZeroMemory ( ch_FileName, sizeof (ch_FileName) );				//	Initialize the file name buffer
	//	If button clicked is a browse button
	//	Initialize the OFN structure
	OFN.lStructSize	=	sizeof (OPENFILENAME);								//	Set size of file open info structure
	OFN.hwndOwner		=	cWnd.hWnd.hWISPOpWnd;												//	Owner of File Open Dialog box
	OFN.nFilterIndex	=	1;															//	File type to use as default
	OFN.lpstrFile		=	ch_FileName;												//	Holds Path and file names returned by dialog
	OFN.nMaxFile		=	sizeof ( ch_FileName );									//	Max size of file names and path to return
	OFN.Flags			=	OFN_FILEMUSTEXIST |
								OFN_PATHMUSTEXIST |
								OFN_NOCHANGEDIR;			//	Options for the dialog
	//	if the Target File Name browse button was clicked
	if ( (HWND)	lp == WCtls._TargetBrs ) {
		OFN.lpstrFilter	=	"Wang COBOL (*.wcb)\0*.WCB\0"
							"Wang Data Conversion (*.wdc)\0"
							"*.WDC\0Text (*.txt)\0*.TXT\0";							//	Default file types
		OFN.lpstrTitle	=	"Select Files";									//	Title of Dialog
		OFN.Flags		=	OFN_ALLOWMULTISELECT |
							OFN_FILEMUSTEXIST |
							OFN_PATHMUSTEXIST |
							OFN_LONGNAMES;							//	Options for the dialog
		if ( GetOpenFileName ( &OFN ) == TRUE ) {								//	Call the open file dialog - if a file was selected and ok was clicked
			cWnd.OptionWnd.AddTargets ( ch_FileName );												//	Call AddTargets func to add the selected file names to the list box
			cApp.FSettings.GetMaster ( );
			EnableWindow ( 
				cWnd.hWnd.hExecuteBtn, TRUE );
			EnableWindow (
				cWnd.hWnd.hExecCompBtn, TRUE );
		}
		else return 0;
		char ch_isDir[_MAX_PATH];
		ZeroMemory ( ch_isDir, sizeof ( ch_isDir ));
		SendMessage ( WCtls._DirPath, 
			WM_GETTEXT, _MAX_PATH, (LPARAM) ch_isDir );
		SetFocus ( WCtls._TargetBrs );
	}
	//	If use specific key file browse button clicked
	if ( (HWND) lp == WCtls._KBrs ) {
		strcpy ( ch_FileName, "" );
		OFN.lpstrFilter	=	"Key Files (*.key)\0*.KEY\0All (*.*)\0*.*\0";
		OFN.nMaxFileTitle	=	0;
		OFN.lpstrTitle		=	"Select File";
		if ( GetOpenFileName ( &OFN ) == TRUE ) {
			SendMessage ( WCtls._K, 
				WM_SETTEXT, 0, (LPARAM) ch_FileName );
		}
	}
	//
	if ( (HWND) lp == WCtls._OBrs ) {
		strcpy ( ch_FileName, "" );
		OFN.lpstrFilter	=	"Option Files (*.opt)\0*.OPT\0All (*.*)\0*.*\0";
		OFN.nMaxFileTitle	=	0;
		OFN.lpstrTitle		=	"Select Files";
		if ( GetOpenFileName ( &OFN ) == TRUE ) {
			SendMessage ( WCtls._O, 
				WM_SETTEXT, 0, (LPARAM) ch_FileName );
		}
	}
	//
	if ( (HWND) lp == WCtls._WBrs ) {
		strcpy ( ch_FileName, "" );
		OFN.lpstrFilter	=	"All (*.*)\0*.*\0";
		OFN.nMaxFileTitle	=	0;
		OFN.lpstrTitle		=	"Select Files";
		if ( GetOpenFileName ( &OFN ) == TRUE ) {
			SendMessage ( WCtls._W, 
				WM_SETTEXT, 0, (LPARAM) ch_FileName );
		}
	}
	char CurDir[_MAX_PATH];
	GetWindowText ( cWnd.OptionWnd.WCtls._DirPath, CurDir, _MAX_PATH );
	//
	if ( (HWND) lp == WCtls._PBrs ) {
		CustCtrl.Dialogs.SelectDirDlg.Create 
			( cWnd.hWnd.hShellWnd, CurDir,
			TRUE, TVL_DIRS, WCtls._P, "Select Prefix Path" );
	}
	//	Input ... browse button
	if ( (HWND) lp == WCtls._IBrs ) {
		CustCtrl.Dialogs.SelectDirDlg.Create 
			( cWnd.hWnd.hShellWnd, 
			CurDir, TRUE, TVL_DIRS, WCtls._I, "Select Input Copybook Path" );
	}
	SetWISPOptions ( FALSE );
	cApp.CmdLine.Build ( FALSE, TC_Translate );
	cApp.FSettings.SetMaster ( );
	return 0;
}


//////////////////////////////////////////////////////////////////////////
//
//	COBOLOps - Controls the COBOL option window
//

//////////////////////////////////////////////////////////////////////////
//
//	cWindows::_OptionWindow::SetCOBOLOptions
//		Saves the current COBOL options to global vars
//
cWindows::_OptionWindow::SetCOBOLOptions ( BOOL Exec )
{
	//	Put the contents of the edit fields into struct in cApp.CmdLine
	GetDlgItemText ( cWnd.hWnd.hCOBOLOpWnd, 
		GetDlgCtrlID ( CCtls._CFlags ),
		cApp.CmdLine.COptions->ch_CFlags, GenText );
	GetDlgItemText ( cWnd.hWnd.hCOBOLOpWnd,
		GetDlgCtrlID ( CCtls._ObjExt ),
		cApp.CmdLine.COptions->ch_ObjExt, GenText );
	GetDlgItemText ( cWnd.hWnd.hCOBOLOpWnd,
		GetDlgCtrlID ( CCtls._OutputDir ),
		cApp.CmdLine.COptions->ch_OutputDir, MaxSwitch );
	GetDlgItemText ( cWnd.hWnd.hCOBOLOpWnd, 
		GetDlgCtrlID ( CCtls._PathToComp ),
		cApp.CmdLine.COptions->ch_PathToComp, MaxSwitch );
	//	Get the check Language button states
	if ( SendMessage ( 
			CCtls._AcuBtn, BM_GETCHECK, 0, 0 ) == BST_CHECKED ) 
		cApp.CmdLine.COptions->_Lang = CL_Acu;
	else cApp.CmdLine.COptions->_Lang = CL_MF;
	//	Test the target file's validity
	//		If the field is empty
	int CurSel = SendMessage ( CCtls._TargetFile,
		CB_GETCURSEL, 0, 0 );
	SendMessage ( CCtls._TargetFile, CB_GETLBTEXT,
		(WPARAM) cApp.CmdLine.COptions->ch_TargetFile, _MAX_PATH );
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	cWindows::_AppWindow::_OptionWindow::_COBOLOps::CreateCCtls
//		Creates the COBOL controls
//
cWindows::_OptionWindow::CreateCCtls ( )
{
	CCtls._TargetFile = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctl_TargetFile );
	CCtls._TargetBrs = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctl_TargetBrs );
	CCtls._DirPath = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctl_DirPath );
	CCtls._PathToComp = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctl_PathToComp );
	CCtls._CompBrs = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctl_CompBrs );
	CCtls._CFlags = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctl_CFlags );
	CCtls._OutputDir = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctl_OutputDir );
	CCtls._OutputDirBrs = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctl_OutputDirBrs );
	CCtls._ObjExt = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctl_ObjExt );
	CCtls._LangGroup = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctl_CLang );
	CCtls._AcuBtn = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctl_AcuBtn );
	CCtls._MFBtn = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctl_MFBtn );
	//	Get seetings from registry
	if ( cApp.Env->RegInfo.COBOLLang == 0 ) {
		SetWindowText ( CCtls._PathToComp, cApp.Env->RegInfo.ch_ACPath );
		SetWindowText ( CCtls._CFlags, cApp.Env->RegInfo.ACOBOLFlags );
		SetWindowText ( CCtls._OutputDir, cApp.Env->RegInfo.ACOBOLOutDir );
		SetWindowText ( CCtls._ObjExt, cApp.Env->RegInfo.ACOBOLObjExt );
	}
	else {
		SetWindowText ( CCtls._PathToComp, cApp.Env->RegInfo.ch_MCPath );
		SetWindowText ( CCtls._CFlags, cApp.Env->RegInfo.MCOBOLFlags );
		SetWindowText ( CCtls._OutputDir, cApp.Env->RegInfo.MCOBOLOutDir );
		SetWindowText ( CCtls._ObjExt, cApp.Env->RegInfo.MCOBOLObjExt );
	}
	SendMessage ( CCtls._AcuBtn, 
		WM_SETFONT, (WPARAM) cWnd.TabFont, TRUE );
	if ( cApp.Env->RegInfo.COBOLLang == 0 )
		SendMessage ( CCtls._AcuBtn, BM_SETCHECK, BST_CHECKED, 0 );
	else SendMessage ( CCtls._MFBtn, BM_SETCHECK, BST_CHECKED, 0 );

	char CurDir[_MAX_PATH];
	GetCurrentDirectory ( _MAX_PATH, CurDir );
	SetWindowText ( CCtls._DirPath, CurDir );

	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	cWindows::_OptionWindow::UnDimCOptions
//		Enables the COBOL controls
//
cWindows::_OptionWindow::UnDimCOptions ( )
{
	//	Get handles for static labels
	CCtls.l_CFlags = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctll_CFlags );
	CCtls.l_FromDir = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctll_FromDir );
	CCtls.l_ObjExt = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctll_ObjExt );
	CCtls.l_OutputDir = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctll_OutputDir );
	CCtls.l_PathToComp = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctll_PathToComp );
	CCtls.l_TargetFile = GetDlgItem ( cWnd.hWnd.hCOBOLOpWnd, ctll_TargetFile );

	EnableWindow ( CCtls._DirPath, TRUE );
	EnableWindow ( CCtls._AcuBtn, TRUE );
	EnableWindow ( CCtls._CFlags, TRUE );
	EnableWindow ( CCtls._LangGroup, TRUE );
	EnableWindow ( CCtls._MFBtn, TRUE );
	EnableWindow ( CCtls._ObjExt, TRUE );
	EnableWindow ( CCtls._OutputDir, TRUE );
	EnableWindow ( CCtls._PathToComp, TRUE );
	EnableWindow ( CCtls.l_CFlags, TRUE );
	EnableWindow ( CCtls._DirPath, TRUE );
	EnableWindow ( CCtls.l_FromDir, TRUE );
	EnableWindow ( CCtls.l_ObjExt, TRUE );
	EnableWindow ( CCtls.l_OutputDir, TRUE );
	EnableWindow ( CCtls.l_PathToComp, TRUE );
	EnableWindow ( CCtls._OutputDirBrs, TRUE );
	EnableWindow ( CCtls._CompBrs, TRUE );
	EnableWindow ( CCtls.l_TargetFile, TRUE );
	EnableWindow ( CCtls._TargetFile, TRUE );

	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	cWindows::_OptionWindow::CBClicked
//		Responds to a button being clicked on the COBOL sheet
//
cWindows::_OptionWindow::CBClicked ( LPARAM lp )
{
	OPENFILENAME OFN;							//	structure to hold information for the common open dialog box, used with browse buttons
	char ch_FileName[1024];					//	Holds the name of the currently selected file

	ZeroMemory ( &OFN, sizeof (OPENFILENAME) );
	ZeroMemory ( ch_FileName, sizeof (ch_FileName) );				//	Initialize the file name buffer
	//	If a radio button is clicked
	if ((HWND) lp == CCtls._AcuBtn) {
		SendMessage ( CCtls._AcuBtn, BM_SETCHECK, BST_CHECKED, 0 );
		SendMessage ( CCtls._MFBtn, BM_SETCHECK, BST_UNCHECKED, 0 );
		GetWindowText ( CCtls._PathToComp, cApp.Env->RegInfo.ch_MCPath, _MAX_PATH );
		SetWindowText ( CCtls._PathToComp, cApp.Env->RegInfo.ch_ACPath );
		GetWindowText ( CCtls._CFlags, cApp.Env->RegInfo.MCOBOLFlags, 256 );
		SetWindowText ( CCtls._CFlags, cApp.Env->RegInfo.ACOBOLFlags );
		GetWindowText ( CCtls._ObjExt, cApp.Env->RegInfo.MCOBOLObjExt, 4 );
		SetWindowText ( CCtls._ObjExt, cApp.Env->RegInfo.ACOBOLObjExt );
		GetWindowText ( CCtls._OutputDir, cApp.Env->RegInfo.MCOBOLOutDir, _MAX_PATH );
		SetWindowText ( CCtls._OutputDir, cApp.Env->RegInfo.ACOBOLOutDir );
		SetCOBOLOptions ( FALSE );
		cApp.CmdLine.Build ( FALSE, TC_Compile );
	}
	if ((HWND) lp == CCtls._MFBtn) {
		SendMessage ( CCtls._AcuBtn, BM_SETCHECK, BST_UNCHECKED, 0 );
		SendMessage ( CCtls._MFBtn, BM_SETCHECK, BST_CHECKED, 0 );
		GetWindowText ( CCtls._PathToComp, cApp.Env->RegInfo.ch_ACPath, _MAX_PATH );
		SetWindowText ( CCtls._PathToComp, cApp.Env->RegInfo.ch_MCPath );
		GetWindowText ( CCtls._CFlags, cApp.Env->RegInfo.ACOBOLFlags, 256 );
		SetWindowText ( CCtls._CFlags, cApp.Env->RegInfo.MCOBOLFlags );
		GetWindowText ( CCtls._ObjExt, cApp.Env->RegInfo.ACOBOLObjExt, 4 );
		SetWindowText ( CCtls._ObjExt, cApp.Env->RegInfo.MCOBOLObjExt );
		GetWindowText ( CCtls._OutputDir, cApp.Env->RegInfo.ACOBOLOutDir, _MAX_PATH );
		SetWindowText ( CCtls._OutputDir, cApp.Env->RegInfo.MCOBOLOutDir );
		SetCOBOLOptions ( FALSE );
		cApp.CmdLine.Build ( FALSE, TC_Compile );
	}
	//	If button clicked is a browse button
	//	Initialize the OFN structure
	OFN.lStructSize	=	sizeof (OPENFILENAME);		//	Set size of file open info structure
	OFN.hwndOwner		=	cWnd.hWnd.hCOBOLOpWnd;					//	Owner of File Open Dialog box
	OFN.nFilterIndex	=	1;									//	File type to use as default
	OFN.lpstrFile		=	ch_FileName;						//	Holds Path and file names returned by dialog
	OFN.nMaxFile		=	sizeof ( ch_FileName );			//	Max size of file names and path to return
	OFN.Flags			=	OFN_NOCHANGEDIR |
								OFN_FILEMUSTEXIST |
								OFN_PATHMUSTEXIST;			//	Options for the dialog
	//	if the Target File Name browse button was clicked
	if ( (HWND)	lp == CCtls._TargetBrs ) {
		OFN.lpstrFilter	=	"COBOL Files (*.COB)\0*.cob\0";							//	Default file types
		OFN.lpstrTitle		=	"Select Files";									//	Title of Dialog
		OFN.Flags			=	OFN_ALLOWMULTISELECT | 
									OFN_FILEMUSTEXIST |
									OFN_PATHMUSTEXIST;								//	Options for the dialog
		if ( GetOpenFileName ( &OFN ) == TRUE ) {								//	Call the open file dialog - if a file was selected and ok was clicked
			cWnd.OptionWnd.AddTargets ( ch_FileName );												//	Call AddTargets func to add the selected file names to the list box
			UnDimCOptions ( );
			EnableWindow ( cWnd.hWnd.hExecuteBtn, TRUE );
			SetCOBOLOptions ( FALSE );
			cApp.CmdLine.Build ( FALSE, TC_Compile );
		}
		SetFocus ( CCtls._TargetBrs );
	}
	//	Compiler browser
	if ( (HWND) lp == CCtls._CompBrs ) {
		strcpy ( ch_FileName, "" );
		OFN.lpstrFilter	=	"Programs (*.exe;*.com;*.bat)\0*.exe;*.com;*.bat\0";
		OFN.nMaxFileTitle	=	0;
		OFN.lpstrTitle		=	"Select COBOL Compiler";
		if ( GetOpenFileName ( &OFN ) == TRUE ) {
			SendMessage ( CCtls._PathToComp, 
				WM_SETTEXT, 0, (LPARAM) ch_FileName );
			SetFocus ( CCtls._CompBrs );
			SetCOBOLOptions ( FALSE );
			cApp.CmdLine.Build ( FALSE, TC_Compile );
		}
	}
	//	OutputDir browse button
	if ( (HWND) lp == CCtls._OutputDirBrs ) {
		char StartDir[_MAX_PATH];
		ZeroMemory ( StartDir, sizeof ( StartDir ));
		GetWindowText ( cWnd.OptionWnd.CCtls._OutputDir, StartDir, _MAX_PATH );
		CustCtrl.Dialogs.SelectDirDlg.Create ( cWnd.hWnd.hShellWnd, NULL,
			FALSE, TVL_DIRS, cWnd.OptionWnd.CCtls._OutputDir,
			"Select Output Directory" );

	}
	cApp.CmdLine.Build ( FALSE, TC_Compile );
	return 0;
}
