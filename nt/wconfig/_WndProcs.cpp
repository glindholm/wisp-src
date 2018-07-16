//////////////////////////////////////////////////////////////////////////
//	This file contains the Window and Dialog procedures for the applicaiton

//	Include Directives
#include "#Classes.h"
#include "#Prototypes.h"
#include "#Externs.h"
#include "#Defines.h"
#define InvalidClr RGB( 255, 255, 128 )
//////////////////////////////////////////////////////////////////////////
//	AppWndProc - Window procedure for the main application window
LRESULT CALLBACK AppWndProc ( HWND hAppWnd, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi,														//	Holds the hi order word of wp
		wpLo;															//	Holds the lo order word of wp
	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	switch ( msg ) {
		case WM_SYSCOLORCHANGE:
			Ctl3dColorChange ( );
			break;
		case WM_DESTROY:
			PostQuitMessage ( 1 );
			break;
		default:
			return ( DefWindowProc ( hAppWnd, msg, wp, lp ));
			break;
	}
	return ( DefWindowProc ( hAppWnd, msg, wp, lp ));
}
//////////////////////////////////////////////////////////////////////////
//		MainDlgProc
BOOL CALLBACK MainDlgProc ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;

	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	switch ( msg ) {
		case WM_INITDIALOG:
			Dialogs.MainDialog.Initialize ( hDlg );
			break;
		case WM_NOTIFY:
			if ( wp == tabRegOps ) {
				NMHDR *nm;
				nm = (NMHDR *) lp;
				switch ( nm->code ) {
					case TCN_SELCHANGE:
						Dialogs.MainDialog.ChngTab ( hDlg );
						break;
				}
			}
			break;
		case WM_COMMAND:
			switch ( wpLo ) {
				case IDOK:
					Dialogs.MainDialog.SaveReg ( hDlg );
					EndDialog ( hDlg, 0 );
					PostQuitMessage ( 0 );
					break;
				case IDCANCEL:
					EndDialog ( hDlg, 1 );
					PostQuitMessage ( 1 );
					break;
				case btnRemove:
					Dialogs.MainDialog.RemoveClicked ( );
					break;
				case btnApply:
					Dialogs.MainDialog.SaveReg ( hDlg );
					break;
				case btnImport:
					cApp.ImportRegData ( hDlg );
					break;
				case btnExport:
					cApp.WriteRegFile ( );
					break;
			}
			break;
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//	General_DP
BOOL CALLBACK General_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;

	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	HWND Caller;
	DWORD dwStackSize = 0, dwThreadID;
	HBRUSH DaBrush;
	LOGBRUSH lb;
	switch ( msg ) {
		case WM_INITDIALOG:
			Dialogs.General.hGeneral = hDlg;
			break;
		case WM_CTLCOLOREDIT:
			ZeroMemory ( &lb, sizeof ( lb ));
			lb.lbStyle = BS_SOLID;
			lb.lbColor = RGB ( 255, 255, 128 );
			DaBrush = CreateBrushIndirect ( &lb );
			if ( (HWND) lp == GetDlgItem 
					( Dialogs.General.hGeneral, S01_WISPDIR )) {
				if ( Dialogs.General.ivWISPDir == FALSE ) {
					SetBkColor ( (HDC) wp, OPAQUE );
					SetBkMode ( (HDC) wp, TRANSPARENT );
					return (int) DaBrush;
				}
				else return (int) GetStockObject ( WHITE_BRUSH );
			}
			if ( (HWND) lp == GetDlgItem 
					( Dialogs.General.hGeneral, S01_WISPCONFIG )) {
				if ( Dialogs.General.ivWISPConfig == FALSE ) {
					SetBkColor ( (HDC) wp, OPAQUE );
					SetBkMode ( (HDC) wp, TRANSPARENT );
					return (int) DaBrush;
				}
				else return (int) GetStockObject ( WHITE_BRUSH );
			}
			if ( (HWND) lp == GetDlgItem 
					( Dialogs.General.hGeneral, S01_USERDIR )) {
				if ( Dialogs.General.ivUserDir == FALSE ) {
					SetBkColor ( (HDC) wp, OPAQUE );
					SetBkMode ( (HDC) wp, TRANSPARENT );
					return (int) DaBrush;
				}
				else return (int) GetStockObject ( WHITE_BRUSH );
			}
			if ( (HWND) lp == GetDlgItem 
					( Dialogs.General.hGeneral, S01_TEMPDIR )) {
				if ( Dialogs.General.ivTempDir == FALSE ) {
					SetBkColor ( (HDC) wp, OPAQUE );
					SetBkMode ( (HDC) wp, TRANSPARENT );
					return (int) DaBrush;
				}
				else return (int) GetStockObject ( WHITE_BRUSH );
			}
			break;
		case WM_COMMAND:
			switch ( wpHi ) {
				case EN_UPDATE:
					CreateThread ( NULL, dwStackSize, 
						( LPTHREAD_START_ROUTINE ) Validate,
						(HWND) lp, NULL, &dwThreadID );
					HDC hDC = GetDC ( (HWND) lp );
					SendMessage ( Dialogs.General.hGeneral,
						WM_CTLCOLOREDIT, (WPARAM) hDC, (LPARAM) lp );
					ReleaseDC ( (HWND) lp, hDC );
					break;
			}
			char sCurDir[_MAX_PATH];
			WIN32_FIND_DATA FindData;
			switch ( wpLo ) {
				case S01_SERVER_BRS:
					Caller = GetDlgItem ( Dialogs.General.hGeneral, S01_SERVER );
					CustCtrl.Dialogs.SelectDirDlg.Create 
						( hDlg, NULL, FALSE, TVL_SERVERS, Caller,
						"WISP Configuration Utility - Select WISP Server" );
					break;
				case S01_WISPDIR_BRS:
					GetDlgItemText ( hDlg, S01_WISPDIR, sCurDir, _MAX_PATH );
					Caller = GetDlgItem ( hDlg, S01_WISPDIR );
					CustCtrl.Dialogs.SelectDirDlg.Create 
						( hDlg, sCurDir, FALSE, TVL_DIRS,
						Caller, "WISP Configuration Utility - Select WISPDIR Directory" );
					break;
				case S01_WISPCONFIG_BRS:
					GetDlgItemText ( hDlg, S01_WISPCONFIG, sCurDir, _MAX_PATH );
					if ( FindFirstFile ( sCurDir, &FindData ) ==
							INVALID_HANDLE_VALUE )
						GetCurrentDirectory ( _MAX_PATH, sCurDir );
					Caller = GetDlgItem ( hDlg, S01_WISPCONFIG );
					CustCtrl.Dialogs.SelectDirDlg.Create 
						( hDlg, sCurDir, FALSE, TVL_DIRS,
						Caller, "WISP Configuration Utility - Select WISPCONFIG Directory" );
					break;
				case S01_USERDIR_BRS:
					GetDlgItemText ( hDlg, S01_USERDIR, sCurDir, _MAX_PATH );
					if ( FindFirstFile ( sCurDir, &FindData ) ==
							INVALID_HANDLE_VALUE )
						GetCurrentDirectory ( _MAX_PATH, sCurDir );
					Caller = GetDlgItem ( hDlg, S01_USERDIR );
					CustCtrl.Dialogs.SelectDirDlg.Create 
						( hDlg, sCurDir, FALSE, TVL_DIRS, Caller,
						"WISP Configuration Utility - Select USERDIR Directory" );
					break;
				case S01_PATH_BRS:
					GetCurrentDirectory ( _MAX_PATH, sCurDir );
					Caller = GetDlgItem ( hDlg, S01_PATH );
					CustCtrl.Dialogs.SelectDirDlg.Create 
						( hDlg, sCurDir, TRUE, TVL_DIRS, Caller,
						"WISP Configuration Utility - Select PATH Directories" );
					break;
				case S01_TEMPDIR_BRS:
					GetDlgItemText ( hDlg, S01_TEMPDIR, sCurDir, _MAX_PATH );
					if ( FindFirstFile ( sCurDir, &FindData ) ==
							INVALID_HANDLE_VALUE )
						GetCurrentDirectory ( _MAX_PATH, sCurDir );
					Caller = GetDlgItem ( hDlg, S01_TEMPDIR );
					CustCtrl.Dialogs.SelectDirDlg.Create 
						( hDlg, sCurDir, FALSE, TVL_DIRS, Caller,
						"WISP Configuration Utility - Select TEMPDIR Directory" );
					break;
			}
			break;
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//		shtRegOps01DlgProc
BOOL CALLBACK Videocap_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;

	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	HWND Caller;
	DWORD dwStackSize = 0, dwThreadID;
	HBRUSH DaBrush;
	LOGBRUSH lb;
	switch ( msg ) {
		case WM_INITDIALOG:
			Dialogs.Videocap.hVideocap = hDlg;
			break;
		case WM_CTLCOLOREDIT:
			ZeroMemory ( &lb, sizeof ( lb ));
			lb.lbStyle = BS_SOLID;
			lb.lbColor = RGB ( 255, 255, 128 );
			DaBrush = CreateBrushIndirect ( &lb );
			if ( (HWND) lp == GetDlgItem 
					( Dialogs.Videocap.hVideocap, S02_VIDEOCAPDIR )) {
				if ( Dialogs.Videocap.ivVideoCapDir == FALSE ) {
					SetBkColor ( (HDC) wp, OPAQUE );
					SetBkMode ( (HDC) wp, TRANSPARENT );
					return (int) DaBrush;
				}
				else return (int) GetStockObject ( WHITE_BRUSH );
			}
			if ( (HWND) lp == GetDlgItem 
					( Dialogs.Videocap.hVideocap, S02_WISPTERM )) {
				if ( Dialogs.Videocap.ivWispTerm == FALSE ) {
					SetBkColor ( (HDC) wp, OPAQUE );
					SetBkMode ( (HDC) wp, TRANSPARENT );
					return (int) DaBrush;
				}
				else return (int) GetStockObject ( WHITE_BRUSH );
			}
			break;
		case WM_COMMAND:
			switch ( wpHi ) {
				case EN_UPDATE:
					CreateThread ( NULL, dwStackSize, 
						( LPTHREAD_START_ROUTINE ) Validate,
						(HWND) lp, NULL, &dwThreadID );
					HDC hDC = GetDC ( (HWND) lp );
					SendMessage ( Dialogs.Videocap.hVideocap,
						WM_CTLCOLOREDIT, (WPARAM) hDC, (LPARAM) lp );
					ReleaseDC ( (HWND) lp, hDC );
					break;
			}
			char sCurDir[_MAX_PATH];
			WIN32_FIND_DATA FindData;
			switch ( wpLo ) {
				case S02_VIDEOCAPDIR_BRS:
					GetDlgItemText ( hDlg, S02_VIDEOCAPDIR, sCurDir, _MAX_PATH );
					if ( FindFirstFile ( sCurDir, &FindData ) ==
							INVALID_HANDLE_VALUE )
						GetCurrentDirectory ( _MAX_PATH, sCurDir );
					Caller = GetDlgItem ( hDlg, S02_VIDEOCAPDIR );
					CustCtrl.Dialogs.SelectDirDlg.Create 
						( hDlg, sCurDir, FALSE, TVL_DIRS,
						Caller, "WISP Configuration Utility - Select VIDEOCAP Directory" );
					break;
			}
			break;
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//		shtRegOps01DlgProc
BOOL CALLBACK EXTRACT_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;

	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	switch ( msg ) {
		case WM_INITDIALOG:
			Dialogs.EXTRACT.hEXTRACT = hDlg;
			break;
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//		shtRegOps01DlgProc
BOOL CALLBACK MESSAGE_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;

	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	HWND Caller;
	DWORD dwStackSize = 0, dwThreadID;
	HBRUSH DaBrush;
	LOGBRUSH lb;
	switch ( msg ) {
		case WM_INITDIALOG:
			Dialogs.MESSAGE.hMESSAGE = hDlg;
			break;
		case WM_CTLCOLOREDIT:
			ZeroMemory ( &lb, sizeof ( lb ));
			lb.lbStyle = BS_SOLID;
			lb.lbColor = RGB ( 255, 255, 128 );
			DaBrush = CreateBrushIndirect ( &lb );
			if ( (HWND) lp == GetDlgItem 
					( Dialogs.MESSAGE.hMESSAGE, S04_SHAREDIR )) {
				if ( Dialogs.MESSAGE.ivShareDir == FALSE ) {
					SetBkColor ( (HDC) wp, OPAQUE );
					SetBkMode ( (HDC) wp, TRANSPARENT );
					return (int) DaBrush;
				}
				else return (int) GetStockObject ( WHITE_BRUSH );
			}
			break;
		case WM_COMMAND:
			switch ( wpHi ) {
				case EN_UPDATE:
					CreateThread ( NULL, dwStackSize, 
						( LPTHREAD_START_ROUTINE ) Validate,
						(HWND) lp, NULL, &dwThreadID );
					HDC hDC = GetDC ( (HWND) lp );
					SendMessage ( Dialogs.MESSAGE.hMESSAGE,
						WM_CTLCOLOREDIT, (WPARAM) hDC, (LPARAM) lp );
					ReleaseDC ( (HWND) lp, hDC );
					break;
			}
			char sCurDir[_MAX_PATH];
			WIN32_FIND_DATA FindData;
			switch ( wpLo ) {
				case S04_SHAREDIR_BRS:
					GetDlgItemText ( hDlg, S04_SHAREDIR, sCurDir, _MAX_PATH );
					if ( FindFirstFile ( sCurDir, &FindData ) ==
							INVALID_HANDLE_VALUE )
						GetCurrentDirectory ( _MAX_PATH, sCurDir );
					Caller = GetDlgItem ( hDlg, S04_SHAREDIR );
					CustCtrl.Dialogs.SelectDirDlg.Create 
						( hDlg, sCurDir, FALSE, TVL_DIRS,
						Caller, "WISP Configuration Utility - Select SHAREDIR Directory" );
					break;
			}
			break;
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//		shtRegOps01DlgProc
BOOL CALLBACK SCRATCH_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;

	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	DWORD dwStackSize = 0, dwThreadID;
	HBRUSH DaBrush;
	LOGBRUSH lb;
	switch ( msg ) {
		case WM_INITDIALOG:
			Dialogs.SCRATCH.hSCRATCH = hDlg;
			break;
		case WM_CTLCOLOREDIT:
			ZeroMemory ( &lb, sizeof ( lb ));
			lb.lbStyle = BS_SOLID;
			lb.lbColor = RGB ( 255, 255, 128 );
			DaBrush = CreateBrushIndirect ( &lb );
			if ( (HWND) lp == GetDlgItem 
					( Dialogs.SCRATCH.hSCRATCH, S05_SCRATCHMODE )) {
				if ( Dialogs.SCRATCH.ivScratchMode == FALSE ) {
					SetBkColor ( (HDC) wp, OPAQUE );
					SetBkMode ( (HDC) wp, TRANSPARENT );
					return (int) DaBrush;
				}
				else return (int) GetStockObject ( WHITE_BRUSH );
			}
			break;
		case WM_COMMAND:
			switch ( wpHi ) {
				case EN_UPDATE:
					CreateThread ( NULL, dwStackSize, 
						( LPTHREAD_START_ROUTINE ) Validate,
						(HWND) lp, NULL, &dwThreadID );
					HDC hDC = GetDC ( (HWND) lp );
					SendMessage ( Dialogs.SCRATCH.hSCRATCH,
						WM_CTLCOLOREDIT, (WPARAM) hDC, (LPARAM) lp );
					ReleaseDC ( (HWND) lp, hDC );
					break;
			}
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//		shtRegOps01DlgProc
BOOL CALLBACK License_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;
	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	DWORD dwStackSize = 0, dwThreadID;
	HBRUSH DaBrush;
	LOGBRUSH lb;

	switch ( msg ) {
		case WM_INITDIALOG:
			Dialogs.License.hLicense = hDlg;
			break;
		case WM_CTLCOLOREDIT:
			ZeroMemory ( &lb, sizeof ( lb ));
			lb.lbStyle = BS_SOLID;
			lb.lbColor = RGB ( 255, 255, 128 );
			DaBrush = CreateBrushIndirect ( &lb );
			if ( (HWND) lp == GetDlgItem 
					( Dialogs.License.hLicense, S06_LICENSEFILE )) {
				if ( Dialogs.License.ivLicenseFile == FALSE ) {
					SetBkColor ( (HDC) wp, OPAQUE );
					SetBkMode ( (HDC) wp, TRANSPARENT );
					return (int) DaBrush;
				}
				else return (int) GetStockObject ( WHITE_BRUSH );
			}
			break;
		case WM_COMMAND:
			switch ( wpHi ) {
				case EN_UPDATE:
					CreateThread ( NULL, dwStackSize, 
						( LPTHREAD_START_ROUTINE ) Validate,
						(HWND) lp, NULL, &dwThreadID );
					HDC hDC = GetDC ( (HWND) lp );
					SendMessage ( Dialogs.License.hLicense,
						WM_CTLCOLOREDIT, (WPARAM) hDC, (LPARAM) lp );
					ReleaseDC ( (HWND) lp, hDC );
					break;
			}
			switch ( wpLo ) {
				case S06_LICENSEFILE_BRS:
					OPENFILENAME ofn;
					char sFileName[_MAX_FNAME];
					ZeroMemory ( &ofn, sizeof ( OPENFILENAME ));
					ZeroMemory ( sFileName, sizeof ( sFileName ));
					ofn.lStructSize	=	sizeof ( OPENFILENAME );
					ofn.hwndOwner		=	hDlg;
					ofn.hInstance		=	cApp.hInstGlb;
					ofn.lpstrFilter	=	"All Files (*.*)\0*.*\0";
					ofn.nFilterIndex	=	1;
					ofn.lpstrFile		=	sFileName;
					ofn.nMaxFile		=	_MAX_FNAME;
					ofn.lpstrTitle		=	"Select Licence File";
					ofn.Flags			=	OFN_FILEMUSTEXIST | OFN_NOCHANGEDIR |
												OFN_PATHMUSTEXIST;
					char StartDir[_MAX_PATH];
					GetDlgItemText ( hDlg, S06_LICENSEFILE, StartDir, _MAX_PATH );
					if ( strlen ( StartDir ) > 0 ) {
						int cnt = strlen ( StartDir );
						while ( StartDir[cnt] != '\\' ) cnt--;
						StartDir[cnt] = '\0';
						ofn.lpstrInitialDir	=	StartDir;
					}

					if ( GetOpenFileName ( &ofn ) == TRUE ) {
						SetDlgItemText ( hDlg, S06_LICENSEFILE, sFileName );
					}
					break;
			}
			break;
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//		shtRegOps01DlgProc
BOOL CALLBACK WISPBin_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;
	char StartDir[_MAX_PATH];
	DWORD dwStackSize = 0, dwThreadID;
	HBRUSH DaBrush;
	LOGBRUSH lb;

	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	switch ( msg ) {
		case WM_INITDIALOG:
			Dialogs.WISPBin.hWISPBin = hDlg;
			break;
		case WM_CTLCOLOREDIT:
			ZeroMemory ( &lb, sizeof ( lb ));
			lb.lbStyle = BS_SOLID;
			lb.lbColor = RGB ( 255, 255, 128 );
			DaBrush = CreateBrushIndirect ( &lb );
			if ( (HWND) lp == GetDlgItem 
					( Dialogs.WISPBin.hWISPBin, S07_WISP )) {
				if ( Dialogs.WISPBin.ivWispExe == FALSE ) {
					SetBkColor ( (HDC) wp, OPAQUE );
					SetBkMode ( (HDC) wp, TRANSPARENT );
					return (int) DaBrush;
				}
				else return (int) GetStockObject ( WHITE_BRUSH );
			}
			if ( (HWND) lp == GetDlgItem 
					( Dialogs.WISPBin.hWISPBin, S07_WEDITOR )) {
				if ( Dialogs.WISPBin.ivWEditorExe == FALSE ) {
					SetBkColor ( (HDC) wp, OPAQUE );
					SetBkMode ( (HDC) wp, TRANSPARENT );
					return (int) DaBrush;
				}
				else return (int) GetStockObject ( WHITE_BRUSH );
			}
			if ( (HWND) lp == GetDlgItem 
					( Dialogs.WISPBin.hWISPBin, S07_DISPLAY )) {
				if ( Dialogs.WISPBin.ivDisplayExe == FALSE ) {
					SetBkColor ( (HDC) wp, OPAQUE );
					SetBkMode ( (HDC) wp, TRANSPARENT );
					return (int) DaBrush;
				}
				else return (int) GetStockObject ( WHITE_BRUSH );
			}
			break;
		case WM_COMMAND:
			switch ( wpHi ) {
				case EN_UPDATE:
					CreateThread ( NULL, dwStackSize, 
						( LPTHREAD_START_ROUTINE ) Validate,
						(HWND) lp, NULL, &dwThreadID );
					HDC hDC = GetDC ( (HWND) lp );
					SendMessage ( Dialogs.General.hGeneral,
						WM_CTLCOLOREDIT, (WPARAM) hDC, (LPARAM) lp );
					ReleaseDC ( (HWND) lp, hDC );
					break;
			}
			OPENFILENAME ofn;
			char sFileName[_MAX_FNAME];
			ZeroMemory ( &ofn, sizeof ( OPENFILENAME ));
			ZeroMemory ( sFileName, sizeof ( sFileName ));
			ofn.lStructSize	=	sizeof ( OPENFILENAME );
			ofn.hwndOwner		=	hDlg;
			ofn.hInstance		=	cApp.hInstGlb;
			ofn.lpstrFilter	=	"Executable Files (*.exe;*.com;*.bat)\0"
										"*.exe;*.com;*.bat\0";
			ofn.nFilterIndex	=	1;
			ofn.lpstrFile		=	sFileName;
			ofn.nMaxFile		=	_MAX_FNAME;
			ofn.Flags			=	OFN_FILEMUSTEXIST | OFN_NOCHANGEDIR |
										OFN_PATHMUSTEXIST;
			switch ( wpLo ) {
				case S07_WISP_BRS:
					ofn.lpstrTitle		=	"Select WISP Executable";
					GetDlgItemText ( hDlg, S07_WISP, StartDir, _MAX_PATH );
					if ( strlen ( StartDir ) > 0 ) {
						int cnt = strlen ( StartDir );
						while ( StartDir[cnt] != '\\' ) cnt--;
						StartDir[cnt] = '\0';
						ofn.lpstrInitialDir	=	StartDir;
					}
					if ( GetOpenFileName ( &ofn ) == TRUE ) {
						SetDlgItemText ( hDlg, S07_WISP, sFileName );
					}
					break;
				case S07_WEDITOR_BRS:
					ofn.lpstrTitle		=	"Select WISP Editor";
					GetDlgItemText ( hDlg, S07_WEDITOR, StartDir, _MAX_PATH );
					if ( strlen ( StartDir ) > 0 ) {
						int cnt = strlen ( StartDir );
						while ( StartDir[cnt] != '\\' ) cnt--;
						StartDir[cnt] = '\0';
						ofn.lpstrInitialDir	=	StartDir;
					}
					if ( GetOpenFileName ( &ofn ) == TRUE ) {
						SetDlgItemText ( hDlg, S07_WEDITOR, sFileName );
					}
					break;
				case S07_DISPLAY_BRS:
					ofn.lpstrTitle		=	"Select WISP Display Utility";
					GetDlgItemText ( hDlg, S07_DISPLAY, StartDir, _MAX_PATH );
					if ( strlen ( StartDir ) > 0 ) {
						int cnt = strlen ( StartDir );
						while ( StartDir[cnt] != '\\' ) cnt--;
						StartDir[cnt] = '\0';
						ofn.lpstrInitialDir	=	StartDir;
					}
					if ( GetOpenFileName ( &ofn ) == TRUE ) {
						SetDlgItemText ( hDlg, S07_DISPLAY, sFileName );
					}
					break;
			}
			break;
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//		shtRegOps01DlgProc
BOOL CALLBACK DISPLAY_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;
	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	DWORD dwStackSize = 0, dwThreadID;
	HBRUSH DaBrush;
	LOGBRUSH lb;
	switch ( msg ) {
		case WM_INITDIALOG:
			Dialogs.DISPLAY.hDISPLAY = hDlg;
			break;
		case WM_CTLCOLOREDIT:
			ZeroMemory ( &lb, sizeof ( lb ));
			lb.lbStyle = BS_SOLID;
			lb.lbColor = RGB ( 255, 255, 128 );
			DaBrush = CreateBrushIndirect ( &lb );
			if ( (HWND) lp == GetDlgItem 
					( Dialogs.DISPLAY.hDISPLAY, S08_DISPLAY8BIT )) {
				if ( Dialogs.DISPLAY.ivUse8Bit == FALSE ) {
					SetBkColor ( (HDC) wp, OPAQUE );
					SetBkMode ( (HDC) wp, TRANSPARENT );
					return (int) DaBrush;
				}
				else return (int) GetStockObject ( WHITE_BRUSH );
			}
			break;
		case WM_COMMAND:
			switch ( wpHi ) {
				case EN_UPDATE:
					CreateThread ( NULL, dwStackSize, 
						( LPTHREAD_START_ROUTINE ) Validate,
						(HWND) lp, NULL, &dwThreadID );
					HDC hDC = GetDC ( (HWND) lp );
					SendMessage ( Dialogs.DISPLAY.hDISPLAY,
						WM_CTLCOLOREDIT, (WPARAM) hDC, (LPARAM) lp );
					ReleaseDC ( (HWND) lp, hDC );
					break;
			}
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//		shtRegOps01DlgProc
BOOL CALLBACK WPROC_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;
	DWORD dwStackSize = 0, dwThreadID;
	HBRUSH DaBrush;
	LOGBRUSH lb;

	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	switch ( msg ) {
		case WM_INITDIALOG:
			Dialogs.WPROC.hWPROC = hDlg;
			break;
		case WM_CTLCOLOREDIT:
			ZeroMemory ( &lb, sizeof ( lb ));
			lb.lbStyle = BS_SOLID;
			lb.lbColor = RGB ( 255, 255, 128 );
			DaBrush = CreateBrushIndirect ( &lb );
			if ( (HWND) lp == GetDlgItem 
					( Dialogs.WPROC.hWPROC, S09_WPROC )) {
				if ( Dialogs.WPROC.ivWPROC == FALSE ) {
					SetBkColor ( (HDC) wp, OPAQUE );
					SetBkMode ( (HDC) wp, TRANSPARENT );
					return (int) DaBrush;
				}
				else return (int) GetStockObject ( WHITE_BRUSH );
			}
			break;
		case WM_COMMAND:
			switch ( wpHi ) {
				case EN_UPDATE:
					CreateThread ( NULL, dwStackSize, 
						( LPTHREAD_START_ROUTINE ) Validate,
						(HWND) lp, NULL, &dwThreadID );
					HDC hDC = GetDC ( (HWND) lp );
					SendMessage ( Dialogs.WPROC.hWPROC,
						WM_CTLCOLOREDIT, (WPARAM) hDC, (LPARAM) lp );
					ReleaseDC ( (HWND) lp, hDC );
					break;
			}
			switch ( wpLo ) {
				case S09_WPROC_BRS:
					OPENFILENAME ofn;
					char sFileName[_MAX_FNAME];
					ZeroMemory ( &ofn, sizeof ( OPENFILENAME ));
					ZeroMemory ( sFileName, sizeof ( sFileName ));
					ofn.lStructSize	=	sizeof ( OPENFILENAME );
					ofn.hwndOwner		=	hDlg;
					ofn.hInstance		=	cApp.hInstGlb;
					ofn.lpstrFilter	=	"Executable Files (*.exe;*.com;*.bat)\0"
												"*.exe;*.com;*.bat\0";
					ofn.nFilterIndex	=	1;
					ofn.lpstrFile		=	sFileName;
					ofn.nMaxFile		=	_MAX_FNAME;
					ofn.lpstrTitle		=	"Select WISP Procedure Interpreter";
					ofn.Flags			=	OFN_FILEMUSTEXIST | OFN_NOCHANGEDIR |
												OFN_PATHMUSTEXIST;

					char StartDir[_MAX_PATH];
					GetDlgItemText ( hDlg, S09_WPROC, StartDir, _MAX_PATH );
					if ( strlen ( StartDir ) > 0 ) {
						int cnt = strlen ( StartDir );
						while ( StartDir[cnt] != '\\' ) cnt--;
						StartDir[cnt] = '\0';
						ofn.lpstrInitialDir	=	StartDir;
					}

					if ( GetOpenFileName ( &ofn ) == TRUE ) {
						SetDlgItemText ( hDlg, S09_WPROC, sFileName );
					}
					break;
			}
			break;
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//		shtRegOps01DlgProc
BOOL CALLBACK ACP_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;
	DWORD dwStackSize = 0, dwThreadID;
	HBRUSH DaBrush;
	LOGBRUSH lb;

	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	HWND Caller;
	switch ( msg ) {
		case WM_INITDIALOG:
			Dialogs.ACP.hACP = hDlg;
			break;
		case WM_CTLCOLOREDIT:
			ZeroMemory ( &lb, sizeof ( lb ));
			lb.lbStyle = BS_SOLID;
			lb.lbColor = RGB ( 255, 255, 128 );
			DaBrush = CreateBrushIndirect ( &lb );
			if ( (HWND) lp == GetDlgItem 
					( Dialogs.ACP.hACP, S10_ACPCONFIGDIR )) {
				if ( Dialogs.ACP.ivACPConfigDir == FALSE ) {
					SetBkColor ( (HDC) wp, OPAQUE );
					SetBkMode ( (HDC) wp, TRANSPARENT );
					return (int) DaBrush;
				}
				else return (int) GetStockObject ( WHITE_BRUSH );
			}
			if ( (HWND) lp == GetDlgItem 
					( Dialogs.ACP.hACP, S10_ACPMAP )) {
				if ( Dialogs.ACP.ivACPMap == FALSE ) {
					SetBkColor ( (HDC) wp, OPAQUE );
					SetBkMode ( (HDC) wp, TRANSPARENT );
					return (int) DaBrush;
				}
				else return (int) GetStockObject ( WHITE_BRUSH );
			}
			break;
		case WM_COMMAND:
			switch ( wpHi ) {
				case EN_UPDATE:
					CreateThread ( NULL, dwStackSize, 
						( LPTHREAD_START_ROUTINE ) Validate,
						(HWND) lp, NULL, &dwThreadID );
					HDC hDC = GetDC ( (HWND) lp );
					SendMessage ( Dialogs.ACP.hACP,
						WM_CTLCOLOREDIT, (WPARAM) hDC, (LPARAM) lp );
					ReleaseDC ( (HWND) lp, hDC );
					break;
			}
			char sCurDir[_MAX_PATH];
			WIN32_FIND_DATA FindData;
			switch ( wpLo ) {
				case S10_ACPCONFIGDIR_BRS:
					GetDlgItemText ( hDlg, S10_ACPCONFIGDIR, sCurDir, _MAX_PATH );
					if ( FindFirstFile ( sCurDir, &FindData ) ==
							INVALID_HANDLE_VALUE )
						GetCurrentDirectory ( _MAX_PATH, sCurDir );
					Caller = GetDlgItem ( hDlg, S10_ACPCONFIGDIR );
					CustCtrl.Dialogs.SelectDirDlg.Create 
						( hDlg, sCurDir, FALSE, TVL_DIRS,
						Caller, "WISP Configuration Utility - Select ACPCONFIG Directory" );
					break;
				case S10_ACPMAP_BRS:
					OPENFILENAME ofn;
					char sFileName[_MAX_FNAME], sFullPath[_MAX_PATH];
					ZeroMemory ( &ofn, sizeof ( OPENFILENAME ));
					ZeroMemory ( sFileName, sizeof ( sFileName ));
					ZeroMemory ( sFullPath, sizeof ( sFullPath ));
					ofn.lStructSize	=	sizeof ( OPENFILENAME );
					ofn.hwndOwner		=	hDlg;
					ofn.hInstance		=	cApp.hInstGlb;
					ofn.lpstrFilter	=	"All Files (*.*)\0*.*\0";
					ofn.nFilterIndex	=	1;
					ofn.lpstrFile		=	sFullPath;
					ofn.nMaxFile		=	_MAX_PATH;
					ofn.lpstrFileTitle	=	sFileName;
					ofn.nMaxFileTitle	=	_MAX_FNAME;
					ofn.lpstrTitle		=	"Select ACPMAP Configuration File";
					ofn.Flags			=	OFN_FILEMUSTEXIST | OFN_NOCHANGEDIR |
												OFN_PATHMUSTEXIST;

					char StartDir[_MAX_PATH];
					GetDlgItemText ( hDlg, S10_ACPMAP, StartDir, _MAX_PATH );
					if ( strlen ( StartDir ) > 0 ) {
						int cnt = strlen ( StartDir );
						while ( StartDir[cnt] != '\\' ) cnt--;
						StartDir[cnt] = '\0';
						ofn.lpstrInitialDir	=	StartDir;
					}

					if ( GetOpenFileName ( &ofn ) == TRUE ) {
						SetDlgItemText ( hDlg, S10_ACPMAP, sFileName );
					}
					break;
			}
			break;
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//		WISPSrvDlgProc
BOOL CALLBACK WISPSrvDlgProc ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;

	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	DWORD Count = 0xFFFFFFFF, szBuf = 1000;
	int cnt = 0;

	switch ( msg ) {
		case WM_INITDIALOG:
			SetDlgItemText ( hDlg, ctl_ServerName, "(LOCAL)" );
			SetFocus ( GetDlgItem ( hDlg, ctl_ServerName ));
			break;
		case WM_COMMAND:
			switch ( wpHi ) {
				case EN_UPDATE:
					char sServerName[_MAX_PATH];
					GetDlgItemText ( hDlg, ctl_ServerName, sServerName, _MAX_PATH );
					if ( strlen ( sServerName ) > 0 )
						EnableWindow ( GetDlgItem ( hDlg, IDOK ), TRUE );
					else EnableWindow ( GetDlgItem ( hDlg, IDOK ), FALSE );
					break;
			}
			switch ( wpLo ) {
				case IDOK:
					char sTemp[_MAX_PATH], sServerName[_MAX_PATH];
					//	Get the value in the edit field
					GetDlgItemText ( hDlg, ctl_ServerName, sTemp, _MAX_PATH );
					cnt = 0;
					//	Increment counter to the first character that isn't a backslash
					while ( sTemp[cnt] == '\\' ) cnt++;
					//	Copy string from the position of the counter to the end of the string to the other variable
					memcpy ( sServerName, &sTemp[cnt], sizeof ( sTemp ) - cnt );
					//	Copy the string to the edit control on the tab sheet
					SetDlgItemText ( Dialogs.General.hGeneral, S01_SERVER, sServerName );
					EndDialog ( hDlg, 0 );
					break;
				case IDCANCEL:
					if ( MessageBox ( cApp.hMainDlg,
						"If you cancel now WISP will still not be configured.\n"
						"Are you sure you want to quit the WISP configuration utility?",
						"Quit WISP Configuration Utility", MB_YESNO ) == IDYES ) {
						cApp.DeleteReg ( );
						EndDialog ( hDlg, 1 );
						PostQuitMessage ( 0 );
						return 0;
					}
					else {
					}
					break;
				case ctl_ServerBrs:
					HANDLE hEnum;
					if ( WNetOpenEnum ( RESOURCE_GLOBALNET, RESOURCETYPE_ANY,
						RESOURCEUSAGE_CONTAINER, NULL, &hEnum ) == NO_ERROR ) {
						HWND Caller = GetDlgItem ( hDlg, ctl_ServerName );
						CustCtrl.Dialogs.SelectDirDlg.Create ( hDlg, 
							NULL, FALSE, TVL_SERVERS, Caller,
							"WISP Configuration Utility - Select WISP Server" );
					}
					else {
						MessageBox ( hDlg, 
							"No network is installed, or the network is not "
							"responding.  You must install the networking "
							"components for the network browser to work.",
							"No Network Installed", MB_OK );
					}
					break;
			}
			break;
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//		WISPDirDlgProc
BOOL CALLBACK WISPDirDlgProc ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;

	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	DWORD Count = 0xFFFFFFFF, szBuf = 1000;
	char DefVal[_MAX_PATH];
	ZeroMemory ( DefVal, sizeof ( DefVal ));

	switch ( msg ) {
		case WM_INITDIALOG:
			if ( stricmp ( cApp.Env.sWISPServer, "(LOCAL)" ) != 0 ) {
				strcpy ( DefVal, "\\\\" );
				strcat ( DefVal, cApp.Env.sWISPServer );
				strcat ( DefVal, "\\wisp" );
			}
			else {
				strcpy ( DefVal, "C:\\wisp" );
			}
			SetDlgItemText ( hDlg, ctl_DirPath, DefVal );
			SetFocus ( GetDlgItem ( hDlg, ctl_DirPath ));
			break;
		case WM_COMMAND:
			switch ( wpHi ) {
				case EN_UPDATE:
					char sDirPath[_MAX_PATH];
					GetDlgItemText ( hDlg, ctl_DirPath, sDirPath, _MAX_PATH );
					if ( strlen ( sDirPath ) > 0 )
						EnableWindow ( GetDlgItem ( hDlg, IDOK ), TRUE );
					else EnableWindow ( GetDlgItem ( hDlg, IDOK ), FALSE );
					break;
			}
			switch ( wpLo ) {
				case IDOK:
					char sDirPath[_MAX_PATH];
					GetDlgItemText ( hDlg, ctl_DirPath, sDirPath, _MAX_PATH );
					SetDlgItemText ( Dialogs.General.hGeneral, S01_WISPDIR, sDirPath );
					EndDialog ( hDlg, 0 );
					break;
				case IDCANCEL:
					if ( MessageBox ( cApp.hMainDlg,
						"If you cancel now WISP will still not be configured.\n"
						"Are you sure you want to quit the WISP configuration utility?",
						"Quit WISP Configuration Utility", MB_YESNO ) == IDYES ) {
						cApp.DeleteReg ( );
						EndDialog ( hDlg, 1 );
						PostQuitMessage ( 0 );
						return 0;
					}
					else {
					}
					break;
				case ctl_DirBrs:
					HWND Caller = GetDlgItem ( hDlg, ctl_DirPath );
					CustCtrl.Dialogs.SelectDirDlg.Create ( hDlg, 
						NULL, FALSE, TVL_DIRS, Caller,
						"WISP Configuration Utility - Select WISP Directory" );
					break;
			}
			break;
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//		WISPConfigDlgProc
BOOL CALLBACK WISPConfigDlgProc ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;

	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	DWORD Count = 0xFFFFFFFF, szBuf = 1000;
	char DefVal[_MAX_PATH];
	ZeroMemory ( DefVal, sizeof ( DefVal ));

	switch ( msg ) {
		case WM_INITDIALOG:
			strcpy ( DefVal, cApp.Env.sWISPDir );
			strcat ( DefVal, "\\config" );
			SetDlgItemText ( hDlg, ctl_DirPath, DefVal );
			SetFocus ( GetDlgItem ( hDlg, ctl_DirPath ));
			break;
		case WM_COMMAND:
			switch ( wpHi ) {
				case EN_UPDATE:
					char sDirPath[_MAX_PATH];
					GetDlgItemText ( hDlg, ctl_DirPath, sDirPath, _MAX_PATH );
					if ( strlen ( sDirPath ) > 0 )
						EnableWindow ( GetDlgItem ( hDlg, IDOK ), TRUE );
					else EnableWindow ( GetDlgItem ( hDlg, IDOK ), FALSE );
					break;
			}
			switch ( wpLo ) {
				case IDOK:
					char sDirPath[_MAX_PATH];
					GetDlgItemText ( hDlg, ctl_DirPath, sDirPath, _MAX_PATH );
					SetDlgItemText ( Dialogs.General.hGeneral, S01_WISPCONFIG, sDirPath );
					EndDialog ( hDlg, 0 );
					break;
				case IDCANCEL:
					if ( MessageBox ( cApp.hMainDlg,
						"If you cancel now WISP will still not be configured.\n"
						"Are you sure you want to quit the WISP configuration utility?",
						"Quit WISP Configuration Utility", MB_YESNO ) == IDYES ) {
						cApp.DeleteReg ( );
						EndDialog ( hDlg, 1 );
						PostQuitMessage ( 0 );
						return 0;
					}
					else {
					}
					break;
				case ctl_DirBrs:
					HWND Caller = GetDlgItem ( hDlg, ctl_DirPath );
					CustCtrl.Dialogs.SelectDirDlg.Create ( hDlg, 
						NULL, FALSE, TVL_DIRS, Caller,
						"WISP Configuration Utility - Select WISP Configuration Directory" );
					break;
			}
			break;
	}
	return 0;
}
