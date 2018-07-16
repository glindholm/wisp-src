//////////////////////////////////////////////////////////////////////////
//
//	This file contains the member function definitions for the 
//		cDialogs::_ACP class
//
#include "#Defines.h"
#include "#Classes.h"
#include "#Externs.h"
#include "#Prototypes.h"
#include "resource.h"


//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_WPROC::Initialize
//		Initializes the WPROC dialog
//
int cDialogs::_WPROC::Initialize ( )
{
	HKEY hKey;
	HWND hDlg = Dialogs.WPROC.hWPROC;
	DWORD BufSize = _MAX_PATH;
	UCHAR sRegValue[_MAX_PATH];
	//	If unable to open key
	if ( CreateRegistryKey (  REGKEY_WISP_WISPBIN_WPROC, &hKey ) == ERROR_SUCCESS ) {
		//	Get value for WPROC field
		BufSize = _MAX_PATH;
		if ( RegQueryValueEx (hKey, REGVAL_WPROC_WPROC, 0, NULL, sRegValue,
			&BufSize ) == ERROR_SUCCESS ) {
			sRegValue[BufSize] = '\0';
			SetDlgItemText (
				Dialogs.WPROC.hWPROC,
				S09_WPROC, (char *) sRegValue );
		}
		else {
			char sDefVal[_MAX_PATH];
			sprintf ( sDefVal, "%s\\bin\\wproc.exe",
				cApp.Env.sWISPDir );
			SetDlgItemText (
				Dialogs.WPROC.hWPROC,
				S09_WPROC, sDefVal );
		}
		//	Get value for WPROCDEBUG field
		BufSize = _MAX_PATH;
		if ( RegQueryValueEx (hKey, REGVAL_WPROC_WPROCDEBUG, 0, NULL, sRegValue,
			&BufSize ) == ERROR_SUCCESS ) {
			sRegValue[BufSize] = '\0';
			SetDlgItemText (
				Dialogs.WPROC.hWPROC, S09_WPROCDEBUG,
				(char *) sRegValue );
		}
		else {
			SetDlgItemText (
				Dialogs.WPROC.hWPROC,
				S09_WPROCDEBUG, "" );
		}
		RegCloseKey ( hKey );
	}

	HWND hCtl = GetDlgItem ( hWPROC, S09_WPROC );
	ChkInvalid ( hCtl );

	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_WPROC::Save
//		Saves the settings from the WPROC dialog to the registry
//
int cDialogs::_WPROC::Save ( )
{
	HWND hDlg;
	HKEY hKey;
	UCHAR sRegVal[_MAX_PATH];

	hDlg = Dialogs.WPROC.hWPROC;
	RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_WISPBIN_WPROC, 0, KEY_ALL_ACCESS, &hKey );

	GetDlgItemText ( hDlg, S09_WPROC, (char *) sRegVal, _MAX_PATH );
	RegSetValueEx ( hKey, REGVAL_WPROC_WPROC, 0, REG_SZ, sRegVal,
		strlen ((char *) sRegVal )+1);
	GetDlgItemText ( hDlg, S09_WPROCDEBUG, (char *) sRegVal, _MAX_PATH );
	RegSetValueEx ( hKey, REGVAL_WPROC_WPROCDEBUG, 0, REG_SZ, sRegVal,
		strlen ((char *) sRegVal )+1);

	RegFlushKey ( hKey );
	RegCloseKey ( hKey );
	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_WPROC::ChkInvalid
//		Validates required fields and if the field contains an
//		invalid entry then it sets the 'iv' flag to false so
//		that the background will be painted yellow
//
void cDialogs::_WPROC::ChkInvalid ( HWND hCtl )
{
	WIN32_FIND_DATA ValFile;
	char sTemp[_MAX_PATH];
	ZeroMemory ( sTemp, sizeof ( sTemp ));
	ZeroMemory ( &ValFile, sizeof ( ValFile ));

	GetWindowText ( hCtl, sTemp, _MAX_PATH );
	if ( hCtl == GetDlgItem 
		( Dialogs.WPROC.hWPROC, S09_WPROC )) {
		if ( FindFirstFile (
			sTemp, &ValFile ) == INVALID_HANDLE_VALUE ) {

			strcat ( sTemp, "\\*.*" );
			if ( FindFirstFile ( sTemp,
				&ValFile ) == INVALID_HANDLE_VALUE  ) {
				ivWPROC = FALSE;
			}
			else ivWPROC = TRUE;
		}
		else ivWPROC = TRUE;
	}
	InvalidateRect ( hCtl, NULL, FALSE );
	return;
}
