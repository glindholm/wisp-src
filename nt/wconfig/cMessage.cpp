//////////////////////////////////////////////////////////////////////////
//
//	This file contains the member function definitions for the 
//		cDialogs::_DISPLAY class
//
#include "#Defines.h"
#include "#Classes.h"
#include "#Externs.h"
#include "#Prototypes.h"
#include "resource.h"

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_MESSAGE::Initialize
//		Initializes the MESSAGE dialog
//
int cDialogs::_MESSAGE::Initialize ( )
{
	HKEY hKey;
	HWND hDlg = Dialogs.MESSAGE.hMESSAGE;
	DWORD BufSize = _MAX_PATH;
	UCHAR sRegValue[_MAX_PATH];
	//	If unable to open key
	if ( CreateRegistryKey ( REGKEY_WISP_VSSUBS_MESSAGE, &hKey ) == ERROR_SUCCESS ) {
		//	Get value for SHAREDIR field
		BufSize = _MAX_PATH;
		if ( RegQueryValueEx (hKey, REGVAL_MESSAGE_SHAREDIR, 0, NULL, sRegValue,
			&BufSize ) == ERROR_SUCCESS ) {
			sRegValue[BufSize] = '\0';
			SetDlgItemText (
				Dialogs.MESSAGE.hMESSAGE,
				S04_SHAREDIR, (char *) sRegValue );
		}
		else {
			char sDefVal[_MAX_PATH];
			sprintf ( sDefVal, "%s\\message", cApp.Env.sWISPDir );
			SetDlgItemText (
				Dialogs.MESSAGE.hMESSAGE,
				S04_SHAREDIR, sDefVal );
		}
		RegCloseKey ( hKey );
	}

	HWND hCtl = GetDlgItem ( hMESSAGE, S04_SHAREDIR );
	ChkInvalid ( hCtl );

	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_MESSAGE::Save
//		Saves the data in the MESSAGE dialog to the registry
//
int cDialogs::_MESSAGE::Save ( )
{
	HWND hDlg;
	HKEY hKey;
	UCHAR sRegVal[_MAX_PATH];

	hDlg = Dialogs.MESSAGE.hMESSAGE;
	RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_VSSUBS_MESSAGE, 0, KEY_ALL_ACCESS, &hKey );

	GetDlgItemText (
		hDlg, S04_SHAREDIR, (char *) sRegVal, _MAX_PATH );
	RegSetValueEx (	hKey, REGVAL_MESSAGE_SHAREDIR, 0, REG_SZ, sRegVal,
		strlen ((char *) sRegVal )+1);

	RegFlushKey ( hKey );
	RegCloseKey ( hKey );
	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_MESSAGE::ChkInvalid
//		Validates required fields and if the field contains an
//		invalid entry then it sets the 'iv' flag to false so
//		that the background will be painted yellow
//
void cDialogs::_MESSAGE::ChkInvalid ( HWND hCtl )
{
	WIN32_FIND_DATA ValFile;
	char sTemp[_MAX_PATH];
	ZeroMemory ( sTemp, sizeof ( sTemp ));
	ZeroMemory ( &ValFile, sizeof ( ValFile ));

	GetWindowText ( hCtl, sTemp, _MAX_PATH );
	if ( hCtl == GetDlgItem ( Dialogs.MESSAGE.hMESSAGE, S04_SHAREDIR )) {
		strcat ( sTemp, "\\*.*" );
		if ( FindFirstFile 
			( sTemp, &ValFile ) == INVALID_HANDLE_VALUE  ) {
			ivShareDir = FALSE;
		}
		else ivShareDir = TRUE;
	}
	InvalidateRect ( hCtl, NULL, FALSE );
	return;
}
