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
//	cDialogs::_DISPLAY::Initialize
//
int cDialogs::_DISPLAY::Initialize ( )
{
	HKEY hKey;
	HWND hDlg = Dialogs.DISPLAY.hDISPLAY;
	DWORD BufSize = _MAX_PATH;
	UCHAR sRegValue[_MAX_PATH];

	if ( CreateRegistryKey ( REGKEY_WISP_WISPBIN_DISPLAY,&hKey ) == ERROR_SUCCESS ) {

		BufSize = _MAX_PATH;
		//	Get the value from the registry
		if ( RegQueryValueEx ( hKey, REGVAL_DISPLAY_WISPDISPLAY8BIT,
			0, NULL, sRegValue, &BufSize ) == ERROR_SUCCESS ) {
			sRegValue[BufSize] = '\0';
			SetDlgItemText (
				Dialogs.DISPLAY.hDISPLAY,
				S08_DISPLAY8BIT, (char *) sRegValue );
		}
		//	If unable to open the registry key then set the
		//	default to "NO"
		else {
			SetDlgItemText ( Dialogs.DISPLAY.hDISPLAY,
				S08_DISPLAY8BIT, "NO" );
		}
		RegCloseKey ( hKey );
	}

	HWND hCtl = GetDlgItem ( hDISPLAY, S08_DISPLAY8BIT );
	ChkInvalid ( hCtl );

	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_DISPLAY::Save
//		Writes the data in the dialog box to the registry
//
int cDialogs::_DISPLAY::Save ( )
{
	HWND hDlg;
	HKEY hKey;
	UCHAR sRegVal[_MAX_PATH];

	hDlg = Dialogs.DISPLAY.hDISPLAY;
	RegOpenKeyEx (HKEY_LOCAL_MACHINE, REGKEY_WISP_WISPBIN_DISPLAY, 0, KEY_ALL_ACCESS, &hKey );

	GetDlgItemText (
		hDlg, S08_DISPLAY8BIT, (char *) sRegVal, _MAX_PATH );
	RegSetValueEx (	hKey, REGVAL_DISPLAY_WISPDISPLAY8BIT, 0, REG_SZ,
		sRegVal, strlen ((char *) sRegVal )+1);

	RegFlushKey ( hKey );
	RegCloseKey ( hKey );
	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_DISPLAY::ChkInvalid
//		Validates required fields and if the field contains an
//		invalid entry then it sets the 'iv' flag to false so
//		that the background will be painted yellow
//
void cDialogs::_DISPLAY::ChkInvalid ( HWND hCtl )
{
	WIN32_FIND_DATA ValFile;
	char sTemp[_MAX_PATH];
	ZeroMemory ( sTemp, sizeof ( sTemp ));
	ZeroMemory ( &ValFile, sizeof ( ValFile ));

	GetWindowText ( hCtl, sTemp, _MAX_PATH );
	//	If the string entered is neither "YES" or "NO" then set the
	//	validation flag to false
	if ( hCtl == GetDlgItem 
		( Dialogs.DISPLAY.hDISPLAY, S08_DISPLAY8BIT )) {
		if ( strcmp ( sTemp, "NO" ) != 0 ) {
			if ( strcmp ( sTemp, "YES" ) != 0 )
				ivUse8Bit = FALSE;
			else ivUse8Bit = TRUE;
		}
		else ivUse8Bit = TRUE;
	}
	InvalidateRect ( hCtl, NULL, FALSE );
	return;
}
