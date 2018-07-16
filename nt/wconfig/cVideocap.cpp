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
//	cDialogs::_Videocap::Initialize
//
int cDialogs::_Videocap::Initialize ( )
{
	HKEY hKey;
	HWND hDlg = Dialogs.Videocap.hVideocap;
	DWORD BufSize = _MAX_PATH;
	UCHAR sRegValue[_MAX_PATH];

	//	If unable to open key
	if ( CreateRegistryKey ( REGKEY_WISP_VIDEOCAP, &hKey ) == ERROR_SUCCESS ) {
		//	Get value for WISPTERM field
		BufSize = _MAX_PATH;
		if ( RegQueryValueEx ( hKey, REGVAL_VIDEOCAP_WISPTERM, 0, NULL, sRegValue,
			&BufSize ) == ERROR_SUCCESS ) {

			sRegValue[BufSize] = '\0';
			SetDlgItemText (
				Dialogs.Videocap.hVideocap,
				S02_WISPTERM, (char *) sRegValue );
		}
		else {
			SetDlgItemText (
				Dialogs.Videocap.hVideocap,
				S02_WISPTERM, "wincon" );
		}
		//	Get value for VIDEOCAPDIR field
		BufSize = _MAX_PATH;
		if ( RegQueryValueEx ( hKey, REGVAL_VIDEOCAP_VIDEOCAP, 0, NULL, sRegValue,
			&BufSize ) == ERROR_SUCCESS ) {
			SetDlgItemText (
				Dialogs.Videocap.hVideocap,
				S02_VIDEOCAPDIR, (char *) sRegValue );
		}
		else {
			char sDefVal[_MAX_PATH];
			sprintf ( sDefVal, "%s\\videocap",
				cApp.Env.sWISPConfig );
			SetDlgItemText (
				Dialogs.Videocap.hVideocap,
				S02_VIDEOCAPDIR, sDefVal );
		}
		RegCloseKey ( hKey );
	}

	HWND hCtl = GetDlgItem ( hVideocap, S02_VIDEOCAPDIR );
	ChkInvalid ( hCtl );

	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_Videocap::Save
//		Saves the data in the Videocap dialog to the registry
//
int cDialogs::_Videocap::Save ( )
{
	HWND hDlg;
	HKEY hKey;
	UCHAR sRegVal[_MAX_PATH];

	hDlg = Dialogs.Videocap.hVideocap;
	RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_VIDEOCAP, 0, KEY_ALL_ACCESS, &hKey );

	GetDlgItemText (
		hDlg, S02_WISPTERM, (char *) sRegVal, _MAX_PATH );
	RegSetValueEx (	hKey, REGVAL_VIDEOCAP_WISPTERM, 0, REG_SZ, sRegVal,
		strlen ((char *) sRegVal )+1);
	GetDlgItemText (
		hDlg, S02_VIDEOCAPDIR, (char *) sRegVal, _MAX_PATH );
	RegSetValueEx (	hKey, REGVAL_VIDEOCAP_VIDEOCAP, 0, REG_SZ, sRegVal,
		strlen ((char *) sRegVal )+1);

	RegFlushKey ( hKey );
	RegCloseKey ( hKey );

	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_Videocap::ChkInvalid
//		Validates required fields and if the field contains an
//		invalid entry then it sets the 'iv' flag to false so
//		that the background will be painted yellow
//
void cDialogs::_Videocap::ChkInvalid ( HWND hCtl )
{
	WIN32_FIND_DATA ValFile;
	char sTemp[_MAX_PATH],
		sTemp1[_MAX_PATH];
	ZeroMemory ( sTemp1, sizeof ( sTemp1 ));
	ZeroMemory ( sTemp, sizeof ( sTemp ));
	ZeroMemory ( &ValFile, sizeof ( ValFile ));

	GetWindowText ( hCtl, sTemp, _MAX_PATH );
	if ( hCtl == GetDlgItem ( 
		Dialogs.Videocap.hVideocap, S02_VIDEOCAPDIR )) {

		strcat ( sTemp, "\\*.*" );
		if ( FindFirstFile
			( sTemp, &ValFile ) == INVALID_HANDLE_VALUE  ) {
			ivVideoCapDir = FALSE;
		}
		else ivVideoCapDir = TRUE;
	}
	//	The WISPTERM entry must be a file in the directory named in the
	//	VIDEOCAPDIR field, so let's build a search string consisting
	//	of the path from VIDEOCAPDIR and the filename in WISPTERM
	if ( hCtl == GetDlgItem (
		Dialogs.Videocap.hVideocap, S02_WISPTERM )) {

		GetDlgItemText ( hVideocap, 
			S02_VIDEOCAPDIR, sTemp1, _MAX_PATH );
		strcat ( sTemp1, "\\" );
		strcat ( sTemp1, sTemp );
		if ( FindFirstFile
			( sTemp1, &ValFile ) == INVALID_HANDLE_VALUE )
		{
			// Check with .vcap extension
			strcat ( sTemp1, ".vcap" );
			if ( FindFirstFile
				( sTemp1, &ValFile ) == INVALID_HANDLE_VALUE )
			{
				ivWispTerm = FALSE;
			}
			else ivWispTerm = TRUE;
		}
		else ivWispTerm = TRUE;
	}
	InvalidateRect ( hCtl, NULL, FALSE );
	return;
}
