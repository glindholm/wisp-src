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
//	cDialogs::_ACP::Initialize
//		Initializes the ACP dialog
//
int cDialogs::_ACP::Initialize ( )
{
	HKEY hKey;
	HWND hDlg = Dialogs.ACP.hACP;
	DWORD BufSize = _MAX_PATH;
	UCHAR sRegValue[_MAX_PATH];

	if ( CreateRegistryKey ( REGKEY_WISP_ACP,&hKey ) == ERROR_SUCCESS ) {
		BufSize = _MAX_PATH;
		//*************************************************************
		//	Get value for ACPCONFIGDIR field
		if ( RegQueryValueEx ( hKey, REGVAL_ACP_ACPCONFIG, 0, NULL, sRegValue,
			&BufSize ) == ERROR_SUCCESS ) {
			sRegValue[BufSize] = '\0';
			SetDlgItemText (
				Dialogs.ACP.hACP, S10_ACPCONFIGDIR,
				(char *) sRegValue );
		}
		//	If unable to get the reg value then set default to the
		//	WISP Configuration directory
		else {
			SetDlgItemText ( Dialogs.ACP.hACP,
				S10_ACPCONFIGDIR, cApp.Env.sWISPConfig );
		}
		//*************************************************************
		//	Get value for ACPMAP field
		BufSize = _MAX_PATH;
		if ( RegQueryValueEx ( hKey, REGVAL_ACP_ACPMAP, 0, NULL, sRegValue,
			&BufSize ) == ERROR_SUCCESS ) {
			sRegValue[BufSize] = '\0';
			SetDlgItemText (
				Dialogs.ACP.hACP, S10_ACPMAP,
				(char *) sRegValue );
		}
		//	If unable to get the reg value then set default to the
		//	string "ACPMAP"
		else {
			SetDlgItemText (
				Dialogs.ACP.hACP, S10_ACPMAP, "ACPMAP" );
		}
		RegCloseKey ( hKey );
	}
	//*********************************************************************
	//	Now run the validation on the fields
	HWND hCtl = GetDlgItem ( hACP, S10_ACPCONFIGDIR );
	ChkInvalid ( hCtl );

	hCtl = GetDlgItem ( hACP, S10_ACPMAP );
	ChkInvalid ( hCtl );

	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_ACP::Save
//		Save the data in the fields of the ACP dialog to the
//		registry
//
int cDialogs::_ACP::Save ( )
{
	HWND hDlg;
	HKEY hKey;
	UCHAR sRegVal[_MAX_PATH];

	hDlg = Dialogs.ACP.hACP;
	//	Open the registry key "Software\NeoMedia\WISP\ACP"
	RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_ACP, 0, KEY_ALL_ACCESS, &hKey );

	GetDlgItemText ( hDlg, S10_ACPCONFIGDIR, (char *) sRegVal, _MAX_PATH );
	RegSetValueEx (	hKey, REGVAL_ACP_ACPCONFIG, 0, REG_SZ, sRegVal,
		strlen ((char *) sRegVal )+1);
	GetDlgItemText ( hDlg, S10_ACPMAP, (char *) sRegVal, _MAX_PATH );
	RegSetValueEx (	hKey, REGVAL_ACP_ACPMAP, 0, REG_SZ, sRegVal,
		strlen ((char *) sRegVal )+1);

	RegFlushKey ( hKey );
	RegCloseKey ( hKey );
	return 0;
}

///////////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_ACP::ChkInvalid
//		Validates required fields and if the field contains an
//		invalid entry then it sets the 'iv' flag to false so
//		that the background will be painted yellow
//
void cDialogs::_ACP::ChkInvalid ( HWND hCtl )
{
	WIN32_FIND_DATA ValFile;
	char sTemp[_MAX_PATH],
		sTemp1[_MAX_PATH];
	ZeroMemory ( sTemp1, sizeof ( sTemp1 ));
	ZeroMemory ( sTemp, sizeof ( sTemp ));
	ZeroMemory ( &ValFile, sizeof ( ValFile ));

	GetWindowText ( hCtl, sTemp, _MAX_PATH );
	//	If the item to validate is the ACPCONFIGDIR field
	if ( hCtl == GetDlgItem ( Dialogs.ACP.hACP, S10_ACPCONFIGDIR )) {
		strcat ( sTemp, "\\*.*" );
		//	If the dir is not found, then set the flag to false
		if ( FindFirstFile 
			( sTemp, &ValFile ) == INVALID_HANDLE_VALUE  ) {
			ivACPConfigDir = FALSE;
		}
		//	Otherwise, set the flag to true
		else ivACPConfigDir = TRUE;
	}
	//	If the item to validate is the ACPMAP field (here we are looking for
	//	a file in the directory specified in the ACPCONFIGDIR field so
	//	we must create a search string by putting the path then the
	//	file name into a buffer and searching for that.
	if ( hCtl == GetDlgItem ( Dialogs.ACP.hACP, S10_ACPMAP )) {
		GetDlgItemText ( hACP, S10_ACPCONFIGDIR, sTemp1, _MAX_PATH );
		strcat ( sTemp1, "\\" );
		strcat ( sTemp1, sTemp );
		if ( FindFirstFile ( sTemp1, &ValFile ) == INVALID_HANDLE_VALUE )
			ivACPMap = FALSE;
		else ivACPMap = TRUE;
	}
	//	InvalidateRect tells Windows to repaint the entire window
	InvalidateRect ( hCtl, NULL, FALSE );
	return;
}
