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
//	cDialogs::_SCRATCH::Initialize
//
int cDialogs::_SCRATCH::Initialize ( )
{
	HKEY hKey;
	HWND hDlg = Dialogs.SCRATCH.hSCRATCH;
	DWORD BufSize = _MAX_PATH;
	UCHAR sRegValue[_MAX_PATH];
	//	If unable to open key
	if ( CreateRegistryKey (REGKEY_WISP_VSSUBS_SCRATCH,&hKey ) == ERROR_SUCCESS ) {
		//	Get value for SCRATCHMODE field
		BufSize = _MAX_PATH;
		if ( RegQueryValueEx ( hKey, REGVAL_SCRATCH_WISPSCRATCHMODE, 0, NULL,
			sRegValue, &BufSize ) == ERROR_SUCCESS ) {
			sRegValue[BufSize] = '\0';
			SetDlgItemText (
				Dialogs.SCRATCH.hSCRATCH,
				S05_SCRATCHMODE, (char *) sRegValue );
		}
		else {
			SetDlgItemText (
				Dialogs.SCRATCH.hSCRATCH,
				S05_SCRATCHMODE, "33" );
		}
		RegCloseKey ( hKey );
	}

	HWND hCtl = GetDlgItem ( hSCRATCH, S05_SCRATCHMODE );
	ChkInvalid ( hCtl );

	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_SCRATCH::Save
//		Saves the data in the SCRATCH dialog to the registry
//
int cDialogs::_SCRATCH::Save ( )
{
	HWND hDlg;
	HKEY hKey;
	UCHAR sRegVal[_MAX_PATH];

	hDlg = Dialogs.SCRATCH.hSCRATCH;
	RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_VSSUBS_SCRATCH, 0, KEY_ALL_ACCESS, &hKey );

	GetDlgItemText (
		hDlg, S05_SCRATCHMODE, (char *) sRegVal, _MAX_PATH );
	RegSetValueEx (	hKey, REGVAL_SCRATCH_WISPSCRATCHMODE, 0, REG_SZ,
		sRegVal, strlen ((char *) sRegVal )+1);

	RegFlushKey ( hKey );
	RegCloseKey ( hKey );
	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_SCRATCH::ChkInvalid
//		Validates required fields and if the field contains an
//		invalid entry then it sets the 'iv' flag to false so
//		that the background will be painted yellow
//
void cDialogs::_SCRATCH::ChkInvalid ( HWND hCtl )
{
	WIN32_FIND_DATA ValFile;
	char sTemp[_MAX_PATH];
	ZeroMemory ( sTemp, sizeof ( sTemp ));
	ZeroMemory ( &ValFile, sizeof ( ValFile ));

	GetWindowText ( hCtl, sTemp, _MAX_PATH );
	//	The field must contain "32" or "33"
	if ( hCtl == GetDlgItem
		( Dialogs.SCRATCH.hSCRATCH, S05_SCRATCHMODE )) {
		if ( strcmp ( sTemp, "32" ) != 0 ) {
			if ( strcmp ( sTemp, "33" ) != 0 )
				ivScratchMode = FALSE;
			else ivScratchMode = TRUE;
		}
		else ivScratchMode = TRUE;
	}
	InvalidateRect ( hCtl, NULL, FALSE );
	return;
}
