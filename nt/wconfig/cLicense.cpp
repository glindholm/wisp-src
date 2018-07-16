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
//	cDialogs::_License::CalcLicenseFile
//		Calculate the license file location
//
void cDialogs::_License::CalcLicenseFile (char* sDefVal )
{
	if ( 0 == _stricmp ( cApp.Env.sWISPServer, "(LOCAL)" ) ) {
		strcpy( sDefVal, cApp.Env.sWISPDir );
	}
	else {
		sprintf ( sDefVal, "\\\\%s\\wisp", cApp.Env.sWISPServer );
	}
	strcat(sDefVal,"\\license.txt");
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_License::UpdateLicenseFile
//		Update the license file location
//
void cDialogs::_License::UpdateLicenseFile ( )
{
	char sDefVal[_MAX_PATH];

	this->CalcLicenseFile(sDefVal);

	SetDlgItemText (
		Dialogs.License.hLicense,
		S06_LICENSEFILE, sDefVal );
}


//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_License::Initialize
//		Initializes the License dialog
//
int cDialogs::_License::Initialize ( )
{
	HKEY hKey;
	HWND hDlg = Dialogs.License.hLicense;

	//	If unable to open key
	if ( CreateRegistryKey ( REGKEY_WISP_LICENSE, &hKey ) == ERROR_SUCCESS ) 
	{
		this->UpdateLicenseFile();
		RegCloseKey ( hKey );
	}

	HWND hCtl = GetDlgItem ( hLicense, S06_LICENSEFILE );
	ChkInvalid ( hCtl );

	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_License::Save
//		Saves the data in the License dialog to the registry
//
int cDialogs::_License::Save ( )
{
	HWND hDlg;
	HKEY hKey;
	char sRegVal[_MAX_PATH];

	hDlg = Dialogs.License.hLicense;
	RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_LICENSE, 0, KEY_ALL_ACCESS, &hKey );

	this->CalcLicenseFile(sRegVal);

	RegSetValueEx (	hKey, REGVAL_LICENSE_FILE, 0, REG_SZ,
		(unsigned char*)sRegVal, strlen (sRegVal )+1);

	RegFlushKey ( hKey );
	RegCloseKey ( hKey );

	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_License::ChkInvalid
//		Validates required fields and if the field contains an
//		invalid entry then it sets the 'iv' flag to false so
//		that the background will be painted yellow
//
void cDialogs::_License::ChkInvalid ( HWND hCtl )
{
	WIN32_FIND_DATA ValFile;
	char sTemp[_MAX_PATH];
	ZeroMemory ( sTemp, sizeof ( sTemp ));
	ZeroMemory ( &ValFile, sizeof ( ValFile ));

//	GetWindowText ( hCtl, sTemp, _MAX_PATH );
	this->CalcLicenseFile(sTemp);
	if ( hCtl == GetDlgItem( Dialogs.License.hLicense, S06_LICENSEFILE )) 
	{
		if ( FindFirstFile( sTemp, &ValFile ) == INVALID_HANDLE_VALUE ) 
		{
			ivLicenseFile = FALSE;
		}
		else ivLicenseFile = TRUE;
	}
	InvalidateRect ( hCtl, NULL, FALSE );
	return;
}
