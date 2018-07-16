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
	if ( 0 == stricmp ( cApp.Env.sWISPServer, "(LOCAL)" ) ) {
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
cDialogs::_License::Initialize ( )
{
	HKEY hKey;
	HWND hDlg = Dialogs.License.hLicense;
	DWORD Disp;
	//	If unable to open key
	if ( RegCreateKeyEx (
		HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP\\License",
		0, NULL, REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS,
		NULL, &hKey, &Disp ) == ERROR_SUCCESS ) 
	{
#ifdef OLD
		DWORD BufSize = _MAX_PATH;
		UCHAR sRegValue[_MAX_PATH];
		//	Get value for LICENSEFILE field
		if ( RegQueryValueEx (
			hKey, "FILE", 0, NULL, sRegValue,
			&BufSize ) == ERROR_SUCCESS ) 
		{
			sRegValue[BufSize] = '\0';
			SetDlgItemText (
				Dialogs.License.hLicense,
				S06_LICENSEFILE, (char *) sRegValue );
		}
#endif /* OLD */
		this->UpdateLicenseFile();
		RegCloseKey ( hKey );
	}

	HWND hCtl = GetDlgItem ( hLicense, S06_LICENSEFILE );
	//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	//	***** CHANGED *****
//	HDC hDC = GetDC ( hCtl );
	//	***** END CHANGED *****
	ChkInvalid ( hCtl );

	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_License::Save
//		Saves the data in the License dialog to the registry
//
cDialogs::_License::Save ( )
{
	HWND hDlg;
	HKEY hKey;
	char sRegVal[_MAX_PATH];

	hDlg = Dialogs.License.hLicense;
	RegOpenKeyEx (
		HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP\\License",
		0, KEY_ALL_ACCESS, &hKey );
#ifdef OLD
	GetDlgItemText (
		hDlg, S06_LICENSEFILE,
		sRegVal, _MAX_PATH );
#endif

	this->CalcLicenseFile(sRegVal);

	RegSetValueEx (
		hKey, "FILE", 0, REG_SZ,
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
