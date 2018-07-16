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
//	cDialogs::_EXTRACT::Initialize
//		Initializes the EXTRACT option sheet
//
cDialogs::_EXTRACT::Initialize ( )
{
	HKEY hKey;
	HWND  hDlg = Dialogs.EXTRACT.hEXTRACT;
	DWORD BufSize = _MAX_PATH, Disp;
	UCHAR sRegValue[_MAX_PATH];
	//	Attempt to open the registry key, we are using RegCreateKeyEx
	//	so that if the key doesn't exist then it will be created at
	//	this time.
	if ( RegCreateKeyEx (
		HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP\\VSSUBS\\EXTRACT",
		0, NULL, REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, 
		NULL, &hKey, &Disp ) == ERROR_SUCCESS ) {

		BufSize = _MAX_PATH;
		//	Get the WISPCPU value from the registry
		if ( RegQueryValueEx (
			hKey, "WISPCPU", 0, NULL,
			sRegValue, &BufSize ) == ERROR_SUCCESS ) {
			sRegValue[BufSize] = '\0';
			SetDlgItemText (
				Dialogs.EXTRACT.hEXTRACT,
				S03_CPUID, (char *) sRegValue );
		}
		//	If unable then set the default
		else {
			SetDlgItemText ( 
				Dialogs.EXTRACT.hEXTRACT, S03_CPUID, "    " );
		}

		BufSize = _MAX_PATH;
		//	Get the WISPNETID value from the registry
		if ( RegQueryValueEx (
			hKey, "WISPNETID", 0, NULL,
			sRegValue, &BufSize ) == ERROR_SUCCESS ) {
			sRegValue[BufSize] = '\0';
			SetDlgItemText (
				Dialogs.EXTRACT.hEXTRACT, 
				S03_NETID, (char *) sRegValue );
		}
		//	If unable then set the default
		else {
			SetDlgItemText (
				Dialogs.EXTRACT.hEXTRACT,
				S03_NETID, "        " );
		}
		RegCloseKey ( hKey );
	}
	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_EXTRACT::Save
//		Save the data from the dialog to the registry
//
cDialogs::_EXTRACT::Save ( )
{
	HWND hDlg;
	HKEY hKey;
	UCHAR sRegVal[_MAX_PATH];

	hDlg = Dialogs.EXTRACT.hEXTRACT;
	RegOpenKeyEx (
		HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP\\VSSUBS\\EXTRACT",
		0, KEY_ALL_ACCESS, &hKey );

	GetDlgItemText (
		hDlg, S03_CPUID, (char *) sRegVal, _MAX_PATH );
	RegSetValueEx (
		hKey, "WISPCPU", 0, REG_SZ, sRegVal,
		strlen ((char *) sRegVal )+1);
	GetDlgItemText (
		hDlg, S03_NETID, (char *) sRegVal, _MAX_PATH );
	RegSetValueEx (
		hKey, "WISPNETID", 0, REG_SZ,
		sRegVal, strlen ((char *) sRegVal )+1);

	RegFlushKey ( hKey );
	RegCloseKey ( hKey );
	return 0;
}

