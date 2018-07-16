//////////////////////////////////////////////////////////////////////////
//
//	This file contains the member function definitions for the 
//		cDialogs::_General class
//
#include "#Defines.h"
#include "#Classes.h"
#include "#Externs.h"
#include "#Prototypes.h"
#include "resource.h"

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_General::HiLiteInvalid
//		Validates required fields and if the field contains an
//		invalid entry then it sets the 'iv' flag to false so
//		that the background will be painted yellow
//
void cDialogs::_General::ChkInvalid ( HWND hCtl )
{
	WIN32_FIND_DATA ValFile;
	char sTemp[_MAX_PATH];
	ZeroMemory ( sTemp, sizeof ( sTemp ));
	ZeroMemory ( &ValFile, sizeof ( ValFile ));

	GetWindowText ( hCtl, sTemp, _MAX_PATH );

	if ( hCtl == GetDlgItem ( Dialogs.General.hGeneral, S01_SERVER )) 
	{
		// Save the SERVER name in the env so it is available for license
		strcpy(cApp.Env.sWISPServer, sTemp);
	}

	if ( hCtl == GetDlgItem ( Dialogs.General.hGeneral, S01_WISPDIR )) {
		// Save the WISPDIR name in the env so it is available for license
		strcpy(cApp.Env.sWISPDir, sTemp);

		strcat ( sTemp, "\\*.*" );
		if ( FindFirstFile
			( sTemp, &ValFile ) == INVALID_HANDLE_VALUE  ) {
			ivWISPDir = FALSE;
		}
		else ivWISPDir = TRUE;
	}

	if ( hCtl == GetDlgItem ( Dialogs.General.hGeneral, S01_WISPCONFIG )) {
		strcat ( sTemp, "\\*.*" );
		if ( FindFirstFile
			( sTemp, &ValFile ) == INVALID_HANDLE_VALUE  ) {
			ivWISPConfig = FALSE;
		}
		else ivWISPConfig = TRUE;
	}

	if ( hCtl == GetDlgItem ( Dialogs.General.hGeneral, S01_USERDIR )) {
		strcat ( sTemp, "\\*.*" );
		if ( FindFirstFile
			( sTemp, &ValFile ) == INVALID_HANDLE_VALUE  ) {
			ivUserDir = FALSE;
		}
		else ivUserDir = TRUE;
	}

	if ( hCtl == GetDlgItem ( Dialogs.General.hGeneral, S01_TEMPDIR )) {
		strcat ( sTemp, "\\*.*" );
		if ( FindFirstFile
			( sTemp, &ValFile ) == INVALID_HANDLE_VALUE  ) {
			ivTempDir = FALSE;
		}
		else ivTempDir = TRUE;
	}
	InvalidateRect ( hCtl, NULL, FALSE );
	return;
}
//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_General::Initialize
//		Initializes the General dialog
//
cDialogs::_General::Initialize ( )
{
	HKEY hKey;
	HWND hDlg = Dialogs.General.hGeneral;
	DWORD BufSize = _MAX_PATH, Disp;
	UCHAR sRegValue[_MAX_PATH];
	BOOL InvReg = FALSE;
	ZeroMemory ( sRegValue, sizeof ( sRegValue ));

	//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	//	***** CHANGED *****
	//	Create the semaphore for the validation thread
//	TextColorSemaphor = CreateSemaphore ( NULL, 1, 1, "ValidationSmphr" );
	//	Open the WISP key, if doesn't exist then create it
	//	***** END CHANGED *****
	RegCreateKeyEx (
		HKEY_LOCAL_MACHINE, "Software\\NeoMedia\\WISP",
		0, NULL, REG_OPTION_NON_VOLATILE,
		KEY_ALL_ACCESS, NULL, &hKey, &Disp );

	//	The return value of the three functions called next specify
	//	whether they completed successfully or not
	switch ( InitServer ( hKey )) {
		case 2: 
			return 2;
			break;
		case 1:
			return 0;
			break;
	}
	if ( InitWISPDir ( hKey ) == 2 ) return 2;
	if ( InitWISPConf ( hKey ) == 2 ) return 2;
	BufSize = _MAX_FNAME;


	BufSize = _MAX_PATH;
	//	Get value for USERDIR field
	if ( RegQueryValueEx ( hKey, "USERSDIR", 0, NULL,
		sRegValue, &BufSize ) == ERROR_SUCCESS ) {

		sRegValue[BufSize] = '\0';
		SetDlgItemText (
			Dialogs.General.hGeneral,
			S01_USERDIR, (char *) sRegValue );
	}
	else {
		char sDefVal[_MAX_PATH];
		if ( '\0' == cApp.Env.sWISPServer[0] ||
		     ' '  == cApp.Env.sWISPServer[0] ||
		     0 == strcmp(cApp.Env.sWISPServer,"(LOCAL)"))
		{
			strcpy( sDefVal, "C:\\users" );
		}
		else
		{
			sprintf ( sDefVal, "\\\\%s\\users", cApp.Env.sWISPServer );
		}
		SetDlgItemText (
			Dialogs.General.hGeneral, S01_USERDIR, sDefVal );
	}
	//	Get value for PATH field
	BufSize = _MAX_PATH;
	if ( RegQueryValueEx ( hKey, "PATH", 0, NULL,
		sRegValue, &BufSize ) == ERROR_SUCCESS ) {

		sRegValue[BufSize] = '\0';
		SetDlgItemText (
			Dialogs.General.hGeneral,
			S01_PATH, (char *) sRegValue );
	}
	else {
		char sDefVal[_MAX_PATH];
		sprintf ( sDefVal, "%s\\bin", cApp.Env.sWISPDir );
		SetDlgItemText (
			Dialogs.General.hGeneral, S01_PATH, sDefVal );
	}
	//	Get value for TEMPDIR field
	BufSize = _MAX_PATH;
	if ( RegQueryValueEx ( hKey, "TMPDIR", 0, NULL,
		sRegValue, &BufSize ) == ERROR_SUCCESS ) {

		sRegValue[BufSize] = '\0';
		SetDlgItemText (
			Dialogs.General.hGeneral,
			S01_TEMPDIR, (char *) sRegValue );
	}
	else {
		SetDlgItemText (
			Dialogs.General.hGeneral, S01_TEMPDIR, "C:\\temp" );
	}
	//	Get value for WISPSORTMEM field
	BufSize = _MAX_PATH;
	if ( RegQueryValueEx ( hKey, "WISPSORTMEM", 0, NULL,
		sRegValue, &BufSize ) == ERROR_SUCCESS ) {

		sRegValue[BufSize] = '\0';
		SetDlgItemText (
			Dialogs.General.hGeneral,
			S01_WISPSORTMEM, (char *) sRegValue );
	}
	else {
		SetDlgItemText (
			Dialogs.General.hGeneral, S01_WISPSORTMEM, "512" );
	}
	if ( hKey != NULL ) 
		RegCloseKey ( hKey );

	//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	//	***** CHANGED *****
	//	Removed HDC assignments because they aren't being used, they
	//	are left over from a previous thing
	HWND hCtl = GetDlgItem ( hGeneral, S01_WISPDIR );
//	HDC hDC = GetDC ( hCtl );
	ChkInvalid ( hCtl );

	hCtl = GetDlgItem ( hGeneral, S01_WISPCONFIG );
//	hDC = GetDC ( hCtl );
	ChkInvalid ( hCtl );

	hCtl = GetDlgItem ( hGeneral, S01_USERDIR );
//	hDC = GetDC ( hCtl );
	ChkInvalid ( hCtl );

	hCtl = GetDlgItem ( hGeneral, S01_TEMPDIR );
//	hDC = GetDC ( hCtl );
	ChkInvalid ( hCtl );
	//	***** END CHANGED *****

	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_General::InitServer
//		Initializes the Server field of the General dialog
//
cDialogs::_General::InitServer ( HKEY hKey )
{
	DWORD BufSize = _MAX_PATH;
	UCHAR sRegValue[_MAX_PATH] = "";

	if (ERROR_SUCCESS == RegQueryValueEx ( 
		hKey, "SERVER", 0, NULL, sRegValue, &BufSize ))
	{
		sRegValue[BufSize] = '\0';
	}
	BOOL tmpBool = TRUE;
	if ( strlen ( (char *) sRegValue ) == 0 ) {
		while ( tmpBool ) {
			//	Ask the user if he wants to import a reg file
			switch ( MessageBox ( cApp.hMainDlg,
				"WISP is not configured on this machine.  Would you "
				"like to\nimport configuration information from a "
				"saved registry file?", 
				"WISP Configuration Utility - Not configured",
				MB_YESNOCANCEL | MB_ICONEXCLAMATION | MB_DEFBUTTON2 ) ) {
				//	If yes, then...
				case IDYES:
					cApp.ImportRegData ( 
						Dialogs.General.hGeneral );
					GetDlgItemText ( 
						Dialogs.General.hGeneral, S01_SERVER,
						cApp.Env.sWISPServer, 32 );
					tmpBool = FALSE;
					return 1;
					break;
				//	If cancel, then...
				case IDCANCEL:
					if ( MessageBox ( cApp.hMainDlg,
						"If you cancel now WISP will still not be "
						"configured.  Are you sure you want to "
						"quit the WISP configuration utility?",
						"Quit WISP Configuration Utility",
						MB_YESNO ) == IDYES ) {
						PostQuitMessage ( 0 );
						return 0;
					}
					else continue;
					break;
				//	If no, then...
				case IDNO:
					if ( DialogBox ( 
						cApp.hInstGlb, MAKEINTRESOURCE 
						(dlgWISPServer), cApp.hMainDlg,
						(DLGPROC) WISPSrvDlgProc ) == 1 ) return 2;
					GetDlgItemText (
						Dialogs.General.hGeneral, S01_SERVER,
						cApp.Env.sWISPServer, _MAX_PATH );
					tmpBool = FALSE;
					break;
			}
		}
	}
	//	otherwise, put the server name into the server field and the env variable
	else {

		SetDlgItemText (
			Dialogs.General.hGeneral,
			S01_SERVER, (char *) sRegValue );
		strcpy ( cApp.Env.sWISPServer, (char *) sRegValue );
	}
	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_General::InitWISPConf
//		Initializes the WISPConfigDir field of the General dialog
//
cDialogs::_General::InitWISPConf ( HKEY hKey )
{
	DWORD BufSize = _MAX_PATH;
	UCHAR sRegValue[_MAX_PATH] = "";

	if (ERROR_SUCCESS == RegQueryValueEx ( 
		hKey, "WISPCONFIG", 0, NULL, sRegValue, &BufSize ))
	{
		sRegValue[BufSize] = '\0';
	}
	if ( strlen ( (char *) sRegValue ) == 0 ) {
		if ( DialogBox ( 
			cApp.hInstGlb, MAKEINTRESOURCE 
			(dlgWISPConfig), cApp.hMainDlg,
			(DLGPROC) WISPConfigDlgProc ) == 1 ) return 2;
		GetDlgItemText (
			Dialogs.General.hGeneral, S01_WISPCONFIG,
			cApp.Env.sWISPConfig, _MAX_PATH );
	}
	else {
		SetDlgItemText (
			Dialogs.General.hGeneral,
			S01_WISPCONFIG, (char *) sRegValue );
		strcpy ( cApp.Env.sWISPConfig, (char *) sRegValue );
	}
	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_General::InitWISPDir
//		Initializes the WISPDir field of the General dialog
//
cDialogs::_General::InitWISPDir ( HKEY hKey )
{
	DWORD BufSize = _MAX_PATH;
	UCHAR sRegValue[_MAX_PATH] = "";

	if (ERROR_SUCCESS == RegQueryValueEx ( 
		hKey, "WISPDIR", 0, NULL, sRegValue, &BufSize ))
	{
		sRegValue[BufSize] = '\0';
	}
	if ( strlen ( (char *) sRegValue ) == 0 ){
		if ( DialogBox (
			cApp.hInstGlb, MAKEINTRESOURCE 
			(dlgWISPDir), cApp.hMainDlg, 
			(DLGPROC) WISPDirDlgProc ) == 1 ) return 2;
		GetDlgItemText (
			Dialogs.General.hGeneral, S01_WISPDIR,
			cApp.Env.sWISPDir, _MAX_PATH );
	}
	else {
		SetDlgItemText (
			Dialogs.General.hGeneral,
			S01_WISPDIR, (char *) sRegValue );
		strcpy ( cApp.Env.sWISPDir, (char *) sRegValue );
	}
	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_General::Save
//		Save the data in the General dialog to the registry
//
cDialogs::_General::Save ( )
{
	HWND hDlg;
	HKEY hKey;
	UCHAR sRegVal[_MAX_PATH];

	hDlg = Dialogs.General.hGeneral;

	RegOpenKeyEx (
		HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP",
		0, KEY_ALL_ACCESS, &hKey );
	
	GetDlgItemText (
		hDlg, S01_SERVER, (char *) sRegVal, _MAX_PATH );
	RegSetValueEx (
		hKey, "SERVER", 0, REG_SZ, sRegVal,
		strlen ((char *) sRegVal )+1);

	GetDlgItemText (
		hDlg, S01_WISPDIR, (char *) sRegVal, _MAX_PATH );
	RegSetValueEx (
		hKey, "WISPDIR", 0, REG_SZ, sRegVal,
		strlen ((char *) sRegVal )+1);

	GetDlgItemText (
		hDlg, S01_WISPCONFIG, (char *) sRegVal, _MAX_PATH );
	RegSetValueEx (
		hKey, "WISPCONFIG", 0, REG_SZ, sRegVal,
		strlen ((char *) sRegVal )+1);

	GetDlgItemText (
		hDlg, S01_USERDIR, (char *) sRegVal, _MAX_PATH );
	RegSetValueEx (
		hKey, "USERSDIR", 0, REG_SZ, sRegVal,
		strlen ((char *) sRegVal )+1);

	GetDlgItemText (
		hDlg, S01_PATH, (char *) sRegVal, _MAX_PATH );
	RegSetValueEx (
		hKey, "PATH", 0, REG_SZ, sRegVal,
		strlen ((char *) sRegVal )+1);

	GetDlgItemText (
		hDlg, S01_TEMPDIR, (char *) sRegVal, _MAX_PATH );
	RegSetValueEx (
		hKey, "TMPDIR", 0, REG_SZ, sRegVal,
		strlen ((char *) sRegVal )+1);

	GetDlgItemText (
		hDlg, S01_WISPSORTMEM, (char *) sRegVal, _MAX_PATH );
	RegSetValueEx (
		hKey, "WISPSORTMEM", 0, REG_SZ, sRegVal,
		strlen ((char *) sRegVal )+1);

	RegFlushKey ( hKey );
	RegCloseKey ( hKey );
	return 0;
}
