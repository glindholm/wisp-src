//////////////////////////////////////////////////////////////////////////
//
//	This file contains the member functions of the cApplication class
//

#include "#Defines.h"
#include "#Classes.h"
#include "#Externs.h"
#include "#Prototypes.h"

//////////////////////////////////////////////////////////////////////////
//
//	cApplication::HaveRequiredPermissions
//		Verifies that the user has the required permissions to
//		write to the HKEY_LOCAL_MACHINE\\Software key.  Note that
//		on WinNT and Win2000, this requires administrative permissions
//
bool cApplication::HaveRequiredPermissions()
{
	HKEY hKey;
	long rc;
	char *msg;
	char temp[16];
	char *msgList[2];

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
		"Software",
		0, KEY_ALL_ACCESS, &hKey );

	switch(rc)
	{
	case ERROR_SUCCESS:
		RegCloseKey(hKey);
		return(true);

	case ERROR_ACCESS_DENIED:
		(void)MessageBox(NULL,
						 "You must have Administrator Privileges in order to run this application.",
						 "Access Denied",
						 MB_OK | MB_ICONERROR);
		break;

	default:
		if (FormatMessage(	FORMAT_MESSAGE_ALLOCATE_BUFFER |
							FORMAT_MESSAGE_FROM_SYSTEM |
							FORMAT_MESSAGE_IGNORE_INSERTS,
							NULL,
							rc,
							0,
							(char *)&msg,
							1,
							NULL) != 0)
		{
			(void)MessageBox(NULL,
							 (const char *)msg,
							 "Error",
							 MB_OK | MB_ICONERROR);
			LocalFree(msg);
			break;
		}
		sprintf(temp, "%ld", rc);
		msgList[0] = temp;
		msgList[1] = NULL;
		if (FormatMessage(	FORMAT_MESSAGE_ALLOCATE_BUFFER |
							FORMAT_MESSAGE_FROM_STRING |
							FORMAT_MESSAGE_ARGUMENT_ARRAY,
							"Error %1 occurred during permission check.",
							0,
							0,
							(char *)&msg,
							1,
							msgList) != 0)
		{
			(void)MessageBox(NULL,
							 (const char *)msg,
							 "Error",
							 MB_OK | MB_ICONERROR);
			LocalFree(msg);
			break;
		}

		(void)MessageBox(NULL, "Error while checking permissions", "Error", MB_OK | MB_ICONERROR);
		break;
	}

	return(false);
}

//////////////////////////////////////////////////////////////////////////
//
//	cApplication::InitApp
//		Initializes and creates the main application window
//
cApplication::InitApp ( HINSTANCE hInst )
{
	WNDCLASS wc;
	wc.style		=	CS_HREDRAW | CS_VREDRAW;
	wc.lpfnWndProc		=	AppWndProc;
	wc.cbClsExtra		=	0;
	wc.cbWndExtra		=	0;
	wc.hInstance		=	hInst;
	wc.hIcon		=
		LoadIcon ( hInst, MAKEINTRESOURCE ( icoMainIcon ));
	wc.hCursor		=	LoadCursor ( NULL, IDC_ARROW );
	wc.hbrBackground	=	(HBRUSH)GetStockObject ( LTGRAY_BRUSH );
	wc.lpszMenuName		=	NULL;
	wc.lpszClassName	=	"AppWndCls";
	RegisterClass ( &wc );

	cApp.hAppWnd = CreateWindow ( 
		"AppWndCls",
		"WISP Configuration Utility",
		WS_OVERLAPPEDWINDOW,
		0, 0, 640, 480,
		NULL, NULL, hInst, NULL );

	cApp.hInstGlb = hInst;
	//	Get OS info
	OSVERSIONINFO OSVer;
	OSVer.dwOSVersionInfoSize = sizeof ( OSVERSIONINFO );
	GetVersionEx ( &OSVer );
	cApp.Env.OS.dwPlatform = OSVer.dwPlatformId;
	cApp.Env.OS.dwMajorVer = OSVer.dwMajorVersion;
	cApp.Env.OS.dwMinorVer = OSVer.dwMinorVersion;
	//	Call the WISP Configuration Utility dialog box -
	//	which is really the main application interface
	DialogBox ( hInst, MAKEINTRESOURCE ( dlgRegOps ), 
		NULL, (DLGPROC) MainDlgProc );

	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	clsAppManager::MsgLoop
//		Main message loop for the application
//
void cApplication::MsgLoop ( )
{
	MSG msg;														//	Holds the message to be processed
	while (															//	Perform loop as long as there are messages in the app's que
		GetMessage (&msg, NULL, 0, 0)) {							//	Get message from que
		TranslateMessage (&msg);									//	Translate virtual-key messages
		DispatchMessage (&msg);										//	Sends the message to the appropriate window to be handled by that window proc
	}
}

///////////////////////////////////////////////////////////////////////////////
//
// cApplication::DeleteReg
//	Deletes the contents of the registry that were created by the WISP
//	configuration utility.
//
int cApplication::DeleteReg ( )
{
	HKEY hKey;
	long rc;

	/*	Delete all keys and values
		The deleting begins at the deepest subkey within a branch, because
		there is no function that will delete a key that contains subkeys.
		Also, we explicitly delete the values in the subkeys because if a
		key contains a subkey it will not be deleted and the value would
		be left unchanged.
	*/

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP\\VSSUBS\\EXTRACT",
		0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RegDeleteValue ( hKey, "WISPCPU" );
		RegDeleteValue ( hKey, "WISPNETID" );
	}
	
	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP\\VSSUBS\\MESSAGE",
		0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RegDeleteValue ( hKey, "SHAREDIR" );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP\\VSSUBS\\SCRATCH",
		0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RegDeleteValue ( hKey, "WISPSCRATCHMODE" );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP\\VSSUBS",
		0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RegDeleteKey ( hKey, "EXTRACT" );
		RegDeleteKey ( hKey, "MESSAGE" );
		RegDeleteKey ( hKey, "SCRATCH" );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP\\WISPBin\\WISPTran\\COBOL\\ACUCOBOL",
		0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RegDeleteValue ( hKey, "COB" );
		RegDeleteValue ( hKey, "COBFLAGS" );
		RegDeleteValue ( hKey, "OUTDIR" );
		RegDeleteValue ( hKey, "OBJEXT" );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP\\WISPBin\\WISPTran\\COBOL\\MFCOBOL",
		0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RegDeleteValue ( hKey, "COB" );
		RegDeleteValue ( hKey, "COBFLAGS" );
		RegDeleteValue ( hKey, "OUTDIR" );
		RegDeleteValue ( hKey, "OBJEXT" );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP\\WISPBin\\WISPTran\\COBOL",
		0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RegDeleteValue ( hKey, "Language" );
		RegDeleteKey ( hKey, "ACUCOBOL" );
		RegDeleteKey ( hKey, "MFCOBOL" );
		RegCloseKey ( hKey );
	}
	
	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP\\WISPBin\\WISPTran",
		0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RegDeleteValue ( hKey, "HSize" );
		RegDeleteValue ( hKey, "VSize" );
		RegDeleteValue ( hKey, "WorkDir" );
		RegDeleteKey ( hKey, "COBOL" );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP\\WISPBin\\DISPLAY",
		0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RegDeleteValue ( hKey, "WISPDISPLAY8BIT" );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP\\WISPBin\\WPROC",
		0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RegDeleteValue ( hKey, "WPROC" );
		RegDeleteValue ( hKey, "WPROCDEBUG" );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP\\WISPBin",
		0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RegDeleteValue ( hKey, "DISPLAY" );
		RegDeleteValue ( hKey, "WEDITOR" );
		RegDeleteValue ( hKey, "WISP" );
		RegDeleteKey ( hKey, "DISPLAY" );
		RegDeleteKey ( hKey, "WPROC" );
		RegDeleteKey ( hKey, "WISPTran" );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP\\ACP",
		0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RegDeleteValue ( hKey, "ACPCONFIG" );
		RegDeleteValue ( hKey, "ACPMAP" );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP\\License",
		0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RegDeleteValue ( hKey, "FILE" );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP\\VIDEOCAP",
		0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RegDeleteValue ( hKey, "VIDEOCAP" );
		RegDeleteValue ( hKey, "WISPTERM" );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia\\WISP",
		0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RegDeleteValue ( hKey, "PATH" );
		RegDeleteValue ( hKey, "SERVER" );
		RegDeleteValue ( hKey, "TMPDIR" );
		RegDeleteValue ( hKey, "USERSDIR" );
		RegDeleteValue ( hKey, "WISPCONFIG" );
		RegDeleteValue ( hKey, "WISPDIR" );
		RegDeleteValue ( hKey, "WISPSORTMEM" );
		RegDeleteKey ( hKey, "ACP" );
		RegDeleteKey ( hKey, "License" );
		RegDeleteKey ( hKey, "VIDEOCAP" );
		RegDeleteKey ( hKey, "VSSUBS" );
		RegDeleteKey ( hKey, "WISPBin" );
		RegDeleteKey ( hKey, "Versions" );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
		"Software\\NeoMedia",
		0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RegDeleteKey ( hKey, "WISP" );
		RegCloseKey ( hKey );
	}

	return 0;
}

///////////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_MainDialog::ImportRegData
//		Imports a registry file by calling regedit.exe
//
cApplication::ImportRegData ( HWND hDlg )
{
	char sFileName[MaxPath];
	OPENFILENAME ofn;
	ZeroMemory ( sFileName, sizeof ( sFileName ));
	ZeroMemory ( &ofn, sizeof ( OPENFILENAME ));
	ofn.lStructSize		=	sizeof ( OPENFILENAME );
	ofn.Flags		=	OFN_FILEMUSTEXIST | OFN_NOCHANGEDIR |
					OFN_PATHMUSTEXIST;
	ofn.hwndOwner		=	hDlg;
	ofn.hInstance		=	cApp.hInstGlb;
	ofn.lpstrFilter		=	"Registry Files (*.reg)\0*.REG\0"
					"All Files (*.*)\0*.*\0";
	ofn.lpstrFile		=	sFileName;
	ofn.nMaxFile		=	_MAX_PATH;
	ofn.lpstrInitialDir	=	NULL;
	ofn.nFilterIndex	=	1;
	ofn.lpstrTitle		=	"Import Registry Data";

	if ( GetOpenFileName ( &ofn ) == TRUE ) {
		/*	If the user selected a file then we can build a command
			line and create a process that calls regedit.exe to
			import the file */
		char sRegCmdLine[MaxPath];
		STARTUPINFO sInfo;
		PROCESS_INFORMATION pi;
		ZeroMemory ( &sInfo, sizeof ( sInfo ));

		sInfo.cb	=	sizeof (STARTUPINFO);

		sprintf ( sRegCmdLine, "RegEdit %s", sFileName );
		CreateProcess (NULL, sRegCmdLine, NULL,
			NULL, TRUE, REALTIME_PRIORITY_CLASS,
			NULL, NULL, &sInfo, &pi);
		//	Wait for the process to return before continuing execution
		//	Otherwise, WConfig reads the data from the registry before
		//	it has been updated by the new process.
		WaitForSingleObject ( pi.hProcess, INFINITE );
	}
	while ( Dialogs.General.Initialize ( ) == 1 );
	Dialogs.Videocap.Initialize ( );
	Dialogs.EXTRACT.Initialize ( );
	Dialogs.MESSAGE.Initialize ( );
	Dialogs.SCRATCH.Initialize ( );
	Dialogs.License.Initialize ( );
	Dialogs.WISPBin.Initialize ( );
	Dialogs.DISPLAY.Initialize ( );
	Dialogs.WPROC.Initialize ( );
	Dialogs.ACP.Initialize ( );

	return 0;
}

void expandBackslashes(char *out, const char *in, int size)
{
	int ix, ox, len;

	len = strlen(in);
	for (ix = ox = 0; ix < len; ix++, ox++)
	{
		if (ix >= size-1)
		{
			break;
		}
		out[ox] = in[ix];

		// If a backslash then double it

		if ('\\' == in[ix])
		{
			out[++ox] = in[ix];
		}
	}
	out[ox] = '\0';
}

///////////////////////////////////////////////////////////////////////////////
//
//	cApplication::WriteReg
//		Exports the registry entries that pertain to the WConfig
//		utility to a text file that is specified by the user
//
void cApplication::WriteRegFile ( )
{
	HKEY hKey;
	char FileName[_MAX_PATH],	//	Name of the file to export to
		ToWrite[102400],	//	Buffer for text that is waiting to be written
		Line[2048];		//	Buffer for formatted registry values before
					//		they are added to the ToWrite buffer
	HANDLE hFile;			//	Handle of the file that the data is written to
	UCHAR sRegVal[1024];	//	Buffer for values that are retrieved from the registry
	ZeroMemory ( Line, sizeof ( Line ));
	ZeroMemory ( sRegVal, sizeof ( sRegVal ));
	ZeroMemory ( ToWrite, sizeof ( ToWrite ));
	ZeroMemory ( FileName, sizeof ( FileName ));
	DWORD szBuf = _MAX_PATH;

	//	Get file name that the data is to be exported to
	OPENFILENAME ofn;
	ZeroMemory ( &ofn, sizeof ( OPENFILENAME ));
	ofn.lStructSize		=	sizeof ( OPENFILENAME );
	ofn.hwndOwner		=	cApp.hMainDlg;
	ofn.hInstance		=	cApp.hInstGlb;
	ofn.lpstrFilter		=	"Registry Files (*.reg)\0*.reg\0";
	ofn.nFilterIndex	=	1;
	ofn.lpstrFile		=	FileName;
	ofn.nMaxFile		=	_MAX_PATH;
	ofn.lpstrTitle		=	"Export Registry";
	ofn.Flags		=	OFN_NOCHANGEDIR | OFN_OVERWRITEPROMPT;
	ofn.lpstrDefExt		=	"reg";

	if ( GetSaveFileName ( &ofn ) == TRUE ) {
		//	Open/Create the file
		hFile = CreateFile ( FileName,
			GENERIC_WRITE, 0, NULL,
			OPEN_ALWAYS,
			FILE_ATTRIBUTE_ARCHIVE, NULL );

		//	The following ints are used for string position pointers
		int cnt = 0,
			StartStr = 0,
			EndStr = 0,
			TokCnt = 0;
		char sRegVal1[1024];
		ZeroMemory ( sRegVal1, sizeof ( sRegVal1 ));

		//	The text file must have this heading at the top, otherwise
		//	regedit does not interpret it as a registry file
		strcpy ( ToWrite, "REGEDIT4\r\n\r\n" );
		//*************************************************************
		//	Save the values in the WISP key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
			"SOFTWARE\\NeoMedia\\WISP", 0, KEY_READ, &hKey );
		strcat ( ToWrite,
			"\r\n[HKEY_LOCAL_MACHINE\\SOFTWARE"
			"\\NeoMedia\\WISP]\r\n" );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the SERVER entry *****
		RegQueryValueEx ( 
			hKey, "SERVER", 0, NULL, sRegVal, &szBuf );
		sprintf ( Line, "\"SERVER\"=\"%s\"\r\n", (char *) sRegVal );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		RegQueryValueEx (
			hKey, "WISPDIR", 0, NULL, sRegVal, &szBuf );

		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));

		sprintf ( Line, "\"WISPDIR\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the WISPCONFIG entry *****
		RegQueryValueEx (
			hKey, "WISPCONFIG", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"WISPCONFIG\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the PATH entry *****
		RegQueryValueEx (
			hKey, "PATH", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"PATH\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the TMPDIR entry *****
		RegQueryValueEx (
			hKey, "TMPDIR", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"TMPDIR\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the WISPSORTMEM entry *****
		RegQueryValueEx (
			hKey, "WISPSORTMEM", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"WISPSORTMEM\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the USERSDIR entry *****
		RegQueryValueEx (
			hKey, "USERSDIR", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"USERSDIR\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		//*************************************************************
		//	OK, now do the WISP\VIDEOCAP key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
			"SOFTWARE\\NeoMedia\\WISP\\VIDEOCAP",
			0, KEY_READ, &hKey );
		strcat ( ToWrite,
			"\r\n[HKEY_LOCAL_MACHINE\\SOFTWARE\\"
			"NeoMedia\\WISP\\VIDEOCAP]\r\n" );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the WISPTERM entry *****
		RegQueryValueEx (
			hKey, "WISPTERM", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"WISPTERM\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the VIDEOCAP entry *****
		RegQueryValueEx (
			hKey, "VIDEOCAP", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"VIDEOCAP\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		//*************************************************************
		//	Now do the WISP\VSSUBS\EXTRACT key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
			"SOFTWARE\\NeoMedia\\WISP\\VSSUBS\\EXTRACT",
			0, KEY_READ, &hKey );
		strcat ( ToWrite,
			"\r\n[HKEY_LOCAL_MACHINE\\"
			"SOFTWARE\\NeoMedia\\WISP\\VSSUBS]\r\n" );
		strcat ( ToWrite,
			"\r\n[HKEY_LOCAL_MACHINE\\SOFTWARE\\"
			"NeoMedia\\WISP\\VSSUBS\\EXTRACT]\r\n" );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the WISPCPU entry *****
		RegQueryValueEx (
			hKey, "WISPCPU", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"WISPCPU\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the WISPNETID entry *****
		RegQueryValueEx (
			hKey, "WISPNETID", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"WISPNETID\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		//*************************************************************
		//	Now do the WISP\VSSUBS\MESSAGE key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
			"SOFTWARE\\NeoMedia\\WISP\\VSSUBS\\MESSAGE",
			0, KEY_READ, &hKey );
		strcat ( ToWrite,
			"\r\n[HKEY_LOCAL_MACHINE\\SOFTWARE\\"
			"NeoMedia\\WISP\\VSSUBS\\MESSAGE]\r\n" );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the SHAREDIR entry *****
		RegQueryValueEx (
			hKey, "SHAREDIR", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"SHAREDIR\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		//*************************************************************
		//	Now do the WISP\VSSUBS\SCRATCH key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
			"SOFTWARE\\NeoMedia\\WISP\\VSSUBS\\SCRATCH",
			0, KEY_READ, &hKey );
		strcat ( ToWrite, "\r\n[HKEY_LOCAL_MACHINE\\SOFTWARE\\"
			"NeoMedia\\WISP\\VSSUBS\\SCRATCH]\r\n" );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the WISPSCRATCHMODE entry *****
		RegQueryValueEx (
			hKey, "WISPSCRATCHMODE", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"WISPSCRATCHMODE\"=\"%s\"\r\n",
			(char *) sRegVal1 );
		strcat ( ToWrite, Line );

		//*************************************************************
		//	Now do the WISP\License key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
			"SOFTWARE\\NeoMedia\\WISP\\License",
			0, KEY_READ, &hKey );
		strcat ( ToWrite,
			"\r\n[HKEY_LOCAL_MACHINE\\SOFTWARE\\"
			"NeoMedia\\WISP\\License]\r\n" );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the FILE entry *****
		RegQueryValueEx (
			hKey, "FILE", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"FILE\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		//*************************************************************
		//	Now do the WISP\Versions key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
			"SOFTWARE\\NeoMedia\\WISP\\Versions",
			0, KEY_READ, &hKey );
		strcat ( ToWrite, 
			"\r\n[HKEY_LOCAL_MACHINE\\SOFTWARE\\"
			"NeoMedia\\WISP\\Versions]\r\n" );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the WISP entry *****
		RegQueryValueEx (
			hKey, "WISP", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"WISP\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		//*************************************************************
		//	Now do the WISP\WISPBin key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
			"SOFTWARE\\NeoMedia\\WISP\\WISPBin",
			0, KEY_READ, &hKey );
		strcat ( ToWrite,
			"\r\n[HKEY_LOCAL_MACHINE\\SOFTWARE\\"
			"NeoMedia\\WISP\\WISPBin]\r\n" );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the WISP entry *****
		RegQueryValueEx (
			hKey, "WISP", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"WISP\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the WEDITOR entry *****
		RegQueryValueEx (
			hKey, "WEDITOR", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"WEDITOR\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the DISPLAY entry *****
		RegQueryValueEx (
			hKey, "DISPLAY", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"DISPLAY\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		//*************************************************************
		//	Now do the WISP\WISPBin\DISPLAY key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
			"SOFTWARE\\NeoMedia\\WISP\\WISPBin\\DISPLAY",
			0, KEY_READ, &hKey );
		strcat ( ToWrite,
			"\r\n[HKEY_LOCAL_MACHINE\\SOFTWARE\\"
			"NeoMedia\\WISP\\WISPBin\\DISPLAY]\r\n" );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the WISPDISPLAY8BIT entry *****
		RegQueryValueEx (
			hKey, "WISPDISPLAY8BIT", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"WISPDISPLAY8BIT\"=\"%s\"\r\n",
			(char *) sRegVal1 );
		strcat ( ToWrite, Line );

		//*************************************************************
		//	Now do the WISP\WISPBin\WPROC key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
			"SOFTWARE\\NeoMedia\\WISP\\WISPBin\\WPROC",
			0, KEY_READ, &hKey );
		strcat ( ToWrite,
			"\r\n[HKEY_LOCAL_MACHINE\\SOFTWARE\\"
			"NeoMedia\\WISP\\WISPBin\\WPROC]\r\n" );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the WPROC entry *****
		RegQueryValueEx (
			hKey, "WPROC", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"WPROC\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the WPROCDEBUG entry *****
		RegQueryValueEx (
			hKey, "WPROCDEBUG", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"WPROCDEBUG\"=\"%s\"\r\n",
			(char *) sRegVal1 );
		strcat ( ToWrite, Line );

		//*************************************************************
		//	Now do the WISP\WISPBin\WISPTRAN key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
			"SOFTWARE\\NeoMedia\\WISP\\WISPBin\\WISPTRAN",
			0, KEY_READ, &hKey );
		strcat ( ToWrite, "\r\n[HKEY_LOCAL_MACHINE\\SOFTWARE\\"
			"NeoMedia\\WISP\\WISPBin\\WISPTRAN]\r\n" );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the HSize entry *****
		RegQueryValueEx (
			hKey, "HSize", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"HSize\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the VSize entry *****
		RegQueryValueEx (
			hKey, "VSize", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"VSize\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the WorkDir entry *****
		RegQueryValueEx (
			hKey, "WorkDir", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"WorkDir\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		//*************************************************************
		//	Now do the WISP\WISPBin\WISPTRAN\COBOL key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
			"SOFTWARE\\NeoMedia\\WISP\\WISPBin\\WISPTRAN\\COBOL",
			0, KEY_READ, &hKey );
		strcat ( ToWrite,
			"\r\n[HKEY_LOCAL_MACHINE\\SOFTWARE\\NeoMedia\\"
			"WISP\\WISPBin\\WISPTRAN\\COBOL]\r\n" );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the Language entry *****
		RegQueryValueEx (
			hKey, "Language", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"Language\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		//*************************************************************
		//	Now do the WISP\WISPBin\WISPTRAN\COBOL\ACUCOBOL key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
			"SOFTWARE\\NeoMedia\\WISP\\WISPBin"
			"\\WISPTRAN\\COBOL\\ACUCOBOL",
			0, KEY_READ, &hKey );
		strcat ( ToWrite,
			"\r\n[HKEY_LOCAL_MACHINE\\SOFTWARE\\NeoMedia\\"
			"WISP\\WISPBin\\WISPTRAN\\COBOL\\ACUCOBOL]\r\n" );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the COB entry *****
		RegQueryValueEx (
			hKey, "COB", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"COB\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the COBFLAGS entry *****
		RegQueryValueEx (
			hKey, "COBFLAGS", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"COBFLAGS\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the OUTDIR entry *****
		RegQueryValueEx (
			hKey, "OUTDIR", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"OUTDIR\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the OBJEXT entry *****
		RegQueryValueEx (
			hKey, "OBJEXT", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"OBJEXT\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		//*************************************************************
		//	Now do the WISP\WISPBin\WISPTRAN\COBOL\MFCOBOL key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
			"SOFTWARE\\NeoMedia\\WISP\\WISPBin"
			"\\WISPTRAN\\COBOL\\MFCOBOL",
			0, KEY_READ, &hKey );
		strcat ( ToWrite,
			"\r\n[HKEY_LOCAL_MACHINE\\SOFTWARE\\NeoMedia\\"
			"WISP\\WISPBin\\WISPTRAN\\COBOL\\MFCOBOL]\r\n" );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the COB entry *****
		RegQueryValueEx (
			hKey, "COB", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"COB\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the COBFLAGS entry *****
		RegQueryValueEx (
			hKey, "COBFLAGS", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"COBFLAGS\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the OUTDIR entry *****
		RegQueryValueEx (
			hKey, "OUTDIR", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"OUTDIR\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the OBJEXT entry *****
		RegQueryValueEx (
			hKey, "OBJEXT", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"OBJEXT\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		//*************************************************************
		//	Now do the WISP\ACP key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE,
			"SOFTWARE\\NeoMedia\\WISP\\ACP",
			0, KEY_READ, &hKey );
		strcat ( ToWrite,
			"\r\n[HKEY_LOCAL_MACHINE\\SOFTWARE"
			"\\NeoMedia\\WISP\\ACP]\r\n" );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the ACPCONFIG entry *****
		RegQueryValueEx (
			hKey, "ACPCONFIG", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"ACPCONFIG\"=\"%s\"\r\n",
			(char *) sRegVal1 );
		strcat ( ToWrite, Line );

		szBuf = _MAX_PATH;
		ZeroMemory ( sRegVal, sizeof ( sRegVal ));
		//	***** Get the value in the ACPMAP entry *****
		RegQueryValueEx (
			hKey, "ACPMAP", 0, NULL, sRegVal, &szBuf );
		expandBackslashes(sRegVal1, (char *)sRegVal, sizeof(sRegVal1));
		sprintf ( Line, "\"ACPMAP\"=\"%s\"\r\n", (char *) sRegVal1 );
		strcat ( ToWrite, Line );

		RegCloseKey ( hKey );
		DWORD Written = 0;
		int foo = strlen ( ToWrite );
		WriteFile ( hFile, ToWrite,
			strlen ( ToWrite ), &Written, NULL );
		SetEndOfFile ( hFile );
		if ( hFile != NULL ) CloseHandle ( hFile );
	}
	return;
}

