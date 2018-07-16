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

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,	"Software", 0, KEY_ALL_ACCESS, &hKey );

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
int cApplication::InitApp ( HINSTANCE hInst )
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

void DisplayErrorMessageBox(long rc, char* title)
{
	LPVOID lpMsgBuf;
	FormatMessage( 
		FORMAT_MESSAGE_ALLOCATE_BUFFER | 
		FORMAT_MESSAGE_FROM_SYSTEM | 
		FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,
		rc,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
		(LPTSTR) &lpMsgBuf,
		0,
		NULL );

	// Display the string.
	MessageBox( NULL, (LPCTSTR)lpMsgBuf, title, MB_OK | MB_ICONINFORMATION );

	// Free the buffer.
	LocalFree( lpMsgBuf );
}

long CreateRegistryKey(char* regKeyName, PHKEY phKey)
{
	long rc;
	DWORD Disp;

	rc = RegCreateKeyEx ( HKEY_LOCAL_MACHINE, regKeyName,
		0, NULL, REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS,
		NULL, phKey, &Disp );

	if (ERROR_SUCCESS != rc)
	{
		DisplayErrorMessageBox(rc, "RegCreateKeyEx()");
	}

	return rc;
}
void RemoveRegistryValue(HKEY hKey, char* regValueName)
{
	long rc;
	UCHAR empty[1];
	empty[0] = '\0';

	// First set value to null since on Vista the Delete does not work.
	rc = RegSetValueEx (	hKey, regValueName, 0, REG_SZ, empty, 1);
	if (ERROR_SUCCESS != rc)
	{
		DisplayErrorMessageBox(rc, "RegSetValueEx()");
	}
	rc = RegFlushKey ( hKey );

	// Now try and delete
	rc = RegDeleteValue ( hKey, regValueName );
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

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_VSSUBS_EXTRACT, 0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RemoveRegistryValue ( hKey, REGVAL_EXTRACT_WISPCPU );
		RemoveRegistryValue ( hKey, REGVAL_EXTRACT_WISPNETID );

		RegCloseKey ( hKey );
	}
	
	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_VSSUBS_MESSAGE, 0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RemoveRegistryValue ( hKey, REGVAL_MESSAGE_SHAREDIR );

		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_VSSUBS_SCRATCH, 0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RemoveRegistryValue ( hKey, REGVAL_SCRATCH_WISPSCRATCHMODE );

		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_VSSUBS, 0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RegDeleteKey ( hKey, "EXTRACT" );
		RegDeleteKey ( hKey, "MESSAGE" );
		RegDeleteKey ( hKey, "SCRATCH" );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_WISPBIN_WISPTRAN_COBOL_ACU, 0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RemoveRegistryValue ( hKey, REGVAL_COBOL_COB );
		RemoveRegistryValue ( hKey, REGVAL_COBOL_COBFLAGS );
		RemoveRegistryValue ( hKey, REGVAL_COBOL_OUTDIR );
		RemoveRegistryValue ( hKey, REGVAL_COBOL_OBJEXT );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_WISPBIN_WISPTRAN_COBOL_MF, 0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RemoveRegistryValue ( hKey, REGVAL_COBOL_COB );
		RemoveRegistryValue ( hKey, REGVAL_COBOL_COBFLAGS );
		RemoveRegistryValue ( hKey, REGVAL_COBOL_OUTDIR );
		RemoveRegistryValue ( hKey, REGVAL_COBOL_OBJEXT );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_WISPBIN_WISPTRAN_COBOL, 0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RemoveRegistryValue ( hKey, REGVAL_COBOL_LANGUAGE );
		RegDeleteKey ( hKey, "ACUCOBOL" );
		RegDeleteKey ( hKey, "MFCOBOL" );
		RegCloseKey ( hKey );
	}
	
	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_WISPBIN_WISPTRAN, 0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RemoveRegistryValue ( hKey, REGVAL_WISPTRAN_HSIZE );
		RemoveRegistryValue ( hKey, REGVAL_WISPTRAN_VSIZE );
		RemoveRegistryValue ( hKey, REGVAL_WISPTRAN_WORKDIR );

		RegDeleteKey ( hKey, "COBOL" );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_WISPBIN_DISPLAY, 0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RemoveRegistryValue ( hKey, REGVAL_DISPLAY_WISPDISPLAY8BIT );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_WISPBIN_WPROC, 0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RemoveRegistryValue ( hKey, REGVAL_WPROC_WPROC );
		RemoveRegistryValue ( hKey, REGVAL_WPROC_WPROCDEBUG );

		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_WISPBIN, 0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RemoveRegistryValue ( hKey, REGVAL_WISPBIN_DISPLAY );
		RemoveRegistryValue ( hKey, REGVAL_WISPBIN_WEDITOR );
		RemoveRegistryValue ( hKey, REGVAL_WISPBIN_WISP );

		RegDeleteKey ( hKey, "DISPLAY" );
		RegDeleteKey ( hKey, "WPROC" );
		RegDeleteKey ( hKey, "WISPTRAN" );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_ACP, 0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RemoveRegistryValue ( hKey, REGVAL_ACP_ACPCONFIG );
		RemoveRegistryValue ( hKey, REGVAL_ACP_ACPMAP );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_LICENSE, 0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RemoveRegistryValue ( hKey, REGVAL_LICENSE_FILE );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_VERSIONS, 0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RemoveRegistryValue ( hKey, REGVAL_VERSIONS_WISP );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,	REGKEY_WISP_VIDEOCAP, 0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RemoveRegistryValue ( hKey, REGVAL_VIDEOCAP_VIDEOCAP );
		RemoveRegistryValue ( hKey, REGVAL_VIDEOCAP_WISPTERM );

		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,	REGKEY_WISP, 0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		RemoveRegistryValue ( hKey, REGVAL_WISP_PATH );
		RemoveRegistryValue ( hKey, REGVAL_WISP_SERVER );
		RemoveRegistryValue ( hKey, REGVAL_WISP_TMPDIR );
		RemoveRegistryValue ( hKey, REGVAL_WISP_USERSDIR );
		RemoveRegistryValue ( hKey, REGVAL_WISP_WISPCONFIG );
		RemoveRegistryValue ( hKey, REGVAL_WISP_WISPDIR );
		RemoveRegistryValue ( hKey, REGVAL_WISP_WISPSORTMEM );

		RegDeleteKey ( hKey, "ACP" );
		RegDeleteKey ( hKey, "License" );
		RegDeleteKey ( hKey, "VIDEOCAP" );
		RegDeleteKey ( hKey, "VSSUBS" );
		RegDeleteKey ( hKey, "WISPBin" );
		RegDeleteKey ( hKey, "Versions" );
		RegCloseKey ( hKey );
	}

	rc = RegOpenKeyEx ( HKEY_LOCAL_MACHINE,	REGKEY_NEOMEDIA_BASE, 0, KEY_ALL_ACCESS, &hKey );
	if (ERROR_SUCCESS == rc)
	{
		rc = RegDeleteKey ( hKey, "WISP" );
		RegCloseKey ( hKey );
	}
	

	return 0;
}
/*
REGEDIT4


[HKEY_LOCAL_MACHINE\SOFTWARE\NeoMedia\WISP]
"SERVER"="MY-SERVER"
"WISPDIR"="\\\\MY-SERVER\\wisp"
"WISPCONFIG"="\\\\MY-SERVER\\wisp\\config"
"PATH"="\\\\MY-SERVER\\wisp\\bin;\\\\MY-SERVER\\wisp\\acu;\\\\MY-SERVER\\volumes\\volrun\\LIB001;\\\\MY-SERVER\\KCSI"
"TMPDIR"="C:\\temp"
"WISPSORTMEM"=" "
"USERSDIR"="\\\\MY-SERVER\\users"

[HKEY_LOCAL_MACHINE\SOFTWARE\NeoMedia\WISP\VIDEOCAP]
"WISPTERM"="wincon"
"VIDEOCAP"="\\\\MY-SERVER\\wisp\\config\\videocap"

*/
#define REG_FILE_KEY_PREFIX "[HKEY_LOCAL_MACHINE\\"
static int ImportRegistryFile(const char* fileName)
{
	FILE*	the_file;
	char	inlin[2048];
	char	regkey[2048];
	char*	ptr;
	HKEY	hKey = NULL;

	the_file = fopen(fileName,"r");	
	if (the_file == NULL)
	{
		return 1; // Open failed
	}

	regkey[0] = '\0';
	while (fgets(inlin,sizeof(inlin),the_file))
	{
		if ('\0' == regkey[0])
		{
			// Looking for a registry key
			if ('[' != inlin[0])
			{
				continue; // Skip line until a registry key is found
			}
		}

		if ('[' == inlin[0]) // Found a registry key
		{
			if (hKey != NULL)
			{
				RegCloseKey ( hKey );
				hKey = NULL;
			}

			// Ensure it starts "[HKEY_LOCAL_MACHINE\"
			if (_strnicmp(inlin, REG_FILE_KEY_PREFIX, strlen(REG_FILE_KEY_PREFIX)) != 0)
			{
				return 2;  // bad key - abort
			}

			strcpy(regkey,&inlin[strlen(REG_FILE_KEY_PREFIX)]);
			ptr = strchr(regkey,']');
			if (NULL == ptr)
			{
				return 2;  // misformed key - abort
			}
			*ptr = '\0';

			// Ensure it's for WISP; all start with "SOFTWARE\\NeoMedia\\WISP"
			if (_strnicmp(regkey, REGKEY_WISP, strlen(REGKEY_WISP)) != 0)
			{
				return 2;  // bad key - abort
			}

			// Add the Key
			if (0 != CreateRegistryKey(regkey, &hKey))
			{
				return 3; // Create failed
			}
		}
		else if ('\"' == inlin[0]) // Found a Registry Value
		{
			// "NAME"="VALUE"
			char* name;
			char* value;
			UCHAR  valueBuf[2048];

			ptr = strchr(&inlin[1],'\"'); // find end of NAME
			if (NULL == ptr)
			{
				return 4; // misformed value
			}
			*ptr = '\0';

			name = &inlin[1];
			ptr++;
			if ('=' != *ptr)
			{
				return 4;
			}
			ptr++;
			if ('\"' != *ptr)
			{
				return 4;
			}
			ptr++;
			value = ptr;
			ptr = strrchr(value,'\"'); // find end of VALUE
			if (NULL == ptr)
			{
				return 4; // misformed value
			}
			*ptr = '\0';

			// Un-expand double backslashes
			int xi, xo, len;
			len = strlen(value);
			for(xi = xo = 0; xi < len; xi++, xo++)
			{
				valueBuf[xo] = value[xi];
				if ('\\' == value[xi] && '\\' == value[xi+1])
				{
					xi++;
				}
			}
			valueBuf[xo] = '\0';

			if (0 != RegSetValueEx (hKey, name, 0, REG_SZ, valueBuf, strlen ((char *) valueBuf )+1))
			{
				return 5; // Set failed
			}

		}

	}

	if (hKey != NULL)
	{
		RegCloseKey ( hKey );
		hKey = NULL;
	}

	fclose(the_file);

	return 0;
	
}

///////////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_MainDialog::ImportRegData
//		Imports a registry file by calling regedit.exe
//
int cApplication::ImportRegData ( HWND hDlg )
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

	if ( GetOpenFileName ( &ofn ) == TRUE ) 
	{
		long rc;
		rc = ImportRegistryFile(sFileName);
		if (rc != 0)
		{
			char mess[1024];
			sprintf(mess,"Import failed with status code=%d",rc);
			MessageBox( NULL, (LPCTSTR)mess, "Import Failed", MB_OK | MB_ICONINFORMATION );
		}
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

static void appendRegkey(char* buf, char* regKey)
{
	strcat( buf, "\r\n[HKEY_LOCAL_MACHINE\\" );
	strcat( buf, regKey );
	strcat( buf, "]\r\n" );
}

static void appendRegValue(char* buf, char* regValueName, HKEY hKey)
{
	DWORD szBuf;
	UCHAR sRegVal[2048];		//	Buffer for values that are retrieved from the registry
	char  Line[2048];		//	Buffer for formatted registry values before
					//		they are added to the ToWrite buffer
	char sRegValExpanded[2048];

	szBuf = sizeof ( sRegVal );
	ZeroMemory ( sRegVal, sizeof ( sRegVal ));

	RegQueryValueEx ( hKey, regValueName, 0, NULL, sRegVal, &szBuf );
	expandBackslashes(sRegValExpanded, (char *)sRegVal, sizeof(sRegValExpanded));

	sprintf ( Line, "\"%s\"=\"%s\"\r\n", regValueName, (char *) sRegValExpanded );
	strcat ( buf, Line );
}

///////////////////////////////////////////////////////////////////////////////
//
//	cApplication::WriteReg
//		Exports the registry entries that pertain to the WConfig
//		utility to a text file that is specified by the user
//
void cApplication::WriteRegFile ( )
{
	HKEY hKey = NULL;
	char FileName[_MAX_PATH];	//	Name of the file to export to
	char ToWrite[102400];		//	Buffer for text that is waiting to be written
	HANDLE hFile;			//	Handle of the file that the data is written to

	ZeroMemory ( ToWrite, sizeof ( ToWrite ));
	ZeroMemory ( FileName, sizeof ( FileName ));

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


		//	The text file must have this heading at the top, otherwise
		//	regedit does not interpret it as a registry file
		strcpy ( ToWrite, "REGEDIT4\r\n\r\n" );

		//*************************************************************
		//	Save the values in the WISP key
		RegOpenKeyEx( HKEY_LOCAL_MACHINE, REGKEY_WISP,0, KEY_READ, &hKey );
		appendRegkey(ToWrite, REGKEY_WISP);
		appendRegValue(ToWrite, REGVAL_WISP_SERVER, hKey);
		appendRegValue(ToWrite, REGVAL_WISP_WISPDIR, hKey);
		appendRegValue(ToWrite, REGVAL_WISP_WISPCONFIG, hKey);
		appendRegValue(ToWrite, REGVAL_WISP_PATH, hKey);
		appendRegValue(ToWrite, REGVAL_WISP_TMPDIR, hKey);
		appendRegValue(ToWrite, REGVAL_WISP_WISPSORTMEM, hKey);
		appendRegValue(ToWrite, REGVAL_WISP_USERSDIR, hKey);
		RegCloseKey ( hKey );
		hKey = NULL;

		//*************************************************************
		//	OK, now do the WISP\VIDEOCAP key
		RegOpenKeyEx( HKEY_LOCAL_MACHINE, REGKEY_WISP_VIDEOCAP,0, KEY_READ, &hKey );
		appendRegkey(ToWrite, REGKEY_WISP_VIDEOCAP);
		appendRegValue(ToWrite, REGVAL_VIDEOCAP_WISPTERM, hKey);
		appendRegValue(ToWrite, REGVAL_VIDEOCAP_VIDEOCAP, hKey);
		RegCloseKey ( hKey );
		hKey = NULL;

		//*************************************************************
		//	Now do the WISP\VSSUBS key
		appendRegkey(ToWrite, REGKEY_WISP_VSSUBS);

		//*************************************************************
		//	Now do the WISP\VSSUBS\EXTRACT key
		RegOpenKeyEx( HKEY_LOCAL_MACHINE, REGKEY_WISP_VSSUBS_EXTRACT,0, KEY_READ, &hKey );
		appendRegkey(ToWrite, REGKEY_WISP_VSSUBS_EXTRACT);
		appendRegValue(ToWrite, REGVAL_EXTRACT_WISPCPU, hKey);
		appendRegValue(ToWrite, REGVAL_EXTRACT_WISPNETID, hKey);
		RegCloseKey ( hKey );
		hKey = NULL;

		//*************************************************************
		//	Now do the WISP\VSSUBS\MESSAGE key
		RegOpenKeyEx( HKEY_LOCAL_MACHINE, REGKEY_WISP_VSSUBS_MESSAGE,0, KEY_READ, &hKey );
		appendRegkey(ToWrite, REGKEY_WISP_VSSUBS_MESSAGE);
		appendRegValue(ToWrite, REGVAL_MESSAGE_SHAREDIR, hKey);
		RegCloseKey ( hKey );
		hKey = NULL;

		//*************************************************************
		//	Now do the WISP\VSSUBS\SCRATCH key
		RegOpenKeyEx( HKEY_LOCAL_MACHINE, REGKEY_WISP_VSSUBS_SCRATCH,0, KEY_READ, &hKey );
		appendRegkey(ToWrite, REGKEY_WISP_VSSUBS_SCRATCH);
		appendRegValue(ToWrite, REGVAL_SCRATCH_WISPSCRATCHMODE, hKey);
		RegCloseKey ( hKey );
		hKey = NULL;

		//*************************************************************
		//	Now do the WISP\License key
		RegOpenKeyEx( HKEY_LOCAL_MACHINE, REGKEY_WISP_LICENSE,0, KEY_READ, &hKey );
		appendRegkey(ToWrite, REGKEY_WISP_LICENSE);
		appendRegValue(ToWrite, REGVAL_LICENSE_FILE, hKey);
		RegCloseKey ( hKey );
		hKey = NULL;

		//*************************************************************
		//	Now do the WISP\Versions key
		RegOpenKeyEx( HKEY_LOCAL_MACHINE, REGKEY_WISP_VERSIONS,0, KEY_READ, &hKey );
		appendRegkey(ToWrite, REGKEY_WISP_VERSIONS);
		appendRegValue(ToWrite, REGVAL_VERSIONS_WISP, hKey);
		RegCloseKey ( hKey );
		hKey = NULL;

		//*************************************************************
		//	Now do the WISP\WISPBin key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_WISPBIN,0, KEY_READ, &hKey );
		appendRegkey(ToWrite, REGKEY_WISP_WISPBIN);
		appendRegValue(ToWrite, REGVAL_WISPBIN_WISP, hKey);
		appendRegValue(ToWrite, REGVAL_WISPBIN_WEDITOR, hKey);
		appendRegValue(ToWrite, REGVAL_WISPBIN_DISPLAY, hKey);
		RegCloseKey ( hKey );
		hKey = NULL;

		//*************************************************************
		//	Now do the WISP\WISPBin\DISPLAY key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_WISPBIN_DISPLAY,0, KEY_READ, &hKey );
		appendRegkey(ToWrite, REGKEY_WISP_WISPBIN_DISPLAY);
		appendRegValue(ToWrite, REGVAL_DISPLAY_WISPDISPLAY8BIT, hKey);
		RegCloseKey ( hKey );
		hKey = NULL;

		//*************************************************************
		//	Now do the WISP\WISPBin\WPROC key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_WISPBIN_WPROC,0, KEY_READ, &hKey );
		appendRegkey(ToWrite, REGKEY_WISP_WISPBIN_WPROC);
		appendRegValue(ToWrite, REGVAL_WPROC_WPROC, hKey);
		appendRegValue(ToWrite, REGVAL_WPROC_WPROCDEBUG, hKey);
		RegCloseKey ( hKey );
		hKey = NULL;

		//*************************************************************
		//	Now do the WISP\WISPBin\WISPTRAN key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_WISPBIN_WISPTRAN,0, KEY_READ, &hKey );
		appendRegkey(ToWrite, REGKEY_WISP_WISPBIN_WISPTRAN);
		appendRegValue(ToWrite, REGVAL_WISPTRAN_HSIZE, hKey);
		appendRegValue(ToWrite, REGVAL_WISPTRAN_VSIZE, hKey);
		appendRegValue(ToWrite, REGVAL_WISPTRAN_WORKDIR, hKey);
		RegCloseKey ( hKey );
		hKey = NULL;

		//*************************************************************
		//	Now do the WISP\WISPBin\WISPTRAN\COBOL key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_WISPBIN_WISPTRAN_COBOL,0, KEY_READ, &hKey );
		appendRegkey(ToWrite, REGKEY_WISP_WISPBIN_WISPTRAN_COBOL);
		appendRegValue(ToWrite, REGVAL_COBOL_LANGUAGE, hKey);
		RegCloseKey ( hKey );
		hKey = NULL;

		//*************************************************************
		//	Now do the WISP\WISPBin\WISPTRAN\COBOL\ACUCOBOL key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_WISPBIN_WISPTRAN_COBOL_ACU,0, KEY_READ, &hKey );
		appendRegkey(ToWrite, REGKEY_WISP_WISPBIN_WISPTRAN_COBOL_ACU);
		appendRegValue(ToWrite, REGVAL_COBOL_COB, hKey);
		appendRegValue(ToWrite, REGVAL_COBOL_COBFLAGS, hKey);
		appendRegValue(ToWrite, REGVAL_COBOL_OUTDIR, hKey);
		appendRegValue(ToWrite, REGVAL_COBOL_OBJEXT, hKey);
		RegCloseKey ( hKey );
		hKey = NULL;

		//*************************************************************
		//	Now do the WISP\WISPBin\WISPTRAN\COBOL\MFCOBOL key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_WISPBIN_WISPTRAN_COBOL_MF,0, KEY_READ, &hKey );
		appendRegkey(ToWrite, REGKEY_WISP_WISPBIN_WISPTRAN_COBOL_MF);
		appendRegValue(ToWrite, REGVAL_COBOL_COB, hKey);
		appendRegValue(ToWrite, REGVAL_COBOL_COBFLAGS, hKey);
		appendRegValue(ToWrite, REGVAL_COBOL_OUTDIR, hKey);
		appendRegValue(ToWrite, REGVAL_COBOL_OBJEXT, hKey);
		RegCloseKey ( hKey );
		hKey = NULL;

		//*************************************************************
		//	Now do the WISP\ACP key
		RegOpenKeyEx ( HKEY_LOCAL_MACHINE, REGKEY_WISP_ACP,0, KEY_READ, &hKey );
		appendRegkey(ToWrite, REGKEY_WISP_ACP);
		appendRegValue(ToWrite, REGVAL_ACP_ACPCONFIG, hKey);
		appendRegValue(ToWrite, REGVAL_ACP_ACPMAP, hKey);
		RegCloseKey ( hKey );
		hKey = NULL;

		//*************************************************************
		DWORD Written = 0;
		int foo = strlen ( ToWrite );
		WriteFile ( hFile, ToWrite,
			strlen ( ToWrite ), &Written, NULL );
		SetEndOfFile ( hFile );
		if ( hFile != NULL ) CloseHandle ( hFile );
	}
	return;
}

