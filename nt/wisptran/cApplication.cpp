/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/

#include "#Defines.h"
#include "#Classes.h"
#include "#Prototypes.h"
#include "#Externs.h"
#include "resource.h"

//////////////////////////////////////////////////////////////////////////
//
//	cApplication::cApplication
//		Constructor
//
cApplication::cApplication ()
{
	//	Initialize the WOptions struct
	CmdLine.WOptions = (cApplication::_CmdLine::_WOptions *)
		malloc (sizeof (cApplication::_CmdLine::_WOptions));
	ZeroMemory (CmdLine.WOptions, _msize (CmdLine.WOptions));
	//	Initialize the COptions struct
	CmdLine.COptions = (cApplication::_CmdLine::_COptions *)
		malloc (sizeof (cApplication::_CmdLine::_COptions));
	ZeroMemory (CmdLine.COptions, _msize (CmdLine.COptions));
	//	Initialize the Environment struct
	Env = (cApplication::_Environment *)
		malloc (sizeof (cApplication::_Environment));
	ZeroMemory (Env, _msize (Env));
	CmdLine.Break = FALSE;
	return;
}
//////////////////////////////////////////////////////////////////////////
//
//	cApplication::~cApplication
//		Destructor
//
cApplication::~cApplication ()
{
	free (CmdLine.WOptions);
	free (CmdLine.COptions);
	free (Env);

	return;
}
//////////////////////////////////////////////////////////////////////////
//
//	cApplication::AppInit
//		Initializes the application
//
int cApplication::AppInit (HINSTANCE hInst)
{
	OSVERSIONINFO VerInfo;
	RECT ScrRes;

	cApp.Thread.hPreEntrySmphr =
		CreateSemaphore ( NULL, 1, 1, "PreEntrySmphr" );

	//	Set Global Flags
	GlbFlags.isTranslating = FALSE;
	GlbFlags.isCompiling = FALSE;

	//	Load Common control library
	InitCommonControls ( );
	//	Get System Info
	VerInfo.dwOSVersionInfoSize	=	sizeof ( OSVERSIONINFO );
	GetVersionEx ( &VerInfo );
	GetWindowRect ( GetDesktopWindow ( ), &ScrRes );
	Env->SysInfo.MajorVer	=	VerInfo.dwMajorVersion;
	Env->SysInfo.MinorVer	=	VerInfo.dwMinorVersion;
	Env->SysInfo.Platform	=	VerInfo.dwPlatformId;
	Env->SysInfo.ScrRes[0]	=	ScrRes.right;
	Env->SysInfo.ScrRes[1]	=	ScrRes.bottom;
	GetWindowsDirectory ( Env->SysInfo.ch_WinDir, _MAX_PATH );
	GetDrvInfo ( );
	//	Get Registry info
	GetReg ( );

	cWnd.CreateWnd (hInst);														//	Create the window classes
	cWnd.ShowWnd (hInst);														//	Initialize and display the windows
	SetFocus (cWnd.OptionWnd.CCtls._TargetFile);
	SetFocus (cWnd.OptionWnd.WCtls._TargetFile);
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	cApplication::MsgLoop
//		The application message loop
//
void cApplication::MsgLoop ()
{
	MSG msg;																			//	Holds the message to be processed
	while (																			//	Perform loop as long as there are messages in the app's que
		GetMessage (&msg, NULL, 0, 0)) {										//	Get message from que
		TranslateMessage (&msg);												//	Translate virtual-key messages
		DispatchMessage (&msg);													//	Sends the message to the appropriate window to be handled by that window proc
	}
}
//////////////////////////////////////////////////////////////////////////
//
//	cApplication::CheckMsg
//		Checks if any messages are waiting in the que
//
void cApplication::CheckMsg ()
{
	MSG msg;																			//	Holds the message to be processed
	GetMessage (&msg, NULL, 0, 0);											//	Get message from que
	TranslateMessage (&msg);													//	Translate virtual-key messages
	DispatchMessage (&msg);														//	Sends the message to the appropriate window to be handled by that window proc
}
//////////////////////////////////////////////////////////////////////////
//
//	cApplication::CloseApp
//		Closes the app
//
int cApplication::CloseApp (int ExitCode, int WinError)
{
    char MsgText[256];

	switch ( ExitCode ) {
		case 1000:
		case 1001:
		case 1002:
			if (MessageBox (cWnd.hWnd.hShellWnd, "Are you sure "
						"you want to quit WISPTran for Windows?",
						"Exit WISPTran?", MB_YESNO) == IDYES) {			//	Display message box to confirm that the user want's to exit the app
				PostQuitMessage ( ExitCode );									//	If yes then close the app
			}
			break;
		case 2001:
		case 2002:
			MessageBox ( cWnd.hWnd.hShellWnd,
				"The data in the registry may not be valid.\r\n"
				"You must run Setup or correct the data to continue.",
				"Invalid Registry Data", MB_OK );
			PostQuitMessage ( ExitCode );
			break;
		case 2003:
			MessageBox ( cWnd.hWnd.hShellWnd,
				"A registry key could not be closed.\r\n"
				"You must run Setup or correct this error to continue.",
				"Invalid Registry Data", MB_OK );
			PostQuitMessage ( ExitCode );
			break;
		case 2004:
			MessageBox ( cWnd.hWnd.hShellWnd,
				"The following registry value could not be accessed:\r\n"
				"HKEY_LOCAL_MACHINE\\Software\\NeoMedia\\WISP\\"
				"WISPBin\\WISPTran\\COBOL\\ACUCOBOL\\COB\r\n"
				"You must run Setup or correct this error to continue.",
				"Invalid Registry Data", MB_OK );
			PostQuitMessage ( ExitCode );
			break;
		case 3001:
			MessageBox ( cWnd.hWnd.hShellWnd,
				"     *** OUT OF MEMORY ***\r\n\n"
				"      Check system resources!",
				"malloc() / realloc() FAILURE", MB_OK);
			PostQuitMessage ( ExitCode );
			break;
		case 3002:
			MessageBox ( cWnd.hWnd.hShellWnd,
				"     *** FILE CREATION ERROR ***\r\n\n"
				"      Could not open file",
				"CreateFile() FAILURE", MB_OK);
			PostQuitMessage ( ExitCode );
			break;
        case 4001:
            sprintf(MsgText,"     *** PRINT ERROR ***\r\n\n"
				"Could not create logical font\n"
                "Windows error = %d", WinError);
            MessageBox ( cWnd.hWnd.hShellWnd, MsgText,
                "CreateFont() FAILURE\n", MB_OK);
			PostQuitMessage ( ExitCode );
        case 4002:
            sprintf(MsgText,"     *** PRINT ERROR ***\r\n\n"
				"Could not start print job\n"
                "Windows error = %d", WinError);
            MessageBox ( cWnd.hWnd.hShellWnd, MsgText,
				"StartDoc() FAILURE", MB_OK);
			PostQuitMessage ( ExitCode );
        case 4003:
            sprintf(MsgText,"     *** PRINT ERROR ***\r\n\n"
				"Error preparing print driver to accept data\n"
                "Windows error = %d", WinError);
            MessageBox ( cWnd.hWnd.hShellWnd, MsgText,
				"StartPage() FAILURE", MB_OK);
			PostQuitMessage ( ExitCode );
        case 4004:
            MessageBox ( cWnd.hWnd.hShellWnd,
				"     *** PRINT ERROR ***\r\n\n"
				"   Error selecting handle into the"
                "   specified device context",
				"SelectObject() FAILURE", MB_OK);
			PostQuitMessage ( ExitCode );
        case 4005:
            sprintf(MsgText,"     *** PRINT ERROR ***\r\n\n"
				"Error writing character string to specified location\n"
                "Windows error = %d", WinError);
            MessageBox ( cWnd.hWnd.hShellWnd, MsgText,
				"TextOut() FAILURE", MB_OK);
			PostQuitMessage ( ExitCode );
        case 4006:
            sprintf(MsgText,"     *** PRINT ERROR ***\r\n\n"
				"Error advancing page\n"
                "Windows error = %d", WinError);
            MessageBox ( cWnd.hWnd.hShellWnd, MsgText,
				"EndPage() FAILURE", MB_OK);
			PostQuitMessage ( ExitCode );
        case 4007:
            sprintf(MsgText,"     *** PRINT ERROR ***\r\n\n"
				"Error ending print job\n"
                "Windows error = %d", WinError);
            MessageBox ( cWnd.hWnd.hShellWnd, MsgText,
				"EndDoc() FAILURE", MB_OK);
			PostQuitMessage ( ExitCode );
	}
	cApp.SetReg ( );
	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//		_CmdLine - Controls the building of the command line and execution
//			of WISP
//

//////////////////////////////////////////////////////////////////////////
//
//	cApplication::CmdLine::Execute
//		Executes the command line specified and displays any
//		output that is returned by the executable
//
int cApplication::_CmdLine::Execute (char *ch_CmdLine, char *ch_FileName, int i_FilesRemain,
											 int i_FilesSelect, int i_TransComp, int tc )
{
	//	Variables
	HANDLE hRedirOutRd, hRedirOutWr;										//	Handles to the read and write ends of the pipe
	SECURITY_ATTRIBUTES saPipe;												//	Security attributes for the pipe
	STARTUPINFO sInfo;															//	Characteristics for the child process
	PROCESS_INFORMATION pi;														//	Buffer for info returned by the child process
	DWORD dwRead;																	//	Buffer for number of bytes read
	int whileCnt = 0;	//	Used to increment while loops
	char *ch_Output,
		*ch_Read,
		ch_Temp[OutputBlock],
		ch_Status[512];
	BOOL isErrors = TRUE;
	i_FilesRemain = i_FilesSelect - i_FilesRemain;

    //  Delete the LOG file before executing the command
    cApp.CmdLine.DeleteLog (ch_FileName);

	//	Update status bar
	if ( i_TransComp == TC_Translate ) 
		sprintf (ch_Status, "Translating file \"%s\", please wait...   "
		"%d files remaining.", ch_FileName, i_FilesRemain);
	else 	sprintf (ch_Status, "Compiling file \"%s\", please wait...  "
				"%d files remaining.", ch_FileName, i_FilesRemain);

	CustCtrl.StatusBar.UpdateMsg (ch_Status);
	//	Initializes structs for use in folowing function calls
	saPipe.nLength		=	sizeof (SECURITY_ATTRIBUTES);					//	Size of the structure
	saPipe.lpSecurityDescriptor	=	NULL;										//	Inherit from parent process
	saPipe.bInheritHandle			=	TRUE;										//	If TRUE handles are inheritable
																		//	Create Pipe to capture and redirect output from command line util
	CreatePipe (&hRedirOutRd, &hRedirOutWr, &saPipe, 0);				//	Write end of pipe is used to catch the output from the DOS app
	SetStdHandle (STD_OUTPUT_HANDLE, hRedirOutWr);						//	SetStandardHandle redirects the output to the write end of the pipe
	//	Redirect stdout
	sInfo.cb				=	sizeof (STARTUPINFO);								//	size of structure
	sInfo.lpReserved	=	NULL;														//	Reserved - must be null
	sInfo.lpDesktop	=	NULL;														//	if NULL then parent's attributes are inherited
	sInfo.lpTitle		=	NULL;														//	Title of the console window
	sInfo.dwFlags		=	
		STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;							//	Determines which optional attributes are used
	sInfo.wShowWindow	=	SW_HIDE;													//	Prevents the console window from appearing
	sInfo.cbReserved2	=	0;															//	Reserved - must be 0
	sInfo.lpReserved2	=	NULL;														//	Reserved - must be NULL
	sInfo.hStdOutput	=	hRedirOutWr;											//	Handle to redirect the stdout to
	sInfo.hStdError	=	hRedirOutWr;											//	Handle to redirect the stderr to
	if ( tc == TC_Translate ) 
		strcat (ch_CmdLine, ch_FileName);

	/*	
		Allocate a output buffer that is big enough to hold the
		current contents plus 4K.
	*/
	int text_len = 0;

	// Get current size of output window
	if ( i_TransComp == TC_Compile )
	{
		text_len = GetWindowTextLength(cWnd.hWnd.hCOutput);
	}
	else // if ( (i_TransComp == TC_Translate) || (i_TransComp == TC_TranComp))
	{
		text_len = GetWindowTextLength(cWnd.hWnd.hWOutput);
	}

	if (0==text_len) // possible error
	{
		if (GetLastError() != ERROR_SUCCESS) 
		{
			// ERROR
		}
	}

	int buf_size = text_len + OutputBlock * 2; // Add 4K

	//	Allocate mem for output string
	ch_Output = (char *) malloc (buf_size);							
	if( NULL == ch_Output )
	{
		cApp.CloseApp ( 3001, 0 );
		ExitProcess(3001);
	}
	ZeroMemory (ch_Output, _msize (ch_Output));

	// Get current window contents
	int actual_text_len = 0;
	if ( i_TransComp == TC_Compile )
	{
		actual_text_len = GetWindowText(cWnd.hWnd.hCOutput, ch_Output, _msize (ch_Output));
	}
	else // if ( (i_TransComp == TC_Translate) || (i_TransComp == TC_TranComp))
	{
		actual_text_len = GetWindowText(cWnd.hWnd.hWOutput, ch_Output, _msize (ch_Output));
	}

	if (0==actual_text_len) // possible error
	{
		if (GetLastError() != ERROR_SUCCESS) 
		{
			// ERROR
		}
	}

	if (actual_text_len > text_len)
	{
		// The actual text lenght can be less then but not greater then the reported lenght.
	}

	strcat (ch_Output, "========================================"
					   "========================================\r\n");
	strcat (ch_Output, ch_CmdLine);
	strcat (ch_Output, "\r\n----------------------------------------"
					       "----------------------------------------\r\n");
	//==Run wisp======================================================
	char CurDir[_MAX_PATH];
	if ( (i_TransComp == TC_Translate) || (i_TransComp == TC_TranComp) )
		GetWindowText ( cWnd.OptionWnd.WCtls._DirPath, CurDir, _MAX_PATH );
	if ( i_TransComp == TC_Compile )
		GetWindowText ( cWnd.OptionWnd.CCtls._DirPath, CurDir, _MAX_PATH );
	CreateProcess (NULL, ch_CmdLine, NULL, NULL, TRUE, 
		NULL, NULL, CurDir, &sInfo, &pi);						//	Creates the new process - executes wisp
	//==Allocate mem for chRead=======================================
	ch_Read = (char *) malloc (OutputBlock + 16);
	if( !ch_Read )
	{
		cApp.CloseApp ( 3001, 0 );
		ExitProcess(3001);
	}
	ZeroMemory (ch_Read, _msize (ch_Read));
	//	Close the handle so that no more output is placed there
	CloseHandle (hRedirOutWr);
	//==Get text returned from WISP===================================
	ZeroMemory ( ch_Temp, sizeof ( ch_Temp ));
	//==While there is more stuff to get from wisp====================
	BOOL foo = TRUE;
	for(;;)
	{
		ReadFile (hRedirOutRd, ch_Temp, sizeof(ch_Temp)-1, &dwRead, NULL);
		if (dwRead == 0)
		{
			break;
		}
		ch_Temp[dwRead] = '\0';
		if (strlen(ch_Read) + strlen(ch_Temp) + 1 > _msize(ch_Read))
		{
			char *tptr;

			tptr = (char *)realloc(ch_Read, _msize(ch_Read) + sizeof(ch_Temp) * 2);
			if( !tptr )
			{
				free(ch_Read);
				ch_Read = NULL;
				cApp.CloseApp ( 3001, 0 );
				ExitProcess(3001);
			}
			ch_Read = tptr;
		}
		strcat(ch_Read, ch_Temp);
	}

	//==If nothing was returned by WISP===============================
	if (strlen (ch_Read) == 0)
	{
		if ( i_TransComp == TC_Translate )
		{
			sprintf (ch_Read, 
				"The file '%s' has translated with no errors or warnings.\r\n",
				ch_FileName);																	//	Print this message to the status window
		}
		else 
		{
			sprintf (ch_Read, 
				"The file '%s' has compiled with no errors or warnings.\r\n",
				ch_FileName);														//	Print this message to the status window
		}

		if (strlen(ch_Read) + strlen(ch_Output) + 1 > _msize (ch_Output))
		{
			char *tptr;

			tptr = (char *) realloc (ch_Output,
				(_msize (ch_Output) + strlen(ch_Read) + 1));
			if( !tptr )
			{
				free(ch_Output);
				ch_Output = NULL;
				cApp.CloseApp ( 3001, 0 );
				ExitProcess(3001);
			}
			ch_Output = tptr;
			buf_size += strlen(ch_Read) + 1;
		}
		strcat (ch_Output, ch_Read);												//	Add it to the Output string
		isErrors = FALSE;
	}
	//==Otherwise, add the returned data to the output string=========
	else 
	{
		if (strlen(ch_Read) + strlen(ch_Output) + 1 > _msize (ch_Output))
		{
			char *tptr;

			tptr = (char *) realloc (ch_Output,
				(_msize (ch_Output) + strlen(ch_Read) + 1));
			if( !tptr )
			{
				free(ch_Output);
				ch_Output = NULL;
				cApp.CloseApp ( 3001, 0 );
				ExitProcess(3001);
			}
			ch_Output = tptr;
			buf_size += strlen(ch_Read) + 1;
		}
		strcat (ch_Output, ch_Read);												//	Add it to the Output string
	}
	free(ch_Read);
    ch_Read = NULL;

	if ( i_TransComp == TC_Translate )
	{	//	Send the text to the output window
		SendMessage (cWnd.hWnd.hWOutput, WM_SETTEXT, 0,
			(LPARAM) ch_Output);
	}

	if ( i_TransComp == TC_Compile )
	{	//	Send the text to the output window
		SendMessage (cWnd.hWnd.hCOutput, WM_SETTEXT, 0,
			(LPARAM) ch_Output);
	}

	if ( i_TransComp == TC_TranComp )
	{	//	Send the text to the output window
		SendMessage (cWnd.hWnd.hWOutput, WM_SETTEXT, 0,
			(LPARAM) ch_Output);
		SendMessage (cWnd.hWnd.hCOutput, WM_SETTEXT, 0,
			(LPARAM) ch_Output);
	}

	//==Scroll as text is added to the window=========================
	int NumLines, TopLine, ToLine;

	if ( i_TransComp == TC_Compile )
	{
		NumLines = SendMessage (cWnd.hWnd.hCOutput,
			EM_GETLINECOUNT, 0, 0);
		TopLine = SendMessage (cWnd.hWnd.hCOutput,
			EM_GETFIRSTVISIBLELINE, 0, 0);
		ToLine = NumLines - TopLine - 6;
		//	Scroll to bottom of window
		SendMessage (cWnd.hWnd.hCOutput, EM_LINESCROLL, 0, ToLine);
	}
	else
	{
		NumLines = SendMessage (cWnd.hWnd.hWOutput,
			EM_GETLINECOUNT, 0, 0);
		TopLine = SendMessage (cWnd.hWnd.hWOutput,
			EM_GETFIRSTVISIBLELINE, 0, 0);
		ToLine = NumLines - TopLine - 6;
		//	Scroll to bottom of window
		SendMessage (cWnd.hWnd.hWOutput, EM_LINESCROLL, 0, ToLine);
	}

	//	Copy last file report to seperate buffer
	if (isErrors == TRUE) {
		char *ch_ToFile, *ch_Tmp;
		int StartStr = 0 , EndStr;
		int MinStartStr;
		/*
			Find the beginning of the report.
			- find the filename that is part of the command string
			- search backwards until you find a line of "===="
			- find the beginning of the "===" line then copy it.
			StartStr is a negative offset.
		*/
		ch_Tmp = strstr (ch_Output, ch_FileName);
		MinStartStr = ch_Output - ch_Tmp;
		// find the start of the command line
		while ((StartStr >= MinStartStr) &&
		       (ch_Tmp[StartStr] != '=')           ) StartStr--;
		// find the start of the "====" line
		while ((StartStr >= MinStartStr) &&
		       (ch_Tmp[StartStr] == '=')           ) StartStr--;
		// StartStr will be one less then it needs to be
		// this is used in the calculations to get one for the
		// trailing null.
		EndStr = strlen (ch_Tmp);
		ch_ToFile = (char *) malloc ((EndStr - StartStr));
		if( !ch_ToFile )
		{
			cApp.CloseApp ( 3001, 0 );
			ExitProcess(3001);
		}
		// This copies the string plus the trailing null.
		memcpy (ch_ToFile, &ch_Tmp[StartStr+1], EndStr - StartStr);
		
		cApp.CmdLine.WriteLog (ch_FileName, ch_ToFile);
		free(ch_ToFile);
        ch_ToFile = NULL;
	}
	free (ch_Output);   //	Free the allocated mem
    ch_Output = NULL;
	buf_size = 0;

	//	Update the status bar
	if ( i_TransComp == TC_Translate )
		CustCtrl.StatusBar.UpdateMsg("Translating complete.  Ready");	
	if ( i_TransComp == TC_Compile )
		CustCtrl.StatusBar.UpdateMsg("Compiling  complete.  Ready");
	if ( i_TransComp == TC_TranComp )
		CustCtrl.StatusBar.UpdateMsg("Translating and Compiling  complete.  Ready");

	CloseHandle ( hRedirOutRd );


	//	If text to print then activate the Print and Clear Output Window menu options
	if ( i_TransComp == TC_Compile )
	{
		text_len = GetWindowTextLength(cWnd.hWnd.hCOutput);
	}
	else // if ( (i_TransComp == TC_Translate) || (i_TransComp == TC_TranComp))
	{
		text_len = GetWindowTextLength(cWnd.hWnd.hWOutput);
	}

	if (text_len > 0)
	{
		EnableMenuItem ( cWnd.hAppMenu, 
				mnu_Print, MF_BYCOMMAND | MF_ENABLED );
		EnableMenuItem ( cWnd.hAppMenu, 
				mnu_ClearOutput, MF_BYCOMMAND | MF_ENABLED );
	}

	return 0;
}
///////////////////////////////////////////////////////////////////////////////
//
//	cApplication::CmdLine::Build
//		Builds the command line string to pass to the process that executes
//		WISP or the COBOL compiler
//
int cApplication::_CmdLine::Build (BOOL Exec, int i_TransComp)
{
	char ch_WCmdLine[MaxCmdLine],		//	Holds a character string that is passed as the command line
		ch_CCmdLine[MaxCmdLine],	//	Holds a character string that is passed as the command line
		ch_WCmdLineNoFile[MaxCmdLine],	//	Holds the command line string but with no file name in it
		ch_CCmdLineNoFile[MaxCmdLine],	//	Holds the command line string but with no file name in it
		ch_CCmdLineTmp[MaxCmdLine],
		ch_WFileName[_MAX_PATH];	//	Temp buffer to hold the name of a file that was selected
	char	ch_cobFile[_MAX_PATH] = "";	//	Buffer for COBOL file name
	char	ch_objFile[_MAX_PATH] = "";	//	Buffer for the COBOL object file name
	char	ch_ObjExt[_MAX_FNAME] = "";	//	Buffer for the COBOL object file name extension
	int cnt,			//	Generic counter for loops
		TotalFiles;		//	The total number of files the user has selected
	//	If Translate or Translate and Compile button clicked
	if ((i_TransComp == TC_Translate) || (i_TransComp == TC_TranComp)) {
		int EntryLen;
		BOOL GenBool = FALSE;

		ZeroMemory (ch_WCmdLine, sizeof (ch_WCmdLine));
		strcpy (ch_WCmdLine, cApp.Env->RegInfo.ch_WPath);

		if (cApp.CmdLine.WOptions->_1 == TRUE) strcat (ch_WCmdLine, " /1");
		if (cApp.CmdLine.WOptions->_4 == TRUE) strcat (ch_WCmdLine, " /4");
		if (cApp.CmdLine.WOptions->_F == TRUE) strcat (ch_WCmdLine, " /F");
		if (cApp.CmdLine.WOptions->_D == TRUE) strcat (ch_WCmdLine, " /D");
		if (cApp.CmdLine.WOptions->_e == TRUE) strcat (ch_WCmdLine, " /e");
		if (cApp.CmdLine.WOptions->_f == TRUE) strcat (ch_WCmdLine, " /f");
		if (cApp.CmdLine.WOptions->_l == TRUE) strcat (ch_WCmdLine, " /l");
		if (cApp.CmdLine.WOptions->_L == TRUE) strcat (ch_WCmdLine, " /L");
		if (cApp.CmdLine.WOptions->_m == TRUE) strcat (ch_WCmdLine, " /m");
		if (cApp.CmdLine.WOptions->_M == TRUE) strcat (ch_WCmdLine, " /M");
		if (cApp.CmdLine.WOptions->_q == TRUE) strcat (ch_WCmdLine, " /q");
		if (cApp.CmdLine.WOptions->_G4 == TRUE) strcat (ch_WCmdLine, " /G4");
		if (cApp.CmdLine.WOptions->_S == TRUE) strcat (ch_WCmdLine, " /S");
		if (cApp.CmdLine.WOptions->_T == TRUE) strcat (ch_WCmdLine, " /T");
		if (cApp.CmdLine.WOptions->_w == TRUE) strcat (ch_WCmdLine, " /w");
		if (cApp.CmdLine.WOptions->_x == TRUE) strcat (ch_WCmdLine, " /x");
		if (cApp.CmdLine.WOptions->_X == TRUE) strcat (ch_WCmdLine, " /X");
		//	Check for extension in file name and add to command line
		if (strlen (cApp.CmdLine.WOptions->ch_K) > 0) {
			EntryLen = strlen (cApp.CmdLine.WOptions->ch_K);
			cnt = EntryLen;
			while (cApp.CmdLine.WOptions->ch_K[cnt] != '.') {
				if (cnt == 0) break;
				cnt--;
			}
			if (cnt == 0 && Exec == TRUE) 
				strcat (cApp.CmdLine.WOptions->ch_K, ".KEY");
			strcat (ch_WCmdLine, " /K");
			strcat (ch_WCmdLine, cApp.CmdLine.WOptions->ch_K);
		}
		//	Check for extension in file name and add to command line
		if (strlen (cApp.CmdLine.WOptions->ch_O) > 0) {					//	If there is a file name in the option file field
			EntryLen = strlen (cApp.CmdLine.WOptions->ch_O);				//	Get the length of the file spec
			cnt = EntryLen;															//	Set pointer to end of file spec
			while (cApp.CmdLine.WOptions->ch_O[cnt] != '.') {				//	Look for a period
				if (cnt == 0) break;												//	If pointer reaches beginning of string then break
				cnt--;																	//	Check the next character
			}
			if (cnt == 0 && Exec == TRUE) 
				strcat (cApp.CmdLine.WOptions->ch_O, ".OPT");				//	If no extension was found then put the default "key" at the end of the file spec
			strcat (ch_WCmdLine, " /O");											//	Add this to the command line
			strcat (ch_WCmdLine, cApp.CmdLine.WOptions->ch_O);
		}
		//	Check these other edit fields to see if they have enything in them - if so copy it to the command line
		if (strlen (cApp.CmdLine.WOptions->ch_P) > 0) {
			strcat (ch_WCmdLine, " /P");
			strcat (ch_WCmdLine, cApp.CmdLine.WOptions->ch_P);
		}
		if (strlen (cApp.CmdLine.WOptions->ch_W) > 0) {
			strcat (ch_WCmdLine, " /W");
			strcat (ch_WCmdLine, cApp.CmdLine.WOptions->ch_W);
		}
		if (strlen (cApp.CmdLine.WOptions->ch_I) > 0) {
			strcat (ch_WCmdLine, " /I");
			strcat (ch_WCmdLine, cApp.CmdLine.WOptions->ch_I);
		}
		//	Copy the selected library to the command line
		strcat (ch_WCmdLine, " /V");
		strcat (ch_WCmdLine, cApp.CmdLine.WOptions->ch_V);
		strcat (ch_WCmdLine, " ");
		//	Determine the total number of files selected
		TotalFiles = SendMessage (
			cWnd.OptionWnd.WCtls._TargetFile,
			CB_GETCOUNT, 0, 0);														//	Get the number of files selected
		strcpy (ch_WCmdLineNoFile, ch_WCmdLine);									//	Makes it easier to cycle through the file names
		cnt = 0;																			//	Set counter to 0
	}
	//	If the Compile or Translate & Compile button clicked
	if ((i_TransComp == TC_Compile) || 
			(i_TransComp == TC_TranComp)) {
		//	Get the Path and file name of the COBOL executable
		GetDlgItemText (cWnd.hWnd.hCOBOLOpWnd,
			GetDlgCtrlID (cWnd.OptionWnd.CCtls._PathToComp),
			ch_CCmdLine, MaxCmdLine);
		strcat ( ch_CCmdLine, " " );
		//	Get the COBOL flags
		GetDlgItemText (cWnd.hWnd.hCOBOLOpWnd,
			GetDlgCtrlID (cWnd.OptionWnd.CCtls._CFlags),
			ch_CCmdLineTmp, GenText);
		strcat (ch_CCmdLine, ch_CCmdLineTmp);
		//	Determine the total number of files selected
		TotalFiles = SendMessage (
			cWnd.OptionWnd.CCtls._TargetFile,
			CB_GETCOUNT, 0, 0);														//	Get the number of files selected
		if ( i_TransComp == TC_TranComp ) {
			TotalFiles = SendMessage (
				cWnd.OptionWnd.WCtls._TargetFile,
				CB_GETCOUNT, 0, 0);														//	Get the number of files selected
		}
		strcpy (ch_CCmdLineNoFile, ch_CCmdLine);									//	Makes it easier to cycle through the file names
		if (( Exec == FALSE ) && ((i_TransComp == TC_Compile ) ||
			(i_TransComp == TC_TranComp ))) {
			//	Get the name of the target file
			SendMessage (
				cWnd.OptionWnd.CCtls._TargetFile,
				CB_GETLBTEXT, 0, (LPARAM) ch_cobFile );			//	Get the corresponding name in the combo box list
			//	Get the Obj Extension
			SendMessage (
				cWnd.OptionWnd.CCtls._ObjExt,
				WM_GETTEXT, _MAX_EXT, (LPARAM) ch_ObjExt );			//	Get the corresponding name in the combo box list
			//	Create obj file name
			strcpy ( ch_objFile, ch_cobFile );
			cnt = strlen ( ch_objFile );
			//	Find where extension begins
			while ( ch_objFile[cnt] != '.' ) {
				if ( cnt == 0 ) break;
				cnt--;
			}
			ch_objFile[cnt] = '\0';
			//	Check if there is a dot in the obj extension specified, if not, put one in
			if ( (strlen ( ch_ObjExt )) > 0 ) {
				if ( ch_ObjExt[0] != '.' ) {
					strcat ( ch_objFile, "." );
					strcat ( ch_objFile, ch_ObjExt );
				}
				else strcat ( ch_objFile, ch_ObjExt );
			}
			//	If AcuCOBOL is selected language
			if (SendMessage (cWnd.OptionWnd.CCtls._AcuBtn,
				BM_GETCHECK, 0, 0) == BST_CHECKED) {
				// Put output dir in Command Line
				if (GetDlgItemText (cWnd.hWnd.hCOBOLOpWnd,
					GetDlgCtrlID (cWnd.OptionWnd.CCtls._OutputDir),
						ch_CCmdLineTmp, MaxCmdLine) == NULL) {
					//	If no dir specified then use the current
					GetDlgItemText (cWnd.hWnd.hCOBOLOpWnd,
						GetDlgCtrlID (cWnd.OptionWnd.CCtls._DirPath),
						ch_CCmdLineTmp, MaxCmdLine);
				}
				if (ch_CCmdLineTmp[strlen (ch_CCmdLineTmp)-1] != '\\')
					strcat ( ch_CCmdLineTmp, "\\" );
				strcat (ch_CCmdLine, " -o ");
				strcat (ch_CCmdLine, ch_CCmdLineTmp);
				//	Must create an obj file name first
				strcat (ch_CCmdLine, ch_objFile);
				strcat (ch_CCmdLine, " ");
				strcat (ch_CCmdLine, ch_cobFile);
			}
			//	If MicroFocus is selected language
			if (SendMessage (
					cWnd.OptionWnd.CCtls._MFBtn,
					BM_GETCHECK, 0, 0) == BST_CHECKED) {
				strcat (ch_CCmdLine, " ");
				strcat (ch_CCmdLine, ch_cobFile);
				strcat (ch_CCmdLine, " copy ");
				strcat (ch_CCmdLine, ch_objFile);
				strcat (ch_CCmdLine, " ");
				//	Put output dir in command line
				if (GetDlgItemText (cWnd.hWnd.hCOBOLOpWnd,
					GetDlgCtrlID (cWnd.
					OptionWnd.CCtls._OutputDir),
						ch_CCmdLineTmp, MaxCmdLine) == NULL) {
					//	If no dir specified then use the current
					GetDlgItemText (cWnd.hWnd.hCOBOLOpWnd, GetDlgCtrlID (cWnd.
						OptionWnd.CCtls._DirPath),
						ch_CCmdLineTmp, MaxCmdLine);
				}
				if ( ch_CCmdLineTmp[strlen (ch_CCmdLineTmp)-1] != '\\' ) 
					strcat ( ch_CCmdLineTmp, "\\" );
				strcat (ch_CCmdLine, ch_CCmdLineTmp);
			}
		}
	}
	int FilesDone = 0;
	//	Execute or update status window as required
	//	While there is more files to wisp - FilesDone keeps track of this
	while ((FilesDone < TotalFiles)) {
		if (Break == TRUE) {
			Break = FALSE;
			break;
		}
		//	If the execute button was clicked
		if (Exec == TRUE) {
			//	If is Translating
			if (( i_TransComp == TC_Translate ) ||
					( i_TransComp == TC_TranComp )) {
				SendMessage (
					cWnd.OptionWnd.WCtls._TargetFile,
					CB_GETLBTEXT, FilesDone, (LPARAM) ch_WFileName);			//	Get the corresponding name in the combo box list
				Execute (ch_WCmdLine, ch_WFileName, FilesDone,
					TotalFiles, i_TransComp, TC_Translate );							//	Call Execute member function
				strcpy (ch_WCmdLine, ch_WCmdLineNoFile);								//	Clear the file name from the command line
			}
			//	If is Compiling
			if (( i_TransComp == TC_Compile ) ||
					( i_TransComp == TC_TranComp )) {
				//	Get the name of the target file
				SendMessage (
					cWnd.OptionWnd.CCtls._TargetFile,
					CB_GETLBTEXT, FilesDone, (LPARAM) ch_cobFile );			//	Get the corresponding name in the combo box list
				//	Get the Obj Extension
				SendMessage (
					cWnd.OptionWnd.CCtls._ObjExt,
					WM_GETTEXT, _MAX_FNAME, (LPARAM) ch_ObjExt );			//	Get the corresponding name in the combo box list
				//	Create obj file name
				strcpy ( ch_objFile, ch_cobFile );
				cnt = strlen ( ch_objFile );
				//	Find where extension begins
				while ( ch_objFile[cnt] != '.' ) {
					if ( cnt == 0 ) break;
					cnt--;
				}
				ch_objFile[cnt] = '\0';
				strcpy ( ch_cobFile, ch_objFile );
				strcat ( ch_cobFile, ".cob" );
				//	Check if there is a dot in the obj extension specified, if not, put one in
				if ( (strlen ( ch_ObjExt )) > 0 ) {
					if ( ch_ObjExt[0] != '.' ) {
						strcat ( ch_objFile, "." );
						strcat ( ch_objFile, ch_ObjExt );
					}
					else strcat ( ch_objFile, ch_ObjExt );
				}
				//	Put the .cob extension on the end of the wcb file
				
				//	If AcuCOBOL is selected language
				if (SendMessage (cWnd.OptionWnd.CCtls.
						_AcuBtn, BM_GETCHECK, 0, 0) == BST_CHECKED) {
					// Put output dir in Command Line
					if (GetDlgItemText (cWnd.hWnd.hCOBOLOpWnd, GetDlgCtrlID (cWnd.
						OptionWnd.CCtls._OutputDir),
							ch_CCmdLineTmp, MaxCmdLine) == NULL) {
						//	If no dir specified then use the current
						GetDlgItemText (cWnd.hWnd.hCOBOLOpWnd, GetDlgCtrlID (cWnd.
							OptionWnd.CCtls._DirPath),
							ch_CCmdLineTmp, MaxCmdLine);
					}
					//	If there is no dir name to use then use the one from the translation
					if ( strlen ( ch_CCmdLineTmp ) == 0 ) {
						GetDlgItemText (cWnd.hWnd.hWISPOpWnd,
							GetDlgCtrlID (cWnd.OptionWnd.WCtls._DirPath),
							ch_CCmdLineTmp, MaxCmdLine);
					}
					if ( ch_CCmdLineTmp[strlen (ch_CCmdLineTmp)-1] != '\\' ) 
						strcat ( ch_CCmdLineTmp, "\\" );
					strcat (ch_CCmdLine, " -o ");
					strcat (ch_CCmdLine, ch_CCmdLineTmp);
					//	Must create an obj file name first
					strcat (ch_CCmdLine, ch_objFile);
					strcat (ch_CCmdLine, " ");
					strcat (ch_CCmdLine, ch_cobFile);
				}
				//	If MicroFocus is selected language
				if (SendMessage (
						cWnd.OptionWnd.CCtls._MFBtn,
						BM_GETCHECK, 0, 0) == BST_CHECKED) {
					strcat (ch_CCmdLine, " ");
					strcat (ch_CCmdLine, ch_cobFile);
					strcat (ch_CCmdLine, " copy ");
					strcat (ch_CCmdLine, ch_objFile);
					strcat (ch_CCmdLine, " ");
					//	Put output dir in command line
					if (GetDlgItemText (cWnd.hWnd.hCOBOLOpWnd, GetDlgCtrlID (cWnd.
						OptionWnd.CCtls._OutputDir),
							ch_CCmdLineTmp, MaxCmdLine) == NULL) {
						//	If no dir specified then use the current
						GetDlgItemText (cWnd.hWnd.hCOBOLOpWnd, GetDlgCtrlID (cWnd.
							OptionWnd.CCtls._DirPath),
							ch_CCmdLineTmp, MaxCmdLine);
					}
					if ( ch_CCmdLineTmp[strlen (ch_CCmdLineTmp)-1] != '\\' ) 
						strcat ( ch_CCmdLineTmp, "\\" );
					strcat (ch_CCmdLine, ch_CCmdLineTmp);
				}
				Execute (ch_CCmdLine, ch_cobFile, FilesDone,
					TotalFiles, i_TransComp, TC_Compile );							//	Call Execute member function
				strcpy (ch_CCmdLine, ch_CCmdLineNoFile);							//	Clear the file name from the command line
			}
			FilesDone++;														//	Go to the next file in the list
		}
		//	If the execute button was not pressed - perhaps a browse button
		else {
			if ( i_TransComp == TC_Translate ) {
				int CurSel = SendMessage (
					cWnd.OptionWnd.WCtls._TargetFile,
					CB_GETCURSEL, 0, 0);												//	Get the currently selected file name
				SendMessage (
					cWnd.OptionWnd.WCtls._TargetFile,
					CB_GETLBTEXT, CurSel, (LPARAM) ch_WFileName);					//	Get the corresponding name in the combo box list
				strcat (ch_WCmdLine, ch_WFileName);										//	Add this name to the command line
				if (SendMessage (cWnd.
						OptionWnd.WCtls._TargetFile,
						CB_GETCOUNT, 0, 0) == 0)
					strcpy (ch_WCmdLine, "");
				SendMessage (cWnd.hWnd.hWCmdLine,
					WM_SETTEXT, 0, (LPARAM) ch_WCmdLine);
			}
			else {
				SendMessage (cWnd.hWnd.hCCmdLine,
					WM_SETTEXT, 0, (LPARAM) ch_CCmdLine);
			}
			break;
		}
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	cApplication::CmdLine::WriteLog ()
//		Writes the output that is returned by the executable to
//		a text file
//
int cApplication::_CmdLine::WriteLog (char *ch_FileName, char *ch_Msg)
{
	HANDLE hFile;
	int cnt;
	DWORD BWritten, dwRead;
    char ch_Temp[OutputBlock];

	//	Look for an extension in that file name
	cnt = strlen (ch_FileName) - 1;
	while (ch_FileName[cnt] != '.') {
		if (ch_FileName[cnt] == '\0') break;
		cnt--;
	}
	//	if found lose it (the extension, I mean)
	if (cnt > 0) {
		cnt++;
		ch_FileName[cnt] = '\0';
	}
	strcat (ch_FileName, "LOG");												//	Put the new extension on
	hFile = CreateFile (ch_FileName,
		GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ,
		NULL, OPEN_ALWAYS, 
		FILE_ATTRIBUTE_NORMAL, NULL);											//	Open or create the file

    //  Seek to the end of the file so will append to log
    for(;;)
    {
        ReadFile(hFile, ch_Temp, sizeof(ch_Temp)-1, &dwRead, NULL); 
        if( dwRead == 0 )
        { 
            break;
	    }
    }

	WriteFile (hFile, ch_Msg, _msize (ch_Msg), &BWritten, NULL);
	CloseHandle (hFile);

	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cApplication::CmdLine::DeleteLog ()
//		Deletes the output .LOG file
//
int cApplication::_CmdLine::DeleteLog (char *ch_FileName)
{
    int cnt;
    char l_FileName[256];

    strcpy(l_FileName, ch_FileName);
	//	Look for an extension in that file name
	cnt = strlen (l_FileName) - 1;
	while (l_FileName[cnt] != '.') {
		if (l_FileName[cnt] == '\0') break;
		cnt--;
	}
	//	if found lose it (the extension, I mean)
	if (cnt > 0) {
		cnt++;
		l_FileName[cnt] = '\0';
	}
	strcat (l_FileName, "LOG");												//	Put the new extension on
	DeleteFile (l_FileName);

	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//		_FSettings - Controls the management of the settings files
//

//////////////////////////////////////////////////////////////////////////
//
//	cApplication::_FSettings::GetMaster ()
//		Gets the settings from the .SET file for a dir
//
int cApplication::_FSettings::GetMaster ()
{
	char ch_MstrOptFile[_MAX_PATH];													//	Holds the full path and file name of the WISPTran.OPT file
	char ch_MasterOpt[1024];															//	Holds the contents of the file
	WIN32_FIND_DATA FileInfo;														//	Holds info returned by FindFirstFile
	HANDLE hMasterOpt;																//	Handle to the file WISPTran.OPT
	DWORD BytesWritten, BytesRead;												//	Used by Read/WriteFile
	//==Open/Create the file WISPTran.SET - if it doesn't exist in the dir then it is created
	strcpy (ch_MstrOptFile, 
		cWnd.OptionWnd.ch_DirPath);								//	Copies the directory path to the string
	strcat (ch_MstrOptFile, "WISPTran.SET");										//	Adds the file name to the string
	hMasterOpt = CreateFile (ch_MstrOptFile,
		GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ,
		NULL, OPEN_ALWAYS, 
		FILE_ATTRIBUTE_NORMAL, NULL);											//	Open or create the file
	FindFirstFile (ch_MstrOptFile, &FileInfo);									//	Gets info on the wisptran.set file
	//==If the file size returned by FindFirstFile is 0 - 
	//		meaning the file didn't exist===============================
	if (FileInfo.nFileSizeLow == 0) {
		char ch_Output[1024];															//	Buffer to hold text to write to the file
		ZeroMemory (ch_Output, sizeof (ch_Output));
		sprintf (ch_Output, 
			"Default WISP options for directory %s\r\n"
			"Flags:\r\n",												
			cWnd.OptionWnd.ch_DirPath);							//	Compose a string containing the directory path
		WriteFile (hMasterOpt,	ch_Output, strlen (ch_Output),	
			&BytesWritten, NULL);													//	Write string to file
	}
	//==...otherwise, if the file exists==============================
	else {
		ReadFile (hMasterOpt, ch_MasterOpt, 1024, &BytesRead, NULL);	//	Read the file into a character buffer
		ch_MasterOpt[BytesRead] = '\0';											//	Null terminate the end of the buffer
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/1") != NULL) {
			cApp.CmdLine.WOptions->_1 = TRUE;									//	Set the state to true in the Options struct
			PostMessage (cWnd.OptionWnd.WCtls._1,
				BM_SETCHECK, BST_CHECKED, 0);									//	Check the check box
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_1 = FALSE;
			PostMessage (cWnd.OptionWnd.WCtls._1,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/4") != NULL) {
			cApp.CmdLine.WOptions->_4 = TRUE;
			PostMessage (cWnd.OptionWnd.WCtls._4,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_4 = FALSE;
			PostMessage (cWnd.OptionWnd.WCtls._4,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/F") != NULL) {
			cApp.CmdLine.WOptions->_F = TRUE;
			PostMessage (cWnd.OptionWnd.WCtls._F,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_F = FALSE;
			PostMessage (cWnd.OptionWnd.WCtls._F,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/D") != NULL) {
			cApp.CmdLine.WOptions->_D = TRUE;
			PostMessage (cWnd.OptionWnd.WCtls._D,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_D = FALSE;
			PostMessage (cWnd.OptionWnd.WCtls._D,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/e") != NULL) {
			cApp.CmdLine.WOptions->_e = TRUE;
			PostMessage (cWnd.OptionWnd.WCtls._e,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_e = FALSE;
			PostMessage (cWnd.OptionWnd.WCtls._e,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/f") != NULL) {
			cApp.CmdLine.WOptions->_f = TRUE;
			PostMessage (cWnd.OptionWnd.WCtls._f,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_f = FALSE;
			PostMessage (cWnd.OptionWnd.WCtls._f,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/l") != NULL) {
			cApp.CmdLine.WOptions->_l = TRUE;
			PostMessage (cWnd.OptionWnd.WCtls._l,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_l = FALSE;
			PostMessage (cWnd.OptionWnd.WCtls._l,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/L") != NULL) {
			cApp.CmdLine.WOptions->_L = TRUE;
			PostMessage (cWnd.OptionWnd.WCtls._L,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_L = FALSE;
			PostMessage (cWnd.OptionWnd.WCtls._L,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/m") != NULL) {
			cApp.CmdLine.WOptions->_m = TRUE;
			PostMessage (cWnd.OptionWnd.WCtls._m,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_m = FALSE;
			PostMessage (cWnd.OptionWnd.WCtls._m,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/M") != NULL) {
			cApp.CmdLine.WOptions->_M = TRUE;
			PostMessage (cWnd.OptionWnd.WCtls._M,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_M = FALSE;
			PostMessage (cWnd.OptionWnd.WCtls._M,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/q") != NULL) {
			cApp.CmdLine.WOptions->_q = TRUE;
			PostMessage (cWnd.OptionWnd.WCtls._q,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_q = FALSE;
			PostMessage (cWnd.OptionWnd.WCtls._q,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/G4") != NULL) {
			cApp.CmdLine.WOptions->_G4 = TRUE;
			PostMessage (cWnd.OptionWnd.WCtls._G4,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_G4 = FALSE;
			PostMessage (cWnd.OptionWnd.WCtls._G4,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/S") != NULL) {
			cApp.CmdLine.WOptions->_S = TRUE;
			PostMessage (cWnd.OptionWnd.WCtls._S,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_S = FALSE;
			PostMessage (cWnd.OptionWnd.WCtls._S,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/T") != NULL) {
			cApp.CmdLine.WOptions->_T = TRUE;
			PostMessage (cWnd.OptionWnd.WCtls._T,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_T = FALSE;
			PostMessage (cWnd.OptionWnd.WCtls._T,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/w") != NULL) {
			cApp.CmdLine.WOptions->_w = TRUE;
			PostMessage (cWnd.OptionWnd.WCtls._w,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_w = FALSE;
			PostMessage (cWnd.OptionWnd.WCtls._w,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/x") != NULL) {
			cApp.CmdLine.WOptions->_x = TRUE;
			PostMessage (cWnd.OptionWnd.WCtls._x,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_x = FALSE;
			PostMessage (cWnd.OptionWnd.WCtls._x,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/X") != NULL) {
			cApp.CmdLine.WOptions->_X = TRUE;
			PostMessage (cWnd.OptionWnd.WCtls._X,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_X = FALSE;
			PostMessage (cWnd.OptionWnd.WCtls._X,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==Now check the options that have accompanying variables=====
		char *ch_Arg;
		//	Check if the KEY file option exists - if so copy the option to the edit control
		if ((ch_Arg = strstr (ch_MasterOpt, "/K")) != NULL) {
			int StrStart, StrStop, cnt;
			StrStart = cnt = 2;														//	Set to first letter of argument
			while (ch_Arg[cnt] != '/') {
				if (ch_Arg[cnt] == '\0') break;									//	Break when the end of the file name is found
				cnt++;
			}
			StrStop = cnt;
			memcpy (cApp.CmdLine.WOptions->ch_K, 
				&ch_Arg[StrStart], StrStop - StrStart);							//	Copy the string to the options struct
			SendMessage (cWnd.OptionWnd.WCtls._K,
				WM_SETTEXT, 0, (LPARAM) cApp.CmdLine.WOptions->ch_K);		//	Display it in the edit box
		}
		//	Otherwise set the var the options struct to null
		else {
			ZeroMemory ( cApp.CmdLine.WOptions->ch_K, 
				sizeof ( cApp.CmdLine.WOptions->ch_K));
			SendMessage ( cWnd.OptionWnd.WCtls._K,
				WM_SETTEXT, 0, (LPARAM) "" );
		}
		//==If a specific option file is set
		if ((ch_Arg = strstr (ch_MasterOpt, "/O")) != NULL) {
			int StrStart, StrStop, cnt;
			StrStart = cnt = 2;														//	Set to first letter of argument
			while (ch_Arg[cnt] != '/') {
				if (ch_Arg[cnt] == '\0') break;									//	Break when the end of the file name is found
				cnt++;
			}
			StrStop = cnt;
			memcpy (cApp.CmdLine.WOptions->ch_O, 
				&ch_Arg[StrStart], StrStop - StrStart);							//	Copy the string to the options struct
			SendMessage (cWnd.OptionWnd.WCtls._O,
				WM_SETTEXT, 0, (LPARAM) cApp.CmdLine.WOptions->ch_O);		//	Display it in the edit box
		}
		//	Otherwise set the var the options struct to null
		else {
			ZeroMemory (cApp.CmdLine.WOptions->ch_O, 
				sizeof (cApp.CmdLine.WOptions->ch_O));
			SendMessage (cWnd.OptionWnd.WCtls._O,
				WM_SETTEXT, 0, (LPARAM) "");										//	Clear the edit box
		}
		//==Check for settings for this option=========================
		if ((ch_Arg = strstr (ch_MasterOpt, "/P")) != NULL) {
			int StrStart, StrStop, cnt;
			StrStart = cnt = 2;														//	Set to first letter of argument
			while (ch_Arg[cnt] != '/') {
				if (ch_Arg[cnt] == '\0') break;									//	Break when the end of the file name is found
				cnt++;
			}
			StrStop = cnt;
			memcpy (cApp.CmdLine.WOptions->ch_P, 
				&ch_Arg[StrStart], StrStop - StrStart);							//	Copy the string to the options struct
			SendMessage (cWnd.OptionWnd.WCtls._P,
				WM_SETTEXT, 0, (LPARAM) cApp.CmdLine.WOptions->ch_P);		//	Display it in the edit box
		}
		//	Otherwise set the var the options struct to null
		else {
			ZeroMemory (cApp.CmdLine.WOptions->ch_P, 
				sizeof (cApp.CmdLine.WOptions->ch_P));
			SendMessage (cWnd.OptionWnd.WCtls._P,
				WM_SETTEXT, 0, (LPARAM) "");										//	Clear the edit box
		}
		//
		if ((ch_Arg = strstr (ch_MasterOpt, "/V")) != NULL) {
			int StrStart, StrStop, cnt;
			StrStart = cnt = 2;														//	Set to first letter of argument
			while (ch_Arg[cnt] != '/') {
				if (ch_Arg[cnt] == '\0') break;									//	Break when the end of the file name is found
				cnt++;
			}
			StrStop = cnt;
			memcpy (cApp.CmdLine.WOptions->ch_V, 
				&ch_Arg[StrStart], StrStop - StrStart);							//	Copy the string to the options struct
			SendMessage (cWnd.OptionWnd.WCtls._V,
				WM_SETTEXT, 0, (LPARAM) cApp.CmdLine.WOptions->ch_V);		//	Display it in the edit box
		}
		//	Otherwise set the var the options struct to null
		else {
			ZeroMemory (cApp.CmdLine.WOptions->ch_V, 
				sizeof (cApp.CmdLine.WOptions->ch_V));
			SendMessage (cWnd.OptionWnd.WCtls._V,
				WM_SETTEXT, 0, (LPARAM) "");										//	Clear the edit box
		}
		//
		if ((ch_Arg = strstr (ch_MasterOpt, "/W")) != NULL) {
			int StrStart, StrStop, cnt;
			StrStart = cnt = 2;														//	Set to first letter of argument
			while (ch_Arg[cnt] != '/') {
				if (ch_Arg[cnt] == '\0') break;									//	Break when the end of the file name is found
				cnt++;
			}
			StrStop = cnt;
			memcpy (cApp.CmdLine.WOptions->ch_W, 
				&ch_Arg[StrStart], StrStop - StrStart);							//	Copy the string to the options struct
			SendMessage (cWnd.OptionWnd.WCtls._W,
				WM_SETTEXT, 0, (LPARAM) cApp.CmdLine.WOptions->ch_W);		//	Display it in the edit box
		}
		//	Otherwise set the var the options struct to null
		else {
			ZeroMemory (cApp.CmdLine.WOptions->ch_W, 
				sizeof (cApp.CmdLine.WOptions->ch_W));
			SendMessage (cWnd.OptionWnd.WCtls._W,
				WM_SETTEXT, 0, (LPARAM) "");										//	Clear the edit box
		}
		//
		if ((ch_Arg = strstr (ch_MasterOpt, "/I")) != NULL) {
			int StrStart, StrStop, cnt;
			StrStart = cnt = 2;														//	Set to first letter of argument
			while (ch_Arg[cnt] != '/') {
				if (ch_Arg[cnt] == '\0') break;									//	Break when the end of the file name is found
				cnt++;
			}
			StrStop = cnt;
			memcpy (cApp.CmdLine.WOptions->ch_I, 
				&ch_Arg[StrStart], StrStop - StrStart);							//	Copy the string to the options struct
			SendMessage (cWnd.OptionWnd.WCtls._I,
				WM_SETTEXT, 0, (LPARAM) cApp.CmdLine.WOptions->ch_I);		//	Display it in the edit box
		}
		//	Otherwise set the var the options struct to null
		else {
			ZeroMemory (cApp.CmdLine.WOptions->ch_I, 
				sizeof (cApp.CmdLine.WOptions->ch_I));
			SendMessage (cWnd.OptionWnd.WCtls._I,
				WM_SETTEXT, 0, (LPARAM) "");										//	Clear the edit box
		}
	}
	CloseHandle (hMasterOpt);
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	cApplication::_FSettings::SetMaster ()
//		Saves the settings for a dir
//
int cApplication::_FSettings::SetMaster ()
{
	char ch_MstrOptFile[_MAX_PATH];													//	Holds the full path and file name of the WISPTran.OPT file
	char ch_MasterOpt[1024];															//	Holds the contents of the file
	HANDLE hMasterOpt;																//	Handle to the file WISPTran.OPT
	DWORD BytesWritten, BytesRead;												//	Used by Read/WriteFile
	char *ch_StartFlags;
	int cnt;
	//==Open the file WISPTran.SET - if it doesn't exist in the dir then it is created
	strcpy (ch_MstrOptFile, 
		cWnd.OptionWnd.ch_DirPath);								//	Copies the directory path to the string
	strcat (ch_MstrOptFile, "WISPTran.SET");										//	Adds the file name to the string

	hMasterOpt = CreateFile (ch_MstrOptFile,
		GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ,
		NULL, OPEN_EXISTING, 
		FILE_ATTRIBUTE_NORMAL, NULL);										//	Open the file
	if( hMasterOpt == INVALID_HANDLE_VALUE )
	{
		int err;

		err = GetLastError();		/* Look in ../MSDEV/src/include/winerror.h */
		cApp.CloseApp ( 3002, err );			/* process error */
	}

	//	FindFirstFile (MasterOpt, &FileInfo);									//	Gets info on the wisptran.set file
	ReadFile (hMasterOpt, ch_MasterOpt, 1024, &BytesRead, NULL);	//	Read the file into a character buffer
	ch_MasterOpt[BytesRead] = '\0';											//	Null terminate the end of the buffer
	ch_StartFlags = strstr (ch_MasterOpt, "Flags:");
	ch_StartFlags[7] = '\0';
	ch_StartFlags = ch_StartFlags + 7;
	//==Go to the end of the file
	if (cApp.CmdLine.WOptions->_1 == TRUE) strcat (ch_MasterOpt, "/1");
	if (cApp.CmdLine.WOptions->_4 == TRUE) strcat (ch_MasterOpt, "/4");
	if (cApp.CmdLine.WOptions->_F == TRUE) strcat (ch_MasterOpt, "/F");
	if (cApp.CmdLine.WOptions->_D == TRUE) strcat (ch_MasterOpt, "/D");
	if (cApp.CmdLine.WOptions->_e == TRUE) strcat (ch_MasterOpt, "/e");
	if (cApp.CmdLine.WOptions->_f == TRUE) strcat (ch_MasterOpt, "/f");
	if (cApp.CmdLine.WOptions->_l == TRUE) strcat (ch_MasterOpt, "/l");
	if (cApp.CmdLine.WOptions->_L == TRUE) strcat (ch_MasterOpt, "/L");
	if (cApp.CmdLine.WOptions->_m == TRUE) strcat (ch_MasterOpt, "/m");
	if (cApp.CmdLine.WOptions->_M == TRUE) strcat (ch_MasterOpt, "/M");
	if (cApp.CmdLine.WOptions->_q == TRUE) strcat (ch_MasterOpt, "/q");
	if (cApp.CmdLine.WOptions->_G4 == TRUE) strcat (ch_MasterOpt, "/G4");
	if (cApp.CmdLine.WOptions->_S == TRUE) strcat (ch_MasterOpt, "/S");
	if (cApp.CmdLine.WOptions->_T == TRUE) strcat (ch_MasterOpt, "/T");
	if (cApp.CmdLine.WOptions->_w == TRUE) strcat (ch_MasterOpt, "/w");
	if (cApp.CmdLine.WOptions->_x == TRUE) strcat (ch_MasterOpt, "/x");
	if (cApp.CmdLine.WOptions->_X == TRUE) strcat (ch_MasterOpt, "/X");

	if (strlen (cApp.CmdLine.WOptions->ch_K) > 0) {
		strcat (ch_MasterOpt, "/K");
		strcat (ch_MasterOpt, cApp.CmdLine.WOptions->ch_K);
	}
	if (strlen (cApp.CmdLine.WOptions->ch_O) > 0) {
		strcat (ch_MasterOpt, "/O");
		strcat (ch_MasterOpt, cApp.CmdLine.WOptions->ch_O);
	}
	if (strlen (cApp.CmdLine.WOptions->ch_P) > 0) {
		strcat (ch_MasterOpt, "/P");
		strcat (ch_MasterOpt, cApp.CmdLine.WOptions->ch_P);
	}
	if (strlen (cApp.CmdLine.WOptions->ch_V) > 0) {
		strcat (ch_MasterOpt, "/V");
		strcat (ch_MasterOpt, cApp.CmdLine.WOptions->ch_V);
	}
	if (strlen (cApp.CmdLine.WOptions->ch_W) > 0) {
		strcat (ch_MasterOpt, "/W");
		strcat (ch_MasterOpt, cApp.CmdLine.WOptions->ch_W);
	}
	if (strlen (cApp.CmdLine.WOptions->ch_I) > 0) {
		strcat (ch_MasterOpt, "/I");
		strcat (ch_MasterOpt, cApp.CmdLine.WOptions->ch_I);
	}
	//	Write the data to the file
	cnt = 0;
	while (ch_StartFlags[cnt] != '\0') {
		cnt++;
	}
	SetFilePointer (hMasterOpt, 0, 0, FILE_BEGIN);
	WriteFile (hMasterOpt, ch_MasterOpt, strlen (ch_MasterOpt),
		&BytesWritten, NULL);
	SetEndOfFile (hMasterOpt);
	CloseHandle (hMasterOpt);
	return 0;
}
/////////////////////////////////////////////////////////////////////
//
//	cApplication::_FSettings::GetSingle
//		Gets the settings for a specific file
//
int cApplication::_FSettings::GetSingle (char *ch_FileName)
{
	char ch_MstrOptFile[_MAX_PATH];													//	Holds the full path and file name of the WISPTran.OPT file
	char ch_MasterOpt[1024],
		ch_OldFileName[256];																//	Holds the contents of the file
	WIN32_FIND_DATA FileInfo;														//	Holds info returned by FindFirstFile
	HANDLE hMasterOpt;																//	Handle to the file WISPTran.OPT
	DWORD BytesWritten, BytesRead;												//	Used by Read/WriteFile
	int cnt = 0;																		//	Generic Counter
	//==Open/Create the file WISPTran.SET - if it doesn't exist in the dir then it is created
	strcpy (ch_MstrOptFile, cWnd.OptionWnd.ch_DirPath);		//	Copies the directory path to the string
	strcpy (ch_OldFileName, ch_FileName);
	cnt = strlen (ch_FileName);
	while (ch_FileName[cnt] != '.') {
		if (cnt == 0) break;
		cnt--;
	}
	if (cnt > 0) ch_FileName[cnt] = '\0';
	strcat (ch_FileName, ".SET");
	strcat (ch_MstrOptFile, ch_FileName);												//	Adds the file name to the string
	hMasterOpt = CreateFile (ch_MstrOptFile,
		GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ,
		NULL, OPEN_ALWAYS, 
		FILE_ATTRIBUTE_NORMAL, NULL);											//	Open or create the file
	FindFirstFile (ch_MstrOptFile, &FileInfo);									//	Gets info on the wisptran.set file
	//==If the file size returned by FindFirstFile is 0 - 
	//		meaning the file didn't exist===============================
	if (FileInfo.nFileSizeLow == 0) {
		char ch_Output[1024];															//	Buffer to hold text to write to the file
		ZeroMemory (ch_Output, sizeof (ch_Output));
		sprintf (ch_Output, 
			"Default WISP options for file: %s\r\n"
			"Flags:\r\n",												
			ch_OldFileName);							//	Compose a string containing the directory path
		WriteFile (hMasterOpt,	ch_Output, strlen (ch_Output),	
			&BytesWritten, NULL);													//	Write string to file
//		CloseHandle (hMasterOpt);												//	Close the file
	}
	//==...otherwise, if the file exists==============================
	else {
		ReadFile (hMasterOpt, ch_MasterOpt, 1024, &BytesRead, NULL);	//	Read the file into a character buffer
		ch_MasterOpt[BytesRead] = '\0';											//	Null terminate the end of the buffer
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/1") != NULL) {
			cApp.CmdLine.WOptions->_1 = TRUE;									//	Set the state to true in the Options struct
			SendMessage (cWnd.OptionWnd.WCtls._1,
				BM_SETCHECK, BST_CHECKED, 0);									//	Check the check box
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_1 = FALSE;
			SendMessage (cWnd.OptionWnd.WCtls._1,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/4") != NULL) {
			cApp.CmdLine.WOptions->_4 = TRUE;
			SendMessage (cWnd.OptionWnd.WCtls._4,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_4 = FALSE;
			SendMessage (cWnd.OptionWnd.WCtls._4,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/F") != NULL) {
			cApp.CmdLine.WOptions->_F = TRUE;
			SendMessage (cWnd.OptionWnd.WCtls._F,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_F = FALSE;
			SendMessage (cWnd.OptionWnd.WCtls._F,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/D") != NULL) {
			cApp.CmdLine.WOptions->_D = TRUE;
			SendMessage (cWnd.OptionWnd.WCtls._D,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_D = FALSE;
			SendMessage (cWnd.OptionWnd.WCtls._D,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/e") != NULL) {
			cApp.CmdLine.WOptions->_e = TRUE;
			SendMessage (cWnd.OptionWnd.WCtls._e,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_e = FALSE;
			SendMessage (cWnd.OptionWnd.WCtls._e,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/f") != NULL) {
			cApp.CmdLine.WOptions->_f = TRUE;
			SendMessage (cWnd.OptionWnd.WCtls._f,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_f = FALSE;
			SendMessage (cWnd.OptionWnd.WCtls._f,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/l") != NULL) {
			cApp.CmdLine.WOptions->_l = TRUE;
			SendMessage (cWnd.OptionWnd.WCtls._l,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_l = FALSE;
			SendMessage (cWnd.OptionWnd.WCtls._l,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/L") != NULL) {
			cApp.CmdLine.WOptions->_L = TRUE;
			SendMessage (cWnd.OptionWnd.WCtls._L,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_L = FALSE;
			SendMessage (cWnd.OptionWnd.WCtls._L,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/m") != NULL) {
			cApp.CmdLine.WOptions->_m = TRUE;
			SendMessage (cWnd.OptionWnd.WCtls._m,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_m = FALSE;
			SendMessage (cWnd.OptionWnd.WCtls._m,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/M") != NULL) {
			cApp.CmdLine.WOptions->_M = TRUE;
			SendMessage (cWnd.OptionWnd.WCtls._M,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_M = FALSE;
			SendMessage (cWnd.OptionWnd.WCtls._M,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/q") != NULL) {
			cApp.CmdLine.WOptions->_q = TRUE;
			SendMessage (cWnd.OptionWnd.WCtls._q,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_q = FALSE;
			SendMessage (cWnd.OptionWnd.WCtls._q,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/G4") != NULL) {
			cApp.CmdLine.WOptions->_G4 = TRUE;
			SendMessage (cWnd.OptionWnd.WCtls._G4,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_G4 = FALSE;
			SendMessage (cWnd.OptionWnd.WCtls._G4,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/S") != NULL) {
			cApp.CmdLine.WOptions->_S = TRUE;
			SendMessage (cWnd.OptionWnd.WCtls._S,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_S = FALSE;
			SendMessage (cWnd.OptionWnd.WCtls._S,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/T") != NULL) {
			cApp.CmdLine.WOptions->_T = TRUE;
			SendMessage (cWnd.OptionWnd.WCtls._T,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_T = FALSE;
			SendMessage (cWnd.OptionWnd.WCtls._T,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/w") != NULL) {
			cApp.CmdLine.WOptions->_w = TRUE;
			SendMessage (cWnd.OptionWnd.WCtls._w,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_w = FALSE;
			SendMessage (cWnd.OptionWnd.WCtls._w,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/x") != NULL) {
			cApp.CmdLine.WOptions->_x = TRUE;
			SendMessage (cWnd.OptionWnd.WCtls._x,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_x = FALSE;
			SendMessage (cWnd.OptionWnd.WCtls._x,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==if an option is set in the file then check the checkbox===
		if (strstr (ch_MasterOpt, "/X") != NULL) {
			cApp.CmdLine.WOptions->_X = TRUE;
			SendMessage (cWnd.OptionWnd.WCtls._X,
				BM_SETCHECK, BST_CHECKED, 0);
		}
		//==Otherwise uncheck the checkbox=============================
		else {
			cApp.CmdLine.WOptions->_X = FALSE;
			SendMessage (cWnd.OptionWnd.WCtls._X,
				BM_SETCHECK, BST_UNCHECKED, 0);
		}
		//==Now check the options that have accompanying variables=====
		char *ch_Arg;
		//	Check if the KEY file option exists - if so copy the option to the edit control
		if ((ch_Arg = strstr (ch_MasterOpt, "/K")) != NULL) {
			int StrStart, StrStop, cnt;
			StrStart = cnt = 2;														//	Set to first letter of argument
			while (ch_Arg[cnt] != '/') {
				if (ch_Arg[cnt] == '\0') break;									//	Break when the end of the file name is found
				cnt++;
			}
			StrStop = cnt;
			memcpy (cApp.CmdLine.WOptions->ch_K, 
				&ch_Arg[StrStart], StrStop - StrStart);							//	Copy the string to the options struct
			SendMessage (cWnd.OptionWnd.WCtls._K,
				WM_SETTEXT, 0, (LPARAM) cApp.CmdLine.WOptions->ch_K);		//	Display it in the edit box
		}
		//	Otherwise set the var the options struct to null
		else {
			ZeroMemory (cApp.CmdLine.WOptions->ch_K, 
				sizeof (cApp.CmdLine.WOptions->ch_K));
			SendMessage (cWnd.OptionWnd.WCtls._K,
				WM_SETTEXT, 0, (LPARAM) "");									//	Clear the edit box
		}
		//==If a specific option file is set
		if ((ch_Arg = strstr (ch_MasterOpt, "/O")) != NULL) {
			int StrStart, StrStop, cnt;
			StrStart = cnt = 2;														//	Set to first letter of argument
			while (ch_Arg[cnt] != '/') {
				if (ch_Arg[cnt] == '\0') break;									//	Break when the end of the file name is found
				cnt++;
			}
			StrStop = cnt;
			memcpy (cApp.CmdLine.WOptions->ch_O, 
				&ch_Arg[StrStart], StrStop - StrStart);							//	Copy the string to the options struct
			SendMessage (cWnd.OptionWnd.WCtls._O,
				WM_SETTEXT, 0, (LPARAM) cApp.CmdLine.WOptions->ch_O);		//	Display it in the edit box
		}
		//	Otherwise set the var the options struct to null
		else {
			ZeroMemory (cApp.CmdLine.WOptions->ch_O, 
				sizeof (cApp.CmdLine.WOptions->ch_O));
			SendMessage (cWnd.OptionWnd.WCtls._O,
				WM_SETTEXT, 0, (LPARAM) "");										//	Clear the edit box
		}
		//==Check for settings for this option=========================
		if ((ch_Arg = strstr (ch_MasterOpt, "/P")) != NULL) {
			int StrStart, StrStop, cnt;
			StrStart = cnt = 2;														//	Set to first letter of argument
			while (ch_Arg[cnt] != '/') {
				if (ch_Arg[cnt] == '\0') break;									//	Break when the end of the file name is found
				cnt++;
			}
			StrStop = cnt;
			memcpy (cApp.CmdLine.WOptions->ch_P, 
				&ch_Arg[StrStart], StrStop - StrStart);							//	Copy the string to the options struct
			SendMessage (cWnd.OptionWnd.WCtls._P,
				WM_SETTEXT, 0, (LPARAM) cApp.CmdLine.WOptions->ch_P);		//	Display it in the edit box
		}
		//	Otherwise set the var the options struct to null
		else {
			ZeroMemory (cApp.CmdLine.WOptions->ch_P, 
				sizeof (cApp.CmdLine.WOptions->ch_P));
			SendMessage (cWnd.OptionWnd.WCtls._P,
				WM_SETTEXT, 0, (LPARAM) "");										//	Clear the edit box
		}
		//
		if ((ch_Arg = strstr (ch_MasterOpt, "/V")) != NULL) {
			int StrStart, StrStop, cnt;
			StrStart = cnt = 2;														//	Set to first letter of argument
			while (ch_Arg[cnt] != '/') {
				if (ch_Arg[cnt] == '\0') break;									//	Break when the end of the file name is found
				cnt++;
			}
			StrStop = cnt;
			memcpy (cApp.CmdLine.WOptions->ch_V, 
				&ch_Arg[StrStart], StrStop - StrStart);							//	Copy the string to the options struct
			SendMessage (cWnd.OptionWnd.WCtls._V,
				WM_SETTEXT, 0, (LPARAM) cApp.CmdLine.WOptions->ch_V);		//	Display it in the edit box
		}
		//	Otherwise set the var the options struct to null
		else {
			ZeroMemory (cApp.CmdLine.WOptions->ch_V, 
				sizeof (cApp.CmdLine.WOptions->ch_V));
			SendMessage (cWnd.OptionWnd.WCtls._V,
				WM_SETTEXT, 0, (LPARAM) "");										//	Clear the edit box
		}
		//
		if ((ch_Arg = strstr (ch_MasterOpt, "/W")) != NULL) {
			int StrStart, StrStop, cnt;
			StrStart = cnt = 2;														//	Set to first letter of argument
			while (ch_Arg[cnt] != '/') {
				if (ch_Arg[cnt] == '\0') break;									//	Break when the end of the file name is found
				cnt++;
			}
			StrStop = cnt;
			memcpy (cApp.CmdLine.WOptions->ch_W, 
				&ch_Arg[StrStart], StrStop - StrStart);							//	Copy the string to the options struct
			SendMessage (cWnd.OptionWnd.WCtls._W,
				WM_SETTEXT, 0, (LPARAM) cApp.CmdLine.WOptions->ch_W);		//	Display it in the edit box
		}
		//	Otherwise set the var the options struct to null
		else {
			ZeroMemory (cApp.CmdLine.WOptions->ch_W, 
				sizeof (cApp.CmdLine.WOptions->ch_W));
			SendMessage (cWnd.OptionWnd.WCtls._W,
				WM_SETTEXT, 0, (LPARAM) "");										//	Clear the edit box
		}
		//
		if ((ch_Arg = strstr (ch_MasterOpt, "/I")) != NULL) {
			int StrStart, StrStop, cnt;
			StrStart = cnt = 2;														//	Set to first letter of argument
			while (ch_Arg[cnt] != '/') {
				if (ch_Arg[cnt] == '\0') break;									//	Break when the end of the file name is found
				cnt++;
			}
			StrStop = cnt;
			memcpy (cApp.CmdLine.WOptions->ch_I, 
				&ch_Arg[StrStart], StrStop - StrStart);							//	Copy the string to the options struct
			SendMessage (cWnd.OptionWnd.WCtls._I,
				WM_SETTEXT, 0, (LPARAM) cApp.CmdLine.WOptions->ch_I);		//	Display it in the edit box
		}
		//	Otherwise set the var the options struct to null
		else {
			ZeroMemory (cApp.CmdLine.WOptions->ch_I, 
				sizeof (cApp.CmdLine.WOptions->ch_I));
			SendMessage (cWnd.OptionWnd.WCtls._I,
				WM_SETTEXT, 0, (LPARAM) "");										//	Clear the edit box
		}
	}
	CloseHandle (hMasterOpt);
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	cApplication::_FSettings::SetSingle ()
//		Saves the settings for a specific file
//
int cApplication::_FSettings::SetSingle (char *ch_FileName)
{
	char ch_MstrOptFile[_MAX_PATH];													//	Holds the full path and file name of the WISPTran.OPT file
	char ch_MasterOpt[1024],
		ch_OldFileName[_MAX_PATH];															//	Holds the contents of the file
	HANDLE hMasterOpt;																//	Handle to the file WISPTran.OPT
	DWORD BytesWritten;												//	Used by Read/WriteFile
	int cnt = 0;
	//==Open the file WISPTran.SET - if it doesn't exist in the dir then it is created
	strcpy (ch_MstrOptFile, 
		cWnd.OptionWnd.ch_DirPath);								//	Copies the directory path to the string
	strcpy (ch_OldFileName, ch_FileName);
	cnt = strlen (ch_FileName);
	while (ch_FileName[cnt] != '.') {
		if (cnt == 0) break;
		cnt--;
	}
	if (cnt > 0) ch_FileName[cnt] = '\0';
	strcat (ch_FileName, ".SET");

	sprintf (ch_MasterOpt, 
		"Saved WISP options for file: %s\r\n"
		"Flags:\r\n",												
		ch_OldFileName);							//	Compose a string containing the directory path

	strcat (ch_MstrOptFile, ch_FileName);												//	Adds the file name to the string
	hMasterOpt = CreateFile (ch_MstrOptFile,
		GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ,
		NULL, CREATE_ALWAYS, 
		FILE_ATTRIBUTE_NORMAL, NULL);										//	Open the file
//	ReadFile (hMasterOpt, chMasterOpt, 1024, &BytesRead, NULL);	//	Read the file into a character buffer
//	chMasterOpt[BytesRead] = '\0';											//	Null terminate the end of the buffer
	char *ch_StartFlags = strstr (ch_MasterOpt, "Flags:\r\n");
	ch_StartFlags[7] = '\0';
	//==Go to the end of the file
	if (cApp.CmdLine.WOptions->_1 == TRUE) strcat (ch_MasterOpt, "/1");
	if (cApp.CmdLine.WOptions->_4 == TRUE) strcat (ch_MasterOpt, "/4");
	if (cApp.CmdLine.WOptions->_F == TRUE) strcat (ch_MasterOpt, "/F");
	if (cApp.CmdLine.WOptions->_D == TRUE) strcat (ch_MasterOpt, "/D");
	if (cApp.CmdLine.WOptions->_e == TRUE) strcat (ch_MasterOpt, "/e");
	if (cApp.CmdLine.WOptions->_f == TRUE) strcat (ch_MasterOpt, "/f");
	if (cApp.CmdLine.WOptions->_l == TRUE) strcat (ch_MasterOpt, "/l");
	if (cApp.CmdLine.WOptions->_L == TRUE) strcat (ch_MasterOpt, "/L");
	if (cApp.CmdLine.WOptions->_m == TRUE) strcat (ch_MasterOpt, "/m");
	if (cApp.CmdLine.WOptions->_M == TRUE) strcat (ch_MasterOpt, "/M");
	if (cApp.CmdLine.WOptions->_q == TRUE) strcat (ch_MasterOpt, "/q");
	if (cApp.CmdLine.WOptions->_G4 == TRUE) strcat (ch_MasterOpt, "/G4");
	if (cApp.CmdLine.WOptions->_S == TRUE) strcat (ch_MasterOpt, "/S");
	if (cApp.CmdLine.WOptions->_T == TRUE) strcat (ch_MasterOpt, "/T");
	if (cApp.CmdLine.WOptions->_w == TRUE) strcat (ch_MasterOpt, "/w");
	if (cApp.CmdLine.WOptions->_x == TRUE) strcat (ch_MasterOpt, "/x");
	if (cApp.CmdLine.WOptions->_X == TRUE) strcat (ch_MasterOpt, "/X");

	if (strlen (cApp.CmdLine.WOptions->ch_K) > 0) {
		strcat (ch_MasterOpt, "/K");
		strcat (ch_MasterOpt, cApp.CmdLine.WOptions->ch_K);
	}
	if (strlen (cApp.CmdLine.WOptions->ch_O) > 0) {
		strcat (ch_MasterOpt, "/O");
		strcat (ch_MasterOpt, cApp.CmdLine.WOptions->ch_O);
	}
	if (strlen (cApp.CmdLine.WOptions->ch_P) > 0) {
		strcat (ch_MasterOpt, "/P");
		strcat (ch_MasterOpt, cApp.CmdLine.WOptions->ch_P);
	}
	if (strlen (cApp.CmdLine.WOptions->ch_V) > 0) {
		strcat (ch_MasterOpt, "/V");
		strcat (ch_MasterOpt, cApp.CmdLine.WOptions->ch_V);
	}
	if (strlen (cApp.CmdLine.WOptions->ch_W) > 0) {
		strcat (ch_MasterOpt, "/W");
		strcat (ch_MasterOpt, cApp.CmdLine.WOptions->ch_W);
	}
	if (strlen (cApp.CmdLine.WOptions->ch_I) > 0) {
		strcat (ch_MasterOpt, "/I");
		strcat (ch_MasterOpt, cApp.CmdLine.WOptions->ch_I);
	}
	//	Write the data to the file
	WriteFile (hMasterOpt, ch_MasterOpt, strlen (ch_MasterOpt),
		&BytesWritten, NULL);
	CloseHandle (hMasterOpt);
	return 0;
}
///////////////////////////////////////////////////////////////////////////////
//
//	cApplication::_FSettings::ChkOptFileExist
//		Checks if a settings file exists for a specific source file
//
BOOL cApplication::_FSettings::ChkOptFileExist (char *ch_FileName)
{
	WIN32_FIND_DATA FileInfo;
	int cnt = 0;

	GetDlgItemText (cWnd.hWnd.hWISPOpWnd,
		GetDlgCtrlID (cWnd.OptionWnd.WCtls._TargetFile),
		ch_FileName, _MAX_PATH);
	cnt = strlen (ch_FileName);
	while (ch_FileName[cnt] != '.') {
		if (cnt == 0) break;
		cnt--;
	}
	if (cnt > 0) ch_FileName[cnt] = '\0';
	strcat (ch_FileName, ".SET");
	if (FindFirstFile (ch_FileName, &FileInfo) != INVALID_HANDLE_VALUE) {
		EnableMenuItem (cWnd.hAppMenu,
			mnu_Set_Ret_File, MF_BYCOMMAND | MF_ENABLED);
		return TRUE;
	}
	else {
		EnableMenuItem (cWnd.hAppMenu,
			mnu_Set_Ret_File, MF_BYCOMMAND | MF_GRAYED);
		return FALSE;
	}
}
//////////////////////////////////////////////////////////////////////////
//
//	cApplication::GetReg
//		Gets the registry settings
//
int cApplication::GetReg ()
{
	HKEY hKey;											//	Handle of the current registry key
	UCHAR RegVal[_MAX_PATH + 1];						//	Temp buffer for value retrieved from the registry
	DWORD szRegVal;								//	Size in bytes, of value to retrieve from registry

	//	Open the registry key
	RegOpenKeyEx ( HKEY_LOCAL_MACHINE, RK_WBin, 0, KEY_READ, &hKey );
	//	Put the path to the wisp.exe file into the global buffer
	szRegVal = sizeof(cApp.Env->RegInfo.ch_WPath)-1;
	if ( RegQueryValueEx ( hKey, "WISP",
			0, NULL, (unsigned char *) cApp.Env->RegInfo.ch_WPath, 
			&szRegVal ) != ERROR_SUCCESS ) {
		strcpy ( cApp.Env->RegInfo.ch_WPath, "" );
	}

	//	Get the COBOL compiler options
	if ( RegOpenKeyEx ( HKEY_LOCAL_MACHINE, RK_WBin_WTran_COBOL, 0,
		KEY_READ, &hKey ) == ERROR_SUCCESS ) {										//	If the key is successfully opened
		szRegVal = sizeof(RegVal)-1;
        RegQueryValueEx ( hKey, "Language", 0,
			NULL, (UCHAR *) RegVal, &szRegVal );										//	Get the COBOL language
		cApp.Env->RegInfo.COBOLLang = atoi ( (char *) RegVal );					//		and put it in the global struct
		//	Open the AcuCOBOL key
		if ( RegOpenKeyEx ( HKEY_LOCAL_MACHINE, RK_WBin_WTran_COBOL_Acu,
			0, KEY_READ, &hKey ) == ERROR_SUCCESS ) {								//	If the key opened successfully
			szRegVal = sizeof(cApp.Env->RegInfo.ch_ACPath)-1;
			RegQueryValueEx ( hKey, "COB", 0, NULL, (UCHAR *) 
				cApp.Env->RegInfo.ch_ACPath, &szRegVal );								//	Get the path to the COBOL executable
			if( strlen(cApp.Env->RegInfo.ch_ACPath) == 0 )
			{
				strcpy(cApp.Env->RegInfo.ch_ACPath, "ccbl32.exe");
			}

			szRegVal = sizeof(cApp.Env->RegInfo.ACOBOLFlags)-1;
			RegQueryValueEx ( hKey, "COBFLAGS", 0, NULL, (UCHAR *)
				cApp.Env->RegInfo.ACOBOLFlags, &szRegVal );							//	Get the COBOL flags
			if( strlen(cApp.Env->RegInfo.ACOBOLFlags) == 0 )
			{
				strcpy(cApp.Env->RegInfo.ACOBOLFlags, "-da4 -Za");
			}
			szRegVal = sizeof(cApp.Env->RegInfo.ACOBOLOutDir)-1;
			RegQueryValueEx ( hKey, "OUTDIR", 0, NULL, (UCHAR *)
				cApp.Env->RegInfo.ACOBOLOutDir, &szRegVal );							//	Get the COBOL output dir
			szRegVal = sizeof(cApp.Env->RegInfo.ACOBOLObjExt)-1;
			RegQueryValueEx ( hKey, "OBJEXT", 0, NULL, (UCHAR *)
				cApp.Env->RegInfo.ACOBOLObjExt, &szRegVal );							//	Get the COBOL output dir
		}
		//	Open the MicroFocus COBOL key
		if ( RegOpenKeyEx ( HKEY_LOCAL_MACHINE, RK_WBin_WTran_COBOL_MF,
			0, KEY_READ, &hKey ) == ERROR_SUCCESS ) {								//	If the key opened successfully
			szRegVal = sizeof(cApp.Env->RegInfo.ch_MCPath)-1;
			RegQueryValueEx ( hKey, "COB", 0, NULL, (UCHAR *)
				cApp.Env->RegInfo.ch_MCPath, &szRegVal );								//	Get the path to the COBOL executable
			if( strlen(cApp.Env->RegInfo.ch_MCPath) == 0 )
			{
				strcpy(cApp.Env->RegInfo.ch_MCPath, "cob.exe");
			}
			szRegVal = sizeof(cApp.Env->RegInfo.MCOBOLFlags)-1;
			RegQueryValueEx ( hKey, "COBFLAGS", 0, NULL, (UCHAR *)
				cApp.Env->RegInfo.MCOBOLFlags, &szRegVal );							//	Get the COBOL flags
			if( strlen(cApp.Env->RegInfo.MCOBOLFlags) == 0 )
			{
				strcpy(cApp.Env->RegInfo.MCOBOLFlags, "-i -a");
			}
			szRegVal = sizeof(cApp.Env->RegInfo.MCOBOLOutDir)-1;
			RegQueryValueEx ( hKey, "OUTDIR", 0, NULL, (UCHAR *)
				cApp.Env->RegInfo.MCOBOLOutDir, &szRegVal );							//	Get the COBOL output dir
			szRegVal = sizeof(cApp.Env->RegInfo.MCOBOLObjExt)-1;
			RegQueryValueEx ( hKey, "OBJEXT", 0, NULL, (UCHAR *)
				cApp.Env->RegInfo.MCOBOLObjExt, &szRegVal );							//	Get the COBOL output dir
		}
	}
	//	Open the WISPTran key
	DWORD Disp;
	RECT ScrRes;
	//	If the WISPTran key does not exist then create it
	RegCreateKeyEx ( HKEY_LOCAL_MACHINE, RK_WBin_WTran,
			0, NULL, REG_OPTION_NON_VOLATILE,
			KEY_ALL_ACCESS, NULL, &hKey, &Disp );
	GetWindowRect( GetDesktopWindow(), &ScrRes );
	//	If the HSize value cannot be read then set it to the default of Screen size
	szRegVal = sizeof(RegVal)-1;
	if ( RegQueryValueEx ( hKey, "HSize", 0, NULL,
		RegVal, &szRegVal ) != ERROR_SUCCESS ) {
		cApp.Env->RegInfo.InitHSize = ScrRes.right;
	}
	//	Place the value of the HSize var into a global var
	else
		cApp.Env->RegInfo.InitHSize = atoi ( (char *) RegVal );				//		and put it in the global struct
	szRegVal = sizeof(RegVal)-1;
	//	If the VSize value cannot be read then set it to the default of Screen size
	if ( RegQueryValueEx ( hKey, "VSize", 0, NULL,
		RegVal, &szRegVal ) != ERROR_SUCCESS ) {
		cApp.Env->RegInfo.InitVSize = ScrRes.bottom;
	}
	//	Place the value of VSize into the global var
	else 
		cApp.Env->RegInfo.InitVSize = atoi ( (char *) RegVal );				//		and put it in the global struct

	szRegVal = sizeof(RegVal)-1;
	if ( RegQueryValueEx ( hKey, "WorkDir", 0,
		NULL, RegVal, &szRegVal ) != ERROR_SUCCESS ){
		GetCurrentDirectory ( sizeof(RegVal)-1, (char *) RegVal );
	}
	SetCurrentDirectory ( (char *) RegVal );
	//	Close the handle
	RegCloseKey ( hKey );

	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	cApplication::SetReg
//		Saves the registry settings
//
int cApplication::SetReg ()
{
	HKEY hCurKey;
	char RegVal[_MAX_PATH];
	RECT WndSize;

	//	Open the registry key to get the wisp exec
	if ( RegOpenKeyEx ( HKEY_LOCAL_MACHINE, RK_WBin_WTran,
			0, KEY_ALL_ACCESS, &hCurKey ) == ERROR_SUCCESS ) {
		//	Save the current app window size
		GetWindowRect ( cWnd.hWnd.hShellWnd, &WndSize );
		_itoa ( (WndSize.right-WndSize.left), RegVal, 10 );
		RegSetValueEx ( hCurKey, "HSize", 0, REG_SZ, (UCHAR*)RegVal, strlen(RegVal)+1 );
		_itoa ( (WndSize.bottom-WndSize.top), RegVal, 10 );
		RegSetValueEx ( hCurKey, "VSize", 0, REG_SZ, (UCHAR*)RegVal, strlen(RegVal)+1 );
		GetWindowText ( cWnd.OptionWnd.WCtls._DirPath, RegVal, _MAX_PATH );
		if ( strlen ( RegVal ) == 0 ) {
			GetCurrentDirectory ( sizeof(RegVal)-1, RegVal );
		}
		RegSetValueEx ( hCurKey, "WorkDir", 0, REG_SZ, (UCHAR*)RegVal, strlen(RegVal)+1 );
		DWORD Disp;
		//	Open the COBOL key
		RegCreateKeyEx ( HKEY_LOCAL_MACHINE, RK_WBin_WTran_COBOL,
				0, NULL, REG_OPTION_NON_VOLATILE,
				KEY_ALL_ACCESS, NULL, &hCurKey, &Disp );
		//	Save the current COBOL settings
		if ( SendMessage ( cWnd.OptionWnd.CCtls._AcuBtn, 
				BM_GETCHECK, 0, 0 ) == BST_CHECKED ) {
            strcpy(RegVal,"0");
			RegSetValueEx ( hCurKey, "Language",
				0, REG_SZ, (UCHAR *) RegVal, strlen(RegVal)+1 );
			RegCreateKeyEx ( HKEY_LOCAL_MACHINE, RK_WBin_WTran_COBOL_Acu,
				0, NULL, REG_OPTION_NON_VOLATILE,
				KEY_ALL_ACCESS, NULL, &hCurKey, &Disp );
		}
		else {
            strcpy(RegVal,"1");
			RegSetValueEx ( hCurKey, "Language", 0, REG_SZ, (UCHAR *) RegVal, strlen(RegVal)+1 );
			RegCreateKeyEx ( HKEY_LOCAL_MACHINE, RK_WBin_WTran_COBOL_MF, 
				0, NULL, REG_OPTION_NON_VOLATILE,
				KEY_ALL_ACCESS, NULL, &hCurKey, &Disp );
		}

        GetWindowText ( cWnd.OptionWnd.CCtls._PathToComp, 
			(char *) RegVal, sizeof(RegVal)-1 );
		RegSetValueEx ( hCurKey, "COB", 0, REG_SZ,
			(UCHAR *) RegVal, strlen(RegVal)+1 );

        GetWindowText ( cWnd.OptionWnd.CCtls._CFlags, 
			RegVal, sizeof(RegVal)-1 );
		RegSetValueEx ( hCurKey, "COBFLAGS", 0, REG_SZ,
			(UCHAR *) RegVal, strlen(RegVal)+1 );

        GetWindowText ( cWnd.OptionWnd.CCtls._OutputDir, 
			RegVal, sizeof(RegVal)-1 );
		RegSetValueEx ( hCurKey, "OUTDIR", 0, REG_SZ,
			(UCHAR *) RegVal, strlen(RegVal)+1 );

        GetWindowText ( cWnd.OptionWnd.CCtls._ObjExt, 
			RegVal, sizeof(RegVal)-1 );
		RegSetValueEx ( hCurKey, "OBJEXT", 0, REG_SZ,
			(UCHAR *) RegVal, strlen(RegVal)+1 );

		RegFlushKey ( hCurKey );
		RegCloseKey ( hCurKey );
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	cApplication::GetDrvInfo
//		Gets info on the drives in the computer
//
int cApplication::GetDrvInfo ( )
{
	int cnt = 0;

	//	Initialize the Drive info struct
	ZeroMemory ( cApp.Env->SysInfo.DrvInfo, sizeof ( cApp.Env->SysInfo.DrvInfo ) );
	//	Determine which drives exist
//	strcpy ( cApp.Env->SysInfo.DrvInfo[0].ch_CFDes, "A:\\" );
//	strcpy ( cApp.Env->SysInfo.DrvInfo[1].ch_CFDes, "B:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[2].ch_CFDes, "C:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[3].ch_CFDes, "D:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[4].ch_CFDes, "E:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[5].ch_CFDes, "F:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[6].ch_CFDes, "G:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[7].ch_CFDes, "H:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[8].ch_CFDes, "I:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[9].ch_CFDes, "J:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[10].ch_CFDes, "K:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[11].ch_CFDes, "L:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[12].ch_CFDes, "M:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[13].ch_CFDes, "N:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[14].ch_CFDes, "O:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[15].ch_CFDes, "P:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[16].ch_CFDes, "Q:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[17].ch_CFDes, "R:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[18].ch_CFDes, "S:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[19].ch_CFDes, "T:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[20].ch_CFDes, "U:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[21].ch_CFDes, "V:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[22].ch_CFDes, "W:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[23].ch_CFDes, "X:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[24].ch_CFDes, "Y:\\" );
	strcpy ( cApp.Env->SysInfo.DrvInfo[25].ch_CFDes, "Z:\\" );

	while ( cnt < 26 ) {
		int DrvType = 0;
		if ( (DrvType = GetDriveType ( cApp.Env->SysInfo.DrvInfo[cnt].ch_CFDes )) != 1 ) {
			char ch_DrvLet[4], ch_Label[_MAX_PATH];
			ZeroMemory ( ch_Label, sizeof ( ch_Label ));
			//	Copy drive letter to temp buffer to construct label
			memcpy ( ch_DrvLet, cApp.Env->SysInfo.DrvInfo[cnt].ch_CFDes, 3 );
			cApp.Env->SysInfo.DrvInfo[cnt].Exist = TRUE;		//	set member
			//	If the drive is a network drive
			DWORD szRemoteName = _MAX_PATH;
			char ch_LocalName[_MAX_PATH];
			if ( DrvType == DRIVE_REMOTE ) {
				strcpy ( ch_LocalName, cApp.Env->SysInfo.DrvInfo[cnt].ch_CFDes );
				ch_LocalName[strlen (ch_LocalName)-1] = '\0';
				WNetGetConnection ( ch_LocalName, ch_Label, &szRemoteName );
				
			}
			else {
				GetVolumeInformation ( ch_DrvLet, ch_Label,
					_MAX_PATH, NULL, 0, NULL, NULL, 0 );
			}
			ch_DrvLet[2] = '\0';
			if ( strlen ( ch_Label ) == 0 ) 
				sprintf ( cApp.Env->SysInfo.DrvInfo[cnt].Lbl, "(%s)", ch_DrvLet );
			else sprintf ( cApp.Env->SysInfo.DrvInfo[cnt].Lbl, 
				"%s   (%s)", ch_Label, ch_DrvLet );
		}
		cnt++;
	}
	cnt = 0;
	//	Get the types of the existing drives
	while ( cnt < 26 ) {
		if ( cApp.Env->SysInfo.DrvInfo[cnt].Exist == TRUE ) {
			char ch_DrvLet[4];
			ZeroMemory ( ch_DrvLet, sizeof ( ch_DrvLet ));
			memcpy ( ch_DrvLet, &cApp.Env->SysInfo.DrvInfo[cnt].ch_CFDes[4], 2 );
			strcat ( ch_DrvLet, "\\" );
			cApp.Env->SysInfo.DrvInfo[cnt].Type = GetDriveType ( ch_DrvLet );
		}
		cnt++;
	}

	return 0;
}

/*
**	History:
**	$Log: cApplication.cpp,v $
**	Revision 1.13  2009/10/17 19:55:36  gsl
**	fix default return type to int
**	
**	Revision 1.12  2003/06/18 16:55:56  gsl
**	Add /F remove /C (replace)
**	
**	Revision 1.11  2003/06/18 16:43:07  gsl
**	Add CVS header and history
**	
**
*/
