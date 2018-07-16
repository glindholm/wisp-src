/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/

//	Includes
#include "#Defines.h"
#include "#Classes.h"
#include "#Prototypes.h"
#include "#Externs.h"
#include "resource.h"

#include <richedit.h>
#include <winuser.h>


//************************************************************************
//	cWindows - Controls general window related aspects of the
//	application

//////////////////////////////////////////////////////////////////////////
//
//	cWindows::cWindows
//		Constructor
//
cWindows::cWindows ( ) 
{
	//	Initialize the GenWC window class - all other window classes are based on this one
	GenWC.style				 = 	CS_HREDRAW | CS_VREDRAW;
	GenWC.cbClsExtra		 = 	0;
	GenWC.cbWndExtra		 = 	0;
	GenWC.lpfnWndProc		 = 	DefWindowProc;
	GenWC.hIcon				 = 	AppIcon;
	GenWC.hCursor			 = 	LoadCursor ( NULL, IDC_ARROW );
	GenWC.hbrBackground	 = 	 GetSysColorBrush(COLOR_HIGHLIGHT);
	GenWC.lpszMenuName	 = 	NULL;

	//	Font used to print text to the status text window
	StdFont = CreateFont ( 15, 0, 0, 0, FW_NORMAL, FALSE,
		FALSE, FALSE, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS,
		CLIP_DEFAULT_PRECIS, PROOF_QUALITY,
		VARIABLE_PITCH | FF_DONTCARE, "Courier New" );
	//	Font used to print text on the option tabs
	TabFont = CreateFont ( 14, 0, 0, 0, FW_NORMAL, FALSE,
		FALSE, FALSE, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS,
		CLIP_DEFAULT_PRECIS, PROOF_QUALITY,
		VARIABLE_PITCH | FF_DONTCARE, "MS Sans Serif" );
}
//////////////////////////////////////////////////////////////////////////
//
//	cWindows::~cWindows
//		Destructor
//
cWindows::~cWindows ( ) 
{
	//	Delete all the GDI Objects that the app created
	DeleteObject ( StdFont );
	DeleteObject ( TabFont );
}
//////////////////////////////////////////////////////////////////////////
//
//	cWindows::CreateWnd - Creates window classes
//		Creates the window classes of the app
//
int cWindows::CreateWnd ( HINSTANCE hInst ) 
{
	WNDCLASS wc = GenWC;
	//	AppWindow
	wc.hInstance		=	hInst;
	wc.lpfnWndProc		= 	ShellWndProc;
	wc.hbrBackground	= 	GetSysColorBrush(COLOR_3DFACE);
	wc.hIcon				=	LoadIcon ( hInst, MAKEINTRESOURCE ( MainIcon ));
	wc.lpszClassName	= 	"ShellWndCls";
	RegisterClass ( &wc );

	cApp.Env->hInstGlb = hInst;
	cWnd.GenWC.hInstance = cApp.Env->hInstGlb;
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	cWindows::ShowWnd
//		Displays the windows of the app
//
int cWindows::ShowWnd ( HINSTANCE hInst ) 
{
	RECT ScrRes;
	cWnd.hAppMenu = LoadMenu ( cApp.Env->hInstGlb, MAKEINTRESOURCE ( MNU_Full )  );
	GetWindowRect ( GetDesktopWindow ( ), &ScrRes );

    //  Verify size settings and use screen size if larger than screen
    //  size or 1/4 screen size if too small.
	if ( cApp.Env->RegInfo.InitHSize > ScrRes.right )
    {
		cApp.Env->RegInfo.InitHSize = ScrRes.right;
    }
	else if( cApp.Env->RegInfo.InitHSize < ScrRes.right / 2)
	{
		cApp.Env->RegInfo.InitHSize = ScrRes.right / 2;
	}

	if ( cApp.Env->RegInfo.InitVSize > ScrRes.bottom )
	{
		cApp.Env->RegInfo.InitVSize = ScrRes.bottom;
	}
	else if( cApp.Env->RegInfo.InitVSize < ScrRes.bottom / 2 )
	{
        cApp.Env->RegInfo.InitVSize = ScrRes.bottom / 2;
    }

	//	Create the windows
	//	Main application window
	cWnd.hWnd.hShellWnd =
		CreateWindow ( "ShellWndCls", AppName DEBUG_TAG, 
		WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN, 
		CW_USEDEFAULT, CW_USEDEFAULT, 
		cApp.Env->RegInfo.InitHSize, cApp.Env->RegInfo.InitVSize,
		NULL, cWnd.hAppMenu, hInst, NULL );
	CustCtrl.StatusBar.Create ( cApp.Env->hInstGlb, cWnd.hWnd.hShellWnd );
	// Option window
	//		Get the heihgt of the option sheets (because if the user is using
	//		large fonts then the dialogs will be bigger than normal)
	cWnd.hWnd.hTabWnd = 
		CreateWindow ( WC_TABCONTROL, "", 
		WS_CHILD | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | WS_VISIBLE, 
		0, 10, cWnd.AppClntRect.right, 
		cWnd.OptionWnd.OptionShtRect.bottom+10, cWnd.hWnd.hShellWnd, 
		NULL, cApp.Env->hInstGlb, NULL );
	SendMessage ( cWnd.hWnd.hTabWnd, 
		WM_SETFONT,  ( WPARAM ) cWnd.TabFont, TRUE );
	//	Create option tabs
	TC_ITEM tcWISP, tcCOBOL;
	tcWISP.mask			=	TCIF_TEXT;
	tcWISP.cchTextMax	=	20;
	tcWISP.pszText		=	"WISP Options";
	tcCOBOL.mask			=	TCIF_TEXT;
	tcCOBOL.cchTextMax	=	20;
	tcCOBOL.pszText		=	"COBOL Options";

	SendMessage ( cWnd.hWnd.hTabWnd, TCM_INSERTITEM, 0, (LPARAM) &tcWISP );
	SendMessage ( cWnd.hWnd.hTabWnd, TCM_INSERTITEM, 1, (LPARAM) &tcCOBOL );
	//	Create the actual option sheets
	cWnd.hWnd.hWISPOpWnd = 
		CreateDialog ( cApp.Env->hInstGlb, MAKEINTRESOURCE ( WISPSheet ), 
		cWnd.hWnd.hTabWnd, (DLGPROC) WISPShtDlgProc );
	ShowWindow ( cWnd.hWnd.hWISPOpWnd, SW_SHOWNORMAL );

	cWnd.hWnd.hCOBOLOpWnd = 
		CreateDialog ( cApp.Env->hInstGlb, MAKEINTRESOURCE ( COBOLSheet ), 
		cWnd.hWnd.hTabWnd, (DLGPROC) COBOLShtDlgProc );
	cWnd.OptionWnd.CreateWCtls ( );
	cWnd.OptionWnd.CreateCCtls ( );
	//	Create the output windows
	cWnd.hWnd.hWISPOutWnd = 
		CreateDialog ( cApp.Env->hInstGlb, MAKEINTRESOURCE ( WISPOutput ), 
		cWnd.hWnd.hShellWnd, (DLGPROC) WISPShtDlgProc );
	cWnd.hWnd.hCOBOLOutWnd = 
		CreateDialog ( cApp.Env->hInstGlb, MAKEINTRESOURCE ( COBOLOutput ), 
		cWnd.hWnd.hShellWnd, (DLGPROC) COBOLShtDlgProc );
	//	Assign handles to the controls in the output dlgs
	cWnd.hWnd.hWCmdLine = GetDlgItem ( cWnd.hWnd.hWISPOutWnd, ctlWCmdLine );
	cWnd.hWnd.hCCmdLine = GetDlgItem ( cWnd.hWnd.hCOBOLOutWnd, ctlCCmdLine );
	cWnd.hWnd.hWOutput = GetDlgItem ( cWnd.hWnd.hWISPOutWnd, ctlWOutput );
	cWnd.hWnd.hCOutput = GetDlgItem ( cWnd.hWnd.hCOBOLOutWnd, ctlCOutput );
	SendMessage ( cWnd.hWnd.hCCmdLine, WM_SETFONT, 
		(WPARAM) cWnd.StdFont, TRUE );
	/*  Send message to set maximum number of chars in RICHEDIT CONTROL */
	SendMessage ( cWnd.hWnd.hCOutput, EM_EXLIMITTEXT, 0, 0L );
	SendMessage ( cWnd.hWnd.hCOutput, EM_SETBKGNDCOLOR, 0, GetSysColor(COLOR_3DFACE) );
	SendMessage ( cWnd.hWnd.hCOutput, WM_SETFONT, 
		(WPARAM) cWnd.StdFont, TRUE );
	SendMessage ( cWnd.hWnd.hCOutput, WM_SETFONT, 
		(WPARAM) cWnd.StdFont, TRUE );
	SendMessage ( cWnd.hWnd.hWCmdLine, WM_SETFONT, 
		(WPARAM) cWnd.StdFont, TRUE );
	/*  Send message to set maximum number of chars in RICHEDIT CONTROL */
	SendMessage ( cWnd.hWnd.hWOutput, EM_EXLIMITTEXT, 0, 0L );
	SendMessage ( cWnd.hWnd.hWOutput, EM_SETBKGNDCOLOR, 0, GetSysColor(COLOR_3DFACE) );
	SendMessage ( cWnd.hWnd.hWOutput, WM_SETFONT, 
		(WPARAM) cWnd.StdFont, TRUE );
	
	//	Execute Button
	cWnd.hWnd.hExecuteBtn = 
		CreateWindow ( "BUTTON", TranslateBtnTxt, 
		WS_CHILD | WS_CLIPSIBLINGS, 
		 325, 4, 140, TabHeight, 
		cWnd.hWnd.hShellWnd, 
		NULL, cApp.Env->hInstGlb, NULL );
	SendMessage ( cWnd.hWnd.hExecuteBtn, WM_SETFONT, 
		 ( WPARAM ) cWnd.TabFont, TRUE );
	EnableWindow ( cWnd.hWnd.hExecuteBtn, FALSE );
	//	Execute & Compile Button
	cWnd.hWnd.hExecCompBtn = 
		CreateWindow ( "BUTTON", TransCompBtnTxt, 
		WS_CHILD | WS_CLIPSIBLINGS, 
		 470, 4, 140, TabHeight, 
		cWnd.hWnd.hShellWnd, 
		NULL, cApp.Env->hInstGlb, NULL );
	SendMessage ( cWnd.hWnd.hExecCompBtn, WM_SETFONT, 
		 ( WPARAM ) cWnd.TabFont, TRUE );
	EnableWindow ( cWnd.hWnd.hExecCompBtn, FALSE );
	//	Create the line that seperates the menu from the client area
	cWnd.hWnd.hMenuDiv = CreateWindowEx ( WS_EX_STATICEDGE, "STATIC", "",
		WS_CHILD,
		0, 0, cWnd.AppClntRect.right, 2, cWnd.hWnd.hShellWnd,
		NULL, cApp.Env->hInstGlb, NULL );

	//	Set the window positions based on the size of the option sheet windows
	GetClientRect ( cWnd.hWnd.hWISPOpWnd, &cWnd.OptionWnd.OptionShtRect );
	SizeAppWindow ( );

	//	Now display the windows
	ShowWindow ( cWnd.hWnd.hShellWnd, SW_SHOWNORMAL );
	ShowWindow ( cWnd.hWnd.hTabWnd, SW_SHOWNORMAL );
	ShowWindow ( cWnd.hWnd.hExecuteBtn, SW_SHOWNORMAL );
	ShowWindow ( cWnd.hWnd.hExecCompBtn, SW_SHOWNORMAL );
	ShowWindow ( cWnd.hWnd.hMenuDiv, SW_SHOWNORMAL );
	//	Move the execute buttons in front of the tab sheet
	SetWindowPos ( cWnd.hWnd.hExecuteBtn, HWND_TOP,
		0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE );
	SetWindowPos ( cWnd.hWnd.hExecCompBtn, HWND_TOP,
		0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE );
	//	And update them as necissary
	UpdateWindow ( cWnd.hWnd.hShellWnd );
	UpdateWindow ( cWnd.hWnd.hTabWnd );
	UpdateWindow ( cWnd.hWnd.hExecuteBtn );
	UpdateWindow ( cWnd.hWnd.hExecCompBtn );

	SizeAppWindow ( );
	return 0;
}

///////////////////////////////////////////////////////////////////////////////
//
//		_AppWindow - Controls the Main Application Window
//

///////////////////////////////////////////////////////////////////////////////
//
//	cWindows::SizeWindow
//		Resizes all the app's windows according to the main app window
int cWindows::SizeAppWindow ( ) 
{
	GetClientRect ( cWnd.hWnd.hShellWnd, &cWnd.AppClntRect );					//	Get the new coordinates of the client area of the main app window

	CustCtrl.StatusBar.Resize ( cWnd.hWnd.hShellWnd );

	//	Resize the option tab window
	SetWindowPos ( cWnd.hWnd.hTabWnd, NULL,
		0, 10,
		cWnd.AppClntRect.right,
		cWnd.OptionWnd.OptionShtRect.bottom+31, 
		SWP_NOZORDER | SWP_NOMOVE );
	//	Resize the menu divider window
	SetWindowPos ( cWnd.hWnd.hMenuDiv,
		NULL, 0, 0, cWnd.AppClntRect.right, 2, 
		SWP_NOZORDER | SWP_NOMOVE );
	//	Resize the output windows - WISP
	SetWindowPos ( cWnd.hWnd.hWISPOutWnd, NULL,
		0, cWnd.OptionWnd.OptionShtRect.bottom+44,
		cWnd.AppClntRect.right,
		cWnd.AppClntRect.bottom - (cWnd.OptionWnd.OptionShtRect.bottom+63), 
		SWP_NOZORDER );
	//	Resize the command line window
	RECT CmdLineCoord, OutCoord;
	GetWindowRect ( cWnd.hWnd.hWISPOutWnd, &OutCoord );
	GetWindowRect ( cWnd.hWnd.hWCmdLine, &CmdLineCoord );
	SetWindowPos ( cWnd.hWnd.hWCmdLine, NULL, 0, 0, 
		cWnd.AppClntRect.right-(CmdLineCoord.left - OutCoord.left),
		20, SWP_NOMOVE | SWP_NOZORDER );
	RECT OutRect;
	GetClientRect ( cWnd.hWnd.hWISPOutWnd, &OutRect );
	SetWindowPos ( cWnd.hWnd.hWOutput, NULL, 0, 0, 
		OutRect.right-5 , OutRect.bottom-26,
		SWP_NOMOVE | SWP_NOZORDER );
	//	Resize the output windows - COBOL
	SetWindowPos ( cWnd.hWnd.hCOBOLOutWnd, NULL,
		0, cWnd.OptionWnd.OptionShtRect.bottom+44,
		cWnd.AppClntRect.right,
		cWnd.AppClntRect.bottom - (cWnd.OptionWnd.OptionShtRect.bottom+63), 
		SWP_NOZORDER );
	SetWindowPos ( cWnd.hWnd.hCCmdLine, NULL, 0, 0, 
		cWnd.AppClntRect.right-(CmdLineCoord.left - OutCoord.left),
		20, SWP_NOMOVE | SWP_NOZORDER );
	SetWindowPos ( cWnd.hWnd.hCOutput, NULL, 0, 0, 
		OutRect.right-5, OutRect.bottom-26,
		SWP_NOMOVE | SWP_NOZORDER );

	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//		_StatusWindow - Controls the window that contains the status text 
//		window
//

//////////////////////////////////////////////////////////////////////////
//
//	cWindows::_AppWindow::_StatusWindow::Print
//		Prints the contents of the output window to the printer
//
int cWindows::PrintOutputWnd ( ) 
{
    int nReqCPI = 12;
    int nReqLPI = 7;
	PRINTDLG pDlg;
    TEXTMETRIC current;
	char *ch_ToPrint;
	int StrLen, err;
	HFONT PrnFont;
	unsigned long buf_size;
    BOOL bPrntStat;
    int iPrntStat;
    HGDIOBJ hObj;
    int nLogPelsX, nLogPelsY;
    int nReqHeight, nReqWidth;
    int nFontHeight, nFontExt;
    int nFontWidth;

	pDlg.lStructSize				 = 	sizeof ( PRINTDLG );
	pDlg.hwndOwner					 = 	cWnd.hWnd.hShellWnd;
	pDlg.hDevMode					 = 	NULL;
	pDlg.hDevNames					 = 	NULL;
	pDlg.hDC							 = 	 ( HDC ) NULL;
	pDlg.Flags						 = 	PD_RETURNDC;
	pDlg.nFromPage					 = 	NULL;
	pDlg.nToPage					 = 	NULL;
	pDlg.nMinPage					 = 	NULL;
	pDlg.nMaxPage					 = 	NULL;
	pDlg.nCopies					 = 	NULL;
	pDlg.hInstance					 = 	 ( HINSTANCE ) NULL;
	pDlg.lCustData					 = 	NULL;
	pDlg.lpfnPrintHook			 = 	NULL;
	pDlg.lpfnSetupHook			 = 	NULL;
	pDlg.lpPrintTemplateName	 = 	NULL;
	pDlg.lpSetupTemplateName	 = 	NULL;
	pDlg.hPrintTemplate			 = 	NULL;
	pDlg.hSetupTemplate			 = 	NULL;

	PrintDlg ( &pDlg );
    
    //  Get the device characteristics for current font, also
    //  pixels per inch.
    nLogPelsX = GetDeviceCaps(pDlg.hDC, LOGPIXELSX);
    nLogPelsY = GetDeviceCaps(pDlg.hDC, LOGPIXELSY);

	//	Create the printer font; compute the size by dividing the
    //  device pixels per inch by the desired CPI an LPI
    nReqHeight = nLogPelsY / nReqLPI;
    nReqWidth  = nLogPelsX / nReqCPI;

	PrnFont = CreateFont (
		nReqHeight, nReqWidth, 				    //	Height, Width
		0, 0, FW_NORMAL, 						//	Escapement, Orientation, Weight
		FALSE, FALSE, FALSE, 					//	Italic, Underline, StrikeOut
		DEFAULT_CHARSET, OUT_DEFAULT_PRECIS,    //	Character set, Output precision
		CLIP_DEFAULT_PRECIS, PROOF_QUALITY, 	//	Clip precision, Quality
		DEFAULT_PITCH | FF_MODERN, 			    //	Pitch and family
		"Courier New" );						//	Font name

    if( PrnFont == NULL )
    {
        if( (err = GetLastError()) != 0 )
        {   // Look in ../MSDEV/src/include/winerror.h
		    cApp.CloseApp ( 4001, err );
        }
    }

    bPrntStat = GetTextMetrics(pDlg.hDC, &current);
    if( bPrntStat == FALSE )
    {
        if( (err = GetLastError()) != 0 )
        {   // Look in ../MSDEV/src/include/winerror.h
		    cApp.CloseApp ( 4001, err );
        }
    }
    nFontHeight = current.tmHeight;
    nFontWidth = current.tmAveCharWidth;
    nFontExt = current.tmExternalLeading;


	// Determine which window to print: Cobol or WISP
	HWND CurOut;
	if ( SendMessage ( cWnd.hWnd.hTabWnd, TCM_GETCURSEL, 0, 0 ) == 0 )
		CurOut = cWnd.hWnd.hWOutput;
	else CurOut = cWnd.hWnd.hCOutput;

	// Allocate a buffer to hold the text + 2K
	int text_len = GetWindowTextLength(CurOut);
    buf_size = text_len + OutputBlock; // Add 2K
	ch_ToPrint =  ( char * ) malloc ( buf_size );
	if( NULL == ch_ToPrint )
	{
		cApp.CloseApp ( 3001, 0 );
		ExitProcess(3001);
	}
	ZeroMemory ( ch_ToPrint, _msize ( ch_ToPrint )  );

	int actual_text_len = GetWindowText(CurOut, ch_ToPrint, _msize (ch_ToPrint));

	StrLen = strlen ( ch_ToPrint );
	
	//	Prep for printing
	DOCINFO DocInfo;
    memset(&DocInfo, 0, sizeof(DocInfo));
	DocInfo.cbSize			 = 	sizeof ( DOCINFO );
	DocInfo.lpszDocName	 = 	"WISPTran Status Output";
	DocInfo.lpszOutput	 = 	NULL;

	char ch_A_Line[81];
	int FindLF = 0, SPointer = 0, EPointer = 0, CurLine = 1, LineHeight = 75,
        PageNum = 1;
	BOOL EndOfPJob = FALSE;
	
	iPrntStat = StartDoc ( pDlg.hDC, &DocInfo );
    if( iPrntStat <= 0 )
    {
		if( (err = GetLastError()) != 0 )
        {   // Look in ../MSDEV/src/include/winerror.h
		    cApp.CloseApp ( 4002, err );
        }
    }

	iPrntStat = StartPage ( pDlg.hDC );
    if( iPrntStat <= 0 )
    {
		if( (err = GetLastError()) != 0 )
        {	// Look in ../MSDEV/src/include/winerror.h
		    cApp.CloseApp ( 4003, err );
        }
    }

	hObj = SelectObject ( pDlg.hDC, PrnFont );
    if( hObj == NULL )
    {
		cApp.CloseApp ( 4004, 0 );		// process error */
    }

#define LINES_PER_PAGE 60
#define COLUMNS_PER_LINE 80

	//	Loop until you reach the end of the buffer to be printed
	while ( TRUE ) 
    {
		//	If at the bottom of the page
		if ((CurLine > LINES_PER_PAGE) || ( EndOfPJob == TRUE )) 
        {
	        iPrntStat = EndPage ( pDlg.hDC );
            if( iPrntStat <= 0 )
            {
		        if( (err = GetLastError()) != 0 )
                {	// Look in ../MSDEV/src/include/winerror.h
		            cApp.CloseApp ( 4006, err );
                }
            }

			if ( EndOfPJob == FALSE ) {
				StartPage ( pDlg.hDC );
				SelectObject ( pDlg.hDC, PrnFont );
			}
			PageNum++;
			CurLine = 1;
			if ( EndOfPJob == TRUE ) break;
		}
		if ( CurLine == 1 )
        {
			CurLine = 4;
			char ch_Footer[100];
			sprintf ( ch_Footer, "WISPTran Translation/Compilation Report"
				"                                 Page %d", PageNum );
			bPrntStat = TextOut ( pDlg.hDC, nFontWidth * 5,
                (nFontHeight + nFontExt),
				ch_Footer, strlen ( ch_Footer ) );
            if( bPrntStat == FALSE )
            {
        		if( (err = GetLastError()) != 0 )
                {	// Look in ../MSDEV/src/include/winerror.h
		            cApp.CloseApp ( 4005, err );
                }
            }
		}
		//	Find the end of the line or the end of the buffer or the next 80 characters 
		//		 - whichever comes first
		while ( FindLF < COLUMNS_PER_LINE ) 
        {
			if ( ch_ToPrint[EPointer] == '\0' ) {
				EndOfPJob = TRUE;
				break;
			}
			if ( ( ch_ToPrint[EPointer] == '\n' ) ||
				 ( ch_ToPrint[EPointer] == '\r' ) ) break;
			else EPointer++;
			FindLF++;
		}
		FindLF = 0;
		memcpy ( ch_A_Line, &ch_ToPrint[SPointer], EPointer - SPointer );
		ch_A_Line[EPointer - SPointer] = '\0';
		bPrntStat = TextOut ( pDlg.hDC, nFontWidth * 5,
            (nFontHeight + nFontExt) * CurLine , 
			ch_A_Line, strlen ( ch_A_Line )  );
        if( bPrntStat == FALSE )
        {
            if( (err = GetLastError()) != 0 )
            {	// Look in ../MSDEV/src/include/winerror.h
		        cApp.CloseApp ( 4005, err );
            }
        }
		CurLine++;
		//	Find next character after the \n and \r characters
		while ( TRUE ) {
			if ( ch_ToPrint[EPointer] == '\n' ) {
				EPointer++;
				continue;
			}
			if ( ch_ToPrint[EPointer] == '\r' ) {
				EPointer++;
				continue;
			}
			break;
		}
		SPointer = EPointer;
	}

	iPrntStat = EndDoc ( pDlg.hDC );
    if( iPrntStat <= 0 )
    {
		if( (err = GetLastError()) != 0 )
        {	// Look in ../MSDEV/src/include/winerror.h
		    cApp.CloseApp ( 4007, err );
        }
    }

	DeleteObject ( PrnFont );

    free( ch_ToPrint);
    ch_ToPrint = NULL;

	return 0;
}


/*
**	History:
**	$Log: cWindows.cpp,v $
**	Revision 1.10  2009/10/17 19:55:36  gsl
**	fix default return type to int
**	
**	Revision 1.9  2003/06/18 16:43:07  gsl
**	Add CVS header and history
**	
**
*/
