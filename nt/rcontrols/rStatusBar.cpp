//////////////////////////////////////////////////////////////////////////
//	This file contains the function bodies for the status bar class

#include "rControls.h"
#include "rResources.h"
#include "rPrototypes.h"

#define StatusBarHeight 18

//////////////////////////////////////////////////////////////////////////
//
//	rControls::_StatusBar::Create
//		Creates the status bar window
//
rControls::_StatusBar::Create ( HINSTANCE hInst, HWND hAppWnd )
{
	WNDCLASS wc;
	wc.style				= 	CS_HREDRAW | CS_VREDRAW;
	wc.hInstance		=	hInst;
	wc.cbClsExtra		= 	0;
	wc.cbWndExtra		= 	0;
	wc.lpfnWndProc		= 	DefWindowProc;
	wc.hIcon				= 	NULL;
	wc.hCursor			= 	LoadCursor ( NULL, IDC_ARROW );
	wc.hbrBackground	=	(HBRUSH)GetStockObject ( LTGRAY_BRUSH );
	wc.lpszMenuName	= 	NULL;
	wc.lpszClassName	= 	"StatusBarWndCls";
	RegisterClass ( &wc );

	RECT AppRect;
	GetClientRect ( hAppWnd, &AppRect );
	//	Status Bar
	hStatusBar = 
		CreateWindow ( "STATIC", "", 
		WS_CHILD | WS_CLIPCHILDREN | WS_CLIPSIBLINGS, 
		0, AppRect.bottom-StatusBarHeight,
		AppRect.right, StatusBarHeight, hAppWnd, NULL, hInst, 
		NULL );
	//	Clock
	hClock = 
		CreateWindowEx ( WS_EX_STATICEDGE, "STATIC", "", 
		WS_CHILD | WS_CLIPCHILDREN | WS_CLIPSIBLINGS, 
		0, 2, 150, StatusBarHeight - 2, hStatusBar, 
		NULL, hInst, NULL );
	SendMessage ( hClock, WM_SETFONT,  ( WPARAM ) CustCtrl.DrawTools.SBFont, TRUE );
	SetTimer ( NULL, NULL, 1000, (TIMERPROC) ClockTimerProc );
	//	Status
	hStatus = 
		CreateWindowEx ( WS_EX_STATICEDGE, "STATIC", "", 
		WS_CHILD | WS_CLIPCHILDREN | WS_CLIPSIBLINGS, 
		153, 2, AppRect.right-153, 
		StatusBarHeight-2, hStatusBar, 
		NULL, hInst, NULL );
	UpdateMsg 
		 ( "Initializing Application, please wait..." );
	SendMessage ( hStatus, WM_SETFONT,  
		( WPARAM ) CustCtrl.DrawTools.SBFont, TRUE );

	ShowWindow ( hStatusBar, SW_NORMAL );
	ShowWindow ( hClock, SW_NORMAL );
	ShowWindow ( hStatus, SW_NORMAL );
	UpdateWindow ( hStatusBar );
	UpdateWindow ( hClock );
	UpdateWindow ( hStatus );

	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	cWindows::_AppWindow::_StatusBar::_Status::UpdateMsg ( ) 
//		Changes the message displayed in the status element
//
rControls::_StatusBar::UpdateMsg ( char *sMsg ) 
{
	strcpy ( sOutput, sMsg );
	SetWindowText ( hStatus, sOutput );

	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	cWindows::_AppWindow::_StatusBar::Resize ( ) 
//		Resizes the status bar to fill the width of the 
//		app window 
//
rControls::_StatusBar::Resize ( HWND hAppWnd ) 
{
	RECT AppRect;
	GetClientRect ( hAppWnd, &AppRect );

	SetWindowPos ( hStatusBar, HWND_TOP,
		0, AppRect.bottom - StatusBarHeight, 
		AppRect.right, StatusBarHeight, NULL );
	SetWindowPos ( hStatus, NULL, 0, 0, 
		AppRect.right-153, StatusBarHeight-2,
		SWP_NOMOVE | SWP_NOZORDER );

	return 0;
}