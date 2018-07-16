#if !defined  PROTOTYPES_H
#define PROTOTYPES_H

//////////////////////////////////////////////////////////////////////////
//		This file contains the function prototypes for the application

LRESULT CALLBACK ShellWndProc ( HWND hShellWnd, UINT msg, WPARAM wp, LPARAM lp );
LRESULT CALLBACK ClockWndProc ( HWND hClockWnd, UINT msg, WPARAM wp, LPARAM lp );
BOOL CALLBACK WISPShtDlgProc ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp );
BOOL CALLBACK COBOLShtDlgProc ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp );

DWORD ThreadManager ( );

#endif
