//////////////////////////////////////////////////////////////////////////
//	This file contains the function prototypes for the functions of the 
//		application that aren't members of a class.  Essentially, window
//		and dialog box procedures

//	Window procedures
LRESULT CALLBACK AppWndProc ( HWND hShellWnd, UINT msg, WPARAM wp, LPARAM lp );

//	Dialog box procedures
BOOL CALLBACK MainDlgProc ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp );
BOOL CALLBACK General_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp );
BOOL CALLBACK Videocap_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp );
BOOL CALLBACK EXTRACT_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp );
BOOL CALLBACK MESSAGE_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp );
BOOL CALLBACK SCRATCH_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp );
BOOL CALLBACK License_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp );
BOOL CALLBACK WISPBin_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp );
BOOL CALLBACK DISPLAY_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp );
BOOL CALLBACK WPROC_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp );
BOOL CALLBACK ACP_DP ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp );
BOOL CALLBACK WISPSrvDlgProc ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp );
BOOL CALLBACK WISPDirDlgProc ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp );
BOOL CALLBACK WISPConfigDlgProc ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp );
DWORD Validate ( HWND hCtl );