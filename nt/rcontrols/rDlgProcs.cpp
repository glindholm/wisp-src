//////////////////////////////////////////////////////////////////////////

#include "rControls.h"
//////////////////////////////////////////////////////////////////////////
//		SelectDirDlgProc
BOOL CALLBACK SelectDirDlgProc ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;

	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	switch ( msg ) {
		case WM_INITDIALOG:
			CustCtrl.Dialogs.SelectDirDlg.Initialize ( hDlg, CustCtrl.hInst );
			break;
		case WM_NOTIFY:
			NMHDR *nms;
			nms = (NMHDR *)lp;
			if ( nms == NULL ) break;
			switch ( nms->idFrom){
				UINT tvAction;
				TV_ITEM tvClicked;
				NM_TREEVIEW *nmtv;
				//	If a notification message was sent from the tree view control
				case TV_AvailDirs:
					switch ( nms->code ) {
						//	if an item is expanding
						case TVN_ITEMEXPANDING:
							nmtv = (NM_TREEVIEW *) lp;
							tvClicked = nmtv->itemNew;
							tvAction = nmtv->action;
							CustCtrl.Ctrl.tvList.
							GetSubDirs ( hDlg, tvClicked, tvAction );
							break;
						case TVN_SELCHANGED:
							nmtv = (NM_TREEVIEW *) lp;
							CustCtrl.Ctrl.tvList.tvClicked = nmtv->itemNew;
							CustCtrl.Ctrl.tvList.RetPath 
								( hDlg, GetDlgItem ( hDlg, TV_AvailDirs ));
							break;
					}
					break;
			}
		case WM_COMMAND:
			switch ( wpLo ) {
				case IDOK:
					CustCtrl.Dialogs.SelectDirDlg.OKClicked ( hDlg );
					break;
				case IDCANCEL:
					EndDialog ( hDlg, 1 );		//	Canceled
					break;
				case ctlAddBtn:
					CustCtrl.Dialogs.SelectDirDlg.AddClicked ( hDlg );
					break;
				case ctlRemoveBtn:
					CustCtrl.Dialogs.SelectDirDlg.RemoveClicked ( hDlg );
					break;
				case ctlMUpBtn:
					CustCtrl.Dialogs.SelectDirDlg.MUpClicked ( hDlg );
					break;
				case ctlMDownBtn:
					CustCtrl.Dialogs.SelectDirDlg.MDownClicked ( hDlg );
					break;
			}
	}
	return 0;		//	If this returned then there was an error
}
//////////////////////////////////////////////////////////////////////////
//		SelectDirDlgProc
BOOL CALLBACK SelectSingleDirDlgProc ( HWND hDlg, UINT msg, WPARAM wp, LPARAM lp )
{
	int wpHi, wpLo;

	wpHi = HIWORD (wp);
	wpLo = LOWORD (wp);
	switch ( msg ) {
		case WM_INITDIALOG:
			CustCtrl.Dialogs.SelectDirDlg.Initialize ( hDlg, CustCtrl.hInst );
			break;
		case WM_NOTIFY:
			NMHDR *nms;
			nms = (NMHDR *)lp;
			if ( nms == NULL ) break;
			switch ( nms->idFrom){
				UINT tvAction;
				TV_ITEM tvClicked;
				NM_TREEVIEW *nmtv;
				//	If a notification message was sent from the tree view control
				case TV_AvailDirs:
					switch ( nms->code ) {
						//	if an item is expanding
						case TVN_ITEMEXPANDING:
							nmtv = (NM_TREEVIEW *) lp;
							tvClicked = nmtv->itemNew;
							tvAction = nmtv->action;
							CustCtrl.Ctrl.tvList.
							GetSubDirs ( hDlg, tvClicked, tvAction );
							break;
						case TVN_SELCHANGED:
							nmtv = (NM_TREEVIEW *) lp;
							CustCtrl.Ctrl.tvList.tvClicked = nmtv->itemNew;
							CustCtrl.Ctrl.tvList.RetPath 
								( hDlg, GetDlgItem ( hDlg, TV_AvailDirs ));
							break;
					}
					break;
			}
		case WM_COMMAND:
			switch ( wpLo ) {
				case IDOK:
					CustCtrl.Dialogs.SelectDirDlg.OKClicked ( hDlg );
					break;
				case IDCANCEL:
					EndDialog ( hDlg, 1 );		//	Canceled
					break;
			}
	}
	return 0;		//	If this returned then there was an error
}

//////////////////////////////////////////////////////////////////////////
//	ClockTimerProc
VOID CALLBACK ClockTimerProc ( HWND hWnd, UINT msg, UINT idEvent, DWORD dwTime )
{
	SYSTEMTIME Time;
	char ch_Time[30], ch_Month[15], ch_Day[15], ch_AMPM[3];
	switch ( msg ) {
		case WM_TIMER:
			GetLocalTime ( &Time );
			switch ( Time.wMonth ) {
				case 1:
					strcpy ( ch_Month, "Jan" );
					break;
				case 2:
					strcpy ( ch_Month, "Feb" );
					break;
				case 3:
					strcpy ( ch_Month, "Mar" );
					break;
				case 4:
					strcpy ( ch_Month, "Apr" );
					break;
				case 5:
					strcpy ( ch_Month, "May" );
					break;
				case 6:
					strcpy ( ch_Month, "Jun" );
					break;
				case 7:
					strcpy ( ch_Month, "Jul" );
					break;
				case 8:
					strcpy ( ch_Month, "Aug" );
					break;
				case 9:
					strcpy ( ch_Month, "Sep" );
					break;
				case 10:
					strcpy ( ch_Month, "Oct" );
					break;
				case 11:
					strcpy ( ch_Month, "Nov" );
					break;
				case 12:
					strcpy ( ch_Month, "Dec" );
					break;
			}
			switch ( Time.wDayOfWeek ) {
				case 0:
					strcpy ( ch_Day, "Sun" );
					break;
				case 1:
					strcpy ( ch_Day, "Mon" );
					break;
				case 2:
					strcpy ( ch_Day, "Tues" );
					break;
				case 3:
					strcpy ( ch_Day, "Wed" );
					break;
				case 4:
					strcpy ( ch_Day, "Thur" );
					break;
				case 5:
					strcpy ( ch_Day, "Fri" );
					break;
				case 6:
					strcpy ( ch_Day, "Sat" );
					break;
			}
			switch ( Time.wHour ) {
				case 0:
					Time.wHour = 12;
					strcpy ( ch_AMPM, "AM" );
					break;
				case 1: case 2: case 3: case 4:
				case 5: case 6: case 7: case 8:
				case 9: case 10: case 11:
					strcpy ( ch_AMPM, "AM" );
					break;
				case 12:
					strcpy ( ch_AMPM, "PM" );
					break;
				case 13: case 14: case 15: case 16:
				case 17: case 18: case 19: case 20:
				case 21: case 22: case 23:
					strcpy ( ch_AMPM, "PM" );
					Time.wHour = Time.wHour - 12;
					break;
			}
			sprintf ( ch_Time, "%s, %s %02d, %d  %2d:%02d %s",
				ch_Day, ch_Month, Time.wDay, Time.wYear,
				Time.wHour, Time.wMinute, ch_AMPM );
			SetWindowText ( CustCtrl.StatusBar.hClock, ch_Time );
			break;
	}
}
