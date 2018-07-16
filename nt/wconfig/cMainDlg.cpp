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

///////////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_MainDialog::ChngTab
//		Activates the dialog for the tab that was clicked
int cDialogs::_MainDialog::ChngTab ( HWND hDlg )
{
	HWND hTabCtl;
	int CurTab;

	hTabCtl = GetDlgItem ( hDlg, tabRegOps );
	CurTab = SendMessage ( hTabCtl, TCM_GETCURSEL, 0, 0 );
	//	Hide the previous tab
	ShowWindow ( Dialogs.General.hGeneral, SW_HIDE );
	ShowWindow ( Dialogs.Videocap.hVideocap, SW_HIDE );
	ShowWindow ( Dialogs.EXTRACT.hEXTRACT, SW_HIDE );
	ShowWindow ( Dialogs.MESSAGE.hMESSAGE, SW_HIDE );
	ShowWindow ( Dialogs.SCRATCH.hSCRATCH, SW_HIDE );
	ShowWindow ( Dialogs.License.hLicense, SW_HIDE );
	ShowWindow ( Dialogs.WISPBin.hWISPBin, SW_HIDE );
	ShowWindow ( Dialogs.DISPLAY.hDISPLAY, SW_HIDE );
	ShowWindow ( Dialogs.WPROC.hWPROC, SW_HIDE );
	ShowWindow ( Dialogs.ACP.hACP, SW_HIDE );

	//	Show the tab that was clicked
	switch ( CurTab ) {
		case 0:
			ShowWindow ( Dialogs.General.hGeneral, SW_SHOW );
			break;
		case 1:
			ShowWindow ( Dialogs.Videocap.hVideocap, SW_SHOW );
			break;
		case 2:
			ShowWindow ( Dialogs.EXTRACT.hEXTRACT, SW_SHOW );
			break;
		case 3:
			ShowWindow ( Dialogs.MESSAGE.hMESSAGE, SW_SHOW );
			break;
		case 4:
			ShowWindow ( Dialogs.SCRATCH.hSCRATCH, SW_SHOW );
			break;
		case 5:
			Dialogs.License.UpdateLicenseFile();
			ShowWindow ( Dialogs.License.hLicense, SW_SHOW );
			break;
		case 6:
			ShowWindow ( Dialogs.WISPBin.hWISPBin, SW_SHOW );
			break;
		case 7:
			ShowWindow ( Dialogs.DISPLAY.hDISPLAY, SW_SHOW );
			break;
		case 8:
			ShowWindow ( Dialogs.WPROC.hWPROC, SW_SHOW );
			break;
		case 9:
			ShowWindow ( Dialogs.ACP.hACP, SW_SHOW );
			break;
	}
	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_MainDialog::Initialize
//		Initialize the Main Dialog
//
int cDialogs::_MainDialog::Initialize ( HWND hDlg )
{
	HWND hTabWnd;
	TC_ITEM TabAttrib[10];

	cApp.hMainDlg = hDlg;
	//*********************************************************************
	//	Set Tab Attributes
	TabAttrib[0].mask	=	TCIF_TEXT;
	TabAttrib[0].pszText	=	"General";
	TabAttrib[0].cchTextMax	=	50;

	TabAttrib[1].mask	=	TCIF_TEXT;
	TabAttrib[1].pszText	=	"Videocap";
	TabAttrib[1].cchTextMax	=	50;

	TabAttrib[2].mask	=	TCIF_TEXT;
	TabAttrib[2].pszText	=	"EXTRACT";
	TabAttrib[2].cchTextMax	=	50;

	TabAttrib[3].mask	=	TCIF_TEXT;
	TabAttrib[3].pszText	=	"MESSAGE";
	TabAttrib[3].cchTextMax	=	50;

	TabAttrib[4].mask	=	TCIF_TEXT;
	TabAttrib[4].pszText	=	"SCRATCH";
	TabAttrib[4].cchTextMax	=	50;

	TabAttrib[5].mask	=	TCIF_TEXT;
	TabAttrib[5].pszText	=	"License";
	TabAttrib[5].cchTextMax	=	50;

	TabAttrib[6].mask	=	TCIF_TEXT;
	TabAttrib[6].pszText	=	"WISPBin";
	TabAttrib[6].cchTextMax	=	50;

	TabAttrib[7].mask	=	TCIF_TEXT;
	TabAttrib[7].pszText	=	"DISPLAY";
	TabAttrib[7].cchTextMax	=	50;

	TabAttrib[8].mask	=	TCIF_TEXT;
	TabAttrib[8].pszText	=	"WPROC";
	TabAttrib[8].cchTextMax	=	50;

	TabAttrib[9].mask	=	TCIF_TEXT;
	TabAttrib[9].pszText	=	"ACP";
	TabAttrib[9].cchTextMax	=	50;

	//*********************************************************************
	//	Create Tabs
	hTabWnd = GetDlgItem ( hDlg, tabRegOps );
	TabCtrl_InsertItem ( hTabWnd, 0, &TabAttrib[0] );
	TabCtrl_InsertItem ( hTabWnd, 1, &TabAttrib[1] );
	TabCtrl_InsertItem ( hTabWnd, 2, &TabAttrib[2] );
	TabCtrl_InsertItem ( hTabWnd, 3, &TabAttrib[3] );
	TabCtrl_InsertItem ( hTabWnd, 4, &TabAttrib[4] );
	TabCtrl_InsertItem ( hTabWnd, 5, &TabAttrib[5] );
	TabCtrl_InsertItem ( hTabWnd, 6, &TabAttrib[6] );
	TabCtrl_InsertItem ( hTabWnd, 7, &TabAttrib[7] );
	TabCtrl_InsertItem ( hTabWnd, 8, &TabAttrib[8] );

// Uncomment to enable ACP tab.
//	TabCtrl_InsertItem ( hTabWnd, 9, &TabAttrib[9] );

	//*********************************************************************
	//	Create dialogs that comprise each tab
	CreateDialog ( 
		cApp.hInstGlb, MAKEINTRESOURCE ( dlgGeneral ),
		hTabWnd, (DLGPROC) General_DP );
	CreateDialog (
		cApp.hInstGlb, MAKEINTRESOURCE ( dlgVideocap ),
		hTabWnd, (DLGPROC) Videocap_DP );
	CreateDialog (
		cApp.hInstGlb, MAKEINTRESOURCE ( dlgEXTRACT ),
		hTabWnd, (DLGPROC) EXTRACT_DP );
	CreateDialog (
		cApp.hInstGlb, MAKEINTRESOURCE ( dlgMESSAGE ),
		hTabWnd, (DLGPROC) MESSAGE_DP );
	CreateDialog (
		cApp.hInstGlb, MAKEINTRESOURCE ( dlgSCRATCH ),
		hTabWnd, (DLGPROC) SCRATCH_DP );
	CreateDialog (
		cApp.hInstGlb, MAKEINTRESOURCE ( dlgLicense ),
		hTabWnd, (DLGPROC) License_DP );
	CreateDialog (
		cApp.hInstGlb, MAKEINTRESOURCE ( dlgWISPBin ),
		hTabWnd, (DLGPROC) WISPBin_DP );
	CreateDialog (
		cApp.hInstGlb, MAKEINTRESOURCE ( dlgDISPLAY ),
		hTabWnd, (DLGPROC) DISPLAY_DP );
	CreateDialog (
		cApp.hInstGlb, MAKEINTRESOURCE ( dlgWPROC ),
		hTabWnd, (DLGPROC) WPROC_DP );
	CreateDialog (
		cApp.hInstGlb, MAKEINTRESOURCE ( dlgACP ),
		hTabWnd, (DLGPROC) ACP_DP ); 

	//*********************************************************************
	//	Hide all but first dialog (the General dialog)
	ShowWindow ( Dialogs.Videocap.hVideocap, SW_HIDE );
	ShowWindow ( Dialogs.EXTRACT.hEXTRACT, SW_HIDE );
	ShowWindow ( Dialogs.MESSAGE.hMESSAGE, SW_HIDE );
	ShowWindow ( Dialogs.SCRATCH.hSCRATCH, SW_HIDE );
	ShowWindow ( Dialogs.License.hLicense, SW_HIDE );
	ShowWindow ( Dialogs.WISPBin.hWISPBin, SW_HIDE );
	ShowWindow ( Dialogs.DISPLAY.hDISPLAY, SW_HIDE );
	ShowWindow ( Dialogs.WPROC.hWPROC, SW_HIDE );
	ShowWindow ( Dialogs.ACP.hACP, SW_HIDE );

	//*********************************************************************
	//	Initialize the sheets
	while ( TRUE ) {
		/*	This loop keeps repeating the initialization of the
		//	General option sheet as long as the three primary
		//	entries aren't valid, it breaks if all's well, and it
		//	terminates the app if the user clicked cancel in any
		//	of the dialog boxes	*/
		int RetCode = 0;
		RetCode = Dialogs.General.Initialize ( );
		if ( RetCode == 0 ) break;
		if ( RetCode == 1 ) continue;
		if ( RetCode == 2 ) {
			PostQuitMessage ( 0 );
			return 0;
		}
	}
	Dialogs.Videocap.Initialize ( );
	Dialogs.EXTRACT.Initialize ( );
	Dialogs.MESSAGE.Initialize ( );
	Dialogs.SCRATCH.Initialize ( );
	Dialogs.License.Initialize ( );
	Dialogs.WISPBin.Initialize ( );
	Dialogs.DISPLAY.Initialize ( );
	Dialogs.WPROC.Initialize ( );
	Dialogs.ACP.Initialize ( );	
	
	SetFocus ( GetDlgItem ( Dialogs.General.hGeneral, S01_SERVER ));
	return 0;
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_MainDialog::RemoveClicked
//		Is called when the user clicks the reset configuration
//		button
//
void cDialogs::_MainDialog::RemoveClicked ( )
{
	if ( MessageBox ( 
		cApp.hMainDlg,
		"Are you sure you want to reset the WISP configuration?",
		"Reset Configuration", MB_YESNO ) == IDYES ) {

		//*************************************************************
		//	Delete all the stuff from the registry that was put
		//	there this app
		cApp.DeleteReg ( );
		//*************************************************************
		//	Clear all the edit controls
		SetDlgItemText ( Dialogs.General.hGeneral, S01_SERVER, "" );
		SetDlgItemText ( Dialogs.General.hGeneral, S01_WISPDIR, "" );
		SetDlgItemText (
			Dialogs.General.hGeneral, S01_WISPCONFIG, "" );
		SetDlgItemText ( Dialogs.General.hGeneral, S01_USERDIR, "" );
		SetDlgItemText ( Dialogs.General.hGeneral, S01_PATH, "" );
		SetDlgItemText ( Dialogs.General.hGeneral, S01_TEMPDIR, "" );
		SetDlgItemText (
			Dialogs.General.hGeneral, S01_WISPSORTMEM, "" );
		SetDlgItemText (
			Dialogs.Videocap.hVideocap, S02_WISPTERM, "" );
		SetDlgItemText (
			Dialogs.Videocap.hVideocap, S02_VIDEOCAPDIR, "" );
		SetDlgItemText ( Dialogs.EXTRACT.hEXTRACT, S03_CPUID, "" );
		SetDlgItemText ( Dialogs.EXTRACT.hEXTRACT, S03_NETID, "" );
		SetDlgItemText ( Dialogs.MESSAGE.hMESSAGE, S04_SHAREDIR, "" );
		SetDlgItemText (
			Dialogs.SCRATCH.hSCRATCH, S05_SCRATCHMODE, "" );
		SetDlgItemText (
			Dialogs.License.hLicense, S06_LICENSEFILE, "" );
		SetDlgItemText ( Dialogs.WISPBin.hWISPBin, S07_WISP, "" );
		SetDlgItemText ( Dialogs.WISPBin.hWISPBin, S07_WEDITOR, "" );
		SetDlgItemText ( Dialogs.WISPBin.hWISPBin, S07_DISPLAY, "" );
		SetDlgItemText (
			Dialogs.DISPLAY.hDISPLAY, S08_DISPLAY8BIT, "" );
		SetDlgItemText ( Dialogs.WPROC.hWPROC, S09_WPROC, "" );
		SetDlgItemText ( Dialogs.WPROC.hWPROC, S09_WPROCDEBUG, "" );
		SetDlgItemText ( Dialogs.ACP.hACP, S10_ACPCONFIGDIR, "" );
		SetDlgItemText ( Dialogs.ACP.hACP, S10_ACPMAP, "" );
		//*************************************************************
		//	Reset all keys and values
		int RetCode = 1;
		while ( TRUE ) {
		/*	This loop keeps repeating the initialization of the
		//	General option sheet as long as the three primary
		//	entries aren't valid, it breaks if all's well, and it
		//	terminates the app if the user clicked cancel in any
		//	of the dialog boxes	*/
			RetCode = Dialogs.General.Initialize ( );
			if ( RetCode == 0 ) break;
			if ( RetCode == 1 ) continue;
			if ( RetCode == 2 ) {
				PostQuitMessage ( 0 );
				return;
				break;
			}
		}
		Dialogs.Videocap.Initialize ( );
		Dialogs.EXTRACT.Initialize ( );
		Dialogs.MESSAGE.Initialize ( );
		Dialogs.SCRATCH.Initialize ( );
		Dialogs.License.Initialize ( );
		Dialogs.WISPBin.Initialize ( );
		Dialogs.DISPLAY.Initialize ( );
		Dialogs.WPROC.Initialize ( );
		Dialogs.ACP.Initialize ( );
	}
}

//////////////////////////////////////////////////////////////////////////
//
//	cDialogs::_MainDialog::SaveReg
//		Calls the individual Save functions for each dialog.
//		These Save functions in turn save the dialog's data to
//		the registry
//
int cDialogs::_MainDialog::SaveReg ( HWND hDlg )
{
	Dialogs.General.Save ( );
	Dialogs.Videocap.Save ( );
	Dialogs.EXTRACT.Save ( );
	Dialogs.MESSAGE.Save ( );
	Dialogs.SCRATCH.Save ( );
	Dialogs.License.Save ( );
	Dialogs.WISPBin.Save ( );
	Dialogs.DISPLAY.Save ( );
	Dialogs.WPROC.Save ( );
	Dialogs.ACP.Save ( );

	return 0;
}
