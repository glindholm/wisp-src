/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
******************************************************************************
*/

/*
**	File:		#Classes.h
**
**	Project:	WISPTran Configuration Utility
**
**	Purpose:	Declares class that are used in the application
**
*/

#ifndef CLASSES_H
#define CLASSES_H
/*
**	Includes
*/
#include "resource.h"
#include "rControls.h"
//#include "CTL3D.H"

/*
**	cDialogs
**	Encapsulates the functionality of the indivdual dialog boxes that appear as
**	option sheets in the tab control.
*/
class cDialogs {
	public:
		// This is the main application window
		class _MainDialog {
			public:
				int Initialize ( HWND hDlg );
				int ChngTab ( HWND hDlg );
				int SaveReg ( HWND hDlg );
				void RemoveClicked ( );
		}MainDialog;
		class _General {
			public:
				HWND hGeneral;
				BOOL ivWISPDir,
					ivWISPConfig,
					ivTempDir,
					ivUserDir;
				int Initialize ( );
				int InitServer ( HKEY );
				int InitWISPDir ( HKEY );
				int InitWISPConf ( HKEY );
				int Save ( );
				// The HWND parameter passed to the ChkInvalid
				//	function is the handle of the control to be 
				//	validated.
				void ChkInvalid ( HWND );
		}General;
		class _Videocap {
			public:
				HWND hVideocap;
				BOOL ivVideoCapDir,
					ivWispTerm;
				int Initialize ( );
				int Save ( );
				void ChkInvalid ( HWND );
		}Videocap;
		class _EXTRACT {
			public:
				HWND hEXTRACT;
				int Initialize ( );
				int Save ( );
		}EXTRACT;
		class _MESSAGE {
			public:
				HWND hMESSAGE;
				BOOL ivShareDir;
				int Initialize ( );
				int Save ( );
				void ChkInvalid ( HWND );
		}MESSAGE;
		class _SCRATCH {
			public:
				HWND hSCRATCH;
				BOOL ivScratchMode;
				int Initialize ( );
				int Save ( );
				void ChkInvalid ( HWND );
		}SCRATCH;
		class _License {
			public:
				HWND hLicense;
				BOOL ivLicenseFile;
				int Initialize ( );
				int Save ( );
				void ChkInvalid ( HWND );
				void UpdateLicenseFile();
				void CalcLicenseFile(char* file);
		}License;
		class _WISPBin {
			public:
				HWND hWISPBin;
				BOOL ivWispExe,
					ivWEditorExe,
					ivDisplayExe;
				int Initialize ( );
				int Save ( );
				void ChkInvalid ( HWND );
		}WISPBin;
		class _DISPLAY {
			public:
				HWND hDISPLAY;
				BOOL ivUse8Bit;
				int Initialize ( );
				int Save ( );
				void ChkInvalid ( HWND );
		}DISPLAY;
		class _WPROC {
			public:
				HWND hWPROC;
				BOOL ivWPROC;
				int Initialize ( );
				int Save ( );
				void ChkInvalid ( HWND );
		}WPROC;
		class _ACP {
			public:
				HWND hACP;
				BOOL ivACPConfigDir,
					ivACPMap;
				int Initialize ( );
				int Save ( );
				void ChkInvalid ( HWND );
		}ACP;
};
///////////////////////////////////////////////////////////////////////////////
//
//	cApplication
//		Controls general aspects of the application
//
class cApplication {
	public:
		// hInst stores the handle of the current instance of the application
		HINSTANCE hInstGlb;
		HWND hAppWnd,		// Handle of the application window, which is never actually used
			hMainDlg;	// Handle of the main dialog box window
		// The _Env struct holds information related to the operating environment,
		//  such as the OS version, and the three main WISP fields.
		struct _Env {
			// Stores info on the operating system
			struct _OS {
				DWORD dwPlatform,
					dwMajorVer,
					dwMinorVer;
			} OS;
			// Stores the values in the coresponding fields of the
			//   General option tab.
			char sWISPServer[32],
				sWISPDir[_MAX_PATH],
				sWISPConfig[_MAX_PATH];
		} Env;
		int ImportRegData ( HWND hDlg );
		// Initializes the application - called from WinMain
		int InitApp ( HINSTANCE hInst );
		void MsgLoop ( );
		int DeleteReg ( );
		void WriteRegFile ( );
		bool HaveRequiredPermissions();
};

#endif 

/*
**	History:
**	$Log: #Classes.h,v $
**	Revision 1.10  2007/08/02 13:22:51  gsl
**	Remove CTL3Dxxxx stuff, obsolete.
**	
**	Revision 1.9  2007/07/31 16:49:43  gsl
**	fix warnings
**	
**	Revision 1.8  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.7  2002/05/20 19:50:25  gsl
**	Update from VSS
**	
 * 
 * 2     11/09/01 3:04p Khunter
 * Added permission checking
 * 
 * 1     7/13/01 11:14a Glindholm
 * 
 *    Rev 1.0   21 Oct 1997 12:57:48   scass
 *  
**	Revision 1.6  1997-03-21 17:31:07-05  gsl
**	Add methods to License
**
**	Revision 1.1  1996-06-21 09:26:29-07  gsl
**	Initial revision
**
**
**
*/

