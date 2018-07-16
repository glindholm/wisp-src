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

//////////////////////////////////////////////////////////////////////////
//
//	This file contains all of the class declarations
//
#if !defined  CLASSES_H
#define CLASSES_H

#define OEMRESOURCE
#include <windows.h>
#include <stdio.h>
#include <malloc.h>
#include <winnetwk.h>
#include <commctrl.h>
#include <direct.h>
#include <WINIOCTL.H>
#include <winnetwk.h>
#include <process.h>
#include <rControls.h>
#include "#Defines.h"
#include "#Prototypes.h"
#include "resource.h"
#include "CTL3D.H"

//*************************************************************************
//
//	cWindows - Controls the windows of the application
//
class cWindows {
	public:
		RECT AppClntRect;				// Holds the coordinates of the client area of the main application window.
		WNDCLASS GenWC;					// Holds generic window class settings.  All other window classes are based on this one.
		HMENU hAppMenu;					// Handle to the application menu.
		HFONT StdFont,					// Font used for printing to the text output window.
			TabFont;					// Font used for printing to the option tabs and other window controls and elements that don't require a monospaced font.
		cWindows ( );					// Class constructor.
		~cWindows ( );					// Class destructor.
		PrintOutputWnd ( );				// Sends the contents of the output window to the printer.
		CreateWnd ( HINSTANCE hInst );	// Creates the window classes.  Args: hInst = the current application instance.
		ShowWnd ( HINSTANCE hInst );	// Initializes and displays the windows. Args: hInst = the current application instance.
		SizeAppWindow ( );				// Resizes all child windows when the main application window is resized, so that they fill the application window.
		struct _hWnd {
			HWND hMenuDiv,		// Menu divider, the line at the top of the window that seperates the menu from the client portion of the application window.
				hCOBOLOutWnd,	// COBOL output dialog window.
				hWISPOutWnd,	// WISP output dialog window.
				hWCmdLine,		// WISP command line window.
				hCCmdLine,		// COBOL command line window.
				hWOutput,		// WISP output window.
				hCOutput,		// COBOL output window.
				hWISPOpWnd,		// WISP option dialog.
				hCOBOLOpWnd,	// COBOL option dialog.
				hTabWnd,		// Tab control for the option sheets.
				hExecuteBtn,	// Translate button.
				hExecCompBtn,	// Translate & Compile button.
				hShellWnd;		// Main app window.
		} hWnd;
		//****************************************************************
		//
		//	_OptionWindow - Controls the WISP and COBOL option windows
		//
		class _OptionWindow {
			public:
				char ch_DirPath[_MAX_PATH];			// Holds the path for the directory of the files to be WISPed.
				RECT OptionsRect;					// Holds the coordinates of the Options window.
				AddTargets ( char *ch_FileSpecs );	// Adds files to the list of files to be WISPed and/or compiled.
				BClicked ( LPARAM lp );				// Determines which button was clicked, what the context of the button was at the time it was clicked, and calls the appropriate function.
				TabClicked ( );						// Responds to one of the option window tabs being clicked.
				UnDimCOptions ( );					// Undims (enables) the controls in the COBOL option window.
				SetCOBOLOptions ( BOOL Exec );		// Copies the settings that the user currently has in the COBOL option window, to a struct in memory.
				CreateCCtls ( );					// Creates and displays the controls in the COBOL option window.
				CBClicked ( LPARAM lp );			// Responds to a button being clicked in the COBOL option window.
				int CurLang;						// Keeps track of which COBOL version is currently selected.
				CreateWCtls ( );					// Creates, displays, and initializes the controls in the WISP option window.
				SetWISPOptions ( BOOL Exec);		// Copies the settings that the user currently has in the WISP option window, to a struct in memory.
				UnDimWOptions ( );					// Undims (enables) the controls in the WISP option window.
				WBClicked ( LPARAM lp );			// Responds to a button being clicked in the WISP option window.
				RECT OptionShtRect;
				struct _WCtls {
					HWND
						l_TargetFile,
						_TargetFile,
						_TargetBrs,				// Handle to the Target File controls
						l_FromDir,
						_DirPath,
						_1,
						_F,
						_x,
						_q,
						_S,
						_D,
						_m,
						_f,
						_w,
						_I,
						l_I,
						_IBrs,
						_K,
						l_K,
						_KBrs,
						_V,
						l_V,
						_L,
						_l,
						_O,
						l_O, _OBrs,
						_G4,
						_T,
						_e,
						_W, l_W, _WBrs,
						_4,
						_P, l_P, _PBrs,
						_M,
						_X;
				}WCtls;
				struct _CCtls {
					HWND
						_TargetFile, l_TargetFile, _TargetBrs,	//	Target file
						l_FromDir, _DirPath,					//	Static text window to display the directory of the currently selected files
						_PathToComp, l_PathToComp, _CompBrs,	//	Path and name of the cobol executable file
						_CFlags, l_CFlags,						//	Input field for the command line flags
						_OutputDir, l_OutputDir, _OutputDirBrs,	//	Input field for the output directory
						_ObjExt, l_ObjExt,						//	Input field for the obj extension
						_LangGroup, _AcuBtn, _MFBtn;			//	Radio buttons for selecting the version of COBOL
				}CCtls;
		}OptionWnd;
};
//************************************************************************
//
//	cApplication - Controls general aspects of the application
//
class cApplication {
	public:
		cApplication ( );		//	Class constructor
		~cApplication ( );		//	Class destructor
		AppInit ( HINSTANCE hInst );	//	Initializes the app
		void MsgLoop ( );		//	Main message loop
		CloseApp ( int ExitCode, int WinError );	//	Closes the app
		void CheckMsg ( );		//	Checks if there are any messages waiting in the que - if so, handles them (used for processes that take a long time, this can be called intermittenly so that the user does not have to wait for the process to end
		GetReg ( );			//	Gets the configuration and environment variables from the registry
		SetReg ( );
		GetDrvInfo ( );
		struct _Thread {
			HANDLE hPreEntrySmphr;
			DWORD Param;
		} Thread;
		class _CmdLine {
			public:
				BOOL Break;					//	Flag used to break a long process if the user clicks the cancel button
				Execute ( 
					char *ch_CmdLine,
					char *ch_FileName,
					int i_FilesRemain,
					int i_FilesSelect,
					int i_TransComp,
					int tc );				//	Executes WISP.EXE, passing the above switches
				Build ( BOOL Exec,
					int i_TransComp );		//	Builds the command line based on the info entered by the user
				WriteLog ( 
					char *ch_FileName,
					char *ch_Msg );			//	Writes the same data that is sent to the output window to a log file for each file being wisped
				DeleteLog ( 
					char *ch_FileName );    //  Deletes the .LOG file
				struct _WOptions {			//	Encasulates the states of the controls/command line switches
					BOOL
						_1,
						_4,
						_F,
						_D,
						_e,
						_f,
						_l,
						_L,
						_m,
						_M,
						_q,
						_G4,
						_S,
						_T,
						_w,
						_x,
						_X;
					char
						ch_TargetFile[_MAX_PATH],
						ch_K[_MAX_PATH],
						ch_O[_MAX_PATH],
						ch_P[_MAX_PATH],
						ch_V[4],
						ch_W[_MAX_PATH],
						ch_I[_MAX_PATH];
				}*WOptions;
				struct _COptions {					//	Encasulates the states of the controls/command line switches
					int _Lang;						//	Tracks whether the AcuCOBOL or the MicroFocus COBOL is selected
					char
						ch_CFlags[GenText],
						ch_ObjExt[GenText],
						ch_OutputDir[_MAX_PATH], 
						ch_PathToComp[_MAX_PATH],
						ch_TargetFile[_MAX_PATH];
				}*COptions;
		}CmdLine;
		class _FSettings {
			public:
				GetMaster ( );							//	Reads the default settings file for the directory
				SetMaster ( );							//	Writes the default settings file for the directory
				GetSingle ( char *ch_FileName );		//	Reads the settings file a the currently selected file
				SetSingle ( char *ch_FileName );		//	Writes the settings file for the currently selected file
				ChkOptFileExist ( char *ch_FileName );	//	Checks if a settings file exists for the currently selected file
		}FSettings;
		struct _Environment {
			char ch_DirPath;							//	Holds the directory path to the application
			int CurAppStatus;							//	Holds the current status of the applicaiton, e.g. startup, idle, shutting down, etc.
			BOOL PaintTabs;
			HWND WLastFocus,
				CLastFocus;
			HINSTANCE hInstGlb;							//	Global instance handle
			struct _SysInfo {
				struct _DrvInfo {
					BOOL Exist;
					char ch_CFDes[7],
						Lbl[_MAX_PATH];
					int Type;
				}DrvInfo[26];
				DWORD MajorVer,							//	Windows major version
					MinorVer,							//	Windows minor version
					Platform,							//	The platform of the os
					ScrRes[2];							//	The screen resolution
				char ch_WinDir[_MAX_PATH];				//	Path of the windows dir
			}SysInfo;
			struct _RegInfo {
				char ch_WPath[_MAX_PATH],
					ch_ACPath[_MAX_PATH],
					ACOBOLFlags[256],
					ACOBOLOutDir[_MAX_PATH],
					ACOBOLObjExt[_MAX_EXT],
					ch_MCPath[_MAX_PATH],
					MCOBOLFlags[256],
					MCOBOLOutDir[_MAX_PATH],
					MCOBOLObjExt[_MAX_EXT];
				int COBOLLang;
				int InitHSize,
					InitVSize;
			} RegInfo;
		}*Env;
};


#endif

/*
**	History:
**	$Log: #Classes.h,v $
**	Revision 1.10  2003/06/18 16:55:56  gsl
**	Add /F remove /C (replace)
**	
**	Revision 1.9  2003/06/18 16:43:06  gsl
**	Add CVS header and history
**	
**
*/
