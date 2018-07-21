/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
*/

//////////////////////////////////////////////////////////////////////////
//	This file contains the class declarations for the custom controls

#if !defined CCONTROLS_H
#define CCONTROLS_H

#include <windows.h>
#include <winnetwk.h>
#include <commctrl.h>
#include <direct.h>
#include <malloc.h>
#include <stdio.h>
#include <WINIOCTL.H>
#include "rResources.h"
#include "rPrototypes.h"

//////////////////////////////////////////////////////////////////////////
//	Define directives for rControls
//

//	File attributes
#define FA_COMPRESSED	0
#define FA_ARCHIVE		1
#define FA_DIRECTORY	2
#define FA_SYSTEM		3
#define FA_HIDDEN		4
#define FA_READONLY		5
#define FA_NORMAL		6
#define FA_TEMP			7
//	Max lengths for character strings
#define MaxFileLen 256
#define MaxPath 1024
//	Type value for TV_DirListControl
#define	TVL_DIRS		0
#define	TVL_SERVERS		1
#define	TVL_DRIVES		2
#define	TVL_SHARES		3
//	Network defines
#define	MAX_ENUM_RESOURCES	0xFFFFFFFF
#define	ENUM_BUF_SIZE	sizeof ( NETRESOURCE )
//	Network object types used in tree view control
#define	TVNet_Root			1
#define	TVNet_MSProvider	2
#define	TVNet_Domain		3
#define	TVNet_Server		4
#define	TVNet_Share			5
#define	TVNet_NWProvider	6
#define	TVNet_RemoteDir	7
#define	TVNet_LocalDir		8
#define	TVNet_LocalDrv		9


class rControls {
	public:
		int Init ( HINSTANCE hInst );
		HINSTANCE hInst;
		class _StatusBar {
			public:
				//	Variables
				HWND hClock, hStatus, hStatusBar;
				char sOutput[100];									//	Buffer to hold the text that displays in the status bar
				//	Functions
				int Create ( HINSTANCE hInst, HWND hAppWnd );
				int ChngTime ( );
				int UpdateMsg ( char *sMsg );						//	Updates the message in the status bar
				int Resize ( HWND hAppWnd );
		} StatusBar;
		class _Ctrl {
			public:
				//	Variables
				//	Funstions
				int GetDrvInfo ( );
				int GetServerInfo ( );
				int GetFileAttribs ( WIN32_FIND_DATA *FileData, int FAttribs[8] );
				//	Structs
				struct _DrvInfo {
					BOOL Exist;
					char sCFDes[7],
						sLbl[_MAX_FNAME];
					unsigned int Type;
				} DrvInfo[26];
				//	Classes
				class _tvList {
					public:
						//	Variables
						HTREEITEM tvgMyComp, tvgNet;
						char stv_StartDir[_MAX_PATH];
						TV_ITEM tvChildDirs[250];
						char stvLabels[100][_MAX_PATH];
						char sPathTok[100][_MAX_PATH];
						OSVERSIONINFO OSVer;
						TV_ITEM tvClicked;
						//	Functions
						int Initialize ( HWND hDlg, HWND hTV_Ctrl );
						int GetSubDirs ( HWND hDlg, TV_ITEM tvItem, UINT tvAction );
						int RetreiveDirs ( char *sSrchPath, TV_ITEM SelItem, HWND hTVWnd );
						int DefaultToDir ( HWND, HWND );
						void GetNetObjects ( TV_ITEM, HWND );
						int RetPath ( HWND hDlg, HWND hTVWnd );
						int GetPathToks ( TV_ITEM, HWND  );
						void GetRemoteDirs ( TV_ITEM, NETRESOURCE, char *UNCName, HWND );
						void InsertNetItems ( TV_ITEM, HANDLE, HWND );
				} tvList;
		}Ctrl;
		class _DrawTools {
			public:
				//	Variables
				HFONT SBFont;
				_DrawTools ( );
				//	Functions
				HBRUSH MakeBrush ( int i_Red, int i_Green, int i_Blue );
				HBRUSH MakeSysBrush ( int i_Index );
		}DrawTools;
		////////////////////////////////////////////////////////////////////
		//		clsDialogManager - Controls the behaviour of dialog boxes
		class _Dialogs {
			public:
				//	Functions
				int DrawDirCB ( HWND hDlg, LPARAM lp );
				//	Classes
				class _SelectDirDlg {											//	Controls the Select directory dialog
					public:
						//	Variables
						HWND Caller;
						BOOL MultiSelect;
						int Type;
						char sTitle[80];
						//	Functions
						int Create ( HWND hParent, /*HINSTANCE hInst, */
							char *sStartDir, BOOL MultiSelect,
							int Type, HWND Caller, char *sTitle );
						int Initialize ( HWND hDlg, HINSTANCE hInst );
						int AddClicked ( HWND hDlg );
						int RemoveClicked ( HWND hDlg );
						int MUpClicked ( HWND hDlg );
						int MDownClicked ( HWND hDlg );
						int OKClicked ( HWND hDlg );
				}SelectDirDlg;
		}Dialogs;
};

extern rControls	CustCtrl;


#endif

/*
**	History:
**	$Log: rControls.h,v $
**	Revision 1.7  2007/07/31 16:49:43  gsl
**	fix warnings
**	
**	Revision 1.6  2003/06/18 16:43:07  gsl
**	Add CVS header and history
**	
**
*/
