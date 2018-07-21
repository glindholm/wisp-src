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

///////////////////////////////////////////////////////////////////////////////
//
//	This file contains the function bodies for the custom controls
//

#include "rControls.h"
#include "rResources.h"
#include "rPrototypes.h"

rControls	CustCtrl;

///////////////////////////////////////////////////////////////////////////////
//
//	rControls::Init
//		Performs required initialization for rControls
//
int rControls::Init ( HINSTANCE hInst )
{
	CustCtrl.hInst = hInst;

	return 0;
}

///////////////////////////////////////////////////////////////////////////////
//
//	rControls::_Ctrl::_tvList::GetSubDirs
//		Inserts subdirectories into tree view ctrl when a dir is
//		expanded
//
int rControls::_Ctrl::_tvList::GetSubDirs
	( HWND hDlg, TV_ITEM tviClicked, UINT tvAction )
{
	HWND hTVWnd;
	TV_ITEM tvParent;
	int cnt = 0;
	char sSrchPath[MaxPath];
	
	hTVWnd = GetDlgItem ( hDlg, TV_AvailDirs );
	//	Check if the item has been expanded previously
	tviClicked.mask = TVIF_STATE | TVIF_HANDLE;
	tviClicked.stateMask = TVIS_EXPANDEDONCE;
	SendMessage ( hTVWnd, TVM_GETITEM, 0, (LPARAM) &tviClicked );

	//	If it has been then return without getting the dir info
	tviClicked.state = tviClicked.state << 25;
	tviClicked.state = tviClicked.state >> 31;
	if ( tviClicked.state == TRUE ) return 0;

	//	Check if the item is expanding or contracting
	switch ( tvAction ) {
		case TVE_EXPAND:
			//	Get the label of the item that is expanding
			tviClicked.pszText = (char *) malloc ( MaxFileLen );
			ZeroMemory (
				tviClicked.pszText, _msize (tviClicked.pszText) );
			tviClicked.mask = TVIF_CHILDREN | TVIF_HANDLE | TVIF_TEXT;
			tviClicked.cchTextMax = MaxFileLen;
			TreeView_GetItem ( hTVWnd, &tviClicked );

			//	Get the handles to the root items (network and my computer)
			TV_ITEM tvNet, tvMyComp;
			ZeroMemory ( &tvNet, sizeof ( TV_ITEM ));
			ZeroMemory ( &tvMyComp, sizeof ( TV_ITEM ));
			tvMyComp.hItem = TreeView_GetRoot ( hTVWnd );
			tvNet.hItem =
				TreeView_GetNextSibling ( hTVWnd, tvMyComp.hItem );

			//	if the item being expanded is a drive 
			tvParent.hItem =
				TreeView_GetParent ( hTVWnd, tviClicked.hItem );
			TV_ITEM tviPrnt;
			tviPrnt = tviClicked;
			ZeroMemory ( sPathTok, sizeof ( sPathTok ));
			strcpy ( sPathTok[0], tviPrnt.pszText );
			cnt = 1;

			//	Get the full path to the selected dir
			GetPathToks ( tviClicked, hTVWnd );
			cnt = 0;
			while ( strlen ( sPathTok[cnt] ) > 0 ) cnt++;
			cnt = cnt - 2;
			if ( !strcmp ( sPathTok[cnt+1], "My Computer" )) {
				if ( cnt >= 0 ) {
					ZeroMemory ( sSrchPath, sizeof ( sSrchPath ));
					memcpy ( sSrchPath, &sPathTok[cnt]
						[strlen( sPathTok[cnt] )-3], 2 );
					sSrchPath[2] = '\\';
					cnt--;
					while ( cnt >= 0 ) {
						strcat ( sSrchPath, sPathTok[cnt] );
						strcat ( sSrchPath, "\\" );
						cnt--;
					}
					strcat ( sSrchPath, "*.*" );
				}
				if ( strcmp ( tviClicked.pszText, "My Computer" ))
					RetreiveDirs ( sSrchPath,
						tviClicked, hTVWnd );
			}
			//	If the item clicked is a remote object
			if ( !strcmp ( sPathTok[cnt+1], "Network" )) {
				GetNetObjects ( tviClicked, hTVWnd );
			}
			//	Create string that will appear in the ctlRetPath control
			RetPath ( hDlg, hTVWnd );
			break;
		case TVE_COLLAPSE:
			ZeroMemory ( tvChildDirs, sizeof ( tvChildDirs ));
			ZeroMemory ( stvLabels, sizeof ( stvLabels ));
			break;
	}
	return 0;
}
///////////////////////////////////////////////////////////////////////////////
//
//	rControls::_Ctrl::_tvList::GetPathToks
//		Gets the path tokens that lead to the item that is expanding
//
int rControls::_Ctrl::_tvList::GetPathToks ( TV_ITEM tviClicked, HWND hTVWnd )
{
	TV_ITEM tviPrnt = tviClicked;
	int cnt = 1;
	ZeroMemory ( sPathTok, sizeof ( sPathTok ));

	//	Get the full path to the selected dir
	tviPrnt.mask		= TVIF_TEXT;
	tviPrnt.cchTextMax	= _MAX_PATH;
	tviPrnt.pszText		= (char *) malloc ( MaxFileLen );
	TreeView_GetItem ( hTVWnd, &tviPrnt );
	strcpy ( sPathTok[0], tviPrnt.pszText );
	while ( TRUE ) {
		if (( tviPrnt.hItem = TreeView_GetParent 
			( hTVWnd, tviPrnt.hItem )) == NULL ) break;
//		tviPrnt.mask = TVIF_TEXT;
//		tviPrnt.cchTextMax = _MAX_PATH;
//		tviPrnt.pszText = (char *) malloc ( MaxFileLen );
		TreeView_GetItem ( hTVWnd, &tviPrnt );
		strcpy ( sPathTok[cnt], tviPrnt.pszText );
		cnt++;
	}

	free(tviPrnt.pszText);
	return 0;
}
///////////////////////////////////////////////////////////////////////////////
//
//	rControls::_Ctrl::_tvList::RetPath
//		Puts the full path of the item that is selected into the
//		static control below the tree view control, this is also the
//		string that is returned from the dialog box
//
int rControls::_Ctrl::_tvList::RetPath ( HWND hDlg, HWND hTVWnd )
{
	int cnt = 0;
	char sRetPath[_MAX_PATH];
	TV_ITEM tvSel;

	tvSel.hItem = TreeView_GetSelection ( hTVWnd );
	GetPathToks ( tvSel, hTVWnd );

	while ( strlen ( sPathTok[cnt] ) > 0 ) cnt++;
	BOOL isLocal = TRUE;
	if ( strcmp ( sPathTok[cnt-1], "My Computer" ) == 0 )
		isLocal = TRUE;
	else isLocal = FALSE;
	//	If it is a local path or mapped drive
	if ( isLocal == TRUE ) {
		if ( strlen ( sPathTok[1] ) > 0 ) {
			//	Find the end of the path tokens
			ZeroMemory ( sRetPath, _MAX_PATH );
			memcpy ( sRetPath, 
				&sPathTok[cnt-2][strlen (
				sPathTok[cnt-2] )-3], 2 );
			strcat ( sRetPath, "\\" );
			cnt--;
			while ( cnt > 1 ) {
				strcat ( sRetPath, sPathTok[cnt-2] );
				strcat ( sRetPath, "\\" );
				cnt--;
			}
			sRetPath[strlen ( sRetPath)-1] = '\0';
			if ( strlen ( sRetPath ) == 2 ) 
				strcat ( sRetPath, "\\" );
		}
		else {
			strcpy ( sRetPath, "No valid object selected." );
		}
	}
	//	If it is a UNC name
	else {
		if ( strlen ( sPathTok[3] ) > 0 ) {
			//	Find the end of the path tokens
			ZeroMemory ( sRetPath, _MAX_PATH );
			strcpy ( sRetPath, sPathTok[cnt-4] );
			strcat ( sRetPath, "\\" );
			cnt = cnt - 5;
			while ( cnt >= 0 ) {
				strcat ( sRetPath, sPathTok[cnt] );
				strcat ( sRetPath, "\\" );
				cnt--;
			}
			sRetPath[strlen ( sRetPath)-1] = '\0';
			if ( strlen ( sRetPath ) == 2 )
				strcat ( sRetPath, "\\" );
		}
		else {
			strcpy ( sRetPath, "No valid object selected." );
		}
	}

	SetWindowText ( GetDlgItem ( hDlg, ctlRetPath ), sRetPath );
	ZeroMemory ( sPathTok, sizeof ( sPathTok ));

	return 0;
}
///////////////////////////////////////////////////////////////////////////////
//
//	rControls::_Controls::_tvList::GetNetObjects
//		Retrieves network objects so they can be displayed in the
//		tree view control
//
void rControls::_Ctrl::_tvList::GetNetObjects ( TV_ITEM tviClicked, HWND hTVWnd )
{
	HANDLE hEnum;
	NETRESOURCE NetRes[100];
	TV_ITEM tvDomain;
	TV_INSERTSTRUCT tvisDomain;
	DWORD dList = MAX_ENUM_RESOURCES, szBuf = ENUM_BUF_SIZE * 100;
	TV_ITEM tvParent;
	char NetToks[100][_MAX_FNAME];
	int cnt = 0;
	char *NetComp;
	int EnumCnt = 0;

	//	Initialize the character strings
	ZeroMemory ( &NetToks, sizeof ( NetToks ));
	ZeroMemory ( &NetRes, sizeof ( NetRes ));
	//	Get info on the item that was clicked
	strcpy ( NetToks[0], tviClicked.pszText );
	tvParent.hItem = 
		TreeView_GetParent ( hTVWnd, tviClicked.hItem );
	//	If the item clicked was not the network root...
	if ( tviClicked.lParam != TVNet_Root ) {
		cnt = 1;
		//	Initialize the members of the TV_ITEM struct so that
		//	we can get the label
		tvParent.mask		= TVIF_TEXT | TVIF_HANDLE;
		tvParent.cchTextMax	= _MAX_FNAME;
		tvParent.pszText	= (char *) malloc ( _MAX_FNAME );
		//	Get the tokens leading to the item that was clicked -
		//	This loop gets the text labels associated with the tree view items
		while ( cnt < 100 ) {
			TreeView_GetItem ( hTVWnd, &tvParent );
			strcpy ( NetToks[cnt], tvParent.pszText );
			tvParent.hItem = 
				TreeView_GetParent ( hTVWnd, tvParent.hItem );
			if ( tvParent.hItem == NULL ) break;
			cnt++;
		}
		cnt--;
		if ( tviClicked.lParam == TVNet_RemoteDir ) {
			char SearchPath[_MAX_PATH];
			int cnt1 = 0;
			ZeroMemory ( SearchPath, sizeof ( SearchPath ));
			while ( NetToks[cnt1][0] != '\\' ) cnt1++;
			strcpy ( SearchPath, NetToks[cnt1] );
			strcat ( SearchPath, "\\" );
			cnt1--;
			while ( cnt1 >= 0 ) {
				strcat ( SearchPath, NetToks[cnt1] );
				strcat ( SearchPath, "\\" );
				cnt1--;
			}
			strcat ( SearchPath, "*.*" );
			RetreiveDirs ( SearchPath, tviClicked, hTVWnd );
			return;
		}
		//	Find the item that was clicked, on the network
		WNetOpenEnum (
			RESOURCE_GLOBALNET, RESOURCETYPE_ANY,
			RESOURCEUSAGE_CONTAINER, NULL, &hEnum );
		dList = MAX_ENUM_RESOURCES; szBuf = ENUM_BUF_SIZE * 100;
		//	If user is running Win95 - 95 returns one network provider each time the
		//		WNetEnumResource function is called (whereas NT returns an array as it's supposed to)
		if ( OSVer.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS ) {
			//	Get the NETRESOURCE struct for each network provider that is running
			while ( WNetEnumResource ( hEnum, &dList,
				&NetRes[0], &szBuf ) != ERROR_NO_MORE_ITEMS ) {
				//	If the string returned in the NETRESOURCE struct is not the one we're looking
				//		for then restart the loop
				if ( strcmp ( NetRes[0].lpProvider,
					NetToks[cnt] ) != 0 ) continue;
				//	otherwise, 
				else {
					WNetCloseEnum ( hEnum );
					break;
				}
			}
		}
		else {
			WNetEnumResource ( hEnum, &dList, NetRes, &szBuf );
		}
		//	Enumerate the network resources - this starts at the root of the network and keeps
		//		enumerating until it reaches the item that was clicked
		while ( TRUE ) {
//			WNetEnumResource
//			( hEnum, &dList, NetRes, &szBuf ) != ERROR_NO_MORE_ITEMS ) {
			char UNCName[_MAX_PATH];
			//	If the remote name is not valid then use the provider name (this is to check the network type)
			while ( dList > 0 ) {
				//	If the lpRemoteName string is not valid then use the lpProvider string instead (this
				//		seems to be the case only when enumerating the network provider and only on Win95
				if ( NetRes[dList-1].lpRemoteName == NULL )
					NetComp = NetRes[dList-1].lpProvider;
				else NetComp = NetRes[dList-1].lpRemoteName;
				//	The remote name of shares is returned as a UNC name.  Since the tree view control only
				//		has the share name and not the server name then we must put the server name before
				//		the share name to create a UNC name.
				if ( tviClicked.lParam == TVNet_Share ) {
					if ( cnt == 0 ) {
						//	Create the UNC name to pass to the GetRemoteDirs func
						char sTemp[_MAX_PATH];
						ZeroMemory ( sTemp,
							sizeof ( sTemp ));
						sprintf ( UNCName, "%s\\%s",
							NetToks[1],
							NetToks[0] );
						int ShareCnt = 0;
						while ( strcmp ( UNCName,
							NetRes[ShareCnt].
							lpRemoteName ) != 0 )
							ShareCnt++;
						GetRemoteDirs ( tviClicked,
							NetRes[ShareCnt],
							UNCName, hTVWnd );
						return;
					}
				}
				//	Compare the remote name (as per the NETRESOURCE struct) with the label of the item that
				//		was clicked.  If they do not match then go on to the next item
				if ( strcmp ( NetComp, NetToks[cnt] ) != 0 ) {
					dList--;
					continue;
				}
				//	...If they do match then break here
				else break;
			}
			//	Open the item found above so that we can enumerate the resources therein...
			WNetCloseEnum ( hEnum );
			int RetVal = 0;
//			if ( NetToks[cnt][0] == '\\' ) 
			if ( (RetVal = WNetOpenEnum (
				RESOURCE_GLOBALNET, RESOURCETYPE_DISK,
				RESOURCEUSAGE_CONNECTABLE |
				RESOURCEUSAGE_CONTAINER,
				&NetRes[dList-1], &hEnum )) == NO_ERROR ) {
				cnt--;
				dList = MAX_ENUM_RESOURCES;
				szBuf = ENUM_BUF_SIZE * 100;
				if ( cnt < 0 ) break;
				ZeroMemory ( NetRes, sizeof ( NetRes ));
				if ( WNetEnumResource
					( hEnum, &dList, NetRes, &szBuf ) !=
					ERROR_NO_MORE_ITEMS )
					continue;
				else break;
			}
			//	If the item cannot be opened...
			else {
				//	If the item is a share then call the GetRemoteDirs func
				tviClicked.mask		=	TVIF_CHILDREN;
				tviClicked.cChildren	=	0;
				TreeView_SetItem ( hTVWnd, &tviClicked );
				return;
			}
		}
		//	The item that was clicked has just been opened so we can now enumerate the resources
		//		therein (all the preceding crap was just to get to the item that was clicked)
		InsertNetItems ( tviClicked, hEnum, hTVWnd );
	}
	//	If the item clicked was the network root
	if ( tviClicked.lParam == TVNet_Root ) {
		OSVERSIONINFO OSVer;
		WNetOpenEnum (
			RESOURCE_GLOBALNET, RESOURCETYPE_ANY,
			RESOURCEUSAGE_CONTAINER, NULL, &hEnum );
		dList = MAX_ENUM_RESOURCES; szBuf = ENUM_BUF_SIZE * 100;
		//	Get the operating system version being used
		OSVer.dwOSVersionInfoSize = sizeof ( OSVERSIONINFO );
		GetVersionEx ( &OSVer );
		//	If it is running Win95
		if ( OSVer.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS ) {
			//	Repeat loop for each network provider because the WNetEnumResource function only returns
			//		one provider at a time on Win95
			while ( WNetEnumResource ( hEnum, &dList,
				NetRes, &szBuf )	!= ERROR_NO_MORE_ITEMS ) {
				tvDomain.mask		= 
					TVIF_CHILDREN | TVIF_TEXT | TVIF_PARAM;
				tvDomain.cchTextMax	= _MAX_FNAME;
				tvDomain.pszText	= 
					NetRes[dList-1].lpProvider;
				tvDomain.cChildren	= 1;
				//	If the item returned is the Microsoft Windows Network then set the type to TVNet_MSProvider
				if ( strstr ( NetRes[dList-1].lpProvider,
					"Microsoft" ) != NULL )
					tvDomain.lParam	= TVNet_MSProvider;
				//	Otherwise set it to TVNet_NWProvider
				else tvDomain.lParam	= TVNet_NWProvider;
				tvisDomain.hParent	= tviClicked.hItem;
				tvisDomain.hInsertAfter	= TVI_SORT;
				tvisDomain.item		= tvDomain;
				//	Insert the item
				TreeView_InsertItem ( hTVWnd, &tvisDomain );
				dList = MAX_ENUM_RESOURCES;
				szBuf = ENUM_BUF_SIZE * 100;
			}
		}
		//	If it is running WinNT then you don't need to do all that nonesense above
		if ( OSVer.dwPlatformId  == VER_PLATFORM_WIN32_NT ) {
			//	If there is stuff to be found
			if ( WNetEnumResource ( hEnum, &dList,
				NetRes, &szBuf ) != ERROR_NO_MORE_ITEMS ) {
				//	Repeat loop for each item returned
				while ( dList > 0 ) {
					tvDomain.mask		=
						TVIF_CHILDREN |
						TVIF_TEXT |TVIF_PARAM;
					tvDomain.cchTextMax	= _MAX_FNAME;
					tvDomain.pszText	=
						NetRes[dList-1].lpProvider;
					tvDomain.cChildren	= 1;
					//	If the item is the Microsoft Windows Network then set the type to TVNet_MSProvider
					if ( strstr ( NetRes[dList-1].lpProvider,
						"Microsoft" ) != NULL )
						tvDomain.lParam	=
							TVNet_MSProvider;
					//	otherwise, set the type to TVNet_NWProvider
					else tvDomain.lParam	= TVNet_NWProvider;
					tvisDomain.hParent	= tviClicked.hItem;
					tvisDomain.hInsertAfter	= TVI_SORT;
					tvisDomain.item		= tvDomain;
					//	Insert the item
					TreeView_InsertItem ( hTVWnd, &tvisDomain );
					dList--;
				}
			}
		}
	}
	//	If the handle was opened, then close it
	if ( hEnum != NULL ) WNetCloseEnum ( hEnum );
}
///////////////////////////////////////////////////////////////////////////////
//
//	rControls::_Controls::_TV_DirList::RetreiveDirs
//		Gets the actual subdirectories that are to be inserted into
//		the tree view control
//
int rControls::_Ctrl::_tvList::RetreiveDirs
	( char *sSrchPath, TV_ITEM SelItem, HWND hTVWnd )
{
	HANDLE hDir, hQDir;
	WIN32_FIND_DATA FindInfo;
	int cnt = 0;

	ZeroMemory ( &tvChildDirs, sizeof ( tvChildDirs ));
	//	If there are files but no dirs
	if (( hQDir =
		FindFirstFile ( sSrchPath, &FindInfo )) != INVALID_HANDLE_VALUE ){
		int isDirs = 0;
		//	Search for dirs
		while ( TRUE ) {
			if (( strcmp ( FindInfo.cFileName, "." )) == 0 ) {
				if ( FindNextFile ( hQDir, &FindInfo ) == FALSE ) break;
				continue;
			}
			if (( strcmp ( FindInfo.cFileName, ".." )) == 0 ) {
				if ( FindNextFile ( hQDir, &FindInfo ) == FALSE ) break;
				continue;
			}
			int FAttribs[8];
			CustCtrl.Ctrl.GetFileAttribs ( &FindInfo, FAttribs );
			if ( FAttribs[FA_DIRECTORY] == 1 ) isDirs++;
			if ( FindNextFile ( hQDir, &FindInfo ) == FALSE ) break;
		}
		//	if none found
		if ( isDirs == 0 ) {
			TV_ITEM tvNoChild;
			tvNoChild.mask = TVIF_CHILDREN;
			tvNoChild.cChildren = 0;
			tvNoChild.hItem = SelItem.hItem;
			TreeView_SetItem ( hTVWnd, &tvNoChild );
		}
	}
	//	if there are no files in the dir
	if (( hDir =
		FindFirstFile ( sSrchPath, &FindInfo )) == INVALID_HANDLE_VALUE ){
		TV_ITEM tvNoChild;
		tvNoChild.mask = TVIF_CHILDREN;
		tvNoChild.cChildren = 0;
		tvNoChild.hItem = SelItem.hItem;
		TreeView_SetItem ( hTVWnd, &tvNoChild );
	}
	else {
		while (( strcmp ( FindInfo.cFileName, "." )) == 0 ) 
			if ( FindNextFile ( hDir, &FindInfo ) == FALSE ) return 0;
		while (( strcmp ( FindInfo.cFileName, ".." )) == 0 )
			if ( FindNextFile ( hDir, &FindInfo ) == FALSE ) return 0;
		int itCnt = 0;
		cnt = 0;
		while ( TRUE ) {
			if ( itCnt != 0 ) 
				if ( FindNextFile ( hDir, &FindInfo ) == FALSE ) break;
			itCnt++;
			//	While the found file is not a directory find the next file
			while ( TRUE ) {
				int FAttribs[8];
				CustCtrl.Ctrl.GetFileAttribs ( &FindInfo, FAttribs );
				if ( FAttribs[FA_DIRECTORY] == 1 ) break;
				if ( FindNextFile ( hDir, &FindInfo ) == FALSE ) return 0;
			}
			TV_INSERTSTRUCT tvis;
			//	Find the next available spot in the tv_item array
			while ( tvChildDirs[cnt].mask != 0 ) cnt++;
			strcpy ( stvLabels[cnt], FindInfo.cFileName );
			tvChildDirs[cnt].mask			= TVIF_CHILDREN | TVIF_HANDLE | TVIF_TEXT | TVIF_PARAM;
			tvChildDirs[cnt].cchTextMax	= MaxFileLen;
			tvChildDirs[cnt].pszText		= stvLabels[cnt];
			tvChildDirs[cnt].cChildren		= 1;
			tvChildDirs[cnt].lParam			= TVNet_RemoteDir;
			tvis.hParent = SelItem.hItem;
			tvis.hInsertAfter = TVI_SORT;
			tvis.item = tvChildDirs[cnt];
			TreeView_InsertItem ( hTVWnd, &tvis );
		}
	}
	return 0;
}
///////////////////////////////////////////////////////////////////////////////
//
//	rControls::_Controls::GetFileAttribs
//		Gets the file attributes of a specified file and puts them
//		into an array
//
int rControls::_Ctrl::GetFileAttribs
	( WIN32_FIND_DATA *FindData, int FAttribs[8] )
{
	DWORD dwTmp;
	int cnt = 0, i_Mask[32];
	
	while ( cnt < 32 ) {
		dwTmp = FindData->dwFileAttributes << cnt;
		i_Mask[cnt] = dwTmp >> 31;
		cnt++;
	}
	FAttribs[0] = i_Mask[20];		//	FILE_ATTRIBUTE_COMPRESSED
	FAttribs[1] = i_Mask[26];		//	FILE_ATTRIBUTE_ARCHIVE
	FAttribs[2] = i_Mask[27];		//	FILE_ATTRIBUTE_DIRECTORY
	FAttribs[3] = i_Mask[29];		//	FILE_ATTRIBUTE_SYSTEM
	FAttribs[4] = i_Mask[30];		//	FILE_ATTRIBUTE_HIDDEN
	FAttribs[5] = i_Mask[31];		//	FILE_ATTRIBUTE_READONLY
	FAttribs[6] = i_Mask[24];		//	FILE_ATTRIBUTE_NORMAL
	FAttribs[7] = i_Mask[23];		//	FILE_ATTRIBUTE_TEMPORARY

	return 0;
}
///////////////////////////////////////////////////////////////////////////////
//
//		DrawTools
//

///////////////////////////////////////////////////////////////////////////////
//
//	rControls::_DrawTools::MakeBrush
//		Creates a brush for painting the background of a window
//
HBRUSH rControls::_DrawTools::MakeBrush ( int i_Red, int i_Green, int i_Blue )
{
	HBRUSH hBrush;
	LOGBRUSH lb;

	lb.lbStyle = BS_SOLID;
	lb.lbColor = RGB ( i_Red, i_Green, i_Blue );
	lb.lbHatch = NULL;

	hBrush = CreateBrushIndirect ( &lb );	

	return hBrush;
}
//////////////////////////////////////////////////////////////////////////
//
//	rControls::_DrawTools::MakeSysBrush
//		Creates a brush based on a stock object
//
HBRUSH rControls::_DrawTools::MakeSysBrush ( int i_Index )
{
	HBRUSH hBrush;
	LOGBRUSH lb;
	COLORREF SysColor;

	SysColor = GetSysColor ( i_Index );
	lb.lbStyle = BS_SOLID;
	lb.lbColor = SysColor;
	lb.lbHatch = NULL;

	hBrush = CreateBrushIndirect ( &lb );	

	return hBrush;
}
//////////////////////////////////////////////////////////////////////////
//
//	rControls::_Controls::GetDrvInfo
//		Gets information about the disk drives on the computer
//
int rControls::_Ctrl::GetDrvInfo ( )
{
	int cnt = 0;

	//	Initialize the Drive info struct
	ZeroMemory ( DrvInfo, sizeof ( DrvInfo ) );
	//	Determine which drives exist
//	strcpy ( DrvInfo[0].sCFDes, "A:\\" );
//	strcpy ( DrvInfo[1].sCFDes, "B:\\" );
	strcpy ( DrvInfo[2].sCFDes, "C:\\" );
	strcpy ( DrvInfo[3].sCFDes, "D:\\" );
	strcpy ( DrvInfo[4].sCFDes, "E:\\" );
	strcpy ( DrvInfo[5].sCFDes, "F:\\" );
	strcpy ( DrvInfo[6].sCFDes, "G:\\" );
	strcpy ( DrvInfo[7].sCFDes, "H:\\" );
	strcpy ( DrvInfo[8].sCFDes, "I:\\" );
	strcpy ( DrvInfo[9].sCFDes, "J:\\" );
	strcpy ( DrvInfo[10].sCFDes, "K:\\" );
	strcpy ( DrvInfo[11].sCFDes, "L:\\" );
	strcpy ( DrvInfo[12].sCFDes, "M:\\" );
	strcpy ( DrvInfo[13].sCFDes, "N:\\" );
	strcpy ( DrvInfo[14].sCFDes, "O:\\" );
	strcpy ( DrvInfo[15].sCFDes, "P:\\" );
	strcpy ( DrvInfo[16].sCFDes, "Q:\\" );
	strcpy ( DrvInfo[17].sCFDes, "R:\\" );
	strcpy ( DrvInfo[18].sCFDes, "S:\\" );
	strcpy ( DrvInfo[19].sCFDes, "T:\\" );
	strcpy ( DrvInfo[20].sCFDes, "U:\\" );
	strcpy ( DrvInfo[21].sCFDes, "V:\\" );
	strcpy ( DrvInfo[22].sCFDes, "W:\\" );
	strcpy ( DrvInfo[23].sCFDes, "X:\\" );
	strcpy ( DrvInfo[24].sCFDes, "Y:\\" );
	strcpy ( DrvInfo[25].sCFDes, "Z:\\" );

	while ( cnt < 26 ) {
		int DrvType = 0;
		if ( (DrvType = GetDriveType ( DrvInfo[cnt].sCFDes )) != 1 ) {
			char sDrvLet[4], sLabel[MaxFileLen];
			ZeroMemory ( sLabel, sizeof ( sLabel ));
			//	Copy drive letter to temp buffer to construct label
			memcpy ( sDrvLet, DrvInfo[cnt].sCFDes, 3 );
			DrvInfo[cnt].Exist = TRUE;
			//	If the drive is a network drive
			DWORD szRemoteName = MaxFileLen;
			char sLocalName[MaxFileLen];
			if ( DrvType == DRIVE_REMOTE ) {
				strcpy ( sLocalName, DrvInfo[cnt].sCFDes );
				sLocalName[strlen (sLocalName)-1] = '\0';
				WNetGetConnection ( sLocalName, sLabel, &szRemoteName );
			}
			else {
				GetVolumeInformation ( sDrvLet, sLabel,
					MaxFileLen, NULL, 0, NULL, NULL, 0 );
			}
			sDrvLet[2] = '\0';
			if ( strlen ( sLabel ) == 0 ) 
				sprintf ( DrvInfo[cnt].sLbl, "(%s)", sDrvLet );
			else sprintf ( DrvInfo[cnt].sLbl, 
				"%s   (%s)", sLabel, sDrvLet );
		}
		cnt++;
	}
	cnt = 0;
	//	Get the types of the existing drives
	while ( cnt < 26 ) {
		if ( DrvInfo[cnt].Exist == TRUE ) {
			DrvInfo[cnt].Type = GetDriveType ( DrvInfo[cnt].sCFDes );
		}
		cnt++;
	}

	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	rControls::_Ctrl::_tvList::Initialize
//		Initializes the dir list tree view control
//
int rControls::_Ctrl::_tvList::Initialize ( HWND hDlg, HWND hTV_Ctrl )
{
	struct _ItemInfo {
		TV_ITEM tvi;
		TV_INSERTSTRUCT tvis;
	}ItemInfo[26];
	TV_ITEM tvNet, tvLocal;
	TV_INSERTSTRUCT tvisNet, tvisLocal;
	HWND hTVDirList;
	int cnt = 0, NumDrives = 0;
	TV_ITEM tvTest;
	TV_INSERTSTRUCT tvisTest;

	//	Get the operating system version
	OSVer.dwOSVersionInfoSize = sizeof ( OSVERSIONINFO );
	GetVersionEx ( &OSVer );

	hTVDirList = GetDlgItem ( hDlg, TV_AvailDirs );
	ZeroMemory ( &tvisNet, sizeof ( TV_INSERTSTRUCT ));
	ZeroMemory ( &tvNet, sizeof ( TV_ITEM ));
	ZeroMemory ( &tvisLocal, sizeof ( TV_INSERTSTRUCT ));
	ZeroMemory ( &tvLocal, sizeof ( TV_ITEM ));
	ZeroMemory ( &tvisTest, sizeof ( TV_INSERTSTRUCT ));
	ZeroMemory ( &tvTest, sizeof ( TV_ITEM ));

	//	If a dir listing is required
	if ( CustCtrl.Dialogs.SelectDirDlg.Type == TVL_DIRS ) {
		//	Insert My Computer item
		tvLocal.mask		= TVIF_CHILDREN | TVIF_TEXT | TVIF_HANDLE;
		tvLocal.cchTextMax	= _MAX_FNAME;
		tvLocal.pszText		= "My Computer";
		tvLocal.cChildren	= 1;
		tvisLocal.hParent	= TVI_ROOT;
		tvisLocal.hInsertAfter	= TVI_LAST;
		tvisLocal.item		= tvLocal;
		tvLocal.hItem = tvgMyComp = TreeView_InsertItem
			( hTV_Ctrl, &tvisLocal );
		//	Insert Net item
		HANDLE hEnum;
		if ( WNetOpenEnum ( RESOURCE_GLOBALNET, RESOURCETYPE_ANY,
			RESOURCEUSAGE_CONTAINER, NULL, &hEnum ) == NO_ERROR ) {
			tvNet.mask				=	TVIF_CHILDREN | TVIF_TEXT | TVIF_PARAM;
			tvNet.cchTextMax		=	_MAX_FNAME;
			tvNet.pszText			=	"Network";
			tvNet.cChildren		=	1;
			tvNet.lParam			=	TVNet_Root;
			tvisNet.hParent		=	TVI_ROOT;
			tvisNet.hInsertAfter	=	TVI_LAST;
			tvisNet.item			=	tvNet;
			tvgNet = TreeView_InsertItem ( hTV_Ctrl, &tvisNet );
			WNetCloseEnum ( hEnum );
		}
		//	Insert drives
		cnt = 0;
		CustCtrl.Ctrl.GetDrvInfo ( );
		SetFocus ( GetDlgItem ( hDlg, TV_AvailDirs ));

		while ( cnt < 26 ) {
			if ( CustCtrl.Ctrl.DrvInfo[cnt].Exist == TRUE ) {
				ItemInfo[cnt].tvi.mask			=
					TVIF_CHILDREN | TVIF_HANDLE | TVIF_TEXT | TVIF_IMAGE;
				ItemInfo[cnt].tvi.pszText		=
					CustCtrl.Ctrl.DrvInfo[cnt].sLbl;
				ItemInfo[cnt].tvi.cchTextMax	= _MAX_PATH;
				ItemInfo[cnt].tvi.cChildren		= 1;
				ItemInfo[cnt].tvis.hParent		= tvLocal.hItem;
				ItemInfo[cnt].tvis.hInsertAfter	= TVI_LAST;
				ItemInfo[cnt].tvis.item			= ItemInfo[cnt].tvi;
				TreeView_InsertItem ( hTV_Ctrl, &ItemInfo[cnt].tvis );
			}
			cnt++;
		}
		//	If we need to default to a dir
		if ( strlen ( stv_StartDir ) > 0 ) {
			CustCtrl.Ctrl.tvList.DefaultToDir ( hTV_Ctrl, hDlg );
		}
	}
	//	If server listing is required
	if ( CustCtrl.Dialogs.SelectDirDlg.Type == TVL_SERVERS ) {
		tvNet.mask				=	TVIF_CHILDREN | TVIF_TEXT | TVIF_PARAM;
		tvNet.cchTextMax		=	_MAX_FNAME;
		tvNet.pszText			=	"Network";
		tvNet.cChildren		=	1;
		tvNet.lParam			=	TVNet_Root;
		tvisNet.hParent		=	TVI_ROOT;
		tvisNet.hInsertAfter	=	TVI_LAST;
		tvisNet.item			=	tvNet;
		tvgNet = TreeView_InsertItem ( hTV_Ctrl, &tvisNet );
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////
//
//	rControls::_Ctrl::_tvList::DefaultToDir
//		Selects the specified directory in the tree view control
//		at the time the control is created
//
int rControls::_Ctrl::_tvList::DefaultToDir ( HWND hTVCtrl, HWND hDlg )
{
	char CurDir[_MAX_PATH];
	char DirToks[100][_MAX_FNAME];
	int cnt = 0;
	int strStart = 0, strEnd = 0, TokNum = 1;
	cnt = 0;
	BOOL isLocal = FALSE;

	ZeroMemory ( DirToks, sizeof ( DirToks ));
	strcpy ( CurDir, stv_StartDir );
	//	The directory to default to is passed as a character string to
	//	the function so we should first check if it's valid
	WIN32_FIND_DATA FindData;

	//	If the dir is remote then return, we aren't allowing a remote
	//	dir as the dir to start the control in
	if (CurDir[0] == '\\') {
		isLocal = FALSE;
		return 0;
	}
		strcpy ( DirToks[0], "My Computer" );
		if ( FindFirstFile ( CurDir,
			&FindData ) == INVALID_HANDLE_VALUE ) {
			GetCurrentDirectory ( _MAX_PATH, CurDir );
		}
		isLocal = TRUE;
	while ( TRUE ) {
		if ( (CurDir[cnt] == '\\') || (CurDir[cnt] == '\0') ) {
			strEnd = cnt;
			memcpy ( DirToks[TokNum], 
				&CurDir[strStart], strEnd-strStart );
			TokNum++;
			strStart = strEnd+1;
		}
		if ( CurDir[cnt] == '\0' ) break;
		cnt++;
	}
	//	Set to cur drive
	TV_ITEM tvIRet;
	char WantLbl[_MAX_FNAME], tvLbl[_MAX_FNAME];
	cnt = 0;
	//	This loop works it's way through the tree view control,
	//	selecting the appropriate item and expanding it until it
	//	reaches the specified starting dir
	while ( strlen ( DirToks[cnt] ) != 0 ) {
		ZeroMemory ( tvLbl, sizeof ( tvLbl ));
		if ( cnt == 1 ) sprintf ( WantLbl, "(%s)", DirToks[1] );
		else sprintf ( WantLbl, "%s", DirToks[cnt] );
		tvIRet.mask		=	TVIF_TEXT;
		tvIRet.cchTextMax	=	_MAX_FNAME;
		tvIRet.pszText		=	(char *) malloc ( _MAX_FNAME );

		if ( cnt == 0 ) tvIRet.hItem = TreeView_GetRoot ( hTVCtrl );
		else tvIRet.hItem =
			TreeView_GetChild ( hTVCtrl, tvIRet.hItem );

		TreeView_GetItem ( hTVCtrl, &tvIRet );
		if ( cnt == 1 ) memcpy ( tvLbl,
			&tvIRet.pszText[strlen (tvIRet.pszText)-4], 4 );
		else strcpy ( tvLbl, tvIRet.pszText );

		if ( _stricmp ( WantLbl, tvLbl ) == 0 ) {
			TreeView_Select ( hTVCtrl, tvIRet.hItem, TVGN_CARET );
		}
		else {
			while ( TRUE ) {
				tvIRet.hItem = 
					TreeView_GetNextSibling (
					hTVCtrl, tvIRet.hItem );
				TreeView_GetItem ( hTVCtrl, &tvIRet );
				if ( cnt == 1 ) memcpy ( tvLbl,
					&tvIRet.pszText
					[strlen (tvIRet.pszText)-4], 4 );
				else strcpy ( tvLbl, tvIRet.pszText );
				if ( _stricmp ( WantLbl, tvLbl ) == 0 ) {
					TreeView_Select ( hTVCtrl,
						tvIRet.hItem, TVGN_CARET );
					break;
				}
			}
		}
		NM_TREEVIEW nmtv;
		ZeroMemory ( &nmtv, sizeof ( nmtv ));
		nmtv.hdr.idFrom		= TV_AvailDirs;
		nmtv.hdr.code		= TVN_ITEMEXPANDING;
		nmtv.action		= TVE_EXPAND;
		nmtv.itemNew		= tvIRet;
		SendMessage ( hDlg, WM_NOTIFY, 
			(WPARAM) hTVCtrl, (LPARAM) &nmtv );
		nmtv.itemNew.mask	=	TVIF_STATE;
		nmtv.itemNew.stateMask	=	TVIS_EXPANDEDONCE;
		nmtv.itemNew.state	=	nmtv.itemNew.state + 64;
		TreeView_SetItem ( hTVCtrl, &nmtv.itemNew );
		TreeView_Expand ( hTVCtrl, nmtv.itemNew.hItem, TVE_EXPAND );

		cnt++;
	}
	return 0;
}
///////////////////////////////////////////////////////////////////////////////
//
//	rControls::_DrawTools::_DrawTools
//		Constructor
//
rControls::_DrawTools::_DrawTools ( )
{
	SBFont = CreateFont ( 14, 0, 0, 0, FW_NORMAL, FALSE,
		FALSE, FALSE, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS,
		CLIP_DEFAULT_PRECIS, PROOF_QUALITY,
		VARIABLE_PITCH | FF_DONTCARE, "MS Sans Serif" );

}
//////////////////////////////////////////////////////////////////////////
//
//	rControls::_Ctrl::_tvList::GetRemoteDirs
//		Gets the subdirs on a remote machine
//
void rControls::_Ctrl::_tvList::GetRemoteDirs
		( TV_ITEM tvShare, NETRESOURCE NetRes, char *UNCName, HWND hParent )
{
	//	Establish connection to remote share
//	if ( WNetAddConnection3 ( hParent, &NetRes,
//		NULL, NULL, NULL ) == NO_ERROR ) {
		//	Get the remote dirs
		char SearchPath[_MAX_PATH];
		ZeroMemory ( SearchPath, sizeof ( SearchPath ));
		strcpy ( SearchPath, UNCName );
		strcat ( SearchPath, "\\*.*" );
		RetreiveDirs ( SearchPath, tvShare, hParent );
	
//	}

	return;
}
//////////////////////////////////////////////////////////////////////////
//
//	rControls::_Controls::_tvList::InsertNetItems
//		Inserts network items into the tree view control
//
void rControls::_Ctrl::_tvList::InsertNetItems ( TV_ITEM tvClicked, HANDLE hEnum, HWND hTVWnd )
{
	DWORD dList = MAX_ENUM_RESOURCES,
		szBuf = ENUM_BUF_SIZE * 100;
	NETRESOURCE NetRes[100];
	TV_ITEM tvNewItem;
	TV_INSERTSTRUCT tvisNewItem;

	ZeroMemory ( NetRes, sizeof ( NetRes ));
	ZeroMemory ( &tvNewItem, sizeof ( tvNewItem ));

	dList = MAX_ENUM_RESOURCES; szBuf = ENUM_BUF_SIZE * 100;
	//	Enumerate the resources contained in the item that was clicked
	int RetVal = 0;
	if ( (RetVal = WNetEnumResource 
		( hEnum, &dList, NetRes, &szBuf )) == NO_ERROR ) {
		//	dList holds the number of items that were returned in the array, so while it is greater
		//		than zero we shall loop
		while ( dList> 0 ) {
			DWORD isContainer;
			//	Determine, first, if this is a container...
			isContainer = NetRes[dList-1].dwUsage;
			isContainer = isContainer << 30;
			isContainer = isContainer >> 31;
			//	...If it isn't, then...
			if ( isContainer == FALSE ) { 
				//	...if if it isn't a disk drive either (meaning, if it is a printer) then skip it and
				//		move on to the next item
				if ( NetRes[dList-1].dwType != RESOURCETYPE_DISK ) {
					dList--;
					continue;
				}
			}
			//*****************************************************************
			//	Create the tree view item for the resource found in the clicked item
			//
			//	This switch determines the type of network resource the clicked item's parent is and
			//		sets the lParam for the clicked item according to that. (The lParam member of the
			//		TV_ITEM struct is for storing application specific information.  I am using it here
			//		to keep track of what type of network resource each item is).
			switch ( tvClicked.lParam ) {
				case TVNet_MSProvider:
					tvNewItem.lParam = TVNet_Domain;
					break;
				case TVNet_NWProvider:
					tvNewItem.lParam = TVNet_Server;
					break;
				case TVNet_Domain:
					tvNewItem.lParam = TVNet_Server;
					break;
				case TVNet_Server:
					tvNewItem.lParam = TVNet_Share;
					break;
			}
			tvNewItem.mask			= TVIF_CHILDREN | TVIF_TEXT | TVIF_PARAM;
			tvNewItem.cchTextMax	= _MAX_FNAME;
			tvNewItem.pszText		= NetRes[dList-1].lpRemoteName;
			tvNewItem.cChildren	= 1;
			//	If this is a network share...
			if ( tvNewItem.lParam == TVNet_Share ) {
				//	Remove the server name from the item label
				char sTemp[_MAX_PATH];
				int StartStr, EndStr;
				StartStr = EndStr = 
					strlen ( NetRes[dList-1].lpRemoteName );
				ZeroMemory ( sTemp, sizeof ( sTemp ));
				while ( NetRes[dList-1].lpRemoteName[StartStr] != '\\' ) StartStr--;
				memcpy ( sTemp, 
					&NetRes[dList-1].lpRemoteName[StartStr+1], EndStr - StartStr );
				tvNewItem.pszText	= sTemp;
				//	Set the cChildren member according to the type of object we need
				//		I.E. if we need servers and this is a server then don't put an
				//		expansion box next to the item, however, if it is a domain then
				//		do put an expansion box next to it
				switch ( CustCtrl.Dialogs.SelectDirDlg.Type ) {
					case TVL_DIRS:
						tvNewItem.cChildren = 1;
						break;
					case TVL_SHARES:
						tvNewItem.cChildren = 0;
						break;
				}
			}
			//	If this is a server...
			if ( tvNewItem.lParam == TVNet_Server ) {
				//	If the required type is servers then don't put an expansion box next to the item
				if ( CustCtrl.Dialogs.SelectDirDlg.Type == TVL_SERVERS )
					tvNewItem.cChildren	= 0;
			}
			tvisNewItem.hParent			= tvClicked.hItem;
			tvisNewItem.hInsertAfter	= TVI_SORT;
			tvisNewItem.item				= tvNewItem;
			//	Insert the item into the tree view
			TreeView_InsertItem ( hTVWnd, &tvisNewItem );
			dList--;
		}
	}
	return;
}

/*
**	History:
**	$Log: rControls.cpp,v $
**	Revision 1.12  2009/10/17 19:55:05  gsl
**	fix default return type to int
**	
**	Revision 1.11  2007/08/07 20:08:44  gsl
**	remove old CHANGED markers
**	
**	Revision 1.10  2003/06/18 16:43:07  gsl
**	Add CVS header and history
**	
**
*/
