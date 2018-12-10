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

#include "#Defines.h"
#include "#Classes.h"
#include "#Prototypes.h"
#include "resource.h"

//	Global (application wide) variables
cApplication		cApp;
cWindows				cWnd;
struct _GlbFlags {
	BOOL isTranslating,
		isCompiling;
}GlbFlags;
//	WinMain - Entry Point
int WINAPI WinMain ( HINSTANCE hInst, HINSTANCE hInstPrev, LPSTR lpCmdLine, int i_nCmdShow )
{
    HINSTANCE hLibrary;

	//Ctl3dRegister ( hInst );
	//Ctl3dAutoSubclass( hInst );
	hLibrary = LoadLibrary("RichEd32.DLL");

	cApp.Env->CurAppStatus = cas_StartUp;										//	Set flag 
	cApp.Env->hInstGlb = CustCtrl.hInst = hInst;								//	Set member in class to hInst
	CustCtrl.hInst = hInst;
	cApp.AppInit ( hInst );

	CustCtrl.StatusBar.UpdateMsg ( "Ready" );
	cApp.Env->CurAppStatus = cas_Running;

	cApp.MsgLoop ( );																	//	Run the message loop

	//Ctl3dUnregister ( hInst );
    FreeLibrary(hLibrary);

	return 0;
}


/*
**	History:
**	$Log: _WispWin.cpp,v $
**	Revision 1.6  2007/08/02 13:22:51  gsl
**	Remove CTL3Dxxxx stuff, obsolete.
**	
**	Revision 1.5  2003/06/18 16:43:07  gsl
**	Add CVS header and history
**	
**
*/
