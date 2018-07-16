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

	Ctl3dRegister ( hInst );
	Ctl3dAutoSubclass( hInst );
	hLibrary = LoadLibrary("RichEd32.DLL");

	cApp.Env->CurAppStatus = cas_StartUp;										//	Set flag 
	cApp.Env->hInstGlb = CustCtrl.hInst = hInst;								//	Set member in class to hInst
	CustCtrl.hInst = hInst;
	cApp.AppInit ( hInst );

	CustCtrl.StatusBar.UpdateMsg ( "Ready" );
	cApp.Env->CurAppStatus = cas_Running;

	cApp.MsgLoop ( );																	//	Run the message loop

	Ctl3dUnregister ( hInst );
    FreeLibrary(hLibrary);

	return 0;
}

