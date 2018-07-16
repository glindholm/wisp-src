//////////////////////////////////////////////////////////////////////////
//	This file contains the entry point for the WISP Configuration Utility
//

#include "#Defines.h"
#include "#Classes.h"
#include "#Prototypes.h"

cDialogs			Dialogs;
cApplication		cApp;

int WINAPI WinMain ( HINSTANCE hInst, HINSTANCE hInstPrev, LPSTR lpCmdLine, int nCmdShow )
{
	if (cApp.HaveRequiredPermissions())
	{
		//Ctl3dRegister ( hInst );
		//Ctl3dAutoSubclass( hInst );
		InitCommonControls ( );
		CustCtrl.Init ( hInst );

		cApp.InitApp ( hInst );
		cApp.MsgLoop ( );


		//Ctl3dUnregister ( hInst );
	}
	return 0;
}