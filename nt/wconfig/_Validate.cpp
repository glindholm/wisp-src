#include "#Classes.h"
#include "#Prototypes.h"
#include "#Externs.h"
#include "#Defines.h"


//////////////////////////////////////////////////////////////////////////
//
//	Validate
DWORD Validate ( HWND hCtl )
{
	HWND hParent = GetParent ( hCtl );
	if ( hParent == Dialogs.General.hGeneral ) {
		Dialogs.General.ChkInvalid ( hCtl );
	}
	if ( hParent == Dialogs.ACP.hACP ) {
		Dialogs.ACP.ChkInvalid ( hCtl );
	}
	if ( hParent == Dialogs.License.hLicense ) {
		Dialogs.License.ChkInvalid ( hCtl );
	}
	if ( hParent == Dialogs.MESSAGE.hMESSAGE ) {
		Dialogs.MESSAGE.ChkInvalid ( hCtl );
	}
	if ( hParent == Dialogs.SCRATCH.hSCRATCH ) {
		Dialogs.SCRATCH.ChkInvalid ( hCtl );
	}
	if ( hParent == Dialogs.Videocap.hVideocap ) {
		Dialogs.Videocap.ChkInvalid ( hCtl );
	}
	if ( hParent == Dialogs.WISPBin.hWISPBin ) {
		Dialogs.WISPBin.ChkInvalid ( hCtl );
	}
	if ( hParent == Dialogs.WPROC.hWPROC ) {
		Dialogs.WPROC.ChkInvalid ( hCtl );
	}
	if ( hParent == Dialogs.DISPLAY.hDISPLAY ) {
		Dialogs.DISPLAY.ChkInvalid ( hCtl );
	}

	return 0;
}