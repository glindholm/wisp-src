/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
******************************************************************************
*/



/*					Include standard header files.								*/

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include internal definitions.	*/
#include "vdata.h"									/* Reference video data base.		*/
#include "vcap.h"									/* Reference videocap data base.	*/
#include "vmodules.h"
#include "vraw.h"


/*					Static data.										*/

static int doing_save = FALSE;								/* Not already doing a save.		*/
static int stty_save = FALSE;								/* Not already saved stty state.	*/

/*					Data involved when state saved and restored.						*/
static int VL_scur_lin = 0;								/* Saved line number (when optimizing).		*/
static int VL_scur_col = 0;								/* Saved column (when optimizing).		*/
static int VL_scur_atr = 0;								/* Saved character rendition.			*/
static int VL_schr_set = 0;								/* Saved character set.				*/

/*					Subroutine entry point.									*/

int VL_vstate(int action)								/* Perform specified action.		*/
{
	int ret;									/* Working registers.			*/

	ret = SUCCESS;									/* Assume all will be ok.		*/
	switch(action)									/* Select the action.			*/
	{
		case VSTATE_DEFAULT:							/* Will synchronize with map.		*/
		{
			VL_vcapload();							/* init vcap stuff                      */
			VL_vbuffering_start();						/* Turn on logical buffering.		*/
			VL_vcap_init_terminal();						/* Send the initial terminal string.	*/
			VL_vmode(CLEAR);							/* Clear all renditions.		*/
			vcharset(DEFAULT);						/* Standard character set.		*/
			vroll(0,MAX_LINES_PER_SCREEN-1);				/* Set scroll area to full screen.	*/
			VL_vbuffering_end();						/* Restore automatic buffering.		*/
			VL_state_active = TRUE;						/* State 0 is now active.		*/
			break;
		}

		case VSTATE_SAVE:							/* Save current setup and status.	*/
		{
			if (doing_save) vre("vstate(VSTATE_SAVE)-Warning: Already in save state.");
			doing_save = TRUE;						/* Flag that we are in save state.	*/
			VL_scur_lin = vcur_lin;						/* Save position.			*/
			VL_scur_col = vcur_col;
			VL_scur_atr = vcur_atr;						/* Save character rendition.		*/
			VL_schr_set = vchr_set;						/* Save character set.			*/
			break;
		}

		case VSTATE_RESTORE:							/* Restore old setup and status.	*/
		{
			if (!doing_save) vre("vstate(VSTATE_RESTORE)-Warning: Not in save state.");
			doing_save = FALSE;						/* Flag that we are not in save state.	*/
			VL_vmode(VL_scur_atr);						/* Restore old character rendition.	*/
			vcharset(VL_schr_set);						/* Restore character set.		*/
			vmove(VL_scur_lin,VL_scur_col);					/* Restore old position.		*/
			break;
		}

		case VSTATE_SAVE_STTY:							/* Save the stty terminal setup.	*/
		{
			if (stty_save) vre("vstate(VSTATE_SAVE_STTY)-Warning: TTY terminal setup already saved.");
			stty_save = TRUE;
			if (VL_vsharedscreen())
			{
				vraw_stty_save();
				vraw_stty_sync();
			}
			break;
		}

		case VSTATE_RESTORE_STTY:
		{
			if (!stty_save) vre("vstate(VSTATE_RESTORE_STTY)-Warning: TTY terminal setup not saved.");
			stty_save = FALSE;
			if (VL_vsharedscreen())
			{
				vcharset(DEFAULT);					/* Put back the character set.		*/
				VL_vmode(CLEAR);						/* Restore the rendition.		*/
				vset_cursor_on();					/* Set cursor on.			*/
				vdefer_restore();					/* Restore from deferred state.		*/
				VL_vcontrol_flush();					/* Dump the buffer.			*/
				vraw_stty_restore();					/* Restore stty's state.		*/
				break;							/* Now we are done.			*/
			}
		}

		default:
		{
			ret = FAILURE;							/* Invalid option.			*/
			vre("vstate(%d)-Invalid control parameter",action);
			break;
		}
	}
	return(ret);
}

/*
**	Routine:	VL_vtty_alloc()
**
**	Function:	Allocate memory for terminal state information save structure.
**
**	Description:	Malloc structure for user defined terminal attributes and return a void
**			pointer to that structure.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:
**	void*		terminal attributes
**
**	Warnings:	None
**
**	History:	
**	10/10/96	Written by SMC
**
*/
void* VL_vtty_alloc()
{
#ifdef unix
	return( vrawttyalloc() );
#else
	return NULL;
#endif
}

/*
**	Routine:	VL_vtty_set()
**
**	Function:	Set the terminal state. 
**
**	Description:	Use the passed in terminal attributes to set the state of
**			of the terminal.
**
**	Arguments:
**	void*		terminal attributes
**
**	Globals:	None
**
**	Return:
**	0		Success
**	1		Failure
**
**	Warnings:	None
**
**	History:	
**	10/10/96	Written by SMC
**
*/
int VL_vtty_set(void* tt)
{
#ifdef unix
	int rc = 0;
	
	if ( vrawttyset(fileno(stdin), tt ) )
	{
		rc = 1;
	}

	return( rc );
#else
	return 0;
#endif
}

/*
**	Routine:	VL_vtty_get()
**
**	Function:	Get terminal state information.
**
**	Description:	Set the user defined pointer to terminal attributes of the current 
**			state of the terminal.
**
**	Arguments:
**	void*		terminal attributes
**
**	Globals:	None
**
**	Return:
**	0		Success
**	1		Failure
**
**	Warnings:	None
**
**	History:	
**	10/10/96	Written by SMC
**
*/
int VL_vtty_get(void* tt)
{
#ifdef unix
	int rc = 0;
	
	if ( vrawttyget(fileno(stdin), tt) )
	{
		rc = 1;
	}

	return( rc );
#else
	return 0;
#endif
}
/*
**	History:
**	$Log: vstate.c,v $
**	Revision 1.19  2003/06/20 15:37:45  gsl
**	VL_ globals
**	
**	Revision 1.18  2003/01/31 19:25:55  gsl
**	Fix copyright header
**	
**	Revision 1.17  2002/07/17 21:06:05  gsl
**	VL_ globals
**	
**	Revision 1.16  2002/07/15 20:16:15  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.15  2002/07/15 17:10:07  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.14  1997/09/22 16:24:55  gsl
**	Change the VSTATE_ DEBUG to STTY
**	
**	Revision 1.13  1997-07-09 12:29:17-04  gsl
**	Change to use new video.h interfaces
**
**	Revision 1.12  1996-11-13 20:32:42-05  gsl
**	use vcontrol_flush()
**
**	Revision 1.11  1996-11-04 11:18:00-08  gsl
**	Add non-unix null actions for the vtty_xxx() routines
**
**	Revision 1.10  1996-10-11 15:16:21-07  gsl
**	drcs update
**
**
**
*/
