static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";


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

/*					Subroutine entry point.									*/

int vstate(int action)									/* Perform specified action.		*/
{
	int ret;									/* Working registers.			*/

	ret = SUCCESS;									/* Assume all will be ok.		*/
	switch(action)									/* Select the action.			*/
	{
		case VSTATE_DEFAULT:							/* Will synchronize with map.		*/
		{
			vcapload();							/* init vcap stuff                      */
			vbuffering_start();						/* Turn on logical buffering.		*/
			vcap_init_terminal();						/* Send the initial terminal string.	*/
			vmode(CLEAR);							/* Clear all renditions.		*/
			vcharset(DEFAULT);						/* Standard character set.		*/
			vroll(0,MAX_LINES_PER_SCREEN-1);				/* Set scroll area to full screen.	*/
			vbuffering_end();						/* Restore automatic buffering.		*/
			state_active = TRUE;						/* State 0 is now active.		*/
			break;
		}

		case VSTATE_SAVE:							/* Save current setup and status.	*/
		{
			if (doing_save) vre("vstate(VSTATE_SAVE)-Warning: Already in save state.");
			doing_save = TRUE;						/* Flag that we are in save state.	*/
			scur_lin = vcur_lin;						/* Save position.			*/
			scur_col = vcur_col;
			scur_atr = vcur_atr;						/* Save character rendition.		*/
			schr_set = vchr_set;						/* Save character set.			*/
			break;
		}

		case VSTATE_RESTORE:							/* Restore old setup and status.	*/
		{
			if (!doing_save) vre("vstate(VSTATE_RESTORE)-Warning: Not in save state.");
			doing_save = FALSE;						/* Flag that we are not in save state.	*/
			vmode(scur_atr);						/* Restore old character rendition.	*/
			vcharset(schr_set);						/* Restore character set.		*/
			vmove(scur_lin,scur_col);					/* Restore old position.		*/
			break;
		}

		case VSTATE_SAVE_STTY:							/* Save the stty terminal setup.	*/
		{
			if (stty_save) vre("vstate(VSTATE_SAVE_STTY)-Warning: TTY terminal setup already saved.");
			stty_save = TRUE;
			if (vsharedscreen())
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
			if (vsharedscreen())
			{
				vcharset(DEFAULT);					/* Put back the character set.		*/
				vmode(CLEAR);						/* Restore the rendition.		*/
				vset_cursor_on();					/* Set cursor on.			*/
				vdefer_restore();					/* Restore from deferred state.		*/
				vcontrol_flush();					/* Dump the buffer.			*/
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
**	Routine:	vtty_alloc()
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
void* vtty_alloc()
{
#ifdef unix
	return( vrawttyalloc() );
#else
	return NULL;
#endif
}

/*
**	Routine:	vtty_set()
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
int vtty_set(void* tt)
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
**	Routine:	vtty_get()
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
int vtty_get(void* tt)
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
