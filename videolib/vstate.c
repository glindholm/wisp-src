			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/


/*					Include standard header files.								*/

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include internal definitions.	*/
#include "vdata.h"									/* Reference video data base.		*/
#include "vcap.h"									/* Reference videocap data base.	*/

/*					Static data.										*/

static int doing_save = FALSE;								/* Not already doing a save.		*/
static int debug_save = FALSE;								/* Not already saved debugger state.	*/

/*					Subroutine entry point.									*/

int vstate(action) int action;								/* Perform specified action.		*/
{
	register int i, ret;								/* Working registers.			*/

	ret = SUCCESS;									/* Assume all will be ok.		*/
	switch(action)									/* Select the action.			*/
	{
		case DEFAULT:								/* Will synchronize with map.		*/
		{
			vcapload();							/* init vcap stuff                      */
			vbuffering(LOGICAL);						/* Turn on logical buffering.		*/
			vcontrol(vcapdef[INIT_TERMINAL]);				/* Send the initial terminal string.	*/
			vmode(CLEAR);							/* Clear all renditions.		*/
			vcharset(DEFAULT);						/* Standard character set.		*/
			vroll(0,MAX_LINES_PER_SCREEN-1);				/* Set scroll area to full screen.	*/
			vbuffering(AUTOMATIC);						/* Restore automatic buffering.		*/
			state_active = TRUE;						/* State 0 is now active.		*/
			break;
		}

		case SAVE:								/* Save current setup and status.	*/
		{
			if (doing_save) vre("vstate(SAVE)-Warning: Already in save state.");
			doing_save = TRUE;						/* Flag that we are in save state.	*/
			scur_lin = vcur_lin;						/* Save position.			*/
			scur_col = vcur_col;
			scur_atr = vcur_atr;						/* Save character rendition.		*/
			schr_set = vchr_set;						/* Save character set.			*/
			break;
		}

		case RESTORE:								/* Restore old setup and status.	*/
		{
			if (!doing_save) vre("vstate(RESTORE)-Warning: Not in save state.");
			doing_save = FALSE;						/* Flag that we are not in save state.	*/
			vmode(scur_atr);						/* Restore old character rendition.	*/
			vcharset(schr_set);						/* Restore character set.		*/
			vmove(scur_lin,scur_col);					/* Restore old position.		*/
			break;
		}

		case SAVE_DEBUG:							/* Save the debugger terminal setup.	*/
		{
			if (debug_save) vre("vstate(SAVE_DEBUG)-Warning: Debugger terminal setup already saved.");
			debug_save = TRUE;
			if (isdebug())
			{
				vraw_stty_save();
				vraw_stty_sync();
			}
			break;
		}

		case RESTORE_DEBUG:
		{
			if (!debug_save) vre("vstate(RESTORE_DEBUG)-Warning: Debugger terminal setup not saved.");
			debug_save = FALSE;
			if (isdebug())
			{
				vcharset(DEFAULT);					/* Put back the character set.		*/
				vmode(CLEAR);						/* Restore the rendition.		*/
				vset(CURSOR,VISIBLE);					/* Set cursor on.			*/
				vdefer(RESTORE);					/* Restore from deferred state.		*/
				vcontrol(DUMP_OUTPUT);					/* Dump the buffer.			*/
				vraw_stty_restore();					/* Restore debugger's state.		*/
				break;							/* Now we are done.			*/
			}
		}

		default:
		{
			ret = FAILURE;							/* Invalid option.			*/
			vre("vstate(%d)-Invalid control parameter, must be SAVE, RESTORE, DEFAULT or debugger control",action);
			break;
		}
	}
	return(ret);
}
