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
			vcontrol(vcapdef[INIT_TERMINAL]);
#if 0
			vset(TERMINAL,ANSI);						/* Turn on VT100/VT200 mode.		*/
			vmove(0,0);							/* Move to home position.		*/
			vset(KEYPAD,NORMAL);						/* Set application keys to normal.	*/
			vset(CURSOR_KEYS,NORMAL);					/* Set cursor keys normal.		*/
			vset(SCROLL,JUMP);						/* Turn on jump scroll.			*/
			vset(ORIGIN,TOP_OF_SCREEN);					/* Origin is top of screen.		*/
			vset(AUTO_WRAP,OFF);						/* Don't auto-wrap at end of line.	*/
			vset(AUTO_REPEAT,ON);						/* Turn auto-repeat on.			*/
			vset(INTERLACE,OFF);						/* Turn off unpleasant interlace.	*/
			vset(AUTO_PRINT,OFF);						/* Don't let the printer print.		*/
			vset(PRINTER,OFF);						/* Turn the printer off too.		*/
			vset(HOST_ECHO,ON);						/* We want to echo, not the terminal.	*/
			vset(NEW_LINE_MODE,OFF);					/* We don't want line-feed to new-line.	*/
			vset(KEYBOARD,UNLOCKED);					/* We want the keyboard enabled.	*/
			vset(INSERT_MODE,REPLACE);					/* We want insert mode replacing.	*/
			vset(CURSOR,VISIBLE);						/* We want a visible cursor.		*/
			vset(PRINT_TERMINATOR,NONE);					/* No form feed from the printer.	*/
			vset(PRINT_EXTENT,FULL_SCREEN);					/* Want full screen on print screen.	*/
#endif
			vmode(CLEAR);							/* Clear all renditions.		*/
			vcharset(DEFAULT);						/* Standard character set.		*/
			vroll(0,MAX_LINES_PER_SCREEN-1);				/* Set scroll area to full screen.	*/
			vbuffering(AUTOMATIC);						/* Restore automatic buffering.		*/
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

		default:
		{
			ret = FAILURE;							/* Invalid option.			*/
			vre("vstate(%d)-Invalid control parameter, must be SAVE, RESTORE or DEFAULT",action);	/* Report. 	*/
			break;
		}
	}
	return(ret);
}
