			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/


/*						Include standard header files.							*/

#include <stdio.h>									/* Include the standard I/O defs.	*/
#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include local definitions too.	*/
#include "vdata.h"									/* Include keyboard mapping definitions	*/
#include "vcap.h"
#include "vmenu.h"

/*						Static data.									*/

static int out_flag;									/* Output control flag.			*/


/*						Subroutine entry point.								*/

int verase(control) int control;							/* Erase all or part of the screen.	*/
{
	register int i, ret;								/* Working registers.			*/

	if ((control == ALL_MENUS) || (control == TOP_MENU) || (control == TO_BOTTOM_MENU))			/* Menus?	*/
	{
		vre("VIDEO-C-VERASEMENU Bad call to verase(), use verase_menu() to erase menus.");
		vexit();
	}

/*						Screen Erase Code								*/

	ret = OPTIMIZED;								/* Assume all will be ok.		*/
	out_flag = FALSE;								/* Assume no output to be done.		*/

	if (new_screen || !vera_op || (optimization <= DATA_ONLY)) 			/* Erase screen unconditionally.	*/
	{
		vdefer(RESTORE);							/* Yes, restore from deferred actions.	*/
		ret = ver_do(control,0);						/* Do the erase.			*/
		vera_op = TRUE;								/* Now turn on erase optimization.	*/
		new_screen = FALSE;							/* And turn off module exit flag.	*/
	}

	else if (optimization <= DEFER_MODE)						/* Do data and controls?		*/
	{
		vdefer(RESTORE);							/* Restore from held actions.		*/
		ret = ver_do(control,0);						/* Do the operation.			*/
	}

	else if ((control != FULL_SCREEN) || (vscr_cng != 0)) ret = ver_do(control,1);	/* We're in block mode so just tag.	*/

	if (control == FULL_SCREEN)							/* Is this a full screen erase?		*/
	{
		if (vclrhome)								/* Does cursor go home on screen clear?	*/
		{
			vcur_lin = 0;							/* Yes, then track where it went.	*/
			vcur_col = 0;
			if (out_flag)							/* Did we really do output?		*/
			{
				tcur_lin = 0;						/* Yes, then track the true values too.	*/
				tcur_col = 0;
			}
		}
		else vmove(0,0);							/* No, then force it home.		*/
	}

	return(ret);
}

/*						Subroutine to actually do the erase.						*/

int ver_do(control,op) int control, op;							/* Erase all or part of the screen.	*/
{
	int ret;									/* Return code.				*/
	register int i,ml,mc;								/* Working registers.			*/

	ml = MAX_LINES_PER_SCREEN-1;							/* Init max lines and columns.		*/
	mc = MAX_COLUMNS_PER_LINE-1;

        switch (control)								/* Select what to erase.		*/
        {
		case FULL_SCREEN:							/* Full screen clear (the jackpot).	*/
		{
			if ((ret = ver_out(efuls_esc, 0, ml, 0, 0, ml, mc, op)) == SUCCESS)		/* Clear whole screen.	*/
			{
				if (!op && ((vrol_top != 0) || (vrol_bot != MAX_LINES_PER_SCREEN-1)))	/* Reset scroll region?	*/
				{
					vrol_op = OFF;							/* Turn off scroll op.	*/
					vroll(vrol_top,vrol_bot);					/* Reset scroll region.	*/
				}
			}
			vscr_cng = 0;							/* Tag as clear.			*/
			break;
		}

		case FROM_BOS:
		{
			ret = ver_out(efbos_esc,0,vcur_lin-1,0,0,vcur_lin,vcur_col,op);		/* Clear from start.	*/
			break;
		}

		case TO_EOS:
		{
			ret = ver_out(eteos_esc,vcur_lin+1,ml,vcur_lin,vcur_col,ml,mc,op);		/* Clear to end.	*/
			break;
		}

		case CURRENT_LINE:
		{
			ret = ver_out(ecurl_esc,vcur_lin,vcur_lin,vcur_lin,0,vcur_lin,mc,op);	/* Clear current line.	*/
			break;
		}

		case FROM_BOL:
		{
			ret = ver_out(efbol_esc,-1,-1,vcur_lin,0,vcur_lin,vcur_col,op);		/* Clear from start.	*/
			break;
		}

		case TO_EOL:
		{
			ret = ver_out(eteol_esc,-1,-1,vcur_lin,vcur_col,vcur_lin,mc,op);		/* Clear to end.	*/
			break;
		}

		default:
		{
			ret = FAILURE;							/* Oops, invalid control.		*/
			vre("verase(%d)-Invalid control parameter.",control);		/* Report the condition.		*/
		}
	}
	return(ret);									/* Return to the caller.		*/
}

ver_out(string,a0,a1,sl,sc,el,ec,op) char string[]; int a0,a1,sl,sc,el,ec,op;		/* Do the operation requested.		*/
{
	register int ret;								/* Working register.			*/

	ver_nat(a0,a1);									/* Update the line attributes?		*/

	if (op)										/* Are we to optimize?			*/
	{
		if (deferred != BLOCK_MODE) vmap(TAG_AS_OLD,sl,sc,el,ec);		/* Tag the map elements old too.	*/
		vdefer(SAVE);								/* Make sure the true data is saved.	*/
		deferred = BLOCK_MODE;							/* And now we are holding all output.	*/
		ret = OPTIMIZED;							/* Report that we optimized.		*/
	}
	else
	{
		if (optimization) vmap(CLEAR,sl,sc,el,ec);				/* And clear out the maps too.		*/
#ifdef MSDOS
		vrawerase( sl,sc,el,ec );						/* Do raw erase function (vrawdos.c).	*/
#else	/* VMS or unix */
		vcontrol(string);							/* Actually clear the screen.		*/
#endif	/* VMS or unix */
		out_flag = TRUE;							/* Output was done.			*/
		ret = SUCCESS;								/* Report that all went ok.		*/
	}
	return(ret);									/* Return to the caller.		*/
}


int ver_nat(a0,a1) int a0,a1;								/* Set in new attributes.		*/
{
	register int i;									/* Working registers.			*/

	if (a0 >= 0)									/* Change the line attributes?		*/
	{
		for(i = a0; i <= a1; i++)						/* Yes, then clear them out.		*/
		{
			vlin_atr[i] = 0;						/* Attributes are removed.		*/
			vlin_cng[i] = 0;						/* And no data on this line now.	*/
		}
	}
	return(SUCCESS);								/* Return to the caller.		*/
}

/*						Subroutine entry point.								*/

int verase_menu(control,md) int control; struct video_menu *md;				/* Erase all, top or bottom menus.	*/
{
	register int ret;								/* Working register (return code).	*/

/*						Menu erasure code.								*/
	ret = FALSE;									/* Assume the erasure will fail.	*/

	switch(control)									/* How should the menu be erased?	*/
	{
		case ALL_MENUS:								/* All menus?				*/
		{
			if (md->path != NULL) 						/* Is there another selected menu?	*/
			{
				ret = verase_menu(ALL_MENUS, md->path);			/* Recurse until no path found.		*/
				md->path = NULL;					/* Now there is no longer a path.	*/
			}
			if (md->save != NULL)						/* Anything to erase?			*/
			{
				vrss(md->save);						/* Restore the screen segment.		*/
				md->save = NULL;					/* Now the control block is free.	*/
				ret = SUCCESS;						/* Did the erasure.			*/
			}
			break;								/* All done.				*/
		}

		case TO_BOTTOM_MENU:							/* Erase all but the bottom menu.	*/
		{
			if (md->path != NULL) 						/* Is there another selected menu?	*/
			{
				ret = verase_menu(ALL_MENUS, md->path);			/* Recurse until no path found.		*/
				md->path = NULL;					/* Now there is no longer a path.	*/
			}
			break;								/* Finished.				*/
		}

		case TOP_MENU:								/* Erase only the top level?		*/
		{
			if (md->path != NULL)						/* Is there another menu above us?	*/
			{
				ret = verase_menu(TOP_MENU, md->path);			/* Yes, then erase it.			*/
				if (ret == SUCCESS) 					/* Did the top level just go?		*/
				{
					md->path = NULL;				/* Yes, then no path to it now.		*/
					ret = FAILURE;					/* Not the top level any more.		*/
				}
			}
			else								/* Is this the top level?		*/
			{
				if (md->save != NULL) vrss(md->save);			/* Restore the screen segment.		*/
				md->save = NULL;					/* Now the control block is free.	*/
				ret = SUCCESS;						/* Yes, then this is the success point.	*/
			}
			break;								/* All done.				*/
		}

		default:
		{
			ret = FAILURE;							/* Oops, invalid control.		*/
			vre("verase_menu(%d)-Invalid control parameter.",control);	/* Report the condition.		*/
		}
	}

	return(ret);									/* And return as appropriate.		*/
}

