static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/


/*						Include standard header files.							*/

#include <stdio.h>									/* Include the standard I/O defs.	*/
#include <stdlib.h>
#include <string.h>

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include local definitions too.	*/
#include "vdata.h"									/* Include keyboard mapping definitions	*/
#include "vcap.h"
#include "vintdef.h"
#include "vmenu.h"
#include "vmodules.h"
#include "verase.h"
#include "vraw.h"

/*						Static data.									*/

static int out_flag;									/* Output control flag.			*/
static int ver_do();
static int ver_out(char* string, int sl, int sc, int el, int ec);			/* Do the operation requested.		*/

/*						Subroutine entry point.								*/

int verase(int control)									/* Erase all or part of the screen.	*/
{
	int ret;									/* Working registers.			*/

	if ((control == ALL_MENUS) || (control == TOP_MENU) || (control == TO_BOTTOM_MENU))			/* Menus?	*/
	{
		vre("VIDEO-C-VERASEMENU Bad call to verase(), use verase_menu() to erase menus.");
		vexit();
	}

/*						Screen Erase Code								*/

	out_flag = FALSE;								/* Assume no output to be done.		*/

	ret = ver_do(control);								/* Do the erase.			*/

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

static int ver_do(int control)								/* Erase all or part of the screen.	*/
{
	int ret;									/* Return code.				*/
	int ml,mc;									/* Working registers.			*/

	vdefer_restore();								/* Restore from held actions.		*/

	ml = MAX_LINES_PER_SCREEN-1;							/* Init max lines and columns.		*/
	mc = MAX_COLUMNS_PER_LINE-1;

        switch (control)								/* Select what to erase.		*/
        {
		case FULL_SCREEN:							/* Full screen clear (the jackpot).	*/
		{
			if ((ret = ver_out(efuls_esc, 0, 0, ml, mc)) == SUCCESS)	/* Clear whole screen.	*/
			{
				if ((vrol_top != 0) || (vrol_bot != MAX_LINES_PER_SCREEN-1))		/* Reset scroll region?	*/
				{
					vrol_op = OFF;							/* Turn off scroll op.	*/
					vroll(vrol_top,vrol_bot);					/* Reset scroll region.	*/
				}
			}
			break;
		}

		case TO_EOS:
		{
			ret = ver_out(eteos_esc,vcur_lin,vcur_col,ml,mc);		/* Clear to end.	*/
			break;
		}

		case TO_EOL:
		{
			ret = ver_out(eteol_esc,vcur_lin,vcur_col,vcur_lin,mc);		/* Clear to end.	*/
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

static int ver_out(char* string, int sl, int sc, int el, int ec)			/* Do the operation requested.		*/
{
	register int ret;								/* Working register.			*/

	if (VOP_OFF != voptlevel()) vmap(CLEAR,sl,sc,el,ec);				/* And clear out the maps too.		*/

#ifdef DIRECTVID
	if (vrawdirectio())
	{
		vrawerase( sl,sc,el,ec );						/* Do raw erase function.		*/
	}
	else
#endif
	{
		vcontrol(string);							/* Actually clear the screen.		*/
	}
		
	out_flag = TRUE;								/* Output was done.			*/
	ret = SUCCESS;									/* Report that all went ok.		*/

	return(ret);									/* Return to the caller.		*/
}


/*						Subroutine entry point.								*/

int verase_menu(int control, struct video_menu *md)					/* Erase all, top or bottom menus.	*/
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

/*
**	History:
**	$Log: verase.c,v $
**	Revision 1.12  1997-07-08 16:57:59-04  gsl
**	Remove unused erase modes
**	Add COSTAR for WIN32 logic
**
**	Revision 1.11  1996-10-11 18:16:03-04  gsl
**	drcs update
**
**
**
*/
