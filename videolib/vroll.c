			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include standard header files.							*/

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include local definitions.		*/
#include "vdata.h"									/* Include keyboard control definitions.*/
#include "vcap.h"

/*						Subroutine entry point.								*/

int vroll(top, bottom) int top; int bottom;						/* Set top and bottom of scroll area.	*/
{
	register int ret;								/* Return code.				*/

	ret = OPTIMIZED;								/* Assume we will be optimized.		*/

	if (top > bottom)
	{
		vre("vroll(%d,%d)-The line number of the scroll area top must be less than the bottom.",top,bottom);
		ret = FAILURE;
	}

	else if ((top < 0) || (bottom >= MAX_LINES_PER_SCREEN))
	{
		vre("vroll-area(%d,%d)-Invalid top or bottom value specified.",top,bottom);
		ret = FAILURE;
	}

	else if ((!vrol_op) || (optimization <= DATA_ONLY))				/* Do the action regardless?		*/
	{
		ret = vrol_do(top,bottom);						/* Do the action.			*/
		vrol_op = TRUE;								/* Off with the one-shot.		*/
	}

	else if (optimization <= DATA_CONTROLS_AND_MOTION)				/* Should we attempt to optimize?	*/
	{
		if ((top != vrol_top) || (bottom != vrol_bot)) ret = vrol_do(top,bottom);	/* Do the action if necessary.	*/
	}

	else if ((top != vrol_top) || (bottom != vrol_bot)) vdefer(SAVE);		/* Deferred mode so make change later.	*/

	vrol_top = top;									/* Remember the state now.		*/
	vrol_bot = bottom;
	return(ret);
}
int vrol_do(top,bottom) int top,bottom;							/* Actually set the scroll region.	*/
{
#ifdef MSDOS
	vrawsetscroll(top,bottom);							/* Set scroll region in vrawdos.c.	*/
#else	/* VMS or unix */
	char temp[8];									/* Temporary character storage.		*/
#ifdef unix
	char *tparm();
#define PARMFUNC tparm
#else
	char *vcparm();
#define PARMFUNC vcparm
#endif
	
	if ( !(top == 0 && bottom == MAX_LINES_PER_SCREEN-1) )
		vcapnull(scrarea_esc,"CHANGE_SCROLL_REGION",1);				/* test if defined			*/
	vdefer(RESTORE);								/* Must restore before actual output.	*/
	vcontrol(PARMFUNC(scrarea_esc,top,bottom));					/* Select the scroll area.		*/
	vha();										/* Adjust for unwanted home move.	*/
#endif	/* VMS or unix */

	return(SUCCESS);								/* And another successful job...	*/
}
