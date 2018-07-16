static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include standard header files.							*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "video.h"								/* Include video definitions.		*/
#include "vlocal.h"								/* Include all the local definitions.	*/ 
#include "vdata.h"								/* Include all keyboard control definitions.	*/
#include "vcap.h"
#include "vmodules.h"
#include "vscreen.h"
#include "vraw.h"

static int vscr_do(char *string);
static int vscr_valid(int state);

/*						Local definitions.								*/

#define HAVE_WIDE	(vscr_atr & VSCREEN_WIDE)					/* Screen is currently in wide mode.	*/
#define HAVE_LIGHT	(vscr_atr & VSCREEN_LIGHT)					/* Screen is currently in light mode.	*/
#define WANT_WIDE	(state & VSCREEN_WIDE)						/* Want wide mode.			*/
#define WANT_NARROW	(state & VSCREEN_NARROW)					/* Want narrow mode.			*/
#define WANT_LIGHT	(state & VSCREEN_LIGHT)						/* Want light mode.			*/
#define WANT_DARK	(state & VSCREEN_DARK)						/* Want dark mode.			*/

/*						Subroutine entry point.								*/

int vscreen(int state)									/* Set screen to a given state.		*/
{
	char string[MAX_ESC*2];								/* Output control string.		*/
	register int ret, changed_color, changed_width, atr;				/* Working registers.			*/

	if (state == VSCREEN_NOOP)
	{
		/*
		**	Clear the "first" flags.
		*/
		width_first = FALSE;
		color_first = FALSE;
		return OPTIMIZED;
	}

	if (state == VSCREEN_DEFAULT) state = VSCREEN_DARK | VSCREEN_NARROW;		/* Allow null argument case.		*/

	if (!vscr_valid(state))								/* Is the argument valid?		*/
	{
		vre("vscreen(%d)-Invalid parameter, must be DARK/LIGHT and/or NARROW/WIDE",state);
		return(FAILURE);
	}

	ret = OPTIMIZED;								/* Assume we will optimize.		*/
	atr = vscr_atr;									/* Put attributes into temp storage.	*/
	changed_color = FALSE;								/* Assume we won't change.		*/
	changed_width = FALSE;
	string[0] = '\0';								/* Initialize the string.		*/

	if (WANT_WIDE) 									/* Find out if we were correct.		*/
	{
		if (width_first || !HAVE_WIDE || (voptlevel() == VOP_OFF))		/* Force if first width change or	*/
		{									/* want to change to WIDE.		*/
			changed_width = TRUE;
			atr = (atr | VSCREEN_WIDE) & ~VSCREEN_NARROW;			/* Flag screen now as wide.		*/
			strcat(string,swide_esc);					/* Put wide into control string.	*/
		}
		width_first = FALSE;							/* No longer the first width change.	*/
	}

	if (WANT_NARROW)								/* Want narrow screen.			*/
	{
		if (width_first || HAVE_WIDE || (voptlevel() == VOP_OFF))		/* Force if first width change or	*/
		{									/* want to change to NARROW.		*/
			changed_width = TRUE;						/* Flag if it is a change.		*/
			atr = (atr | VSCREEN_NARROW) & ~VSCREEN_WIDE;			/* Flag now as narrow.			*/
			strcat(string,snarw_esc);					/* Create non-wide control string.	*/
		}
		width_first = FALSE;							/* No longer the first width change.	*/
	}

	if (WANT_LIGHT)									/* Want light screen.			*/
	{
		if (color_first || !HAVE_LIGHT || (voptlevel() == VOP_OFF))		/* First color change? or		*/
		{									/* want to change to LIGHT.		*/
			changed_color = TRUE;						/* Flag the change.			*/
			atr = (atr | VSCREEN_LIGHT) & ~VSCREEN_DARK;			/* Flag now as light.			*/
			strcat(string,slight_esc);					/* Create light control string.		*/
		}
		color_first = FALSE;							/* No longer the first color change.	*/
	}

	if (WANT_DARK)									/* Want dark screen.			*/
	{
		if (color_first || HAVE_LIGHT || (voptlevel() == VOP_OFF))		/* First color change? or		*/
		{									/* want to change to DARK.		*/
			changed_color = TRUE;						/* Flag change.				*/
			atr = (atr | VSCREEN_DARK) & ~VSCREEN_LIGHT;			/* Flag now as dark.			*/
			strcat(string,sdark_esc);					/* Create non-light control string.	*/
		}
		color_first = FALSE;							/* No longer the first color change.	*/
	}

	vscr_wid = (atr & VSCREEN_WIDE ? 132 : 80);					/* Select screen width flag.		*/

#ifdef DIRECTVID
	if (vrawdirectio())
	{
		vscr_atr = atr;								/* Rember the current attributes.	*/
		return(SUCCESS);
	}
#endif

	if (strlen(string) > 0)
	{
		synch_required = TRUE;							/* Make sure screen is synchronized.	*/
		vbuffering(VBUFF_START);						/* Group the output together.		*/
		verase(FULL_SCREEN);							/* Erase the screen before the change.	*/

		if ((!vscr_op) || (voptlevel() <= VOP_DATA_ONLY))			/* Should we optimize?			*/
	        {
			ret = vscr_do(string);						/* Do data string or other change.      */
		}
		else
		{
			if (!changed_color && !changed_width)
			{
				ret = OPTIMIZED;					/* Did the width or color change?	*/
			}
	      		else								/* Yes so process according to op.	*/
			{
				if (voptlevel() <= VOP_DEFER_MODE)
					ret = vscr_do(string);				/* Looks like we can optimize.		*/
				else
					vdefer_save();					/* Must defer...			*/
			}
		}
		vbuffering(VBUFF_END);							/* Now dump buffers as appropriate.	*/
	}

	vscr_atr = atr;									/* Rember the current attributes.	*/

	return(ret);									/* And we're all done.			*/
}

int vscreen_check(int state)
{
	return (vscr_atr & state) ? 1 : 0;
}


/*					Subroutine to do actual output								*/

static int vscr_do(char *string)
{

	vdefer_restore();								/* Yes, restore what we were doing.	*/
	vcontrol(string);								/* Output the data.			*/
	vscr_op = ON;									/* Now the one shot is on.		*/
	return(SUCCESS);								/* Return successful condition.		*/
}


/*					Subroutine to check valididy of state							*/

static int vscr_valid(int state)
{
	if ((WANT_DARK && WANT_LIGHT) || (WANT_NARROW && WANT_WIDE)) return(FAILURE);	/* Does the caller want the impossible?	*/
	if (!(state & (VSCREEN_DARK|VSCREEN_LIGHT|VSCREEN_NARROW|VSCREEN_WIDE))) 
		return(FAILURE);							/* Does he/she want anything?		*/
	return(SUCCESS);
}
/*
**	History:
**	$Log: vscreen.c,v $
**	Revision 1.12  1997/07/09 16:14:49  gsl
**	Add support for direct IO
**	Change to use new video.h interfaces
**	
**	Revision 1.11  1997-01-11 15:44:23-05  gsl
**	changed vscreen() so that it only resets and erases the screen
**	if the specified option is valid for this terminal (esc string > 0)
**	Added vscreen_check() to test the current state of the screen attribute.
**
**	Revision 1.10  1996-10-11 15:16:19-07  gsl
**	drcs update
**
**
**
*/
