			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include standard header files.							*/

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include all the local definitions.	*/ 
#include "vdata.h"									/* Include all keyboard control definitions.	*/
#include "vcap.h"

/*						Local definitions.								*/

#define HAVE_WIDE	(vscr_atr & WIDE)						/* Screen is currently in wide mode.	*/
#define HAVE_LIGHT	(vscr_atr & LIGHT)						/* Screen is currently in light mode.	*/
#define WANT_WIDE	(state & WIDE)							/* Want wide mode.			*/
#define WANT_NARROW	(state & NARROW)						/* Want narrow mode.			*/
#define WANT_LIGHT	(state & LIGHT)							/* Want light mode.			*/
#define WANT_DARK	(state & DARK)							/* Want dark mode.			*/

/*						Subroutine entry point.								*/

vscreen(state) int state;								/* Set screen to a given state.		*/
{
	char string[MAX_ESC*2];								/* Output control string.		*/
	register int j, ret, changed_color, changed_width, atr;				/* Working registers.			*/

	if (state == 0) state = DARK | NARROW;						/* Allow null argument case.		*/

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
		if (width_first || !HAVE_WIDE || (optimization == OFF))			/* Force if first width change or	*/
		{									/* want to change to WIDE.		*/
			changed_width = TRUE;
			atr = (atr | WIDE) & ~NARROW;					/* Flag screen now as wide.		*/
			strcat(string,swide_esc);					/* Put wide into control string.	*/
			synch_required = TRUE;						/* Make sure screen is synchronized.	*/
		}
		width_first = FALSE;							/* No longer the first width change.	*/
	}

	if (WANT_NARROW)								/* Want narrow screen.			*/
	{
		if (width_first || HAVE_WIDE || (optimization == OFF))			/* Force if first width change or	*/
		{									/* want to change to NARROW.		*/
			changed_width = TRUE;						/* Flag if it is a change.		*/
			atr = (atr | NARROW) & ~WIDE;					/* Flag now as narrow.			*/
			strcat(string,snarw_esc);					/* Create non-wide control string.	*/
			synch_required = TRUE;						/* Make sure screen is synchronized.	*/
		}
		width_first = FALSE;							/* No longer the first width change.	*/
	}

	if (WANT_LIGHT)									/* Want light screen.			*/
	{
		if (color_first || !HAVE_LIGHT || (optimization == OFF))		/* First color change? or		*/
		{									/* want to change to LIGHT.		*/
			changed_color = TRUE;						/* Flag the change.			*/
			atr = (atr | LIGHT) & ~DARK;					/* Flag now as light.			*/
			strcat(string,slight_esc);					/* Create light control string.		*/
			synch_required = TRUE;						/* Make sure screen is synchronized.	*/
		}
		color_first = FALSE;							/* No longer the first color change.	*/
	}

	if (WANT_DARK)									/* Want dark screen.			*/
	{
		if (color_first || HAVE_LIGHT || (optimization == OFF))			/* First color change? or		*/
		{									/* want to change to DARK.		*/
			changed_color = TRUE;						/* Flag change.				*/
			atr = (atr | DARK) & ~LIGHT;					/* Flag now as dark.			*/
			strcat(string,sdark_esc);					/* Create non-light control string.	*/
			synch_required = TRUE;						/* Make sure screen is synchronized.	*/
		}
		color_first = FALSE;							/* No longer the first color change.	*/
	}

	vscr_wid = (atr & WIDE ? 132 : 80);						/* Select screen width flag.		*/
	vbuffering(LOGICAL);								/* Group the output together.		*/
	verase(FULL_SCREEN);								/* Erase the screen before the change.	*/

	if ((!vscr_op) || (optimization <= DATA_ONLY))					/* Should we optimize?			*/
        {
		ret = vscr_do(string);							/* Do data string or other change.      */
#ifdef OLD_unix
		/*
		**	This was causing pointless delays in many cases.
		**	The sleep(1) logic has been moved into vdisplay() where actual screen width changes occur.
		*/
		if (changed_width)
			sleep(1);							/* Wait for some terminals to do it.    */
#endif
	}
	else
		if (!changed_color && !changed_width)
			ret = OPTIMIZED;						/* Did the width or color change?	*/
      		else									/* Yes so process according to op.	*/
		{
			if (optimization <= DEFER_MODE)
				ret = vscr_do(string);					/* Looks like we can optimize.		*/
			else
				vdefer(SAVE);						/* Must defer...			*/
		}

	vscr_atr = atr;									/* Rember the current attributes.	*/
	vbuffering(AUTOMATIC);								/* Now dump buffers as appropriate.	*/
	return(ret);									/* And we're all done.			*/
}


/*					Subroutine to do actual output								*/

int vscr_do(string) char string[];
{

	vdefer(RESTORE);								/* Yes, restore what we were doing.	*/
	vcontrol(string);								/* Output the data.			*/
	vscr_op = ON;									/* Now the one shot is on.		*/
	return(SUCCESS);								/* Return successful condition.		*/
}


/*					Subroutine to check valididy of state							*/

int vscr_valid(state) int state;
{
	if ((WANT_DARK && WANT_LIGHT) || (WANT_NARROW && WANT_WIDE)) return(FAILURE);	/* Does the caller want the impossible?	*/
	if (!(state & (DARK|LIGHT|NARROW|WIDE))) return(FAILURE);			/* Does he/she want anything?		*/
	return(SUCCESS);
}
