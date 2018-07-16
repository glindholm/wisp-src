			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include required header files.							*/

#include "video.h"
#include "vlocal.h"									/* Include local definitions.		*/
#include "vdata.h"									/* Include keyboard control definitions.*/
#include "vcap.h"

/*						Subroutine entry point.								*/

int vmode(control) int control;								/* Select character rendition.		*/
{
	register int ret;								/* Return control flag.			*/

	ret = OPTIMIZED;								/* Assume we will optimize.		*/

	if ((control < 0) || (control > BOLD+BLINK+REVERSE+UNDERSCORE))			/* Are the parameters ok?		*/
	{
		ret = FAILURE;								/* No, so tag the failure.		*/
		vre("vmode(%d)-Invalid parameter.",control);				/* Report the error.			*/
	}

	else if ((!vmod_op) || (optimization <= DATA_ONLY))				/* Op OFF, TRACKING_ONLY or DATA_ONLY?	*/
	{
		ret = vmod_do(control);							/* Perform the action.			*/
		vmod_op = TRUE;								/* Reset for next time.			*/
	}

	else if (optimization <= DATA_CONTROLS_AND_MOTION)				/* Check if data changing.		*/
	{
		if (control != vcur_atr) ret = vmod_do(control);			/* If changing, do the action.		*/
	}

	else if (vcur_atr != control) vdefer(SAVE);					/* We're in DEFER or BLOCK mode.	*/

	vcur_atr = control;								/* Remember what current attribs are.	*/
	return(ret);
}

/*				Subroutine to actually set character attributes.						*/

int vmod_do(control) int control;							/* Do the requested action.		*/
{
	char mode_string[MAX_ESC*5];							/* Working string.			*/
	register int j, ret;								/* Working registers, and return flag.	*/

	vdefer(RESTORE);								/* Going to do output so restore first.	*/

#ifdef MSDOS
	vrawattribute( control );							/* Set attribute in vrawdos.c module.	*/
#else	/* VMS or unix */
	strcpy(mode_string,mdclr_esc);							/* Initialize for clear all renditions.	*/
	if (control & BOLD) strcat(mode_string,mdbld_esc);				/* Do we want the bold rendition?	*/
	if (control & UNDERSCORE) strcat(mode_string,mdundr_esc);			/* Do we want the underscore rendition.	*/
	if (control & BLINK) strcat(mode_string,mdblk_esc);				/* Do we want the blinking rendition?	*/
	if (control & REVERSE) strcat(mode_string,mdrvrs_esc);				/* Do we want reverse video rendition.	*/

	vcontrol(mode_string);								/* Turn on the rendition.		*/
#endif	/* VMS or unix */
	return(SUCCESS);								/* Report that we did it.		*/
}
