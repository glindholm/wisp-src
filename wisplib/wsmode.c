			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			    Copyright (c) 1990				*/
			/*    An unpublished work by GregoryL. Adams.  All rights reserved.	*/
			/************************************************************************/


/*						Include required header files.							*/

#include <v/video.h>
#include <v/vlocal.h>
#include <v/vdata.h>
#include <v/vcap.h>
#include "idsistd.h"

/*						Subroutine entry point.								*/

int wsmode(control) int control;							/* Select character rendition.		*/
{
	register int j;									/* Working registers.			*/
	unsigned char mode_string[MAX_ESC*5];						/* Output string buffer.		*/

	if ((!vmod_op) || (control != vcur_atr))					/* First time or changing?		*/
	{
#ifdef MSDOS
		vrawattribute(control);
#else	/* VMS and unix */
		strcpy(mode_string,mdclr_esc);						/* Put leading escape seq into string.	*/
		if (control & BOLD) strcat(mode_string,mdbld_esc);			/* Do we want the bold rendition?	*/
		if (control & UNDERSCORE) strcat(mode_string,mdundr_esc);		/* Do we want the underscore rendition.	*/
		if (control & BLINK) strcat(mode_string,mdblk_esc);			/* Do we want the blinking rendition?	*/
		if (control & REVERSE) strcat(mode_string,mdrvrs_esc);			/* Do we want reverse video rendition.	*/
		vcontrol(mode_string);							/* Turn on the rendition.		*/
#endif	/* VMS and unix */
		vcur_atr = control;							/* Remember what current attribs are.	*/
		vmod_op = TRUE;								/* Reset for next time.			*/
	}
	return(SUCCESS);
}
