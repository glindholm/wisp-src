/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include required header files.							*/

#include <string.h>
#include "video.h"
#include "vlocal.h"									/* Include local definitions.		*/
#include "vmodules.h"
#include "vdata.h"									/* Include keyboard control definitions.*/
#include "vcap.h"
#include "vraw.h"

static int vmod_do();

/*						Subroutine entry point.								*/

int VL_vmode(int control)									/* Select character rendition.		*/
{
	register int ret;								/* Return control flag.			*/

	ret = OPTIMIZED;								/* Assume we will optimize.		*/

	if ((control < 0) || (control > VMODE_BOLD+VMODE_BLINK+VMODE_REVERSE+VMODE_UNDERSCORE))	/* Are the parameters ok?	*/
	{
		ret = FAILURE;								/* No, so tag the failure.		*/
		vre("vmode(%d)-Invalid parameter.",control);				/* Report the error.			*/
	}
	else if ((!VL_vmod_op) || (voptlevel() <= VOP_DATA_ONLY))				/* Op OFF, TRACKING_ONLY or DATA_ONLY?	*/
	{
		ret = vmod_do(control);							/* Perform the action.			*/
		VL_vmod_op = TRUE;								/* Reset for next time.			*/
	}
	else if (voptlevel() <= VOP_DEFER_MOTION_ONLY)					/* Check if data changing.		*/
	{
		if (control != vcur_atr) ret = vmod_do(control);			/* If changing, do the action.		*/
	}
	else 
	{
		if (vcur_atr != control) vdefer_save();					/* We're in DEFER or BLOCK mode.	*/
	}

	vcur_atr = control;								/* Remember what current attribs are.	*/
	return(ret);
}

/*				Subroutine to actually set character attributes.						*/

static int vmod_do(int control)								/* Do the requested action.		*/
{
	char mode_string[MAX_ESC*5];							/* Working string.			*/

	vdefer_restore();								/* Going to do output so restore first.	*/

#ifdef DIRECTVID
	if (vrawdirectio())
	{
		vrawattribute(control);							/* Set attribute 			*/
	}
	else
#endif
	{
		strcpy(mode_string,mdclr_esc);						/* Initialize for clear all renditions.	*/
		if (control & VMODE_BOLD) 	strcat(mode_string,mdbld_esc);		/* Do we want the bold rendition?	*/
		if (control & VMODE_UNDERSCORE) strcat(mode_string,mdundr_esc);		/* Do we want the underscore rendition.	*/
		if (control & VMODE_BLINK) 	strcat(mode_string,mdblk_esc);		/* Do we want the blinking rendition?	*/
		if (control & VMODE_REVERSE) 	strcat(mode_string,mdrvrs_esc);		/* Do we want reverse video rendition.	*/

		vcontrol(mode_string);							/* Turn on the rendition.		*/
	}
	
	return(SUCCESS);								/* Report that we did it.		*/
}
/*
**	History:
**	$Log: vmode.c,v $
**	Revision 1.15  2003/06/20 15:04:28  gsl
**	VL_ globals
**	
**	Revision 1.14  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.13  2002/07/17 21:06:03  gsl
**	VL_ globals
**	
**	Revision 1.12  1997/07/08 21:19:17  gsl
**	Add COSTAR for WIN32 directio logic
**	Change to use new video.h interfaces
**	
**	Revision 1.11  1996-10-11 18:16:13-04  gsl
**	drcs update
**
**
**
*/
