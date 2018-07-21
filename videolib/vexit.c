/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
******************************************************************************
*/

			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include standard header files.							*/

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"
#include "vcap.h"
#include "vintdef.h"
#include "vdata.h"
#include "vmodules.h"

/*						Subroutine entry point.								*/

void VL_vexit()
{
	int op;
	VL_trace("VL_vexit()");

	if (!VL_video_inited) return;							/* Don't need to do because not inited.	*/

	op = VL_exit_options;								/* Exit options selected.		*/
	vdefer_restore();								/* Restore from optimization.		*/
	if (op & NORMALIZE) vstate(VSTATE_DEFAULT);					/* Set terminal to a known state.	*/
	if (op & WIDE)   vscreen(VSCREEN_WIDE);						/* Wide screen wanted?			*/
	if (op & NARROW) vscreen(VSCREEN_NARROW);					/* Narrow screen wanted?		*/
	if (op & LIGHT)  vscreen(VSCREEN_LIGHT);					/* Light screen wanted?			*/
	if (op & DARK)   vscreen(VSCREEN_DARK);						/* Dark screen wanted?			*/
	if (op & CLEAR_SCREEN) verase(FULL_SCREEN);					/* Erase the screen?			*/
	if (op & MOVE_AND_SCROLL)							/* Move and scroll?			*/
	{
		vmove(MAX_LINES_PER_SCREEN-1,0);					/* Set position to screen lower left.	*/
		vprint("\n");								/* Scroll one line.			*/
	}
	if (op & MOVE_BOTTOM)								/* Move to bottom (no scroll)		*/
	{
		vmove(MAX_LINES_PER_SCREEN-1,0);					/* Set position to screen lower left.	*/
	}
	vdefer_restore();								/* Restore again.			*/
	VL_vcap_reset_terminal();
	VL_vcontrol_flush();								/* Make sure all output goes out.	*/
	vrawexit();									/* Call low level to flush buffers etc.	*/
	VL_synch_required = TRUE;								/* And now a synch is required.		*/
	return;										/* Don't really exit.			*/
}
/*
**	History:
**	$Log: vexit.c,v $
**	Revision 1.19  2011/08/25 23:40:15  gsl
**	tracing
**	
**	Revision 1.18  2003/06/20 15:37:44  gsl
**	VL_ globals
**	
**	Revision 1.17  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.16  2002/07/17 21:06:01  gsl
**	VL_ globals
**	
**	Revision 1.15  2002/07/16 14:11:49  gsl
**	VL_ globals
**	
**	Revision 1.14  2002/07/15 17:10:03  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.13  2002/07/12 20:40:44  gsl
**	Global unique WL_ changes
**	
**	Revision 1.12  1997/07/08 20:59:08  gsl
**	Change to use new video.h defines and interfaces
**	
**	Revision 1.11  1996-11-13 20:30:02-05  gsl
**	Use VL_vcontrol_flush()
**
**	Revision 1.10  1996-10-11 15:16:04-07  gsl
**	drcs update
**
**
**
*/
