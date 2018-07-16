static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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

void vexit()
{
	extern int exit_options;							/* What are the exit options.		*/
	int op;

	if (!video_inited) return;							/* Don't need to do because not inited.	*/

	op = exit_options;								/* Exit options selected.		*/
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
	vcap_reset_terminal();
	vcontrol_flush();								/* Make sure all output goes out.	*/
	vrawexit();									/* Call low level to flush buffers etc.	*/
	synch_required = TRUE;								/* And now a synch is required.		*/
	return;										/* Don't really exit.			*/
}
/*
**	History:
**	$Log: vexit.c,v $
**	Revision 1.12  1997/07/08 20:59:08  gsl
**	Change to use new video.h defines and interfaces
**	
**	Revision 1.11  1996-11-13 20:30:02-05  gsl
**	Use vcontrol_flush()
**
**	Revision 1.10  1996-10-11 15:16:04-07  gsl
**	drcs update
**
**
**
*/
