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
#include "vdata.h"

/*						Subroutine entry point.								*/

int vexit()
{
	extern int exit_options;							/* What are the exit options.		*/
	register int i, op;

	if (!video_inited) return(SUCCESS);						/* Don't need to do because not inited.	*/

	op = exit_options;								/* Exit options selected.		*/
	vdefer(RESTORE);								/* Restore from optimization.		*/
	if (op & NORMALIZE) vstate(NORMAL);						/* Set terminal to a known state.	*/
	if (op & WIDE) vscreen(WIDE);							/* Wide screen wanted?			*/
	if (op & NARROW) vscreen(NARROW);						/* Narrow screen wanted?		*/
	if (op & LIGHT) vscreen(LIGHT);							/* Light screen wanted?			*/
	if (op & DARK) vscreen(DARK);							/* Dark screen wanted?			*/
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
	vdefer(RESTORE);								/* Restore again.			*/
	vcontrol(vcapdef[RESET_TERMINAL]);
	vcontrol(DUMP_OUTPUT);								/* Make sure all output goes out.	*/
	vrawexit();									/* Call low level to flush buffers etc.	*/
	synch_required = TRUE;								/* And now a synch is required.		*/
	return(SUCCESS);								/* Don't really exit.			*/
}
