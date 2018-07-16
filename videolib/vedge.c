			/************************************************************************/
			/*									*/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*									*/
			/*			    Copyright (c) 1987				*/
			/*									*/
			/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
			/*									*/
			/************************************************************************/


/*						Include standard header files.							*/

#include "video.h"									/* Include video definitions.		*/


/*						Subroutine entry point.								*/

int vedge(line) register int line;							/* Determine edge of screen.		*/
{
	extern int vscr_wid, vlin_atr[MAX_LINES_PER_SCREEN];				/* Reference screen and line widths.	*/
	register int i;									/* Working registers.			*/

	i = vscr_wid;									/* Assume edge is screen width.		*/
	if (vlin_atr[line] != 0) i = i / 2;						/* Nope, edge is 1/2 screen width.	*/
	return(i);									/* All done.				*/
}
