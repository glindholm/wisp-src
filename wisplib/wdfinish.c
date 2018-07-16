/* 			WDFINISH, subroutine to restore screen after a DISPLAY verb.						*/

#include <v/video.h>

wdfinish()
{
	vmove(23,0);
	vprint("Press RETURN to continue.");
	vgetc();
	wpopscr();								/* Pop the current screen.			*/
	vset(CURSOR,INVISIBLE);							/* Set the cursor invisible.			*/
	vdefer(RESTORE);							/* Bring it up to date.				*/
}
