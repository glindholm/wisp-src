/* 			WDINIT, subroutine to close out, save screen, set up for a DISPLAY verb.				*/

#include <v/video.h>

wdinit()
{
	wpushscr();								/* Push the current screen.			*/
	vmove(0,0);
	vmode(CLEAR);
	verase(FULL_SCREEN);							/* Erase it.					*/
	vset(CURSOR,VISIBLE);							/* Set the cursor visible.			*/
	vdefer(RESTORE);							/* Make it up to date.				*/
}
