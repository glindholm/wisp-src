/*						Include required header files.							*/

#include <v/video.h>									/* Reference the VIDEO library.		*/
#include "vwang.h"									/* Reference workstation emulation.	*/

static char vw_no_mod[2];

/*						Subroutine entry point.								*/

WDISPLAY(screen)
unsigned char *screen;
{
	char vw_function[1], vw_no_mod[2], vw_lines[1];					/* vwang parameters.			*/
	unsigned char dummy[40];							/* Dummy storage.			*/

#ifdef	MSDOS										/* Debug hook for MS-DOS		*/
	v_modeflag( 1, 15, 'D' );							/* Blue box, white 'D'			*/
#endif
	vw_function[0] = WRITE_ALL;
	vw_lines[0] = 24;								/* Determine the number of lines.	*/
	wpushscr();
	vwang(vw_function,screen,vw_lines,"A",dummy,vw_no_mod);				/* Go do the I/O action.		*/
	vgetm();									/* Wait for some char.			*/
	wpopscr();

#ifdef	MSDOS										/* Debug hook for MS-DOS		*/
	v_modeflag( 6, 10, 'C' );							/* Gold box, green 'C'			*/
#endif
	return(1);									/* Return a success code.		*/
}
