			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*						Include required header files.							*/

#include <v/video.h>									/* Reference the VIDEO library.		*/
#include "idsistd.h"
#include "vwang.h"									/* Reference workstation emulation.	*/

static char vw_no_mod[2];

/*						Subroutine entry point.								*/

WDISPLAY(screen)
unsigned char *screen;
{
	char vw_function[1], vw_no_mod[2], vw_lines[1];					/* vwang parameters.			*/
	unsigned char dummy[40];							/* Dummy storage.			*/

	vw_function[0] = DISPLAY_AND_READ;
	vw_lines[0] = 24;								/* Determine the number of lines.	*/
	wpushscr();
	vwang(vw_function,screen,vw_lines,"A",dummy,vw_no_mod);				/* Go do the I/O action.		*/
	wpopscr();

	return(1);									/* Return a success code.		*/
}
