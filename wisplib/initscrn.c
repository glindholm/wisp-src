			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/*					Reference standard include files.							*/

#include <v/video.h>
#include <v/vlocal.h>
#include <v/vdata.h>
#include "idsistd.h"
#include "wperson.h"

/*					Data declarations.									*/

static int term_type = VT300;									/* Default to VT100 terminal.	*/
int kp_on = FALSE;										/* Assume keypad will be off.	*/

/*					Subroutine to initialize the screen.							*/

static int no_term = 0;
extern int rts_first;										/* First time control flag.	*/

extern char language_path[];									/* this is from wperson.c. it is*/
												/* loaded from the OPTIONS file */
int init_screen()
{
	int dummy, attributes;									/* Terminal attribute masks.	*/
	int retcod;
	register int i;										/* Working integer.		*/

	if (wbackground()) 
	{
		no_term = 1;
		return(0);									/* In background, no terminal.	*/
	}

	retcod = 1;										/* Assume success.		*/
	if (rts_first)										/* Is this the first time?	*/
	{
		int4	def_bgchange, def_excolor, def_bgcolor;

		rts_first = FALSE;								/* No longer the 1st time.	*/
		voptimize(DEFER_MODE);								/* Select appropriate optimiz.	*/

		vstate(DEFAULT);								/* Select default screen setup.	*/
		if (vscr_wid == 80) vscreen(NARROW);						/* Select dark narrow screen.	*/
		else vscreen(WIDE);								/* else dark wide screen.	*/

		get_defs(DEFAULTS_BGCHANGE,(char*)&def_bgchange);
		get_defs(DEFAULTS_EXCOLOR,(char*)&def_excolor);
		get_defs(DEFAULTS_BGCOLOR,(char*)&def_bgcolor);

		vonexit(NORMALIZE|CLEAR_SCREEN);						/* Clear the screen on exit	*/
		if (def_bgchange && def_excolor) vonexit(NORMALIZE|CLEAR_SCREEN|LIGHT);
		if (def_bgchange && !def_excolor) vonexit(NORMALIZE|CLEAR_SCREEN|DARK);

		if (def_bgchange)								/* Should we change background?	*/
		{	
			if (def_bgcolor) vscreen(LIGHT);					/* Set the screen grey.		*/
			else vscreen(DARK);							/* Set the screen black.	*/
		}
		else color_first = FALSE;							/* Then leave it alone always.	*/

		if ( strlen(language_path) )							/* if language was specified,   */
		  vlanguage( language_path );							/* use it and call vlanguage    */
	}

	if ((term_type == VT100) && kp_on) vset(KEYPAD,APPLICATIONS);				/* Applications keypad on?	*/
	else vset(KEYPAD,NORMAL);
	vset(CURSOR,INVISIBLE);									/* Don't show the cursor.	*/
	ws_erap(FULL_SCREEN);									/* Erase the full screen.	*/
	vmode(0);										/* Return to normal rendition.	*/

	return(retcod);	       									/* Return to caller, all ok...	*/
}

int check_scrn()
{
	if (rts_first) return(-1);								/* Hasn't been checked yet.	*/

	if (no_term)	return(0);								/* Say no terminal.		*/
	else		return(1);								/* Say yes terminal.		*/
}
