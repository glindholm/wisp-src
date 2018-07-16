static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wdisplay.c
**
**	Project:	WISPLIB
**
**	RCS:		$Source:$
**
**	Purpose:	Cobol DISPLAY verb
**
**	Routines:	
**	WDISPLAY()
*/

/*
**	Includes
*/

#include <string.h>

#include "idsistd.h"
#include "vwang.h"
#include "wisplib.h"
#include "werrlog.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/

/*
**	ROUTINE:	WDISPLAY()
**
**	FUNCTION:	Emulate Wang VS COBOL verb DISPLAY
**
**	DESCRIPTION:	Take a screen buffer for a COBOL DISPLAY verb and change it
**			into a GETPARM for DISPLAY.
**
**			For historical reason the 4 bytes of the screen buffer are
**			skipped (the order area).
**
**	ARGUMENTS:	
**	screen		The raw vwang screen.
**
**	GLOBALS:	None
**
**	RETURN:		none
**
**	WARNINGS:	The buff comming in is 1924 bytes (24*80+4) but only
**			1185 bytes (15*79) starting at position 4 are displayed.
**
*/
void WDISPLAY(char screen[1924])
{
	int4	argcnt;
	char	aid[1];
	int4	message_text_cnt=1;
	char	message_text[ 18*(80-1) ];			/* 18 lines of 79 characters can be displayed. */
	int4	message_text_len = sizeof(message_text);

	wtrace("DISPLAY","VERB", "[%40.40s...]", &screen[4]);
	
	memset(message_text, ' ', sizeof(message_text));
	memcpy(&message_text[0],   "PRESS (ENTER) TO CONTINUE PROGRAM", 33);
	memcpy(&message_text[3*(80-1)], &screen[4], 15*(80-1));  /* Copy starting past the order-area */

	wswap(&message_text_cnt);
	wswap(&message_text_len);
	
	argcnt = 8;
	wvaset(&argcnt);
	
	GETPARM("I ", "R", "DISPLAY ", aid, "DISP", "COBOL ", 
		message_text, &message_text_len);
	

}

/*
**	ROUTINE:	WSTOP()
**
**	FUNCTION:	Emulate Wang VS COBOL verb STOP
**
**	DESCRIPTION:	Take a buffer for a COBOL STOP verb and change it
**			into a GETPARM for STOP.
**
**	ARGUMENTS:	
**	buff		The input buffer
**
**	GLOBALS:	None
**
**	RETURN:		none
**
**	WARNINGS:	The buff comming in is 1920 bytes (24*80) but only the 
**			first 1185 bytes (15*79) are displayed.
**
*/
void WSTOP(char buff[1920])
{
	int4	argcnt;
	char	aid[1];
	int4	message_text_cnt=1;
	char	message_text[ 18*(80-1) ];			/* 18 lines of 79 characters can be displayed. */
	int4	message_text_len = sizeof(message_text);

	wtrace("STOP","VERB", "[%40.40s...]", buff);

	memset(message_text, ' ', sizeof(message_text));
	memcpy(&message_text[0],   "PRESS (ENTER) TO CONTINUE PROGRAM", 33);
	memcpy(&message_text[3*(80-1)], buff, 15*(80-1));

	wswap(&message_text_cnt);
	wswap(&message_text_len);
	
	argcnt = 8;
	wvaset(&argcnt);
	
	GETPARM("I ", "R", "STOP    ", aid, "STOP", "COBOL ", 
		message_text, &message_text_len);
	

}

#ifdef OLD
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
#endif

/*
**	History:
**	$Log: wdisplay.c,v $
**	Revision 1.13  1997-05-08 16:17:16-04  gsl
**	Add WSTOP and traceing
**
**	Revision 1.12  1997-05-01 21:02:49-04  gsl
**	Add include <string.h>
**
**	Revision 1.11  1997-05-01 16:38:26-04  gsl
**	Include wisplib.h
**
**	Revision 1.10  1997-04-22 21:39:23-04  gsl
**	Rewrite WDISPLAY to issue a GETPARM as does the Wang
**
**	Revision 1.9  1996-08-19 18:33:10-04  gsl
**	drcs update
**
**
**
*/
