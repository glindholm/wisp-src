/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/

/*
**	File:		wdisplay.c
**
**	Project:	WISPLIB
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
#include "vssubs.h"
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
	char	aid[1];
	int4	message_text_cnt=1;
	char	message_text[ 18*(80-1) ];			/* 18 lines of 79 characters can be displayed. */
	int4	message_text_len = sizeof(message_text);

	wtrace("DISPLAY","VERB", "[%40.40s...]", &screen[4]);
	
	memset(message_text, ' ', sizeof(message_text));
	memcpy(&message_text[0],   "PRESS (ENTER) TO CONTINUE PROGRAM", 33);
	memcpy(&message_text[3*(80-1)], &screen[4], 15*(80-1));  /* Copy starting past the order-area */

	WL_wswap(&message_text_cnt);
	WL_wswap(&message_text_len);
	
	WL_set_va_count(8);
	
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
	char	aid[1];
	int4	message_text_cnt=1;
	char	message_text[ 18*(80-1) ];			/* 18 lines of 79 characters can be displayed. */
	int4	message_text_len = sizeof(message_text);

	wtrace("STOP","VERB", "[%40.40s...]", buff);

	memset(message_text, ' ', sizeof(message_text));
	memcpy(&message_text[0],   "PRESS (ENTER) TO CONTINUE PROGRAM", 33);
	memcpy(&message_text[3*(80-1)], buff, 15*(80-1));

	WL_wswap(&message_text_cnt);
	WL_wswap(&message_text_len);
	
	WL_set_va_count(8);
	GETPARM("I ", "R", "STOP    ", aid, "STOP", "COBOL ", 
		message_text, &message_text_len);
	

}


/*
**	History:
**	$Log: wdisplay.c,v $
**	Revision 1.18  2003/02/17 22:07:17  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.17  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.16  2002/08/01 14:09:10  gsl
**	type warnings
**	
**	Revision 1.15  2002/07/12 17:01:02  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.14  2002/07/09 04:13:54  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.13  1997/05/08 20:17:16  gsl
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
