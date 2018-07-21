/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "idsistd.h"
#include "vwang.h"
#include "werrlog.h"
#include "wexit.h"
#include "costar.h"
#include "win32err.h"
#include "wperson.h"
#include "wisplib.h"
#include "link.h"

/*
**	Routine:	WL_werr_message_box()
**
**	Function:	Display a message in a message box using vwang.
**
**	Description:	This is a vwang replacement for the VIDEO routine vre_window().
**			It draws a lines graphic box and displays the message "overtop"
**			of the existing screen.
**
**			For ACUCOBOL native screens call WACUERROR
**
**	Arguments:
**	instr		The message string to display.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
*/
void WL_werr_message_box(const char *instr)
{
	static	int	entry_flag = 0;
	unsigned char	wsb[WSB_LENGTH], function[1], lines[1];
	int	len, row;

	if (wbackground())
	{
		return;
	}
	
#ifdef WIN32
	/*
	**	This ensure the win32 message box is only used when running as a
	**	normal windows client and not thru telnet or with costar.
	*/
	if (use_w4w() && !use_costar())
	{
		if (WL_win32_message_box(instr))
		{
			wexit(0);
		}
		else
		{
			return;
		}
	}
#endif

	if (entry_flag)
	{
		/*
		**	A recursive call to this routine was attempted. 
		**	Just log the error and return to stop the recursion.
		*/
		WL_werr_write("ERROR: Routine werr_message_box() was recursively called.");
		WL_werr_write(instr);
		return;
	}
	entry_flag = 1;

	/*
	**	Handle errors with native screens thru cobol.
	*/
	if (wisp_nativescreens())
	{
		char	buf[1500];
		char	buflen9999[5];
		int	len;

		char	*parms[2];
		int4	lens[2];
		int4	rc;
		
		/*
		**	CALL "WACUERROR"/"WMFNERROR" USING BUFF, LEN.
		**
		**	01  BUFF PIC X(1500).
		**	01  LEN  PIC 9999.
		*/

		len = strlen(instr);
		if (len > sizeof(buf)) 
		{
			len = sizeof(buf);
		}

		memset(buf,' ',sizeof(buf));
		memcpy(buf,instr,len);

		parms[0] = buf;
		lens[0] = sizeof(buf);

		sprintf(buflen9999, "%04d", len);
		
		parms[1] = buflen9999;
		lens[1] = 4;
		
		if (wisp_acu_cobol())
		{
			WL_call_acucobol("WACUERROR", 2, parms, lens, &rc);
		}
		else if (wisp_mf_cobol())
		{
			WL_call_acucobol("WMFNERROR", 2, parms, lens, &rc);
		}

		/*
		**	If there was an error then fall thru to the vwang code.
		*/
		if (0 == rc)
		{
			entry_flag = 0;
			return;
		}
		
	}
	
	memset(wsb, ' ', sizeof(wsb));
	memcpy(wsb, "\010\300\001\001",4);

	row = 0;
	memcpy(&wsb[OA_LENGTH + WSB_COLS * row +  0], "\204*", 2);
	memset(&wsb[OA_LENGTH + WSB_COLS * row +  2], '*', 77);
	memcpy(&wsb[OA_LENGTH + WSB_COLS * row + 79], "*", 1);
	row++;
	memcpy(&wsb[OA_LENGTH + WSB_COLS * row +  0], "\204* ", 3);
	memcpy(&wsb[OA_LENGTH + WSB_COLS * row + 78], " *", 2);
	row++;

	len = strlen(instr);
	for(; len>0; row++)
	{
		memcpy(&wsb[OA_LENGTH + WSB_COLS * row +  0], "\204* ", 3);
		memcpy(&wsb[OA_LENGTH + WSB_COLS * row +  3], instr, strlen(instr) );
		memcpy(&wsb[OA_LENGTH + WSB_COLS * row + 78], " *", 2);
		instr += WSB_COLS - 5;
		len -= WSB_COLS - 5;
	}
	memcpy(&wsb[OA_LENGTH + WSB_COLS * row +  0], "\204* ", 3);
	memcpy(&wsb[OA_LENGTH + WSB_COLS * row + 78], " *", 2);
	row++;
	memcpy(&wsb[OA_LENGTH + WSB_COLS * row +  0], "\204* ", 3);
	strcpy((char*)(&wsb[OA_LENGTH + WSB_COLS * row + 23]),"**** Press (ENTER) to continue ****");
	memcpy(&wsb[OA_LENGTH + WSB_COLS * row + 78], " *", 2);
	row++;
	memcpy(&wsb[OA_LENGTH + WSB_COLS * row +  0], "\204*", 2);
	memset(&wsb[OA_LENGTH + WSB_COLS * row +  2], '*', 77);
	memcpy(&wsb[OA_LENGTH + WSB_COLS * row + 79], "*", 1);
	lines[0] = row + 1;

	function[0] = WRITE_ALL;
	vwang(function, wsb, lines, "00X",NULL,NULL);
	function[0] = READ_ALL;
	vwang(function, wsb, lines, "00X",NULL,NULL);

	entry_flag = 0;
}

/*
**	History:
**	$Log: werrvre.c,v $
**	Revision 1.32  2003/08/25 21:10:17  gsl
**	MF Native Screens
**	
**	Revision 1.31  2003/02/04 16:30:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.30  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.29  2002/12/06 22:52:24  gsl
**	WL_werr_message_box(const char*)
**	
**	Revision 1.28  2002/10/18 19:14:07  gsl
**	Cleanup
**	
**	Revision 1.27  2002/08/01 15:07:35  gsl
**	type warnings
**	
**	Revision 1.26  2002/08/01 14:09:10  gsl
**	type warnings
**	
**	Revision 1.25  2002/07/10 21:05:30  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.24  2002/07/02 04:00:38  gsl
**	change acu_cobol and mf_cobol to wisp_acu_cobol() and wisp_mf_cobol()
**	
**	Revision 1.23  2001/11/02 14:28:08  gsl
**	Changed to (ENTER)
**	
**	Revision 1.22  1998-12-15 09:49:17-05  gsl
**	Fix win32 logic to not use message box if telneting
**
**	Revision 1.21  1998-01-22 10:21:33-05  gsl
**	In werr_message_box(), check if in background and return if so.
**
**	Revision 1.20  1998-01-09 16:53:21-05  gsl
**	Changed the box drawing characters, because the '|' is mapped to
**	the british pound sign in CHARMAP
**
**	Revision 1.19  1997-10-17 16:54:41-04  gsl
**	Fix problem with multiple errors with native screens
**
**	Revision 1.18  1997-09-30 14:06:17-04  gsl
**	Add includes to fix warnings
**
**	Revision 1.17  1997-09-22 15:03:56-04  gsl
**	Add ACUCOBOL native screens support
**
**	Revision 1.16  1997-07-14 08:29:48-04  gsl
**	Fixed for COSTAR on WIN32.
**	Changed so the windows message box is not used with COSTAR.
**	Move the win32_message_box() to win32err.c
**
**	Revision 1.15  1997-03-25 09:21:39-05  gsl
**	Add a cancel button to the WIN32 message box. On unix this is done
**	thru vwang() so you can go to HELP and cancel.
**
**	Revision 1.14  1996-09-16 19:02:09-04  jockc
**	include HWND of console in MessageBox() call to attach error
**	message box to parent window.
**
**	Revision 1.12  1996-08-19 15:33:11-07  gsl
**	drcs update
**
**
**
*/
