			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			 Copyright (c) 1988 - 1993			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/



#include <stdio.h>
#include <string.h>

#ifndef NOSTDLIB
#include <stdlib.h>
#endif

#include "video.h"
#include "vlocal.h"
#include "vdata.h"


/*
**	Routine:	valert()
**
**	Function:	To display an ALERT window with up to 3 options.
**
**	Description:	This routine displays an ALERT window and allows the user
**			to choose an option.
**
**	Arguments:
**	message		The text to display. Multiple lines can be separated with newline chars.
**	option_enter	The text for the (ENTER) option or NULL if not used. It is highly recommended
**			that you always supply an enter option.
**	option_1	The text for the (1) option or NULL if not used.
**	option_16	The text for the (16) option or NULL if not used.
**
**	Globals:	None
**
**	Return:		
**	0, 1, or 16 	based on the option selected.
**	-1		error.
**
**	Warnings:	On an error 16 is returned even if option 16 was not supplied.
**
**	History:	
**	03/31/93	Written by GSL
**
*/
int valert(message,option_enter,option_1,option_16)
char	*message;
char	*option_enter;
char	*option_1;
char	*option_16;
{
#define LINESIZE 64
	extern int verbose;								/* Verbosity flag.			*/
	int	row,col,rows,cols;
	char	*messlines[24];
	int	lines,cnt,i;
	char	*mess, *ptr;
	unsigned char *vsss(), *save;							/* Memory save pointer.			*/
	char	lastline[80],opt0[80],opt1[80],opt2[80];
	char	buff[256];
	int	rc,optcnt;

	if (!verbose) return(-1);							/* Return to the caller.		*/

	opt0[0] = (char)0;
	opt1[0] = (char)0;
	opt2[0] = (char)0;

	optcnt = 0;
	if (option_enter)	{ sprintf(opt0,"(ENTER) %s ",option_enter); optcnt++; }
	if (option_1)		{ sprintf(opt1,"(1) %s ",option_1); optcnt++; }
	if (option_16)		{ sprintf(opt2,"(16) %s",option_16); optcnt++; }

	if (strlen(opt0)+strlen(opt1)+strlen(opt2) > LINESIZE)
	{
		opt0[LINESIZE/optcnt] = (char)0;
		opt1[LINESIZE/optcnt] = (char)0;
		opt2[LINESIZE/optcnt] = (char)0;
	}
	sprintf(buff,"%s%s",opt0,opt1);
	memset(lastline,' ',LINESIZE);
	memcpy(lastline,buff,strlen(buff));
	if (option_16)
	{
		memcpy(&lastline[LINESIZE-strlen(opt2)],opt2,strlen(opt2));
	}
	lastline[LINESIZE] = (char)0;

	mess = message;
	lines=0;

	while(*mess)
	{
		messlines[lines] = malloc(LINESIZE+1);
		memset(messlines[lines],' ',LINESIZE);

		if (ptr = strchr(mess,'\n'))
		{
			cnt = ptr - mess;
		}
		else
		{
			cnt = strlen(mess);
		}

		if (cnt > LINESIZE) cnt = LINESIZE;

		memcpy(messlines[lines],mess,cnt);
		messlines[lines][LINESIZE] = (char)0;
		mess += cnt;
		lines++;
		if (*mess == '\n') mess++;
	}

	row = 4;									/* Determine where to put the window.	*/
	col = 6;

	rows = lines + 3;								/* Number of rows (including border)	*/
	cols = LINESIZE + 2;								/* Number of cols (including border)	*/

	vbuffering(LOGICAL);								/* Start buffering.			*/
	save = vsss(row, col, rows, cols);						/* Save the window area.		*/
	vbell();									/* Let the bells ring.			*/

	vmode(BOLD|REVERSE);								/* Select the background.		*/
	vcharset(DEFAULT);								/* Default character set.		*/

	for (i = 0; i < lines; i++)
	{
		vtext(BOLD|REVERSE,row+i+1,col+1,messlines[i]);
		free(messlines[i]);
	}
	vtext(BOLD|REVERSE,row+i+1,col+1,lastline);
	vgrid(row,col,rows,cols,0,0);							/* Outline the window.			*/
	vmove(row+i+1,col+32);								/* Move to an appropriate position.	*/
	for(rc = -1; rc == -1;)
	{
		int	pf;
		pf = vgetm();								/* Wait for a key.			*/

		if (option_enter && (pf == return_key || pf == enter_key)) 	rc = 0;
		else if (option_1  && pf == fn1_key)				rc = 1;
		else if (option_16 && pf == fn16_key)				rc = 16;
		else vbell();
	}

	vrss(save);									/* Restore the memory area.		*/
	vbuffering(AUTOMATIC);

	return(rc);
}