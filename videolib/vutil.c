/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** $Id:$
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
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			 Copyright (c) 1988 - 1991			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/


/*					Include required header files.								*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include <sys/types.h>
#include <sys/stat.h>

#ifdef WIN32
#include <io.h>
#endif

#include <errno.h>
#include <time.h>

#include "vutil.h"
#include "video.h"
#include "vlocal.h"
#include "vdata.h"
#include "vmodules.h"
#include "vraw.h"

/*					Local definitions.									*/

#define DISPLAY_LINE 2

static char vre_logfile[128] = "videoerr.log"; 	/* Log ver() errors to this file. */

static int rvis_space = VMODE_REVERSE|VMODE_UNDERSCORE|VMODE_BLINK|VMODE_BOLD;		/* Visible on reverse screen in these cases.	*/
static int vis_space = VMODE_REVERSE|VMODE_UNDERSCORE|VMODE_BLINK;			/* Visible on normal screen in these cases.	*/


/*
 *	Set the log file
 */
void VL_vre_set_logfile(const char* filepath)
{
	if (NULL == filepath)
	{
		vre_logfile[0] = '\0';
	}
	else
	{
		strcpy(vre_logfile, filepath);
	}
}

/*
 *	Write to the log file
 */
void VL_vre_write_logfile(const char* buff)						/* Write out a buffer to the error log.	*/
{
	int created_log = 0;
	FILE *efile;									/* Pointer to error log file.		*/
	time_t clock;
	char	timestamp[40];

	if ('\0' == vre_logfile[0])							/* If no log file just return		*/
	{
		return;
	}
	
	efile = fopen(vre_logfile,"a");							/* First try to append.			*/
	if (!efile) 
	{
		efile = fopen(vre_logfile,"w");						/* Doesn't exist, create it.		*/
		created_log = 1;
	}

	if (!efile)									/* If can't open then use stderr	*/
	{
		fprintf(stderr,"%s\n",buff);
		return;
	}

	clock = time(0);								/* Format timestamp	*/
	strcpy(timestamp, ctime(&clock));
	timestamp[strlen(timestamp) - 1] = '\0';					/* Remove the trailing newline 		*/

	fprintf(efile,"%s %s\n",timestamp, buff);					/* Write buff to file with timestamp	*/

	fclose(efile);									/* All done!				*/

	if ( created_log )
	{
		chmod(vre_logfile, 0666);						/* Allow all to write.			*/
	}
}


/*					Report internal errors if verbose.							*/
int VL_vre(char *text, ...)
{
	static int already_in_vre = 0;
	char string[PRINT_BUFFER_SIZE];
	va_list args;
	
	va_start(args,text);

	if (already_in_vre)
	{
		/*
		**	If vre() is called recursively then we have completely lost control
		**	of the terminal (probably disconnected) so just exit.
		**	(Don't call vexit as it will probably result in another vre() call.)
		*/
		exit(2);
	}

	already_in_vre = 1;

	if (VL_verbose)									/* Don't report unless verbose.		*/
	{
		vsprintf(string, text,args);						/* Format the message.			*/

		VL_vre_write_logfile(string);						/* Log the message			*/
		
		vrawerror(string);							/* Display message			*/
	}

	already_in_vre = 0;
	return(SUCCESS);								/* Return to the caller.		*/
}

int VL_vre_window(char *text, ...)
{
	char string[PRINT_BUFFER_SIZE];
	register int i;
	int linesize;									/* Chars of text per line.		*/
	int currrow;									/* Current row.				*/
	int row,col,rows,cols;
	int eot;									/* End of text flag.			*/
	unsigned char *save;							/* Memory save pointer.			*/
	va_list args;

	va_start(args,text);

	if (!VL_verbose) return(FAILURE);							/* Return to the caller.		*/
	vsprintf(string, text,args);							/* Display the text.			*/

	row = 4;									/* Determine where to put the window.	*/
	col = 6;
	linesize = 64;

	rows = ((strlen(string)+linesize-1) / linesize) + 3;				/* Number of rows (including border)	*/
	cols = linesize + 2;								/* Number of cols (including border)	*/

	if (!VL_state_active) { vstate(0); verase(FULL_SCREEN); }				/* Initialize if not already done.	*/

	VL_vbuffering_start();					       		/* Start buffering.			*/
	save = vsss(row, col, rows, cols);						/* Save the window area.		*/
	vbell();									/* Let the bells ring.			*/

	VL_vmode(VMODE_BOLD|VMODE_REVERSE);						/* Select the background.		*/
	vcharset(VCS_DEFAULT);								/* Default character set.		*/
	eot = FALSE;									/* Not at end of text yet.		*/
	currrow = 0;
	for (i = 0; i < linesize*(rows-3); i++)
	{
		if (i == linesize*currrow)
		{
			currrow++;							/* Next row.				*/
			vmove(row+currrow,col+1);					/* Move to next row			*/
		}

		if (!eot && (string[i] != CHAR_NULL)) vputc(string[i]);			/* Output the character.		*/
		else
		{
			eot = TRUE;							/* Assume we just reached eot.		*/
			vputc(' ');							/* Output a space.			*/
		}
	}
	currrow++;
	vtext(VMODE_BOLD|VMODE_REVERSE,row+currrow,col+1,"Depress any key to continue...                                  ");
	vgrid(row,col,rows,cols,0,0);							/* Outline the window.			*/
	vmove(row+currrow,col+32);							/* Move to an appropriate position.	*/
	vgetm();									/* Wait for a key.			*/

	vrss(save);									/* Restore the memory area.		*/
	VL_vbuffering_end();

	return(SUCCESS);
}


/*					Calculate the array position in screen map.						*/
int VL_vml(int y)										/* Calculate the virtual map line no.	*/
{
	extern int vmap_top;								/* Reference to current map top.	*/
	register int i;									/* Working register.			*/

	i = vmap_top + y;								/* Assume virtual = physical.		*/
	while (i >= MAX_LINES_PER_SCREEN) i = i - MAX_LINES_PER_SCREEN;			/* Normalize back on the screen.	*/
	return(i);									/* Return the position in the map.	*/
}
/*					Calculate the array position in screen map.						*/
/* This is the same as vml() except you must supply the top arg.								*/
int VL_vmlx(int top, int y)								/* Calculate the virtual map line no.	*/
{
	register int i;									/* Working register.			*/

	i = top + y;									/* Assume virtual = physical.		*/
	while (i >= MAX_LINES_PER_SCREEN) i = i - MAX_LINES_PER_SCREEN;			/* Normalize back on the screen.	*/
	return(i);									/* Return the position in the map.	*/
}


/*					Adjust for false movement to home position.						*/

int VL_vha(void)									/* Move to vcur_lin and vcur_col.	*/
{
	extern int vcur_lin, vcur_col;							/* Current postion.			*/
	register int i, j;								/* Working registers.			*/

	i = vcur_lin;									/* i = line we should be on.		*/
	j = vcur_col;									/* j = column we should be on.		*/
	vcur_lin = 0;									/* But we're actually at home.		*/
	vcur_col = 0;
	return(vmove(i,j));								/* Now go to where we should be.	*/
}


/*					Subroutine to determine if data is visible.						*/

int VL_visible(char c, int a)								/* Check char c with attributes a.	*/
{
	if (c != ' ') return(TRUE);							/* As space?				*/
	if ((vscr_atr & VSCREEN_LIGHT) && (a & (rvis_space))) return(TRUE);		/* Is space visible?			*/
	if ((vscr_atr & VSCREEN_DARK ) && (a & ( vis_space))) return(TRUE);
	return(FALSE);									/* Not visible.				*/
}

/*					SubroutineS to adjust global change tracking flags.					*/

int VL_vmaskc(int cset)									/* Mask all but character set bits.	*/
{
	return(cset & (VCS_GRAPHICS|VCS_ROM_STANDARD|VCS_ROM_GRAPHICS|VCS_DOWN_LOADED));/* Return the value.			*/
}

int VL_vmaskm(int mode)									/* Mask all but rendition bits.		*/
{
	return(mode & (VMODE_BOLD|VMODE_UNDERSCORE|VMODE_BLINK|VMODE_REVERSE));		/* Return the value.			*/
}


/*						Save screen section								*/

unsigned char *vsss(int row, int col, int rows, int cols)				/* Save a screen segment.		*/
{
	register int i, j, k;								/* Working registers.			*/
	unsigned char *save, *s;							/* Memory allocation pointers.		*/
	int savesize;

	VL_vbuffering_start();							/* This is all one section.		*/
	vdefer_restore();								/* Restore from any deferred states.	*/

	savesize = 9 + (rows*cols*2);
	save = (unsigned char *)malloc( savesize );					/* Ask for the memory.			*/

	if (save == NULL) 								/* Did we get it?			*/
	{
		vre("Error in vsss(), unable to allocate memory to save screen section.");	
	}
	else
	{
		s = save;								/* Initialize working pointer.		*/

		*s++ = (unsigned char) row;						/* Save the row value.			*/
		*s++ = (unsigned char) col;						/* Save the column value.		*/
		*s++ = (unsigned char) rows;						/* Save the number of rows.		*/
		*s++ = (unsigned char) cols;						/* Save the number of columns.		*/

		for (i = row; i < row+rows; i++)					/* Loop through rows.			*/
		{
			k = vml(i);							/* Get the true map position.		*/
			for (j = col; j < col+cols; j++)				/* Loop through the columns.		*/
			{
				*s++ = vatr_map[k][j];					/* Save the attribute value.		*/
				*s++ = vchr_map[k][j];					/* Save the character value.		*/
			}
		}

		*s++ = (unsigned char) vcur_lin;					/* Save where the cursor is.		*/
		*s++ = (unsigned char) vcur_col;
		*s++ = (unsigned char) vcur_atr;
		*s++ = (unsigned char) vchr_set;
		*s++ = (unsigned char) vcur_set[VSET_CURSOR];				/* Save the cursor status.		*/

		if ((s-save) > savesize)
		{
			vre("Error in vsss(), save used > savesize.");
		}
	}

	VL_vbuffering_end();								/* End of the logical section.		*/
	return(save);									/* Return the pointer.			*/
}

/*						Restore screen section.								*/

int VL_vrss(unsigned char *loc)								/* Restore a screen segment.		*/
{
	register int i, j, m;								/* Working registers.			*/
	unsigned char *s;								/* Memory allocation pointers.		*/
	int row,col,rows,cols;								/* Position information.		*/

	VL_vbuffering_start();							/* This is all one section.		*/
	vdefer_restore();								/* Restore from any deferred states.	*/

	s = loc;									/* Initialize working pointer.		*/

	row = (int) *s++;								/* Get the position information.	*/
	col = (int) *s++;
	rows = (int) *s++;
	cols = (int) *s++;

	for (i = row; i < row+rows; i++)						/* Loop through rows.			*/
	{
		vmove(i,col);								/* Move to the start location.		*/
		for (j = col; j < col+cols; j++)					/* Loop through the columns.		*/
		{
			if ((m = VL_vmaskm(*s)) != vcur_atr) VL_vmode(m);			/* Select the mode.			*/
			if ((m = VL_vmaskc(*s++)) != vchr_set) vcharset(m);		/* Select the character set.		*/
			vputc(*s++);							/* Output the character.		*/
		}
	}

	i = (int) *s++;									/* Get the current position.		*/
	j = (int) *s++;
	vmove(i,j);									/* Restore it.				*/
	i = (int) *s++;
	VL_vmode(i);									/* Restore the rendition and char set.	*/
	i = (int) *s++;
	vcharset(i);
	i = (int) *s++;									/* Make the cursor visible again.	*/
	if (i) 
		vset_cursor_on();
	else
		vset_cursor_off();

	free(loc);									/* Free the memory.			*/
	loc = NULL;									/* Now set the pointer to a null.	*/
	VL_vbuffering_end();								/* End of the logical section.		*/
	return(SUCCESS);								/* Return the pointer.			*/
}

/*			Invalidate a section of the map (assume caller knows what he is doing!)					*/

void VL_varb(int row, int col, int rows, int cols)						/* Invalidate map section.		*/
{
	register int i, j, k;								/* Working registers.			*/

	for (i = row; i < row+rows; i++)						/* Loop through rows.			*/
	{
		k = vml(i);								/* Get the true map position.		*/
		for (j = col; j < col+cols; j++) vchr_map[k][j] = 0177;			/* Set to an impossible character.	*/
	}
}

void VL_vtitle(const char *titlestr)
{
	vrawtitle(titlestr);
}


/*
**	History:
**	$Log: vutil.c,v $
**	Revision 1.20  2010/02/10 03:52:33  gsl
**	fix warnings for redefined functions
**	
**	Revision 1.19  2003/01/31 19:25:55  gsl
**	Fix copyright header
**	
**	Revision 1.18  2002/07/17 21:06:05  gsl
**	VL_ globals
**	
**	Revision 1.17  2002/07/15 20:16:16  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.16  2002/07/15 17:10:07  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.15  2001/10/12 20:06:53  gsl
**	Add logging of vre() messages
**	
**	Revision 1.14  1997-07-09 12:37:52-04  gsl
**	Change to use new interface
**	Remove obsolete routines.
**	Fix interface to vraw routines
**
**	Revision 1.13  1997-01-11 18:37:23-05  gsl
**	For WIN32 vre() does a vrawerror() which puts up a message box
**
**	Revision 1.12  1996-11-12 15:16:57-08  jockc
**	added vtitle function
**
**	Revision 1.11  1996-07-17 14:26:08-07  jockc
**	changed hard coded optional args to use stdarg ... syntax
**
**	Revision 1.10  1996-03-28 13:16:07-08  gsl
**	Fix defines and prototype
**
 * Revision 1.9  1996/03/12  13:25:59  gsl
 * fic vbuffering()
 * thats FIX.
 *
**
**
*/
