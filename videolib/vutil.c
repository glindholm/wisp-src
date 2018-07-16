			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			 Copyright (c) 1988 - 1991			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/


/*					Include required header files.								*/

#include <stdio.h>
#ifndef unix	/* VMS or MSDOS */
#include <stdlib.h>
#endif
#ifndef VMS	/* unix or MSDOS */
#endif
#include "video.h"
#include "vlocal.h"
#include "vdata.h"

/*					Local definitions.									*/

#define DISPLAY_LINE 2


/*					Report internal errors if verbose.							*/

int vre(text,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) char text[]; long arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8;
{
	extern int verbose;								/* Verbosity control.			*/
	char string[PRINT_BUFFER_SIZE];

	if (verbose)									/* Don't report unless verbose.		*/
	{
		sprintf(string, text,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);		/* Display the text.			*/
		printf("\007\r\n%s\r\n",string);
		printf("Depress any key to continue...\r\n");
		vgetm();
	}
	return(SUCCESS);								/* Return to the caller.		*/
}

int vre_window(text,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) char text[]; long arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8;
{
	extern int verbose;								/* Verbosity flag.			*/
	char string[PRINT_BUFFER_SIZE];
	register int i;
	int 	linesize;								/* Chars of text per line.		*/
	int	currrow;								/* Current row.				*/
	int row,col,rows,cols;
	int eot;									/* End of text flag.			*/
	unsigned char *vsss(), *save;							/* Memory save pointer.			*/

	if (!verbose) return(FAILURE);							/* Return to the caller.		*/
	sprintf(string, text,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);			/* Display the text.			*/

#ifdef unix
	vraw_stty_set(); /* insure stty is properly setup in case animator has changed it */
#endif

	row = 4;									/* Determine where to put the window.	*/
	col = 6;
	linesize = 64;

	rows = ((strlen(string)+linesize-1) / linesize) + 3;				/* Number of rows (including border)	*/
	cols = linesize + 2;								/* Number of cols (including border)	*/

	vbuffering(LOGICAL);								/* Start buffering.			*/
	save = vsss(row, col, rows, cols);						/* Save the window area.		*/
	vbell();									/* Let the bells ring.			*/

	vmode(BOLD|REVERSE);								/* Select the background.		*/
	vcharset(DEFAULT);								/* Default character set.		*/
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
	vtext(BOLD|REVERSE,row+currrow,col+1,"Depress any key to continue...                                  ");
	vgrid(row,col,rows,cols,0,0);							/* Outline the window.			*/
	vmove(row+currrow,col+32);							/* Move to an appropriate position.	*/
	vgetm();									/* Wait for a key.			*/

	vrss(save);									/* Restore the memory area.		*/
	vbuffering(AUTOMATIC);

	return(SUCCESS);
}


/*					Calculate the array position in screen map.						*/
int vml(y) int y;									/* Calculate the virtual map line no.	*/
{
	extern int vmap_top;								/* Reference to current map top.	*/
	register int i;									/* Working register.			*/

	i = vmap_top + y;								/* Assume virtual = physical.		*/
	while (i >= MAX_LINES_PER_SCREEN) i = i - MAX_LINES_PER_SCREEN;			/* Normalize back on the screen.	*/
	return(i);									/* Return the position in the map.	*/
}
/*					Calculate the array position in screen map.						*/
/* This is the same as vml() except you must supply the top arg.								*/
int vmlx(top,y) int top,y;								/* Calculate the virtual map line no.	*/
{
	register int i;									/* Working register.			*/

	i = top + y;									/* Assume virtual = physical.		*/
	while (i >= MAX_LINES_PER_SCREEN) i = i - MAX_LINES_PER_SCREEN;			/* Normalize back on the screen.	*/
	return(i);									/* Return the position in the map.	*/
}


/*					Adjust for false movement to home position.						*/

int vha()										/* Move to vcur_lin and vcur_col.	*/
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

int visible(c,a) char c; int a;								/* Check char c with attributes a.	*/
{
	extern int vis_space, rvis_space;						/* Reference visible blank renditions.	*/
	if (c  < ' ') return(-1);							/* A control character?			*/
	if (c != ' ') return(TRUE);							/* As space?				*/
	if ((vscr_atr & LIGHT) && (a & (rvis_space))) return(TRUE);			/* Is space visible?			*/
	if ((vscr_atr & DARK ) && (a & ( vis_space))) return(TRUE);
	return(FALSE);									/* Not visible.				*/
}

/*					SubroutineS to adjust global change tracking flags.					*/

int vic(line) int line;									/* Adjust global change count for line.	*/
{
	extern int vscr_cng, vlin_cng[MAX_LINES_PER_SCREEN];				/* Global change control flags.		*/

	if (vscr_cng++ > MAP_SIZE) vscr_cng = MAP_SIZE;					/* More changes than chars per screen?	*/
	if (vlin_cng[line]++ > MAX_COLUMNS_PER_LINE) vlin_cng[line] = MAX_COLUMNS_PER_LINE;
	return(SUCCESS);								/* Return to the caller.		*/
}

int vdc(line) int line;									/* Adjust global change count for line.	*/
{
	extern int vscr_cng, vlin_cng[MAX_LINES_PER_SCREEN];				/* Global change control flags.		*/

	if (--vscr_cng < 0) vscr_cng = 0;						/* More changes than chars per screen?	*/
	if (--vlin_cng[line] < 0) vlin_cng[line] = 0;
	return(SUCCESS);								/* Return to the caller.		*/
}

int vmasks(size) int size;								/* Mask all but size bits.		*/
{
	return(size & (DOUBLE_WIDTH|DOUBLE_HEIGHT));					/* Return the value.			*/
}

int vmaskc(cset) int cset;								/* Mask all but character set bits.	*/
{
	return(cset & (GRAPHICS|ROM_STANDARD|ROM_GRAPHICS|DOWN_LOADED));		/* Return the value.			*/
}

int vmaskm(mode) int mode;								/* Mask all but rendition bits.		*/
{
	return(mode & (BOLD|UNDERSCORE|BLINK|REVERSE));					/* Return the value.			*/
}


/*						Save screen section								*/

unsigned char *vsss(row,col,rows,cols) int row,col,rows,cols;				/* Save a screen segment.		*/
{
	register int i, j, k;								/* Working registers.			*/
	unsigned char *save, *s;							/* Memory allocation pointers.		*/

	vbuffering(LOGICAL);								/* This is all one section.		*/
	vdefer(RESTORE);								/* Restore from any deferred states.	*/

	save = (unsigned char *)malloc( 9 + (rows*cols*2));				/* Ask for the memory.			*/
	s = save;									/* Initialize working pointer.		*/

	*s++ = (unsigned char) row;							/* Save the row value.			*/
	*s++ = (unsigned char) col;							/* Save the column value.		*/
	*s++ = (unsigned char) rows;							/* Save the number of rows.		*/
	*s++ = (unsigned char) cols;							/* Save the number of columns.		*/

	if (save == NULL) vre("Error in vsss(), unable to allocate memory to save screen section.");	/* Did we get it?	*/
	else
	{
		for (i = row; i < row+rows; i++)					/* Loop through rows.			*/
		{
			k = vml(i);							/* Get the true map position.		*/
			for (j = col; j < col+cols; j++)				/* Loop through the columns.		*/
			{
				*s++ = vatr_map[k][j];					/* Save the attribute value.		*/
				*s++ = vchr_map[k][j];					/* Save the character value.		*/
			}
		}
	}

	*s++ = (unsigned char) vcur_lin;						/* Save where the cursor is.		*/
	*s++ = (unsigned char) vcur_col;
	*s++ = (unsigned char) vcur_atr;
	*s++ = (unsigned char) vchr_set;
	*s++ = (unsigned char) vcur_set[14];						/* Save the cursor status.		*/

	vbuffering(AUTOMATIC);								/* End of the logical section.		*/
	return(save);									/* Return the pointer.			*/
}

/*						Restore screen section.								*/

int vrss(loc) unsigned char *loc; 							/* Restore a screen segment.		*/
{
	register int i, j, m;								/* Working registers.			*/
	unsigned char *s;								/* Memory allocation pointers.		*/
	int row,col,rows,cols;								/* Position information.		*/

	vbuffering(LOGICAL);								/* This is all one section.		*/
	vdefer(RESTORE);								/* Restore from any deferred states.	*/

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
			if ((m = vmaskm(*s)) != vcur_atr) vmode(m);			/* Select the mode.			*/
			if ((m = vmaskc(*s++)) != vchr_set) vcharset(m);		/* Select the character set.		*/
			vputc(*s++);							/* Output the character.		*/
		}
	}

	i = (int) *s++;									/* Get the current position.		*/
	j = (int) *s++;
	vmove(i,j);									/* Restore it.				*/
	i = (int) *s++;
	vmode(i);									/* Restore the rendition and char set.	*/
	i = (int) *s++;
	vcharset(i);
	i = (int) *s++;									/* Make the cursor visible again.	*/
	vset(CURSOR,i);

	free(loc);									/* Free the memory.			*/
	loc = NULL;									/* Now set the pointer to a null.	*/
	vbuffering(AUTOMATIC);								/* End of the logical section.		*/
	return(SUCCESS);								/* Return the pointer.			*/
}

/*			Invalidate a section of the map (assume caller knows what he is doing!)					*/

varb(row,col,rows,cols) int row,col,rows,cols;						/* Invalidate map section.		*/
{
	register int i, j, k;								/* Working registers.			*/

	for (i = row; i < row+rows; i++)						/* Loop through rows.			*/
	{
		k = vml(i);								/* Get the true map position.		*/
		for (j = col; j < col+cols; j++) vchr_map[k][j] = 0177;			/* Set to an impossible character.	*/
	}
}
