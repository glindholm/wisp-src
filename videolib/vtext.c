			/************************************************************************/
			/*									*/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*									*/
			/*			    Copyright (c) 1987				*/
			/*									*/
			/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
			/*									*/
			/************************************************************************/


/*						Include standard header files.							*/

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include local references.		*/

/*						Subroutine entry point.								*/

int vtext(display,row,column,text,a0,a1,a2,a3) int display, row, column; char *text; long a0, a1, a2, a3;
{
	int push_flag;									/* Flags.				*/
	register int i, j;								/* Working registers.			*/

	vbuffering(LOGICAL);								/* Turn on logical buffering.		*/
	if (display < 0)								/* Old rendition restored on exit?	*/
	{
		display = -display;							/* Convert to positive entity.		*/
		vstate(SAVE);								/* Save information.			*/
		push_flag = TRUE;							/* Flag that we have to restore later.	*/
	}
	else push_flag = FALSE;								/* Flag that we don't have to restore.	*/
	vcharset(vmaskc(display));							/* Select character set.		*/
	vmode(vmaskm(display));								/* Select character rendition.		*/
	if ((display & DOUBLE_HEIGHT) && (row-1 >= 0))					/* Is this a double high line.		*/
	{
		vmove(row-1,column);							/* Move to the last line.		*/
		vsize(row-1,DOUBLE_TOP);						/* Select the new size.			*/
		vprint(text,a0,a1,a2,a3);						/* Print the text.			*/
		vmove(row,column);							/* Move to the bottom line.		*/
		vsize(row,DOUBLE_BOTTOM);						/* Select the size.			*/
	}
	else										/* Not a double height line.		*/
	{
		vmove(row,column);							/* Move to the line.			*/
		vsize(row,vmasks(display));						/* Select line size.			*/
	}
	vprint(text,a0,a1,a2,a3);							/* Output the text.			*/
	if (push_flag) vstate(RESTORE);							/* Do we have to restore?		*/
	vbuffering(AUTOMATIC);								/* Restore automatic buffering.		*/
	return(SUCCESS);								/* Return to the caller.		*/
}
