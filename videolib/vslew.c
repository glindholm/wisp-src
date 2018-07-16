static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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
#include "vmodules.h"

/*						Subroutine entry point.								*/

int vslew(nrows, ncols) int nrows, ncols;						/* Slew by nrows and ncols.		*/
{
	extern int vcur_lin, vcur_col;							/* Reference current line and column.	*/
	register int  i, j, ret;							/* Working registers.			*/

	ret = SUCCESS;									/* Assume all will be successful	*/

	i = vcur_lin + nrows;								/* Determine the new position.		*/
	j = vcur_col + ncols;

	if ((i < 0) || (i >= MAX_LINES_PER_SCREEN) || (j < 0) || (j >= vedge(i)))	/* Is target location on the screen?	*/
	{
		vre("vslew(%d,%d)-From position (%d,%d) is off the screen.",nrows,ncols,vcur_lin,vcur_col);
		ret = FAILURE;
	}

	else ret = vmove(i,j);								/* Do the move.				*/

	return(ret);									/* Return to the caller.		*/
}
/*
**	History:
**	$Log: vslew.c,v $
**	Revision 1.9  1996-10-11 18:16:20-04  gsl
**	drcs update
**
**
**
*/
