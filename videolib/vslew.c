/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
******************************************************************************
*/

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
#include "vdata.h"									/* Include keyboard mapping definitions	*/
#include "vmodules.h"

/*						Subroutine entry point.								*/

int VL_vslew(nrows, ncols) int nrows, ncols;						/* Slew by nrows and ncols.		*/
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
**	Revision 1.12  2003/06/20 15:37:45  gsl
**	VL_ globals
**	
**	Revision 1.11  2003/01/31 19:25:55  gsl
**	Fix copyright header
**	
**	Revision 1.10  2002/07/15 20:16:14  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.9  1996/10/11 22:16:20  gsl
**	drcs update
**	
**
**
*/
