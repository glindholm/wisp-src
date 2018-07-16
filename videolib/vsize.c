			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/


/*						Include standard header files.							*/

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include local definitions.		*/
#include "vdata.h"

/*						Subroutine entry point.								*/
int vsize(line,size) int size,line;
{
	register int ret;								/* Working register.			*/

	ret = OPTIMIZED;								/* Assume we will optimize.		*/

	if ((line < 0) || (line >= MAX_LINES_PER_SCREEN))				/* Is the line number valid?		*/
	{
		vre("vsize(%d,%d)-Invalid line number, must be in range 0 to MAX_LINES_PER_SCREEN",line,size);
		return(FAILURE);							/* Return failure.			*/
	}

	else if ((!vsiz_op) || (optimization <= DATA_ONLY))				/* Should we optimize at all?		*/
	{
		ret = vsiz_do(line,size);						/* Set the line to the selected size.	*/
		vsiz_op = ON;								/* Now turn on local optimization.	*/
	}

	else if (optimization <= DATA_CONTROLS_AND_MOTION)				/* Should we try to optimize?		*/
	{
		if (vlin_atr[line] != size) ret = vsiz_do(line,size);			/* Yes, then only do if changing.	*/
	}

	else if (vlin_atr[line] != size) vdefer(SAVE);					/* Else we defer it.			*/

	vlin_atr[line] = size;								/* Record the change.			*/
	return(ret);									/* Return to the caller.		*/
}

/*						Subroutine to actually set size.						*/

int vsiz_do(line,size) int line,size;							/* Set line to a specific size.		*/
{
	register int temp;								/* Working register.			*/
	char string[MAX_ESC];								/* Working string.			*/

	switch(size)									/* Select the size wanted.		*/
	{

		case SINGLE_WIDTH:  {strcpy(string,snglwide_esc); break;}			/* Single width.		*/
		case DOUBLE_WIDTH:  {vsiz_clear(line); strcpy(string,dblwide_esc); break;}	/* Double width string.		*/
		case DOUBLE_TOP:    {vsiz_clear(line); strcpy(string,dbltop_esc); break;}	/* Top half of double height.	*/
		case DOUBLE_BOTTOM: {vsiz_clear(line); strcpy(string,dblbot_esc); break;}	/* Bottom half of double hi.	*/
		case DOUBLE_HEIGHT:								/* Double hi (top and bottom).	*/
		{
			if (line > 0)							/* Is line valid for double height?	*/
			{
				vsize(line,DOUBLE_BOTTOM);				/* Yes, so call ourself properly.	*/
				vsize(line-1,DOUBLE_TOP);				/* Select top and bottom.		*/
				string[0] = '\0';					/* Nothing to output.			*/
			}
			else								/* Cannot do this on line 0.		*/
			{
				vre("vsize()-Double height text cannot start on line 0.");
				return(FAILURE);
			}
			break;
		}
		default:								/* Oops, a little error here.		*/
		{
			vre("vsize(%d,%d)-Invalid size specified",line,size);		/* Report the problem.			*/
			return(FAILURE);
		}
	}

	if (line == vcur_lin) vcontrol(string);						/* Already on the line so just output.	*/
	else										/* We have to move to the line so...	*/
	{
		temp = vcur_lin;							/* Remember what line we are on.	*/
		vmov_op = OFF;								/* Really do the move.			*/
		vmove(line,vcur_col);							/* Move to the line.			*/
		vcontrol(string);							/* Now do the change.			*/
		vmov_op = OFF;								/* Really do the move.			*/
		vmove(temp,vcur_col);							/* Now back to where we were.		*/
	}

	return(SUCCESS);								/* Unconditionally successful.		*/
}

/*					Subroutine to clear right hand edge of map.						*/

int vsiz_clear(line) int line;								/* Clear map edge when setting size.	*/
{
	register int k;									/* Working register.			*/

	k = vml(line);									/* Compute line index.			*/
	memset(&vchr_map[k][vscr_wid/2],' ',vscr_wid/2);				/* Set character map to spaces.		*/
	memset(&vatr_map[k][vscr_wid/2],'\0',vscr_wid/2);				/* Set attribute map to 0.		*/
	memset(&vchr_map[k][vscr_wid/2],'\0',vscr_wid/2);				/* Set change map to 0.			*/
	return(SUCCESS);								/* Return to the caller.		*/
}
