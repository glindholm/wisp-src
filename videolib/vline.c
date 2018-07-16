			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			 Copyright (c) 1987-1991			*/
			/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
			/************************************************************************/


/*						Include standard header files.							*/

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include local definitions.		*/
#include "vdata.h"
#include "vcap.h"


/*						Global and static data.								*/

static unsigned char x, q, l, k, m, j, w, v, t, u, n;					/* ANSI DEC like short form ints.	*/
static unsigned char ctable[132];							/* Map of crossing characters.		*/
static unsigned char klntuwx[8];
static unsigned char jknuvwq[8];
static unsigned char jmntuvx[8];
static unsigned char lmntvwq[8];

/*						Subroutine entry point.								*/

int vline(type, length) int type, length;						/* Draw a line.				*/
{
	register int i, j0, ret;							/* Working registers.			*/

	x = vcapdef[GRAPHSTR][SINGLE_VERTICAL_BAR];
	q = vcapdef[GRAPHSTR][SINGLE_HORIZONTAL_BAR];
	l = vcapdef[GRAPHSTR][SINGLE_UPPER_LEFT_CORNER];
	k = vcapdef[GRAPHSTR][SINGLE_UPPER_RIGHT_CORNER];
	m = vcapdef[GRAPHSTR][SINGLE_LOWER_LEFT_CORNER];
	j = vcapdef[GRAPHSTR][SINGLE_LOWER_RIGHT_CORNER];
	w = vcapdef[GRAPHSTR][SINGLE_UPPER_TEE];
	v = vcapdef[GRAPHSTR][SINGLE_LOWER_TEE];
	t = vcapdef[GRAPHSTR][SINGLE_LEFT_TEE];
	u = vcapdef[GRAPHSTR][SINGLE_RIGHT_TEE];
	n = vcapdef[GRAPHSTR][SINGLE_CROSS];

	ctable[0] = q;
	ctable[1] = v;
	ctable[2] = q;
	ctable[3] = m;
	ctable[4] = w;
	ctable[5] = n;
	ctable[6] = l;
	ctable[7] = t;
	ctable[8] = q;
	ctable[9] = j;
	ctable[10] = q;
	ctable[11] = v;
	ctable[12] = k;
	ctable[13] = u;
	ctable[14] = w;
	ctable[15] = n;
	ctable[16] = x;
	ctable[17] = x;
	ctable[18] = t;
	ctable[19] = m;
	ctable[20] = x;
	ctable[21] = x;
	ctable[22] = l;
	ctable[23] = t;
	ctable[24] = u;
	ctable[25] = j;
	ctable[26] = n;
	ctable[27] = v;
	ctable[28] = k;
	ctable[29] = u;
	ctable[30] = w;
	ctable[31] = n;
	ctable[32] = CHAR_NULL;

	klntuwx[0] = k;
	klntuwx[1] = l;
	klntuwx[2] = n;
	klntuwx[3] = t;
	klntuwx[4] = u;
	klntuwx[5] = w;
	klntuwx[6] = x;
	klntuwx[7] = CHAR_NULL;

	jknuvwq[0] = j;
	jknuvwq[1] = k;
	jknuvwq[2] = n;
	jknuvwq[3] = u;
	jknuvwq[4] = v;
	jknuvwq[5] = w;
	jknuvwq[6] = q;
	jknuvwq[7] = CHAR_NULL;

	jmntuvx[0] = j;
	jmntuvx[1] = m;
	jmntuvx[2] = n;
	jmntuvx[3] = t;
	jmntuvx[4] = u;
	jmntuvx[5] = v;
	jmntuvx[6] = x;
	jmntuvx[7] = CHAR_NULL;

	lmntvwq[0] = l;
	lmntvwq[1] = m;
	lmntvwq[2] = n;
	lmntvwq[3] = t;
	lmntvwq[4] = v;
	lmntvwq[5] = w;
	lmntvwq[6] = q;
	lmntvwq[7] = CHAR_NULL;

	ret = SUCCESS;									/* Assume all will be successful.	*/
	switch(type)									/* Determine the type of line.		*/
	{
		case VERTICAL: case FAT_VERTICAL:					/* Vertical lines?			*/
		{
			i = vcur_lin + length;						/* Determine end position.		*/
			if (length < 0)	i++;						/* Adjust for direction.		*/
			else i--;
			if ((i < 0) || (i >= MAX_LINES_PER_SCREEN))			/* End position valid?			*/
			{
				vre("vline(%d,%d)-Length too long, drawing from line %d.",type,length,vcur_lin);
				ret = FAILURE;
			}
			else if (length) ret = do_vert(type,length,i);			/* Draw the line if not zero length	*/
			break;								/* All done.				*/
		}
		case HORIZONTAL: case FAT_HORIZONTAL:
		{
			i = vcur_col + length;						/* Determine end position of line.	*/
			if (length < 0)	i++;						/* Adjust for direction.		*/
			else i--;
			if ((i < 0) || (i >= vscr_wid))					/* Still on the screen?			*/
			{
				vre("vline(%d,%d)-Length too long, drawing from line %d.",type,length,vcur_lin);
				ret = FAILURE;
			}
			else if (length) ret = do_horiz(type, length, i);		/* Draw the line if not zero length	*/
			break;								/* Don't drop through.			*/
		}
		default:								/* Hmmm, he doesn't know what he wants.	*/
		{
			vre("vline(%d,%d)-Invalid line type.");				/* Report the condition.		*/
			ret = FAILURE;							/* Return with failure.			*/
		}
	}
	return(ret);									/* Return to the caller.		*/
}

/*				Subroutine to draw vertical lines.								*/

static int do_vert(type, length, end_line) int type, length, end_line;			/* Draw vertical line.			*/
{
	int cs_save, md_save;								/* Save locations.			*/
	unsigned char tchar;								/* A temp string.			*/
	unsigned char cross;								/* The crossing character.		*/
	int  upper, lower, here, cvalue;						/* work vars				*/
	register int i, j0, k0, m0, n0;							/* Working registers.			*/

	vbuffering(LOGICAL);								/* Turn on logical buffering.		*/

	j0 = vcur_lin;									/* Assume we start at current line.	*/
	m0 = vcur_col;
	k0 = 1;										/* Going in positive direction.		*/

	if (end_line < vcur_lin)							/* Where to start?			*/
	{
		if (vlin_op) j0 = end_line;						/* If optimizing, start at end of line.	*/
		else k0 = -1;								/* No, then go in reverse direction.	*/
		length = -length;							/* Get absolute value of length.	*/
	}

	cs_save = vchr_set;								/* Remember the old character set.	*/
	md_save = vcur_atr;								/* Remember the old rendition.		*/
	if (type == FAT_VERTICAL) vmode(REVERSE | (vcur_atr & BOLD));			/* Remove all rendition except bold.	*/
	else vcharset(GRAPHICS);							/* Or go into graphics mode.		*/

	for (i = 0; i < length; i++)							/* Loop...				*/
	{
		vmove(j0+(i*k0),m0);							/* Move to where this element starts.	*/
		here = vml(vcur_lin);							/* Get this line.			*/

		if (type == FAT_VERTICAL) vprint("  ");					/* Output spaces in fat case.		*/
		else if (is_gr(here,vcur_col))						/* We are crossing something.		*/
		{
			n0 = j0+(i*k0);							/* Calc actual screen row		*/

			if (vcur_lin > 0)  upper = vml(vcur_lin-1);			/* Get array position of upper line	*/
			else		   upper = -1;

			if (vcur_lin < MAX_LINES_PER_SCREEN-1) lower = vml(vcur_lin+1);	/* Get lower line			*/
			else		    lower = -1;

			cvalue = 16;							/* Table char value is 16		*/
			if (((k0 == -1) && (i < (length-1))) ||
				(upper != -1) && is_gr(upper,vcur_col))			/* Hit by a line from above		*/
			{
				tchar = vchr_map[upper][vcur_col];			/* Get the character map element.	*/
				if (((k0 == -1) && (i < (length-1))) ||
					strchr(klntuwx,tchar))				/* The upper char is a descender.	*/
				{
					cvalue += 1;					/* Flag it				*/
				}
			}
			if ((vcur_col < MAX_COLUMNS_PER_LINE+1) && is_gr(here,vcur_col+1))
			{		 						/* Hit by a line from the right?	*/
				tchar = vchr_map[here][vcur_col+1];			/* Get the character map element.	*/
				if (strchr(jknuvwq,tchar))				/* The right char is a connector.	*/
				{
					cvalue += 2;					/* Flag it				*/
				}
			}

			if (((k0 == 1) && (i < (length-1))) ||
				((lower != -1) && is_gr(lower,n0)))			/* Hit by a line from below		*/
			{
				tchar = vchr_map[lower][n0];				/* Get the character map element.	*/
				if (((k0 == 1) && (i < (length-1))) ||
					strchr(jmntuvx,tchar))				/* The lower char is an ascender.	*/
				{
					cvalue += 4;					/* Flag it				*/
				}
			}
			if ((vcur_col > 0) && is_gr(here,vcur_col-1))			/* Hit by a line from the left		*/
			{
				tchar = vchr_map[here][vcur_col-1];			/* Get the character map element.	*/
				if (strchr(lmntvwq,tchar))				/* The left char is a connector.	*/
				{
					cvalue += 8;					/* Flag it.				*/
				}
			}
			cross = ctable[cvalue];						/* Get the character.			*/
			vputc(cross);							/* Print it out.			*/
		}
		else vputc(x);								/* Otherwise, just drawing a line.	*/
	}

	vcharset(cs_save);								/* Restore the old character set.	*/
	vmode(md_save);									/* Restore the old mode.		*/
	vmove(end_line,m0);								/* Move to the end of the line.		*/
	vbuffering(AUTOMATIC);								/* Dump the buffer as appropriate.	*/
	if (vlin_op) return(OPTIMIZED);							/* Return optimized if we did.		*/
	vlin_op = TRUE;									/* Line optimization on.		*/
	return(SUCCESS);								/* Return just success.			*/
}
/*					Subroutine to draw horizontal lines.							*/

static int do_horiz(type, length, end_col) int type, length, end_col;			/* Draw a horizontal line.		*/
{
	int md_save, cs_save;								/* Save locations.			*/
	unsigned char tchar;								/* A temp string.			*/
	unsigned char cross;								/* The crossing character.		*/
	int  upper, lower, here, cvalue;						/* work vars				*/
	register int i, j0, k0, m0, n0;							/* Working registers.			*/

	vbuffering(LOGICAL);								/* Turn on logical buffering.		*/

	j0 = vcur_col;									/* Assume we start at current column.	*/
	m0 = vcur_lin;
	k0 = 1;										/* Going in positive direction.		*/

	if (end_col < vcur_col)								/* Where to start?			*/
	{
		if (vlin_op) j0 = end_col;						/* If optimizing, start at end of line.	*/
		else k0 = -1;								/* No, then go in reverse direction.	*/
		length = -length;							/* Get absolute value of length.	*/
	}

	cs_save = vchr_set;								/* Remember the old character set.	*/
	md_save = vcur_atr;
	if (type == FAT_HORIZONTAL) vmode(REVERSE | (vcur_atr & BOLD));			/* Remove all rendition except bold.	*/
	else vcharset(GRAPHICS);							/* Or go into graphics mode.		*/

	if (vcur_lin > 0)  upper = vml(vcur_lin-1);					/* Get array position of upper line.	*/
	else		   upper = -1;

	if (vcur_lin < MAX_LINES_PER_SCREEN-1) lower = vml(vcur_lin+1);			/* Get lower line.			*/
	else		    lower = -1;

	here = vml(vcur_lin);								/* Get this line.			*/

	for (i = 0; i < length; i++)							/* Loop...				*/
	{
		n0 = j0+(i*k0);								/* Calculate screen column.		*/
		vmove(m0,n0);								/* Move to where this element starts.	*/
		if (type == FAT_HORIZONTAL) vprint(" ");				/* Print spaces for fat line.		*/
		else if (is_gr(here,n0))						/* We are crossing something.		*/
		{
			cvalue = 0;							/* Table character value is null.	*/
			if ((upper != -1) && is_gr(upper,n0))				/* Hit by a line from above?		*/
			{
				tchar = vchr_map[upper][n0];				/* Get the character map element.	*/
				if (strchr(klntuwx,tchar))				/* The upper char is a descender.	*/
				{
					cvalue += 1;					/* Flag it.				*/
				}
			}
			if (((k0 == 1) && (i < (length-1))) || 				/* Hit by a line from the right?	*/
			((n0 < MAX_COLUMNS_PER_LINE+1) && is_gr(here,n0+1)))
			{
				tchar = vchr_map[here][n0+1];				/* Get the character map element.	*/
				if (((k0 == 1) && (i < (length-1))) ||
						 strchr(jknuvwq,tchar))			/* The right char is a connector.	*/
				{
					cvalue += 2;					/* Flag it.				*/
				}
			}
			if ((lower != -1) && is_gr(lower,n0))				/* Hit by a line from below?		*/
			{
				tchar = vchr_map[lower][n0];				/* Get the character map element.	*/
				if (strchr(jmntuvx,tchar))				/* The lower char is an ascender.	*/
				{
					cvalue += 4;					/* Flag it.				*/
				}
			}
			if (((k0 == -1) && (i < (length-1))) ||
				((n0 > 0) && is_gr(here,n0-1)))				/* Hit by a line from the left?		*/
			{
				tchar = vchr_map[here][n0-1];				/* Get the character map element.	*/
				if (((k0 == -1) && (i < (length-1))) ||
					strchr(lmntvwq,tchar))				/* The left char is a connector.	*/
				{
					cvalue += 8;					/* Flag it.				*/
				}
			}
			cross = ctable[cvalue];						/* Get the character.			*/
			if (cross != q)							/* Make sure it's special.		*/
			{
				vmove(m0,n0);						/* Move to where this element starts.	*/
				vputc(cross);						/* Print it out.			*/
			}
			else vputc(q);
		}
		else vputc(q);								/* Else we are just drawing a line.	*/
	}

	vcharset(cs_save);								/* Restore the old character set.	*/
	vmode(md_save);									/* Restore old mode.			*/
	vmove(m0,end_col);								/* Move to the end of the line.		*/
	vbuffering(AUTOMATIC);								/* Restore automatic buffer dumpint.	*/
	if (vlin_op) return(OPTIMIZED);							/* Return optimized if we did.		*/
	vlin_op = TRUE;									/* Line optimization on.		*/
	return(SUCCESS);								/* Return just success.			*/
}

static int is_gr(row,col) int row,col;				/* Routine to see if a position is a line drawing character.	*/
{
	unsigned char tchar;

	if (vmap_cng[row][col] < 0) return(FALSE);					/* Is it NOT old data?			*/
	if (!(vatr_map[row][col] & GRAPHICS)) return(FALSE);				/* And if new, is it a graphics char?	*/
	tchar = vchr_map[row][col];
	if (0==strchr(vcapdef[GRAPHSTR],tchar)) return(FALSE);				/* Is it in the graphics list?		*/
	return(SUCCESS);								/* It is a graphics character.		*/
}

