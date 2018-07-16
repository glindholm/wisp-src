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
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			 Copyright (c) 1987-1991			*/
			/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
			/************************************************************************/


/*						Include standard header files.							*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include local definitions.		*/
#include "vmodules.h"
#include "vdata.h"
#include "vcap.h"


/*						Global and static data.								*/

static unsigned char x, q, l, k, m, j, w, v, t, u, n;					/* ANSI DEC like short form ints.	*/
static unsigned char ctable[132];							/* Map of crossing characters.		*/
static unsigned char klntuwx[8];
static unsigned char jknuvwq[8];
static unsigned char jmntuvx[8];
static unsigned char lmntvwq[8];

static int do_vert(int type, int length, int end_line);					/* Draw vertical line.			*/
static int do_horiz(int type, int length, int end_col);					/* Draw a horizontal line.		*/
static int is_gr(int row, int col);				/* Routine to see if a position is a line drawing character.	*/


/*						Subroutine entry point.								*/

int VL_vline(int type, int length)								/* Draw a line.				*/
{
	static int first = 1;
	register int ret;								/* Working registers.			*/

	if (first)
	{
		char graphstr[20];
		
		first = 0;
		
		strcpy(graphstr,VL_vcapvalue(GRAPHSTR));
		
		x = graphstr[SINGLE_VERTICAL_BAR];
		q = graphstr[SINGLE_HORIZONTAL_BAR];
		l = graphstr[SINGLE_UPPER_LEFT_CORNER];
		k = graphstr[SINGLE_UPPER_RIGHT_CORNER];
		m = graphstr[SINGLE_LOWER_LEFT_CORNER];
		j = graphstr[SINGLE_LOWER_RIGHT_CORNER];
		w = graphstr[SINGLE_UPPER_TEE];
		v = graphstr[SINGLE_LOWER_TEE];
		t = graphstr[SINGLE_LEFT_TEE];
		u = graphstr[SINGLE_RIGHT_TEE];
		n = graphstr[SINGLE_CROSS];

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
	}
	

	ret = SUCCESS;									/* Assume all will be successful.	*/
	switch(type)									/* Determine the type of line.		*/
	{
		case VLINE_VERTICAL: case VLINE_FAT_VERTICAL:				/* Vertical lines?			*/
		{
			int	end_line;
			
			end_line = vcur_lin + length;					/* Determine end position.		*/
			if (length < 0)	end_line++;					/* Adjust for direction.		*/
			else end_line--;
			if ((end_line < 0) || (end_line >= MAX_LINES_PER_SCREEN))	/* End position valid?			*/
			{
				vre("vline(%d,%d)-Length too long, drawing from line %d.",type,length,vcur_lin);
				ret = FAILURE;
			}
			else if (length) 
			{
				ret = do_vert(type,length,end_line);			/* Draw the line if not zero length	*/
			}
			break;								/* All done.				*/
		}
		case VLINE_HORIZONTAL: case VLINE_FAT_HORIZONTAL:
		{
			int	end_col;
			
			end_col = vcur_col + length;					/* Determine end position of line.	*/
			if (length < 0)	end_col++;					/* Adjust for direction.		*/
			else end_col--;
			if ((end_col < 0) || (end_col >= VL_vscr_wid))			/* Still on the screen?			*/
			{
				vre("vline(%d,%d)-Length too long, drawing from line %d.",type,length,vcur_lin);
				ret = FAILURE;
			}
			else if (length) 
			{
				ret = do_horiz(type, length, end_col);			/* Draw the line if not zero length	*/
			}
			
			break;								/* Don't drop through.			*/
		}
		default:								/* Hmmm, he doesn't know what he wants.	*/
		{
			vre("vline()-Invalid line type=%d.", type);			/* Report the condition.		*/
			ret = FAILURE;							/* Return with failure.			*/
		}
	}
	return(ret);									/* Return to the caller.		*/
}

/*				Subroutine to draw vertical lines.								*/

static int do_vert(int type, int length, int end_line)					/* Draw vertical line.			*/
{
	int cs_save, md_save;								/* Save locations.			*/
	unsigned char tchar;								/* A temp string.			*/
	unsigned char cross;								/* The crossing character.		*/
	int  upper, lower, here, cvalue;						/* work vars				*/
	register int i, j0, k0, m0, n0;							/* Working registers.			*/

	VL_vbuffering_start();							/* Turn on logical buffering.		*/

	j0 = vcur_lin;									/* Assume we start at current line.	*/
	m0 = vcur_col;
	k0 = 1;										/* Going in positive direction.		*/

	if (end_line < vcur_lin)							/* Where to start?			*/
	{
		if (VL_vlin_op) j0 = end_line;						/* If optimizing, start at end of line.	*/
		else k0 = -1;								/* No, then go in reverse direction.	*/
		length = -length;							/* Get absolute value of length.	*/
	}

	cs_save = vchr_set;								/* Remember the old character set.	*/
	md_save = vcur_atr;								/* Remember the old rendition.		*/
	if (type == VLINE_FAT_VERTICAL) VL_vmode(VMODE_REVERSE | (vcur_atr & VMODE_BOLD));	/* Remove all rendition except bold.	*/
	else vcharset(GRAPHICS);							/* Or go into graphics mode.		*/

	for (i = 0; i < length; i++)							/* Loop...				*/
	{
		vmove(j0+(i*k0),m0);							/* Move to where this element starts.	*/
		here = vml(vcur_lin);							/* Get this line.			*/

		if (type == VLINE_FAT_VERTICAL) vprint("  ");				/* Output spaces in fat case.		*/
		else if (is_gr(here,vcur_col))						/* We are crossing something.		*/
		{
			n0 = j0+(i*k0);							/* Calc actual screen row		*/

			if (vcur_lin > 0)  upper = vml(vcur_lin-1);			/* Get array position of upper line	*/
			else		   upper = -1;

			if (vcur_lin < MAX_LINES_PER_SCREEN-1) lower = vml(vcur_lin+1);	/* Get lower line			*/
			else		    lower = -1;

			cvalue = 16;							/* Table char value is 16		*/
			if (((k0 == -1) && (i < (length-1))) ||
			   ((upper != -1) && is_gr(upper,vcur_col)))			/* Hit by a line from above		*/
			{
				tchar = vchr_map[upper][vcur_col];			/* Get the character map element.	*/
				if (((k0 == -1) && (i < (length-1))) ||
					strchr((char *)klntuwx,(char)tchar))		/* The upper char is a descender.	*/
				{
					cvalue += 1;					/* Flag it				*/
				}
			}
			if ((vcur_col < MAX_COLUMNS_PER_LINE+1) && is_gr(here,vcur_col+1))
			{		 						/* Hit by a line from the right?	*/
				tchar = vchr_map[here][vcur_col+1];			/* Get the character map element.	*/
				if (strchr((char *)jknuvwq,(char)tchar))		/* The right char is a connector.	*/
				{
					cvalue += 2;					/* Flag it				*/
				}
			}
			else if ((vcur_col == 0) && (vchr_map[here][0] == q)) cvalue += 2;	/* Special left case.		*/

			if (((k0 == 1) && (i < (length-1))) ||
				((lower != -1) && is_gr(lower,n0)))			/* Hit by a line from below		*/
			{
				tchar = vchr_map[lower][n0];				/* Get the character map element.	*/
				if (((k0 == 1) && (i < (length-1))) ||
					strchr((char *)jmntuvx,(char)tchar))		/* The lower char is an ascender.	*/
				{
					cvalue += 4;					/* Flag it				*/
				}
			}
			if ((vcur_col > 0) && is_gr(here,vcur_col-1))			/* Hit by a line from the left		*/
			{
				tchar = vchr_map[here][vcur_col-1];			/* Get the character map element.	*/
				if (strchr((char *)lmntvwq,(char)tchar))		/* The left char is a connector.	*/
				{
					cvalue += 8;					/* Flag it.				*/
				}
			}
			else if ((vchr_map[here][vcur_col] == q) && (vcur_col >= VL_vscr_wid-2)) cvalue += 8;	/* Edge?	*/

			cross = ctable[cvalue];						/* Get the character.			*/
			vputc(cross);							/* Print it out.			*/
		}
		else vputc(x);								/* Otherwise, just drawing a line.	*/
	}

	vcharset(cs_save);								/* Restore the old character set.	*/
	VL_vmode(md_save);									/* Restore the old mode.		*/
	vmove(end_line,m0);								/* Move to the end of the line.		*/
	VL_vbuffering_end();								/* Dump the buffer as appropriate.	*/
	if (VL_vlin_op) return(OPTIMIZED);							/* Return optimized if we did.		*/
	VL_vlin_op = TRUE;									/* Line optimization on.		*/
	return(SUCCESS);								/* Return just success.			*/
}
/*					Subroutine to draw horizontal lines.							*/

static int do_horiz(int type, int length, int end_col)					/* Draw a horizontal line.		*/
{
	int md_save, cs_save;								/* Save locations.			*/
	unsigned char tchar;								/* A temp string.			*/
	unsigned char cross;								/* The crossing character.		*/
	int  upper, lower, here, cvalue;						/* work vars				*/
	register int i, j0, k0, m0, n0;							/* Working registers.			*/

	VL_vbuffering_start();							/* Turn on logical buffering.		*/

	j0 = vcur_col;									/* Assume we start at current column.	*/
	m0 = vcur_lin;
	k0 = 1;										/* Going in positive direction.		*/

	if (end_col < vcur_col)								/* Where to start?			*/
	{
		if (VL_vlin_op) j0 = end_col;						/* If optimizing, start at end of line.	*/
		else k0 = -1;								/* No, then go in reverse direction.	*/
		length = -length;							/* Get absolute value of length.	*/
	}

	cs_save = vchr_set;								/* Remember the old character set.	*/
	md_save = vcur_atr;
	if (type == VLINE_FAT_HORIZONTAL) VL_vmode(VMODE_REVERSE | (vcur_atr & VMODE_BOLD));/* Remove all rendition except bold.	*/
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
		if (type == VLINE_FAT_HORIZONTAL) vprint(" ");				/* Print spaces for fat line.		*/
		else if (is_gr(here,n0))						/* We are crossing something.		*/
		{
			cvalue = 0;							/* Table character value is null.	*/
			if ((upper != -1) && is_gr(upper,n0))				/* Hit by a line from above?		*/
			{
				tchar = vchr_map[upper][n0];				/* Get the character map element.	*/
				if (strchr((char *)klntuwx,(char)tchar))		/* The upper char is a descender.	*/
				{
					cvalue += 1;					/* Flag it.				*/
				}
			}
			if (((k0 == 1) && (i < (length-1))) || 				/* Hit by a line from the right?	*/
			((n0 < MAX_COLUMNS_PER_LINE+1) && is_gr(here,n0+1)))
			{
				tchar = vchr_map[here][n0+1];				/* Get the character map element.	*/
				if (((k0 == 1) && (i < (length-1))) ||
					strchr((char *)jknuvwq,(char)tchar))		/* The right char is a connector.	*/
				{
					cvalue += 2;					/* Flag it.				*/
				}
			}
			if ((lower != -1) && is_gr(lower,n0))				/* Hit by a line from below?		*/
			{
				tchar = vchr_map[lower][n0];				/* Get the character map element.	*/
				if (strchr((char *)jmntuvx,(char)tchar))		/* The lower char is an ascender.	*/
				{
					cvalue += 4;					/* Flag it.				*/
				}
			}
			if (((k0 == -1) && (i < (length-1))) ||
				((n0 > 0) && is_gr(here,n0-1)))				/* Hit by a line from the left?		*/
			{
				tchar = vchr_map[here][n0-1];				/* Get the character map element.	*/
				if (((k0 == -1) && (i < (length-1))) ||
					strchr((char *)lmntvwq,(char)tchar))		/* The left char is a connector.	*/
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
	VL_vmode(md_save);									/* Restore old mode.			*/
	vmove(m0,end_col);								/* Move to the end of the line.		*/
	VL_vbuffering_end();								/* Restore automatic buffer dumpint.	*/
	if (VL_vlin_op) return(OPTIMIZED);							/* Return optimized if we did.		*/
	VL_vlin_op = TRUE;									/* Line optimization on.		*/
	return(SUCCESS);								/* Return just success.			*/
}

static int is_gr(int row, int col)				/* Routine to see if a position is a line drawing character.	*/
{
	unsigned char tchar;

	if (VMAP_CNG_OLDDATA == vmap_cng[row][col]) return(FALSE);			/* Is it NOT old data?			*/
	if (!(vatr_map[row][col] & GRAPHICS)) return(FALSE);				/* And if new, is it a graphics char?	*/
	tchar = vchr_map[row][col];
	if (0==strchr(VL_vcapvalue(GRAPHSTR),(char)tchar)) return(FALSE);			/* Is it in the graphics list?		*/
	return(SUCCESS);								/* It is a graphics character.		*/
}
/*
**	History:
**	$Log: vline.c,v $
**	Revision 1.18  2003/06/20 15:04:28  gsl
**	VL_ globals
**	
**	Revision 1.17  2003/02/05 15:23:59  gsl
**	Fix -Wall warnings
**	
**	Revision 1.16  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.15  2002/07/17 21:06:02  gsl
**	VL_ globals
**	
**	Revision 1.14  2002/07/16 13:40:22  gsl
**	VL_ globals
**	
**	Revision 1.13  2002/07/15 20:16:09  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.12  2002/07/15 17:52:55  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.11  1998/10/13 18:49:53  gsl
**	Change to use VMAP_CNG_OLDDATA
**	
**	Revision 1.10  1997-07-08 17:12:10-04  gsl
**	change to using VL_vcapvalue()
**	use new video.h defines
**
**	Revision 1.9  1996-03-12 08:22:50-05  gsl
**	Change to VBUFF_START/END and VLINE_xxx
**	Only initialize character table the first time in.
**
**
**
*/
