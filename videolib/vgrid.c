			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1991				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

			/************************************************************************/
			/*									*/
			/*	This subroutine  will produce a grid on the  screen		*/
			/*	starting at position [irow,icol].   The  width  and		*/
			/*	length of the grid are specified by ncols and nrows.		*/
			/*	The row and column density is specified by rden and		*/
			/*	cden.    Note that if rden and cden are zero then a 		*/
			/*	"lined"  box will be generated;    if both are zero		*/
			/*	then an empty box will be built. 				*/
			/*									*/
			/*	If ncols or nrows take the line off the screen, the		*/
			/*	line will be clipped (as opposed to wrapped around).		*/
			/*									*/
			/************************************************************************/

#include "video.h"
#include "vlocal.h"
#include "vdata.h"
#include "vcap.h"

static unsigned char x, q, l, k, m, j, w, v, t, u, n;					/* ANSI DEC like short form ints.	*/

vgrid(irow,icol,nrows,ncols,rden,cden) int irow,icol,nrows,ncols,rden,cden;
{
	char string[134];
	register int i;

	x = vcapdef[GRAPHSTR][SINGLE_VERTICAL_BAR];					/* Define short forms to make it	*/
	q = vcapdef[GRAPHSTR][SINGLE_HORIZONTAL_BAR];					/*   easier to work with.		*/
	l = vcapdef[GRAPHSTR][SINGLE_UPPER_LEFT_CORNER];
	k = vcapdef[GRAPHSTR][SINGLE_UPPER_RIGHT_CORNER];
	m = vcapdef[GRAPHSTR][SINGLE_LOWER_LEFT_CORNER];
	j = vcapdef[GRAPHSTR][SINGLE_LOWER_RIGHT_CORNER];
	w = vcapdef[GRAPHSTR][SINGLE_UPPER_TEE];
	v = vcapdef[GRAPHSTR][SINGLE_LOWER_TEE];
	t = vcapdef[GRAPHSTR][SINGLE_LEFT_TEE];
	u = vcapdef[GRAPHSTR][SINGLE_RIGHT_TEE];
	n = vcapdef[GRAPHSTR][SINGLE_CROSS];

	vbuffering(LOGICAL);
	vmove(irow,icol);

	if (nrows+ncols)								/* Just a move unless we go somewhere.	*/
	{
		vcharset(GRAPHICS);

		if (rden >= nrows) rden = 0;
		if (cden >= ncols) cden = 0;

		if      (nrows == 0) vline(HORIZONTAL,ncols);
		else if (ncols == 0) vline(VERTICAL,nrows);
		else if ((nrows == 1) && (ncols == 1)) vputc(x);
		else if ((nrows == 1) && (ncols >= 2))
		{
			vgridpack(string,l,w,k,ncols,cden);
			vmove(irow,icol);
			vprint(string);
		}
		else if ((nrows == 2) && (ncols == 1))
		{
			vputc(l);
			vmove(irow+1,icol);
			vputc(m);
		}
		else
		{
			if ((rden == 1) || (cden == 0))
			{
				vline(VERTICAL,nrows);
				vmove(irow,icol+ncols-1);
				vline(VERTICAL,nrows);
			}
			else
			{
				for (i = 0; i < ncols-1; i = i + cden)
				{
					vline(VERTICAL,nrows);
					vslew(-(nrows-1),cden);
				}
				vmove(irow,icol+ncols-1);
				vline(VERTICAL,nrows);
			}
			vcharset(GRAPHICS);
			vgridpack(string,m,v,j,ncols,cden);
			vmove(irow+nrows-1,icol);
			vprint(string);
			vgridpack(string,l,w,k,ncols,cden);
			vmove(irow,icol);
			vprint(string);
			if (rden)
			{
				vgridpack(string,t,n,u,ncols,cden);
				for (i = irow+rden; i < irow+nrows-1; i = i+rden)
				{
					vmove(i,icol);
					vprint(string);
				}
			}
		}
		vcharset(DEFAULT);
	}
	vbuffering(AUTOMATIC);
	return(SUCCESS);
}

vgridpack(string,sc,ic,ec,ncols,nden)

	char string[];									/* String of up to 132 characters.	*/
	char sc;									/* The first character of the string	*/
	char ec;									/* The end character of the string	*/
	char ic;									/* Intersecting character in the string	*/
	int ncols;									/* The length of the string		*/
	int nden;									/* The spacing of intersecting chars	*/
{
	register int i;

	for (i = 0; i < 132; i++) string[i] = q;

	if (nden)
	{
		for (i = 0; i < ncols; i = i + nden) string[i] = ic;
	}

	string[ncols-1] = ec;								/* Insert the ending character.		*/
	string[0] = sc;									/* Insert the starting character	*/
	string[ncols] = CHAR_NULL;							/* Terminate with a null		*/

	return(SUCCESS);
}
