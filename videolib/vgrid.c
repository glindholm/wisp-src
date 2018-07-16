static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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

#include <string.h>

#include "video.h"
#include "vlocal.h"
#include "vdata.h"
#include "vcap.h"
#include "vmodules.h"

static void vgridpack();

static unsigned char x, q, l, k, m, j, w, v, t, u, n;					/* ANSI DEC like short form ints.	*/

vgrid(irow,icol,nrows,ncols,rden,cden) int irow,icol,nrows,ncols,rden,cden;
{
	char string[134];
	register int i;
	char graphstr[20];
	
	strcpy(graphstr,vcapvalue(GRAPHSTR));

	x = graphstr[SINGLE_VERTICAL_BAR];					/* Define short forms to make it	*/
	q = graphstr[SINGLE_HORIZONTAL_BAR];					/*   easier to work with.		*/
	l = graphstr[SINGLE_UPPER_LEFT_CORNER];
	k = graphstr[SINGLE_UPPER_RIGHT_CORNER];
	m = graphstr[SINGLE_LOWER_LEFT_CORNER];
	j = graphstr[SINGLE_LOWER_RIGHT_CORNER];
	w = graphstr[SINGLE_UPPER_TEE];
	v = graphstr[SINGLE_LOWER_TEE];
	t = graphstr[SINGLE_LEFT_TEE];
	u = graphstr[SINGLE_RIGHT_TEE];
	n = graphstr[SINGLE_CROSS];

	vbuffering_start();
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
	vbuffering_end();
	return(SUCCESS);
}

static void vgridpack(string,sc,ic,ec,ncols,nden)

	char string[];									/* String of up to 132 characters.	*/
	char sc;									/* The first character of the string	*/
	char ec;									/* The end character of the string	*/
	char ic;									/* Intersecting character in the string	*/
	int ncols;									/* The length of the string		*/
	int nden;									/* The spacing of intersecting chars	*/
{
	int i;

	for (i = 0; i < 132; i++) string[i] = q;

	if (nden)
	{
		for (i = 0; i < ncols; i = i + nden) string[i] = ic;
	}

	string[ncols-1] = ec;								/* Insert the ending character.		*/
	string[0] = sc;									/* Insert the starting character	*/
	string[ncols] = CHAR_NULL;							/* Terminate with a null		*/
}
/*
**	History:
**	$Log: vgrid.c,v $
**	Revision 1.10  1997-07-08 17:00:25-04  gsl
**	Change to use new video.h interfaces
**
**	Revision 1.9  1996-10-11 18:16:05-04  gsl
**	drcs update
**
**
**
*/
