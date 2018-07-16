/************************************************************************/
/*           VIDEO - Video Interactive Development Environment          */
/*                          Copyright (c) 1987                          */
/*      An unpublished work by Greg L. Adams.  All rights reserved.     */
/************************************************************************/

#include <stdio.h>
#include <v/video.h>
#include <v/vlocal.h>
#include <v/vdata.h>

testw()
{
	unsigned char *vsss(), *remember;
	register int i,j;
	int irow = 3;
	int icol = 30;
	int irows = 7;
	int icols = 40;

	remember = vsss(irow,icol,irows,icols);

	vbuffering(LOGICAL);

	vmode(REVERSE);
	vmove(irow,icol);
	vline(HORIZONTAL,icols);
	vmove(irow,icol+icols-1);
	vline(VERTICAL,irows);
	vmove(irow,icol);
	vline(VERTICAL,irows);
	vmove(irow+irows-1,icol);
	vline(HORIZONTAL,icols);

	for (i = irow+1; i < irow+irows-1; i++)
	{
		vmove(i,icol+1);
		for (j = icol+1; j < icol+icols-1; j++) vputc(' ');
	}

	vmove(irow+1,icol+2);
	vprint("Screen segment saved %o.",remember);

	vmove(irow+irows-2,icol+2);
	vprint("Depress any key to continue...");
	vgetc();

	vmode(CLEAR);

	vrss(remember);
	vtext(0,23,0,"Depress any key to continue... ");
	vgetc();
	return(SUCCESS);
}
