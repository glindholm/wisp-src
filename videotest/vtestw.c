/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

#include <stdio.h>
#include <video.h>
#include <vlocal.h>
#include <vdata.h>
#include <vmodules.h>
int testw()
{
	unsigned char *remember;
	register int i,j;
	int irow = 3;
	int icol = 30;
	int irows = 7;
	int icols = 40;

	remember = vsss(irow,icol,irows,icols);

	VL_vbuffering_start();

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

	VL_vbuffering_end();
	
	vmove(irow+irows-2,icol+2);
	vprint("Depress any key to continue...");
	vgetc();

	vmode(CLEAR);

	vrss(remember);
	vtext(0,23,0,"Depress any key to continue... ");
	vgetc();
	return(SUCCESS);
}
