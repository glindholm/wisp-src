/************************************************************************/
/*									*/
/*	     VIDEO - Video Interactive Development Environment		*/
/*									*/
/*			    Copyright (c) 1987				*/
/*									*/
/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <video.h>
#include <vlocal.h>			/* Normal user wouldn't do this.*/
#include <vmodules.h>

static int testo1();

testo()
{
	testo1();
	return(SUCCESS);
}

static int testo1()
{
	extern char vchr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];
	register int i;

	vmove(0,0);
	for (i = 0; i < 100; i++)
	{
vprint("%d ..... ////// ..... ////// ..... %d ..... ////// ..... ////// ..... %d\n",i,i,i);
	}
	return(SUCCESS);
}
