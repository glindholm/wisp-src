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

int testo()
{
	testo1();
	return(SUCCESS);
}

static int testo1()
{
	register int i;

	vmove(0,0);
	for (i = 0; i < 100; i++)
	{
vprint("%d ..... ////// ..... ////// ..... %d ..... ////// ..... ////// ..... %d\n",i,i,i);
	}
	return(SUCCESS);
}
