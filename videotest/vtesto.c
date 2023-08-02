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
