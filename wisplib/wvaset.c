/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
*/


/*

	wvaset(argcount)	This routine is called from unix before a "vararg" call to simualate the va_count on VMS.
				All it does is set WVACOUNT which is returned on the next WL_va_count() call.

*/
#include "idsistd.h"

static int WVACOUNT = 0;

void WVASET(int *x)
{
	WVACOUNT = *x;
}

void WVASETV(int4 x)	/* MF: CALL "WVASETV" USING VALUE nn */
{
	WVACOUNT = x;
}

void WL_set_va_count(int x)
{
	WVACOUNT = x;
}

int WL_va_count()
{
	return(WVACOUNT);
}

/*
**	History:
**	$Log: wvaset.c,v $
**	Revision 1.16  2003/01/31 19:08:36  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.15  2003/01/28 20:30:22  gsl
**	Change MF to use WVASETV
**	
**	Revision 1.14  2003/01/28 20:15:01  gsl
**	Change MF to use WVASETV
**	
**	Revision 1.13  2002/07/16 16:24:50  gsl
**	Globals
**	
**	Revision 1.12  2002/07/12 17:01:08  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.11  2002/07/11 20:29:20  gsl
**	Fix WL_ globals
**	
**	Revision 1.10  2002/06/21 03:10:46  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.9  1996/08/19 22:33:26  gsl
**	drcs update
**	
**
**
*/
