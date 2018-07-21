/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
**
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
** CVS
**
**
**
**
******************************************************************************
*/

/*
**	File:		wretcode.c
**
**	Project:	wisp/utils
**
**
**
**	Purpose:	called by shell to get 3 digit status code back from COBOL
**
**	Routines:	
*/

#ifdef unix

/*
**	Includes
*/

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>

#include "wisplib.h"
#include "vssubs.h"

#include "filext.h"

int main()
{
	int retval;
	char mtxt[4];
	
	RETCODE(mtxt);
	if (mtxt[0]==0&&mtxt[1]==0&&mtxt[2]==0) retval=0;
	else
	  if (mtxt[0]==' '&&mtxt[1]==' '&&mtxt[2]==' ') retval=0;
	else
	  retval = (mtxt[0]-0x30) * 100 + (mtxt[1]-0x30) * 10 + (mtxt[2]-0x30);
	printf("%d",retval);	
	return 0;
}

#include "wutils.h"

#endif


/*
**	History:
**	$Log: wretcode.c,v $
**	Revision 1.12  2003/02/04 20:42:49  gsl
**	fix -Wall warnings
**	
**	Revision 1.11  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.10  2002/06/25 18:18:36  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.9  1996/07/26 17:49:37  gsl
**	Fix undefined
**	
**	Revision 1.8  1995-04-25 02:58:58-07  gsl
**	drcs state V3_3_15
**
 * Revision 1.7  1995/04/17  11:51:20  gsl
 * drcs state V3_3_14
 *
 * Revision 1.6  1995/04/07  15:22:13  gsl
 * remove the filext.h -- was the wrong solution.
 *
 * Revision 1.5  1995/04/07  14:34:53  gsl
 * add include of filext.h
 *
**
**
*/
