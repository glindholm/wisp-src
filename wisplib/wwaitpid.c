/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
*/


/*
**	wwaitpid.c
*/

#ifdef unix
#include <stdio.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/wait.h>

#include "idsistd.h"

#ifndef WEXITSTATUS
#define WIFEXITED(x)    ( !((x) & 0xff) )
/* evaluates to the low-order 8 bits of the child exit status   */
#define WEXITSTATUS(x)  (int)(WIFEXITED(x) ? (((x) >> 8) & 0xff) : -1)
/* evaluates to a non-zero value if status returned for abnormal termination */

#endif

void WL_wwaitpid(int pid, int* rc)	/* Wait for process pid to complete		*/
{
	int	stat_loc;

	do
	{
		wait(&stat_loc);
	} while( kill(pid,0) == 0 );

        *rc=WEXITSTATUS(stat_loc);

}

#endif

/*
**	History:
**	$Log: wwaitpid.c,v $
**	Revision 1.12  2003/02/04 16:02:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.11  2003/01/31 19:08:36  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.10  2002/07/12 19:10:22  gsl
**	Global unique WL_ changes
**	
**	Revision 1.9  1996/01/02 15:40:38  gsl
**	*** empty log message ***
**	
**
**
*/
