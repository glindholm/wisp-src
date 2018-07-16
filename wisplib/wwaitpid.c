			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	wwaitpid.c
*/

#ifdef unix
#include <stdio.h>
#include <sys/types.h>

#if !defined(u3b2) && !defined(NCR32)
#include <sys/wait.h>
#endif
#include "idsistd.h"

#ifndef WEXITSTATUS
#define WIFEXITED(x)    ( !((x) & 0xff) )
/* evaluates to the low-order 8 bits of the child exit status   */
#define WEXITSTATUS(x)  (int)(WIFEXITED(x) ? (((x) >> 8) & 0xff) : -1)
/* evaluates to a non-zero value if status returned for abnormal termination */

#endif

wwaitpid(pid,rc)								/* Wait for process pid to complete		*/
int *rc,pid;

{
	int	stat_loc;

	do
	{
		wait(&stat_loc);
	} while( kill(pid,0) == 0 );

        *rc=WEXITSTATUS(stat_loc);

#ifdef DEBUG
printf("\n\r wait (stat_loc = %8x) (rc = %d)\n\r",stat_loc, *rc);
getchar();
#endif

}

#endif

