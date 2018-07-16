static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

/*
**	wwaitpid.c
*/

#ifdef unix
#include <stdio.h>
#include <sys/types.h>

#if !defined(ATT3B2) && !defined(NCR32)
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

#ifdef TESTING
printf("\n\r wait (stat_loc = %8x) (rc = %d)\n\r",stat_loc, *rc);
getchar();
#endif

}

#endif

/*
**	History:
**	$Log: wwaitpid.c,v $
**	Revision 1.9  1996/01/02 15:40:38  gsl
**	*** empty log message ***
**	
**
**
*/
