			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/* Routines to wait for a record to become available.										*/

/* wfwait - Wait for a hard lock to clear. The caller does not want to apply a lock, so only needs to check hard lock.		*/
/*																*/
/*	stat contains the COBOL status code.											*/
/*	timer is a 32 bit binary value and contains the time-out counter in 1/100ths of a second.				*/
/*		If 0, time-out is not requested and no value is returned.							*/

#include "idsistd.h"
#include "wglobals.h"

void wfwait(stat,timer) char *stat; int4 *timer;				/* Wait for locks to clear.				*/
{
	uint4 delay, delaysec;					/* Delay period.					*/

	if (stat[0] == hardlock[0] && 
	    stat[1] == hardlock[1]    ) 				/* A hard lock record lock				*/
	{
#ifdef VMS
		delay = 20;						/* Yes, then delay for 1/5 of a second.			*/
		wswap(&delay);						/* Pre-swap because wpause() will swap back.		*/
		wpause(&delay);						/* Now do the wait.					*/
		wswap(&delay);						/* Un-swap 						*/
#endif
#ifdef unix
		delaysec = 1;						/* Under UNIX, wait 1 second.				*/
		sleep(delaysec);					/* zzzzzzzzzzz.						*/
		delay = delaysec * 100;					/* Return delay in 1/100ths of a sec.			*/
#endif
#ifdef MSDOS
		delay = 20;						/* Yes, then delay for 1/5 of a second.			*/
		wswap(&delay);						/* Pre-swap because wpause() will swap back.		*/
		wpause(&delay);						/* Now do the wait.					*/
		wswap(&delay);						/* Un-swap 						*/
#endif
	}
	else
	{
		*timer = 0;
		return;		
	}

	if (*timer > 0)							/* Is timeout tracking requested?			*/
	{
		if ((*timer = (*timer - delay)) < 0) *timer = 0;	/* Yes, then decrement the timer but not below zero.	*/
	}
}

/* wfswait - Wait for a soft or hard lock to clear. The caller is interested in applying a lock, and needs the record free.	*/

void wfswait(stat, timer) char *stat; int4 *timer;
{
	uint4 delay, delaysec;					/* Delay period.					*/

	if ( (stat[0] == hardlock[0] && stat[1] == hardlock[1]) ||	/* A hard lock record lock or				*/
	     (stat[0] == softlock[0] && stat[1] == softlock[1])    )	/* a soft lock record lock				*/
	{
#ifdef VMS
		delay = 20;						/* Yes, then delay for 1/5 of a second.			*/
		wswap(&delay);						/* Pre-swap because wpause() will swap back.		*/
		wpause(&delay);						/* Now do the wait.					*/
		wswap(&delay);						/* Un-swap 						*/
#endif
#ifdef unix
		delaysec = 1;						/* Under UNIX, wait 1 second.				*/
		sleep(delaysec);					/* zzzzzzzzzzz.						*/
		delay = delaysec * 100;					/* Return delay in 1/100ths of a sec.			*/
#endif
#ifdef MSDOS
		delay = 20;						/* Yes, then delay for 1/5 of a second.			*/
		wswap(&delay);						/* Pre-swap because wpause() will swap back.		*/
		wpause(&delay);						/* Now do the wait.					*/
		wswap(&delay);						/* Un-swap 						*/
#endif
	}
	else
	{
		*timer = 0;
		return;		
	}

	if (*timer > 0)							/* Is timeout tracking requested?			*/
	{
		if ((*timer = (*timer - delay)) < 0) *timer = 0;	/* Yes, then decrement the timer but not below zero.	*/
	}
}
