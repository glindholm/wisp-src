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


/* Routines to wait for a record to become available.										*/

/* wfwait - Wait for a hard lock to clear. The caller does not want to apply a lock, so only needs to check hard lock.		*/
/*																*/
/*	status contains the COBOL status code.											*/
/*	timer is a 32 bit binary value and contains the time-out counter in 1/100ths of a second.				*/
/*		If 0, time-out is not requested and no value is returned.							*/

#include "idsistd.h"
#include "wglobals.h"
#include "wisplib.h"

void WFWAIT(char* status, int4* timer)					/* Wait for locks to clear.				*/
{
	uint4 delay;							/* Delay period.					*/

	if (0==memcmp(status, wisp_get_hardlock(), 2))			/* A hard lock record lock				*/
	{
		delay = 10;						/* Yes, then delay for 1/10 of a second.		*/
		WL_hpause(delay);
		if (*timer > 0)						/* Is timeout tracking requested?			*/
		{
			*timer -= delay;
		}

		if (*timer < 0) 
		{
			*timer = 0;					/* Yes, then decrement the timer but not below zero.	*/
		}
	}
	else
	{
		*timer = 0;
	}

}

void SLEEPONE(void) /* Sleep one second */
{
	WL_hpause(100);
}



/*
**	History:
**	$Log: wfwait.c,v $
**	Revision 1.18  2003/03/10 22:41:59  gsl
**	Add SLEEPONE to sleep one second as a replacement for WFWAIT
**	
**	Revision 1.17  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.16  2002/10/01 18:51:23  gsl
**	rename status
**	
**	Revision 1.15  2002/07/19 22:07:14  gsl
**	Renaming cobol api routines for global uniqueness
**	
**	Revision 1.14  2002/07/10 21:05:32  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.13  2002/07/01 04:02:43  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.12  2002/06/21 03:51:02  gsl
**	Remove softlock
**	
**	Revision 1.11  2001/09/27 19:31:56  gsl
**	Change to use WL_hpause(10)
**	for 1/10 second delay
**	
**	Revision 1.10  1997-03-12 13:25:47-05  gsl
**	changed to use WIN32 define
**
**	Revision 1.9  1996-08-19 18:33:17-04  gsl
**	drcs update
**
**
**
*/
