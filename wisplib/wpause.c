static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/* Simulate the Wang PAUSE routine.												*/

#include "idsistd.h"
#include "movebin.h"
#include "werrlog.h"
#include "wisplib.h"
#include "wispnt.h"

void wpause(int4* hsec)								/* Pause program for hsec (1/100) seconds.	*/
{
#define		ROUTINE		82000

	int4	ltime;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);					/* Log the entry.				*/

	GETBIN(&ltime,hsec,4);							/* Align and put into local copy.		*/
	wswap(&ltime);								/* Swap the word order for VMS.			*/
	hpause(ltime);
}

#ifdef VMS
#include <descrip.h>
#endif

void hpause(int4 hundredths)
{
	unsigned hsec, ticks;
	
	if (hundredths <= 0) return;

	hsec = hundredths/100;								/* Convert large hundredths.		*/
	ticks = hundredths - (hsec*100);						/* Get the remainder.			*/

#ifdef WIN32
	hsleep(hundredths);
#endif

#if defined(unix) || defined(VMS) || defined(MSDOS)
	if (hsec > 0) 
	{
		sleep(hsec);
	}

	if (ticks > 0)									/* Any fractions to sleep?		*/
	{
#ifdef VMS
		long int binary_time[2];							/* Binary value for VMS timed waits.	*/
		binary_time[0] = ticks * (-100000);		 			/* Multiply by 1/100 to get total wait.	*/
		binary_time[1] = 0xffffffff;						/* Fill in the high order bits.		*/
		sys$schdwk(0,0,binary_time,0);						/* Schedule a wakeup.			*/
		sys$hiber();								/* Hibernate if we will wake up.	*/
#else
		if (ticks >= 50) sleep(1);						/* Sleep to the rounded up amount.	*/
		else sleep(0);								/* If small, use the overhead.		*/
#endif
	}
#endif /* unix || VMS || MSDOS */

}
/*
**	History:
**	$Log: wpause.c,v $
**	Revision 1.11  1997-12-04 18:14:02-05  gsl
**	changed to wispnt.h
**
**	Revision 1.10  1996-08-19 18:33:19-04  gsl
**	drcs update
**
**
**
*/
