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

	GETBIN(&ltime,hsec,4);							/* Align and put into local copy.		*/
	wswap(&ltime);								/* Swap the word order.				*/

	wtrace("WPAUSE","PAUSE", "HSEC=[%d]", ltime);
	hpause(ltime);
}

#ifdef WIN32
void hpause(int4 hundredths)
{
	if (hundredths > 0)
	{
		hsleep(hundredths);
	}
}
#endif

#ifdef unix

#ifdef SCO
#define NO_USLEEP 1
#endif /* SCO */


#ifndef NO_USLEEP /* !NO_USLEEP (double negative) - have a usleep() so use it */

#include <unistd.h>
void hpause(int4 hundredths)
{
	if (hundredths >= 100) /* Greater then one second */
	{
		/*
		 *	If greater then a second then use sleep()
		 */
		sleep((unsigned int)(hundredths / 100));

		/*
		 *	Calc remaining hunderdths
		 */
		hundredths = hundredths % 100;
	}

	if (hundredths > 0)
	{
		/*
		 *	Sleep for remaining microseconds (1/1000000).
		 */
		usleep((unsigned int)hundredths * 10000U);
	}	
}

#else /* NO_USLEEP - If no usleep() then simulate it.  */

void hpause(int4 hundredths)
{
	unsigned hsec, ticks;
	
	if (hundredths <= 0) return;

	hsec = hundredths/100;								/* Convert large hundredths.		*/
	ticks = hundredths - (hsec*100);						/* Get the remainder.			*/

	if (hsec > 0) 
	{
		sleep(hsec);
	}

	if (ticks > 0)									/* Any fractions to sleep?		*/
	{
		if (ticks >= 50) sleep(1);						/* Sleep to the rounded up amount.	*/
		else sleep(0);								/* If small, use the overhead.		*/

	}
}

#endif /* NO_USLEEP */
#endif /* unix */

/*
**	History:
**	$Log: wpause.c,v $
**	Revision 1.15  2002/02/14 15:42:33  gsl
**	Merge in the SCO use of old code without usleep()
**	Part of 4.4.01 on SCO
**	
**	Revision 1.14  2001-10-03 15:01:48-04  gsl
**	SOlaris 2.5.1 doesn't define useconds_t so cast to unsigned
**
**	Revision 1.13  2001-09-27 14:12:01-04  gsl
**	Add wtrace
**
**	Revision 1.12  2001-09-27 14:06:03-04  gsl
**	ON unix use usleep() to support sleeping in 1/100 second increments
**
**	Revision 1.11  1997-12-04 18:14:02-05  gsl
**	changed to wispnt.h
**
**	Revision 1.10  1996-08-19 18:33:19-04  gsl
**	drcs update
**
**
**
*/
