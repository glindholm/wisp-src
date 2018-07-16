/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
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
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/

/* Simulate the Wang PAUSE routine.												*/

#include "idsistd.h"
#include "werrlog.h"
#include "wisplib.h"
#include "vssubs.h"
#include "wispnt.h"

void PAUSE(int4* hsec)								/* Pause program for hsec (1/100) seconds.	*/
{
#define		ROUTINE		82000

	int4	ltime;

	ltime = WL_get_swap(hsec);

	wtrace("PAUSE","PAUSE", "HSEC=[%d]", ltime);
	WL_hpause(ltime);
}

#ifdef WIN32
void WL_hpause(int4 hundredths)
{
	if (hundredths > 0)
	{
		WL_hsleep(hundredths);
	}
}
#endif

#ifdef unix

#ifdef SCO
#define NO_USLEEP 1
#endif /* SCO */


#ifndef NO_USLEEP /* !NO_USLEEP (double negative) - have a usleep() so use it */

#include <unistd.h>
void WL_hpause(int4 hundredths)
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

void WL_hpause(int4 hundredths)
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
**	Revision 1.20  2003/02/17 22:07:17  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.19  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.18  2002/07/16 16:24:51  gsl
**	Globals
**	
**	Revision 1.17  2002/07/12 17:01:04  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.16  2002/07/10 21:05:35  gsl
**	Fix globals WL_ to make unique
**	
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
