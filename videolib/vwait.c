static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

/*
**	ROUTINE:	vwait()
**
**	FUNCTION:	Sleep for a while
**
**	DESCRIPTION:	The seconds and hundredths are added together and thats 
**			how long to sleep for.
**
**	ARGUMENTS:	
**	seconds		Number of seconds to sleep
**	hundredths	Number of hundredths of seconds to sleep
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	Time is not exact.
**
*/


#ifdef VMS
#include <descrip.h>
void vwait(int seconds, int hundredths)
{
	long int binary_time[2];							/* Binary value for VMS timed waits.	*/
	int hsec, ticks;								/* Integer hundredth's of a second.	*/

	hsec = hundredths/100;								/* Convert large hundredths.		*/
	ticks = hundredths - (hsec*100);						/* Get the remainder.			*/

	if (seconds+hsec) sleep(seconds+hsec);						/* Sleep away the seconds.		*/

	if (ticks)									/* Any fractions to sleep?		*/
	{
		binary_time[0] = ticks * (-100000);		 			/* Multiply by 1/100 to get total wait.	*/
		binary_time[1] = 0xffffffff;						/* Fill in the high order bits.		*/
		sys$schdwk(0,0,binary_time,0);						/* Schedule a wakeup.			*/
		sys$hiber();								/* Hibernate if we will wake up.	*/
	}
}
#endif /* VMS */

#if defined(unix) || defined(MSDOS)
extern unsigned sleep(unsigned secs);

void vwait(int seconds, int hundredths)
{
	unsigned sleep_secs;
	
	sleep_secs = seconds + (hundredths + 50)/100;

	while(sleep_secs > 0)
	{
		sleep_secs = sleep(sleep_secs);
	}
}
#endif /* unix || MSDOS */

#ifdef WIN32
#include <windows.h>

void vwait(int seconds, int hundredths)
{
	Sleep(seconds * 1000 + hundredths * 10);
}
#endif


/*
**	History:
**	$Log: vwait.c,v $
**	Revision 1.10  1997-07-09 12:39:12-04  gsl
**	CHange interface to use only two args (secs, hundredths)
**	Rewrite for WIN32 so accurate.
**
**	Revision 1.9  1996-10-11 18:16:24-04  gsl
**	drcs update
**
**
**
*/
