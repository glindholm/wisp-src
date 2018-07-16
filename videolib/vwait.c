/* Perform a timed wait.													*/

#ifdef VMS
#include <descrip.h>
#endif

#include "video.h"

int vwait(hours,minutes,seconds,hundredths) int hours, minutes, seconds, hundredths;
{
#ifdef VMS
	long int binary_time[2];							/* Binary value for VMS timed waits.	*/
#endif
	int hsec, ticks;								/* Integer hundredth's of a second.	*/
	hsec = hundredths/100;								/* Convert large hundredths.		*/
	ticks = hundredths - (hsec*100);						/* Get the remainder.			*/
	if (hours+minutes+seconds+hsec) sleep((hours*60*60)+(minutes*60)+seconds+hsec);	/* Sleep away the hours.		*/
	if (ticks)									/* Any fractions to sleep?		*/
#ifdef VMS
	{
		binary_time[0] = ticks * (-100000);		 			/* Multiply by 1/100 to get total wait.	*/
		binary_time[1] = 0xffffffff;						/* Fill in the high order bits.		*/
		sys$schdwk(0,0,binary_time,0);						/* Schedule a wakeup.			*/
		sys$hiber();								/* Hibernate if we will wake up.	*/
	}
#else
	{
		if (ticks >= 50) sleep(1);						/* Sleep to the rounded up amount.	*/
		else sleep(0);								/* If small, use the overhead.		*/
	}
#endif
}
