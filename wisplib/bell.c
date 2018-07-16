#include "movebin.h"
#include "werrlog.h"

#ifdef	BELL
#undef	BELL
#endif

BELL(bell_count)								/* WANG BELL function				*/
long	*bell_count;
{
#define		ROUTINE		2000

	long	l_times;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);					/* Log the entry.				*/

	GETBIN(&l_times,bell_count,4);						/* Align int.					*/
	wswap(&l_times);							/* Swap the word order for VMS.			*/

	while( l_times > 0 )
	{
		vbell();							/* Ring it bell_count times.			*/
		l_times--;
	}
}
