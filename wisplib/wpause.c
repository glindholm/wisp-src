/* Simulate the Wang PAUSE routine.												*/

#include "idsistd.h"
#include "movebin.h"
#include "werrlog.h"

wpause(hsec)									/* Pause program for hsec (1/100) seconds.	*/
int4 *hsec;
{
#define		ROUTINE		82000

	int4	ltime;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);					/* Log the entry.				*/

	GETBIN(&ltime,hsec,4);							/* Align and put into local copy.		*/
	wswap(&ltime);								/* Swap the word order for VMS.			*/
	vwait(0,0,0,(int)ltime);						/* Perform the wait.				*/
}
