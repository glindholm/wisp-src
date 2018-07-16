#include <string.h>
#ifndef VMS	/* unix or MSDOS */
#include <memory.h>
#endif
#include "movebin.h"
#include "werrlog.h"

DAY(date,dow)									/* Return the day of the week for a given date.	*/
char *date;									/* The date YYMMDD				*/
long *dow;									/* The day of the week. 1=sunday		*/
{
#define		ROUTINE		13000

	long tmp;
	extern	long	dates_calc();
	char date_passed[7];

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);					/* Say we're here.				*/

	memset(date_passed,0,sizeof(date_passed));	
	memcpy(date_passed,date,6);
	valid_date("010106",1);							/* Jan 06, 1901 is a Sunday			*/
	if (valid_date(date_passed,2)<0)
	{
		werrlog(ERRORCODE(2),date_passed,0,0,0,0,0,0,0);		/* Bad date passed.				*/
	}
	else
	{
		tmp = (dates_calc() % 7)+1;
		wswap(&tmp);
		PUTBIN(dow,&tmp,sizeof(long));					/* Copy the day of week value			*/
	}
}
