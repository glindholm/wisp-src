/*----
Various general purpose getparm routines
------*/
#define	_GP_C
#include "vseglb.h"

/*----
Initialize 4 byte integers 0-255 for getparm processing
------*/
init_gpint()
{
	int idx;

	if(GPINT[1])
		return;

	for(idx = 0; idx < 255; ++idx)
		{
		GPINT[idx] = idx;
		wswap(&GPINT[idx]);
		}
}

/*----
Display and read a getparm, and return the translated PF code
------*/
long display_and_read_gp()
{
	static long two = 2;
	long gppf_tran();

	wvaset(&two);
	GETPARM(&gparg,&gpcnt);
	return(gppfkey = gppf_tran(gppfrcvr[0]));
}

/*----
Translate the GP codes '@' 'A' etc into values
0-32
------*/
static long gppf_tran(gppf)
int gppf;
{
	if(( gppf <= 'P') && (gppf >= '@'))
		return(gppf - '@');
	if((gppf <= 'p') && (gppf >= 'a'))
		return( (gppf - 'a') + 17);
	return(0);
}
 
