/*----
Various general purpose getparm routines
------*/
#define	_GP_C
#include "idsistd.h"
#include "vseglb.h"

static int4 gppf_tran();
int4 display_and_read_gp();

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
int4 display_and_read_gp()
{
	static int4 two = 2;
	char settab;
	
	wvaset(&two);
	GETPARM(&gparg,&gpcnt);
	gppfkey = gppf_tran(gppfrcvr[0]);

	return gppfkey;

}

/*----
Translate the GP codes '@' 'A' etc into values
0-32
------*/
static int4 gppf_tran(gppf)
int gppf;
{
	if(( gppf <= 'P') && (gppf >= '@'))
		return(gppf - '@');
	if((gppf <= 'p') && (gppf >= 'a'))
		return( (gppf - 'a') + 17);
	return(0);
}
 
