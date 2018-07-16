static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*----
Various general purpose getparm routines
------*/
#define	_GP_C
#include "idsistd.h"
#include "vsegp.h"
#include "vseglb.h"
#include "wisplib.h"


static int4 gppf_tran(int gppf);

/*----
Initialize 4 byte integers 0-255 for getparm processing
------*/
void init_gpint(void)
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
int4 display_and_read_gp(void)
{
	static int4 two = 2;
	
	wvaset(&two);
	GETPARM(&gparg,&gpcnt);
	gppfkey = gppf_tran(gppfrcvr[0]);

	return gppfkey;

}

/*----
Translate the GP codes '@' 'A' etc into values
0-32
------*/
static int4 gppf_tran(int gppf)
{
	if(( gppf <= 'P') && (gppf >= '@'))
		return(gppf - '@');
	if((gppf <= 'p') && (gppf >= 'a'))
		return( (gppf - 'a') + 17);
	return(0);
}
 
/*
**	History:
**	$Log: vsegp.c,v $
**	Revision 1.10  1996-09-03 18:24:05-04  gsl
**	drcs update
**
**
**
*/
