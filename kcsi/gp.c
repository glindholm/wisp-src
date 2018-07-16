static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*----
Various general purpose getparm routines
------*/
#define	_GP_C
#include "gp.h"
#include "intdef.h"
#include "kcsifunc.h"

static char sccs_id[]="@(#)gp.c	1.1 12/23/92";

static int4 gppf_tran(int gppf);

/*----
Initialize 4 byte integers 0-255 for getparm processing
------*/
void init_gpint()
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

	wvaset(&two);
	GETPARM(&gparg,&gpcnt);
	return(gppfkey = gppf_tran(gppfrcvr[0]));
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
**	$Log: gp.c,v $
**	Revision 1.8  1996/09/17 23:34:08  gsl
**	drcs update
**	
**
**
*/
