/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
******************************************************************************
*/


/*----
Various general purpose getparm routines
------*/
#define	_GP_C
#include "idsistd.h"
#include "vsegp.h"
#include "vseglb.h"
#include "wisplib.h"
#include "vssubs.h"


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
		WL_wswap(&GPINT[idx]);
	}
}

/*----
  Display and read a getparm, and return the translated PF code
  ------*/
int4 display_and_read_gp(void)
{	
	GETPARM2(gparg,gpcnt);
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
**	Revision 1.14  2003/02/19 22:16:13  gsl
**	Add GETPARM2() the 2 arg interface to GETPARM()
**	
**	Revision 1.13  2003/02/17 22:10:57  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.12  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.11  2002/07/12 17:17:07  gsl
**	Global unique WL_ changes
**	
**	Revision 1.10  1996/09/03 22:24:05  gsl
**	drcs update
**	
**
**
*/
