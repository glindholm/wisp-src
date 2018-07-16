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
void GP_init_gpint()
{
	int idx;

	if(GP_INT[1])
		return;

	for(idx = 0; idx < 255; ++idx)
		{
		GP_INT[idx] = idx;
		WL_wswap(&GP_INT[idx]);
		}
}

/*----
Display and read a getparm, and return the translated PF code
------*/
int4 GP_display_and_read()
{
	WL_set_va_count(2);
	GETPARM(&GP_arg,&GP_cnt);
	return(GP_pfkey = gppf_tran(GP_pfrcvr[0]));
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
**	Revision 1.8.2.1  2002/11/12 15:56:24  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.10  2002/10/24 14:20:39  gsl
**	Make globals unique
**	
**	Revision 1.9  2002/07/12 17:17:01  gsl
**	Global unique WL_ changes
**	
**	Revision 1.8  1996/09/17 23:34:08  gsl
**	drcs update
**	
**
**
*/
