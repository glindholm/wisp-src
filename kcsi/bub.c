static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include "kcsifunc.h"

/*----
Generic bubble sort
------*/

static char sccsid[]="@(#)bub.c	1.1 4/14/93";

static int changes;

static void pass_down(char *base,int nel,int len,int (*comp)(),int (*swap)());
static void pass_up(char *base,int nel,int len,int (*comp)(),int (*swap)());

void KCSI_bub_sort(char *base,int nel,int len,int (*comp)(),int (*swap)())
{

	changes = 1;
	while(changes)
		{
		pass_up(base,nel,len,comp,swap);
		if(changes)
			pass_down(base,nel,len,comp,swap);
		}
}

void static pass_down(char *base,int nel,int len,int (*comp)(),int (*swap)())
{
	char *lo,*hi;
	int rc;
	
	changes = 0;
	if(nel < 2)
		return;
	lo = base;
	hi = base + len;

	--nel;
	while(nel--)
		{
		rc = (*comp)(lo,hi);
		if(rc < 0)
			{
			(*swap)(lo,hi);
			changes = 1;
			}
		lo += len;
		hi += len;
		}
}

static void pass_up(char *base,int nel,int len,int (*comp)(),int (*swap)())
{
	char *hi, *lo;
	int rc;

	changes = 0;
	if(nel < 2)
		return;

	hi = base + (len * (nel - 1));
	lo = hi - len;

	--nel;
	while(nel--)
		{
		rc = (*comp)(lo,hi);
		if(rc < 0)
			{
			(*swap)(lo,hi);
			changes = 1;
			}
		lo -= len;
		hi -= len;
		}
}
	
/*
**	History:
**	$Log: bub.c,v $
**	Revision 1.3.2.1  2002/11/12 15:56:19  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.4  2002/07/25 15:20:30  gsl
**	Globals
**	
**	Revision 1.3  1996/09/17 23:45:26  gsl
**	drcs update
**	
**
**
*/
