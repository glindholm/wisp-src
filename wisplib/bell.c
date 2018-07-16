static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include "idsistd.h"
#include "movebin.h"
#include "werrlog.h"
#include "wisplib.h"
#include "vwang.h"
#include "wperson.h"

#ifdef	BELL
#undef	BELL
#endif

void BELL(int4 *bell_count)							/* WANG BELL function				*/
{
#define		ROUTINE		2000

	int4	l_times;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);					/* Log the entry.				*/

	l_times = get_swap(bell_count);

	if (!nativescreens())
	{
		vwang_bell(l_times);
	}
}
/*
**	History:
**	$Log: bell.c,v $
**	Revision 1.11  1997-10-23 16:05:55-04  gsl
**	Add missing  include
**
**	Revision 1.10  1997-10-17 14:07:01-04  gsl
**	Add nativescreens() conditionals
**
**	Revision 1.9  1996-08-19 18:32:09-04  gsl
**	drcs update
**
**
**
*/
