static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include "idsistd.h"
#include "werrlog.h"

void CANCEL(void)
{
#define		ROUTINE		6000

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);
}
/*
**	History:
**	$Log: cancel.c,v $
**	Revision 1.9  1996/08/19 22:32:11  gsl
**	drcs update
**	
**
**
*/
