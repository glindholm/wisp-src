static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include "idsistd.h"
#include "werrlog.h"

void CEXIT(void)
{
#define		ROUTINE		7000

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);
}
/*
**	History:
**	$Log: cexit.c,v $
**	Revision 1.9  1996/08/19 22:32:11  gsl
**	drcs update
**	
**
**
*/
