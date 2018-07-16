static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/* IDSI proprietary stuff										*/

#include <string.h>

#include "idsistd.h"

void cgetneg(int4 *value)
{
	int4 tmp1,tmp2;

	memcpy(&tmp1,value,4);
	tmp2 = -tmp1;
	memcpy(value,&tmp2,4);
}
/*
**	History:
**	$Log: cgetneg.c,v $
**	Revision 1.10  1996-08-19 18:32:12-04  gsl
**	drcs update
**
**
**
*/
