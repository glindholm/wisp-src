static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*  Terminates screen 'file'	*/

#include "idsistd.h"
#include "vwang.h"

#ifndef NULL
#define NULL 0
#endif

void WSCLOSE(void)
{
	unsigned char function;
	
	function = CLOSE_WORK_STATION;							/* Set up for call to vwang.		*/
	vwang(&function,NULL,NULL,NULL,NULL,NULL);
}
/*
**	History:
**	$Log: wsclose.c,v $
**	Revision 1.9  1996-08-19 18:33:21-04  gsl
**	drcs update
**
**
**
*/
