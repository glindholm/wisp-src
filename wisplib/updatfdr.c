static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

#include "idsistd.h"
#include "werrlog.h"
#define		ROUTINE		65000

void UPDATFDR()
{
	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/
	werrlog(102,"UPDATFDR not supported",0,0,0,0,0,0,0);
}
/*
**	History:
**	$Log: updatfdr.c,v $
**	Revision 1.9  1996-08-19 18:33:02-04  gsl
**	drcs update
**
**
**
*/
