static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1991				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

/*			This is a stubb to replace the actual trigger routine. THIS MUST NEVER BE				*/
/*			REPLACED. To have an actual trigger, this routine is replaced by an					*/
/*			alternate routine of the same name.									*/

#include "video.h"

int vtrigger()
{
	return(FALSE);									/* This does not trigger.		*/
}
/*
**	History:
**	$Log: vtrigger.c,v $
**	Revision 1.9  1996-10-11 18:16:22-04  gsl
**	drcs update
**
**
**
*/
