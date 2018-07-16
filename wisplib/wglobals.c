static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**									
**			WGLOBALS.C	-	Wisp Global variables.	
**									
**			Any global variables used by WISP can be placed here.
**			This module can be linked with modules that need these
**			globals but do not need every module from WISP too.
*/



#define EXT_WGLOBALS
#include "idsistd.h"
#include "wglobals.h"

#define	EXT_COBRUN
#include "cobrun.h"

#ifdef __ALPHA
#define EXT_FILEXT
#include "filext.h"
#endif
void wglobals()
{
	return;	
}

/*
**	History:
**	$Log: wglobals.c,v $
**	Revision 1.9  1996/08/19 22:33:17  gsl
**	drcs update
**	
**
**
*/
