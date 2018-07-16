			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	setprogid()
*/

#ifdef MSDOS
#include <memory.h>
#endif

#include "wglobals.h"

setprogid(wisp_application_name)						/* Set global variable to current program id.	*/
char *wisp_application_name;
{
	memcpy(WISPPROGID,wisp_application_name,8);
}


