			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/
#include <stdio.h>
#include "idsistd.h"
#include "wdefines.h"

no_wispconfig()
{
	vonexit( 0 );
	printf("\n\r %%STARTUP-F-WISPCONFIG %s is undefined.\n\r\n\r\n\r",WISP_CONFIG_ENV);
	wexit(1L);
}

