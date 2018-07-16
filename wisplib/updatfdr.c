			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

#include "idsistd.h"
#include "werrlog.h"
#define		ROUTINE		65000

UPDATFDR()
{
	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/
	werrlog(102,"UPDATFDR not supported",0,0,0,0,0,0,0);
}
