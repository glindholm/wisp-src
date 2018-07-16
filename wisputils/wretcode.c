			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#ifdef unix

/* program: retcode													*/
/* called by shell to get 3 digit status code back from COBOL								*/ 
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <stdio.h>
#include <errno.h>

main()
{
	int retval;
	char mtxt[4];
	
	RETCODE(mtxt);
	if (mtxt[0]==0&&mtxt[1]==0&&mtxt[2]==0) retval=0;
	else
	  if (mtxt[0]==' '&&mtxt[1]==' '&&mtxt[2]==' ') retval=0;
	else
	  retval = (mtxt[0]-0x30) * 100 + (mtxt[1]-0x30) * 10 + (mtxt[2]-0x30);
	printf("%d",retval);	
	
}

#include "wutils.h"

#endif

