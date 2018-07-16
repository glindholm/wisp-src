			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/


/*						Include standard header files.							*/

#include <stdio.h>										/* Reference standard I/O.	*/
#include "werrlog.h"

#define EXT_FILEXT
#include "filext.h"

/*						Entry point.									*/

main(argc,argv) int argc; char *argv[];
{
#define		ROUTINE		14000
	register int i;										/* Working integer.		*/

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);							/* Say where we are.		*/
	initglbs("DISPLAY ");

	init_screen();										/* Initialize for Wang screen.	*/
	if (argc > 1)										/* Did user give a filename?	*/
	{
		if ((i = greclen(argv[1])) >= 0) vdisplay(argv[1],i);				/* Get the record length.	*/
		else if (i == -2) werrlog(ERRORCODE(2),argv[1],0,0,0,0,0,0,0);			/* Protection violation.	*/
		else werrlog(ERRORCODE(4),argv[1],0,0,0,0,0,0,0);				/* Error on OPEN.		*/
	}
	else wfile_disp();									/* Else, put up a screen.	*/
	vexit();										/* All done.			*/
}
