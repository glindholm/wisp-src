static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/


/*						Include standard header files.							*/

#include <stdio.h>										/* Reference standard I/O.	*/
#include "werrlog.h"
#include "vwang.h"
#include "wisplib.h"

#define EXT_FILEXT
#include "filext.h"

/*						Entry point.									*/

int main(int argc, char *argv[])
{
#define		ROUTINE		14000
	register int i;										/* Working integer.		*/

#ifdef VMS
	/*
	**	Patch for VMS/Alpha.
	**	If program run with no arguments then argc is being set to 2 instead of 1,
	**	and the argv[1] is null.
	**	Check if argc is 2 and argv[1] in null then change argc to 1.
	*/
	if (2==argc && !argv[1])
	{
		argc = 1;
	}
#endif

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);							/* Say where we are.		*/
	initglbs("DISPLAY ");
	vwang_title("WISP DISPLAY");

	init_screen();										/* Initialize for Wang screen.	*/
	if (argc > 1)										/* Did user give a filename?	*/
	{
		if ((i = greclen(argv[1])) >= 0) vdisplay(argv[1],i);				/* Get the record length.	*/
		else if (i == -2) werrlog(ERRORCODE(2),argv[1],0,0,0,0,0,0,0);			/* Protection violation.	*/
		else werrlog(ERRORCODE(4),argv[1],0,0,0,0,0,0,0);				/* Error on OPEN.		*/
	}
	else wfile_disp();									/* Else, put up a screen.	*/
	vwang_shut();										/* All done.			*/
	return 0;
}
/*
**	History:
**	$Log: display.c,v $
**	Revision 1.11  1996-11-18 19:06:16-05  jockc
**	added call to vwang_title to set screen title
**
**	Revision 1.10  1996-07-23 17:34:42-07  gsl
**	Fix for NT
**
**	Revision 1.9  1996-07-23 11:12:50-07  gsl
**	drcs update
**
**
**
*/
