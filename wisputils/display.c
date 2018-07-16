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
#include "wfiledis.h"
#include "wdefines.h"

#define EXT_FILEXT
#include "filext.h"

/*						Entry point.									*/

int main(int argc, char *argv[])
{
#define		ROUTINE		14000

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
		internal_display(argv[1]);
	}
	else
	{
		char	filename[COB_FILEPATH_LEN + 1];

		if (display_util_getparms(filename))
		{
			internal_display(filename);
		}
	}
	
	vwang_shut();										/* All done.			*/
	return 0;
}
/*
**	History:
**	$Log: display.c,v $
**	Revision 1.13  1998/08/03 21:26:07  jlima
**	Support Logical Volume Translation to long file names containing embedded blanks.
**	
**	Revision 1.12  1998-05-05 13:30:46-04  gsl
**	Change to use new display frontend routines
**
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
