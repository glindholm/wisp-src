/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/



/*						Include standard header files.							*/

#include <stdio.h>										/* Reference standard I/O.	*/
#include "werrlog.h"
#include "vwang.h"
#include "wisplib.h"
#include "wfiledis.h"
#include "wdefines.h"

#include "filext.h"

/*						Entry point.									*/

int main(int argc, char *argv[])
{
	WL_initglbs("DISPLAY ");
	vwang_title("WISP DISPLAY");

	vwang_init_screen();										/* Initialize for Wang screen.	*/

	if (argc > 1)										/* Did user give a filename?	*/
	{
		WL_internal_display(argv[1], 0);
	}
	else
	{
		char	filename[COB_FILEPATH_LEN + 1];
		int recsize = 0;

		if (WL_display_util_getparms(filename,&recsize))
		{
			WL_internal_display(filename, recsize);
		}
	}
	
	vwang_shut();										/* All done.			*/
	return 0;
}
/*
**	History:
**	$Log: display.c,v $
**	Revision 1.20  2003/02/20 23:14:35  gsl
**	Add OPTIONS get to DISPLAY utility that gets the record size RECSIZE
**	
**	Revision 1.19  2003/02/04 18:50:26  gsl
**	fix copyright header
**	
**	Revision 1.18  2002/12/10 17:09:11  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.17  2002/07/10 21:06:29  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.16  2002/07/09 04:13:51  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.15  2002/06/26 01:42:47  gsl
**	Remove VMS code
**	
**	Revision 1.14  2002/06/25 18:18:34  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
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
