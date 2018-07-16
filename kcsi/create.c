static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";


/*----
Modules

vscrglb.c	globals for create

vscrmain.c	main() module

vscrhelp.c	help screens called from valrious places in the 
		running program.

vscrout.c	Screens, entry and validation for the output file to
		be created, including the file, org, record len,
		primary key and alternate key screens.

vscrblk.c	Accepts user input for multiple blocks and
		fields until exhausted. Includes logic for 
		allocating, linking and freeing blocks
		and fields.

vscrffld.c	Data entry for defining a field from a file
		to be included in the output.

vscrbchk.c	Checks after each record processed from the
		input specifications, whether the block
		has reached its logical end.

vscrsqz.c	Array squeezing routine.

vscrspky.c	Routine for setting the global split key
		variable.

vscrdbg.c	Routines to format and print a file containing
		create debug information.

vscreoj.c	End of Job Routine.

vscrfile.c	Logic to create, and format the output records.

------*/

/*----
RNotes

		Release Notes - 3/26/94

v. 1.7 3/26/94
-----
Problem:	Binary Sequential File type Requested by several
		users.

Resolution:	A Binary sequential file type has been added as
		a file type 'B'. This file type is fixed length
		records, the file contains only the data
		in the record.
+++++

v. 1.6 3/17/94	Not Released


v. 1.5 3/3/94

Problem:	INPOS not being used correctly as the starting
		point for an inputfield in a file.

Resolution:	INPOS logic corrected to start input from
		the correct position in a record.

-----
+++++

v. 1.4 3/3/94
	Internal version control only. No changes in the utility.

v. 1.3 10/31/93

Problem:	Relative files being created that consume large
		disk space. 

Resolution:	Open output logic for relative files corrected
		to create corrrect size files.

v. 1.2 08/15/93

	Initial Release for AIX, using AcuCOBOL Vision 3.0 files.

EndRNotes
------*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "create.h"
#include "kcsifunc.h"
#include "vscrglb.h"
#include "kwisp.h"

/*
**	Structures and Defines
*/

/*
**	Static data
*/

static char applname[]="CREATE  ";



#ifdef KCSI_MFX
int main(int argc, char **argv)
{
	cobinit();
	CREATE();
#ifdef OLD
	WL_initglbs(applname);

	kcsi_create_main();

	kcsi_exit(0);
#endif /* OLD */
	return 0;
}
#endif /* KCSI_MFX */

#ifdef WIN32
/*
**	Older versions of Acucobol (5.1 and earlier) exposed
**	the f_errno variable as extern short.
**	
**	In 5.2  f_errno is replaced by (*Astdlib_f_errno())  
**	which is defined int sub.h.
*/

/* For CREATE on Windows we are using Acucobol 4.1 so use old version */

extern  short		f_errno;
short *Astdlib_f_errno() /* From ACU 5.2 sub.h */
{
	return &f_errno;
}

#endif

/*
**	History:
**	$Log: create.c,v $
**	Revision 1.2.2.1  2002/11/14 16:02:59  gsl
**	Replace cr_debug the trace level
**	
**	Revision 1.2  2002/10/24 17:30:38  gsl
**	For MF call CREATE() so INITWISP2 gets called and licenses get checked
**	
**	Revision 1.1  2002/10/22 19:29:07  gsl
**	renamed from vscrmain.c
**	
**	Revision 1.20  2002/10/18 20:13:04  gsl
**	Move Acucobol 5.1 f_error support from kv3.c to vscrmain.c
**	
**	Revision 1.19  2002/10/17 21:22:20  gsl
**	move kcsi_create_main() from vscrmain.c to vscrglb.c
**	The only thing left in vscmain.c is main()
**	
**	Revision 1.18  2002/10/17 16:36:37  gsl
**	move routines  from vscrmain.c to vscrfile.c
**	
**	Revision 1.17  2002/07/25 15:20:22  gsl
**	Globals
**	
**	Revision 1.16  2002/07/10 21:06:27  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.15  2002/06/25 18:18:34  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.14  2002/06/25 17:46:03  gsl
**	Remove WISPFILEXT as a global, now must go thru set/get routines
**	
**	Revision 1.13  2002/06/21 20:48:14  gsl
**	Rework the IS_xxx bit flags and now include from wcommon.h instead of duplicate
**	definitions.
**	
**	Revision 1.12  1997/12/19 22:10:13  gsl
**	fix warnings
**	
**	Revision 1.11  1997-12-19 17:01:17-05  gsl
**	add include
**
**	Revision 1.10  1997-08-15 10:36:37-04  scass
**	Moved create_version() to version.c so builds properly and
**	still maintains the information.
**
**	Revision 1.9  1997-04-21 15:19:26-04  gsl
**	Add windows title
**
**	Revision 1.8  1997-03-26 09:12:12-05  gsl
**	Add create_version() which forms a version number from the define CREATE_VERSION
**
**	Revision 1.7  1997-03-26 08:41:19-05  gsl
**	Change version to 2.0
**
**	Revision 1.6  1996-10-02 20:12:12-04  gsl
**	added a KCSI_WIN32 platform code
**
**	Revision 1.5  1996-10-02 15:13:33-07  gsl
**	Remove WISPLIB include files
**
**	Revision 1.4  1996-10-02 09:10:55-07  gsl
**	Add standard headers
**	Fix prototypes and warnings
**	Changed vexit to vwang_shut()
**
**
**
*/
