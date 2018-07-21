/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
******************************************************************************
*/


/*
**	File:		miscsubs.c
**
**	Purpose:	To hold misc C subs
**
**	Routines:	
**	globaldata()		Generate tmp file name for global data passing.
**	delete_globaldata()	Delete the globaldata file.
**
*/

/*
**	Includes
*/
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#ifdef unix
#include <unistd.h>
#endif
#ifdef WIN32
#include <io.h>
#include <direct.h>
#endif
#include "idsistd.h"
#include "wfname.h"
#include "wcommon.h"
#include "envs.h"
#include "wanguid.h"
#include "wdefines.h"
#include "wisplib.h"
#include "vssubs.h"
#include "wperson.h"
#include "filext.h"

#ifdef WIN32
#define unlink(file)			_unlink(file)
#endif

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static Function Prototypes
*/

/*
**	Routine:	globaldata()
**
**	Function:	Generate tmp file name for global data passing.
**
**	Description:	The tmp file name is generated from the GID.
**
**			A single "LINKED" process thread will all use the same
**			tmp file for global data.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		Pointer to global tmp filepath.
**
**	Warnings:	None
**
**	History:	
**	12/07/93	Written by GSL
**	10/13/95	Changed to use GID and not store in env. GSL
**
*/
char *globaldata()
{
	static	int	first = 1;
	static	char	filepath[80];

	if (first)
	{
		first = 0;

		if (!WL_fexists(wisptmpdir(NULL)))
		{
#ifdef unix
			mkdir(wisptmpdir(NULL), 0777);
			chmod(wisptmpdir(NULL), 0777);
#endif
#ifdef WIN32
			_mkdir(wisptmpdir(NULL));
#endif

		}

		sprintf(filepath,"%s/WPROC_%s_%u.gbl",wisptmpdir(NULL),WL_longuid(),(unsigned)WL_wgetpgrp());
	}

	return(filepath);

}


void delete_globaldata() 
{
	unlink(globaldata());
}

/*
**	ROUTINE:	tempproc()
**
**	FUNCTION:	Generate a unique temp proc filename to be used by SUBMIT
**
**	DESCRIPTION:	
**
**	ARGUMENTS:
**	file		The wang style file name generated "UID#### "
**	lib		The wang style lib name generated "@SUBMIT@"
**	vol		The wang style vol name generated WORKVOL
**	filepath	The native style path name with a ".wps" extension.
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	If WORKVOL is not set it will use SPOOLVOL.
**
*/

void tempproc(char *file, char *lib, char *vol, char *filepath)
{
	int4	mode;
	char	ext[39];
	char	*ptr;

	memcpy(file,"##      ",8);
	memcpy(&file[2],WL_wanguid3(),3);

	memset(ext,' ',sizeof(ext));
	memcpy(ext,"wps",3);
	WSETFILEXT(ext);

	memcpy(lib,"@SUBMIT@",8);
	
	WL_set_va_count(2);
	EXTRACT("WV",vol);

	/*
	**	We use IS_PRINTFILE to trick wfname into not changing
	**	this into a work file (WV/WL) when it see the ## in the 
	**	file name.
	*/
	mode = IS_OUTPUT | IS_PRINTFILE;
	ptr = WL_wfname_backfill(&mode,vol,lib,file,filepath);
	*ptr = (char)0;
}


/*
**	History:
**	$Log: miscsubs.c,v $
**	Revision 1.33  2011/10/29 20:09:14  gsl
**	Fix ISO routine name warnins on WIN32
**	
**	Revision 1.32  2009/10/18 21:04:46  gsl
**	fix windows warnings
**	
**	Revision 1.31  2003/02/17 22:07:18  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.30  2003/02/04 18:50:26  gsl
**	fix copyright header
**	
**	Revision 1.29  2002/07/29 15:46:54  gsl
**	getwfilext -> WGETFILEXT
**	setwfilext -> WSETFILEXT
**	setwispfilext -> WSETFILEXT
**	
**	Revision 1.28  2002/07/12 19:10:25  gsl
**	Global unique WL_ changes
**	
**	Revision 1.27  2002/07/12 17:17:05  gsl
**	Global unique WL_ changes
**	
**	Revision 1.26  2002/07/10 21:06:28  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.25  2002/07/10 04:27:40  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.24  2002/07/09 04:14:05  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.23  2002/06/28 04:03:01  gsl
**	Work on native version of wfopen and wfname
**	
**	Revision 1.22  2002/06/27 04:12:43  gsl
**	Clean up status/mode bits
**	
**	Revision 1.21  2002/06/25 18:18:41  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.20  2002/06/25 17:46:06  gsl
**	Remove WISPFILEXT as a global, now must go thru set/get routines
**	
**	Revision 1.19  2001/10/10 18:25:11  gsl
**	Change globaldata() file name to WPROC_userid_pgid
**	
**	Revision 1.18  1998-10-13 15:47:42-04  gsl
**	Add missing include sys/stat.h
**
**	Revision 1.17  1998-08-31 15:33:20-04  gsl
**	fixed
**
**	Revision 1.16  1998-08-31 15:13:57-04  gsl
**	drcs update
**
**	Revision 1.15  1997-06-10 01:51:52-04  scass
**	corrected wvaset to use wispcommon prototype.
**
**	Revision 1.14  1997-06-10 01:11:21-04  scass
**	Changed int_32 to long
**
**	Revision 1.13  1997-06-10 00:47:47-04  scass
**	Changed int_32 back to long
**
**	Revision 1.12  1997-06-09 17:41:26-04  scass
**	int4 -> int_32
**
**	Revision 1.11  1996-09-10 11:40:22-04  gsl
**	Combine some unix and WIN32 code
**
**	Revision 1.10  1996-08-23 14:29:51-07  gsl
**	Change to use wisptmpdir()
**
**	Revision 1.9  1996-08-22 17:39:35-07  gsl
**	Change to print wgetpgrp() as a Hex value instead of decimal as it
**	can be negative.
**
**	Revision 1.8  1996-07-25 17:00:39-07  gsl
**	Fix c++ comments to be c comments
**
**	Revision 1.7  1996-07-25 16:47:12-07  gsl
**	Fix for NT using standard wisp header files
**
**	Revision 1.6  1995-10-19 03:46:26-07  gsl
**	Add routine tempproc() which generates a temp proc filename
**	that is used by SUBMIT to generate a proc
**
 * Revision 1.5  1995/10/16  13:53:21  gsl
 * The location of the temp globals file is no longer stored int the env
 * variable WPROC_GLOBALS, it is generated each time from the GID.
 * If the GID changes (as in a SUBMIT) then proc will not get confused
 * as to who owns the temp globals file.
 *
 * Revision 1.4  1995/10/12  12:17:15  gsl
 * Moved the env defines to envs.h
 *
**
**
*/
