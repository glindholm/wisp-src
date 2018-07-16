static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1991				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

/*						Keystroke Macro Manager								*/

#include <stdio.h>
#include <string.h>
#ifndef NOSTDLIB
#include <stdlib.h>
#endif

#include "video.h"									/* Include header files.		*/
#include "vlocal.h"
#include "vdata.h"
#include "vmodules.h"

static char filename[64];								/* Name of the file.			*/

char *vfilename(ext) char *ext;								/* Make a video filename.		*/
{
	char *ptr;									/* Temp char ptr			*/

#ifdef VMS
	strcpy(filename,"sys$login:video.");						/* Store the root name.			*/
	strcat(filename,ext);								/* Add the extension.			*/
#endif

#ifdef unix
	if (!(ptr=getenv("HOME")))							/* Get the HOME dir.			*/
	{
		ptr = ".";								/* If home not found use "."		*/
	}
	strcpy(filename,ptr);
	strcat(filename,"/.video");							/* Make it a non-visible file.		*/
	strcat(filename,ext);								/* Add the extension.			*/
#endif

#ifdef MSDOS
	if (!(ptr=getenv("HOME")))							/* Get the HOME dir.			*/
	{
		ptr = "C:";								/* If home not found use "C:"		*/
	}
	strcpy(filename,ptr);
	strcat(filename,"\\video.");							/* Store in the root.			*/
	strcat(filename,ext);
#endif
#ifdef WIN32
	if (!(ptr=getenv("VIDEOHOME")))							/* Get the HOME dir.			*/
	{
		ptr = "C:";								/* If home not found use "C:"		*/
	}
	strcpy(filename,ptr);
	strcat(filename,"\\video.");							/* Store in the root.			*/
	strcat(filename,ext);
#endif

	return(filename);								/* Return a pointer to the filename.	*/
}

/*						Subroutine entry point.								*/

FILE *vopenf(ext,how) char *ext, *how;
{

#ifdef VMS
	if ((how[0] == 'w') && (how[1] = '+')) 						/* Delete previous if open for update.	*/
	{
		delete(vfilename(ext));							/* Delete the file.			*/
		how[1] = CHAR_NULL;							/* Don't do an update write.		*/
	}
#endif

	return(fopen(vfilename(ext),how));						/* Open the file.			*/
}


/*
**	Routine:	vinfoname()
**
**	Function:	To construct an OSD name for a VIDEOINFO file.
**
**	Description:	This routine is passed a simple filename that should
**			reside in the VIDEOINFO directory, and it constructs
**			a full native path for that file.
**
**			On VMS it prefixes the filename with "V:".
**			On Unix and MSDOS it uses environment variables and defaults as follows.
**
**				If $VIDEOINFO defined use it.
**				Else if $VIDEOCAP defined use it.			\ For compatiblity with WISP.
**				Else if $WISPCONFIG defined use $WISPCONFIG/videocap.	/
**				Else if $OPEN3KCONFIG defined use $OPEN3KCONFIG/videocap.	/
**				Else use the default.
**
**
**	Input:
**	name		The simple filename I.e "simple.dat". It must be valid on all platforms so 8.3 and no special chars.	
**
**	Output:		None
**
**	Return:		Pointer to the full native path name.
**
**	Warnings:	This routine does NOT check for existence of the file.
**			The pointer return will remain valid until vinfoname() is called again.
**
**	History:	
**	07/21/93	Written by GSL
**
*/
char *vinfoname(name)
char *name;
{
	static char s_vinfoname[80];
	char	*vinfodir, vinfodirbuff[80];

#ifdef VMS
	strcpy(s_vinfoname,"V:");
	strcat(s_vinfoname,name);
#else
	if (vinfodir = getenv("VIDEOINFO"))
	{
		/* Got the video info directory */
	}
	else if (vinfodir = getenv("VIDEOCAP"))
	{
		/* Got the video info directory */
	}
	else if (vinfodir = getenv("WISPCONFIG"))
	{
		vbldfilepath(vinfodirbuff,vinfodir,"videocap");
		vinfodir = vinfodirbuff;
	}
	else if (vinfodir = getenv("OPEN3KCONFIG"))
	{
		vbldfilepath(vinfodirbuff,vinfodir,"videocap");
		vinfodir = vinfodirbuff;
	}
	else
	{
		vinfodir = VIDEOINFODIR;
	}

	vbldfilepath(s_vinfoname,vinfodir,name);
#endif

	return( s_vinfoname );
}

/*
**	History:
**	$Log: vopenf.c,v $
**	Revision 1.12  1998-05-21 11:44:58-04  gsl
**	For WIN32 use the env VIDEOHOME for directory of ede files or default to C:
**
**	Revision 1.11  1996-10-11 18:16:14-04  gsl
**	drcs update
**
**
**
*/
