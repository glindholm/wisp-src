/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
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

char *VL_vfilename(ext) char *ext;								/* Make a video filename.		*/
{
	char *ptr;									/* Temp char ptr			*/

#ifdef unix
	if (!(ptr=getenv("HOME")))							/* Get the HOME dir.			*/
	{
		ptr = ".";								/* If home not found use "."		*/
	}
	strcpy(filename,ptr);
	strcat(filename,"/.video");							/* Make it a non-visible file.		*/
	strcat(filename,ext);								/* Add the extension.			*/
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

FILE *VL_vopenf(ext,how) char *ext, *how;
{

	return(fopen(VL_vfilename(ext),how));						/* Open the file.			*/
}


/*
**	Routine:	VL_vinfoname()
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
char *VL_vinfoname(name)
char *name;
{
	static char s_vinfoname[80];
	char	*vinfodir, vinfodirbuff[80];

	if ((vinfodir = getenv("VIDEOINFO")))
	{
		/* Got the video info directory */
	}
	else if ((vinfodir = getenv("VIDEOCAP")))
	{
		/* Got the video info directory */
	}
	else if ((vinfodir = getenv("WISPCONFIG")))
	{
		VL_vbldfilepath(vinfodirbuff,vinfodir,"videocap");
		vinfodir = vinfodirbuff;
	}
	else if ((vinfodir = getenv("OPEN3KCONFIG")))
	{
		VL_vbldfilepath(vinfodirbuff,vinfodir,"videocap");
		vinfodir = vinfodirbuff;
	}
	else
	{
		vinfodir = VIDEOINFODIR;
	}

	VL_vbldfilepath(s_vinfoname,vinfodir,name);

	return( s_vinfoname );
}

/*
**	History:
**	$Log: vopenf.c,v $
**	Revision 1.18  2003/01/31 20:58:40  gsl
**	Fix -Wall warnings
**	
**	Revision 1.17  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.16  2002/07/15 20:56:40  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.15  2002/07/15 20:16:12  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.14  2002/07/15 17:52:55  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.13  2002/06/26 01:42:49  gsl
**	Remove VMS code
**	
**	Revision 1.12  1998/05/21 15:44:58  gsl
**	For WIN32 use the env VIDEOHOME for directory of ede files or default to C:
**	
**	Revision 1.11  1996-10-11 18:16:14-04  gsl
**	drcs update
**
**
**
*/
