/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
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
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/


/*
	 WFACCESS ... A special routine to determine whether a file exists and whether the user has access to that file.

			WFACCESS is passed a native filepath and a mode and it returns a status code.

			Status Code		Description
			===========		===========
			ACC_ALLOWED		Access allowed
			ACC_DENIED		File access not allowed
			ACC_NOFILE		File doesn't exist (Mode= IO or INPUT)
			ACC_NODIR		Directory Path doesn't exist
			ACC_MISSING		Directory or File doesn't exist
			ACC_LOCKED		File is exclusively locked 
			ACC_NOLOCK		File is in a state where it can't be locked
			ACC_NOLINK		File can not be physically located at this time
			ACC_BADDIR		Path has invalid directories in it
			ACC_READONLY		Write access requested on a readonly device
			ACC_INUSE		File in in use
			ACC_EXISTS		File currectly exists and cannot be overridden
			ACC_OUTEXISTS		File exists and your trying to open output
			ACC_SYSLIMIT		System limits have been exceeded
			ACC_BADDEV		Device can not be accessed
			ACC_BADVOL		No translation for VOLUME
			ACC_UNKNOWN		UNKNOWN
*/

#include <stdio.h>									/* Standard I/O include file.		*/
#include <errno.h>
#include <string.h>

#ifdef WIN32
#include <io.h>
#endif
#ifdef unix
#include <unistd.h>
#endif

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>                          

#include "idsistd.h"
#include "wdefines.h"
#include "wcommon.h"
#include "wfaccess.h"
#include "cobrun.h"
#include "idsisubs.h"
#include "wisplib.h"
#include "werrlog.h"


static int x_wfaccess(char* filename, int is_output, int is_indexed);

/*
**	Routine:	wisp_file_access()
**
**	Function:	To check the access of a file before it is opened by cobol.
**
**	Description:	This routine checks if a file exists or not and if we have
**			access to it depending on the mode.
**			We only check if the file exists because access() uses the
**			the real UID not the effective UID.
**
**	Arguments:
**	native_path	The native file path as returned from wfname(). (Space padded)
**	is_output	Open outout
**	is_indexed	Indexed file
**
**	Globals:	None
**
**	Return:		The access code.
**	ACC_ALLOWED	The file exists (IO,INPUT) or can be created (OUTPUT). 
**	ACC_MISSING	The doesn't exist (IO,INPUT).
**	ACC_OUTEXISTS	The file exists (OUTPUT).
**	ACC_DENIED	Access is denied.
**	ACC_UNKNOWN	File exists but can't tell if access allowed.
**	ACC_xxx		Misc other access errors.
**	
**	Warnings:	If this routine returns an ACC_UNKNOWN you should go ahead and
**			let COBOL attempt to access the file.
**
**	History:	
**	mm/dd/yy	Written by xxx
**	02/12/93	Rewritten to not use of eaccess(). GSL
**	04/13/93	Removed call to access() to check read and write access. GSL
**	04/26/93	Changed to use fexists() instead of access() to check if file exists. GSL
**
*/
int wisp_file_access(char* native_path, int is_output, int is_indexed)
{
	char 	filename[COB_FILEPATH_LEN + 1];	 				/* The filename (null terminated string)	*/
	int	rc;

	cobx2cstr(filename,native_path,COB_FILEPATH_LEN);  			/* Move the filename in.			*/
	rc = x_wfaccess(filename, is_output, is_indexed);

	if (WL_wtracing())
	{
		char	*result;
		
		switch(rc)
		{
		case ACC_OUTEXISTS:	result = "ACC_OUTEXISTS";	break;
		case ACC_ALLOWED:	result = "ACC_ALLOWED";		break;
		case ACC_BADDIR:	result = "ACC_BADDIR";		break;
		case ACC_NODIR:		result = "ACC_NODIR";		break;
		case ACC_READONLY:	result = "ACC_READONLY";	break;
		case ACC_EXISTS:	result = "ACC_EXISTS";		break;
		case ACC_UNKNOWN:	result = "ACC_UNKNOWN";		break;
		case ACC_MISSING:	result = "ACC_MISSING";		break;
		default:		result = "ACC_????";		break;
		}
		
		wtrace("WFACCESS","RESULT","File=[%s] is_output=%d is_indexed=%d %s rc=%d ", 
			filename, is_output, is_indexed, result, rc);
	}

	return rc;
}

static int x_wfaccess(char* filename, int is_output, int is_indexed)
{
	char	fileidx[81];							/* Filename with .idx extension			*/
	int  	file_desc;

	strcpy(fileidx,filename);						/* Construct fileidx				*/
	strcat(fileidx,".idx");

        if (is_output)								/* Is this an open output ?			*/
	{    
		if (fexists( filename))						/* check if file exists				*/
		{
			return(ACC_OUTEXISTS);					/* File exists					*/
		}

		if (is_indexed)						/* CISAM uses .idx for INDEXED			*/
		{
			if (fexists(fileidx))					/* check if idx file exists			*/
			{
				return(ACC_OUTEXISTS);				/* File exists					*/
			}
		}

		if (-1 != (file_desc = creat(filename, 00666)))			/* See if she'll open up.			*/
		{
			close(file_desc);
			wisp_unlink(filename);
			return( ACC_ALLOWED );
		}
										/* We got an error.				*/
		if ( filename[0] == NULL_CHAR ) return( ACC_BADDIR );

		switch( errno )
		{
		case ENOENT:	return( ACC_NODIR );
		case ENOTDIR:	return( ACC_BADDIR );
		case EROFS:	return( ACC_READONLY );
		case EISDIR:	return( ACC_EXISTS );
		case EFAULT:	return( ACC_BADDIR );
		}
		return( ACC_UNKNOWN );
	}
	else									/* IO or INPUT mode				*/
	{
		if (fexists(filename))						/* Check if file exists				*/
		{
			return(ACC_UNKNOWN);					/* File exists but access is unknown		*/
		}

		if (is_indexed)							/* CISAM uses .idx for INDEXED			*/
		{
			if (fexists(fileidx))					/* check if idx file exists			*/
			{
				strcpy(filename,fileidx);			/* Use the idx part as the name			*/
				return(ACC_UNKNOWN);				/* File exists but access is unknown		*/
			}
		}
		return(ACC_MISSING);						/* File was not found				*/

	}                                                                                                                         
}

/*
**	History:
**	$Log: wfaccess.c,v $
**	Revision 1.23  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.22  2003/01/31 18:48:36  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.21  2002/12/11 17:03:09  gsl
**	use wisp_unlink()
**	
**	Revision 1.20  2002/07/18 21:04:29  gsl
**	Remove MSDOS code
**	
**	Revision 1.19  2002/07/10 21:05:30  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.18  2002/07/01 04:02:42  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.17  2002/06/27 04:12:41  gsl
**	Clean up status/mode bits
**	
**	Revision 1.16  2002/06/21 01:23:18  gsl
**	Remove VMS
**	
**	Revision 1.15  1998/08/03 21:15:25  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks.
**	
**	Revision 1.14  1998-05-12 13:52:31-04  gsl
**	Add wtrace() logig
**
**	Revision 1.13  1997-04-29 13:39:02-04  gsl
**	Move acc_message() to wfopen.c
**
**	Revision 1.12  1997-03-12 13:19:48-05  gsl
**	Changed to use WIN32 define
**
**	Revision 1.11  1996-09-10 11:48:27-04  gsl
**	combine include code
**
**	Revision 1.10  1996-08-19 15:33:12-07  gsl
**	drcs update
**
**
**
*/
