/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
**	File:		runtype.c
**
**	Purpose:	Routines used to determine type of file.
**
**	Routines:	WL_runtype()	What kind of "runnable" file is this.
**
*/

#include <stdio.h>
#include <fcntl.h>
#include <string.h>

#ifdef unix
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

#ifdef WIN32
#include <io.h>
#endif

#include "idsistd.h"
#include "runtype.h"
#include "wdefines.h"
#include "idsisubs.h"
#include "wisplib.h"
#include "werrlog.h"

#ifdef WIN32
#include "isonames.h"
#endif

/*
**	Routine:	WL_runtype()
**
**	Function:	To figure out the run type of the file.
**
**	Description:	First check the extension on the file.
**			If we can't determine the type then call isexec().
**
**	Arguments:
**		filespec	The full filename spec.
**
**	Return:		The run type.
**
**	Warnings:	None
**
*/

#ifdef unix
static int osd_runtype(const char* filespec)
{
	char	buff[256];
	int	i, rc;
	int	fh;

	if (!WL_fexists(filespec)) return(RUN_ACCESS);

	strcpy(buff,splitext((char *)filespec));

	if (buff[0])							/* There is an extension, find out what	it is.	*/
	{
		if (0==strcmp(buff,".int")) 	return(RUN_MF);
		if (0==strcmp(buff,".gnt")) 	return(RUN_MF);
#ifdef HPUX
		if (0==strcmp(buff,".sl")) 	return(RUN_MF);		/* Micro Focus Shared objects use .sl on HPUX */
#else
		if (0==strcmp(buff,".so")) 	return(RUN_MF);
#endif
		if (0==strcmp(buff,".sh")) 	return(RUN_SHELL);
		if (0==strcmp(buff,".wps")) 	return(RUN_PROC);
		if (0==strcmp(buff,".wpr")) 	return(RUN_PROCOBJ);
		if (0==strcmp(buff,".cbx")) 	return(RUN_ACUCOBOL);
		if (0==strcmp(buff,".acu")) 	return(RUN_ACUCOBOL);
		if (0==strcmp(buff,".wcb")) 	return(RUN_NOT);
		if (0==strcmp(buff,".cob")) 	return(RUN_NOT);
		if (0==strcmp(buff,".vix")) 	return(RUN_NOT);
		if (0==strcmp(buff,".idx")) 	return(RUN_NOT);
		if (0==strcmp(buff,".o")) 	return(RUN_NOT);
		if (0==strcmp(buff,".exe")) 	return(RUN_EXEC);	/* This is a way to force it to think it's executable */
	}

	switch(WL_isexec(filespec))					/* Call WL_isexec to check the magic number	*/
	{
	case ISEXEC:	return(RUN_EXEC);
	case ISACU:	return(RUN_ACUCOBOL);
	case ISMFINT:	return(RUN_MF);
	case NOTEXEC:	break;
	case ACCERR:	break;
	}

	/*
	**	If the 'x' bit is not set then assume not executable.
	*/
	{
		mode_t mode;
		if (0 != WL_stat_mode(filespec,&mode))
		{
			return(RUN_ACCESS);
		}
		if (   (mode & S_IXUSR)
		    || (mode & S_IXGRP)
		    || (mode & S_IXOTH) )
		{
			/* The execute bit is set so we will assume it is executable */
		}
		else
		{
			return RUN_NOT;
		}
	}

	/*
	**	Read the file to see if it contains regular text characters. 
	**	If it does assume a shell script.
	**	Otherwise assume it is executable.
	*/

	fh = open(filespec,O_RDONLY,0);
	if ( fh == -1 )
	{
		return(RUN_EXEC);
	}

	rc = read(fh,buff,sizeof(buff));
	for(i=0;i<rc;i++)
	{
		if (buff[i] >= ' ' && buff[i] <= '~') continue;
		if (buff[i] == '\n' ||						/* Normal control chars (plus bell 07) allowed	*/
		    buff[i] == '\f' ||
		    buff[i] == '\r' ||
		    buff[i] == '\b' ||
		    buff[i] == '\t' ||
		    buff[i] == 0x07   ) continue;
		close(fh);
		return(RUN_EXEC);
	}
	close(fh);
	return(RUN_SHELL);
}
#endif /* unix */

#ifdef WIN32
static int osd_runtype(const char* filespec)
{
	char	buff[64];
	int	rc;
	int	fh;

	if (!WL_fexists(filespec)) return(RUN_ACCESS);

	strcpy(buff,splitext((char *)filespec));

	if (buff[0])
	{
		WL_lower_string(buff);
		if (0==strcmp(buff,".exe")) 	return(RUN_EXEC);
		if (0==strcmp(buff,".com")) 	return(RUN_EXEC);
		if (0==strcmp(buff,".int")) 	return(RUN_MF);
		if (0==strcmp(buff,".gnt")) 	return(RUN_MF);
		if (0==strcmp(buff,".so")) 	return(RUN_MF);
		if (0==strcmp(buff,".bat")) 	return(RUN_SHELL);
		if (0==strcmp(buff,".wps")) 	return(RUN_PROC);
		if (0==strcmp(buff,".wpr")) 	return(RUN_PROCOBJ);
		if (0==strcmp(buff,".cbx")) 	return(RUN_ACUCOBOL);
		if (0==strcmp(buff,".acu")) 	return(RUN_ACUCOBOL);
		if (0==strcmp(buff,".wcb")) 	return(RUN_NOT);
		if (0==strcmp(buff,".cob")) 	return(RUN_NOT);
		if (0==strcmp(buff,".vix")) 	return(RUN_NOT);
		if (0==strcmp(buff,".obj")) 	return(RUN_NOT);
	}

	/*
	**	Last chance: Read the file to see if it is an ACUCOBOL program.
	*/

	fh = _open(filespec,O_RDONLY|O_BINARY,0);
	if ( fh == -1 )
	{
		return(RUN_ACCESS);
	}

	rc = read(fh,buff,4);
	close(fh);

	if (rc == 4 && 0==memcmp(buff,"\x10\x12\x14\x20",4))
	{
		return(RUN_ACUCOBOL);
	}

	return(RUN_UNKNOWN);
}
#endif /* WIN32 */

const char *WL_runtype_text(int type)
{
	const char* typetext;
	switch(type)
	{
	case RUN_UNKNOWN:	typetext = "Unknown";		break;
	case RUN_ACCESS:	typetext = "Unable to access";	break;
	case RUN_NOT:		typetext = "Not runnable";	break;
	case RUN_EXEC:		typetext = "Executable";	break;
	case RUN_ACUCOBOL:	typetext = "Acucobol object";	break;
	case RUN_MF:		typetext = "Micro Focus object";break;
	case RUN_SHELL:		typetext = "Script";		break;
	case RUN_PROC:		typetext = "Procedure";		break;
	case RUN_PROCOBJ:	typetext = "Procedure object";	break;
	default:		typetext = "?";			break;
	}

	return typetext;
}

int WL_runtype(const char* filespec)
{
	int type;

	type = osd_runtype(filespec);

	if (WL_wtracing())
	{
		switch(type)
		{
		case RUN_ACCESS:
			WL_wtrace("RUNTYPE","ACCESS","Unable to access [%s] runtype=[%d] errno=%d", 
				filespec, type, errno);
			break;

		case RUN_UNKNOWN:
		case RUN_NOT:
		case RUN_EXEC:
		case RUN_ACUCOBOL:
		case RUN_MF:
		case RUN_SHELL:
		case RUN_PROC:
		case RUN_PROCOBJ:
		default:
			WL_wtrace("RUNTYPE","TYPE","File [%s] has runtype=[%d] (%s)", 
				filespec, type, WL_runtype_text(type));
			break;
		}
	}

	return type;
}

/*
**	History:
**	$Log: runtype.c,v $
**	Revision 1.22  2011/10/29 20:09:14  gsl
**	Fix ISO routine name warnins on WIN32
**	
**	Revision 1.21  2003/01/31 18:48:36  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.20  2002/12/11 20:58:41  gsl
**	Enhance tracing of runtype
**	
**	Revision 1.19  2002/12/11 20:33:37  gsl
**	Enhance tracing of runtype
**	
**	Revision 1.18  2002/10/04 21:00:54  gsl
**	Change to use WL_stat_xxx() routines
**	
**	Revision 1.17  2002/08/20 16:09:49  gsl
**	Add support for Micro Focus Shared Object files .so/.sl
**	
**	Revision 1.16  2002/07/25 17:03:43  gsl
**	MSFS->WIN32
**	
**	Revision 1.15  2002/07/18 21:04:27  gsl
**	Remove MSDOS code
**	
**	Revision 1.14  2002/07/10 21:05:23  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.13  2001/11/12 22:43:08  gsl
**	Open in O_BINARY (WIN32)
**	
**	Revision 1.12  1998-10-14 12:00:16-04  gsl
**	Enhanced runtype() to check the filemode to see if the 'X' execute bit
**	was set. If set then default to executable if other type not determined.
**
**	Revision 1.11  1998-05-18 10:12:03-04  gsl
**	Add .o and .obj as NOT_RUN.
**	WIN32 changed to check for RUN_ACUCOBOL even if it has an unknow extension.
**
**	Revision 1.10  1998-05-14 17:01:44-04  gsl
**	Add .vix as NOT_RUN and fix RUN_ACUCOBOL logig so it looks at the full magic
**	number.
**
**	Revision 1.9  1998-03-09 13:51:44-05  gsl
**	make filespec const
**
**	Revision 1.8  1998-03-09 13:48:47-05  gsl
**	Make filespec const
**
**	Revision 1.7  1996-09-16 17:44:04-04  gsl
**	Add extensions .CBX and .ACU to be Acucobol object files
**
**	Revision 1.6  1996-08-19 15:32:51-07  gsl
**	drcs update
**
**
**
*/
