static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		runtype.c
**
**	Purpose:	Routines used to determine type of file.
**
**	Routines:	runtype()	What kind of "runnable" file is this.
**
**
**	History:
**	11/17/92	Separated from isexec.c into runtype.c. GSL
**	01/26/93	Added MSDOS. GSL
**
*/

#include <stdio.h>
#include <fcntl.h>
#include <string.h>

#ifdef unix
#include <sys/types.h>
#include <sys/stat.h>
#endif

#ifdef _MSC_VER
#include <io.h>
#endif

#include "idsistd.h"
#include "runtype.h"
#include "wdefines.h"
#include "idsisubs.h"
#include "wisplib.h"

/*
**	Routine:	runtype()
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
**	History:	07/22/92	Written by GSL
**			03/19/93	Added .cob and .wcb as NOT runable. GSL
**
*/

#ifdef unix
int runtype(const char* filespec)
{
	char	buff[256];
	int	i, rc;
	int	fh;
	struct	stat	filestat;

	if (!fexists(filespec)) return(RUN_ACCESS);

	strcpy(buff,splitext((char *)filespec));

	if (buff[0])								/* There is an extension, find out what	it is.	*/
	{
		if (0==strcmp(buff,".int")) 	return(RUN_MF);
		if (0==strcmp(buff,".gnt")) 	return(RUN_MF);
#ifdef HPUX
		if (0==strcmp(buff,".sl")) 	return(RUN_MF);
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
		if (0==strcmp(buff,".exe")) 	return(RUN_EXEC); /* This is a way to force it to think it's executable */
	}

	switch(isexec(filespec))						/* Call isexec to check the magic number	*/
	{
	case ACCERR:	return(RUN_UNKNOWN);
	case ISEXEC:	return(RUN_EXEC);
	case ISACU:	return(RUN_ACUCOBOL);
	case ISMFINT:	return(RUN_MF);
	case NOTEXEC:	break;
	}

	/*
	**	If the 'x' bit is not set then assume not executable.
	*/
	if (0 != stat(filespec,&filestat))
	{
		return(RUN_UNKNOWN);
	}
	if (   (filestat.st_mode & S_IXUSR)
	    || (filestat.st_mode & S_IXGRP)
	    || (filestat.st_mode & S_IXOTH) )
	{
		/* The execute bit is set so we will assume it is executable */
	}
	else
	{
		return RUN_NOT;
	}

	/*
	**	Read the file to see if it contains regular text characters. If it does assume a shell script.
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

#ifdef MSFS
int runtype(const char* filespec)
{
	char	buff[64];
	int	rc;
	int	fh;

	if (!fexists(filespec)) return(RUN_ACCESS);

	strcpy(buff,splitext((char *)filespec));

	if (buff[0])
	{
		upper_string(buff);
		if (0==strcmp(buff,".EXE")) 	return(RUN_EXEC);
		if (0==strcmp(buff,".COM")) 	return(RUN_EXEC);
		if (0==strcmp(buff,".INT")) 	return(RUN_MF);
		if (0==strcmp(buff,".GNT")) 	return(RUN_MF);
		if (0==strcmp(buff,".SO")) 	return(RUN_MF);
		if (0==strcmp(buff,".BAT")) 	return(RUN_SHELL);
		if (0==strcmp(buff,".WPS")) 	return(RUN_PROC);
		if (0==strcmp(buff,".WPR")) 	return(RUN_PROCOBJ);
		if (0==strcmp(buff,".CBX")) 	return(RUN_ACUCOBOL);
		if (0==strcmp(buff,".ACU")) 	return(RUN_ACUCOBOL);
		if (0==strcmp(buff,".WCB")) 	return(RUN_NOT);
		if (0==strcmp(buff,".COB")) 	return(RUN_NOT);
		if (0==strcmp(buff,".VIX")) 	return(RUN_NOT);
		if (0==strcmp(buff,".OBJ")) 	return(RUN_NOT);
	}

	/*
	**	Last chance: Read the file to see if it is an ACUCOBOL program.
	*/

	fh = open(filespec,O_RDONLY|O_BINARY,0);
	if ( fh == -1 )
	{
		return(RUN_UNKNOWN);
	}

	rc = read(fh,buff,4);
	close(fh);

	if (rc == 4 && 0==memcmp(buff,"\x10\x12\x14\x20",4))
	{
		return(RUN_ACUCOBOL);
	}

	return(RUN_UNKNOWN);
}
#endif /* MSFS */
/*
**	History:
**	$Log: runtype.c,v $
**	Revision 1.13.2.1  2002/08/20 17:56:39  gsl
**	Add support for Micro Focus Shared Object files .so/.sl
**	V4_4_04
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
