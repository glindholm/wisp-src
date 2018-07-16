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

#include "idsistd.h"
#include "runtype.h"
#include "wdefines.h"
#include "idsisubs.h"

/*
**	Routine:	runtype()
**
**	Function:	To figure out the run type of the file.
**
**	Description:	First check the extension on the file.
**			If we can't determine the type then call isexec().
**				.int	MF int code
**				.gnt	MF gnt code
**				.sh	Shell script
**				.wps	PROC
**				.wpr	PROC object
**				.wcb	NOT exec
**				.cob	NOT exec
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
int runtype(filespec)
char	*filespec;
{
	char	buff[256];
	int	i, rc;
	int	fh;

	if (!fexists(filespec)) return(RUN_ACCESS);

	strcpy(buff,splitext(filespec));

	if (buff[0])								/* There is an extension, find out what	it is.	*/
	{
		if (0==strcmp(buff,".int")) 	return(RUN_MFINT);
		if (0==strcmp(buff,".gnt")) 	return(RUN_MFGNT);
		if (0==strcmp(buff,".sh")) 	return(RUN_SHELL);
		if (0==strcmp(buff,".wps")) 	return(RUN_PROC);
		if (0==strcmp(buff,".wpr")) 	return(RUN_PROCOBJ);
		if (0==strcmp(buff,".wcb")) 	return(RUN_NOT);
		if (0==strcmp(buff,".cob")) 	return(RUN_NOT);
	}

	switch(isexec(filespec))						/* Call isexec to check the magic number	*/
	{
	case ACCERR:	return(RUN_UNKNOWN);
	case ISEXEC:	return(RUN_EXEC);
	case ISACU:	return(RUN_ACUCOBOL);
	case ISMFINT:	return(RUN_MFINT);
	case NOTEXEC:	break;
	}

	/*
	**	Last chance: Read the file to see if it contains regular text characters. If it does assume a shell script.
	*/

	fh = open(filespec,O_RDONLY,0);
	if ( fh == -1 )
	{
		return(RUN_UNKNOWN);
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
		return(RUN_NOT);
	}
	close(fh);
	return(RUN_SHELL);
}
#endif /* unix */

#ifdef MSDOS
int runtype(filespec)
char	*filespec;
{
	char	buff[64];
	int	i, rc;
	int	fh;

	if (!fexists(filespec)) return(RUN_ACCESS);

	strcpy(buff,splitext(filespec));

	if (buff[0])
	{
		upper_string(buff);
		if (0==strcmp(buff,".EXE")) 	return(RUN_EXEC);
		if (0==strcmp(buff,".COM")) 	return(RUN_EXEC);
		if (0==strcmp(buff,".INT")) 	return(RUN_MFINT);
		if (0==strcmp(buff,".GNT")) 	return(RUN_MFGNT);
		if (0==strcmp(buff,".BAT")) 	return(RUN_SHELL);
		if (0==strcmp(buff,".WPS")) 	return(RUN_PROC);
		if (0==strcmp(buff,".WPR")) 	return(RUN_PROCOBJ);
		if (0==strcmp(buff,".WCB")) 	return(RUN_NOT);
		if (0==strcmp(buff,".COB")) 	return(RUN_NOT);
		return(RUN_NOT);
	}

	/*
	**	Last chance: Read the file to see if it is an ACUCOBOL program.
	*/

	fh = open(filespec,O_RDONLY,0);
	if ( fh == -1 )
	{
		return(RUN_UNKNOWN);
	}

	rc = read(fh,buff,2);
	close(fh);

	if (rc == 2 && buff[0] == 0x10 && buff[1] == 0x12) 
	{
		return(RUN_ACUCOBOL);
	}

	return(RUN_UNKNOWN);
}
#endif /* MSDOS */
