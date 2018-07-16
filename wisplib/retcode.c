static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		retcode.c
**
**	Project:	WISP/LIB
**
**	RCS:		$Source:$
**
**	Purpose:	Return code processing routines
**
**	Routines:	
**	RETCODE()	Get the last Return Code value
**	setretcode()	Set the Return Code value
**	delete_retcode() Delete the return code temp file
*/

/*
**	Includes
*/

#include <stdio.h>
#include <string.h>

#ifdef WIN32
#include <io.h>
#include <direct.h>
#endif

#include "idsistd.h"
#include "idsisubs.h"
#include "werrlog.h"
#include "wdefines.h"
#include "wisplib.h"
#include "paths.h"
#include "filext.h"
#include "wperson.h"
#include "wmalloc.h"
#include "osddefs.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/
#if defined(unix) || defined(WIN32) || defined(MSDOS)
static char* rcfilepath(void);
#endif

#define		ROUTINE		54000

#if defined(unix) || defined(WIN32) || defined(MSDOS)
void RETCODE(char code[3])
{
	int	rc;
	char	rcbuff[10];
	FILE	*fh;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	strcpy(rcbuff,"000");
	if (fh = fopen(rcfilepath(),FOPEN_READ_TEXT))
	{
		if (1==fscanf(fh,"%d",&rc))
		{
			sprintf(rcbuff,"%03d",rc);
		}
		fclose(fh);
	}
	memcpy(code,rcbuff,3);
}
#endif /* unix || WIN32 || MSDOS */

#ifdef VMS

#define LIB$K_CLI_GLOBAL_SYM	2
#define RETCODE_SYMBOL	"$W_RETURN_CODE"

void RETCODE(char code[3])
{
	char	buff[80];
	int4	len;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	memcpy(code,"000",3);							/* Assume OK					*/

	if ( 0 == getsymb(RETCODE_SYMBOL,buff,&len) )
	{
		switch(len)
		{
		case 0:	break;
		case 1:	code[2] = buff[0];
			break;
		case 2:	code[1] = buff[0];
			code[2] = buff[1];
			break;
		default:
			memcpy(code,buff,3);
		}
	}
}
#endif

/*
**	NAME:	setretcode.c
*/

/* subroutine: setretcode													*/
/* called by COBOL to pass PIC 999 status code back to calling shell								*/


#undef ROUTINE
#define		ROUTINE		59000

#if defined(unix) || defined(WIN32) || defined(MSDOS)
void setretcode(char* code)
{
	extern int4 LINKRETCODE;
	int	rc;
	char	rcbuff[10];
	FILE	*fh;

	werrlog(ERRORCODE(1),code,0,0,0,0,0,0,0);

	memcpy(WISPRETURNCODE,code,3);						/* Copy cobol data area to C data area		*/

	memcpy(rcbuff,code,3);
	rcbuff[3] = (char)0;
	LINKRETCODE = ATOI4(rcbuff);						/* Convert to int4				*/
	rc = LINKRETCODE;

	if (0==rc)
	{
		/*
		**	Special case for RC of ZERO, no file is created.
		**	Remove pre-existing file.
		*/
		delete_retcode();
		return;
	}

	if (!fexists(wisptmpdir(NULL)))						/* Ensure wisptmpdir(NULL) exists		*/
	{
		if (mkdir ( wisptmpdir(NULL), 0777))
		{
			werrlog(ERRORCODE(4),wisptmpdir(NULL), errno,0,0,0,0,0,0);
			return;
		}
		chmod ( wisptmpdir(NULL),0777);
	}

	if (fh = fopen(rcfilepath(),FOPEN_WRITE_TEXT))
	{
		fprintf(fh,"%03d\n",rc);
		fclose(fh);
	}
	else
	{
		werrlog(ERRORCODE(4),rcfilepath(), errno,0,0,0,0,0,0);
	}

}
#endif /* unix || WIN32 || MSDOS */

#ifdef VMS
void setretcode(char* rc)
{
	int4	size;

	werrlog(ERRORCODE(1),rc,0,0,0,0,0,0,0);

	size=3;
	if (*rc == '0')								/* Remove leading zeros				*/
	{
		rc++;
		size--;
	}
	if (*rc == '0')
	{
		rc++;
		size--;
	}

	setsymb(RETCODE_SYMBOL,rc,size,LIB$K_CLI_GLOBAL_SYM);
}
#endif


#if defined(unix) || defined(WIN32) || defined(MSDOS)
/*
**	ROUTINE:	rcfilepath()
**
**	FUNCTION:	Get path to ReturnCode file
**
**	DESCRIPTION:	The file is in the wisptmpdir and is named RC_<gid>
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		The filepath
**
**	WARNINGS:	None
**
*/
static char* rcfilepath(void)
{
	static char *rcfilename = NULL;

	if (!rcfilename)
	{
		char	file[40];
		char	path[128];
		
		sprintf(file, "RC_%04X", wgetpgrp());
		buildfilepath(path, wisptmpdir(NULL), file);

		rcfilename = wstrdup(path);
	}
	
	return rcfilename;
}

/*
**	ROUTINE:	delete_retcode()
**
**	FUNCTION:	Delete the return code temp file
**
**	DESCRIPTION:	Unlink the file
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	Doesn't check if file exists.
**
*/
void delete_retcode(void)
{
	unlink(rcfilepath());
}

#endif /* unix || WIN32 || MSDOS*/

/*
**	History:
**	$Log: retcode.c,v $
**	Revision 1.15  1998-12-09 09:39:02-05  gsl
**	Use FOPEN mode defines
**
**	Revision 1.14  1996-11-11 16:24:45-05  gsl
**	Added standard headers/comments etc.
**	Added delete_retcode() and combined duplicate code
**
**	Revision 1.13  1996-09-10 08:46:07-07  gsl
**	Combine mkdir() code
**
**	Revision 1.12  1996-08-23 14:03:29-07  gsl
**	Changed to use wisptmpdir()
**
**	Revision 1.11  1996-08-19 15:32:50-07  gsl
**	drcs update
**
**
**
*/
