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
#include "wispcfg.h"
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

static char	WISPRETURNCODE[3] = {'0','0','0'};

int WL_get_internal_retcode(void)
{
	return atol ( WISPRETURNCODE );
}

void WL_set_internal_retcode(unsigned int rc)
{
	char buf[10];
	sprintf(buf, "%03u", rc);
	memcpy(WISPRETURNCODE, buf, 3);
}


#define		ROUTINE		54000

void RETCODE(char code[3])
{
	int	rc;
	char	rcbuff[10];
	FILE	*fh;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	strcpy(rcbuff,"000");
	if (fh = fopen(wisprcfilepath(),FOPEN_READ_TEXT))
	{
		if (1==fscanf(fh,"%d",&rc))
		{
			sprintf(rcbuff,"%03d",rc);
		}
		fclose(fh);
	}
	memcpy(code,rcbuff,3);
}


/*
**	NAME:	setretcode.c
*/

/* subroutine: setretcode													*/
/* called by COBOL to pass PIC 999 status code back to calling shell								*/


#undef ROUTINE
#define		ROUTINE		59000

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

	if (fh = fopen(wisprcfilepath(),FOPEN_WRITE_TEXT))
	{
		fprintf(fh,"%03d\n",rc);
		fclose(fh);
	}
	else
	{
		werrlog(ERRORCODE(4),wisprcfilepath(), errno,0,0,0,0,0,0);
	}

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
	unlink(wisprcfilepath());
}


/*
**	History:
**	$Log: retcode.c,v $
**	Revision 1.18.2.1  2002/11/14 21:12:25  gsl
**	Replace WISPFILEXT and WISPRETURNCODE with set/get calls
**	
**	Revision 1.18  2001/10/31 20:30:44  gsl
**	moved wisprcfilepath() to wispcfg.c
**	
**	Revision 1.17  2001-10-10 15:00:00-04  gsl
**	Changed wisprcfilepath() to be RC_userid_pgid
**
**	Revision 1.16  2001-10-10 14:30:30-04  gsl
**	Remove VMS
**
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
