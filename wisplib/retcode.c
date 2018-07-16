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
**	SETRETCODE()	Set the Return Code value
**	WL_delete_retcode() Delete the return code temp file
*/

/*
	Special Register:

	77 RETURN-CODE  PIC 999.

	Updated by:
	BEGIN TRANSACTION set RETURN-CODE
	COMMIT set RETURN-CODE
	SORT set RETURN-CODE
	ROLLBACK set RETURN-CODE
	MOVE n TO RETURN-CODE
	CALL statements set RETURN-CODE
	FREE ALL ON ERROR set RETURN-CODE
	STOP RUN sets RETURN-CODE

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
#ifdef unix
#include <unistd.h>
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
#include "wglobals.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
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

/*
**	Static Function Prototypes
*/

void RETCODE(char code[WANG_RETCODE_LEN])
{
	int	rc;
	char	rcbuff[10];
	FILE	*fh;

	WL_wtrace("RETCODE","ENTRY","Entry into RETCODE");

	strcpy(rcbuff,"000");
	if ((fh = fopen(wisprcfilepath(),FOPEN_READ_TEXT)))
	{
		if (1==fscanf(fh,"%d",&rc))
		{
			sprintf(rcbuff,"%03d",rc);
		}
		fclose(fh);
	}
	memcpy(code,rcbuff,WANG_RETCODE_LEN);
	WL_wtrace("RETCODE","RETURN","RETURN-CODE=[%3.3s]", code);
}


/*
**	NAME:	setretcode.c
*/

/* subroutine: SETRETCODE													*/
/* called by COBOL to pass PIC 999 status code back to calling shell								*/

void SETRETCODE(const char code[WANG_RETCODE_LEN])	/* setretcode -> SETRETCODE */
{
	int	rc;
	char	rcbuff[10];
	FILE	*fh;

	WL_wtrace("SETRETCODE","ENTRY","Entry into SETRETCODE(%3.3s)", code);

	memcpy(WISPRETURNCODE,code,WANG_RETCODE_LEN);				/* Copy cobol data area to C data area		*/

	memcpy(rcbuff,code,WANG_RETCODE_LEN);
	rcbuff[WANG_RETCODE_LEN] = (char)0;
	rc = ATOI4(rcbuff);
	wisp_set_LINKRETCODE(rc);						/* Convert to int4				*/

	if (0==rc)
	{
		/*
		**	Special case for RC of ZERO, no file is created.
		**	Remove pre-existing file.
		*/
		WL_delete_retcode();
		return;
	}

	if ((fh = fopen(wisprcfilepath(),FOPEN_WRITE_TEXT)))
	{
		fprintf(fh,"%03d\n",rc);
		fclose(fh);
	}
	else
	{
		werrlog(WERRCODE(59004),wisprcfilepath(), errno,0,0,0,0,0,0);
	}

}


/*
**	ROUTINE:	WL_delete_retcode()
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
void WL_delete_retcode(void)
{
	wisp_unlink(wisprcfilepath());
}


/*
**	History:
**	$Log: retcode.c,v $
**	Revision 1.32  2003/07/03 18:55:06  gsl
**	trace
**	
**	Revision 1.31  2003/05/22 14:08:40  gsl
**	Add WANG_RETCODE_LEN
**	
**	Revision 1.30  2003/02/04 17:05:01  gsl
**	Fix -Wall warnings
**	
**	Revision 1.29  2003/01/31 18:54:38  gsl
**	Fix copyright header
**	
**	Revision 1.28  2003/01/31 18:48:36  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.27  2002/12/11 17:03:07  gsl
**	use wisp_unlink()
**	
**	Revision 1.26  2002/12/09 21:09:30  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.25  2002/07/30 19:12:40  gsl
**	SETRETCODE
**	
**	Revision 1.24  2002/07/29 21:13:26  gsl
**	setretcode -> SETRETCODE
**	
**	Revision 1.23  2002/07/12 19:10:15  gsl
**	Global unique WL_ changes
**	
**	Revision 1.22  2002/07/12 17:01:00  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.21  2002/07/10 21:05:23  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.20  2002/07/01 04:02:40  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.19  2002/06/25 18:18:40  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
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
**	Added WL_delete_retcode() and combined duplicate code
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
