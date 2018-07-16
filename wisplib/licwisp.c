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
**	File:		licwisp.c
**
**	Project:	WISPLIB
**
**	RCS:		$Source:$
**
**	Purpose:	License routines
**
*/

/*
**	Includes
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "idsisubs.h"
#include "wlicense.h"
#include "wispcfg.h"
#include "wmalloc.h"
#include "machid.h"


/*
**	Routine:	WLIC_product_name()
**
**	Function:	Return the product name.
**
**	Description:	Returns a pointer to a static data area that contains the product name.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		Pointer to product name.
**
**	Warnings:	None
**
*/
const char *WLIC_product_name(void)
{
	static	char	name[] = {"WISP"};

	return(name);
}

/*
**	Routine:	WLIC_license_filepath()
**
**	Function:	Return the filepath to the license file.
**
**	Description:	Returns a pointer to a static data area that contains 
**			the license filepath.
**
**			UNIX (OLD):	/lib/wisp.license
**			UNIX (NEW):	$WISPCONFIG/wisp.license
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		Pointer to license filepath.
**
**	Warnings:	None
**
*/
#ifdef unix
const char *WLIC_license_filepath(void)
{
	static	char*	path = NULL;
	
	if (!path)
	{
		char	buff[256];

		sprintf(buff,"%s/wisp.license", wispconfigdir());
		path = wisp_strdup(buff);
	}

	return(path);
}
#endif /* unix */
#ifdef WIN32
const char *WLIC_license_filepath(void)
{
	static	char*	path = NULL;
	
	if (!path)
	{
		char	buff[256];

		if (0 != strcmp(wispserver(), DEF_WISPSERVER))
		{
			sprintf(buff,"\\\\%s\\WISP\\LICENSE.TXT", wispserver());
		}
		else
		{
			sprintf(buff,"%s\\LICENSE.TXT", wispdir());
		}

		path = wisp_strdup(buff);
	}

	return(path);
}
#endif /* WIN32 */


/*
**	Routine:	WLIC_lic_trantable()
**
**	Function:	Return the license encoding translation table.
**
**	Description:	Returns a pointer to a static data area that contains the license
**			encoding translation table.  The table must contains:
**				letters:	A-Z
**				Numbers:	1-9
**			It is 35 bytes long and the characters are scrabbled.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		Pointer to license encoding translation table.
**
**	Warnings:	None
*/
const char *WLIC_lic_trantable(void)
{
	static	char	tt_tran[] = {"QAZ9CX1EU2GR3IO4YL5MJN6PH7SFT8VDWKB"};	/* WISP */

	return(tt_tran);
}

/*
**	History:
**	$Log: licwisp.c,v $
**	Revision 1.17  2003/06/18 13:12:38  gsl
**	change unix license file to $WISPCONFIG/wisp.license
**	
**	Revision 1.16  2003/02/13 20:47:53  gsl
**	missing include
**	
**	Revision 1.15  2003/02/13 20:45:02  gsl
**	On unix change the license file from /lib/wisp.license to
**	$WISPCONFIG/wisp.{machineid}.license
**	
**	Revision 1.14  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.13  2003/01/08 17:30:44  gsl
**	Move WLIC_authlogfile to wauthorize.c
**	
**	Revision 1.12  2002/07/10 21:05:18  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.11  2002/07/02 21:15:25  gsl
**	Rename wstrdup
**	
**	Revision 1.10  2002/06/25 15:21:53  gsl
**	Change to use wmalloc()
**	
**	Revision 1.9  2002/06/21 03:10:37  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.8  1997/03/21 15:24:58  gsl
**	Changed the license_filepath() for WIN32 to point to the share name WISP
**	on the SERVER.
**	
**	Revision 1.7  1997-03-14 10:32:54-05  gsl
**	Add license_filepath() logic for WIN32
**
**	Revision 1.6  1996-08-19 18:32:26-04  gsl
**	drcs update
**
**
**
*/
