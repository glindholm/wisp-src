static char copyright[]="Copyright (c) 1995-1997 NeoMedia Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

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


/*
**	Routine:	product_name()
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
**	History:	
**	09/13/93	Written by GSL
**
*/
char *product_name(void)
{
	static	char	name[] = {"WISP"};

	return(name);
}

/*
**	Routine:	license_filepath()
**
**	Function:	Return the filepath to the license file.
**
**	Description:	Returns a pointer to a static data area that contains the license filepath.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		Pointer to license filepath.
**
**	Warnings:	None
**
**	History:	
**	09/13/93	Written by GSL
**
*/
char *license_filepath(void)
{
#ifdef unix
	static	char	path[] = {"/lib/wisp.license"};
#endif
#ifdef MSDOS
	static	char	path[] = {"C:\\WISP.LIC"};
#endif
#ifdef WIN32
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

		path = malloc(strlen(buff)+1);
		strcpy(path,buff);
	}
#endif

	return(path);
}

/*
**	Routine:	x_license_filepath()
**
**	Function:	Return the filepath to the hidden temporary license file.
**
**	Description:	Returns a pointer to a static data area that contains the filepath
**			for the hidden temporary file.  This is used to keep the inode
**			on systems that don't have a machine id.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		Pointer to hidden temporary license filepath.
**
**	Warnings:	None
**
**	History:	
**	09/13/93	Written by GSL
**
*/
char *x_license_filepath(void)
{
#ifdef unix
	static	char	path[] = {"/lib/.wisp.license"};
#endif
#ifdef MSDOS
	static	char	path[] = {"C:\\WISP.LIX"};
#endif
#ifdef WIN32
	static	char*	path = NULL;
	
	if (!path)
	{
		char	buff[256];

		if (0 != strcmp(wispserver(), DEF_WISPSERVER))
		{
			sprintf(buff,"\\\\%s\\WISP\\LICENSEX.TXT", wispserver());
		}
		else
		{
			sprintf(buff,"%s\\LICENSEX.TXT", wispdir());
		}

		path = malloc(strlen(buff)+1);
		strcpy(path,buff);
	}
#endif

	return(path);
}

/*
**	Routine:	lic_trantable()
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
**
**	History:	
**	09/13/93	Written by GSL
**
*/
char *lic_trantable(void)
{
	static	char	tt_tran[] = {"QAZ9CX1EU2GR3IO4YL5MJN6PH7SFT8VDWKB"};	/* WISP */

	return(tt_tran);
}

/*
**	Routine:	authlogfile()
**
**	Function:	Return the authorize log filename.
**
**	Description:	Returns a pointer to a static data area that contains the file name.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		Pointer to name.
**
**	Warnings:	None
**
**	History:	
**	09/13/93	Written by GSL
**
*/
char *authlogfile(void)
{
	static	char	name[] = {"wauthorize.log"};

	return(name);
}

/*
**	History:
**	$Log: licwisp.c,v $
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
