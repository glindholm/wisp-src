static char copyright[]="Copyright (c) 1997 NeoMedia Technologies, Inc., All rights reserved.";
static char rcsid[]="$Id:$";

#include <stdio.h>

/*
**	Structures and Defines
*/
#ifdef DEBUG
#define DEBUG_STR " (DEBUG)"
#else
#define DEBUG_STR ""
#endif

/*
**	Static data
*/
static char platform_str[3] =
{
#ifdef	KCSI_UNIX
	'u',
#elif	KCSI_WIN32
	'w',
#else
	'x',
#endif

#ifdef	KCSI_ACU
	'a',
#elif	KCSI_MF
	'm',
#else
	'x',
#endif
0};


char *create_version(void);
char *create_platform(void);

/*
**	ROUTINE:	create_version()
**
**	FUNCTION:	Return the create version string
**
**	DESCRIPTION:	Form a version string from CREATE_VERSION of the form "9.9" and return it.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		Pointer to version string const
**
**	WARNINGS:	None
**
*/
char *create_version(void)
{
	static char the_version[20];
	static int first = 1;

	if (first)
	{
		sprintf(the_version,"%d.%d%s",(int)(CREATE_VERSION / 10), (int)(CREATE_VERSION % 10),DEBUG_STR);
		first = 0;
	}

	return the_version;
}

/*
**	ROUTINE:	create_platform()
**
**	FUNCTION:	Return the create platform string
**
**	DESCRIPTION:	Form a version string from create_platform[] of the form "xx" and return it.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		Pointer to platform string const
**
**	WARNINGS:	None
**
*/
char *create_platform(void)
{
	return( platform_str );
}

/*
**	History:
**	$Log: version.c,v $
**	Revision 1.2  1997-12-19 16:59:27-05  gsl
**	Add include
**
**	Revision 1.1  1997-08-15 10:33:34-04  scass
**	Initial revision
**
**
**
*/
