static char copyright[]="Copyright (c) 1997 NeoMedia Technologies, Inc., All rights reserved.";
static char rcsid[]="$Id:$";

#include <stdio.h>
#include "create.h"

/*
**	Structures and Defines
*/
#ifdef DEBUG
#define DEBUG_STR " (DEBUG)"
#else
#define DEBUG_STR ""
#endif

/*
# To change the version for KCSI, change KCSI_VERSION here. 
#	wisp/src/kcsi/cridvers.c
#	wisp/src/kcsi/version.c
#	wisp/src/kcsi/kcsilibs.umf
#	wisp/src/kcsi/kcsicob.mak
#	wisp/src/kcsi/kcsi_relnotes.txt
#	wisp/src/kcsi/kcsintsetup.txt
#	wisp/src/kcsi/kcsi_acu_install.txt
#	wisp/src/kcsi/kcsi_mf_install.txt
#	wisp/src/kcsi/kcsi_packlist.txt
#	wisp/src/doc/wisp_relnotes.lis
#	wisp/src/acu/wruncbl.umf
#	wisp/src/acu/wrun32wisp_kcsi_acu52.mak
*/
#define KCSI_VERSION	4000

/*
**	Static data
*/
static char platform_str[3] =
{
#ifdef	KCSI_UNIX
	'u',
#elif	defined(KCSI_WIN32)
	'w',
#else
	'x',
#endif

#ifdef	KCSI_ACU
	'a',
#elif	defined(KCSI_MFX)
	'm',
#else
	'x',
#endif
0};

/*
**	ROUTINE:	create_version()
**
**	FUNCTION:	Return the create version string
**
**	DESCRIPTION:	Form a version string from KCSI_VERSION of the form "9.9.99" and return it.
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
		char buf[10];
		sprintf(buf,"%04d", (int)(KCSI_VERSION));
		sprintf(the_version,"%c.%c.%c%c%s",buf[0],buf[1],buf[2],buf[3],DEBUG_STR);
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
**	Revision 1.3.2.1  2002/11/12 15:56:39  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.9  2002/10/22 17:59:02  gsl
**	Combined KCSI
**	
**	Revision 1.8  2002/10/22 16:00:31  gsl
**	Combine CRID_VERSION and CREATE_VERSION into KCSI_VERSION
**	
**	Revision 1.7  2002/10/21 17:12:18  gsl
**	fix #elif
**	
**	Revision 1.6  2002/10/21 16:45:21  gsl
**	CREATE version is set in version.c for unix
**	
**	Revision 1.5  2002/10/18 20:13:28  gsl
**	CREATE Version 3.5.50
**	
**	Revision 1.4  2002/10/17 21:22:44  gsl
**	cleanup
**	
**	Revision 1.3  2001/09/06 15:25:17  gsl
**	Change to 9.9.99 format version
**	
**	Revision 1.2  1997-12-19 16:59:27-05  gsl
**	Add include
**
**	Revision 1.1  1997-08-15 10:33:34-04  scass
**	Initial revision
**
**
**
*/
