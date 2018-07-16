static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

/*
**	File:		version.c
**
**	Project:	video/lib
**
**	RCS:		$Source:$
**
**	Purpose:	VIDEO screen handler version
**
*/

static char ident[] = "@(#)VIDEO screen handler, DevTech Migrations, Inc.  10/20/95 v3.3.18";

#include <string.h>

void libvideo_version(char *version)
{
	char *cptr;
	register cont;

	cont = 1;
	cptr=ident;
	while (cont && *cptr)
	{
		if ( (*cptr == 'v') && (*(cptr+1) != 'T')) cont = 0;
		else cptr++;
	}
	strcpy(version,cptr);
}



/*
**	History:
**	$Log: version.c,v $
**	Revision 1.4  1996/10/11 22:35:03  gsl
**	Fix rcsid and copyright
**	
**	Revision 1.3  1996-10-11 15:16:04-07  gsl
**	drcs update
**
**	Revision 1.2  1996-07-18 11:43:51-07  jockc
**	include string.h for strcpy()
**
**	Revision 1.1  1995-10-20 02:50:29-07  scass
**	Initial revision
**
 *
*/
