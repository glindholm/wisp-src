/*
******************************************************************************
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
******************************************************************************
*/


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

static char ident[] = "@(#)VIDEO screen handler, NeoMedia Technologies, Inc.  10/20/95 v3.3.18";

#include <string.h>

void VL_libvideo_version(char *version)
{
	char *cptr;
	int cont;

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
**	Revision 1.8  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.7  2003/01/31 20:58:40  gsl
**	Fix -Wall warnings
**	
**	Revision 1.6  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.5  2002/07/15 17:10:03  gsl
**	Videolib VL_ gobals
**	
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
