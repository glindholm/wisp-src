/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC. All Rights Reserved.
**
** $Id:$
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

#include <string.h>

void VL_libvideo_version(char *version)
{
	strcpy(version,"v4.0.00");
}
