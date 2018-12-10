/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/


/*
**	File:		dbfile.c
**
**	Purpose:	To hold routines for DataBase files.
**
**	Routines:	
**	WL_setdbfile()	Set the IS_DBFILE flag in a status word.
**
**
*/

#include "idsistd.h"
#include "wcommon.h"
#include "wfname.h"

/*
**	Routine:	WL_setdbfile()
**
**	Function:	Set the IS_DBFILE flag in a status word.
**
**	Description:	If flag is set then set IS_DBFILE otherwise clear it.
**
**	Arguments:
**	mode		The file status mode
**	flag		The state we want IS_DBFILE set to.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	01/06/94	Written by GSL
**
*/
void WL_setdbfile(int4 *mode, int flag)
{
	if (flag)
	{
		*mode |= IS_DBFILE;	/* Set  IS_DBFILE flag */
	}
	else
	{
		*mode &= ~IS_DBFILE;	/* Clear IS_DBFILE flag*/
	}
}

void WL_setdbfile_attr(char *file_attributes, int flag)
{
	int4 mode;

	wisp_fileattr2mode(file_attributes, &mode);
	WL_setdbfile(&mode, flag);
	wisp_mode2fileattr(mode, file_attributes);

}
/*
**	History:
**	$Log: dbfile.c,v $
**	Revision 1.9  2003/03/06 21:33:22  gsl
**	Add WL_setdbfile_attr() for use with file ATTR
**	
**	Revision 1.8  2003/01/31 17:23:48  gsl
**	Fix  copyright header
**	
**	Revision 1.7  2002/07/11 20:29:07  gsl
**	Fix WL_ globals
**	
**	Revision 1.6  2002/06/26 06:26:22  gsl
**	Mode/status bit field changes
**	
**	Revision 1.5  1996/08/19 22:32:16  gsl
**	drcs update
**	
**
**
*/
