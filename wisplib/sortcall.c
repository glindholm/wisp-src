/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
**	File:		sortcall.c
**
**	Routines:	
**	SORTCALL()	Wang VSSUB SORTCALL.
**
**
*/

#include "idsistd.h"
#include "idsisubs.h"
#include "wcommon.h"
#include "werrlog.h"
#include "wisplib.h"

/*
	SORTCALL	For UNIX and WIN32 this routine will call WISPSORT.  It will use the info from SORTINFO
			for the extra arguments.
*/
void SORTCALL(char* sortdata, int4 *retcode)
{
	char	filetype;
	int4	recsize;
	int4	*sortcode_ptr;

	WL_wtrace("SORTCALL","ENTRY","Entry into SORTCALL");

	WL_getsortinfo(&filetype, &recsize, &sortcode_ptr);

	WISPSORT(sortdata,&filetype,&recsize,sortcode_ptr,retcode);
	return;
}

/*
**	History:
**	$Log: sortcall.c,v $
**	Revision 1.16  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.15  2003/01/28 21:19:52  gsl
**	fix prototype
**	
**	Revision 1.14  2002/12/09 21:09:32  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.13  2002/07/18 21:04:28  gsl
**	Remove MSDOS code
**	
**	Revision 1.12  2002/07/10 21:05:25  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.11  2002/06/21 03:10:41  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.10  1996/08/19 22:32:57  gsl
**	drcs update
**	
**
**
*/
