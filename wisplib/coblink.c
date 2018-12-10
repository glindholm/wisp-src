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


#include "idsistd.h"
#include "wisplib.h"
#include "werrlog.h"
#include "vssubs.h"

void COBLINK(const char *progname)
{
	int4	compcode,retcode;

	WL_wtrace("COBLINK","ENTRY","Entry into COBLINK(%8.8s)", progname);

	compcode=0;
	retcode=0;

	WL_set_va_count(4);
	LINK2(progname, (int4) 8, " ", (int4) 1, &compcode, (int4) 4, &retcode, (int4) 4);

	WL_wswap(&compcode);
	WL_wswap(&retcode);

	if ( compcode == 8 && retcode == 20 )					/* If not found then try TYPE = S		*/
	{
		WL_set_va_count(4);
		LINK2(progname, (int4) 8, "S", (int4) 1, &compcode, (int4) 4, &retcode, (int4) 4);
	}
}

/*
**	History:
**	$Log: coblink.c,v $
**	Revision 1.16  2003/02/17 22:07:18  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.15  2003/01/31 17:23:48  gsl
**	Fix  copyright header
**	
**	Revision 1.14  2002/12/09 21:09:27  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.13  2002/10/18 19:14:12  gsl
**	Cleanup
**	
**	Revision 1.12  2002/07/12 17:00:54  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.11  1997/10/23 20:06:16  gsl
**	Make progname a "const"
**	
**	Revision 1.10  1996-08-19 18:32:13-04  gsl
**	drcs update
**
**
**
*/
