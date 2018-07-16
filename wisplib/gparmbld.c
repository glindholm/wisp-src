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


#include <string.h>
#include <varargs.h>
#include "idsistd.h"
#include "werrlog.h"
#include "wdefines.h"
#include "wisplib.h"
#include "vssubs.h"



void getparmbuild(va_alist)
va_dcl
{
	va_list arg_list;
	static char* gp_args[GETPARM_MAX_ARGS];
	static int cnt=0;
	int arg_count;
	int i;

	va_start(arg_list);
	arg_count = WL_va_count();

	WL_wtrace("GETPARMBUILD","ENTRY","Entry into GETPARMBUILD arg_count=%d prior_cnt=%d total=%d",
		arg_count, cnt, arg_count+cnt);

	if (arg_count > 0)
	{
		for (i=0; i<arg_count; i++)
		{
			gp_args[cnt] = va_arg(arg_list,char*);
			++cnt;
		}
	}
	else /* arg_count == 0 */
	{
		/*
		**	Called with no args means we have all the args
		**	so do the CALL "GETPARM".
		*/
		GETPARM2(gp_args,cnt);							/* Use the new method			*/

		/*
		**	Reset
		*/
		for (i=0; i<cnt; i++)
		{
			gp_args[i] = NULL;
		}
		cnt = 0;								/* RESET ALL STATICS.			*/
	}
}

/*
**	History:
**	$Log: gparmbld.c,v $
**	Revision 1.17  2003/02/19 22:16:13  gsl
**	Add GETPARM2() the 2 arg interface to GETPARM()
**	
**	Revision 1.16  2003/02/17 22:07:17  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.15  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.14  2002/12/09 21:09:28  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.13  2002/07/16 16:24:55  gsl
**	Globals
**	
**	Revision 1.12  2002/07/12 17:00:56  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.11  2002/07/11 20:29:09  gsl
**	Fix WL_ globals
**	
**	Revision 1.10  1996/08/19 22:32:22  gsl
**	drcs update
**	
**
**
*/
