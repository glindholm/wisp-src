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


#include <string.h>
#include <stdarg.h>
#include "idsistd.h"
#include "werrlog.h"
#include "wdefines.h"
#include "wisplib.h"
#include "vssubs.h"



void getparmbuild(char* arg0, ...)
{
	va_list arg_list;
	static char* gp_args[GETPARM_MAX_ARGS];
	static int cnt=0;
	int arg_count;
	int i;

	arg_count = WL_va_count();

	WL_wtrace("GETPARMBUILD","ENTRY","Entry into GETPARMBUILD arg_count=%d prior_cnt=%d total=%d",
		arg_count, cnt, arg_count+cnt);

	if (arg_count > 0)
	{
		/* arg0 will be invalid if arg_count==0 */
		va_start(arg_list, arg0);

		/*
		 * To support <stdarg.h> (instead of older <vargars.h>)
		 * which doesn't support no-args:
		 * The first arg is "hardcoded" as arg0 
		 */
		gp_args[cnt++] = arg0;

		for (i=0; i<arg_count-1; i++)
		{
			gp_args[cnt++] = va_arg(arg_list,char*);
		}
		va_end(arg_list);
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
**	Revision 1.18  2005/06/13 18:43:38  gsl
**	Change to use stdarg.h
**	
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
