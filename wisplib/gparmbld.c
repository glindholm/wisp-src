static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#include <string.h>
#include <varargs.h>
#include "idsistd.h"
#include "werrlog.h"
#include "wdefines.h"
#include "wisplib.h"

struct argst { char *ptrs[500]; };

static struct argst args;

void getparmbuild(va_alist)
va_dcl
{
#define		ROUTINE		21000

	va_list arg_list;
	int arg_count;
	int4 two=2;
	static int cnt=0, i=0;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say hi.				*/

	va_start(arg_list);
	arg_count = va_count(arg_list);
	va_start(arg_list);

	if (arg_count)
		for (;arg_count;)
		{
			args.ptrs[cnt] = va_arg(arg_list,char*);
			--arg_count;
			++cnt;
		}
	else
	{
		wvaset(&two);
		GETPARM(&args,&cnt);							/* Use the new method			*/

		cnt = 0;								/* RESET ALL STATICS.			*/
		i = 0;
		memset(&args,0,sizeof(args));
	}
}

/*
**	History:
**	$Log: gparmbld.c,v $
**	Revision 1.10  1996-08-19 18:32:22-04  gsl
**	drcs update
**
**
**
*/
