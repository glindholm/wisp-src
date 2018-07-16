#ifdef unix
#include <varargs.h>
#include "werrlog.h"
#include "wdefines.h"

struct argst { char *ptrs[500]; };

static struct argst args;

getparmbuild(va_alist)
va_dcl
{
#define		ROUTINE		21000

	va_list arg_list;
	int arg_count;
	long two=2L;
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
#endif	/* unix */

#ifdef MSDOS

#include <string.h>
#include <memory.h>
#include <varargs.h>

#include "werrlog.h"
#include "wdefines.h"

struct argst { char *ptrs[500]; };

static struct argst args;

getparmbuild(va_alist)
va_dcl
{
#define		ROUTINE		21000

	va_list arg_list;
	int arg_count;
	long two=2L;
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
#endif	/* MSDOS */
