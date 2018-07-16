			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


#include "cobrun.h"

COBLINK(progname)
char	*progname;									/* 8 char file name			*/
{
	long	four=4L;								/* 4 byte long holding value "4".	*/
	long	compcode,retcode;

	compcode=0;
	retcode=0;

	wvaset(&four);
	if (acu_cobol)								/* If ACU then pass in the lengths.		*/
	{
		LINK(progname,8, " ",1, &compcode,4, &retcode,4);
	}
	else
	{
		LINK(progname,   " ",   &compcode,   &retcode);
	}

	wswap(&compcode);
	wswap(&retcode);

	if ( compcode == 8 && retcode == 20 )					/* If not found then try TYPE = S		*/
	{
		wvaset(&four);
		if (acu_cobol)
		{
			LINK(progname,8, "S",1, &compcode,4, &retcode,4);
		}
		else
		{
			LINK(progname,   "S",   &compcode,   &retcode);
		}
	}
}



#ifdef OLDVMS
#include <varargs.h>									/* Function uses variable params.	*/
#include <v/video.h>

#include "wfiles.h"
#include "wcommon.h"
#include "werrlog.h"

/* perform a WANG style COBLINK call												*/

struct 	{
		char *the_parm[16];							/* A place to put the parameter list.	*/
	} parm_list;

extern int lnk_depth;

COBLINK(va_alist)									/* There are a variable number of args.	*/
va_dcl
{
#define		ROUTINE		10000

	va_list the_args;

	register int i;
	int  to_do,retval;
	char *progname;
	long compcode,retcode;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we're here.			*/

#ifdef OLDunix
	werrlog(ERRORCODE(2),0,0,0,0,0,0,0,0);						/* Not yet implemented.			*/
	return;
#endif
	va_start(the_args);								/* Find out how many args inthe stack.	*/
	to_do = va_count(the_args);
	va_start(the_args);

	progname = va_arg(the_args, char*);						/* Get the program name.		*/
	to_do--;

	if (!memcmp(progname,"DISPLAY ",8))						/* If they are calling DISPLAY, then	*/
	{										/* call WFILE_DISP.			*/
		setprogid(progname);
		wfile_disp();
		return;
	}

	i = 0;
	while (to_do--)
	{
		parm_list.the_parm[i++] = va_arg(the_args, char*);
	}

	va_end(the_args);

	lnk_depth++;									/* Increment the depth.			*/

	switch(lnk_depth)
	{
		case 1:
		{
			wclink1(progname,parm_list,&retval);				/* Call the COBOL link.			*/
			break;
		}
		case 2:
		{
			wclink2(progname,parm_list,&retval);				/* Call the COBOL link.			*/
			break;
		}
		case 3:
		{
			wclink3(progname,parm_list,&retval);				/* Call the COBOL link.			*/
			break;
		}
		case 4:
		{
			wclink4(progname,parm_list,&retval);				/* Call the COBOL link.			*/
			break;
		}
		case 5:
		{
			wclink5(progname,parm_list,&retval);				/* Call the COBOL link.			*/
			break;
		}
		case 6:
		{
			wclink6(progname,parm_list,&retval);				/* Call the COBOL link.			*/
			break;
		}
		case 7:
		{
			wclink7(progname,parm_list,&retval);				/* Call the COBOL link.			*/
			break;
		}
		case 8:
		{
			wclink8(progname,parm_list,&retval);				/* Call the COBOL link.			*/
			break;
		}
		case 9:
		{
			wclink9(progname,parm_list,&retval);				/* Call the COBOL link.			*/
			break;
		}
		case 10:
		{
			wclink10(progname,parm_list,&retval);				/* Call the COBOL link.			*/
			break;
		}
		case 11:
		{
			wclink11(progname,parm_list,&retval);				/* Call the COBOL link.			*/
			break;
		}
		case 12:
		{
			wclink12(progname,parm_list,&retval);				/* Call the COBOL link.			*/
			break;
		}
		case 13:
		{
			wclink13(progname,parm_list,&retval);				/* Call the COBOL link.			*/
			break;
		}
		case 14:
		{
			wclink14(progname,parm_list,&retval);				/* Call the COBOL link.			*/
			break;
		}
		case 15:
		{
			wclink15(progname,parm_list,&retval);				/* Call the COBOL link.			*/
			break;
		}
		case 16:
		{
			wclink16(progname,parm_list,&retval);				/* Call the COBOL link.			*/
			break;
		}

	}

	lnk_depth--;

	if (retval == 1) return;							/* It was a success.			*/

	progname[8] = '\0';
	compcode=0;
	retcode=0;

	LINK(progname, " ", &compcode, &retcode);
}
#endif
