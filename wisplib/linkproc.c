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
**	File:		linkproc.c
**
**	Project:	wisp/lib
**
**	Purpose:	To run a shell script and pass parameters
**
**	Routines:	
**	LINKPROC()
*/


/*	LINKPROC	Preforms a WANG style LINK to a procedure (DCL or UNIX shell) with parameters.
			This is a one-way passing of parameters, no pass-back is done.
			There is a limit of 8 parameters, and PARMCNT must be correct.

			ARGUMENTS

			file		char 8			The filename 
			library		char 8			The library	(default PROGLIB)
			volume		char 6			The volume	(default PROGVOL)
			parmcnt		int 2 (binary)		The number of parameters to follow
			parmlen-1	int 2 (binary)		The lenght of the first parameter
			parm-1		char variable		The first parameter
			parmlen-2	int 2 (binary)
			parm-2		char variable
			...
			returncode	int 4			The return code
			compcode	int 4			The exit code from the script. (Optional)

			RETURNCODE

			    0		SUCCESS
			   20		FILE NOT FOUND
			   28		ACCESS DENIED
			   52		INVALID PROC FILE (NOT A PROC)

			WERRLOG

			    2		parmcnt too large
			    3		file not found
			    4           exec failed
			    5		access denied
			    6		not yet implemented on VMS
			    7		invalid proc file
*/

/*
**	Includes
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>									/* Function uses variable params.	*/
#include <errno.h>
#include <string.h>

#ifdef unix
#include <ctype.h>
#include <signal.h>
#include <unistd.h>
#endif


#include "idsistd.h"
#include "vwang.h"
#include "wdefines.h"
#include "wperson.h"
#include "wfiles.h"
#include "wcommon.h"
#include "runtype.h"
#include "wfname.h"
#include "wisplib.h"
#include "level.h"
#include "sharemem.h"
#include "wexit.h"
#include "wispcfg.h"
#include "link.h"

#include "werrlog.h"
/*
28502	%%LINKPROC-F-PARMCNT Invalid parmcnt %d
28504	%%LINKPROC-F-EXEC Exec failed to %.60s [errno=%d]
*/

/*
**	Structures and Defines
*/

#define WNOTMTD 	 4
#define WVOLBUSY	 8
#define WDNOTFOUND 	16
#define WFNOTFOUND 	20
#define WLEXCEED 	24
#define WPERM 		28
#define WIOERR 		44
#define WINVPROG 	52
#define WNOMEM 		60

#define		MAX_LINKPROC_PARMS	8

/*
**	Globals and Externals
*/

/*
**	Static data
*/

/*
**	Function Prototypes
*/

#ifdef unix
static int fixerr(int code);
#endif



void LINKPROC(char* progname, char* libname, char* volname, short* parmcnt_ptr, ...)
{
	va_list		the_args;
	short		parmcnt;
	short		parmlen[MAX_LINKPROC_PARMS];
	char		parmtext[MAX_LINKPROC_PARMS][256];
	int4		*retcode, *compcode;
	char 		filespec[COB_FILEPATH_LEN+1];
	int4 		mode;
	int 		i;
	char 		*p;
	int 		ftyp;
	int		arg_count;
	int4		l_retcode, l_compcode;
	char		l_lib[9], l_vol[7];
#ifdef WIN32
	char		command[516];
	int  		cpos;
#endif
#ifdef unix
	int		pid;
#endif

	l_retcode = 0;
	l_compcode = 0;

	va_start(the_args, parmcnt_ptr);
	arg_count = WL_va_count();

	WL_wtrace("LINKPROC","ENTRY", "Proc=[%8.8s] Lib=[%8.8s] Vol=[%6.6s] Count=[%d] args=%d",
		progname, libname, volname, (int) *parmcnt_ptr,  arg_count);


	memset( l_vol, ' ', 6 );
	memset( l_lib, ' ', 8 );
	if (libname)
	{
		memcpy(l_lib,libname,8);
	}
	if (volname)
	{
		memcpy(l_vol,volname,6);
	}
	if (l_lib[0] == ' ') 
	{
		WL_get_defs(DEFAULTS_PL,l_lib);						/* Get the PROGLIB default.		*/
	}
	if (l_vol[0] == ' ') 
	{
		WL_get_defs(DEFAULTS_PV,l_vol);						/* Get the PROGVOL default.		*/
	}
	l_lib[8] = '\0';
	l_vol[6] = '\0';

	parmcnt = *parmcnt_ptr;								/* Get the parameter count.		*/
	arg_count -= 4;
	if (parmcnt > MAX_LINKPROC_PARMS || parmcnt < 0)
	{
		werrlog(WERRCODE(28502),parmcnt,0,0,0,0,0,0,0);
		return;
	}

	mode = 0;
	p = WL_wfname(&mode,l_vol,l_lib,progname,filespec);				/* expand the name 			*/
	*p = (char)0;

#if defined(WIN32)
	memset(command,' ',sizeof(command));						/* Init the command string.		*/
	strcpy(command,filespec);							/* Now add the name of the procedure.	*/
	cpos = strlen(filespec);							/* Find out the end of the string.	*/
#endif

	for( i=0; i<parmcnt;i++)
	{
		parmlen[i]   = *(va_arg(the_args, short*));				/* Get the parameter length.		*/
		memcpy(parmtext[i], va_arg(the_args, char*), parmlen[i]);		/* Get the parameter.			*/
		arg_count -= 2;
		parmtext[i][parmlen[i]] = '\0';
#if defined(WIN32)
		if (parmlen[i] > 0)
		{
			if ((cpos + parmlen[i] + 3) > 128)
			{
				WL_werrlog_error(WERRCODE(28508),"LINKPROC","CMD", 
					"Command line is too long");			/* Write an error, line too big.	*/
				wexit(-1);						/* BAD.					*/
			}

			command[cpos++] = ' ';						/* Insert a space.			*/
			command[cpos++] = '\"';						/* And Quotes.				*/
			memcpy(&command[cpos],parmtext[i],parmlen[i]);
			cpos += parmlen[i];
			command[cpos++] = '\"';						/* Close Quotes.			*/
			command[cpos] = (char)0;					/* NULL terminate command		*/
		}
#endif
	}

	retcode = va_arg(the_args, int4*);						/* The return code from the program.	*/
	arg_count--;
	compcode = 0;
	if ( arg_count == 1 )
	{
		compcode = va_arg(the_args, int4*);
	}

	va_end(the_args);


	if ( !fexists(filespec) )
	{
		WL_put_swap( retcode, 20 );			/* File Not Found			*/
		WL_wtrace("LINKPROC","NOTFOUND","File not found [%s]",filespec);
		return;
	}

	ftyp = WL_runtype(filespec);							/* get the run type			*/

	switch(ftyp)
	{
	case RUN_SHELL:
		/*  This is the only type allowed to proceed. */
		break;

	case RUN_ACCESS:
		{
			WL_put_swap( retcode, 28 );
			WL_wtrace("LINKPROC","ACCESS","Unable to access file [%s]",filespec);
			return;
		}
	case RUN_UNKNOWN:
		{
			WL_put_swap( retcode, 28 );		/* Access denied.			*/
			WL_wtrace("LINKPROC","UNKNOWN","Unknown runtype file [%s]",filespec);
			return;
		}

	default:
		{
			WL_put_swap( retcode, 52 );		/* Invalid file type.			*/
			WL_wtrace("LINKPROC","FILETYPE","Invalid file type for file [%s]",filespec);
			return;
		}
	}

	*retcode = 0;

#ifdef unix
	if (VL_vsharedscreen())								/* If screen is shared		*/
	{
		vwang_stty_save();							/* Save the current stty values		*/
	}
#endif /* unix */

	vwang_shut();									/* Reset the terminal.			*/

#ifdef unix
	if (VL_vsharedscreen())								/* If screen is shared			*/
	{
		/*
		**	Force the stty into a SANE state.
		**	If there has been no screen I/O then vwang_shut() will have done nothing.
		*/
		vwang_stty_sane();
	}

	signal(SIGCLD,  SIG_DFL);							/* Use Default DEATH-OF-CHILD signal	*/

	switch (pid = fork())
	{
		case 0:									/* is child process 			*/
		{
			char *sh_parm[17];
			int idx;

			WL_set_progdefs_env(l_vol,l_lib);				/* Set up PROGLIB and PROGVOL		*/
			WL_clear_progdefs();						/* Clear PROGLIB/VOL from symbol	*/

			memset(sh_parm,(char)0,sizeof(sh_parm));
			idx=0;
			sh_parm[idx++]=wispshellexe();					/* std argv[0] is progname 		*/
			sh_parm[idx++]=filespec;					/* name of shell script for /bin/sh 	*/
			for (i=0; i<parmcnt; i++)					/* start at elem 1 in parm_list, 	*/
				sh_parm[idx++]=parmtext[i];				/* elem 2 in sh_parm 			*/
			sh_parm[idx] = '\0';						/* null terminate it			*/

			WL_newlevel();							/* Increment the link-level		*/
			execvp(sh_parm[0],sh_parm);

			werrlog(WERRCODE(28504),filespec,errno,0,0,0,0,0,0);
			*retcode=fixerr(errno);
			exit(*retcode);
		}
		default:								/* is parent process 			*/
		{
			WL_wwaitpid(pid,&l_compcode);
			l_retcode = 0;
		}
	}

	signal(SIGCLD,  SIG_IGN);							/* Ignore DEATH-OF-CHILD signal		*/

	if (VL_vsharedscreen())								/* If screen is shared			*/
	{
		vwang_stty_restore();							/* Restore the saved stty values	*/
	}
#endif /* unix */

#if defined(WIN32)
	WL_newlevel();
	l_retcode = WL_wsystem(command);
	l_compcode = 0;
	WL_oldlevel();
#endif /*  WIN32 */

	vwang_synch();								/* Resync the video				*/
	WL_load_defaults();							/* Reload defaults in case they changed		*/
	vwang_set_reinitialize(TRUE);						/* Set so return from link re-inits screen	*/

	WL_ppunlink(WL_linklevel());						/* Putparm UNLINK			*/

	WL_put_swap( retcode, l_retcode );

	if ( compcode )
	{
		WL_put_swap( compcode, l_compcode );
	}
	return;

}

#ifdef unix
static int fixerr(int code)
{
	switch (code)
	{
		case EPERM: case EACCES:	
				return WPERM;
		case ENOENT: 	return WFNOTFOUND;
		case EIO:	return WIOERR;
		case ENXIO:	return WNOTMTD;
		case ENOEXEC:	return WINVPROG;
		case EAGAIN:	return WLEXCEED;
		case ENOMEM:	return WNOMEM;
		case EBUSY:	return WVOLBUSY;
		case ENOTDIR:	return WDNOTFOUND;
		default:	return code;
	}
}
#endif

/*
**	History:
**	$Log: linkproc.c,v $
**	Revision 1.37  2003/01/31 17:14:05  gsl
**	Fix Wall warnings and copyright
**	
**	Revision 1.36  2003/01/31 17:09:48  gsl
**	Fix Wall warnings and copyright
**	
**	Revision 1.35  2003/01/31 17:03:35  gsl
**	Fix Wall warnings and copyright
**	
**	Revision 1.34  2003/01/31 17:00:12  gsl
**	Fix Wall warnings and copyright
**	
**	Revision 1.33  2003/01/31 16:58:26  gsl
**	Fix Wall warnings and copyright
**	
**	Revision 1.32  2003/01/29 19:42:49  gsl
**	Fix -Wall warnings
**	
**	Revision 1.31  2003/01/20 18:34:02  gsl
**	Changed to use stdarg.h
**	
**	Revision 1.30  2002/12/11 20:33:37  gsl
**	Enhance tracing of runtype
**	
**	Revision 1.29  2002/12/10 20:54:13  gsl
**	use WERRCODE()
**	
**	Revision 1.28  2002/12/10 17:09:18  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.27  2002/12/09 21:09:28  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.26  2002/07/16 16:24:55  gsl
**	Globals
**	
**	Revision 1.25  2002/07/15 17:09:59  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.24  2002/07/12 19:10:13  gsl
**	Global unique WL_ changes
**	
**	Revision 1.23  2002/07/12 17:00:57  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.22  2002/07/11 20:29:10  gsl
**	Fix WL_ globals
**	
**	Revision 1.21  2002/07/11 15:21:43  gsl
**	Fix WL_ globals
**	
**	Revision 1.20  2002/07/10 21:05:18  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.19  2002/07/09 04:14:00  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.18  2002/06/21 20:49:29  gsl
**	Rework the IS_xxx bit flags and the WFOPEN_mode flags
**	
**	Revision 1.17  2002/06/21 03:10:37  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.16  1998/07/31 19:41:48  gsl
**	change NAME_LENGTH to COB_FILEPATH_LEN.
**	
**	Revision 1.15  1997-11-21 16:36:19-05  gsl
**	Fixed the VL_vsharedscreen() logic to work from help and add the sync logic.
**
**	Revision 1.14  1997-09-22 12:32:59-04  gsl
**	Change isdebug() to the more general VL_vsharedscreen()
**
**	Revision 1.13  1996-11-13 15:49:14-05  gsl
**	Changes for NT
**
**	Revision 1.12  1996-10-08 17:21:47-07  gsl
**	replace shell_var() with wispshellexe()
**
**	Revision 1.11  1996-07-15 10:06:22-07  gsl
**	Fixed for NT
**
**	Revision 1.10  1995-04-25 02:53:02-07  gsl
**	drcs state V3_3_15
**
 * Revision 1.9  1995/04/17  11:46:21  gsl
 * drcs state V3_3_14
 *
 * Revision 1.8  1995/03/10  14:07:51  gsl
 * fix headers
 *
 * Revision 1.7  1995/03/09  15:49:26  gsl
 * replace video calls with vwang calls
 *
**
**
*/
