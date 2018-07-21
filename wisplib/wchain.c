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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef unix
#include <unistd.h>
#endif


#include "idsistd.h"
#include "wisplib.h"
#include "werrlog.h"
#include "wcommon.h"
#include "wdefines.h"
#include "wfname.h"
#include "filext.h"
#include "runtype.h"
#include "level.h"
#include "vwang.h"

#ifdef unix
#include "wrunconf.h"
#endif


#ifdef unix
#define WCHAIN_DEFINED

void WCHAIN( volname, libname, filename )						/* Chain to new program; NO RETURN.	*/
char	*volname, *libname, *filename;
{
	char	*end_name;
	int4	mode;
	extern	char	**environ;
	char	path[80], temp[80];
	int	ftyp;
	int	savelevel;

	WL_wtrace("WCHAIN","ENTRY",
		"Entry into WCHAIN vol(%6.6s) lib(%8.8s) file(%8.8s)",
		volname, libname, filename);

	mode = 0;	
	end_name = WL_wfname( &mode, volname, libname, filename, path );	
	*end_name = '\0';

	strcpy(temp,path);
	if ( !WL_fexists(temp) )
	{
		werrlog(WERRCODE(70002),path,0,0,0,0,0,0,0);
		return;
	}

	strcpy(path,temp);

	ftyp = WL_isexec(path);
	if ( ftyp != ISEXEC && ftyp != ISACU )
	{
		werrlog(WERRCODE(70004),path,0,0,0,0,0,0,0);
		return;
	}

	vwang_shut();								/* reset the terminal				*/

	savelevel = WL_linklevel();						/* Save the link-level in case RUN fails	*/
	WL_zerolevel();								/* Set link-level to zero.			*/

	if (ftyp == ISEXEC)
	{
		execle( path, path, (char *)0, environ );

		vwang_synch();							/* ReSync Video.				*/
	
		werrlog(WERRCODE(70006),path,errno,0,0,0,0,0,0);
		WL_setlevel(savelevel);						/* Restore the link-level			*/
		return;
	}

	if (ftyp == ISACU)
	{
		struct wruncfg cfg;
		char	options[sizeof(cfg.wrun_options)];
		char	*optr;
		int	arg;
		char	*sh_parm[64];

		WL_wrunconfig(&cfg);

		strcpy(options,cfg.wrun_options);

		arg=0;
		sh_parm[arg++] = cfg.wrun_runcbl;
					
		for( optr=options; *optr; optr++ )
		{
			for(;*optr==' ';optr++);				/* Scan til first non-space 	*/
			if (! *optr ) break;
					
			sh_parm[arg++] = optr;					/* Point to option		*/
			
			for(;*optr && *optr != ' ';optr++);			/* Scan til space.		*/
			if (! *optr ) break;
					
			*optr = '\0';						/* Null terminate the option	*/
		}				

		sh_parm[arg++] = path;						/* The program	to run 		*/
		sh_parm[arg++] = '\0';						/* null terminate it		*/

		execvp(sh_parm[0],sh_parm);

		vwang_synch();								/* ReSync Video.			*/

		werrlog(WERRCODE(70008),cfg.wrun_runcbl,options,path,errno,0,0,0,0);
		WL_setlevel(savelevel);						/* Restore the link-level			*/
		return;
	}
}
#endif	/* unix */

#ifndef WCHAIN_DEFINED
void WCHAIN( volname, libname, filename )
char	*volname, *libname, *filename;
{
	WL_werrlog_error(WERRCODE(70000),"WCHAIN", "ENTRY", 
		"WCHAIN routine is NOT SUPPORTED");
}
#endif	/* not defined */

/*
**	History:
**	$Log: wchain.c,v $
**	Revision 1.30  2003/06/23 15:01:52  gsl
**	Removed lowercase wchain() api
**	
**	Revision 1.29  2003/03/19 21:11:44  gsl
**	Remove USE_PVPL flag
**	
**	Revision 1.28  2003/03/12 18:18:10  gsl
**	FIx -Wall warnings
**	
**	Revision 1.27  2003/02/04 16:02:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.26  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.25  2002/12/10 21:59:21  gsl
**	fis WERRCODE number
**	
**	Revision 1.24  2002/12/10 20:54:09  gsl
**	use WERRCODE()
**	
**	Revision 1.23  2002/12/09 21:09:33  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.22  2002/12/09 19:15:36  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.21  2002/07/12 19:10:18  gsl
**	Global unique WL_ changes
**	
**	Revision 1.20  2002/07/11 15:21:44  gsl
**	Fix WL_ globals
**	
**	Revision 1.19  2002/07/10 21:05:29  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.18  2002/07/09 04:13:55  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.17  2002/06/21 20:49:30  gsl
**	Rework the IS_xxx bit flags and the WFOPEN_mode flags
**	
**	Revision 1.16  2002/06/21 03:10:43  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.15  1998/10/14 18:02:42  gsl
**	Add include of runtype.h
**	
**	Revision 1.14  1997-03-12 13:18:04-05  gsl
**	CHange to use a standard not-defined routine if WCHAIN_DEFINED is not defined
**
**	Revision 1.13  1997-02-25 09:51:53-05  gsl
**	Correct options size
**
**	Revision 1.12  1996-08-19 18:33:08-04  gsl
**	drcs update
**
**
**
*/
