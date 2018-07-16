static char copyright[]="Copyright (c) 1995-1998 NeoMedia Technologies, All rights reserved.";
static char rcsid[]="$Id:$";

#ifdef VMS
#include <descrip.h>
#endif

#include "idsistd.h"
#include "werrlog.h"
#include "wcommon.h"
#include "wdefines.h"
#include "wfname.h"
#include "filext.h"
#include "runtype.h"

#ifdef unix
#include "wrunconf.h"
#endif


#ifdef VMS
#define WCHAIN_DEFINED

void WCHAIN( volname, libname, filename )						/* Chain to new program; NO RETURN.	*/
char	*volname, *libname, *filename;
{
#define		ROUTINE		70000
	char	*end_name;
	int4	mode;
	int	savelevel;
	char	path[80];
#include "wchain.d"

	werrlog(ERRORCODE(1),volname, libname, filename,0,0,0,0,0);

	if (WISPFILEXT[0] == ' ' || WISPFILEXT[0] == '\0')
	{
		setwispfilext("EXE");
	}
	mode = IS_SUBMIT;	
	end_name = wfname( &mode, volname, libname, filename, path );	
	*end_name = '\0';

	path_desc.dsc$w_length = strlen(path);
	vwang_shut();
	savelevel = linklevel();						/* Save the link-level in case RUN fails	*/
	zerolevel();								/* Set link-level to zero.			*/
	LIB$RUN_PROGRAM( &path_desc );

	/*
	**	If we get to this point then the whain() has failed.
	*/
	setlevel(savelevel);							/* Restore link-level.				*/
	vwang_synch();								/* ReSync Video.				*/

	werrlog(ERRORCODE(3),path,errno,0,0,0,0,0,0);
	return;
}
#endif	/* VMS */

#ifdef unix
#define WCHAIN_DEFINED

void WCHAIN( volname, libname, filename )						/* Chain to new program; NO RETURN.	*/
char	*volname, *libname, *filename;
{
#define		ROUTINE		70000
	char	*end_name;
	int4	mode;
	extern	char	**environ;
	char	path[80], temp[80];
	int	ftyp;
	int	savelevel;

	werrlog(ERRORCODE(1),volname, libname, filename,0,0,0,0,0);

	mode = IS_SUBMIT;	
	end_name = wfname( &mode, volname, libname, filename, path );	
	*end_name = '\0';

	strcpy(temp,path);
	if ( !fexists(temp) )
	{
		werrlog(ERRORCODE(2),path,0,0,0,0,0,0,0);
		return;
	}

	strcpy(path,temp);

	ftyp = isexec(path);
	if ( ftyp != ISEXEC && ftyp != ISACU )
	{
		werrlog(ERRORCODE(4),path,0,0,0,0,0,0,0);
		return;
	}

	vwang_shut();								/* reset the terminal				*/

	savelevel = linklevel();						/* Save the link-level in case RUN fails	*/
	zerolevel();								/* Set link-level to zero.			*/

	if (ftyp == ISEXEC)
	{
		execle( path, path, (char *)0, environ );

		vwang_synch();							/* ReSync Video.				*/
	
		werrlog(ERRORCODE(6),path,errno,0,0,0,0,0,0);
		setlevel(savelevel);						/* Restore the link-level			*/
		return;
	}

	if (ftyp == ISACU)
	{
		struct wruncfg cfg;
		char	options[sizeof(cfg.wrun_options)];
		char	*eptr, *optr;
		int	arg;
		char	*sh_parm[64];

		wrunconfig(&cfg);

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

		werrlog(ERRORCODE(8),cfg.wrun_runcbl,options,path,errno,0,0,0,0);
		setlevel(savelevel);						/* Restore the link-level			*/
		return;
	}
}

void wchain( volname, libname, filename )						/* Lower case equivalent.		*/
char	*volname, *libname, *filename;
{
	WCHAIN( volname, libname, filename );
}
#endif	/* unix */

#ifndef WCHAIN_DEFINED
void WCHAIN( volname, libname, filename )
char	*volname, *libname, *filename;
{
#define		ROUTINE		70000

	werrlog(ERRORCODE(1),volname, libname, filename,0,0,0,0,0);
	werrlog(102,"(WCHAIN) Function NOT-SUPPORTED",0,0,0,0,0,0,0);
}
#endif	/* not defined */

/*
**	History:
**	$Log: wchain.c,v $
**	Revision 1.15  1998-10-14 14:02:42-04  gsl
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
