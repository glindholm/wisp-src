			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#ifdef VMS
#include <descrip.h>
#endif

#include <v/video.h>
#include "idsistd.h"
#include "werrlog.h"
#include "wcommon.h"
#include "wdefines.h"

#ifdef unix
#include "wrunconf.h"
#endif

extern char WISPFILEXT[39];
char *wfname();


#ifdef VMS
int WCHAIN( volname, libname, filename )						/* Chain to new program; NO RETURN.	*/
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
	vexit();
	savelevel = linklevel();						/* Save the link-level in case RUN fails	*/
	zerolevel();								/* Set link-level to zero.			*/
	LIB$RUN_PROGRAM( &path_desc );

	/*
	**	If we get to this point then the whain() has failed.
	*/
	setlevel(savelevel);							/* Restore link-level.				*/
	vsynch();								/* ReSync Video.				*/

	werrlog(ERRORCODE(3),path,errno,0,0,0,0,0,0);
	return;
}
#endif	/* VMS */

#ifdef unix

int WCHAIN( volname, libname, filename )						/* Chain to new program; NO RETURN.	*/
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

	vexit();								/* reset the terminal				*/

	savelevel = linklevel();						/* Save the link-level in case RUN fails	*/
	zerolevel();								/* Set link-level to zero.			*/

	if (ftyp == ISEXEC)
	{
		execle( path, path, (char *)0, environ );

		vsynch();							/* ReSync Video.				*/
	
		werrlog(ERRORCODE(6),path,errno,0,0,0,0,0,0);
		setlevel(savelevel);						/* Restore the link-level			*/
		return;
	}

	if (ftyp == ISACU)
	{
		struct wruncfg cfg;
		char	options[80];
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

		vsynch();								/* ReSync Video.			*/

		werrlog(ERRORCODE(8),cfg.wrun_runcbl,options,path,errno,0,0,0,0);
		setlevel(savelevel);						/* Restore the link-level			*/
		return;
	}
}

int wchain( volname, libname, filename )						/* Lower case equivalent.		*/
char	*volname, *libname, *filename;
{
	WCHAIN( volname, libname, filename );
}
#endif	/* unix */

#ifdef MSDOS

void WCHAIN( volname, libname, filename )						/* Chain to new program; NO RETURN.	*/
char	*volname, *libname, *filename;
{
#define		ROUTINE		70000

	werrlog(ERRORCODE(1),volname, libname, filename,0,0,0,0,0);
	werrlog(102,"(WCHAIN) Function NOT-SUPPORTED",0,0,0,0,0,0,0);
}

#endif	/* MSDOS */
