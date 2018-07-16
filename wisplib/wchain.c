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
#include "werrlog.h"
#include "wcommon.h"

#ifdef unix
#include "wrunconf.h"
#endif

#ifdef MSDOS
#include <stdio.h>
#include <process.h>
#include <errno.h>
#include <stdlib.h>
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
	long	mode;
	char	path[80];
$DESCRIPTOR(path_desc,path);

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
	LIB$RUN_PROGRAM( &path_desc );

	vsynch();									/* ReSync Video.			*/

	werrlog(ERRORCODE(3),path,errno,0,0,0,0,0,0);
	return;
}
#endif	/* VMS */

#ifdef unix

#define ISEXEC 0
#define ISACU  2

int WCHAIN( volname, libname, filename )						/* Chain to new program; NO RETURN.	*/
char	*volname, *libname, *filename;
{
#define		ROUTINE		70000
	char	*end_name;
	long	mode;
	extern	char	**environ;
	char	path[80], temp[80];
	int	ftyp;
	int	found;
	int	try_upper;

	werrlog(ERRORCODE(1),volname, libname, filename,0,0,0,0,0);

	found = 0;
	try_upper = 1;

	mode = IS_SUBMIT;	
	end_name = wfname( &mode, volname, libname, filename, path );	
	*end_name = '\0';

	strcpy(temp,path);
try_again:
	if ( 0 == access( temp, 00 ) )	found = 1;
	else
	{
		if ( ! osd_ext( temp ) )						/* if execl fails then try file.exe	*/
		{
			strcat( temp, ".exe" );
			if ( 0 == access( temp, 00 ) ) 
			{
				found = 1;
			}
		}
	}

	if (!found && try_upper)							/* Try uppercase			*/
	{
		int	ii,start;

		strcpy(temp,path);
		start = strlen(temp)-1;
		if ( osd_ext(temp) )							/* If there is an ext back over it	*/
		{
			for( ii=start; temp[ii] != '.'; ii-- );
			start--;
		}

		for( ii=start; ii>=0; ii-- )						/* Make file filename part uppercase.	*/
		{
			if (temp[ii] == '/') break;

			temp[ii] = toupper(temp[ii]);
		}
		try_upper = 0;

		goto try_again;
	}

	if (!found)
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

	vexit();									/* reset the terminal			*/

	if (ftyp == ISEXEC)
	{
		execle( path, path, (char *)0, environ );

		vsynch();								/* ReSync Video.			*/
	
		werrlog(ERRORCODE(6),path,errno,0,0,0,0,0,0);
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
		options[0] = '\0';
		if ( eptr = (char *)getenv(WRUNOPTIONS_ENV) )
		{
			strcpy(options,eptr);
		}
		else
		{
			strcpy(options,cfg.wrun_options);
		}

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

#define ISEXEC 0
#define ISACU  2

void WCHAIN( volname, libname, filename )						/* Chain to new program; NO RETURN.	*/
char	*volname, *libname, *filename;
{
#define		ROUTINE		70000
	char	*end_name;
	long	mode;
	char	path[80], temp[80];
	int	ftyp;
	int	found;
	int	try_upper;

	werrlog(ERRORCODE(1),volname, libname, filename,0,0,0,0,0);

	found = 0;
	try_upper = 1;

	mode = IS_SUBMIT;	
	end_name = wfname( &mode, volname, libname, filename, path );	
	*end_name = '\0';

	strcpy(temp,path);
try_again:
	if ( 0 == access( temp, 00 ) )	found = 1;
	else
	{
		if ( ! osd_ext( temp ) )						/* if execl fails then try file.exe	*/
		{
			strcat( temp, ".exe" );
			if ( 0 == access( temp, 00 ) ) 
			{
				found = 1;
			}
		}
	}

	if (!found && try_upper)							/* Try uppercase			*/
	{
		int	ii,start;

		strcpy(temp,path);
		start = strlen(temp)-1;
		if ( osd_ext(temp) )							/* If there is an ext back over it	*/
		{
			for( ii=start; temp[ii] != '.'; ii-- );
			start--;
		}

		for( ii=start; ii>=0; ii-- )						/* Make file filename part uppercase.	*/
		{
			if (temp[ii] == '/') break;

			temp[ii] = toupper(temp[ii]);
		}
		try_upper = 0;

		goto try_again;
	}

	if (!found)
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

	vexit();									/* reset the terminal			*/

	if (ftyp == ISEXEC)
	{
		execle( path, path, (char *)0, environ );

		vsynch();								/* ReSync Video.			*/
	
		werrlog(ERRORCODE(6),path,errno,0,0,0,0,0,0);
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
		options[0] = '\0';
		if ( eptr = (char *)getenv(WRUNOPTIONS_ENV) )
		{
			strcpy(options,eptr);
		}
		else
		{
			strcpy(options,cfg.wrun_options);
		}

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
		return;
	}
}

			/* As of 17-JUL-91, if this lowercase version is included, the MSDOS LIB routine gives the warning:	*/
			/* WISP.bak(wchain) : warning U4151: '_wchain' : symbol defined in module wchain, redefinition ignored	*/
#ifdef	NOT_YET		/* It has been removed until this warning is resolved.							*/

int wchain( volname, libname, filename )						/* Lower case equivalent.		*/
char	*volname, *libname, *filename;
{
	WCHAIN( volname, libname, filename );
}
#endif	/* #ifdef NOT_YET */

#endif	/* #ifdef MSDOS */
