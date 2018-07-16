/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
******************************************************************************
*/

/*
**	File:		wrun.c
**
**	Project:	wisp/utils
**
**	RCS:		$Source:$
**
**	Purpose:	Frontend to COBOL runtime system
**
**	On WIN32 two versions can be built; a windows version and a console version.
**	The windows version is the default and is used so that an unwanted console
**	windows isn't displayed before the acucobol runtime starts. Windows apps
**	can not inherit consoles from console apps so normally we want this to be
**	a windows application.  However, if using the NT telnet support with a
**	console version of the Acucobol runtime then you need to use WRUNT.EXE 
**	the console version of wrun.  To build WRUNT.EXE define WRUNT and don't
**	include win32wrn.c in the build.
*/

/*
	wrun	Wrun is the frontend to ACUCOBOL, Micro Focus COBOL/2 and AIX VS COBOL runtime.
		Wrun will take any options and save them in a shell variable for use with LINK and SUBMIT.
		If no options are specified it will use any options specified in the config file $WISPCONFIG/wrunconfig.
		In $WISPCONFIG/wrunconfig you may also alias runcbl.
		If the option "-?" is used it will display the full command that it would issue instead of doing it.
		If the option "--" is used it will issue the command with NO options.
		If no options, program or parameters are included it will display the default runcbl and options.
		If the USING clause is specified then the ACUUSING frontend is used for ACUCOBOL.
		All of the options apply only to ACUCOBOL

	wrun [options] [program] [USING] [parameters]

		options
		=======
		-# nnn
		-nnn
		-A aaa
		-B
		-C ccc 
		-D
		-E eee
		+E eee
		-I iii
		-K kkk
		-L
		-O ooo
		+O ooo
		-R rrr
		-S
		-V
		-W
		-X
		-Y yyy

*/


/*
**	Includes
*/


#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include <signal.h>
#include <string.h>
#include <stdarg.h>

#ifdef unix
#include <unistd.h>
#endif

#if defined(WIN32)
#include <process.h>
#include <io.h>
#endif

#include "wrunconf.h"
#include "wdefines.h"
#include "link.h"
#include "wcommon.h"
#include "wispvers.h"
#include "idsisubs.h"
#include "wisplib.h"
#include "wispcfg.h"
#include "platsubs.h"

/*
**	Structures and Defines
*/

#define MAX_EXEC_PARMS	64

/*
**	Globals and Externals
*/
void wrun_message_box(char *line1, char *line2);

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/

void msgprintf(const char* format, ...);

#ifdef WIN32
#ifndef WRUNT
#define WRUNMAIN
#endif
#endif

#ifdef WRUNMAIN
int wrunmain	(int argc, char *argv[])
#else
int main	(int argc, char *argv[])
#endif
{
	char	program[80];
	int	i;
	int	arg;
	int	addspace;
	struct	wruncfg	cfg;
	char	options[sizeof(cfg.wrun_options)];
	int	parg;
	int	parmcnt;
	char	*optr;
	char	*parm[MAX_EXEC_PARMS];
	int	showit;
	int	nooptions;
	int	usingclause, parm_file_written;
	char	argcount_string[20];
	int	acu_cobol, aix_cobol, mf_cobol;
	int	rc;
#ifdef unix
	int	pid;
#endif

	struct 	str_parm parm_list;
	struct 	str_len len_list;
	char	linkkey[80];								/* Key to link parm area		*/
	char	buff[128];

#ifdef WIN32
#ifndef WRUNT
	if (wisptelnet())
	{
		printf("\nThis is the Windows version of WRUN.\n"
		       "It can not be run within a telnet session.\n"
		       "Use the console version WRUNT.EXE when in telnet.\n");
		return 1;
	}
#endif
#endif

	if (0!=access(wispconfigdir(),00))
	{
		msgprintf( "%%WRUN-W-WISPCONFIG Warning WISPCONFIG=%s Directory not found.\n",wispconfigdir());
	}


	parm_file_written = 0;

#ifdef TEST
	for(i=0; i<argc; i++)
	{
		printf("i=%2d  argv[i]=<%s>\n",i,argv[i]);
	}
#endif

	showit = 0;
	if (argc == 1) showit=1;						/* If only one arg then just show the default.	*/

	nooptions = 0;								/* Default to passing options (unless --)	*/

	/*
		Build up options string
	*/

	options[0] = '\0';
	arg = 1;
	addspace = 0;
	for(arg=1;arg<argc;)
	{
		if ( argv[arg][0] != '-' && argv[arg][0] != '+' ) break;	/* Not an option so were done.			*/

		if ( argv[arg][1] == '?' )					/* -? Only show the command.			*/
		{
			showit = 1;
			arg++;
		}
		else if ( argv[arg][1] == '-' )					/* -- Use no options.				*/
		{
			nooptions = 1;
			arg++;
		}
		else
		{
			if ( addspace )
			{
				strcat(options, " ");
				addspace = 0;
			}

			strcat(options, argv[arg]);
			addspace = 1;

			switch( toupper(argv[arg++][1]) )			/* ACU options that require a parameter		*/
			{
				case '#':
				case 'A':
				case 'C':
				case 'E':
				case 'K':
				case 'I':
				case 'O':
				case 'R':
				case 'Y':
				{
					strcat(options," ");
					strcat(options,argv[arg++]);		/* Get the parameter.				*/
					break;
				}
			}
		}
	}

	if (nooptions) options[0] = '\0';					/* If -- then null the options string		*/

#ifdef TEST
	printf("options=<%s>\n",options);
#endif

	/*
		Extract the program
	*/

	if ( arg < argc )
	{
		strcpy(program,argv[arg++]);
	}
	else
	{
		program[0] = '\0';
	}

#ifdef TEST
	printf("program=<%s>\n",program);
#endif

	/*
		Test for a USING clause
	*/

	usingclause = 0;
	if (arg < argc)
	{
		char buff[80];
		strcpy(buff,argv[arg]);
		upper_string(buff);
		if (strcmp(buff,"USING") == 0)
		{
			usingclause = 1;
			arg++;
		}
	}

	/*
		Mark where the parameters start and get a count.
	*/

	parg = arg;
	parmcnt = 0;
	while( arg < argc )
	{
		parmcnt += 1;
		arg++;
	}


	/*
		Save options in shell variable
	*/

	if (options[0])								/* Save cmd options for later use		*/
	{
		char	envstring[128], *env_ptr;
		int	msize;

		sprintf(envstring,"%s=%s",WRUNOPTIONS_ENV,options);
		msize = strlen(envstring)+1;
		env_ptr = (char *)malloc( msize );
		if ( ! env_ptr )
		{
			msgprintf( "%%WRUN-F-MALLOC Malloc of %d bytes failed.\n",msize);
			exit(2);
		}
		strcpy(env_ptr,envstring);
		if ( putenv(env_ptr) )
		{
			msgprintf( "%%WRUN-F-PUTENV Unable to put environment varible %s.\n",WRUNOPTIONS_ENV );
			exit(4);
		}

#ifdef TEST
		{
			char	*ptr;
			ptr = getenv(WRUNOPTIONS_ENV);
			printf("%s=<%s>\n",WRUNOPTIONS_ENV,ptr);
		}
#endif
	}

	/*
		Load wrun config file info
	*/

	wrunconfig(&cfg);

#ifdef TEST
	printf("cfg.wrun_options=<%s>\n",cfg.wrun_options);
	printf("cfg.wrun_runcbl =<%s>\n",cfg.wrun_runcbl);
	printf("cfg.wrun_cobtype=<%s>\n",cfg.wrun_cobtype);
#endif

	acu_cobol = (strcmp(cfg.wrun_cobtype,"ACU")==0);
	aix_cobol = (strcmp(cfg.wrun_cobtype,"AIX")==0);
	mf_cobol  = (strcmp(cfg.wrun_cobtype,"MF" )==0);

	if (!options[0] && !nooptions)
	{
		strcpy(options,cfg.wrun_options);
	}

#ifdef TEST
	printf("options=<%s>\n",options);
#endif

	/*
		Build the parameter list for exec

		1) The RTS name. i.e. wruncbl
		2) The OPTIONS.  i.e. -b
	*/	

	arg=0;
	parm[arg++] = cfg.wrun_runcbl;
	
	for( optr=options; *optr; optr++ )
	{
		for(;*optr==' ';optr++);		/* Scan til first non-space 	*/
		if (! *optr ) break;

		parm[arg++] = optr;			/* Point to option		*/

		for(;*optr && *optr != ' ';optr++);	/* Scan til space.		*/
		if (! *optr ) break;

		*optr = '\0';				/* Null terminate the option	*/
	}

	/*
		ACU with USING 		(RTS) (OPTIONS) ACUUSING program count arg1 arg2 arg3 ... 
		    without		(RTS) (OPTIONS)          program      [arg1 arg2 arg3 ...]

		MF  with USING		(RTS) (OPTIONS) MFLINK   RUNUSING
		    without		(RTS) (OPTIONS)          program      [arg1 arg2 arg3 ...]

		??? with USING		*** ERROR ***
		    without		(RTS) (OPTIONS)          program      [arg1 arg2 arg3 ...]
	*/
	
	if (acu_cobol)
	{
		if (usingclause)
		{
			parm[arg++] = "ACUUSING";				/* Add ACUUSING to command line			*/
		}

		parm[arg++] = program;						/* Add program name to command line		*/

		if (usingclause)
		{
			sprintf(argcount_string,"%02d",argc-parg);
			parm[arg++] = argcount_string;				/* Add count to command line			*/
		}

		for(i=parg;i < argc; i++)
		{
			parm[arg++] = argv[i];					/* Add args to command line			*/
		}
	}
	else if (aix_cobol || mf_cobol)
	{
		if (usingclause)
		{
			parm[arg++] = "MFLINK";					/* Add MFLINK to command line			*/
			parm[arg++] = "RUNUSING";				/* Add RUNUSING to command line			*/
		}
		else
		{
			parm[arg++] = program;					/* Add program name to command line		*/

			for(i=parg;i < argc; i++)
			{
				parm[arg++] = argv[i];				/* Add args to command line			*/
			}
		}
	}
	else
	{
		if (usingclause)
		{
			msgprintf( "%%WRUN-F-COBOL Unknown COBOL type [%s]\n",cfg.wrun_cobtype);
			exit(6);
		}
		else
		{
			parm[arg++] = program;					/* Add program name to command line		*/

			for(i=parg;i < argc; i++)
			{
				parm[arg++] = argv[i];				/* Add args to command line			*/
			}
		}
	}
	parm[arg++] = '\0';

	if (!showit && usingclause && (aix_cobol || mf_cobol))
	{

		for(i=0; i<parmcnt; i++)
		{
			parm_list.parm[i] = argv[i+parg];
			len_list.len[i] = strlen(parm_list.parm[i]);
		}
		writeunixlink(program, parmcnt, &parm_list, &len_list, linkkey);

		sprintf(buff,"%s=%s",WISP_LINK_ENV,linkkey);			/* Store the linkkey in env		*/
		setenvstr(buff);
		parm_file_written = 1;
	}


#ifdef TEST
	for(i=0; parm[i]; i++)
	{
		printf("i=%2d  parm[i]=<%s>\n",i,parm[i]);
	}
#endif

	/*
		Do the exec
	*/

	if (!showit)	
	{
#ifdef unix
		signal( SIGINT,  SIG_IGN );
		signal( SIGQUIT, SIG_IGN );
		signal( SIGILL,  SIG_IGN );
#ifdef SIGEMT
		signal( SIGEMT,  SIG_IGN );
#endif
		signal( SIGBUS,  SIG_IGN );
		signal( SIGSEGV, SIG_IGN );

		saveterm();

		switch( pid = fork() )
		{
		case 0: /* Child */
			execvp(parm[0],parm);
			msgprintf("%%WRUN-F-EXEC Unable to exec [%s] errno=%d\n",parm[0],errno);
			exit(errno);
			break;

		case -1: /* fork failed */
			msgprintf("%%WRUN-F-FORK Unable to fork errno=%d\n",errno);
			rc = errno;
			break;

		default: /* Parent */
			wwaitpid(pid,&rc);
			restoreterm();
		}
#endif /* unix */
#if defined(MSDOS) || defined(WIN32)
		rc = spawnvp(P_WAIT,(const char *)parm[0],(const char **)parm);
		if (rc == -1)
		{
			switch(errno)
			{
			case ENOENT:
				msgprintf("wrun: file not found [%s]\n",parm[0]);
				break;
			case ENOEXEC:
				msgprintf("wrun: file not executable [%s]\n",parm[0]);
				break;
			default:
				msgprintf("wrun: spawn failed [errno=%d]\n",errno);
				break;
			}
			for(i=0; parm[i]; i++)
			{
				msgprintf("%s ",parm[i]);
			}
			msgprintf("\n");
		}

#endif /* MSDOS || WIN32 */
	}
	else
	{
		char line2[1024], tmp[256];

#ifdef WRUNT
		sprintf(line2,"WRUNT \t\t= WISP Version %s [%s]\n",wisp_version(), WL_platform_name());
#else
		sprintf(line2,"WRUN \t\t= WISP Version %s [%s]\n",wisp_version(), WL_platform_name());
#endif
		sprintf(tmp,"WISPCONFIG \t= %s\n", wispconfigdir());
		strcat(line2, tmp);

		sprintf(tmp,"COBOL \t\t= %s\n", cfg.wrun_cobtype);
		strcat(line2, tmp);

		if (acu_cobol)
		{
			char *ptr = getenv("A_CONFIG");
			sprintf(tmp,"A_CONFIG \t= %s\n", ((ptr)?ptr:"NULL"));
			strcat(line2,tmp);
		}

		sprintf(tmp,"RTS \t\t= %s\n", parm[0]);
		strcat(line2, tmp);

		strcat(line2, "OPTIONS \t= ");
		for(i=1; parm[i]; i++)
		{
			sprintf(tmp,"%s ",parm[i]);
			strcat(line2,tmp);
		}
		msgprintf("%s\n", line2);
		
		rc = 0;
	}

	if ( parm_file_written )							/* delete the temp file			*/ 
	{
		unlink(linkkey);
	} 

	return rc;
}

#ifdef unix
#include "saveterm.c"
#endif /* unix */

void msgprintf(const char* format, ...)
{
	va_list args;

	va_start(args,format);

#ifdef WIN32
#ifdef WRUNT
	vprintf(format,args);
#else
	{
		char	buff[1024];
		vsprintf(buff,format,args);
		wrun_message_box("WRUN",buff);
	}
	
#endif
#else
	vprintf(format,args);
#endif
	
}


#include "wutils.h"

/*
**	History:
**	$Log: wrun.c,v $
**	Revision 1.19.2.2  2003/02/10 20:21:33  gsl
**	Add platform to version
**	
**	Revision 1.19.2.1  2002/09/05 19:22:27  gsl
**	LINUX
**	
**	Revision 1.19  1999/05/26 15:21:48  gsl
**	fix warning on WIN32 with WRUNT
**	
**	Revision 1.18  1999-03-01 17:37:17-05  gsl
**	Make changes on WIN32 so that WRUNT the console version of WRUN can
**	be built by defining WRUNT.
**
**	Revision 1.17  1999-03-01 17:05:45-05  gsl
**	Change so that on Windows wrun can be built as WRUNT a telent (console)
**	version of WRUN.  This is based on the define WRUNT.
**
**	Revision 1.16  1999-02-24 19:12:25-05  gsl
**	For WIN32, changed to use the wisptelnet() routine
**
**	Revision 1.15  1999-02-23 17:08:15-05  gsl
**	For telnet on NT do a regular print instead of a message box
**
**	Revision 1.14  1997-02-25 09:37:42-05  gsl
**	Correct size of options
**
**	Revision 1.13  1997-02-20 16:05:17-05  gsl
**	Add "-K kkk" as an acucobol flag
**
**	Revision 1.12  1997-01-20 20:28:08-05  gsl
**	Changed the no-program message to display more useful information
**
**	Revision 1.11  1996-11-12 14:56:29-08  jockc
**	added call to message box in case of no args
**
**	Revision 1.9  1996-10-08 17:47:03-07  gsl
**	replace getenv() to wispconfigdir()
**
**	Revision 1.7  1995-06-15 03:10:17-07  gsl
**	Change to use wisp_version().
**	Add standard headers
**
**
**
*/
