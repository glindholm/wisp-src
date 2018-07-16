			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

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


#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <signal.h>
#include "wrunconf.h"
#include "wdefines.h"
#include "wcommon.h"

#define MAX_EXEC_PARMS	64

main	(argc,argv)
int	argc;
char	*argv[];
{
	char	options[80];
	char	program[80];
	char	parameters[80];
	int	i,j,k;
	int	arg;
	int	addspace;
	struct	wruncfg	cfg;
	int	parg;
	int	parmcnt;
	char	*optr;
	char	*parm[MAX_EXEC_PARMS];
	int	showit;
	int	nooptions;
	int	usingclause, parm_file_written;
	char	argcount_string[20];
	int	acu_cobol, aix_cobol, mf_cobol;
	int	pid, rc;

	struct 	{ char *the_parm[MAX_LINK_PARMS]; } parm_list;
	struct 	{ long  the_len [MAX_LINK_PARMS]; }  len_list;
	char	linkkey[80];								/* Key to link parm area		*/
	char	buff[128];

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
		Extract the parameters
	*/

	parameters[0] = '\0';
	parg = arg;
	parmcnt = 0;
	while( arg < argc )
	{
		parmcnt += 1;
		strcat(parameters,argv[arg++]);
		if ( arg < argc ) strcat(parameters," ");
	}

#ifdef TEST
	printf("parameters=<%s>\n",parameters);
#endif

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
			printf( "%%WRUN-F-MALLOC Malloc of %d bytes failed.\n",msize);
			exit(2);
		}
		strcpy(env_ptr,envstring);
		if ( putenv(env_ptr) )
		{
			printf( "%%WRUN-F-PUTENV Unable to put environment varible %s.\n",WRUNOPTIONS_ENV );
			exit(4);
		}

#ifdef TEST
		{
			char	*ptr;
			ptr = (char *)getenv(WRUNOPTIONS_ENV);
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
			printf( "%%WRUN-F-COBOL Unknown COBOL type [%s]\n",cfg.wrun_cobtype);
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
			parm_list.the_parm[i] = argv[i+parg];
			len_list.the_len[i] = strlen(parm_list.the_parm[i]);
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
		signal( SIGINT,  SIG_IGN );
		signal( SIGQUIT, SIG_IGN );
		signal( SIGILL,  SIG_IGN );
		signal( SIGEMT,  SIG_IGN );
		signal( SIGBUS,  SIG_IGN );
		signal( SIGSEGV, SIG_IGN );

		saveterm();

		switch( pid = fork() )
		{
		case 0: /* Child */
			execvp(parm[0],parm);
			printf("%%WRUN-F-EXEC Unable to exec [%s] errno=%d\n",parm[0],errno);
			exit(errno);
			break;

		case -1: /* fork failed */
			printf("%%WRUN-F-FORK Unable to fork errno=%d\n",errno);
			rc = errno;
			break;

		default: /* Parent */
			wwaitpid(pid,&rc);
			restoreterm();
		}

	}
	else
	{
		printf("wrun: Version=[%s]\n",WISP_VERSION);
		printf("[%s] ",cfg.wrun_cobtype);
		for(i=0; parm[i]; i++)
		{
			printf("%s ",parm[i]);
		}
		printf("\n");
		rc = 0;
	}

	if ( parm_file_written )							/* delete the temp file			*/ 
	{
		unlink(linkkey);
	} 

	exit(rc);
}

#include "saveterm.c"

#include "wutils.h"

