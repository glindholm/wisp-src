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


/*
**	File:		submit.c
**
**	Project:	wisp/lib
**
**	Purpose:	To emulate the Wang SUBMIT VSSUB
**
**	Routines:	SUBMIT()		The SUBMIT routine
**			SETSUBMIT()		Sets up parameters for next SUBMIT
**			unix_submit()		The unix implementation of SUBMIT
**			fix_exec_errno()	Fix the unix error codes to be Wang error codes
**
*/


/*
**	Includes
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdarg.h>

#ifdef unix
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#endif

#ifdef WIN32
#include <process.h>
#endif

#include "submit.h"
#include "vssubs.h"
#include "idsistd.h"
#include "idsisubs.h"
#include "wcommon.h"
#include "wperson.h"
#include "werrlog.h"
#include "wdefines.h"
#include "wglobals.h"
#include "filext.h"
#include "wisplib.h"
#include "wfname.h"
#include "wispcfg.h"
#include "level.h"
#include "wexit.h"
#include "wmalloc.h"
#include "wanguid.h"
#include "filgparm.h"
#include "assert.h"

#ifdef WIN32
#include "win32spn.h"
#endif

#include <signal.h>
#include "wrunconf.h"
#include "runtype.h"
#include "wispnt.h"

#ifdef WIN32
#include "isonames.h"
#endif

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/
#ifdef WIN32
static int subvar(char* buff, const char* var, const char* value);
#endif

#ifdef unix
static int unix_submit(
	char	*filespec,	/* name of file to submit		*/
	char	*jobname,	/* name of job				*/
	char	jobclass,
	char	jobstat,
	char	jobdisp,	/* the jobclass, status, disposition    */
	char	*p_vol,		/* PROGVOL				*/
	char	*p_lib);	/* PROGLIB				*/
static int fix_exec_errno(int err);
#endif

#ifdef WIN32
static int win32_submit(
	char	*filespec,	/* name of file to submit		*/
	char	*jobname,	/* name of job				*/
	char	jobclass,
	char	jobstat,
	char	jobdisp,	/* the jobclass, status, disposition    */
	char	*p_vol,		/* PROGVOL				*/
	char	*p_lib);	/* PROGLIB				*/
#endif

/*
	SUBMIT() - SUBMIT a command procedure to the standard batch queue

	Arg2 thru arg11 are optional but if any are included all preceding
	must also be included.

	arg1	file		alpha	8
	arg2	library		alpha	8
	arg3	volume		alpha	6
	arg4	job_name	alpha	8
	arg5	status		alpha	1
	arg6	disposition	alpha	1
	arg7	job_class	alpha	1
	arg8	abort_action	alpha	1	IGNORED	
	arg9	cpu_time_limit	int	4	IGNORED	
	arg10	limit_flag	alpha	1	IGNORED	
	arg11	priority	int	4	IGNORED	
	arg12	return_code	int	4
*/

void SUBMIT(char* l_file, ...)
{
	va_list the_args;								/* Define a pointer to the list.	*/
	int arg_count, args_remaining;							/* Number of arguments.			*/
	char l_lib[9],									/* Local library name.			*/
	     l_vol[7],									/* Local volume name.			*/
	     *l_job_name,								/* Address of the job name.		*/
	     l_status,									/* Address of the job submit status.	*/
	     l_disposition,								/* Address of the disposition action.	*/
	     l_job_class,								/* Address of the job class value.	*/
	     l_abort_action,								/* Address of the abort action.		*/
	     l_cpu_limit_action;							/* Address of the CPU limit action.	*/
	char	*ptr;

	char *end_name;

	int4 *l_cpu_limit,								/* Address of 4 byte int. value.	*/
 	     *l_return_code,								/* Address of 4 byte return code value.	*/
	     dummy_return_code;
	int4 *dummy_int4;

	int4 rc;
	int4 cpulim;
	char name[132];
	int4 mode;
	char jobname[9];
	int4 wang_retcod;								/* Value expected from Wang COBOL.	*/

	arg_count = WL_va_count();
	WL_wtrace("SUBMIT","ENTRY", "File=[%8.8s] args=%d", l_file, arg_count);

	/* arg1	file	alpha	8 */
	va_start(the_args, l_file);
	args_remaining = arg_count - 1;							/* Set a working variable.		*/

	WL_load_options();								/* Get usage constants.			*/
	wang_retcod = SUBMIT_RC_0_SUCCESS;

	memset( l_vol, ' ', SIZEOF_VOL );
	memset( l_lib, ' ', SIZEOF_LIB );

	/* arg2	library	alpha	8 */
	if (args_remaining > 1)								/* Are we NOT at the last arg ?		*/
	{
		ptr = va_arg(the_args, char*);						/* Get address of the library name.	*/
		args_remaining--;							/* One less argument.			*/
		if (ptr)
		{
			memcpy(l_lib,ptr,SIZEOF_LIB);
			leftjust(l_lib,SIZEOF_LIB);
		}
	}
	if (l_lib[0] == ' ') 
	{
		WL_get_defs(DEFAULTS_PL,l_lib);						/* Get the PROGLIB default.		*/
	}
	l_lib[SIZEOF_LIB] = '\0';
	if (l_lib[0] == ' ') 
	{
		WL_wtrace("SUBMIT","BLANKLIB", "LIBRARY is blank and PROGLIB is not set");
		wang_retcod = SUBMIT_RC_56_INVALID_OPTIONS;
	}

	/* arg3	volume	alpha	6 */
	if (args_remaining > 1)								/* Are we NOT at the last arg ?		*/
	{
		ptr = va_arg(the_args, char*);						/* Get address of the volume name.	*/
		args_remaining--;							/* One less argument.			*/
		if (ptr)
		{
			memcpy(l_vol,ptr,SIZEOF_VOL);
			leftjust(l_vol,SIZEOF_VOL);
		}
	}
	if (l_vol[0] == ' ') 
	{
		WL_get_defs(DEFAULTS_PV,l_vol);						/* Get the PROGVOL default.		*/
	}
	l_vol[SIZEOF_VOL] = '\0';
	if (l_vol[0] == ' ') 
	{
		WL_wtrace("SUBMIT","BLANKVOL", "VOLUME is blank and PROGVOL is not set");
		wang_retcod = SUBMIT_RC_56_INVALID_OPTIONS;
	}

	/* arg4	job_name alpha	8 */
	memcpy(jobname, l_file, 8);							/* Job name defaults to file name	*/
	jobname[8] = '\0';
	if (args_remaining > 1)								/* Are we NOT at the last arg ?		*/
	{
		l_job_name = va_arg(the_args, char*);					/* Get address of the job name.		*/
		args_remaining--;							/* One less argument.			*/

		if (' ' != l_job_name[0])						/* If not blank use the jobname		*/
		{
			memcpy(jobname,l_job_name,8);
			jobname[8] = '\0';
		}
	}

	/* arg5	status	alpha	1 */
	l_status = ' ';
	if (args_remaining > 1)								/* Are we NOT at the last arg ?		*/
	{
		l_status = *va_arg(the_args, char*);					/* Get address of the status value.	*/
		args_remaining--;							/* One less argument.			*/
		if (!strchr("RH ",l_status)) wang_retcod = SUBMIT_RC_900_INVALID_STATUS;/* If invalid status.			*/
	}
	if (l_status == ' ')
	{
		WL_get_defs(DEFAULTS_JS,&l_status);
	}

	/* arg6	disposition	alpha	1 */
	if (args_remaining > 1)								/* Are we NOT at the last arg ?		*/
	{
		l_disposition = *va_arg(the_args, char*);				/* Get the address of the disp. value.	*/
		args_remaining--;							/* One less argument.			*/
		if (!strchr("DRPO",l_disposition)) wang_retcod = SUBMIT_RC_901_INVALID_DISP;	/* If invalid disposition.		*/
	}
	else	l_disposition = 'D';

	/* arg7	job_class	alpha	1 */
	l_job_class = ' ';
	if (args_remaining > 1)								/* Are we NOT at the last arg ?		*/
	{
		l_job_class = *va_arg(the_args, char*);					/* Get the address of the class value.	*/
		args_remaining--;							/* One less argument.			*/
		if (!strchr("ABCDEFGHIJKLMNOPQRSTUVWXYZ ",l_job_class)) 
		{
			wang_retcod = SUBMIT_RC_902_INVALID_JOBCLASS; 			/* If invalid job class.		*/
		}
		
	}
	if (l_job_class == ' ')
	{
		WL_get_defs(DEFAULTS_JC,&l_job_class);
	}

	/* arg8	abort_action	alpha	1 */
	if (args_remaining > 1)								/* Are we NOT at the last arg ?		*/
	{
		/* IGNORED */
		l_abort_action = *va_arg(the_args, char*);				/* Get address of abort action value.	*/
		args_remaining--;							/* One less argument.			*/
		if (!strchr("DNR",l_abort_action)) wang_retcod = SUBMIT_RC_903_INVALID_ABORTACT; /* If invalid abort action.		*/
	}
	else	l_abort_action = 'R';

	/* arg9	cpu_time_limit	int	4 */
	if (args_remaining > 1)								/* Are we NOT at the last arg ?		*/
	{
		/* IGNORED */
		l_cpu_limit = va_arg(the_args, int4*);					/* Get address of the cpu time limit.	*/
		args_remaining--;							/* One less argument.			*/
	}
	else	l_cpu_limit = 0;

	if (l_cpu_limit)
	{
		cpulim = WL_get_swap(l_cpu_limit);					/* Get requested cpu limit. (1/100 sec)	*/
		if (cpulim == -1)							/* Use usage constants.			*/
		{
			char	def_proc_cpu[6];
			WL_get_defs(DEFAULTS_JL,def_proc_cpu);

			cpulim  = (def_proc_cpu[0] - '0') * 36000;			/* Do tens of hours.			*/
			cpulim += (def_proc_cpu[1] - '0') * 3600;			/* Do hours.				*/
			cpulim += (def_proc_cpu[2] - '0') * 600;			/* Tens of minutes.			*/
			cpulim += (def_proc_cpu[3] - '0') * 60;				/* Minutes.				*/
			cpulim += (def_proc_cpu[4] - '0') * 10;				/* Tens of seconds.			*/
			cpulim += (def_proc_cpu[5] - '0');				/* Seconds.				*/
			cpulim *= 100;							/* Time is in hundredths.		*/
		}
		else if (cpulim < 0 )	/* If invalid CPU time limit.		*/
		{
			wang_retcod = SUBMIT_RC_904_INVALID_TIME;
		}
	}
	else
	{
		cpulim = 0;
	}

	/* arg10	limit_flag	alpha	1 */
	if (args_remaining > 1)								/* Are we NOT at the last arg ?		*/
	{
		/* IGNORED */
		l_cpu_limit_action = *va_arg(the_args, char*);				/* Get address of the cpu limit action.	*/
		args_remaining--;							/* One less argument.			*/
		if (!strchr("CPW",l_cpu_limit_action)) wang_retcod = SUBMIT_RC_905_INVALID_LIMIT;	/* If invalid limit flag.		*/
	}
	else	l_cpu_limit_action = 'W';
         
	/* arg11	priority	int	4 */
	if (args_remaining > 1)
	{
		/* IGNORED */
		dummy_int4 = va_arg(the_args, int4*);
		args_remaining--;
	}

	/* arg12	return_code	int	4 */
	/*
	**	The RETURN-CODE is NOT an optional argument.  However there is an error in the Wang VSSUB manual which
	**	says it is an optional argument. (The examples show otherwise.)
	*/
	if (args_remaining >= 1)							/* Are we at the last arg ?		*/
	{
		l_return_code = va_arg(the_args, int4*);				/* Get the address of the return code.	*/
	}
	else
	{
		l_return_code = &dummy_return_code;
	}
	va_end(the_args);

	WL_wtrace("SUBMIT","ARGS","File=[%8.8s] Lib=[%8.8s] Vol=[%6.6s] JN=[%8.8s] Status=%c Disp=%c Class=%c",
	       l_file, l_lib, l_vol, jobname, l_status, l_disposition, l_job_class);

	if (wang_retcod)								/* If the return code has been set	*/
	{
		WL_wtrace("SUBMIT","RETURN","Return code = %d", wang_retcod);
		WL_put_swap(l_return_code, wang_retcod);
		return;
	}

	mode = 0;
	end_name = WL_wfname(&mode,l_vol,l_lib,l_file,name);				/* now make a name for ourselves...	*/
	*end_name = '\0';								/* Null terminate properly.		*/

#ifdef unix
	WL_wtrace("SUBMIT","UNIX","Filespec=[%s] PV=[%6.6s] PL=[%8.8s]", name, l_vol, l_lib);
	rc = unix_submit(name,jobname,l_job_class,l_status,l_disposition,l_vol,l_lib);
#endif
#ifdef WIN32
	WL_wtrace("SUBMIT","WIN32","Filespec=[%s] PV=[%6.6s] PL=[%8.8s]", name, l_vol, l_lib);
	rc = win32_submit(name,jobname,l_job_class,l_status,l_disposition,l_vol,l_lib);
#endif

	WL_wtrace("SUBMIT","RETURN","Return code = %d", rc);

	WL_put_swap(l_return_code, rc);
}

/* These are referenced by que_job, and must be declared the same.								*/

#define MAX_SUBMIT_PARMS	8
#define MAX_SUBPARM_LEN		256

static short subp_len[MAX_SUBMIT_PARMS];			/* The length of each parm.		*/
static char  subp_text[MAX_SUBMIT_PARMS][MAX_SUBPARM_LEN];	/* The parm data.			*/
static int   subp_num = 0;					/* The number of parms.			*/

/* Subroutine SETSUBMIT, used to set the values of the parms to be passed to a procedure when SUBMIT is used.			*/
/* CALL "SETSUBMIT" USING NUM-PARMS, PARM-1-LEN, PARM-1, PARM-2-LEN, PARM-2 ...							*/
/* NUM-PARMS is the number of parms to be passed, it is a 2 byte field. PARM-1-LEN thru PARM-N-LEN are also 2-bytes and describe*/
/* the length of each parm.													*/

void SETSUBMIT(short *arg_count_ptr, ...)
{
	va_list the_args;								/* Define a pointer to the list.	*/
	short arg_count;
	char *ptr;
	
	va_start(the_args, arg_count_ptr);

	arg_count = *arg_count_ptr;

	subp_num = 0;

	if (arg_count < 0 || arg_count > MAX_SUBMIT_PARMS)
	{
		WL_werrlog_error(WERRCODE(64000),"SETSUBMIT", "MAXPARMS", 
			"Invalid number of parameters Count=%d",arg_count);
	}
	else
	{
		while (arg_count)								/* First copy all the new args.		*/
		{
			subp_len[subp_num] = *va_arg(the_args,short *);				/* Get the length.			*/

			if (subp_len[subp_num] < 1 || subp_len[subp_num] >= MAX_SUBPARM_LEN)
			{
				WL_werrlog_error(WERRCODE(64000),"SETSUBMIT", "PARMLEN", 
					"Invalid parameter Lenght=%d",subp_len[subp_num]);
				break;
			}

			ptr = va_arg(the_args, char *);						/* and the text.			*/
			memcpy(subp_text[subp_num],ptr,(int4) subp_len[subp_num]);		/* Copy to local area.			*/
			subp_text[subp_num][subp_len[subp_num]] = '\0';				/* Null terminate the parameters	*/
			subp_num++;
			arg_count--;
		}
	}
	va_end(the_args);
}


#ifdef unix
static int unix_submit(
	char	*filespec,	/* name of file to submit		*/
	char	*jobname,	/* name of job				*/
	char	jobclass,
	char	jobstat,
	char	jobdisp,	/* the jobclass, status, disposition    */
	char	*p_vol,		/* PROGVOL				*/
	char	*p_lib)		/* PROGLIB				*/
{
	int 	pid,ftyp;
	int 	retcode, ii;
	char	*sh_parm[64];
	char    jclass[4],jmode[20];
	char    *jclassp, *jmodep;
	int     hasmode,hasclass;
	struct wruncfg cfg;
	char	options[sizeof(cfg.wrun_options)];
	char	*optr;
	int	arg;


	retcode = SUBMIT_RC_0_SUCCESS;

	if (!fexists(filespec))
	{
		retcode = SUBMIT_RC_20_FILE_NOT_FOUND;					/* 20 = file not found.			*/
	}

	if ( retcode == SUBMIT_RC_0_SUCCESS )
	{
		ftyp = WL_runtype(filespec);						/* Get the run type			*/

		switch(ftyp)
		{
		default:
		case RUN_NOT:
		case RUN_ACCESS:
		case RUN_UNKNOWN:
			retcode = SUBMIT_RC_24_IMPROPER_FILETYPE;
			break;

		case RUN_EXEC:
		case RUN_ACUCOBOL:
		case RUN_MF:
		case RUN_SHELL:
		case RUN_PROC:
		case RUN_PROCOBJ:
			break;
		}

	}


	if ( retcode == SUBMIT_RC_0_SUCCESS )
	{
		switch (pid = fork())
		{
			case 0:								/* is child process 			*/
			{
				char    envstring[80];
				char	tty[20];
				int	pid;
				int	nice_value;

				signal( SIGHUP,  SIG_IGN );				/* Ignore HangUps & term signals.	*/
				signal( SIGTERM, SIG_IGN );

				WL_zerolevel();						/* set link-level to zero		*/

				if (0 == WL_getscmapnice(jobclass,&nice_value))
				{
					WL_wtrace("SUBMIT","UNIX","Nice value = %d",nice_value);
					nice(nice_value);
				}

				WL_set_progdefs_env(p_vol,p_lib);			/* Set up PROGLIB and PROGVOL		*/
				WL_clear_progdefs();					/* Clear PROGLIB/VOL from symbol	*/

				pid = getpid();
				sprintf(envstring,"%s=%06d",WISP_PID_ENV,pid);		/* Save the PID in the environment.	*/
				if (WL_setenvstr(envstring))
				{
					werrlog(WERRCODE(64004),envstring,0,0,0,0,0,0,0);
					wexit(WERRCODE(64004));
				}

				sprintf(envstring, "WISPJOBNAME=%s", jobname);
				if (WL_setenvstr(envstring))
				{
					werrlog(WERRCODE(64004),envstring,0,0,0,0,0,0,0);
					wexit(WERRCODE(64004));
				}

				if (getenv(WISP_CANCELEXIT_ENV))			/* If CANCEL EXIT set then		*/
				{
					sprintf(envstring,"%s=0",WISP_CANCELEXIT_ENV);	/* Turn off CANCEL EXIT			*/
					if (WL_setenvstr(envstring))
					{
						werrlog(WERRCODE(64004),envstring,0,0,0,0,0,0,0);
						wexit(WERRCODE(64004));
					}
				}

				if ( ! wbackground() )					/* If not already in background		*/
				{
					WL_ttyid5(tty);
					sprintf(envstring,"%s=%s",WISP_TTY_ENV,tty);	/* Save the TTY in the environment.	*/
					if (WL_setenvstr(envstring))
					{
						werrlog(WERRCODE(64004),envstring,0,0,0,0,0,0,0);
						wexit(WERRCODE(64004));
					}

					sprintf(envstring,"%s=true",WISP_BACKGROUND_ENV); /* Set BACKGROUND flag in environment.*/
					if (WL_setenvstr(envstring))
					{
						werrlog(WERRCODE(64004),envstring,0,0,0,0,0,0,0);
						wexit(WERRCODE(64004));
					}
				}

				if (getenv(WISP_GID_ENV))				/* If GID was set then reset it.	*/
				{
					sprintf(envstring,"%s=%d",WISP_GID_ENV,pid);
					if (WL_setenvstr(envstring))
					{
						werrlog(WERRCODE(64004),envstring,0,0,0,0,0,0,0);
						wexit(WERRCODE(64004));
					}
				}
#if defined(BSD) || defined(_BSD) || defined(OSF1_ALPHA)
				setpgrp(pid,pid); 					/* Set the PROCESS GROUP ID		*/
#else
				setpgrp(); 						/* Set the PROCESS GROUP ID		*/
#endif

				freopen("/dev/null","r",stdin);				/* close these files			*/
				freopen("/dev/null","w",stdout);
				freopen("/dev/null","w",stderr);

				switch(ftyp)
				{
				case RUN_EXEC:
					sh_parm[0] = filespec;				/* File spec to exec			*/
					for (ii=0; ii < subp_num; ii++)			/* load SETSUBMIT parameters		*/
					{
						sh_parm[ii+1] = subp_text[ii];
					}
					sh_parm[ii+1] = '\0';				/* Null terminate the arg list		*/
					break;

				case RUN_SHELL:
					sh_parm[0] = wispshellexe();			/* exec the shell			*/
					sh_parm[1] = filespec;				/* shell script to exec			*/
					for (ii=0; ii < subp_num; ii++)			/* load SETSUBMIT parameters		*/
					{
						sh_parm[ii+2] = subp_text[ii];
					}
					sh_parm[ii+2] = '\0';				/* Null terminate the arg list		*/
					break;
					
				case RUN_ACUCOBOL:
				case RUN_MF:
					{
						WL_wrunconfig(&cfg);
						strcpy(options,cfg.wrun_options);

						arg=0;
						sh_parm[arg++] = cfg.wrun_runcbl;
					
						for( optr=options; *optr; optr++ )
						{
							for(;*optr==' ';optr++);		/* Scan til first non-space 	*/
							if (! *optr ) break;
						
							sh_parm[arg++] = optr;			/* Point to option		*/
					
							for(;*optr && *optr != ' ';optr++);	/* Scan til space.		*/
							if (! *optr ) break;
					
							*optr = '\0';				/* Null terminate the option	*/
						}				

						sh_parm[arg++] = filespec;			/* the program name		*/
						sh_parm[arg++] = '\0';				/* null terminate it		*/
						break;
					}

				case RUN_PROC:
				case RUN_PROCOBJ:
					{
						sh_parm[0] = (char*)WL_wprocexe();	/* argv[0] is the "wproc" program	*/
						sh_parm[1] = filespec;			/* argv[1] in the filespec	 	*/
						for (ii=0; ii < subp_num; ii++)		/* load SETSUBMIT parameters		*/
						{
							sh_parm[ii+2] = subp_text[ii];
						}
						sh_parm[ii+2] = '\0';			/* Null terminate the arg list		*/

					}
				}
				if (*(WL_batchqueue_name()) != '\0')
				{
					int	shell_cmd_len;
					char	*shell_cmd;

					/*
					**	Calculate the length of the shell command then malloc it.
					*/
					for (ii=0, shell_cmd_len=1; sh_parm[ii]; ++ii)
					{
						shell_cmd_len += strlen(sh_parm[ii]) + 1;
						if ( strchr(sh_parm[ii], ' ') )		/* Is there any embedded blank?		*/
							shell_cmd_len += 2;		/* Account for double quote wrapping	*/
					}
					shell_cmd = wisp_malloc(shell_cmd_len);
					
					/*
					**	Assemble the shell command
					*/
					for (ii=0, shell_cmd[0]='\0'; sh_parm[ii]; ++ii)
					{
						dqw_strcat(shell_cmd,sh_parm[ii]);	/* double quote wrap if needed		*/
						strcat(shell_cmd," ");					
					}
					if (jobclass)
					{
						sprintf(jclass,"-C%c",jobclass);
						hasclass=TRUE;
					}
					else
					{
						hasclass=FALSE;
					}
					hasmode=FALSE;
					if (jobstat=='H')
					{
						strcpy(jmode,"-Mhold");
						hasmode=TRUE;
					}
					if (jobdisp=='R')	
					{
						if (hasmode)
						{
							strcat(jmode,",re");
						}
						else
						{
							strcpy(jmode,"-Mre");
							hasmode=TRUE;
						}
					}
					if (hasmode)
					{	
						jmodep=jmode;
					}
					else
					{	
						jmodep=NULL;
					}
					if (hasclass)
					{	
						jclassp=jclass;
					}
					else
					{	
						jclassp=NULL;
					}

					WL_wtrace("SUBMIT","EXECLP","%s -c %s -T %s %s",
					       WL_batchqueue_name(), shell_cmd, jobname, 
					       (jclassp?jclassp:"(NULL)"), 
					       (jmodep?jmodep:"(NULL)"));
					
					execlp( WL_batchqueue_name(), WL_batchqueue_name(), 
						"-c", shell_cmd, "-T", jobname, jclassp, jmodep, NULL);
					retcode=fix_exec_errno(errno);
					werrlog(WERRCODE(64006),WL_batchqueue_name(),errno,retcode,0,0,0,0,0);
					exit(retcode);
				}
				else
				{
					if (WL_wtracing())
					{
						char	tbuff[1024], pbuff[256];
						int i;
						
						tbuff[0] = '\0';
						
						for(i=0; sh_parm[i]; i++)
						{
							sprintf(pbuff,"P%d[%s] ",i,sh_parm[i]);
							strcat(tbuff,pbuff);
						}
						WL_wtrace("SUBMIT","EXECVP","%s",tbuff);
					}
					
					execvp( sh_parm[0], sh_parm );
					retcode=fix_exec_errno(errno);
					werrlog(WERRCODE(64006),sh_parm[0],errno,retcode,0,0,0,0,0);
					exit(retcode);
				}

			}
			case -1:							/* Fork failed				*/
			{
				werrlog(WERRCODE(64008),errno,0,0,0,0,0,0,0);

				retcode = SUBMIT_RC_99_FORK_FAILED;
				break;
			}
			default:							/* is parent process	 		*/
			{
				retcode = SUBMIT_RC_0_SUCCESS;
				break;
			}
		}
	}

	WL_wtrace("SUBMIT","COMPLETED","Submitted [%s] with RC=[%d]",
		filespec, retcode);

	subp_num = 0;									/* Clear any SETSUBMIT parameters	*/

	return retcode;
}

static int fix_exec_errno(int code)
{
	switch (code)
	{
	case 0:		return SUBMIT_RC_0_SUCCESS;
	case E2BIG:	return SUBMIT_RC_56_INVALID_OPTIONS;
	case EACCES:	return SUBMIT_RC_28_ACCESS_DENIED;
	case EAGAIN:	return SUBMIT_RC_52_SYSTEM_ERROR;
	case ENOENT: 	return SUBMIT_RC_20_FILE_NOT_FOUND;
	case ENOTDIR:	return SUBMIT_RC_16_LIBRARY_NOT_FOUND;
	case ENOEXEC:	return SUBMIT_RC_24_IMPROPER_FILETYPE;
	case ENOMEM:	return SUBMIT_RC_52_SYSTEM_ERROR;
	case EIO:	return SUBMIT_RC_52_SYSTEM_ERROR;
	default:	return 10000+code;
	}
}
#endif	/* #ifdef unix */

#ifdef WIN32
static int win32_submit(
	char	*filespec,	/* name of file to submit		*/
	char	*jobname,	/* name of job				*/
	char	jobclass,
	char	jobstat,
	char	jobdisp,	/* the jobclass, status, disposition    */
	char	*p_vol,		/* PROGVOL				*/
	char	*p_lib)		/* PROGLIB				*/
{

	int 	ftyp;
	int 	retcode, ii;
	char	*sh_parm[64];
	int 	system_ret;
	char	tty[20];
	struct wruncfg cfg;
	char	options[sizeof(cfg.wrun_options)];
	char	*optr;
	int	arg;
		

	retcode = SUBMIT_RC_0_SUCCESS;

	if (!fexists(filespec))
	{
		retcode = SUBMIT_RC_20_FILE_NOT_FOUND;					/* 20 = file not found.			*/
	}

	if ( retcode == SUBMIT_RC_0_SUCCESS )
	{
		ftyp = WL_runtype(filespec);						/* Get the run type			*/

		switch(ftyp)
		{
		default:
		case RUN_NOT:
		case RUN_ACCESS:
		case RUN_UNKNOWN:
			retcode = SUBMIT_RC_24_IMPROPER_FILETYPE;
			break;

		case RUN_EXEC:
		case RUN_ACUCOBOL:
		case RUN_MF:
		case RUN_SHELL:
		case RUN_PROC:
		case RUN_PROCOBJ:
			break;
		}

	}


	if ( retcode == SUBMIT_RC_0_SUCCESS )
	{
		char    envstring[80];
		int	pid;
		
		sprintf(envstring,"%s=0", WISP_LINKLEVEL_ENV);		/* set link-level to zero		*/
		WL_win32SetNewEnv(envstring);
		
		sprintf(envstring,"%s=%s", WISP_PROGVOL_ENV, p_vol);
		WL_win32SetNewEnv(envstring);
		sprintf(envstring,"%s=%s", WISP_PROGLIB_ENV, p_lib);
		WL_win32SetNewEnv(envstring);

		pid = getpid();
		sprintf(envstring,"%s=%06d",WISP_PID_ENV,pid);		/* Save the PID in the environment.	*/
		WL_win32SetNewEnv(envstring);

		if (getenv(WISP_CANCELEXIT_ENV))			/* If CANCEL EXIT set then		*/
		{
			sprintf(envstring,"%s=0",WISP_CANCELEXIT_ENV);	/* Turn off CANCEL EXIT			*/
			WL_win32SetNewEnv(envstring);
		}
		
		if ( ! wbackground() )					/* If not already in background		*/
		{
			sprintf(envstring,"%s=true",WISP_BACKGROUND_ENV); /* Set BACKGROUND flag in environment.*/
			WL_win32SetNewEnv(envstring);

			WL_ttyid5(tty);
			sprintf(envstring,"%s=%s",WISP_TTY_ENV,tty);	/* Save the TTY in the environment.	*/
			WL_win32SetNewEnv(envstring);
		}

		sprintf(envstring, "WISPJOBNAME=%s", jobname);
		WL_win32SetNewEnv(envstring);
		
		sprintf(envstring,"%s=-1",WISP_GID_ENV);		/* Reset the GID			*/
		WL_win32SetNewEnv(envstring);

		switch(ftyp)
		{
		case RUN_EXEC:
			sh_parm[0] = filespec;				/* File spec to exec			*/
			for (ii=0; ii < subp_num; ii++)			/* load SETSUBMIT parameters		*/
			{
				sh_parm[ii+1] = subp_text[ii];
			}
			sh_parm[ii+1] = '\0';				/* Null terminate the arg list		*/
			break;
		
		case RUN_SHELL:
			sh_parm[0] =  filespec;				/* shell script to exec			*/
			for (ii=0; ii < subp_num; ii++)			/* load SETSUBMIT parameters		*/
			{
				sh_parm[ii+1] = subp_text[ii];
			}
			sh_parm[ii+1] = '\0';				/* Null terminate the arg list		*/
			break;
			
		case RUN_ACUCOBOL:
		case RUN_MF:
			{
				WL_wrunconfig(&cfg);
				strcpy(options,cfg.wrun_options);
		
				arg=0;
				sh_parm[arg++] = cfg.wrun_runcbl;
			
				for( optr=options; *optr; optr++ )
				{
					for(;*optr==' ';optr++);		/* Scan til first non-space 	*/
					if (! *optr ) break;
				
					sh_parm[arg++] = optr;			/* Point to option		*/
			
					for(;*optr && *optr != ' ';optr++);	/* Scan til space.		*/
					if (! *optr ) break;
			
					*optr = '\0';				/* Null terminate the option	*/
				}				
		
				sh_parm[arg++] = filespec;			/* the program name		*/
				sh_parm[arg++] = '\0';				/* null terminate it		*/
				break;
			}
		
		case RUN_PROC:
		case RUN_PROCOBJ:
			{
				int wproc_index;
				
				wproc_index = 0;
				sh_parm[wproc_index] = (char*)WL_wprocexe();		/* argv[0] is the "wproc" program	*/
				sh_parm[wproc_index+1] = filespec;			/* argv[1] in the filespec	 	*/
				for (ii=0; ii < subp_num; ii++)				/* load SETSUBMIT parameters		*/
				{
					sh_parm[ii+wproc_index+2] = subp_text[ii];
				}
				sh_parm[ii+wproc_index+2] = '\0';			/* Null terminate the arg list		*/
			}
		}

		/*
		** 	If BATCHQUEUE is specified in OPTIONS file this means that there
		**	is a queue management system in use.  We will build the command
		**	needed to submit the job into the queue.  The command used is
		**	fully comfigurable in the OPTIONS file.
		**
		**	If not specified in the OPTIONS file then we default to using the 
		**	Argent Queue Manager (AQM) QSUBMIT command.
		**
		**	We allow a different command to be used when run on Windows 95 and
		**	on Windows NT.  The AQM QSUBMIT command does not run on Windows 95
		**	so we generate an REXEC command to run QSUBMIT on the NT server
		**	instead of the 95 client.
		**
		**	The batch commands can contains a number of macros which we replace
		**	to form the final command be for the command is issued.
		*/
		if (*(WL_batchqueue_name()) != '\0')
		{
			int arg_len;
			char s_vol[6+1], s_lib[8+1];
			char log_path[256];
			
			int4 log_file_mode;
			
			int	subparams_len;
			char	*subparams;
			char 	cmd_string[1024];
			char 	*trunc_idx;
			char 	*end_name;
			char    cqmap[120];
			char 	l_username[SUBMIT_USER_LEN+1], l_password[SUBMIT_PASS_LEN+1];
			int	initial_gp;
			
			/*
			**	Create a batchlog filepath
			*/
			WL_loadpad(s_vol, WL_batchlogvol(), 6);
			WL_loadpad(s_lib, WL_batchloglib(), 8);
			
			log_file_mode = 0;
			WSETFILEXT("log");
			end_name=WL_wfname(&log_file_mode, s_vol, s_lib, jobname, log_path);
			*end_name='\0';
			
			if (0!=makepath(log_path))
			{
				retcode=SUBMIT_RC_56_INVALID_OPTIONS;
				goto win32_submit_return;
				
			}
			
			/*
			**	Calculate the length of the parameters then malloc it.
			*/
			for (ii=1, subparams_len=1; sh_parm[ii]; ++ii)
			{
				arg_len = strlen(sh_parm[ii]);
				for (trunc_idx = sh_parm[ii] + arg_len -1;
				     trunc_idx >= sh_parm[ii] && *(trunc_idx) == ' ';
				     --trunc_idx);
				*++trunc_idx = '\0';
				
				subparams_len += strlen(sh_parm[ii]) + 1;

				if ( strchr(sh_parm[ii], ' ')  )			/* Is there any embedded blank?		*/
					subparams_len += 2;				/* Account for double quote wrapping	*/

			}
			subparams = wisp_malloc(subparams_len);
			
			/*
			**	String the parameters together separated by spaces
			*/
			for (ii=1, subparams[0]='\0'; sh_parm[ii]; ++ii)
			{
				dqw_strcat(subparams,sh_parm[ii]);			/* double quote wrap if needed		*/
				strcat(subparams," ");					
			}
			ASSERT((int)strlen(subparams) < subparams_len);

			/*
			**	Lookup the submit class translation from CQMAP
			*/
			cqmap[0] = '\0';
			if (jobclass)
			{
				WL_getcqmap(jobclass, cqmap);
				ASSERT(strlen(cqmap) < sizeof(cqmap));
			}

			/*
			**	Load the username and password into local variables (null terminated)
			*/
			strcpy(l_username, WL_batchuser());
			if (WL_batchpass())
			{
				strcpy(l_password, WL_batchpass());
			}
			else
			{
				l_password[0] = '\0';
			}
			
			/*
			**	If password or username is not known then getparm it.
			*/
			initial_gp = 1;
			while (!*l_password || !*l_username)
			{
				char 	username[SUBMIT_USER_LEN+1], password[SUBMIT_PASS_LEN+1];
				char 	save[3];
				
				/*
				**	Load user and pass into blank padded fields for the getparm
				*/
				WL_loadpad(username, l_username, sizeof(username));
				WL_loadpad(password, l_password, sizeof(password));

				memcpy(save, "NO ", 3);
			
				if ( 0 != WL_password_getparm(initial_gp, 
							   username, sizeof(username)-1, 
							   password, sizeof(password)-1, 
							   save, "Username and Password required by SUBMIT"))
				{
					/* Getparm was aborted */
					retcode = SUBMIT_RC_1000_ABORTED;
					goto win32_submit_return;
				}
				
				leftjust(password,sizeof(password)-1);
				leftjust(username,sizeof(username)-1);
				WL_cobx2cstr(l_username, username, sizeof(username)-1);
				WL_cobx2cstr(l_password, password, sizeof(password)-1);

				if (0 == memcmp(save,"YES",3))
				{
					WL_setbatchuser(l_username);
					WL_setbatchpass(l_password);
				}
				initial_gp = 0;
			}

			/*
			**	If %BATCHCMD% has not been set from the OPTIONS file then
			**	set it here with the default value.
			*/
			if (!WL_batchcmd() || strlen(WL_batchcmd()) == 0)
			{
				WL_setbatchcmd("QSUBMIT %SUBFILE% %CQMAP% /NAME=%SUBNAME%"
					    " /NONOTIFY /PARAMS=\"%SUBPARAMS%\" /LOG_FILE=%BATCHLOG%"
					    " %SUBSTAT% /USER=%BATCHUSER% /PASSWORD=%BATCHPASS%");
			}
			
			/*
			**	If the Windows 95 batch command has not been set from OPTIONS file
			**	then set it here with the default value.
			*/
			if (!WL_batchcmd95() || strlen(WL_batchcmd95()) == 0)
			{
				WL_setbatchcmd95("REXEC.EXE %SERVER% -l %BATCHUSER% -p %BATCHPASS% %BATCHCMD%");
			}
			
			/*
			**	Check if NT or 95 and use the corresponding batch command.
			**	If not Windows NT then assume 95.
			*/
			if (!WL_win32_nt())
			{
				strcpy(cmd_string, WL_batchcmd95());
				subvar(cmd_string, "%BATCHCMD%", WL_batchcmd());
			}
			else
			{
				strcpy(cmd_string, WL_batchcmd());
			}
			
			/*
			**	Substitute the variables into the batch command
			*/
			subvar(cmd_string, "%SUBFILE%",   sh_parm[0]);
			subvar(cmd_string, "%CQMAP%",     cqmap);
			subvar(cmd_string, "%SUBNAME%",   jobname);
			subvar(cmd_string, "%SUBPARAMS%", subparams);
			free(subparams);
			subvar(cmd_string, "%BATCHLOG%",  log_path);
			if (jobstat=='H')
			{
				if (!WL_batchhold()) WL_setbatchhold("/AFTER=+365");
				
				subvar(cmd_string, "%SUBSTAT%", WL_batchhold());
			}
			else
			{
				if (!WL_batchrun()) WL_setbatchrun("");
				
				subvar(cmd_string, "%SUBSTAT%", WL_batchrun());
			}
			subvar(cmd_string, "%BATCHUSER%", l_username);
			subvar(cmd_string, "%BATCHPASS%", l_password);
			subvar(cmd_string, "%SERVER%", WL_batchserver());

			ASSERT(strlen(cmd_string) < sizeof(cmd_string));

			/*
			**	Issue the batch command.
			**	This will but the job into the queue and return so we
			**	will wait for it to complete and hide the child window.
			*/ 
			system_ret = WL_win32spawnlp( NULL , cmd_string, SPN_HIDDEN_CMD|SPN_WAIT_FOR_CHILD|SPN_CAPTURE_OUTPUT);


			/*
			**	Get the return code or exit code from the spawn.
			**	Use it to generate a SUBMIT return code by adding 10000.
			*/
			switch(system_ret)
			{
			case 0:		
				retcode = SUBMIT_RC_0_SUCCESS;
				break;
				
			default:	
				retcode = 10000 + system_ret;
				break;
			}
		}
		else
		{
			int	shell_cmd_len;
			char	*shell_cmd;
			/*
			**	Calculate the length of the shell command then malloc it.
			*/
			for (ii=0, shell_cmd_len=1; sh_parm[ii]; ++ii)
			{
				shell_cmd_len += strlen(sh_parm[ii]) + 1;
				if ( strchr(sh_parm[ii], ' ')  )			/* Is there any embedded blank		*/
					shell_cmd_len += 2;				/* Account for double quote wrapping	*/
			}
			shell_cmd = wisp_malloc(shell_cmd_len);
			
			/*
			**	Assemble the shell command
			*/
			strcpy(shell_cmd,sh_parm[0]);
			for (ii=1; sh_parm[ii]; ++ii)
			{
				strcat(shell_cmd," ");
				dqw_strcat(shell_cmd,sh_parm[ii]);			/* double quote wrap if needed		*/
			}

			/*
			**  CHILD will be DETACHED
			*/
			system_ret = WL_win32spawnlp( NULL , shell_cmd , SPN_SUBMIT_CHILD );

			free(shell_cmd);

			if (system_ret == 1)
			{
				retcode = SUBMIT_RC_56_INVALID_OPTIONS;
			}
		}
	}
	
	
	WL_wtrace("SUBMIT","COMPLETED","Submitted [%s] with RC=[%d]",
		filespec, retcode);

	subp_num = 0;									/* Clear any SETSUBMIT parameters	*/

win32_submit_return:
	
	return retcode;
}
#endif

#ifdef WIN32
/*
**	ROUTINE:	subvar()
**
**	FUNCTION:	Substitute variable in a string
**
**	DESCRIPTION:	Replace all occurances of "var" in "buff" with "value"
**
**	ARGUMENTS:	
**	buff		The string to be modified
**	var		The varible to look for (e.g %SERVER%)
**	value		The replacement value
**
**	GLOBALS:	None
**
**	RETURN:		Number of occurances replaced
**
**	WARNINGS:	Max buff size is 1024
**
*/
static int subvar(char* buff, const char* var, const char* value)
{
	int 	subcnt;
	int	pos;
	char	temp[1024];
	char	*ptr;

	WL_wtrace("SUBMIT","SUBVAR","In var=[%s] value=[%s] buff=[%s]", var, value, buff);
	ptr = buff;
	for(subcnt=0; (pos = strpos(buff,var)) != -1; subcnt++)
	{
		ptr  = &buff[pos];						/* point to location of search string	*/
		strcpy(temp, ptr+strlen(var));					/* copy end of line into temp string	*/
		strcpy(ptr,value);						/* put replacement string in place	*/
		strcat(ptr,temp);						/* put end line back into source	*/
	}
		
	WL_wtrace("SUBMIT","SUBVAR","Out buff=[%s]", buff);
	return subcnt;
}
#endif

/*
**	ROUTINE:	WL_submit_err()
**
**	FUNCTION:	Translate SUBMIT return code into an error message
**
**	DESCRIPTION:	Lookup the return code and return string.
**
**	ARGUMENTS:	
**	error		The SUBMIT return code
**
**	GLOBALS:	none
**
**	RETURN:		Pointer to a error message
**
**	WARNINGS:	None
**
*/
const char* WL_submit_err(int error)
{
	const char* errptr;
	
	switch(error)
	{
	case SUBMIT_RC_0_SUCCESS:		errptr = "Success";			break;
	case SUBMIT_RC_4_VOLUME_NOT_FOUND:	errptr = "Volume not found";		break;
	case SUBMIT_RC_8_VOLUME_BUSY:		errptr = "Volume locked";		break;
	case SUBMIT_RC_16_LIBRARY_NOT_FOUND:	errptr = "Library not found";		break;
	case SUBMIT_RC_20_FILE_NOT_FOUND:	errptr = "File not found";		break;
	case SUBMIT_RC_24_IMPROPER_FILETYPE:	errptr = "Improper file type";		break;
	case SUBMIT_RC_28_ACCESS_DENIED:	errptr = "Access denied";		break;
	case SUBMIT_RC_40_INVALID_FILE_SPEC:	errptr = "Invalid file lib vol";	break;
	case SUBMIT_RC_52_SYSTEM_ERROR:		errptr = "System Error";		break;
	case SUBMIT_RC_56_INVALID_OPTIONS:	errptr = "Invalid options";		break;
	case SUBMIT_RC_99_FORK_FAILED:		errptr = "Fork Failed";			break;
	case SUBMIT_RC_900_INVALID_STATUS:	errptr = "Invalid Status";		break;
	case SUBMIT_RC_901_INVALID_DISP:	errptr = "Invalid Disposition";		break;
	case SUBMIT_RC_902_INVALID_JOBCLASS:	errptr = "Invalid Job Class";		break;
	case SUBMIT_RC_903_INVALID_ABORTACT:	errptr = "Invalid Abort action";	break;
	case SUBMIT_RC_904_INVALID_TIME:	errptr = "Invalid Time limit";		break;
	case SUBMIT_RC_905_INVALID_LIMIT:	errptr = "Invalid Limit flag";		break;
	case SUBMIT_RC_1000_ABORTED:		errptr = "Aborted by user";		break;
	default:				errptr = "Unknown return code";		break;
	}
	
	return errptr;
}


/*
**	History:
**	$Log: submit.c,v $
**	Revision 1.65  2011/10/29 20:09:14  gsl
**	Fix ISO routine name warnins on WIN32
**	
**	Revision 1.64  2009/10/18 20:26:29  gsl
**	Remove obsolete win32std.h
**	
**	Revision 1.63  2007/08/08 18:54:51  gsl
**	TT#74 file names with embedded spaces.
**	use WL_cobx2cstr() to convert the cobol input field to a C string.
**	
**	Revision 1.62  2003/03/19 20:32:11  gsl
**	error checking for blank vol/lib and blank defaults
**	
**	Revision 1.61  2003/03/19 20:30:09  gsl
**	error checking for blank vol/lib and blank defaults
**	
**	Revision 1.60  2003/03/19 20:03:17  gsl
**	Standardize SUBMIT return code defines
**	
**	Revision 1.59  2003/02/05 15:23:59  gsl
**	Fix -Wall warnings
**	
**	Revision 1.58  2003/02/04 17:05:01  gsl
**	Fix -Wall warnings
**	
**	Revision 1.57  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.56  2003/01/21 16:59:40  gsl
**	fix SETSUBMIT va_start() call
**	
**	Revision 1.55  2003/01/20 17:01:56  gsl
**	Change to use stdarg.h
**	plus document args
**	
**	Revision 1.54  2002/12/10 17:09:16  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.53  2002/12/09 21:09:32  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.52  2002/12/09 19:15:34  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.51  2002/12/04 21:01:52  gsl
**	FIx const char * warnings
**	
**	Revision 1.50  2002/12/04 20:52:17  gsl
**	Add to OPTIONS file
**	WPROC
**	WPROCDEBUG
**	ACPCONFIG
**	ACPMAP
**	WISP_SCRATCH_MODE/WISPSCRATCHMODE
**	WISP_DISPLAY_8BIT/DISPLAY8BIT/WISPDISPLAY8BIT
**	WISPSYSADMIN
**	
**	Revision 1.49  2002/08/20 16:09:49  gsl
**	Add support for Micro Focus Shared Object files .so/.sl
**	
**	Revision 1.48  2002/07/29 15:46:49  gsl
**	getwfilext -> WGETFILEXT
**	setwfilext -> WSETFILEXT
**	setwispfilext -> WSETFILEXT
**	
**	Revision 1.47  2002/07/16 16:24:51  gsl
**	Globals
**	
**	Revision 1.46  2002/07/12 20:40:38  gsl
**	Global unique WL_ changes
**	
**	Revision 1.45  2002/07/12 19:10:17  gsl
**	Global unique WL_ changes
**	
**	Revision 1.44  2002/07/12 17:01:01  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.43  2002/07/11 20:29:14  gsl
**	Fix WL_ globals
**	
**	Revision 1.42  2002/07/11 14:52:51  gsl
**	Fix WL_ globals
**	
**	Revision 1.41  2002/07/10 21:05:26  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.40  2002/07/09 04:13:57  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.39  2002/07/08 21:10:11  gsl
**	Alpha now uses setpgrp(pid,pid) format
**	
**	Revision 1.38  2002/07/02 21:15:29  gsl
**	Rename wstrdup
**	
**	Revision 1.37  2002/06/25 15:21:53  gsl
**	Change to use wmalloc()
**	
**	Revision 1.36  2002/06/21 20:49:29  gsl
**	Rework the IS_xxx bit flags and the WFOPEN_mode flags
**	
**	Revision 1.35  2002/06/21 03:10:41  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.34  2001/11/27 20:43:41  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.33  1999-09-27 09:09:51-04  gsl
**	FIx call to setpgrp() for OSF1 (Digital UNIX)
**
**	Revision 1.32  1999-01-19 17:06:25-05  gsl
**	For BSD check add defines BSD and _BSD
**
**	Revision 1.31  1999-01-19 10:27:31-05  gsl
**	Fix the ifdef around setpgrp() to BSD
**
**	Revision 1.30  1998-08-03 17:14:22-04  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks.
**
**	Revision 1.29  1998-07-07 17:48:40-04  gsl
**	Add unix tracing of the exec calls
**
**	Revision 1.28  1998-05-05 17:35:38-04  gsl
**	WIN32 change spawn mode to new flags
**
**	Revision 1.27  1998-05-05 13:21:25-04  gsl
**	Update spawn flags for WIN32
**
**	Revision 1.26  1997-12-04 18:12:43-05  gsl
**	changed to wispnt.h
**
**	Revision 1.25  1997-10-02 08:57:26-04  gsl
**	fix warning
**
**	Revision 1.24  1997-08-27 15:15:34-04  gsl
**	Apply same change to unix code
**
**	Revision 1.23  1997-08-27 15:05:22-04  gsl
**	FIx error on WIN32,
**	Was saving a pointer to a local var which went out of scope
**
**	Revision 1.22  1997-08-23 18:37:29-04  gsl
**	Move the batch options routines to wperson.c
**
**	Revision 1.21  1997-08-23 17:12:26-04  gsl
**	Rewrite all the WIN32 submit logic when using AQM of other queue manager.
**	Add support for REXEC a QSUBMIT command from WIndows 95.
**	Add support of username and password required by AQM and REXEC
**
**	Revision 1.20  1997-07-16 15:09:14-04  gsl
**	Change for WIN32 to always set GID=-1
**
**	Revision 1.19  1997-05-12 17:21:54-04  gsl
**	Correct submit for WIN32
**	Added tracing, fix submit to WPROC, added all the setting of environment
**	variables correctly init the new process
**
**	Revision 1.18  1997-05-01 17:21:35-04  gsl
**	removed unused hasclass from NT
**
**	Revision 1.17  1997-05-01 16:39:19-04  gsl
**	Remmove jclassp stuff for NT as is not used
**
**	Revision 1.16  1997-02-25 09:52:17-05  gsl
**	Correct options size
**
**	Revision 1.15  1996-12-10 19:30:44-05  jockc
**	added code to move AQM log files to VOLSPL/AQMLOG..
**	added missing free()'s for a few malloc'd blocks
**
**	Revision 1.14  1996-12-09 11:04:34-08  jockc
**	cleaned up some warnings (included headers for proto)...
**	added win32 submit code
**
**	Revision 1.13  1996-10-08 17:26:30-07  gsl
**	replaced shell_var() with wispshellexe()
**	replaced getenv() with WL_wprocexe()
**
**	Revision 1.12  1996-09-03 14:42:56-07  gsl
**	For NT add error message "NOT YET IMPLEMENTED"
**
**	Revision 1.11  1996-07-09 16:56:18-07  gsl
**	Fix prototypes and includes for NT
**	Reuse unix code for NT.
**	Wrote a stub winnt_submit() routine **** NOT FINISHED YET ***
**
**	Revision 1.10  1996-05-08 06:16:10-07  gsl
**	Fix call to setpgrp()
**
 * Revision 1.9  1995/07/10  10:23:55  gsl
 * Fixed problem J&M was having with LBM batchqueue.
 * The shell_cmd variable was only 256 long.
 * Now it will malloc the size needed.
 * Also added error checking on the SETSUBMIT
 *
**
**
**	05/29/92	Fix RETURN-CODE, it is not optional, Wang manual was wrong. GSL
**	06/10/92	Removed the unused "flags" args from unix_submit(). GSL
**	07/24/92	Changed to use new wfname() and runtype() UNIX. GSL
**	11/17/92	Added SETSUBMIT params to unix_submit of EXEC & PROC. GSL
**
*/
