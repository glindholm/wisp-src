			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		submit.c
**
**	Purpose:	To emulate the Wang SUBMIT VSSUB
**
**	Routines:	SUBMIT()		The SUBMIT routine
**			SETSUBMIT()		Sets up parameters for next SUBMIT
**			unix_submit()		The unix implementation of SUBMIT
**			fixerr()		Fix the unix error codes to be Wang error codes
**
**
**	History:
**			mm/dd/yy	Written by OLD
**			05/29/92	Fix RETURN-CODE, it is not optional, Wang manual was wrong. GSL
**			06/10/92	Removed the unused "flags" args from unix_submit(). GSL
**			07/24/92	Changed to use new wfname() and runtype() UNIX. GSL
**			11/17/92	Added SETSUBMIT params to unix_submit of EXEC & PROC. GSL
*/

#ifndef MSDOS						/* There is no "background" concept for MSDOS, so, this is removed.	*/

#include <varargs.h>
#define  NAME_LENGTH 80

#ifdef unix
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#include <signal.h>
static int unix_submit();
static int fixerr();
#endif

#ifdef VMS
#include <ssdef.h>
#include <rmsdef.h>
#include <libdef.h>
#include <descrip.h>
#endif

#include "idsistd.h"
#include "que_jobs.h"
#include "wcommon.h"
#include "wperson.h"
#include "movebin.h"
#include "werrlog.h"
#include "wdefines.h"
#include "filext.h"

#ifdef unix
#include "wrunconf.h"
#include "runtype.h"
#endif

char	*strchr();
char	*shell_var();

#define WPERM 28
#define WFNOTFOUND 20
#define WIOERR 44
#define WNOTMTD 4
#define WINVPROG 52
#define WLEXCEED 24
#define WNOMEM 60
#define WVOLBUSY 8
#define WDNOTFOUND 16

/* 				SUBMIT a command procedure to the standard batch queue						*/

SUBMIT(va_alist)									/* Variable number of arguments.	*/
va_dcl											/* Define the list structure.		*/
{
#define		ROUTINE		64000
	va_list the_args;								/* Define a pointer to the list.	*/
	int arg_count, args_remaining, i;						/* Number of arguments.			*/
	char *l_file,									/* Address of the filename.		*/
	     l_lib[9],									/* Local library name.			*/
	     l_vol[7],									/* Local volume name.			*/
	     *l_job_name,								/* Address of the job name.		*/
	     l_status,									/* Address of the job submit status.	*/
	     l_disposition,								/* Address of the disposition action.	*/
	     l_job_class,								/* Address of the job class value.	*/
	     l_abort_action,								/* Address of the abort action.		*/
	     l_cpu_limit_action,							/* Address of the CPU limit action.	*/
	     *dummy_arg;								/* Used to burn off extra args.		*/
	char	*ptr;

	char *end_name, *wfname();

	int  *l_cpu_limit,								/* Address of 4 byte int. value.	*/
 	     *l_return_code,								/* Address of 4 byte return code value.	*/
	     dummy_return_code;

	int4 rc;
	int cpulim;
	char name[132],qname[40];
	int4 mode;
	char jobname[9];
	int flags;
	int4 wang_retcod;								/* Value expected from Wang COBOL.	*/
#ifdef VMS
	pq_id *pq_ptr;									/* Pointer to the proc queue list.	*/
	char result[132], *context;							/* Vars to use with lib$find_file	*/
#include "submit.d"
#endif

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	va_start(the_args);								/* Set pointer to top of stack.		*/
	arg_count = va_count(the_args);							/* Determine the number of arguments.	*/
	args_remaining = arg_count;							/* Set a working variable.		*/
	va_start(the_args);								/* Reset pointer to top of stack.	*/

	l_file = va_arg(the_args, char*);						/* Get address of the filename.		*/
	args_remaining--;								/* One less argument.			*/

	wpload();									/* Get usage constants.			*/
	wang_retcod = 0;

	memset( l_vol, ' ', 6 );
	memset( l_lib, ' ', 8 );

	if (args_remaining > 1)								/* Are we NOT at the last arg ?		*/
	{
		ptr = va_arg(the_args, char*);						/* Get address of the library name.	*/
		args_remaining--;							/* One less argument.			*/
		if (ptr)
		{
			memcpy(l_lib,ptr,8);
		}
	}
	if (l_lib[0] == ' ') 
	{
		get_defs(DEFAULTS_PL,l_lib);						/* Get the PROGLIB default.		*/
	}
	l_lib[8] = '\0';

	if (args_remaining > 1)								/* Are we NOT at the last arg ?		*/
	{
		ptr = va_arg(the_args, char*);						/* Get address of the volume name.	*/
		args_remaining--;							/* One less argument.			*/
		if (ptr)
		{
			memcpy(l_vol,ptr,6);
		}
	}
	if (l_vol[0] == ' ') 
	{
		get_defs(DEFAULTS_PV,l_vol);						/* Get the PROGVOL default.		*/
	}
	l_vol[6] = '\0';

	if (args_remaining > 1)								/* Are we NOT at the last arg ?		*/
	{
		l_job_name = va_arg(the_args, char*);					/* Get address of the job name.		*/
		memcpy(jobname,l_job_name,8);
		jobname[8] = '\0';
		args_remaining--;							/* One less argument.			*/
	}                                                                               
	else
	{
		jobname[0] = '\0';							/* Use default jobname.			*/
	}

	if (*jobname == ' ') *jobname = '\0';						/* If spaces, make blank.		*/

	l_status = ' ';
	if (args_remaining > 1)								/* Are we NOT at the last arg ?		*/
	{
		l_status = *va_arg(the_args, char*);					/* Get address of the status value.	*/
		args_remaining--;							/* One less argument.			*/
		if (!strchr("RH ",l_status)) wang_retcod = 900;				/* If invalid status.			*/
	}
	if (l_status == ' ')
	{
		get_defs(DEFAULTS_JS,&l_status);
	}

	if (args_remaining > 1)								/* Are we NOT at the last arg ?		*/
	{
		l_disposition = *va_arg(the_args, char*);				/* Get the address of the disp. value.	*/
		args_remaining--;							/* One less argument.			*/
		if (!strchr("DRPO",l_disposition)) wang_retcod = 901;			/* If invalid disposition.		*/
	}
	else	l_disposition = 'D';

	l_job_class = ' ';
	if (args_remaining > 1)								/* Are we NOT at the last arg ?		*/
	{
		l_job_class = *va_arg(the_args, char*);					/* Get the address of the class value.	*/
		args_remaining--;							/* One less argument.			*/
		if (!strchr("ABCDEFGHIJKLMNOPQRSTUVWXYZ ",l_job_class)) wang_retcod = 902; /* If invalid job class.		*/
	}
	if (l_job_class == ' ')
	{
		get_defs(DEFAULTS_JC,&l_job_class);
	}

	if (args_remaining > 1)								/* Are we NOT at the last arg ?		*/
	{
		l_abort_action = *va_arg(the_args, char*);				/* Get address of abort action value.	*/
		args_remaining--;							/* One less argument.			*/
		if (!strchr("DNR",l_abort_action)) wang_retcod = 903;			/* If invalid abort action.		*/
	}
	else	l_abort_action = 'R';

	if (args_remaining > 1)								/* Are we NOT at the last arg ?		*/
	{
		l_cpu_limit = va_arg(the_args, int*);					/* Get address of the cpu time limit.	*/
		args_remaining--;							/* One less argument.			*/
		if ((*l_cpu_limit < 0 && *l_cpu_limit != -1) || *l_cpu_limit > 99 )	/* If invalid CPU time limit.		*/
		{
			wang_retcod = 904;
		}
	}
	else	l_cpu_limit = 0;

	if (args_remaining > 1)								/* Are we NOT at the last arg ?		*/
	{
		l_cpu_limit_action = *va_arg(the_args, char*);				/* Get address of the cpu limit action.	*/
		args_remaining--;							/* One less argument.			*/
		if (!strchr("CPW",l_cpu_limit_action)) wang_retcod = 905;		/* If invalid limit flag.		*/
	}
	else	l_cpu_limit_action = 'W';
         
	/*
	**	The RETURN-CODE is NOT an optional argument.  However there is an error in the Wang VSSUB manual which
	**	says it is an optional argument. (The examples show otherwise.)
	*/
	if (args_remaining >= 1)							/* Are we at the last arg ?		*/
	{
		l_return_code = va_arg(the_args, int*);					/* Get the address of the return code.	*/
	}
	else
	{
		l_return_code = &dummy_return_code;
	}

	if (wang_retcod)								/* If the return code has been set	*/
	{
		wswap( &wang_retcod );							/* Set up so is returned correctly.	*/
		PUTBIN(l_return_code,&wang_retcod,sizeof(int4));
		return;
	}

#ifdef VMS
	loadpad(WISPFILEXT,"COM",sizeof(WISPFILEXT));					/* NOTE: can only submit .COM under VMS.*/
#endif
	mode = IS_SUBMIT;
	end_name = wfname(&mode,l_vol,l_lib,l_file,name);				/* now make a name for ourselves...	*/
	*end_name = '\0';								/* Null terminate properly.		*/

	if (l_cpu_limit)
	{
		cpulim = *l_cpu_limit;
		wswap(&cpulim);								/* Get requested cpu limit. (1/100 sec)	*/
		if (cpulim == -1)							/* Use usage constants.			*/
		{
			char	def_proc_cpu[6];
			get_defs(DEFAULTS_JL,def_proc_cpu);

			cpulim  = (def_proc_cpu[0] - '0') * 36000;			/* Do tens of hours.			*/
			cpulim += (def_proc_cpu[1] - '0') * 3600;			/* Do hours.				*/
			cpulim += (def_proc_cpu[2] - '0') * 600;			/* Tens of minutes.			*/
			cpulim += (def_proc_cpu[3] - '0') * 60;				/* Minutes.				*/
			cpulim += (def_proc_cpu[4] - '0') * 10;				/* Tens of seconds.			*/
			cpulim += (def_proc_cpu[5] - '0');				/* Seconds.				*/
			cpulim *= 100;							/* Time is in hundredths.		*/
		}
	}
	else
	{
		cpulim = 0;
	}

#ifdef VMS
	pq_ptr = get_pq_list();								/* Get a pointer to the queue list	*/

	do
	{
		if (pq_ptr->class == l_job_class) break;				/* look for the queue that matches	*/
		pq_ptr = (pq_id *) pq_ptr->next;					/* next one				*/
	} while (pq_ptr);

	if (pq_ptr)	strcpy(qname,pq_ptr->qname);					/* Send the printout to the right queue.*/
	else		qname[0] = '\0';						/* Or send it nowhere.			*/
	flags = 0;
	if (l_status == 'H') flags |= Q_HOLD_JOB;					/* Hold the job if they want.		*/
	context = 0;
	f_desc.dsc$w_length = strlen(name);						/* Set the length of the descriptor.	*/
	rc = lib$find_file(&f_desc,&r_desc,&context,0,0,0,0);				/* See if the file exists first.	*/
	lib$find_file_end(&context);							/* End the FIND_FILE context.		*/
	if (rc == SS$_NORMAL || rc == RMS$_NORMAL)					/* If LIB$FIND_FILE was successful	*/
	{
		rc = que_job(BATCH_QUEUE,qname,name,jobname,cpulim,0,flags);		/* so send to the queue.		*/
	}

	if (rc == SS$_NORMAL || rc == RMS$_NORMAL)					/* Queue of job was successful.		*/
	{
		rc = 0;
		wswap( &rc );
		PUTBIN(l_return_code,&rc,sizeof(int4));					/* Return code from queued job.		*/
	}
	else
	{										/* Set return code from queued job:	*/
		wang_retcod = 56;							/* Default return code.			*/
		if (rc == RMS$_DNR || rc == RMS$_DPE) wang_retcod = 4;			/* Device not ready or			*/
											/* Device positioning error.		*/
		else if (rc == RMS$_WLK) wang_retcod = 8;				/* Device currently write locked when	*/
											/* write access was attempted.		*/
		else if (rc == RMS$_FLK) wang_retcod = 12;				/* File is locked by another user.	*/
		else if (rc == RMS$_DNF || rc == RMS$_DEV) wang_retcod = 16;		/* Directory not found.			*/
		else if (rc == RMS$_FNF) wang_retcod = 20; 				/* File not found.			*/
		else if (rc == RMS$_PRV) wang_retcod = 28;				/* Insufficient priveledge or file prot.*/
		else if (rc == LIB$_INVARG) wang_retcod = 40;				/* Invalid arguments.			*/
		else if (rc == RMS$_SPL) wang_retcod = 44;				/* Submit command file option to a 	*/
											/* Close service failed.		*/
		else if (rc == SS$_ACCVIO) wang_retcod = 52; 				/* Access violation.			*/

		wswap( &wang_retcod );							/* Set up so is returned correctly.	*/
		PUTBIN(l_return_code,&wang_retcod,sizeof(int4));
	}
#endif	/* #ifdef VMS */
#ifdef unix
	rc = unix_submit(name,jobname,l_job_class,l_vol,l_lib);
	wswap(&rc);
	PUTBIN(l_return_code,&rc,sizeof(int4));
#endif
}

/* These are referenced by que_job, and must be declared the same.								*/

#define MAX_SUBMIT_PARMS	8

short subp_len[MAX_SUBMIT_PARMS];							/* The length of each parm.		*/
char  subp_text[MAX_SUBMIT_PARMS][256];							/* The parm data.			*/
int   subp_num = 0;									/* The number of parms.			*/

/* Subroutine SETSUBMIT, used to set the values of the parms to be passed to a procedure when SUBMIT is used.			*/
/* CALL "SETSUBMIT" USING NUM-PARMS, PARM-1-LEN, PARM-1, PARM-2-LEN, PARM-2 ...							*/
/* NUM-PARMS is the number of parms to be passed, it is a 2 byte field. PARM-1-LEN thru PARM-N-LEN are also 2-bytes and describe*/
/* the length of each parm.													*/

SETSUBMIT(va_alist)									/* Variable number of arguments.	*/
va_dcl											/* Define the list structure.		*/
{
	va_list the_args;								/* Define a pointer to the list.	*/
	short arg_count;
	int i;
	char *ptr;
	va_start(the_args);								/* Set pointer to top of stack.		*/

	arg_count = *va_arg(the_args, short *);						/* Get argument count.			*/

	subp_num = 0;

	while (arg_count)								/* First copy all the new args.		*/
	{
		subp_len[subp_num] = *va_arg(the_args,short *);				/* Get the length.			*/
		ptr = va_arg(the_args, char *);						/* and the text.			*/
		memcpy(subp_text[subp_num],ptr,(int4) subp_len[subp_num]);		/* Copy to local area.			*/
		subp_text[subp_num][subp_len[subp_num]] = '\0';				/* Null terminate the parameters	*/
		subp_num++;
		arg_count--;
	}
}


#ifdef unix
static int unix_submit(filespec,jobname,jobclass,p_vol,p_lib)
char *filespec;										/* name of file to submit		*/
char *jobname;										/* name of job				*/
char  jobclass;										/* the jobclass				*/
char	*p_vol;										/* PROGVOL				*/
char	*p_lib;										/* PROGLIB				*/
{
	int 	pid,ftyp;
	char 	*p;									/* scratch pointer			*/
	int 	retcode, i, ii;
	char	*sh_parm[64];

	retcode = 0;

	if (!fexists(filespec))
	{
		retcode = 20;								/* 20 = file not found.			*/
	}

	if ( retcode == 0 )
	{
		ftyp = runtype(filespec);						/* Get the run type			*/

		switch(ftyp)
		{
		default:
		case RUN_NOT:
		case RUN_ACCESS:
		case RUN_UNKNOWN:
			retcode = 24;
			break;

		case RUN_EXEC:
		case RUN_ACUCOBOL:
		case RUN_MFINT:
		case RUN_MFGNT:
		case RUN_SHELL:
		case RUN_PROC:
		case RUN_PROCOBJ:
			break;
		}

	}


	if ( retcode == 0 )
	{
		switch (pid = fork())
		{
			case 0:								/* is child process 			*/
			{
				int	handle;
				char    envstring[80];
				char	buff[80];
				char	tty[20];
				char	*env_ptr;
				int	msize;
				int	pid;
				int	nice_value;

				signal( SIGHUP,  SIG_IGN );				/* Ignore HangUps & term signals.	*/
				signal( SIGTERM, SIG_IGN );

				zerolevel();						/* set link-level to zero		*/

				if (0 == getscmapnice(jobclass,&nice_value))
				{
					nice(nice_value);
				}

				setprogdefs(p_vol,p_lib);				/* Set up PROGLIB and PROGVOL		*/
				clearprogsymb();					/* Clear PROGLIB/VOL from symbol	*/

				pid = getpid();
				sprintf(envstring,"%s=%06d",WISP_PID_ENV,pid);		/* Save the PID in the environment.	*/
				if (setenvstr(envstring))
				{
					werrlog(ERRORCODE(4),envstring,0,0,0,0,0,0,0);
					wexit(ERRORCODE(4));
				}

				if (getenv(WISP_CANCELEXIT_ENV))			/* If CANCEL EXIT set then		*/
				{
					sprintf(envstring,"%s=0",WISP_CANCELEXIT_ENV);	/* Turn off CANCEL EXIT			*/
					if (setenvstr(envstring))
					{
						werrlog(ERRORCODE(4),envstring,0,0,0,0,0,0,0);
						wexit(ERRORCODE(4));
					}
				}

				if ( ! wbackground() )					/* If not already in background		*/
				{
					ttyid5(tty);
					sprintf(envstring,"%s=%s",WISP_TTY_ENV,tty);	/* Save the TTY in the environment.	*/
					if (setenvstr(envstring))
					{
						werrlog(ERRORCODE(4),envstring,0,0,0,0,0,0,0);
						wexit(ERRORCODE(4));
					}

					sprintf(envstring,"%s=true",WISP_BACKGROUND_ENV); /* Set BACKGROUND flag in environment.*/
					if (setenvstr(envstring))
					{
						werrlog(ERRORCODE(4),envstring,0,0,0,0,0,0,0);
						wexit(ERRORCODE(4));
					}
				}

				if (getenv(WISP_GID_ENV))				/* If GID was set then reset it.	*/
				{
					sprintf(envstring,"%s=%d",WISP_GID_ENV,pid);
					if (setenvstr(envstring))
					{
						werrlog(ERRORCODE(4),envstring,0,0,0,0,0,0,0);
						wexit(ERRORCODE(4));
					}
				}

				setpgrp(pid,pid);					/* Set the PROCESS GROUP ID		*/

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

					execvp( sh_parm[0], sh_parm );			/* Do the exec				*/

					retcode=fixerr(errno);				/* xlat unix errno to wang error# 	*/
					werrlog(ERRORCODE(6),filespec,errno,retcode,0,0,0,0,0);
					break;

				case RUN_SHELL:
					sh_parm[0] = shell_var();			/* exec the shell			*/
					sh_parm[1] = filespec;				/* shell script to exec			*/
					for (ii=0; ii < subp_num; ii++)			/* load SETSUBMIT parameters		*/
					{
						sh_parm[ii+2] = subp_text[ii];
					}
					sh_parm[ii+2] = '\0';				/* Null terminate the arg list		*/

					execvp( sh_parm[0], sh_parm );			/* Do the exec				*/

					retcode=fixerr(errno);
					werrlog(ERRORCODE(6),sh_parm[0],errno,retcode,0,0,0,0,0);
					break;

				case RUN_ACUCOBOL:
				case RUN_MFINT:
				case RUN_MFGNT:
					{
						struct wruncfg cfg;
						char	options[80];
						char	*eptr, *optr;
						int	arg;

						wrunconfig(&cfg);
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

						execvp(sh_parm[0],sh_parm);			/* do the EXEC			*/

						/*
							If we get here then there is an error and the exec failed.
						*/

						retcode=fixerr(errno);
						werrlog(ERRORCODE(6),sh_parm[0],errno,retcode,0,0,0,0,0);
						break;
					}

				case RUN_PROC:
				case RUN_PROCOBJ:
					{
						char	*name_ptr;

						if ( !(name_ptr = (char *)getenv("WPROC")) )	/* Get the name of "wproc"	*/
						{
							name_ptr = "wproc";		/* Use "wproc" as the default name	*/
						}
						sh_parm[0] = name_ptr;			/* argv[0] is the "wproc" program	*/
						sh_parm[1] = filespec;			/* argv[1] in the filespec	 	*/
						for (ii=0; ii < subp_num; ii++)		/* load SETSUBMIT parameters		*/
						{
							sh_parm[ii+2] = subp_text[ii];
						}
						sh_parm[ii+2] = '\0';			/* Null terminate the arg list		*/

						execvp(sh_parm[0],sh_parm);

						retcode=fixerr(errno);
						werrlog(ERRORCODE(6),sh_parm[0],errno,retcode,0,0,0,0,0);
						break;
					}
				}
				exit(retcode);
				break;
			}
			case -1:							/* Fork failed				*/
			{
				werrlog(ERRORCODE(8),errno,0,0,0,0,0,0,0);

				retcode = 12;						/* 12 = all buffer full.		*/
				break;
			}
			default:							/* is parent process	 		*/
			{
				retcode = 0;
				break;
			}
		}
	}

	werrlog(ERRORCODE(3),filespec,retcode,0,0,0,0,0,0);

	subp_num = 0;									/* Clear any SETSUBMIT parameters	*/

	return retcode;
}

static int fixerr(code)
int code;
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
#endif	/* #ifdef unix */

#endif	/* #ifndef MSDOS */
#ifdef MSDOS
SUBMIT()
{
	werrlog(102,"SUBMIT: Not Yet Implemented",0,0,0,0,0,0,0);
}
SETSUBMIT()
{
	werrlog(102,"SETSUBMIT: Not Yet Implemented",0,0,0,0,0,0,0);
}
#endif /* MSDOS */

