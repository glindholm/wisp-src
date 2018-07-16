			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	linkproc.c
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

#ifdef unix
#include <errno.h>
#include <ctype.h>
#include <signal.h>
#endif

#ifdef VMS
#include <ssdef.h>
#include <rmsdef.h>
#include <climsgdef.h>
#endif

#ifdef MSDOS
#include <process.h>
#endif

#include <stdio.h>
#include <v/video.h>
#include <varargs.h>									/* Function uses variable params.	*/

#include "idsistd.h"
#include "wdefines.h"
#include "movebin.h"
#include "werrlog.h"
#include "wperson.h"
#include "wfiles.h"
#include "wcommon.h"
#include "runtype.h"

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
#define		ROUTINE		28500

extern	int	rts_first ;								/* First time flag for screen init.	*/

#ifdef unix
static int fixerr();
#endif

char	*shell_var();

void LINKPROC(va_alist)									/* There are a variable number of args.	*/
va_dcl
{
	va_list		the_args;
	char 		*progname,*libname,*volname;
	short		parmcnt;
	short		parmlen[MAX_LINKPROC_PARMS];
	char		parmtext[MAX_LINKPROC_PARMS][256];
	int4		*retcode, *compcode;
	char 		filespec[NAME_LENGTH+1];
	int4 		mode;
	int 		i, idx;
	char 		*p;
	int  		not_found,cpos;
	int 		pid,ftyp;
	char 		*sh_parm[17], *wfname();
	int		arg_count;
	int4		l_retcode, l_compcode;
	char		l_lib[9], l_vol[7];
	int		savelevel;
	char		command[516];
	uint4		status;
	uint4		vms_status;

	l_retcode = 0;
	l_compcode = 0;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	va_start(the_args);
	arg_count = va_count(the_args);
	va_start(the_args);

	progname = va_arg(the_args, char*);						/* Get the program name.		*/
	libname = va_arg(the_args, char*);						/* Get the library name.		*/
	volname = va_arg(the_args, char*);						/* Get the volume name.			*/

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
		get_defs(DEFAULTS_PL,l_lib);						/* Get the PROGLIB default.		*/
	}
	if (l_vol[0] == ' ') 
	{
		get_defs(DEFAULTS_PV,l_vol);						/* Get the PROGVOL default.		*/
	}
	l_lib[8] = '\0';
	l_vol[6] = '\0';

	parmcnt = *(va_arg(the_args, short*));						/* Get the parameter count.		*/
	arg_count -= 4;
	if (parmcnt > MAX_LINKPROC_PARMS || parmcnt < 0)
	{
		werrlog(ERRORCODE(2),parmcnt,0,0,0,0,0,0,0);
		return;
	}

	mode = IS_SUBMIT;
	p = wfname(&mode,l_vol,l_lib,progname,filespec);				/* expand the name 			*/
	*p = (char)0;

#ifdef VMS
	memset(command,' ',sizeof(command));						/* Init the command string.		*/
	command[0] = '@';								/* Start command with an '@' symbol.	*/
	strcpy(&command[1],filespec);							/* Now add the name of the procedure.	*/
	cpos = strlen(filespec) + 1;							/* Find out the end of the string.	*/
#endif
#ifdef MSDOS
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
#ifdef VMS
		if ((cpos + parmlen[i]) > sizeof(command)-4)
		{
			werrlog(ERRORCODE(6),parmcnt,0,0,0,0,0,0,0);			/* Write an error, line too big.	*/
			wexit(-1);							/* BAD.					*/
		}

		command[cpos++] = ' ';							/* Insert a space.			*/
		command[cpos++] = '\"';							/* And Quotes.				*/
		memcpy(&command[cpos],parmtext[i],parmlen[i]);
		cpos += parmlen[i];
		command[cpos++] = '\"';							/* Close Quotes.			*/
#endif
#ifdef MSDOS
		if (parmlen[i] > 0)
		{
			if ((cpos + parmlen[i] + 3) > 128)
			{
				werrlog(ERRORCODE(6),parmcnt,0,0,0,0,0,0,0);		/* Write an error, line too big.	*/
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

#ifdef VMS
	savelevel = linklevel();							/* Save the link-level			*/
	newlevel();									/* increment the link-level		*/
	command[cpos] = '\0';								/* Null terminate.			*/
	status = spawn2 (3,command,"",&vms_status);					/* Then execute the command.		*/
	if (status != SS$_NORMAL && status != RMS$_NORMAL && status != CLI$_NORMAL)
	{
		if (status == RMS$_DNF || status == RMS$_DEV || RMS$_FNF) status = 20;	/* File not found.			*/
		else if (status == RMS$_PRV) status = 28;				/* Insufficient priveledge or file prot.*/
		else status = 52;							/* Invalid file.			*/
	}
	else 
	{
		status = 0;								/* Successful.				*/
		ppunlink(savelevel);							/* Putparm UNLINK			*/
	}
	wswap( &status );
	PUTBIN(retcode,&status,4);
	setlevel(savelevel);								/* Restore the link-level		*/
	return;
#endif

#ifndef VMS
	if ( !fexists(filespec) )
	{
		int4 twenty=20;
		wswap( &twenty );
		PUTBIN(retcode,&twenty,sizeof(twenty));					/* File Not Found			*/
		werrlog(ERRORCODE(3),filespec,0,0,0,0,0,0,0);
		return;
	}

	ftyp = runtype(filespec);							/* get the run type			*/

	switch(ftyp)
	{
	case RUN_SHELL:
		/*  This is the only type allowed to proceed. */
		break;

	case RUN_ACCESS:
	case RUN_UNKNOWN:
		{
			int4	l28=28;							/* Access denied.			*/
			wswap( &l28 );
			PUTBIN(retcode,&l28,4);
			werrlog(ERRORCODE(5),filespec,0,0,0,0,0,0,0);
			return;
			break;
		}
		break;

	default:
		{
			int4	l52=52;							/* Invalid file type.			*/
			wswap( &l52 );
			PUTBIN(retcode,&l52,4);
			werrlog(ERRORCODE(7),filespec,0,0,0,0,0,0,0);
			return;
			break;
		}
		break;
	}

	*retcode = 0;

#ifdef unix
	if (!ishelpactive() && isdebug())						/* If COBOL debugger running		*/
	{
		vraw_stty_save();							/* Save the current stty values		*/
	}
#endif /* unix */

	vexit();									/* Reset the terminal.			*/

#ifdef unix
	signal(SIGCLD,  SIG_DFL);							/* Use Default DEATH-OF-CHILD signal	*/

	switch (pid = fork())
	{
		case 0:									/* is child process 			*/
		{
			setprogdefs(l_vol,l_lib);					/* Set up PROGLIB and PROGVOL		*/
			clearprogsymb();						/* Clear PROGLIB/VOL from symbol	*/

			memset(sh_parm,(char)0,sizeof(sh_parm));
			idx=0;
			sh_parm[idx++]=shell_var();					/* std argv[0] is progname 		*/
			sh_parm[idx++]=filespec;					/* name of shell script for /bin/sh 	*/
			for (i=0; i<parmcnt; i++)					/* start at elem 1 in parm_list, 	*/
				sh_parm[idx++]=parmtext[i];				/* elem 2 in sh_parm 			*/
			sh_parm[idx] = '\0';						/* null terminate it			*/

			newlevel();							/* Increment the link-level		*/
			execvp(sh_parm[0],sh_parm);

			werrlog(ERRORCODE(4),filespec,errno,0,0,0,0,0,0);
			*retcode=fixerr(errno);
			exit(*retcode);
		}
		default:								/* is parent process 			*/
		{
			wwaitpid(pid,&l_compcode);
			l_retcode = 0;
		}
	}

	signal(SIGCLD,  SIG_IGN);							/* Ignore DEATH-OF-CHILD signal		*/

	if (!ishelpactive() && isdebug())						/* If COBOL debugger running		*/
	{
		vraw_stty_restore();							/* Restore the saved stty values	*/
	}
#endif /* unix */

#ifdef MSDOS
	newlevel();
	l_retcode = system(command);
	l_compcode = 0;
	oldlevel();
#endif /* MSDOS */

	vsynch();								/* Resync the video				*/
	load_defaults();							/* Reload defaults in case they changed		*/
	rts_first = TRUE ;							/* Set so return from link re-inits screen	*/

	ppunlink(linklevel());								/* Putparm UNLINK			*/

	wswap( &l_retcode );
	PUTBIN(retcode,&l_retcode,4);

	if ( compcode )
	{
		wswap( &l_compcode );
		PUTBIN(compcode,&l_compcode,4);
	}
	return;

#endif /* !VMS */
}

#ifdef unix
static fixerr(code)
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
#endif
