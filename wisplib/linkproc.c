			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
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
#include <a.out.h>
#include <errno.h>
#include <ctype.h>
#include <signal.h>
#endif

#ifdef VMS
#include <ssdef.h>
#include <rmsdef.h>
#include <climsgdef.h>
#endif

#include <stdio.h>
#include <v/video.h>
#include <varargs.h>									/* Function uses variable params.	*/

#include "wdefines.h"
#include "movebin.h"
#include "werrlog.h"
#include "wperson.h"
#include "wfiles.h"
#include "wcommon.h"

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

LINKPROC(va_alist)									/* There are a variable number of args.	*/
va_dcl
{
	va_list		the_args;
	char 		*progname,*libname,*volname;
	short		parmcnt;
	short		parmlen[MAX_LINKPROC_PARMS];
	char		parmtext[MAX_LINKPROC_PARMS][256];
	long		*retcode, *compcode;
	char 		testname[NAME_LENGTH+1];
	char 		pname[NAME_LENGTH+1];
	long 		mode;
	int 		i;
	char 		*p;
	int  		not_found,cpos;
	int 		pid,ftyp;
	char 		*sh_parm[17], *wfname();
	int		arg_count;
	long		l_retcode, l_compcode;
	char		l_lib[9], l_vol[7];

#ifdef VMS
	char		command[516];							/* A buffer for the vms command.	*/
	long		status;
#endif

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
		return(0);
	}

	mode = IS_SUBMIT;
	p = wfname(&mode,l_vol,l_lib,progname,testname);				/* expand the name 			*/
	*p = (char)0;

#ifdef VMS
	memset(command,' ',516);							/* Init the command string.		*/
	command[0] = '@';								/* Start command with an '@' symbol.	*/
	strcpy(&command[1],testname);							/* Now add the name of the procedure.	*/
	cpos = strlen(testname) + 1;							/* Find out the end of the string.	*/
#endif

	for( i=0; i<parmcnt;i++)
	{
		parmlen[i]   = *(va_arg(the_args, short*));				/* Get the parameter length.		*/
		memcpy(parmtext[i], va_arg(the_args, char*), parmlen[i]);		/* Get the parameter.			*/
		arg_count -= 2;
		parmtext[i][parmlen[i]] = '\0';
#ifdef VMS
		if ((cpos + parmlen[i]) > 512)
		{
			werrlog(ERRORCODE(6),parmcnt,0,0,0,0,0,0,0);			/* Write an error, line too big.	*/
			exit(-1);							/* BAD.					*/
		}

		command[cpos++] = ' ';							/* Insert a space.			*/
		command[cpos++] = '\"';							/* And Quotes.				*/
		memcpy(&command[cpos],parmtext[i],parmlen[i]);
		cpos = cpos + parmlen[i];
		command[cpos++] = '\"';							/* Close Quotes.			*/
#endif
	}

	retcode = va_arg(the_args, long*);						/* The return code from the program.	*/
	arg_count--;
	compcode = 0;
	if ( arg_count == 1 )
	{
		compcode = va_arg(the_args, long*);
	}

	va_end(the_args);
#ifdef VMS
	command[cpos] = '\0';								/* Null terminate.			*/
	status = spawn(3,command,"");							/* Then execute the command.		*/
	if (status != SS$_NORMAL && status != RMS$_NORMAL && status != CLI$_NORMAL)
	{
		if (status == RMS$_DNF || status == RMS$_DEV || RMS$_FNF) status = 20;	/* File not found.			*/
		else if (status == RMS$_PRV) status = 28;				/* Insufficient priveledge or file prot.*/
		else status = 52;							/* Invalid file.			*/
	}
	else status = 0;								/* Successful.				*/
	wswap( &status );
	PUTBIN(retcode,&status,4);
	return;
#endif

#ifdef unix
	memset(pname,(char)0,sizeof(pname));

	not_found = findexts(testname,pname,0);

	if ( not_found )
	{
		long twenty=20;
		wswap( &twenty );
		PUTBIN(retcode,&twenty,sizeof(twenty));					/* File Not Found			*/
		werrlog(ERRORCODE(3),testname,0,0,0,0,0,0,0);
		return;
	}

	ftyp = isexec(pname);								/* decide if it's exec'able or 		*/
	switch(ftyp)
	{
	case NOTEXEC:
		break;

	default:
	case ISEXEC:
	case ISACU:
	case ISMFINT:
		{
			long	l52=52;							/* Invalid file type.			*/
			wswap( &l52 );
			PUTBIN(retcode,&l52,4);
			werrlog(ERRORCODE(7),pname,0,0,0,0,0,0,0);
			return;
			break;
		}

	case ACCERR:
		{
			long	l28=28;							/* Access denied.			*/
			wswap( &l28 );
			PUTBIN(retcode,&l28,4);
			werrlog(ERRORCODE(5),pname,0,0,0,0,0,0,0);
			return;
			break;
		}
	}

	vexit();									/* Reset the terminal.			*/

	signal(SIGCLD,  SIG_DFL);							/* Use Default DEATH-OF-CHILD signal	*/

	*retcode = 0;
	switch (pid = fork())
	{
		case 0:									/* is child process 			*/
		{
			setprogdefs(l_vol,l_lib);					/* Set up PROGLIB and PROGVOL		*/
			clearprogsymb();						/* Clear PROGLIB/VOL from symbol	*/

			memset(sh_parm,(char)0,sizeof(sh_parm));
			sh_parm[0]=(char *)shell_var();					/* std argv[0] is progname 		*/
			sh_parm[1]=pname;						/* name of shell script for /bin/sh 	*/
			for (i=0; i<parmcnt; i++)					/* start at elem 1 in parm_list, 	*/
				sh_parm[i+2]=parmtext[i];				/* elem 2 in sh_parm 			*/
			sh_parm[i+2] = '\0';						/* null terminate it			*/
			execvp(sh_parm[0],sh_parm);
			werrlog(ERRORCODE(4),pname,errno,0,0,0,0,0,0);
			*retcode=fixerr(errno);
			exit(*retcode);
		}
		default:								/* is parent process 			*/
		{
			wwaitpid(pid,&l_compcode);
			l_retcode = 0;
			vsynch();
			wpl_usr(&defaults);
			rts_first = TRUE ;					/* Set so return from link re-inits screen	*/
		}
	}

	signal(SIGCLD,  SIG_IGN);							/* Ignore DEATH-OF-CHILD signal		*/

	wswap( &l_retcode );
	PUTBIN(retcode,&l_retcode,4);

	if ( compcode )
	{
		wswap( &l_compcode );
		PUTBIN(compcode,&l_compcode,4);
	}
	return;

#endif
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
