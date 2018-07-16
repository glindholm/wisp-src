			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	link.c	
*/

/*	LINK	Wang USERSUB LINK	*/

#include <stdio.h>
#include <varargs.h>									/* Function uses variable params.	*/

#ifdef unix
#include <a.out.h>
#include <errno.h>
#include <ctype.h>
#include <signal.h>
#include "wrunconf.h"
#endif

#ifdef VMS
#include <ssdef.h>
#include <libdef.h>
#include <rmsdef.h>
#include <descrip.h>
#include <climsgdef.h>
#endif

#ifdef MSDOS
#include <process.h>
#include <stdlib.h>
#include <errno.h>
#endif


#include "wglobals.h"
#include "wdefines.h"

#include "movebin.h"
#include "werrlog.h"
#include "wperson.h"
#include "wfiles.h"
#include "wcommon.h"
#include "cobrun.h"
#include <v/video.h>

#define MAX_CALLS	16								/* Maximum call depth			*/

#define	ROUTINE		28000

#define	MAX_VARGS	128								/* MAX_LINK_PARMS + control params too	*/

static	char	*varg_pointer[MAX_VARGS] ;						/* pointers to variable param list	*/
static	long	varg_length[MAX_VARGS] ;						/* length of each parameter in list	*/

static	int	link_arg_count;								/* Save link arg count			*/

static char rval[4];

#ifdef VMS
#define	NULL_FILE_PATH		"*NULL*"
#endif

#ifdef unix
char *shell_var();
static char link_filespec[NAME_LENGTH+1];						/* Native program path			*/
static char link_callspec[NAME_LENGTH+1];						/* the "CALL" spec			*/
#endif

extern char WISPRETURNCODE[3];
extern char WISPFILEXT[39];
extern	int	rts_first ;								/* First time flag for screen init.	*/

static	vms_descriptor = 0 ;								/* flag for vms descriptor arguments	*/
static	aix_link = 0 ;									/* flag for LINKAIX arguments		*/
static	mf_link = 0 ;									/* flag for LINKMF arguments		*/

LINK ( va_alist )									/* LINK: Load arg into table		*/
va_dcl											/* variable argument list		*/
{
	int	i ;
	va_list	link_args ;								/* descriptor arguments			*/

	va_start ( link_args ) ;							/* start the walk down the parameters	*/
	link_arg_count = va_count ( link_args ) ;					/* Find out how many args in the stack.	*/
	va_start ( link_args ) ;							/* restart the walk down the parameters	*/

	for ( i = 0 ; i < link_arg_count ; ++i )
	{
		varg_pointer[i] = va_arg( link_args, char * );				/* Get the next argument pointer	*/

		if ( acu_cobol )							/* If this is acu cobol;		*/
		{
			varg_length[i] = va_arg( link_args, long);			/* Get the argument length		*/
		}
		else									/* Else, this is NOT acu cobol;		*/
		{
			varg_length[i] = 0 ;						/* Set the argument length to zero	*/
		}
	}

	va_end ( link_args ) ;								/* end the parameter walk		*/

	do_link () ;									/* call LINK with no arg list		*/
}


#ifdef	VMS		/* Start VMS only block of code	*/				/* only if in vms			*/
LINKDESC ( va_alist )									/* LINK with vms descriptors		*/
va_dcl											/* variable argument list		*/
{
	int	i ;
	va_list	desc_args ;								/* descriptor arguments			*/
	struct  dsc$descriptor  *va_dsc ;						/* Descriptor pointer from va list      */


	va_start ( desc_args ) ;							/* start the walk down the parameters	*/
	link_arg_count = va_count ( desc_args ) ;					/* get the argument count		*/
	va_start( desc_args ) ;								/* restart the argument list		*/

	for ( i = 0 ; i < link_arg_count ; ++i )					/* for each argument in va_alist	*/
	{
		va_dsc = va_arg( desc_args, struct dsc$descriptor*);			/* Get the next descriptor		*/
		varg_pointer[i] = va_dsc->dsc$a_pointer ;				/* Get the argument pointer		*/
		varg_length[i] = va_dsc->dsc$w_length ;					/* Get the argument length		*/
	}

	va_end ( desc_args ) ;								/* end the parameter walk		*/

	vms_descriptor = 1 ;								/* set flag for descriptor arguments	*/
	do_link () ;									/* call LINK with no arg list		*/
	vms_descriptor = 0 ;								/* reset flag for no descriptors	*/
}
#endif	/* #ifdef VMS */			/* End VMS only block of code	*/

#ifdef unix
LINKAIX ( va_alist )									/* LINK for AIX VS COBOL		*/
va_dcl											/* variable argument list		*/
{
	va_list	link_args ;								/* descriptor arguments			*/
	int	i;
	long	*lenp;


	va_start ( link_args ) ;							/* start the walk down the parameters	*/
	link_arg_count = va_count ( link_args ) ;					/* Find out how many args in the stack.	*/
	va_start ( link_args ) ;							/* restart the walk down the parameters	*/

	for ( i = 0 ; i < link_arg_count ; ++i )
	{
		varg_pointer[i] = va_arg( link_args, char * );				/* Get the next argument pointer	*/
		varg_length[i] = va_arg( link_args, long);				/* Get the argument length		*/
	}

	va_end ( link_args ) ;								/* end the parameter walk		*/

	aix_link = 1 ;									/* set flag for descriptor arguments	*/
	do_link () ;									/* call LINK with no arg list		*/
	aix_link = 0 ;									/* reset flag for no descriptors	*/
}
#endif	/* #ifdef unix */

#ifndef VMS
LINKMF ( va_alist )									/* LINK for Micro Focus COBOL/2		*/
va_dcl											/* variable argument list		*/
{
	va_list	link_args ;								/* descriptor arguments			*/
	int	i;
	long	*lenp;


	va_start ( link_args ) ;							/* start the walk down the parameters	*/
	link_arg_count = va_count ( link_args ) ;					/* Find out how many args in the stack.	*/
	va_start ( link_args ) ;							/* restart the walk down the parameters	*/

	for ( i = 0 ; i < link_arg_count ; ++i )
	{
		varg_pointer[i] = va_arg( link_args, char * );				/* Get the next argument pointer	*/
		varg_length[i] = va_arg( link_args, long);				/* Get the argument length		*/
	}

	va_end ( link_args ) ;								/* end the parameter walk		*/

	mf_link = 1 ;									/* set flag for descriptor arguments	*/
	do_link () ;									/* call LINK with no arg list		*/
	mf_link = 0 ;									/* reset flag for no descriptors	*/
}
#endif	/* NOT VMS */

static	do_link()
{
	int	arg_length_known ;				/* (acu_cobol || vms_descriptor || aix_link || mf_link)		*/
	int	va_i ;									/* index for varg_pointer & length.	*/

											/* These are the arguments to LINK.	*/
	char 	*progname;								/* (1) Program Name			*/
	char	*ltype;									/* (2) Link Type			*/
	char 	*libname;								/* (3) Library				*/
	char	*volname;								/* (4) Volume				*/
	char	*arg_special;								/* (5) Special				*/
	char	*arg_ext_opt;								/* (6) Extended Options			*/
	long	parmcnt;								/* (7) Parameter Count			*/
	struct 	{ char *the_parm[MAX_LINK_PARMS]; } parm_list;				/* (8) Parm(s)				*/
	struct 	{ long  the_len [MAX_LINK_PARMS]; }  len_list;				/*     Parm lengths			*/
	char	*can_exit;								/* (9) Cancel Exit Flag			*/
	char	*mess;									/* (10) Message				*/
	long	messlen;								/* (11) Message Length			*/
	char	*helpflg;								/* (12) HELP disable			*/
	char	*pfkey;									/* (13) (reserved)			*/
	char	*cantext;								/* (14) Cancel Receiver			*/
	long	canlen;									/* (15) Cancel Receiver Length		*/
	long	*comcode;								/* (16) Completion Code			*/
	long 	*retcod;								/* (17) Return Code			*/

											/* These are used for all platforms.	*/
	int	to_do;
	char	*dummy_char;
	long	arg_used;
	char	*this_parm;
	int	arg_len;								/* lenght of the previous arg		*/
	int	i;
	char 	l_vol[7], l_lib[9];
	int	pathcnt;
	long	mode;
	int	not_found;
	char 	testname[NAME_LENGTH+1];
	int	typeext;
	long	retval,scrval;
	int	parm_file_written;

#ifndef MSDOS	/* unix and VMS only */

	char	lowname[9];
	long	status;
	int	k, try_upper, pid, ftyp, try_dot;
	long	tcomcode;								/* Temp comcode				*/
	char	**tmp, *strchr();
	char	*sh_parm[64], *p, *wfname();
	char	linkkey[80];								/* Key to link parm area		*/
	char	vmskey[80];								/* Key to link parm area		*/

	int	num;
	char	temp_retval[4];								/* Temp string to hold return value.	*/
	long	wang_retcod;								/* Value expected from Wang COBOL.	*/
	int	spawn_action;								/* Action to call spawn flag.		*/
	int	vmstry;									/* Flag to indicate search tries.	*/
	int	cpos;
	char	*cp;
#endif	/* #ifndef MSDOS (unix and VMS) */

#ifdef VMS
	char    spawn_name[256] ;							/* Full name with parameters for spawn  */
	char	*ptr ;									/* pointer to a char in file_path	*/
	char	progname_str[16] ;							/* string version of progname		*/
#endif	/* #ifdef VMS */


#ifndef unix	/* VMS and MSDOS only	*/
	char file_path[256];								/* Spawn file path is built here.	*/
#endif	/* #ifndef unix (VMS and MSDOS)	*/

#ifndef VMS	/* unix and MSDOS only	*/
	char	*osd_path();
#endif	/* #ifndef VMS (unix and MSDOS)	*/

	void (*old_SIGINT)();								/* Save old signals			*/
	void (*old_SIGQUIT)();

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	if (lnk_depth == MAX_CALLS)
	{
		werrlog(ERRORCODE(2),lnk_depth,0,0,0,0,0,0,0);
		return(0);
	}

	arg_length_known = acu_cobol || vms_descriptor || aix_link || mf_link ;		/* Any of these know arg lengths.	*/

	memset(&parm_list,0,sizeof(parm_list));						/* set parm_list to zeroes		*/

	to_do =	link_arg_count ; 							/* Get the arg count			*/
	va_i = 0 ;									/* Start looking at the arg table	*/

	progname = varg_pointer[va_i++] ;						/* Get the program name.		*/
	to_do--;

	if (!memcmp(progname,"DISPLAY ",8))						/* If they are calling DISPLAY, then	*/
	{										/* call WFILE_DISP.			*/
		setprogid(progname);
		wfile_disp();
		return(0);
	}

	libname = "	";								/* Give initial values.			*/
	volname = "      ";
	ltype = " ";
	parmcnt = 0L;
	can_exit = " ";
	helpflg = " ";
	messlen = 0;
	canlen = 0;
	retcod = &scrval;
	comcode = retcod;

	if (to_do < 3 ) goto get_retval;

	ltype = varg_pointer[va_i++] ;							/* Get the link type info.		*/
	to_do--;

	if (to_do < 3 ) goto get_retval;

	libname = varg_pointer[va_i++] ;						/* Get the library name.		*/
	to_do--;

	if (to_do < 3 ) goto get_retval;

	volname = varg_pointer[va_i++] ;						/* Get the volume name.			*/
	to_do--;

	if (to_do < 3 ) goto get_retval;

	dummy_char = varg_pointer[va_i++] ;						/* OPTIONAL get special			*/
	to_do--;
	arg_used = 0;

	if (	*dummy_char == ' ' ||
     		*dummy_char == 'T' ||
		*dummy_char == 'O' ||
     		*dummy_char == 'C' )
	{
		arg_special=dummy_char;
		arg_used = 1;
	}
	if ( arg_used )									/* If arg is used then get next one.	*/
	{
		if (to_do < 3 ) goto get_retval;

		dummy_char = varg_pointer[va_i++] ;					/* OPTIONAL get Extended options.	*/
		to_do--;
		arg_used = 0;
	}
	if (	*dummy_char == ' ' ||
     		*dummy_char == 'R' )
	{
		arg_ext_opt=dummy_char;
		arg_used = 1;
	}
	if ( arg_used )									/* If arg is used then get next one.	*/
	{
		if (to_do < 3 ) goto get_retval;

		dummy_char = varg_pointer[va_i++] ;					/* Get parmcnt.				*/
		to_do--;
		arg_used = 0;
	}
	GETBIN(&parmcnt,dummy_char,4);							/* The argument count.			*/
	wswap(&parmcnt);
	if ( parmcnt > MAX_LINK_PARMS )
	{
		werrlog(ERRORCODE(8),MAX_LINK_PARMS,parmcnt,0,0,0,0,0,0);
		wexit(ERRORCODE(8));
	}
	for (i=0; i<MAX_LINK_PARMS; i++)
	{
		parm_list.the_parm[i] = 0;						/* Clear out the pointers.		*/
	}
	if (parmcnt)									/* If there are arguments...		*/
	{										/* Then get their addresses.		*/
		for (i=0; i<parmcnt; i++)
		{
			this_parm = varg_pointer[va_i] ;				/* Get the argument pointer.		*/
			parm_list.the_parm[i] = this_parm;

			if ( arg_length_known )
			{
				arg_len = varg_length[va_i] ;				/* Get the argument length.		*/
				len_list.the_len[i] = arg_len;				/* Save length in a table.		*/
			}
			va_i++;								/* this time increment after assignment	*/
			to_do--;
		}

		for (i=parmcnt; i<MAX_LINK_PARMS; i++) 
		{
			parm_list.the_parm[i] = this_parm;				/* Fill rest with last parm.		*/
		}
	}
	if (to_do < 3 ) goto get_retval;

	can_exit = varg_pointer[va_i++] ;						/* Get the cancel exit flag.		*/
	to_do--;
	if (	*can_exit != 'C' && 
	    	*can_exit != 'N' && 
		*can_exit != 'D' && 
		*can_exit != 'P'   ) 
	{
		*can_exit = ' ';
	}
	if (to_do < 3 ) goto get_retval;

	mess = varg_pointer[va_i++] ;							/* Get the pf16 message string		*/
	to_do--;
	if (to_do < 3 ) goto get_retval;

	messlen = *(varg_pointer[va_i++]) ;						/* Get the pf16 message length.		*/
	wswap(&messlen);
	to_do--;
	if (to_do < 3 ) goto get_retval;

	helpflg = varg_pointer[va_i++] ;						/* Get the help disable flag.		*/
	to_do--;
	if (to_do < 3 ) goto get_retval;

	pfkey = varg_pointer[va_i++] ;							/* Get the pfkey flag mask		*/
	to_do--;
	if (to_do < 3 ) goto get_retval;

	cantext = varg_pointer[va_i++] ;						/* Get the cancel text receiver.	*/
	to_do--;
	if (to_do < 3 ) goto get_retval;

	canlen = *(varg_pointer[va_i++]) ;						/* Get the text receiver length.	*/
	wswap(&canlen);
	to_do--;
	if (to_do < 3 ) goto get_retval;

get_retval:
	comcode = (long *)varg_pointer[va_i++] ;					/* Get the completion code from link.	*/

	retcod = (long *)varg_pointer[va_i++] ;						/* Get the program return code.		*/


	if ( *ltype == ' ' )								/* Use Program vol & lib.		*/
	{
		get_defs(DEFAULTS_PV,l_vol);	/* NOTE: If PV/PL is not set then RV/RL	are returned */
		get_defs(DEFAULTS_PL,l_lib);
	}
	else if ( *ltype == 'P' )							/* Use arg3 & arg4			*/
	{
		memcpy( l_vol, volname, 6 );
		memcpy( l_lib, libname, 8 );
	}
	else if ( *ltype == 'S' )							/* look in system dirs			*/
	{
		pathcnt = 0;
		memset( l_vol, ' ', 6 );
		memset( l_lib, ' ', 8 );
	}
	else										/* Invalid link type.			*/
	{
		swap_put ( comcode, 8L ) ;						/* Unsuccessful link.			*/
		swap_put ( retcod, 40L ) ;						/* Invalid Parameter.			*/

		werrlog(ERRORCODE(3),progname,ltype,0,0,0,0,0,0);
		return(0);
	}

	l_vol[6] = '\0';
	l_lib[8] = '\0';

#ifdef VMS
/*************************************************  Start of VMS section  *******************************************************/

	wclink ( progname, &parm_list, &retval ) ;					/* Call the COBOL link. (in LINKSUBS.C)	*/

	if (retval == 1)								/* Static link was a success.		*/
	{
		retval = atol(WISPRETURNCODE);

		setretcode(WISPRETURNCODE);

		swap_put ( comcode, 0L ) ;						/* Successful link.			*/
		swap_put ( retcod, retval ) ;						/* Return code from link program.	*/

		return;
	}

	strncpy ( progname_str, progname, 8 ) ;						/* make progname a string		*/
	for ( i = 0 ; i < 8 ; ++i )
	{
		if ( progname_str[i] == ' ' )
		{
			break ;
		}
	}
	progname_str[i] = '\0' ;

	mode = IS_SUBMIT;								/* wfname flag = use .COM as extension	*/
	vmstry = 0;

vmsagain:
	if (*ltype == 'S')								/* Look in system area.			*/
	{
		switch ( vmstry )
		{
			case 0:
			{
				strcpy ( file_path, "SYS$SYSTEM:" ) ;
				strcat ( file_path, progname_str ) ;
				strcat ( file_path, ".COM" ) ;
				spawn_action = 3 ;					/* to start a .COM file			*/
				break ;
			}
			case 1:
			{
				strcpy ( file_path, "SYS$SYSTEM:" ) ;
				strcat ( file_path, progname_str ) ;
				strcat ( file_path, ".EXE" ) ;
				spawn_action = 1 ;					/* to start a .EXE file			*/
				break ;
			}
			case 2:
			{
				strcpy ( file_path, "WISP$ROOT:[COM]" ) ;		/*  then look in WISP$ROOT:[COM].	*/
				strcat ( file_path, progname_str ) ;
				strcat ( file_path, ".COM" ) ;
				spawn_action = 3 ;					/* to start a .COM file			*/
				break ;
			}
			case 3:
			{
				strcpy ( file_path, "WISP$ROOT:[EXE]" ) ;		/*  then look in WISP$ROOT:[EXE],	*/
				strcat ( file_path, progname_str ) ;
				strcat ( file_path, ".EXE" ) ;
				spawn_action = 1 ;					/* to start a .EXE file			*/
				break ;
			}
		}
		vmstry++;								/* Increment the number of tries.	*/
	}
	else if (*ltype == 'P')
	{
		switch ( vmstry )
		{
			case 0:								/* First look for a .EXE		*/
			{								/* Wasn't found, run the proc		*/
				ptr = wfname(&mode,l_vol,l_lib,progname,file_path);	/* mode = IS_SUBMIT; extension = .COM	*/
				*ptr = '\0' ;						/* null terminate file_path.		*/
				spawn_action = 3;
				break ;
			}
			case 1:
			{
				setwispfilext("EXE");					/* Use .EXE for just one wfname call	*/
				ptr = wfname(&mode,l_vol,l_lib,progname,file_path);	/* the proper file name.		*/
				*ptr = '\0' ;						/* null terminate file_path.		*/
				spawn_action = 1;
				break ;
			}
		}
		vmstry++;
	}
	else
	{
		switch ( vmstry )
		{
			case 0:							/* First look in current directory.	*/
			{
				strcpy ( file_path, progname_str ) ;
				strcat ( file_path, ".COM" ) ;
				spawn_action = 3;
				break ;
			}
			case 1:							/* else generate to execute a .COM	*/
			{
				strcpy ( file_path, progname_str ) ;
				strcat ( file_path, ".EXE" ) ;
				spawn_action = 1;
				break ;
			}
			case 2:							/* Else look in RUNVOL:[RUNLIB]		*/
			{
				ptr = wfname(&mode,l_vol,l_lib,progname,file_path);	/* mode = IS_SUBMIT; extension = .COM	*/
				*ptr = '\0' ;						/* null terminate file_path.		*/
				spawn_action = 3;
				break ;
			}
			case 3:
			{
				setwispfilext("EXE");					/* Use .EXE for just one wfname call	*/
				ptr = wfname(&mode,l_vol,l_lib,progname,file_path);	/* the proper file name.		*/
				*ptr = '\0' ;						/* null terminate file_path.		*/
				spawn_action = 1;
				break ;
			}
		}
		vmstry++;
	}
	status = access ( file_path, 0 ) ;						/* Check file; does it exist?		*/

	if (status != 0 )
	{
		if ((*ltype == 'S' || *ltype == ' ') && vmstry < 4) goto vmsagain;	/* Try with next specification.		*/
		if (*ltype == 'P' && vmstry < 2) goto vmsagain;				/* Try with next specification.		*/
		status = RMS$_FNF ;							/* set status to RMS file not found.	*/
	}
	else
	{
		if ( vms_descriptor && ( spawn_action == 1 ) )				/* If command is a .EXE file		*/
		{       /* ** ** **     */
			if ( parmcnt > 0 ) 						/* if there are any parameters;		*/
			{
				writevmslink( file_path, parmcnt, &parm_list, &len_list, vmskey);
			}
			else
			{
				strcpy ( vmskey, NULL_FILE_PATH ) ;			/* VMSLINKSUB won't try to read args.	*/
			}
			vonexit( NORMALIZE );						/* don't clear src			*/
			vexit();							/* Reset the terminal.			*/

							/* build spawn name starting with "@WISP$ROOT:[COM]VMS$START$LINK $"    */

			strcpy ( spawn_name, "@WISP$ROOT:[COM]VMS$START$LINK $" ) ;     /* Start with .COM file			*/
			strcat ( spawn_name, file_path ) ;				/* cat program name			*/
			strcat ( spawn_name, " " ) ;					/* cat space				*/
			strcat ( spawn_name, vmskey ) ;					/* cat parameter file name      	*/

						/* then call spawn with spawn_action == 3					*/
			status = spawn( 3, spawn_name, "" ) ;				/* Spawn the command; the 3 means .COM  */

					/* ** ** **     after spawn, figure out what to do about return codes, etc.		*/

			if (status == SS$_NORMAL || status == CLI$_NORMAL)		/* Spawn was successful.		*/
			{
				swap_put ( comcode, 0L ) ;				/* Successful link			*/
				swap_put ( retcod, 0L ) ;				/* Return code == 0 for success.	*/
			}
			else
			{								/* Set return code from spawned program:*/
				wang_retcod = vms_to_wang_codes ( status ) ;		/* convert VMS code to WANG code.	*/

				sprintf(temp_retval,"%03d",wang_retcod);		/* Put wang_retval into string format.  */
				setretcode(temp_retval);
				swap_put ( comcode, 8L ) ;				/* Unsuccessful link.			*/
				swap_put ( retcod, wang_retcod ) ;			/* Set up so is returned correctly.	*/
			}

			if ( parmcnt > 0 ) 						/* if there are any parameters;		*/
			{
				if ( access ( vmskey, 0 ) )				/* if parameter file does NOT exist;	*/
				{							/* subroutine terminated abnormally.	*/
					swap_put ( comcode, 16L ) ;			/* Subroutine exited abnormally		*/
					swap_put ( retcod, 0L ) ;			/* Reset return code to zero.		*/
				}
				else							/* ELSE, parameter file DOES exist	*/
				{							/* SO, read parameters into memory	*/
					readvmslink(file_path, parmcnt, &parm_list, &len_list, vmskey, &tcomcode, &retstat);
				}
			}

			if ( *can_exit == ' ' )						/* Cancel-Exit not at this level	*/
			{
				if ( LOGOFFFLAG )					/* A lower level called logoff		*/
				{
					wexit(32L);					/* Terminate this level			*/
				}
			}
			else
			{
				CANEXITFLAG--;						/* We've just come thru a Cancel-Exit   */
				LOGOFFFLAG = 0;						/* Cancel the logoff			*/
			}

			vsynch() ;
			wpl_usr ( &defaults ) ;
			rts_first = TRUE ;					/* Set so return from link re-inits screen	*/

			vonexit( NORMALIZE | CLEAR_SCREEN );				/* Reset onexit status			*/

			return ;

		}       /* ** ** **     */
		else									/* Else command is NOT a .EXE file      */
		{									/* OR is NOT a LINKDESC call		*/
			if ( spawn_action == 3 )					/* if file is a .COM file;		*/
			{
				strcpy ( spawn_name, "@" ) ;				/* @ means run .COM file for VMS	*/
				strcat ( spawn_name, file_path ) ;			/* Add .COM file to execute.		*/
			}
			else
			{
				strcpy ( spawn_name, file_path ) ;			/* .EXE file				*/
			}
			status = spawn ( spawn_action , spawn_name, "" ) ;		/* Spawn the command.			*/
		}
	}

	if (status == SS$_NORMAL || status == CLI$_NORMAL)				/* Spawn was successful.		*/
	{
		swap_put ( comcode, 0L ) ;						/* Successful link.			*/
		wang_retcod = 0;							/* Return code == 0 means successful.	*/
	}
	else
	{
		swap_put ( comcode, 8L ) ;						/* Unsuccessful link.			*/
											/* Set return code from spawned program:*/
		wang_retcod = vms_to_wang_codes ( status ) ;				/* convert VMS code to WANG code.	*/
	}
	sprintf(temp_retval,"%03d",wang_retcod);					/* Put wang_retval into string format.	*/

	setretcode(temp_retval);

	swap_put ( retcod, wang_retcod );						/* Set up so is returned correctly.	*/

	return;
/*************************************************  End of VMS section  *********************************************************/
#endif

#ifdef unix
/*************************************************  Start of UNIX section  ******************************************************/

	try_dot = 1;									/* Init flag to look in . curr dir	*/

	memset(link_filespec,(char)0,sizeof(link_filespec));				/* Clear filespec field			*/
	memset(testname,(char)0,sizeof(testname));					/* Clear testname field			*/

	if ( *ltype != 'S' )
	{
		mode = IS_SUBMIT;
		wfname(&mode,l_vol,l_lib,progname,testname);				/* expand the name 			*/
		p = strchr(testname,' ');
		*p = (char)0;
	}
	else
	{
		for( k=0; k<8 && progname[k] != ' ' && progname[k] != '\0'; k++)
		{
			lowname[k] = tolower(progname[k]);
		}
		lowname[k] = '\0';
	}

nextpath:

	if ( *ltype == 'S' )								/* Look in SYSTEM dir's			*/
	{
		pathcnt += 1;
		if ( 0 == osd_path(pathcnt) )						/* No more paths			*/
		{
			swap_put ( comcode, 8L ) ;					/* Unsuccessful link			*/
			swap_put ( retcod, 20L ) ;					/* File not found.			*/

			werrlog(ERRORCODE(5),lowname,0,0,0,0,0,0,0);
			return;
		}
		strcpy( testname, osd_path(pathcnt));
		if ( testname[0] ) strcat( testname, "/" );
		strcat( testname, lowname );
	}

	try_upper = 1;									/* If not found as lower then try upcase*/

notfoundtryupper:

	not_found = findexts(testname,link_filespec,&typeext);

	if ( not_found )
	{
		if ( try_upper )
		{
			int	ii;

			for( ii=strlen(testname)-1; ii>=0; ii-- )			/* Make file filename part uppercase.	*/
			{
				if (testname[ii] == '/') break;

				testname[ii] = toupper(testname[ii]);
			}

			try_upper = 0;							/* Don't try upper again		*/
			goto notfoundtryupper;
		}


		if ( *ltype == 'S' )							/* If 'look in system'			*/
		{
			goto nextpath;							/* Try next path.			*/
		}

		if ( *ltype == ' ' && try_dot )						/* Try the 'progvol & proglib'		*/
		{
			try_dot = 0;

			mode = IS_SUBMIT;
			memcpy(l_vol,".     ",6);
			memcpy(l_lib,".       ",8);
			wfname(&mode,l_vol,l_lib,progname,testname);			/* expand the name 			*/
			p = strchr(testname,' ');
			*p = (char)0;

			goto nextpath;
		}
		swap_put ( comcode, 8L ) ;						/* Unsuccessful link			*/
		swap_put ( retcod, 20L ) ;						/* File not found.			*/

		werrlog(ERRORCODE(7),testname,0,0,0,0,0,0,0);

		return;
	}

	ftyp = isexec(link_filespec);							/* decide if it's exec'able or 		*/
	if (ftyp==ACCERR)
	{
		swap_put ( comcode, 8L ) ;						/* Unsuccessful link			*/
		swap_put ( retcod, 28L ) ;						/* Access denied.			*/

		werrlog(ERRORCODE(9),link_filespec,0,0,0,0,0,0,0);
		return;
	}

	if (typeext == DOTGNT)								/* Micro Focus .gnt files have the	*/
	{										/* same magic number as ISEXEC.		*/
		ftyp = ISMFINT;								/* Change to behave like .int files	*/
	}

	/*
		Set up "link_callspec" this is the value that the link frontend (ACULINK or MFLINK) is to 
		use in the CALL stmt.
	*/

	switch(ftyp)
	{
	case ISMFINT:
		strcpy(link_callspec,testname);						/* Full path without extension		*/
		break;
	case ISEXEC:									/* Routine name only, uppercase		*/
		unloadpad(link_callspec,progname,8);
		upper_string(link_callspec);
		break;
	default:									/* Full path				*/
		strcpy(link_callspec,link_filespec);
		break;
	}

	if ( arg_length_known || 0 == parmcnt )						/* For ACU & AIX write out params	*/
	{
		writeunixlink(link_callspec, parmcnt, &parm_list, &len_list, linkkey);
		parm_file_written = 1;
	}
	else
	{
		parm_file_written = 0;
	}

	if ( *can_exit != ' ' ) 							/* If cancel exit flag is set then	*/
	{										/* increment the counter.		*/
		CANEXITFLAG++;								/* (There can be multiple canexits	*/
	}										/*  in effect; one per link-level.)	*/

	if ( ftyp == ISACU || ftyp == ISMFINT )						/* If going to COBOL don't clear sreen.	*/
	{
		vonexit( NORMALIZE );
	} 

	vexit();									/* Reset the terminal.			*/

	old_SIGINT  = signal(SIGINT,  SIG_IGN);						/* Ignore signals			*/
	old_SIGQUIT = signal(SIGQUIT, SIG_IGN);

	signal(SIGCLD,  SIG_DFL);							/* Use Default DEATH-OF-CHILD signal	*/


	retstat = 0;
	tcomcode = 0;

	switch (pid = fork())								/* Fork into two processes		*/
	{
	case 0: /* CHILD PROCESS */

		if (parm_file_written)							/* If we wrote a parm file then...	*/
		{
			char	buff[128];
			sprintf(buff,"%s=%s",WISP_LINK_ENV,linkkey);			/* Store the linkkey in env		*/
			setenvstr(buff);
		}

		if (*can_exit != ' ')						/* Mark the cancel-exit in a shell var so it is	*/
		{								/* even if we go across a script etc.		*/
			char	buff[128];
			sprintf(buff,"%s=%d",WISP_CANCELEXIT_ENV,getpid());
			setenvstr(buff);
		}

		setprogdefs(l_vol,l_lib);						/* Set up PROGLIB and PROGVOL		*/
		clearprogsymb();							/* Clear PROGLIB/VOL from symbol	*/

		switch (ftyp)
		{
		case ISEXEC: 
			sh_parm[0] = link_filespec;					/* argv[0] is the filespec		*/

			if (parm_file_written)						/* If wrote parm file then...		*/
			{
				sh_parm[1] = '\0';					/* null terminate it.			*/
			}
			else								/* If no parm file then we don't know	*/
			{								/* the parm lengths so assume C style	*/
				for (i=0; i < parmcnt; ++i)				/* null terminated strings (for LPI)	*/
					sh_parm[i+1]=parm_list.the_parm[i];		/* and load them onto argv list.	*/
				sh_parm[i+1] = '\0';					/* Null terminate it			*/
			}

			execvp(sh_parm[0],sh_parm);					/* Do the exec.				*/

			werrlog(ERRORCODE(4),link_filespec,errno,0,0,0,0,0,0);
			*retcod=fixerr(errno);						/* xlat unix errno to wang error# 	*/
			break;

		case NOTEXEC:								/* Assume a shell script		*/
			memset(sh_parm,(char)0,sizeof(sh_parm));

			sh_parm[0]=shell_var();						/* argv[0] is the shell (i.e /bin/sh)	*/
			sh_parm[1]=link_filespec;					/* argv[1] in the filespec	 	*/

			if (parm_file_written)
			{
				/*
				**	Load the argument list so it can be used by SHELL SCRIPTS.  This is the same
				**	way as LINKPROC; we can do this because we know the lengths.  Assume that there
				**	are no embedded NULLs in the parameters.
				*/
				for (i=0; i < parmcnt; ++i)
				{
					sh_parm[i+2]=(char *)malloc(len_list.the_len[i] + 1);		/* Malloc space		*/
					memcpy(sh_parm[i+2],parm_list.the_parm[i],len_list.the_len[i]);	/* Load parm		*/
					sh_parm[i+2][len_list.the_len[i]] = '\0';			/* Null terminate parm	*/
				}
				sh_parm[i+2] = '\0';					/* null terminate it			*/
			}
			else
			{
				/*
				**	We don't know the lengths so we assume the parameters are NULL terminated.
				**	This is only used from LPI COBOL (I think).
				*/
				for (i=0; i < parmcnt; ++i)				/* load args				*/
				{
					sh_parm[i+2]=parm_list.the_parm[i];
				}
				sh_parm[i+2] = '\0';					/* null terminate it			*/
			}

			execvp(sh_parm[0],sh_parm);

			werrlog(ERRORCODE(6),link_filespec,errno,0,0,0,0,0,0);
			*retcod=fixerr(errno);
			break;

		case ISACU: 								/* An acu_cobol object			*/
		case ISMFINT:								/* Micro focus .int or .gnt code	*/
			{
				struct wruncfg cfg;
				char	options[80];
				char	*eptr, *optr;
				int	arg;

				wrunconfig(&cfg);					/* Load wrunconfig options file		*/
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
				sh_parm[arg++] = cfg.wrun_runcbl;			/* argv[0] is the cobol RTS		*/
					
				for( optr=options; *optr; optr++ )			/* add the options 			*/
				{
					for(;*optr==' ';optr++);			/* Scan til first non-space 		*/
					if (! *optr ) break;
					
					sh_parm[arg++] = optr;				/* Point to option			*/
					
					for(;*optr && *optr != ' ';optr++);		/* Scan til space.			*/
					if (! *optr ) break;
					
					*optr = '\0';					/* Null terminate the option		*/
				}				

				if (parm_file_written)					/* If the parm file was written then we	*/
				{							/* are using the WISP frontend routines */

					if ( ftyp == ISACU )				/* ACUCOBOL object file			*/
					{
						sh_parm[arg++] = "ACULINK";		/* ACU Link interface program		*/
						sh_parm[arg++] = linkkey;		/* Key to parm area			*/
					}

					if (ftyp == ISMFINT )				/* Micro Focus GNT or INT file		*/
					{
						sh_parm[arg++] = "MFLINK";		/* MF Link interface program		*/
					}
				}

				sh_parm[arg++] = link_callspec;				/* Name of program to start		*/
				/* NOTE: This will normally be a comment except if LINK is called from a non-COBOL program.	*/

				sh_parm[arg++] = '\0';					/* null terminate it			*/

				execvp(sh_parm[0],sh_parm);				/* Start the new program.		*/

				werrlog(ERRORCODE(14),sh_parm[0],link_filespec,errno,0,0,0,0,0);
				*retcod=fixerr(errno);
				break;
			}
		}
		exit(*retcod);
		break;

	case -1:									/* The link failed			*/
		werrlog(ERRORCODE(11),errno,0,0,0,0,0,0,0);
		tcomcode = 8;
		retstat = 99;
		break;

	default: /* PARENT PROCESS */

		wwaitpid(pid,&retstat);							/* Wait for linked process to complete	*/
		retstat = 0;
		vsynch();								/* Resynch video			*/
		wpl_usr(&defaults);							/* Reload defaults: may have changed	*/
		rts_first = TRUE ;							/* Return from link re-inits screen	*/
		break;
	}

	signal(SIGINT,  old_SIGINT);							/* Reset the signals after the link	*/
	signal(SIGQUIT, old_SIGQUIT);
	signal(SIGCLD,  SIG_IGN);							/* Ignore DEATH-OF-CHILD signal		*/

	vonexit( NORMALIZE | CLEAR_SCREEN );						/* Reset onexit status to clear screen	*/

	if ( parm_file_written )							/* If parm file written then clean up	*/
	{
		readunixlink(link_filespec, parmcnt, &parm_list, &len_list, linkkey, &tcomcode, &retstat);
	} 

	if ( pid == -1 )								/* link failed				*/
	{
		tcomcode = 8;
		retstat = 99;
	}

	if ( *can_exit == ' ' )								/* Cancel-Exit not at this level	*/
	{
		char	*gptr;
		int	kpid;

		if ( LOGOFFFLAG )							/* A lower level called logoff		*/
		{
			wexit(32L);							/* Terminate this level			*/
		}

		gptr = (char *)getenv(WISP_CANCELEXIT_ENV);			/* If the cancel-exit var is set and the process*/
		if ( gptr )							/* it points to is NOT running then we are 	*/
										/* midway thru a LOGOFF with cancel-exit.	*/
										/* (See LOGOFF) so just exit.			*/
		{
			if ( 1 == sscanf(gptr,"%d",&kpid) && kpid != 0 )	/* A kpid==0 means cancelexit was set and has	*/
			{							/* been turned off because we crossed a SUBMIT.	*/
				if ( kill(kpid,0) != 0 )			/* Check if cancelexit process is alive.	*/
				{
					LOGOFFFLAG = 1;
					wexit(32L);
				}
			}
		}
	}
	else
	{
		CANEXITFLAG--;								/* We've just come thru a Cancel-Exit	*/
		LOGOFFFLAG = 0;								/* Cancel the logoff			*/
		sleep(1);								/* Allow the lower process to exit	*/
	}

	swap_put ( comcode, tcomcode ) ;						/* swap and set the completion code.	*/
	swap_put ( retcod, retstat ) ;							/* swap and set the return code.	*/

	return;

/*************************************************  End of UNIX section  ********************************************************/
#endif	/* #ifdef unix */							

#ifdef MSDOS
/*************************************************  Start of MSDOS section  *****************************************************/

								/*								*/
								/* When LINK is called, a disk search is performed.  As soon as	*/
								/* a file is found, the search stops and the file is executed.	*/
								/*								*/
								/* The directories to search are determined by the ltype value:	*/
								/*								*/
								/*  'S' - Look in the "path" environment variable directories.	*/
								/*  'P' - Use arguments volname and libname for a directory.	*/
								/*  ' ' - Use runvol/runlib first, then the current directory.	*/
								/*								*/
								/* Every DOS file name has an extension for its file type.	*/
								/*								*/
								/* Each directory is searched for progname plus an extension:	*/
								/*								*/
								/*	.EXE	A large binary executable file.			*/
								/*	.COM	A small binary executable file.			*/
								/*	.BAT	A text file of DOS commands (A batch file).	*/
								/*	.GNT	A Micro Focus COBOL/2 native object file.	*/
								/*	.INT	A Micro Focus COBOL/2 intermediate object file.	*/
								/*								*/
								/* If .EXE .COM or .BAT file is found, system() is called.	*/
								/* If .GNT or .INT is found, wclink() is called.		*/
								/*								*/
								/* system() performs a system call to start another program.	*/
								/* wclink() performs a COBOL call based on the parameter count.	*/

	not_found = 1;								/* Flag gets set to 0 when file is found.	*/

	if( *ltype == 'S' )							/* Look for progname in "path" directories.	*/
	{
		char *path_ptr;							/* Pointer to each "path" directory.		*/

		pathcnt = 1;							/* Get ready for first "path" directory.	*/
		path_ptr = osd_path ( pathcnt );				/* Get first directory from "path" env var.	*/

		while( not_found && ( NULL != path_ptr ) )			/* Continue until found or no more "path" dirs.	*/
		{
			strcpy ( testname, path_ptr );				/* Save path value in testname.			*/

			not_found = findexts ( testname, file_path, &typeext );	/* Check for name with each file extension.	*/

			if( not_found )						/* If none of the extensions matched a file.	*/
			{
				pathcnt++;					/* Get ready for next "path" directory.		*/
				path_ptr = osd_path ( pathcnt );		/* Get next directory from "path" env var.	*/
			}
		}
	}
	else									/* ltype == 'P' (l_vol/l_lib) or ' ' + current.	*/
	{
		int dirs_to_try;						/* Directories to try flag for ltype ' ' & 'P'.	*/

		if ( *ltype == ' ' )	dirs_to_try = 2;			/* For ' ', try RUN(vol\lib) and current dir.	*/
		else			dirs_to_try = 1;			/* For 'P', try the parameters vol\lib only.	*/

		mode = IS_NOEXTENSION ;						/* wfname should not add an extension to path.	*/

		while ( dirs_to_try && not_found )				/* Only repeat if ltype == ' ' and not_found.	*/
		{
			dirs_to_try--;						/* Flag for ltype == ' ' repeat only.		*/

			wfname ( &mode, l_vol, l_lib, progname, testname );	/* Create a testname from l_vol\l_lib\progname.	*/

			not_found = findexts ( testname, file_path, &typeext );	/* Check for name with each file extension.	*/

			if( not_found && dirs_to_try )				/* If none of the extensions matched a file.	*/
			{							/* And first time through with ltype == ' '.	*/
				memcpy ( l_vol, ".     ",   6 );		/* Set l_vol to current directory.		*/
				memcpy ( l_lib, ".       ", 8 );		/* Set l_lib to current directory.		*/
			}
		}
	}

	if( not_found )								/* progname not found in any directory.		*/
	{
		swap_put( comcode, 8L );					/* Unsuccessful link.				*/
		swap_put( retcod, 20L );					/* File not found.				*/

		werrlog( ERRORCODE(7), testname,0,0,0,0,0,0,0);

		return( 0 );							/* Exit LINK now.				*/
	}

	if( ( typeext == DOTGNT ) || ( typeext == DOTINT ) )			/* .GNT or .INT Micro Focus COBOL/2 file.	*/
	{
		wclink( file_path, &parm_list, &retval );			/* Call subroutine through wclink module.	*/

		if( retval == 1 )						/* Successful call.				*/
		{
			retstat = atol ( WISPRETURNCODE );

			swap_put ( comcode, 0L ) ;
			swap_put ( retcod, retstat );
		}
		else
		{
			swap_put( comcode, 8L );				/* Unsuccessful link.				*/
			swap_put( retcod, 20L );				/* File not found.				*/

			werrlog( ERRORCODE(7), testname,0,0,0,0,0,0,0);
		}
	}
	else									/* .BAT .COM or .EXE DOS executabe file found.	*/
	{
		vexit();							/* Clear video for a system call.		*/

		system( file_path );						/* Execute file_path with a command processor.	*/

		vsynch();							/* Synchronize video after a system call.	*/
		wpl_usr( &defaults );
		rts_first = TRUE;						/* Tell video to redraw the screen.		*/

		swap_put ( comcode, 0L ) ;
		swap_put ( retcod,  0L );
	}

	return(0);								/* Exit LINK now.				*/

/*************************************************  End of MSDOS section  *******************************************************/
#endif	/* #ifdef MSDOS */

}

/*	End of	link.c	*/

