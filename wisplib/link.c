			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		link.c
**
**	Purpose:	To emulate the VSSUB routine LINK.
**
**	Routines:	
**	LINK()		Generic LINK entry point
**	LINKDESC()	VMS for args passed by descriptor (/DLINK)
**	LINKAIX()	AIX arg/length paired arguments
**	LINKMF()	Micro Focus (unix) arg/length paired arguments
**	LINK2()		Generic arg/length paired arguments
**	do_link()	Actual LINK routine
**	searchpath()	To search the path for a file to run/exec.
**	findrun()	To find the file to run.
**	firstproc()	To record the filepath of the first proc run.
**
**	Warnings:	*** LINK must be reenterent for VMS and MSDOS ***
**			It will be called recursively so don't use any static variables.
**
**	History:
**	12/22/92	Added the MSDOS/ACUCOBOL logic. GSL
**
*/


#include <stdio.h>
#include <varargs.h>									/* Function uses variable params.	*/

#ifdef unix
#include <errno.h>
#include <ctype.h>
#include <signal.h>
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

#include "idsistd.h"
#include "link.h"
#include "wglobals.h"
#include "wdefines.h"
#include "movebin.h"
#include "wperson.h"
#include "wfiles.h"
#include "wcommon.h"
#include "cobrun.h"
#include <v/video.h>
#include "runtype.h"
#include "wrunconf.h"
#include "idsisubs.h"

#include "werrlog.h"
#define	ROUTINE		28000
/*
28001	%%LINK-I-ENTRY Entry into LINK
28002	%%LINK-E-MAXDEPTH Maximum link depth reached %d
28003	%%LINK-I-BADPARAM Progname %8.8s Invalid parameter TYPE %1.1s
28004	%%LINK-E-EXECVE Exec to %.60s failed [errno=%d]
28005	%%LINK-I-NOTFOUND System routine %8.8s Not Found
28006	%%LINK-E-EXECVE Exec /bin/sh %.60s failed [errno=%d]
28007	%%LINK-I-NOTFOUND File %.60s Not Found
28008	%%LINK-F-MAXPARMS Maximum %d parms. Called with %d parms.
28009	%%LINK-I-NOACCESS Access denied to %.60s
28010	%%LINK-F-MKDIR Unable to mkdir %s [errno=%d]
28011	%%LINK-I-NOTRUN Not a runnable file %.60s
28012	%%LINK-F-FOPEN Unable to fopen %s [errno=%d] in %s
28013	%%LINK-I-NOMEMORY Not enough memory to load %.60s
28014   %%LINK-F-EXECVP Exec of %.60s failed while linking to %8.8s [errno=%d]
28016	%%LINK-E-RECURSIVE Recursive LINK to file %.60s
28018	%%LINK-E-FORK Fork failed [errno=%d]
28020	%%LINK-E-ERROR %s
*/

#define MAX_CALLS	16								/* Maximum call depth			*/
#define	MAX_VARGS	128								/* MAX_LINK_PARMS + control params too	*/

#define LENGTH_KNOWN		1
#define LENGTH_NOT_KNOWN	0

#ifdef unix
char *shell_var();
#endif /* unix */

#ifndef VMS
char	*osd_path();
#endif /* !VMS */

char *splitext();
extern char *wfname();
static int do_link();
static int searchpath();

extern char WISPRETURNCODE[3];
extern char WISPFILEXT[39];
extern	int	rts_first ;								/* First time flag for screen init.	*/

LINK ( va_alist )									/* LINK: Load arg into table		*/
va_dcl											/* variable argument list		*/
{
	va_list	link_args ;								/* descriptor arguments			*/
	int	i ;
	int	length_known;
	int	varg_count;								/* Total arg count			*/
	char	*varg_pointer[MAX_VARGS] ;						/* pointers to variable param list	*/
	int4	varg_length[MAX_VARGS] ;						/* length of each parameter in list	*/

	length_known = (acu_cobol) ? LENGTH_KNOWN : LENGTH_NOT_KNOWN;			/* If ACU the lengths are known		*/

	va_start ( link_args ) ;							/* start the walk down the parameters	*/
	varg_count = va_count ( link_args ) ;						/* Find out how many args in the stack.	*/
	va_start ( link_args ) ;							/* restart the walk down the parameters	*/

	for ( i = 0 ; i < varg_count ; ++i )
	{
		varg_pointer[i] = va_arg( link_args, char * );				/* Get the next argument pointer	*/

		if ( length_known )							/* If lengths known			*/
		{
			varg_length[i] = va_arg( link_args, int4);			/* Get the argument length		*/
		}
		else									/* Else, this is NOT acu cobol;		*/
		{
			varg_length[i] = 0 ;						/* Set the argument length to zero	*/
		}
	}

	va_end ( link_args ) ;								/* end the parameter walk		*/

	do_link(varg_count, varg_pointer, varg_length, length_known);			/* Do the LINK				*/
}


#ifdef	VMS		/* Start VMS only block of code	*/				/* only if in vms			*/
LINKDESC ( va_alist )									/* LINK with vms descriptors		*/
va_dcl											/* variable argument list		*/
{
	int	i ;
	va_list	desc_args ;								/* descriptor arguments			*/
	int	varg_count;								/* Total arg count			*/
	char	*varg_pointer[MAX_VARGS] ;						/* pointers to variable param list	*/
	int4	varg_length[MAX_VARGS] ;						/* length of each parameter in list	*/
	struct  dsc$descriptor  *va_dsc ;						/* Descriptor pointer from va list      */


	va_start ( desc_args ) ;							/* start the walk down the parameters	*/
	varg_count = va_count ( desc_args ) ;						/* Find out how many args in the stack.	*/
	va_start( desc_args ) ;								/* restart the argument list		*/

	for ( i = 0 ; i < varg_count ; ++i )						/* for each argument in va_alist	*/
	{
		va_dsc = va_arg( desc_args, struct dsc$descriptor*);			/* Get the next descriptor		*/
		varg_pointer[i] = va_dsc->dsc$a_pointer ;				/* Get the argument pointer		*/
		varg_length[i] = va_dsc->dsc$w_length ;					/* Get the argument length		*/
	}

	va_end ( desc_args ) ;								/* end the parameter walk		*/

	do_link(varg_count, varg_pointer, varg_length, LENGTH_KNOWN);			/* Do the link				*/
}
#endif /* VMS */

#ifdef unix
LINKAIX ( va_alist )									/* LINK for AIX VS COBOL		*/
va_dcl											/* variable argument list		*/
{
	va_list	link_args ;								/* descriptor arguments			*/
	int	i;
	int	varg_count;								/* Total arg count			*/
	char	*varg_pointer[MAX_VARGS] ;						/* pointers to variable param list	*/
	int4	varg_length[MAX_VARGS] ;						/* length of each parameter in list	*/

	va_start ( link_args ) ;							/* start the walk down the parameters	*/
	varg_count = va_count ( link_args ) ;						/* Find out how many args in the stack.	*/
	va_start ( link_args ) ;							/* restart the walk down the parameters	*/

	for ( i = 0 ; i < varg_count ; ++i )
	{
		varg_pointer[i] = va_arg( link_args, char * );				/* Get the next argument pointer	*/
		varg_length[i] = va_arg( link_args, int4);				/* Get the argument length		*/
	}

	va_end ( link_args ) ;								/* end the parameter walk		*/

	do_link(varg_count, varg_pointer, varg_length, LENGTH_KNOWN);			/* Do the link				*/
}
#endif	/* unix */

#ifndef VMS
LINKMF ( va_alist )									/* LINK for Micro Focus COBOL/2		*/
va_dcl											/* variable argument list		*/
{
	va_list	link_args ;								/* descriptor arguments			*/
	int	i;
	int	varg_count;								/* Total arg count			*/
	char	*varg_pointer[MAX_VARGS] ;						/* pointers to variable param list	*/
	int4	varg_length[MAX_VARGS] ;						/* length of each parameter in list	*/

	va_start ( link_args ) ;							/* start the walk down the parameters	*/
	varg_count = va_count ( link_args ) ;						/* Find out how many args in the stack.	*/
	va_start ( link_args ) ;							/* restart the walk down the parameters	*/

	for ( i = 0 ; i < varg_count ; ++i )
	{
		varg_pointer[i] = va_arg( link_args, char * );				/* Get the next argument pointer	*/
		varg_length[i] = va_arg( link_args, int4);				/* Get the argument length		*/
	}

	va_end ( link_args ) ;								/* end the parameter walk		*/

	do_link(varg_count, varg_pointer, varg_length, LENGTH_KNOWN);			/* Do the link				*/
}
#endif	/* !VMS */

/*
**	Routine:	LINK2()
**
**	Function:	To provide an entry point to LINK which expects argument/length pairs.
**
**	Description:	This is a frontend to LINK which expects all args to be paired with
**			there length.
**			This routine should be the standard entry point into LINK and should
**			eventually replace LINKMF, LINKAIX, and LINK (for acucobol).
**
**	Arguments:	Argument/length pairs:
**			The arguments are all pointers.
**			The lengths are all longs.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	The va_count() is fudged and is expecting 1 for each pair of args.
**			On UNIX this is handled by sub85.c and wvaset(), on VMS need to half the value.
**
*/
LINK2 ( va_alist )									/* Generic paired arg LINK		*/
va_dcl											/* variable argument list		*/
{
	va_list	link_args ;								/* descriptor arguments			*/
	int	i;
	int	varg_count;								/* Total arg count			*/
	char	*varg_pointer[MAX_VARGS] ;						/* pointers to variable param list	*/
	int4	varg_length[MAX_VARGS] ;						/* length of each parameter in list	*/

	va_start ( link_args ) ;							/* start the walk down the parameters	*/
	varg_count = va_count ( link_args ) ;						/* Find out how many args in the stack.	*/
#ifdef VMS
	varg_count = (varg_count) ? varg_count / 2: 0;					/* For VMS half the arg count		*/
#endif
	va_start ( link_args ) ;							/* restart the walk down the parameters	*/

	for ( i = 0 ; i < varg_count ; ++i )
	{
		varg_pointer[i] = va_arg( link_args, char * );				/* Get the next argument pointer	*/
		varg_length[i]  = va_arg( link_args, int4);				/* Get the argument length		*/
	}

	va_end ( link_args ) ;								/* end the parameter walk		*/

	do_link(varg_count, varg_pointer, varg_length, LENGTH_KNOWN);			/* Do the link				*/
}

/*
**	Routine:	do_link()
**
**	Function:	To emulate the Wang VSSUB routine LINK.
**
**	Description:	This is the routine that does the actual LINK.
**			It is only accessable from one of the documented frontend 
**			routines such as LINK, LINK2, LINKMF etc.  These frontends
**			setup the argument/length parameter pairs into a standard
**			format that do_link() can use.
**
**	Arguments:
**	var_count	The number of arguments passed to the frontend routine.
**			Note, an argument/length pair counts as one argument.
**	varg_pointer	Array of argument pointers.
**	varg_length	Array of argument lengths. (May not be available)
**	arg_length_known 
**			Flag that indicates if var_length array has been set up.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	12/23/92	Modified for MSDOS/ACUCOBOL. GSL
**	10/20/93	Modified for VMS style return status and completion status on spawn.  SMC
**
*/
static	do_link(varg_count, varg_pointer, varg_length, arg_length_known)
int4	varg_count;									/* Total arg count			*/
char	*varg_pointer[];								/* pointers to variable param list	*/
int4	varg_length[];									/* length of each parameter in list	*/
int4	arg_length_known;								/* Arg lengths are known		*/
{
	int4	va_i ;									/* index for varg_pointer & length.	*/

											/* These are the arguments to LINK.	*/
	char 	*progname;								/* (1) Program Name			*/
	char	*ltype;									/* (2) Link Type			*/
	char 	*libname;								/* (3) Library				*/
	char	*volname;								/* (4) Volume				*/
	char	*arg_special;								/* (5) Special				*/
	char	*arg_ext_opt;								/* (6) Extended Options			*/
	int4	parmcnt;								/* (7) Parameter Count			*/
	struct 	str_parm parm_list;							/* (8) Parm(s)				*/
	struct 	str_len  len_list;							/*     Parm lengths			*/
	char	*can_exit;								/* (9) Cancel Exit Flag			*/
	char	*mess;									/* (10) Message				*/
	int4	messlen;								/* (11) Message Length			*/
	char	*helpflg;								/* (12) HELP disable			*/
	char	*pfkey;									/* (13) (reserved)			*/
	char	*cantext;								/* (14) Cancel Receiver			*/
	int4	canlen;									/* (15) Cancel Receiver Length		*/
	int4	*comcode;								/* (16) Completion Code			*/
	int4 	*retcod;								/* (17) Return Code			*/

											/* These are used for all platforms.	*/
	int4	to_do;
	char	*dummy_char;
	int4	arg_used;
	char	*this_parm;
	int4	arg_len;								/* lenght of the previous arg		*/
	int4	i;
	char 	l_vol[7], l_lib[9], l_file[9];
	int4	mode;
	int4	not_found;
	char 	testname[NAME_LENGTH+1];
	int4	retval;
	int4	dummy_long;
	int4	parm_file_written;
	int4	savelevel;
	char	*p, *wfname();
	char	buff[256];
	int4	ftyp;
	int4	wang_compcode;								/* Temp compcode			*/
	int4	wang_retcode;								/* Value expected from Wang COBOL.	*/

#ifdef VMS
	uint4	vms_return;								/* VMS return status.			*/
	uint4	vms_status;								/* VMS completion status.		*/
	int	spawn_action;								/* Action to call spawn flag.		*/
	int	vmstry;									/* Flag to indicate search tries.	*/
	char	vmskey[80];								/* Key to link parm area		*/
#endif
#ifdef unix
	int4	exit_code;
	int	pid;
#endif

#ifndef MSDOS
	char	**tmp, *strchr();
	char	*sh_parm[64];
	char	linkkey[80];								/* Key to link parm area		*/

	int	num;
	char	temp_retval[4];								/* Temp string to hold return value.	*/
/*	int	cpos; */
/*	char	*cp;  */
#endif /* !MSDOS */

	char 	link_filespec[256];							/* Native program path			*/
	char 	link_callspec[256];							/* the "CALL" spec			*/
	char	*ptr ;									/* pointer to a char in link_filespec	*/
	char	progname_str[16] ;							/* string version of progname		*/


	void (*old_SIGINT)();								/* Save old signals			*/
	void (*old_SIGQUIT)();
	void (*old_SIGCLD)();

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	if (lnk_depth == MAX_CALLS)
	{
		werrlog(ERRORCODE(2),lnk_depth,0,0,0,0,0,0,0);
		return(0);
	}

	memset(&parm_list,0,sizeof(parm_list));						/* set parm_list to zeroes		*/

	to_do =	varg_count ; 								/* Get the arg count			*/
	va_i = 0 ;									/* Start looking at the arg table	*/

	/*
	**	DECODE THE ARGUMENTS:
	**	=====================
	**
	**	There are 17 arguments to include later arguments you must include all earlier args except ARG5 and ARG6
	**	which can be omitted. Some occur only in pairs 3+4, 7+8, 10+11, 14+15.  Arguments 16 and 17 are documented
	**	as being required BUT customers report they are not required.  If the argument lengths are known we use
	**	it plus the number of remaining args to help decode which args are which.  
	**
	**	ASSUMPTIONS:
	**	- Only take the first of a pair if have enough args to complete the pair.
	**	- If 2 args remain and first is 4 bytes int4 then these are args 16 and 17.
	**	- Args 16 and 17 will normally be a pair but 17 could be missing.
	**
	**	STATE TABLE:
	**	      CURRENT	NEXT
	**		ARG1	ARG2  || ARG16 || END
	**		ARG2	ARG3  || ARG16 || END
	**		ARG3	ARG4
	**		ARG4	ARG5  || ARG6  || ARG7  || ARG16 || END
	**		ARG5	ARG6  || ARG7  || ARG16 || END
	**		ARG6	ARG7  || ARG16 || END
	**		ARG7	ARG8
	**		ARG8	ARG9  || ARG16 || END
	**		ARG9	ARG10 || ARG16 || END
	**		ARG10	ARG11
	**		ARG11	ARG12 || ARG16 || END
	**		ARG12	ARG13 || ARG16 || END
	**		ARG13	ARG14 || ARG16 || END
	**		ARG14	ARG15
	**		ARG15	ARG16 || END
	**		ARG16	ARG17 || END
	**		ARG17	END
	*/

ARG1:	/* PROGRAM	Alpha(8) */
	progname = varg_pointer[va_i++] ;						/* Get the program name.		*/
	to_do--;

	memcpy(l_file,progname,8);
	leftjust(l_file,8);
	upper_mem(l_file,8);
	l_file[8] = (char)0;

	libname = "	";								/* Give initial values.			*/
	volname = "      ";
	ltype = " ";
	parmcnt = 0L;
	can_exit = " ";
	helpflg = " ";
	messlen = 0;
	canlen = 0;
	retcod  = &dummy_long;
	comcode = &dummy_long;

	if (to_do == 0) goto ARGEND;
	if (to_do > 2) goto ARG2;
	if (arg_length_known)
	{
		if (varg_length[va_i] == 4) goto ARG16;
	}
	else if (to_do == 2 ) goto ARG16;

ARG2:	/* LINK TYPE 	Alpha(1) */
	ltype = varg_pointer[va_i++] ;							/* Get the link type info.		*/
	to_do--;

	if (to_do == 0) goto ARGEND;
	if (to_do == 1) goto ARG16;
	if (to_do >  2) goto ARG3;
	if (arg_length_known)
	{
		if (varg_length[va_i] == 4) goto ARG16;
	}
	else goto ARG16;

ARG3:	/* LIBRARY	Alpha(8) */
	libname = varg_pointer[va_i++] ;						/* Get the library name.		*/
	to_do--;

ARG4:	/* VOLUME	Alpha(6) */
	volname = varg_pointer[va_i++] ;						/* Get the volume name.			*/
	to_do--;

	if (to_do == 0) goto ARGEND;
	if (arg_length_known)
	{
		if (varg_length[va_i] == 1) goto ARG5;
		if (varg_length[va_i] == 2) goto ARG6;
		if (varg_length[va_i] == 4)						/* goto ARG7 or ARG16			*/
		{
			/*
			**	The next arg is either 7 or 16 
			**		to_do = 1	must be 7
			**		to_do > 2	must be 7
			**		to_do = 2	ASSUME 16
			*/
			if (to_do == 2) goto ARG16;
			goto ARG7;
		}
	}
	else if (to_do == 2 ) goto ARG16;

ARG5:	/* SPECIAL	Alpha(1) */
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
		if (to_do == 0) goto ARGEND;
		if (arg_length_known)
		{
			if (varg_length[va_i] == 4)					/* goto ARG7 or ARG16			*/
			{
				/*
				**	The next arg is either 7 or 16 
				**		to_do = 1	Probably 7
				**		to_do > 2	must be 7
				**		to_do = 2	ASSUME 16
				*/
				if (to_do == 2) goto ARG16;
				goto ARG7;
			}
		}
		else if (to_do == 2 ) goto ARG16;

ARG6:	/* EXT OPTS	Alpha(2) */
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
		if (to_do == 0) goto ARGEND;
		/* 
		**	If to_do equals 2 then the next arg is either 7 or 16
		**	assume 7 and check after to see if wrong then backup and goto 16.
		*/

ARG7:	/* PARM COUNT	Int(4) */
		dummy_char = varg_pointer[va_i++] ;					/* Get parmcnt.				*/
		to_do--;
		arg_used = 0;
	}
	GETBIN(&parmcnt,dummy_char,4);							/* The argument count.			*/
	wswap(&parmcnt);

	if (to_do == 1)									/* Check if we made a wrong turn	*/
	{
		int	wrong_turn;
		wrong_turn = 0;

		if (parmcnt > to_do)
		{
			wrong_turn = 1;
		}
		else if (parmcnt == 0)
		{
			if (arg_length_known)
			{
				if (varg_length[va_i] == 4) wrong_turn = 1;
			}
			else wrong_turn = 1;
		}

		if (wrong_turn)
		{
			/*
			**	Ooops we took a wrong turn, that last arg was really arg16. Backup one arg and go to ARG16.
			*/
			parmcnt = 0;
			to_do = 2;
			va_i--;
			goto ARG16;
		}
	}

	if ( parmcnt > MAX_LINK_PARMS )
	{
		werrlog(ERRORCODE(8),MAX_LINK_PARMS,parmcnt,0,0,0,0,0,0);
		wexit(ERRORCODE(8));
	}
	for (i=0; i<MAX_LINK_PARMS; i++)
	{
		parm_list.parm[i] = 0;							/* Clear out the pointers.		*/
	}
	if (parmcnt)									/* If there are arguments...		*/
	{										/* Then get their addresses.		*/
		for (i=0; i<parmcnt; i++)
		{
ARG8:	/* PARMS Var(var) */
			this_parm = varg_pointer[va_i] ;				/* Get the argument pointer.		*/
			parm_list.parm[i] = this_parm;

			if ( arg_length_known )
			{
				arg_len = varg_length[va_i] ;				/* Get the argument length.		*/
				len_list.len[i] = arg_len;				/* Save length in a table.		*/
			}
			va_i++;								/* this time increment after assignment	*/
			to_do--;
		}

		for (i=parmcnt; i<MAX_LINK_PARMS; i++) 
		{
			parm_list.parm[i] = this_parm;					/* Fill rest with last parm.		*/
		}
	}

	if (to_do == 0) goto ARGEND;
	if (to_do == 2) goto ARG16;
	
ARG9:	/* CANCELEXIT	Alpha(1) */
	can_exit = varg_pointer[va_i++] ;						/* Get the cancel exit flag.		*/
	to_do--;
	if (	*can_exit != 'C' && 
	    	*can_exit != 'N' && 
		*can_exit != 'D' && 
		*can_exit != 'P'   ) 
	{
		*can_exit = ' ';
	}

	if (to_do == 0) goto ARGEND;
	if (to_do == 1) goto ARG16;
	if (to_do >  2) goto ARG10;
	if (arg_length_known)
	{
		if (varg_length[va_i] == 4) goto ARG16;
	}
	else goto ARG16;

ARG10:	/* MESSAGE	Alpha(var) */
	mess = varg_pointer[va_i++] ;							/* Get the pf16 message string		*/
	to_do--;

ARG11:	/* MESS LENGTH	Int(4) */
	messlen = *(varg_pointer[va_i++]) ;						/* Get the pf16 message length.		*/
	wswap(&messlen);
	to_do--;

	if (to_do == 0) goto ARGEND;
	if (to_do != 2) goto ARG12;
	if (arg_length_known)
	{
		if (varg_length[va_i] == 1) goto ARG12;
		if (varg_length[va_i] == 4) goto ARG16;
	}
	else goto ARG16;

ARG12:	/* HELP DISABLE	Alpha(1) */
	helpflg = varg_pointer[va_i++] ;						/* Get the help disable flag.		*/
	to_do--;

	if (to_do == 0) goto ARGEND;
	if (to_do != 2) goto ARG13;
	if (arg_length_known)
	{
		if (varg_length[va_i] == 2) goto ARG13;
		if (varg_length[va_i] == 4) goto ARG16;
	}
	else goto ARG16;

ARG13:	/* reserved	Alpha(2) */
	pfkey = varg_pointer[va_i++] ;							/* Get the pfkey flag mask		*/
	to_do--;

	if (to_do == 0) goto ARGEND;
	if (to_do == 1) goto ARG16;
	if (to_do >= 2) goto ARG14;
	if (arg_length_known)
	{
		if (varg_length[va_i] == 4) goto ARG16;
	}
	else goto ARG16;

ARG14:	/* CANCEL RCVR	Alpha(var) */
	cantext = varg_pointer[va_i++] ;						/* Get the cancel text receiver.	*/
	to_do--;

ARG15:	/* CANCEL RCVR LEN  Int(4) */
	canlen = *(varg_pointer[va_i++]) ;						/* Get the text receiver length.	*/
	wswap(&canlen);
	to_do--;

	if (to_do == 0) goto ARGEND;

ARG16:	/* COMP-CODE	Int(4) */
	comcode = (int4 *)varg_pointer[va_i++] ;					/* Get the completion code from link.	*/
	to_do--;

	if (to_do == 0) goto ARGEND;

ARG17:	/* RETURN-CODE	Int(4) */
	retcod = (int4 *)varg_pointer[va_i++] ;						/* Get the program return code.		*/

ARGEND:	/* END OF ARGUMENTS */

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
		memset( l_vol, ' ', 6 );
		memset( l_lib, ' ', 8 );
	}
	else										/* Invalid link type.			*/
	{
		swap_put ( comcode, 8 ) ;						/* Unsuccessful link.			*/
		swap_put ( retcod, 40 ) ;						/* Invalid Parameter.			*/

		werrlog(ERRORCODE(3),l_file,ltype,0,0,0,0,0,0);
		return(0);
	}

	leftjust(l_vol,6);
	leftjust(l_lib,8);
	upper_mem(l_vol,6);
	upper_mem(l_vol,8);
	l_vol[6] = '\0';
	l_lib[8] = '\0';


	/*
	**	Do internal translations
	*/

	if (0==memcmp(l_file,"DISPLAY ",8))						/* If they are calling DISPLAY, then	*/
	{										/* call WFILE_DISP.			*/
#ifdef unix
		if (!ishelpactive() && isdebug())					/* If COBOL debugger running		*/
		{
			vraw_stty_save();						/* Save the current stty values		*/
		}
#endif
		setprogid(l_file);
		wfile_disp();
#ifdef unix
		if (!ishelpactive() && isdebug())					/* If COBOL debugger running		*/
		{
			vraw_stty_restore();						/* Restore the saved stty values	*/
		}
#endif
		swap_put ( comcode, 0L ) ;						/* swap and set the completion code.	*/
		swap_put ( retcod, 0L ) ;						/* swap and set the return code.	*/
		return(0);
	}

	if (0==memcmp(l_file,"COPY    ",8)) 
		memcpy(l_file,"WCOPY   ",8);						/* Translate 	COPY --> WCOPY		*/
	else if (0==memcmp(l_file,"SORT    ",8)) 
		memcpy(l_file,"WSORT   ",8);						/* 		SORT --> WSORT		*/

	unloadpad( progname_str, l_file, 8 ) ;						/* make progname a string		*/

#ifdef VMS
/*************************************************  Start of VMS section  *******************************************************/

	/*
	**	We first try an internal "soft link" via a COBOL call.
	**
	**	NOTE:	We use newlevel()/oldlevel() around the wclink() instead of a save & restore because this section
	**		of code is recursive.  Wclink() can start a routine which calls LINK.
	*/
	newlevel();									/* Increment the link-level		*/
	wclink ( l_file, &parm_list, &retval ) ;					/* Call the COBOL link. (in LINKSUBS.C)	*/
	oldlevel();									/* Decrement the link-level		*/

	if (retval == 1)								/* Static link was a success.		*/
	{
		retval = atol(WISPRETURNCODE);

		setretcode(WISPRETURNCODE);

		swap_put ( comcode, 0L ) ;						/* Successful link.			*/
		swap_put ( retcod, retval ) ;						/* Return code from link program.	*/

		ppunlink(linklevel());							/* Putparm UNLINK			*/
		return;
	}

	vmstry = 0;
	spawn_action = 1;								/* Initialize to .EXE			*/

vmsagain:
	if (*ltype == 'S')								/* Look in system area.			*/
	{
		switch ( vmstry )
		{
			case 0:
			{
				strcpy ( link_filespec, "SYS$SYSTEM:" ) ;
				strcat ( link_filespec, progname_str ) ;
				strcat ( link_filespec, ".COM" ) ;
				spawn_action = 3 ;					/* to start a .COM file			*/
				break ;
			}
			case 1:
			{
				strcpy ( link_filespec, "SYS$SYSTEM:" ) ;
				strcat ( link_filespec, progname_str ) ;
				strcat ( link_filespec, ".EXE" ) ;
				spawn_action = 1 ;					/* to start a .EXE file			*/
				break ;
			}
			case 2:
			{
				strcpy ( link_filespec, "WISP$ROOT:[COM]" ) ;		/*  then look in WISP$ROOT:[COM].	*/
				strcat ( link_filespec, progname_str ) ;
				strcat ( link_filespec, ".COM" ) ;
				spawn_action = 3 ;					/* to start a .COM file			*/
				break ;
			}
			case 3:
			{
				strcpy ( link_filespec, "WISP$ROOT:[EXE]" ) ;		/*  then look in WISP$ROOT:[EXE],	*/
				strcat ( link_filespec, progname_str ) ;
				strcat ( link_filespec, ".EXE" ) ;
				spawn_action = 1 ;					/* to start a .EXE file			*/
				break ;
			}
		}
		vmstry++;								/* Increment the number of tries.	*/
	}
	else if (*ltype == 'P')
	{
		mode = IS_SUBMIT;
		ptr = wfname(&mode,l_vol,l_lib,l_file,link_filespec);			/* This will check .COM then .EXE	*/
		*ptr = '\0' ;								/* null terminate link_filespec.	*/
		vmstry++;
	}
	else
	{
		switch ( vmstry )
		{
			case 0:								/* First look in PRGVOL:[PRGLIB]	*/
			{
				mode = IS_SUBMIT;
				ptr = wfname(&mode,l_vol,l_lib,l_file,link_filespec);	/* This will check .COM and .EXE	*/
				*ptr = '\0' ;						/* null terminate link_filespec.	*/
				break ;
			}
			case 1:								/* Next look in current directory.	*/
			{
				strcpy ( link_filespec, progname_str ) ;
				strcat ( link_filespec, ".COM" ) ;
				spawn_action = 3;
				break ;
			}
			case 2:								/* try for .COM				*/
			{
				strcpy ( link_filespec, progname_str ) ;
				strcat ( link_filespec, ".EXE" ) ;
				spawn_action = 1;
				break ;
			}
		}
		vmstry++;
	}
	vms_return = (0==fexists( link_filespec )) ? 1:0;				/* Check file; does it exist?		*/

	if (vms_return != 0 )
	{
		if (*ltype == 'S' && vmstry < 4) goto vmsagain;				/* Try with next specification.		*/
		if (*ltype == ' ' && vmstry < 3) goto vmsagain;				/* Try with next specification.		*/

		/*
		**	FILE NOT FOUND
		*/
		swap_put ( comcode, 8L ) ;						/* Unsuccessful link.			*/
		wang_retcode = 20;							/* File not found			*/

		sprintf(temp_retval,"%03d",wang_retcode);				/* Put wang_retval into string format.	*/
		setretcode(temp_retval);
		
		swap_put ( retcod, wang_retcode );
		return;
	}

	wang_retcode = 0;								/* Default to success			*/
	wang_compcode = 0;

	if ( 0==strcmp(splitext(link_filespec),".EXE") ) spawn_action = 1;
	if ( 0==strcmp(splitext(link_filespec),".COM") ) spawn_action = 3;

	if ( arg_length_known && ( spawn_action == 1 ) )				/* /DLINK to an EXE			*/
	{
		/*
		**	Perform a dynamic LINK (/DLINK) with parameter passing.
		*/

		if ( parmcnt > 0 ) 							/* if there are any parameters;		*/
		{
			writevmslink( link_filespec, parmcnt, &parm_list, &len_list, vmskey);
		}
		else
		{
			strcpy ( vmskey, NULL_FILE_NAME ) ;				/* VMSLINKSUB won't try to read args.	*/
		}
		vonexit( NORMALIZE );							/* don't clear src			*/
		vexit();								/* Reset the terminal.			*/

							/* build spawn name starting with "@WISP$ROOT:[COM]VMS$START$LINK $"    */

		strcpy ( link_callspec, "@WISP$ROOT:[COM]VMS$START$LINK $" ) ;  	/* Start with .COM file			*/
		strcat ( link_callspec, link_filespec ) ;				/* cat program name			*/
		strcat ( link_callspec, " " ) ;						/* cat space				*/
		strcat ( link_callspec, vmskey ) ;					/* cat parameter file name      	*/

		savelevel = linklevel();						/* Save the link-level.			*/
		/* It is up to the spawned program to do a newlevel().	*/
		vms_return = spawn2( 3, link_callspec, "", &vms_status ) ;		/* Spawn the command; the 3 means .COM  */

		setlevel(savelevel);							/* Restore the link-level		*/

		/*
		**	After spawn, figure out what to do about return codes, etc.		
		*/

		if (vms_return == SS$_NORMAL || vms_return == CLI$_NORMAL)		/* Spawn was successful.		*/
		{
			ppunlink(savelevel);						/* Putparm UNLINK			*/

			if ( parmcnt > 0 ) 						/* if there are any parameters;		*/
			{
				if ( !fexists(vmskey) )					/* if parameter file does NOT exist;	*/
				{							/* subroutine terminated abnormally.	*/
					wang_compcode = 16;				/* Subroutine exited abnormally		*/
					wang_retcode = 0;				/* Reset return code to zero.		*/
				}
				else							/* ELSE, parameter file DOES exist	*/
				{							/* SO, read parameters into memory	*/
					readvmslink(link_filespec, parmcnt, &parm_list, &len_list, vmskey, 
							&wang_compcode, &wang_retcode);
				}
			}
			else
			{
				if (1 == vms_status)
				{
					/* 
					**	VMS normal status.
					*/
					wang_retcode = 0;
				}
				else if (1 == vms_status % 10000 )
				{
					/*
					**	WISP coded return codes == (RC * 10000 + 1)
					**	the "+ 1" is to make them VMS warnings/informational codes
					*/
					wang_retcode = (vms_status - 1) / 10000;
				}
				else
				{
					/*
					**	This is not a WISP coded return code.
					*/
					wang_retcode = vms_status;
				}
			}
		}
		else
		{
			wang_compcode = 8;						/* Unsuccessful link.			*/
			wang_retcode = vms_to_wang_codes ( vms_return ) ;		/* convert VMS code to WANG code.	*/
		}


		if ( *can_exit == ' ' )							/* Cancel-Exit not at this level	*/
		{
			if ( LOGOFFFLAG )						/* A lower level called logoff		*/
			{
				wexit(32L);						/* Terminate this level			*/
			}
		}
		else
		{
			CANEXITFLAG--;							/* We've just come thru a Cancel-Exit   */
			LOGOFFFLAG = 0;							/* Cancel the logoff			*/
		}

		vsynch() ;
		rts_first = TRUE ;						/* Set so return from link re-inits screen	*/

		vonexit( NORMALIZE | CLEAR_SCREEN );					/* Reset onexit status			*/

	}
	else
	{
		/*
		**	(ELSE) is not an EXE or not a LINKDESC (/DLINK).
		**
		**	Handles .COM files and .EXE without LINKDESC
		*/

		savelevel = linklevel();						/* Save the link-level.			*/
		if ( spawn_action == 3 )						/* if file is a .COM file;		*/
		{
			strcpy ( link_callspec, "@" ) ;					/* @ means run .COM file for VMS	*/
			strcat ( link_callspec, link_filespec ) ;			/* Add .COM file to execute.		*/
			newlevel();							/* Increment the link-level		*/
		}
		else
		{
			strcpy ( link_callspec, link_filespec ) ;			/* .EXE file				*/
			/* For EXE files it is up to the spawned program to do a newlevel() call.				*/
		}
		vms_return = spawn2 ( spawn_action , link_callspec, "", &vms_status ) ; /* Spawn the command.			*/

		setlevel(savelevel);							/* Restore the link-level		*/


		if (vms_return == SS$_NORMAL || vms_return == CLI$_NORMAL)		/* Spawn was successful.		*/
		{
			ppunlink(savelevel);						/* Putparm UNLINK			*/
			wang_compcode = 0;						/* Successful link.			*/

			if (1 == vms_status)
			{
				/* 
				**	VMS normal status.
				*/
				wang_retcode = 0;
			}
			else if (1 == vms_status % 10000 )
			{
				/*
				**	WISP coded return codes == (RC * 10000 + 1)
				**	the "+ 1" is to make them VMS warnings/informational codes
				*/
				wang_retcode = (vms_status - 1) / 10000;
			}
			else
			{
				/*
				**	This is not a WISP coded return code.
				*/
				wang_retcode = vms_status;
			}

		}
		else
		{
			wang_compcode = 8;						/* Unsuccessful link.			*/
											/* Set return code from spawned program:*/
			wang_retcode = vms_to_wang_codes ( vms_return ) ;		/* convert VMS code to WANG code.	*/
		}

	}

	if (0 == wang_compcode)
	{
		load_defaults();
	}

	sprintf(temp_retval,"%03d",wang_retcode);					/* Put wang_retcode into string format.  */
	setretcode(temp_retval);

	swap_put ( comcode, wang_compcode );
	swap_put ( retcod, wang_retcode );

	return;
/*************************************************  End of VMS section  *********************************************************/
#endif /* VMS */

#ifdef unix
/*************************************************  Start of UNIX section  ******************************************************/

	not_found = 1;									/* assume not found			*/

	if ( *ltype == ' ' || *ltype == 'P' )
	{
		mode = IS_SUBMIT;
		p = wfname(&mode,l_vol,l_lib,l_file,link_filespec);			/* expand the name 			*/
		*p = (char)0;

		if ( fexists(link_filespec) )
		{
			not_found = 0;							/* found it				*/
		}
		else if (*ltype == ' ')							/* Try the current dir			*/
		{
			mode = IS_SUBMIT;
			memcpy(l_vol,".     ",6);
			memcpy(l_lib,".       ",8);
			p = wfname(&mode,l_vol,l_lib,l_file,link_filespec);		/* expand the name 			*/
			*p = (char)0;
			if ( fexists(link_filespec) )
			{
				not_found = 0;						/* found it				*/
			}
		}

		if (not_found)
		{
			swap_put ( comcode, 8L ) ;					/* Unsuccessful link			*/
			swap_put ( retcod, 20L ) ;					/* File not found.			*/
			werrlog(ERRORCODE(7),link_filespec,0,0,0,0,0,0,0);
			return;
		}
	}

	if ( *ltype == 'S' )								/* Scan along the $PATH			*/
	{
		not_found = searchpath(progname_str,link_filespec);

		if (not_found)
		{
			swap_put ( comcode, 8L ) ;					/* Unsuccessful link			*/
			swap_put ( retcod, 20L ) ;					/* File not found.			*/
			werrlog(ERRORCODE(5),progname_str,0,0,0,0,0,0,0);
			return;
		}
	}

	/*
	**	We've found the unix full file spec.  Find out what run type it is.
	*/

	ftyp = runtype(link_filespec);							/* get the run type			*/

	switch(ftyp)
	{
	default:
	case RUN_NOT:
		swap_put ( comcode, 8L ) ;						/* Unsuccessful link			*/
		swap_put ( retcod, 52L ) ;						/* NOT a runable file			*/
		werrlog(ERRORCODE(11),link_filespec,0,0,0,0,0,0,0);
		return;

	case RUN_ACCESS:
	case RUN_UNKNOWN:
		swap_put ( comcode, 8L ) ;						/* Unsuccessful link			*/
		swap_put ( retcod, 28L ) ;						/* Access denied.			*/
		werrlog(ERRORCODE(9),link_filespec,0,0,0,0,0,0,0);
		return;

	case RUN_EXEC:
	case RUN_ACUCOBOL:
	case RUN_MFINT:
	case RUN_MFGNT:
	case RUN_SHELL:
	case RUN_PROC:
	case RUN_PROCOBJ:
		break;
	}

	/*
		Set up "link_callspec" this is the value that the link frontend (ACULINK or MFLINK) is to 
		use in the CALL stmt.
	*/

	switch(ftyp)
	{
	case RUN_MFINT:
	case RUN_MFGNT:
		strcpy(link_callspec,link_filespec);					/* Full path without extension.		*/
#ifdef OLD
		p = &link_callspec[strlen(link_callspec)-4];				/* Point to where the '.' should be.	*/
		if (*p == '.') *p = (char)0;						/* Remove the extension.		*/
#endif
		break;

	case RUN_EXEC:									/* (Used by Native Micro Focus)		*/
		strcpy(link_callspec,progname_str);					/* Routine name only, uppercase		*/
		break;

	default:									/* Full path				*/
		strcpy(link_callspec,link_filespec);
		break;
	}

	if ( arg_length_known || 0 == parmcnt )						/* For ACU & MF write out params	*/
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

	if ( ftyp == RUN_ACUCOBOL || ftyp == RUN_MFINT || ftyp == RUN_MFGNT ||		/* If going to COBOL or PROC then	*/
	     ftyp == RUN_PROC || ftyp == RUN_PROCOBJ )			
	{
		vonexit( NORMALIZE );							/* don't clear sreen.			*/
	} 

	if (!ishelpactive() && isdebug())						/* If COBOL debugger running		*/
	{
		vraw_stty_save();							/* Save the current stty values		*/
	}

	vexit();									/* Reset the terminal.			*/

	old_SIGINT  = signal(SIGINT,  SIG_IGN);						/* Ignore signals			*/
	old_SIGQUIT = signal(SIGQUIT, SIG_IGN);
	old_SIGCLD  = signal(SIGCLD,  SIG_DFL);						/* Use Default DEATH-OF-CHILD signal	*/


	wang_retcode = 0;
	wang_compcode = 0;

	switch (pid = fork())								/* Fork into two processes		*/
	{
	case 0: /* CHILD PROCESS */

		if (parm_file_written)							/* If we wrote a parm file then...	*/
		{
			sprintf(buff,"%s=%s",WISP_LINK_ENV,linkkey);			/* Store the linkkey in env		*/
			setenvstr(buff);
		}

		if (*can_exit != ' ')						/* Mark the cancel-exit in a shell var so it is	*/
		{								/* even if we go across a script etc.		*/
			sprintf(buff,"%s=%d",WISP_CANCELEXIT_ENV,getpid());
			setenvstr(buff);
		}

		setprogdefs(l_vol,l_lib);						/* Set up PROGLIB and PROGVOL		*/
		clearprogsymb();							/* Clear PROGLIB/VOL from symbol	*/

		switch (ftyp)
		{
		case RUN_EXEC: 
			sh_parm[0] = link_filespec;					/* argv[0] is the filespec		*/

			if (parm_file_written)						/* If wrote parm file then...		*/
			{
				sh_parm[1] = '\0';					/* null terminate it.			*/
			}
			else								/* If no parm file then we don't know	*/
			{								/* the parm lengths so assume C style	*/
				for (i=0; i < parmcnt; ++i)				/* null terminated strings (for LPI)	*/
					sh_parm[i+1]=parm_list.parm[i];			/* and load them onto argv list.	*/
				sh_parm[i+1] = '\0';					/* Null terminate it			*/
			}

			/* If this program wants to play the link-level game it must do the newlevel() call.			*/
			execvp(sh_parm[0],sh_parm);					/* Do the exec.				*/

			werrlog(ERRORCODE(4),link_filespec,errno,0,0,0,0,0,0);
			vexit();							/* Reset the screen from the werrlog	*/
			*retcod=fixerr(errno);						/* xlat unix errno to wang error# 	*/
			break;

		case RUN_SHELL:								/* Assume a shell script		*/
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
					sh_parm[i+2]=malloc(len_list.len[i] + 1);			/* Malloc space		*/
					memcpy(sh_parm[i+2],parm_list.parm[i],len_list.len[i]);		/* Load parm		*/
					sh_parm[i+2][len_list.len[i]] = '\0';				/* Null terminate parm	*/
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
					sh_parm[i+2]=parm_list.parm[i];
				}
				sh_parm[i+2] = '\0';					/* null terminate it			*/
			}

			newlevel();							/* Increment the link-level		*/
			execvp(sh_parm[0],sh_parm);

			werrlog(ERRORCODE(4),sh_parm[0],errno,0,0,0,0,0,0);
			vexit();							/* Reset the screen from the werrlog	*/
			*retcod=fixerr(errno);
			break;

		case RUN_ACUCOBOL: 							/* An acu_cobol object			*/
		case RUN_MFINT:								/* Micro focus .int or .gnt code	*/
		case RUN_MFGNT:
			{
				struct wruncfg cfg;
				char	options[80];
				char	*eptr, *optr;
				int	arg;

				wrunconfig(&cfg);					/* Load wrunconfig options file		*/

				strcpy(options,cfg.wrun_options);

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

					if ( ftyp == RUN_ACUCOBOL )			/* ACUCOBOL object file			*/
					{
						sh_parm[arg++] = "ACULINK";		/* ACU Link interface program		*/
						sh_parm[arg++] = linkkey;		/* Key to parm area			*/
					}
					else					/* Micro Focus GNT or INT file		*/
					{
						sh_parm[arg++] = "MFLINK";		/* MF Link interface program		*/
					}
				}

				sh_parm[arg++] = link_callspec;				/* Name of program to start		*/
				/* NOTE: This will normally be a comment except if LINK is called from a non-COBOL program.	*/

				sh_parm[arg++] = '\0';					/* null terminate it			*/

				/* The COBOL program will do a newlevel() call in initwisp()					*/
				execvp(sh_parm[0],sh_parm);				/* Start the new program.		*/

				werrlog(ERRORCODE(14),sh_parm[0],l_file,errno,0,0,0,0,0);
				vexit();						/* Reset the screen from the werrlog	*/
				*retcod=fixerr(errno);
				break;
			}

		case RUN_PROC:
		case RUN_PROCOBJ:
			{
				char	*name_ptr;
				char	*debug_ptr;
				int	arg;

				if ( !(name_ptr = getenv("WPROC")) )			/* Get the name of "wproc"		*/
				{
					name_ptr = "wproc";				/* Use "wproc" as the default name	*/
				}
				arg = 0;
				sh_parm[arg++] = name_ptr;				/* argv[0] is the "wproc" program	*/
				if ( debug_ptr = getenv("WPROCDEBUG") )			/* Check for debug flags		*/
				{
					sh_parm[arg++] = debug_ptr;			/* Add the debug flags			*/
					freopen("wproc.trace","w",stdout);		/* Redirect stdout			*/
				}
				sh_parm[arg++] = "-p";					/* Use parameter file			*/
				sh_parm[arg++] = link_filespec;				/* argv[2] in the filespec	 	*/
				sh_parm[arg++] = '\0';					/* null terminate it			*/

				newlevel();						/* Increment the link-level		*/
				execvp(sh_parm[0],sh_parm);

				werrlog(ERRORCODE(4),sh_parm[0],errno,0,0,0,0,0,0);
				vexit();						/* Reset the screen from the werrlog	*/
				*retcod=fixerr(errno);
				break;
			}
		}
		exit(*retcod);
		break;

	case -1:									/* The link failed			*/
		werrlog(ERRORCODE(18),errno,0,0,0,0,0,0,0);
		wang_compcode = 8;
		wang_retcode = 99;
		break;

	default: /* PARENT PROCESS */

		wwaitpid(pid,&exit_code);						/* Wait for linked process to complete	*/
		vsynch();								/* Resynch video			*/
		load_defaults();							/* Reload defaults: may have changed	*/
		rts_first = TRUE ;							/* Return from link re-inits screen	*/
		break;
	}

	signal(SIGINT,  old_SIGINT);							/* Reset the signals after the link	*/
	signal(SIGQUIT, old_SIGQUIT);
	signal(SIGCLD,  old_SIGCLD);							/* Ignore DEATH-OF-CHILD signal		*/

	if (!ishelpactive() && isdebug())						/* If COBOL debugger running		*/
	{
		vraw_stty_restore();							/* Restore the saved stty values	*/
	}

	vonexit( NORMALIZE | CLEAR_SCREEN );						/* Reset onexit status to clear screen	*/

	ppunlink(linklevel());								/* Putparm UNLINK			*/

	if ( parm_file_written )							/* If parm file written then clean up	*/
	{
		int4	the_comp_code = 0;
		int4	the_ret_code = 0;

		readunixlink(link_filespec, parmcnt, &parm_list, &len_list, linkkey, &the_comp_code, &the_ret_code);

		if (the_comp_code != -1)
		{
			wang_compcode = the_comp_code;
			wang_retcode = the_ret_code;
		}
		else
		{
			wang_compcode = 0;
			wang_retcode = exit_code;
		}
	} 
	else
	{
		wang_compcode = 0;
		wang_retcode = exit_code;
	}

	if ( pid == -1 )								/* link failed				*/
	{
		wang_compcode = 8;
		wang_retcode = 99;
	}

	if ( *can_exit == ' ' )								/* Cancel-Exit not at this level	*/
	{
		char	*gptr;
		int	kpid;

		if ( LOGOFFFLAG )							/* A lower level called logoff		*/
		{
			wexit(32L);							/* Terminate this level			*/
		}

		gptr = getenv(WISP_CANCELEXIT_ENV);				/* If the cancel-exit var is set and the process*/
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

	swap_put ( comcode, wang_compcode ) ;						/* swap and set the completion code.	*/
	swap_put ( retcod, wang_retcode ) ;						/* swap and set the return code.	*/

	return;

/*************************************************  End of UNIX section  ********************************************************/
#endif	/* unix */							

#ifdef MSDOS
/*************************************************  Start of MSDOS section  *****************************************************/

	not_found = 1;								/* Flag gets set to 0 when file is found.	*/

	if ( *ltype == ' ' || *ltype == 'P' )
	{
		mode = IS_SUBMIT;
		p = wfname(&mode,l_vol,l_lib,l_file,link_filespec);			/* expand the name 			*/
		*p = (char)0;

		if ( fexists(link_filespec) )
		{
			not_found = 0;							/* found it				*/
		}
	}

	/*
	**	Note: On MSDOS the current directory is ALWAYS checked first before the PATH.
	*/
	if (not_found && (*ltype == ' ' || *ltype == 'S' ))				/* Try the current dir			*/
	{
		mode = IS_SUBMIT;
		memcpy(l_vol,".     ",6);
		memcpy(l_lib,".       ",8);
		p = wfname(&mode,l_vol,l_lib,l_file,link_filespec);			/* expand the name 			*/
		*p = (char)0;
		if ( fexists(link_filespec) )
		{
			not_found = 0;							/* found it				*/
		}
	}

	if (not_found)
	{
		if ( *ltype == 'S' )							/* Scan along the $PATH			*/
		{
			not_found = searchpath(progname_str,link_filespec);

			if (not_found)
			{
				swap_put ( comcode, 8L ) ;				/* Unsuccessful link			*/
				swap_put ( retcod, 20L ) ;				/* File not found.			*/
				werrlog(ERRORCODE(5),progname_str,0,0,0,0,0,0,0);
				return(0);
			}
		}
		else
		{
			swap_put ( comcode, 8L ) ;					/* Unsuccessful link			*/
			swap_put ( retcod, 20L ) ;					/* File not found.			*/	
			werrlog(ERRORCODE(7),link_filespec,0,0,0,0,0,0,0);
			return(0);
		}
	}


	/*
	**	We've found the MSDOS full file spec.  Find out what run type it is.
	*/

	upper_string(link_filespec);						/* Make sure we are uppercase			*/

	ftyp = runtype(link_filespec);							/* get the run type			*/

	if (ftyp == RUN_SHELL || ftyp == RUN_EXEC)
	{
		char	command[256];
		int4	rc;

		savelevel = linklevel();					/* Save the link-level				*/
		if (ftyp == RUN_SHELL)						/* If a BAT file the increment the link-level	*/
		{								/* (else its up to the program to newlevel())	*/
			newlevel();
		}

		strcpy(command,link_filespec);					/* Now add the name of the procedure.	*/
		if (arg_length_known)
		{
			int	cpos;
			cpos = strlen(command);					/* Find out the end of the string.	*/
			for(i=0; i<parmcnt; i++)
			{
				if ((cpos + len_list.len[i] + 3) > 128)
				{
					swap_put ( comcode, 8L ) ;		/* Unsuccessful link				*/
					swap_put ( retcod, 60L ) ;		/* Command too big				*/
					werrlog(ERRORCODE(20),"Command is greater then 128 bytes",0,0,0,0,0,0,0);
					return(0);
				}
				command[cpos++] = ' ';				/* Insert a space.			*/
				command[cpos++] = '\"';				/* And Quotes.				*/
				memcpy(&command[cpos], parm_list.parm[i], (size_t)len_list.len[i]);
				cpos += len_list.len[i];
				command[cpos++] = '\"';				/* Close Quotes.			*/
				command[cpos] = (char)0;			/* NULL terminate command		*/
			}
		}
		
		vexit();							/* Clear video for a system call.		*/
		rc = system( command );						/* Execute with a command processor.		*/

		setlevel(savelevel);						/* Restore the link-level			*/
		ppunlink(savelevel);						/* Putparm UNLINK				*/

		vsynch();							/* Synchronize video after a system call.	*/
		load_defaults();
		rts_first = TRUE;						/* Tell video to redraw the screen.		*/

		if (rc == -1)
		{
			swap_put ( comcode, 8L ) ;
			swap_put ( retcod,  0L );
		}
		else
		{
			swap_put ( comcode, 0L ) ;
			swap_put ( retcod,  rc );
		}
	}
#ifdef DMF
	else if ( ftyp == RUN_MFINT || ftyp == RUN_MFGNT )			/* .GNT or .INT Micro Focus COBOL/2 file.	*/
	{
		/*
		**	We first try an internal "soft link" via a COBOL call.
		**
		**	NOTE:	We use newlevel()/oldlevel() around the wclink() instead of a save & restore because this section
		**		of code is recursive.  Wclink() can start a routine which calls LINK.
		*/
		newlevel();							/* Increment the link-level			*/
		wclink( link_filespec, &parm_list, &retval );			/* Call subroutine through wclink module.	*/
		oldlevel();							/* Decrement the link-level			*/

		if( retval == 1 )						/* Successful call.				*/
		{
			ppunlink(linklevel());					/* Putparm UNLINK			*/
			wang_retcode = atol ( WISPRETURNCODE );

			swap_put ( comcode, 0L ) ;
			swap_put ( retcod, wang_retcode );
		}
		else
		{
			swap_put( comcode, 8L );				/* Unsuccessful link.				*/
			swap_put( retcod, 20L );				/* File not found.				*/

			werrlog( ERRORCODE(7), testname,0,0,0,0,0,0,0);
		}
	}
#endif /* DMF */
#ifdef DACU
	else if ( ftyp == RUN_ACUCOBOL )					/* ACUCOBOL					*/
	{
		/*
		**	If we are already in an ACUCOBOL runtime then we do a
		**	"soft" link using call_acucobol().
		*/
		if (acu_cobol)
		{
			int	rc;

			newlevel();
			call_acucobol( link_filespec, parmcnt, parm_list.parm, len_list.len, &rc );
			oldlevel();

			switch(rc)
			{
			case 0:	/* SUCCESS */
				ppunlink(linklevel());					/* Putparm UNLINK			*/
				wang_retcode = atol ( WISPRETURNCODE );
				swap_put ( comcode, 0L ) ;				/* Successful link			*/
				swap_put ( retcod, wang_retcode );
				break;

			case 1:	/* Program file missing or inaccessible */
				swap_put ( comcode, 8L ) ;				/* Unsuccessful link			*/
				swap_put ( retcod, 20L ) ;				/* File not found.			*/
				werrlog(ERRORCODE(7),link_filespec,0,0,0,0,0,0,0);
				break;

			case 2:	/* Not a COBOL program */
			case 3:	/* Corrupted program file */
			case 5:	/* Unsupported object code version number */
				swap_put ( comcode, 8L ) ;				/* Unsuccessful link			*/
				swap_put ( retcod, 52L ) ;				/* NOT a runable file			*/
				werrlog(ERRORCODE(11),link_filespec,0,0,0,0,0,0,0);
				break;

			case 4: /* Not enough memory to load file */
				swap_put ( comcode, 8L ) ;				/* Unsuccessful link			*/
				swap_put ( retcod, 60L ) ;				/* NOT enough memory			*/
				werrlog(ERRORCODE(13),link_filespec,0,0,0,0,0,0,0);
				break;
	
			case 6: /* Recursive CALL of a program */
				swap_put ( comcode, 8L ) ;				/* Unsuccessful link			*/
				swap_put ( retcod, 50L ) ;				/* Recursive call			*/
				werrlog(ERRORCODE(16),link_filespec,0,0,0,0,0,0,0);
				break;

			default:
				swap_put ( comcode, 8L ) ;				/* Unsuccessful link			*/
				swap_put ( retcod, 99L ) ;
				sprintf(buff,"call_acucobol() failed on file [%s] with [%d]",link_filespec,rc);
				werrlog(ERRORCODE(20),buff,0,0,0,0,0,0,0);
				break;
			}
		}
		else	/* Not in runtime */
		{
			/*
			**	Not in an ACUCOBOL runtime so must start one to LINK to a COBOL program.
			*/
			struct wruncfg cfg;
			char	command[256];	/* MSDOS command line limit */
			int	rc;

			wrunconfig(&cfg);					/* Load wrunconfig options file			*/
			sprintf(command,"%s %s %s", cfg.wrun_runcbl, cfg.wrun_options, link_filespec);
			if (strlen(command) > 128)
			{
				werrlog(ERRORCODE(20),"system(command) too int4",0,0,0,0,0,0,0);
			}
			else
			{
				/*
				**	Normally we would do a vexit() next except on DOS this causes a lowlevel
				**	clear screen in vrawexit().  On DOS this vrawexit() changes the blue background
				**	to black which is exactly what we are trying to avoid with the vonexit(NORMALIZE)
				**	calls.  On DOS we don't actually change the input-mode so we can live without
				**	a vexit() to restore it. So we will just drop this logic until we can figure
				**	out what is the proper behaviour and fix it.
				*/
				/* vonexit( NORMALIZE ); */			/* don't clear sreen.				*/
				/* vexit(); */					/* Shutdown video				*/

				savelevel = linklevel();			/* Save the link-level				*/

				rc = system(command);				/* Execute the command				*/

				setlevel(savelevel);				/* Restore the link-level			*/

				vsynch();					/* Resynch video				*/
				rts_first = TRUE ;				/* Return from link re-inits screen		*/
				/* vonexit( NORMALIZE | CLEAR_SCREEN ); */	/* Reset onexit status to clear screen		*/

				if (rc == 0)					/* If system(command) successful		*/
				{
					ppunlink(savelevel);			/* Putparm UNLINK				*/
					swap_put ( comcode, 0L ) ;		/* Successful link				*/
					swap_put ( retcod, 0L ) ;
					load_defaults();			/* Reload defaults: may have changed		*/
				}
				else						/* system(command) failed			*/
				{
					sprintf(buff,"system(command) failed [errno=%d]",errno);
					werrlog(ERRORCODE(20),buff,0,0,0,0,0,0,0);
					werrlog(102,command,0,0,0,0,0,0,0);

					swap_put ( comcode, 8L ) ;		/* Unsuccessful link				*/
					swap_put ( retcod, 60L ) ;
				}
			}

		}
	}
#endif /* DACU */
	else									/* Invalid type					*/
	{
		swap_put ( comcode, 8L ) ;					/* Unsuccessful link				*/
		swap_put ( retcod, 52L ) ;					/* NOT a runable file				*/
		werrlog(ERRORCODE(11),link_filespec,0,0,0,0,0,0,0);
	}

	return(0);								/* Exit LINK now.				*/

/*************************************************  End of MSDOS section  *******************************************************/
#endif	/* MSDOS */

}

#ifndef VMS
/*
**	Routine:	searchpath()
**
**	Function:	To search the path for a file to run/exec.
**
**	Description:	Search the path for the file to run/exec.  Try upper/lower case names
**			and try different extensions.
**
**	Arguments:	
**		filename	(I)	The filename to look for.
**		filespec	(O)	The resultant full file spec.
**
**	Return:	
**		0	File was found
**		1	Not found
**
**	Warnings:	None
**
**	History:	07/31/92	Written by GSL
**
*/

static int searchpath(filename,filespec)
char	*filename, *filespec;
{
	char	testname[80];
	char	lowname[9], upname[9];
	int	pathcnt;
	int	not_found;

#ifdef unix
	strcpy(lowname,filename);
	lower_string(lowname);
#endif
	strcpy(upname,filename);
	upper_string(upname);

	not_found = 1;
	for(pathcnt=1; not_found && osd_path(pathcnt); pathcnt++)		/* while not found & more paths		*/
	{
		/*
		**	Build test filepath (uppercase)
		**	We check uppercase first because ACUCOBOL & MF objects are usually uppercase.
		**
		**	NOTE:   We check all the extensions against the uppercase name before we check any against
		**		the lowercase name.
		*/
		buildfilepath( testname, osd_path(pathcnt), upname);
		not_found = findexts(testname,filespec);			/* Find the file & extension		*/

#ifdef unix
		if (not_found)
		{
			/*
			**	build test filepath (lowercase)
			*/
			buildfilepath( testname, osd_path(pathcnt), lowname);
			not_found = findexts(testname,filespec);		/* Find the file & extension		*/
		}
#endif /* unix */
	}
	return(not_found);
}
#endif /* !VMS */

#ifdef unix
/*
**	Routine:	findrun()
**
**	Function:	To find the file to run.
**
**	Description:	This routine is called from "wproc" (Lexical Procedure Intrp) it's job is to return a native filespec
**			and the linktype to use.
**			If lib or vol are supplied then it is a linktype 'P'
**			If lib and vol are blank it will first look at PROGLIB/PROGVOL then the current dir (linktype ' ').
** 			If still not found it will search the $PATH (linktype 'S').
**
**	Input:		file		Wang style filename
**			lib		Wang style library
**			vol		Wang style volume
**
**	Output:		native		The native path
**			linktype	The linktype to use
**
**	Return:		0	File was found
**			1	Not found
**
**	Warnings:	If not found then native and linktype are undefined.
**
**	History:	07/31/92	Written by GSL
**
*/

int findrun(file,lib,vol,native,linktype)
char	file[8];
char	lib[8];
char	vol[6];
char	*native;
char	linktype[1];
{
	int4	mode;
	char	l_file[9], l_lib[9], l_vol[7];
	char	*ptr;

	loadpad(l_file,file,8); l_file[8] = (char)0;
	loadpad(l_lib, lib, 8); l_lib[8]  = (char)0;
	loadpad(l_vol, vol, 6); l_vol[6]  = (char)0;

	if (0 != memcmp(l_lib,"        ",8) || 0 != memcmp(l_vol,"      ",6))		/* If lib or vol supplied - use em	*/
	{
		linktype[0] = 'P';
		mode = IS_SUBMIT;
		ptr = wfname(&mode,l_vol,l_lib,l_file,native);				/* expand the name 			*/
		*ptr = (char)0;
		return (fexists(native)) ? 0 : 1;					/* Return if found or not		*/
	}

	/*
	**	Neither lib nor vol was supplied.
	*/
	linktype[0] = ' ';								/* Try PROGLIB/PROGVOL			*/
	get_defs(DEFAULTS_PV,l_vol);
	get_defs(DEFAULTS_PL,l_lib);
	mode = IS_SUBMIT;
	ptr = wfname(&mode,l_vol,l_lib,l_file,native);
	*ptr = (char)0;
	if (fexists(native)) return(0);							/* If found then return			*/

	memcpy(l_vol,".     ",6);							/* Try the current dir			*/
	memcpy(l_lib,".       ",8);
	mode = IS_SUBMIT;
	ptr = wfname(&mode,l_vol,l_lib,l_file,native);
	*ptr = (char)0;
	if (fexists(native)) return(0);							/* If found then return			*/

	linktype[0] = 'S';								/* Try searching the $PATH		*/
	unloadpad(l_file,file,8);
	return(searchpath(l_file,native));						/* Return if it was found or not	*/
}

/*
**	Routine:	firstproc()
**
**	Function:	Called by "wproc" to record the filepath of the first proc run.
**
**	Description:	This routine is called upon entry to "wproc" and is passed the full filepath of the proc to run as
**			it was given on the command line.  This is done so we can simulate the PROGLIB/PROGVOL functionality.
**			This routine will strip off the file name and record the dir-path so that it can be checked later when
**			looking for a file.
**
**	Arguments:
**	filepath	The full filepath of the first proc.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	11/17/92	Written by GSL
**
*/
int firstproc(filepath)
char	*filepath;
{
	/*
	**	NOT YET IMPLEMENTED
	*/
	return(0);
}

#endif /* unix */

/*	End of	link.c	*/

