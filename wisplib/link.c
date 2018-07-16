static char copyright[]="Copyright (c) 1988-1999 NeoMedia Technologies, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		link.c
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
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
*/
#define LINK_C

/*
**	Includes
*/

#include <stdio.h>
#include <stdlib.h>
#include <varargs.h>									/* Function uses variable params.	*/
#include <errno.h>

#if defined(unix) || defined(_MSC_VER)
#include <ctype.h>
#include <sys/types.h>
#include <signal.h>
#endif

#ifdef VMS
#include <ssdef.h>
#include <libdef.h>
#include <rmsdef.h>
#include <descrip.h>
#include <climsgdef.h>
#endif

#if defined(MSDOS) || defined(WIN32)
#include <process.h>
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
#include "runtype.h"
#include "wrunconf.h"
#include "idsisubs.h"
#include "paths.h"
#include "wfname.h"
#include "wisplib.h"
#include "wexit.h"
#include "linkvect.h"
#include "filext.h"
#include "level.h"
#include "vwang.h"
#include "sharemem.h"
#include "wmalloc.h"
#include "wispcfg.h"
#include "setprgid.h"
#include "wfiledis.h"

#ifdef WIN32
#include "win32spn.h"
#endif

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

/*
**	Structures and Defines
*/
#ifndef MAX
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#endif
#ifndef MIN
#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#endif

#define MAX_LINKLEVELS	16								/* Maximum call depth			*/
#define	MAX_VARGS	128								/* MAX_LINK_PARMS + control params too	*/

#define LENGTH_KNOWN		1
#define LENGTH_NOT_KNOWN	0

/*
**	Globals and Externals
*/
extern void call_acucobol(char* name, int parmcnt, char* parms[], int lens[], int* rc);
extern void call_mfcobol(char* name, int parmcnt, char* parms[], int lens[], int* rc);

/*
**	Static data
*/

static struct 
{
	/*
	**	linklevel_stack - save data items for soft links that are
	**			  saved in env for hard links.	
	*/
	char	progvol[SIZEOF_VOL];
	char	proglib[SIZEOF_LIB];
	fstruct	*temp_file_list;
	pstruct	*print_file_list;
} linklevel_stack[MAX_LINKLEVELS];

static void prelink_save_state(int linklevel)
{
	/*
	**	Save soft link level stuff
	*/
	get_defs(DEFAULTS_PV, linklevel_stack[linklevel].progvol);
	get_defs(DEFAULTS_PL, linklevel_stack[linklevel].proglib);
	linklevel_stack[linklevel].temp_file_list = g_temp_file_list;
	linklevel_stack[linklevel].print_file_list = g_print_file_list;
	g_temp_file_list = NULL;
	g_print_file_list = NULL;
}
static void postlink_restore_state(int linklevel)
{
	/*
	**	Restore soft link level stuff
	*/
	g_print_file_list = linklevel_stack[linklevel].print_file_list;
	g_temp_file_list = linklevel_stack[linklevel].temp_file_list;
	set_defs(DEFAULTS_PV, linklevel_stack[linklevel].progvol);
	set_defs(DEFAULTS_PL, linklevel_stack[linklevel].proglib);
	save_defaults();
}


/*
**	Function Prototypes
*/

static void do_link();
static int searchnative(const char* nativename, char* filespec);
static int searchpath(const char* wangfilename, char* filespec);

extern void call_acucobol_error(int rc, int4 *wang_retcode, int4 *wang_compcode, char *link_filespec);



int LINK ( va_alist )									/* LINK: Load arg into table		*/
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

	return(0);
}


#ifdef	VMS		/* Start VMS only block of code	*/				/* only if in vms			*/
int LINKDESC ( va_alist )								/* LINK with vms descriptors		*/
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

	return(0);
}
#endif /* VMS */

#ifdef unix
int LINKAIX ( va_alist )								/* LINK for AIX VS COBOL		*/
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

	return(0);
}
#endif	/* unix */

#ifndef VMS
int LINKMF ( va_alist )									/* LINK for Micro Focus COBOL/2		*/
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

	return(0);
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
int LINK2 ( va_alist )									/* Generic paired arg LINK		*/
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

	return(0);
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
static	void do_link(
		int varg_count,								/* Total arg count			*/
		char	*varg_pointer[],						/* pointers to variable param list	*/
		int4	varg_length[],							/* length of each parameter in list	*/
		int	arg_length_known)						/* Arg lengths are known		*/
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
	int	arg_used;
	char	*this_parm;
	int4	arg_len;								/* lenght of the previous arg		*/
	int4	i;
	char 	l_vol[SIZEOF_VOL+1], l_lib[SIZEOF_LIB+1], l_file[SIZEOF_FILE+1];
	int4	mode;
	int4	not_found;
	int4	retval;
	int4	dummy_long;
	int4	parm_file_written = 0;
	int4	savelevel;
	char	*p;
	char	buff[1024];
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
	int	pid;

	void (*old_SIGINT)();								/* Save old signals			*/
	void (*old_SIGQUIT)();
	void (*old_SIGCLD)();
#endif
#ifdef WIN32
	int     save_errno;
#endif
#if defined(unix) || defined(VMS)
	char	**tmp;

	int	num;
	char	temp_retval[4];								/* Temp string to hold return value.	*/
#endif /* unix || VMS */

	char	*sh_parm[64];
	char	linkkey[80];								/* Key to link parm area		*/
	int4	exit_code;
	char	*cobol_frontend;
	char 	link_filespec[WISP_FILEPATH_LEN];					/* Native program path			*/
	char 	link_callspec[WISP_FILEPATH_LEN];					/* the "CALL" spec			*/
	char	progname_str[WISP_FILEPATH_LEN];					/* string version of progname		*/
	char	display_filename[COB_FILEPATH_LEN + 1];					/* Needs same scope as parm_list	*/
	int	native_substitute = 0;
	int	link_separate_window = 0;
	char	tracemsg[1024];
	char	tracebuf[256];
	int	tlen;
	int	soft_link;

	wtrace("LINK","ENTRY","Progname=[%8.8s] Linktype=[%1.1s] (level=%d) Argcnt=[%d]", 
	       varg_pointer[0], varg_pointer[1], linklevel(), varg_count);
	tracemsg[0] = '\0';

	if (lnk_depth == MAX_LINKLEVELS)
	{
		werrlog(ERRORCODE(2),lnk_depth,0,0,0,0,0,0,0);
		return;
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

/* ARG1: PROGRAM	Alpha(8) */
	progname = varg_pointer[va_i++] ;						/* Get the program name.		*/
	to_do--;
	sprintf(tracemsg,"Progname(1)=[%8.8s]",progname);

	memcpy(l_file,progname,SIZEOF_FILE);
	leftjust(l_file,SIZEOF_FILE);
	upper_mem(l_file,SIZEOF_FILE);
	l_file[SIZEOF_FILE] = (char)0;

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
	sprintf(tracebuf," Type(2)=[%c]",*ltype);
	strcat(tracemsg,tracebuf);

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
	sprintf(tracebuf," Lib(3)=[%8.8s]",libname);
	strcat(tracemsg,tracebuf);

/* ARG4:   VOLUME	Alpha(6) */
	volname = varg_pointer[va_i++] ;						/* Get the volume name.			*/
	to_do--;
	sprintf(tracebuf," Vol(4)=[%6.6s]",volname);
	strcat(tracemsg,tracebuf);

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
		sprintf(tracebuf," Special(5)=[%c]",*arg_special);
		strcat(tracemsg,tracebuf);
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
		sprintf(tracebuf," Extopt(6)=[%c]",*arg_ext_opt);
		strcat(tracemsg,tracebuf);
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
	parmcnt = get_swap((int4*)dummy_char);						/* The argument count.			*/

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

	sprintf(tracebuf," Parmcnt(7)=[%d]",parmcnt);
	strcat(tracemsg,tracebuf);

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
/* ARG8:  PARMS Var(var) */
			this_parm = varg_pointer[va_i] ;				/* Get the argument pointer.		*/
			parm_list.parm[i] = this_parm;

			if ( arg_length_known )
			{
				arg_len = varg_length[va_i] ;				/* Get the argument length.		*/
				len_list.len[i] = arg_len;				/* Save length in a table.		*/
				sprintf(tracebuf," Arg%d(8)=[len=%d]",i+1,arg_len);
				strcat(tracemsg,tracebuf);
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
	
/* ARG9:   CANCELEXIT	Alpha(1) */
	can_exit = varg_pointer[va_i++] ;						/* Get the cancel exit flag.		*/
	to_do--;
	sprintf(tracebuf," Canexit(9)=[%c]",*can_exit);
	strcat(tracemsg,tracebuf);
	if (	*can_exit != ' ' && 
	    	*can_exit != 'C' && 
	    	*can_exit != 'N' && 
		*can_exit != 'D' && 
		*can_exit != 'P'   ) 
	{
		can_exit = " ";
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

/* ARG11:  MESS LENGTH	Int(4) */
	messlen = get_swap((int4*)varg_pointer[va_i++]);				/* Get the pf16 message length.		*/
	to_do--;

	tlen=MIN(messlen,20);
	tlen=MAX(0,tlen);
	sprintf(tracebuf," Mess(10)=[%*.*s%s]  Messlen(11)=[%d]", tlen, tlen, mess, (messlen>tlen?"...":""),messlen);
	strcat(tracemsg,tracebuf);

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
	sprintf(tracebuf," Helpflag(12)=[%c]",*helpflg);
	strcat(tracemsg,tracebuf);

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
	sprintf(tracebuf," Reserved(13)=[%2.2s]",pfkey);
	strcat(tracemsg,tracebuf);

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

/* ARG15:  CANCEL RCVR LEN  Int(4) */
	canlen = get_swap((int4*)varg_pointer[va_i++]);					/* Get the text receiver length.	*/
	to_do--;

	tlen=MIN(canlen,20);
	tlen=MAX(0,tlen);
	sprintf(tracebuf," Cantext(14)=[%*.*s%s] Canlen(15)=[%d]",tlen,tlen,cantext,(canlen>tlen?"...":""),canlen);
	strcat(tracemsg,tracebuf);

	if (to_do == 0) goto ARGEND;

ARG16:	/* COMP-CODE	Int(4) */
	comcode = (int4 *)varg_pointer[va_i++] ;					/* Get the completion code from link.	*/
	to_do--;
	strcat(tracemsg," Compcode(16)=[]");

	if (to_do == 0) goto ARGEND;

/* ARG17:  RETURN-CODE	Int(4) */
	retcod = (int4 *)varg_pointer[va_i++] ;						/* Get the program return code.		*/
	strcat(tracemsg," Retcode(17)=[]");

ARGEND:	/* END OF ARGUMENTS */

	wtrace("LINK","ARGS","%s",tracemsg);

	if ( *ltype == ' ' )								/* Use Program vol & lib.		*/
	{
		get_defs(DEFAULTS_PV,l_vol);	/* NOTE: If PV/PL is not set then RV/RL	are returned */
		get_defs(DEFAULTS_PL,l_lib);
	}
	else if ( *ltype == 'P' )							/* Use arg3 & arg4			*/
	{
		memcpy( l_vol, volname, SIZEOF_VOL );
		memcpy( l_lib, libname, SIZEOF_LIB );
	}
	else if ( *ltype == 'S' )							/* look in system dirs			*/
	{
		memset( l_vol, ' ', SIZEOF_VOL );
		memset( l_lib, ' ', SIZEOF_LIB );
	}
	else										/* Invalid link type.			*/
	{
		swap_put ( comcode, (int4)8 ) ;						/* Unsuccessful link.			*/
		swap_put ( retcod, (int4)40 ) ;						/* Invalid Parameter.			*/

		werrlog(ERRORCODE(3),l_file,ltype,0,0,0,0,0,0);
		return;
	}

	leftjust(l_vol,SIZEOF_VOL);
	leftjust(l_lib,SIZEOF_LIB);
	upper_mem(l_vol,SIZEOF_VOL);
	upper_mem(l_lib,SIZEOF_LIB);
	l_vol[SIZEOF_VOL] = '\0';
	l_lib[SIZEOF_LIB] = '\0';

	/*
	**	Vector a "LINK" to a VSSUB into a call
	*/
	if (islinkvector(l_file))
	{
		wtrace("LINK", "VECTOR", "Using linkvector() for FILE=%s", l_file);
		
		if (retval = linkvector(l_file, parmcnt, &parm_list))
		{
			/*
			**	Link failed
			*/
			swap_put ( comcode, (int4)8 );
			swap_put ( retcod, retval );
		}
		else
		{
			/*
			**	All is OK
			*/
			swap_put ( comcode, (int4)0 );
			swap_put ( retcod, (int4)0 );
		}
		return;
	}

	/*
	**	Do internal translations
	*/
	if (0==memcmp(l_file,"COPY    ",SIZEOF_FILE)) 
		memcpy(l_file,"WCOPY   ",SIZEOF_FILE);					/* Translate 	COPY --> WCOPY		*/
	else if (0==memcmp(l_file,"SORT    ",SIZEOF_FILE)) 
		memcpy(l_file,"WSORT   ",SIZEOF_FILE);					/* 		SORT --> WSORT		*/

	unloadpad( progname_str, l_file, SIZEOF_FILE );					/* make progname a string		*/

	if (0==memcmp(l_file,"VSEDIT  ",SIZEOF_FILE))
	{
		if (utils_in_windows())
		{
			link_separate_window = 1;
		}
	}

	if (0==memcmp(l_file,"DISPLAY ",SIZEOF_FILE))
	{
		if (utils_in_windows())
		{
			link_separate_window = 1;
		}

		/*
		**	If using a custom display utility (DISPLAYUTIL option) then substitute the program name.
		*/
		if (custom_display_utility())
		{
			native_substitute = 1;
			strcpy(progname_str,custom_display_utility());
		}
		else if (link_separate_window)
		{
			/*
			**	Using the WISP DISPLAY utility into a separate window (WIN32 only).
			**	Must use the native substitute logic to avoid pitfalls.
			**	- one of the problems is the the GETPARM will be issued then not find DISPLAY.
			*/
			native_substitute = 1;
#ifdef unix
			strcpy(progname_str,"display");
#endif
#ifdef WIN32
			strcpy(progname_str,"display.exe");
#endif
		}
		
		/*
		**	If using a custom display utility then we may need to issue the DISPLAY getparms
		**	because we assume it doesn't issue getparms.
		**	Likewise if using DISPLAY but running in a separate window we need to issue
		**	the DISPLAY getparms because it will not have access to this session's PUTPARMS.
		*/
		if (custom_display_utility() || link_separate_window)
		{
			/*
			**	If no arguments then issue the DISPLAY GETPARMS to get the filename.
			*/
			if (0 == parmcnt)
			{
				int bGotFile;
				
				newlevel();

				bGotFile = display_util_getparms(display_filename);

				oldlevel();						/* Decrement the link-level		*/
				ppunlink(linklevel());					/* Putparm UNLINK			*/
				
				if (bGotFile)
				{
					parmcnt = 1;
					parm_list.parm[0] = display_filename;		/* NOTE: pointer to local variable	*/
					len_list.len[0] = strlen(display_filename);
				}
				else
				{
					/* No filename from getparm - PF16 pressed - bail */

					swap_put( comcode, (int4)0 );
					swap_put( retcod, (int4)16 );

					wtrace("LINK", "RETURN", "FILE=DISPLAY compcode=0 retcode=16 (level=%d)", linklevel());

					return;
				}
			}

		}
		else if (use_internal_display())
		{
			char	save_progid[9];

			/*
			**	Internal soft-link to DISPLAY utility
			*/
		
#ifdef unix
			if (vsharedscreen())						/* If screen is shared			*/
			{
				vwang_stty_save();					/* Save the current stty values		*/
			}
#endif
			strcpy(save_progid, getprogid());
			newlevel();							/* Increment the link-level		*/
			setprogid("DISPLAY ");


			/*
			**	Issues the getparms and if a file was supplied then display it.
			*/
			if (display_util_getparms(display_filename))
			{
				/*
				**	Do the DISPLAY
				*/
				wpushscr();
			
				internal_display(display_filename);

				wpopscr();
			}

			setprogid(save_progid);
			oldlevel();							/* Decrement the link-level		*/
			ppunlink(linklevel());						/* Putparm UNLINK			*/

#ifdef unix
			if (vsharedscreen())						/* If screen is shared			*/
			{
				vwang_stty_restore();					/* Restore the saved stty values	*/
			}
#endif
			swap_put ( comcode, (int4)0 ) ;					/* swap and set the completion code.	*/
			swap_put ( retcod, (int4)0 ) ;					/* swap and set the return code.	*/
			return;
		}
	}


#ifdef VMS
/*************************************************  Start of VMS section  *******************************************************/

	/*
	**	We first try an internal "soft link" via a COBOL call.
	**
	**	NOTE:	This section of code is recursive.  
	**		Wclink() can start a routine which calls LINK.
	*/
	if ( *can_exit != ' ' )	CANEXITFLAG++;
	savelevel = linklevel();						/* Save the link-level.			*/
	newlevel();									/* Increment the link-level		*/
	wclink ( l_file, &parm_list, &retval ) ;					/* Call the COBOL link. (in LINKSUBS.C)	*/
	setlevel(savelevel);								/* Restore the link-level		*/

	if ( *can_exit != ' ' )	CANEXITFLAG--;

	if (retval == 1)								/* Static link was a success.		*/
	{
		retval = atol(WISPRETURNCODE);

/*		setretcode(WISPRETURNCODE); */

		swap_put ( comcode, (int4)0 ) ;						/* Successful link.			*/
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
		char *ptr;
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
				char *ptr;
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
	vms_return = (0==isafile( link_filespec )) ? 1:0;				/* Check file; does it exist?		*/

	if (vms_return != 0 )
	{
		if (*ltype == 'S' && vmstry < 4) goto vmsagain;				/* Try with next specification.		*/
		if (*ltype == ' ' && vmstry < 3) goto vmsagain;				/* Try with next specification.		*/

		/*
		**	FILE NOT FOUND
		*/
		swap_put ( comcode, (int4)8 ) ;						/* Unsuccessful link.			*/
		wang_retcode = 20;							/* File not found			*/

		sprintf(temp_retval,"%03d",wang_retcode);				/* Put wang_retval into string format.	*/
/*		setretcode(temp_retval); */
		
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

		if ( *can_exit != ' ' )	CANEXITFLAG++;

		if ( parmcnt > 0 ) 							/* if there are any parameters;		*/
		{
			writevmslink( link_filespec, parmcnt, &parm_list, &len_list, vmskey);
		}
		else
		{
			strcpy ( vmskey, NULL_FILE_NAME ) ;				/* VMSLINKSUB won't try to read args.	*/
		}
		vwang_noclear_on_shut();						/* don't clear src			*/
		vwang_shut();								/* Reset the terminal.			*/

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
				if ( !isafile(vmskey) )					/* if parameter file does NOT exist;	*/
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
				wexit(32);						/* Terminate this level			*/
			}
		}
		else
		{
			CANEXITFLAG--;							/* We've just come thru a Cancel-Exit   */
			LOGOFFFLAG = 0;							/* Cancel the logoff			*/
		}

		vwang_synch() ;
		vwang_set_reinitialize(TRUE);						/* Return from link re-inits screen	*/

		vwang_clear_on_shut();							/* Reset onexit status			*/

	}
	else
	{
		/*
		**	(ELSE) is not an EXE or not a LINKDESC (/DLINK).
		**
		**	Handles .COM files and .EXE without LINKDESC
		*/

		if ( *can_exit != ' ' )	CANEXITFLAG++;

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

		if ( *can_exit != ' ' )	CANEXITFLAG++;

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
/*	setretcode(temp_retval); */

	swap_put ( comcode, wang_compcode );
	swap_put ( retcod, wang_retcode );

	return;
/*************************************************  End of VMS section  *********************************************************/
#endif /* VMS */

#if defined(unix) || defined(MSDOS) || defined(WIN32)

	/*
	**	ltype   - link type
	**	-------------------
	**	 ' '	- Current location
	**	 'P'	- Passed, used passed args only
	**	 'S'	- SYSTEM, look along path
	*/

	not_found = 1;

	wang_retcode = 0;
	wang_compcode = 0;

	if (native_substitute)
	{
		if ( isafile(progname_str) )
		{
			strcpy(link_filespec, progname_str);
			not_found = 0;
		}

		if (not_found)
		{
			not_found = searchnative(progname_str,link_filespec);
		}
		
#ifdef WIN32
		if (not_found)
		{
			not_found = whichenvpath(progname_str,link_filespec);
		}
#endif
		if (not_found)
		{
			swap_put( comcode, (int4)8 );
			swap_put( retcod, (int4)20 );
			werrlog(ERRORCODE(5),progname_str,0,0,0,0,0,0,0);
			return;
		}
	}

	if ( not_found && (*ltype == ' ' || *ltype == 'P') )
	{
		mode = IS_SUBMIT;
		p = wfname(&mode,l_vol,l_lib,l_file,link_filespec);
		*p = (char)0;

		if ( isafile(link_filespec) )
		{
			not_found = 0;
		}
	}

#if defined(MSDOS) || defined(WIN32)
	/*
	**	Note: On MSDOS the current directory is ALWAYS checked first before the PATH.
	*/
	if (not_found && (*ltype == ' ' || *ltype == 'S' ))
#else
	if (not_found && *ltype == ' ')
#endif
	{
		/*
		**	Check current directory
		*/
		mode = IS_SUBMIT;
		memcpy(l_vol,".     ",SIZEOF_VOL);
		memcpy(l_lib,".       ",SIZEOF_LIB);
		p = wfname(&mode,l_vol,l_lib,l_file,link_filespec);			/* expand the name 			*/
		*p = (char)0;
		if ( isafile(link_filespec) )
		{
			not_found = 0;							/* found it				*/
		}
	}

	if (not_found && *ltype == 'S')
	{
		/*
		**	Scan along the $PATH
		*/
		if (not_found)
		{
			not_found = searchpath(progname_str,link_filespec);
		}

		if (not_found)
		{
			swap_put( comcode, (int4)8 );
			swap_put( retcod, (int4)20 );
			werrlog(ERRORCODE(5),progname_str,0,0,0,0,0,0,0);
			return;
		}
	}

	if (not_found)
	{
		/* 
		   Unsuccessful link
		   File not found.	
		*/
		swap_put ( comcode, (int4)8 ) ;
		swap_put ( retcod, (int4)20 ) ;
		werrlog(ERRORCODE(7),link_filespec,0,0,0,0,0,0,0);
		return;
	}

	/*
	**	We've found the full file spec.  Find out what run type it is.
	*/
#ifdef MSDOS
	upper_string(link_filespec);
#endif

	/*
	**	Get the run type then check if we can handle it.
	*/
	ftyp = runtype(link_filespec);

	switch(ftyp)
	{
	default:
	case RUN_NOT: /* NOT a runable file			*/
		swap_put ( comcode, (int4)8 ) ;
		swap_put ( retcod, (int4)52 ) ;
		werrlog(ERRORCODE(11),link_filespec,0,0,0,0,0,0,0);
		return;

	case RUN_ACCESS: /* Access denied.			*/
	case RUN_UNKNOWN:
		swap_put ( comcode, (int4)8 ) ;
		swap_put ( retcod, (int4)28 ) ;
		werrlog(ERRORCODE(9),link_filespec,0,0,0,0,0,0,0);
		return;

	case RUN_ACUCOBOL:
		cobol_frontend = "ACULINK";
		break;

	case RUN_MFINT:
	case RUN_MFGNT:
		cobol_frontend = "MFLINK";
		break;

	case RUN_EXEC:
	case RUN_SHELL:
		cobol_frontend = NULL;
		break;

#if defined(unix) || defined(WIN32)
	case RUN_PROC:
	case RUN_PROCOBJ:
		cobol_frontend = NULL;
		break;
#endif
	}

	/*
		Set up "link_callspec" this is the value that the link frontend (ACULINK or MFLINK) is to 
		use in the CALL stmt.
	*/
	strcpy(link_callspec,link_filespec);	/* Full path without extension.		*/
	if (RUN_EXEC == ftyp)
	{
		/* (Used by Native Micro Focus)		*/
		/* Routine name only, uppercase		*/
		strcpy(link_callspec,progname_str);
	}


	if ( *can_exit != ' ' )
	{
		/* 
		**	If cancel exit flag is set then increment the counter.
		**	(There can be multiple canexits	in effect; one per link-level.)	
		**
		**	NOTE: This must be done before the parms file is written out. [writeunixlink()]
		*/
		CANEXITFLAG++;
	}

#endif /* unix || MSDOS || WIN32 */



#ifdef unix
/*************************************************  Start of UNIX section  ******************************************************/

	parm_file_written = 0;
	exit_code = 0;
	pid = 0;
	soft_link = 0;
	
	/*
	**	Handle SOFTLINK case first.
	*/
	if (softlink() && 
	    ((RUN_ACUCOBOL == ftyp && acu_cobol) ||
	     ((RUN_MFINT == ftyp || RUN_MFGNT == ftyp) && mf_cobol)))
	{
		/*
		**	Already in a COBOL runtime so do a "soft" link.
		**
		**	NOTE: This code must be re-entrant.
		*/
		int	rc;
		char	saverunname[8];

		soft_link = 1;
		
		/*
		**	Save soft link level stuff.
		**	Must be done before PROGLIB/PROGVOL are changed.
		*/
		prelink_save_state(linklevel());

		/*
		**	Handle PROGLIB/PROGVOL processing
		*/
		setprogdefs(l_vol,l_lib);
		clearprogsymb();
		
		memcpy(saverunname, WISPRUNNAME, 8);
		memcpy(WISPRUNNAME, l_file, 8);
		
		savelevel = linklevel();
		newlevel();

		if (acu_cobol)
		{
			wtrace("LINK","SOFTLINK", "call_acucobol() softlink to [%s] parmcnt=%d", link_filespec, parmcnt);

			call_acucobol( link_filespec, parmcnt, parm_list.parm, len_list.len, &rc );
			call_acucobol_error(rc, &wang_retcode, &wang_compcode, link_filespec);
		}
		else /* MF COBOL */
		{
			wtrace("LINK","SOFTLINK", "call_mfcobol() softlink to [%s] parmcnt=%d", link_filespec, parmcnt);
			
			call_mfcobol( link_filespec, parmcnt, parm_list.parm, len_list.len, &rc );

			/*
			**	The RC is not reliable, it always seems to return 1
			*/
			wang_compcode = 0;
			wang_retcode  = atol ( WISPRETURNCODE );
		}

		setlevel(savelevel);

		memcpy(WISPRUNNAME, saverunname, 8);

		/*
		**	Bypass all the hardlink code and go to the cleanup code.
		*/
		goto done_soft_link;
	}

	if ( arg_length_known || 0 == parmcnt )
	{
		/*
		**	Write out the params file
		*/
		writeunixlink(link_callspec, parmcnt, &parm_list, &len_list, linkkey);
		parm_file_written = 1;
	}

	/*
	**	If not in background then take care of shutting down screen handling.
	*/
	if (!wbackground())
	{
		if (ftyp == RUN_ACUCOBOL || ftyp == RUN_MFINT || ftyp == RUN_MFGNT ||
		    ftyp == RUN_PROC || ftyp == RUN_PROCOBJ )			
		{
			/*
			**	If going to COBOL or PROC don't clear screen
			*/
			vwang_noclear_on_shut();
		} 

		if (vsharedscreen())							/* If screen is shared			*/
		{
			vwang_stty_save();						/* Save the current stty values		*/
		}

		vwang_shut();								/* Reset the terminal.			*/

		if (vsharedscreen())							/* If screen is shared			*/
		{
			/*
			**	Force the stty into a SANE state.
			**	If there has been no screen I/O then vwang_shut() will have done nothing.
			*/
			vwang_stty_sane();
		}
	}
	
	old_SIGINT  = signal(SIGINT,  SIG_IGN);						/* Ignore signals			*/
	old_SIGQUIT = signal(SIGQUIT, SIG_IGN);
	old_SIGCLD  = signal(SIGCLD,  SIG_DFL);						/* Use Default DEATH-OF-CHILD signal	*/


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

			/*
			**	If linking to an exe then assume C style parameters (null terminated strings).
			**	(The parmfile may also have been written.)
			*/
			for (i=0; i < parmcnt; ++i)
			{
				sh_parm[i+1]=parm_list.parm[i];
			}
			sh_parm[i+1] = '\0';						/* End the arg list			*/


			buff[0] = '\0';
			for(i=0; sh_parm[i]; i++) 
			{
				strcat(buff, sh_parm[i]);
				strcat(buff, " ");
			}
			wtrace("LINK", "EXEC", "%s", buff);

			/* If this program wants to play the link-level game it must do the newlevel() call.			*/
			execvp(sh_parm[0],sh_parm);					/* Do the exec.				*/

			werrlog(ERRORCODE(4),link_filespec,errno,0,0,0,0,0,0);
			vwang_shut();							/* Reset the screen from the werrlog	*/
			*retcod=fixerr(errno);						/* xlat unix errno to wang error# 	*/
			break;

		case RUN_SHELL:								/* Assume a shell script		*/
			memset(sh_parm,(char)0,sizeof(sh_parm));

			sh_parm[0]=wispshellexe();					/* argv[0] is the shell (i.e /bin/sh)	*/
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
					sh_parm[i+2]=wmalloc(len_list.len[i] + 1);			/* Malloc space		*/
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

			buff[0] = '\0';
			for(i=0; sh_parm[i]; i++) 
			{
				strcat(buff, sh_parm[i]);
				strcat(buff, " ");
			}
			wtrace("LINK", "SHELL", "%s", buff);

			newlevel();							/* Increment the link-level		*/
			execvp(sh_parm[0],sh_parm);

			werrlog(ERRORCODE(4),sh_parm[0],errno,0,0,0,0,0,0);
			vwang_shut();							/* Reset the screen from the werrlog	*/
			*retcod=fixerr(errno);
			break;

		case RUN_ACUCOBOL: 							/* An acu_cobol object			*/
		case RUN_MFINT:								/* Micro focus .int or .gnt code	*/
		case RUN_MFGNT:
			{
				struct wruncfg cfg;
				char	options[sizeof(cfg.wrun_options)];
				char	*eptr, *optr;
				int	arg;

				wrunconfig(&cfg);					/* Load wrunconfig options file		*/

				strcpy(options, cfg.wrun_options);

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
						sh_parm[arg++] = cobol_frontend;	/* ACU Link interface program		*/
						sh_parm[arg++] = linkkey;		/* Key to parm area			*/
					}
					else						/* Micro Focus GNT or INT file		*/
					{
						sh_parm[arg++] = cobol_frontend;	/* MF Link interface program		*/
					}
				}

				sh_parm[arg++] = link_callspec;				/* Name of program to start		*/
				/* NOTE: This will normally be a comment except if LINK is called from a non-COBOL program.	*/

				sh_parm[arg++] = '\0';					/* null terminate it			*/

				buff[0] = '\0';
				for(arg=0; sh_parm[arg]; arg++) 
				{
					strcat(buff, sh_parm[arg]);
					strcat(buff, " ");
				}
				wtrace("LINK", "COBOL", "%s", buff);

				/* The COBOL program will do a newlevel() call in initwisp()					*/
				execvp(sh_parm[0],sh_parm);				/* Start the new program.		*/

				werrlog(ERRORCODE(14),sh_parm[0],l_file,errno,0,0,0,0,0);
				vwang_shut();						/* Reset the screen from the werrlog	*/
				*retcod=fixerr(errno);
				break;
			}

		case RUN_PROC:
		case RUN_PROCOBJ:
			{
				char	*name_ptr = wprocexe();				/* Get the name of "wproc"		*/
				char	*debug_ptr = wprocflags();
				int	arg;

				arg = 0;
				sh_parm[arg++] = name_ptr;				/* argv[0] is the "wproc" program	*/
				if ( debug_ptr && *debug_ptr )				/* Check for debug flags		*/
				{
					sh_parm[arg++] = debug_ptr;			/* Add the debug flags			*/
				/*	freopen("wproc.trace","w",stdout); */		/* Redirect stdout			*/
				}
				sh_parm[arg++] = "-p";					/* Use parameter file			*/
				sh_parm[arg++] = link_filespec;				/* argv[2] in the filespec	 	*/
				sh_parm[arg++] = '\0';					/* null terminate it			*/

				buff[0] = '\0';
				for(arg=0; sh_parm[arg]; arg++) 
				{
					strcat(buff, sh_parm[arg]);
					strcat(buff, " ");
				}
				wtrace("LINK", "WPROC", "%s", buff);
				
				execvp(sh_parm[0],sh_parm);

				werrlog(ERRORCODE(4),sh_parm[0],errno,0,0,0,0,0,0);
				vwang_shut();						/* Reset the screen from the werrlog	*/
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
		load_defaults();							/* Reload defaults: may have changed	*/
		break;
	}

	signal(SIGINT,  old_SIGINT);							/* Reset the signals after the link	*/
	signal(SIGQUIT, old_SIGQUIT);
	signal(SIGCLD,  old_SIGCLD);							/* Ignore DEATH-OF-CHILD signal		*/

done_soft_link:

	/*
	**	If not in background then take care of resetting screen handling.
	*/
	if (!wbackground())
	{
		vwang_synch();								/* Resynch video			*/
		vwang_set_reinitialize(TRUE);						/* Return from link re-inits screen	*/

		if (vsharedscreen())							/* If screen is shared			*/
		{
			vwang_stty_restore();						/* Restore the saved stty values	*/
		}

		vwang_clear_on_shut();							/* Reset onexit status to clear screen	*/
	}
	
	ppunlink(linklevel());								/* Putparm UNLINK			*/

	if (soft_link)
	{
		/*
		**	Restore soft link level stuff.
		**	Needs to be done before any wexit() because it restores the temp file list.
		*/
		postlink_restore_state(linklevel());
	}

	if ( parm_file_written )							/* If parm file written then clean up	*/
	{
		int4	the_comp_code = 0;
		int4	the_ret_code = 0;

		readunixlink(parmcnt, &parm_list, &len_list, linkkey, &the_comp_code, &the_ret_code);

		if (the_comp_code != -1)
		{
			wang_compcode = the_comp_code;
			wang_retcode = the_ret_code;
		}
		else
		{
			if (cobol_frontend)
			{
				/*
				**	This should only happen if the frontend routine was not found.
				*/
				werrlog(ERRORCODE(22),cobol_frontend,0,0,0,0,0,0,0);
				wang_compcode = 8;
				wang_retcode = 80;
			}
			else
			{
				wang_compcode = 0;
				wang_retcode = exit_code;
			}
		}
	} 
	else if (!soft_link)
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
			wtrace("LINK", "CANEXIT", "A lower link level called LOGOFF (level=%d)", linklevel());
			
			wexit(32);							/* Terminate this level			*/
		}

		gptr = getenv(WISP_CANCELEXIT_ENV);				/* If the cancel-exit var is set and the process*/
		if ( gptr && *gptr && 0!=strcmp(gptr,"0"))			/* it points to is NOT running then we are 	*/
										/* midway thru a LOGOFF with cancel-exit.	*/
										/* (See LOGOFF) so just exit.			*/
		{
			if ( 1 == sscanf(gptr,"%d",&kpid) && kpid != 0 )	/* A kpid==0 means cancelexit was set and has	*/
			{							/* been turned off because we crossed a SUBMIT.	*/
				if ( kill((pid_t)kpid,0) != 0 )			/* Check if cancelexit process is alive.	*/
				{
					LOGOFFFLAG = 1;
					wexit(32);
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

	wtrace("LINK", "RETURN", "FILE=%s compcode=%ld retcode=%ld (level=%d)", 
	       l_file, (long)wang_compcode, (long)wang_retcode, linklevel());
	
	swap_put ( comcode, wang_compcode ) ;						/* swap and set the completion code.	*/
	swap_put ( retcod, wang_retcode ) ;						/* swap and set the return code.	*/

	return;

/*************************************************  End of UNIX section  ********************************************************/
#endif	/* unix */							

#if defined(WIN32)
/******************************************  Start of WIN32 section  *****************************************************/

	/*
	**	RUN_ACUCOBOL
	**	RUN_MFINT/RUN_MFGNT
	**	RUN_EXEC
	**	RUN_SHELL
	**	RUN_PROC/RUN_PROCOBJ
	*/

	parm_file_written = 0;
	exit_code = 0;
	soft_link = 0;

	/*
	**	Save soft link level stuff.
	**	Must be done before PROGLIB/PROGVOL are changed.
	*/
	prelink_save_state(linklevel());

	/*
	**	Handle PROGLIB/PROGVOL processing
	*/
	setprogdefs(l_vol,l_lib);
	clearprogsymb();

	if (RUN_ACUCOBOL == ftyp && acu_cobol && softlink())
	{
		/*
		**	Already in an ACUCOBOL runtime so do a "soft" link using call_acucobol().
		**
		**	NOTE: This code must be re-entrant.
		*/
		int	rc;
		char	saverunname[8];

		soft_link = 1;
		
		memcpy(saverunname, WISPRUNNAME, 8);
		memcpy(WISPRUNNAME, l_file, 8);
		
		savelevel = linklevel();
		newlevel();

		wtrace("LINK","SOFTLINK", "Acucobol softlink to [%s] parmcnt=%d", link_filespec, parmcnt);

		call_acucobol( link_filespec, parmcnt, parm_list.parm, len_list.len, &rc );

		setlevel(savelevel);

		call_acucobol_error(rc, &wang_retcode, &wang_compcode, link_filespec);

		memcpy(WISPRUNNAME, saverunname, 8);
	}
	else if ((RUN_MFINT == ftyp || RUN_MFGNT == ftyp) && mf_cobol && softlink())
	{
		/*
		**	Already in a MicroFocus runtime so do a "soft" link using call_mfcobol().
		*/
	}
	else
	{
		/*
		**	HARD LINKS
		**
		**	This section handles LINK by spawning a new process.
		**	This involves writing parmameters out to the temp parm file
		**	and setting up context via environment variables.
		*/

		if ( arg_length_known || 0 == parmcnt )
		{
			/*
			**	Write out the params file
			*/
			writeunixlink(link_callspec, parmcnt, &parm_list, &len_list, linkkey);

			/*
			**	Store the linkkey in env
			*/
			sprintf(buff,"%s=%s",WISP_LINK_ENV,linkkey);
			win32SetNewEnv(buff);

			parm_file_written = 1;
		}

		if (*can_exit != ' ')
		{
			/* 
			**	Mark the cancel-exit in a shell var so it is available
			** 	even if we go across a script etc.
			*/
			sprintf(buff,"%s=%d",WISP_CANCELEXIT_ENV,getpid());
			win32SetNewEnv(buff);
		}

		savelevel = linklevel();

		if (!wbackground() && !link_separate_window)
		{
			vwang_shut();
		}

		/*
		**	Type specific spawn logic
		*/

		if (RUN_ACUCOBOL == ftyp ||
		    RUN_MFINT    == ftyp || 
		    RUN_MFGNT    == ftyp   )
		{
			/*
			**	Do a hard link to a COBOL program by starting a new RTS
			*/
			struct wruncfg cfg;
			char	options[sizeof(cfg.wrun_options)];
			char	*optr;
			int	arg;

			wrunconfig(&cfg);

			if (RUN_ACUCOBOL==ftyp)
			{
				if (0!=strcmp(cfg.wrun_cobtype,"ACU"))
				{
					werrlog(ERRORCODE(20),"wrunconfig is not configured for ACUCOBOL",0,0,0,0,0,0,0);
					wang_retcode = 8;
					wang_compcode = 52;
					goto done_hard_link;
				}
			}
			else 
			{
				if (0!=strcmp(cfg.wrun_cobtype,"MF"))
				{
					werrlog(ERRORCODE(20),"wrunconfig is not configured for MICROFOCUS",0,0,0,0,0,0,0);
					wang_retcode = 8;
					wang_compcode = 52;
					goto done_hard_link;
				}
			}
		

			/*
			**	Build the parameters for the spawn
			**
			**	arg[0] is the cobol RTS
			*/
			sh_parm[0] = cfg.wrun_runcbl;
			arg=1;
	
			/*
			**	Split the wrun options into tokens each one is added to as a parameter.
			*/
			strcpy(options, cfg.wrun_options);
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
					sh_parm[arg++] = cobol_frontend;	/* ACU Link interface program		*/
					sh_parm[arg++] = linkkey;		/* Key to parm area			*/
				}
				else						/* Micro Focus GNT or INT file		*/
				{
					sh_parm[arg++] = cobol_frontend;	/* MF Link interface program		*/
				}
			}
	
			sh_parm[arg++] = link_callspec;				/* Name of program to start		*/
			/* NOTE: This will normally be a comment except if LINK is called from a non-COBOL program.	*/
	
			sh_parm[arg++] = '\0';					/* null terminate it			*/

		}
		else if (RUN_EXEC == ftyp)
		{
			/*
			**	Spawn an executable file
			*/
			sh_parm[0] = link_filespec;					/* argv[0] is the filespec		*/

			/*
			**	If linking to an exe then assume C style parameters (null terminated strings).
			**	(The parmfile may also have been written.)
			*/
			for (i=0; i < parmcnt; ++i)
			{
				sh_parm[i+1]=parm_list.parm[i];
			}
			sh_parm[i+1] = '\0';						/* End the arg list			*/

		}
		else if (RUN_SHELL == ftyp)
		{
			/*
			**	Spawn a command processor
			*/
			char	command[256];
			int	cpos;
			
			strcpy(command, link_filespec);
			if (parmcnt && arg_length_known)
			{
				cpos = strlen(command);
				
				for(i=0; i<parmcnt; i++)
				{
					if ((cpos + len_list.len[i] + 3) >= 128)
					{
						werrlog(ERRORCODE(20),"Command is greater then 128 bytes",0,0,0,0,0,0,0);

						/*
						**	Set exit_code and errno to force...
						**
						**	wang_compcode = 8;
						**	wang_retcode = 60;  command too big
						*/	
						exit_code = -1;
						errno = E2BIG;
						
						goto done_hard_link;
						
					}
					/*
					**	Put each arg in quotes
					*/
					command[cpos++] = ' ';
					command[cpos++] = '\"';
					memcpy(&command[cpos], parm_list.parm[i], len_list.len[i]);
					cpos += len_list.len[i];
					command[cpos++] = '\"';
					command[cpos] = (char)0;
				}
			}
			else if (parmcnt)
			{
				werrlog(ERRORCODE(20),"Parmameter lengths unknown",0,0,0,0,0,0,0);
			}

			wtrace("LINK", "WSYSTEM", "Command = [%s]", command);
			
			savelevel = linklevel();
			newlevel();

			exit_code = wsystem(command);
			
			setlevel(savelevel);

			goto done_hard_link;
		}
		else if (RUN_PROC == ftyp || RUN_PROCOBJ == ftyp)
		{
			/*
			**	Spawn WPROC
			*/
			char	*name_ptr = wprocexe();				/* Get the name of "wproc"		*/
			char	*debug_ptr = wprocflags();
			int	arg;

			sh_parm[0] = name_ptr;
			arg = 1;
			if ( debug_ptr && *debug_ptr )				/* Check for debug flags		*/
			{
				sh_parm[arg++] = debug_ptr;			/* Add the debug flags			*/
			/*	freopen("wproc.trace","w",stdout);	*/	/* Redirect stdout			*/
			}
			sh_parm[arg++] = "-p";					/* Use parameter file			*/
			sh_parm[arg++] = link_filespec;				/* argv[2] in the filespec	 	*/
			sh_parm[arg++] = '\0';					/* null terminate it			*/
		}
		else
		{
			/*
			**	ERROR - Will never happen
			*/
			goto done_hard_link;
		}


		buff[0] = '\0';
		for(i=0; sh_parm[i]; i++) 
		{
			strcat(buff, sh_parm[i]);
			strcat(buff, " ");
		}
		wtrace("LINK", "HARDLINK", "%s", buff);
	
	
		/*
		**	SPAWN the new process
		*/
		if (wbackground())
		{
			exit_code = win32spawnvp(sh_parm, SPN_WAIT_FOR_CHILD);
		}
		else if (RUN_ACUCOBOL == ftyp ||
			 RUN_MFINT    == ftyp || 
			 RUN_MFGNT    == ftyp   )
		{
			
			/*	
			**  
			**	If NATIVESCREENS and linking to a COBOL program then don't HIDE CHILD
			**	because Acucobol doesn't un-hide it's window.
			**	Don't call nativescreens() because will be false if called from wshell.
			**
			**	if (get_wisp_option("NATIVESCREENS"))
			**		exit_code = win32spawnvp(sh_parm, SPN_HIDE_PARENT|SPN_WAIT_FOR_CHILD);
			*/

			/*
			** 	Specify HIDE PARENT so that the new console will open over the current
			** 	one.  There is no way to get ACUCOBOL to reuse the current console;
			** 	a new one is always created
			** 	HIDE CHILD will start the console hidden so it can be moved.
			**
			** 	exit_code = win32spawnvp(sh_parm, SPN_HIDE_PARENT|SPN_HIDE_CHILD|SPN_WAIT_FOR_CHILD); 
			*/

			/*
			**	Removed the HIDE CHILD because it was causing Acucobol's message boxes
			**	to be hidden and then hang.  Also it wasn't doing any good.
			**	On 95 the console was not being created hidden. (95 ignored it)
			**	On NT the console didn't need to be moved because it gets created in the 
			**	correct location.
			*/

			/*
			 *	If using a console version of the acucobol runtime then don't hide the parent
			 *	because we will inherit the console.
			 */
			if (get_wisp_option("CONSOLEACU"))
			{
				exit_code = win32spawnvp(sh_parm, SPN_WAIT_FOR_CHILD);
			}
			else
			{
				exit_code = win32spawnvp(sh_parm, SPN_HIDE_PARENT|SPN_WAIT_FOR_CHILD);
			}
		}
		else
		{
			/*
			**	Linking to an executable.
			**
			**	Could be WPROC or WSORT or some other exe.
			*/
			
			if (link_separate_window)
			{
				exit_code = win32spawnvp(sh_parm, SPN_STANDALONE_CHILD);
			}
			else
			{
				/*
				**	We do NOT hide the parent because if child is a console app
				**	then it will inherit the same console (hide parent would 
				**	also hide the child since they are the same).
				**
				**	In the future should test what child is and if a Windows app
				**	then we should hide the parent or do standalone.
				*/
				exit_code = win32spawnvp(sh_parm, SPN_WAIT_FOR_CHILD);
			}
		}			
		save_errno = errno;
		/*
		**	Post-Processing following return from the spawn.
		*/
	done_hard_link:
		
		setlevel(savelevel);

		if (!wbackground() && !link_separate_window)
		{
			vwang_synch();
			vwang_set_reinitialize(TRUE);
		}

		if (-1 == exit_code)
		{
			/*
			**	Spawn failed so set the compcode and retcode
			*/
			werrlog(ERRORCODE(14),sh_parm[0],l_file,errno,0,0,0,0,0);
			wang_compcode = 8;
			switch(save_errno)
			{
			case E2BIG:	wang_retcode = 60; break;
			case EINVAL:	wang_retcode = 99; break;
			case ENOENT:	wang_retcode = 20; break;
			case ENOEXEC:	wang_retcode = 52; break;
			case ENOMEM:	wang_retcode = 50; break;
			default:	wang_retcode = 99; break;
			}
		}
		else
		{
			if (!parm_file_written)
			{
				wang_compcode = 0;
				wang_retcode = exit_code;
			}
		}
		
	}
	

	/*
	**	Do a putparm UNLINK
	*/
	ppunlink(linklevel());

	/*
	**	Post-processing of parm file
	**	Get the compcode and retcode.
	*/
	if ( parm_file_written )
	{
		int4	the_comp_code = 0;
		int4	the_ret_code = 0;

		readunixlink(parmcnt, &parm_list, &len_list, linkkey, &the_comp_code, &the_ret_code);

		if (the_comp_code != -1)
		{
			wang_compcode = the_comp_code;
			wang_retcode = the_ret_code;
		}
		else
		{
			/*
			**	The parm file was unchanged so either the link failed or
			**	the linked to program didn't understand the parm file protocol.
			**
			**	If exit_code == -1 then we've already set the codes
			*/
			if (exit_code != -1)
			{
				if (cobol_frontend)
				{
					/*
					**	This should only happen if the frontend routine was not found.
					*/
					werrlog(ERRORCODE(22),cobol_frontend,0,0,0,0,0,0,0);
					wang_compcode = 8;
					wang_retcode = 80;
				}
				else
				{
					wang_compcode = 0;
					wang_retcode = exit_code;
				}
			}
			
		}
	} 

	/* Reload defaults: may have changed	*/
	load_defaults(); 

	/*
	**	Restore soft link level stuff.
	**	Needs to be done before any wexit() because it restores the temp file list.
	*/
	postlink_restore_state(linklevel());

	/*
	**	Post-prcessing for CANCEL EXIT
	*/
	if ( *can_exit == ' ' )
	{
		/*
		**	Cancel-Exit is not at this level.
		**	If a lower level called LOGOFF() then exit
		*/
		if ( LOGOFFFLAG )
		{
			wtrace("LINK", "CANEXIT", "A lower link level called LOGOFF (level=%d)", linklevel());
			wexit(32);
		}
	}
	else
	{
		/*
		**	This level has a cancel-exit so decrement the counter
		**	and cancel any lower level LOGOFF()
		*/
		CANEXITFLAG--;
		LOGOFFFLAG = 0;
	}

	/*
	**	Restore vwang
	*/
	if (!wbackground() && !link_separate_window)
	{
		vwang_synch();
		vwang_set_reinitialize(TRUE);
	}

	wtrace("LINK", "RETURN", "FILE=%s compcode=%ld retcode=%ld (level=%d)", 
	       l_file, (long)wang_compcode, (long)wang_retcode, linklevel());
       
	/*
	**	Set the return and comp codes
	*/
	swap_put ( comcode, wang_compcode ) ;
	swap_put ( retcod, wang_retcode );
	return;

/******************************************  End of WIN32 section  *******************************************************/
#endif	/* WIN32 */

#if defined(MSDOS)
/******************************************  Start of MSDOS section  *****************************************************/


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
					swap_put ( comcode, (int4)8 ) ;		/* Unsuccessful link				*/
					swap_put ( retcod, (int4)60 ) ;		/* Command too big				*/
					werrlog(ERRORCODE(20),"Command is greater then 128 bytes",0,0,0,0,0,0,0);
					if ( *can_exit != ' ' )
					{
						CANEXITFLAG--;
					}
					return;
				}
				command[cpos++] = ' ';				/* Insert a space.			*/
				command[cpos++] = '\"';				/* And Quotes.				*/
				memcpy(&command[cpos], parm_list.parm[i], (size_t)len_list.len[i]);
				cpos += len_list.len[i];
				command[cpos++] = '\"';				/* Close Quotes.			*/
				command[cpos] = (char)0;			/* NULL terminate command		*/
			}
		}
		
		vwang_shut();							/* Clear video for a system call.		*/
		rc = system( command );						/* Execute with a command processor.		*/

		setlevel(savelevel);						/* Restore the link-level			*/
		ppunlink(savelevel);						/* Putparm UNLINK				*/

		vwang_synch();							/* Synchronize video after a system call.	*/
		load_defaults();
		vwang_set_reinitialize(TRUE);					/* Return from link re-inits screen		*/

		if (rc == -1)
		{
			swap_put ( comcode, (int4)8 ) ;
			swap_put ( retcod,  (int4)0 );
		}
		else
		{
			swap_put ( comcode, (int4)0 ) ;
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

			swap_put ( comcode, (int4)0 ) ;
			swap_put ( retcod, wang_retcode );
		}
		else
		{
			swap_put( comcode, (int4)8 );				/* Unsuccessful link.				*/
			swap_put( retcod, (int4)20 );				/* File not found.				*/

			werrlog( ERRORCODE(7), link_filespec,0,0,0,0,0,0,0);
		}
	}
#endif /* DMF */
#if defined(DACU)
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

			if (0==rc)
			{
				/*
				**	If successful then do a putparm UNLINK
				*/
				ppunlink(linklevel());
			}
			
			call_acucobol_error(rc, &wang_retcode, &wang_compcode, link_filespec)

			swap_put ( comcode, wang_compcode ) ;
			swap_put ( retcod, wang_retcode );

		}
		else	/* Not in runtime */
		{
			/*
			**	Not in an ACUCOBOL runtime so must start one to LINK to a COBOL program.
			*/
			struct wruncfg cfg;
			char	command[1024];	/* This exceeds the MSDOS command line limit */
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
				**	Normally we would do a vwang_shut() next except on DOS this causes a lowlevel
				**	clear screen in vrawexit().  On DOS this vrawexit() changes the blue background
				**	to black which is exactly what we are trying to avoid with the vwang_noclear_on_shut()
				**	calls.  On DOS we don't actually change the input-mode so we can live without
				**	a vwang_shut() to restore it. So we will just drop this logic until we can figure
				**	out what is the proper behaviour and fix it.
				*/
				/* vwang_noclear_on_shut(); */			/* don't clear sreen.				*/
				/* vwang_shut(); */				/* Shutdown video				*/

				savelevel = linklevel();			/* Save the link-level				*/

				rc = system(command);				/* Execute the command				*/

				setlevel(savelevel);				/* Restore the link-level			*/

				vwang_synch();					/* Resynch video				*/
				vwang_set_reinitialize(TRUE);			/* Return from link re-inits screen		*/
				/* vwang_clear_on_shut(); */			/* Reset onexit status to clear screen		*/

				if (rc == 0)					/* If system(command) successful		*/
				{
					ppunlink(savelevel);			/* Putparm UNLINK				*/
					swap_put ( comcode, (int4)0 ) ;		/* Successful link				*/
					swap_put ( retcod, (int4)0 ) ;
					load_defaults();			/* Reload defaults: may have changed		*/
				}
				else						/* system(command) failed			*/
				{
					sprintf(buff,"system(command) failed [errno=%d]",errno);
					werrlog(ERRORCODE(20),buff,0,0,0,0,0,0,0);
					werrlog(102,command,0,0,0,0,0,0,0);

					swap_put ( comcode, (int4)8 ) ;		/* Unsuccessful link				*/
					swap_put ( retcod, (int4)60 ) ;
				}
			}

		}
	}
#endif /* DACU */
	else									/* Invalid type					*/
	{
		swap_put ( comcode, (int4)8 ) ;					/* Unsuccessful link				*/
		swap_put ( retcod, (int4)52 ) ;					/* NOT a runable file				*/
		werrlog(ERRORCODE(11),link_filespec,0,0,0,0,0,0,0);
	}

	if ( *can_exit != ' ' )
	{
		CANEXITFLAG--;
	}

	return;									/* Exit LINK now.				*/

/******************************************  End of MSDOS section  *******************************************************/
#endif	/* MSDOS */

}

#if defined(unix) || defined(MSDOS) || defined(WIN32)

void call_acucobol_error(int rc, int4 *wang_retcode, int4 *wang_compcode, char *link_filespec)
{
	char	buff[256];
	
	switch(rc)
	{
	case 0:	/* SUCCESS */
		*wang_compcode = 0;
		*wang_retcode  = atol ( WISPRETURNCODE );
		break;

	case 1:	/* Program file missing or inaccessible */
		*wang_compcode = 8;
		*wang_retcode  = 20;	/* File not found.			*/
		werrlog(ERRORCODE(7),link_filespec,0,0,0,0,0,0,0);
		break;

	case 2:	/* Not a COBOL program */
	case 3:	/* Corrupted program file */
	case 5:	/* Unsupported object code version number */
		*wang_compcode = 8;
		*wang_retcode  = 52;	/* NOT a runable file			*/
		werrlog(ERRORCODE(11),link_filespec,0,0,0,0,0,0,0);
		break;

	case 4: /* Not enough memory to load file */
		*wang_compcode = 8;
		*wang_retcode  = 60;	/* NOT enough memory			*/
		werrlog(ERRORCODE(13),link_filespec,0,0,0,0,0,0,0);
		break;
	
	case 6: /* Recursive CALL of a program */
		*wang_compcode = 8;
		*wang_retcode  = 50;	/* Recursive call			*/
		werrlog(ERRORCODE(16),link_filespec,0,0,0,0,0,0,0);
		break;

	default:
		*wang_compcode = 8;
		*wang_retcode  = 99;
		sprintf(buff,"call_acucobol() failed on file [%s] with [%d]",link_filespec,rc);
		werrlog(ERRORCODE(20),buff,0,0,0,0,0,0,0);
		break;
	}
}
#endif

#if defined(unix) || defined(MSFS)
/*
**	Routine:	searchpath()
**
**	Function:	To search the path for a file to run/exec.
**
**	Description:	Search the path for the file to run/exec.  Try upper/lower case names
**			and try different extensions.
**
**	Arguments:	
**		wangfilename	(I)	The filename to look for.
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

static int searchpath(const char* wangfilename, char* filespec)
{
	char	testname[80];
	char	upname[80];
	int	pathcnt;
	int	not_found;
#ifdef unix
	char	lowname[80];
#endif
	
#ifdef unix
	strcpy(lowname,wangfilename);
	lower_string(lowname);
#endif
	strcpy(upname,wangfilename);
	upper_string(upname);

	not_found = 1;
	for(pathcnt=1; not_found && link_path_seg(pathcnt); pathcnt++)
	{
		/*
		**	Build test filepath (uppercase)
		**	We check uppercase first because ACUCOBOL & MF objects are usually uppercase.
		**
		**	NOTE:   We check all the extensions against the uppercase name before we check any against
		**		the lowercase name.
		*/
		if (fexists(link_path_seg(pathcnt)))
		{
			buildfilepath( testname, link_path_seg(pathcnt), upname);
			not_found = findexts(testname,filespec);

#ifdef unix
			if (not_found)
			{
				/*
				**	build test filepath (lowercase)
				*/
				buildfilepath( testname, link_path_seg(pathcnt), lowname);
				not_found = findexts(testname,filespec);
			}
#endif /* unix */
		}
	}

	if (not_found)
	{
		wtrace("SEARCHPATH", "RETURN", "FILE=%s NOT FOUND ON SEARCH PATH", wangfilename);
	}
	else
	{
		wtrace("SEARCHPATH", "RETURN", "FILE=%s PATH=%s", wangfilename, filespec);
	}
	
	return(not_found);
}

/*
**	Search the path for a native style file name (e.g. "Notepad.exe").
*/
static int searchnative(const char* nativename, char* filespec)
{
	int	pathcnt;

	for(pathcnt=1; link_path_seg(pathcnt); pathcnt++)
	{
		if (fexists(link_path_seg(pathcnt)))
		{
			char	testname[256];

			buildfilepath( testname, link_path_seg(pathcnt), nativename);

			if (fexists(testname))
			{
				strcpy(filespec,testname);

				wtrace("SEARCHNATIVE", "RETURN", "FILE=%s PATH=%s", nativename, filespec);
				return 0;
			}
			
		}
	}

	wtrace("SEARCHNATIVE", "RETURN", "FILE=%s NOT FOUND ON SEARCH PATH", nativename);
	
	return 1;
}
#endif /* unix || MSFS */

#if defined(unix) || defined(WIN32)
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

int findrun(char file[SIZEOF_FILE],char lib[SIZEOF_LIB], char vol[SIZEOF_VOL], char* native, char linktype[1])
{
	int4	mode;
	char	l_file[SIZEOF_FILE+1], l_lib[SIZEOF_FILE+1], l_vol[SIZEOF_VOL+1], l_filename[SIZEOF_FILE+1];
	char	*ptr;

	loadpad(l_file,file,SIZEOF_FILE); 	l_file[SIZEOF_FILE] = (char)0;
	loadpad(l_lib, lib, SIZEOF_LIB); 	l_lib[SIZEOF_LIB]  = (char)0;
	loadpad(l_vol, vol, SIZEOF_VOL); 	l_vol[SIZEOF_VOL]  = (char)0;

	if (islinkvector(l_file))
	{
		unloadpad(native,file,SIZEOF_FILE);
		linktype[0] = 'S';
		return(0);
	}

	/*
	**	Translate:
	**		SORT	->	WSORT
	**		COPY	->	WCOPY
	*/
	if (0==memcmp(l_file,"SORT    ",SIZEOF_FILE))
	{
		memcpy(l_file,"WSORT   ",SIZEOF_FILE);
	}
	else if (0==memcmp(l_file,"COPY    ",SIZEOF_FILE))
	{
		memcpy(l_file,"WCOPY   ",SIZEOF_FILE);
	}

	unloadpad(l_filename,l_file,SIZEOF_FILE);

	if (0 != memcmp(l_lib,"        ",SIZEOF_LIB) || 
	    0 != memcmp(l_vol,"      ",SIZEOF_VOL))					/* If lib or vol supplied - use em	*/
	{
		linktype[0] = 'P';
		mode = IS_SUBMIT;
		ptr = wfname(&mode,l_vol,l_lib,l_file,native);				/* expand the name 			*/
		*ptr = (char)0;
		if (isafile(native))							/* Check if found			*/
		{
			return(0);							/* FOUND				*/
		}

		if (0==memcmp(l_lib,"@SYSTEM@",SIZEOF_LIB))
		{
			/*
			**	If the library is @SYSTEM@ then try the $PATH
			*/
			linktype[0] = 'S';						/* Try searching the $PATH		*/
			return(searchpath(l_filename,native));				/* Return if it was found or not	*/
		}
		return(1);								/* NOT FOUND				*/
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
	if (isafile(native)) return(0);							/* If found then return			*/

	memcpy(l_vol,".     ",SIZEOF_VOL);						/* Try the current dir			*/
	memcpy(l_lib,".       ",SIZEOF_LIB);
	mode = IS_SUBMIT;
	ptr = wfname(&mode,l_vol,l_lib,l_file,native);
	*ptr = (char)0;
	if (isafile(native)) return(0);							/* If found then return			*/

	linktype[0] = 'S';								/* Try searching the $PATH		*/
	return(searchpath(l_filename,native));						/* Return if it was found or not	*/
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
int firstproc(char* filepath)
{
	/*
	**	NOT YET IMPLEMENTED
	*/
	return(0);
}

#endif /* unix || WIN32 */


/*	End of	link.c	*/

/*
**	History:
**	$Log: link.c,v $
**	Revision 1.58  1999-03-04 09:41:46-05  gsl
**	fix warning
**
**	Revision 1.57  1999-02-24 20:29:52-05  gsl
**	For WIN32 add CONSOLEACU option support for using a Acucobol console
**	application runtime system.  This is for NT telnet support.
**
**	Revision 1.56  1998-12-14 13:40:25-05  gsl
**	Add the SOFTLINK code to UNIX for both ACU & MF
**
**	Revision 1.55  1998-12-09 15:01:38-05  gsl
**	Fix the WIN32 logic for win98 to use win32SetNewEnv() instead of setenvstr()
**	for the WISP_LINK_ENV and WISP_CANCELEXIT_ENV.
**	Removed the cancelexit cleanup env logic since it is no longer needed.
**
**	Revision 1.54  1998-11-02 11:20:52-05  gsl
**	Remove the HIDE CHILD flag on WIN32 for Acucobol
**
**	Revision 1.53  1998-11-02 10:22:35-05  gsl
**	Change WIN32 BAT file processing to use wsystem().
**	This was failing when costar was used.
**
**	Revision 1.52  1998-10-29 11:02:02-05  gsl
**	Fixed the SOFTLINK changes. Only applies to NT.
**
**	Revision 1.51  1998-10-22 14:10:01-04  gsl
**	For softlink levels store the g_temp_file_list and g_print_file_list
**	in the softlink stack. Then restore upon return.
**	Fix TRK#763 where linklevel were incorrectly sharing the temp file list.
**	,
**
**	Revision 1.50  1998-09-08 14:56:39-04  gsl
**	Link to WPROC with debugging nolonger redirects stdout to wproc.trace.
**	This is now handled in WPROC, it writes debugging output to the
**	file wprocdebug.log instead of to stdout.
**
**	Revision 1.49  1998-08-03 16:50:27-04  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks.
**
**	Revision 1.48  1998-07-20 16:15:20-04  gsl
**	Fix WIN32 link to DISPLAY.EXE in a separate window
**
**	Revision 1.47  1998-07-08 17:49:53-04  gsl
**	Split searchpath into two, searchpath() and searchnative()
**
**	Revision 1.46  1998-06-30 18:27:42-04  gsl
**	Fix problem on WIN32 when using UTILSWINDOWS with the WISP DISPLAY utility.
**	The child DISPLAY process does not have access to the parents PUTPARMS,
**	need to use the custom_display_utility() GETPARM logic for DISPLAY.
**
**	Revision 1.45  1998-05-28 09:14:24-04  gsl
**	Fix the args tracing logic to correctly get the messlen and the canlen
**
**	Revision 1.44  1998-05-27 11:46:22-04  gsl
**	Enhanced trace logic to show args
**
**	Revision 1.43  1998-05-05 17:34:46-04  gsl
**	WIN32 don't hide child if native screens
**	On softlink save and restore the WISPRUNNAME
**
**	Revision 1.42  1998-05-05 13:32:25-04  gsl
**	Change the processing of DISPLAY and VSEDIT to support UTILSWINDOWS option
**	and to support DISPLAYUTIL option. Most changes affect WIN32 only
**
**	Revision 1.41  1998-04-13 13:17:08-04  gsl
**	Only do the screen handler shutdown and reset if not in background
**
**	Revision 1.40  1998-01-22 10:20:02-05  gsl
**	for WIN32 if in background don't use the SPN_HIDE_PARENT flag on the spawn.
**
**	Revision 1.39  1997-12-04 18:10:55-05  gsl
**	change osd_path() to link_path_seg()
**
**	Revision 1.38  1997-11-21 16:35:48-05  gsl
**	Fixed the vsharedscreen() logic to work from HELP
**
**	Revision 1.37  1997-10-23 16:11:56-04  gsl
**	Fix how a link to DISPLAY is handled.
**	An soft-link to DISPLAY is only done if use_internal_display() is true.
**	A link to an exe (the DISPLAY utility) now always passes command line
**	arguments even if a parmfile was created.
**
**	Revision 1.36  1997-10-20 11:04:09-04  gsl
**	Change to use get_swap()
**
**	Revision 1.35  1997-09-24 11:43:07-04  gsl
**	Make call_acucobol_error() external
**
**	Revision 1.34  1997-09-22 12:32:26-04  gsl
**	Change isdebug() to the more general vsharedscreen()
**
**	Revision 1.33  1997-06-09 11:18:50-04  gsl
**	Fix memory overwrite by can_exit variable
**
**	Revision 1.32  1997-05-13 10:00:27-04  gsl
**	Increase the temp buffer size,
**	Fix the upper_mem() on the lib (was on vol twice)
**	Change all the hardcoded sizes (6,8,8) to defines for SIZEOF_LIB etc
**
**	Revision 1.31  1997-05-02 22:03:01-04  gsl
**	Removed _flushall() for NT
**	It was emptying the input stream for WPROC
**
**	Revision 1.30  1997-05-01 22:49:02-04  gsl
**	Fix problem linking to WPROC on NT introduced in revision 1.24
**	Arg 0 must be the WPROC exe path. On NT the path to WPROC is always
**	fully qualified.
**	Fix processing of wprocflags() when an empty string is returned.
**
**	Revision 1.29  1997-05-01 16:37:59-04  gsl
**	Fix buff error for NT
**
**	Revision 1.28  1997-04-15 23:08:48-04  gsl
**	Update to use wtrace()
**
**	Revision 1.27  1997-04-03 17:02:22-05  gsl
**	Add trace info messages on result of LINK
**
**	Revision 1.26  1997-02-25 09:52:38-05  gsl
**	Correct options size
**
**	Revision 1.25  1996-12-11 16:27:13-05  gsl
**	Change calls to fexists() into isafile() because in these cases
**	a directory would not be valid
**
**	Revision 1.24  1996-12-09 11:03:02-08  jockc
**	win32: fixed up code for calling wproc.. passing prog name
**	NULL so that createprocess will use the $PATH.. including win32spn.h
**	for proto for win32 spawn routines.. updated win32spawn calling
**	syntax
**
**	Revision 1.23  1996-10-08 17:20:51-07  gsl
**	Replace getenv() with wprocexe() and wprocflags()
**	replace shell_val() with wispshellexe()
**
**	Revision 1.22  1996-09-16 16:00:55-07  jockc
**	change win32 call to spawnvp to call win32spawn (win32spn.c)
**
**	Revision 1.21  1996-09-04 17:18:31-07  gsl
**	Move the softlink stuff to wperson.c
**
**	Revision 1.20  1996-09-03 14:41:18-07  gsl
**	Set the default to NOT use soft links (Only applies to NT)
**	Add USESOFTLINK() and USEHARDLINK() to allow control of soft linking
**
**	Revision 1.19  1996-08-30 18:45:45-07  gsl
**	Add the NT code for linking to a BAT file
**
**	Revision 1.18  1996-08-29 17:09:44-07  gsl
**	For NT added the link logic for WPROC and EXEs.
**	Corrected the link-level logic through-out.
**
**	Revision 1.17  1996-08-28 19:37:32-07  gsl
**	Fix restore of cancel-exit for NT if not previously set
**
**	Revision 1.16  1996-08-28 17:57:13-07  gsl
**	Add hard link to a COBOL program for NT
**
**	Revision 1.15  1996-08-27 17:28:08-07  gsl
**	Correct the CANCEL EXIT processing  - VMS and DOS were not setting
**	the CANEXITFLAG. unix was setting the flag after the parms file was
**	already written so it was never properly passed along.
**	Combined common code from unix MSDOS and NT together.
**	Started writting the NT specific linking code *** NOT FINISHED ***
**
**	Revision 1.14  1996-07-17 14:50:51-07  gsl
**	change to use wmalloc()
**
**	Revision 1.13  1996-07-17 09:18:46-07  gsl
**	Make some changes for NT
**
**	Revision 1.12  1996-07-15 10:05:46-07  gsl
**	Fix use of include filext.h
**
**	Revision 1.11  1996-06-28 16:41:49-07  gsl
**	fix prototypes and includes.
**	This now compiles cleanly on NT the NT specific logic has not yet
**	been written.
**
**	Revision 1.10  1995-04-25 02:52:59-07  gsl
**	drcs state V3_3_15
**
 * Revision 1.9  1995/04/17  11:46:19  gsl
 * drcs state V3_3_14
 *
 * Revision 1.8  1995/03/10  14:06:52  gsl
 * fix headers
 *
 * Revision 1.7  1995/03/09  15:39:01  gsl
 * changed all the video calls into vwang calls
 *
**
**
*/

