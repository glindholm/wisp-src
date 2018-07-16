			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*
**	wprint.c
*/

#include <stdio.h>

#ifdef VMS
#include <ssdef.h>
#include <rmsdef.h>
#include <libdef.h>
#endif

#ifdef MSDOS
#include <io.h>
#include <process.h>
#include <stdlib.h>
#include <errno.h>
#endif

#include "que_jobs.h"
#include "wperson.h"
#include "werrlog.h"
#include "wglobals.h"

wprint(native_fname,mode,disposition,copies,prt_class,prt_form,return_code)
char 	*native_fname;							/* The native filepath to print.			*/
char	mode;								/* The printmode H, S, K, P, O.				*/
char	*disposition;							/* The disposition DS, DX, RS (NULL if not used)	*/
									/*  if not NULL then only valid modes are H and S.	*/
char	prt_class;							/* The print class.					*/
int	copies;								/* Number of copies.					*/
int	prt_form;							/* The form number.					*/
int	*return_code;							/* The return code.					*/
{
#define		ROUTINE		83500
	char 	jname[8],qname[132];
	int 	qflags;
	prt_id 	*prt_ptr;								/* Pointer to the printer list.		*/
	int	deleteit;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	if (mode == 'K')
	{
		*return_code = 0;							/* Don't print if KEEP mode.		*/
		return(0);
	}

	deleteit = 0;
	if ( !disposition && mode == 'S')                   deleteit = 1;		/* If Spool a disp is NULL then delete	*/
	if (  disposition && 0==memcmp(disposition,"DX",2)) deleteit = 1;		/* Disp is DX then delete.		*/

#ifdef VMS
	prt_ptr = prt_list;								/* Get a pointer to the printer list	*/

	do
	{
		if ((prt_ptr->class == prt_class) && (prt_ptr->prtnum == defaults.prt_num))
			break;								/* look for the printer that matches	*/
		prt_ptr = (prt_id *) prt_ptr->next;					/* next one				*/
	} while (prt_ptr);

	if (prt_ptr)
	{
		strcpy(qname,prt_ptr->qname);						/* Send the printout to the right queue.*/
	}
	else
	{
		qname[0] = '\0';							/* Or send it nowhere.			*/
	}

	qflags = 0;									/* Set up the flags.			*/

	if (mode == 'H') qflags |= Q_HOLD_JOB;						/* Hold the job if requested.		*/

	if (deleteit)    qflags |= Q_DELETE_FILE;					/* Delete the file as requested.	*/

	jname[0] = '\0';								/* Send to the queue			*/
	*return_code = que_job(PRINT_QUEUE,qname,native_fname,jname,copies,prt_form,qflags);

	if (*return_code == SS$_NORMAL) *return_code = 0;				/* Set to expected Wang Code.		*/
	else
	{		
		int wang_retcod;							/* Set return code from queued job:	*/
		long rc;

		rc = *return_code;
		wang_retcod = 56;							/* Default return code.			*/
		if (rc == RMS$_DNR || rc == RMS$_DPE) wang_retcod = 4;			/* Device not ready or			*/
											/* Device positioning error.		*/
		else if (rc == RMS$_WLK) wang_retcod = 8;				/* Device currently write locked when	*/
											/* write access was attempted.		*/
		else if (rc == RMS$_FLK) wang_retcod = 12;				/* File currently locked by another user.*/
		else if (rc == RMS$_DNF || rc == RMS$_DEV) wang_retcod = 16;		/* Directory not found.			*/
		else if (rc == RMS$_FNF) wang_retcod = 20; 				/* File not found.			*/
		else if (rc == RMS$_PRV) wang_retcod = 28;				/* Insufficient priveledge or file prot.*/
		else if (rc == LIB$_INVARG) wang_retcod = 40;				/* Invalid arguments.			*/
		else if (rc == RMS$_SPL) wang_retcod = 44;				/* Submit command file option to a 	*/
											/* Close service failed.		*/
		else if (rc == SS$_ACCVIO) wang_retcod = 52; 				/* Access violation.			*/
		 *return_code = wang_retcod;
	}
#endif	/* VMS */

#ifdef unix
	if (opt_idsiprint)
	{
		*return_code = idsi_print( native_fname, copies, prt_form, prt_class, disposition, mode);
	}
	else
	{
		*return_code = lp_print( native_fname, copies, prt_form, prt_class, deleteit, mode);
	}
#endif

#ifdef MSDOS
	*return_code = lp_print( native_fname, copies, prt_form, prt_class, deleteit, mode);
#endif
}

#ifdef unix
static int idsi_print( file, copies, formnum, lpclass,  disposition, mode )
char	*file;
int	copies;
int	formnum;
char	lpclass;
char	*disposition;
char	mode;
{
	char	l_mode, l_disp, l_form[10];
	int	rc;

	wpload();

	switch(mode)
	{
	case 'H':
		l_mode = 'H';								/* SPOOL with HOLD			*/
		l_disp = 'D';
		break;
	case 'K':
		return(0);
		break;
	case 'S':
		l_mode = 'S';
		l_disp = 'D';
		break;
	case 'P':									/* SPOOL it.				*/
		l_mode = 'S';
		l_disp = 'S';
		break;
	default:
		return(99);
		break;
	}

	if (disposition)
	{
		if      (0==memcmp(disposition,"DX",2)) l_disp = 'D';		/* Delete after printing.			*/
		else if (0==memcmp(disposition,"RS",2)) l_disp = 'R';		/* Respool after printing.			*/
		else if (0==memcmp(disposition,"DS",2)) l_disp = 'S';		/* Save after printing.				*/
		else return(99);
	}

	sprintf(l_form,"%03d",formnum);

	ilpwisp( file, l_mode, l_disp, copies, lpclass, l_form, defaults.prt_num, &rc );

	return(rc);
}

static int lp_print( file, copies, formnum, lpclass,  delete_after, mode )
char	*file;
int	copies;
int	formnum;
char	lpclass;
int	delete_after;
char	mode;
{
#undef		ROUTINE
#define		ROUTINE		46500
	char	cmd[256];
	int	rc, size, zerofile;
	char	xcopies[20];
	char	suppress[10];
                                                
	werrlog(ERRORCODE(1),file,copies,formnum,delete_after,0,0,0,0);

	switch(mode)
	{
	case 'H':
	case 'K':
		return(0);								/* Don't print if HOLD mode.		*/
		break;
	case 'S':
	case 'P':									/* SPOOL it.				*/
		break;
	default:
		return(99);
		break;
	}

	if ( access( file, 00 ) )							/* Does file exist?			*/
	{
		return( 20 );
	}
	
	if ( eaccess( file, 04 ) )							/* Can we read the file?		*/
	{
		return( 28 );
	}


	wpload();

	rc  = 0;
	zerofile = 0;

	size = filesize(file);
	if ( size == 0 )
	{
		werrlog(ERRORCODE(5),file,0,0,0,0,0,0,0);
		zerofile = 1;
	}                     

	if ( ! zerofile  && copies > 0  )
	{
		if ( copies > 1 )
		{
			sprintf(xcopies,"-n%d",copies);
		}
		else
		{
			xcopies[0] = '\0';
		}

		strcpy(suppress,"-s");

#ifdef ultrix
		suppress[0] = '\0';
#endif

		werrpath();								/* Init werrlog_path			*/

		sprintf( cmd, "lp -c %s %s %s %s %s %s>>%s 2>&1", 
				suppress, 
				wforms(formnum), 
				wlpclass(lpclass), 
				getprmap(defaults.prt_num),
				xcopies, 
				file, 
				werrlog_path );

		rc = wsystem( cmd );
		if ( rc != 0 )
		{
			werrlog(ERRORCODE(2),file,rc,errno,0,0,0,0,0); 
			werrlog(102,cmd,0,0,0,0,0,0,0);
			return( 40 );
		}
	}

	if ( delete_after )								/* If 'S' then delete the printed file.	*/
	{
		rc = unlink(file);
		if ( rc < 0 )
		{
			werrlog(ERRORCODE(3),file,errno,0,0,0,0,0,0);
			return( 28 );
		}
	}

	if ( zerofile ) rc = 24;

	return( rc );
}
#endif	/* unix */

#ifdef MSDOS

static int lp_print( file, copies, formnum, lpclass,  delete_after, mode )
char	*file;
int	copies;
int	formnum;
char	lpclass;
int	delete_after;
char	mode;
{
#undef		ROUTINE
#define		ROUTINE		46500
	static	int	first_print=1;
	static	char	prt_cmd[16];
	char	*prt_env;
	char	cmd[256];
	int	rc, size, zerofile;
	char	xcopies[20];
	char	suppress[10];
                                                
	werrlog(ERRORCODE(1),file,copies,formnum,delete_after,0,0,0,0);

	switch(mode)
	{
	case 'H':
	case 'K':
		return(0);								/* Don't print if HOLD mode.		*/
		break;
	case 'S':
	case 'P':									/* SPOOL it.				*/
		break;
	default:
		return(99);
		break;
	}

	if ( access( file, 00 ) )							/* Does file exist?			*/
	{
		return( 20 );
	}
	
	if ( eaccess( file, 04 ) )							/* Can we read the file?		*/
	{
		return( 28 );
	}

	wpload();

	rc  = 0;
	zerofile = 0;

	size = filesize(file);
	if ( size == 0 )
	{
		werrlog(ERRORCODE(5),file,0,0,0,0,0,0,0);
		zerofile = 1;
	}                     

	if ( ! zerofile  && copies > 0  )
	{
		werrpath();								/* Init werrlog_path			*/

		if ( first_print )							/* If first time to print,		*/
		{
			first_print = 0;						/* No longer first time.		*/
			if( prt_env = getenv( "PRINT" ) )				/* Is PRINT environment variable set?	*/
			{
				strcpy( prt_cmd, prt_env );				/* Use its value as the print command.	*/
			}
			else								/* If "PRINT" is not set,		*/
			{
				strcpy( prt_cmd, "PRINT" );				/* Use "PRINT" as the default command.	*/
			}
		}
		sprintf( cmd, "%s %s", prt_cmd, file );					/* Build print command with file name.	*/

		while( copies-- )
		{
			rc = system( cmd );
			if ( rc != 0 )
			{
				werrlog(ERRORCODE(2),file,rc,errno,0,0,0,0,0); 
				werrlog(102,cmd,0,0,0,0,0,0,0);
				return( 40 );
			}
		}
	}

	if ( zerofile ) rc = 24;

	return( rc );
}
#endif	/* MSDOS */
