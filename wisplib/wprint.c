			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
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

#include "idsistd.h"
#include "que_jobs.h"
#include "wperson.h"
#include "werrlog.h"
#include "wglobals.h"

#ifdef unix
static int idsi_print();
static int lp_print();
#endif

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
	int4	printer;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	if (mode == 'K')
	{
		*return_code = 0;							/* Don't print if KEEP mode.		*/
		return(0);
	}

	get_defs(DEFAULTS_PR,&printer);

	deleteit = 0;
	if ( !disposition && mode == 'S')                   deleteit = 1;		/* If Spool a disp is NULL then delete	*/
	if (  disposition && 0==memcmp(disposition,"DX",2)) deleteit = 1;		/* Disp is DX then delete.		*/

#ifdef VMS
	prt_ptr = get_prt_list();							/* Get a pointer to the printer list	*/

	do
	{
		if ((prt_ptr->class == prt_class) && (prt_ptr->prtnum == printer))
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
		int4 rc;

		rc = *return_code;
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
		 *return_code = wang_retcod;
	}
#endif	/* VMS */

#ifdef unix
	if (opt_idsiprint)
	{
		*return_code = idsi_print( native_fname, copies, prt_form, prt_class, printer, disposition, mode);
	}
	else
	{
		*return_code = lp_print( native_fname, copies, prt_form, prt_class, printer, deleteit, mode);
	}
#endif

#ifdef MSDOS
	*return_code = dos_print( native_fname, copies, prt_form, prt_class, printer, deleteit, mode);
#endif
}

#ifdef unix
static int idsi_print( file, copies, formnum, lpclass, printer, disposition, mode )
char	*file;
int	copies;
int	formnum;
char	lpclass;
int4	printer;
char	*disposition;
char	mode;
{
	char	exe[80],l_form[10],l_copies[10],l_class[10],l_printer[10];
	int	rc;
	char	*ptr;
	char	cmd[256];
	char    modestr[64];
	int del=0, re=0, hold=0;
	
#undef		ROUTINE
#define		ROUTINE		46400

	werrlog(ERRORCODE(1),file,copies,formnum,0,0,0,0,0);

	switch(mode)
	{
	case 'H':
		hold=1;
		del=1;
		break;
	case 'K':
		return(0);
		break;
	case 'S':
		del=1;
		break;
	case 'P':									/* SPOOL it.				*/
		break;
	default:
		return(99);
		break;
	}

	if (disposition)
	{
		if      (0==memcmp(disposition,"DX",2))
		{
			del=1;								/* Delete after printing.	*/
		}
		else if (0==memcmp(disposition,"RS",2)) 
		{
			re=1;								/* respool */
			del=0;
		}
		else if (0==memcmp(disposition,"DS",2))
		{
			re=0;
			del=0;
		}
		else return(99);
	}

	sprintf(l_form,"-f%03d",formnum);
	sprintf(l_copies,"-n%d",copies?copies:1);
	if (lpclass!=' ')
	{
		sprintf(l_class,"-C%c",lpclass);
	}
	else
	{
		strcpy(l_class,"");
	}
	if (printer)
	{
		sprintf(l_printer,"-P%d",printer);
	}
	else
	{
		strcpy(l_printer,"");
	}
	werrpath();								/* Init werrlog_path			*/

	if (re || del || hold)
	{
		strcpy(modestr,"-M");
		if (hold)
		{
			strcat(modestr,"hold");
			if (re || del)
			{
				strcat(modestr,",");
			}
		}
		if (re)
		{
			strcat(modestr,"re");
		}
		else if (del)
		{
			strcat(modestr,"del");
		}
	}
	else
	{
		strcpy(modestr,"");
	}

	if (ptr = getenv("UNIQUE_PRINT"))
	{
		strcpy(exe,ptr);
	}
	else if (opt_pqunique)
	{
		strcpy(exe,"unique");
	}
	else
	{
		strcpy(exe,"ilp");
	}

	sprintf( cmd, "%s %s %s %s %s %s %s >>%s 2>&1", 
		exe,
		modestr,
		l_class,
		l_form,
		l_copies,
		l_printer,
		file, 
		werrlog_path );

	rc = wsystem( cmd );
	if ( rc != 0 )
	{
		werrlog(ERRORCODE(2),file,rc,errno,0,0,0,0,0); 
		werrlog(102,cmd,0,0,0,0,0,0,0);
		return( 40 );
	}
	return(rc);
}

static int lp_print( file, copies, formnum, lpclass, printer, delete_after, mode)
char	*file;
int	copies;
int	formnum;
char	lpclass;
int4	printer;
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

	if ( !fexists(file) )								/* Does file exist?			*/
	{
		return( 20 );
	}
	
	if ( !fcanread(file) )								/* Can we read the file?		*/
	{
		return( 28 );
	}

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
				getprmap(printer),
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

