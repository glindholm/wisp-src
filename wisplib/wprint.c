static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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
#include <stdlib.h>
#include <errno.h>

#ifdef VMS
#include <ssdef.h>
#include <rmsdef.h>
#include <libdef.h>
#endif

#ifdef MSDOS
#include <io.h>
#include <process.h>
#endif

#include "idsistd.h"
#include "que_jobs.h"
#include "wperson.h"
#include "werrlog.h"
#include "wglobals.h"

#ifdef VMS
static int vms_print( char *file, int copies, int formnum, char lpclass, int4 printer, char *disposition, char mode );
#endif

#ifdef unix
static int idsi_print( char *file, int copies, int formnum, char lpclass, int4 printer, char *disposition, char mode );
static int lp_print( char *file, int copies, int formnum, char lpclass, int4 printer, char *disposition, char mode);
#endif

#ifdef WIN32
int win32_printfile( char *file, int copies, int formnum, char lpclass, int4 printer, char *disposition, char mode );
#endif

/*
**	Routine:	wprint()
**
**	Function:	Common frontend to WISP printing facilities.
**
**	Description:	Get the default printer number then call the specific print routine.
**
**	Arguments:
**	native_fname	The file to print
**	mode		The print mode 'H','K','S','P'
**	disposition	The disposition "DX", "RS", "DS" or NULL
**	copies		Number of copies
**	prt_class	The print class
**	prt_form	The form number
**	return_code	The return code
**			 0		Success
**			 4		Device not ready
**			 8		Write locked
**			12		File locked
**			16		Directory not found
**			20		File not found
**			24		Empty file
**			28		Access denied
**			40		Invalid arguments
**			44		???
**			56		Unknown error
**			60		Invalid mode
**			61		Invlaid disposition
**			62		UNIQUE command failed
**			63		LP command failed
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	09/27/94	Documented and split out vms_print(). GSL
**
*/
void wprint(native_fname,mode,disposition,copies,prt_class,prt_form,return_code)
char 	*native_fname;							/* The native filepath to print.			*/
char	mode;								/* The printmode H, S, K, P, O.				*/
char	*disposition;							/* The disposition DS, DX, RS (NULL if not used)	*/
									/*  if not NULL then only valid modes are H and S.	*/
int	copies;								/* Number of copies.					*/
char	prt_class;							/* The print class.					*/
int	prt_form;							/* The form number.					*/
int	*return_code;							/* The return code.					*/
{
#define		ROUTINE		83500
	int	deleteit = 0;
	int4	printer;

	wtrace("WPRINT", "ENTRY", "File=[%s] Mode=[%c] Disp=[%2.2s] Copies=[%d] Class=[%c] Form=[%d]",
	       native_fname, mode, (disposition)?disposition:"??", copies, prt_class, prt_form);

	if (mode == 'K')
	{
		*return_code = 0;							/* Don't print if KEEP mode.		*/
		return;
	}

	get_defs(DEFAULTS_PR,&printer);

#ifdef VMS
	*return_code = vms_print( native_fname, copies, prt_form, prt_class, printer, disposition, mode);
#endif

#ifdef unix
	if (opt_idsiprint)
	{
		*return_code = idsi_print( native_fname, copies, prt_form, prt_class, printer, disposition, mode);
	}
	else
	{
		*return_code = lp_print( native_fname, copies, prt_form, prt_class, printer, disposition, mode);
	}
#endif

#ifdef MSDOS
	deleteit = 0;
	if ( !disposition && mode == 'S' ) deleteit = 1;				/* If SPOOL and disp is NULL then delete.*/
	if ( disposition && 0==memcmp(disposition,"DX",2)) deleteit = 1;		/* Disp is DX then delete.		*/

	*return_code = dos_print( native_fname, copies, prt_form, prt_class, printer, deleteit, mode);
#endif

#ifdef WIN32
	*return_code = win32_printfile( native_fname, copies, prt_form, prt_class, 
					printer, disposition, mode);
#endif
}

#ifdef VMS
static int vms_print( char *native_fname, int copies, int prt_form, char prt_class, int4 printer, char *disposition, char mode )
{
#undef		ROUTINE
#define		ROUTINE		83500

	char 	jname[8],qname[132];
	int 	qflags;
	prt_id 	*prt_ptr;								/* Pointer to the printer list.		*/
	int	deleteit;
	int	return_code;

	deleteit = 0;
	if ( !disposition && mode == 'S' ) deleteit = 1;				/* If SPOOL and disp is NULL then delete.*/
	if ( disposition && 0==memcmp(disposition,"DX",2)) deleteit = 1;		/* Disp is DX then delete.		*/

	if ( disposition && 0==memcmp(disposition,"RS",2))				/* Disp is RS, VMS does not have dup.	*/
	{										/*  functionality so give warning and	*/
		werrlog(ERRORCODE(3),native_fname,0,0,0,0,0,0,0);			/*  print as DS.			*/
	}

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
	return_code = que_job(PRINT_QUEUE,qname,native_fname,jname,copies,prt_form,qflags);

	if (return_code == SS$_NORMAL) return_code = 0;					/* Set to expected Wang Code.		*/
	else
	{		
		int wang_retcod;							/* Set return code from queued job:	*/
		int4 rc;

		rc = return_code;
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
		else if (rc == SS$_ACCVIO) wang_retcod = 28; 				/* Access violation.			*/
		return_code = wang_retcod;
	}

	return return_code;
}
#endif

#ifdef unix
static int idsi_print( char *file, int copies, int formnum, char lpclass, int4 printer, char *disposition, char mode )
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
	case 'S':
		del=1;
		break;
	case 'P':									/* SPOOL it.				*/
		break;
	default:
		return(60);
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
		else return(61);
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
		strcpy(exe,"ulp");
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
		werrpath() );

	rc = wsystem( cmd );
	if ( rc != 0 )
	{
		werrlog(ERRORCODE(4),exe,file,rc,errno,0,0,0,0,0); 
		werrlog(102,cmd,0,0,0,0,0,0,0);
		return( 62 );
	}
	return(rc);
}

/*
**	Routine:	lp_print()
**
**	Function:	Print a file using LP
**
**	Description:	Build up an LP command then issue it.
**			This routine is used for both the LP and NP commands.
**
**	Arguments:
**	file		The file to print
**	copies		Number of copies
**	formnum		The form number
**	lpclass		The print class
**	printer		The printer number
**	disposition	The disposition "DX", "RS", "DS"
**	mode		The print mode 'H','K','S','P'
**
**	Globals:	None
**
**	Return:
**	0		Success
**	20		File not found
**	24		Empty file
**	28		Unable to read
**	60		Invalid mode
**	63		LP command failed
**
**	Warnings:	None
**
**	History:	
**	09/27/94	Documented and changed return codes. GSL
**
*/
static int lp_print( char *file, int copies, int formnum, char lpclass, int4 printer, char *disposition, char mode)
{
#undef		ROUTINE
#define		ROUTINE		46500
	char	cmd[256];
	int	rc, size, zerofile;
	char	xcopies[20];
	char	*suppress_flag;
	int	delete_after, respool;
	char	*copies_flag, *base_command;
                                                
	werrlog(ERRORCODE(1),file,copies,formnum,delete_after,0,0,0,0);

	if (opt_pqnp())
	{
		/*
		**	Support for the HP network printing product.
		*/
		base_command = "np";
		copies_flag = "-c ";
		suppress_flag = "-s";
	}
	else
	{
		base_command = "lp -c";
		copies_flag = "-n";
		suppress_flag = "-s";
#ifdef ULTRIX
		suppress_flag = "";
#endif
	}

	delete_after = 0;
	respool = 0;

	switch(mode)
	{
	case 'H': /* Hold */
	case 'K': /* Keep */
		return(0);

	case 'S': /* Spool and Delete */
		delete_after = 1;
		break;

	case 'P': /* Print (do not delete) */
		break;

	default:
		return(60);
	}

	if (disposition)
	{
		if      (0==memcmp(disposition,"DX",2))
		{
			delete_after = 1;						/* Delete after printing.	*/
		}
		else if (0==memcmp(disposition,"RS",2)) 
		{
			delete_after = 0;
			respool = 1;
		}
		else if (0==memcmp(disposition,"DS",2))
		{
			delete_after = 0;
		}
		else return(61);
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
			sprintf(xcopies,"%s%d", copies_flag, copies);
		}
		else
		{
			xcopies[0] = '\0';
		}


		sprintf( cmd, "%s %s %s %s %s %s %s>>%s 2>&1", 
				base_command,
				suppress_flag, 
				wforms(formnum), 
				wlpclass(lpclass), 
				getprmap(printer),
				xcopies, 
				file, 
				werrpath() );

		rc = wsystem( cmd );
		if ( rc != 0 )
		{
			werrlog(ERRORCODE(2),file,rc,errno,0,0,0,0,0); 
			werrlog(102,cmd,0,0,0,0,0,0,0);
			return( 63 );
		}
	}

	if ( delete_after )
	{
		rc = unlink(file);
		if ( rc < 0 )
		{
			werrlog(ERRORCODE(3),file,errno,0,0,0,0,0,0);
		}
	}

	if ( zerofile ) rc = 24;

	return( rc );
}
#endif	/* unix */
/*
**	History:
**	$Log: wprint.c,v $
**	Revision 1.15  1998-01-05 15:46:30-05  gsl
**	Add wtrace
**
**	Revision 1.14  1996-09-05 13:53:50-04  jockc
**	remove win32_printfile().. (now in win32_print
**	file called win32prt.c)
**
**	Revision 1.13  1996-08-19 15:33:20-07  gsl
**	drcs update
**
**
**
*/
