static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wprint.c
**
**	Project:	WISP/LIB
**
**	RCS:		$Source:$
**
**	Purpose:	All printing goes through the wprint() routine.
**
**	Routines:	
**	wprint()	
**	
*/

/*
**	Includes
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
#include "idsisubs.h"
#include "que_jobs.h"
#include "wperson.h"
#include "werrlog.h"
#include "wglobals.h"
#include "wisplib.h"
#include "wmalloc.h"

#ifdef WIN32
#include "win32spn.h"
#endif

/*
**	Static Function Prototypes
*/

#ifdef VMS
static int vms_print( const char *file, int copies, int formnum, char lpclass, 
		     int4 printer, const char *disposition, char mode );
#endif

#ifdef unix
static int unique_print( const char *file, int copies, int formnum, char lpclass, 
		      int4 printer, const char *disposition, char mode );
static int lp_print( const char *file, int copies, int formnum, char lpclass, 
		    int4 printer, const char *disposition, char mode);
#endif

#ifdef WIN32
int win32_printfile( const char *file, int copies, int formnum, char lpclass, 
		    int4 printer, const char *disposition, char mode );
#endif

static int generic_print( const char *file, int copies, int formnum, char lpclass, 
			 int4 printer, const char *disposition, char mode);

/*
**	Routine:	wprint()
**
**	Function:	Common frontend to WISP printing facilities.
**
**	Description:	Get the default printer number then call the specific print routine.
**			If mode is 'K' then just return.
**			If disposition in NULL then set it based on mode.
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
**
*/
void wprint(const char* native_fname, char mode, const char* disposition, 
	    int copies, char prt_class, int prt_form, int* return_code)
{
#define		ROUTINE		83500
	int	deleteit = 0;
	int4	printer;

	wtrace("WPRINT", "ENTRY", "File=[%s] Mode=[%c] Disp=[%2.2s] Copies=[%d] Class=[%c] Form=[%d]",
	       (native_fname)?native_fname:"(nil)", mode, (disposition)?disposition:"??", copies, prt_class, prt_form);

	if (mode == 'K')
	{
		*return_code = 0;							/* Don't print if KEEP mode.		*/
		return;
	}

	/*
	**	Validate all the parameters.
	*/
	if (NULL == native_fname)
	{
		wtrace("WPRINT","ARGS", "File name is NULL");
		*return_code = 40;
		return;
	}

	if ( !fexists(native_fname) )
	{
		/* File does not exist */
		*return_code = 20;
		return;
	}
	
	if (copies < 1)
	{
		wtrace("WPRINT","ARGS", "Copies is less then 1 [%d]", copies);
		*return_code = 40;
		return;
	}

	if (prt_form < 0 || prt_form > 999)
	{
		wtrace("WPRINT","ARGS", "Form number is invalid [%d]", prt_form);
		*return_code = 40;
		return;
	}

	if (! (' ' == prt_class || (prt_class >= 'A' && prt_class <= 'Z'))) 
	{
		wtrace("WPRINT","ARGS", "Class is invalid [%c]",prt_class);
		*return_code = 40;
		return;
	}
	
	if (NULL == disposition)
	{
		/*
		**	Set the disposition based on mode.
		**	   Mode 'S' means print and delete. "DX"
		**	   Mode 'P' means print and save. "DS"	
		*/
		if ('S' == mode)
		{
			disposition = "DX";
		}
		else
		{
			disposition = "DS";
		}
	}
	else
	{
		if (!(0==memcmp("DS",disposition,2) || 
		      0==memcmp("DX",disposition,2) || 
		      0==memcmp("RS",disposition,2)   ))
		{
			wtrace("WPRINT","ARGS", "Disposition is invalid [%2.2s]", disposition);
			*return_code = 40;
			return;
		}
	}
	
	switch(mode)
	{
	case 'S':	
		break;
	case 'H':
		break;
	case 'P':
		mode = 'S';
		break;
	default:
		wtrace("WPRINT","ARGS", "Mode is invalid [%c]", mode);
		*return_code = 40;
		return;
	}


	/*
	**	Get the printer number
	*/
	get_defs(DEFAULTS_PR,&printer);
	
	if (printer < 0 || printer > 999)
	{
		wtrace("WPRINT","ARGS", "Printer number is invalid [%d] using 000", printer);
		printer = 0;
	}

	/*
	**	VMS and MSDOS are old and only support there native printing.
	*/
#ifdef VMS
	*return_code = vms_print( native_fname, copies, prt_form, prt_class, printer, disposition, mode);
	return;
#endif

#ifdef MSDOS
	deleteit = 0;
	if ( !disposition && mode == 'S' ) deleteit = 1;				/* If SPOOL and disp is NULL then delete.*/
	if ( disposition && 0==memcmp(disposition,"DX",2)) deleteit = 1;		/* Disp is DX then delete.		*/

	*return_code = dos_print( native_fname, copies, prt_form, prt_class, printer, deleteit, mode);
	return
#endif

	/*
	**	Call the platform/print queue specific routine to print.
	*/

	switch(opt_printqueue)
	{
	case PQ_GENERIC:
		*return_code = generic_print( native_fname, copies, prt_form, prt_class, printer, disposition, mode);
		return;

	case PQ_UNIQUE:
	case PQ_ILP:
#ifdef unix
		*return_code = unique_print( native_fname, copies, prt_form, prt_class, printer, disposition, mode);
		return;
#endif
#ifdef WIN32
		*return_code = generic_print( native_fname, copies, prt_form, prt_class, printer, disposition, mode);
		return;
#endif
		break;

	case PQ_LP:
	case PQ_NP:
#ifdef unix
		*return_code = lp_print( native_fname, copies, prt_form, prt_class, printer, disposition, mode);
		return;
#endif

	case PQ_DEFAULT:
	default:
#ifdef unix
		*return_code = unique_print( native_fname, copies, prt_form, prt_class, printer, disposition, mode);
		return;
#endif
#ifdef WIN32
		*return_code = win32_printfile( native_fname, copies, prt_form, prt_class, 
					printer, disposition, mode);
		return;
#endif
	}
	


}

#ifdef VMS
static int vms_print( const char *native_fname, int copies, int prt_form, char prt_class, 
		     int4 printer, const char *disposition, char mode )
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
static int unique_print( const char *file, int copies, int formnum, char lpclass, 
		      int4 printer, const char *disposition, char mode )
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
	else if (PQ_ILP == opt_printqueue)
	{
		strcpy(exe,"ilp");
	}
	else
	{
		strcpy(exe,"ulp -q");
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
static int lp_print( const char *file, int copies, int formnum, char lpclass, 
		    int4 printer, const char *disposition, char mode)
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

	if (PQ_NP == opt_printqueue)
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
**	ROUTINE:	subvar()
**
**	FUNCTION:	Substitute variable in a string
**
**	DESCRIPTION:	Replace all occurances of "var" in "buff" with "value"
**
**	ARGUMENTS:	
**	buff		The string to be modified
**	var		The varible to look for (e.g %FILE%)
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

	/*
	**	Check for recursive substitutions.
	*/
	if (strpos(value, var) != -1)
	{
		sprintf(temp,"%%WPRINT-E-MACRO Print macro %s contains a recursive substitution [%s]", var, value);
		werrlog(104,temp,0,0,0,0,0,0,0);
		value = "";
	}
	
	pos = 0;
	for(subcnt=0; (pos = strpos(buff,var)) != -1; subcnt++)
	{
		ptr  = &buff[pos];						/* point to location of search string	*/
		strcpy(temp, ptr+strlen(var));					/* copy end of line into temp string	*/
		strcpy(ptr,value);						/* put replacement string in place	*/
		strcat(ptr,temp);						/* put end line back into source	*/
		pos++;
	}

	return subcnt;
}

/*
**	ROUTINE:	generic_print()
**
**	FUNCTION:	Implements a interface to a generic print queue based on PQ options.
**
**	DESCRIPTION:	The PQCMD option is used as a template for the print queue command.
**			The template should contain a number of macros which are substituted
**			with the actual runtime values.
**
**			There are three types of macros, "simple", "map", and "optional".
**			The simple macros (e.g. %FILE%) are always replaced by there substitution values.
**			The map macros (e.g. %LPMAP%) lookup there value in a map file, if 
**			a map file entry is not found then it substitutes to nothing.
**			The optional macros (e.g. %HOLDOPT%) only have a value when a certain
**			condition is true, otherwise they substitute to nothing. The value
**			of optional macros is defined by OPTIONS file entries.
**
**			The order of macro substition is optional macros first, map macros second, then simple macros.
**			Optional macro values may contain map and simple macros but can not contain optional macros. 
**			Map macro values may contain simple macros but can not contain optional or map macros. 
**
**	Option file entry	Meaning
**	-----------------	-------
**	PQCMD			The print command template
**	PQCLASSOPT		The %CLASSOPT% macro when class is not " " (blank)
**	PQCOPIESOPT		The %COPIESOPT% macro when copies not 1
**	PQPRTNUMOPT		The %PRTNUMOPT% macro when printer number not 000
**	PQFORMOPT		The %FORMOPT% macro when form number not 000
**	PQHOLDOPT		The %HOLDOPT% macro when mode is 'H' (hold)
**	PQDELOPT		The %DELOPT% macro when disposition is "DX" (delete after)
**	PQREQOPT		The %REQOPT% macro when disposition is "RS" (requeue after)
**
**	PQDELETELOCAL		Delete file locally if disp="DX" and not hold.
**	PQNOHOLD		Don't issue print command if mode='H'
**
** 	Macro		Meaning
**	-----		-------
** 	%FILE%		The file to print (fully qualified)
** 	%CLASS%		The PRTCLASS value "A"-"Z" and " "
** 	%FORM%		The FORM_NUM value "000"-"999"
** 	%COPIES%	The number of copies to print
** 	%PRTNUM%	The PRINTER value "000"-"999"
** 	%LINES%		The LINES per page value "000"-"999"
** 	%DISP%		The disposition "DS", "DX", "RS"
** 	%MODE%		The mode "S" or "H"
** 	%XID%		The EXTRACT "ID" value. (3 character user id.)
** 	%XNA%		The EXTRACT "NA" value. (Long user name.)
**
** 	(Optional macros)
** 	%CLASSOPT%	PQCLASSOPT when %CLASS% is not " ".
** 	%COPIESOPT%	PQCOPIESOPT when %COPIES% is not 1.
** 	%FORMOPT%	PQFORMOPT when %FORM% is not 000.
** 	%PRTNUMOPT%	PQPRTNUMOPT when %PRTNUM% is not 000.
** 	%HOLDOPT%	PQHOLDOPT when %MODE% is "H". (Queue with Hold.)
** 	%DELOPT%	PQDELOPT when %DISP% is "DX". (Delete after printing.)
** 	%REQOPT%  	PQREQOPT when %DISP% is "RS". (Requeue after printing.)
**
**	(Map file macros)
**	%LPMAP%		LPMAP file entry based on PRTCLASS
**	%PRMAP%		PRMAP file entry based on PRINTER
**	%FORMMAP%	FORMS file entry based on FORM_NUM
**
*/

static int generic_print( const char *file, int copies, int formnum, char lpclass, 
			 int4 printer, const char *disposition, char mode)
{
	static const char *pqcmd = NULL;
	static const char *xid = NULL;
	static const char *xna = NULL;
	
	char	cmd[1024];
	const char *cptr;
	char	sub[80];
	int4	lines;
	int4	args;
	int	rc;
	int	i;

	/*
	**	Check for no hold option and return if hold.
	*/
	if ('H'==mode && get_wisp_option("PQNOHOLD"))
	{
		wtrace("WPRINT","PQNOHOLD", "Hold (MODE=H) not supported, file not printed [%s]",file);
		return 0;
	}

	/*
	**	Load the PQCMD template into cmd.
	*/
	if (NULL == pqcmd)
	{
		pqcmd = get_wisp_option("PQCMD");
		if (NULL==pqcmd || '\0' == *pqcmd)
		{
			pqcmd = NULL;
			werrlog(104,"%%WPRINT-E-PQCMD The PQCMD option has not been set in the OPTIONS file.",0,0,0,0,0,0,0);
			return 44;
		}
	}
	strcpy(cmd, pqcmd);

	/*
	**	Substitute each of the OPT macros.
	**
	**	Macro		Option		
	**	========	=========	
	**	%CLASSOPT%	PQCLASSOPT  	
	**	%COPIESOPT%	PQCOPIESOPT 	
	**	%FORMOPT%	PQFORMOPT 	
	**	%PRTNUMOPT%	PQPRTNUMOPT 	
	**	%HOLDOPT%	PQHOLDOPT   	
	**	%DELOPT%	PQDELOPT    	
	**	%REQOPT%	PQREQOPT    	
	*/

	/* 
	**   	%CLASSOPT%  if class is != ' ' 
	*/
	cptr = "";
	if (' ' != lpclass)
	{
		cptr = get_wisp_option("PQCLASSOPT");
		if (NULL==cptr)
		{
			cptr = "";
		}
	}
	subvar(cmd, "%CLASSOPT%", cptr);

	/* 
	**	%COPIESOPT%  if copies is != 1 
	*/
	cptr = "";
	if (1 != copies)
	{
		cptr = get_wisp_option("PQCOPIESOPT");
		if (NULL==cptr)
		{
			cptr = "";
		}
	}
	subvar(cmd, "%COPIESOPT%", cptr);

	/* 
	**	%FORMOPT%  if form is != 000 
	*/
	cptr = "";
	if (0 != formnum)
	{
		cptr = get_wisp_option("PQFORMOPT");
		if (NULL==cptr)
		{
			cptr = "";
		}
	}
	subvar(cmd, "%FORMOPT%", cptr);
	
	/* 
	**   %PRTNUMOPT%  if printer is != 000 
	*/
	cptr = "";
	if (0 != printer)
	{
		cptr = get_wisp_option("PQPRTNUMOPT");
		if (NULL==cptr)
		{
			cptr = "";
		}
	}
	subvar(cmd, "%PRTNUMOPT%", cptr);
	
	/*
	**	%HOLDOPT%  if mode == 'H' 
	*/
	cptr = "";
	if ('H' == mode)
	{
		cptr = get_wisp_option("PQHOLDOPT");
		if (NULL==cptr)
		{
			cptr = "";
		}
	}
	subvar(cmd, "%HOLDOPT%", cptr);
	
	/* 
	**   	%DELOPT%  if disposition == "DX" 
	*/
	cptr = "";
	if (0==memcmp("DX",disposition,2))
	{
		cptr = get_wisp_option("PQDELOPT");
		if (NULL==cptr)
		{
			cptr = "";
		}
	}
	subvar(cmd, "%DELOPT%", cptr);
	
	/* 
	**	%REQOPT%  if disposition == "RS" 
	*/
	cptr = "";
	if (0==memcmp("RS",disposition,2))
	{
		cptr = get_wisp_option("PQREQOPT");
		if (NULL==cptr)
		{
			cptr = "";
		}
	}
	subvar(cmd, "%REQOPT%", cptr);

	/*
	**	Map file macros.
	**
	**	Macro		Value
	**	-----		-----
	**	%LPMAP%		LPMAP file entry based on PRTCLASS
	**	%PRMAP%		PRMAP file entry based on PRINTER
	**	%FORMMAP%	FORMS file entry based on FORM_NUM
	*/

	subvar(cmd, "%LPMAP%", wlpclass(lpclass));
	subvar(cmd, "%PRMAP%", getprmap(printer));
	subvar(cmd, "%FORMMAP%", wforms(formnum));


	/*
	**	Substitute each of the remaining macros.
	**
	**	Macro		Value
	**	======		=====
	** 	%FILE%		The file to print (fully qualified)
	** 	%CLASS%		The PRTCLASS value "A"-"Z" and " "
	** 	%FORM%		The FORM_NUM value "000"-"999"
	** 	%COPIES%	The number of copies to print
	** 	%PRTNUM%	The PRINTER value "000"-"999"
	** 	%DISP%		The disposition "DS", "DX", "RS"
	** 	%MODE%		The mode "S" or "H"
	**
	** 	%LINES%		The LINES per page value "000"-"999"
	** 	%XID%		The EXTRACT "ID" value.
	** 	%XNA%		The EXTRACT "NA" value.
	**
	*/

	subvar(cmd, "%FILE%", file);
	
	sub[0] = lpclass;
	sub[1] = '\0';
	subvar(cmd, "%CLASS%", sub);

	sprintf(sub,"%03d", formnum);
	subvar(cmd, "%FORM%", sub);

	sprintf(sub,"%d", copies);
	subvar(cmd, "%COPIES%", sub);

	sprintf(sub,"%03d", printer);
	subvar(cmd, "%PRTNUM%", sub);
	
	sub[0] = disposition[0];
	sub[1] = disposition[1];
	sub[2] = '\0';
	subvar(cmd, "%DISP%", sub);
	
	sub[0] = mode;
	sub[1] = '\0';
	subvar(cmd, "%MODE%", sub);

	args=2;
	wvaset(&args);
	EXTRACT("LI",&lines);
	wswap(&lines);
	sprintf(sub,"%03d", lines);
	subvar(cmd, "%LINES%", sub);

	if (NULL==xid)
	{
		args=2;
		wvaset(&args);
		EXTRACT("ID",sub);
		sub[3] = '\0';
		xid = wstrdup(sub);
	}
	subvar(cmd, "%XID%", xid);

	if (NULL==xna)
	{
		args=2;
		wvaset(&args);
		EXTRACT("NA",sub);
		sub[24] = '\0';
		/* Remove trailing blanks from the name */
		for(i=23; i>=0 && ' '==sub[i];i--)
		{
			sub[i] = '\0';
		}
		xna = wstrdup(sub);
	}
	subvar(cmd, "%XNA%", xna);
	
#ifdef unix
	/*
	**	Redirect the output to wisperr.log file.
	*/
	strcat(cmd,">>");
	strcat(cmd,werrpath());
	strcat(cmd," 2>&1");
	rc = wsystem( cmd );
#endif
#ifdef WIN32
	if (wtracing())
	{
		rc = win32spawnlp(NULL, cmd, SPN_HIDDEN_CMD|SPN_WAIT_FOR_CHILD|SPN_CAPTURE_OUTPUT);
	}
	else
	{
		rc = win32spawnlp(NULL, cmd, SPN_HIDDEN_CMD|SPN_WAIT_FOR_CHILD|SPN_NO_INHERIT);
	}
#endif

	if ( rc != 0 )
	{
		char buff[256];
		
		sprintf(buff, "%%WPRINT-E-FAILED Print of %s failed rc=%d errno=%d", file, rc, errno);
		werrlog(104,buff,0,0,0,0,0,0,0);
		werrlog(102,cmd,0,0,0,0,0,0,0);
		return( 44 );
	}

	/*
	**	Delete after, locally if disp="DX" and not hold.
	*/
	if (0==memcmp("DX",disposition,2) && 
	    get_wisp_option("PQDELETELOCAL") &&
	    'H' != mode)
	{
		wtrace("WPRINT","PQDELETELOCAL", "Print file deleted locally [%s]",file);
		unlink(file);
	}
	
	return(rc);
}


/*
**	History:
**	$Log: wprint.c,v $
**	Revision 1.16  1998-10-23 11:10:32-04  gsl
**	Implement the PQCMD generic print queue interface.
**
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
