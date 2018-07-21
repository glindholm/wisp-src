/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
*/

/*
**	File:		wprint.c
**
**	Project:	WISP/LIB
**
**	Purpose:	All printing goes through the WL_wprint() routine.
**
**	Routines:	
**	WL_wprint()	
**	
*/

/*
**	Includes
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "idsistd.h"
#include "idsisubs.h"
#include "wperson.h"
#include "werrlog.h"
#include "wglobals.h"
#include "wisplib.h"
#include "wmalloc.h"
#include "vssubs.h"

#ifdef WIN32
#include "win32spn.h"
#endif

/*
**	Static Function Prototypes
*/


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
**	Routine:	WL_wprint()
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
void WL_wprint(const char* native_fname, char mode, const char* disposition, 
	    int copies, char prt_class, int prt_form, int* return_code)
{
	int4	printer;

	WL_wtrace("WPRINT", "ENTRY", "File=[%s] Mode=[%c] Disp=[%2.2s] Copies=[%d] Class=[%c] Form=[%d]",
	       (native_fname)?native_fname:"(null)", mode, (disposition)?disposition:"  ", copies, prt_class, prt_form);


	/*
	**	Validate all the parameters.
	*/
	if (NULL == native_fname)
	{
		WL_wtrace("WPRINT","ARGS", "File name is NULL");
		*return_code = PRINT_RC_40_INVALID_PARAM;
		return;
	}

	if ( !fexists(native_fname) )
	{
		/* File does not exist */
		*return_code = PRINT_RC_20_FILE_NOT_FOUND;
		return;
	}
	
	switch(mode)
	{
	case 'K':	/* KEEP - don't print */	
		*return_code = PRINT_RC_0_SUCCESS;
		return;

	case 'S':	/* SPOOL */	
		break;
	case 'H':	/* HOLD */
		break;
	case 'P':	/* PRINT and save */
		break;

	default:
		WL_wtrace("WPRINT","ARGS", "Mode is invalid [%c]", mode);
		*return_code = PRINT_RC_40_INVALID_PARAM;
		return;
	}

	if (copies < 1)
	{
		WL_wtrace("WPRINT","ARGS", "Copies is less then 1 [%d]", copies);
		*return_code = PRINT_RC_40_INVALID_PARAM;
		return;
	}

	if (prt_form < 0 || prt_form > 999)
	{
		WL_wtrace("WPRINT","ARGS", "Form number is invalid [%d]", prt_form);
		*return_code = PRINT_RC_40_INVALID_PARAM;
		return;
	}

	if (! (' ' == prt_class || (prt_class >= 'A' && prt_class <= 'Z'))) 
	{
		WL_wtrace("WPRINT","ARGS", "Class is invalid [%c]",prt_class);
		*return_code = PRINT_RC_40_INVALID_PARAM;
		return;
	}
	
	if (NULL == disposition)
	{
		/*
		**	Set the disposition based on mode.
		**	   Mode 'S' means print and delete. "DX"
		**	   Mode 'H' means hold, print and delete. "DX"
		**	   Mode 'P' means print and save. "DS"	
		*/
		if ('P' == mode)
		{
			disposition = "DS";
		}
		else
		{
			disposition = "DX";
		}
	}
	else
	{
		if (!(0==memcmp("DS",disposition,2) || 
		      0==memcmp("DX",disposition,2) || 
		      0==memcmp("RS",disposition,2)   ))
		{
			WL_wtrace("WPRINT","ARGS", "Disposition is invalid [%2.2s]", disposition);
			*return_code = PRINT_RC_40_INVALID_PARAM;
			return;
		}
	}
	
	if ('P' == mode )
	{
		/* 
		 *  Mode=P is only used to set the disposition, switch back to 'S'.
		 */
		mode = 'S';
	}

	/*
	**	Get the printer number
	*/
	WL_get_defs(DEFAULTS_PR,&printer);
	
	if (printer < 0 || printer > 999)
	{
		WL_wtrace("WPRINT","ARGS", "Printer number is invalid [%d] using 000", printer);
		printer = 0;
	}

	/*
	**	Call the platform/print queue specific routine to print.
	*/

	switch(WL_opt_printqueue)
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
	
	WL_wtrace("unique_print", "ENTRY", "File=[%s] Mode=[%c] Disp=[%2.2s] Copies=[%d] Class=[%c] Form=[%d] Printer=[%d]",
	       file, mode, disposition, copies, lpclass, formnum, printer);


	switch(mode)
	{
	case 'H':
		hold=1;
		break;

	case 'S':
		break;

	default:
		return(PRINT_RC_60_INVALID_MODE);
	}

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
	else return(PRINT_RC_61_INVALID_DISP);

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

	if (printer != 0)
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

	if ((ptr = getenv("UNIQUE_PRINT")))
	{
		strcpy(exe,ptr);
	}
	else if (PQ_ILP == WL_opt_printqueue)
	{
		strcpy(exe,"ilp");
	}
	else
	{
		strcpy(exe,"ulp -q");
	}

	sprintf( cmd, "%s %s %s %s %s %s '%s'", 
		exe,
		modestr,
		l_class,
		l_form,
		l_copies,
		l_printer,
		file );

	rc = WL_run_unixcommand_silent( cmd );
	if ( rc != 0 )
	{
		werrlog(WERRCODE(46404),exe,file,rc,errno,0,0,0,0,0); 
		WL_werrlog_error(WERRCODE(46404),"WPRINT", "CMD", "%s", cmd);
		return( PRINT_RC_62_UNIQUE_ERROR );
	}
	return(PRINT_RC_0_SUCCESS);
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
	char	cmd[256];
	int	rc, size, zerofile;
	char	xcopies[20];
	char	*suppress_flag;
	int	delete_after, respool;
	char	*copies_flag, *base_command;

	WL_wtrace("LP_PRINT", "ENTRY", "File=[%s] Mode=[%c] Disp=[%2.2s] Copies=[%d] Class=[%c] Form=[%d] Printer=[%d]",
	       file, mode, disposition, copies, lpclass, formnum, printer);

	if (PQ_NP == WL_opt_printqueue)
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
		return(PRINT_RC_60_INVALID_MODE);
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
		else return(PRINT_RC_61_INVALID_DISP);
	}

	if ( !WL_fexists(file) )								/* Does file exist?			*/
	{
		return( PRINT_RC_20_FILE_NOT_FOUND );
	}
	
	if ( !WL_fcanread(file) )								/* Can we read the file?		*/
	{
		return( PRINT_RC_28_ACCESS_DENIED );
	}

	rc  = 0;
	zerofile = 0;

	size = WL_filesize(file);
	if ( size == 0 )
	{
		WL_wtrace("LP_PRINT","ZEROLEN","Zero lenght file not printed [%s]", file);
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


		sprintf( cmd, "%s %s %s %s %s %s '%s'", 
				base_command,
				suppress_flag, 
				WL_wforms(formnum), 
				WL_wlpclass(lpclass), 
				WL_getprmap(printer),
				xcopies, 
				file);

		rc = WL_run_unixcommand_silent( cmd );
		if ( rc != 0 )
		{
			if (rc != -1)
			{
				/*
				**	errno is only meaningful when rc == -1 so reset otherwise.
				*/
				errno = 0;
			}
			werrlog(WERRCODE(46502),file,rc,errno,0,0,0,0,0); 
			WL_werrlog_error(WERRCODE(46502),"WPRINT", "CMD", "%s", cmd);
			return( PRINT_RC_63_LP_ERROR );
		}
	}

	if ( delete_after )
	{
		rc = wisp_unlink(file);
		if ( rc < 0 )
		{
			WL_wtrace("LP_PRINT","UNLINK","Unable to delete %s errno=%d",
				file,errno);
		}
	}

	if ( zerofile ) rc = PRINT_RC_24_INVALID_FILETYPE;

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
		WL_werrlog_error(WERRCODE(83504),"WPRINT", "MACRO", 
			"Print macro %s contains a recursive substitution [%s]", 
			var, value);
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
	int	rc;
	int	i;


	WL_wtrace("generic_print", "ENTRY", "File=[%s] Mode=[%c] Disp=[%2.2s] Copies=[%d] Class=[%c] Form=[%d] Printer=[%d]",
	       file, mode, disposition, copies, lpclass, formnum, printer);

	/*
	**	Check for no hold option and return if hold.
	*/
	if ('H'==mode && WL_get_wisp_option("PQNOHOLD"))
	{
		WL_wtrace("WPRINT","PQNOHOLD", "Hold (MODE=H) not supported, file not printed [%s]",file);
		return PRINT_RC_0_SUCCESS;
	}

	/*
	**	Load the PQCMD template into cmd.
	*/
	if (NULL == pqcmd)
	{
		pqcmd = WL_get_wisp_option("PQCMD");
		if (NULL==pqcmd || '\0' == *pqcmd)
		{
			pqcmd = NULL;
			WL_werrlog_error(WERRCODE(83504),"WPRINT", "PQCMD", 
				"The PQCMD option has not been set in the OPTIONS file.");
			return PRINT_RC_99_QUEUE_NOT_AVAILABLE;
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
		cptr = WL_get_wisp_option("PQCLASSOPT");
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
		cptr = WL_get_wisp_option("PQCOPIESOPT");
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
		cptr = WL_get_wisp_option("PQFORMOPT");
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
		cptr = WL_get_wisp_option("PQPRTNUMOPT");
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
		cptr = WL_get_wisp_option("PQHOLDOPT");
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
		cptr = WL_get_wisp_option("PQDELOPT");
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
		cptr = WL_get_wisp_option("PQREQOPT");
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

	subvar(cmd, "%LPMAP%",   WL_wlpclass(lpclass));
	subvar(cmd, "%PRMAP%",   WL_getprmap(printer));
	subvar(cmd, "%FORMMAP%", WL_wforms(formnum));


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

	WL_set_va_count(2);
	EXTRACT2("LI",&lines);
	WL_wswap(&lines);
	sprintf(sub,"%03d", lines);
	subvar(cmd, "%LINES%", sub);

	if (NULL==xid)
	{
		WL_set_va_count(2);
		EXTRACT2("ID",sub);
		sub[3] = '\0';
		xid = wisp_strdup(sub);
	}
	subvar(cmd, "%XID%", xid);

	if (NULL==xna)
	{
		WL_set_va_count(2);
		EXTRACT2("NA",sub);
		sub[24] = '\0';
		/* Remove trailing blanks from the name */
		for(i=23; i>=0 && ' '==sub[i];i--)
		{
			sub[i] = '\0';
		}
		xna = wisp_strdup(sub);
	}
	subvar(cmd, "%XNA%", xna);
	
#ifdef unix
	rc = WL_run_unixcommand_silent( cmd );
#endif
#ifdef WIN32
	if (WL_wtracing())
	{
		rc = WL_win32spawnlp(NULL, cmd, SPN_HIDDEN_CMD|SPN_WAIT_FOR_CHILD|SPN_CAPTURE_OUTPUT);
	}
	else
	{
		rc = WL_win32spawnlp(NULL, cmd, SPN_HIDDEN_CMD|SPN_WAIT_FOR_CHILD|SPN_NO_INHERIT);
	}
#endif

	if ( rc != 0 )
	{		
		WL_werrlog_error(WERRCODE(83504),"WPRINT", "FAILED", 
			"Print of %s failed rc=%d errno=%d", file, rc, errno);
		WL_werrlog_error(WERRCODE(83502),"WPRINT", "CMD", 
			"%s", cmd);
		return( PRINT_RC_99_QUEUE_NOT_AVAILABLE );
	}

	/*
	**	Delete after, locally if disp="DX" and not hold.
	*/
	if (0==memcmp("DX",disposition,2) && 
	    WL_get_wisp_option("PQDELETELOCAL") &&
	    'H' != mode)
	{
		WL_wtrace("WPRINT","PQDELETELOCAL", "Print file deleted locally [%s]",file);
		wisp_unlink(file);
	}
	
	return(PRINT_RC_0_SUCCESS);
}


/*
**	History:
**	$Log: wprint.c,v $
**	Revision 1.34  2003/03/28 20:15:57  gsl
**	Add EXTRACT2
**	
**	Revision 1.33  2003/03/19 22:26:19  gsl
**	Standardize PRINT RC defines
**	
**	Revision 1.32  2003/02/17 22:07:17  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.31  2003/02/04 17:22:57  gsl
**	Fix -Wall warnings
**	
**	Revision 1.30  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.29  2002/12/12 20:48:23  gsl
**	Fix bogus errno reports when LP fails
**	
**	Revision 1.28  2002/12/11 17:03:11  gsl
**	use wisp_unlink()
**	
**	Revision 1.27  2002/12/10 17:09:13  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.26  2002/12/09 19:15:38  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.25  2002/07/22 20:32:34  gsl
**	Unix commands put filenames in 'quotes' because they can contain $
**	
**	Revision 1.24  2002/07/12 20:40:41  gsl
**	Global unique WL_ changes
**	
**	Revision 1.23  2002/07/12 17:01:04  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.22  2002/07/10 21:05:36  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.21  2002/07/10 04:27:33  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.20  2002/07/02 21:15:36  gsl
**	Rename wstrdup
**	
**	Revision 1.19  2002/03/26 15:26:39  gsl
**	Fixed Mode=Hold to default to Disp=DX
**	In WISP 4.3.02 Mode=Hold was incorrectly changed to default to Disp=DS
**	Improved the tracing
**	
**	Revision 1.18  2001-11-27 15:49:06-05  gsl
**	Remove VMS & MSDOS
**
**	Revision 1.17  2001-11-01 10:49:22-05  gsl
**	Replace WL_wsystem() with run_unixcommand_silent() for unix
**
**	Revision 1.16  1998-10-23 11:10:32-04  gsl
**	Implement the PQCMD generic print queue interface.
**
**	Revision 1.15  1998-01-05 15:46:30-05  gsl
**	Add WL_wtrace
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
