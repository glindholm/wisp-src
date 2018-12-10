/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/


#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "idsistd.h"
#include "wcommon.h"
#include "wperson.h"
#include "werrlog.h"
#include "wfname.h"
#include "wisplib.h"
#include "filext.h"
#include "vssubs.h"

/*
**	PRINT ( file, [lib, [vol, [mode, [disp, [copies, [class, [form,]]]]]]] returncode)
**	Args 2 thru 8 are optional but if any is specified all previous must also be.
**
**	arg1	file		Alpha[8]
**	arg2	lib		Alpha[8]
**	arg3	vol		Alpha[6]
**	arg4	mode		Alpha[1]
**	arg5	disp		Alpha[2]
**	arg6	copies		Int[4]
**	arg7	class		Alpha[1]
**	arg8	form		Int[4]
**	arg9	returncode	Int[4]
*/

void PRINT(char *l_file, ...)
{
	va_list the_args;								/* Define a pointer to the list.	*/
	int4	arg_count;								/* Number of arguments.			*/
	int4	num_copies,form_num,return_code;

	char *l_lib,									/* Address of the library name.		*/
	     *l_vol,									/* Address of the volume name.		*/
	     *l_mode,									/* Address of the job print mode.	*/
	     *l_disposition,								/* Address of the disposition action.	*/
	     l_class;									/* print class				*/

	int4	*l_return_code,								/* Address of 4 byte return code value.	*/
		*int4_ptr;

	char name[132], *end_name;
	int4 mode;

	return_code = PRINT_RC_0_SUCCESS;

	arg_count = WL_va_count();							/* Determine the number of arguments.	*/
	WL_wtrace("PRINT","ENTRY","File=[%8.8s] args=%d", l_file, arg_count);

	if (arg_count < 2 || arg_count > 9)
	{
		WL_werrlog_error(WERRCODE(46002), "PRINT", "ARGCNT", "Invalid number of arguments [cnt=%d]", arg_count);
		return;
	}

	va_start(the_args, l_file);

	/* ARG2 - lib */ 
	if (arg_count > 2)
	{
		l_lib = va_arg(the_args, char*);					/* Get address of the library name.	*/
	}
	else
	{
		l_lib = "        ";							/* Use a dummy if none found.		*/
	}

	/* ARG3 - vol */ 
	if (arg_count > 3)
	{
		l_vol = va_arg(the_args, char*);					/* Get address of the volume name.	*/
	}
	else
	{
		l_vol = "      ";							/* Blank is default.			*/
	}

	/* ARG4 - mode */ 
	if (arg_count > 4)
	{
		l_mode = va_arg(the_args, char*);					/* Get address of the mode value.	*/

		if ( *l_mode != 'S' && *l_mode != 'H' && *l_mode != 'P')
		{
			WL_wtrace("PRINT","MODE","Unsupported PRINT MODE=[%c]",*l_mode);
			return_code = PRINT_RC_40_INVALID_PARAM;
		}
	}                                                      
	else
	{
		l_mode = "S";								/* Default is spooled.			*/
	}
                                                                                        
	/* ARG5 - disp */ 
	if (arg_count > 5)
	{
		l_disposition = va_arg(the_args, char*);				/* Get the address of the disp. value.	*/
	}
	else
	{
		l_disposition = "DS";							/* Default is Dequeue and save.		*/
	}

	/* ARG6 - copies */ 
	if (arg_count > 6)
	{
		int4_ptr = va_arg(the_args, int4*);					/* Get the address of the copies value.	*/
		num_copies = WL_get_swap(int4_ptr);
	}
	else
	{
		num_copies = 1;
	}
                                                                                        
	/* ARG7 - class */ 
	if (arg_count > 7)
	{
		l_class = *(va_arg(the_args, char*));					/* Get print class value.		*/
		if (l_class == ' ') 
			WL_get_defs(DEFAULTS_PC,&l_class);				/* Use the default.			*/
	}
	else
	{
		WL_get_defs(DEFAULTS_PC,&l_class);					/* Use the default.			*/
	}

	/* ARG8 - form */ 
	if (arg_count > 8)
	{
		int4_ptr = va_arg(the_args, int4*);					/* Get address of the form number.	*/
		form_num = WL_get_swap(int4_ptr);
		if ( form_num == 255 )
			WL_get_defs(DEFAULTS_FN,(char*)&form_num);			/* Use the default.			*/
	}
	else
	{
		WL_get_defs(DEFAULTS_FN,(char*)&form_num);				/* Use the default.			*/
	}
         
	/* ARG9 - returncode */ 
	l_return_code = va_arg(the_args, int4*);					/* Get the address of the return code.	*/
	va_end(the_args);

	if (return_code != PRINT_RC_0_SUCCESS)								/* There is an error so exit PRINT.	*/
	{
		WL_put_swap( l_return_code, return_code );
		return;
	}

	mode = IS_PRINTFILE;
	end_name = WL_wfname(&mode,l_vol,l_lib,l_file,name);				/* now make a name for ourselves...	*/
	*end_name = 0;

	WL_wprint(name,*l_mode,l_disposition,num_copies,l_class,form_num,&return_code);
	WL_put_swap( l_return_code, return_code );
}
/*
**	History:
**	$Log: print.c,v $
**	Revision 1.24  2003/03/19 22:26:19  gsl
**	Standardize PRINT RC defines
**	
**	Revision 1.23  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.22  2003/01/17 18:25:19  gsl
**	Switch PRINT to use stdarg.h
**	
**	Revision 1.21  2003/01/15 21:09:40  gsl
**	document varargs
**	
**	Revision 1.20  2002/12/10 17:09:18  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.19  2002/12/09 21:09:29  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.18  2002/07/16 16:24:54  gsl
**	Globals
**	
**	Revision 1.17  2002/07/12 19:10:15  gsl
**	Global unique WL_ changes
**	
**	Revision 1.16  2002/07/12 17:00:58  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.15  2002/07/11 20:29:11  gsl
**	Fix WL_ globals
**	
**	Revision 1.14  2002/07/10 21:05:21  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.13  2002/06/25 17:46:04  gsl
**	Remove WISPFILEXT as a global, now must go thru set/get routines
**	
**	Revision 1.12  1996/08/19 22:32:39  gsl
**	drcs update
**	
**
**
*/
