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

/*						Include necessary header files.							*/

#include <stdio.h>
#include <varargs.h>

#include "idsistd.h"
#include "wcommon.h"
#include "wperson.h"
#include "movebin.h"
#include "werrlog.h"
#include "wfname.h"
#include "wisplib.h"
#include "filext.h"


/* 				PRINT a file.											*/
#define		ROUTINE		46000
void PRINT(va_alist)										/* Variable number of arguments.	*/
va_dcl											/* Define the list structure.		*/
{
	va_list the_args;								/* Define a pointer to the list.	*/
	int4	arg_count, x;							/* Number of arguments.			*/
	int4	num_copies,form_num,return_code;
	int4	l_val;									/* to move int to int4 value.		*/

	char *l_file,									/* Address of the filename.		*/
	     *l_lib,									/* Address of the library name.		*/
	     *l_vol,									/* Address of the volume name.		*/
	     *l_mode,									/* Address of the job print mode.	*/
	     *l_disposition,								/* Address of the disposition action.	*/
	     l_class;									/* print class				*/

	int4	*l_return_code,								/* Address of 4 byte return code value.	*/
		l_copies,								/* Local number of copys.		*/
		*long_ptr,
		l_form;									/* Local form number.			*/

	char name[132], *end_name;
	int4 mode;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	return_code = 0;
	wpload();									/* Make sure usage constants are loaded.*/

	va_start(the_args);								/* Set pointer to top of stack.		*/
	arg_count = va_count(the_args);							/* Determine the number of arguments.	*/
	x = arg_count;									/* Set a working variable.		*/
	va_start(the_args);								/* Reset pointer to top of stack.	*/ 

	l_file = va_arg(the_args, char*);						/* Get address of the filename.		*/
	x--;										/* One less argument.			*/
                                                                                        
	if (x > 1)									/* Are we NOT at the last arg ?		*/
	{
		l_lib = va_arg(the_args, char*);					/* Get address of the library name.	*/
		x--;									/* One less argument.			*/
	}
	else
	{
		l_lib = "        ";							/* Use a dummy if none found.		*/
	}

	if (x > 1)									/* Are we NOT at the last arg ?		*/
	{
		l_vol = va_arg(the_args, char*);					/* Get address of the volume name.	*/
		x--;									/* One last argument.			*/
	}
	else
	{
		l_vol = "      ";							/* Blank is default.			*/
	}

	if (x > 1)									/* Are we NOT at the last arg ?		*/
	{
		l_mode = va_arg(the_args, char*);					/* Get address of the mode value.	*/
		x--;									/* One less argument.			*/

		if ( *l_mode != 'S' && *l_mode != 'H' && *l_mode != 'P')
		{
			werrlog(ERRORCODE(3),*l_mode,0,0,0,0,0,0,0);
			return_code = 40;
		}
	}                                                      
	else
	{
		l_mode = "S";								/* Default is spooled.			*/
	}
                                                                                        
	if (x > 1)									/* Are we NOT at the last arg ?		*/
	{
		l_disposition = va_arg(the_args, char*);				/* Get the address of the disp. value.	*/
		x--;									/* One less argument.			*/
	}
	else
	{
		l_disposition = "DS";							/* Default is Dequeue and save.		*/
	}

	if (x > 1)									/* Are we NOT at the last arg ?		*/
	{
		long_ptr = va_arg(the_args, int4*);					/* Get the address of the copies value.	*/
		GETBIN(&l_copies,long_ptr,4);
		l_val = l_copies;							/* Now the value.			*/
		wswap(&l_val);								/* Correct the word order.		*/
		num_copies = (int)l_val;
		x--;									/* One less argument.			*/
	}
	else
	{
		num_copies = 1;
	}
                                                                                        
	if (x > 1)									/* Are we NOT at the last arg ?		*/
	{
		l_class = *(va_arg(the_args, char*));					/* Get print class value.		*/
		x--;									/* One less argument.			*/
		if (l_class == ' ') 
			get_defs(DEFAULTS_PC,&l_class);					/* Use the default.			*/
	}
	else
	{
		get_defs(DEFAULTS_PC,&l_class);						/* Use the default.			*/
	}

	if (x > 1)									/* Are we NOT at the last arg ?		*/
	{
		long_ptr = va_arg(the_args, int4*);					/* Get address of the form number.	*/
		GETBIN(&l_form,long_ptr,4);
		l_val = l_form;
		wswap(&l_val);
		form_num = (int)l_val;
		x--;									/* One less arg.			*/
		if ( form_num == 255 )
			get_defs(DEFAULTS_FN,(char*)&form_num);				/* Use the default.			*/
	}
	else
	{
		get_defs(DEFAULTS_FN,(char*)&form_num);					/* Use the default.			*/
	}
         
	l_return_code = va_arg(the_args, int4*);					/* Get the address of the return code.	*/

	if (return_code)								/* There is an error so exit PRINT.	*/
	{
		l_val = (int4) return_code;
		wswap(&l_val);
		PUTBIN(l_return_code,&l_val,4);	
		return;
	}

	if ((WISPFILEXT[0] == ' ') || !memcmp(WISPFILEXT,"LIS ",4))			/* If it's a listing file.		*/
	{
		mode = IS_PRINTFILE;							/* Set the 4th bit.			*/
	}
	else
	{
		mode = 0;
	}

	end_name = wfname(&mode,l_vol,l_lib,l_file,name);				/* now make a name for ourselves...	*/
	*end_name = 0;

	wprint(name,*l_mode,l_disposition,num_copies,l_class,form_num,&return_code);
	l_val = (int4)return_code;
	wswap(&l_val);
	PUTBIN(l_return_code,&l_val,4);	
}
/*
**	History:
**	$Log: print.c,v $
**	Revision 1.12  1996/08/19 22:32:39  gsl
**	drcs update
**	
**
**
*/
