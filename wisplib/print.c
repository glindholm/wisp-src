			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include necessary header files.							*/

#include <stdio.h>
#include "wcommon.h"
#include "wperson.h"
#include "movebin.h"
#include "werrlog.h"
#include <varargs.h>

extern char WISPFILEXT[39];
extern char werrlog_path[80];

/* 				PRINT a file.											*/
#define		ROUTINE		46000
PRINT(va_alist)										/* Variable number of arguments.	*/
va_dcl											/* Define the list structure.		*/
{
	va_list the_args;								/* Define a pointer to the list.	*/
	int	arg_count, x, i;							/* Number of arguments.			*/
	int	num_copies,form_num,return_code;
	long	l_val;									/* to move int to long value.		*/

	char *l_file,									/* Address of the filename.		*/
	     *l_lib,									/* Address of the library name.		*/
	     *l_vol,									/* Address of the volume name.		*/
	     *l_mode,									/* Address of the job print mode.	*/
	     *l_disposition,								/* Address of the disposition action.	*/
	     *l_class,									/* Address of the print class		*/
	      dummy_arg;								/* Used to burn off extra args.		*/

	long	*l_return_code,								/* Address of 4 byte return code value.	*/
		l_copies,								/* Local number of copys.		*/
		*long_ptr,
		l_form;									/* Local form number.			*/

	char sav_mode, name[132], *end_name;
	char *wfname();									/* wfname returns a point to a string.	*/
	long mode;

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
		long_ptr = va_arg(the_args, long*);					/* Get the address of the copies value.	*/
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
		l_class = va_arg(the_args, char*);					/* Get address of print class value.	*/
		x--;									/* One less argument.			*/
		if (*l_class == ' ') l_class = &defaults.prt_class;			/* Use the default.			*/
	}
	else
	{
		l_class = &defaults.prt_class;						/* Use the default.			*/
	}

	if (x > 1)									/* Are we NOT at the last arg ?		*/
	{
		long_ptr = va_arg(the_args, long*);					/* Get address of the form number.	*/
		GETBIN(&l_form,long_ptr,4);
		l_val = l_form;
		wswap(&l_val);
		form_num = (int)l_val;
		x--;									/* One less arg.			*/
		if ( form_num == 255 ) form_num = defaults.prt_form;
	}
	else
	{
		form_num = defaults.prt_form;						/* Use the default.			*/
	}
         
	l_return_code = va_arg(the_args, long*);					/* Get the address of the return code.	*/

	if (return_code)								/* There is an error so exit PRINT.	*/
	{
		l_val = (long) return_code;
		wswap(&l_val);
		PUTBIN(l_return_code,&l_val,4);	
		return(0);
	}
	sav_mode = defaults.prt_mode;							/* Save the default print mode.		*/
	defaults.prt_mode = *l_mode;							/* Set the current mode to whatever...	*/

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
	defaults.prt_mode = sav_mode;							/* Restore the print mode default.	*/

	wprint(name,*l_mode,l_disposition,num_copies,*l_class,form_num,&return_code);
	l_val = (long)return_code;
	wswap(&l_val);
	PUTBIN(l_return_code,&l_val,4);	
}
