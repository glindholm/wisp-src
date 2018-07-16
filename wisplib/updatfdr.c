			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/* UPDATFDR.C ... This routine simulates some of the Wang-VS UPDATFDR subroutine operation.					*/

#include "werrlog.h"
#define		ROUTINE		65000

UPDATFDR()
{
	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/
}



#ifdef OLD
#ifdef VMS
#include <rms.h>
#include <stdio.h>
#include <ssdef.h>
#include <ctype.h>
#include <varargs.h>
#include "wcommon.h"

extern char WISPFILEXT[39];
char *wfname();

UPDATFDR(va_alist)	  								/* Function uses variable arguments.	*/
va_dcl
{
	va_list the_args;
	int arg_count, *l_int, *retcod, access_code;
	char *end_name;
	char *l_vol,*l_lib,*l_file, *l_range, *l_key, *l_value;
	char l_name[80];
	long wfname_mode;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/
	return;		/* doesn't do anythig useful so return.	*/
                    
	va_start(the_args);								/* Set pointer to the top of the stack.	*/
	arg_count = va_count(the_args);
	va_start(the_args);								/* Set pointer to the top of the stack.	*/

	l_range = va_arg(the_args, char*);						/* Get addr. of the range.		*/
	l_file = va_arg(the_args, char*);						/* Get addr. of the filename argument.	*/
	l_lib  = va_arg(the_args, char*);						/* Get addr. of the library argument.	*/
	l_vol  = va_arg(the_args, char*);						/* Get addr. of the volume argument.	*/

	l_key = va_arg(the_args, char*);						/* Get addr. of the first keyword.	*/

	arg_count -= 5;
	wfname_mode = 0;

	if (WISPFILEXT[0] == ' ')							/* Need to use our file extension.	*/
	{
		setwispfilext("DAT");							/* Just copy it in.			*/
	}
	else if (!memcmp(WISPFILEXT,"LIS ",4))						/* Special case for printfiles.		*/
	{
		wfname_mode = IS_PRINTFILE;
	}

	end_name = wfname(&wfname_mode,l_vol,l_lib,l_file,l_name);			/* generate a VMS file name		*/

	*end_name = '\0';								/* Null terminate			*/

	if (access(l_name,0))								/* does the file exist?			*/
	{
		access_code = 20;							/* No, set retcod = 20			*/
		wswap(&access_code);							/* swap the words			*/
	}
	else
	{										/* yes it exists, set retcod = 0	*/
		access_code = 0;
	}

	while (arg_count)
	{
		switch(l_key[0])							/* Determine what the next arg type is.	*/
		{
			case 'F':
			{
				switch(l_key[1])
				{
					case 'C':
					{
						l_value = va_arg(the_args, char*);	/* Address of a value receiver.		*/
						arg_count--;
						/* set file protection */
						break;
					}
					default:
					{
						l_value = va_arg(the_args, char*);	/* Address of a value receiver.		*/
						arg_count--;
						werrlog(ERRORCODE(2),l_key,0,0,0,0,0,0,0);
						break;
					}
				}
				break;
			}
			default:
			{
				l_value = va_arg(the_args, char*);			/* Address of a value receiver.		*/
				arg_count--;
				werrlog(ERRORCODE(2),l_key,0,0,0,0,0,0,0);
				break;
			}
		}
		l_key = va_arg(the_args, char*);					/* Get the next field indicator value.	*/
		arg_count--;
	}       

	retcod = (int *)l_key; 								/* When no more args, it's return code. */

	*retcod = access_code;
}
#endif
#endif                                                                                       
