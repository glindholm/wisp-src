/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
**	File:		filecopy.c
**
**	Project:	wisp/lib
**
**	Purpose:	This routine allows the copying of a file to another file
**
**	Routines:	
**	FILECOPY()	
*/


/*
**	Includes
*/

#include <stdio.h>

#include <string.h>
#include <stdarg.h>                                                                    /* This routine uses a variable number	*/
											/* of arguments.			*/
#include "idsistd.h"
#include "fcopy.h"
#include "wfiles.h"
#include "wcommon.h"
#include "wperson.h"
#include "werrlog.h"
#include "filext.h"
#include "paths.h"
#include "wfname.h"
#include "idsisubs.h"
#include "wisplib.h"
#include "wdefines.h"

/*
	FILECOPY( infile, inlib, invol, outfile, [outlib, [outvol]], retcode )
*/

void FILECOPY(const char* arg1_infile,
	      const char* arg2_inlib,
	      const char* arg3_invol,
	      const char* arg4_outfile,
	      ...)
{
	va_list the_args;
	int arg_count;
	int4 *return_code;
	int4 filecopy_status;								/* Status from the lib call.		*/
	char  *ptr;									/* Pointers to passed arguments.	*/
	char file[9],lib[9],vol[7],new_file[9],new_lib[9],new_vol[7];
	char old_filename[WISP_FILEPATH_LEN];
	char old_filename_idx[WISP_FILEPATH_LEN];
	char new_filename[WISP_FILEPATH_LEN];
	char new_filename_idx[WISP_FILEPATH_LEN];
	int4 mode;
	char *name_end;									/* build filename from wang parts	*/
	int nvalid;									/* Not Valid call flag.			*/
	int has_ext;

	filecopy_status = 0;
	nvalid = 0;

	arg_count = WL_va_count();							/* How many args are there ?		*/

	WL_wtrace("FILECOPY","ENTRY","FILECOPY([%8.8s], [%8.8s], [%6.6s], [%8.8s], ...) args=%d",
		arg1_infile, arg2_inlib, arg3_invol, arg4_outfile, arg_count);

	if (arg_count < 5 || arg_count > 7)
	{
		WL_werrlog(WERRCODE(18202),"","","",0,0,0,0,0);				/* Invalid arguments.			*/
		return;
	}

	strcpy(file,"        ");
	strcpy(lib, "        ");
	strcpy(vol, "      ");
	strcpy(new_file,file);
	strcpy(new_lib, lib);
	strcpy(new_vol, vol);


	memcpy(file, arg1_infile, 8);
	if (0==memcmp(file,"        ",8))
	{
		nvalid = 1;								/* Invalid: no file name		*/
	}

	memcpy(lib, arg2_inlib, 8);
	if (0==memcmp(lib,"        ",8))						/* If the library is not specified	*/
	{										/* then use defualt INLIB.		*/
		WL_get_defs(DEFAULTS_IL,lib);
	}

	memcpy(vol,arg3_invol,6);
	if (0==memcmp(vol,"      ",6))				      			/* If the volume is not specified	*/
	{										/* then use defualt INVOL.		*/
		WL_get_defs(DEFAULTS_IV,vol);
		if (0==memcmp(vol,"      ",6))						/* Must specify a volume.		*/
		{
			nvalid = 1;
			filecopy_status = 4;
		}         
	}

	memcpy(new_file, arg4_outfile, 8);
	if (0==memcmp(new_file,"        ",8)) 						/* If the new file is not specified.	*/
	{
		memcpy(new_file,file,8);
	}

	va_start(the_args, arg4_outfile);						/* Setup varargs */

	if (arg_count>5)
	{
		ptr = va_arg(the_args, char*);						/* Get addr. of the new library name.	*/
		memcpy(new_lib,ptr,8);
	}
	if (0==memcmp(new_lib,"        ",8)) memcpy(new_lib,lib,8);			/* If the new lib not specified.	*/

	if (arg_count>6)
	{
		ptr = va_arg(the_args, char*);						/* Get addr. of the new volume name.	*/
		memcpy(new_vol,ptr,6);
	}
	if (0==memcmp(new_vol,"      ",6)) memcpy(new_vol,vol,6);			/* If the new vol not specified.	*/

	return_code = va_arg(the_args, int4*);						/* Get the addr. of the return code.	*/

	va_end(the_args);

	if (nvalid)
	{
		if (nvalid == 1) werrlog(WERRCODE(18202),file,lib,vol,0,0,0,0,0);	/* Invalid arguments.			*/
		else if (nvalid == 2) werrlog(WERRCODE(18204),file,lib,vol,0,0,0,0,0);	/* Not yet supported.			*/

		if (!filecopy_status) filecopy_status = 44;
		goto filecopy_return;
	}

	wtrace("FILECOPY","ARGS","Oldfile=[%8.8s %8.8s %6.6s] Newfile=[%8.8s %8.8s %6.6s]",
	       file, lib, vol, new_file, new_lib, new_vol);

	mode = 0;

	name_end = WL_wfname(&mode, vol, lib, file, old_filename);      		/* Construct the native filename.	*/
	*name_end = '\0'; 								/* null terminate it			*/

	mode = IS_OUTPUT;

	ptr = splitext(old_filename);
	if (*ptr == '.') ptr++;								/* Point past the '.'			*/
	WSETFILEXT(ptr);								/* Reset EXT for new_filename		*/

	if (*new_file == '#')
	{
		filecopy_status = 56;
		goto filecopy_return;
	}

	name_end = WL_wfname(&mode, new_vol, new_lib, new_file, new_filename);		/* Construct the new native filename.	*/
	*name_end = '\0'; 								/* null terminate it			*/



	/*
	**	Make sure the file extensions are the same.
	*/
	{
		char	*ext_old, *ext_new;

		ext_old = WL_osd_ext(old_filename);
		ext_new = WL_osd_ext(new_filename);

		if ( 	(ext_old && ext_new && 0 != strcmp(ext_old,ext_new)) ||
			(ext_old && !ext_new) ||
			(!ext_old && ext_new)   )
		{
			/*
			**	Remove extension from new filename
			*/
			if (ext_new)
			{
				ext_new--;
				*ext_new = (char)0;
			}

			/*
			**	If old filename has an extension then copy it to the new filename.
			*/
			if (ext_old)
			{
				strcat(new_filename,".");
				strcat(new_filename,ext_old);
			}
		}
	}

	if (0!=makepath(new_filename))							/* Ensure directories are created	*/
	{
		filecopy_status=24;
		goto filecopy_return;
	}

	/*
	**	Have to handle these cases
	**
	**	1) Vision files	
	**		foo
	**
	**	2) CISAM 
	**		foo 
	**		foo.idx
	**
	**	3) CISAM (with .dat)
	**		foo.dat
	**		foo.idx
	**
	**	4) Vision4
	**		foo [.dat]
	**		foo.vix
	*/

	has_ext = hasext(old_filename);

	/*
	**	Check if there is an index portion and setup paths to copy it.
	*/
	strcpy(old_filename_idx, old_filename);
	strcpy(new_filename_idx, new_filename);
	if (has_ext)
	{
		if ((ptr = strrchr(old_filename_idx,'.'))) *ptr = '\0';		/* cut off the extension			*/
		if ((ptr = strrchr(new_filename_idx,'.'))) *ptr = '\0';
	}
	strcat(old_filename_idx,".idx");
	strcat(new_filename_idx,".idx");
	if (!fexists(old_filename_idx))						/* See if there is a .idx index portion		*/
	{
		if ((ptr = strrchr(old_filename_idx,'.'))) *ptr = '\0';		/* cut off the extension			*/
		if ((ptr = strrchr(new_filename_idx,'.'))) *ptr = '\0';

		strcat(old_filename_idx,".vix");
		strcat(new_filename_idx,".vix");
		if (!fexists(old_filename_idx))					/* See if there is a .vix index portion		*/
		{
			/* No index portion */
			*old_filename_idx = '\0';
			*new_filename_idx = '\0';
		}
	}

	/*
	**	Check if file exists
	*/
	if (!fexists(old_filename))
	{
		if (has_ext)
		{
			/*
			**	File doesn't exist and it has an extension so nothing else to try.
			*/
			filecopy_status=20;
			goto filecopy_return;
		}

		/*
		**	Try a .dat extension
		*/
		strcat(old_filename,".dat");
		strcat(new_filename,".dat");
		
		if (!fexists(old_filename))
		{
			/*
			**	File doesn't exist
			*/
			filecopy_status=20;
			goto filecopy_return;
		}
	}
	
	/*
	**	The old file exists.
	**	Ensure the new file doesn't already exist.
	*/
	if (fexists(new_filename))
	{
		filecopy_status = 52;
		goto filecopy_return;
	}
	if (*new_filename_idx && fexists(new_filename_idx))
	{
		filecopy_status = 52;
		goto filecopy_return;
	}

	/*
	**	Do the copy
	*/
	if (wisp_fcopy(old_filename,new_filename))
	{
		filecopy_status=24;
		goto filecopy_return;
	}
	if (*old_filename_idx)
	{
		if (wisp_fcopy(old_filename_idx,new_filename_idx))
		{
			/*
			**	The idx copy failed, try to undo the data portion copy before returning
			*/
			wisp_unlink(new_filename);
			
			filecopy_status=24;
			goto filecopy_return;
		}
	}
	

                               
filecopy_return:

	wtrace("FILECOPY","RETURN","Return Code = [%d]", filecopy_status);
	
	WL_put_swap(return_code, filecopy_status);
}                                                         


/*
**	History:
**	$Log: filecopy.c,v $
**	Revision 1.31  2003/01/31 21:24:13  gsl
**	fix -Wall warnings
**	
**	Revision 1.30  2003/01/31 17:23:48  gsl
**	Fix  copyright header
**	
**	Revision 1.29  2003/01/16 19:41:27  gsl
**	WL_werrlog
**	
**	Revision 1.28  2003/01/16 17:27:44  gsl
**	Switch to stdarg
**	
**	Revision 1.27  2002/12/11 17:03:06  gsl
**	use wisp_unlink()
**	
**	Revision 1.26  2002/12/10 20:54:15  gsl
**	use WERRCODE()
**	
**	Revision 1.25  2002/12/09 21:09:27  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.24  2002/10/18 19:14:11  gsl
**	Cleanup
**	
**	Revision 1.23  2002/07/29 15:46:50  gsl
**	getwfilext -> WGETFILEXT
**	setwfilext -> WSETFILEXT
**	setwispfilext -> WSETFILEXT
**	
**	Revision 1.22  2002/07/16 16:24:56  gsl
**	Globals
**	
**	Revision 1.21  2002/07/12 19:10:11  gsl
**	Global unique WL_ changes
**	
**	Revision 1.20  2002/07/12 17:00:55  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.19  2002/07/11 20:29:07  gsl
**	Fix WL_ globals
**	
**	Revision 1.18  2002/07/11 14:33:59  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.17  2002/07/10 21:05:16  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.16  2002/06/26 01:41:13  gsl
**	Change fcopy() to wisp_fcopy()
**	
**	Revision 1.15  2002/06/25 17:46:03  gsl
**	Remove WISPFILEXT as a global, now must go thru set/get routines
**	
**	Revision 1.14  2002/06/21 03:10:35  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.13  1998/05/19 21:57:26  gsl
**	fix warning on WIN32
**	
**	Revision 1.12  1998-05-15 17:20:09-04  gsl
**	Add support for vision4 plus trace logic
**
**	Revision 1.11  1997-03-12 12:55:00-05  gsl
**	Changed define to WIN32
**
**	Revision 1.10  1996-07-08 12:50:05-04  gsl
**	fix warning
**
**	Revision 1.9  1996-06-27 17:13:27-07  gsl
**	Reuse the unix and MSDOS code for NT.
**
**	Revision 1.8  1995-04-27 08:51:29-07  gsl
**	Check if the new file already exists and return 52 if it does.
**
 * Revision 1.7  1995/04/25  09:52:41  gsl
 * drcs state V3_3_15
 *
 * Revision 1.6  1995/04/17  11:46:05  gsl
 * drcs state V3_3_14
 *
 * Revision 1.5  1995/03/10  13:48:38  gsl
 * fix header
 *
**
**
*/
