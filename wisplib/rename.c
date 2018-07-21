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
**	File:		rename.c
**
**	Purpose:	To hold RENAME vssub.
**
**	Routines:	
**	RENAME()	The emulation of vssub RENAME.
**
**
*/


#include <stdio.h>
#include <string.h>
#include <stdarg.h>                                                                    /* This routine uses a variable number	*/
											/* of arguments.			*/
#include "idsistd.h"
#include "wfname.h"
#include "wfiles.h"
#include "wcommon.h"
#include "wperson.h"
#include "werrlog.h"
#include "idsisubs.h"
#include "wisplib.h"
#include "filext.h"
#include "wdefines.h"
#include "vssubs.h"

void WL_rename_va_list(const char* rtype, char* file, char* lib, char* vol, va_list the_args);


/*
**	CALL "RENAME" USING 'G', FILE, LIBRARY, VOLUME, NEWFILE [,NEWLIB [,BYPASS [,ACCESS [,OPEN]]]] RETCODE
**	CALL "RENAME" USING 'F', FILE, LIBRARY, VOLUME, NEWFILE          [,BYPASS [,ACCESS [,OPEN]]]  RETCODE
**	CALL "RENAME" USING 'L', xxxx, LIBRARY, VOLUME,           NEWLIB [,BYPASS [,ACCESS [,OPEN]]]  RETCODE
**
**		(1) TYPE 	Alpha(1) 'G', 'L', 'F'
**		(2) FILE	Alpha(8)
**		(3) LIBRARY	Alpha(8)
**		(4) VOLUME	Alpha(6)
**		(5) NEWFILE	Alpha(8)
**		(6) NEWLIB	Alpha(8) (Optional)
**		(7) BYPASS	Alpha(1) N/A (Optional)
**		(8) ACCESS	Alpha(1) N/A (Optional)
**		(9) OPEN	Alpha(1) N/A (Optional)
**		(10) RETCODE	Int(4)
*/

void RENAME(const char* type, char* file, char* lib, char* vol, ...)
{
	va_list the_args;
	va_start(the_args, vol);
	WL_rename_va_list(type, file, lib, vol, the_args);
	va_end(the_args);
}

void WL_rename_va_list(const char* rtype, char* file, char* lib, char* vol, va_list the_args)
{
	int arg_count, *return_code;							/* A variable and a pointer.		*/
	int4 rename_status;								/* Status from the lib call.		*/
	char *the_item, *new_file, *new_lib;
	char old_filename[WISP_FILEPATH_LEN];
	char old_filename_idx[WISP_FILEPATH_LEN];
	char new_filename[WISP_FILEPATH_LEN];
	char new_filename_idx[WISP_FILEPATH_LEN];
	int4 mode;
	char *name_end;									/* build filename from wang parts	*/
	int nvalid;									/* Not Valid call flag.			*/
	int dat_done, has_ext;
	char *ptr;
	int	lib_rename;


	rename_status = RENAME_RC_0_SUCCESS;
	lib_rename = 0;

	/* va_start(the_args);	*/
	arg_count = WL_va_count();							/* How many args are there ?		*/

	WL_wtrace("RENAME","ENTRY","Entry into RENAME(type=[%c], file=[%8.8s], lib=[%8.8s], vol=[%6.6s], ...) args=%d", 
		*rtype, file, lib, vol, arg_count);

	if (arg_count < 6 || arg_count > 10)
	{
		WL_werrlog_error(WERRCODE(53000),"RENAME", "ARGS", 
			"Invalid number of arguments [%d], valid range is 6 to 10.", 
			arg_count);
		return;
	}

	nvalid = 0;
	
	/* ARG1 - TYPE */
	arg_count--;

	if (!strrchr("FLG",*rtype))							/* Check to see if fund type is valid.	*/
	{
		nvalid = 1;								/* Set not valid so doesn't try.	*/
		rename_status = RENAME_RC_44_INVALID_PARAMETER;				/* Invalid func type.			*/
	}

	/* ARG2 - FILE */
	arg_count--;

	if (*rtype == 'F' && !strncmp(file,"        ",8))				/* If file rename and file not specified*/
	{
		nvalid = 1;								/* Set to invalid call.			*/
		rename_status = RENAME_RC_20_FILE_NOT_FOUND;
	}

	/* ARG3 - LIBRARY */
	arg_count--;									/* One less argument.			*/

	if (!strncmp(lib,"        ",8))							/* If the library is not specified	*/
	{										/* then use defualt INLIB.		*/
		WL_get_defs(DEFAULTS_IL,lib);
	}

	/* ARG4 - VOLUME */
 	arg_count--;									/* One less argument.			*/

	if (!strncmp(vol,"      ",6))							/* If the volume is not specified	*/
	{										/* then use defualt INVOL.		*/
		WL_get_defs(DEFAULTS_IV,vol);
		if (!strncmp(vol,"      ",6))						/* Must specify a volume.		*/
		{
			nvalid = 1;
			rename_status = RENAME_RC_4_VOLUME_NOT_FOUND;
		}         
	}

	/* ARG5 - NEWFILE  (omitted for TYPE='L') */
	if (*rtype != 'L')								/* If not doing a library rename.	*/
	{
		new_file = va_arg(the_args, char*);					/* Get addr. of the new file name.	*/
		arg_count--;								/* One less argument.			*/

		if (*rtype == 'F')
		{
			if (!strncmp(new_file,"        ",8))				/* If the new file is not specified.	*/
			{								/* Must specify file if type F.		*/
				nvalid = 3;
				rename_status = RENAME_RC_56_NEW_NAME_INVALID;		/* New file is invalid.			*/
			}
		}
		else if (!strncmp(new_file,"        ",8)) new_file = file;		/* If the new file is not specified.	*/

		if ('#' == new_file[0])
		{
				nvalid = 3;
				rename_status = RENAME_RC_56_NEW_NAME_INVALID;		/* New file is invalid.			*/
		}
	}
	else	new_file = file;							/* Use old file.			*/

	/* ARG6 - NEWLIB  (omitted for TYPE='F') */
	if ((arg_count > 1) && (*rtype != 'F'))						/* Get the new lib name if needed.	*/
	{
		new_lib = va_arg(the_args, char*);					/* Get addr. of the new library name.	*/
		arg_count--;								/* One less arg.			*/

		if (*rtype == 'L')
		{
			if (!strncmp(new_lib,"        ",8))				/* If the new lib is not specified.	*/
			{								/* Must specify new lib.		*/
				nvalid = 3;
				rename_status = RENAME_RC_44_INVALID_PARAMETER;		/* New lib is invalid.			*/
			}
			else if (!strncmp(lib,new_lib,8))				/* Did not specify a diff new lib name.	*/
			{
				nvalid = 3;
				rename_status = RENAME_RC_52_ALREADY_EXISTS;
			}
		}
		else if (!strncmp(new_lib,"        ",8)) new_lib = lib;			/* If the new lib not specified.	*/ 
	}
	else	new_lib = lib;								/* Use old library name.		*/

	/* ARG7 - BYPASS  (Optional) */
	if (arg_count > 1)
	{
		the_item = va_arg(the_args, char*);
		arg_count -= 1;
	}

	/* ARG8 - ACCESS  (Optional) */
	if (arg_count > 1)
	{
		the_item = va_arg(the_args, char*);
		arg_count -= 1;
	}

	/* ARG9 - OPEN  (Optional) */
	if (arg_count > 1)
	{
		the_item = va_arg(the_args, char*);
		arg_count -= 1;
	}

	/* ARG10 - RETCODE */
	if (1 == arg_count)
	{
		return_code = va_arg(the_args, int*);					/* Get the addr. of the return code.	*/
		arg_count -= 1;
	}
	else if (arg_count < 1)
	{
		WL_werrlog_error(WERRCODE(53000),"RENAME", "ARGS", 
			"Invalid argument list, missing argument RETCODE.");
		return;
	}
	else
	{
		WL_werrlog_error(WERRCODE(53000),"RENAME", "ARGS", 
			"Invalid argument list, extra arguments found.");
		return;
	}

	wtrace("RENAME", "ARGS", "(resolved) Type=[%c] File=[%8.8s] Lib=[%8.8s] Vol=[%6.6s] NFile=[%8.8s] NLib=[%8.8s]",
				*rtype, file, lib, vol, new_file, new_lib);

	if (nvalid)
	{
		if (RENAME_RC_0_SUCCESS == rename_status) 
		{
			rename_status = RENAME_RC_44_INVALID_PARAMETER;
		}
		goto rename_return;
	}

	if (!strncmp(file,"        ",8) || *rtype == 'L')				/* Blank file or type=L means lib mode.	*/
	{
		lib_rename = 1;
	}

	if (lib_rename) 								/* Doing a library rename.		*/
	{
		name_end = WL_wanglib2path(vol, lib, old_filename);
		*name_end = (char)0;
	}
	else
	{
		mode = 0;
		name_end = WL_wfname(&mode, vol, lib, file, old_filename);        		/* Construct the native filename.	*/
		*name_end = (char)0;
	}

	if (lib_rename)									/* wfname leaves the trailing '/' on	*/
	{
		old_filename[strlen(old_filename)-1] = (char)0;				/* Remove trailing separator		*/
	}

	

	if (lib_rename) 
	{
		name_end = WL_wanglib2path(vol, new_lib, new_filename);
		*name_end = (char)0;
	}
	else										/* If not a library			*/
	{
		ptr = splitext(old_filename);
		if (*ptr == '.') ptr++;							/* Point past the '.'			*/
		WSETFILEXT(ptr);							/* Reset EXT for new_filename		*/

		mode = IS_OUTPUT;
		name_end = WL_wfname(&mode, vol, new_lib, new_file, new_filename);	/* Construct the new native filename.	*/
		*name_end = (char)0;
	}



	if (lib_rename)									/* wfname leaves the trailing '/' on	*/
	{
		new_filename[strlen(new_filename)-1] = (char)0;				/* Remove trailing separator		*/
	}

	if ( !lib_rename && 0 != strcmp(lib,new_lib) )					/* If file ren && lib is different.	*/
	{
		if ( 0!=makepath(new_filename))
		{
			rename_status = RENAME_RC_24_ACCESS_DENIED;
			goto rename_return;
		}
	}

		/*
		**	Have to handle 3 cases
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
		**	4) Vision 4 files
		**		foo
		**		foo.vix
		*/

	has_ext = hasext(old_filename);						/* does it already have extension?		*/

	/*
	**	Check if there is an index portion and setup paths to rename it.
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
			rename_status=RENAME_RC_20_FILE_NOT_FOUND;		/* yes, so return file not found		*/
			goto rename_return;
		}

		/*
		**	Try a .dat extension
		*/
		strcat(old_filename,".dat");
		strcat(new_filename,".dat");
		
		if (!fexists(old_filename))
		{
			if (lib_rename)
			{
				rename_status=RENAME_RC_16_LIBRARY_NOT_FOUND;	/* Lib does not exist */
			}
			else
			{
				rename_status=RENAME_RC_20_FILE_NOT_FOUND;	/* file does not exist */
			}
			goto rename_return;
		}
	}
	
	/*
	**	The old file exists.
	**	Ensure the new file doesn't already exist.
	*/
	if (fexists(new_filename))
	{
		rename_status = RENAME_RC_52_ALREADY_EXISTS;
		goto rename_return;
	}
	if (*new_filename_idx && fexists(new_filename_idx))
	{
		rename_status = RENAME_RC_52_ALREADY_EXISTS;
		goto rename_return;
	}

	/*
	**	Do the rename
	*/
	if (WL_rename(old_filename,new_filename))
	{
		rename_status=RENAME_RC_24_ACCESS_DENIED;
		goto rename_return;
	}
	if (*old_filename_idx)
	{
		if (WL_rename(old_filename_idx,new_filename_idx))
		{
			/*
			**	The idx rename failed, try to undo the data portion rename before returning
			*/
			if (WL_rename(new_filename,old_filename))
			{
				rename_status=RENAME_RC_48_IO_ERROR;	/* Serious error, half the file was renamed */
				goto rename_return;
			}
			
			rename_status=RENAME_RC_24_ACCESS_DENIED;
			goto rename_return;
		}
	}
	
	dat_done = 0;

                               
rename_return:
	wtrace("RENAME", "RETURN", "Return code = %ld errno=[%d]", (long)rename_status, errno);
	
	WL_put_swap(return_code, rename_status);
}                                                         

int WL_rename(const char* old_filename, const char* new_filename)
{
	int rc = 0;
	int save_errno = 0;
	errno = 0;
	rc = rename(old_filename,new_filename);
	save_errno = errno;
	WL_wtrace("rename","return","Old=[%s] New=[%s] rc=[%d] errno=[%d]",
		old_filename, new_filename, rc, save_errno);
	errno = save_errno;
	return rc;
}

/*
**	History:
**	$Log: rename.c,v $
**	Revision 1.42  2003/08/04 14:40:18  gsl
**	remove obsolete code
**	
**	Revision 1.41  2003/02/04 17:05:01  gsl
**	Fix -Wall warnings
**	
**	Revision 1.40  2003/01/31 18:54:38  gsl
**	Fix copyright header
**	
**	Revision 1.39  2003/01/30 21:11:22  gsl
**	Change RENAME to use stdarg.h
**	
**	Revision 1.38  2003/01/30 19:43:24  gsl
**	Change RENAME to use vssubs.h and define all the return codes
**	WL_file_rename() changed to WL_rename()
**	
**	Revision 1.37  2003/01/30 15:20:57  gsl
**	WL_rename() change args to const and add tracing
**	
**	Revision 1.36  2003/01/29 20:45:49  gsl
**	comments
**	
**	Revision 1.35  2002/12/10 17:09:17  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.34  2002/12/09 19:15:32  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.33  2002/10/18 19:14:09  gsl
**	Cleanup
**	
**	Revision 1.32  2002/07/29 15:46:50  gsl
**	getwfilext -> WGETFILEXT
**	setwfilext -> WSETFILEXT
**	setwispfilext -> WSETFILEXT
**	
**	Revision 1.31  2002/07/23 21:24:56  gsl
**	wrename -> RENAME
**	
**	Revision 1.30  2002/07/23 21:09:09  gsl
**	wrename -> RENAME
**	
**	Revision 1.29  2002/07/23 20:49:50  gsl
**	globals
**	
**	Revision 1.28  2002/07/22 20:32:34  gsl
**	Unix commands put filenames in 'quotes' because they can contain $
**	
**	Revision 1.27  2002/07/16 16:24:53  gsl
**	Globals
**	
**	Revision 1.26  2002/07/12 19:10:15  gsl
**	Global unique WL_ changes
**	
**	Revision 1.25  2002/07/12 17:01:00  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.24  2002/07/11 20:29:12  gsl
**	Fix WL_ globals
**	
**	Revision 1.23  2002/07/10 21:05:22  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.22  2002/06/28 04:02:59  gsl
**	Work on native version of wfopen and wfname
**	
**	Revision 1.21  2002/06/26 06:26:21  gsl
**	Mode/status bit field changes
**	
**	Revision 1.20  2002/06/25 17:46:04  gsl
**	Remove WISPFILEXT as a global, now must go thru set/get routines
**	
**	Revision 1.19  2002/06/21 03:10:39  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.18  1998/11/30 17:24:43  gsl
**	Add beter error checked to detect missing arguments and document args
**	
**	Revision 1.17  1998-08-03 17:10:18-04  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks.
**
**	Revision 1.16  1998-05-15 14:15:58-04  gsl
**	Add support for vision4 files and changed unix to use rename() routine.
**	I reworked the way 2 part files (cisam, vision4) are handled and left
**	the old code ifdef'ed for reference
**
**	Revision 1.15  1997-05-01 16:39:50-04  gsl
**	Remove unneeded errbuff
**
**	Revision 1.14  1997-04-15 23:10:20-04  gsl
**	Update to use wtrace()
**
**	Revision 1.13  1997-03-12 13:11:43-05  gsl
**	change to use WIN32 define
**
**	Revision 1.12  1996-08-19 18:32:49-04  gsl
**	drcs update
**
**
**
*/
