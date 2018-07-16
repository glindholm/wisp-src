/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
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
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/

/*
**	File:		rename.c
**
**	Purpose:	To hold RENAME vssub.
**
**	RCS:		$Source:$
**
**	Routines:	
**		wrename()		The emulation of vssub RENAME (translation done by WISP).
**
**
*/


#include <stdio.h>
#include <string.h>
#include <varargs.h>                                                                    /* This routine uses a variable number	*/
											/* of arguments.			*/
#include "idsistd.h"
#include "wfname.h"
#include "wfiles.h"
#include "wcommon.h"
#include "movebin.h"
#include "wperson.h"
#include "werrlog.h"
#include "cobrun.h"
#include "idsisubs.h"
#include "wisplib.h"
#include "filext.h"
#include "wdefines.h"

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
void wrename(va_alist)
va_dcl
{
#define	ROUTINE		53000
	va_list the_args;								/* A pointer to traverse the stack.	*/
	int arg_count, *return_code;							/* A variable and a pointer.		*/
	int4 rename_status;								/* Status from the lib call.		*/
	char *rtype,*the_item, *file, *lib, *vol, *new_file, *new_lib;			/* Pointers to passed arguments.	*/
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


	rename_status = 0;
	lib_rename = 0;

	va_start(the_args);								/* Set pointer to top of stack.		*/
	arg_count = va_count(the_args);							/* How many args are there ?		*/
	va_start(the_args);								/* Go back to the top of the stack.	*/

	if (arg_count < 6 || arg_count > 10)
	{
		char msg[256];
		sprintf(msg,"%%RENAME-E-ARGS Invalid number of arguments [%d], valid range is 6 to 10.", arg_count);
		werrlog(104, msg, 0,0,0,0,0,0,0,0);
		
		return;
	}

	nvalid = 0;
	
	/* ARG1 - TYPE */
	rtype = va_arg(the_args, char*);						/* Get the rename type code.		*/
	arg_count--;									/* One less argument.			*/

	if (!strrchr("FLG",*rtype))							/* Check to see if fund type is valid.	*/
	{
		nvalid = 1;								/* Set not valid so doesn't try.	*/
		rename_status = 44;							/* Invalid func type.			*/
	}

	/* ARG2 - FILE */
	file = va_arg(the_args, char*);							/* Get addr. of the file.		*/
	arg_count--;									/* One less argument.			*/

	if (*rtype == 'F' && !strncmp(file,"        ",8))				/* If file rename and file not specified*/
	{
		nvalid = 1;								/* Set to invalid call.			*/
		rename_status = 20;
	}

	/* ARG3 - LIBRARY */
	lib = va_arg(the_args, char*);							/* Get addr. of the lib.		*/
	arg_count--;									/* One less argument.			*/

	if (!strncmp(lib,"        ",8))							/* If the library is not specified	*/
	{										/* then use defualt INLIB.		*/
		get_defs(DEFAULTS_IL,lib);
	}

	/* ARG4 - VOLUME */
	vol = va_arg(the_args, char*);	   						/* Get addr. of the vol.		*/
 	arg_count--;									/* One less argument.			*/

	if (!strncmp(vol,"      ",6))							/* If the volume is not specified	*/
	{										/* then use defualt INVOL.		*/
		get_defs(DEFAULTS_IV,vol);
		if (!strncmp(vol,"      ",6))						/* Must specify a volume.		*/
		{
			nvalid = 1;
			rename_status = 4;
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
				rename_status = 56;					/* New file is invalid.			*/
			}
		}
		else if (!strncmp(new_file,"        ",8)) new_file = file;		/* If the new file is not specified.	*/

		if ('#' == new_file[0])
		{
				nvalid = 3;
				rename_status = 56;					/* New file is invalid.			*/
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
				rename_status = 44;					/* New lib is invalid.			*/
			}
			else if (!strncmp(lib,new_lib,8))				/* Did not specify a diff new lib name.	*/
			{
				nvalid = 3;
				rename_status = 52;
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
		werrlog(104, "%RENAME-E-ARGS Invalid argument list, missing argument RETCODE.", 0,0,0,0,0,0,0,0);
		return;
	}
	else
	{
		werrlog(104, "%RENAME-E-ARGS Invalid argument list, extra arguments found.", 0,0,0,0,0,0,0,0);
		return;
	}

	wtrace("RENAME", "ARGS", "(resolved) Type=[%c] File=[%8.8s] Lib=[%8.8s] Vol=[%6.6s] NFile=[%8.8s] NLib=[%8.8s]",
				*rtype, file, lib, vol, new_file, new_lib);

	if (nvalid)
	{
		if (0 == rename_status) 
		{
			rename_status = 44;
		}
		goto rename_return;
	}

	if (!strncmp(file,"        ",8) || *rtype == 'L')				/* Blank file or type=L means lib mode.	*/
	{
		lib_rename = 1;
	}

	mode = 0;
	if (lib_rename) mode |= IS_LIB;							/* Doing a library rename.		*/


	/* SAVE_WISPFILEXT;  don't save it -- we will use the ext of old_filename for new_filename */

	name_end = wfname(&mode, vol, lib, file, old_filename);        			/* Construct the native filename.	*/
	*name_end = (char)0;

	if (lib_rename)								/* wfname leaves the trailing '/' on	*/
	{
		old_filename[strlen(old_filename)-1] = (char)0;				/* Remove trailing separator		*/
	}


	mode = IS_OUTPUT;
	if (lib_rename) mode |= IS_LIB;


	if(!lib_rename)									/* If not a library			*/
	{
		ptr = splitext(old_filename);
		if (*ptr == '.') ptr++;							/* Point past the '.'			*/
		WSETFILEXT(ptr);							/* Reset EXT for new_filename		*/
	}

	name_end = wfname(&mode, vol, new_lib, new_file, new_filename);			/* Construct the new native filename.	*/
	*name_end = (char)0;

	if (lib_rename)								/* wfname leaves the trailing '/' on	*/
	{
		new_filename[strlen(new_filename)-1] = (char)0;				/* Remove trailing separator		*/
	}

	if ( !lib_rename && 0 != strcmp(lib,new_lib) )					/* If file ren && lib is different.	*/
	{
		if ( 0!=makepath(new_filename))
		{
			rename_status=24;
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
			rename_status=20;					/* yes, so return file not found		*/
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
				rename_status=16;	/* Lib does not exist */
			}
			else
			{
				rename_status=20;	/* file does not exist */
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
		rename_status = 52;
		goto rename_return;
	}
	if (*new_filename_idx && fexists(new_filename_idx))
	{
		rename_status = 52;
		goto rename_return;
	}

	/*
	**	Do the rename
	*/
	if (file_rename(old_filename,new_filename))
	{
		rename_status=24;
		goto rename_return;
	}
	if (*old_filename_idx)
	{
		if (file_rename(old_filename_idx,new_filename_idx))
		{
			/*
			**	The idx rename failed, try to undo the data portion rename before returning
			*/
			if (file_rename(new_filename,old_filename))
			{
				rename_status=48;	/* Serious error, half the file was renamed */
				goto rename_return;
			}
			
			rename_status=24;
			goto rename_return;
		}
	}
	
	dat_done = 0;

                               
rename_return:
	wtrace("RENAME", "RETURN", "Return code = %ld errno=[%d]", (long)rename_status, errno);
	
	wswap(&rename_status);
	PUTBIN(return_code,&rename_status,sizeof(int4));
}                                                         

#if defined(unix) && defined(NORENAME)
static int unix_shell_move(const char* old_filename, const char* new_filename)
{
	char	cmd[256];

	sprintf(cmd,"mv '%s' '%s' >/dev/null 2>&1",old_filename,new_filename);
	return (wsystem(cmd));
}
#endif /* unix */

int file_rename(char* old_filename, char* new_filename)
{
	int rc = 0;
	int save_errno = 0;
	errno = 0;
#if defined(unix) && defined(NORENAME)
	rc = unix_shell_move(old_filename,new_filename);
#else	
	rc = rename(old_filename,new_filename);
#endif
	save_errno = errno;
	wtrace("rename","return","Old=[%s] New=[%s] rc=[%d] errno=[%d]",
		old_filename, new_filename, rc, save_errno);
	errno = save_errno;
	return rc;
}

/*
**	History:
**	$Log: rename.c,v $
**	Revision 1.18.2.1.2.2  2003/02/10 19:14:29  gsl
**	Removed VMS code and applied a number of the changes from $HEAD
**	
**	Revision 1.18.2.1.2.1  2002/11/14 21:12:25  gsl
**	Replace WISPFILEXT and WISPRETURNCODE with set/get calls
**	
**	Revision 1.18.2.1  2002/08/19 15:31:04  gsl
**	4403a
**	
**	Revision 1.18  1998-11-30 12:24:43-05  gsl
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
