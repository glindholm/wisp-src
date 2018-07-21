/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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

/*
**	File:		scratch.c
**
**	Project:	WISPLIB
**
**	Purpose:	VSSUB SCRATCH
**
*/

/* Scratch a file or a library													*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include <sys/types.h>

#ifdef unix
#include <dirent.h>
#include <unistd.h>
#endif

#if defined(WIN32)
#include <direct.h>
#endif


#include <errno.h>
#include <stdarg.h>

#include "idsistd.h"
#include "wcommon.h"
#include "wdefines.h"
#include "idsisubs.h"
#include "wisplib.h"
#include "wfname.h"
#include "filext.h"
#include "wispcfg.h"
#include "werrlog.h"
#include "vssubs.h"

#ifdef WIN32
#define rmdir(dir) _rmdir(dir)
#endif

#ifndef ETXTBSY
#define ETXTBSY 26
#endif


static int scratch_mode_32 = -1;

static int do_delete(char* file, int lib_flag, int* err_type);

/*
**   	SCRATCH(Type, File, Lib, Vol, [Exp_flag, [Limit_flag,]] Retcode)
**
**	Arg1	Type	Alpha(1) 	The type of scratch 'F' = File, 'L' = Library
**	Arg2	File	Alpha(8)	The File
**	Arg3	Lib	Alpha(8)	The library
**	Arg4	Vol	Alpha(6)
**	Arg5	Exp	Alpha(1)	Not supported (optional)
**	Arg6	Limit	Alpha(1) 	Not supported (optional)
**	Arg7	Retcode	Int(4)		Return code
**				
*/

void SCRATCH(	char *tflag,		/* The type of scratch, F=file, L=library		*/
		char *fname,		/* alpha(8) name of the file to scratch			*/
		char *lib,		/* library name, alpha(8).				*/
		char *vol,		/* volume, alpha(6)					*/
		...)
{
	va_list	the_args;
	int	arg_count;
	char *eflag;							/* expiration flag. B=bypass checking, " "=no bypass	*/
	char *lflag;							/* limitation flag.  L=restrict access rights.		*/
	int4 *retcod;							/* return code						*/
	char the_file[132];						/* actual file name					*/
	char	*ptr;
	int	lib_flag;
	int4	ret, rc;
	char	l_file[9], l_lib[9], l_vol[7];				/* Local copies of the file lib and vol			*/

	ret = SCRATCH_RC_0_SUCCESS;
	lib_flag = FALSE;

	va_start(the_args, vol);
	arg_count = WL_va_count();

	WL_wtrace("SCRATCH","ARGS","Type=[%c] File=[%8.8s] Lib=[%8.8s] Vol=[%6.6s] Args=%d",
	       *tflag, fname, lib, vol, arg_count);

	if (arg_count < 5 || arg_count > 7)
	{
		/* Invalid number of args */
		WL_werrlog_error(WERRCODE(54004), "SCRATCH", "ARGCNT", "Invalid number of arguments [cnt=%d]", arg_count);
		return;
	}

	if (arg_count > 5)
	{
		eflag = va_arg(the_args,char*);
	}
	if (arg_count > 6)
	{
		lflag = va_arg(the_args,char*);
	}
	retcod = va_arg(the_args,int4*);
	va_end(the_args);

	if ( (*tflag == 'L') || (*tflag =='l') )
	{
		lib_flag = TRUE;
		strcpy(l_file,"        ");
	}
	else
	{
		memcpy(l_file,fname,8);
		l_file[8] = (char)0;
		leftjust(l_file,8);
		if (!l_file[0] || ' ' == l_file[0])
		{
			ret = SCRATCH_RC_20_FILE_NOT_FOUND;
			goto scratch_return;
		}
	}

	if ( -1 == scratch_mode_32 )
	{
		if (0==strcmp(wispscratchmode(),"32"))
		{
			/*
			**	This is a hack to provide backwards compatiblity.
			**	Version 3.2 of srcatch did not check if lib and vol 
			**	were supplied, so wfname() defaulted them to inlib/invol.
			*/
			scratch_mode_32 = 1;
		}
		else
		{
			scratch_mode_32 = 0;
		}
	}

	memcpy(l_lib,lib,8);
	l_lib[8] = (char)0;
	leftjust(l_lib,8);
	if ( (!l_lib[0] || ' ' == l_lib[0]) && !scratch_mode_32 )
	{
		ret = SCRATCH_RC_16_LIBRARY_NOT_FOUND;
		goto scratch_return;
	}

	memcpy(l_vol,vol,6);
	l_vol[6] = (char)0;
	leftjust(l_vol,6);
	if ( (!l_vol[0] || ' ' == l_vol[0]) && !scratch_mode_32 )
	{
		ret = SCRATCH_RC_4_VOLUME_NOT_FOUND;
		goto scratch_return;
	}


	if (lib_flag)
	{
		ptr = WL_wanglib2path( l_vol, l_lib, the_file );
		*ptr = (char)0;
	}
	else
	{
		int4 mode = 0;
		ptr = WL_wfname( &mode, l_vol, l_lib, l_file, the_file );
		*ptr = (char)0;
	}

	if ( lib_flag )								/* scratch the library				*/
	{
		char libstr[80];
		strcpy( libstr, the_file );
		libstr[ strlen(libstr)-1 ] = '\0';				/* Remove the trailing separator '/' or '\'	*/

		rc = 0;
		if ( !fexists(libstr) ) 					/* Check if library exists			*/
		{
			rc = 1;
			ret = SCRATCH_RC_16_LIBRARY_NOT_FOUND;
		}

		buildfilepath(the_file, libstr, "*");				/* Use wildcard for filename			*/
	}

	if ( ret == SCRATCH_RC_0_SUCCESS )
	{											/* Do all versions.		*/
		int st_err;									/* Flag to decide which error	*/

		st_err = 0;									/* Set to use errno values.	*/
		rc = do_delete(the_file,lib_flag,&st_err);					/* now delete it		*/

		switch (rc)
		{
			case 0: break;								/* no error 			*/
			case ENOTDIR: ret=SCRATCH_RC_16_LIBRARY_NOT_FOUND; break;		/* Libary not found		*/
			case EPERM:   ret=SCRATCH_RC_20_FILE_NOT_FOUND; break;			/* permission denied 		*/
			case EBUSY:   ret=SCRATCH_RC_32_FILE_IN_USE; break;			/* file in use			*/
			case ETXTBSY: ret=SCRATCH_RC_32_FILE_IN_USE; break;			/* file in use			*/
			case EROFS:   ret=SCRATCH_RC_52_FILE_BYPASSED; break;			/* permission denied		*/
			case EFAULT:  ret=SCRATCH_RC_48_IO_ERROR; break;			/* I/O error			*/
			case ENOENT:  ret=SCRATCH_RC_20_FILE_NOT_FOUND; break;			/* file not found		*/
			case EACCES:  ret= (lib_flag) ? SCRATCH_RC_52_FILE_BYPASSED:SCRATCH_RC_24_ACCESS_DENIED; break;	/* permission denied 		*/
			default:      ret=errno; break;						/* default: errno		*/
		}
	}

scratch_return:
	wtrace("SCRATCH","RETURN", "Return Code=[%d]", ret);
	
	WL_put_swap(retcod, ret);
}




static int do_delete(char* file, int lib_flag, int* err_type)
{
	char fpat[128];								/* pattern incl. wildcards			*/
	char *filename;
	int	first;
	char	*ptr;

	strcpy(fpat,file);							/* copy to our descriptor			*/

	if (lib_flag && strchr(fpat,'*'))
	{
		char	delspec[WISP_FILEPATH_LEN];
		int	save_rc;

		first = 1;

		save_rc = 0;

		strcpy(fpat,splitpath(fpat));					/* Remove the file from the path		*/
		while ((filename=WL_s_nextfile(fpat,&first)))
		{
			buildfilepath(delspec,fpat,filename);
			if (wisp_unlink(delspec)<0) 
			{
				save_rc = errno;
				if ( save_rc != EACCES ) return save_rc;
			}
		}
		if ( save_rc ) return( 52 );
		if ( rmdir(fpat) ) return( 52 );
		return( 0 );
	}
	else if ( ! lib_flag )
	{
		char tempfile[WISP_FILEPATH_LEN];
		int has_ext;
		int	deleted_data = 0;
		int	deleted_idx = 0;

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
		**
		**	5) Corrupt file - data portion missing
		**		foo.idx
		*/

		has_ext = hasext(fpat);

		/*
		 *	First delete the data portion
		 */
		if ( fexists(fpat) )
		{
			if (wisp_unlink(fpat)<0) 
			{
				return errno;
			}
			deleted_data = 1;
		}
		else if (!has_ext)
		{
			/*
			 *	If no extension then try .dat
			 */
			strcpy(tempfile,fpat);
			strcat(tempfile,".dat");
			if ( fexists(tempfile) )
			{
				if (wisp_unlink(tempfile)<0) 
				{
					return errno;
				}
				deleted_data = 1;
			}
		}

		/*
		**	The data portion has been deleted.
		**
		**	Check if there is an index portion to delete
		*/
		strcpy(tempfile,fpat);
		if (has_ext)
		{
			if ((ptr = strrchr(tempfile,'.')))       			/* cut off the extension			*/
			{
				*ptr = '\0';
			}
		}

		strcat(tempfile,".idx");					/* Try CISAM .idx file				*/
		if ( fexists(tempfile) )
		{
			if (wisp_unlink(tempfile)<0) 
			{
				return errno;
			}
			deleted_idx = 1;
		}
		else
		{
			if ((ptr = strrchr(tempfile,'.'))) *ptr = '\0';		/* cut off the extension			*/

			strcat(tempfile,".vix");				/* Try Vision4 .vix file			*/
			if ( fexists(tempfile) )
			{
				if (wisp_unlink(tempfile)<0) 
				{
					return errno;
				}
				deleted_idx = 1;
			}
		}

		if (!deleted_data && !deleted_idx)
		{
			return ENOENT;	/* nothing was found */
		}
		

		/*
		**	Try to remove the directory.  If empty it will be removed otherwise the rmdir will fail.
		*/
		strcpy(tempfile,splitpath(fpat));
		rmdir(tempfile);						/* Attempt to remove the directory		*/
	}
	else
	{
		return 44;							/* Lib flag with no '*' ?			*/
	}

	return( 0 );
}



/*
**	History:
**	$Log: scratch.c,v $
**	Revision 1.32  2011/10/29 20:09:14  gsl
**	Fix ISO routine name warnins on WIN32
**	
**	Revision 1.31  2003/02/04 16:02:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.30  2003/01/31 18:54:38  gsl
**	Fix copyright header
**	
**	Revision 1.29  2003/01/29 21:50:08  gsl
**	Switch to use vssubs.h
**	
**	Revision 1.28  2003/01/17 15:32:16  gsl
**	trace
**	
**	Revision 1.27  2003/01/16 19:40:32  gsl
**	Change SCRTACH to use stdarg.h
**	
**	Revision 1.26  2002/12/11 17:03:07  gsl
**	use wisp_unlink()
**	
**	Revision 1.25  2002/10/18 19:14:09  gsl
**	Cleanup
**	
**	Revision 1.24  2002/07/16 16:24:53  gsl
**	Globals
**	
**	Revision 1.23  2002/07/12 19:10:15  gsl
**	Global unique WL_ changes
**	
**	Revision 1.22  2002/07/12 17:01:00  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.21  2002/07/11 20:29:12  gsl
**	Fix WL_ globals
**	
**	Revision 1.20  2002/07/10 04:27:36  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.19  2002/06/28 04:02:59  gsl
**	Work on native version of wfopen and wfname
**	
**	Revision 1.18  2002/06/21 03:10:40  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.17  1999/08/20 17:05:58  gsl
**	Found and fixed the real problem with SCRATCH. It was failing to delete the
**	idx file portion if the filepath contained '.' characters. It was mistaking
**	where the extension was so it was mis-forming the file.idx path.
**	
**	Revision 1.16  1999-08-20 10:20:01-04  gsl
**	Fix problem where SCRATCH would not delete the idx portion of a file
**	if it couldn't find a data portion.
**
**	Revision 1.15  1998-05-15 15:16:00-04  gsl
**	Add support for Vision4 plus tracing
**
**	Revision 1.14  1996-10-08 20:24:47-04  gsl
**	replace getenv() with wispscratchmode() call
**
**	Revision 1.13  1996-08-19 15:32:52-07  gsl
**	drcs update
**
**
**
*/
