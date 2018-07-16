static char copyright[]="Copyright (c) 1995- 1998 NeoMedia Technologies, All rights reserved.";
static char rcsid[]="$Id:$";

/*
**	File:		rename.c
**
**	Purpose:	To hold RENAME vssub.
**
**	RCS:		$Source:$
**
**	Routines:	
**		wrename()		The emulation of vssub RENAME (translation done by WISP).
**		check_existence()	VMS: See if a file exists.
**		create_dir()		VMS: Creates a directory.
**
**
*/


#ifdef VMS
#include <rmsdef.h>
#include <descrip.h>
#include <ssdef.h>
#include <fab.h>
#endif

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

#ifdef VMS
#include "rename1.d"
	struct FAB fab1; 							/* Structure point to File ATT Block.		*/
	int rms_status;
	int existence_status, x, i;
	int4 flags;
	char null_str[1];
#endif

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
		if (nvalid == 1) werrlog(ERRORCODE(2),*rtype,file,lib,vol,0,0,0,0);	/* Invalid arguments.			*/
		else if (nvalid == 2) werrlog(ERRORCODE(4),*rtype,new_file,new_lib,0,0,0,0,0);

		if (!rename_status) rename_status = 44;
		goto rename_return;
	}

	if (!strncmp(file,"        ",8) || *rtype == 'L')				/* Blank file or type=L means lib mode.	*/
	{
		lib_rename = 1;
	}

	mode = 0;
	if (lib_rename) mode |= IS_LIB;							/* Doing a library rename.		*/

#ifdef VMS
	if (0 == memcmp(WISPFILEXT,"LIS",3)) mode |= IS_PRINTFILE;			/* Special if print file		*/
#endif

	/* SAVE_WISPFILEXT;  don't save it -- we will use the ext of old_filename for new_filename */

	name_end = wfname(&mode, vol, lib, file, old_filename);        			/* Construct the native filename.	*/
	*name_end = (char)0;

#ifndef VMS
	if (mode & IS_LIB)								/* wfname leaves the trailing '/' on	*/
	{
		old_filename[strlen(old_filename)-1] = (char)0;				/* Remove trailing separator		*/
	}
#endif /* !VMS */

	
#ifdef VMS
	if (lib_rename)
	{
		/*
		**	Change  "USER$DISK:[MYLIB]"   into  "USER$DISK:[000000]MYLIB.DIR "
		*/
		char *pos, *fp, tfile[9];

		pos = strrchr(old_filename,'[');					/* Find the [ symbol.			*/
		pos++;									/* Move past [ symbol.			*/
		fp = pos;
		i = 0;
		while (*fp != ']') tfile[i++] = *fp++;					/* Copy file name to temp var.		*/
		tfile[i] = '\0';
		*pos = '\0';
		strcat(old_filename,"000000]");
		strcat(old_filename,tfile);
		strcat(old_filename,".DIR ");						/* Concat the file indicator.		*/
	}
	o_desc.dsc$w_length = strlen(old_filename);
#endif

	mode = IS_OUTPUT;
	if (lib_rename) mode |= IS_LIB;

#ifdef VMS
	if (0 == memcmp(WISPFILEXT,"LIS",3)) mode |= IS_PRINTFILE;			/* Special if print file		*/
#endif

	if(!lib_rename)									/* If not a library			*/
	{
		ptr = splitext(old_filename);
		if (*ptr == '.') ptr++;							/* Point past the '.'			*/
		cstr2cobx(WISPFILEXT,ptr,sizeof(WISPFILEXT));				/* Reset EXT for new_filename		*/
	}

	name_end = wfname(&mode, vol, new_lib, new_file, new_filename);			/* Construct the new native filename.	*/
	*name_end = (char)0;

#ifdef VMS
	if (lib_rename)
	{										/* Blank filename is rename directory.	*/
		/*
		**	Change  "USER$DISK:[MYLIB]"   into  "USER$DISK:[000000]MYLIB.DIR "
		*/
		char *pos, *fp, tfile[9];

		pos = strrchr(new_filename,'[');					/* Find the [ symbol.			*/
		pos++;									/* Move past [ symbol.			*/
		fp = pos;
		i = 0;
		while (*fp != ']') tfile[i++] = *fp++;					/* Copy file name to temp var.		*/
		tfile[i] = '\0';
		*pos = '\0';
		strcat(new_filename,"000000]");
		strcat(new_filename,tfile);
		strcat(new_filename,".DIR ");						/* Concat the file indicator.		*/
	}

	fab1 = cc$rms_fab;							/* Initialize fab to rms file.			*/
	fab1.fab$b_fac = FAB$M_PUT;						/* File to be output.				*/

	fab1.fab$l_fna = old_filename;						/* Name of file.				*/
	fab1.fab$b_fns = sizeof(old_filename) -1;				/* Size of file name.				*/
	fab1.fab$l_fop = 0;							/* Set file option to create.			*/
	fab1.fab$b_shr = FAB$M_NIL;						/* Set file to no share.			*/
										/* Open output file with name from wfanme.	*/
	rms_status = sys$open(&fab1);
	if (rms_status == RMS$_FLK || rms_status == RMS$_ACT || rms_status == RMS$_BUSY) /* Is the file in use?			*/
	{
		rename_status=32;						/* Do not rename the file and return		*/
		sys$close(&fab1);						/* a status of 32.  Close the file.		*/
		goto rename_return;
	}
	sys$close(&fab1);							/* Close the file.				*/
	n_desc.dsc$w_length = strlen(new_filename);
	null_str[0] = (char)0;
	null_desc.dsc$w_length = 0;
	flags = 1;
											 /* Call the lib function to rename it.	*/
 	rename_status = LIB$RENAME_FILE(&o_desc, &n_desc, &null_desc, &null_desc, &flags);

	if (rename_status == RMS$_FEX || rename_status == RMS$_ENT) rename_status = 52;
	else if (rename_status == RMS$_DEV) rename_status = 4;
	else if (rename_status == RMS$_RMV) rename_status = 24;
	else if (rename_status == RMS$_DNF) 
	{
		existence_status = 0;							/* Couldn't rename it to the new dir.	*/
		existence_status = check_existence(old_filename);			/* See if the old dir exists.		*/
		if (existence_status)							/* Now create the new directory.	*/
		{
			create_dir(new_lib, vol);
			rename_status = LIB$RENAME_FILE(&o_desc, &n_desc);		/* And then rename all the files.	*/
			if (rename_status == RMS$_DNF) rename_status = 16;
			else if (rename_status == RMS$_RMV) rename_status = 24;
		}
		else
		{
			rename_status = 16;
		}
	}

	if (rename_status == RMS$_FNF)
	{
		if (lib_rename) rename_status = 16;
		else rename_status = 20;
	}
	else if (rename_status == 1) rename_status = 0;					/* VAX 1 is WANG 0			*/

#else /* !VMS */

	if (mode & IS_LIB)								/* wfname leaves the trailing '/' on	*/
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
		if (ptr = strrchr(old_filename_idx,'.')) *ptr = '\0';		/* cut off the extension			*/
		if (ptr = strrchr(new_filename_idx,'.')) *ptr = '\0';
	}
	strcat(old_filename_idx,".idx");
	strcat(new_filename_idx,".idx");
	if (!fexists(old_filename_idx))						/* See if there is a .idx index portion		*/
	{
		if (ptr = strrchr(old_filename_idx,'.')) *ptr = '\0';		/* cut off the extension			*/
		if (ptr = strrchr(new_filename_idx,'.')) *ptr = '\0';

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
			/*
			**	File doesn't exist
			*/
			rename_status=20;
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

#ifdef OLD	
	if (fexists(old_filename))						/* file exist in this form			*/
	{
		if (fexists(new_filename))					/* New file already exists.			*/
		{
			rename_status = 52;
			goto rename_return;
		}

		if (file_rename(old_filename,new_filename))
		{
			rename_status=24;
			goto rename_return;
		}
		dat_done = 1;
	}
	else if (has_ext)
	{
		rename_status=20;						/* yes, so return file not found		*/
		goto rename_return;
	}


	if (!has_ext)								/* May be CISAM files				*/
	{
		strcat(old_filename,".idx");					/* else try it with a .dat extension		*/
		strcat(new_filename,".idx");
		if (!fexists(old_filename))					/* still not found				*/
		{
			if (dat_done) rename_status = 0;			/* It was just a seq file.			*/
			else	      rename_status = 20;			/* Really not found.				*/
			goto rename_return;
		}

		if (fexists(new_filename))					/* Does new file already exists?		*/
		{
			rename_status = 52;
			goto rename_return;
		}

		if (file_rename(old_filename,new_filename))
		{
			rename_status=24;
			goto rename_return;
		}

		if (!dat_done)							/* May be CISAM with .dat			*/
		{
			strcpy(strrchr(old_filename,'.'),".dat");		/* replace the '.dat' with a '.idx'		*/
			strcpy(strrchr(new_filename,'.'),".dat");
			if (file_rename(old_filename,new_filename))
			{
				rename_status=48;				/* Serious error if only half worked.		*/
				goto rename_return;
			}
			dat_done = 1;
		}
	}

	if ( !dat_done )
	{
		rename_status=20;						/* yes, so return file not found		*/
		goto rename_return;
	}
#endif /* OLD */
#endif /* !VMS */
                               
rename_return:
	wtrace("RENAME", "RETURN", "Return code = %ld errno=[%d]", (long)rename_status, errno);
	
	wswap(&rename_status);
	PUTBIN(return_code,&rename_status,sizeof(int4));
}                                                         

#if defined(unix) && defined(NORENAME)
static int unix_shell_move(const char* old_filename, const char* new_filename)
{
	char	cmd[256];

	sprintf(cmd,"mv \"%s\" \"%s\" >/dev/null 2>&1",old_filename,new_filename);
	return (wsystem(cmd));
}
#endif /* unix */

#if defined(unix) || defined(MSDOS) || defined(WIN32)
int file_rename(char* old_filename, char* new_filename)
{
#if defined(unix) && defined(NORENAME)
	return unix_shell_move(old_filename,new_filename);
#else	
	errno = 0;
	return rename(old_filename,new_filename);
#endif
}
#endif /* unix || MSDOS || WIN32 */

#ifdef VMS
check_existence(filename)								/* Check to see if a file exists.	*/
char *filename;
{
                            
	char l_filename[132], result[132], *context, *statval;
	int status;
#include "rename2.d"

	memset(l_filename,' ', sizeof(l_filename));
	strcpy(l_filename, filename);
	t_desc.dsc$w_length = strlen(l_filename);					/* Set the length of the descriptor.	*/
	memset(result,' ', sizeof(result));

	context = 0;
	statval = 0;
	status = LIB$FIND_FILE(&t_desc, &r_desc, &context, 0, 0, &statval, 0);
	lib$find_file_end(&context);							/* End the FIND_FILE context		*/
	if (status == RMS$_NORMAL || status == SS$_NORMAL) return(1);
                            
	return(0);
}

create_dir(nlib, vol)									/* Create a new directory.		*/
char *nlib, *vol;
{
	char dir_spec[132], file[9];
	int4 wfname_mode;
	int mkdir_status;
#include "rename3.d"

	wfname_mode = 0;
	wfname_mode |= IS_LIB;								/* Set the mode for libraries.		*/
	strcpy(file,"        ");							/* No file name for directory.		*/
	memset(dir_spec, '\0', sizeof(dir_spec));
	wfname(&wfname_mode, vol, nlib, file, dir_spec);				/* Make the name.			*/
 	mkdir_status = LIB$CREATE_DIR(&d_desc);
}
#endif
/*
**	History:
**	$Log: rename.c,v $
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
