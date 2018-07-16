			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		rename.c
**
**	Purpose:	To hold RENAME vssub.
**
**	Routines:	
**		wrename()		The emulation of vssub RENAME (translation done by WISP).
**		check_existence()	VMS: See if a file exists.
**		create_dir()		VMS: Creates a directory.
**
**	History:
**			mm/dd/yy	Written by OLD
**			08/04/92	Updated for wfname changes and headers added. GSL
**
*/


#ifdef VMS
#include <rmsdef.h>
#include <descrip.h>
#include <ssdef.h>
#include <fab.h>
#endif
#include <string.h>
#include <varargs.h>                                                                    /* This routine uses a variable number	*/
											/* of arguments.			*/
#include "idsistd.h"
#include "wfiles.h"
#include "wcommon.h"
#include "movebin.h"
#include "wperson.h"
#include "werrlog.h"
#include "cobrun.h"
#include "idsisubs.h"

extern char WISPFILEXT[39];

wrename(va_alist)
va_dcl
{
#define	ROUTINE		53000
	va_list the_args;								/* A pointer to traverse the stack.	*/
	int arg_count, *return_code;							/* A variable and a pointer.		*/
	int4 rename_status;								/* Status from the lib call.		*/
	char *rtype,*the_item, *file, *lib, *vol, *new_file, *new_lib;			/* Pointers to passed arguments.	*/
	char old_filename[132], new_filename[132];					/* Strings to contain the filenames.	*/
	int existence_status, x, i;
	int4 mode;
	char *name_end, *wfname();							/* build filename from wang parts	*/
	char *strchr();									/* return pointer to char in string	*/
	char null_str[1];
	int4 flags;
	int nvalid;									/* Not Valid call flag.			*/
	int dat_done, has_ext;
	char *ptr;
	int	lib_rename;

#ifdef VMS
#include "rename1.d"
	struct FAB fab1; 							/* Structure point to File ATT Block.		*/
	int rms_status;
#endif

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/
	rename_status = 0;
	lib_rename = 0;

	va_start(the_args);								/* Set pointer to top of stack.		*/
	arg_count = va_count(the_args);							/* How many args are there ?		*/
	va_start(the_args);								/* Go back to the top of the stack.	*/

	nvalid = 0;
	rtype = va_arg(the_args, char*);						/* Get the rename type code.		*/
	arg_count--;									/* One less argument.			*/
	if (!strrchr("FLG",*rtype))							/* Check to see if fund type is valid.	*/
	{
		nvalid = 1;								/* Set not valid so doesn't try.	*/
		rename_status = 44;							/* Invalid func type.			*/
	}
	file = va_arg(the_args, char*);							/* Get addr. of the file.		*/
	arg_count--;									/* One less argument.			*/
	if (*rtype == 'F' && !strncmp(file,"        ",8))				/* If file rename and file not specified*/
	{
		nvalid = 1;								/* Set to invalid call.			*/
		rename_status = 20;
	}
	lib = va_arg(the_args, char*);							/* Get addr. of the lib.		*/
	arg_count--;									/* One less argument.			*/
	if (!strncmp(lib,"        ",8))							/* If the library is not specified	*/
	{										/* then use defualt INLIB.		*/
		get_defs(DEFAULTS_IL,lib);
	}
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
			else if (!strncmp(file,new_file,8))				/* Did not specify a diff new file name.*/
			{
				nvalid = 3;
				rename_status = 52;
			}
		}
		else if (!strncmp(new_file,"        ",8)) new_file = file;		/* If the new file is not specified.	*/
	}
	else	new_file = file;							/* Use old file.			*/

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
			else if (*rtype == 'L' && !strncmp(lib,new_lib,8))		/* Did not specify a diff new lib name.	*/
			{
				nvalid = 3;
				rename_status = 52;
			}
		}
		else if (!strncmp(new_lib,"        ",8)) new_lib = lib;			/* If the new lib not specified.	*/ 
	}
	else	new_lib = lib;								/* Use old library name.		*/

	while (arg_count > 1)								/* Pop args till the last one.		*/
	{										/* Bypass, Access, Open flags not used.	*/
		the_item = va_arg(the_args, char*);					/* Trash an argument.			*/
		arg_count -= 1;								/* One less arg.			*/
	}

	return_code = va_arg(the_args, int*);						/* Get the addr. of the return code.	*/

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
		loadpad(WISPFILEXT,ptr,sizeof(WISPFILEXT));				/* Reset EXT for new_filename		*/
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
		*/

	dat_done = 0;
	has_ext = hasext(old_filename);						/* does it already have extension?		*/

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

#endif /* !VMS */
                               
rename_return:
	wswap(&rename_status);
	PUTBIN(return_code,&rename_status,sizeof(int4));
}                                                         

#ifdef unix
int file_rename(old_filename,new_filename)
char	*old_filename, *new_filename;
{
	char	cmd[256];

	sprintf(cmd,"mv %s %s >/dev/null 2>&1",old_filename,new_filename);
	return (wsystem(cmd));
}
#endif /* unix */
#ifdef MSDOS
int file_rename(old_filename,new_filename)
char	*old_filename, *new_filename;
{
	return (rename(old_filename,new_filename));
}
#endif /* MSDOS */

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
	extern char *wfname();
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
