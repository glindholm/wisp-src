static char copyright[]="Copyright (c) 1988-1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		filecopy.c
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
**
**	Purpose:	???
**
**	Routines:	
**	xxx()		???
*/

/* FILECOPY.C ... This routine allows the copying of a file to another file.							*/

/*
**	Includes
*/

#ifdef VMS
#include <rmsdef.h>
#include <descrip.h>
#include <ssdef.h>
#include <fab.h>
#include <lnmdef.h>
#include <climsgdef.h>
#include "spawn.h"
#endif

#include <string.h>
#include <varargs.h>                                                                    /* This routine uses a variable number	*/
											/* of arguments.			*/
#include "idsistd.h"
#include "fcopy.h"
#include "wfiles.h"
#include "wcommon.h"
#include "movebin.h"
#include "wperson.h"
#include "werrlog.h"
#include "cobrun.h"
#include "filext.h"
#include "paths.h"
#include "wfname.h"
#include "idsisubs.h"
#include "wisplib.h"

/*
	FILECOPY( infile, inlib, invol, outfile, [outlib, [outvol]], retcode )
*/

void FILECOPY(va_alist)
va_dcl
{
#define	ROUTINE		18200
	va_list the_args;								/* A pointer to traverse the stack.	*/
	int arg_count;
	int4 *return_code;
	int4 filecopy_status;								/* Status from the lib call.		*/
	char  *ptr;							/* Pointers to passed arguments.	*/
	char file[9],lib[9],vol[7],new_file[9],new_lib[9],new_vol[7];
	char old_filename[256], new_filename[256];					/* Strings to contain the filenames.	*/
	int4 mode;
	char *name_end;									/* build filename from wang parts	*/
	int nvalid;									/* Not Valid call flag.			*/
	int dat_done, has_ext;

#ifdef VMS
#include "filecopy.d"
	struct FAB fab1; 								/* Structure point to File ATT Block.	*/
	int rms_status;
	char cmd[255];									/* buffer to hold cmd string 		*/
	uint4 vms_status;
#endif

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/
	filecopy_status = 0;
	nvalid = 0;

	va_start(the_args);								/* Set pointer to top of stack.		*/
	arg_count = va_count(the_args);							/* How many args are there ?		*/
	if (arg_count < 4 || arg_count > 7)
	{
		werrlog(ERRORCODE(2),"","","",0,0,0,0,0);				/* Invalid arguments.			*/
		return;
	}

	strcpy(file,"        ");
	strcpy(lib, "        ");
	strcpy(vol, "      ");
	strcpy(new_file,file);
	strcpy(new_lib, lib);
	strcpy(new_vol, vol);

	va_start(the_args);								/* Go back to the top of the stack.	*/

	ptr = va_arg(the_args, char*);							/* Get addr. of the file.		*/
	arg_count--;									/* One less argument.			*/
	memcpy(file,ptr,8);
	if (0==memcmp(file,"        ",8))
	{
		nvalid = 1;								/* Invalid: no file name		*/
	}

	ptr = va_arg(the_args, char*);							/* Get addr. of the lib.		*/
	arg_count--;									/* One less argument.			*/
	memcpy(lib,ptr,8);
	if (0==memcmp(lib,"        ",8))						/* If the library is not specified	*/
	{										/* then use defualt INLIB.		*/
		get_defs(DEFAULTS_IL,lib);
	}

	ptr = va_arg(the_args, char*);	   						/* Get addr. of the vol.		*/
 	arg_count--;									/* One less argument.			*/
	memcpy(vol,ptr,6);
	if (0==memcmp(vol,"      ",6))				      			/* If the volume is not specified	*/
	{										/* then use defualt INVOL.		*/
		get_defs(DEFAULTS_IV,vol);
		if (0==memcmp(vol,"      ",6))						/* Must specify a volume.		*/
		{
			nvalid = 1;
			filecopy_status = 4;
		}         
	}

	ptr = va_arg(the_args, char*);							/* Get addr. of the new file name.	*/
	arg_count--;									/* One less argument.			*/
	memcpy(new_file,ptr,8);
	if (0==memcmp(new_file,"        ",8)) memcpy(new_file,file,8);			/* If the new file is not specified.	*/

	if (arg_count>1)
	{
		ptr = va_arg(the_args, char*);						/* Get addr. of the new library name.	*/
		arg_count--;								/* One less arg.			*/
		memcpy(new_lib,ptr,8);
	}
	if (0==memcmp(new_lib,"        ",8)) memcpy(new_lib,lib,8);			/* If the new lib not specified.	*/

	if (arg_count>1)
	{
		ptr = va_arg(the_args, char*);						/* Get addr. of the new volume name.	*/
		arg_count--;								/* One less arg.			*/
		memcpy(new_vol,ptr,6);
	}
	if (0==memcmp(new_vol,"      ",6)) memcpy(new_vol,vol,6);			/* If the new vol not specified.	*/

	return_code = va_arg(the_args, int4*);						/* Get the addr. of the return code.	*/

	if (nvalid)
	{
		if (nvalid == 1) werrlog(ERRORCODE(2),file,lib,vol,0,0,0,0,0);		/* Invalid arguments.			*/
		else if (nvalid == 2) werrlog(ERRORCODE(4),file,lib,vol,0,0,0,0,0);	/* Not yet supported.			*/

		if (!filecopy_status) filecopy_status = 44;
		goto filecopy_return;
	}

	mode = 0;

	/* SAVE_WISPFILEXT;  don't save it -- we will use the ext of old_filename for new_filename */

	name_end = wfname(&mode, vol, lib, file, old_filename);        			/* Construct the native filename.	*/
	*name_end = '\0'; 								/* null terminate it			*/

	mode = IS_OUTPUT;

	ptr = splitext(old_filename);
	if (*ptr == '.') ptr++;								/* Point past the '.'			*/
	loadpad(WISPFILEXT,ptr,sizeof(WISPFILEXT));					/* Reset EXT for new_filename		*/

	if (*new_file == '#')
	{
		filecopy_status = 56;
		goto filecopy_return;
	}

	name_end = wfname(&mode, new_vol, new_lib, new_file, new_filename);		/* Construct the new native filename.	*/
	*name_end = '\0'; 								/* null terminate it			*/

#ifdef VMS

	filecopy_status = check_existence(old_filename);
	if (!filecopy_status)
	{
		filecopy_status=20;							/* File not found.			*/
		goto filecopy_return;
	}

	if (check_existence(new_filename))						/* New file already exists		*/
	{
		filecopy_status = 52;
		goto filecopy_return;
	}

	o_desc.dsc$w_length = strlen(old_filename);
	fab1 = cc$rms_fab;								/* Initialize fab to rms file.		*/
	fab1.fab$l_fna = old_filename;							/* Name of file.			*/
	fab1.fab$b_fns = strlen(old_filename);						/* Size of file name.			*/
	fab1.fab$b_shr = FAB$M_NIL;							/* Set file to no share.		*/
											/* Open file with name from wfanme.	*/
	rms_status = sys$open(&fab1);
	if (rms_status == RMS$_ACT || rms_status == RMS$_BUSY) 				/* Is the file in use?			*/
	{
		filecopy_status=32;							/* Do not copy the file and return	*/
		sys$close(&fab1);							/* a status of 32.  Close the file.	*/
		goto filecopy_return;
	}
	sys$close(&fab1);								/* Close the file.			*/

	makepath(new_filename);								/* Ensure directories are created	*/
	sprintf(cmd,"BACKUP/IGNORE=INTERLOCK %s %s",old_filename,new_filename);
	filecopy_status = spawn2(SPAWN_CMD_QUIET,cmd,"", &vms_status);

	if (filecopy_status != SS$_NORMAL)						/* IF spawn failed ...			*/
	{
		if (filecopy_status == SS$_ACCVIO) filecopy_status = 24;		/* Access violation. No update access.	*/
		else filecopy_status = 48;						/* An I/O error occured.		*/
	}
	else
	{
		if (vms_status != SS$_NORMAL && vms_status != 268435457)		/* IF spawned command failed ...	*/
		{
			if (vms_status == 279150708)					/* Insufficient privs or file protection*/
			{								/*  violation.				*/
				filecopy_status = 24;
			}
			else	filecopy_status = 48;
		}
		else
		{
			filecopy_status = 0;						/* Set to 0 for successful.		*/
		}
	}
#endif

#if defined(unix) || defined(MSDOS) || defined(WIN32)

	/*
	**	Make sure the file extensions are the same.
	*/
	{
		char	*ext_old, *ext_new;

		ext_old = osd_ext(old_filename);
		ext_new = osd_ext(new_filename);

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
	has_ext = hasext(old_filename);

	if (fexists(old_filename))							/* file exist in this form		*/
	{
		if (fexists(new_filename))						/* Does new file already exists?	*/
		{
			filecopy_status = 52;
			goto filecopy_return;
		}

		if (fcopy(old_filename,new_filename))
		{
			filecopy_status=24;
			goto filecopy_return;
		}

		dat_done = 1;
	}
	else if (has_ext) 								/* does it already have extension?	*/
	{
		filecopy_status=20;							/* yes, so return file not found	*/
		goto filecopy_return;
	}


	if (!has_ext)									/* May be CISAM files.			*/
	{
		strcat(old_filename,".idx");						/* else try it with a .dat extension	*/
		strcat(new_filename,".idx");
		if (!fexists(old_filename))						/* still not found			*/
		{
			if (dat_done) filecopy_status = 0;				/* It was just a seq file.		*/
			else	      filecopy_status = 20;				/* Really not found.			*/
			goto filecopy_return;
		}

		if (fexists(new_filename))						/* Does new file already exists?	*/
		{
			filecopy_status = 52;
			goto filecopy_return;
		}

		if (fcopy(old_filename,new_filename))
		{
			filecopy_status=24;
			goto filecopy_return;
		}

		if (!dat_done)							/* May be CISAM with .dat			*/
		{
			strcpy(strrchr(old_filename,'.'),".dat");			/* replace the '.dat' with a '.idx'	*/
			strcpy(strrchr(new_filename,'.'),".dat");

			if (fcopy(old_filename,new_filename))
			{
				filecopy_status=48;					/* Serious error if only half worked.	*/
				goto filecopy_return;
			}
			dat_done = 1;
		}
	}

	if ( !dat_done )
	{
		filecopy_status=20;							/* yes, so return file not found	*/
		goto filecopy_return;
	}

#endif /* unix || MSDOS || WIN32 */
                               
filecopy_return:
	wswap(&filecopy_status);
	PUTBIN(return_code,&filecopy_status,sizeof(int4));
}                                                         


/*
**	History:
**	$Log: filecopy.c,v $
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
