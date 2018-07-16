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

/* Scratch a file or a library													*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef VMS
#include <ssdef.h>
#include <descrip.h>
#include <rmsdef.h>
#include <file.h>	    								/* File definition include file.	*/
#include <rms.h>
#endif

#ifndef VMS
#include <sys/types.h>
#endif

#ifdef unix
#include <dirent.h>
#endif

#if defined(MSDOS) || defined(_MSC_VER)
#include <direct.h>
#endif

#ifdef MSDOS
#include <dos.h>
#endif

#include <errno.h>
#include <varargs.h>

#include "idsistd.h"
#include "wcommon.h"
#include "wdefines.h"
#include "movebin.h"
#include "cobrun.h"
#include "idsisubs.h"
#include "wisplib.h"
#include "wfname.h"
#include "filext.h"
#include "wispcfg.h"

#ifndef ETXTBSY
#define ETXTBSY 26
#endif


static int scratch_mode_32 = -1;

static int4 chkrms(int status, int lib_fl);
static int do_delete(char* file, int lib_flag, int* err_type);
static int del_dir(char* file_spec);



void SCRATCH(va_alist)
va_dcl
{
	va_list	the_args;
	int	arg_count;
	char *tflag;							/* The type of scratch, F=file, L=library		*/
	char *fname;							/* alpha(8) name of the file to scratch			*/
	char *lib;							/* library name, alpha(8).				*/
	char *vol;							/* volume, alpha(6)					*/
	char *eflag;							/* expiration flag. B=bypass checking, " "=no bypass	*/
	char *lflag;							/* limitation flag.  L=restrict access rights.		*/
	int4 *retcod;							/* return code						*/
	int4 mode;							/* the mode for wfname					*/
	char the_file[132];						/* actual file name					*/
	char	*ptr;
	int	lib_flag, vmstry;
	int4	ret, rc;
	char	l_file[9], l_lib[9], l_vol[7];				/* Local copies of the file lib and vol			*/
#ifdef VMS
	int  	j;
	int4 status;
	char result[256], *context, *statval;						/* Vars to use with lib$find_file	*/
#include "scratch1.d"

#endif	/* VMS */

	ret = 0;
	rc = 0;
	lib_flag = FALSE;
	vmstry = 0;
	mode = 0;

	va_start(the_args);
	arg_count = va_count(the_args);
	va_start(the_args);

	tflag = va_arg(the_args,char*);
	fname = va_arg(the_args,char*);
	lib   = va_arg(the_args,char*);
	vol   = va_arg(the_args,char*);
	if (arg_count > 5)
		eflag = va_arg(the_args,char*);
	if (arg_count > 6)
		lflag = va_arg(the_args,char*);
	retcod = va_arg(the_args,int4*);

	if ( (*tflag == 'L') || (*tflag =='l') )
	{
		mode |= IS_LIB;									/* this is a library		*/
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
			ret = 20;
			wswap(&ret);
			PUTBIN(retcod,&ret,sizeof(int4));
			return;
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
		ret = 16;
		wswap(&ret);
		PUTBIN(retcod,&ret,sizeof(int4));
		return;
	}

	memcpy(l_vol,vol,6);
	l_vol[6] = (char)0;
	leftjust(l_vol,6);
	if ( (!l_vol[0] || ' ' == l_vol[0]) && !scratch_mode_32 )
	{
		ret = 4;
		wswap(&ret);
		PUTBIN(retcod,&ret,sizeof(int4));
		return;
	}

#ifdef VMS
	if (0==memcmp(WISPFILEXT,"LIS",3)) mode |= IS_PRINTFILE;
#endif

	ptr = wfname( &mode, l_vol, l_lib, l_file, the_file );					/* create native filename	*/
	*ptr = (char)0;
#ifdef VMS
	if ( lib_flag ) strcat(ptr,"*.*;*");						/* Scratch library - file name is *.*;*	*/
	else strcat(ptr,";*");								/* Add semicolon for delete.		*/
	n_desc.dsc$w_length = strlen(the_file);						/* Set the length in the descriptor.	*/ 
	context = 0;
	statval = 0;
	status = lib$find_file(&n_desc,&r_desc,&context,0,0,&statval,0);		/* See if the file exists first.	*/
	lib$find_file_end(&context);							/* End the FIND_FILE context		*/
	if (status != SS$_NORMAL && status != RMS$_NORMAL)
	{
		if (lib_flag) 	ret = 16;
		else		ret = 20;
	}
#else /* !VMS */

	if ( lib_flag )								/* scratch the library				*/
	{
		char libstr[80];
		strcpy( libstr, the_file );
		libstr[ strlen(libstr)-1 ] = '\0';				/* Remove the trailing separator '/' or '\'	*/

		rc = 0;
		if ( !fexists(libstr) ) 					/* Check if library exists			*/
		{
			rc = 1;
			ret = 16;
		}

		buildfilepath(the_file, libstr, "*");				/* Use wildcard for filename			*/
	}
#endif /* !VMS */

	if ( ret == 0 )
	{											/* Do all versions.		*/
		int st_err;									/* Flag to decide which error	*/
#ifdef VMS
		int4 stat;									/* value to use.		*/
#endif

		st_err = 0;									/* Set to use errno values.	*/
		rc = do_delete(the_file,lib_flag,&st_err);					/* now delete it		*/
#ifdef VMS
		if (lib_flag) rc = del_dir(the_file);					/* If library delete then delete dir.	*/
		if (st_err)									/* If should check RMS errors.	*/
		{
			stat = rc;								/* Assign stat to return code.	*/
			rc = EVMSERR;								/* Set so will call chkrms.	*/
		}
		else  stat = vaxc$errno;							/* Set so if need to call chkrms*/
#endif	/* VMS */

		switch (rc)
		{
			case 0: break;								/* no error 			*/
#ifndef WATCOM
			case ENOTDIR: ret=16; break;						/* Libary not found		*/
			case EPERM:   ret=20; break;						/* permission denied 		*/
			case EBUSY:   ret=32; break;						/* file in use			*/
			case ETXTBSY: ret=32; break;						/* file in use			*/
			case EROFS:   ret=52; break;						/* permission denied		*/
			case EFAULT:  ret=48; break;						/* I/O error			*/
#endif
			case ENOENT:  ret=20; break;						/* file not found		*/
			case EACCES:  ret= (lib_flag) ? 52:24; break;				/* permission denied 		*/
#ifdef VMS
			case EVMSERR: ret=chkrms(stat,lib_flag); break;				/* Check RMS error.		*/
#endif
			default:      ret=errno; break;						/* default: errno		*/
		}
	}
	wswap(&ret);
	PUTBIN(retcod,&ret,sizeof(int4));
}

#ifdef VMS
static int4 chkrms(int status, int lib_fl)								/* Check RMS error code.	*/
{
	int4 wrc;										/* Wang style return code.	*/
	switch (status)
	{
		case RMS$_DNR:
		{
			wrc = 4;								/* Wang - Volume not mounted.	*/
			break;
		}
		case RMS$_SHR:	
		{
			wrc = 8;								/* Wang - Volume used exclusively*/
			break;									/* by another user.		*/
		}
		case RMS$_MKD: case RMS$_RMV:
		{
			wrc = 12;								/* Wang - All buffers in use, 	*/
			break;									/* no deletion.			*/
		}
		case RMS$_FNF: case RMS$_WLD: case RMS$_FND: case RMS$_DNF:
		{
			wrc = (lib_fl) ? 16:20; 						/* Wang - File/Library not found.*/
			break;
		}
		case RMS$_PRV:case RMS$_ACC:
		{
			wrc = 24;								/* Wang - Update access denied,	*/
			break;									/* no deletion (single file only)*/
		}
		case RMS$_EXP:
		{
			wrc = 28;								/* Wang - Unexpired file, no 	*/
			break;									/* deletion.			*/
		}
		case RMS$_ACT: case RMS$_FLK: case RMS$_RSA: case RMS$_BUSY:
		{
			wrc = 32;								/* Wang - File in use, no delete.*/
			break;
		}
		case RMS$_ACS: case RMS$_AID: case RMS$_ATR: case RMS$_BLN:			/* Error with FAB.		*/
		{
			wrc = 36;								/* Wang - VTOC error.		*/
			break;
		}
		case RMS$_SUP: case RMS$_NET: case RMS$_NETFAIL:
		{
			wrc = 56;								/* Wang - Cluster link down or	*/
			break;									/* unable to allocate resources.*/
		}
		default:      wrc = 48;	break;							/* set default to I/O error.	*/
	}
	return(wrc);										/* Return the Wang return code.	*/
}
#endif	/* VMS */

#ifdef VMS
static int do_delete(char* file, int lib_flag, int* err_type)
{
	char *p, *strchr(), *strrchr();
	char fpat[128];								/* pattern incl. wildcards			*/
	int	rc;
	int status;								/* status from system routine			*/
	char fnam[128];								/* filename returned by system 			*/
	char *context, *pos;
	int first, err_fl;
#include "scratch2.d"

	rc = 0;									/* Init the return code.			*/
	strcpy(fpat,file);							/* copy to our descriptor			*/

	first = TRUE;								/* Set flag for first file to delete.		*/
	context=0;
	p_desc.dsc$w_length=strlen(fpat);					/* expand if it contains wc's			*/
	pos = strchr(fpat,'*');							/* Get ptr to pos'n of asterisk in filename.	*/
	err_fl = FALSE;								/* Set flag for no errors.			*/
	do									/* Do for each file.				*/
	{
		status = lib$find_file(&p_desc,&f_desc,&context,0,0,0,0);	/* Get system file name.			*/
		if (status == RMS$_NORMAL)
		{
			p = strchr(fnam,' ');
			if (p) *p = '\0';						/* Null terminate returned file name.	*/
			if (unlink(fnam) < 0)						/* Try to delete file.			*/
			{
				rc = errno;						/* Return error - FAILURE.		*/
				goto return_point;
			}
			if (pos == NULL)
			{
				rc =  0;						/* Single file - SUCCESSFUL.		*/
				goto return_point;
			}
		}
		else if (first)								/* If on the first/single file.		*/
		{
			*err_type = 1;							/* Set to use RMS error.		*/
			rc =  status;
			goto return_point;
		}
		else if (lib_flag)							/* If deleting a library and error on 	*/
		{									/* at least one of the files.		*/
			if ((status == RMS$_FLK) || (status == RMS$_PRV) || (status == RMS$_EXP) || (status == RMS$_BUSY) ||
			    (status == RMS$_ACT)) 
			{
				err_fl = TRUE;						/* Set error flag so shows an error.	*/
				status == RMS$_NORMAL;					/* Set status so will continue with lib.*/
			}
		}
		first = FALSE;								/* Set flag so not first file.		*/
	} while (status == RMS$_NORMAL);
	if (err_fl)									/* Open,protected, or unexpired file	*/
	{										/* bypassed.				*/
		rc =  52;								/* (Library deletion only.)		*/
	}
	else 
	{
		rc = 0;									/* Return zero because was successful.	*/
	}
return_point:
	lib$find_file_end(&context);							/* End the FIND_FILE context		*/
	return( rc );
}

#else /* !VMS */

static int do_delete(char* file, int lib_flag, int* err_type)
{
	char fpat[128];								/* pattern incl. wildcards			*/
	char *filename;
	int	first;

	strcpy(fpat,file);							/* copy to our descriptor			*/

	if (lib_flag && strchr(fpat,'*'))
	{
		char	delspec[128];
		int	save_rc;

		first = 1;

		save_rc = 0;

		strcpy(fpat,splitpath(fpat));					/* Remove the file from the path		*/
		while (filename=s_nextfile(fpat,&first))
		{
			buildfilepath(delspec,fpat,filename);
			if (unlink(delspec)<0) 
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
		char tempfile[80];
		int found, has_ext, dat_done;

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

		found = 0;
		dat_done = 0;
		has_ext = hasext(fpat);

		if ( fexists(fpat) )
		{
			found = 1;
			if (unlink(fpat)<0) return errno;
			dat_done = 1;
		}
		else if (has_ext) 
		{
			return ENOENT;						/* If it has ext then thats all we do		*/
		}

		if (!has_ext)							/* May be CISAM files				*/
		{
			strcpy(tempfile,fpat);
			strcat(tempfile,".idx");
			if ( fexists(tempfile) )
			{
				found = 1;
				if (unlink(tempfile)<0) return errno;
			}

			if (!dat_done)						/* May be CISAM with .dat			*/
			{
				strcpy(tempfile,fpat);
				strcat(tempfile,".dat");
				if ( fexists(tempfile) )
				{
					found = 1;
					if (unlink(tempfile)<0) return errno;
					dat_done=1;
				}
			}
		}

		if (!found) return ENOENT;

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
#endif /* !VMS */


#ifdef VMS
static int del_dir(char* file_spec)						/* Attempt to delete the directory.		*/
{
	char the_dir[132], *cpos;
	struct FAB fab;									/* RMS data structures.			*/
	struct NAM nam;
	int ret_code;
	int pcnt, dirlen;
	register i;

	return(0);		/* Not working yet!  NEED TO FINISH.  SMB  12/14/90	*/

	i = 0;
	while (file_spec[i] != ']') the_dir[i] = file_spec[i++];		/* Copy dir name to temp var.			*/
	cpos = &the_dir[i];
	memcpy(cpos,".DIR;",5);							/* Copy type to dir specification.		*/
	the_dir[i+6] = '\0';							/* Null terminate the string.			*/
	pcnt = 0;
	dirlen = strlen(the_dir);
	for (i = dirlen; i >= 0; i--)						/* Find position of . or [ so generate dir 	*/
	{									/*  name properly.				*/
		if (the_dir[i] == '.' || the_dir[i] == '[')
		{
			if (the_dir[i] == '.') pcnt++;				/* Is the . before type description .DIR	*/
			else							/* else is the [ and no more path.		*/
			{
				char temp[128];
				register j;					/* Put 000000 in the dir name string.		*/

				cpos = &the_dir[i+1];
				strcpy(temp,cpos);
				memcpy(cpos,"000000]",7);			/* Copy to dir specification.			*/
				cpos = &the_dir[i+8];
				memcpy(cpos,temp,strlen(temp));
				the_dir[i+7+strlen(temp)] = '\0';		/* Null terminate the string.			*/
				break;
			}
			if (pcnt == 2 && the_dir[i] == '.')
			{
				the_dir[i] = ']';
				break;
			}
		}
	}

	fab = cc$rms_fab;	 							/* Intialize the FAB structure.		*/
	fab.fab$l_fna = the_dir;							/* Set address of filename string.	*/
	fab.fab$b_fns = strlen(the_dir);						/* Set the size of the filename string.	*/
	fab.fab$l_nam = &nam;								/* Address of the NAM structure block.	*/
	fab.fab$b_fac = FAB$M_DEL;							/* Set access flags.			*/
	fab.fab$l_fop = FAB$V_DLT;							/* Set to delete file when closed.	*/
	fab.fab$b_shr = FAB$M_SHRDEL;							/* Allow all access.			*/
	nam = cc$rms_nam;								/* Initialize the NAM structure block.	*/

	sys$open(&fab);									/* Attempt to open the file.		*/
	if (fab.fab$w_ifi) sys$close(&fab);						/* Close the file.			*/

	if (fab.fab$l_sts == RMS$_SUC || fab.fab$l_sts == RMS$_NORMAL) return(0);	/* Was a success ful delete of dir.	*/
	else return(16);
}
#endif	/* VMS */
/*
**	History:
**	$Log: scratch.c,v $
**	Revision 1.14  1996-10-08 20:24:47-04  gsl
**	replace getenv() with wispscratchmode() call
**
**	Revision 1.13  1996-08-19 15:32:52-07  gsl
**	drcs update
**
**
**
*/
