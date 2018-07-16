			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/* Scratch a file or a library													*/

#ifdef VMS
#include <ssdef.h>
#include <descrip.h>
#include <rmsdef.h>
#include <file.h>	    								/* File definition include file.	*/
#include <rms.h>
#endif

#ifdef unix
#include <sys/types.h>
#include <dirent.h>
#endif

#ifdef MSDOS
#include <direct.h>
#include <dos.h>
#include <sys/types.h>
#endif

#include <errno.h>
#include <varargs.h>
#include <v/video.h>

#include "wcommon.h"
#include "wdefines.h"
#include "movebin.h"
#include "cobrun.h"

extern char WISPFILEXT[39];
extern char *wfname();

SCRATCH(va_alist)
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
	long int  *retcod;						/* return code						*/
	long mode;							/* the mode for wfname					*/
	char the_file[132];						/* actual file name					*/
	int  	j;
	char	*ptr;
	int	lib_flag, vmstry;
	long	ret, rc, status;
#ifdef VMS
	char result[256], *context, *statval;						/* Vars to use with lib$find_file	*/

$DESCRIPTOR(n_desc,the_file);
$DESCRIPTOR(r_desc,result);
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
	retcod = va_arg(the_args,long*);

	if ( (*tflag == 'L') || (*tflag =='l') )
	{
		mode |= IS_LIB;									/* this is a library		*/
		lib_flag = TRUE;
	}

vmstryagain:
	if (!memcmp(WISPFILEXT,"LIS ",4))
	{
		mode |= IS_PRINTFILE;
		vmstry++;
	} 
	ptr = wfname( &mode, vol, lib, fname, the_file );					/* create native filename	*/
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
		if (vmstry < 1)								/* Try with next specification.		*/
		{
			vmstry++;
			setwispfilext("LIS");						/* Set file extension.			*/
			goto vmstryagain;
		}
	}
#endif	/* VMS */

#ifdef unix
	if ( lib_flag )										/* scratch the library		*/
	{
		char libstr[80];
		strcpy( libstr, the_file );
		if ( libstr[ strlen(libstr)-1 ] == '/' )
		{
			libstr[ strlen(libstr)-1 ] = '\0';
		}

		if ( rc = access( libstr, 00 ) ) ret = 16;

		strcpy( the_file, libstr );
		strcat( the_file, "/*" );							/* file name is *		*/
	}
#endif	/* unix */

#ifdef MSDOS
	if ( lib_flag )										/* scratch the library		*/
	{
		char libstr[80];
		strcpy( libstr, the_file );
		if ( libstr[ strlen(libstr)-1 ] == '\\' )
		{
			libstr[ strlen(libstr)-1 ] = '\0';
		}

		if ( rc = access( libstr, 00 ) ) ret = 16;

		strcpy( the_file, libstr );
		strcat( the_file, "\\*.*" );							/* file name is *		*/
	}
#endif	/* MSDOS */

	if ( ret == 0 )
	{											/* Do all versions.		*/
		int st_err;									/* Flag to decide which error	*/
		long stat;									/* value to use.		*/

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
			case ENOTDIR: ret=16; break;						/* Libary not found		*/
			case ENOENT:  ret=20; break;						/* file not found		*/
			case EACCES:  ret= (lib_flag) ? 52:24; break;				/* permission denied 		*/
			case EPERM:   ret=20; break;						/* permission denied 		*/
			case EBUSY:   ret=32; break;						/* file in use			*/
			case ETXTBSY: ret=32; break;						/* file in use			*/
			case EROFS:   ret=52; break;						/* permission denied		*/
			case EFAULT:  ret=48; break;						/* I/O error			*/
#ifdef VMS
			case EVMSERR: ret=chkrms(stat,lib_flag); break;				/* Check RMS error.		*/
#endif
			default:      ret=errno; break;						/* default: errno		*/
		}
	}
	wswap(&ret);
	PUTBIN(retcod,&ret,sizeof(long));
}

#ifdef VMS
static long chkrms(status,lib_fl)								/* Check RMS error code.	*/
int status, lib_fl;
{
	long wrc;										/* Wang style return code.	*/
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
static int do_delete(file, lib_flag, err_type)
char	*file;
int	lib_flag, *err_type;
{
	char *p, *strchr(), *strrchr();
	char fpat[128];								/* pattern incl. wildcards			*/
	int	rc;
	int status;								/* status from system routine			*/
	char fnam[128];								/* filename returned by system 			*/
	char *context, *pos;
	int first, err_fl;
$DESCRIPTOR(f_desc, fnam);
$DESCRIPTOR(p_desc, fpat);

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
			if (delete(fnam) < 0)						/* Try to delete file.			*/
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
#endif /* VMS */

#ifdef unix
static int do_delete(file, lib_flag, err_type)
char	*file;
int	lib_flag, *err_type;
{
	char *p, *strchr(), *strrchr();
	char fpat[128];								/* pattern incl. wildcards			*/
	char *s_nextfile();							/* function to read directories			*/
	DIR *cur;								/* dir pointer for dirent routines		*/
	char *filename;
	char	*ptr;

	strcpy(fpat,file);							/* copy to our descriptor			*/

	if (lib_flag && strchr(fpat,'*'))
	{
		char	delspec[128];
		int	save_rc;

		cur = NULL;
		save_rc = 0;

		p = strrchr(fpat,'/');
		*p = (char)0;
		while (filename=s_nextfile(fpat,&cur)) 
		{
			sprintf(delspec,"%s/%s",fpat,filename);
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
		int found;

		found = 0;

		if ( 0 == access(fpat,00) )
		{
			found = 1;
			if (unlink(fpat)<0) return errno;
		}
		else
		{
			if (strchr(fpat,'.')) return ENOENT;				/* If it has ext then thats all we do	*/
		}

		if (cisam_files || run_files == FILES_OTHER)
		{
			strcpy(tempfile,fpat);
			strcat(tempfile,".idx");
			if ( 0 == access(tempfile,00) )
			{
				found = 1;
				if (unlink(tempfile)<0) return errno;
			}

			strcpy(tempfile,fpat);
			strcat(tempfile,".dat");
			if ( 0 == access(tempfile,00) )
			{
				found = 1;
				if (unlink(tempfile)<0) return errno;
			}

		}

		if (!found) return ENOENT;

		/*
		**	Try to remove the directory.  If empty it will be removed otherwise the rmdir will fail.
		*/
		strcpy(tempfile,fpat);
		if ( ptr = strrchr(&tempfile[1],'/') )				/* Find the last '/' in the path		*/
		{
			*ptr = '\0';						/* Remove the file part of the name.		*/
			rmdir(tempfile);					/* Attempt to remove the directory		*/
		}
	}
	else
	{
		return 44;								/* Lib flag with no '*' ?		*/
	}

	return( 0 );
}
#endif /* unix */

#ifdef MSDOS
static int do_delete(file, lib_flag, err_type)
char	*file;
int	lib_flag, *err_type;
{
	char *p, *strchr(), *strrchr();
	char fpat[128];								/* pattern incl. wildcards			*/
	static char *s_nextfile(char *, int *);					/* function to read directories			*/
	int doing_wild;								/* directory flag for readnext routines		*/
	char *filename;

	strcpy(fpat,file);							/* copy to our descriptor			*/

	if (lib_flag && strchr(fpat,'*'))						/* Actually, "*.*" will be in fpat.	*/
	{
		char	delspec[128];
		int	save_rc;

		doing_wild = 0;
		save_rc = 0;

		p = strrchr(fpat,'\\');
		*p = (char)0;
		while (filename=s_nextfile(fpat,&doing_wild)) 
		{
			sprintf(delspec,"%s\\%s",fpat,filename);
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
		int found;

		found = 0;

		if ( 0 == access(fpat,00) )
		{
			found = 1;
			if (unlink(fpat)<0) return errno;
		}
		else
		{
			if (strchr(fpat,'.')) return ENOENT;				/* If it has ext then thats all we do	*/
		}

		if (cisam_files || run_files == FILES_OTHER)
		{
			strcpy(tempfile,fpat);
			strcat(tempfile,".idx");
			if ( 0 == access(tempfile,00) )
			{
				found = 1;
				if (unlink(tempfile)<0) return errno;
			}

			strcpy(tempfile,fpat);
			strcat(tempfile,".dat");
			if ( 0 == access(tempfile,00) )
			{
				found = 1;
				if (unlink(tempfile)<0) return errno;
			}

		}

		if (!found) return ENOENT;
	}
	else
	{
		return 44;								/* Lib flag with no '*' ?		*/
	}

	return( 0 );
}
#endif	/* MSDOS */

#ifdef unix
static char *s_nextfile(path,curdir)
char *path;
DIR **curdir;
{
	struct dirent *dp;

	if ( ! *curdir )
	{
		if (!(*curdir = opendir(path))) 
		{
			return(NULL);
		}
	}

	for (;;)
	{
		if ((dp=readdir(*curdir))==NULL)
		{
			closedir(*curdir);
			return(NULL);
		}

		if (0 != strcmp(dp->d_name,".") &&				/* skip entry for . */
		    0 != strcmp(dp->d_name,"..")   )				/* skip entry for .. */
		{
			return dp->d_name; 
		}
	}
}
#endif	/* unix */

#ifdef MSDOS
static char *s_nextfile(path,curdir)
char *path;
int *curdir;
{
	char	wildpath[128];
	static	char	file_name[13];
	static	struct	find_t	fileinfo;

	if ( ! *curdir )
	{
		strcpy( wildpath, path );
		strcat( wildpath, "\\*.*" );

		if( 0 != _dos_findfirst( wildpath,
			(_A_NORMAL | _A_RDONLY),				/* | _A_SUBDIR if directories also wanted.	*/
			&fileinfo) )
		{
			return(NULL);
		}
		*curdir = 1;
		strcpy( file_name, fileinfo.name );
		return( file_name );
	}
	if ( 0 != _dos_findnext( &fileinfo ) )
	{
		*curdir = 0;
		return(NULL);
	}	
	strcpy( file_name, fileinfo.name );
	return( file_name );
}
#endif	/* MSDOS */

#ifdef VMS
static int del_dir(file_spec)							/* Attempt to delete the directory.		*/
char *file_spec;
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
