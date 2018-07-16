static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	 WFACCESS ... A special routine to determine whether a file exists and whether the user has access to that file.

			WFACCESS is passed a native filepath and a mode and it returns a status code.

			Status Code		Description
			===========		===========
			ACC_ALLOWED		Access allowed
			ACC_DENIED		File access not allowed
			ACC_NOFILE		File doesn't exist (Mode= IO or INPUT)
			ACC_NODIR		Directory Path doesn't exist
			ACC_MISSING		Directory or File doesn't exist
			ACC_LOCKED		File is exclusively locked 
			ACC_NOLOCK		File is in a state where it can't be locked
			ACC_NOLINK		File can not be physically located at this time
			ACC_BADDIR		Path has invalid directories in it
			ACC_READONLY		Write access requested on a readonly device
			ACC_INUSE		File in in use
			ACC_EXISTS		File currectly exists and cannot be overridden
			ACC_OUTEXISTS		File exists and your trying to open output
			ACC_SYSLIMIT		System limits have been exceeded
			ACC_BADDEV		Device can not be accessed
			ACC_BADVOL		No translation for VOLUME
			ACC_UNKNOWN		UNKNOWN
*/

#include <stdio.h>									/* Standard I/O include file.		*/
#include <errno.h>
#include <string.h>

#ifdef _MSC_VER
#include <io.h>
#endif

#if defined(unix) || defined(MSDOS) || defined(WIN32)
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>                          
#endif

#include "idsistd.h"
#include "wdefines.h"
#include "wcommon.h"
#include "wfaccess.h"
#include "cobrun.h"
#include "idsisubs.h"
#include "wisplib.h"
#include "werrlog.h"

extern char wfilestat[2];								/* Last filestat form wfilechk		*/

#ifdef VMS
#include <file.h>	    								/* File definition include file.	*/
#include <rms.h>
#include <descrip.h>

int wfaccess(char* native_path, int4* mode)
{

	char 	filename[81];			 					/* Pre allocated storage for file name.	*/
	char 	tempname[81];
        int  	file_stat;								/* Value returned by the open function.	*/
	int  	file_desc;
	int  	close_stat;
	int  	x, ret_code, i;
	char	*ptr;
	unsigned dmode;
	char dstr[81];
	char object_name[81], expanded_name[81];					/* Output from $PARSE and $SEARCH	*/

	struct FAB fab;									/* RMS data structures.			*/
	struct NAM nam;

	memcpy(filename,native_path,80);  						/* Move the filename in.		*/
	ptr = memchr(filename,' ',80);
	if ( ptr ) *ptr = NULL_CHAR;							/* Null terminate it.			*/
	else filename[80] = NULL_CHAR;


	if (*mode & IS_IO)								/* Is this an IO open ?			*/
	{										/* Yup.					*/
		fab = cc$rms_fab; 							/* Intialize the FAB structure.		*/
		fab.fab$l_dna = 0;							/* No default name.			*/
		fab.fab$b_dns = 0;							/* No size either.			*/
		fab.fab$l_fna = filename;						/* Set address of filename string.	*/
		fab.fab$b_fns = strlen(filename);					/* Set the size of the filename string.	*/
		fab.fab$l_nam = &nam;							/* Address of the NAM structure block.	*/
		fab.fab$b_fac = FAB$M_DEL+FAB$M_GET+FAB$M_PUT+FAB$M_UPD;		/* Set access flags.		*/

		nam = cc$rms_nam;							/* Initialize the NAM structure block.	*/
		nam.nam$l_esa = expanded_name;						/* Set address of the expanded name.	*/
		nam.nam$b_ess = sizeof(expanded_name);					/* Set size of expanded name string.	*/
		nam.nam$l_rsa = object_name;						/* Set address of resultant string.	*/
		nam.nam$b_rss = sizeof(object_name);					/* Set size of resultant name string.	*/
		nam.nam$b_nop = NAM$M_NOCONCEAL;

		if (*mode & IS_NOWRITE)							/* no writers will be allowed.		*/
		{
			fab.fab$b_shr = FAB$M_SHRGET;					/* Allow only read access.		*/
		}
		else									/* Allow writers.			*/
		{
			fab.fab$b_shr = FAB$M_SHRGET+FAB$M_SHRPUT+FAB$M_SHRUPD+FAB$M_SHRDEL;	/* Allow all access.		*/
		}

		ret_code = sys$open(&fab);						/* Attempt to open the file.		*/

		if (fab.fab$w_ifi) sys$close(&fab);					/* Close the file.			*/

		if (ret_code == RMS$_SUC || ret_code == RMS$_NORMAL)
		{									/* All OK.				*/
			return(ACC_ALLOWED);
		}

		goto report_in_access;							/* Now report what happened.		*/
	}

        if (*mode & IS_OUTPUT)								/* Is this an open output ?		*/
	{    
		fab = cc$rms_fab; 							/* Intialize the FAB structure.		*/
		fab.fab$l_dna = 0;							/* No default name.			*/
		fab.fab$b_dns = 0;							/* No size either.			*/
		fab.fab$l_fna = filename;						/* Set address of filename string.	*/
		fab.fab$b_fns = strlen(filename);					/* Set the size of the filename string.	*/
		fab.fab$l_nam = &nam;							/* Address of the NAM structure block.	*/

		nam = cc$rms_nam;							/* Initialize the NAM structure block.	*/
		nam.nam$l_esa = expanded_name;						/* Set address of the expanded name.	*/
		nam.nam$b_ess = sizeof(expanded_name);					/* Set size of expanded name string.	*/
		nam.nam$l_rsa = object_name;						/* Set address of resultant string.	*/
		nam.nam$b_rss = sizeof(object_name);					/* Set size of resultant name string.	*/
		nam.nam$b_nop = NAM$M_NOCONCEAL;

		ret_code = sys$parse(&fab);						/* Parse the name for $SEARCH.		*/
		ret_code = sys$search(&fab);						/* Search for the file.			*/

		switch( ret_code )
		{
		case	RMS$_FNF:
		case	RMS$_NMF:	return(ACC_ALLOWED);

		case	RMS$_NORMAL:	return(ACC_OUTEXISTS);

		case	RMS$_DVI:	return(ACC_MISSING);

		case	RMS$_DME:	return(ACC_SYSLIMIT);

		case	RMS$_DEV:
		case	RMS$_SUP:
		case	RMS$_WCC:
		case	RMS$_ESA:	return(ACC_BADDIR);

		case	RMS$_SUPPORT:
		case	RMS$_NET:
		case	RMS$_DNR:
		case	RMS$_NETFAIL:	return(ACC_NOLINK);

		case	RMS$_PRV:	return(ACC_DENIED);

		case	RMS$_DNF:	return(ACC_NODIR);

		case	RMS$_RST:
		case	RMS$_NOVALPRS:
		case	RMS$_FND:
		case	RMS$_RSS:
		case	RMS$_NAM:
		case	RMS$_FAB:
		case	RMS$_CHN:
		case	RMS$_RSL:
		case	RMS$_IFI:
		case	RMS$_ESL:
		case	RMS$_BLN:
		case	RMS$_SYS:
		case	RMS$_STR:
		case	RMS$_ACS:	return(ACC_UNKNOWN);
		}
		return(ACC_UNKNOWN);
	}
	else										/* Open it up for input.		*/
	{

		fab = cc$rms_fab; 							/* Intialize the FAB structure.		*/
		fab.fab$l_dna = 0;							/* No default name.			*/
		fab.fab$b_dns = 0;							/* No size either.			*/
		fab.fab$l_fna = filename;						/* Set address of filename string.	*/
		fab.fab$b_fns = strlen(filename);					/* Set the size of the filename string.	*/
		fab.fab$l_nam = &nam;							/* Address of the NAM structure block.	*/

		nam = cc$rms_nam;							/* Initialize the NAM structure block.	*/
		nam.nam$l_esa = expanded_name;						/* Set address of the expanded name.	*/
		nam.nam$b_ess = sizeof(expanded_name);					/* Set size of expanded name string.	*/
		nam.nam$l_rsa = object_name;						/* Set address of resultant string.	*/
		nam.nam$b_rss = sizeof(object_name);					/* Set size of resultant name string.	*/
		nam.nam$b_nop = NAM$M_NOCONCEAL;

		if (*mode & IS_NOWRITE)							/* no writers will be allowed.		*/
		{
			fab.fab$b_shr = FAB$M_SHRGET;					/* Allow only read access.		*/
		}
		else									/* Allow writers. Must be SPECIAL-INPUT.*/
		{
			fab.fab$b_shr = FAB$M_SHRGET+FAB$M_SHRPUT+FAB$M_SHRUPD+FAB$M_SHRDEL;	/* Allow all access.		*/
		}

		ret_code = sys$open(&fab);						/* Attempt to open the file.		*/
		if (fab.fab$w_ifi) sys$close(&fab);					/* Close the file.			*/

		if (ret_code == RMS$_SUC || ret_code == RMS$_NORMAL)
		{
			return(ACC_ALLOWED);
		}

report_in_access:
		switch( ret_code )
		{
		case	RMS$_ACT:	return(ACC_INUSE);
		case	RMS$_BUG_DDI:	return(ACC_BADDIR);
		case	RMS$_CRMP:	return(ACC_SYSLIMIT);
		case	RMS$_DIR:	return(ACC_BADDIR);
		case	RMS$_DEV:	return(ACC_BADDIR);
		case	RMS$_DME:	return(ACC_SYSLIMIT);
		case	RMS$_DNF:	return(ACC_NODIR);
		case	RMS$_DNR:	return(ACC_NOLINK);
		case	RMS$_DVI:	return(ACC_MISSING);
		case	RMS$_ENQ:	return(ACC_NOLOCK);
		case	RMS$_FLK:	return(ACC_NOLOCK);
		case	RMS$_FNF:	return(ACC_NOFILE);
		case	RMS$_FNM:	return(ACC_BADDIR );
		case	RMS$_IFI:	return(ACC_INUSE);
		case	RMS$_NET:	return(ACC_NOLINK);
		case	RMS$_NETFAIL:	return(ACC_NOLINK);
		case	RMS$_NOD:	return(ACC_BADDIR );
		case	RMS$_PRV:	return(ACC_DENIED);
		case	RMS$_SUP:	return(ACC_BADDIR);
		case	RMS$_SUPPORT:	return(ACC_NOLINK);
		case	RMS$_SYN:	return(ACC_BADDIR);
		}
		
		return(ACC_UNKNOWN);						/* RMS services open was not successful.*/
	}                                                                                                                         
}
#endif	/* VMS */

#if defined(unix) || defined(MSDOS) || defined(WIN32)
static int x_wfaccess(char* filename, int4* mode);

/*
**	Routine:	wfaccess()
**
**	Function:	To check the access of a file before it is opened by cobol.
**
**	Description:	This routine checks if a file exists or not and if we have
**			access to it depending on the mode.
**			We only check if the file exists because access() uses the
**			the real UID not the effective UID.
**
**	Arguments:
**	native_path	The native file path as returned from wfname(). (Space padded)
**	mode		The mode.
**
**	Globals:	None
**
**	Return:		The access code.
**	ACC_ALLOWED	The file exists (IO,INPUT) or can be created (OUTPUT). 
**	ACC_MISSING	The doesn't exist (IO,INPUT).
**	ACC_OUTEXISTS	The file exists (OUTPUT).
**	ACC_DENIED	Access is denied.
**	ACC_UNKNOWN	File exists but can't tell if access allowed.
**	ACC_xxx		Misc other access errors.
**	
**	Warnings:	If this routine returns an ACC_UNKNOWN you should go ahead and
**			let COBOL attempt to access the file.
**
**	History:	
**	mm/dd/yy	Written by xxx
**	02/12/93	Rewritten to not use of eaccess(). GSL
**	04/13/93	Removed call to access() to check read and write access. GSL
**	04/26/93	Changed to use fexists() instead of access() to check if file exists. GSL
**
*/
int wfaccess(char* native_path, int4* mode)
{
	char 	filename[COB_FILEPATH_LEN + 1];	 				/* The filename (null terminated string)	*/
	int	rc;

	cobx2cstr(filename,native_path,COB_FILEPATH_LEN);  			/* Move the filename in.			*/
	rc = x_wfaccess(filename, mode);

	if (wtracing())
	{
		char	*result;
		
		switch(rc)
		{
		case ACC_OUTEXISTS:	result = "ACC_OUTEXISTS";	break;
		case ACC_ALLOWED:	result = "ACC_ALLOWED";		break;
		case ACC_BADDIR:	result = "ACC_BADDIR";		break;
		case ACC_NODIR:		result = "ACC_NODIR";		break;
		case ACC_READONLY:	result = "ACC_READONLY";	break;
		case ACC_EXISTS:	result = "ACC_EXISTS";		break;
		case ACC_UNKNOWN:	result = "ACC_UNKNOWN";		break;
		case ACC_MISSING:	result = "ACC_MISSING";		break;
		default:		result = "ACC_????";		break;
		}
		
		wtrace("WFACCESS","RESULT","File=[%s] Mode=[x0%08X] %s rc=%d ", filename, *mode, result, rc);
	}

	return rc;
}

static int x_wfaccess(char* filename, int4* mode)
{
	char	fileidx[81];							/* Filename with .idx extension			*/
	int  	file_desc;

	strcpy(fileidx,filename);						/* Construct fileidx				*/
	strcat(fileidx,".idx");

        if (*mode & IS_OUTPUT)							/* Is this an open output ?			*/
	{    
		if (fexists( filename))						/* check if file exists				*/
		{
			return(ACC_OUTEXISTS);					/* File exists					*/
		}

		if (*mode & IS_INDEXED)						/* CISAM uses .idx for INDEXED			*/
		{
			if (fexists(fileidx))					/* check if idx file exists			*/
			{
				return(ACC_OUTEXISTS);				/* File exists					*/
			}
		}

		if (-1 != (file_desc = creat(filename, 00666)))			/* See if she'll open up.			*/
		{
			close(file_desc);
			unlink(filename);
			return( ACC_ALLOWED );
		}
										/* We got an error.				*/
		if ( filename[0] == NULL_CHAR ) return( ACC_BADDIR );

		switch( errno )
		{
		case ENOENT:	return( ACC_NODIR );
#ifndef MSDOS
		case ENOTDIR:	return( ACC_BADDIR );
		case EROFS:	return( ACC_READONLY );
		case EISDIR:	return( ACC_EXISTS );
		case EFAULT:	return( ACC_BADDIR );
#endif
		}
		return( ACC_UNKNOWN );
	}
	else									/* IO or INPUT mode				*/
	{
		if (fexists(filename))						/* Check if file exists				*/
		{
			return(ACC_UNKNOWN);					/* File exists but access is unknown		*/
		}

		if (*mode & IS_INDEXED)						/* CISAM uses .idx for INDEXED			*/
		{
			if (fexists(fileidx))					/* check if idx file exists			*/
			{
				strcpy(filename,fileidx);			/* Use the idx part as the name			*/
				return(ACC_UNKNOWN);				/* File exists but access is unknown		*/
			}
		}
		return(ACC_MISSING);						/* File was not found				*/

	}                                                                                                                         
}
#endif	/* unix  || MSDOS || WIN32 */

/*
**	History:
**	$Log: wfaccess.c,v $
**	Revision 1.15  1998/08/03 21:15:25  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks.
**	
**	Revision 1.14  1998-05-12 13:52:31-04  gsl
**	Add wtrace() logig
**
**	Revision 1.13  1997-04-29 13:39:02-04  gsl
**	Move acc_message() to wfopen.c
**
**	Revision 1.12  1997-03-12 13:19:48-05  gsl
**	Changed to use WIN32 define
**
**	Revision 1.11  1996-09-10 11:48:27-04  gsl
**	combine include code
**
**	Revision 1.10  1996-08-19 15:33:12-07  gsl
**	drcs update
**
**
**
*/
