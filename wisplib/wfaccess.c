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

#include "idsistd.h"
#include "wdefines.h"
#include "wcommon.h"
#include "wfaccess.h"
#include "cobrun.h"

extern char wfilestat[2];								/* Last filestat form wfilechk		*/
extern char ACUFILESTAT[4];								/* Acucobol extended file status	*/

#ifdef VMS
#include <file.h>	    								/* File definition include file.	*/
#include <rms.h>
#include <descrip.h>
#include <string.h>

int wfaccess(native_path, mode)	                                               		/* Gotta know where the args are.	*/

char *native_path;									/* Addr. of file name and open mode arg */
int4 *mode;
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

#if defined(unix) || defined(MSDOS)

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>                          
#include <memory.h>

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
int wfaccess(native_path, mode)
char native_path[80];
int4 *mode;
{
	char 	filename[81];			 				/* The filename (null terminated string)	*/
	char	fileidx[81];							/* Filename with .idx extension			*/
	int  	file_desc;
	char	*ptr;

	unloadpad(filename,native_path,80);  					/* Move the filename in.			*/
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

#ifdef OLD
		return_stat = access( filename, 00 );				/* check if file exists				*/
		if (return_stat != 0)
		{
			if (*mode & IS_INDEXED)					/* CISAM uses .idx for INDEXED			*/
			{
				return_stat = access( fileidx, 00 );		/* check if idx file exists			*/
			}
			if (return_stat != 0)
			{
				return(ACC_MISSING);				/* File was not found				*/
			}
			else
			{
				strcpy(filename,fileidx);			/* Perpare to use filename as the name		*/
			}
		}

		if ( 0 != access(filename,04) )					/* Check READ access				*/
		{
			switch(errno)
			{
			case EACCES:	return(ACC_DENIED);
			case ENOENT:	return(ACC_MISSING);
			}
			return(ACC_UNKNOWN);
		}

		if (*mode & IS_IO)						/* If IO then also check WRITE access		*/
		{
			if ( 0 != access(filename,02) )				/* Check WRITE access				*/
			{
				switch(errno)
				{
				case EACCES:	return(ACC_DENIED);
				case ENOENT:	return(ACC_MISSING);
				}
				return(ACC_UNKNOWN);
			}
		}

		return(ACC_ALLOWED);
#endif /* OLD */
	}                                                                                                                         
}
#endif	/* unix  || MSDOS */

#ifdef OLD
int eaccess(file,mode)									/* Check Effective Access		*/
char *file;
int4 mode;
{
	int our_euid, file_uid, our_egid, file_gid;
	ushort amode;
	struct stat statbuf;
	int	rc;

	rc = stat(file,&statbuf);
	if ( rc )
	{
		switch( errno )
		{
		case ENOTDIR:	return( ACC_BADDIR );
		case ENOENT:	return( ACC_MISSING );
		case EACCES:	return( ACC_DENIED );
		case EFAULT:	return( ACC_BADDIR );
		case EINTR:	return( ACC_UNKNOWN );
		}
		return( ACC_UNKNOWN );
	}

	if (statbuf.st_mode & S_IFDIR) return( ACC_BADDIR );

	amode = statbuf.st_mode;							/* get file's access mode		*/
	file_uid = (int)statbuf.st_uid;							/* and owner 				*/
	file_gid = (int)statbuf.st_gid;							/* / group info				*/
	our_euid = geteuid();								/* get our effective uid		*/
	our_egid = getegid();								/* and effective gid			*/
	if ( (file_uid == our_euid) &&
		( (amode & 0700 & (mode << 6)) == (mode << 6) ) ) return(ACC_ALLOWED);	/* if we own file, check file's u bits	*/
	if ( (file_gid == our_egid) && 
		( (amode & 070 & (mode << 3)) == (mode << 3))) return(ACC_ALLOWED);	/* if we are same grp, chk g bits	*/
	if ((amode & 07 & mode) == mode) return(ACC_ALLOWED);				/* compare other bits			*/
	return(ACC_DENIED);
}
#endif /* OLD */

acc_message(access_status,msgbuf)
int	access_status;
char   *msgbuf;
{
	switch( access_status )
	{
	case ACC_DENIED:
		strcpy(msgbuf,"READ OR WRITE ACCESS DENIED FOR SPECIFIED FILE.");
		break;
	case ACC_NOFILE:
		strcpy(msgbuf,"SPECIFIED FILE DOES NOT EXIST.");
		break;
	case ACC_NODIR:                                                    
		strcpy(msgbuf,"UNABLE TO CREATE DIRECTORY FOR SPECIFIED FILE.");
		break;
	case ACC_LOCKED:
		strcpy(msgbuf,"SPECIFIED FILE IS LOCKED BY ANOTHER USER.");
		break;
	case ACC_NOLOCK:
		strcpy(msgbuf,"UNABLE TO LOCK SPECIFIED FILE.");
		break;
	case ACC_NOLINK:
		strcpy(msgbuf,"UNABLE TO PHYSICALLY ACCESS SPECIFIED FILE.");
		break;
	case ACC_BADDIR:                                                             
		strcpy(msgbuf,"INVALID DIRECTORY IN PATH OF SPECIFIED FILE.");
		break;
	case ACC_READONLY:
		strcpy(msgbuf,"WRITE ACCESS REQUESTED ON READ-ONLY DEVICE.");
		break;
	case ACC_INUSE:
		strcpy(msgbuf,"SPECIFIED FILE IN USE BY ANOTHER USER.");
		break;
	case ACC_EXISTS:
		strcpy(msgbuf,"SPECIFIED FILE ALREADY EXISTS, NO WRITE ACCESS.");
		break;
	case ACC_SYSLIMIT:
		strcpy(msgbuf,"A SYSTEM LIMIT HAS BEEN EXCEEDED.");
		break;
	case ACC_MISSING:
		strcpy(msgbuf,"SPECIFIED FILE OR DIRECTORY PATH DOES NOT EXIST.");
		break;
	case ACC_BADDEV:
		strcpy(msgbuf,"UNABLE TO ACCESS SPECIFIED DEVICE.");
		break;
	case ACC_BADVOL:
		strcpy(msgbuf,"VOLUME NOT FOUND.");
		break;
	case ACC_OUTEXISTS:
		strcpy(msgbuf,"FILE EXISTS, RENAME OR PRESS PF3 TO CONTINUE.");
		break;
	case ACC_ERROPEN:
		strcpy(msgbuf,"OPEN FAILED WITH FILE STATUS ");
		{
			char	fsbuf[40];
			if ((mf_cobol || aix_cobol) && wfilestat[0] == '9')
			{
				sprintf(fsbuf,"[9/RT%03d]",(unsigned)wfilestat[1]);
			}
			else
			{
				sprintf(fsbuf,"[%2.2s]", wfilestat);
			}
			strcat(msgbuf,fsbuf);
		}
		if (acu_cobol)
		{
			char acustring[40];

			if ( ACUFILESTAT[2] < ' ' || ACUFILESTAT[2] > '~' ||			/* If status not printable	*/
			     ACUFILESTAT[3] < ' ' || ACUFILESTAT[3] > '~' ) 
			{
				sprintf(acustring,"[0x%02x,0x%02x]",(unsigned int)(ACUFILESTAT[2]),(unsigned int)(ACUFILESTAT[3]));
			}
			else
			{
				sprintf(acustring,"[%c%c]",ACUFILESTAT[2],ACUFILESTAT[3]);
			}
			strcat(msgbuf,acustring);
		}
		break;
	case ACC_UNKNOWN:
		strcpy(msgbuf,"UNABLE TO OPEN SPECIFIED FILE. (REASON UNKNOWN)");
		break;
	default:
		strcpy(msgbuf,"UNABLE TO OPEN SPECIFIED FILE. (REASON UNAVAILABLE)");
		break;
	}
}
