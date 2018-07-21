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
**	File:		wfopen.c
**
**	Project:	WISP
**
**	Purpose:	Wang VS COBOL Open logic
**
**	Routines:	
**	WFOPEN()	Obsolete
**	WFOPEN2()
**	WFOPEN3()
*/

/*
**	Includes
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef WIN32
#include <io.h>
#endif
#ifdef unix
#include <unistd.h>
#endif

#include "idsistd.h"
#include "wperson.h"
#include "wangkeys.h"									/* Include the wang pfkey defs.		*/
#include "wcommon.h"
#include "wfaccess.h"
#include "cobrun.h"
#include "wglobals.h"
#include "filext.h"
#include "wmalloc.h"
#include "assert.h"
#include "wdefines.h"
#include "rvmap.h"
#include "wisplib.h"
#include "wexit.h"
#include "idsisubs.h"
#include "wfname.h"
#include "filgparm.h"

#include "werrlog.h"
/*
78001   %%WFOPEN-I-ENTRY Entry into wfopen file(%8.8s) lib(%8.8s) vol(%6.6s) appl(%8.8s)
*/


/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/
extern const char *wisp_get_last_filecheckstatus();
extern const char *wisp_get_last_filecheckstatus_ext();


/*
**	Static data
*/


/*
**	Static Function Prototypes
*/
static void acc_message(int access_status, 
			char* msgbuf, 
			const char filestatus[2],
			const char *filestatus_extended);

/********************************************************************************************************************************/
void WFOPEN4(
	     char *attrstr,				/* File attributes (10 chars)		*/
	     char *vol,					/* the WANG volume name	(6 chars)	*/
	     char *lib,					/* The WANG library name (8 chars)	*/
	     char *file,				/* The file name	(8 chars)	*/
	     char *path,				/* The resultant name			*/
	     const char *appl,				/* The COBOL program id (8 chars)	*/
	     const char *prname,			/* The PRNAME (optional).		*/
	     const int4 *openmode)			/* The open mode			*/
{
	int4 mode;

	wisp_fileattr2mode(attrstr, &mode);

	WFOPEN3(&mode, vol, lib, file, path, appl, prname, openmode);

	wisp_mode2fileattr(mode, attrstr);
}

/********************************************************************************************************************************/
void WFOPEN(						/* WISP 2.0B and earlier		*/
	int4 *mode,					/* the mode of opening			*/
	char *vol,					/* the WANG volume name	(6 chars)	*/
	char *lib,					/* The WANG library name (8 chars)	*/
	char *file,					/* The file name	(8 chars)	*/
	char *name,					/* The resultant name			*/
	const char *prname)				/* The PRNAME (optional).		*/
{
	WFOPEN2(mode,vol,lib,file,name,wisp_get_runname(),prname);
}

/********************************************************************************************************************************/
/*
	WFOPEN3 	This version adds a new parameter "openmode" which tells the mode the file is being opened in.
			It uses this openmode to set the bits in mode instead of doing this is the COBOL code, and then
			calls WFOPEN2();
*/
void WFOPEN3(						/* WISP 3.0 and later			*/
	     int4 *mode,				/* the mode of opening			*/
	     char *vol,					/* the WANG volume name	(6 chars)	*/
	     char *lib,					/* The WANG library name (8 chars)	*/
	     char *file,				/* The file name	(8 chars)	*/
	     char *name,				/* The resultant name			*/
	     const char *appl,				/* The COBOL program id (8 chars)	*/
	     const char *prname,			/* The PRNAME 				*/
	     const int4 *openmode)			/* The open mode			*/
{
	uint4	set,clear;
	int4	the_openmode;

	the_openmode = *openmode;
	set = 0;
	clear = 0;

	/*
		OPENMODE		IS_OUTPUT	IS_IO		IS_NOTSHARE	IS_EXTEND	IS_SORT
		========
		INPUT			0		0		1		0		0
		SHARED			0		1		0		0		0
		OUTPUT			1		0		1		0		0
		EXTEND			1		0		1		1		0
		SPECIAL-INPUT		0		0		0		0		0
		I-O			0		1		1		0		0
		SORT			1		0		1		0		1
	*/

	if (the_openmode == WFOPEN_SHARED && 				/* If OPEN SHARED for a  SEQ file change to EXTEND.	*/
	    ((*mode & IS_SEQDYN) || (*mode & IS_SEQSEQ)))
		the_openmode = WFOPEN_EXTEND;

	switch(the_openmode)
	{
	case WFOPEN_INPUT:
		set   = 0;
		clear = IS_EXTEND | IS_OUTPUT | IS_IO | IS_SORT;
		break;
	case WFOPEN_SHARED:
		set   = IS_IO;
		clear = IS_EXTEND | IS_OUTPUT | IS_SORT;
		break;
	case WFOPEN_OUTPUT:
		set   = IS_OUTPUT;
		clear = IS_EXTEND | IS_IO | IS_SORT;
		break;
	case WFOPEN_EXTEND:
		set   = IS_EXTEND | IS_OUTPUT;
		clear = IS_IO | IS_SORT;
		break;
	case WFOPEN_SPECIAL_INPUT:
		set   = 0;
		clear = IS_EXTEND | IS_OUTPUT | IS_IO | IS_SORT;
		break;
	case WFOPEN_I_O:
		set   = IS_IO;
		clear = IS_EXTEND | IS_OUTPUT | IS_SORT;
		break;
	case WFOPEN_SORT:
		set   = IS_SORT | IS_OUTPUT;
		clear = IS_EXTEND | IS_IO;
		break;
	}

	*mode = *mode | set;
	*mode = *mode & ~clear;

	WFOPEN2(mode,vol,lib,file,name,appl,prname);
}

/********************************************************************************************************************************/
void WFOPEN2(						/* WISP 2.0C and later			*/
	int4 *mode,					/* the mode of opening			*/
	char *vol,					/* the WANG volume name	(6 chars)	*/
	char *lib,					/* The WANG library name (8 chars)	*/
	char *file,					/* The file name	(8 chars)	*/
	char *cob_name,					/* The resultant name			*/
	const char *appl,				/* The COBOL program id (8 chars)	*/
	const char *prname)				/* The PRNAME 				*/
{
											/* These are static so they will be set */
											/* on an IS_ERROR repeat.		*/
	static	char *l_vol,*l_lib,*l_file,*l_name;					/* local vars cause the vax protects	*/
	static  const char *l_prname;
	static	int4 native_mode;
	static	char getparm_type[3];
	static	char	prtclass[1];
	static	int4	form;
	static	int4	copies;
	static	char	orig_file[9];							/* Var to save original passed in file.	*/

	static	char	remote_filepath[COB_FILEPATH_LEN];				/* The remote filepath via RVMAP	*/
	static	int	remote_flag = 0;						/* Flag if this is a remote file	*/
	static  char	saveExt[WISP_FILE_EXT_SIZE];

	char temp[132];
	int access_status;
	char pf_rcvr[1];								/* Stores the PF-KEY selection.		*/
	char *msg1,*msg2;
	char intv_type;									/* Intervention type E or R (rename)	*/
	char attrstr[WISP_FILE_ATTR_SIZE];

	wisp_mode2fileattr(*mode, attrstr);

	WL_wtrace("WFOPEN","ENTRY","File=[%8.8s] Lib=[%8.8s] Vol=[%6.6s] Attr=[%10.10s] App=[%8.8s] Prname=[%8.8s]",
	       file, lib, vol, attrstr, appl, prname);

	WL_setprogid(appl);								/* Set the global var program id.	*/

	/*---------------------*/
	if( *mode & IS_ERROR )								/* This is the second time thru after	*/
	{										/* an error on the COBOL OPEN stmt.	*/
											/* Goto openerror, all of the file/lib	*/
											/* etc pointers have already been set 	*/
											/* and are static.			*/
		goto openerror;
	}
	/*---------------------*/

	native_mode = 0L;								/* Use WANG style file spec		*/

	dispchars(vol,SIZEOF_VOL);							/* Change all non-display chars to sp	*/
	dispchars(lib,SIZEOF_LIB);
	dispchars(file,SIZEOF_FILE);

	leftjust(vol,SIZEOF_VOL);							/* Left justify file/lib/vol.		*/
	leftjust(lib,SIZEOF_LIB);
	leftjust(file,SIZEOF_FILE);

	l_vol = vol;									/* set pointers				*/
	l_lib = lib;
	l_file = file;                                                                                                            
	l_name = cob_name;
	memset(l_name, ' ', COB_FILEPATH_LEN);						/* Clear out the 80 character filename.	*/

	l_prname = prname;								/* Use the passed PRNAME		*/

	memcpy(orig_file,l_prname,SIZEOF_FILE);						/* Save original file name passed in.	*/

	WL_get_defs(DEFAULTS_PC,prtclass);
	WL_get_defs(DEFAULTS_FN,&form);
	copies=1;

	/* 
	**	Perform the Initial "Hidden" getparm (Once only)
	**	The IS_GETPARM logic to ensure the "ID" getparm occurs only once
	**	was removed in version 3.1 (don't know why).  The functionality
	**	was tested on the Wang and is now reinstated.
	*/
	if ( !(*mode & IS_GETPARM) )							/* If not yet called.			*/
	{
		*mode |= IS_GETPARM;							/* Set the IS_GETPARM flag.		*/

		strcpy(getparm_type,"ID");						/* "ID" is for hidden getparms		*/
		msg1="OVERRIDE FILE SPECIFICATIONS.";
		msg2="PLEASE RESPECIFY FILENAME.";
                                       
		WL_file_getparm3(l_file,l_lib,l_vol,l_prname,"WFOPEN",
			      &native_mode,getparm_type,l_name,msg1,msg2,pf_rcvr,'E',orig_file,
			      prtclass,&form,&copies,
			      (*mode & IS_OUTPUT),
			      (*mode & IS_PRINTFILE),
			      (*mode & IS_IO),
			      0 /* IS_SHARED */
			      );

		if (pf_rcvr[0] == PFKEY_16_PRESSED)
		{
			wisp_set_LINKCOMPCODE(16);
			wexit(16L);							/* Exit The program.			*/
		}
	}

	strcpy(getparm_type,"I ");							/* Start off as an initial getparm.	*/

                                       
translate_name:
	/*
	**	Translate the Wang style name into a native file path
	*/

	if (!native_mode)
	{

		if (l_file[0] == ' ')							/* If no filename then			*/
		{
			if ( (*mode & IS_PRINTFILE) || (*mode & IS_SORT) )		/* If a printfile or sortfile		*/
			{

	
				l_file[0] = '#';					/* generate a file name.		*/
				l_file[1] = '#';
				memcpy(&l_file[2], wisp_get_runname(), 4);
			}
			else
			{
				access_status = ACC_NOFILE;
				goto respecify;
			}
		}

		*mode &= ~IS_SCRATCH;							/* Clear the scratch flag.		*/
											/* In case it was set by a previous call*/
		WGETFILEXT(saveExt);							/* Save the special file extension.	*/

		WL_wfname_backfill(mode,l_vol,l_lib,l_file,l_name);			/* generate a native file name		*/

	}

	/*
	**	Generate a remote filepath based on RVMAP path prefixes.
	**	This is used with ACUServer and FileShare2.
	*/
	remote_flag = WL_remote_volume(l_name, remote_filepath);
                                                                                        
/* check_access: */
	/*
	**	Check to see if we have access to the file.
	*/

	access_status = ACC_UNKNOWN;	     						/* Initialize the status.		*/

	if ( !OPTION_CREATEVOLUMEON && !native_mode && !WL_wlgtrans(l_vol, temp) )		/* Check if there is volume translation */
	{
		access_status = ACC_BADVOL;
	}

	if (*mode & IS_DBFILE)
	{
		/*
		**	For DATABASE files bypass the normal access checking.
		*/
		access_status = ACC_ALLOWED;
	}

	/*
	**	This is a test for AcuSERVER.
	**	If a remote file syntax (leading '@') then bypass normal access checking.
	*/
	if ('@' == l_name[0])
	{
		access_status = ACC_ALLOWED;
	}

	if (ACC_UNKNOWN == access_status)
	{
		access_status = wisp_file_access(l_name, (*mode & IS_OUTPUT), (*mode & IS_INDEXED));/* See if the user can access it.	*/
	}

openerror:
	if ( *mode & IS_ERROR )								/* A COBOL error occured while trying	*/
	{										/* to oppen the file - this is the 2nd	*/
											/* time thru wfopen.			*/
		*mode &= ~IS_ERROR;							/* Clear the IS_ERROR bit		*/
		access_status = ACC_ERROPEN;						/* Set access_status			*/

		if ( 0==memcmp(wisp_get_last_filecheckstatus(), wisp_get_filelock(), 2)) 		/* Check if file locked.		*/
		{
			access_status = ACC_LOCKED;	
		}
	}


	if ( ( access_status == ACC_NODIR   ||
	       access_status == ACC_MISSING ||
	       access_status == ACC_UNKNOWN   ) &&					/* The directoy path may not be created */
	     ( *mode & IS_OUTPUT              )    )
	{
		char cstr_l_name[COB_FILEPATH_LEN + 1];

		cobx2cstr(cstr_l_name, l_name, COB_FILEPATH_LEN);

		if ( makepath( cstr_l_name) )						/* Try to make the dir path		*/
		{
			access_status = ACC_NODIR;
		}
		else
		{
			access_status = wisp_file_access(l_name, (*mode & IS_OUTPUT), (*mode & IS_INDEXED));/* Try again to access it.	*/
			if ( access_status == ACC_MISSING )
				access_status = ACC_NOFILE;
		}
	}

	if (access_status == ACC_OUTEXISTS)						/* If output and file exists then we	*/
	{										/* generate a getparm except when it is	*/
		if ( (*mode & IS_WORK)   ||						/* a workfile, or a tempfile, or a 	*/
		     (*mode & IS_TEMP)   ||						/* seqdyn file - in which case a getparm*/
		     (*mode & IS_SEQDYN) ||						/* is not generated.			*/
		     (*mode & IS_EXTEND) ||
		     (*mode & IS_PRINTFILE && *mode & IS_NORESPECIFY) ||
		     OPTION_OUTPUTVERIFYOFF    )
			access_status = ACC_ALLOWED;
	}

respecify:
	/*
	**	If access is not allowed then this is where we issue a respecify getparm.
	*/

	if (access_status != ACC_ALLOWED && access_status != ACC_UNKNOWN)           	/* Access granted ?			*/
	{                                                                       	/* Nope. 				*/
		char  msgbuf[80];

		if (*mode & IS_NORESPECIFY)
		{
			goto wfopen_return;						/* NO_RESPECIFY file ?	If so, return.	*/
		}

		acc_message( access_status, msgbuf, 
			wisp_get_last_filecheckstatus(), 
			wisp_get_last_filecheckstatus_ext() );				/* get the access message		*/

		intv_type = 'E';
		if (access_status == ACC_OUTEXISTS) intv_type = 'R';			/* add PF3 option to delete		*/

		msg1=msgbuf;
		msg2="PLEASE RESPECIFY FILENAME.";

		strcpy(getparm_type,"R ");						/* Issue an RESPECIFY getparm.		*/

		WL_file_getparm3(l_file,l_lib,l_vol,l_prname,"WFOPEN",
				&native_mode,getparm_type,l_name,msg1,msg2,pf_rcvr,intv_type,orig_file,
				prtclass,&form,&copies,
				(*mode & IS_OUTPUT),
				(*mode & IS_PRINTFILE),
				(*mode & IS_IO),
				0 /* IS_SHARED */
				);


		if (pf_rcvr[0] == PFKEY_16_PRESSED)
		{
			wisp_set_LINKCOMPCODE(16);
			wexit(16L);							/* Exit The program.			*/
		}

		if (pf_rcvr[0] != PFKEY_3_PRESSED)
		{
			if (!native_mode)						/* What name entry mode are we in ?	*/
			{								
				WSETFILEXT(saveExt);				/* Restore the special extension.	*/
			}
			goto translate_name;  						/* re-translate the name etc.		*/
		}
	}


/* acc_allowed: */
	/*
	**	We believe that access is allowed so we are going to get ready
	**	to fall thur and allow the COBOL OPEN to occur.
	*/

	if ((*mode & IS_TEMP) && 
	    (*mode & IS_INDEXED) && 
	    (*mode & IS_OUTPUT)     )
	{
		/*
		**	This only really needs to be done if we are creating a CISAM file
		**	but no safe way of telling since acucobol can use cisam.
		*/
		cobx2cstr(temp,l_name,COB_FILEPATH_LEN);
		wisp_unlink(temp);								/* The delete the lockfile		*/
	}

	if ((*mode & IS_PRINTFILE) && (*mode & IS_OUTPUT))				/* Is this a print file open for output	*/
	{       
		wisp_pstruct	*plptr;

		plptr = (wisp_pstruct *) wisp_malloc(sizeof(wisp_pstruct));		/* Make a new item			*/
		plptr->nextfile = WL_g_print_file_list;					/* Link the existing list to the end	*/
		WL_g_print_file_list = plptr;						/* New item is the head of the list	*/

		cobx2cstr(WL_g_print_file_list->name, l_name, COB_FILEPATH_LEN);
		WL_g_print_file_list->form = form;						/* Set the current form.		*/
		WL_g_print_file_list->class = prtclass[0];					/* Set the current class.		*/
		WL_g_print_file_list->numcopies = copies;					/* Default to spool one copy.		*/
	}
	else if (*mode & IS_SCRATCH)							/* Otherwise save the temp file name	*/
	{
		wisp_fstruct *new_fstruct;

		new_fstruct = (wisp_fstruct *) wisp_malloc(sizeof(wisp_fstruct));			/* get some memory to start the list	*/
		new_fstruct->nextfile = WL_g_temp_file_list;				/* Link the existing list to the end	*/
		WL_g_temp_file_list = new_fstruct;

		memcpy(WL_g_temp_file_list->vol,l_vol,SIZEOF_VOL);				/* Save the volume			*/
		memcpy(WL_g_temp_file_list->lib,l_lib,SIZEOF_LIB);				/* Save the library			*/
		memcpy(WL_g_temp_file_list->file,l_file,SIZEOF_FILE);			/* Save the file name			*/
		memcpy(WL_g_temp_file_list->name,l_name,COB_FILEPATH_LEN);			/* Save the name			*/

	}

	if (!native_mode)
	{
		/*
		**	Update the PUTPARM with updated file info.
		**	The WL_use_last_prb() is an internal "hint" to GETPARM
		**	to use the last PRB.  This will prevent GETPARM from
		**	possibly updating the wrong PUTPARM if there are 
		**	multiple with the same PRNAME.
		**
		**	GETPARM does not fully correctly handle link-levels
		**	and PUTPARM chaining so this scenerio can happen:
		**	If the original PUTPARM is not labeled then
		**	it will be deleted once used (instead of just being
		**	marked as used).  If there is another PUTPARM with the
		**	same PRNAME even at a lower link-level it will be found 
		**	by the RD GETPARM and incorrectly updated.  The call to 
		**	WL_use_last_prb() tell GETPARM to just update the last
		**	PUTPARM if it still exists instead of going looking
		**	for a matching PUTPARM.
		*/	
		WL_use_last_prb();								/* Use the last PRB found for GETPARM	*/
		strcpy(getparm_type,"RD");						/* Issue an RESPECIFY_DEFAULT getparm.	*/
		WL_file_getparm3(l_file,l_lib,l_vol,l_prname,"WFOPEN",
				&native_mode,getparm_type,l_name,"","",pf_rcvr,'E',orig_file,
				prtclass,&form,&copies,
				(*mode & IS_OUTPUT),
				(*mode & IS_PRINTFILE),
				(*mode & IS_IO),
				0 /* IS_SHARED */
				);

	}

	if (remote_flag) 
	{
		/*
		**	If this is a remote file then copy the remote_filepath
		**	into the COBOL native file name area so COBOL will
		**	use the remote name to open the file.
		*/
		memcpy(l_name, remote_filepath, COB_FILEPATH_LEN);
		WL_wtrace("WFOPEN","REMOTE","Remote filepath=[%80.80s]", l_name);
	}
	
wfopen_return:
	
	wisp_mode2fileattr(*mode, attrstr);
	WL_wtrace("WFOPEN","RETURN","Cobpath=[%80.80s] File=[%8.8s] Lib=[%8.8s] Vol=[%6.6s] Attr=[%10.10s]",
	       cob_name, file, lib, vol, attrstr);

}

static void acc_message(int access_status, 
			char* msgbuf, 
			const char filestatus[2],
			const char *filestatus_extended)
{
	char	fsbuf[40];

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
		strcpy(msgbuf,"FILE EXISTS, RENAME AND PRESS (ENTER) OR PRESS (3) TO CONTINUE.");
		break;
	case ACC_ERROPEN:
		strcpy(msgbuf,"OPEN FAILED WITH FILE STATUS ");

		if (wisp_mf_cobol() && filestatus[0] == '9')
		{
			sprintf(fsbuf,"[9/RT%03d]",(unsigned)filestatus[1]);
		}
		else
		{
			sprintf(fsbuf,"[%2.2s]", filestatus);
		}
		strcat(msgbuf,fsbuf);

		if (wisp_acu_cobol())
		{
			char acustring[40];
			int len, i;
			
			len = strlen(filestatus_extended);

			if (len > 2)
			{
				strcpy(acustring, "[");
				for (i=2; i<len; i++)
				{
					if (filestatus_extended[i] < ' ' || 
					    filestatus_extended[i] > '~')	/* If status not printable	*/
					{
						sprintf(fsbuf,"(0x%02x)",(unsigned int)(filestatus_extended[i]));
					}
					else
					{
						sprintf(fsbuf,"%c",filestatus_extended[i]);
					}
					strcat(acustring,fsbuf);
				}
				strcat(acustring, "]");

				strcat(msgbuf,acustring);
			}
			
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

/*
**	History:
**	$Log: wfopen.c,v $
**	Revision 1.47  2003/04/24 13:43:32  gsl
**	Comments on WL_use_last_prb()
**	
**	Revision 1.46  2003/03/17 17:21:17  gsl
**	Change to use  WFOPEN4
**	
**	Revision 1.45  2003/03/06 21:37:34  gsl
**	Change trace to show ATTR instead of MODE
**	
**	Revision 1.44  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.43  2003/01/31 18:48:36  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.42  2002/12/11 17:03:09  gsl
**	use wisp_unlink()
**	
**	Revision 1.41  2002/12/09 21:45:44  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.40  2002/07/29 15:46:47  gsl
**	getwfilext -> WGETFILEXT
**	setwfilext -> WSETFILEXT
**	setwispfilext -> WSETFILEXT
**	
**	Revision 1.39  2002/07/29 14:47:20  gsl
**	wfopen2 ->WFOPEN2
**	wfopen3 ->WFOPEN3
**	
**	Revision 1.38  2002/07/23 20:49:49  gsl
**	globals
**	
**	Revision 1.37  2002/07/12 19:10:19  gsl
**	Global unique WL_ changes
**	
**	Revision 1.36  2002/07/12 17:01:03  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.35  2002/07/11 20:29:17  gsl
**	Fix WL_ globals
**	
**	Revision 1.34  2002/07/10 21:05:31  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.33  2002/07/02 21:15:32  gsl
**	Rename wstrdup
**	
**	Revision 1.32  2002/07/02 04:04:23  gsl
**	call to get last filecheck status
**	
**	Revision 1.31  2002/07/01 04:02:43  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.30  2002/06/28 04:02:58  gsl
**	Work on native version of wfopen and wfname
**	
**	Revision 1.29  2002/06/27 04:12:41  gsl
**	Clean up status/mode bits
**	
**	Revision 1.28  2002/06/26 04:25:03  gsl
**	Cleanup mode/status bit fields
**	
**	Revision 1.27  2002/06/25 17:46:05  gsl
**	Remove WISPFILEXT as a global, now must go thru set/get routines
**	
**	Revision 1.26  2002/06/21 20:49:30  gsl
**	Rework the IS_xxx bit flags and the WFOPEN_mode flags
**	
**	Revision 1.25  2002/06/21 03:51:41  gsl
**	Remove lpi_cobol
**	
**	Revision 1.24  2002/06/21 03:10:45  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.23  2001/11/02 15:09:20  gsl
**	Tweak PF3 message
**	Remove VMS code
**	
**	Revision 1.22  1999-01-05 10:08:11-05  gsl
**	Fix sig11 bug.
**	The msg1 and msg2 fields were uninitialized the second time in because
**	of the earlier change that prevents the hidden getparm the second time in.
**
**	Revision 1.21  1998-11-17 15:46:05-05  gsl
**	Reinstate the IS_GETPARM logic to ensure that the initial hidden
**	getparm is only done the first time a file is opened.
**
**	Revision 1.20  1998-10-22 14:09:26-04  gsl
**	Simplify the g_temp_file_list and WL_g_print_file_list processing.
**
**	Revision 1.19  1998-08-03 17:20:47-04  jlima
**	Support Logical Volume Translation with long file names containing eventual embedded blanks.
**
**	Revision 1.18  1998-07-10 10:21:53-04  gsl
**	Fixed RD call to file_getparm() to use 'E' for intv_type.
**
**	Revision 1.17  1998-05-27 10:17:49-04  gsl
**	Add better WL_wtrace logic
**
**	Revision 1.16  1997-04-29 13:39:46-04  gsl
**	Moved acc_message() from wfaccess.c
**	Changed to understand the long file status codes from acucobol
**
**	Revision 1.15  1996-07-17 17:54:40-04  gsl
**	change to use wmalloc()
**
**	Revision 1.14  1996-07-10 17:12:01-07  gsl
**	fix prototyypes and includes for NT
**
**	Revision 1.13  1996-06-28 09:16:37-07  gsl
**	change delete() to unlink()
**
**	Revision 1.12  1996-01-04 02:39:32-08  gsl
**	Add include of rvmap.h
**
 * Revision 1.11  1996/01/04  10:35:45  gsl
 * Added the Remote Volume support so as to support AcuServer and FileShare2.
 * After filename translation remote_volume() is called to generate a
 * remote filepath. If this is a remote file then the remote filepath
 * is returned to COBOL for the OPEN.
 *
 * Revision 1.10  1996/01/03  14:03:11  gsl
 * Replace hard-coded literals with defines
 *
 * Revision 1.9  1996/01/03  11:57:03  gsl
 * Added std headers and comments
 *
**
**
*/
