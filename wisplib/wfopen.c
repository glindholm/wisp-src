static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wfopen.c
**
**	Project:	WISP
**
**	RCS:		$Source:$
**
**	Purpose:	Wang VS COBOL Open logic
**
**	Routines:	
**	wfopen()	Obsolete
**	wfopen2()
**	wfopen3()
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

#include "werrlog.h"
#define		ROUTINE		78000
/*
78001   %%WFOPEN-I-ENTRY Entry into wfopen file(%8.8s) lib(%8.8s) vol(%6.6s) appl(%8.8s)
*/


/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/
extern void lastacufilestat(char *buff);

extern void wfopen2();
extern void wfopen3(int4 *mode, char *vol, char *lib, char *file, char *name, char *appl, char *prname, int4 *openmode);

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/
static void acc_message(int access_status, char* msgbuf);


/********************************************************************************************************************************/
void wfopen(mode,vol,lib,file,name,prname)							/* WISP 2.0B and earlier		*/
int4 *mode;										/* the mode of opening			*/
char *vol;										/* the WANG volume name	(6 chars)	*/
char *lib;										/* The WANG library name (8 chars)	*/
char *file;										/* The file name	(8 chars)	*/
char *name;										/* The resultant name			*/
char *prname;										/* The PRNAME (optional).		*/
{
	wfopen2(mode,vol,lib,file,name,WISPRUNNAME,prname);
}

/********************************************************************************************************************************/
/*
	wfopen3 	This version adds a new parameter "openmode" which tells the mode the file is being opened in.
			It uses this openmode to set the bits in mode instead of doing this is the COBOL code, and then
			calls wfopen2();
*/
void wfopen3(										/* WISP 3.0 and later			*/
	     int4 *mode,								/* the mode of opening			*/
	     char *vol,									/* the WANG volume name	(6 chars)	*/
	     char *lib,									/* The WANG library name (8 chars)	*/
	     char *file,								/* The file name	(8 chars)	*/
	     char *name,								/* The resultant name			*/
	     char *appl,								/* The COBOL program id (8 chars)	*/
	     char *prname,								/* The PRNAME (optional).		*/
	     int4 *openmode)								/* The open mode			*/
{
	uint4	set,clear;
	int4	the_openmode;

	the_openmode = *openmode;
	set = 0;
	clear = 0;

	/*
		OPENMODE		IS_OUTPUT	IS_IO		IS_NOWRITE	IS_EXTEND	IS_SORT
		========
		INPUT			0		0		1		0		0
		SHARED			0		1		0		0		0
		OUTPUT			1		0		1		0		0
		EXTEND			1		0		1		1		0
		SPECIAL-INPUT		0		0		0		0		0
		I-O			0		1		1		0		0
		SORT			1		0		1		0		1
	*/

	if (the_openmode == OPEN_SHARED && 				/* If OPEN SHARED for a  SEQ file change to EXTEND.	*/
	    ((*mode & IS_SEQDYN) || (*mode & IS_SEQSEQ)))
		the_openmode = OPEN_EXTEND;

	switch(the_openmode)
	{
	case OPEN_INPUT:
		set   = IS_NOWRITE;
		clear = IS_EXTEND | IS_OUTPUT | IS_IO | IS_SORT;
		break;
	case OPEN_SHARED:
		set   = IS_IO;
		clear = IS_EXTEND | IS_OUTPUT | IS_NOWRITE | IS_SORT;
		break;
	case OPEN_OUTPUT:
		set   = IS_OUTPUT | IS_NOWRITE;
		clear = IS_EXTEND | IS_IO | IS_SORT;
		break;
	case OPEN_EXTEND:
		set   = IS_EXTEND | IS_OUTPUT | IS_NOWRITE;
		clear = IS_IO | IS_SORT;
		break;
	case OPEN_SPECIAL_INPUT:
		set   = 0;
		clear = IS_EXTEND | IS_OUTPUT | IS_NOWRITE | IS_IO | IS_SORT;
		break;
	case OPEN_I_O:
		set   = IS_NOWRITE | IS_IO;
		clear = IS_EXTEND | IS_OUTPUT | IS_SORT;
		break;
	case OPEN_SORT:
		set   = IS_SORT | IS_OUTPUT | IS_NOWRITE;
		clear = IS_EXTEND | IS_IO;
		break;
	}

	*mode = *mode | set;
	*mode = *mode & ~clear;

	wfopen2(mode,vol,lib,file,name,appl,prname);
}

/********************************************************************************************************************************/
void wfopen2(mode,vol,lib,file,cob_name,appl,prname)					/* WISP 2.0C and later			*/
int4 *mode;										/* the mode of opening			*/
char *vol;										/* the WANG volume name	(6 chars)	*/
char *lib;										/* The WANG library name (8 chars)	*/
char *file;										/* The file name	(8 chars)	*/
char *cob_name;										/* The resultant name			*/
char *appl;										/* The COBOL program id (8 chars)	*/
char *prname;										/* The PRNAME (optional).		*/
{
											/* These are static so they will be set */
											/* on an IS_ERROR repeat.		*/
	static	char *l_vol,*l_lib,*l_file,*l_name,*l_prname;				/* local vars cause the vax protects	*/
	static	int4 native_mode;							/* Set to 1 if in VAX/VMS native mode.	*/
	static	char getparm_type[3];
	static	char	prtclass[1];
	static	int4	form;
	static	int4	copies;
	static	char	orig_file[9];							/* Var to save original passed in file.	*/

	static	char	remote_filepath[COB_FILEPATH_LEN];				/* The remote filepath via RVMAP	*/
	static	int	remote_flag = 0;						/* Flag if this is a remote file	*/

	char temp[132];
	int i,access_status;
	char pf_rcvr[1];								/* Stores the PF-KEY selection.		*/
	char *msg1,*msg2;
	char intv_type;									/* Intervention type E or R (rename)	*/

	wtrace("WFOPEN","ENTRY","File=[%8.8s] Lib=[%8.8s] Vol=[%6.6s] Mode=[0x%08X] App=[%8.8s] Prname=[%8.8s]",
	       file, lib, vol, *mode, appl, prname);

	setprogid(appl);								/* Set the global var program id.	*/

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

	if (*mode & IS_PRNAME)								/* If PRNAME is passed then		*/
		l_prname = prname;							/* Use the passed PRNAME		*/
	else
		l_prname = "        ";							/* Else use blank			*/

	memcpy(orig_file,l_prname,SIZEOF_FILE);						/* Save original file name passed in.	*/

	get_defs(DEFAULTS_PC,prtclass);
	get_defs(DEFAULTS_FN,&form);
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
                                       
		file_getparm2(*mode,l_file,l_lib,l_vol,l_prname,"WFOPEN",
			      &native_mode,getparm_type,l_name,msg1,msg2,pf_rcvr,'E',orig_file,prtclass,&form,&copies);

		if (pf_rcvr[0] == PFKEY_16_PRESSED)
		{
			LINKCOMPCODE = 16;
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
				l_file[2] = WISPRUNNAME[0];
				l_file[3] = WISPRUNNAME[1];
				l_file[4] = WISPRUNNAME[2];
				l_file[5] = WISPRUNNAME[3];
			}
			else
			{
				access_status = ACC_NOFILE;
				goto respecify;
			}
		}

		*mode &= ~IS_SCRATCH;							/* Clear the scratch flag.		*/
											/* In case it was set by a previous call*/
		*mode |= IS_BACKFILL;							/* Backfill the file/lib/vol.		*/
		SAVE_WISPFILEXT;							/* Save the special file extension.	*/

		wfname(mode,l_vol,l_lib,l_file,l_name);					/* generate a native file name		*/

	}

	/*
	**	Generate a remote filepath based on RVMAP path prefixes.
	**	This is used with ACUServer and FileShare2.
	*/
	remote_flag = remote_volume(l_name, remote_filepath);
                                                                                        
/* check_access: */
	/*
	**	Check to see if we have access to the file.
	*/

	access_status = ACC_UNKNOWN;	     						/* Initialize the status.		*/

	if ( !opt_createvolumeon && !native_mode && !wlgtrans(l_vol, temp) )		/* Check if there is volume translation */
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
		access_status = wfaccess(l_name, mode);					/* See if the user can access it.	*/
	}

openerror:
	if ( *mode & IS_ERROR )								/* A COBOL error occured while trying	*/
	{										/* to oppen the file - this is the 2nd	*/
											/* time thru wfopen.			*/
		*mode &= ~IS_ERROR;							/* Clear the IS_ERROR bit		*/
		access_status = ACC_ERROPEN;						/* Set access_status			*/

		if ( wfilestat[0] == filelock[0] &&
		     wfilestat[1] == filelock[1]    ) access_status = ACC_LOCKED;	/* Check if file locked.		*/
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
			access_status = wfaccess(l_name, mode);				/* Try again to access it.		*/
			if ( access_status == ACC_MISSING )
				access_status = ACC_NOFILE;
		}
	}

	if ( lpi_cobol && (*mode & IS_SEQDYN) && (*mode & IS_OUTPUT) && (access_status == ACC_ALLOWED) )
	{										/* If SEQ/DYN and OUTPUT and doesn't	*/
											/* exist (ACC_ALLOWED means it doesn't  */
											/* exist) then create the file (empty).	*/
		int	fdesc;
		char	xname[COB_FILEPATH_LEN + 1];

		cobx2cstr(xname, l_name, COB_FILEPATH_LEN); 

		fdesc = creat( xname, 0666 );
		if ( fdesc != -1 )
		{
			close( fdesc );
		}
		else
		{
			access_status = ACC_UNKNOWN;
		}
	}

	if (access_status == ACC_OUTEXISTS)						/* If output and file exists then we	*/
	{										/* generate a getparm except when it is	*/
		if ( (*mode & IS_WORK)   ||						/* a workfile, or a tempfile, or a 	*/
		     (*mode & IS_TEMP)   ||						/* seqdyn file - in which case a getparm*/
		     (*mode & IS_SEQDYN) ||						/* is not generated.			*/
		     (*mode & IS_EXTEND) ||
		     (*mode & IS_PRINTFILE && *mode & IS_NORESPECIFY) ||
		     opt_outputverifyoff    )
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

		acc_message( access_status, msgbuf );					/* get the access message		*/

		intv_type = 'E';
		if (access_status == ACC_OUTEXISTS) intv_type = 'R';			/* add PF3 option to delete		*/

		msg1=msgbuf;
		msg2="PLEASE RESPECIFY FILENAME.";

		strcpy(getparm_type,"R ");						/* Issue an RESPECIFY getparm.		*/

		file_getparm2(*mode,l_file,l_lib,l_vol,l_prname,"WFOPEN",
				&native_mode,getparm_type,l_name,msg1,msg2,pf_rcvr,intv_type,orig_file,prtclass,&form,&copies);


		if (pf_rcvr[0] == PFKEY_16_PRESSED)
		{
			LINKCOMPCODE = 16;
			wexit(16L);							/* Exit The program.			*/
		}

		if (pf_rcvr[0] != PFKEY_3_PRESSED)
		{
			if (!native_mode)						/* What name entry mode are we in ?	*/
			{								
				RESTORE_WISPFILEXT;					/* Restore the special extension.	*/
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
		unlink(temp);								/* The delete the lockfile		*/
	}

	if ((*mode & IS_PRINTFILE) && (*mode & IS_OUTPUT))				/* Is this a print file open for output	*/
	{       
		pstruct	*plptr;

		plptr = (pstruct *) wmalloc(sizeof(pstruct));				/* Make a new item			*/
		plptr->nextfile = g_print_file_list;					/* Link the existing list to the end	*/
		g_print_file_list = plptr;						/* New item is the head of the list	*/

		cobx2cstr(g_print_file_list->name, l_name, COB_FILEPATH_LEN);
		g_print_file_list->form = form;						/* Set the current form.		*/
		g_print_file_list->class = prtclass[0];					/* Set the current class.		*/
		g_print_file_list->numcopies = copies;					/* Default to spool one copy.		*/
	}
	else if (*mode & IS_SCRATCH)							/* Otherwise save the temp file name	*/
	{
		fstruct *new_fstruct;

		new_fstruct = (fstruct *) wmalloc(sizeof(fstruct));			/* get some memory to start the list	*/
		new_fstruct->nextfile = g_temp_file_list;				/* Link the existing list to the end	*/
		g_temp_file_list = new_fstruct;

		memcpy(g_temp_file_list->vol,l_vol,SIZEOF_VOL);				/* Save the volume			*/
		memcpy(g_temp_file_list->lib,l_lib,SIZEOF_LIB);				/* Save the library			*/
		memcpy(g_temp_file_list->file,l_file,SIZEOF_FILE);			/* Save the file name			*/
		memcpy(g_temp_file_list->name,l_name,COB_FILEPATH_LEN);			/* Save the name			*/

		if ((*mode & IS_OUTPUT) && !(*mode & IS_SORT) && !(*mode & IS_EXTEND))	/* if the file was open for output only	*/
		{									/* but was not a SORT or EXTEND file.	*/

			if (vax_cobol)
			{
				i = 0;							/* On VMS we must delete it to keep from*/
				do							/* getting too many new versions.	*/
				{
					temp[i] = l_name[i];
					i++;
				} while ((l_name[i] != ' ') && (i < COB_FILEPATH_LEN));
				temp[i] = '\0';
				strcat(temp,";");					/* add ;				*/
				unlink(temp);						/* try to delete it			*/
			}
		}
	}

	if (!native_mode)
	{
		use_last_prb();								/* Use the last PRB found for GETPARM	*/
		strcpy(getparm_type,"RD");						/* Issue an RESPECIFY_DEFAULT getparm.	*/
		file_getparm2(*mode,l_file,l_lib,l_vol,l_prname,"WFOPEN",
				&native_mode,getparm_type,l_name,"","",pf_rcvr,'E',orig_file,prtclass,&form,&copies);
	}

	if (remote_flag) 
	{
		/*
		**	If this is a remote file then copy the remote_filepath
		**	into the COBOL native file name area so COBOL will
		**	use the remote name to open the file.
		*/
		memcpy(l_name, remote_filepath, COB_FILEPATH_LEN);
		wtrace("WFOPEN","REMOTE","Remote filepath=[%80.80s]", l_name);
	}
	
wfopen_return:
	
	wtrace("WFOPEN","RETURN","Cobpath=[%80.80s] File=[%8.8s] Lib=[%8.8s] Vol=[%6.6s] Mode=[0x%08X]",
	       cob_name, file, lib, vol, *mode);

}

static void acc_message(int access_status, char* msgbuf)
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

		if ((mf_cobol || aix_cobol) && wfilestat[0] == '9')
		{
			sprintf(fsbuf,"[9/RT%03d]",(unsigned)wfilestat[1]);
		}
		else
		{
			sprintf(fsbuf,"[%2.2s]", wfilestat);
		}
		strcat(msgbuf,fsbuf);

		if (acu_cobol)
		{
			char acustring[40];
			char acufilestat[11];
			int len, i;
			
			lastacufilestat(acufilestat);						/* Get the last file status	*/
			len = strlen(acufilestat);

			if (len > 2)
			{
				strcpy(acustring, "[");
				for (i=2; i<len; i++)
				{
					if (acufilestat[i] < ' ' || acufilestat[i] > '~')	/* If status not printable	*/
					{
						sprintf(fsbuf,"(0x%02x)",(unsigned int)(acufilestat[i]));
					}
					else
					{
						sprintf(fsbuf,"%c",acufilestat[i]);
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
**	Revision 1.23  2001-11-02 10:09:20-05  gsl
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
**	Simplify the g_temp_file_list and g_print_file_list processing.
**
**	Revision 1.19  1998-08-03 17:20:47-04  jlima
**	Support Logical Volume Translation with long file names containing eventual embedded blanks.
**
**	Revision 1.18  1998-07-10 10:21:53-04  gsl
**	Fixed RD call to file_getparm() to use 'E' for intv_type.
**
**	Revision 1.17  1998-05-27 10:17:49-04  gsl
**	Add better wtrace logic
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
