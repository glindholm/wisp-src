			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/* wfopen -- this is the routine which WISP makes COBOL programs call just before they open a file				*/

#include <ctype.h>
#include <errno.h>

#ifdef VMS
#include <ssdef.h>
#endif

#ifndef unix	/* VMS or MSDOS */
#include <stdlib.h>
#endif

#ifndef VMS	/* unix or MSDOS */
#include <malloc.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif

#ifdef MSDOS
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

#include "werrlog.h"
#define		ROUTINE		78000
/*
78001   %%WFOPEN-I-ENTRY Entry into wfopen file(%8.8s) lib(%8.8s) vol(%6.6s) appl(%8.8s)
*/


char *wfname();

void wfopen2();

/********************************************************************************************************************************/
wfopen(mode,vol,lib,file,name,prname)							/* WISP 2.0B and earlier		*/
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
void wfopen3(mode,vol,lib,file,name,appl,prname,openmode)				/* WISP 3.0 and later			*/
int4 *mode;										/* the mode of opening			*/
char *vol;										/* the WANG volume name	(6 chars)	*/
char *lib;										/* The WANG library name (8 chars)	*/
char *file;										/* The file name	(8 chars)	*/
char *name;										/* The resultant name			*/
char *appl;										/* The COBOL program id (8 chars)	*/
char *prname;										/* The PRNAME (optional).		*/
int4 *openmode;										/* The open mode			*/
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
void wfopen2(mode,vol,lib,file,name,appl,prname)					/* WISP 2.0C and later			*/
int4 *mode;										/* the mode of opening			*/
char *vol;										/* the WANG volume name	(6 chars)	*/
char *lib;										/* The WANG library name (8 chars)	*/
char *file;										/* The file name	(8 chars)	*/
char *name;										/* The resultant name			*/
char *appl;										/* The COBOL program id (8 chars)	*/
char *prname;										/* The PRNAME (optional).		*/
{
											/* These are static so they will be set */
											/* on an IS_ERROR repeat.		*/
static	char *l_vol,*l_lib,*l_file,*l_name,*l_prname;					/* local vars cause the vax protects	*/
static	int4 native_mode;								/* Set to 1 if in VAX/VMS native mode.	*/
static	char getparm_type[3];
static	char	prtclass[1];
static	int4	form;
static	int4	copies;
static	char	orig_file[9];								/* Var to save original passed in file.	*/

	char temp[132];
	int i,j,access_status, pfkey;
	char pf_rcvr[1];								/* Stores the PF-KEY selection.		*/
	char text_type[1], key_word[1];
 	int4 va_cnt;
	char *msg1,*msg2;
	char intv_type;									/* Intervention type E or R (rename)	*/

	werrlog(ERRORCODE(1),file,lib,vol,appl,0,0,0,0);				/* Say we are here.			*/

	setprogid(appl);								/* Set the global var program id.	*/
	if( *mode & IS_ERROR )								/* This is the second time thru after	*/
	{										/* an error on the COBOL OPEN stmt.	*/
											/* Goto openerror, all of the file/lib	*/
											/* etc pointers have already been set 	*/
											/* and are static.			*/
		goto openerror;
	}

	native_mode = 0L;								/* Use WANG style file spec		*/

	dispchars(vol,6);								/* Change all non-display chars to sp	*/
	dispchars(lib,8);
	dispchars(file,8);

	leftjust(vol,6);								/* Left justify file/lib/vol.		*/
	leftjust(lib,8);
	leftjust(file,8);

	l_vol = vol;									/* set pointers				*/
	l_lib = lib;
	l_file = file;                                                                                                            
	l_name = name;

	if (*mode & IS_PRNAME)								/* If PRNAME is passed then		*/
		l_prname = prname;							/* Use the passed PRNAME		*/
	else
		l_prname = "        ";							/* Else use blank			*/

	memcpy(orig_file,l_prname,8);							/* Save original file name passed in.	*/

	get_defs(DEFAULTS_PC,prtclass);
	get_defs(DEFAULTS_FN,&form);
	copies=1;

	/* 
	**	Perform the Initial "Hidden" getparm 
	*/

	strcpy(getparm_type,"ID");							/* "ID" is for hidden getparms		*/

	memset(l_name, ' ', NAME_LENGTH);						/* Clear out the 80 character filename.	*/
	msg1="OVERRIDE FILE SPECIFICATIONS.";
	msg2="PLEASE RESPECIFY FILENAME.";
                                       
	file_getparm2(*mode,l_file,l_lib,l_vol,l_prname,"WFOPEN",
		&native_mode,getparm_type,l_name,msg1,msg2,pf_rcvr,'E',orig_file,prtclass,&form,&copies);

	if (pf_rcvr[0] == PFKEY_16_PRESSED)
	{
		LINKCOMPCODE = 16;
		wexit(16L);								/* Exit The program.			*/
	}

	getparm_type[0] = 'I';								/* Start off as an initial getparm.	*/
	getparm_type[1] = ' ';
	getparm_type[2] = '\0';

                                       
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

#ifdef VMS
	if (*mode & IS_PRINTFILE)							/* VAX ON-LINE printing			*/
	{
		char	printmode;
		get_defs(DEFAULTS_PM,&printmode);
		if ('O' == printmode)							/* If VAX ON-LINE printing assume OK	*/
		{
			return;
		}
	}
#endif /* VMS */

                                                                                        
check_access:
	/*
	**	Check to see if we have access to the file.
	*/

	access_status = ACC_UNKNOWN;	     						/* Initialize the status.		*/

#ifndef VMS
	if ( !opt_createvolumeon && !native_mode && !wlgtrans(l_vol, temp) )		/* Check if there is volume translation */
	{
		access_status = ACC_BADVOL;
	}
#endif

	if (*mode & IS_DBFILE)
	{
		/*
		**	For DATABASE files bypass the normal access checking.
		*/
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
		if ( makepath( l_name ) )						/* Try to make the dir path		*/
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
		char	xname[80];

		for(i=0; l_name[i] != ' '; i++) xname[i] = l_name[i];
		xname[i] = '\0';

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
		char *p_prname;
		char  msgbuf[80];

		if (*mode & IS_NORESPECIFY)
		{
			return;								/* NO_RESPECIFY file ?	If so, return.	*/
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


acc_allowed:
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
		unloadpad(temp,l_name,80);
		delete(temp);								/* The delete the lockfile		*/
	}

	if ((*mode & IS_PRINTFILE) && (*mode & IS_OUTPUT))				/* Is this a print file open for output	*/
	{       
		if (!plist)								/* haven't saved any yet		*/
		{
			plist = (pstruct *) malloc(sizeof(pstruct));			/* get some memory to start the list	*/
			plptr = plist;							/* set the current file ptr to it	*/
			plptr->nextfile = 0;						/* null ptr				*/
		}
		else           
		{
			plptr->nextfile = (struct pstruct *) malloc(sizeof(pstruct));	/* get some memory for the next item	*/
			plptr = (pstruct *) plptr->nextfile;				/* update the list			*/
			plptr->nextfile = 0;						/* set null ptr				*/
		}

		memset(plptr->name,'\0',sizeof(plptr->name));
		for(i=0; i<80 && l_name[i] != ' ' && l_name[i] != '\0'; i++)
			plptr->name[i] = l_name[i];					/* Save the name			*/
		plptr->form = form;							/* Set the current form.		*/
		plptr->class = prtclass[0];						/* Set the current class.		*/
		plptr->numcopies = copies;						/* Default to spool one copy.		*/
	}
	else if (*mode & IS_SCRATCH)							/* Otherwise save the temp file name	*/
	{
		if (!flist)								/* haven't saved any yet		*/
		{
			flist = (fstruct *) malloc(sizeof(fstruct));			/* get some memory to start the list	*/
			flptr = flist;							/* set the current file ptr to it	*/
			flptr->nextfile = 0;						/* null ptr				*/
		}
		else
		{
			flptr->nextfile = (struct fstruct *) malloc(sizeof(fstruct));	/* get some memory for the next item	*/
			flptr = (fstruct *) flptr->nextfile;				/* update the list			*/
			flptr->nextfile = 0;						/* set null ptr				*/
		}

		memcpy(flptr->vol,l_vol,6);						/* Save the volume			*/
		memcpy(flptr->lib,l_lib,8);						/* Save the library			*/
		memcpy(flptr->file,l_file,8);						/* Save the file name			*/
		memcpy(flptr->name,l_name,NAME_LENGTH);					/* Save the name			*/

		if ((*mode & IS_OUTPUT) && !(*mode & IS_SORT) && !(*mode & IS_EXTEND))	/* if the file was open for output only	*/
		{									/* but was not a SORT or EXTEND file.	*/

			if (vax_cobol)
			{
				i = 0;							/* On VMS we must delete it to keep from*/
				do							/* getting too many new versions.	*/
				{
					temp[i] = l_name[i];
					i++;
				} while ((l_name[i] != ' ') && (i < NAME_LENGTH));
				temp[i] = '\0';
				strcat(temp,";");					/* add ;				*/
				delete(temp);						/* try to delete it			*/
			}
		}
	}

	if (!native_mode)
	{
		use_last_prb();								/* Use the last PRB found for GETPARM	*/
		strcpy(getparm_type,"RD");						/* Issue an RESPECIFY_DEFAULT getparm.	*/
		file_getparm2(*mode,l_file,l_lib,l_vol,l_prname,"WFOPEN",
				&native_mode,getparm_type,l_name,msg1,msg2,pf_rcvr,intv_type,orig_file,prtclass,&form,&copies);
	}

	return;
}
