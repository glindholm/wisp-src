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

/********************************************************************************************************************************
*																*
*	SETFILE.C -- Subroutine to emulate modification of WANG User File Blocks.						*
*																*
********************************************************************************************************************************/
	/*********************************************************************
	*                                                                    *
	*        CALL "SETFILE" USING FILE-NAME                              *
	*                             KEYWORD                                *
	*                             KEYWORD-VALUE                          *
	*                                                                    *
	*     ITEM            TYPE           COMMENTS                        *
	*     ----            ----           --------                        *
	* 1.  FILE-NAME       FROM FD        FILE DEFINED IN DATA DIVISION   *
	*                                                                    *
	* 2.  KEYWORD         ALPHA (2)      KEYWORD SPECIFYING THE FIELD    *
	*                                    TO BE UPDATED                   *
	* 3.  KEYWORD-VALUE   VARIABLE       VALUE THE FIELD IS UPDATED WITH *
	*                                    SPACES IN VALUE CAUSE NO UPDATE *
	*                                    TO OCCUR                        *
	*                                                                    *
	*                                                                    *
	*      KEYWORD        VALUE TYPE      VALUE SPECIFIED                *
	*      -------        ----------      ---------------                *
	*        F            ALPHA (8)       NAME OF FILE                   *
	*        FN           NUMERIC(3)      FORM NUMBER                    *
	*        L            ALPHA (8)       NAME OF LIBRARY                *
	*        NC           NUMERIC(5)      NUMBER OF COPIES               *
	*        PC           ALPHA(1)        PRINT CLASS                    *
	*        V            ALPHA (6)       NAME OF VOLUME                 *
	*                                                                    *
	*********************************************************************/

#include <stdlib.h>
#include <string.h>

#include <stdarg.h>
#include "idsistd.h"
#include "wfiles.h"
#include "wisplib.h"
#include "werrlog.h"
#include "wfname.h"
#include "idsisubs.h"
#include "wglobals.h"
#include "wdefines.h"
#include "wcommon.h"


#define	KEYLIMIT    16									/* Maximum number of key values.	*/
#define FILENAMELEN  8									/* Length of WANG file name.		*/
#define LIBRARYLEN   8									/* Length of WANG library name.		*/
#define VOLUMELEN    6									/* Length of WANG volume name.		*/
#define FORMNUMLEN   3									/* Length of passed form number.	*/
#define NUMCOPLEN    5									/* Length of passed number of copies.	*/

static int setfile_keyword(char* vol, char* lib, char* file, char* keyword, char* keyvalue);

void SETFILE2(char* vol, char* lib, char* file, ...)
{
	va_list the_args;								/* Define argument stack pointer.	*/
	int arg_count;									/* Number of arguments in stack.	*/
	char *keyvalue;									/* Keyvalue arguments passed from COBOL.*/
	char *keyword;									/* Keyword arguments passed from COBOL.	*/

	va_start(the_args, file);
	arg_count = WL_va_count();							/* Count number of arguments.		*/

	if (arg_count < 5)
	{
		WL_werrlog_error(WERRCODE(104),"SETFILE2", "ARGS", 
			"Insufficient parameters supplied in call to SETFILE2, call ignored.");
		return;
	}

	WL_wtrace("SETFILE2","ENTRY", "Vol=[%6.6s] Lib=[%8.8s] File=[%8.8s] args=%d",
		vol, lib, file, arg_count);

	arg_count -= 3;
	while (arg_count > 1)
	{
		keyword = va_arg(the_args, char*);
		keyvalue = va_arg(the_args, char*);
		arg_count -= 2;

		if (0 != setfile_keyword(vol, lib, file, keyword, keyvalue))
		{
			return;
		}
	}
}

void SETFILE(char* vol, char* lib, char* file, char* status, ...)
{
	va_list the_args;								/* Define argument stack pointer.	*/
	int arg_count;									/* Number of arguments in stack.	*/
	char *keyvalue;									/* Keyvalue arguments passed from COBOL.*/
	char *keyword;									/* Keyword arguments passed from COBOL.	*/

	va_start(the_args, status);
	arg_count = WL_va_count();							/* Count number of arguments.		*/

	if (arg_count < 6)
	{
		WL_werrlog_error(WERRCODE(104),"SETFILE", "ARGS", 
			"Insufficient parameters supplied in call to SETFILE, call ignored.");
		return;
	}

	WL_wtrace("SETFILE","ENTRY", "Vol=[%6.6s] Lib=[%8.8s] File=[%8.8s] args=%d",
		vol, lib, file, arg_count);

	arg_count -= 4;
	while (arg_count > 1)
	{
		keyword = va_arg(the_args, char*);
		keyvalue = va_arg(the_args, char*);
		arg_count -= 2;

		if (0 != setfile_keyword(vol, lib, file, keyword, keyvalue))
		{
			return;
		}
	}
}

static int setfile_keyword(char* vol, char* lib, char* file, char* keyword, char* keyvalue)
{
	int4 mode=0;									/* wfname mode				*/
	char fname[COB_FILEPATH_LEN];							/* wfname constructed print file name.	*/
	char *eoname;									/* needed due to trimming		*/
	char string[8];									/* Used for atoi conversion.		*/
	wisp_pstruct *lptr;								/* A local pointer into the structure.	*/
	char buff[20];
	int keyvalue_len = 0;

	if (mode != IS_PRINTFILE)					/* Do only if don't already know it's printfile	*/
	{
		if (strncmp(keyword,"FN",2) == 0 ||				/* If request is to set form number or		*/
		    strncmp(keyword,"NC",2) == 0 ||				/* set number of copies or			*/
		    strncmp(keyword,"PC",2) == 0)				/* set print class, then find the pstruct.	*/
		{
		    if (!WL_g_print_file_list)					/* If no print files are presently open.	*/
		    {
			WL_werrlog_error(WERRCODE(104),"SETFILE", "NOFILES", 
				"SETFILE called with no open print file, call ignored.");
		    }
		    mode = IS_PRINTFILE;					/* mode for wfname representing print file.	*/
										/* Use wfname instead of wfopen because wfopen	*/
										/* generates new pstruct elements.		*/
		    eoname = WL_wfname(&mode,vol,lib,file,fname);		/* Construct the print file name.		*/
		    *eoname = '\0';						/* fname is now a cstr				*/

		    lptr = WL_g_print_file_list;				/* Start at head of print file list.		*/
		    do								/* Find the correct element of the list.	*/
		    {
			if (!strcmp(lptr->name,fname)) break;			/* If the name's right we're done.		*/
			lptr = lptr->nextfile;					/* Otherwise, look at the next element.		*/
		    } while (lptr);

		    if (!lptr)							/* If we never found the right name.		*/
		    {
			WL_werrlog_error(WERRCODE(104),"SETFILE", "BADFILES", 
				"SETFILE called to set print attributes for non print file, call ignored.");
			return 1;
		    }
		}								/* Here, lptr points to the correct pstruct.	*/
	}

	if (strncmp(keyword,"F ",2) == 0)				/* Check for SET-FILE-NAME keyword.		*/
	{
		keyvalue_len = FILENAMELEN;

		if (keyvalue[0] != ' ')
 			strncpy(file,keyvalue,FILENAMELEN);		/* File name is in keyvalue, copy to UFB.	*/
		else
 			strncpy(keyvalue,file,FILENAMELEN);		/* File name is in UFB, copy to keyvalue.	*/
	}
	else if (strncmp(keyword,"FN",2) == 0)				/* Check for SET-FORM-NUMBER keyword.		*/
	{
		keyvalue_len = FORMNUMLEN;

		strncpy(string,keyvalue,FORMNUMLEN);			/* Copy decimal string to temporary location.	*/
		string[FORMNUMLEN]='\0';				/* NULL terminate.				*/
		lptr->form = atoi(string);				/* Set form number in pstruct.			*/
	}
	else if (strncmp(keyword,"L ",2) == 0)				/* Check for SET-LIBRARY-NAME keyword.		*/
	{
		keyvalue_len = LIBRARYLEN;
		if (keyvalue[0] != ' ')
 			strncpy(lib,keyvalue,LIBRARYLEN);		/* Library name is in keyvalue, copy to UFB.	*/
		else
 			strncpy(keyvalue,lib,LIBRARYLEN);		/* Library name is in UFB, copy to keyvalue.	*/
	}
	else if (strncmp(keyword,"NC",2) == 0)				/* Check for SET-NUMBER-COPIES keyword.		*/
	{
		keyvalue_len = NUMCOPLEN;
		strncpy(string,keyvalue,NUMCOPLEN);			/* Copy decimal string to temporary location.	*/
		string[NUMCOPLEN]='\0';					/* NULL terminate.				*/
		lptr->numcopies = atoi(string);				/* Set number of copies in pstruct.		*/
	}
	else if (strncmp(keyword,"PC",2) == 0)				/* Check for SET-PRINT-CLASS keyword.		*/
	{
		keyvalue_len = 1;
		lptr->class = *keyvalue;				/* Set print class in pstruct.			*/
	}
	else if (strncmp(keyword,"V ",2) == 0)				/* Check for SET-VOLUME-NAME keyword.		*/
	{
		keyvalue_len = VOLUMELEN;
		if (keyvalue[0] != ' ')
 			strncpy(vol,keyvalue,VOLUMELEN);		/* Volume name is in keyvalue, copy to UFB.	*/
		else
 			strncpy(keyvalue,vol,VOLUMELEN);		/* Volume name is in UFB, copy to keyvalue.	*/
	}
	else
	{
		WL_werrlog_error(WERRCODE(104),"SETFILE", "INVALID", 
			"SETFILE called with invalid Keyword=[%2.2s]", keyword);
		return 1;
	}

	memcpy(buff, keyvalue, keyvalue_len);
	buff[keyvalue_len] = '\0';
	WL_wtrace("SETFILE","KEYWORD","Keyword=[%2.2s] Value=[%s]",
			keyword, buff);

	return 0;
}
/*
**	History:
**	$Log: setfile.c,v $
**	Revision 1.25  2003/03/17 17:21:51  gsl
**	Change to use  SETFILE2
**	
**	Revision 1.24  2003/03/06 22:06:28  gsl
**	fix warnings
**	
**	Revision 1.23  2003/03/06 21:31:57  gsl
**	Add SETFILE2() for changes from MODE to ATTR
**	
**	Revision 1.22  2003/01/31 18:54:38  gsl
**	Fix copyright header
**	
**	Revision 1.21  2003/01/20 20:13:53  gsl
**	Changed to use stdarg.h
**	
**	Revision 1.20  2002/12/09 19:15:33  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.19  2002/09/30 21:02:01  gsl
**	update
**	
**	Revision 1.18  2002/07/16 16:24:52  gsl
**	Globals
**	
**	Revision 1.17  2002/07/12 19:10:16  gsl
**	Global unique WL_ changes
**	
**	Revision 1.16  2002/07/11 20:29:13  gsl
**	Fix WL_ globals
**	
**	Revision 1.15  2002/07/01 04:02:40  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.14  2002/06/21 03:10:40  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.13  1998/10/22 18:11:57  gsl
**	change to use g_print_file_list
**	fix werrlog processing
**	
**	Revision 1.12  1998-08-03 17:11:39-04  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks.
**
**	Revision 1.11  1996-08-19 18:32:54-04  gsl
**	drcs update
**
**
**
*/
