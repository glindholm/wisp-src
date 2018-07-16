static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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

#ifdef MSDOS
#include <math.h>
#endif

#include <varargs.h>
#include "idsistd.h"
#include "wfiles.h"
#include "wisplib.h"
#include "werrlog.h"
#include "wfname.h"
#include "idsisubs.h"
#include "wglobals.h"
#include "wdefines.h"


#define	KEYLIMIT    16									/* Maximum number of key values.	*/
#define FILENAMELEN  8									/* Length of WANG file name.		*/
#define LIBRARYLEN   8									/* Length of WANG library name.		*/
#define VOLUMELEN    6									/* Length of WANG volume name.		*/
#define FORMNUMLEN   3									/* Length of passed form number.	*/
#define NUMCOPLEN    5									/* Length of passed number of copies.	*/


void SETFILE(va_alist)	  								/* Function uses variable arguments.	*/

va_dcl

{
	va_list the_args;								/* Define argument stack pointer.	*/
	int arg_count;									/* Number of arguments in stack.	*/
	char *vol,*lib,*file,*status;							/* Input parameters, vol lib file stat.	*/
	char *keyvalue;									/* Keyvalue arguments passed from COBOL.*/
	char *keyword;									/* Keyword arguments passed from COBOL.	*/
	char fname[COB_FILEPATH_LEN];							/* wfname constructed print file name.	*/
	char *eoname;									/* needed due to trimming		*/
	int4 mode=0;									/* wfname mode				*/
	char string[8];									/* Used for atoi conversion.		*/
	pstruct *lptr;									/* A local pointer into the structure.	*/

	va_start(the_args);								/* Set pointer to the top of the stack.	*/
	arg_count = va_count(the_args);							/* Count number of arguments.		*/

	va_start(the_args);								/* Back to top of stack.		*/

	if (arg_count > 4)
	{
		vol  = va_arg(the_args, char*);						/* Obtain pointer to volume argument.	*/
		lib  = va_arg(the_args, char*);						/* Obtain pointer to library argument.	*/
		file = va_arg(the_args, char*);						/* Obtain pointer to filename argument.	*/
		status = va_arg(the_args, char*);					/* Obtain pointer to file status arg.	*/
		arg_count -= 4;								/* Decrement arg counter.		*/
	}
	else
	{
		werrlog(104,"%%SETFILE-E-ERROR Insufficient parameters supplied in call to SETFILE, call ignored.",
			0,0,0,0,0,0,0);
		return;
	}

	while (arg_count > 1)							/* While still at least 2 parameters.		*/
	{
		keyword = va_arg(the_args, char*);				/* Obtain pointer to keyword.			*/
		keyvalue = va_arg(the_args, char*);				/* Obtain pointer to keyvalue.			*/
		arg_count -= 2;							/* Decrement arg counter.			*/

                if (mode != 2)							/* Do only if don't already know it's printfile	*/
		if (strncmp(keyword,"FN",2) == 0 ||				/* If request is to set form number or		*/
		    strncmp(keyword,"NC",2) == 0 ||				/* set number of copies or			*/
		    strncmp(keyword,"PC",2) == 0)				/* set print class, then find the pstruct.	*/
		{
		    if (!g_print_file_list)					/* If no print files are presently open.	*/
		    {
			werrlog(104,"%%SETFILE-E-ERROR SETFILE called with no open print file, call ignored.",0,0,0,0,0,0,0);
			return;
		    }
		    mode = 2;							/* mode for wfname representing print file.	*/
										/* Use wfname instead of wfopen because wfopen	*/
										/* generates new pstruct elements.		*/
		    eoname = wfname(&mode,vol,lib,file,fname);			/* Construct the print file name.		*/
		    *eoname = '\0';						/* fname is now a cstr				*/

		    lptr = g_print_file_list;					/* Start at head of print file list.		*/
		    do								/* Find the correct element of the list.	*/
		    {
			if (!strcmp(lptr->name,fname)) break;			/* If the name's right we're done.		*/
			lptr = lptr->nextfile;					/* Otherwise, look at the next element.		*/
		    } while (lptr);

		    if (!lptr)							/* If we never found the right name.		*/
		    {
			werrlog(104,"%%SETFILE-E-ERROR SETFILE called to set print attributes for non print file, call ignored.",
				0,0,0,0,0,0,0);
			return;
		    }
		}								/* Here, lptr points to the correct pstruct.	*/

		if (strncmp(keyword,"F ",2) == 0)				/* Check for SET-FILE-NAME keyword.		*/
		{
			if (keyvalue[0] != ' ')
 				strncpy(file,keyvalue,FILENAMELEN);		/* File name is in keyvalue, copy to UFB.	*/
			else
 				strncpy(keyvalue,file,FILENAMELEN);		/* File name is in UFB, copy to keyvalue.	*/
		}
		else
		if (strncmp(keyword,"FN",2) == 0)				/* Check for SET-FORM-NUMBER keyword.		*/
		{
			strncpy(string,keyvalue,FORMNUMLEN);			/* Copy decimal string to temporary location.	*/
			string[FORMNUMLEN]='\0';				/* NULL terminate.				*/
			lptr->form = atoi(string);				/* Set form number in pstruct.			*/
		}
		else
		if (strncmp(keyword,"L ",2) == 0)				/* Check for SET-LIBRARY-NAME keyword.		*/
		{
			if (keyvalue[0] != ' ')
 				strncpy(lib,keyvalue,LIBRARYLEN);		/* Library name is in keyvalue, copy to UFB.	*/
			else
 				strncpy(keyvalue,lib,LIBRARYLEN);		/* Library name is in UFB, copy to keyvalue.	*/
		}
		else
		if (strncmp(keyword,"NC",2) == 0)				/* Check for SET-NUMBER-COPIES keyword.		*/
		{
			strncpy(string,keyvalue,NUMCOPLEN);			/* Copy decimal string to temporary location.	*/
			string[NUMCOPLEN]='\0';					/* NULL terminate.				*/
			lptr->numcopies = atoi(string);				/* Set number of copies in pstruct.		*/
		}
		else
		if (strncmp(keyword,"PC",2) == 0)				/* Check for SET-PRINT-CLASS keyword.		*/
		{
			lptr->class = *keyvalue;				/* Set print class in pstruct.			*/
		}
		else
		if (strncmp(keyword,"V ",2) == 0)				/* Check for SET-VOLUME-NAME keyword.		*/
		{
			if (keyvalue[0] != ' ')
 				strncpy(vol,keyvalue,VOLUMELEN);		/* Volume name is in keyvalue, copy to UFB.	*/
			else
 				strncpy(keyvalue,vol,VOLUMELEN);		/* Volume name is in UFB, copy to keyvalue.	*/
		}
	}
}
/*
**	History:
**	$Log: setfile.c,v $
**	Revision 1.13  1998-10-22 14:11:57-04  gsl
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
