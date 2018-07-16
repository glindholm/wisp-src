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

/* WFILE_DISP.C ... 	This is the front end of the Wang style DISPLAY routine.  It directs the user to enter a Wang VS style	*/
/*	      		file, library, and volume specification.  The target system filename is then constructed and the	*/
/*			vdisplay() routine is called to actually show the file.							*/


/*			Include required header files.										*/
                
#include <stdio.h>
#include <string.h>
#include "idsistd.h"
#include "wperson.h"
#include "wfiles.h"									/* Interface with WFNAME.		*/
#include "wcommon.h"
#include "wangkeys.h"
#include "wfname.h"
#include "werrlog.h"
#include "level.h"
#include "wisplib.h"
#include "sharemem.h"
#include "vwang.h"
#include "filext.h"
#include "wdefines.h"


/*			Subroutine entry point.											*/

void wfile_disp(void)
{
#define		ROUTINE		77100
	char file[SIZEOF_FILE];
	char library[SIZEOF_LIB];
	char volume[SIZEOF_VOL];
	char filename[COB_FILEPATH_LEN];
	char entry_message[80];
	char *end_name;
	char tempname[COB_FILEPATH_LEN], orig_file[SIZEOF_FILE];
	int4 mode;
	int4 native_mode;
	char getparm_type[3];
	char pf_key;
	FILE *fh;
	int displaying = TRUE;								/* Flag set to loop until PF16 pushed.	*/

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	wpload();									/* Load personality stuff.		*/
	native_mode = 0L;                                           			/* Start off in WANG_VS mode.		*/

	displaying = TRUE;								/* Keep displaying.			*/
	while (displaying)								/* Loop until no more to display.	*/
	{

		displaying = FALSE;							/* Only do one display.			*/

		memset(file, ' ', SIZEOF_FILE);						/* Initialize variables goin' to	*/
		memset(orig_file, ' ', SIZEOF_FILE);					/* file_getparm().			*/
		memset(tempname, ' ', COB_FILEPATH_LEN);
		get_defs(DEFAULTS_IV,volume);
		get_defs(DEFAULTS_IL,library);

		strcpy(entry_message,"To display a file, enter the name and location of the file. ");
		getparm_type[0] = 'I';							/* Start off as an initial getparm.	*/
		getparm_type[1] = ' ';
		getparm_type[2] = '\0';
		mode = 0;
                         
get_name_loop:						      				/* A label to go to.			*/

		file_getparm2(mode,file,library,volume,"INPUT   ","DISP  ",		/* Allow the user to enter the name.	*/
			&native_mode,getparm_type,tempname,
			entry_message,NULL,&pf_key,'E',orig_file,NULL,NULL,NULL);
											/* If they pressed PF16 to get out of	*/
											/* get_name, these variables will all	*/
											/* be equal to spaces.			*/

		if (pf_key == PFKEY_16_PRESSED || (file[0] == ' ' && library[0] == ' ' && volume[0] == ' ' && tempname[0] == ' '))
		{
			displaying = FALSE;						/* Set to stop display loop.		*/
		}                   
		else
		{
			int i;

			if (native_mode)
			{
				memcpy(filename,tempname,COB_FILEPATH_LEN);
				*strchr(filename,' ') = '\0';				/* Null terminate.			*/
			}
			else		  						/* Are we NOT in native mode?		*/
			{								/* Yup.					*/
				mode = IS_PRINTFILE;					/* Tell wfname what type of file.	*/
				SAVE_WISPFILEXT;					/* Save the file extension.		*/
				end_name = wfname(&mode, volume, library, file, filename);	/* Construct native filename.	*/
				*end_name = '\0';					/* Null terminate.			*/
				memcpy(tempname,filename,strlen(filename));
			}

			if (fh = fopen(filename,"r"))					/* See if we can open file		*/
			{
				fclose(fh);						/* Success - we have access		*/
			}
			else
			{								/* Nope.  Give a new entry message.	*/
				strcpy(entry_message,"UNABLE TO OPEN FILE, ACCESS DENIED or FILE MISSING");
				getparm_type[0] = 'R';					/* Now its a respecify getparm.		*/
				getparm_type[1] = ' ';
				RESTORE_WISPFILEXT;					/* Restore the extension.		*/
				goto get_name_loop;					/* And loop back up.			*/
			}

			/*
			**	Update the PRB for backwards referencing.
			*/
			if ( !native_mode && 'R' == getparm_type[0] )
			{
				use_last_prb();
				getparm_type[0] = 'R';
				getparm_type[1] = 'D';
				file_getparm2(mode,file,library,volume,"INPUT   ","DISP  ",
					&native_mode,getparm_type,tempname,
					entry_message,NULL,&pf_key,'E',orig_file,NULL,NULL,NULL);
			}

			/*
			**	Do the DISPLAY
			*/
			wpushscr();	      						/* Save the screen map and state.	*/
			if ((i = greclen(filename)) >= 0) vdisplay(filename,i);		/* Get the record length.		*/
			else if (i == -2) werrlog(ERRORCODE(2),filename,0,0,0,0,0,0,0);	/* Protection violation.		*/
			else if (i == -1) werrlog(ERRORCODE(4),filename,0,0,0,0,0,0,0);	/* Error on OPEN.			*/
			else vdisplay(filename,255);  					/* Display the file using max buffsize.	*/
			wpopscr();							/* Restore the screen and map.		*/
		}
	}

	return;			      							/* All done mate.			*/
}

/*
**	ROUTINE:	use_internal_display()
**
**	FUNCTION:	Test if using an internal (soft-link) to DISPLAY utility
**
**	DESCRIPTION:	Default is to use the internal display facility.
**			If nativescreens or if the EXTDISPLAY options is set
**			the use the external DISPLAY utility.
**
**	ARGUMENTS:	none
**
**	GLOBALS:	none
**
**	RETURN:		
**	1		(TRUE)  Use internal display.
**	0		(FALSE) LINK to external DISPLAY utility
**
**	WARNINGS:	none
**
*/
int use_internal_display(void)
{
	static int rc = -1;
	
	if (-1 == rc)
	{
		if (nativescreens())
		{
			rc = 0;
		}
		else if (get_wisp_option("EXTDISPLAY"))
		{
			rc = 0;
		}
		else
		{
			rc = 1;
		}
	}
	return rc;
}

/*
**	ROUTINE:	link_display()
**
**	FUNCTION:	Display a file (either internal or external display)
**
**	DESCRIPTION:	If an internal display call vdisplay() directly.
**			If an external display call LINK.
**
**	ARGUMENTS:	
**	filepath	The native file path to display.
**
**	GLOBALS:	None
**
**	RETURN:		
**	0		Failed
**	1		Success
**
**	WARNINGS:	None
**
*/
int link_display(const char* filepath)
{
	if (use_internal_display())
	{
		int	reclen;

		reclen = greclen(filepath);
		if (reclen >= 0)
		{
			return vdisplay(filepath,reclen);
		}
		return 0;
	}
	else
	{
		int4	argcnt, onecnt, compcode, retcode;
		char	the_file[WISP_FILEPATH_LEN];

		if (strlen(filepath) >= sizeof(the_file))
		{
			werrlog(104,"%%DISPLAY-F-NAME Name of file to display is too big",0,0,0,0,0,0,0);
			return 0;
		}
		
		memset(the_file,'\0',sizeof(the_file));
		strcpy(the_file,filepath);
		
		put_swap(&onecnt, 1);

		argcnt = 8;
		wvaset(&argcnt);
		LINK2("DISPLAY ", 	(int4)SIZEOF_FILE,
		      "S",		(int4)1,
		      "@SYSTEM@",	(int4)SIZEOF_LIB,
		      "SYSTEM",		(int4)SIZEOF_VOL,
		      &onecnt,		(int4)sizeof(int4),
		      the_file,		(int4)sizeof(the_file),
		      &compcode,	(int4)sizeof(int4),
		      &retcode,		(int4)sizeof(int4));

		wswap(&compcode);
		wswap(&retcode);

		if (8 == compcode)
		{
			werrlog(104,"%%DISPLAY-F-LINK Link to DISPLAY failed",0,0,0,0,0,0,0);
			return 0;
		}
		else
		{
			return 1;
		}
	}
}

/*
**	History:
**	$Log: wfiledis.c,v $
**	Revision 1.12  1997-10-23 16:21:41-04  gsl
**	Add use_internal_display()
**	Add link_display() as a front-end to vdisplay() or to LINK to DISPLAY.
**	In wfile_disp() moved the link-level logic into LINK where it handles
**	the soft-link to DISPLAY.
**
**	Revision 1.11  1996-08-19 18:33:15-04  gsl
**	drcs update
**
**
**
*/
