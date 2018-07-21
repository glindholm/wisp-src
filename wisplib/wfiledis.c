/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/


/* WFILE_DISP.C ... 	This is the front end of the Wang style DISPLAY routine.  It directs the user to enter a Wang VS style	*/
/*	      		file, library, and volume specification.  The target system filename is then constructed and the	*/
/*			vdisplay() routine is called to actually show the file.							*/


/*			Include required header files.										*/
                
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "idsistd.h"
#include "wperson.h"
#include "wfiles.h"									/* Interface with WFNAME.		*/
#include "wcommon.h"
#include "wangkeys.h"
#include "wfname.h"
#include "level.h"
#include "wisplib.h"
#include "sharemem.h"
#include "vwang.h"
#include "filext.h"
#include "wdefines.h"
#include "wfiledis.h"
#include "wmalloc.h"
#include "wispcfg.h"

#include "idsisubs.h"
#include "filgparm.h"

#ifdef WIN32
#include "win32spn.h"
#endif

#include "werrlog.h"
#include "vssubs.h"


/* OLD frontend for internal display */
void WL_wfile_disp(void)
{
	char	filename[COB_FILEPATH_LEN + 1];
	int	recsize = 0;

	/*
	**	Issues the getparms and if a file was supplied then display it.
	*/
	if (WL_display_util_getparms(filename, &recsize))
	{
		/*
		**	Do the DISPLAY
		*/
		vwang_wpushscr();

		WL_internal_display(filename, recsize);

		vwang_wpopscr();
	}
}


/*
**	ROUTINE:	WL_display_util_getparms()
**
**	FUNCTION:	Issue the DISPLAY getparms and return the filename.
**
**	DESCRIPTION:	Issues the INPUT getparm.
**
**	ARGUMENTS:	
**	filename	The returned filename to be displayed.
**
**	GLOBALS:	None
**
**	RETURN:		
**	0		Exit without displaying a file
**	1		A filename was supplied.
**
**	WARNINGS:	Ensure the link-level is correct before calling this routine
**			because GETPARM is dependent on the link-level.
**
*/
int WL_display_util_getparms(char *filename, int* recsize)
{
	char file[SIZEOF_FILE];
	char library[SIZEOF_LIB];
	char volume[SIZEOF_VOL];
	char entry_message[80];
	char *end_name;
	char tempname[COB_FILEPATH_LEN], orig_file[SIZEOF_FILE];
	int4 mode;
	int4 native_mode;
	char getparm_type[3];
	char pf_key;

	WL_wpload();									/* Load personality stuff.		*/
	native_mode = 0L;                                           			/* Start off in WANG_VS mode.		*/

	memset(file, ' ', SIZEOF_FILE);						/* Initialize variables goin' to	*/
	memset(orig_file, ' ', SIZEOF_FILE);					/* file_getparm().			*/
	memset(tempname, ' ', COB_FILEPATH_LEN);
	WL_get_defs(DEFAULTS_IV,volume);
	WL_get_defs(DEFAULTS_IL,library);

	strcpy(entry_message,"To display a file, enter the name and location of the file. ");
	getparm_type[0] = 'I';							/* Start off as an initial getparm.	*/
	getparm_type[1] = ' ';
	getparm_type[2] = '\0';
	mode = 0;
        
	for(;;)
	{
		WL_file_getparm3(file,library,volume,"INPUT   ","DISP  ",		/* Allow the user to enter the name.	*/
			&native_mode,getparm_type,tempname,
			entry_message,NULL,&pf_key,'E',orig_file,NULL,NULL,NULL,
			0,0,0,0);
											/* If they pressed PF16 to get out of	*/
											/* get_name, these variables will all	*/
											/* be equal to spaces.			*/

		if (PFKEY_16_PRESSED == pf_key)
		{
			/* Exit without displaying a file */
			return 0;
		}

		if (native_mode)
		{
			cobx2cstr(filename, tempname, COB_FILEPATH_LEN); 
		}
		else
		{
			if (0 != memcmp(file,"        ",8))
			{
				char saveExt[39];
				mode = IS_PRINTFILE;					/* Tell wfname what type of file.	*/
				WGETFILEXT(saveExt);					/* Save the file extension.		*/
				end_name = WL_wfname_backfill(&mode, volume, library, file, filename);
				*end_name = '\0';					/* Null terminate.			*/
				memcpy(tempname,filename,strlen(filename));
				WSETFILEXT(saveExt);				/* Restore the extension.		*/
			}
			else
			{
				filename[0] = '\0';
			}
		}

		if (WL_fcanread(filename))	/* See if we can read the file */
		{
			/* Success - we have access		*/

			/*
			**	Update the PRB for backwards referencing.
			*/
			if ( !native_mode && 'R' == getparm_type[0] )
			{
				WL_use_last_prb();
				getparm_type[0] = 'R';
				getparm_type[1] = 'D';
				WL_file_getparm3(file,library,volume,"INPUT   ","DISP  ",
					&native_mode,getparm_type,tempname,
					entry_message,NULL,&pf_key,'E',orig_file,NULL,NULL,NULL,
					0,0,0,0);
			}

			wtrace("DISPLAY","GETPARM","Selected file [%s] to display",filename);
			

			WL_display_util_options_getparm(recsize);

			return 1;
		}
										/* Nope.  Give a new entry message.	*/
		strcpy(entry_message,"UNABLE TO OPEN FILE, ACCESS DENIED or FILE MISSING");
		getparm_type[0] = 'R';					/* Now its a respecify getparm.		*/
		getparm_type[1] = ' ';
	}
}

/*
**	ROUTINE:	WL_utils_in_windows()
**
**	FUNCTION:	Flag if utilities are to be run in separate windows (UTILSWINDOWS option)
**
**	DESCRIPTION:	This applies to DISPLAY and VSEDIT from mngfiles().
**			It is only meaningful on windowing environments (NT/95)
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		
**	0		Run utils in same window.
**	1		Run utils in their own window
**
**	WARNINGS:	None
**
*/
int WL_utils_in_windows(void)
{
	static int rc = -1;
	
	if (-1 == rc)
	{
		rc = 0;
		
#ifdef WIN32
		if (WL_no_windows())
		{
			rc = 0;
		}
		else if (WL_get_wisp_option("UTILSWINDOWS"))
		{
			rc = 1;
			wtrace("OPTIONS","UTILSWINDOWS","Using Utilities in Windows option");
		}
#endif /* WIN32 */
	}
	return rc;
}

/*
**	ROUTINE:	WL_use_internal_display()
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
int WL_use_internal_display(void)
{
	static int rc = -1;
	
	if (-1 == rc)
	{
		if (wisp_nativescreens())
		{
			rc = 0;
		}
		else if (WL_custom_display_utility())
		{
			rc = 0;
		}
		else if (WL_get_wisp_option("EXTDISPLAY"))
		{
			rc = 0;
		}
		else if (WL_utils_in_windows())
		{
			rc = 0;
		}
		else
		{
			rc = 1;
		}

		wtrace("DISPLAY","MODE","Using %s DISPLAY Utility", rc ? "Internal":"External");
		
	}
	return rc;
}

/*
**	ROUTINE:	WL_link_display()
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
int WL_link_display(const char* filepath)
{
	if (WL_use_internal_display())
	{
		int rc;
		
		vwang_wpushscr();				/* This forces vwang/video initialization */
		rc = WL_internal_display(filepath, 0);
		vwang_wpopscr();

		return rc;
	}
	else
	{
		int4	onecnt, compcode, retcode;
		char	the_file[WISP_FILEPATH_LEN];

		if (strlen(filepath) >= sizeof(the_file))
		{
			WL_werrlog_error(WERRCODE(104),"DISPLAY", "NAME", 
				"Name of file to display is too long.");
			return 0;
		}
		
		memset(the_file,'\0',sizeof(the_file));
		strcpy(the_file,filepath);
		
		WL_put_swap(&onecnt, 1);

		WL_set_va_count(8);
		LINK2("DISPLAY ", 	(int4)SIZEOF_FILE,
		      "S",		(int4)1,
		      "@SYSTEM@",	(int4)SIZEOF_LIB,
		      "SYSTEM",		(int4)SIZEOF_VOL,
		      &onecnt,		(int4)sizeof(int4),
		      the_file,		(int4)sizeof(the_file),
		      &compcode,	(int4)sizeof(int4),
		      &retcode,		(int4)sizeof(int4));

		WL_wswap(&compcode);
		WL_wswap(&retcode);

		if (8 == compcode)
		{
			WL_werrlog_error(WERRCODE(104),"DISPLAY", "LINK", 
				"Link to DISPLAY failed");
			return 0;
		}
		else
		{
			return 1;
		}
	}
}


/*
**	ROUTINE:	WL_internal_display()
**
**	FUNCTION:	Display the file using vdisplay()
**
**	DESCRIPTION:	Get the record length and call vdisplay().
**
**	ARGUMENTS:	
**	filepath	The file to be displayed.
**	reclen		The record length (0 = variable)
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
int WL_internal_display(const char* filepath, int reclen)
{
	return WL_vdisplay(filepath, reclen);
}

/*
**	ROUTINE:	WL_custom_display_utility()
**
**	FUNCTION:	Return the name of the custom DISPLAY utility (DISPLAYUTIL)
**
**	DESCRIPTION:	Get the name from option DISPLAYUTIL.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	none
**
**	RETURN:		The name of the custom display utility or NULL if 
**			no custom display utility.  The name may be an exe which can
**			be found on the path ("notepad") or a fully qualified path
**			to the executable ("C:\BIN\MYDISPLAY.EXE").
**
**	WARNINGS:	None
**
*/
const char* WL_custom_display_utility(void)
{
	static int first = 1;
	static char *exe = NULL;
	
	if (first)
	{
		const char *ptr;

		first = 0;

#ifdef WIN32
		if (WL_no_windows())
		{
			/*
			**	On Windows the DISPLAYUTIL option is disabled if
			**      there is no windows (running in telnet).
			*/
			return exe;
		}
#endif
		
		if ((ptr = WL_get_wisp_option("DISPLAYUTIL")) && *ptr)
		{
			char buff[256];
			if (1 == sscanf(ptr,"%s",buff))
			{
				exe = wisp_strdup(buff);
				wtrace("OPTIONS","DISPLAYUTIL", "Using display utility [%s]",exe);
			}
		}
	}	
	
	return exe;
}


/*
**	History:
**	$Log: wfiledis.c,v $
**	Revision 1.35  2003/02/20 23:14:34  gsl
**	Add OPTIONS get to DISPLAY utility that gets the record size RECSIZE
**	
**	Revision 1.34  2003/02/17 22:07:17  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.33  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.32  2002/12/09 21:45:44  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.31  2002/12/09 19:15:36  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.30  2002/10/07 20:54:25  gsl
**	Change to us WL_fcanread() instead of fopen() the file
**	
**	Revision 1.29  2002/07/29 15:46:49  gsl
**	getwfilext -> WGETFILEXT
**	setwfilext -> WSETFILEXT
**	setwispfilext -> WSETFILEXT
**	
**	Revision 1.28  2002/07/12 19:10:19  gsl
**	Global unique WL_ changes
**	
**	Revision 1.27  2002/07/12 17:01:03  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.26  2002/07/11 20:29:17  gsl
**	Fix WL_ globals
**	
**	Revision 1.25  2002/07/10 21:05:31  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.24  2002/07/09 04:13:54  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.23  2002/07/02 21:15:32  gsl
**	Rename wstrdup
**	
**	Revision 1.22  2002/06/28 04:02:58  gsl
**	Work on native version of wfopen and wfname
**	
**	Revision 1.21  2002/06/27 04:12:41  gsl
**	Clean up status/mode bits
**	
**	Revision 1.20  2002/06/26 04:25:04  gsl
**	Cleanup mode/status bit fields
**	
**	Revision 1.19  2002/06/26 01:42:46  gsl
**	Remove VMS code
**	
**	Revision 1.18  2002/06/25 17:46:05  gsl
**	Remove WISPFILEXT as a global, now must go thru set/get routines
**	
**	Revision 1.17  1999/02/23 21:58:53  gsl
**	moved the no_windows() routine to wispcfg.c
**	
**	Revision 1.16  1999-02-23 15:38:34-05  gsl
**	Add no_windows() routine which returns true on unix and when using
**	telnet on windows NT.
**	Disable the UTILSWINDOW and DISPLAYUTIL options on windows when
**	the no_windows() tells us were in telnet.
**
**	Revision 1.15  1999-01-21 16:30:31-05  gsl
**	Surround call to internal_display() with vwang_wpushscr() and vwang_wpopscr() just like it
**	is done in LINK.  This is needed to fix TRK#918, CRID REPORT utility
**	when printing a report to the screen DEVICE=DISPLAY uses link_display()
**	this garbles the screen because vwang is not initialized because
**	all io has been thru GETPARMS.
**
**	Revision 1.14  1998-08-03 17:19:28-04  jlima
**	SUpport Logical Volume Translation to long file names containing eventual embedded blanks.
**
**	Revision 1.13  1998-05-05 13:50:10-04  gsl
**	Add support for custom display utility DISPLAYUTIL and for UTILSWINDOWS.
**	Rewrote much of the display frontend routines.
**
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
