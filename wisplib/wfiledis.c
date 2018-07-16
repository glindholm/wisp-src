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


#ifdef WIN32
#include "win32spn.h"
#endif

#include "werrlog.h"
#define		ROUTINE		77100


/* OLD frontend for internal display */
void wfile_disp(void)
{
	char	filename[COB_FILEPATH_LEN + 1];

	/*
	**	Issues the getparms and if a file was supplied then display it.
	*/
	if (display_util_getparms(filename))
	{
		/*
		**	Do the DISPLAY
		*/
		wpushscr();

		internal_display(filename);

		wpopscr();
	}
}

/*
**	ROUTINE:	display_util_getparms()
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
int display_util_getparms(char *filename)
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

	wpload();									/* Load personality stuff.		*/
	native_mode = 0L;                                           			/* Start off in WANG_VS mode.		*/

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
        
	for(;;)
	{
		file_getparm2(mode,file,library,volume,"INPUT   ","DISP  ",		/* Allow the user to enter the name.	*/
			&native_mode,getparm_type,tempname,
			entry_message,NULL,&pf_key,'E',orig_file,NULL,NULL,NULL);
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
				mode = IS_PRINTFILE|IS_BACKFILL;			/* Tell wfname what type of file.	*/
				WGETFILEXT(saveExt);					/* Save the file extension.		*/
				end_name = wfname(&mode, volume, library, file, filename);	/* Construct native filename.	*/
				*end_name = '\0';					/* Null terminate.			*/
				memcpy(tempname,filename,strlen(filename));
				WSETFILEXT(saveExt);					/* Restore the extension.		*/
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
				use_last_prb();
				getparm_type[0] = 'R';
				getparm_type[1] = 'D';
				file_getparm2(mode,file,library,volume,"INPUT   ","DISP  ",
					&native_mode,getparm_type,tempname,
					entry_message,NULL,&pf_key,'E',orig_file,NULL,NULL,NULL);
			}

			wtrace("DISPLAY","GETPARM","Selected file [%s] to display",filename);
			
			return 1;
		}
										/* Nope.  Give a new entry message.	*/
		strcpy(entry_message,"UNABLE TO OPEN FILE, ACCESS DENIED or FILE MISSING");
		getparm_type[0] = 'R';					/* Now its a respecify getparm.		*/
		getparm_type[1] = ' ';
	}
}

/*
**	ROUTINE:	utils_in_windows()
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
int utils_in_windows(void)
{
	static int rc = -1;
	
	if (-1 == rc)
	{
		rc = 0;
		
#ifdef WIN32
		if (no_windows())
		{
			rc = 0;
		}
		else if (get_wisp_option("UTILSWINDOWS"))
		{
			rc = 1;
			wtrace("OPTIONS","UTILSWINDOWS","Using Utilities in Windows option");
		}
#endif /* WIN32 */
	}
	return rc;
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
		else if (custom_display_utility())
		{
			rc = 0;
		}
		else if (get_wisp_option("EXTDISPLAY"))
		{
			rc = 0;
		}
		else if (utils_in_windows())
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
int WL_link_display(const char* filepath)
{
	if (use_internal_display())
	{
		int rc;
		
		wpushscr();					/* This forces vwang/video initialization */
		rc = internal_display(filepath);
		wpopscr();

		return rc;
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
**	ROUTINE:	internal_display()
**
**	FUNCTION:	Display the file using vdisplay()
**
**	DESCRIPTION:	Get the record length and call vdisplay().
**
**	ARGUMENTS:	
**	filepath	The file to be displayed.
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
int internal_display(const char* filepath)
{
	int reclen;
	
	reclen = greclen(filepath);

	if (reclen >= 0) 
	{
		return vdisplay(filepath,reclen);
	}
	
	if (reclen == -2) 
	{
		werrlog(ERRORCODE(2),filepath,0,0,0,0,0,0,0);	/* Protection violation.		*/
		return 0;
	}
	
	if (reclen == -1) 
	{
		werrlog(ERRORCODE(4),filepath,0,0,0,0,0,0,0);	/* Error on OPEN.			*/
		return 0;
	}
	
	return vdisplay(filepath,255);  					/* Display the file using max buffsize.	*/
}

/*
**	ROUTINE:	custom_display_utility()
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
const char* custom_display_utility(void)
{
	static int first = 1;
	static char *exe = NULL;
	
	if (first)
	{
		const char *ptr;

		first = 0;

#ifdef WIN32
		if (no_windows())
		{
			/*
			**	On Windows the DISPLAYUTIL option is disabled if
			**      there is no windows (running in telnet).
			*/
			return exe;
		}
#endif
		
		if ((ptr = get_wisp_option("DISPLAYUTIL")) && *ptr)
		{
			char buff[256];
			if (1 == sscanf(ptr,"%s",buff))
			{
				exe = wstrdup(buff);
				wtrace("OPTIONS","DISPLAYUTIL", "Using display utility [%s]",exe);
			}
		}
	}	
	
	return exe;
}


/*
**	History:
**	$Log: wfiledis.c,v $
**	Revision 1.17.2.3  2002/11/14 21:12:27  gsl
**	Replace WISPFILEXT and WISPRETURNCODE with set/get calls
**	
**	Revision 1.17.2.2  2002/11/12 16:00:23  gsl
**	Applied global unique changes to be compatible with combined KCSI
**	
**	Revision 1.17.2.1  2002/10/09 21:03:04  gsl
**	Huge file support
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
**	Surround call to internal_display() with wpushscr() and wpopscr() just like it
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
