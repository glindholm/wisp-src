/*
******************************************************************************
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
******************************************************************************
*/


/*
**	File:		vsemain.c
**
**	Purpose:	?
**
**	Routines:	
**	main()
**	process_args()
**	vse_init()
**	vse()
**	val_inputs()
**	val_language()
**	val_file()
**	val_sysname()
**	val_vs_name()
**	val_file_exists()
**	init_input_fields()
**
**
**	History:	Originally written by Mo Budlong, Finished by Jock Cooper.
**
*/

/*----
Invoke as

vsedit	or	vsedit filename

The keys to the kingdom

modules

vsemain.c	Includes main() and routines to process command line
		args. If args are found, the program goes straight
		to vsedit(), otherwise vse() is called which
		calls a Wang type front screen to request
		file name and language.

vsedit.c	This is the first layer below vsemain. vsedit(receives
		a file name and language (inupper case). This initializes
		the defaults for the requested language, loads the
		file and numbers the lines. This also includes the
		routine to save a file, although it is not invoked
		from within this module. vsedit calls vse_menus() which
		repated calls one or the other of the two main
		menus in the system, vse_standard_menu() and
		vse_special_menu()

vsestmnu.c	vse_standard_menu() is a screen of menu picks that
		include all the standard editing options. Insert
		Delete, Modify, Copy Move etc, as well as 
		Display the file and select the special menu.
		The functions duplicate functions that are available
		from the display and edit screen. These functions
		are not implemented at this level of the menu which
		can only be used to (1) Display the File or (16)
		Start the special menu.
		This module is little more than a menu display
		and function dispatcher.
		Implemented functions are
		1 Edit the file
		16 Exit to Special menu

vsespmnu.c	vse_special_menu() includes all of the functions
		associated with actions around the file rather than
		in it such as set defaults, create a new output
		file, save current file, external copy.
		This module is also basically a menu and dispatcher.
		Implemented functions are
		1 Edit the file
		3 switch to standard menu
		4 restart (Edit another file
		5 Create (only for a new file)
		6 Save (only for an existing file)
		16 exit

vsedscr.c	vse_ed_scr() is the main edting screen. It displays
		a file page by page and allows pf key options
		to edit the file.
		1.	Display the file (does nothing)
		2.	Top of file (implemented)
		3.	Bottom of file (implemented)
		4.	Page Up (implemented)
		5.	Page Down (implemented)
		6.	Line Up (implemented)
		7.	Line Down (implemented)
		8.	Search backward or forward (implemented)
		9.	Modify Current screen (Implemented)
		10.	Search and replace (NOT implemented)
		11.	insert new line (implemented)
		12.	delete range of lines (implemented only for files
			that include line numbering)
		13.	move lines in the file (NOT implemented)
		14.	copy lines in the file (NOT implemented)
		15.	show cursor column (NOT implmented)
		16.	Exit to Special menu.

vsedmod.c	vse_ed_mod() is called when (9)Modify is pressed.
		This makes the entire screen modifiable, allows user
		input, and then saves the results in the work area.

vsedins.c	vse_ed_ins() is called when (11)Insert is pressed.
		It opens a single modifiable line on the screen at the
		cursor, and allows user input. The resulting line
		is inserted into the work space.

vsedel.c	vse_ed_del() is called when (12)Delete is pressed.
		It allows entry of a sinlge line number or range of
		lines by line number and deltes the requested range.
		It only allows deletion by line number, so at the
		moment only works on COBOL or line numbered files.

vsedfnd.c	vse_ed_fnd() is called when (8)Find is pressed. It
		allows entry of a test field or line number, and
		advances the screen to display that line when found.

Miscellaneuos Modules.
vsetxt.c	The work area is built by loading a double linked
		list in memory containing a pointer to the text
		line, a pointer to a previous struct, next struct,
		and a line number. This module includes routines
		for allocating the memeory, creating, inserting,
		deleting, etc the TEXT structures.

vseglb.c	Includes the definitions of all globals which are
		actually defined and declared in vseglb.h

vseglb.h	Includes definitions/declarations of all globals,
		some constants, and few bits of shorthand for
		clearing fields. Definition of the TEXT struct.

vsegp.c		Includes a few short routines for initializing and
		displaying GETPARMS.

vsegp.h		Includes some constants for PFKEYS and some shorthand
		for defining and loading getparms.

vseinp.c	A single INPUT GETPARM that approximates the initial
		screen of the VS Editor.

vsecre.c	A single OUTPUT GETPARM that asks the user for the name
		of the output file when creating a new file.

vsescr.c	A set of short routines to load a vwang screen for
		display

vsescr.h	Some shorthand that allows screens to be defined much
		more easily and readably. (See examples in vsestmnu.c
		vsespmnu.c vsedscr.c

vsenaf.c	A couple of quick display routines to let the user
		know that he has selected a NOT AVAILABLE function.

vseutl.c	A collection of odd utility routines to truncate and
		space pad a field, check for a files existence,
		check if a field is blank.
------*/

/*----
Missing pieces are many

3.	The output screen does not look like the Wang screen.

5.	Since the work space is creating by Mallocing, there should
be a message that space can not be allocated. This version just
aborts.


------*/

#include <stdio.h>

#include "idsistd.h"

#include "filext.h"

#include "vsedit.h"
#include "vseglb.h"
#include "vseutl.h"

#include "wcommon.h"
#include "wperson.h"
#include "wfname.h"
#include "idsisubs.h"
#include "level.h"
#include "sharemem.h"
#include "vwang.h"
#include "wisplib.h"
#include "wexit.h"
#include "filext.h"


static void vse_init(void);
static void vse(void);
static void val_inputs(void);
static int val_language(void);
static void val_file(void);
static void val_sysname(void);
static void val_vs_name(void);
static int val_file_exists(char *sysname);
static void init_input_fields(void);
static int set_read_only(int flag);



static char error_message[81];
static int screen_error;

static char applname[9]="VSEDIT  ";

int main(int argc,char **argv)
{
	strcpy(VSE_COPYRIGHT,	        
/*12345678901234567890123456789012345678901234567890123456789012345678901234567890*/
  "(c) Shell Stream Software LLC - VSEDIT Integrated Development Editor - v 5.01");

	WL_wpload();
	vsedit_globals();
	WL_initglbs(applname);
	vse_init();
	vwang_title("WISP VSEDIT");
	vwang_init_screen();
	if(screen_error)
	{
		goto cleanup_exit;
	}

	if (argc > 1)
	{
		/*
		**	This handles a filename on the command line.
		*/
		strcpy(vse_sysname,argv[1]);		
		vse_native = 1;
		strcpy(vse_gp_input_language,"         ");

		/*
		**	Set the language based on the extension
		*/
		if (hasext(vse_sysname))
		{
			char	ext[40];

			strcpy(ext, splitext(vse_sysname));
			WL_upper_string(ext);
			if      (0==strcmp(ext,".WCB"))	strcpy(vse_gp_input_language,"WCB      ");
			else if (0==strcmp(ext,".COB"))	strcpy(vse_gp_input_language,"COB      ");
			else if (0==strcmp(ext,".WPS"))	strcpy(vse_gp_input_language,"WPS      ");
			else if (0==strcmp(ext,".BAS"))	strcpy(vse_gp_input_language,"BAS      ");
		}
	}

	/*
	**	Run the editor.
	*/
	vse();

	/*
	**	Cleanup and exit
	*/
cleanup_exit:
	vwang_shut();
	WL_wexit(0);
	return 0;
}

static void vse_init(void)
{
	screen_error = 0;
	strcpy(error_message,"");

	init_gpint();
	strcpy(vse_gp_input_language,"WCB      ");

	WL_get_defs(DEFAULTS_IV,vse_gp_input_volume);
	WL_get_defs(DEFAULTS_IL,vse_gp_input_library);
}

static void vse(void)
{
	for(;;)
	{
		vse_input(error_message);
		screen_error = 0;
		strcpy(error_message,"");
		if(vse_input_pick == 16)
		{
			break;
		}
		if( (vse_input_pick == 5) &&
		    (!vse_native)          )
		{
			translate_name(vse_gp_input_file, vse_gp_input_library, vse_gp_input_volume, vse_sysname);
			vse_trunc(vse_sysname);
		}
			
		if(vse_input_pick == 0)
		{
			val_inputs();
			if(!screen_error)
			{
				if (vsedit(vse_sysname))
				{
					screen_error = 1;
					strcpy(error_message,"Unable to LOAD file");
				}
				else if (vse_special_pick == 16)
					return;
				else
					init_input_fields();
			}
		}
		if(vse_input_pick == 5)
		{
			vse_native = vse_native?0:1;
		}
	}
}

/*
**	Routine:	translate_name()
**
**	Function:	Translates a wang style name to a native filepath.
**
**	Description:	This is a frontend to wfname.  If the file name is
**			blank then it returns a blank native_name;
**			The file extention is set based on the language.
**
**	Arguments:
**	wang_file	The 8 char wang file name
**	wang_lib	The 8 char wang lib name
**	wang_vol	The 6 char wang vol name
**	wang_native	The returned native path name blank padded with a null in the last position.
**
**	Globals:	None
**
**	Return:
**	0		Got a native name
**	1		File name was blank and native name is blank.
**
**	Warnings:	None
**
**	History:
**	03/11/94	Written by GSL
**
*/
int translate_name(char *wang_file, char *wang_lib, char *wang_vol, char *native_name)
{
	int4 	mode;

	mode = 0;

	memset(native_name,' ',VSE_SYSNAME_LEN);
	native_name[VSE_SYSNAME_LEN] = 0;

	if(isblankstr(wang_file,VSE_FILENAME_LEN))
	{
		return 1;
	}

	WSETFILEXT(lang_ext());

	WL_wfname(&mode,wang_vol,wang_lib,wang_file,native_name);
	native_name[VSE_SYSNAME_LEN] = 0;

	return 0;
}

static void val_inputs(void)
{
	val_language();
	if(!screen_error)
	{
		val_file();
	}
}

static int val_language(void)
{
	if (0 == init_lang(vse_gp_input_language) )
	{
		return(1);
	}

	screen_error = 1;
	strcpy(error_message,"The LANGUAGE specified is unknown or not supported");
	return(0);
}

static void val_file(void)
{
	if(vse_native)
		val_sysname();
	else
		val_vs_name();
}

static void val_sysname(void)
{
	if(isblankstr(vse_sysname,VSE_SYSNAME_LEN))
	{
		vse_new_file = 1;
		return;
	}
	else
		vse_new_file = 0;

	val_file_exists(vse_sysname);
}	
		
static void val_vs_name(void)
{

	if(isblankstr(vse_gp_input_file,VSE_FILENAME_LEN))
	{
		vse_new_file = 1;
		return;
	}
	else
		vse_new_file = 0;

	translate_name(vse_gp_input_file, vse_gp_input_library, vse_gp_input_volume, vse_sysname);
	vse_trunc(vse_sysname);
	val_file_exists(vse_sysname);
}

static int val_file_exists(char *sysname)
{
	if(!vse_exists(sysname))
	{
		screen_error = 1;
		strcpy(error_message,"The file was NOT FOUND.");
	}

	if (!screen_error)
	{
		FILE	*ff;

		ff = fopen(sysname,"r");
		if (ff)
		{
			fclose(ff);
		}
		else
		{
			screen_error = 1;
			strcpy(error_message,"File can not be read.");
		}
	}

	if (!screen_error)
	{
		FILE	*ff;

		ff = fopen(sysname,"r+");
		if (ff)
		{
			fclose(ff);
			set_read_only(0);
		}
		else
		{
			set_read_only(1);
		}
	}
	return(!screen_error);
}

static void init_input_fields(void)
{
	CLEAR_FIELD(vse_gp_input_file);
	CLEAR_FIELD(vse_sysname);
}

static int read_only_flag = 0;
static int set_read_only(int flag)
{
	return(read_only_flag = flag);
}
int is_read_only(void)
{
	return(read_only_flag);
}
/*
**	History:
**	$Log: vsemain.c,v $
**	Revision 1.39  2010/01/10 00:36:15  gsl
**	refactor utils to add vse_ prefix to avoid conflicts with trunc
**	vse_trunc
**	
**	Revision 1.38  2009/10/18 21:01:34  gsl
**	Copyright
**	
**	Revision 1.37  2004/04/13 15:59:49  gsl
**	Change VSEDIT version to 5.01 with Move Single Line Bug fix
**	
**	Revision 1.36  2003/02/05 21:47:54  gsl
**	fix -Wall warnings
**	
**	Revision 1.35  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.34  2002/07/29 15:46:51  gsl
**	getwfilext -> WGETFILEXT
**	setwfilext -> WSETFILEXT
**	setwispfilext -> WSETFILEXT
**	
**	Revision 1.33  2002/07/12 19:10:26  gsl
**	Global unique WL_ changes
**	
**	Revision 1.32  2002/07/12 17:17:07  gsl
**	Global unique WL_ changes
**	
**	Revision 1.31  2002/07/11 14:34:00  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.30  2002/07/10 21:06:39  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.29  2002/07/09 04:14:05  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.28  2002/06/25 18:40:49  gsl
**	fix include
**	
**	Revision 1.27  2002/06/25 18:18:42  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.26  2002/06/25 17:46:07  gsl
**	Remove WISPFILEXT as a global, now must go thru set/get routines
**	
**	Revision 1.25  2002/03/28 15:17:33  gsl
**	used define for copyright year
**	
**	Revision 1.24  2002-03-26 16:27:48-05  gsl
**	(C) 2002
**
**	Revision 1.23  2001-11-16 10:18:03-05  gsl
**	Change copyright date.
**
**	Revision 1.22  1998-09-09 11:38:16-04  gsl
**	fix the copyright
**	,'
**
**	Revision 1.21  1997-12-19 15:36:49-05  gsl
**	Fix copyright
**	change version to 2.13
**
**	Revision 1.20  1996-12-12 13:21:53-05  gsl
**	DTMI -> NeoMedia
**
**	Revision 1.19  1996-11-18 16:18:47-08  jockc
**	added call to vwang_title to set screen title
**
**	Revision 1.18  1996-11-11 12:04:06-08  gsl
**	Changed to use wexit() so proper cleanup is done
**
**	Revision 1.17  1996-09-03 15:24:08-07  gsl
**	drcs update
**
**
**
*/
