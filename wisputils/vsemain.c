/*----
Invoke as

vse	or	vse filename	or vse filename language
where language can be
-cobol -c -text -shell

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

1.	There should be some checking of the file at load up
time to verify that it seems to match the language type requested.
Particulary for COBOL where line numbers are needed. WANG does this
with a BADNUMS screen letting the user know that expected line numbers
were not found. At the moment it just loads up and does
automatic numbering if it hits any problems.

2.	At close time, the user may only Create if a new file was
created, or save over an old fiel if an old file was edited. It
should allow an old file to be edited, and new one created on output.

3.	The output screen does not look like the Wang screen.

4.	During insertion, when trying to insert between two lines
that are one digit apart, the Wang stops and tells you that the file
needs to be renumbered before further insertions can take place. IF
the user requests renumbering it is then done. THis editor just
automatically renumbers.

5.	Since the work space is creating by Mallocing, there should
be a message that space can not be allocated. This version just
aborts.

6.	Bunch of functions missing
	a.	Delete by text range as well a lline number
	b.	Copy block of text
	c.	Move Block of text
	d.	External Copy.
	e.	Set tabs, case and search style

7.	If this is used in WANG environment, it should issue
	a set of default default getparms that can be responded to
	by putparms to allow customizing the edit space.


------*/

#include <stdio.h>

#include "vseglb.h"
char *build_system_name();

static char error_message[81];
static int screen_error;

static char applname[9]="VSE     ";
char WISPRETURNCODE[4]="000";

main(argc,argv)
int argc;
char **argv;
{
	initglbs(applname);
	vse_init();
	process_args(argc,argv);
	if(screen_error)
		return;
	if(vse_sysname[0] > ' ')
		vsedit(vse_sysname,vse_gp_input_language);
	else
		vse();
	vexit();
}

process_args(argc,argv)
int argc;
char **argv;
{
	int argi;
	char *argstr;

	for(argi=1;argi < argc; ++argi)
		{
/* If anything was on the command line, than we are in native mode */
		vse_native = 1;
		argstr = argv[argi];
		if(*argstr == '-')
			{
			++argstr;
			if(vse_gp_input_language[0] > ' ')
				ignoring(argstr);
			else
				strcpy(vse_gp_input_language,argstr);
			}
		else
			{
			if(vse_sysname[0] > ' ')
				ignoring(argstr);
			else
				strcpy(vse_sysname,argstr);
			}
		}

/* Validate the choices */
/* If its nothing, make it COBOL */
	trunc(vse_gp_input_language);
	if(vse_gp_input_language[0] == 0)
		strcpy(vse_gp_input_language,COBOL_LANGUAGE);
	else
		vse_native = 1;
	strupr(vse_gp_input_language);
	untrunc(vse_gp_input_language,VSE_LANGUAGE_LEN);
	if(!(val_language()))
		{
		screen_error = 1;
		printf("%s\n",error_message);
		return;
		}
/* 
   If a file was named on the command line, then the first screen
   will be skipped. The file is either new or existing.
 */
	if(vse_sysname[0])
		{
		if(val_sysname_file_exists())
			;
		else
			vse_new_file = 1;
		}
	screen_error = 0;
}

static ignoring(str)
char *str;
{
	printf("Extra input <%s> ignored\n",str);
}

vse_init()
{
	init_gpint();
}

vse()
{
	for(;;)
		{
		screen_error = 0;
		vse_input(error_message);
		strcpy(error_message,"");
		if(vse_input_pick == 16)
			{
			break;
			}
		if( (vse_input_pick == 5) &&
		    (!vse_native)          )
			{
			build_system_name();
			}
			
		if(vse_input_pick == 0)
			{
			val_inputs();
			if(!screen_error)
				{
				vsedit(vse_sysname,vse_gp_input_language);
				if(vse_special_pick == 16)
					break;
				else
					init_input_fields();
				}
			}
		if(vse_input_pick == 5)
			{
			vse_native = vse_native?0L:1L;
			}
		}
}

char *build_system_name()
{
	long mode;
	char *wfname(),*eoname;

	mode = 0;

	if(isblank(vse_gp_input_file,VSE_FILENAME_LEN))
		{
		CLEAR_GLOBAL(vse_sysname);
		return(vse_sysname);
		}

	eoname = wfname(&mode,vse_gp_input_volume,vse_gp_input_library,
		vse_gp_input_file,vse_sysname);
	if(isblank(vse_gp_input_ext,VSE_EXT_LEN))
		return;
	*eoname = '.';
	++eoname;
	memcpy(eoname,vse_gp_input_ext,VSE_EXT_LEN);
	
}

static val_inputs()
{
	val_language();
	if(!screen_error)
		val_file();
}

static val_language()
{
	if(!(strcmp(vse_gp_input_language,COBOL_LANGUAGE)))
		return(1);
	if(!(strcmp(vse_gp_input_language,TEXT_LANGUAGE)))
		return(1);
	if(!(strcmp(vse_gp_input_language,C_LANGUAGE)))
		return(1);
	if(!(strcmp(vse_gp_input_language,SHELL_LANGUAGE)))
		return(1);
	screen_error = 1;
	strcpy(error_message,"Invalid Language selected");
	return(0);
}

val_file()
{
	if(vse_native)
		val_sysname();
	else
		val_vs_name();
}

static val_sysname()
{
	if(isblank(vse_sysname,VSE_SYSNAME_LEN))
		{
		vse_new_file = 1;
		return;
		}
	else
		vse_new_file = 0;

	val_sysname_file_exists();
}	
		
static val_vs_name()
{

	if(isblank(vse_gp_input_file,VSE_FILENAME_LEN))
		{
		vse_new_file = 1;
		return;
		}
	else
		vse_new_file = 0;

	build_system_name();
	val_sysname_file_exists();
}

val_sysname_file_exists()
{

	trunc(vse_sysname);
	if(!exists(vse_sysname))
		{
		screen_error = 1;
		strcpy(error_message,"File not Found");
		}
	untrunc(vse_sysname,VSE_SYSNAME_LEN);
	return(!screen_error);
}

init_input_fields()
{
	CLEAR_GLOBAL(vse_gp_input_file);
	CLEAR_GLOBAL(vse_sysname);
}

