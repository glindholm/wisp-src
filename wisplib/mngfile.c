			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <signal.h>
#include <errno.h>

#ifdef unix
#include <pwd.h>
#include <grp.h>
extern char *sys_errlist[];
extern int   sys_nerr;
#endif

#ifdef MSDOS
#include <dos.h>
#include <io.h>
#endif

#include "idsistd.h"
#include "runtype.h"
#include "wcommon.h"
#include "vwang.h"
#include "scnfacs.h"
#include "wperson.h"
#include "wdefines.h"
#include "werrlog.h"
#define ROUTINE 91000

#define SCREENSIZE 	1924
#define ROW_FILESPEC	5

#ifdef unix
#define DDS_CHAR	'/'
#define DDS_STR		"/"
#endif
#ifdef MSDOS
#define DDS_CHAR	'\\'
#define DDS_STR		"\\"
#endif

char	*wfname();
char	*nextfile();

static int vol_cmp();									/* Compare routine for vol_ring		*/
static int dir_cmp();									/* Compare routine for dir_ring		*/

static int mng_entry_screen();
static int mng_dir_screen();
static int mng_file_screen();
static int build_entry_screen();
static int select_dir_entry();
static int add_file_adjust();
static int remove_file_adjust();
static int select_display();
static int select_edit();
static int select_scratch();
static int scratch_directory();
static int select_print();
static int select_rename();
static int build_dir_screen();
static int file_screen();
static int get_vol_choice();
static int get_dir_choice();
static int vol_add();
static int load_dir_ring();
static int disp_screen();
static int stattype();
static int typefile();
static int addpath();
static int modestring();
static int putmessage();
static int load_native_path();
static int is_blank();
static int wsb_init();
static int wsb_init_wcc();
static int save_oa_pos();
static int restore_oa_pos();
static int init_oa_pos();
static int wsb_put_text();
static int wsb_put_field();
static int wsb_get_field();
static int wsb_message();
static int wsb_build_filespec();
static int entry_screen();
static int dir_screen();
static int build_print_screen();
static int build_rename_screen();
static int return_file_screen();
static int build_file_screen();
static int do_protect();
static int get_filespec();
static int load_vol_ring();
static int dir_add();
static int libpath();
static int uppath();
static int wsb_init_oa();
static int wsb_put_tab();

static char *vol_ring;									/* The volume ring pointer		*/
static struct vol_struct								/* Struct for vol info			*/
{
	int	currow;									/* The cursor row tabstop		*/
	int	curcol;									/* The cursor col tabstop		*/
	char	wang[6];								/* The Wang style VOLUME		*/
	char	path[80];								/* The unix file path			*/
} vol_item;										/* A work item				*/

static char *dir_ring;									/* The volume ring pointer		*/
static struct dir_struct								/* Struct for dir info			*/
{
	int	currow;									/* The cursor row tabstop		*/
	int	curcol;									/* The cursor col tabstop		*/
	int	type;									/* The TYPE of the file			*/
	char	file[80];								/* The filename				*/
} dir_item;										/* A work item				*/

static char	wang_file[8];								/* Current Wang style FILENAME		*/
static char	wang_lib[8];								/* Current Wang style LIBRARY		*/
static char	wang_vol[6];								/* Current Wang style VOLUME		*/
static char	native_path[80];							/* Current native file path		*/

static int	native_path_loaded;							/* Is the native path loaded		*/

static int	modify_allowed;								/* Are modify operations allowed	*/
static int	display_allowed;							/* Is DISPLAY of file allowed		*/

#define STYLE_UNKNOWN	0
#define STYLE_FILE	1
#define	STYLE_LIB	2
#define STYLE_VOL	3
#define STYLE_PATH	4

static int	file_style;								/* The STYLE of file specification	*/

static char	screen_message[80];							/* Display this message on the screen	*/
static int	sound_alarm;								/* Sound the alarm 			*/

static int	g_protect;								/* Show PROTECT screen in file_screen	*/

static int	first_entry;								/* First ENTRY screen flag		*/
static int	first_dir_load;								/* First time dir load called		*/

#define ENTRY_SCREEN	1
#define DIR_SCREEN	2
#define FILE_SCREEN	3
#define EXIT_SCREEN	4

static int	next_screen;								/* Screen to display next		*/
static int	last_screen;								/* Last screen displayed		*/
static int	file_screen_return;							/* Return to from file_screen		*/
static int	print_screen_return;							/* Return to from print_screen		*/

#define TYPE_UNKNOWN	0
#define TYPE_NOTFOUND	1
#define TYPE_NOACCESS	2
#define TYPE_ERROR	3
#define TYPE_DIR	4
#define TYPE_REG	5
#define TYPE_FIFO	6
#define TYPE_CHR	7
#define TYPE_BLK	8
#define TYPE_SPECIAL	9
#define TYPE_HIDDEN	10

#define FILE_UNKNOWN	0
#define	FILE_EXEC	1
#define FILE_ACUOBJ	2
#define FILE_MFINT	3
#define FILE_CISAM	4
#define FILE_VISION2	5
#define FILE_VISION3	6
#define FILE_TEXT	7
#define FILE_DATA	8
#define FILE_FHISAMI	9
#define FILE_FHISAMD	10
#define FILE_MFGNT	11
#define FILE_BATCH	12
#define FILE_PROC	13
#define FILE_PROCOBJ	14

#define VOL_OPTION_NONE		0
#define VOL_OPTION_WANG		1
#define VOL_OPTION_NATIVE	2
#define VOL_OPTION_VOLUME	3
#define VOL_OPTION_BOTH		4

#define DIR_OPTION_NONE		0
#define DIR_OPTION_DIR		1
#define DIR_OPTION_FILE		2

#define PF_ENTER 	0
#define PF_RETURN	1
#define PF_FIRST	2
#define PF_LAST		3
#define PF_PREVIOUS	4
#define PF_NEXT		5
#define PF_RENAME	7
#define PF_SCRATCH	8
#define PF_PROTECT	9
#define PF_PRINT	10
#define PF_DISPLAY	11
#define PF_EDIT		12
#define PF_SCREEN	15

#define NOMOD		0
#define MODIFIABLE	1

/*
	mngfile		Manage Files and Libraries main entry point.
			This routine is called from the COMMAND PROCESSOR screen.

91001	%%MANAGEFILES-I-ENTRY Entry into Manage Files
91002	%%MANAGEFILES-F-RING [%s] [%s]
*/
int mngfile()
{
	uint4 dflags;
	int	stop;									/* Stop this routine			*/

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	vol_ring = 0;									/* Rings are undefined			*/
	dir_ring = 0;
	g_protect = 0;

	first_entry = 1;								/* This is first ENTRY SCREEN		*/
	first_dir_load = 1;

	wpload();									/* Load the DEFAULTS struct		*/

	get_defs(DEFAULTS_FLAGS,(char*)&dflags);					/* Get the defaults flags		*/
	modify_allowed = (dflags & HELP_CHANGE_FILES_LIBS);				/* Is modify operations allowed		*/
	display_allowed = (dflags & HELP_DISPLAY);					/* Is DISPLAY allowed			*/

	get_defs(DEFAULTS_IV,wang_vol);							/* Default to INVOL INLIB		*/
	get_defs(DEFAULTS_IL,wang_lib);
	memset(wang_file,' ',8);

	native_path_loaded = 0;

	next_screen = ENTRY_SCREEN;							/* First screen is ENTRY screen		*/
	file_style = STYLE_LIB;								/* Assume VOL:LIB file style		*/

	stop = 0;
	while(!stop)									/* Loop until STOP			*/
	{
		switch(next_screen)							/* Display the NEXT SCREEN		*/
		{
		case ENTRY_SCREEN:
			stop = mng_entry_screen();
			break;

		case DIR_SCREEN: 
			stop = mng_dir_screen();
			break;

		case FILE_SCREEN: 
			stop = mng_file_screen();
			break;

		case EXIT_SCREEN:
		default:
			stop = 1;
			break;
		}
	}

	if (vol_ring) ring_close(vol_ring);						/* Free up the ring structures		*/
	if (dir_ring) ring_close(dir_ring);

	return(0);
}

/*
	mng_entry_screen		The entry screen to display on entry.
*/
static int mng_entry_screen()
{
	int	rc;

	file_screen_return = ENTRY_SCREEN;						/* Return here from FILE_SCREEN		*/

	if (0==memcmp(wang_vol,"      ",6))						/* If no VOLUME then use invol inlib	*/
	{
		get_defs(DEFAULTS_IV,wang_vol);
		get_defs(DEFAULTS_IL,wang_lib);
	}

	if (!vol_ring)									/* If undefined then load vol_ring	*/
	{
		if (rc = load_vol_ring()) return(rc);
	}

	while(next_screen == ENTRY_SCREEN)
	{
		if (rc = entry_screen()) return(rc);					/* Display ENTRY_SCREEN			*/
	}
	return(0);
}

static int mng_dir_screen()
{
	int	rc;

	while(next_screen == DIR_SCREEN)
	{
		if (rc = dir_screen()) return(rc);					/* Display DIR_SCREEN			*/
	}
	return(0);
}

static int mng_file_screen()
{
	int	rc;

	while(next_screen == FILE_SCREEN)
	{
		if (rc = file_screen()) return(rc);					/* Display FILE_SCREEN			*/
	}
	return(0);
}

/*
	entry_screen		Display the ENTRY_SCREEN
*/
static int entry_screen()
{
	static	int	start_item;
	static	char	volcurpos[2];
	char	wsb[SCREENSIZE];
	int	pfkey;
	int	item_cnt,end_item;
	char	pf_list[80];
	int	rc;
	int	option;

	ring_count(vol_ring,&item_cnt);							/* Get the item count from vol_ring	*/

	if (first_entry)
	{
		first_entry = 0;
		start_item = 0;								/* Start at the first item		*/
		init_oa_pos(volcurpos);							/* Save the cursor position in O-A	*/
	}

	for(;;)
	{
		if (rc = build_entry_screen(wsb,item_cnt,start_item,&end_item,pf_list)) return(rc);	/* Build wsb 		*/
		restore_oa_pos(wsb,volcurpos);						/* Restore the cursor position in O-A	*/
		pfkey = disp_screen(wsb,pf_list);					/* Display and Read the screen		*/
		save_oa_pos(wsb,volcurpos);						/* Save the cursor position in O-A	*/
		last_screen = ENTRY_SCREEN;						/* Last screen display is ENTRY_SCREEN	*/

		switch(pfkey)								/* Take action based on key pressed	*/
		{
		case PF_ENTER: 
			memset(wang_vol,' ',6);						/* Clear the selections			*/
			memset(wang_lib,' ',8);
			memset(wang_file,' ',8);
			native_path[0] = '\0';
			native_path_loaded = 0;

			if (rc=get_vol_choice(wsb,item_cnt,start_item,end_item,&option)) return(rc);	/* Get ENTER choice	*/
			switch(option)
			{
			case VOL_OPTION_NONE:	
				putmessage("Please choose an option");			/* Nothing specified			*/
				break;

			case VOL_OPTION_BOTH:	
				putmessage("Choose only ONE option!");			/* Multiple actions requested		*/
				break;

			case VOL_OPTION_WANG:						/* Wang style selection			*/
				if (is_blank(wang_file,8)) 
				{
					next_screen = DIR_SCREEN;	
					if (is_blank(wang_lib,8))
					{
						file_style = STYLE_VOL;			/* Volume selected (lib & file blank)	*/
						wlgtrans( wang_vol, native_path );	/* Load vol with translation.		*/
						native_path_loaded = 1;
					}
					else
					{
						file_style = STYLE_LIB;			/* Library selected (file is blank)	*/
					}
				}
				else
				{
					next_screen = FILE_SCREEN;			/* File selected			*/
					file_style = STYLE_FILE;
				}
				return(0);
				break;

			case VOL_OPTION_NATIVE:						/* Native file path selected		*/
				next_screen = DIR_SCREEN;
				return(0);
				break;

			case VOL_OPTION_VOLUME:						/* VOLUME tabstop selected		*/
				next_screen = DIR_SCREEN;
				return(0);
				break;
			}
			break;

		case PF_RETURN: 
			next_screen = EXIT_SCREEN;					/* (1) EXIT file manager		*/
			return(0);
			break;

		case PF_FIRST:	
			start_item = 0;							/* (2) FIRST				*/
			break;

		case PF_LAST: 
			start_item = item_cnt-20;					/* (3) LAST				*/
			break;

		case PF_PREVIOUS: 
			start_item -= 20;						/* (4) PREV				*/
			if (start_item < 0) start_item=0;
			break;

		case PF_NEXT: 
			start_item = end_item;						/* (5) NEXT				*/
			break;

		case PF_SCREEN:
			wsh_progprnt(0);
			putmessage("Screen printed");
			break;
		default: /* HELP */
			next_screen = EXIT_SCREEN;
			return(0);
			break;
		}
	}
}

/*
	build_entry_screen	Build the screen buffer (wsb) for ENTRY_SCREEN based on the vol_ring entries.
*/
static int build_entry_screen(wsb,item_cnt,start_item,end_item,pf_list)
char	wsb[SCREENSIZE];								/* Screen buffer			*/
int4	item_cnt;									/* Number of VOLUME items in vol_ring	*/
int4	start_item;									/* Starting position in vol_ring	*/
int4	*end_item;									/* last+1 item displayed on screen	*/
char	*pf_list;									/* vwang pfkey list			*/
{
	char	vol[6], lib[8], file[8], filepath[80];					/* All are blank padded			*/
	int4	rc;
	int4	mode;
	int4	pos, row, col;
	char	buff[255];

	memcpy(vol,wang_vol,6);								/* Load local variable with file info	*/
	memcpy(lib,wang_lib,8);
	memcpy(file,wang_file,8);

	if (native_path_loaded)
	{
		loadpad(filepath,native_path,80);
	}
	else if (file[0] == ' ')
	{
		libpath(vol,lib,filepath);						/* Construct filepath from lib & vol	*/
	}
	else
	{
		mode = 0;
		wfname(&mode,vol,lib,file,filepath);
	}

	wsb_init(wsb);									/* Initialize wsb			*/
	wsb_put_text(wsb,1,30,0,"***  Manage Files  ***");
	wsb_message(wsb);								/* Load message				*/
	wsb_put_text(wsb,3,2,0,"Enter the file to manage:");

	wsb_build_filespec(wsb,vol,lib,file,filepath,STYLE_FILE,MODIFIABLE,ROW_FILESPEC);

	wsb_put_text(wsb,8,2,0,"Or select an entry from the list of known Volumes by positioning the cursor");
	wsb_put_text(wsb,9,2,0,"at the associated Tabstop:");
	wsb_put_text(wsb,22, 2,0,"(1) Exit");
	wsb_put_text(wsb,22,50,0,"(ENTER) To display a directory");
	wsb_put_text(wsb,24,50,0,"(15) Print Screen");

	strcpy(pf_list,"000115");
	if (start_item > 0)  								/* Add FIRST and PREV options		*/
	{
		wsb_put_text(wsb,23, 2,0,"(2) First");
		wsb_put_text(wsb,23,15,0,"(4) Previous");
		strcat(pf_list,"0204");
	}

#define VOLCOLWIDTH	40
#define VOLSTARTROW	11
#define VOLNUMROWS	10

	pos = start_item;
	for(col=2;col<=80-VOLCOLWIDTH+2;col+=VOLCOLWIDTH)				/* Load the VOLUMES in 2 columns	*/
	{
		for(row=VOLSTARTROW;row<(VOLSTARTROW+VOLNUMROWS);row++)
		{
			if (pos>=item_cnt)
			{
				pos = item_cnt;
				col = 80;						/* Break outer loop			*/
				break;							/* Break inner loop			*/
			}

			if (rc=ring_remove(vol_ring,pos,(char*)&vol_item))		/* Remove item from ring		*/
			{
				werrlog(ERRORCODE(2),"vol_ring 1",ring_error(rc),0,0,0,0,0,0);
				return(rc);
			}

			vol_item.currow = row;						/* update cursor tabstop position	*/
			vol_item.curcol = col;

			sprintf(buff,"%6.6s  %s",vol_item.wang,vol_item.path);		/* Construct screen display of VOLUME	*/
			if (strlen(buff) > VOLCOLWIDTH-4)
			{
				buff[VOLCOLWIDTH-4] = '+';				/* Truncate long names			*/
				buff[VOLCOLWIDTH-3] = '\0';
			}

			wsb_put_tab(wsb,row,col);					/* Add tabstop				*/
			wsb_put_text(wsb,row,col+2,0,buff);				/* Add item				*/

			if (rc=ring_add(vol_ring,0,(char*)&vol_item))			/* Replace item into ring		*/
			{
				werrlog(ERRORCODE(2),"vol_ring 2",ring_error(rc),0,0,0,0,0,0);
				return(rc);
			}

			pos++;								/* Next position			*/
		}
	}

	*end_item = pos;								/* Set end_item to last+1		*/
	if (item_cnt > *end_item) 
	{
		wsb_put_text(wsb,24, 2,0,"(3) Last");					/* Add LAST & NEXT options		*/
		wsb_put_text(wsb,24,15,0,"(5) Next");
		strcat(pf_list,"0305");
	}	
	strcat(pf_list,"X");								/* Terminated pfkey list for vwang	*/
	return(0);
}

/*
	dir_screen
*/
static int dir_screen()
{
	static	int start_item;
	static	char dircurpos[2];
	char	wsb[SCREENSIZE];
	int	pfkey;
	int	item_cnt,end_item;
	char	pf_list[80];
	int	rc;
	int	option;
	int	type;
	char	filepath[255];
	char	buff[255];
	int	i;
	int	force_load;

	force_load = 0;

	if (file_style == STYLE_FILE)							/* If FILE then goto FILE_SCREEN	*/
	{
		next_screen = FILE_SCREEN;
		return(0);
	}

	load_native_path();								/* Load the native file path		*/

	i = strlen(native_path);
	if (i == 0)									/* If NO path then go ENTRY_SCREEN	*/
	{
		putmessage("Invalid File Specification");
		next_screen = ENTRY_SCREEN;
		native_path_loaded = 0;
		return(0);
	}

#ifdef unix
	if (i>1 && native_path[i-1] == DDS_CHAR)					/* Remove trailing '/' character	*/
	{
		native_path[i-1] = '\0';
	}
#endif
#ifdef MSDOS
	if (((i>3 && native_path[1]==':') || (i>1 && native_path[1] != ':')) &&
		native_path[i-1] == DDS_CHAR)						/* Remove trailing '\' character	*/
	{
		native_path[i-1] = '\0';
	}
#endif

	type = stattype(native_path);							/* STAT the file to get TYPE		*/

	switch(type)
	{
	case TYPE_NOTFOUND:
		putmessage("File Not Found");
		next_screen = ENTRY_SCREEN;
		return(0);
		break;

	case TYPE_NOACCESS:
		putmessage("Access denied");
		next_screen = ENTRY_SCREEN;
		return(0);
		break;

	case TYPE_ERROR:
		putmessage("Error attempting to access file");
		next_screen = ENTRY_SCREEN;
		return(0);
		break;

	case TYPE_DIR:									/* Only continue if a DIRECTORY		*/
		break;

	case TYPE_REG:
	case TYPE_FIFO:
	case TYPE_CHR:
	case TYPE_BLK:
	case TYPE_SPECIAL:
	case TYPE_HIDDEN:
		next_screen = FILE_SCREEN;
		return(0);
		break;

	default:
		putmessage("Unknown File Type");
		next_screen = ENTRY_SCREEN;
		return(0);
		break;
	}

	if (last_screen == ENTRY_SCREEN ||						/* If coming from ENTRY_SCREEN or	*/
	    last_screen == DIR_SCREEN	   )						/*    coming from DIR_SCREEN then	*/
	{
		start_item = 0;								/* Reset the start position		*/
		init_oa_pos(dircurpos);							/* Init the cursor position		*/
	}

	while(next_screen == DIR_SCREEN)
	{
		if (rc = load_dir_ring(force_load)) return(rc);
		force_load = 0;
		ring_count(dir_ring,&item_cnt);

		if (item_cnt == 0)
		{
			putmessage("Unable to read Directory");
			next_screen = ENTRY_SCREEN;
			return(0);
		}

		if (rc = build_dir_screen(wsb,item_cnt,start_item,&end_item,pf_list)) return(rc);

		restore_oa_pos(wsb,dircurpos);						/* Use the stored cursor pos		*/

		pfkey = disp_screen(wsb,pf_list);					/* Display the screen			*/

		save_oa_pos(wsb,dircurpos);						/* Save the cursor position		*/

		last_screen = DIR_SCREEN;						/* Last screen display is DIR_SCREEN	*/
		file_screen_return = DIR_SCREEN;


		switch(pfkey)
		{
		case PF_ENTER: 		/* Enter */
		case PF_RENAME: 	/* Rename */
		case PF_SCRATCH: 	/* Scratch */
		case PF_PROTECT: 	/* Protect */
		case PF_PRINT: 		/* Print */
		case PF_DISPLAY:	/* Display */
		case PF_EDIT:		/* Edit */
			if (rc=get_dir_choice(wsb,item_cnt,start_item,end_item,&option,filepath)) return(rc);

			if (option == DIR_OPTION_NONE)
			{
				putmessage("You must position the cursor at a Tabstop!");
				break;
			}

			addpath(buff,native_path,filepath);				/* add the dir to the path		*/

			switch(pfkey)
			{
			case PF_ENTER: 		/* Enter */
				if (select_dir_entry(option,filepath)) return(0);
				break;
			case PF_RENAME: 	/* Rename */
				if (0==strcmp(filepath,".") || 0==strcmp(filepath,".."))
				{
					putmessage("You can not RENAME \".\" or \"..\"");
				}
				else if (0==add_file_adjust(filepath))
				{
					select_rename(option);
					remove_file_adjust();
				}
				break;
			case PF_SCRATCH: 	/* Scratch */
				if (0==strcmp(filepath,".") || 0==strcmp(filepath,".."))
				{
					putmessage("You can not SCRATCH \".\" or \"..\"");
				}
				else if (option == DIR_OPTION_FILE)
				{
					if (0 == select_scratch(buff,wsb))
					{
						force_load = 1;
					}
				}
				else
				{
					if (0 == scratch_directory(buff,wsb))
					{
						force_load = 1;
					}
				}
				break;
			case PF_PROTECT: 	/* Protect */
				if (0==add_file_adjust(filepath))
				{
					g_protect = 1;
					next_screen = FILE_SCREEN;
					file_screen();
					g_protect = 0;
					remove_file_adjust();
				}
				break;
			case PF_PRINT: 		/* Print */
				if (option == DIR_OPTION_FILE)
				{
					if (0==add_file_adjust(filepath))
					{
						select_print();
						remove_file_adjust();
					}
				}
				else
				{
					putmessage("You can not PRINT a directory");
				}
				break;
			case PF_DISPLAY:	/* Display */
				if (option == DIR_OPTION_FILE)
				{
					select_display(buff);
				}
				else
				{
					putmessage("You can not DISPLAY a directory");
				}
				break;
			case PF_EDIT:		/* Edit the file */
				if (option == DIR_OPTION_FILE)
				{
					select_edit(buff);
				}
				else
				{
					putmessage("You can not EDIT a directory");
				}
			}

			break;

		case PF_RETURN: 	/* Return to Volume Display */
			next_screen = ENTRY_SCREEN;
			return(0);
			break;

		case PF_FIRST:		/* First */
			start_item = 0;
			init_oa_pos(dircurpos);						/* Init the cursor position		*/
			break;

#ifdef unix
#define DIRSCREENITEMS	30
#endif
#ifdef MSDOS
#define DIRSCREENITEMS	40
#endif

		case PF_LAST: 		/* Last */
			start_item = item_cnt-DIRSCREENITEMS;
			init_oa_pos(dircurpos);						/* Init the cursor position		*/
			break;

		case PF_PREVIOUS: 	/* Previous */
			start_item -= DIRSCREENITEMS;
			if (start_item < 0) start_item=0;
			init_oa_pos(dircurpos);						/* Init the cursor position		*/
			break;

		case PF_NEXT: 		/* Next */
			start_item = end_item;
			init_oa_pos(dircurpos);						/* Init the cursor position		*/
			break;

		case PF_SCREEN:
			wsh_progprnt(0);
			putmessage("Screen printed");
			break;
		default: /* HELP */
			next_screen = EXIT_SCREEN;
			return(0);
			break;
		}

	}
}

static int select_dir_entry(option,filepath)
int	option;
char	*filepath;
{
	char	buff[256];

	if (option == DIR_OPTION_DIR)	/* Directory */
	{
		if (0 == strcmp(filepath,".."))						/* the dir selected is ".." - go UP	*/
		{
			switch (file_style)
			{
			case STYLE_VOL:							/* Leave the wang style names		*/
				file_style = STYLE_PATH;
				break;
			case STYLE_LIB:							/* Go from LIB style to VOL style	*/
				file_style = STYLE_VOL;
				memset(wang_lib,' ',8);					/* clear wang_lib			*/
				break;
			}
			uppath(native_path);						/* remove lowest directory from path	*/
			return(1);							/* return out of dir_screen()		*/
		}

		if (0 == strcmp(filepath,"."))						/* If '.' selected then display as FILE	*/
		{
			next_screen = FILE_SCREEN;
			return(1);							/* return out of dir_screen()		*/
		}

		addpath(buff,native_path,filepath);					/* add the dir to the path		*/
		if (!fexists(buff))
		{
			putmessage("Directory Not Found");
		}
		else if (!fcanread(buff))
		{
			putmessage("Unable to read Directory");
		}
		else
		{
			switch (file_style)
			{
			case STYLE_VOL:							/* Go from VOL style to LIB style	*/
				if (strlen(filepath) <= 8)
				{
					file_style = STYLE_LIB;
					loadpad(wang_lib,filepath,8);			/* updated wang_lib			*/
					upper_mem(wang_lib,8);				/* shift to uppercase			*/
				}
				else
				{
					file_style = STYLE_PATH;			/* Leaving wang style file names	*/
				}
				break;
			case STYLE_LIB:		
				file_style = STYLE_PATH;				/* Leaving wang style file names	*/
				break;
			}
			strcpy(native_path,buff);
			return(1);							/* return out of dir_screen()		*/
		}
	}
	else /* DIR_OPTION_FILE */							/* Selected a FILE			*/
	{
		if (0==add_file_adjust(filepath))
		{
			next_screen = FILE_SCREEN;					/* next screen is the FILE screen	*/
			return(1);							/* return out of dir_screen()		*/
		}
	}
	return(0);									/* return to dir_screen()		*/
}

static int add_file_adjust(filepath)
char	*filepath;
{
	char	buff[256];

	addpath(buff,native_path,filepath);					/* Add file to the full path		*/
	if (fexists(buff))
	{
		switch (file_style)
		{
		case STYLE_VOL:							/* leaving wang style file names	*/
			file_style = STYLE_PATH;
			break;
		case STYLE_LIB:							/* go from LIB style to FILE style	*/
			if (strlen(filepath) <= 8)
			{
				file_style = STYLE_FILE;
				loadpad(wang_file,filepath,8);			/* updated wang_file			*/
				upper_mem(wang_file,8);				/* shift to uppercase			*/
			}
			else
			{
				file_style = STYLE_PATH;			/* Leaving wang style file names	*/
			}
			break;
		}
		strcpy(native_path,buff);
		return(0);
	}
	else
	{
		putmessage("File Not Found");
		return(1);
	}
}

static remove_file_adjust()
{
	uppath(native_path);

	if (file_style == STYLE_FILE)
	{
		file_style = STYLE_LIB;
		memset(wang_file,' ',8);
	}
}

static select_display(file)
char	*file;
{
	if (!fexists(file))
	{
		putmessage("File Not Found");
	}
	else if (!fcanread(file))
	{
		putmessage("Read Access Denied");
	}
	else
	{
		vdisplay(file,255);							/* Display the file			*/
	}
}

static select_edit(file)
char	*file;
{
	char	*ptr;
	char	cmd[256], mess[256];
	int	rc;

	if (!fexists(file))
	{
		putmessage("File Not Found");
	}
	else if (!fcanread(file))
	{
		putmessage("Read Access Denied");
	}
	else
	{
		if (!(ptr = getenv("WEDITOR")))
		{
			ptr = "vsedit";
		}
		sprintf(cmd,"%s %s",ptr,file);
		WISPSHUT();
		rc = wsystem(cmd);
		WISPSYNC();
		if (rc)
		{
			sprintf(mess,"EDIT terminated: \"%s\" [rc=%d]",cmd,rc);
			putmessage(mess);
		}
	}
}

static int select_scratch(file,wsb)
char	*file;
char	*wsb;
{
	int	pfkey;
	char	buff[256];

	wsb_init_wcc(wsb);								/* init the WCC in the order area	*/
	memset(&wsb[4+80],' ',80);							/* Clear the message area		*/
	memset(&wsb[4+19*80],' ',5*80);							/* Clear the pfkeys area		*/
	wsb_put_field(wsb,22, 2,0,"Press (ENTER) to SCRATCH the file, or (1) to return",BOLD_TEXT);

	pfkey = disp_screen(wsb,"0001X");

	switch(pfkey)
	{
	case 0:
		if ( unlink(file) )
		{
			switch(errno)
			{
			case ENOENT:	putmessage("File not found");		break;
			case EACCES:	putmessage("Access Denied");		break;
			case EBUSY:	putmessage("File is busy");		break;
			default:	
				sprintf(buff,"Unable to SCRATCH file [errno=%d]",errno);
				putmessage(buff);
				break;
			}
			return(1);							/* Scratch failed			*/
		}
		else
		{
			return(0);							/* Scratch succeeded			*/
		}
		break;
	case 1:
		return(1);								/* Scratch failed			*/
		break;
	default: /* HELP */
		next_screen = EXIT_SCREEN;
		return(1);								/* Scratch failed			*/
		break;
	}
}

static int scratch_directory(file,wsb)
char	*file;
char	*wsb;
{
	int	pfkey;

	wsb_init_wcc(wsb);								/* init the WCC in the order area	*/
	memset(&wsb[4+80],' ',80);							/* Clear the message area		*/
	memset(&wsb[4+19*80],' ',5*80);							/* Clear the pfkeys area		*/
	wsb_put_field(wsb,22, 2,0,"Press (ENTER) to SCRATCH the DIRECTORY, or (1) to return",BOLD_TEXT);

	pfkey = disp_screen(wsb,"0001X");

	switch(pfkey)
	{
	case 0:
		if ( rmdir(file) )
		{
			switch(errno)
			{
			case EINVAL:	putmessage("Can not SCRATCH current directory");	break;
			case EEXIST:	putmessage("Directory is not empty");			break;
			case ENOENT:	putmessage("Directory not found");			break;
			case EACCES:	putmessage("Access Denied");				break;
			case EBUSY:	putmessage("Directory is busy");			break;
			default:
				if (errno < sys_nerr)
				{
					putmessage(sys_errlist[errno]);
				}
				else
				{
					putmessage("Unable to SCRATCH the directory");
				}
				break;
			}
			return(1);
		}
		else
		{
			return(0);
		}
		break;
	case 1:
		return(1);
		break;
	default: /* HELP */
		next_screen = EXIT_SCREEN;
		return(1);
		break;
	}
}

static int select_print()
{
	char	wsb[SCREENSIZE];
	char	pf_list[80];
	int	pfkey;
	char	print_mode[10], print_class[10], scratch_after[10], copies_char[10], form_char[10], disp[10];
	int4	tlong;
	int4	copies_num, form_num;
	int4	mod;
	int4	rc;

	print_mode[0] = 'S';
	get_defs(DEFAULTS_PC,print_class);
	memcpy(scratch_after,"NO ",3);
	memcpy(copies_char,"    1",5);
	get_defs(DEFAULTS_FN,(char*)&tlong);
	sprintf(form_char,"%03ld",tlong);
	
	for(;;)
	{
		build_print_screen(wsb,pf_list,print_mode,print_class,scratch_after,copies_char,form_char);
		pfkey = disp_screen(wsb,pf_list);					/* Display and Read the screen		*/

		switch(pfkey)
		{
		case PF_ENTER:
			wsb_get_field(wsb, 9,48,1, print_mode);
			wsb_get_field(wsb,11,48,1, print_class);
			wsb_get_field(wsb,14,48,3, scratch_after);
			wsb_get_field(wsb,16,48,5, copies_char);
			wsb_get_field(wsb,17,48,3, form_char);

			if (print_mode[0] != 'S' && 
			    print_mode[0] != 'H'    )
			{
				putmessage("Invalid PRINT MODE");
				break;
			}

			disp[0] = 'D';
			if (0 == memcmp(scratch_after,"NO ",3))
			{
				disp[1] = 'S';
			}
			else if (0 == memcmp(scratch_after,"YES",3))
			{
				disp[1] = 'X';
			}
			else
			{
				putmessage("Invalid SCRATCH AFTER");
				break;
			}

			copies_char[5] = '\0';
			if (1 != sscanf(copies_char,"%d",&copies_num))
			{
				putmessage("Invalid COPIES");
				break;
			}

			form_char[3] = '\0';
			if (1 != sscanf(form_char,"%d",&form_num))
			{
				putmessage("Invalid FORM #");
				break;
			}

			wprint(native_path,print_mode[0],disp,copies_num,print_class[0],form_num,&rc);
			switch(rc)
			{
			case  0:	putmessage("File printed");  return(0); break;
			case 20:	putmessage("File Not Found");		break;
			case 28:	putmessage("Access Denied");		break;
			case 40:	putmessage("Invalid argument");		break;
			default:	putmessage("Unable to print file");	break;
			}
			break;
		case PF_RETURN:
			return(0);
			break;
		case PF_SCREEN:
			wsh_progprnt(0);
			putmessage("Screen printed");
			break;
		default: /* HELP */
			next_screen = EXIT_SCREEN;
			return(0);
			break;
		}
	}
}

static int build_print_screen(wsb,pf_list,print_mode,print_class,scratch_after,copies,print_form)
char	wsb[SCREENSIZE];
char	*pf_list;
char	*print_mode;
char	*print_class;
char	*scratch_after;
char	*copies;
char	*print_form;
{
	char	vol[6], lib[8], file[8], filepath[80];
	char	buff[255];

	memcpy(vol,wang_vol,6);
	memcpy(lib,wang_lib,8);
	memcpy(file,wang_file,8);
	loadpad(filepath,native_path,sizeof(filepath));


	wsb_init(wsb);
	wsb_put_text(wsb,1,30,0,"***  Print File  ***");
	wsb_message(wsb);

	wsb_build_filespec(wsb,vol,lib,file,filepath,file_style,NOMOD,ROW_FILESPEC);

	wsb_put_field(wsb,20,2,0,"Press (ENTER) to PRINT, or Select:",BOLD_UNDER_TEXT);
	wsb_put_text(wsb,22, 2,0,"(1) Return to Display");
	wsb_put_text(wsb,24,50,0,"(15) Print Screen");
	strcpy(pf_list,"000115X");

	wsb_put_text(wsb, 9,30,0,"PRINT MODE      = +     (S=SPOOL,H=HOLD)");
	wsb_put_text(wsb,11,30,0,"PRINT CLASS     = +     (A-Z)");
	wsb_put_text(wsb,13,30,0,"Scratch File after Printing?");
	wsb_put_text(wsb,14,30,0,"Scratch         = +++");
	wsb_put_text(wsb,16,30,0,"COPIES          = +++++ (1-32767)");
	wsb_put_text(wsb,17,30,0,"FORM #          = +++   (0-254)");

	wsb_put_field(wsb, 9,48,1, print_mode,		UPCASE_FIELD);
	wsb_put_field(wsb,11,48,1, print_class,		UPCASE_FIELD);
	wsb_put_field(wsb,14,48,3, scratch_after,	UPCASE_FIELD);
	wsb_put_field(wsb,16,48,5, copies,		UPCASE_FIELD);
	wsb_put_field(wsb,17,48,3, print_form,		UPCASE_FIELD);

	return(0);
}

static int select_rename(file_type)
int4	file_type;
{
	char	wsb[SCREENSIZE];
	char	pf_list[80];
	int4	pfkey;
	char	vol[6],lib[8],file[8],filepath[80];
	char	path[256],buff[256];
	int4	option;
	int4	mode;
	char	*ptr;
	int4	dest_spec;
	int4	rc;
	char	pbuf[80];
	char	ebuf[8][80];
	int4	ecnt;
	FILE	*fp;
	void	(*save_sig)();

	ecnt = 0;

	memcpy(vol,wang_vol,6);
	memcpy(lib,wang_lib,8);
	memcpy(file,wang_file,8);
	loadpad(filepath,native_path,sizeof(filepath));

	switch(file_style)
	{
	case STYLE_PATH:
		memset(vol, ' ',6);
	case STYLE_VOL:
		memset(lib, ' ',8);
	case STYLE_LIB:
		memset(file,' ',8);
	}

	for(;;)
	{
		dest_spec = 0;
		build_rename_screen(wsb,pf_list,ecnt,ebuf,vol,lib,file,filepath);
		pfkey = disp_screen(wsb,pf_list);

		switch(pfkey)
		{
		case PF_RETURN:	/* Return */
			return(0);
			break;
		case PF_ENTER:	/* RENAME */
		case 2:		/* RENAME */
		case 3: 	/* COPY */
		case 5: 	/* LINK */
		case 6: 	/* SYMBOLIC LINK */
			get_filespec(wsb,vol,lib,file,filepath,&option,10);
			switch (option)
			{
			case VOL_OPTION_NONE:
				putmessage("No Destination specified");
				break;
			case VOL_OPTION_BOTH:
				putmessage("Specify only ONE Destination");
				break;
			case VOL_OPTION_WANG:
				if (file[0] == ' ') file[0] = '.';
				if (lib[0]  == ' ') lib[0]  = '.';
				if (vol[0]  == ' ') vol[0]  = '.';
				mode = 0;
				wfname(&mode,vol,lib,file,filepath);
				unloadpad(path,filepath,79);
				dest_spec = 1;
				break;
			case VOL_OPTION_NATIVE:
				unloadpad(path,filepath,79);
				dest_spec = 1;
				break;
			}
			break;
		case PF_SCREEN:	/* PRINT SCREEN */
			wsh_progprnt(0);
			putmessage("Screen printed");
			break;
		default: /* HELP */
			next_screen = EXIT_SCREEN;
			return(0);
			break;
		}

		if (dest_spec)
		{
			if (0==strcmp(native_path,path))
			{
				putmessage("Destination file must be different!");
			}
			else if (fexists(path))
			{
				putmessage("Destination file already exists!");
			}
			else
			{
				makepath( path );				/* Ensure the target path exists		*/

#ifdef unix
				switch(pfkey)
				{
				case PF_ENTER:
				case 2:
					sprintf(buff,"mv -f %s %s 2>&1",native_path,path);
					break;
				case 3:
					sprintf(buff,"cp %s %s 2>&1",native_path,path);
					break;
				case 5:
					sprintf(buff,"ln -f %s %s 2>&1",native_path,path);
					break;
				case 6:
					sprintf(buff,"ln -s %s %s 2>&1",native_path,path);
					break;
				}

				save_sig = signal(SIGCLD,SIG_DFL);
				fp = popen(buff,"r");
				if (!fp)
				{
					signal(SIGCLD,save_sig);
					putmessage("Command cannot be executed");
				}
				else
				{
					ecnt=0;
					while( ecnt<8 && fgets(pbuf,80,fp) != NULL )
					{
						pbuf[strlen(pbuf)-1] = '\0';		/* remove the newline			*/
						strcpy(ebuf[ecnt],pbuf);
						ecnt++;
					}

					pclose(fp);
					signal(SIGCLD,save_sig);
					return(0);
				}
#endif /* unix */
#ifdef MSDOS
				switch(pfkey)
				{
				case PF_ENTER:
				case 2:
					if (0 == rename(native_path,path))
					{
						return(0);
					}

					switch(errno)
					{
					case ENOENT:
					case EACCES:
						if (file_type == DIR_OPTION_DIR)
						{
							sprintf(buff,"Cannot MOVE directory, only RENAME! [errno=%d]",errno);
						}
						else
						{
							sprintf(buff,"Unable to create new file! [errno=%d]",errno);
						}
						break;
					case EXDEV:
						sprintf(buff,"Cannot RENAME to a different device! [errno=%d]",errno);
						break;
					default:
						sprintf(buff,"RENAME failed [errno=%d]",errno);
						break;
					}
					putmessage(buff);
					break;
				}
#endif /* MSDOS */
			}
		}
	}
}

static int build_rename_screen(wsb,pf_list,ecnt,ebuf,dvol,dlib,dfile,dfilepath)
char	wsb[SCREENSIZE];
char	*pf_list;
int	ecnt;
char	ebuf[8][80];
char	dvol[6], dlib[8], dfile[8], dfilepath[80];
{
	char	vol[6], lib[8], file[8], filepath[80];
	char	buff[255];
	int	i;

	memcpy(vol,wang_vol,6);
	memcpy(lib,wang_lib,8);
	memcpy(file,wang_file,8);
	loadpad(filepath,native_path,sizeof(filepath));

	wsb_init(wsb);
#ifdef unix
	wsb_put_text(wsb,1,26,0,"***  Rename/Copy File  ***");
#endif
#ifdef MSDOS
	wsb_put_text(wsb,1,28,0,"***  Rename File  ***");
#endif
	wsb_message(wsb);

	wsb_build_filespec(wsb,vol,lib,file,filepath,file_style,NOMOD,ROW_FILESPEC);

	wsb_put_field(wsb,8,2,0,"Enter the Destination file specification below",BOLD_TEXT);

	wsb_build_filespec(wsb,dvol,dlib,dfile,dfilepath,STYLE_FILE,MODIFIABLE,10);
	
	wsb_put_field(wsb,20,2,0,"Press (ENTER) to RENAME or Select:",BOLD_UNDER_TEXT);
	strcpy(pf_list,"00");
	wsb_put_text(wsb,22, 2,0,"(1) Return");		strcat(pf_list,"01");
#ifdef unix
	wsb_put_text(wsb,23, 2,0,"(2) RENAME");		strcat(pf_list,"02");
	wsb_put_text(wsb,24, 2,0,"(3) COPY");		strcat(pf_list,"03");
	wsb_put_text(wsb,23,20,0,"(5) LINK");		strcat(pf_list,"05");
	wsb_put_text(wsb,24,20,0,"(6) Symbolic LINK");	strcat(pf_list,"06");
#endif
	wsb_put_text(wsb,24,50,0,"(15) Print Screen");	strcat(pf_list,"15");

	strcat(pf_list,"X");
/*	strcpy(pf_list,"010203050615X"); */

	for (i=0;i<ecnt;i++)
	{
		wsb_put_text(wsb,13+i,2,0,ebuf[i]);
	}

	return(0);
}

static int build_dir_screen(wsb,item_cnt,start_item,end_item,pf_list)
char	wsb[SCREENSIZE];
int4	item_cnt;
int4	start_item;
int4	*end_item;
char	*pf_list;
{
	char	vol[6], lib[8], file[8], filepath[80];
	int4	mode;
	int	rc;
	int4	pos, row, col;
	char	buff[255];

	memcpy(vol,wang_vol,6);
	memcpy(lib,wang_lib,8);
	memcpy(file,wang_file,8);
	loadpad(filepath,native_path,sizeof(filepath));

	wsb_init(wsb);
	wsb_put_text(wsb,1,30,0,"***  Directories  ***");
	wsb_message(wsb);

	sprintf(buff,"Directory contains %d files.",item_cnt);
	wsb_put_text(wsb,3,25,0,buff);

	wsb_build_filespec(wsb,vol,lib,file,filepath,file_style,NOMOD,ROW_FILESPEC);

	wsb_put_field(wsb,19,2,0,"Position cursor to a File or Directory and press (ENTER) or Select:",BOLD_UNDER_TEXT);
	strcpy(pf_list,"00");
		wsb_put_text(wsb,21, 2,0,"(1) Return to Volume Display");	strcat(pf_list,"01");

	if (modify_allowed)
	{
		wsb_put_text(wsb,21,35,0," (7) Rename");			strcat(pf_list,"07");
		wsb_put_text(wsb,22,35,0," (8) Scratch");			strcat(pf_list,"08");
#ifdef unix
		wsb_put_text(wsb,23,35,0," (9) Protect");			strcat(pf_list,"09");
#endif
	}

		wsb_put_text(wsb,24,35,0,"(10) Print");				strcat(pf_list,"10");

	if (display_allowed)
	{
		wsb_put_text(wsb,21,50,0,"(11) Display");			strcat(pf_list,"11");
	}
	if (modify_allowed)
	{
		wsb_put_text(wsb,22,50,0,"(12) Edit");				strcat(pf_list,"12");
	}
		wsb_put_text(wsb,24,50,0,"(15) Print Screen");			strcat(pf_list,"15");

/*	strcpy(pf_list,"000107080910111215"); */
	if (start_item > 0)  
	{
		wsb_put_text(wsb,23, 2,0,"(2) First");
		wsb_put_text(wsb,23,15,0,"(4) Previous");
		strcat(pf_list,"0204");
	}

#ifdef unix
#define DIRCOLWIDTH	26
#endif
#ifdef MSDOS
#define DIRCOLWIDTH	19
#endif
#define DIRSTARTROW	8
#define DIRNUMROWS	10

	pos = start_item;
	for(col=2;col<=80-DIRCOLWIDTH;col+=DIRCOLWIDTH)
	{
		for(row=DIRSTARTROW;row<(DIRSTARTROW+DIRNUMROWS);row++)
		{
			if (pos>=item_cnt)
			{
				pos = item_cnt;
				col = 80;						/* Break outer loop			*/
				break;							/* Break inner loop			*/
			}

			if (rc=ring_remove(dir_ring,pos,(char*)&dir_item))
			{
				werrlog(ERRORCODE(2),"dir_ring 1",ring_error(rc),0,0,0,0,0,0);
				return(rc);
			}

			dir_item.currow = row;
			dir_item.curcol = col;

			strcpy(filepath,dir_item.file);
			switch(dir_item.type)
			{
			case TYPE_DIR:
				strcat(filepath,DDS_STR);
				break;
			case TYPE_REG:
				break;
			default:
				strcat(filepath," @");
				break;
			}

			if (0==strcmp(dir_item.file,".."))
			{
				strcat(filepath," (UP)");
			}

			if (strlen(filepath) > DIRCOLWIDTH-4)
			{
				filepath[DIRCOLWIDTH-4] = '+';
				filepath[DIRCOLWIDTH-3] = '\0';
			}

			wsb_put_tab(wsb,dir_item.currow,dir_item.curcol);
			wsb_put_text(wsb,dir_item.currow,col+2,0,filepath);

			if (rc=ring_add(dir_ring,0,(char*)&dir_item))
			{
				werrlog(ERRORCODE(2),"dir_ring 2",ring_error(rc),0,0,0,0,0,0);
				return(rc);
			}

			pos++;
		}
	}

	*end_item = pos;
	if (item_cnt > *end_item) 
	{
		wsb_put_text(wsb,24, 2,0,"(3) Last");
		wsb_put_text(wsb,24,15,0,"(5) Next");
		strcat(pf_list,"0305");
	}
	strcat(pf_list,"X");
	return(0);
}

/*
	file_screen
*/
static int file_screen()
{
	char	wsb[SCREENSIZE];
	int	pfkey;
	char	pf_list[80];
	char	filepath[255];
	int	rc, i;
	int	option;
	int4	mode;
	int	protect;
	int	filetype;

	load_native_path();								/* load the native file path		*/

	i = strlen(native_path);
	if (i == 0)									/* If NO path then go ENTRY_SCREEN	*/
	{
		putmessage("Invalid File Specification");
		next_screen = ENTRY_SCREEN;
		native_path_loaded = 0;
		return(0);
	}

	protect = g_protect;

	while(next_screen == FILE_SCREEN)
	{
		if (!fexists(native_path))
		{
			putmessage("File Not Found");
			return_file_screen();
			return(0);
		}

		filetype = stattype(native_path);

		if (rc = build_file_screen(wsb,pf_list,protect,filetype)) return(rc);
		pfkey = disp_screen(wsb,pf_list);
		last_screen = FILE_SCREEN;						/* Last screen display is FILE_SCREEN	*/

		switch(pfkey)
		{
#ifdef unix
		case PF_ENTER:
			if (protect)
			{
				do_protect(wsb);
				protect = 0;
			}
			break;
#endif
		case PF_RETURN:
			if (protect)
			{
				protect = 0;
			}
			else
			{
				return_file_screen();
				return(0);
			}
			break;
		case PF_RENAME:
			select_rename((filetype == TYPE_DIR)?DIR_OPTION_FILE:DIR_OPTION_FILE);
			break;
		case PF_SCRATCH:
			if (select_scratch(native_path,wsb) == 0)
			{
				return_file_screen();
				return(0);
			}
			break;
		case PF_PROTECT:
			protect = 1;
			break;
		case PF_PRINT:
			select_print();
			break;
		case PF_DISPLAY:
			select_display(native_path);
			break;
		case PF_EDIT:
			select_edit(native_path);
			break;
		case PF_SCREEN:
			wsh_progprnt(0);
			putmessage("Screen printed");
			break;
		default: /* HELP */
			next_screen = EXIT_SCREEN;
			return(0);
			break;
		}

		if (g_protect) 
		{
			next_screen = DIR_SCREEN;
			return(0);
		}

	}

	return(0);
}

static int return_file_screen()
{
	next_screen = file_screen_return;
	if ( TYPE_DIR != stattype(native_path) )
	{
		if (next_screen == DIR_SCREEN)
		{
			remove_file_adjust();
		}
	}
	return(0);
}

static int build_file_screen(wsb,pf_list,protect,filetype)
char	wsb[SCREENSIZE];
char	*pf_list;
int	protect;
int	filetype;
{
	char	vol[6], lib[8], file[8], filepath[80];
	int	rc,col,row;
	char	buff[80];
	char	type[80];
	struct	stat	filestat;
#ifdef unix
	struct	passwd	*pw;
	struct	group	*gr;
	char	owner[80], group[80], protection[20];
#endif

	memcpy(vol,wang_vol,6);
	memcpy(lib,wang_lib,8);
	memcpy(file,wang_file,8);
	loadpad(filepath,native_path,sizeof(filepath));

	wsb_init(wsb);
	wsb_put_text(wsb,1,28,0,"***  File Attributes  ***");
	wsb_message(wsb);

	wsb_build_filespec(wsb,vol,lib,file,filepath,file_style,NOMOD,ROW_FILESPEC);

	if (protect)
	{
		wsb_put_field(wsb,22, 2,0,"Press (ENTER) to PROTECT the file, or (1) to return",BOLD_TEXT);
		strcpy(pf_list,"0001X");
	}
	else
	{
		if (file_screen_return == ENTRY_SCREEN)
		{
			wsb_put_text(wsb,21, 2,0,"(1) Return to Volume Display");
		}
		else
		{
			file_screen_return = DIR_SCREEN;
			wsb_put_text(wsb,21, 2,0,"(1) Return to Directory Display");
		}
		strcpy(pf_list,"01");

		if (modify_allowed)
		{
			wsb_put_text(wsb,21,35,0," (7) Rename");	strcat(pf_list,"07");
			wsb_put_text(wsb,22,35,0," (8) Scratch");	strcat(pf_list,"08");
#ifdef unix
			wsb_put_text(wsb,23,35,0," (9) Protect");	strcat(pf_list,"09");
#endif
		}
		if (TYPE_REG == filetype)
		{
			wsb_put_text(wsb,24,35,0,"(10) Print");		strcat(pf_list,"10");
		}
		if (TYPE_REG == filetype && display_allowed)
		{
			wsb_put_text(wsb,21,50,0,"(11) Display");	strcat(pf_list,"11");
		}
		if (TYPE_REG == filetype && modify_allowed)
		{
			wsb_put_text(wsb,22,50,0,"(12) Edit");		strcat(pf_list,"12");
		}

		wsb_put_text(wsb,24,50,0,"(15) Print Screen");	strcat(pf_list,"15");
		strcat(pf_list,"X");
/*		strcpy(pf_list,"0107080910111215X"); */
	}

	switch (filetype)
	{
	case TYPE_DIR:
		strcpy(type,"DIRECTORY");
		break;
	case TYPE_FIFO:
		strcpy(type,"FIFO SPECIAL");
		break;
	case TYPE_CHR:
		strcpy(type,"CHARACTER SPECIAL");
		break;
	case TYPE_BLK:
		strcpy(type,"BLOCK SPECIAL");
		break;
	case TYPE_SPECIAL:
		strcpy(type,"MS-DOS SPECIAL");
		break;
	case TYPE_HIDDEN:
		strcpy(type,"HIDDEN SPECIAL");
		break;
	case TYPE_REG:
		switch(typefile(native_path))
		{
		case FILE_EXEC:
			strcpy(type,"EXECUTABLE");
			break;
		case FILE_ACUOBJ:
			strcpy(type,"ACUCOBOL OBJECT CODE");
			break;
		case FILE_MFINT:
			strcpy(type,"MICRO FOCUS INTERMEDIATE CODE");
			break;
		case FILE_CISAM:
			strcpy(type,"CISAM INDEX");
			break;
		case FILE_VISION2:
			strcpy(type,"VISION 2 (ACUCOBOL)");
			break;
		case FILE_VISION3:
			strcpy(type,"VISION 3 (ACUCOBOL)");
			break;
		case FILE_TEXT:
			strcpy(type,"TEXT");
			break;
		case FILE_DATA:
			strcpy(type,"DATA");
			break;
		case FILE_FHISAMI:
			strcpy(type,"MICRO FOCUS FH ISAM (INDEX)");
			break;
		case FILE_FHISAMD:
			strcpy(type,"MICRO FOCUS FH ISAM (DATA)");
			break;
		case FILE_MFGNT:
			strcpy(type,"MICRO FOCUS GNT CODE");
			break;
		case FILE_BATCH:
			strcpy(type,"MSDOS BATCH FILE");
			break;
		case FILE_PROC:
			strcpy(type,"PROCEDURE SOURCE");
			break;
		case FILE_PROCOBJ:
			strcpy(type,"PROCEDURE OBJECT");
			break;
		case FILE_UNKNOWN:
		default:
			strcpy(type,"ORDINARY");
			break;
		}
		break;

	case TYPE_UNKNOWN:
	default:
		strcpy(type,"UNKNOWN");
		break;
	}

	memset(&filestat,0,sizeof(filestat));

	wsb_put_text(wsb, 8,2,0,"File type is");
	wsb_put_field(wsb,8,20,0,type,BOLD_TEXT);

#ifdef MSDOS
	/*
	**	There is a bug the stat() routine in Intel C Code Builder version 1.0e.
	**	If you try to stat() a directory on C: or B: it attempts to read the wrong drive.
	**	To work around this don't do a stat() on a directory until fixed.
	*/
	if (filetype != TYPE_DIR)
#endif
	{
		if (stat(native_path,&filestat))
		{
			switch(errno)
			{
			case ENOENT:
				putmessage("File Not Found");
				break;
			case EACCES:
				putmessage("Access denied");
				break;
			default:
				putmessage("Error attempting to STAT the file");
				break;
			}
		}

		sprintf(buff,"%ld",filestat.st_size);
		wsb_put_text(wsb, 9,2,0,"Size in bytes is");
		wsb_put_field(wsb,9,20,0,buff,BOLD_TEXT);

		strcpy(buff,ctime(&filestat.st_mtime));
		buff[strlen(buff)-1] = '\0';
		wsb_put_text(wsb,11,2,0,"Last Modified");
		wsb_put_field(wsb,11,20,0,buff,BOLD_TEXT);
	}

#ifdef unix
	if (pw = getpwuid(filestat.st_uid))
	{
		strcpy(owner,pw->pw_name);
	}
	else
	{
		strcpy(owner,"(unknown)");
	}

	if (gr = getgrgid(filestat.st_gid))
	{
		strcpy(group,gr->gr_name);
	}
	else
	{
		strcpy(group,"(unknown)");
	}

	modestring(&filestat,protection);

	sprintf(buff,"%s %s (%d) %s (%d)",protection,owner,filestat.st_uid,group,filestat.st_gid);
	wsb_put_text(wsb,13,2,0,"Protection is");
	wsb_put_field(wsb,13,20,0,buff,BOLD_TEXT);

	col =  2;
	row = 15;
	wsb_put_text(wsb,row+0,col,0,"                      Read   Write  Execute");
	wsb_put_text(wsb,row+1,col,0,"User  xxxxxxxxxxxxxxxx +       +       +   ");
	wsb_put_text(wsb,row+2,col,0,"Group xxxxxxxxxxxxxxxx +       +       +   ");
	wsb_put_text(wsb,row+3,col,0,"Other                  +       +       +   ");

	sprintf(buff,"%s (%d)               ",owner,filestat.st_uid);
	wsb_put_field(wsb,row+1,col+6,16,buff,BOLD_TEXT);

	sprintf(buff,"%s (%d)               ",group,filestat.st_gid);
	wsb_put_field(wsb,row+2,col+6,16,buff,BOLD_TEXT);

	wsb_put_field(wsb,row+1,col+23,1,(protection[1]=='r')?"X":" ",(protect)?UPCASE_FIELD:BOLD_TEXT);
	wsb_put_field(wsb,row+1,col+31,1,(protection[2]=='w')?"X":" ",(protect)?UPCASE_FIELD:BOLD_TEXT);
	wsb_put_field(wsb,row+1,col+39,1,(protection[3]=='x')?"X":" ",(protect)?UPCASE_FIELD:BOLD_TEXT);
	wsb_put_field(wsb,row+2,col+23,1,(protection[4]=='r')?"X":" ",(protect)?UPCASE_FIELD:BOLD_TEXT);
	wsb_put_field(wsb,row+2,col+31,1,(protection[5]=='w')?"X":" ",(protect)?UPCASE_FIELD:BOLD_TEXT);
	wsb_put_field(wsb,row+2,col+39,1,(protection[6]=='x')?"X":" ",(protect)?UPCASE_FIELD:BOLD_TEXT);
	wsb_put_field(wsb,row+3,col+23,1,(protection[7]=='r')?"X":" ",(protect)?UPCASE_FIELD:BOLD_TEXT);
	wsb_put_field(wsb,row+3,col+31,1,(protection[8]=='w')?"X":" ",(protect)?UPCASE_FIELD:BOLD_TEXT);
	wsb_put_field(wsb,row+3,col+39,1,(protection[9]=='x')?"X":" ",(protect)?UPCASE_FIELD:BOLD_TEXT);
#endif /* unix */

	return(0);
}

#ifdef unix
static do_protect(wsb)
{
	int	col,row,mode;
	char	ur,uw,ux,gr,gw,gx,or,ow,ox;

	row=15;
	col=2;
	wsb_get_field(wsb,row+1,col+23,1,&ur);
	wsb_get_field(wsb,row+1,col+31,1,&uw);
	wsb_get_field(wsb,row+1,col+39,1,&ux);
	wsb_get_field(wsb,row+2,col+23,1,&gr);
	wsb_get_field(wsb,row+2,col+31,1,&gw);
	wsb_get_field(wsb,row+2,col+39,1,&gx);
	wsb_get_field(wsb,row+3,col+23,1,&or);
	wsb_get_field(wsb,row+3,col+31,1,&ow);
	wsb_get_field(wsb,row+3,col+39,1,&ox);

	mode = 0;
	if (ur != ' ') mode += 0400;
	if (uw != ' ') mode += 0200;
	if (ux != ' ') mode += 0100;
	if (gr != ' ') mode += 0040;
	if (gw != ' ') mode += 0020;
	if (gx != ' ') mode += 0010;
	if (or != ' ') mode += 0004;
	if (ow != ' ') mode += 0002;
	if (ox != ' ') mode += 0001;

	if (chmod(native_path,mode))
	{
		switch(errno)
		{
		case ENOENT:	putmessage("File Not Found");	break;
		case EACCES:	putmessage("Access Denied");	break;
		case EPERM:	putmessage("Not Owner of File");	break;
		default:	putmessage("Unable to change protections of file");
		}
	}
}
#endif /* unix */

/*
	get_vol_choice		Find out what was requested and set OPTION and file_style.

				option
				  0	Do nothing
				  1	Use vol:lib:file 
				  2	Use native path
				  3	Use selection from volume list
*/
static int get_vol_choice(wsb,item_cnt,start_item,end_item,option)
char	wsb[SCREENSIZE];
int	item_cnt;
int	start_item;
int	end_item;
int	*option;
{
	int	row, col, pos, rc;
	char	vol[6],lib[8],file[8],path[80];

	*option = VOL_OPTION_NONE;
	file_style = STYLE_UNKNOWN;

	col = (int)wsb[2];
	row = (int)wsb[3];

	if ( row >=VOLSTARTROW  && row <(VOLSTARTROW+VOLNUMROWS) )
	{
		for(pos=start_item; pos<end_item; pos++)
		{
			if (rc=ring_get(vol_ring,pos,(char*)&vol_item))
			{
				werrlog(ERRORCODE(2),"vol_ring 3",ring_error(rc),0,0,0,0,0,0);
				return(rc);
			}

			if ( vol_item.currow == row && vol_item.curcol == col )
			{
				*option = VOL_OPTION_VOLUME;
				if (vol_item.wang[0] == '(' ||				/* Don't set if not real volume		*/
				    vol_item.wang[0] == '.'   )
				{
					file_style = STYLE_PATH;
				}
				else
				{
					memcpy(wang_vol,vol_item.wang,6);
					file_style = STYLE_VOL;
				}
			
				strcpy(native_path,vol_item.path);
				native_path_loaded = 1;
				return(0);
			}
		}
	} 

	get_filespec(wsb,vol,lib,file,path,option,ROW_FILESPEC);

	memcpy(wang_vol,vol,6);
	memcpy(wang_lib,lib,8);
	memcpy(wang_file,file,8);
	unloadpad(native_path,path,79);
	native_path_loaded = 1;

	if (*option == VOL_OPTION_NATIVE)
	{
		file_style = STYLE_PATH;
	}
	else if (*option == VOL_OPTION_WANG)
	{
		native_path_loaded = 0;
	}

	return(0);
}

static	get_filespec(wsb,vol,lib,file,path,option,startrow)
char	wsb[SCREENSIZE];
char	vol[6],lib[8],file[8],path[80];
int	*option;
int	startrow;
{
	int	modv,modl,modf,modp;
	int	blank_wang, blank_native;

	modv=wsb_get_field(wsb,startrow,11,6,vol);
	modl=wsb_get_field(wsb,startrow,29,8,lib);
	modf=wsb_get_field(wsb,startrow,50,8,file);
	modp=wsb_get_field(wsb,startrow+1,2,79,path);

	leftjust(vol,6);
	leftjust(lib,8);
	leftjust(file,8);
	leftjust(path,79);

/*
	NATIVE		WANG		RESULT
	=====		=====		======
	blank		blank		error
	blank		xxx		Wang
	xxx		blank		Native
	nomod		nomod		Wang
	mod		mod		error
	mod		nomod		Native
	nomod		mod		Wang
*/

	blank_wang = 0;
	blank_native = 0;

	if (vol[0] == ' ' && lib[0] == ' ' && file[0] == ' ')
	{
		blank_wang = 1;
	}

	if (path[0] == ' ')
	{
		blank_native = 1;
	}

	if ( blank_wang && blank_native)
	{
		*option = VOL_OPTION_NONE;
	}
	else if (blank_wang)
	{
		*option = VOL_OPTION_NATIVE;
	}
	else if (blank_native)
	{
		*option = VOL_OPTION_WANG;
	}
	else if (modp && (modv || modl || modf))					/* If choose both then redisplay	*/
	{
		*option = VOL_OPTION_BOTH;
	}
	else if (modv || modl || modf)							/* Choose the vol:lib:file		*/
	{
		*option = VOL_OPTION_WANG;
	}
	else if (modp)									/* Choose the native path		*/
	{
		*option = VOL_OPTION_NATIVE;
	}
	else										/* Default to vol:lib:file		*/
	{
		*option = VOL_OPTION_WANG;
	}
}

/*
	get_dir_choice		Find out what file was selected, return it's position and set option.

				Option
				  0	No file was selected
				  1	directory selected
				  2	file selected
			
*/
static int get_dir_choice(wsb,item_cnt,start_item,end_item,option,filename)
char	wsb[SCREENSIZE];
int	item_cnt;
int	start_item;
int	end_item;
int	*option;
char	*filename;
{
	int	row, col, pos, rc;

	*option = DIR_OPTION_NONE;

	col = (int)wsb[2];
	row = (int)wsb[3];

	if ( row >=DIRSTARTROW  && row < (DIRSTARTROW+10) )
	{
		for(pos=start_item; pos<end_item; pos++)
		{
			if (rc=ring_get(dir_ring,pos,(char*)&dir_item))
			{
				werrlog(ERRORCODE(2),"dir_ring 3",ring_error(rc),0,0,0,0,0,0);
				return(rc);
			}

			if ( dir_item.currow == row && dir_item.curcol == col )
			{
				if (dir_item.type == TYPE_DIR)	*option = DIR_OPTION_DIR;
				else				*option = DIR_OPTION_FILE;
				strcpy(filename,dir_item.file);
				return(0);
			}
		}
	} 
	return(0);
}


/*==============================================================================================================================*/
/*	vol_ring routines													*/
/*==============================================================================================================================*/

/*
	load_vol_ring	Load the vol_ring with the list of volumes and there translations.
*/
static int load_vol_ring()
{
	logical_id	*logical_ptr;
	int	rc;

	if (!vol_ring)
	{
		if (rc = ring_open(&vol_ring, sizeof(vol_item),10,10,vol_cmp,1))
		{
			werrlog(ERRORCODE(2),"vol_ring 4",ring_error(rc),0,0,0,0,0,0);
			return(rc);
		}
	}

	vol_item.currow = 0;
	vol_item.curcol = 0;

	memcpy(vol_item.wang,".     ",6);
	getcwd(vol_item.path,80);
	if (rc = vol_add()) return(rc);

	memcpy(vol_item.wang,"(HOME)",6);
	strcpy(vol_item.path,getenv("HOME"));
	if (rc = vol_add()) return(rc);

	memcpy(vol_item.wang,"(ROOT)",6);
#ifdef unix
	strcpy(vol_item.path,DDS_STR);
#endif
#ifdef MSDOS
	strcpy(vol_item.path,"C:\\");
#endif
	if (rc = vol_add()) return(rc);

	logical_ptr = get_logical_list();

	while(logical_ptr)
	{
		memset(vol_item.wang,' ',6);
		memcpy(vol_item.wang,logical_ptr->logical,strlen(logical_ptr->logical));
		strcpy(vol_item.path,logical_ptr->translate);
		if (0!=memcmp(vol_item.wang,".     ",6) &&				/* Don't load "." volume		*/
		    0!=memcmp(vol_item.wang,"      ",6)   )				/* Don't load blank volume		*/
		{
			if (rc = vol_add()) return(rc);
		}
		logical_ptr = (logical_id *)logical_ptr->next;
	}

	return(0);
}

/*
	vol_add		Add vol_item to vol_ring and report error messages.
*/
static int vol_add()
{
	int	rc;

	if (rc = ring_add(vol_ring,0,(char*)&vol_item))
	{
		werrlog(ERRORCODE(2),"vol_ring 5",ring_error(rc),0,0,0,0,0,0);
		return(rc);
	}
	return(0);
}

/*
	vol_cmp		Volume compare:  This is the ring comparison routine.
*/
static int vol_cmp(i1,i2)
struct	vol_struct *i1, *i2;
{
	return(memcmp(i1->wang,i2->wang,6));
}

/*==============================================================================================================================*/
/*	dir_ring routines													*/
/*==============================================================================================================================*/

/*
	load_dir_ring	Load the dir_ring with the list of files and directories
*/
static int load_dir_ring(force_load)
int	force_load;
{
	int	rc;
	int	len;
	char	filepath[80];
	char	*ptr;
	char	*context;
	int	cnt;

#ifdef unix
	/*
	**	MSDOS stat() routine has bug try to stat() a directory.
	**	Also stat.st_ino is not reliable on DOS for a directory.
	*/

	static struct	stat	old_stat, new_stat;

	if (first_dir_load)
	{
		memset(&old_stat,0,sizeof(old_stat));
		first_dir_load = 0;
	}

	if (stat(native_path,&new_stat))
	{
		putmessage("Unable to STAT the Directory");
		return(0);
	}

	if ( !force_load &&
	    old_stat.st_ino   == new_stat.st_ino &&				/* if directory is already loaded and 		*/
	    old_stat.st_dev   == new_stat.st_dev    )				/* has not been modified then don't reload	*/
	{
		if (old_stat.st_mtime == new_stat.st_mtime)
		{
			return(0);
		}
	}
	memcpy(&old_stat,&new_stat,sizeof(old_stat));
#endif /* unix */

	if (dir_ring)
	{
		if (rc = ring_close(dir_ring))
		{
			werrlog(ERRORCODE(2),"dir_ring 4",ring_error(rc),0,0,0,0,0,0);
			return(rc);
		}
		dir_ring = 0;
	}

	if (!dir_ring)
	{
		if (rc = ring_open(&dir_ring, sizeof(dir_item),10,10,dir_cmp,1))
		{
			werrlog(ERRORCODE(2),"dir_ring 5",ring_error(rc),0,0,0,0,0,0);
			return(rc);
		}
	}

	dir_item.currow = 0;
	dir_item.curcol = 0;
	cnt = 0;

	context = NULL;
	while( ptr = nextfile(native_path,&context) )
	{
		strcpy(dir_item.file,ptr);
		strcpy(filepath,native_path);
		len = strlen(filepath);
		if (len>0 && filepath[len-1] != DDS_CHAR)			/* If no trailing DDS then add it		*/
		{
			strcat(filepath,DDS_STR);
		}
		strcat(filepath,dir_item.file);
		dir_item.type = stattype(filepath);
		if (rc = dir_add())
		{
			nextfile(NULL,&context);
			return(rc);
		}
		cnt++;
	}
	nextfile(NULL,&context);

	return(0);
}

/*
	dir_add		Add dir_item to dir_ring and report error messages.
*/
static int dir_add()
{
	int	rc;

	if (rc = ring_add(dir_ring,0,(char*)&dir_item))
	{
		werrlog(ERRORCODE(2),"dir_ring 6",ring_error(rc),0,0,0,0,0,0);
		return(rc);
	}
	return(0);
}

/*
	dir_cmp		Directory compare:  This is the ring comparison routine.
*/
static int dir_cmp(i1,i2)
struct	dir_struct *i1, *i2;
{
	return(strcmp(i1->file,i2->file));
}

/*==============================================================================================================================*/
/*	General routines													*/
/*==============================================================================================================================*/

/*
	disp_screen	Display and read a screen and return the pfkey
*/
static int disp_screen(wsb,pfkey_list)
char	wsb[SCREENSIZE];
char	*pfkey_list;
{
	char	function, lines;
	char	pfkey[2], status[2];

	function = DISPLAY_AND_READ_ALTERED;
	lines = 24;

	if (sound_alarm)
	{
		wsb[1] |= 0x40;
		sound_alarm = 0;
	}

	vwang((unsigned char*)&function,(unsigned char*)wsb,(unsigned char*)&lines,
	      (unsigned char*)pfkey_list,(unsigned char*)pfkey,(unsigned char*)status);

	screen_message[0] = '\0';							/* Clear error message			*/

	return( (int)((pfkey[0]-'0')*10 + (pfkey[1]-'0')));
}

/*
	libpath		Call wfname to get the library path.
			If lib is blank then just change to dot (.).
*/
static libpath(vol,lib,path)
char	vol[6], lib[8], path[80];
{
	char	l_file[8], l_lib[8], l_vol[6];
	int4	mode;
	int	blank_vol, blank_lib;

	blank_vol = 0;
	blank_lib = 0;

	memset(l_file,' ',8);
	memcpy(l_lib,lib,8);
	memcpy(l_vol,vol,6);

	if (is_blank(l_vol,6))
	{
		l_vol[0] = '.';
		blank_vol = 1;
	}

	if (is_blank(l_lib,8))
	{
		l_lib[0] = '.';
		blank_lib = 1;
	}

	if (blank_vol && blank_lib)
	{
		memset(path,' ',80);
#ifdef OLD
		path[0] = '.';
		path[1] = DDS_CHAR;
#endif
	}
	else if (blank_lib)
	{
		char	buff[80];

		memset(path,' ',80);
		wlgtrans(l_vol,buff);
		memcpy(path,buff,strlen(buff));
	}
	else
	{
		mode = IS_LIB;
		wfname(&mode,l_vol,l_lib,l_file,path);
	}
}

/*
	stattype	Determine the type of the file from STAT.
*/
static int stattype(path)
char	*path;
{
#ifdef unix
	int	rc;
	struct stat buf;

	if ( stat(path,&buf) )
	{
		switch(errno)
		{
		case ENOENT:
			rc = TYPE_NOTFOUND;
			break;
		case EACCES:
			rc = TYPE_NOACCESS;
			break;
		default:
			rc = TYPE_ERROR;
			break;
		}
		return(rc);
	}

	if      (S_ISDIR (buf.st_mode))  rc = TYPE_DIR;
	else if (S_ISREG (buf.st_mode))  rc = TYPE_REG;
	else if (S_ISFIFO(buf.st_mode))  rc = TYPE_FIFO;
	else if (S_ISCHR (buf.st_mode))  rc = TYPE_CHR;
	else if (S_ISBLK (buf.st_mode))  rc = TYPE_BLK;
	else			  	 rc = TYPE_UNKNOWN;

	return(rc);
#endif /* unix */
#ifdef MSDOS
	int	rc;
	unsigned attrib;

	if (0==_dos_getfileattr(path,&attrib))
	{
		if      (attrib & _A_SUBDIR) 	rc = TYPE_DIR;
		else if (attrib & _A_RDONLY) 	rc = TYPE_REG;
		else if (attrib & _A_VOLID)  	rc = TYPE_SPECIAL;
		else if (attrib & _A_HIDDEN) 	rc = TYPE_HIDDEN;
		else if (attrib & _A_SYSTEM) 	rc = TYPE_SPECIAL;
		else if (attrib & _A_ARCH) 	rc = TYPE_REG;
		else if (attrib == _A_NORMAL) 	rc = TYPE_REG;		/* Normal is 0x00 */
		else				rc = TYPE_UNKNOWN;
	}
	else
	{
		rc = TYPE_NOTFOUND;
	}
	return(rc);
#endif /* MSDOS */
}

/*
	typefile	Try to guess at the contents of the file.
*/
static int typefile(file)
char	*file;
{
	int	fh;
	unsigned char	buff[20];
	int	rc;
	int	i;

	fh = open(file,O_RDONLY,0);
	if ( fh == -1 )
	{
		return(FILE_UNKNOWN);
	}
	rc = read(fh,(char *)buff,6);
	close(fh);
	if (rc != 6)
	{
		return(FILE_UNKNOWN);
	}

	if ( (buff[0] == 0x10 && buff[1] == 0x12 && buff[2] == 0x14 && buff[3] == 0x16) ||
	     (buff[3] == 0x10 && buff[2] == 0x12 && buff[1] == 0x14 && buff[0] == 0x16)    )
	{
		if (buff[5] == 0x03) return(FILE_VISION3);
		else		     return(FILE_VISION2);
	}

	if ( buff[0] == 0xFE && buff[1] == 0x53 )
	{
		return(FILE_CISAM);
	}

	if ( buff[0] == 0x31 && buff[1] == 0xFE )
	{
		return(FILE_FHISAMI);
	}

	if ( buff[0] == 0x30 && buff[1] == 0x7E )
	{
		return(FILE_FHISAMD);
	}

	switch(runtype(file))
	{
#ifdef MSDOS
	case RUN_EXEC:		return(FILE_EXEC);
	case RUN_SHELL:		return(FILE_BATCH);
	case RUN_ACUCOBOL:	return(FILE_ACUOBJ);
#endif
	case RUN_MFINT:		return(FILE_MFINT);
	case RUN_MFGNT:		return(FILE_MFGNT);
	case RUN_PROC:		return(FILE_PROC);
	case RUN_PROCOBJ:	return(FILE_PROCOBJ);
	}

#ifdef unix
	switch(isexec(file))
	{
	case ISEXEC:
		return(FILE_EXEC);
		break;
	case ISACU:
		return(FILE_ACUOBJ);
		break;
	case ISMFINT:
		return(FILE_MFINT);
		break;
	}
#endif /* unix */

	fh = open(file,O_RDONLY,0);
	if ( fh == -1 )
	{
		return(FILE_UNKNOWN);
	}

	for(i=0;i<255;i++)
	{
		rc = read(fh,(char *)buff,1);
		if (rc != 1) break;

		if (buff[0] >= ' ' && buff[0] <= '~') continue;
		if (buff[0] == 0x0a ||
		    buff[0] == 0x0c ||
		    buff[0] == 0x0d ||
		    buff[0] == 0x09   ) continue;
		close(fh);
		return(FILE_DATA);
	}

	close(fh);
	return(FILE_TEXT);
}

/*
	uppath		This routine removes the lowest level entry from a file path ( up to "/").
			If no leading '/' then return the current directory.
*/
static uppath(path)
char	*path;
{
	int	i;

	for(i=strlen(path)-1; i>0; i--)
	{
		if ( path[i] == DDS_CHAR ) break;
	}

	if ( path[i] != DDS_CHAR )
	{
		getcwd(path,80);
	}
#ifdef unix
	else if ( i > 0 )
	{
		path[i] = (char)0;
	}
	else if ( i == 0 )
	{
		strcpy(path,DDS_STR);
	}
#endif /* unix */
#ifdef MSDOS
	else if ( i == 0 )			/*  "\xxx"	==>	"C:\"	*/
	{
		getcwd(path,80);
		path[3] = (char)0;
	}
	else if ( path[1] == ':' )		/* "C:xxx"	*/
	{
		if ( i == 2 )			/* "C:\xxx"	==>	"C:\"	*/
		{
			path[3] = (char)0;
		}
		else				/* "C:xxx\xxx"	==>	"C:xxx"	*/
		{
			path[i] = (char)0;
		}
	}
	else					/* "xxx\xxx"	==>	"xxx"	*/
	{
		path[i] = (char)0;
	}
#endif /* MSDOS */

	return(0);
}

/*
	addpath		Create a new path by adding a file to an path
*/
static addpath(newpath,oldpath,file)
char	*newpath, *oldpath, *file;
{
	if ( oldpath[0] )
	{
		strcpy(newpath,oldpath);
		if (newpath[strlen(newpath)-1] != DDS_CHAR)
		{
			strcat(newpath,DDS_STR);
		}
	}
	strcat(newpath,file);
}

#ifdef unix
/*
	modestring	This routine is given a stat struct and returns a "ls" style string i.e.  "-rwxr-x---"
*/
static modestring(filestat,mode)
struct	stat	*filestat;
char	*mode;
{
	strcpy(mode,"----------");

	if      ( S_ISDIR (filestat->st_mode) ) mode[0] = 'd';
	else if ( S_ISCHR (filestat->st_mode) ) mode[0] = 'c';
	else if ( S_ISBLK (filestat->st_mode) ) mode[0] = 'b';
	else if ( S_ISFIFO(filestat->st_mode) ) mode[0] = 'p';

	if (filestat->st_mode & S_IRUSR) mode[1] = 'r';
	if (filestat->st_mode & S_IWUSR) mode[2] = 'w';
	if (filestat->st_mode & S_IXUSR) mode[3] = 'x';

	if (filestat->st_mode & S_IRGRP) mode[4] = 'r';
	if (filestat->st_mode & S_IWGRP) mode[5] = 'w';
	if (filestat->st_mode & S_IXGRP) mode[6] = 'x';

	if (filestat->st_mode & S_IROTH) mode[7] = 'r';
	if (filestat->st_mode & S_IWOTH) mode[8] = 'w';
	if (filestat->st_mode & S_IXOTH) mode[9] = 'x';
}
#endif /* unix */

static putmessage(message)
char *message;
{
	strcpy(screen_message,message);
	sound_alarm = 1;
}

static load_native_path()
{
	char	buff[256];
	int4	mode;

	if (!native_path_loaded)
	{
		switch(file_style)
		{
		case STYLE_FILE:							/* Use wfname based on VOL:LIB:FILE	*/
			mode = 0;
			wfname(&mode,wang_vol,wang_lib,wang_file,buff);
			unloadpad(native_path,buff,79);
			break;
		case STYLE_LIB:								/* Use libpath based on VOL:LIB		*/
			libpath(wang_vol,wang_lib,buff);
			unloadpad(native_path,buff,79);
			break;
		case STYLE_VOL:								/* Use wlgtrans based on VOL		*/
			wlgtrans( wang_vol, native_path );				/* Load vol with translation.		*/
			break;
		case STYLE_PATH:							/* Assume it really is loaded		*/
			break;
		}
		native_path_loaded = 1;							/* Mark it as loaded			*/
	}
}

static int is_blank(data,len)
char	*data;
int	len;
{
	int	i;

	for(i=0;i<len;i++)
	{
		if (data[i] != ' ') return(0);
	}
	return(1);
}

/*==============================================================================================================================*/
/*	General "wsb" screen buffer routines.											*/
/*==============================================================================================================================*/

/*
	wsb_init	Initialize the screen buffer
*/
static wsb_init(wsb)
char	wsb[SCREENSIZE];
{
	memset(wsb,' ',SCREENSIZE);
	wsb_init_oa(wsb);
}

static wsb_init_oa(wsb)
char	wsb[SCREENSIZE];
{
	wsb[0] = (char)1;
	wsb_init_wcc(wsb);
	wsb[2] = (char)0;
	wsb[3] = (char)0;
}

static wsb_init_wcc(wsb)
char	wsb[SCREENSIZE];
{
	wsb[1] = (char)POSITION_CURSOR|UNLOCK_KEYBOARD;
}

/*
	save_oa_pos	Save the order area cursor position
*/
static int save_oa_pos(wsb,curpos)
char	wsb[SCREENSIZE];
char	curpos[2];
{
	memcpy(curpos,&wsb[2],2);
	return(0);
}
static int restore_oa_pos(wsb,curpos)
char	wsb[SCREENSIZE];
char	curpos[2];
{
	memcpy(&wsb[2],curpos,2);
	return(0);
}
static int init_oa_pos(curpos)
char	curpos[2];
{
	curpos[0] = (char)0;
	curpos[1] = (char)0;
	return(0);
}

/*
	wsb_offset	Calculate the offset into a 1924 byte screen buffer for a row and column (one based.)
*/
static int wsb_offset(row,col)
int	row,col;
{
	return( 4 + (row-1)*80 + (col-1) );
}

/*
	wsb_put_text	Put a character string into a 1924 byte screen buffer.
			Row and Col are one based.
*/
static wsb_put_text(wsb, row, col, len, text)
char 	wsb[SCREENSIZE]; 
int 	row, col, len; 
char 	*text;
{
	int	offset;

	offset = wsb_offset(row,col);
	if (len) memcpy(&wsb[offset],text,len);						/* Copy len characters.			*/
	else     memcpy(&wsb[offset],text,strlen(text));				/* Copy null terminated.		*/
}

/*
	wsb_get_text	Get a character string from a 1924 byte screen buffer.
			Row and Col are one based.
*/
static wsb_get_text(wsb, row, col, len, text)
char 	wsb[SCREENSIZE]; 
int 	row, col, len; 
char 	*text;
{
	int	offset;

	offset = wsb_offset(row,col);
	memcpy(text,&wsb[offset],len);							/* Copy len characters.			*/
}

/*
	wsb_put_field	Put a field into the screen buffer at the given location with the given fac.
			Row and Col are one based.
*/
static wsb_put_field(wsb, row, col, len, text, fac)
char 	wsb[SCREENSIZE]; 
int 	row, col, len; 
char 	*text;
char	fac;
{
	int	offset;

	wsb_put_text(wsb,row,col,len,text);

	offset = wsb_offset(row,col);
	wsb[offset-1] = fac;

	if (!len) len = strlen(text);
	offset +=len;

	if (col+len < 80) wsb[offset] = PLAIN_TEXT;
}

/*
	wsb_get_field	Get a field into the screen buffer at the given location and return true if modified.
			Row and Col are one based.
*/
static int wsb_get_field(wsb, row, col, len, text)
char 	wsb[SCREENSIZE]; 
int 	row, col, len; 
char 	*text;
{
	int	offset;

	wsb_get_text(wsb,row,col,len,text);

	offset = wsb_offset(row,col);
	return( (int)(wsb[offset-1] & 0x40) );
}

static wsb_put_tab(wsb,row,col)
char	wsb[SCREENSIZE];
int	row, col;
{
	wsb_put_field(wsb,row,col,1,"-",(char)NUMPROT_FIELD);
}

static wsb_message(wsb)
char	wsb[SCREENSIZE];
{
	wsb_put_field(wsb,2,2,0,screen_message,BOLD_TEXT);
}

static wsb_build_filespec(wsb,vol,lib,file,filepath,style,mod,startrow)
char	*wsb, *vol, *lib, *file, *filepath;
int	style, mod, startrow;
{
	switch(style)
	{
	case STYLE_PATH:
#ifdef unix
		wsb_put_text (wsb,startrow,2,0,"UNIX File Specification:");
#endif
#ifdef MSDOS
		wsb_put_text (wsb,startrow,2,0,"MS-DOS File Specification:");
#endif
		break;
	case STYLE_FILE:
		wsb_put_text (wsb,startrow,39,0,"FILENAME = ++++++++");
		wsb_put_field(wsb,startrow,50,8,file,(mod) ? UPCASE_FIELD:BOLD_TEXT);
	case STYLE_LIB:
		wsb_put_text (wsb,startrow,19,0,"LIBRARY = ++++++++");
		wsb_put_field(wsb,startrow,29,8,lib,(mod) ? UPCASE_FIELD:BOLD_TEXT);
	case STYLE_VOL:
		wsb_put_text (wsb,startrow, 2,0,"VOLUME = ++++++");
		wsb_put_field(wsb,startrow,11,6,vol,(mod) ? UPCASE_FIELD:BOLD_TEXT);
	}
	wsb_put_field(wsb,startrow+1,2,79,filepath,(mod) ? STANDARD_FIELD:BOLD_TEXT);
}


/*==============================================================================================================================*/
/*	Dummy MAIN routine for testing.												*/
/*==============================================================================================================================*/

#ifdef MAIN
#define EXT_FILEXT
#include "filext.h"
main()
{
	initglbs("MNGFILES");
	w_err_flag = 1+2+4+8;					/* enable file screen exceptions	*/
	mngfile();
	wexit(0);
}
#endif


