static char copyright[]="Copyright (c) 1988-1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		mngfile.c
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
**
**	Purpose:	Emulate Wang Manage Files and Libraries
**
**	Routines:
**	mngfile()
*/

/*
**	Includes
*/

#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <signal.h>
#include <errno.h>
#include <string.h>

#ifdef unix
#include <unistd.h>
#include <pwd.h>
#include <grp.h>
extern char *sys_errlist[];
extern int   sys_nerr;
#endif

#ifdef WIN32
#include <direct.h>
#include <io.h>
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_TEXT
#define O_TEXT 0
#endif

#include "idsistd.h"
#include "runtype.h"
#include "wcommon.h"
#include "vwang.h"
#include "scnfacs.h"
#include "wperson.h"
#include "wdefines.h"
#include "werrlog.h"
#include "wfname.h"
#include "ring.h"
#include "wisplib.h"
#include "idsisubs.h"
#include "paths.h"
#include "wispcfg.h"
#include "screen.h"
#include "wsb.h"
#include "wfiledis.h"
#include "wfvision.h"
#include "wfcisam.h"
#include "assert.h"

/*
**	Structures and Defines
*/

#define ROUTINE 91000
/*
91001	%%MANAGEFILES-I-ENTRY Entry into Manage Files
91002	%%MANAGEFILES-F-RING [%s] [%s]
*/

#define ROW_FILESPEC	5

#define DDS_CHAR	DIR_SEPARATOR
#define DDS_STR		DIR_SEPARATOR_STR

#ifndef S_ISDIR
#define S_ISFIFO(m)	(((m)&(_S_IFMT)) == (_S_IFIFO))
#define S_ISDIR(m)	(((m)&(_S_IFMT)) == (_S_IFDIR))
#define S_ISCHR(m)	(((m)&(_S_IFMT)) == (_S_IFCHR))
#define S_ISBLK(m)	(((m)&(_S_IFMT)) == (_S_IFBLK))
#define S_ISREG(m)	(((m)&(_S_IFMT)) == (_S_IFREG))
#endif

#define PF_LIST_LEN	80

#define COL_VOL		11
#define COL_LIB		29
#define COL_FILE	50
#define COL_PATH	2
/*
**	Globals and Externals
*/

/*
**	Static data
*/


static char *vol_ring;									/* The volume ring pointer		*/
static struct vol_struct								/* Struct for vol info			*/
{
	int	currow;									/* The cursor row tabstop		*/
	int	curcol;									/* The cursor col tabstop		*/
	char	wang[SIZEOF_VOL];							/* The Wang style VOLUME		*/
	char	path[COB_FILEPATH_LEN];							/* The unix file path			*/
} vol_item;										/* A work item				*/

static char *dir_ring;									/* The volume ring pointer		*/
static struct dir_struct								/* Struct for dir info			*/
{
	int	currow;									/* The cursor row tabstop		*/
	int	curcol;									/* The cursor col tabstop		*/
	int	type;									/* The TYPE of the file			*/
	char	file[COB_FILEPATH_LEN];							/* The filename				*/
} dir_item;										/* A work item				*/

static char	wang_file[SIZEOF_FILE];							/* Current Wang style FILENAME		*/
static char	wang_lib[SIZEOF_LIB];							/* Current Wang style LIBRARY		*/
static char	wang_vol[SIZEOF_VOL];							/* Current Wang style VOLUME		*/
static char	native_path[WISP_FILEPATH_LEN + 1];					/* Current native file path		*/

static int	native_path_loaded;							/* Is the native path loaded		*/

static int	modify_allowed;								/* Are modify operations allowed	*/
static int	display_allowed;							/* Is DISPLAY of file allowed		*/

#define STYLE_UNKNOWN	0
#define STYLE_FILE	1
#define	STYLE_LIB	2
#define STYLE_VOL	3
#define STYLE_PATH	4

static int	file_style;								/* The STYLE of file specification	*/

static char	screen_message[MAX_SCREEN_FIELD_SIZE + 1];				/* Display this message on the screen	*/
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
#define FILE_VISION4D	15
#define FILE_VISION4I	16
#define FILE_SHELL	17

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
#define PF_DISPLAY_11	11
#define PF_EDIT_12	12
#define PF_PRTSCRN_15	15
#define PF_PRTSCRN_11	11

#define NOMOD		0
#define MODIFIABLE	1


/*
**	Function Prototypes
*/
extern int wsystem_interactive(const char *cmd);

static int vol_cmp();									/* Compare routine for vol_ring		*/
static int dir_cmp();									/* Compare routine for dir_ring		*/

static int mng_entry_screen(void);
static int entry_screen(void);
static int dnr_entry_screen(int item_cnt, int start_item, int* p_end_item, 
			    int* p_pfkey, int* p_currow, int* p_curcol, int* p_option);

static int mng_dir_screen(void);
static int mng_file_screen(void);

static int build_dir_screen(HWSB hWsb, int item_cnt, int start_item, int* p_end_item, char* pf_list);

static int select_dir_entry(int option, const char* filepath);
static int add_file_adjust(const char* filepath);
static void remove_file_adjust(void);
static void select_display(const char* file);
static void select_edit(const char* file);
static int select_scratch(const char* file, HWSB hWsb, int currow, int curcol);
static int scratch_file(const char *file);
static int scratch_dir(const char* file);
static int select_print(void);
static int select_rename(int file_type);
static int file_screen(void);
static int get_vol_choice(HWSB hWsb, int row, int col, int item_cnt, int start_item, int end_item, int* option);
static int get_dir_choice(int row, int col, int item_cnt, int start_item, int end_item, int* p_option, char* filename);
static int vol_add(void);
static int load_dir_ring(int force_load);
static int stattype(const char* path);
static int typefile(const char* file);
static void addpath(char* newpath, const char* oldpath, const char* file);
#ifdef unix
static modestring(struct stat *filestat, char *mode);
#endif
static void putmessage(const char* message);
static void set_native_path(const char* new_path);
static void clear_native_path(void);
static void load_native_path(void);
static int is_blank(const char* data, int len);
static int dir_screen(void);
static int dnr_print_screen(char* print_mode, char* print_class, char* scratch_after, char* copies, char* print_form);
static int build_rename_screen(HWSB hWsb, 
			       char* pf_list, 
			       int ecnt, 
			       char ebuf[8][80],
			       char dvol[SIZEOF_VOL], 
			       char dlib[SIZEOF_LIB], 
			       char dfile[SIZEOF_FILE], 
			       char dfilepath[COB_FILEPATH_LEN]);
static int return_file_screen(void);
static int build_file_screen(HWSB hWsb, char* pf_list, int protect, int filetype);
static change_protection(HWSB hWsb);
static void wsb_get_filespec(HWSB hWsb,
			     char vol[SIZEOF_VOL],
			     char lib[SIZEOF_LIB],
			     char file[SIZEOF_FILE],
			     char path[COB_FILEPATH_LEN],
			     int *option,
			     int startrow);
static int load_vol_ring(void);
static int dir_add(void);
static void libpath(char vol[SIZEOF_VOL], char lib[SIZEOF_LIB], char path[COB_FILEPATH_LEN]);
static int uppath(char* path);
static void clear_file_specs(void);

static void wsb_clearmessage(HWSB hWsb);
static void wsb_clearrow(HWSB hWsb, int row);

static void wsb_screen_message(HWSB hWsb);
static void wsb_build_filespec(HWSB hWsb, 
			       const char* vol, 
			       const char* lib, 
			       const char* file, 
			       const char* filepath, 
			       int style, 
			       int mod, 
			       int startrow);


/*
	mngfile		Manage Files and Libraries main entry point.
			This routine is called from the COMMAND PROCESSOR screen.

*/
int mngfile(void)
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
	modify_allowed = ((dflags & HELP_CHANGE_FILES_LIBS) ? TRUE : FALSE); 		/* Is modify operations allowed		*/
	display_allowed = ((dflags & HELP_DISPLAY) ? TRUE : FALSE);			/* Is DISPLAY allowed			*/

	clear_file_specs();
	
	get_defs(DEFAULTS_IV,wang_vol);							/* Default to INVOL INLIB		*/
	get_defs(DEFAULTS_IL,wang_lib);

	next_screen = ENTRY_SCREEN;							/* First screen is ENTRY screen		*/
	file_style = STYLE_LIB;								/* Assume VOL:LIB file style		*/

	screen_message[0] = '\0';

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
static int mng_entry_screen(void)
{
	int	rc;

	file_screen_return = ENTRY_SCREEN;						/* Return here from FILE_SCREEN		*/

	if (is_blank(wang_vol,SIZEOF_VOL))						/* If no VOLUME then use invol inlib	*/
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

static int mng_dir_screen(void)
{
	int	rc;

	while(next_screen == DIR_SCREEN)
	{
		if (rc = dir_screen()) return(rc);					/* Display DIR_SCREEN			*/
	}
	return(0);
}

static int mng_file_screen(void)
{
	int	rc;

	while(next_screen == FILE_SCREEN)
	{
		if (rc = file_screen()) return(rc);					/* Display FILE_SCREEN			*/
	}
	return(0);
}

#define VOLNUMCOLS	1
#define VOLCOLWIDTH	(WSB_COLS / VOLNUMCOLS)
#define VOLSTARTROW	11
#define VOLNUMROWS	10
#define VOLSCREENITEMS	(VOLNUMCOLS * VOLNUMROWS)
#define VOLTABPOS	2					/* Tabstop position within column */
#define VOLNAMEPOS	4					/* Wang volume position within column */
#define VOLPATHPOS	12					/* Path position within column */
#define VOLPATHWIDTH	(VOLCOLWIDTH - VOLPATHPOS)		/* Path width */

/*
	entry_screen		Display the ENTRY_SCREEN
*/
static int entry_screen(void)
{
	static	int	start_item;
	static	int	currow, curcol;
	int	pfkey;
	int	item_cnt,end_item;
	int	rc;
	int	option;
	int	done;
	
	ring_count(vol_ring,&item_cnt);							/* Get the item count from vol_ring	*/

	if (first_entry)
	{
		first_entry = 0;
		start_item = 0;								/* Start at the first item		*/
		currow = 0;
		curcol = 0;
	}

	rc = 0;
	done = 0;
	
	while(!done)
	{
		if (rc = dnr_entry_screen(item_cnt, start_item, &end_item, &pfkey, &currow, &curcol, &option))
		{
			done = 1;
			break;
		}

		last_screen = ENTRY_SCREEN;						/* Last screen display is ENTRY_SCREEN	*/

		if (pfkeys12())
		{
			if (PF_PRTSCRN_11 == pfkey)
			{
				pfkey = PF_PRTSCRN_15;
			}
		}

		switch(pfkey)								/* Take action based on key pressed	*/
		{
		case PF_ENTER: 

			switch(option)
			{
			case VOL_OPTION_NONE:	
				putmessage("Please choose an option");			/* Nothing specified			*/
				break;

			case VOL_OPTION_BOTH:	
				putmessage("Choose only ONE option!");			/* Multiple actions requested		*/
				break;

			case VOL_OPTION_WANG:						/* Wang style selection			*/
				if (is_blank(wang_file,SIZEOF_FILE)) 
				{
					next_screen = DIR_SCREEN;	
					if (is_blank(wang_lib,SIZEOF_LIB))
					{
						char	buff[MAX_TRANSLATE+1];
						
						file_style = STYLE_VOL;			/* Volume selected (lib & file blank)	*/
						wlgtrans( wang_vol, buff );		/* Load vol with translation.		*/
						set_native_path(buff);
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
				rc = 0;
				done = 1;
				break;

			case VOL_OPTION_NATIVE:						/* Native file path selected		*/
				next_screen = DIR_SCREEN;
				rc = 0;
				done = 1;
				break;

			case VOL_OPTION_VOLUME:						/* VOLUME tabstop selected		*/
				next_screen = DIR_SCREEN;
				rc = 0;
				done = 1;
				break;
			}
			break;

		case PF_RETURN: 
			next_screen = EXIT_SCREEN;					/* (1) EXIT file manager		*/
			rc = 0;
			done = 1;
			break;

		case PF_FIRST:	
			start_item = 0;							/* (2) FIRST				*/
			break;

		case PF_LAST: 
			start_item = item_cnt-VOLSCREENITEMS;				/* (3) LAST				*/
			if (start_item < 0) start_item=0;
			break;

		case PF_PREVIOUS: 
			start_item -= VOLSCREENITEMS;					/* (4) PREV				*/
			if (start_item < 0) start_item=0;
			break;

		case PF_NEXT: 
			start_item = end_item;						/* (5) NEXT				*/
			break;

		case PF_PRTSCRN_15:
			if (!nativescreens())
			{
				screen_print();
				putmessage("Screen printed");
			}
			break;

		default: /* HELP */
			next_screen = EXIT_SCREEN;
			rc = 0;
			done = 1;
			break;
		}
	}
	
	return(rc);
}

/*
	dnr_entry_screen	Build the screen buffer for ENTRY_SCREEN based on the vol_ring entries.
*/
static int dnr_entry_screen(int item_cnt, int start_item, int* p_end_item, 
			    int* p_pfkey, int* p_currow, int* p_curcol, int* p_option)
{
	HWSB	hWsb;
	char	pf_list[PF_LIST_LEN];
	char	vol[SIZEOF_VOL], lib[SIZEOF_LIB], file[SIZEOF_FILE], filepath[COB_FILEPATH_LEN];
	int	rc;
	int	pos, row, coloff;
	char	buff[255];
	int	volnamepos;

	if (nativescreens())
	{
		volnamepos = VOLNAMEPOS+1;
	}
	else
	{
		volnamepos = VOLNAMEPOS;
	}
	
	rc = 0;
	hWsb = wsb_new();

	memcpy(vol,wang_vol,SIZEOF_VOL);						/* Load local variable with file info	*/
	memcpy(lib,wang_lib,SIZEOF_LIB);
	memcpy(file,wang_file,SIZEOF_FILE);

	if (native_path_loaded)
	{
		cstr2cobx(filepath,native_path,sizeof(filepath));
	}
	else if (file[0] == ' ')
	{
		libpath(vol,lib,filepath);						/* Construct filepath from lib & vol	*/
	}
	else
	{
		int4	mode;

		mode = 0;
		wfname(&mode,vol,lib,file,filepath);
	}
	
	cobx2cstr(native_path, filepath, COB_FILEPATH_LEN);
	native_path_loaded = 1;

	wsb_add_text(hWsb, 1, 0, "***  Manage Files  ***");
	wsb_screen_message(hWsb);
	wsb_add_text(hWsb, 3, 2, "Enter the file to manage:");

	wsb_build_filespec(hWsb,vol,lib,file,filepath,STYLE_FILE,MODIFIABLE,ROW_FILESPEC);

	wsb_add_text(hWsb, 8, 2,"Or select an entry from the list of known Volumes by positioning the cursor");
	wsb_add_text(hWsb, 9, 2,"at the associated Tabstop:");
	wsb_add_text(hWsb,22, 2,"(1) Exit");
	wsb_add_text(hWsb,22,50,"(ENTER) To display a directory");
	strcpy(pf_list,"0001");

	if (nativescreens())
	{
		/* No print screen */
	}
	else if (pfkeys12())
	{
		wsb_add_text(hWsb,24,50,"(11) Print Screen");
		strcat(pf_list,"11");
	}
	else
	{
		wsb_add_text(hWsb,24,50,"(15) Print Screen");
		strcat(pf_list,"15");
	}
	

	if (start_item > 0)  								/* Add FIRST and PREV options		*/
	{
		wsb_add_text(hWsb,23, 2,"(2) First");
		wsb_add_text(hWsb,23,15,"(4) Previous");
		strcat(pf_list,"0204");
	}

	pos = start_item;
	for(coloff=0; (coloff+VOLCOLWIDTH-1) < WSB_COLS; coloff+=VOLCOLWIDTH) 		/* Load the VOLUMES in 2 columns	*/
	{
		for(row=VOLSTARTROW;row<(VOLSTARTROW+VOLNUMROWS);row++)
		{
			if (pos>=item_cnt)
			{
				pos = item_cnt;
				coloff = WSB_COLS;					/* Break outer loop			*/
				break;							/* Break inner loop			*/
			}

			if (rc=ring_remove(vol_ring,pos,(char*)&vol_item))		/* Remove item from ring		*/
			{
				werrlog(ERRORCODE(2),"vol_ring 1",ring_error(rc),0,0,0,0,0,0);
				return(rc);
			}

			vol_item.currow = row;						/* update cursor tabstop position	*/
			vol_item.curcol = coloff + VOLTABPOS;

			wsb_add_tabstop(hWsb, row, coloff + VOLTABPOS);			/* Add tabstop				*/

			sprintf(buff,"%6.6s",vol_item.wang);
			wsb_add_text(hWsb,row, coloff + volnamepos, buff);		/* Add Wang VOLUME			*/
			

			/* Construct screen display of VOLUME	*/
			if (strlen(vol_item.path) > VOLPATHWIDTH)
			{
				int off;
				off = strlen(vol_item.path) - VOLPATHWIDTH + 1;
				
				sprintf(buff,"+%s",&vol_item.path[off]);
			}
			else
			{
				strcpy(buff,vol_item.path);	
			}
			wsb_add_text(hWsb,row, coloff + VOLPATHPOS, buff);		/* Add volume path			*/

			if (rc=ring_add(vol_ring,0,(char*)&vol_item))			/* Replace item into ring		*/
			{
				werrlog(ERRORCODE(2),"vol_ring 2",ring_error(rc),0,0,0,0,0,0);
				return(rc);
			}

			pos++;								/* Next position			*/
		}
	}

	*p_end_item = pos;								/* Set end_item to last+1		*/
	if (item_cnt > *p_end_item) 
	{
		wsb_add_text(hWsb,24, 2,"(3) Last");					/* Add LAST & NEXT options		*/
		wsb_add_text(hWsb,24,15,"(5) Next");
		strcat(pf_list,"0305");
	}	
	strcat(pf_list,"33X");								/* Terminated pfkey list for vwang	*/


	if (sound_alarm)
	{
		sound_alarm = 0;
		wsb_set_alarm(hWsb);
	}
	wsb_display_and_read(hWsb, pf_list, p_pfkey, p_currow, p_curcol);

	if (PF_ENTER == *p_pfkey)
	{
		
		/* Get ENTER choice	*/
		rc = get_vol_choice(hWsb, *p_currow, *p_curcol, item_cnt, start_item, *p_end_item, p_option);

	}
	
	wsb_delete(hWsb);

	return rc;
}

/*
	get_vol_choice		Find out what was requested and set OPTION and file_style.

				option
				  0	Do nothing
				  1	Use vol:lib:file 
				  2	Use native path
				  3	Use selection from volume list
*/
static int get_vol_choice(HWSB hWsb, int row, int col, int item_cnt, int start_item, int end_item, int* p_option)
{
	int	pos, rc;
	char	vol[SIZEOF_VOL],lib[SIZEOF_LIB],file[SIZEOF_FILE],path[COB_FILEPATH_LEN];

	*p_option = VOL_OPTION_NONE;
	file_style = STYLE_UNKNOWN;

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
				clear_file_specs();

				*p_option = VOL_OPTION_VOLUME;
				if (vol_item.wang[0] == '(' ||				/* Don't set if not real volume		*/
				    vol_item.wang[0] == '.'   )
				{
					file_style = STYLE_PATH;
				}
				else
				{
					memcpy(wang_vol,vol_item.wang,SIZEOF_VOL);
					file_style = STYLE_VOL;
				}
			
				set_native_path(vol_item.path);
				return(0);
			}
		}
	} 

	wsb_get_filespec(hWsb,vol,lib,file,path,p_option,ROW_FILESPEC);

	memcpy(wang_vol,vol,SIZEOF_VOL);
	memcpy(wang_lib,lib,SIZEOF_LIB);
	memcpy(wang_file,file,SIZEOF_FILE);
	cobx2cstr(native_path,path,sizeof(path));
	native_path_loaded = 1;

	if (*p_option == VOL_OPTION_NATIVE)
	{
		file_style = STYLE_PATH;
	}
	else if (*p_option == VOL_OPTION_WANG)
	{
		native_path_loaded = 0;
	}

	return(0);
}

#define DIRNUMCOLS	3
#define DIRCOLWIDTH	(WSB_COLS / DIRNUMCOLS)
#define DIRSTARTROW	8
#define DIRNUMROWS	10
#define DIRSCREENITEMS	(DIRNUMCOLS * DIRNUMROWS)
#define DIRTABPOS	2					/* Tabstop position within column */
#define DIRPATHPOS	4					/* Path position within column */

/*
	dir_screen
*/
static int dir_screen(void)
{
	static	int start_item;
	static	int	currow, curcol;
	HWSB	hWsb;
	char	pf_list[PF_LIST_LEN];
	int	pfkey;
	int	item_cnt,end_item;
	int	rc;
	int	option;
	int	type;
	char	filepath[255];
	char	fullpath[255];
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
#ifdef WIN32
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

	case TYPE_NOACCESS:
		putmessage("Access denied");
		next_screen = ENTRY_SCREEN;
		return(0);

	case TYPE_ERROR:
		putmessage("Error attempting to access file");
		next_screen = ENTRY_SCREEN;
		return(0);

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

	default:
		putmessage("Unknown File Type");
		next_screen = ENTRY_SCREEN;
		return(0);
	}

	if (last_screen == ENTRY_SCREEN ||						/* If coming from ENTRY_SCREEN or	*/
	    last_screen == DIR_SCREEN	   )						/*    coming from DIR_SCREEN then	*/
	{
		start_item = 0;								/* Reset the start position		*/
		currow = 0;
		curcol = 0;
	}

	while(next_screen == DIR_SCREEN)
	{
		if (rc = load_dir_ring(force_load)) 
		{
			return(rc);
		}
		
		force_load = 0;
		ring_count(dir_ring,&item_cnt);

		if (item_cnt == 0)
		{
			putmessage("Unable to read Directory");
			next_screen = ENTRY_SCREEN;
			return(0);
		}

		hWsb = wsb_new();

		if (rc = build_dir_screen(hWsb,item_cnt,start_item,&end_item,pf_list)) 
		{
			wsb_delete(hWsb);
			return(rc);
		}
		
		if (sound_alarm)
		{
			sound_alarm = 0;
			wsb_set_alarm(hWsb);
		}
		wsb_display_and_read(hWsb, pf_list, &pfkey, &currow, &curcol);

		last_screen = DIR_SCREEN;						/* Last screen display is DIR_SCREEN	*/
		file_screen_return = DIR_SCREEN;

		if (pfkeys12())
		{
			if (PF_PRTSCRN_11 == pfkey)
			{
				pfkey = PF_PRTSCRN_15;
			}
		}

#ifdef WIN32
		if (nativescreens() && 6 == pfkey)
		{
			pfkey = PF_PRINT;
		}
#endif

		switch(pfkey)
		{
		case PF_ENTER: 		/* Enter */
		case PF_RENAME: 	/* Rename */
		case PF_SCRATCH: 	/* Scratch */
		case PF_PROTECT: 	/* Protect */
		case PF_PRINT: 		/* Print */
		case PF_DISPLAY_11:	/* Display */
		case PF_EDIT_12:	/* Edit */
			if (rc=get_dir_choice(currow,curcol,item_cnt,start_item,end_item,&option,filepath)) 
			{
				wsb_delete(hWsb);
				return(rc);
			}

			if (option == DIR_OPTION_NONE)
			{
				putmessage("You must position the cursor at a Tabstop!");
				break;
			}

			addpath(fullpath,native_path,filepath);				/* add the dir to the path		*/

			switch(pfkey)
			{
			case PF_ENTER: 		/* Enter */
				if (select_dir_entry(option,filepath)) 
				{
					wsb_delete(hWsb);
					return(0);
				}
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
					if ( 0 == select_scratch(fullpath, hWsb, currow, curcol))
					{
						force_load = 1;
					}
				}
				else	/* Scratch this DIRECTORY */
				{
					char *msg;
					
					wsb_clearmessage(hWsb);
					wsb_clearrow(hWsb,20);
					wsb_clearrow(hWsb,21);
					wsb_clearrow(hWsb,22);
					wsb_clearrow(hWsb,23);
					wsb_clearrow(hWsb,24);
					
					msg = "Press (ENTER) to SCRATCH the DIRECTORY, or (1) to return";
					
					wsb_add_field(hWsb,22,2,FAC_PROT_BOLD,msg,strlen(msg));

					wsb_display_and_read(hWsb, "000133X", &pfkey, &currow, &curcol);

					switch(pfkey)
					{
					case 0:
						if (0 == scratch_dir(fullpath))
						{
							force_load = 1;
						}
						break;
					case 1:
						break;
					default:
						next_screen = EXIT_SCREEN;
						break;
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

			case PF_DISPLAY_11:	/* Display */
				if (option == DIR_OPTION_FILE)
				{
					select_display(fullpath);
				}
				else
				{
					putmessage("You can not DISPLAY a directory");
				}
				break;

			case PF_EDIT_12:		/* Edit the file */
				if (option == DIR_OPTION_FILE)
				{
					select_edit(fullpath);
				}
				else
				{
					putmessage("You can not EDIT a directory");
				}
			}

			break;

		case PF_RETURN: 	/* Return to Volume Display */
			next_screen = ENTRY_SCREEN;
			wsb_delete(hWsb);
			return(0);

		case PF_FIRST:		/* First */
			start_item = 0;
			currow = 0;
			curcol = 0;
			break;

		case PF_LAST: 		/* Last */
			start_item = item_cnt-DIRSCREENITEMS;
			if (start_item < 0) start_item=0;
			currow = 0;
			curcol = 0;
			break;

		case PF_PREVIOUS: 	/* Previous */
			start_item -= DIRSCREENITEMS;
			if (start_item < 0) start_item=0;
			currow = 0;
			curcol = 0;
			break;

		case PF_NEXT: 		/* Next */
			start_item = end_item;
			currow = 0;
			curcol = 0;
			break;

		case PF_PRTSCRN_15:
			if (!nativescreens())
			{
				screen_print();
				putmessage("Screen printed");
			}
			break;
		default: /* HELP */
			next_screen = EXIT_SCREEN;
			wsb_delete(hWsb);
			return(0);
		}

		wsb_delete(hWsb);
	}
	return 0;
}

static int build_dir_screen(HWSB hWsb, int item_cnt, int start_item, int* p_end_item, char* pf_list)
{
	char	vol[SIZEOF_VOL], lib[SIZEOF_LIB], file[SIZEOF_FILE], filepath[COB_FILEPATH_LEN];
	int	rc;
	int	pos, row, coloff;
	char	buff[255];
	int	dirpathpos;
	int	dirpathwidth;
	char	*msg;

	if (nativescreens())
	{
		dirpathpos = DIRPATHPOS+1;
	}
	else
	{
		dirpathpos = DIRPATHPOS;
	}
	dirpathwidth = DIRCOLWIDTH - dirpathpos;

	rc = 0;
	
	memcpy(vol,wang_vol,SIZEOF_VOL);
	memcpy(lib,wang_lib,SIZEOF_LIB);
	memcpy(file,wang_file,SIZEOF_FILE);
	cstr2cobx(filepath,native_path,sizeof(filepath));

	wsb_add_text(hWsb,1,0,"***  Directories  ***");
	wsb_screen_message(hWsb);

	sprintf(buff,"Directory contains %d files.",item_cnt);
	wsb_add_text(hWsb,3,25,buff);

	wsb_build_filespec(hWsb,vol,lib,file,filepath,file_style,NOMOD,ROW_FILESPEC);

	msg = "Position cursor to a File or Directory and press (ENTER) or Select:";
	wsb_add_field(hWsb, 19, 2, FAC_PROT_BOLD_LINE, msg, strlen(msg));
	
	strcpy(pf_list,"00");
	wsb_add_text(hWsb,21, 2, "(1) Return to Volume Display");
	strcat(pf_list,"01");

	if (modify_allowed)
	{
		wsb_add_text(hWsb,21,35," (7) Rename");
		strcat(pf_list,"07");
		wsb_add_text(hWsb,22,35," (8) Scratch");
		strcat(pf_list,"08");
#ifdef unix
		wsb_add_text(hWsb,23,35," (9) Protect");
		strcat(pf_list,"09");
#endif
	}

#ifdef WIN32
	/*
	**	Native screens on WIN32 cannot map F10 key so use F6 instead.
	*/
	if (nativescreens())
	{
		wsb_add_text(hWsb,24,35," (6) Print");
		strcat(pf_list,"06");
	}
	else
#endif
	{
		wsb_add_text(hWsb,24,35,"(10) Print");
		strcat(pf_list,"10");
	}

	if (!pfkeys12())
	{
		if (display_allowed)
		{
			wsb_add_text(hWsb,21,50,"(11) Display");
			strcat(pf_list,"11");
		}
		if (modify_allowed)
		{
			wsb_add_text(hWsb,22,50,"(12) Edit");
			strcat(pf_list,"12");
		}
	}

	if (nativescreens())
	{
		/* No print screen */
	}
	else if (pfkeys12())
	{
		wsb_add_text(hWsb,24,50,"(11) Print Screen");
		strcat(pf_list,"11");
	}
	else
	{
		wsb_add_text(hWsb,24,50,"(15) Print Screen");
		strcat(pf_list,"15");
	}
	
	if (start_item > 0)  
	{
		wsb_add_text(hWsb,23, 2,"(2) First");
		wsb_add_text(hWsb,23,15,"(4) Previous");
		strcat(pf_list,"0204");
	}

	pos = start_item;
	for(coloff=0; (coloff+DIRCOLWIDTH-1) < WSB_COLS; coloff+=DIRCOLWIDTH)
	{
		for(row=DIRSTARTROW;row<(DIRSTARTROW+DIRNUMROWS);row++)
		{
			if (pos>=item_cnt)
			{
				pos = item_cnt;
				coloff = WSB_COLS;					/* Break outer loop			*/
				break;							/* Break inner loop			*/
			}

			if (rc=ring_remove(dir_ring,pos,(char*)&dir_item))
			{
				werrlog(ERRORCODE(2),"dir_ring 1",ring_error(rc),0,0,0,0,0,0);
				return(rc);
			}

			dir_item.currow = row;
			dir_item.curcol = coloff + DIRTABPOS;

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

			if ((int)strlen(filepath) > dirpathwidth)
			{
				filepath[dirpathwidth-1] = '+';
				filepath[dirpathwidth]   = '\0';
			}

			wsb_add_tabstop(hWsb, dir_item.currow, coloff + DIRTABPOS);
			wsb_add_text(hWsb, dir_item.currow, coloff + dirpathpos, filepath);

			if (rc=ring_add(dir_ring,0,(char*)&dir_item))
			{
				werrlog(ERRORCODE(2),"dir_ring 2",ring_error(rc),0,0,0,0,0,0);
				return(rc);
			}

			pos++;
		}
	}

	*p_end_item = pos;
	if (item_cnt > *p_end_item) 
	{
		wsb_add_text(hWsb,24, 2,"(3) Last");
		wsb_add_text(hWsb,24,15,"(5) Next");
		strcat(pf_list,"0305");
	}
	strcat(pf_list,"33X");

	return(0);
}

/*
	get_dir_choice		Find out what file was selected, return it's position and set option.

				Option
				  0	No file was selected
				  1	directory selected
				  2	file selected
			
*/

static int get_dir_choice(int row, int col, int item_cnt, int start_item, int end_item, int* p_option, char* filename)
{
	int	pos, rc;

	*p_option = DIR_OPTION_NONE;

	if ( row >=DIRSTARTROW  && row < (DIRSTARTROW+DIRNUMROWS) )
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
				if (dir_item.type == TYPE_DIR)	*p_option = DIR_OPTION_DIR;
				else				*p_option = DIR_OPTION_FILE;
				strcpy(filename,dir_item.file);
				return(0);
			}
		}
	} 
	return(0);
}

static int select_dir_entry(int option, const char* filepath)
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
				memset(wang_lib,' ',SIZEOF_LIB);			/* clear wang_lib			*/
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
				if (strlen(filepath) <= SIZEOF_LIB)
				{
					file_style = STYLE_LIB;
					loadpad(wang_lib,filepath,SIZEOF_LIB);		/* updated wang_lib			*/
					upper_mem(wang_lib,SIZEOF_LIB);			/* shift to uppercase			*/
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

static int add_file_adjust(const char* filepath)
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
			if (strlen(filepath) <= SIZEOF_FILE)
			{
				file_style = STYLE_FILE;
				loadpad(wang_file,filepath,SIZEOF_FILE);	/* updated wang_file			*/
				upper_mem(wang_file,SIZEOF_FILE);		/* shift to uppercase			*/
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

static void remove_file_adjust(void)
{
	uppath(native_path);

	if (file_style == STYLE_FILE)
	{
		file_style = STYLE_LIB;
		memset(wang_file,' ',SIZEOF_FILE);
	}
}

static void select_display(const char* file)
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
		link_display(file);							/* Display the file			*/
	}
}


/*
**	ROUTINE:	select_edit()
**
**	FUNCTION:	Edit a file using weditorexe( )
**
**	DESCRIPTION:	Receives a c str containing the fullpath to a file that needs to be Edited.
**			Verifies if this file can be reached.
**			Assembles a cmd string containing the editor program and the file to be edited.
**			Passes the cmd into wsystem_interactive( ).
**			Passes an error message into putmessage( ) depending on wsystem_interactive( ) results.
**
**	ARGUMENTS:	(I)		file
**
**	GLOBALS:	none	
**
**	RETURN:		void
**
**	WARNINGS:	?
**
*/
static void select_edit(const char* file)
{
	char	cmd[SIZEOF_CMD], mess[SIZEOF_MESS];
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
		int len;
		len = strlen(weditorexe()) + strlen(file) + 4;
		if (SIZEOF_CMD < len  )
		{
			putmessage("Filepath too long");
		}
		else
		{
			sprintf(cmd,"'%s' '%s'",weditorexe(),file);			/* quote wrap file		*/
			rc = wsystem_interactive(cmd);

			if (rc)
			{
				sprintf(mess,"EDIT terminated: \"%s\" [rc=%d]",cmd,rc);
				putmessage(mess);
			}
		}
	}
}

static int scratch_file(const char *file)
{
	char buff[256];
	
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
}

static int select_scratch(const char* file, HWSB hWsb, int currow, int curcol)
{
	int	pfkey;
	char 	*msg;
	
	wsb_clearmessage(hWsb);
	wsb_clearrow(hWsb,20);
	wsb_clearrow(hWsb,21);
	wsb_clearrow(hWsb,22);
	wsb_clearrow(hWsb,23);
	wsb_clearrow(hWsb,24);
					
	msg = "Press (ENTER) to SCRATCH the file, or (1) to return";
	wsb_add_field(hWsb,22,2,FAC_PROT_BOLD,msg,strlen(msg));

	wsb_display_and_read(hWsb, "000133X", &pfkey, &currow, &curcol);

	switch(pfkey)
	{
	case 0:
		return scratch_file(file);

	case 1:
		return(1);								/* Scratch failed			*/
	default: /* HELP */
		next_screen = EXIT_SCREEN;
		return(1);								/* Scratch failed			*/
	}
}

static int scratch_dir(const char* file)
{
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
}

static int select_print(void)
{
	int	pfkey;
	char	print_mode[10], print_class[10], scratch_after[10], copies_char[10], form_char[10], disp[10];
	int4	tlong;
	int4	copies_num, form_num;
	int4	rc;

	print_mode[0] = 'S';
	get_defs(DEFAULTS_PC,print_class);
	memcpy(scratch_after,"NO ",3);
	memcpy(copies_char,"    1",5);
	get_defs(DEFAULTS_FN,(char*)&tlong);
	sprintf(form_char,"%03ld",tlong);
	
	for(;;)
	{
		pfkey = dnr_print_screen(print_mode,print_class,scratch_after,copies_char,form_char);

		if (pfkeys12())
		{
			if (PF_PRTSCRN_11 == pfkey)
			{
				pfkey = PF_PRTSCRN_15;
			}
		}

		switch(pfkey)
		{
		case PF_ENTER:

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
			case  0:	putmessage("File printed");  		return(0);
			case 20:	putmessage("File Not Found");		break;
			case 28:	putmessage("Access Denied");		break;
			case 40:	putmessage("Invalid argument");		break;
			default:	putmessage("Unable to print file");	break;
			}
			break;
		case PF_RETURN:
			return(0);
		case PF_PRTSCRN_15:
			if (!nativescreens())
			{
				screen_print();
				putmessage("Screen printed");
			}
			break;
		default: /* HELP */
			next_screen = EXIT_SCREEN;
			return(0);
		}
	}
}

static int dnr_print_screen(char* print_mode, char* print_class, char* scratch_after, char* copies_char, char* form_char)
{
	HWSB 	hWsb;
	int	currow, curcol;
	char	pf_list[PF_LIST_LEN];
	char	vol[SIZEOF_VOL], lib[SIZEOF_LIB], file[SIZEOF_FILE], filepath[COB_FILEPATH_LEN];
	char	*msg;
	int 	pfkey;

	memcpy(vol,wang_vol,SIZEOF_VOL);
	memcpy(lib,wang_lib,SIZEOF_LIB);
	memcpy(file,wang_file,SIZEOF_FILE);
	cstr2cobx(filepath,native_path,sizeof(filepath));

	hWsb = wsb_new();

	wsb_add_text(hWsb,1,30,"*** Print File ***");
	wsb_screen_message(hWsb);

	wsb_build_filespec(hWsb,vol,lib,file,filepath,file_style,NOMOD,ROW_FILESPEC);

	msg = "Press (ENTER) to PRINT, or Select:";
	wsb_add_field(hWsb, 20, 2, FAC_PROT_BOLD_LINE, msg, strlen(msg));
	wsb_add_text(hWsb,22, 2,"(1) Return to Display");
	strcpy(pf_list,"0001");

	if (nativescreens())
	{
		/* No print screen */
	}
	else if (pfkeys12())
	{
		wsb_add_text(hWsb,24,50,"(11) Print Screen");
		strcat(pf_list,"11");
	}
	else
	{
		wsb_add_text(hWsb,24,50,"(15) Print Screen");
		strcat(pf_list,"15");
	}
	strcat(pf_list,"33X");

	wsb_add_text(hWsb, 9,30,"PRINT MODE      = +     (S=SPOOL,H=HOLD)");
	wsb_add_text(hWsb,11,30,"PRINT CLASS     = +     (A-Z)");
	wsb_add_text(hWsb,13,30,"Scratch File after Printing?");
	wsb_add_text(hWsb,14,30,"Scratch         = +++");
	wsb_add_text(hWsb,16,30,"COPIES          = +++++ (1-32767)");
	wsb_add_text(hWsb,17,30,"FORM #          = +++   (0-254)");

	wsb_add_field(hWsb, 9,48,FAC_MOD_BOLD_UP, print_mode, 1);
	wsb_add_field(hWsb,11,48,FAC_MOD_BOLD_UP, print_class,1);
	wsb_add_field(hWsb,14,48,FAC_MOD_BOLD_UP, scratch_after,3);
	wsb_add_field(hWsb,16,48,FAC_MOD_BOLD_UP, copies_char,5);
	wsb_add_field(hWsb,17,48,FAC_MOD_BOLD_UP, form_char,3);


	currow = 0;
	curcol = 0;
	
	if (sound_alarm)
	{
		sound_alarm = 0;
		wsb_set_alarm(hWsb);
	}
	wsb_display_and_read(hWsb, pf_list, &pfkey, &currow, &curcol);

	if (PF_ENTER == pfkey)
	{
		wsb_get_field(hWsb, 9,48, print_mode,1);
		wsb_get_field(hWsb,11,48, print_class,1);
		wsb_get_field(hWsb,14,48, scratch_after,3);
		wsb_get_field(hWsb,16,48, copies_char,5);
		wsb_get_field(hWsb,17,48, form_char,3);
	}

	wsb_delete(hWsb);
	
	return (pfkey);
}

static int select_rename(int file_type)
{
	HWSB	hWsb;
	int	currow, curcol;
	char	pf_list[PF_LIST_LEN];
	int	pfkey;
	char	vol[SIZEOF_VOL],lib[SIZEOF_LIB],file[SIZEOF_FILE],
		filepath[COB_FILEPATH_LEN];
	char	path[256],buff[256];
	int	option;
	int4	mode;
	int	dest_spec;
	char	ebuf[8][80];
	int	ecnt=0;

	currow = 0;
	curcol = 0;

	memcpy(vol,wang_vol,SIZEOF_VOL);
	memcpy(lib,wang_lib,SIZEOF_LIB);
	memcpy(file,wang_file,SIZEOF_FILE);

	cstr2cobx(filepath,native_path,sizeof(filepath));

	switch(file_style)
	{
	case STYLE_PATH:
		memset(vol, ' ',SIZEOF_VOL);
	case STYLE_VOL:
		memset(lib, ' ',SIZEOF_LIB);
	case STYLE_LIB:
		memset(file,' ',SIZEOF_FILE);
	}

	for(;;)
	{
		dest_spec = 0;

		hWsb = wsb_new();
		build_rename_screen(hWsb,pf_list,ecnt,ebuf,vol,lib,file,filepath);

		if (sound_alarm)
		{
			sound_alarm = 0;
			wsb_set_alarm(hWsb);
		}
		wsb_display_and_read(hWsb, pf_list, &pfkey, &currow, &curcol);

		if (pfkeys12())
		{
			if (PF_PRTSCRN_11 == pfkey)
			{
				pfkey = PF_PRTSCRN_15;
			}
		}

		switch(pfkey)
		{
		case PF_RETURN:	/* Return */
			wsb_delete(hWsb);
			return(0);
		case PF_ENTER:	/* RENAME */
		case 2:		/* RENAME */
		case 3: 	/* COPY */
		case 5: 	/* LINK */
		case 6: 	/* SYMBOLIC LINK */
			wsb_get_filespec(hWsb,vol,lib,file,filepath,&option,10);
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
				cobx2cstr(path,filepath,sizeof(filepath));

				dest_spec = 1;
				break;
			case VOL_OPTION_NATIVE:
				cobx2cstr(path,filepath,sizeof(filepath));
				dest_spec = 1;
				break;
			}
			break;
		case PF_PRTSCRN_15:	/* PRINT SCREEN */
			if (!nativescreens())
			{
				screen_print();
				putmessage("Screen printed");
			}
			break;
		default: /* HELP */
			next_screen = EXIT_SCREEN;
			wsb_delete(hWsb);
			return(0);
		}

		wsb_delete(hWsb);

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
#ifdef unix
				void	(*save_sig)();
				FILE	*fp;
				char	pbuf[80];
#endif

				makepath( path );	/* Ensure the target path exists		*/
#ifdef unix

				switch(pfkey)
				{
				case PF_ENTER:
				case 2:
					sprintf(buff,"mv -f '%s' '%s' 2>&1",native_path,path);
					break;
				case 3:
					sprintf(buff,"cp '%s' '%s' 2>&1",native_path,path);
					break;
				case 5:
					sprintf(buff,"ln -f '%s' '%s' 2>&1",native_path,path);
					break;
				case 6:
					sprintf(buff,"ln -s '%s' '%s' 2>&1",native_path,path);
					break;
				}

				wtrace("MNGFILE","COMMAND","popen(\"%s\")",buff);
				
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
#ifdef WIN32
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
#endif /* WIN32 */
			}
		}
	}
}

static int build_rename_screen(HWSB hWsb, 
			       char* pf_list, 
			       int ecnt, 
			       char ebuf[8][80],
			       char dvol[SIZEOF_VOL], 
			       char dlib[SIZEOF_LIB], 
			       char dfile[SIZEOF_FILE], 
			       char dfilepath[COB_FILEPATH_LEN])
{
	char	vol[SIZEOF_VOL], lib[SIZEOF_LIB], file[SIZEOF_FILE], filepath[COB_FILEPATH_LEN];
	int	i;
	char	*msg;

	memcpy(vol,wang_vol,SIZEOF_VOL);
	memcpy(lib,wang_lib,SIZEOF_LIB);
	memcpy(file,wang_file,SIZEOF_FILE);
	cstr2cobx(filepath,native_path,sizeof(filepath));

#ifdef unix
	wsb_add_text(hWsb,1,0,"***  Rename/Copy File  ***");
#endif
#ifdef WIN32
	wsb_add_text(hWsb,1,0,"***  Rename File  ***");
#endif
	wsb_screen_message(hWsb);

	wsb_build_filespec(hWsb,vol,lib,file,filepath,file_style,NOMOD,ROW_FILESPEC);

	msg = "Enter the Destination file specification below";
	wsb_add_field(hWsb,8,2,FAC_PROT_BOLD, msg, strlen(msg));

	wsb_build_filespec(hWsb,dvol,dlib,dfile,dfilepath,STYLE_FILE,MODIFIABLE,10);
	
	msg = "Press (ENTER) to RENAME or Select:";
	wsb_add_field(hWsb,20,2,FAC_PROT_BOLD_LINE, msg, strlen(msg));
	strcpy(pf_list,"00");

	wsb_add_text(hWsb,22, 2,"(1) Return");		strcat(pf_list,"01");
#ifdef unix
	wsb_add_text(hWsb,23, 2,"(2) RENAME");		strcat(pf_list,"02");
	wsb_add_text(hWsb,24, 2,"(3) COPY");		strcat(pf_list,"03");
	wsb_add_text(hWsb,23,20,"(5) LINK");		strcat(pf_list,"05");
	wsb_add_text(hWsb,24,20,"(6) Symbolic LINK");	strcat(pf_list,"06");
#endif

	if (nativescreens())
	{
		/* No print screen */
	}
	else if (pfkeys12())
	{
		wsb_add_text(hWsb,24,50,"(11) Print Screen");	strcat(pf_list,"11");
	}
	else
	{
		wsb_add_text(hWsb,24,50,"(15) Print Screen");	strcat(pf_list,"15");
	}
	

	strcat(pf_list,"33X");

	for (i=0;i<ecnt;i++)
	{
		wsb_add_text(hWsb,13+i,2,ebuf[i]);
	}

	return(0);
}

/*
	file_screen
*/
static int file_screen(void)
{
	HWSB	hWsb;
	int	currow, curcol;
	int	pfkey;
	char	pf_list[PF_LIST_LEN];
	int	rc, i;
	int	protect;
	int	filetype;

	currow = 0;
	curcol = 0;
	rc = 0;
	
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

		hWsb = wsb_new();

		if (rc = build_file_screen(hWsb,pf_list,protect,filetype)) 
		{
			wsb_delete(hWsb);
			return(rc);
		}
		
		if (sound_alarm)
		{
			sound_alarm = 0;
			wsb_set_alarm(hWsb);
		}
		wsb_display_and_read(hWsb, pf_list, &pfkey, &currow, &curcol);

		last_screen = FILE_SCREEN;						/* Last screen display is FILE_SCREEN	*/

		/*
		**	Convert pfkey values
		*/
		if (pfkeys12())
		{
			switch(pfkey)
			{
			case 3:
				pfkey = PF_DISPLAY_11;
				break;
			case 4:
				pfkey = PF_EDIT_12;
				break;
			case 11:
				pfkey = PF_PRTSCRN_15;
				break;
			}
		}

#ifdef WIN32
		if (nativescreens() && 6 == pfkey)
		{
			pfkey = PF_PRINT;
		}
#endif

		switch(pfkey)
		{
#ifdef unix
		case PF_ENTER:
			if (protect)
			{
				change_protection(hWsb);
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
				wsb_delete(hWsb);
				return(0);
			}
			break;
		case PF_RENAME:
			select_rename((filetype == TYPE_DIR)?DIR_OPTION_DIR:DIR_OPTION_FILE);
			break;
		case PF_SCRATCH:
			if ( 0 == select_scratch(native_path, hWsb, currow, curcol))
			{
				return_file_screen();
				wsb_delete(hWsb);
				return(0);
			}
			break;
		case PF_PROTECT:
			protect = 1;
			break;
		case PF_PRINT:
			select_print();
			break;
		case PF_DISPLAY_11:
			select_display(native_path);
			break;
		case PF_EDIT_12:
			select_edit(native_path);
			break;
		case PF_PRTSCRN_15:
			if (!nativescreens())
			{
				screen_print();
				putmessage("Screen printed");
			}
			break;
		default: /* HELP */
			next_screen = EXIT_SCREEN;
			wsb_delete(hWsb);
			return(0);
		}

		if (g_protect) 
		{
			next_screen = DIR_SCREEN;
			wsb_delete(hWsb);
			return(0);
		}

		wsb_delete(hWsb);
	}

	return rc;
}

static int return_file_screen(void)
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

static int build_file_screen(HWSB hWsb, char* pf_list, int protect, int filetype)
{
	char	vol[SIZEOF_VOL], lib[SIZEOF_LIB], file[SIZEOF_FILE], filepath[COB_FILEPATH_LEN];
	char	buff[80];
	char	type[80];
	struct	stat	filestat;
	char	*msg;
#ifdef unix
	fac_t	the_fac;
	int	rc,col,row;
	struct	passwd	*pw;
	struct	group	*gr;
	char	owner[80], group[80], protection[20];
#endif

	memcpy(vol,wang_vol,SIZEOF_VOL);
	memcpy(lib,wang_lib,SIZEOF_LIB);
	memcpy(file,wang_file,SIZEOF_FILE);
	cstr2cobx(filepath,native_path,sizeof(filepath));
	wsb_add_text(hWsb,1,28,"***  File Attributes  ***");
	wsb_screen_message(hWsb);

	wsb_build_filespec(hWsb,vol,lib,file,filepath,file_style,NOMOD,ROW_FILESPEC);

	if (protect)
	{
		msg = "Press (ENTER) to PROTECT the file, or (1) to return";
		wsb_add_field(hWsb, 22, 2, FAC_PROT_BOLD, msg, strlen(msg));
		strcpy(pf_list,"000133X");
	}
	else
	{
		if (file_screen_return == ENTRY_SCREEN)
		{
			wsb_add_text(hWsb,21, 2,"(1) Return to Volume Display");
		}
		else
		{
			file_screen_return = DIR_SCREEN;
			wsb_add_text(hWsb,21, 2,"(1) Return to Directory Display");
		}
		strcpy(pf_list,"01");

		if (modify_allowed)
		{
			wsb_add_text(hWsb,21,35," (7) Rename");
			strcat(pf_list,"07");
			wsb_add_text(hWsb,22,35," (8) Scratch");
			strcat(pf_list,"08");
#ifdef unix
			wsb_add_text(hWsb,23,35," (9) Protect");
			strcat(pf_list,"09");
#endif
		}
		if (TYPE_REG == filetype)
		{
#ifdef WIN32
			/*
			**	Native screens on WIN32 cannot map F10 key so use F6 instead.
			*/
			if (nativescreens())
			{
				wsb_add_text(hWsb,24,35," (6) Print");
				strcat(pf_list,"06");
			}
			else
#endif
			{
				wsb_add_text(hWsb,24,35,"(10) Print");
				strcat(pf_list,"10");
			}
		}

		if (pfkeys12())
		{
			if (TYPE_REG == filetype && display_allowed)
			{
				wsb_add_text(hWsb,23,2,"(3) Display");	
				strcat(pf_list,"03");
			}
			if (TYPE_REG == filetype && modify_allowed)
			{
				wsb_add_text(hWsb,24,2,"(4) Edit");
				strcat(pf_list,"04");
			}
		}
		else
		{
			if (TYPE_REG == filetype && display_allowed)
			{
				wsb_add_text(hWsb,21,50,"(11) Display");
				strcat(pf_list,"11");
			}
			if (TYPE_REG == filetype && modify_allowed)
			{
				wsb_add_text(hWsb,22,50,"(12) Edit");
				strcat(pf_list,"12");
			}
		}

		if (nativescreens())
		{
			/* No print screen */
		}
		else if (pfkeys12())
		{
			wsb_add_text(hWsb,24,50,"(11) Print Screen");
			strcat(pf_list,"11");
		}
		else
		{
			wsb_add_text(hWsb,24,50,"(15) Print Screen");
			strcat(pf_list,"15");
		}
		
		strcat(pf_list,"33X");
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
			strcpy(type,"VISION 2");
			break;
		case FILE_VISION3:
			strcpy(type,"VISION 3");
			break;
		case FILE_VISION4D:
			strcpy(type,"VISION 4 (DATA)");
			break;
		case FILE_VISION4I:
			strcpy(type,"VISION 4 (INDEX)");
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
		case FILE_SHELL:
			strcpy(type,"SHELL COMMANDS");
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

	wsb_add_text(hWsb, 8,2,"File type is");
	wsb_add_field(hWsb,8,20,FAC_PROT_BOLD, type, strlen(type));

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
		wsb_add_text(hWsb, 9,2,"Size in bytes is");
		wsb_add_field(hWsb,9,20,FAC_PROT_BOLD, buff, strlen(buff));

		strcpy(buff,ctime(&filestat.st_mtime));
		buff[strlen(buff)-1] = '\0';
		wsb_add_text(hWsb,11,2,"Last Modified");
		wsb_add_field(hWsb,11,20,FAC_PROT_BOLD, buff, strlen(buff));
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
	wsb_add_text(hWsb,13,2,"Protection is");
	wsb_add_field(hWsb,13,20,FAC_PROT_BOLD, buff, strlen(buff));

	col =  2;
	row = 15;
	wsb_add_text(hWsb,row+0,col,"                      Read   Write  Execute");
	wsb_add_text(hWsb,row+1,col,"User                   +       +       +   ");
	wsb_add_text(hWsb,row+2,col,"Group                  +       +       +   ");
	wsb_add_text(hWsb,row+3,col,"Other                  +       +       +   ");

	sprintf(buff,"%s (%d)",owner,filestat.st_uid);
	wsb_add_field(hWsb,row+1,col+6,FAC_PROT_BOLD, buff, strlen(buff));

	sprintf(buff,"%s (%d)",group,filestat.st_gid);
	wsb_add_field(hWsb,row+2,col+6,FAC_PROT_BOLD, buff, strlen(buff));

	if (protect)
	{
		the_fac = FAC_MOD_BOLD_UP;
	}
	else
	{
		the_fac = FAC_PROT_BOLD;
	}

	wsb_add_field(hWsb,row+1,col+23,the_fac,(protection[1]=='r')?"X":" ",1);
	wsb_add_field(hWsb,row+1,col+31,the_fac,(protection[2]=='w')?"X":" ",1);
	wsb_add_field(hWsb,row+1,col+39,the_fac,(protection[3]=='x')?"X":" ",1);

	wsb_add_field(hWsb,row+2,col+23,the_fac,(protection[4]=='r')?"X":" ",1);
	wsb_add_field(hWsb,row+2,col+31,the_fac,(protection[5]=='w')?"X":" ",1);
	wsb_add_field(hWsb,row+2,col+39,the_fac,(protection[6]=='x')?"X":" ",1);

	wsb_add_field(hWsb,row+3,col+23,the_fac,(protection[7]=='r')?"X":" ",1);
	wsb_add_field(hWsb,row+3,col+31,the_fac,(protection[8]=='w')?"X":" ",1);
	wsb_add_field(hWsb,row+3,col+39,the_fac,(protection[9]=='x')?"X":" ",1);
#endif /* unix */

	return(0);
}

#ifdef unix
static change_protection(HWSB hWsb)
{
	int	col,row;
	mode_t	mode;
	char	ur,uw,ux,gr,gw,gx,or,ow,ox;

	row=15;
	col=2;

	wsb_get_field(hWsb,row+1,col+23,&ur,1);
	wsb_get_field(hWsb,row+1,col+31,&uw,1);
	wsb_get_field(hWsb,row+1,col+39,&ux,1);

	wsb_get_field(hWsb,row+2,col+23,&gr,1);
	wsb_get_field(hWsb,row+2,col+31,&gw,1);
	wsb_get_field(hWsb,row+2,col+39,&gx,1);

	wsb_get_field(hWsb,row+3,col+23,&or,1);
	wsb_get_field(hWsb,row+3,col+31,&ow,1);
	wsb_get_field(hWsb,row+3,col+39,&ox,1);

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

static void wsb_get_filespec(HWSB hWsb,
			     char vol[SIZEOF_VOL],
			     char lib[SIZEOF_LIB],
			     char file[SIZEOF_FILE],
			     char path[COB_FILEPATH_LEN],
			     int *option,
			     int startrow)
{
	int	modv,modl,modf,modp;
	int	blank_wang, blank_native;
	char	testpath[COB_FILEPATH_LEN];

	wsb_get_field(hWsb,startrow,COL_VOL,vol,SIZEOF_VOL);
	wsb_get_field(hWsb,startrow,COL_LIB,lib,SIZEOF_LIB);
	wsb_get_field(hWsb,startrow,COL_FILE,file,SIZEOF_FILE);
	wsb_get_field(hWsb,startrow+1,COL_PATH,path,COB_FILEPATH_LEN-1);
	/* The path screen field is 79 and path is 80 so plug a space into the last char */
	path[COB_FILEPATH_LEN-1] = ' ';

	leftjust(vol,SIZEOF_VOL);
	leftjust(lib,SIZEOF_LIB);
	leftjust(file,SIZEOF_FILE);
	leftjust(path,COB_FILEPATH_LEN-1);

	modv = memcmp(wang_vol,    vol,  SIZEOF_VOL);
	modl = memcmp(wang_lib,    lib,  SIZEOF_LIB);
	modf = memcmp(wang_file,   file, SIZEOF_FILE);
	cstr2cobx(testpath,native_path,COB_FILEPATH_LEN /*-1*/);

	modp = memcmp(testpath, path, COB_FILEPATH_LEN-1);
	
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


/*==============================================================================================================================*/
/*	vol_ring routines													*/
/*==============================================================================================================================*/

/*
	load_vol_ring	Load the vol_ring with the list of volumes and there translations.
*/
static int load_vol_ring(void)
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

	memcpy(vol_item.wang,".     ",SIZEOF_VOL);
	getcwd(vol_item.path,COB_FILEPATH_LEN);
	if (rc = vol_add()) return(rc);

	memcpy(vol_item.wang,"(HOME)",SIZEOF_VOL);
	strcpy(vol_item.path,wisphomedir(NULL));
	if (rc = vol_add()) return(rc);

	memcpy(vol_item.wang,"(ROOT)",SIZEOF_VOL);
#ifdef unix
	strcpy(vol_item.path,DDS_STR);
#endif
#ifdef WIN32
	strcpy(vol_item.path,"C:\\");
#endif
	if (rc = vol_add()) return(rc);

	logical_ptr = get_logical_list();

	while(logical_ptr)
	{
		memset(vol_item.wang,' ',SIZEOF_VOL);
		memcpy(vol_item.wang,logical_ptr->logical,strlen(logical_ptr->logical));
		strcpy(vol_item.path,logical_ptr->translate);
		if (0!=memcmp(vol_item.wang,".     ",SIZEOF_VOL) &&			/* Don't load "." volume		*/
		    0!=memcmp(vol_item.wang,"      ",SIZEOF_VOL)   )			/* Don't load blank volume		*/
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
static int vol_add(void)
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
	return(memcmp(i1->wang,i2->wang,SIZEOF_VOL));
}

/*==============================================================================================================================*/
/*	dir_ring routines													*/
/*==============================================================================================================================*/

/*
	load_dir_ring	Load the dir_ring with the list of files and directories
*/
static int load_dir_ring(int force_load)
{
	int	rc;
	int	len;
	char	filepath[WISP_FILEPATH_LEN];
	char	*ptr;
	char	*context;
	int	cnt;

#ifdef unix
	/*
	**	This code only works on unix because only there is stat() fully implemented.
	**	Also stat.st_ino is not reliable on WIN32 or DOS for a directory.
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
		if (strlen(native_path) + 1 + strlen(ptr) >= sizeof(filepath))
		{
			putmessage("File path is too long");
			return(0);
		}
		
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
static int dir_add(void)
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
#ifdef WIN32
	/* Case-insensitive compare */
	return(_stricmp(i1->file,i2->file));
#else
	return(strcmp(i1->file,i2->file));
#endif
}

/*==============================================================================================================================*/
/*	General routines													*/
/*==============================================================================================================================*/

/*
	libpath		Call wfname to get the library path.
			If lib is blank then just change to dot (.).
*/
static void libpath(char vol[SIZEOF_VOL], char lib[SIZEOF_LIB], char path[COB_FILEPATH_LEN])
{
	char	l_file[SIZEOF_FILE], l_lib[SIZEOF_LIB], l_vol[SIZEOF_VOL];
	int4	mode;
	int	blank_vol, blank_lib;

	blank_vol = 0;
	blank_lib = 0;

	memset(l_file,' ',SIZEOF_FILE);
	memcpy(l_lib,lib,SIZEOF_LIB);
	memcpy(l_vol,vol,SIZEOF_VOL);

	if (is_blank(l_vol,SIZEOF_VOL))
	{
		l_vol[0] = '.';
		blank_vol = 1;
	}

	if (is_blank(l_lib,SIZEOF_LIB))
	{
		l_lib[0] = '.';
		blank_lib = 1;
	}

	if (blank_vol && blank_lib)
	{
		memset(path,' ',COB_FILEPATH_LEN);
#ifdef OLD
		path[0] = '.';
		path[1] = DDS_CHAR;
#endif
	}
	else if (blank_lib)
	{
		char	buff[MAX_TRANSLATE+1];

		memset(path,' ',COB_FILEPATH_LEN);
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
static int stattype(const char* path)
{
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
#ifdef unix
	else if (S_ISBLK (buf.st_mode))  rc = TYPE_BLK;
#endif
	else			  	 rc = TYPE_UNKNOWN;

	return(rc);
}

/*
	typefile	Try to guess at the contents of the file.
*/
static int typefile(const char* file)
{
	int	fh;
	unsigned char	header[256];
	int	header_len;
	int	i;

	fh = open(file,O_RDONLY|O_BINARY,0);
	if ( fh == -1 )
	{
		return(FILE_UNKNOWN);
	}
	header_len = read(fh,(char *)header,sizeof(header));
	close(fh);
	if (header_len < VISION_MAGIC_LEN)
	{
		return(FILE_UNKNOWN);
	}

	if (0==memcmp(header, VISION2BE_MAGIC, VISION_MAGIC_LEN) ||
	    0==memcmp(header, VISION2LE_MAGIC, VISION_MAGIC_LEN)   )
	{
		return FILE_VISION2;
	}

	if (0==memcmp(header, VISION3_MAGIC, VISION_MAGIC_LEN))
	{
		return FILE_VISION3;
	}

	if (0==memcmp(header, VISION4I_MAGIC, VISION_MAGIC_LEN))
	{
		return FILE_VISION4I;
	}

	if (0==memcmp(header, VISION4D_MAGIC, VISION_MAGIC_LEN))
	{
		return FILE_VISION4D;
	}

	if ( header[0] == 0xFE && header[1] == 0x53 )
	{
		return(FILE_CISAM);
	}

	if ( header[0] == 0x31 && header[1] == 0xFE )
	{
		return(FILE_FHISAMI);
	}

	if ( header[0] == 0x33 && header[1] == 0xFE )
	{
		return(FILE_FHISAMI);
	}

	if ( header[0] == 0x30 && header[1] == 0x7E )
	{
		return(FILE_FHISAMD);
	}

	switch(runtype(file))
	{
	case RUN_EXEC:		return(FILE_EXEC);
#ifdef WIN32
	case RUN_SHELL:		return(FILE_BATCH);
#endif
#ifdef unix
	case RUN_SHELL:		return(FILE_TEXT);
#endif
	case RUN_ACUCOBOL:	return(FILE_ACUOBJ);
	case RUN_MFINT:		return(FILE_MFINT);
	case RUN_MFGNT:		return(FILE_MFGNT);
	case RUN_PROC:		return(FILE_PROC);
	case RUN_PROCOBJ:	return(FILE_PROCOBJ);
	}

	/*
	 *	Search for non-text characters in header
	 */
	for(i=0;i<header_len;i++)
	{
		if (header[i] >= ' ' && header[i] <= '~') continue;
		if (header[i] == 0x0a ||
		    header[i] == 0x0c ||
		    header[i] == 0x0d ||
		    header[i] == 0x09   ) continue;
		return(FILE_DATA);
	}

	return(FILE_TEXT);
}

/*
	uppath		This routine removes the lowest level entry from a file path ( up to "/").
			If no leading '/' then return the current directory.
*/
static int uppath(char* path)
{
	int	i;

	for(i=strlen(path)-1; i>0; i--)
	{
		if ( path[i] == DDS_CHAR ) break;
	}

	if ( path[i] != DDS_CHAR )
	{
		getcwd(path,COB_FILEPATH_LEN);
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
#ifdef WIN32
	else if ( i == 0 )			/*  "\xxx"	==>	"C:\"	*/
	{
		getcwd(path,COB_FILEPATH_LEN);
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
#endif /* MSFS */

	return(0);
}

/*
	addpath		Create a new path by adding a file to an path
*/
static void addpath(char* newpath, const char* oldpath, const char* file)
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
static modestring(struct stat *filestat, char *mode)
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

static void putmessage(const char* message)
{
	int len;

	len = strlen(message);

	if ( MAX_SCREEN_FIELD_SIZE < len )
	{
		strcpy(screen_message, "+");
		strcat(screen_message, message + (len - MAX_SCREEN_FIELD_SIZE + 1));
	}
	else
	{
		strcpy(screen_message,message);
	}
	sound_alarm = 1;
}

static void set_native_path(const char* new_path)
{
	strcpy(native_path, new_path);
	native_path_loaded = 1;
}
static void clear_native_path(void)
{
	native_path[0] = '\0';
	native_path_loaded = 0;
}

	
static void load_native_path(void)
{
	char	buff[COB_FILEPATH_LEN];
	int4	mode;

	if (!native_path_loaded)
	{
		switch(file_style)
		{
		case STYLE_FILE:							/* Use wfname based on VOL:LIB:FILE	*/
			mode = 0;
			wfname(&mode,wang_vol,wang_lib,wang_file,buff);
			cobx2cstr(native_path,buff,COB_FILEPATH_LEN);
			break;
		case STYLE_LIB:								/* Use libpath based on VOL:LIB		*/
			libpath(wang_vol,wang_lib,buff);
			cobx2cstr(native_path,buff,COB_FILEPATH_LEN);
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

static int is_blank(const char* data, int len)
{
	int	i;

	for(i=0;i<len;i++)
	{
		if (data[i] != ' ') return(0);
	}
	return(1);
}

static void clear_file_specs(void)
{
	memset(wang_vol,' ',SIZEOF_VOL);
	memset(wang_lib,' ',SIZEOF_LIB);
	memset(wang_file,' ',SIZEOF_FILE);
	clear_native_path();
}

/*==============================================================================================================================*/
/*	Additional wsb routines.												*/
/*==============================================================================================================================*/

static void wsb_clearmessage(HWSB hWsb)
{
	char	msg[WSB_COLS];
	memset(msg,' ',sizeof(msg));
	msg[sizeof(msg)-1] = '\0';

	wsb_add_field(hWsb,2,2,FAC_PROT_BLANK,msg,strlen(msg));
}

static void wsb_clearrow(HWSB hWsb, int row)
{
	char	msg[WSB_COLS+1];
	memset(msg,' ',sizeof(msg));
	msg[sizeof(msg)-1] = '\0';
	
	wsb_add_text(hWsb, row, 1, msg);
}

static void wsb_screen_message(HWSB hWsb)
{
	if (screen_message[0])
	{
		wsb_add_field(hWsb,2,2,FAC_PROT_BOLD,screen_message,strlen(screen_message));
		screen_message[0] = '\0';
	}
}


static void wsb_build_filespec(HWSB hWsb, 
			       const char* vol, 
			       const char* lib, 
			       const char* file, 
			       const char* filepath, 
			       int style, 
			       int mod, 
			       int startrow)
{
	fac_t the_fac;
	char	l_vol[SIZEOF_VOL+1], l_lib[SIZEOF_LIB+1], l_file[SIZEOF_FILE+1],
		l_path[MAX_SCREEN_FIELD_SIZE + 1];

	if (mod)
	{
		the_fac = FAC_MOD_BOLD_UP;

		memcpy(l_vol,vol,SIZEOF_VOL);
		memcpy(l_lib,lib,SIZEOF_LIB);
		memcpy(l_file,file,SIZEOF_FILE);
		memcpy(l_path,filepath, MAX_SCREEN_FIELD_SIZE);
		l_vol[SIZEOF_VOL] = '\0';
		l_lib[SIZEOF_LIB] = '\0';
		l_file[SIZEOF_FILE] = '\0';
		l_path[MAX_SCREEN_FIELD_SIZE] = '\0';
	}
	else
	{
		the_fac = FAC_PROT_BOLD_UP;

		unloadpad(l_vol,vol,SIZEOF_VOL);
		unloadpad(l_lib,lib,SIZEOF_LIB);
		unloadpad(l_file,file,SIZEOF_FILE);
		cobx2cstr(l_path,filepath,MAX_SCREEN_FIELD_SIZE);
	}

	if (STYLE_PATH == style)
	{
		wsb_add_text (hWsb,startrow,2,"File Specification:");
	}

	if (STYLE_VOL  == style ||
	    STYLE_LIB  == style ||
	    STYLE_FILE == style   )
	{
		wsb_add_text (hWsb,startrow,COL_VOL-9,"VOLUME =");
		wsb_add_field(hWsb,startrow,COL_VOL, the_fac, l_vol, strlen(l_vol));
	}

	if (STYLE_LIB  == style ||
	    STYLE_FILE == style   )
	{
		wsb_add_text (hWsb,startrow,COL_LIB-10,"LIBRARY =");
		wsb_add_field(hWsb,startrow,COL_LIB, the_fac, l_lib, strlen(l_lib));
	}	

	if (STYLE_FILE == style)
	{
		wsb_add_text (hWsb,startrow,COL_FILE-11,"FILENAME =");
		wsb_add_field(hWsb,startrow,COL_FILE, the_fac, l_file, strlen(l_file));
	}

	the_fac =  (mod) ? FAC_MOD_BOLD:FAC_PROT_BOLD;
	wsb_add_field(hWsb, startrow+1, COL_PATH, the_fac, l_path, strlen(l_path));
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


/*
**	History:
**	$Log: mngfile.c,v $
**	Revision 1.35.2.1  2002/08/19 15:31:02  gsl
**	4403a
**	
**	Revision 1.35  2001-11-12 17:43:44-05  gsl
**	Open in O_BINARY (WIN32)
**
**	Revision 1.34  2001-11-12 16:27:39-05  gsl
**	VISION2 has 2 magic numbers (Big Endian & Little Endian)
**
**	Revision 1.33  2001-10-30 15:32:23-05  gsl
**	Changed to used defines for VISION magic numbers
**
**	Revision 1.32  2001-10-16 17:36:58-04  gsl
**	Changed the VOLUME display to be a single column instead of 2 so longer
**	volume mappings can be displayed.
**	Remove MSDOS
**
**	Revision 1.31  2000-01-07 11:07:38-05  gsl
**	Fixed load_dir_ring() and native_path to allow filenames longer the 80.
**	It was aborting previously.
**	There is still many functions that are unusable for filenames over 80
**	but this seems to have fixed the aborts.
**
**	Revision 1.30  1999-09-13 15:48:13-04  gsl
**	fix missing return code
**
**	Revision 1.29  1999-01-05 11:34:42-05  gsl
**	Fix warning
**
**	Revision 1.28  1998-10-14 11:58:10-04  gsl
**	Fixed how file type was determined on unix.
**	Was calling runtype() then isexec() now just calls runtype() because
**	runtype() calls isexec().
**
**	Revision 1.27  1998-08-03 17:08:02-04  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks.
**
**	Revision 1.26  1998-07-09 16:15:53-04  gsl
**	fix a one byte memory override in wsb_clearrow()
**
**	Revision 1.25  1998-05-14 17:13:12-04  gsl
**	add header
**
**	Revision 1.24  1998-05-14 16:57:34-04  gsl
**	Add support for vision4 files
**
**	Revision 1.23  1998-05-07 09:07:22-04  gsl
**	WIN32 with nativescreens() F10 is not usable so change PRINT file to F6.
**
**	Revision 1.22  1998-05-05 17:27:55-04  gsl
**	For WIN32 dir_cmp() sorts the files in case-insensitive order.
**
**	Revision 1.21  1998-04-10 11:08:58-04  gsl
**	Add 33 to all the pf_list's to grep the help key so that help does
**	not get recursively called.
**
**	Revision 1.20  1998-03-16 14:33:01-05  gsl
**	Fix warnings on NT
**
**	Revision 1.19  1998-03-13 17:59:59-05  gsl
**	Update for Nativescreens
**
**	Revision 1.18  1998-01-09 14:43:49-05  gsl
**	Change the way long volume mapping is truncated. Was on the right now on
**	the left.  I think its more important to see the end of the filepath.
**
**	Revision 1.17  1997-12-18 09:11:47-05  gsl
**	change wsh_protprnt(0) with screen_print()
**
**	Revision 1.16  1997-12-17 13:24:02-05  gsl
**	In the popen commands quoted all the filenames
**
**	Revision 1.15  1997-10-23 16:14:21-04  gsl
**	change vdisplay() to link_display()
**
**	Revision 1.14  1997-09-30 14:07:35-04  gsl
**	Add support for pfkeys12()
**
**	Revision 1.13  1997-07-29 14:55:16-04  gsl
**	Add the new magic number of I3 and I4 micro focus files.
**
**	Revision 1.12  1996-10-08 20:22:34-04  gsl
**	replace getenv() with weditorexe()
**
**	Revision 1.11  1996-09-10 10:54:58-07  gsl
**	Change to using wisphomedir()
**
**	Revision 1.10  1996-08-19 10:03:33-07  gsl
**	Fix a section of code for NT which used inode numbers which are
**	not reliable for directoies
**
**	Revision 1.9  1996-07-03 16:28:22-07  gsl
**	Fix includes and prototypes and reuse unix and/or msdos code for NT
**
**	Revision 1.8  1995-06-13 06:34:13-07  gsl
**	fix compiler warnings
**
 * Revision 1.7  1995/04/25  09:53:13  gsl
 * drcs state V3_3_15
 *
 * Revision 1.6  1995/04/17  11:46:32  gsl
 * drcs state V3_3_14
 *
 * Revision 1.5  1995/03/10  16:46:29  gsl
 * fixed headers
 *
**
**
*/
