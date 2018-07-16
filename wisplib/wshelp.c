static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wshelp.c
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
**
**	Purpose:	WISP command processor
**
**	Routines:	
**	wsh_help()
**	get_psb_char()
**	wsh_progprnt()
**	wsc_init()
**	wput()
**	wpcen()
**	wget()
**	ishelpactive()
**	sethelpactive()
*/

/*
**	Includes
*/

#include <stdio.h>									/* Include standard I/O definitions.	*/
#include <stdlib.h>
#include <time.h>									/* Include time definitions.		*/
#include <ctype.h>
#include <string.h>

#ifdef unix
#include <signal.h>
#include <unistd.h>
static int wsh_prtque();
#endif

#ifdef WIN32
#include <direct.h>
#endif

#include "osddefs.h"
#ifdef FUNC_UNAME
#include <sys/utsname.h>
#endif

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include video definitions.		*/
#include "vdata.h"
#include "vcap.h"									/* Include video key definitions.	*/
#include "vchinese.h"

#include "idsistd.h"
#include "wcommon.h"									/* Include COMMON defines		*/
#include "wperson.h"									/* Include personality definitions.	*/
#include "vwang.h"									/* Include wisp/video interface defs.	*/
#include "scnfacs.h"									/* Include screen FAC definitions.	*/
#include "wglobals.h"
#include "werrlog.h"
#include "wdefines.h"									/* Include bordering limits def.	*/
#include "cobrun.h"
#include "wanguid.h"
#include "wispvers.h"
#include "wisplib.h"
#include "level.h"
#include "idsisubs.h"
#include "wexit.h"
#include "sharemem.h"
#include "wispnt.h"
#include "platsubs.h"
#include "wispcfg.h"
#include "wlicense.h"
#include "machid.h"
#include "wrunconf.h"
#include "submit.h"
#include "wsb.h"
#include "screen.h"

/*
**	Structures and Defines
*/

#define ROUTINE		86000

/*
**	Globals and Externals
*/
extern int  vmap_top;
extern struct save_screen *vscrn_stack;						/* Reference to external video struct.	*/

int noprogscrn = 0;									/* No program screen flag.		*/

/*
**	Static data
*/

static int	wang_style=1;
static int	g_prog_running;
static int	g_savelevel;
static char	run_complete_message[80];

/*
**	Function Prototypes
*/

static void wsh_terminal(void); 							/* Get a terminal menu selection.	*/
static void wsh_setpsb(void);								/* Change pseudo blank characteristics.	*/
static void wsh_setcurchar(void);							/* Change cursor characteristics.	*/
static void wsh_setbackground(void);							/* Change cursor characteristics.	*/
static void wsh_copyright(void);							/* Display the copyright screen.	*/
static void wsh_info(void);								/* Display internal info		*/
static void wsh_env_info(void);								/* Display internal info		*/
static void wsh_usage_constants(void);							/* Display usage constants		*/
static void wsh_show_charset(void);
static void wsh_queuemgmnt(void);
static void wsh_submit(void);
static void wsh_usewrite(void);
static void wsh_devices(void);
static void wsh_run_program(char *program);						/* Run a program.			*/
static void wsh_utils(void); 								/* Select utilities.			*/
static void wsh_extras(void);
static void wsb_perr(HWSB hWsb, char *text);
static void wsb_build_non_wang_base(HWSB hWsb);
static int wsh_cancel(void);
static int wsh_exit(void);
static char *formated_time(char *time_string);

static void wsh_uc_loop(int flag);
static void wsh_usage_print(void)	{ wsh_uc_loop(1); }
static void wsh_usage_submit(void)	{ wsh_uc_loop(2); }
static void wsh_usage_file(void)	{ wsh_uc_loop(3); }
static int wsh_uc_file(void);								/* Set the usage constants.		*/
static int wsh_uc_print(void);								/* Set the usage constants.		*/
static int wsh_uc_submit(void);								/* Set the usage constants.		*/

#ifdef unix
static void wsh_uqueue(int type);
static void wsh_queue_print(void) 	{ wsh_uqueue(1); }
static void wsh_queue_batch(void) 	{ wsh_uqueue(0); }
#endif

static void wsh_display_errlog(void) 	{ link_display(werrpath()); }

static void wsh_print_prog_screen(void) { wsh_progprnt(1); }
static void wsh_print_cmd_screen(void)  { /* wsh_progprnt(0); */ screen_print(); }
static void wsh_run_prog_prompt(void)	{ wsh_run_program("        "); }	    
			    
#ifdef unix
static void wsh_shell(void);
#endif
#ifdef MSDOS
static void wsh_dos(void);
#endif

static void wsh_command(void)
{
#ifdef VMS
	wsh_dcl();
#endif
#ifdef unix
	wsh_shell();
#endif
#ifdef MSDOS
	wsh_dos();
#endif
}

static void wsh_manage_files(void)	
{ 
#ifdef unix
	/* This is temporary until mngfile() is converted to nativescreens() */
	if (nativescreens())
	{
		vwang_stty_save();
	}
#endif
	
	mngfile(); 

#ifdef unix
	if (nativescreens())
	{
		vwang_stty_restore();
	}
#endif
}

/*
**	WISPHELP is the cobol callable interface to the HELP screen.
*/
void WISPHELP(void)
{
	wsh_help(1);
}

int wsh_help(int prog_running)								/* Put up the shell screen.		*/
{
	HWSB	hWsb;
	int	pfval, currow, curcol;
	char temp[81];
	char allowed_pfkeys[81];							/* Allowed PF keys.			*/
	char id[32], tty_str[5];							/* Vars for extract.			*/
	char	time_temp[24];
	int	row, col1, col2, time_row, time_col;
        int 	pfkey_map[33];								/* Map for non-wang style screen pfkeys	*/
	uint4 dflags;
	int	rc=0;
	void (*func_vector[34])(void);
	int	i;
#define HELP_UTILS	(HELP_EDIT|HELP_DISPRINT|HELP_CRID)

#ifdef NOT_YET
	struct 
	{
		int	col;	/* Relative column 0-1 */
		int	row;	/* Relative rows   1-8 */
		int	pfkey;  /* Pfkey number 1-32 */
		int	dflag;
		char	*str1;
		char	*str2;
		void	(*func)(void);
	} help_func_table[] = 
	{
		0, 1, 1,  -1, 			"RUN program or procedure",	"Run a program",	wsh_run_prog_prompt,
		0, 1, 1,  -1, 			"CONTINUE Processing",		"Resume Processing",	NULL,
		0, 2, 2,  HELP_SET_FILES,	"SET File Usage Constants",	"Volume and Library Defaults", wsh_usage_file,
		0, 3, 3,  HELP_SET_PRINTER,	"SET Print Mode Defaults",	"Print parameter Defaults",    wsh_usage_print,
		0, 4, 4,  HELP_SET_PROC,	"SET Submit Procedure Defaults","Submit parameter Defaults",   wsh_usage_submit,
#ifndef VMS
		0, 5, 5,  HELP_MANAGE_FILES_LIBS,"Manage FILES/LIBRARIES",	"Manage Files",		wsh_manage_files,
#endif
#ifdef AIX
		0, 6, 6,  HELP_MANAGE_SYSTEM,	"Manage SYSTEM",		"SMIT",			wsh_devices,
#endif
#ifdef VMS
		0, 7, 7,  HELP_QUEUE_MNGMNT,	"Manage QUEUES",		"Queue Management",	wsh_queuemgmnt,
#endif
#ifdef unix
		0, 7, 7,  HELP_QUEUE_MNGMNT,	"Manage QUEUES",		"Queues",	wsh_queuemgmnt,
		0, 7, 7,  HELP_QUEUE_MNGMNT,	"Manage PRINT QUEUE",		"Print Queue",	wsh_queue_print,
		0, 7, 7,  HELP_QUEUE_MNGMNT,	"Manage BATCH QUEUE",		"Batch Queue",	wsh_queue_batch,
#endif
		0, 8, 8,  -1,			"Display Error Log",		"Display Error Log",	wsh_display_errlog,
		1, 1, 9,  HELP_UTILS,		"Use UTILITIES",		"Utilities",		wsh_utils,
		1, 1, 9,  HELP_DISPLAY,		"Use DISPLAY",			"DISPLAY utility",	wsh_utils,
		1, 2, 10, HELP_TERMINAL,	"Configure TERMINAL",		"Terminal characteristics",	wsh_terminal,
#ifdef VMS
		1, 3, 11, HELP_COMMANDS,	"Enter COMMANDS",		"Enter DCL commands",	wsh_dcl,	
#endif
#ifdef unix
		1, 3, 11, HELP_COMMANDS,	"Enter COMMANDS",		"Enter UNIX commands",	wsh_shell,
#endif
#ifdef MSDOS
		1, 3, 11, HELP_COMMANDS,	"Enter COMMANDS",		"Enter MS-DOS commands",wsh_dos,	
#endif
		1, 4, 12, HELP_SUBMIT,		"SUBMIT procedure",		"SUBMIT procedure",	wsh_submit,
		1, 5, 13, HELP_USAGE_WRITE,	"SAVE environment",		"Write usage constants",wsh_usewrite,
		1, 6, 14, HELP_PRINT_SCREEN,	"PRINT PROGRAM screen",		"Print program screen",	wsh_print_prog_screen,
		1, 7, 15, HELP_PRINT_SCREEN,	"PRINT COMMAND screen",		"Print command screen",	wsh_print_cmd_screen,
		1, 8, 16, -1,			"EXIT program",			"Exit HELP processor",	NULL,
		1, 8, 16, -1,  			"CANCEL Processing",		"CANCEL Processing",	NULL,

		-1, -1, -1, -1, NULL, NULL, NULL
	};
#endif /* NOT_YET */
	
	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	wpload();									/* Load the personality information.	*/
	get_defs(DEFAULTS_FLAGS,&dflags);						/* Get the defaults flags		*/

	if ( !(dflags & HELP_ENABLED))							/* Help is disabled.			*/
	{
		vwang_write_bell();							/* Ring the bell.			*/
		return 0;
	}

	/*
	**	Print screen and setting terminal attributes is not supported
	**	with native screens.
	*/
	if (nativescreens())
	{
		dflags &= ~(HELP_PRINT_SCREEN | HELP_TERMINAL);
	}

	/*
	**	The linklevel is incremented to prevent
	**	sideeffects of ppunlink() that can occur 
	**	when another process is spawned from help.
	*/
	g_savelevel = linklevel();

	if (opt_helpstyle == 2)
	{
		wang_style = 0;
	}
	else
	{
		wang_style = 1;
	}

	g_prog_running = prog_running;

	strcpy(allowed_pfkeys,"00");							/* Always allow return.			*/
	sethelpactive(TRUE);								/* Help is now active.			*/

	strcpy(id,longuid());								/* Get the user id.			*/
	sprintf(tty_str,"%4ld",workstation());						/* Convert value to a string.		*/
	strcpy(run_complete_message,"  ");						/* Blank the run complete message	*/


	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	if (wang_style)
	{
		time_row = 4;
		time_col = 50;

		wsb_add_text(hWsb, 1,  0, "***  WISP Command Processor  ***");
		wsb_add_text(hWsb, 2,  0, "NeoMedia Technologies Inc.");

		sprintf(temp,"Workstation %s Ready",tty_str);
		wsb_add_text(hWsb, 4,  8, temp);

		sprintf(temp,"Hello %s", id);
		wsb_add_text(hWsb, 6,  0, temp);					/* Write the ID centered		*/

		temp[0] = '\0';
#ifdef FUNC_UNAME
		{
			struct utsname unix_name;

			if ( -1 != uname(&unix_name) )
			{
				sprintf(temp, "Welcome to %s - %s", unix_name.sysname, unix_name.nodename);
			}
		}
#endif
#ifdef WIN32
		if (0==strcmp(computername(NULL),DEF_COMPUTERNAME))
		{
			strcpy(temp, "Welcome to Windows");
		}
		else
		{
			sprintf(temp, "Welcome to Windows - %s", computername(NULL));
		}
#endif
		if (!temp[0])
		{
			strcpy(temp, "Welcome to the system");
		}
		wsb_add_text(hWsb, 7,  0, temp);					/* Write the welcome message		*/
		wsb_add_text(hWsb, 9, 15, "Press (HELP) at any screen to return to this menu.");

		if (prog_running)							/* Is a program running?		*/
		{
			if (wisp_progname[0] && wisp_progname[0] != ' ')		/* We know the program name		*/
			{
				if (wisp_screen[0] && wisp_screen[0] != ' ')		/* We also know the screen name		*/
				{
					if (outctx)
					  sprintf(temp, " Active program: %s    Screen: %s ", wisp_progname, wisp_screen);
					else
					  sprintf(temp, "[Active program: %s    Screen: %s]", wisp_progname, wisp_screen);
				}
				else
				{
					if (outctx)
					  sprintf(temp, " Active program: %s ", wisp_progname);
					else
					  sprintf(temp, "[Active program: %s]", wisp_progname);
				}
			}
			else
			{
				if (WISPRUNNAME[0] != ' ')
				{
					if (outctx)
					  sprintf(temp," Active program: %8.8s ",WISPRUNNAME);
					else
					  sprintf(temp,"[Active program: %8.8s]",WISPRUNNAME);
				}
				else
				{
					strcpy(temp,"*** A program is active ***");
				}
			}
			wsb_add_text(hWsb, 12, 0, temp);
		}
		wsb_add_text(hWsb, 14, 19, "Use the function keys to select a command:");
	}
	else /* NON-Wang style HELP screen */
	{
		memset(pfkey_map,0,sizeof(pfkey_map));

		time_row = 5;
		time_col = 55;

		wsb_build_non_wang_base(hWsb);

	} /* End of screen header generation */

	if (wang_style)
	{
		row = 16;
		col1 = 5;
		col2 = 47;
	}
	else
	{
		row = 12;
		col1 = 10;
		col2 = 50;
	}

	for(i=0; i<=33; i++)
	{
		func_vector[i] = NULL;
	}
	
	if (prog_running) 	
	{
		if (wang_style)
			wsb_add_text(hWsb, row+0, col1, "(1) CONTINUE Processing");
		else
			wsb_add_menu_item(hWsb,row+0, col1, 1, "Resume Processing");
	}
	else 			
	{
		if (wang_style)
			wsb_add_text(hWsb, row+0, col1, "(1) RUN program or procedure");
		else
			wsb_add_menu_item(hWsb,row+0, col1, 1, "Run a program");

		func_vector[1] = wsh_run_prog_prompt;
	}
	strcat(allowed_pfkeys,"01");

	if (wang_style && pfkeys12())
	{
		if ((dflags & HELP_SET_FILES)   ||
		    (dflags & HELP_SET_PRINTER) ||
		    (dflags & HELP_SET_PROC))
		{
			wsb_add_text(hWsb, row+1, col1, "(2) SET Usage Constants");
			strcat(allowed_pfkeys,"02");
			func_vector[2] = wsh_usage_file;
		}

		if (1)
		{
			wsb_add_text(hWsb, row+2, col1, "(3) SHOW Error Log");
			strcat(allowed_pfkeys, "03");
			func_vector[3] = wsh_display_errlog;
		}

		if (dflags & HELP_QUEUE_MNGMNT)
		{
#if defined(unix) || defined(VMS)
#ifdef unix
			if (opt_idsiprint || opt_batchman)
#endif
			{
				wsb_add_text(hWsb, row+3,  col1, "(4) Manage QUEUES");
				strcat(allowed_pfkeys,"04");
				func_vector[4] = wsh_queuemgmnt;
			}
#endif
		}

		if (dflags & HELP_MANAGE_FILES_LIBS)
		{
#ifndef VMS
			wsb_add_text(hWsb, row+4, col1, "(5) Manage FILES/LIBRARIES");
			strcat(allowed_pfkeys, "05");
			func_vector[5] = wsh_manage_files;
#endif
		}
		if (dflags & HELP_MANAGE_SYSTEM)
		{
#ifdef AIX
			wsb_add_text(hWsb, row+5, col1, "(6) Manage SYSTEM");
			strcat(allowed_pfkeys, "06");
			func_vector[6] = wsh_devices;
#endif
		}

		if ((dflags & HELP_EDIT) || (dflags & HELP_DISPRINT) || (dflags & HELP_CRID) || (dflags & HELP_DISPLAY))
		{
			wsb_add_text(hWsb, row+0, col2, " (7) Use UTILITIES");
			strcat(allowed_pfkeys,"07");
			func_vector[7] = wsh_utils;
		}

		if (1)
		{
			wsb_add_text(hWsb, row+1, col2, " (8) Extras");
			strcat(allowed_pfkeys,"08");
			func_vector[8] = wsh_extras;
		}

		if (dflags & HELP_SUBMIT)
		{
			wsb_add_text(hWsb, row+2, col2, " (9) SUBMIT procedure");
			strcat(allowed_pfkeys,"09");
			func_vector[9] = wsh_submit;
		}

		if (prog_running && (dflags & HELP_PRINT_SCREEN) && !noprogscrn)
		{
			wsb_add_text(hWsb, row+3, col2, "(10) PRINT PROGRAM screen");
			strcat(allowed_pfkeys,"10");
			func_vector[10] = wsh_print_prog_screen;
		}

		if (dflags & HELP_PRINT_SCREEN)
		{
			wsb_add_text(hWsb, row+4, col2, "(11) PRINT COMMAND screen");
			strcat(allowed_pfkeys,"11");
			func_vector[11] = wsh_print_cmd_screen;
		}

		if (!prog_running)
		{
			wsb_add_text(hWsb, row+5, col2, "(12) EXIT program");
			strcat(allowed_pfkeys,"12");
		}
		else if (dflags & HELP_CANCEL)
		{
			wsb_add_text(hWsb, row+5, col2, "(12) CANCEL Processing");
			strcat(allowed_pfkeys,"12");
		}

	}
	else if (wang_style && !pfkeys12())
	{
		if (dflags & HELP_SET_FILES)
		{
			wsb_add_text(hWsb, row+1, col1, "(2) SET File Usage Constants");
			strcat(allowed_pfkeys,"02");
			func_vector[2] = wsh_usage_file;
		}

		if (dflags & HELP_SET_PRINTER)
		{
			wsb_add_text(hWsb, row+2, col1, "(3) SET Print Mode Defaults");
			strcat(allowed_pfkeys,"03");
			func_vector[3] = wsh_usage_print;
		}

		if (dflags & HELP_SET_PROC)
		{
			wsb_add_text(hWsb, row+3,  col1, "(4) SET Submit Procedure Defaults");
			strcat(allowed_pfkeys,"04");
			func_vector[4] = wsh_usage_submit;
		}

		if (dflags & HELP_MANAGE_FILES_LIBS)
		{
#ifndef VMS
			wsb_add_text(hWsb, row+4, col1, "(5) Manage FILES/LIBRARIES");
			strcat(allowed_pfkeys, "05");
			func_vector[5] = wsh_manage_files;
#endif
		}
		if (dflags & HELP_MANAGE_SYSTEM)
		{
#ifdef AIX
			wsb_add_text(hWsb, row+5, col1, "(6) Manage SYSTEM");
			strcat(allowed_pfkeys, "06");
			func_vector[6] = wsh_devices;
#endif
		}

		if (dflags & HELP_QUEUE_MNGMNT)
		{
#ifdef VMS
			wsb_add_text(hWsb, row+6,  col1, "(7) Manage QUEUES");
			strcat(allowed_pfkeys,"07");
			func_vector[7] = wsh_queuemgmnt;
#endif
#ifdef unix
			if (opt_idsiprint && opt_batchman)
			{
				wsb_add_text(hWsb, row+6,  col1, "(7) Manage QUEUES");
				strcat(allowed_pfkeys,"07");
				func_vector[7] = wsh_queuemgmnt;
			}
			else if (opt_idsiprint)
			{
				wsb_add_text(hWsb, row+6,  col1, "(7) Manage PRINT QUEUE");
				strcat(allowed_pfkeys,"07");
				func_vector[7] = wsh_queue_print;
			}
			else if (opt_batchman)
			{
				wsb_add_text(hWsb, row+6,  col1, "(7) Manage BATCH QUEUE");
				strcat(allowed_pfkeys,"07");
				func_vector[7] = wsh_queue_batch;
			}
#endif
		}

		if (1)
		{
			wsb_add_text(hWsb, row+7, col1, "(8) SHOW Error Log");
			strcat(allowed_pfkeys, "08");
			func_vector[8] = wsh_display_errlog;
		}

		if ((dflags & HELP_EDIT) || (dflags & HELP_DISPRINT) || (dflags & HELP_CRID) || (dflags & HELP_DISPLAY))
		{
			wsb_add_text(hWsb, row+0, col2, " (9) Use UTILITIES");
			strcat(allowed_pfkeys,"09");
			func_vector[9] = wsh_utils;
		}

		if (dflags & HELP_TERMINAL)
		{
			wsb_add_text(hWsb, row+1, col2, "(10) Configure TERMINAL");
			strcat(allowed_pfkeys,"10");
			func_vector[10] = wsh_terminal;
		}

#if defined(unix) || defined(VMS) || defined(MSDOS)
		if (dflags & HELP_COMMANDS)
		{
			wsb_add_text(hWsb, row+2, col2, "(11) Enter COMMANDS");
			strcat(allowed_pfkeys,"11");
			func_vector[11] = wsh_command;
		}
#endif /* unix || VMS || MSDOS */

		if (dflags & HELP_SUBMIT)
		{
			wsb_add_text(hWsb, row+3, col2, "(12) SUBMIT procedure");
			strcat(allowed_pfkeys,"12");
			func_vector[12] = wsh_submit;
		}

		if (dflags & HELP_USAGE_WRITE)
		{
			wsb_add_text(hWsb, row+4, col2, "(13) SAVE environment");
			strcat(allowed_pfkeys,"13");
			func_vector[13] = wsh_usewrite;
		}

		if (prog_running && (dflags & HELP_PRINT_SCREEN) && !noprogscrn)
		{
			wsb_add_text(hWsb, row+5, col2, "(14) PRINT PROGRAM screen");
			strcat(allowed_pfkeys,"14");
			func_vector[14] = wsh_print_prog_screen;
		}

		if (dflags & HELP_PRINT_SCREEN)
		{
			wsb_add_text(hWsb, row+6, col2, "(15) PRINT COMMAND screen");
			strcat(allowed_pfkeys,"15");
			func_vector[15] = wsh_print_cmd_screen;
		}

		if (!prog_running)
		{
			wsb_add_text(hWsb, row+7, col2, "(16) EXIT program");
			strcat(allowed_pfkeys,"16");
		}
		else if (dflags & HELP_CANCEL)
		{
			wsb_add_text(hWsb, row+7, col2, "(16) CANCEL Processing");
			strcat(allowed_pfkeys,"16");
		}
	}
	else /* !wang_style */
	{
		if (dflags & HELP_SET_FILES)
		{
			wsb_add_menu_item(hWsb, row+1, col1, 2, "Volume and Directory Defaults");
			strcat(allowed_pfkeys,"02");
			func_vector[2] = wsh_usage_file;
		}

		if (dflags & HELP_SET_PRINTER)
		{
			wsb_add_menu_item(hWsb, row+2, col1, 3, "Print parameter Defaults");
			strcat(allowed_pfkeys,"03");
			func_vector[3] = wsh_usage_print;
		}

		if (dflags & HELP_SET_PROC)
		{
			wsb_add_menu_item(hWsb, row+3, col1, 4, "Submit parameter Defaults");
			strcat(allowed_pfkeys,"04");
			func_vector[4] = wsh_usage_submit;
		}
	
		if (dflags & HELP_MANAGE_FILES_LIBS)
		{
#ifndef VMS
			wsb_add_menu_item(hWsb, row+4, col1, 5, "Manage Files");
			strcat(allowed_pfkeys, "05");
			func_vector[5] = wsh_manage_files;
#endif
		}

		if (dflags & HELP_MANAGE_SYSTEM)
		{
#ifdef AIX
			wsb_add_menu_item(hWsb, row+5, col1, 6, "SMIT");
			strcat(allowed_pfkeys, "06");
			func_vector[6] = wsh_devices;
#endif
		}

		if (dflags & HELP_QUEUE_MNGMNT)
		{
#ifdef VMS
			wsb_add_menu_item(hWsb, row+6, col1, 7, "Queue Management");
			strcat(allowed_pfkeys,"07");
			func_vector[7] = wsh_queuemgmnt;
#endif
#ifdef unix
			if (opt_idsiprint && opt_batchman)
			{
				wsb_add_menu_item(hWsb, row+6, col1, 7, "Queues");
				strcat(allowed_pfkeys,"07");
				func_vector[7] = wsh_queuemgmnt;
			}
			else if (opt_idsiprint)
			{
				wsb_add_menu_item(hWsb, row+6, col1, 7, "Print Queue");
				strcat(allowed_pfkeys,"07");
				func_vector[7] = wsh_queue_print;
			}
			else if (opt_batchman)
			{
				wsb_add_menu_item(hWsb, row+6, col1, 7, "Batch Queue");
				strcat(allowed_pfkeys,"07");
				func_vector[7] = wsh_queue_batch;
			}
#endif
		}

		if (1)
		{
			wsb_add_menu_item(hWsb, row+7, col1, 8, "SHOW Error Log");
			strcat(allowed_pfkeys, "08");
			func_vector[8] = wsh_display_errlog;
		}

		if ((dflags & HELP_EDIT) || (dflags & HELP_DISPRINT) || (dflags & HELP_CRID) || (dflags & HELP_DISPLAY))
		{
			wsb_add_menu_item(hWsb, row+0, col2, 9, "Utilities");
			strcat(allowed_pfkeys,"09");
			func_vector[9] = wsh_utils;
		}

		if (dflags & HELP_TERMINAL)
		{
			wsb_add_menu_item(hWsb, row+1, col2, 10, "Terminal characteristics");
			strcat(allowed_pfkeys,"10");
			func_vector[10] = wsh_terminal;
		}

#if defined(unix) || defined(VMS) || defined(MSDOS)
		if (dflags & HELP_COMMANDS)
		{
#ifdef VMS
			wsb_add_menu_item(hWsb, row+2, col2, 11, "Enter DCL commands");
#endif
#ifdef unix
			wsb_add_menu_item(hWsb, row+2, col2, 11, "Enter UNIX commands");
#endif
#ifdef MSDOS
			wsb_add_menu_item(hWsb, row+2, col2, 11, "Enter MS-DOS commands");
#endif	
			strcat(allowed_pfkeys,"11");
			func_vector[11] = wsh_command;
		}
#endif /* unix || VMS || MSDOS */

		if (dflags & HELP_SUBMIT)
		{
			wsb_add_menu_item(hWsb, row+3, col2, 12, "SUBMIT procedure");
			strcat(allowed_pfkeys,"12");
			func_vector[12] = wsh_submit;
		}

		if (dflags & HELP_USAGE_WRITE)
		{
			wsb_add_menu_item(hWsb, row+4, col2, 13, "Write usage constants");
			strcat(allowed_pfkeys,"13");
			func_vector[13] = wsh_usewrite;
		}

		if (prog_running && (dflags & HELP_PRINT_SCREEN) && !noprogscrn)
		{
			wsb_add_menu_item(hWsb, row+5, col2, 14, "Print program screen");
			strcat(allowed_pfkeys,"14");
			func_vector[14] = wsh_print_prog_screen;
		}

		if (dflags & HELP_PRINT_SCREEN)
		{
			wsb_add_menu_item(hWsb, row+6, col2, 15, "Print command screen");
			strcat(allowed_pfkeys,"15");
			func_vector[15] = wsh_print_cmd_screen;
		}

		if (!prog_running)
		{
			wsb_add_menu_item(hWsb, row+7, col2, 16, "Exit HELP processor");
			strcat(allowed_pfkeys,"16");
		}
		else if (dflags & HELP_CANCEL)
		{
			wsb_add_menu_item(hWsb, row+7, col2, 16, "CANCEL Processing");
			strcat(allowed_pfkeys,"16");
		}

	}


	if (!wang_style)
	{
		strcpy(allowed_pfkeys,"00");						/* Only ENTER is allowed		*/
	}

	strcat(allowed_pfkeys,"282930313233X");						/* Add hidden keys and terminate list 	*/

	func_vector[28] = wsh_show_charset;
	func_vector[29] = wsh_env_info;
	func_vector[30] = wsh_usage_constants;
	func_vector[31] = wsh_info;
	func_vector[32] = wsh_copyright;

	for(;;)
	{
		wsb_add_text(hWsb, time_row, time_col, formated_time(time_temp) );
		wsb_display_and_read(hWsb, allowed_pfkeys, &pfval, &currow, &curcol);
		
#ifdef OLD
		do
		{
			char function,lines,term[2],no_mod[2];

			/*
			**	This loop updates the clock until a pfkey gets pressed
			*/

			wsb_add_text(hWsb, time_row, time_col, formated_time(time_temp) );

			vwang_timeout(30);						/* Set the timer (30 secs).		*/

			lines = WSB_ROWS;						/* Use full screen.			*/
			function = DISPLAY_AND_READ;					/* Use DISPLAY_AND_READ.		*/
			vwang(&function,scrn,&lines,allowed_pfkeys,term,no_mod);	/* Call Wang emulation to fill screen.	*/

			vwang_timeout(0);						/* Cancel timer				*/

			pfval = (term[0] - '0')*10 + (term[1] - '0');
			
		} while ( AID_UNLOCKED == vwang_aid());

#endif /* OLD */

		if (pfval >= 1 && pfval <= 32)
		{
			if (func_vector[pfval])
			{
				/*
				**	If there is a function for this pfkey then call it.
				*/
				(*func_vector[pfval])();
			}
			else if (1 == pfval)
			{
				/*
				**	"CONTINUE Processing" 
				*/
				rc = 1;
				break;
			}
			else if ((!wang_style && 16 == pfval) ||
				 (wang_style && 16 == pfval && !pfkeys12()) ||
				 (wang_style && 12 == pfval &&  pfkeys12())   )
			{
				if (prog_running)
				{
					if (wsh_cancel())
					{
						LINKCOMPCODE = 16;
						setretcode("016");
						wexit(16L);
					}
				}
				else
				{
					if (wsh_exit())
					{
						rc = 0;
						break;
					}
				}
			}
		}
	
		if ( wang_style && !prog_running )
		{
			/*
			**	Update the screen with the run complete message.
			*/
			strcpy(temp,run_complete_message);
			memset(run_complete_message,' ',strlen(temp));			/* Blank out the run complete message	*/
			wsb_add_text(hWsb, 12, 0, temp);
		}
	}

	wsb_delete(hWsb);
	
	if (linklevel() != g_savelevel) setlevel(g_savelevel);				/* Restore the link-level		*/

	sethelpactive(FALSE);								/* Help no longer active.		*/
	return rc;
}

static void wsh_terminal(void) 								/* Get a terminal menu selection.	*/
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;
	char 	okkeys[81];
	uint4 	dflags;
	int	col;

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	if (wang_style)
	{
		col = 21;
		
		wsb_add_text(hWsb,  1,  0, "***  Configure Terminal ***");
		wsb_add_text(hWsb, 24,  0, "Press (HELP) to return to the command processor, (1) to exit.");
		strcpy(okkeys,"01");							/* No keys ok yet.			*/
	}
	else
	{
		col = 8;

		wsb_build_non_wang_base(hWsb);
		wsb_add_menu_item(hWsb, 11, col, 1, "Return to MAIN HELP menu");
	}


	get_defs(DEFAULTS_FLAGS,&dflags);						/* Get the defaults flags		*/

	if (dflags & HELP_SETPSB)
	{
		if (wang_style)
			wsb_add_text(hWsb,  8, col, "(2) PSEUDO blank characteristics.");
		else
			wsb_add_menu_item(hWsb,  12, col, 2, "PSEUDO blank characteristics");
		strcat(okkeys,"02");
	}

	if (dflags & HELP_SETCURCHAR)
	{
		if (wang_style)
			wsb_add_text(hWsb, 10, col, "(3) CURSOR characteristics.");
		else
			wsb_add_menu_item(hWsb, 13, col, 3, "CURSOR characteristics");
		strcat(okkeys,"03");
	}

	if (dflags & HELP_SCREEN)
	{
		if (wang_style)
			wsb_add_text(hWsb, 12, col, "(4) SCREEN characteristics.");
		else
			wsb_add_menu_item(hWsb, 14, col, 4, "SCREEN characteristics.");
		strcat(okkeys,"04");
	}

	strcat(okkeys,"111533X");

	if (!wang_style)
		strcpy(okkeys,"00111533X");

	for(;;)
	{
		wsb_display_and_read(hWsb, okkeys, &pfkey, &currow, &curcol);

		switch(pfkey)
		{
		case 1:
		case 33:
			wsb_delete(hWsb);
			return;

		case 2:
			wsh_setpsb();						/* Set the pseudo blank characteristics.*/
			break;
		case 3:
			wsh_setcurchar();					/* Set the cursor characteristics.	*/
			break;
		case 4:
			wsh_setbackground();					/* Set the cursor background.		*/
			break;
			
		case 11:
		case 15:
			wsh_print_cmd_screen();
			break;
		
		case 0:
		default:
			break;
		}
	}
}

/*
	get_psb_char:	Get pseudo blank character.
			This routine is passed a selection character (def_select) and returns the pseudo blank char (pb_char)
			and the real selection char (pb_select).
			It knowns how to handle converison from old PERSONALITY files that stored the pseudo blank char
			instead of the selection char.

	char	def_select;		The selection character "B1234"
	char	*pb_char;		The Pseudo blank char (returned)
	char	*pb_select;		The selection char  (returned)

*/
void get_psb_char(char def_select,char *pb_char, char *pb_select)
{
	pb_select[0] = def_select;

	switch(def_select)								/* Initialize fields from personality.	*/
	{
	case 'B':
		pb_char[0] = ' ';
		break;
	case '1':
		pb_char[0] = PBLANK1;
		break;
	case '2':
		pb_char[0] = PBLANK2;
		break;
	case '3':
		pb_char[0] = PBLANK3;
		break;
	case '4':
		pb_char[0] = PBLANK4;
		break;
	default:									/* OLD personality stored char itself	*/
		pb_char[0] = def_select;
		if      (def_select == PBLANK1) pb_select[0] = '1';		/* Character is a diamond.		*/
		else if (def_select == PBLANK2) pb_select[0] = '2';		/* Character is a degree symbol.	*/
		else if (def_select == PBLANK3) pb_select[0] = '3';		/* Character is a period.		*/
		else if (def_select == PBLANK4) pb_select[0] = '4';		/* Character is a square.		*/
		else 				pb_select[0] = 'B';		/* Character is a blank.		*/
		break;
	}
}

static void wsh_setpsb(void)								/* Change pseudo blank characteristics.	*/
{
	char screen[WSB_LENGTH+1];							/* Pointer to working screen routine.	*/
	char function, lines, term[2], no_mod[2];					/* Working variables.			*/
	char pb_char[4], pb_rend[4], pb_select[4];	
	int pb_chset, pb_r;
	int valid, i, changed;
	char	def_psb_select;
	int4	def_psb_charset;
	int4	def_psb_rendition;

	wsc_init(screen,0,0);								/* Initialize the screen layout.	*/

	wput(screen, 1,15,FAC_DEFAULT_TEXT,"*** Change the pseudo blank characteristics ***");
	wput(screen, 5, 2,FAC_DEFAULT_TEXT,"Pseudo blank character      :");
	wput(screen, 6, 2,FAC_DEFAULT_TEXT,"Using pseudo blank rendition:");
	wput(screen,10,10,FAC_DEFAULT_TEXT,"Character Options");
	wput(screen,10,40,FAC_DEFAULT_TEXT,"Rendition Options");
	wput(screen,12,10,FAC_DEFAULT_TEXT,"B    blank ");
	wput(screen,13,10,FAC_DEFAULT_TEXT,"1");
	wput(screen,14,10,FAC_DEFAULT_TEXT,"2");
	wput(screen,15,10,FAC_DEFAULT_TEXT,"3");
	wput(screen,16,10,FAC_DEFAULT_TEXT,"4");
	wput(screen,12,40,FAC_DEFAULT_TEXT,"N  -  NORMAL");
	wput(screen,13,40,FAC_DEFAULT_TEXT,"U  -  UNDERSCORE");
	wput(screen,14,40,FAC_DEFAULT_TEXT,"R  -  REVERSE");
	wput(screen,15,40,FAC_DEFAULT_TEXT,"B  -  Both UNDERSCORE and REVERSE");
	wput(screen,18,2,FAC_DEFAULT_TEXT,"The pseudo blank character will");
	wput(screen,19,2,FAC_DEFAULT_TEXT,"fill the modifiable fields with");
	wput(screen,20,2,FAC_DEFAULT_TEXT,"the indicated character.");
	wput(screen,18,37,FAC_DEFAULT_TEXT,"The indicated rendition will be turned");
	wput(screen,19,37,FAC_DEFAULT_TEXT,"on for modifiable fields and display");
	wput(screen,20,37,FAC_DEFAULT_TEXT,"with the pseudo blank character.");
	wput(screen,24,2,FAC_DEFAULT_TEXT,"Change the information as appropriate and press (ENTER), (1) to exit.");

	get_defs(DEFAULTS_PSB_CHAR,&def_psb_select);
	get_defs(DEFAULTS_PSB_SET,&def_psb_charset);
	get_defs(DEFAULTS_PSB_REN,&def_psb_rendition);

	get_psb_char(def_psb_select,pb_char,pb_select);
	pb_select[1] = '\0';								/* Null terminate the string.		*/
	pb_char[1] = '\0';								/* Null terminate the string.		*/
	pb_chset = def_psb_charset;
	pb_r = def_psb_rendition;
	if (pb_r == 2)  pb_rend[0] = 'U';						/* Rendition is UNDERLINE.		*/
	else if (pb_r == 8)  pb_rend[0] = 'R';						/* Rendition is REVERSE.		*/
	else if (pb_r == 10) pb_rend[0] = 'B';						/* Both UNDERSCORE and REVERSE.		*/
	else pb_rend[0] = 'N';								/* Rendition is Normal.			*/
	pb_rend[1] = '\0';								/* Null terminate the string.		*/

	wput(screen,5,40,FAC_DEFAULT_FIELD,pb_select);					/* Print correct char set from choices.	*/
	wput(screen, 6,40,FAC_DEFAULT_FIELD,pb_rend);					/* Print correct rendition from choices.*/

	function = WRITE_ALL;								/* Use display full screen function.	*/
	lines = WSB_ROWS;
	vwang(&function,screen,&lines,"0001X",term,no_mod);				/* Call Wang emulation to fill screen.	*/
	vcharset(VCS_GRAPHICS);

	vmove(12,15);	vprint("%c", PBLANK1);
	vmove(13,15);	vprint("%c", PBLANK2);
	vmove(14,15);	vprint("%c", PBLANK3);
	vmove(15,15);	vprint("%c", PBLANK4);

	vmove(4,33);
	vprint("%c", *pb_char);								/* Print pseudo blank character.	*/
	vcharset(VCS_DEFAULT);
	vwang_set_synch(FALSE);								/* Vprint sets the flag to TRUE which I	*/
											/* don't want so reset to FALSE.	*/
	function = READ_ALTERED;							/* Use read altered function.		*/
	lines = WSB_ROWS;
	
	valid = TRUE;									/* Assume it starts with valid values	*/
	changed = FALSE;								/* Nothing has changed yet		*/

	for(;;)										/* Repeat until data input is valid.	*/
	{
		vwang(&function,screen,&lines,"000115X",term,no_mod);			/* Call Wang emulation to fill screen.	*/
		function = DISPLAY_AND_READ_ALTERED;					/* Set up for second time thru loop	*/

		if ((term[0] == '0') && (term[1] == '1'))				/* If PF01 press then abort		*/
		{
			changed = FALSE;
			break;
		}

		if ((term[0] == '1') && (term[1] == '5'))				/* If PF15 - PRINT screen		*/
		{
			/* wsh_print_cmd_screen(); can not use because of video stuff */
			wsh_progprnt(0);
			continue;
		}

		if (no_mod[0] == 'M')
		{
			changed = TRUE;							/* Values have been changed		*/
		}
		else
		{
			changed = FALSE;						/* Values have been changed		*/
		}

		if (changed)
		{
			valid = TRUE;							/* Assume valid values			*/

			wget(screen, 5,40,pb_select);					/* Input the pseudo blank option.	*/
			wget(screen, 6,40,pb_rend);					/* Input the rendition.			*/

			pb_select[0] = toupper(pb_select[0]);				/* Make sure the char is uppercase.	*/
			if ((i = strpos("B1234",pb_select)) < 0)			/* Was a valid char option given?	*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wput(screen, 5,40,FAC_ERROR_FIELD,pb_select);		/* Make it blink.			*/
				screen[2] = 0;						/* Reset cursor position.		*/
				screen[3] = 0;
			}
			else
			{
				pb_chset = 16;

				if      (pb_select[0] == '1')  pb_char[0] = PBLANK1;	/* Assign default pseudo chars		*/
				else if (pb_select[0] == '2')  pb_char[0] = PBLANK2;
				else if (pb_select[0] == '3')  pb_char[0] = PBLANK3;
				else if (pb_select[0] == '4')  pb_char[0] = PBLANK4;
  				else
				{
					pb_chset = 0;
					pb_char[0] = ' ';				/* Default pseudo char as a blank.	*/
				}                                                     
				pb_char[1] = '\0';					/* Null terminate the string.		*/
			}
			pb_rend[0] = toupper(pb_rend[0]);				/* Make sure the char is uppercase.	*/
			if ((i = strpos("NURB",pb_rend)) < 0)				/* Was a valid rendition given?		*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wput(screen, 6,40,FAC_ERROR_FIELD,pb_rend);		/* Make it blink.			*/
				screen[2] = 0;						/* Reset cursor position.		*/
				screen[3] = 0;
			}
			else
			{
				if (pb_rend[0] == 'U')  pb_r = 2;			/* Assign UNDERSCORE rendition.		*/
				else if (pb_rend[0] == 'R')  pb_r = 8;			/* Assign REVERSE rendition.		*/
				else if (pb_rend[0] == 'B')  pb_r = 10;			/* Assign both UNDERSCORE and REVERSE.	*/
				else pb_r = 0;						/* Default Normal rendition.		*/
			}
		}

		if (valid) break;
	}

	if (changed && valid)								/* If values changed and are valid	*/
	{
		def_psb_select = pb_select[0];
		def_psb_charset = pb_chset;
		def_psb_rendition = pb_r;
		set_defs(DEFAULTS_PSB_CHAR,&def_psb_select);
		set_defs(DEFAULTS_PSB_SET,&def_psb_charset);
		set_defs(DEFAULTS_PSB_REN,&def_psb_rendition);
		save_defaults();							/* Write out the personality info.	*/
	}
	vwang_set_synch(TRUE);								/* Synch now required.			*/
}

static void wsh_setcurchar(void)							/* Change cursor characteristics.	*/
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;

	int4 automove, autotab, mp_cursor;
	char amfl[2], atfl[2],mpfl[2];
	int valid, changed;
	char	temp[20];

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	wsb_add_text(hWsb, 1, 0,"*** Change the cursor characteristics ***");
	wsb_add_text(hWsb, 5, 5,"        Auto tab flag = ");
	wsb_add_text(hWsb, 7, 5,"       Auto move flag = ");
	wsb_add_text(hWsb, 9, 5,"Menu pick cursor flag = ");
	wsb_add_text(hWsb,24, 0,"Change the information as appropriate and press (ENTER), (1) to exit.");

	get_defs(DEFAULTS_AUTOTAB,&autotab);						/* Initialize fields from personality.	*/
	get_defs(DEFAULTS_AUTOMOVE,&automove);
	get_defs(DEFAULTS_MP_CURSOR,&mp_cursor);

	if (autotab) strcpy(atfl,"Y");							/* Init the string value.		*/
	else	      strcpy(atfl,"N");

	if (automove) strcpy(amfl,"Y");			
	else	      strcpy(amfl,"N");

	if (mp_cursor) strcpy(mpfl,"Y");
	else	      strcpy(mpfl,"N");

	wsb_add_field(hWsb, 5,30,FAC_DEFAULT_FIELD,atfl,1);
	wsb_add_field(hWsb, 7,30,FAC_DEFAULT_FIELD,amfl,1);
	wsb_add_field(hWsb, 9,30,FAC_DEFAULT_FIELD,mpfl,1);

	valid = TRUE;									/* Assume it starts with valid values	*/
	changed = FALSE;								/* Nothing has changed yet		*/

	for(;;)										/* Repeat until data input is valid.	*/
	{
		wsb_display_and_read(hWsb, "0001111533X", &pfkey, &currow, &curcol);

		if (1==pfkey || 33==pfkey)						/* If PF01 press then abort		*/
		{
			changed = FALSE;
			break;
		}

		if (11==pfkey || 15==pfkey)						/* If PF15 - PRINT screen		*/
		{
			wsh_print_cmd_screen();
			continue;
		}

		wsb_get_field(hWsb, 5,30,temp,1);
		if (temp[0] != atfl[0])
		{
			atfl[0] = temp[0];
			changed = TRUE;
		}
		
		wsb_get_field(hWsb, 7,30,temp,1);
		if (temp[0] != amfl[0])
		{
			amfl[0] = temp[0];
			changed = TRUE;
		}

		wsb_get_field(hWsb, 9,30,temp,1);
		if (temp[0] != mpfl[0])
		{
			mpfl[0] = temp[0];
			changed = TRUE;
		}

		if (changed)
		{
			int pos;

			valid = TRUE;							/* Assume valid values			*/

			if ((pos = strpos("YNTF10",atfl)) < 0)				/* Was a valid logical given?		*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wsb_add_field(hWsb, 5,30,FAC_ERROR_FIELD,atfl,1);	/* Make it blink.			*/
				currow = 5;
				curcol = 30;
			}
			else
			{
				if (pos == 0 || pos == 2 || pos == 4) autotab = TRUE;	/* Set auto_tab_flag to TRUE.		*/
				else autotab = FALSE;
			}
			if ((pos = strpos("YNTF10",amfl)) < 0)				/* Was a valid logical given?		*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wsb_add_field(hWsb, 7,30,FAC_ERROR_FIELD,amfl,1);	/* Make it blink.			*/
				currow = 7;
				curcol = 30;
			}
			else
			{
				if (pos == 0 || pos == 2 || pos == 4) automove = TRUE;	/* Set auto_move_flag to TRUE.		*/
				else automove = FALSE;
			}  
			if ((pos = strpos("YNTF10",mpfl)) < 0)				/* Was a valid logical given?		*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wsb_add_field(hWsb, 9,30,FAC_ERROR_FIELD,mpfl,1);	/* Make it blink.			*/
				currow = 9;
				curcol = 30;
			}
			else
			{
				if (pos == 0 || pos == 2 || pos == 4) mp_cursor = TRUE;	/* Set mp_cursor to TRUE.		*/
				else mp_cursor = FALSE;
			}  
		}
                                                                                                 
		if (valid) break;
	}

	wsb_delete(hWsb);

	if (changed && valid)								/* If values changed and are valid	*/
	{
		set_defs(DEFAULTS_AUTOTAB,&autotab);
		set_defs(DEFAULTS_AUTOMOVE,&automove);
		set_defs(DEFAULTS_MP_CURSOR,&mp_cursor);
		save_defaults();							/* Write out the personality info.	*/
	}
}

static void wsh_setbackground(void)							/* Change cursor characteristics.	*/
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;

	int4 bgchange, bgcolor, excolor;
	char bgch[2], bgco[2], exco[2];
	int valid, changed;

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	wsb_add_text(hWsb, 1, 0,"*** Screen Background Selection ***");
	wsb_add_text(hWsb, 5, 5,"    Change background =    (No means leave the background as it is.)");
	wsb_add_text(hWsb, 7, 5,"          Select grey =    (No means select a black background.)");
	wsb_add_text(hWsb, 9, 5,"            Exit grey =    (No means select black on exit.)");
	wsb_add_text(hWsb,24, 0,"Change the information as appropriate and press (ENTER), (1) to exit.");

	get_defs(DEFAULTS_BGCHANGE,&bgchange);						/* Initialize fields from personality.	*/
	get_defs(DEFAULTS_BGCOLOR,&bgcolor);
	get_defs(DEFAULTS_EXCOLOR,&excolor);

	if (bgchange) strcpy(bgch,"Y");							/* Init the string value.		*/
	else	      strcpy(bgch,"N");

	if (bgcolor) strcpy(bgco,"Y");			
	else	     strcpy(bgco,"N");

	if (excolor) strcpy(exco,"Y");
	else         strcpy(exco,"N");

	wsb_add_field(hWsb, 5,30,FAC_DEFAULT_FIELD,bgch,1);
	wsb_add_field(hWsb, 7,30,FAC_DEFAULT_FIELD,bgco,1);
	wsb_add_field(hWsb, 9,30,FAC_DEFAULT_FIELD,exco,1);

	valid = TRUE;									/* Assume it starts with valid values	*/
	changed = FALSE;								/* Nothing has changed yet		*/

	for(;;)										/* Repeat until data input is valid.	*/
	{
		wsb_display_and_read(hWsb, "0001111533X", &pfkey, &currow, &curcol);

		if (1==pfkey || 33==pfkey)						/* If PF01 press then abort		*/
		{
			changed = FALSE;
			break;
		}

		if (11==pfkey || 15==pfkey)						/* If PF15 - PRINT screen		*/
		{
			wsh_print_cmd_screen();
			continue;
		}

		wsb_get_field(hWsb, 5,30,bgch,1);
		wsb_get_field(hWsb, 7,30,bgco,1);
		wsb_get_field(hWsb, 9,30,exco,1);

		changed = TRUE;								/* Values have been changed		*/

		if (changed)
		{
			int pos;

			valid = TRUE;							/* Assume we're valid.			*/

			if ((pos = strpos("YNTF10",bgch)) < 0)				/* Was a valid logical given?		*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wsb_add_field(hWsb, 5,30,FAC_ERROR_FIELD,bgch,1);	/* Make it blink.			*/
				currow = 5;
				curcol = 30;
			}
			else
			{
				if (pos == 0 || pos == 2 || pos == 4) bgchange = TRUE;	/* Set change flag to TRUE.		*/
				else bgchange = FALSE;
			}

			if ((pos = strpos("YNTF10",bgco)) < 0)				/* Was a valid logical given?		*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wsb_add_field(hWsb, 7,30,FAC_ERROR_FIELD,bgco,1);	/* Make it blink.			*/
				currow = 7;
				curcol = 30;
			}
			else
			{
				if (pos == 0 || pos == 2 || pos == 4) bgcolor = TRUE;	/* Set color to grey.			*/
				else bgcolor  = FALSE;
			}

			if ((pos = strpos("YNTF10",exco)) < 0)				/* Was a valid logical given?		*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wsb_add_field(hWsb, 9,30,FAC_ERROR_FIELD,exco,1);	/* Make it blink.			*/
				currow = 9;
				curcol = 30;
			}
			else
			{
				if (pos == 0 || pos == 2 || pos == 4) excolor = TRUE;	/* Set color to grey.			*/
				else excolor  = FALSE;
			}  
		}                                                                                                                 
		if (valid) break;
	}

	wsb_delete(hWsb);

	if (changed && valid)								/* If values changed and are valid	*/
	{
		set_defs(DEFAULTS_BGCHANGE,&bgchange);
		set_defs(DEFAULTS_BGCOLOR,&bgcolor);
		set_defs(DEFAULTS_EXCOLOR,&excolor);
		save_defaults();							/* Write out the personality info.	*/
		if (bgchange)
		{
			if (excolor) vonexit(NORMALIZE|CLEAR_SCREEN|VSCREEN_LIGHT);
			else         vonexit(NORMALIZE|CLEAR_SCREEN|VSCREEN_DARK);
		}
	}
}


static void wsh_uc_loop(int flag)
{
	while(1==flag || 2==flag || 3==flag)
	{
		switch(flag)
		{
		case 1:	flag = wsh_uc_print();	break;
		case 2:	flag = wsh_uc_submit();	break;
		case 3:	flag = wsh_uc_file();	break;
		}
	}
}

static int wsh_uc_file(void)								/* Set the usage constants.		*/
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;
	char	def_inlib[9], 	def_invol[7];
	char	def_outlib[9], 	def_outvol[7];
	char	def_runlib[9], 	def_runvol[7];
	char	def_spoolib[9], def_spoolvol[7];
	char	def_worklib[9], def_workvol[7];
	int	rc = 0;

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	wsb_add_text(hWsb, 1,20,"*** Set Volume and Library Defaults ***");
	wsb_add_text(hWsb, 5, 2,"Default location (Library and Volume) of files, assuming that these");
	wsb_add_text(hWsb, 6, 2,"values are not provided by the program itself or logically reassigned:");

	wsb_add_text(hWsb, 9, 8,"Input files:            INLIB   =               INVOL    =");
	wsb_add_text(hWsb,11, 8,"Output files:           OUTLIB  =               OUTVOL   =");
	wsb_add_text(hWsb,13, 8,"User programs:          RUNLIB  =               RUNVOL   =");
	wsb_add_text(hWsb,15, 8,"Spooled print files:    SPOOLLIB=               SPOOLVOL =");
	wsb_add_text(hWsb,17, 8,"Work/Temporary files:   WORKLIB =               WORKVOL  =");

	get_defs(DEFAULTS_IL,def_inlib);		get_defs(DEFAULTS_IV,def_invol);
	get_defs(DEFAULTS_OL,def_outlib);		get_defs(DEFAULTS_OV,def_outvol);
	get_defs(DEFAULTS_RL,def_runlib);		get_defs(DEFAULTS_RV,def_runvol);
	get_defs(DEFAULTS_SL,def_spoolib);		get_defs(DEFAULTS_SV,def_spoolvol);
	get_defs(DEFAULTS_WL,def_worklib);		get_defs(DEFAULTS_WV,def_workvol);

	def_inlib[8]    = (char)0;							/* Make sure the fields are nulled out.	*/
	def_outlib[8]   = (char)0;
	def_runlib[8]   = (char)0;
	def_spoolib[8]  = (char)0;
	def_worklib[8]  = (char)0;
	def_invol[6]    = (char)0;
	def_outvol[6]   = (char)0;
	def_runvol[6]   = (char)0;
	def_spoolvol[6] = (char)0;
	def_workvol[6]  = (char)0;

	wsb_add_field(hWsb, 9,43,FAC_DEFAULT_FIELD,def_inlib,    SIZEOF_LIB);  
	wsb_add_field(hWsb, 9,68,FAC_DEFAULT_FIELD,def_invol,    SIZEOF_VOL);

	wsb_add_field(hWsb,11,43,FAC_DEFAULT_FIELD,def_outlib,   SIZEOF_LIB);  
	wsb_add_field(hWsb,11,68,FAC_DEFAULT_FIELD,def_outvol,   SIZEOF_VOL);

	wsb_add_field(hWsb,13,43,FAC_DEFAULT_FIELD,def_runlib,   SIZEOF_LIB);  
	wsb_add_field(hWsb,13,68,FAC_DEFAULT_FIELD,def_runvol,   SIZEOF_VOL);

	wsb_add_field(hWsb,15,43,FAC_DEFAULT_FIELD,def_spoolib,  SIZEOF_LIB);  
	wsb_add_field(hWsb,15,68,FAC_DEFAULT_FIELD,def_spoolvol, SIZEOF_VOL);

	wsb_add_text(hWsb,17,43,  def_worklib);  
	wsb_add_field(hWsb,17,68,FAC_DEFAULT_FIELD,def_workvol,  SIZEOF_VOL);

	wsb_add_text(hWsb,20,8,"Change the information as appropriate and press (ENTER)");
	wsb_add_text(hWsb,22,8,"or Press  (1) Set Print Mode Defaults");
	wsb_add_text(hWsb,23,8,"          (2) Set Submit Procedure Defaults");
	wsb_add_text(hWsb,24,8,"       (HELP) Return to the Command Processor");

	for(;;)
	{
		wsb_display_and_read(hWsb, "000102111533X", &pfkey, &currow, &curcol);
                                                       
		if (1 == pfkey)
		{
			rc = 1;
			break;
		}

		if (2 == pfkey)
		{
			rc = 2;
			break;
		}
		
		if (11 == pfkey || 15 == pfkey)
		{
			wsh_print_cmd_screen();						/* PRINT screen		*/
		}
		else
		{
			break;
		}
		
	}
	
	if ( 0 == pfkey )
	{
		/*
		**	Pressed ENTER so get new values
		*/

		wsb_get_field(hWsb, 9,43, def_inlib,    SIZEOF_LIB);  
		wsb_get_field(hWsb, 9,68, def_invol,    SIZEOF_VOL);

		wsb_get_field(hWsb,11,43, def_outlib,   SIZEOF_LIB);  
		wsb_get_field(hWsb,11,68, def_outvol,   SIZEOF_VOL);

		wsb_get_field(hWsb,13,43, def_runlib,   SIZEOF_LIB);  
		wsb_get_field(hWsb,13,68, def_runvol,   SIZEOF_VOL);

		wsb_get_field(hWsb,15,43, def_spoolib,  SIZEOF_LIB);  
		wsb_get_field(hWsb,15,68, def_spoolvol, SIZEOF_VOL);

		wsb_get_field(hWsb,17,68, def_workvol,  SIZEOF_VOL);

		set_defs(DEFAULTS_IL,def_inlib);		set_defs(DEFAULTS_IV,def_invol);
		set_defs(DEFAULTS_OL,def_outlib);		set_defs(DEFAULTS_OV,def_outvol);
		set_defs(DEFAULTS_RL,def_runlib);		set_defs(DEFAULTS_RV,def_runvol);
		set_defs(DEFAULTS_SL,def_spoolib);		set_defs(DEFAULTS_SV,def_spoolvol);
								set_defs(DEFAULTS_WV,def_workvol);

		save_defaults();							/* Write out the personality info.	*/
	}

	wsb_delete(hWsb);

	return rc;
}

static void wsh_copyright(void)								/* Display the copyright screen.	*/
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;
	char 	buff[80];
	char	temp[80];
	char	platname[80], platcode[2];

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	wsb_add_text(hWsb, 1, 0,"*** Copyright Information ***");
	wsb_add_text(hWsb, 4, 0,"The WISP runtime library");
	wsb_add_text(hWsb, 5, 0,"Copyright (c) 1988-1997  NeoMedia Technologies Incorporated");
	wsb_add_text(hWsb, 6, 0,"2201 2nd Street Suite 600, Fort Myers FL 33901 (941) 337-3434");

	if (outctx)
	{
		sprintf(buff,"Version=%s  Library=%d  Screen=%d  ",wisp_version(), LIBRARY_VERSION, SCREEN_VERSION);
	}
	else
	{
		sprintf(buff,"Version=[%s] Library=[%d] Screen=[%d]",wisp_version(), LIBRARY_VERSION, SCREEN_VERSION);
	}
	wsb_add_text(hWsb, 22, 0,buff);

	whatplat(platname,platcode);
	sprintf(temp, "Platform = %s (%2.2s)", platname,platcode);
	wsb_add_text(hWsb, 18, 0, temp);

#ifdef FUNC_UNAME
	{
		struct utsname unix_name;
		if ( -1 != uname(&unix_name) )
		{
			sprintf(temp, "%s %s %s %s %s", unix_name.sysname, 
							unix_name.nodename,
							unix_name.release,
							unix_name.version,
							unix_name.machine);
			wsb_add_text(hWsb, 20, 0, temp);
		}
	}
#endif
#ifdef WIN32
	sprintf(temp, "Windows %s", computername(NULL));
	wsb_add_text(hWsb, 20, 0, temp);
#endif

	/*
	**	Bottom of screen
	*/

	if (!nativescreens())
	{
		wsb_add_text(hWsb, 24, 25,"(ENTER) Return");

		if (pfkeys12())
		{
			wsb_add_text(hWsb, 24, 41,"(11) Print");
		}
		else
		{
			wsb_add_text(hWsb, 24, 41,"(15) Print");
		}
	}
	else
	{
		wsb_add_text(hWsb, 24, 0,"(ENTER) Return");
	}

	for(;;)
	{
		wsb_display_and_read(hWsb, "000111151633X", &pfkey, &currow, &curcol);

		switch(pfkey)
		{
		case 11:
		case 15:
			wsh_print_cmd_screen();
			break;
		
		case 0:
		case 1:
		case 16:
		case 33:
			wsb_delete(hWsb);
			return;

		default:
			break;
		}
	}
}

static void wsh_show_charset(void)
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;
	int	row, col;
	int	the_char;
	char	buff[80];

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();
	
	wsb_add_text(hWsb, 1, 0, "*** Screen Character Set Mapping ***");

	wsb_add_text(hWsb, 3, 1, "ddd=decimal value   (hh)=hexadecimal value   c=screen character");
	wsb_add_text(hWsb, 4, 1, "--------- --------- --------- --------- --------- --------- --------- ---------");
	wsb_add_text(hWsb, 5, 1, "ddd(hh)=c ddd(hh)=c ddd(hh)=c ddd(hh)=c ddd(hh)=c ddd(hh)=c ddd(hh)=c ddd(hh)=c");
	wsb_add_text(hWsb, 6, 1, "--------- --------- --------- --------- --------- --------- --------- ---------");

	row = 0;
	col = 0;
	
	for( the_char=0; the_char < 128; the_char++)
	{
		/*
		**	We are going to have 8 columns of 16 values
		**	row (0-15) and col (0-7) are relative numbers.
		*/
		if (row == 16)
		{
			row = 0;
			col++;
		}

		sprintf(buff,"%3d(%02X)=%c", the_char, the_char, (unsigned char) the_char);
		wsb_add_text(hWsb, row+7, col*10 + 1, buff);

		row++;
	}
	wsb_add_text(hWsb, 23, 1, "--------- --------- --------- --------- --------- --------- --------- ---------");

	/*
	**	Bottom of screen
	*/

	wsb_add_text(hWsb, 24, 25,"(ENTER) Return");

	if (!nativescreens())
	{
		if (pfkeys12())
		{
			wsb_add_text(hWsb, 24, 41,"(11) Print");
		}
		else
		{
			wsb_add_text(hWsb, 24, 41,"(15) Print");
		}
	}

	for(;;)
	{
		wsb_display_and_read(hWsb, "000111151633X", &pfkey, &currow, &curcol);

		switch(pfkey)
		{
		case 11:
		case 15:
			wsh_print_cmd_screen();
			break;
		
		case 0:
		case 1:
		case 16:
		case 33:
			wsb_delete(hWsb);
			return;

		default:
			break;
		}
	}

}


static void wsh_usage_constants(void)
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;

	char 	buff[80], defs[80];
	int	row, col;
	char	lib[SIZEOF_LIB+1], vol[SIZEOF_VOL+1];
	int4	int4defs;
	int4	args;

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	wsb_add_text(hWsb, 1, 0, "*** Usage Constants ***");

	row = 3;
	col = 2;
	
	args=2;
	wvaset(&args);
	EXTRACT("IL",lib); lib[SIZEOF_LIB] = '\0';
	EXTRACT("IV",vol); vol[SIZEOF_VOL] = '\0';
	sprintf(buff,"IL = %8.8s  IV = %6.6s", lib, vol);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("OL",lib); lib[SIZEOF_LIB] = '\0';
	EXTRACT("OV",vol); vol[SIZEOF_VOL] = '\0';
	sprintf(buff,"OL = %8.8s  OV = %6.6s", lib, vol);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("RL",lib); lib[SIZEOF_LIB] = '\0';
	EXTRACT("RV",vol); vol[SIZEOF_VOL] = '\0';
	sprintf(buff,"RL = %8.8s  RV = %6.6s", lib, vol);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("SL",lib); lib[SIZEOF_LIB] = '\0';
	EXTRACT("SV",vol); vol[SIZEOF_VOL] = '\0';
	sprintf(buff,"SL = %8.8s  SV = %6.6s", lib, vol);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("WL",lib); lib[SIZEOF_LIB] = '\0';
	EXTRACT("WV",vol); vol[SIZEOF_VOL] = '\0';
	sprintf(buff,"WL = %8.8s  WV = %6.6s", lib, vol);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("PL",lib); lib[SIZEOF_LIB] = '\0';
	EXTRACT("PV",vol); vol[SIZEOF_VOL] = '\0';
	sprintf(buff,"PL = %8.8s  PV = %6.6s", lib, vol);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("CL",lib); lib[SIZEOF_LIB] = '\0';
	EXTRACT("CV",vol); vol[SIZEOF_VOL] = '\0';
	sprintf(buff,"CL = %8.8s  CV = %6.6s", lib, vol);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("XV",defs); defs[SIZEOF_VOL] = '\0';
	sprintf(buff,"XV = %s", defs);
	wsb_add_text(hWsb, row++, col+15,buff);


	row = 3;
	col = 30;

	wvaset(&args);
	EXTRACT("PM",defs); defs[1] = '\0';
	sprintf(buff,"PM = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("PC",defs); defs[1] = '\0';
	sprintf(buff,"PC = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("JC",defs); defs[1] = '\0';
	sprintf(buff,"JC = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("JS",defs); defs[1] = '\0';
	sprintf(buff,"JS = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("TT",defs); defs[1] = '\0';
	sprintf(buff,"TT = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("CF",defs); defs[8] = '\0';
	sprintf(buff,"CF = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("C#",defs); defs[4] = '\0';
	sprintf(buff,"C# = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("W$",defs); defs[8] = '\0';
	sprintf(buff,"W$ = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	/*
	   INT4 
	*/

	row = 3;
	col = 45;

	wvaset(&args);
	EXTRACT("E:",&int4defs);
	wswap(&int4defs);
	sprintf(buff,"E: = %ld", (long)int4defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("FN",&int4defs);
	wswap(&int4defs);
	sprintf(buff,"FN = %ld", (long)int4defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("G#",&int4defs);
	wswap(&int4defs);
	sprintf(buff,"G# = %ld", (long)int4defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("PR",&int4defs);
	wswap(&int4defs);
	sprintf(buff,"PR = %ld", (long)int4defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("JL",&int4defs);
	wswap(&int4defs);
	sprintf(buff,"JL = %ld", (long)int4defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("LI",&int4defs);
	wswap(&int4defs);
	sprintf(buff,"LI = %ld", (long)int4defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("P:",&int4defs);
	wswap(&int4defs);
	sprintf(buff,"P: = %ld", (long)int4defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("T#",&int4defs);
	wswap(&int4defs);
	sprintf(buff,"T# = %ld", (long)int4defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("W#",&int4defs);
	wswap(&int4defs);
	sprintf(buff,"W# = %ld", (long)int4defs);
	wsb_add_text(hWsb, row++, col,buff);


	/*
	**	Second half of screen
	*/
	row = 12;
	col = 2;

	wvaset(&args);
	EXTRACT("ID",defs); defs[3] = '\0';
	sprintf(buff,"ID = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("I8",defs); defs[8] = '\0';
	sprintf(buff,"I8 = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("IX",defs); defs[32] = '\0';
	sprintf(buff,"IX = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("LU",defs); defs[32] = '\0';
	sprintf(buff,"LU = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("NA",defs); defs[24] = '\0';
	sprintf(buff,"NA = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	wvaset(&args);
	EXTRACT("S$",defs); defs[15] = '\0';
	sprintf(buff,"S$ = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);


	/*
	**	Bottom of screen
	*/
	wsb_add_text(hWsb, 24, 25,"(ENTER) Return");

	if (!nativescreens())
	{
		if (pfkeys12())
		{
			wsb_add_text(hWsb, 24, 41,"(11) Print");
		}
		else
		{
			wsb_add_text(hWsb, 24, 41,"(15) Print");
		}
	}

	for(;;)
	{
		wsb_display_and_read(hWsb, "000111151633X", &pfkey, &currow, &curcol);

		switch(pfkey)
		{
		case 11:
		case 15:
			wsh_print_cmd_screen();
			break;
		
		case 0:
		case 1:
		case 16:
		case 33:
			wsb_delete(hWsb);
			return;

		default:
			break;
		}
	}
}

static void wsh_env_info(void)
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;
	char 	buff[1024];
	int	row, col;
	char	*ptr;

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	wsb_add_text(hWsb, 1, 0, "*** Environment Information ***");

	row = 3;
	col = 2;
	
	sprintf(buff,"WISPGID       = %s", (ptr=getenv("WISPGID")) ? ptr : "(nil)");
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"WISPCANEXIT   = %s", (ptr=getenv("WISPCANEXIT")) ? ptr : "(nil)");
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"WISPLICENSE   = %s", (ptr=getenv("WISPLICENSE")) ? ptr : "(nil)");
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"WISPLINKLEVEL = %s", (ptr=getenv("WISPLINKLEVEL")) ? ptr : "(nil)");
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"WISPPID       = %s", (ptr=getenv("WISPPID")) ? ptr : "(nil)");
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"WISPTTY       = %s", (ptr=getenv("WISPTTY")) ? ptr : "(nil)");
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"WISPLINK      = %s", (ptr=getenv("WISPLINK")) ? ptr : "(nil)");
	wsb_add_text(hWsb, row++, col,buff);

	
	/*
	**	Bottom of screen
	*/
	sprintf(buff,"PATH (env)    = %s", (ptr=getenv("PATH")) ? ptr : "(nil)");
	wsb_add_text(hWsb, 18, 2,buff);
	

	wsb_add_text(hWsb, 24, 25,"(ENTER) Return");

	if (!nativescreens())
	{
		if (pfkeys12())
		{
			wsb_add_text(hWsb, 24, 41,"(11) Print");
		}
		else
		{
			wsb_add_text(hWsb, 24, 41,"(15) Print");
		}
	}

	for(;;)
	{
		wsb_display_and_read(hWsb, "000111151633X", &pfkey, &currow, &curcol);

		switch(pfkey)
		{
		case 11:
		case 15:
			wsh_print_cmd_screen();
			break;
		
		case 0:
		case 1:
		case 16:
		case 33:
			wsb_delete(hWsb);
			return;

		default:
			break;
		}
	}
}

static void wsh_info(void)
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;
	char 	buff[1024], value[80];
	int	row, col;
	char	*ptr;
	struct wruncfg cfg;

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	wsb_add_text(hWsb, 1, 0, "*** Internal Information ***");

	row = 3;
	col = 2;

	sprintf(buff,"WISPCONFIG  = %s", wispconfigdir());
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"WISPDIR     = %s", wispdir());
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"HOME        = %s", wisphomedir(NULL));
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"CWD         = %s", (ptr=getcwd(NULL,256)) ? ptr : "(nil)");
	wsb_add_text(hWsb, row++, col,buff);
	if (ptr) free(ptr);

	sprintf(buff,"licensefile = %s", license_filepath());
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"wisptmpbase = %s", wisptmpbasedir(NULL));
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"wtmpdir     = %s", wtmpdir(NULL));
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"wispterm    = %s", wisptermfilepath(NULL));
	wsb_add_text(hWsb, row++, col,buff);

	row++;

	sprintf(buff,"MACHID      = %s", (0==getmachineid(value)) ? value : "(unknown)");
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"COMPNAME    = %s", computername(NULL));
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"SERVER      = %s", wispserver());
	wsb_add_text(hWsb, row++, col,buff);
	
	
	/*
	**	Bottom right quarter of screen
	*/

	col = 40;
	row = 12;

	sprintf(buff,"linklevel = %d", linklevel());
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"sortmem   = %d", wispsortmemk());
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"ttyname   = %s", ttyname(0));
	wsb_add_text(hWsb, row++, col,buff);

	ttyid5(value);
	sprintf(buff,"ttyid5    = %s", value);
	wsb_add_text(hWsb, row++, col,buff);

	/*
	**	Next to bottom
	*/
	col = 2;
	row = 16;
	
	wrunconfig(&cfg);

	sprintf(buff,"wrun_cob    = %s", cfg.wrun_cobtype);
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"wrun_run    = %s", cfg.wrun_runcbl);
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"wrun_opt    = %s", cfg.wrun_options);
	wsb_add_text(hWsb, row++, col,buff);
	

	sprintf(buff,"LINKPATH = %s", wisplinkpath());
	wsb_add_text(hWsb, 20, 2,buff);
	

	/*
	**	Bottom of screen
	*/
	wsb_add_text(hWsb, 24, 25,"(ENTER) Return");

	if (!nativescreens())
	{
		if (pfkeys12())
		{
			wsb_add_text(hWsb, 24, 41,"(11) Print");
		}
		else
		{
			wsb_add_text(hWsb, 24, 41,"(15) Print");
		}
	}

	for(;;)
	{
		wsb_display_and_read(hWsb, "000111151633X", &pfkey, &currow, &curcol);

		switch(pfkey)
		{
		case 11:
		case 15:
			wsh_print_cmd_screen();
			break;
		
		case 0:
		case 1:
		case 16:
		case 33:
			wsb_delete(hWsb);
			return;

		default:
			break;
		}
	}
}

#ifdef VMS
static void wsh_dcl(void)								/* Issue DCL commands.			*/
{
	uint4	vms_status;

	if (g_prog_running) newlevel();							/* Extra link-level			*/
	newlevel();									/* Increment the link-level		*/
	spawn2 (0,"","Type LOGOFF followed by (ENTER) to continue.",&vms_status);	/* Now Spawn a sub process.		*/
	oldlevel();
	ppunlink(linklevel());								/* Putparm UNLINK			*/
	setlevel(g_savelevel);								/* Restore the link-level		*/
}
#endif	/* VMS */

#ifdef VMS
static void wsh_queuemgmnt(void)
{
	setup_qm();									/* Setup routine for queue management	*/
}
#endif
#ifdef unix
static void wsh_queuemgmnt(void)
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;
	char 	okkeys[81];

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	okkeys[0] = '\0';
	
	if (wang_style)
	{
		wsb_add_text(hWsb,  1,  0, "***  Manage Queues  ***");
		wsb_add_text(hWsb, 24,  0, "Press (HELP) to return to the command processor, (1) to exit.");
		strcat(okkeys,"01");
	}
	else
	{
		wsb_build_non_wang_base(hWsb);
		wsb_add_menu_item(hWsb, 12, 10, 1, "Return to MAIN HELP menu");
	}

	if (opt_idsiprint)
	{
		if (wang_style)
			wsb_add_text(hWsb,  9, 21, "(2) Manage Print Queue.");
		else
			wsb_add_menu_item(hWsb, 13, 10, 2, "Manage Print Queue");
		strcat(okkeys,"02");
	}
	
	if (opt_batchman)
	{
		if (wang_style)
			wsb_add_text(hWsb, 11, 21, "(3) Manage Batch Queue.");
		else
			wsb_add_menu_item(hWsb, 14, 10, 3, "Manage Batch Queue");
		strcat(okkeys,"03");
	}

	if (wang_style)
	{
		strcat(okkeys,"111533X");
	}
	else
	{
		strcpy(okkeys,"00111533X");
	}
	
	for(;;)
	{
		wsb_display_and_read(hWsb, okkeys, &pfkey, &currow, &curcol);
		
		if      (pfkey == 2)  
		{
			wsh_queue_print();
		}
		else if (pfkey == 3)  
		{
			wsh_queue_batch();
		}
		else if (pfkey == 15 || (pfkey == 11 && pfkeys12())) 
		{
			wsh_print_cmd_screen();
		}
		else 
		{
			break;
		}
	}

	wsb_delete(hWsb);
}
#endif

#ifdef unix
static void wsh_uqueue(int type)
{
	int	pid, rc;
	char	*sh_ptr, *qtype;
	char 	*queue_cmd;
	int 	st;
	char	errbuff[1024];
	
	sh_ptr = wispshellexe();

	wpushscr();
	vwang_shut();
	signal(SIGCLD,  SIG_DFL);						/* Use Default DEATH-OF-CHILD signal	*/

	if (type==1)
	{
		queue_cmd = getenv("UNIQUE_MAN");
		qtype="Print Queue Daemon";
		if (queue_cmd==NULL)
		{
			if (opt_pqunique)
			{
				queue_cmd="unique";
				qtype = "UniQue Print Queue Daemon";
			}
			else
			{
				queue_cmd="ilpman";
				qtype = "IDSI Print Queue Daemon  ";
			}
		}
		switch(pid=fork())
		{
			case 0:
			{	
				st = execlp(queue_cmd,queue_cmd,"-q","-w",(char *)0);
				exit(9);
				break;
			}
			default:
			{
				wwaitpid(pid,&rc);
				break;
			}
		}
	}
	else 
	{
		qtype = "UniQue Batch Queue Daemon";
		queue_cmd= getenv("BATCH_MAN");
		if (queue_cmd==NULL)
		{
			queue_cmd = batchman_name;
		}
		switch(pid=fork())
		{
			case 0:
			{	
				st = execlp(queue_cmd,queue_cmd,(char *)0);
				exit(9);
				break;
			}
			default:
			{
				wwaitpid(pid,&rc);
				break;
			}
		}
		
	}
	signal(SIGCLD,  SIG_IGN);					/* Ignore DEATH-OF-CHILD signal			*/
	vwang_init_term();						/* Initialize the terminal			*/
	wpopscr();

	if (rc)
	{
		switch(rc)
		{	
		      case 1:
			sprintf(errbuff,"%s not running.\nContact system administrator",qtype);
			werr_message_box(errbuff);
			break;
		      case 9:
			sprintf(errbuff,"Cannot find the queue management program \"%s\".\nContact system administrator",
				queue_cmd);
			werr_message_box(errbuff);
			break;
		      default:
			sprintf(errbuff,"Error code %d when trying to run \"%s\".\nContact system administrator",rc,queue_cmd);
			werr_message_box(errbuff);
			break;
		}
	}
}
#endif
#ifdef unix
static void wsh_shell(void)								/* Issue unix shell commands.		*/
{
	int	pid, rc;
	char	*sh_ptr;

	if (g_prog_running) newlevel();							/* Extra link-level			*/

	sh_ptr = wispshellexe();

	wpushscr();
	vwang_shut();
	signal(SIGCLD,  SIG_DFL);							/* Use Default DEATH-OF-CHILD signal	*/

	switch(pid=fork())
	{
		case 0:
		{	
			newlevel();							/* Increment the link-level for shell	*/
			execlp(sh_ptr,sh_ptr,(char *)0);
			break;
		}
		default:
		{
			wwaitpid(pid,&rc);
			break;
		}
	}

	signal(SIGCLD,  SIG_IGN);							/* Ignore DEATH-OF-CHILD signal		*/
	vwang_init_term();								/* Initialize the terminal		*/
	wpopscr();

	ppunlink(linklevel());								/* Putparm UNLINK			*/
	setlevel(g_savelevel);								/* Restore the link-level		*/
}
#endif	/* unix */

#ifdef MSDOS
static void wsh_dos(void)								/* Issue dos shell commands.		*/
{
	wpushscr();
	vwang_shut();

	if (g_prog_running) newlevel();							/* Extra link-level			*/
	newlevel();									/* Increment the link-level for shell	*/
	system("COMMAND");
	oldlevel();

	vwang_init_term();								/* Initialize the terminal		*/
	wpopscr();

	ppunlink(linklevel());								/* Putparm UNLINK			*/
	setlevel(g_savelevel);								/* Restore the link-level		*/
}
#endif	/* MSDOS */

static int wsh_uc_print(void)								/* Set the usage constants.		*/
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;

	char mode[2], class[2], number[4], form[4], dplines[4];				/* Working versions of field data.	*/
	int valid;									/* Validation successful flag.		*/
	int tlines;									/* Integer test for lines per page.	*/
	int4	def_prt_num;
	int4	def_prt_form;
	int4	def_prt_lines;
	int	rc = 0;

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	wsb_add_text(hWsb, 1, 0,"*** Set Print Mode Defaults ***");

	wsb_add_text(hWsb, 4, 2,"Print mode defaults for spooled, queued and online printing:  PRNTMODE =");
	wsb_add_text(hWsb, 6, 6,    "S - Spool printer files. Print then delete.");
	wsb_add_text(hWsb, 7, 6,    "P - Print and keep. Spool printer files, do not delete.");
	wsb_add_text(hWsb, 8, 6,    "H - Hold printer files. Spool for hold.");
	wsb_add_text(hWsb, 9, 6,    "K - Keep printer files, do not print.");
#ifdef VMS
	wsb_add_text(hWsb,10, 6,    "O - Online printing directly to device LPn:");
#endif
	wsb_add_text(hWsb,12, 2,"Default spooled print file print class.      ('A' thru 'Z'):  PRTCLASS =");
	wsb_add_text(hWsb,14, 2,"Default form number.                         (000 thru 254):  FORM_NUM =");
#ifdef VMS
	wsb_add_text(hWsb,16, 2,"Default print device number.   (0 for LP0:, 1 for LP1 etc.):  PRINTER  =");
#else
	wsb_add_text(hWsb,16, 2,"Default printer number:                                       PRINTER  =");
#endif
	wsb_add_text(hWsb,18, 2,"Default Lines per Page                       (000 thru 255):  LINES    =");

	wsb_add_text(hWsb,20,8,"Change the information as appropriate and press (ENTER)");
	wsb_add_text(hWsb,22,8,"or Press  (2) Set Submit Procedure Defaults");
	wsb_add_text(hWsb,23,8,"          (3) Set File Defaults");
	wsb_add_text(hWsb,24,8,"       (HELP) Return to the Command Processor");

	get_defs(DEFAULTS_PM,mode);							/* Get the print mode data.		*/
	get_defs(DEFAULTS_PC,class);							/* Get the print class data.		*/
	get_defs(DEFAULTS_PR,&def_prt_num);						/* Get the printer number.		*/
	get_defs(DEFAULTS_FN,&def_prt_form);						/* Get the printer form number.		*/
	get_defs(DEFAULTS_LI,&def_prt_lines);						/* Get the default lines per page.	*/

	mode[1] = (char)0;
	class[1] = (char)0;
	sprintf(number, "%03d",def_prt_num);
	sprintf(form,   "%03d",def_prt_form);		
	sprintf(dplines,"%03d",def_prt_lines);	

	wsb_add_field(hWsb, 4,75,FAC_DEFAULT_FIELD,mode,1);
	wsb_add_field(hWsb,12,75,FAC_DEFAULT_FIELD,class,1);
	wsb_add_field(hWsb,14,75,FAC_NUMERIC_FIELD,form,3);
	wsb_add_field(hWsb,16,75,FAC_NUMERIC_FIELD,number,3);
	wsb_add_field(hWsb,18,75,FAC_NUMERIC_FIELD,dplines,3);

	for(;;)										/* Repeat until data input is valid.	*/
	{
		wsb_display_and_read(hWsb, "000203111533X", &pfkey, &currow, &curcol);

		if (2 == pfkey)
		{
			rc = 2;
			break;
		}
		if (3 == pfkey)
		{
			rc = 3;
			break;
		}

		if (11 == pfkey || 15 == pfkey)
		{
			wsh_print_cmd_screen();
			continue;
		}		

		wsb_get_field(hWsb, 4,75,mode,1);
		wsb_get_field(hWsb,12,75,class,1);
		wsb_get_field(hWsb,14,75,form,3);
		wsb_get_field(hWsb,16,75,number,3);
		wsb_get_field(hWsb,18,75,dplines,3);

		valid = TRUE;								/* Assume we're valid.			*/
		if (0 == pfkey)								/* Should we validate?			*/
		{
			mode[0] = toupper(mode[0]);					/* Make sure the char is uppercase.	*/
			if (strpos("OSHKP",mode) < 0)					/* Was a valid print class given?	*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wsb_add_field(hWsb, 4,75,FAC_ERROR_FIELD,mode,1);	/* Make it blink.			*/
				currow = 4;
				curcol = 75;
			}
			class[0] = toupper(class[0]);					/* Make print class upper case.		*/
						/***** ADD VALIDATION FOR PRINT CLASS HERE ******/
			tlines = atoi(dplines);						/* Convert lines to integer.		*/
			if (tlines < 0 || tlines > 255)
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wsb_add_field(hWsb,18,75,FAC_NUM_ERROR_FIELD,dplines,3);	/* Make it blink.		*/
				currow = 18;
				curcol = 75;
			}
		}

		if (TRUE == valid)
		{
			break;
		}
	}

	if (0 == pfkey)									/* Copy to data base?			*/
	{
		def_prt_num   = atoi(number);
		def_prt_form  = atoi(form);
		def_prt_lines = atoi(dplines);

		set_defs(DEFAULTS_PM,mode);
		set_defs(DEFAULTS_PC,class);
		set_defs(DEFAULTS_PR,&def_prt_num);
		set_defs(DEFAULTS_FN,&def_prt_form);
		set_defs(DEFAULTS_LI,&def_prt_lines);

		save_defaults();							/* Write out the personality info.	*/
	}

	wsb_delete(hWsb);

	return rc;
}

static int wsh_uc_submit(void)								/* Set the usage constants.		*/
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;
	char mode[2], class[2], t_hr[3], t_min[3], t_sec[3];				/* Working versions of field data.	*/
	int valid;									/* Validation successful flag.		*/
	char	def_proc_cpu[7];
	int	rc = 0;

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	wsb_add_text(hWsb, 1, 0,"*** Set Submit Procedure Defaults ***");

	wsb_add_text(hWsb, 5, 2,"Current defaults for procedure submittal under program control:");
	wsb_add_text(hWsb, 8, 6,"Default submittal status:");
	wsb_add_text(hWsb, 9,12,"R - Run,  Proc eligible for scheduling");
	wsb_add_text(hWsb,10,12,"H - Hold, Do not run proc until released");
	wsb_add_text(hWsb,11,22,"by submitter or operator");
	wsb_add_text(hWsb,11,58,"JOBQUEUE =");
	wsb_add_text(hWsb,13, 6,"Default proc scheduling class  (\"A\" thru \"Z\"):");
	wsb_add_text(hWsb,13,58,"JOBCLASS =");
	wsb_add_text(hWsb,15, 6,"Default proc execution time limit:");
	wsb_add_text(hWsb,16,12,"Central processor time in format HH:MM:SS");
	wsb_add_text(hWsb,17,12,"- No value (or zero) indicates \"No limit\"");
	wsb_add_text(hWsb,17,58,"JOBLIMIT =");
	wsb_add_text(hWsb,17,72,":");
	wsb_add_text(hWsb,17,77,":");

	wsb_add_text(hWsb,20,8,"Change the information as appropriate and press (ENTER)");
	wsb_add_text(hWsb,22,8,"or Press  (1) Set Print Mode Defaults");
	wsb_add_text(hWsb,23,8,"          (3) Set File Defaults");
	wsb_add_text(hWsb,24,8,"       (HELP) Return to the Command Processor");

	get_defs(DEFAULTS_JS,mode);							/* Get the proc status mode data.	*/
	get_defs(DEFAULTS_JC,class);							/* Get the proc class data.		*/
	get_defs(DEFAULTS_JL,def_proc_cpu);

	mode[1] = 0;
	class[1] = 0;

	t_hr[0] = def_proc_cpu[0];
	t_hr[1] = def_proc_cpu[1];
	t_hr[2] = '\0';

	t_min[0] = def_proc_cpu[2];
	t_min[1] = def_proc_cpu[3];
	t_min[2] = '\0';

	t_sec[0] = def_proc_cpu[4];
	t_sec[1] = def_proc_cpu[5];
	t_sec[2] = '\0';

	wsb_add_field(hWsb,11,69,FAC_DEFAULT_FIELD,mode,1);
	wsb_add_field(hWsb,13,69,FAC_DEFAULT_FIELD,class,1);

	wsb_add_field(hWsb,17,69,FAC_NUMERIC_FIELD,t_hr,2);
	wsb_add_field(hWsb,17,74,FAC_NUMERIC_FIELD,t_min,2);
	wsb_add_field(hWsb,17,79,FAC_NUMERIC_FIELD,t_sec,2);

	for(;;)										/* Repeat until data input is valid.	*/
	{
		wsb_display_and_read(hWsb, "000103111533X", &pfkey, &currow, &curcol);

		if (1 == pfkey)
		{
			rc = 1;
			break;
		}
		if (3 == pfkey)
		{
			rc = 3;
			break;
		}

		if (11 == pfkey || 15 == pfkey)
		{
			wsh_print_cmd_screen();
			continue;
		}

		wsb_get_field(hWsb,11,69,mode,1);
		wsb_get_field(hWsb,13,69,class,1);
		wsb_get_field(hWsb,17,69,t_hr,2);
		wsb_get_field(hWsb,17,74,t_min,2);
		wsb_get_field(hWsb,17,79,t_sec,2);

		valid = TRUE;								/* Assume we're valid.			*/
		if (0 == pfkey)								/* Should we validate?			*/
		{
			mode[0] = toupper(mode[0]);					/* Make sure the char is uppercase.	*/
			if (strpos("RH",mode) < 0)					/* Was a valid print class given?	*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wsb_add_field(hWsb,11,69,FAC_ERROR_FIELD,mode,1);	/* Make it blink.			*/
				currow = 11;
				curcol = 69;
			}
			class[0] = toupper(class[0]);					/* Make print class upper case.		*/

			if (!isdigit(t_hr[0])) t_hr[0] = '0';
			if (!isdigit(t_hr[1])) t_hr[1] = '0';
			if (!isdigit(t_min[0])) t_min[0] = '0';
			if (!isdigit(t_min[1])) t_min[1] = '0';
			if (!isdigit(t_sec[0])) t_sec[0] = '0';
			if (!isdigit(t_sec[1])) t_sec[1] = '0';
		}

		if(TRUE == valid)
		{
			break;
		}
	}

	if (0==pfkey)									/* Copy to data base?			*/
	{
		def_proc_cpu[0] = t_hr[0];
		def_proc_cpu[1] = t_hr[1];
		def_proc_cpu[2] = t_min[0];
		def_proc_cpu[3] = t_min[1];
		def_proc_cpu[4] = t_sec[0];
		def_proc_cpu[5] = t_sec[1];

		set_defs(DEFAULTS_JS,mode);
		set_defs(DEFAULTS_JC,class);
		set_defs(DEFAULTS_JL,def_proc_cpu);

		save_defaults();							/* Write out the personality info.	*/
	}

	wsb_delete(hWsb);

	return rc;
}

int wsh_progprnt(int scrn_seq_no)							/* The screen seq. no.  Starts at 1.	*/
{                                                                                                                                 
              
	int wcurwidth;									/* Current Wang screen width.		*/
	char *l_vchr_map;								/* Pointer to Video character map.	*/
	int  l_vmap_top;
	char *work_area_1, *work_area_2;						/* A work area.				*/
	char *ptr;
	char filelibvol[23];
	int retcd;
	struct save_screen *vscrn_struct_ptr;						/* Local pointer to a copy of the stack	*/
	int i,x, work_area_1_size, work_area_2_size;					/* Working variables.			*/

	if (nativescreens())
	{
		return 0;
	}

	wcurwidth = vwang_wcurwidth();
	work_area_1_size = (MAX_COLUMNS_PER_LINE * MAX_LINES_PER_SCREEN);		/* Compute the size of the char map.	*/
	work_area_1 = malloc(work_area_1_size);						/* Allocate space required.		*/

	if (!work_area_1)								/* Space allocated ?			*/
        {
		werr_message_box("Error, wsh_progprnt() unable to obtain required memory.");
		return(0);								/* Scoot along down the line.		*/
	}
                                                            
	work_area_2_size = (wcurwidth + BORDERED_COLUMN_TOTAL) * (MAX_LINES_PER_SCREEN + BORDERED_LINE_TOTAL);
	work_area_2 = malloc(work_area_2_size);						/* Allocate required space.		*/

	if (!work_area_2)								/* Space allocated ?			*/
	{
		werr_message_box("Error, wsh_progprnt() unable to obtain required memory.");
		return(0);								/* Scoot along down the line.		*/
	}

	if (scrn_seq_no == 0)								/* Get the current screen ?		*/
	{
		l_vchr_map = (char *)vchr_map;						/* Yup.  Point directly to it.		*/
		l_vmap_top = vmap_top;
	}
	else
	{			    							/* Nope.  Strip apart the stack.	*/
		if (vscrn_stack)							/* Is there anything on the stack ?	*/
		{
			vscrn_struct_ptr = vscrn_stack;					/* Point to the top of the stack.	*/
			scrn_seq_no--;							/* One less screen to pop off.		*/
			for (x = scrn_seq_no; x; x--)					/* Strip the stack.			*/
			{
				if (vscrn_struct_ptr)					/* Make sure we're not pointing off to  */
				{							/* never never land.	 		*/
 					vscrn_struct_ptr = vscrn_struct_ptr->prev_ptr;	/* Where's the previous stack entry ?	*/
				}
				else
				{							/* Somethin's wrong.			*/
		 			werr_message_box("Error, wsh_progprnt(). Sequence count, stack element mismatch.");
					return(0);					/* Long gone daddy.			*/
				}
			}
			l_vchr_map = vscrn_struct_ptr->xchr_map;			/* Point to the copy of the char. map.	*/
			l_vmap_top = vscrn_struct_ptr->xmap_top;
		}
		else
		{
			werr_message_box("ERROR, wsh_progprnt(). No program screen to print.");
			return(0);
		}
        }

/*	memcpy(work_area_1, l_vchr_map, work_area_1_size);	*/			/* Make a local copy of the map.	*/
	/*
	**	The character map (vchr_map) may be scrolled by video so we can't just copy it to the work area.
	**	Using the vmap_top and vmlx() we calc where each row begins and load work area one line at a time.
	*/
	ptr = work_area_1;
	for(i=0; i<MAX_LINES_PER_SCREEN; i++)
	{
		memcpy(ptr,&l_vchr_map[vmlx(l_vmap_top,i)*MAX_COLUMNS_PER_LINE],MAX_COLUMNS_PER_LINE);
		ptr += MAX_COLUMNS_PER_LINE;
	}

	strip_facs(work_area_1, work_area_1_size,VIDEO_TYPE);		               /* Remove non-printing characters.	*/
	border_screen(work_area_2,  work_area_1, wcurwidth, MAX_COLUMNS_PER_LINE, MAX_LINES_PER_SCREEN);			

	sprintf(filelibvol,"##%3.3s                 ",wanguid3());

	retcd = 0;
	retcd = di_write_file(work_area_2, work_area_2_size, (wcurwidth + BORDERED_COLUMN_TOTAL), filelibvol, filelibvol);
	if (retcd)									/* Some error when trying to print	*/
	{										/* the current screen.			*/
		werrlog(ERRORCODE(2),retcd,0,0,0,0,0,0,0);
	}
	return(0);
}

static void wsh_usewrite(void)
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;
	char 	dst[78];
	char 	*ptr; 
	char 	buff[128];

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();
											/* Init destination file name to spaces.*/
	wisp_defaults_path(buff);
	loadpad(dst,buff,sizeof(dst)-1);
	dst[sizeof(dst)-1] = '\0';

	wsb_add_text(hWsb, 1, 0, "*** Write Usage Constants ***");

	wsb_add_text(hWsb, 6, 3,"Enter filename to write:");
	wsb_add_field(hWsb, 8, 3, FAC_UPLOW_FIELD, dst, sizeof(dst)-1);
	wsb_add_text(hWsb,11, 5,"Enter a file name to write the usage constants to, or");
	wsb_add_text(hWsb,12, 5,"press (ENTER) to use the personality default file.");
	wsb_add_text(hWsb,24, 0,"Change the information as appropriate and press (ENTER), (1) to exit.");

	for(;;)
	{
		wsb_display_and_read(hWsb, "0001111533X", &pfkey, &currow, &curcol);

		switch(pfkey)
		{
		case 0:
			wsb_get_field(hWsb, 8, 3, dst, sizeof(dst)-1);
			dst[sizeof(dst)-1] = '\0';
			leftjust(dst,sizeof(dst)-1);				/* left justify & null terminate the 	*/
			if (ptr = strchr(dst,' ')) *ptr=0;			/* personality file name.		*/
			load_defaults();					/* Be sure they exist.			*/
			write_defaults_to_file(dst);				/* Store the file.			*/

			/* Fall thru to return */
		case 1:
		case 33:
			wsb_delete(hWsb);
			return;
			
		case 11:
		case 15:
			wsh_print_cmd_screen();
			break;

		default:
			break;
		}
	}
}

int wsc_init(char *screen, int row, int col)						/* Initialize screen image for vwang().	*/
{
	memset(screen,' ',WSB_LENGTH);							/* Blank the screen.			*/
	screen[OA_ROW] = 1;								/* Set the order area, row # 1.		*/
	screen[OA_WCC] = (char)(POSITION_CURSOR|UNLOCK_KEYBOARD); /* 0240; */		/* Set the allowances.			*/
	screen[OA_CURSOR_ROW] = row;
	screen[OA_CURSOR_COL] = col;
	return(SUCCESS);
}

int wput(char *screen, int row, int col, int fac, char *text)				/* Put text and fields into screen map.	*/
{
	register int i,j;								/* Working storage.			*/

	if ((col-2) < 0) werr_message_box("wsh_dispatch - Invalid field position, FAC off left hand side of screen.");
	i = ((row-1) * WSB_COLS) + (col-1) + OA_LENGTH;					/* Location in screen map.		*/
	j = strlen(text);								/* Determine length of string.		*/
	screen[i-1] = fac;								/* Lay in the fac.			*/
	memcpy(&screen[i],text,j);							/* Lay in the data.			*/
	screen[i+j] = FAC_DEFAULT_TEXT;							/* Put an end of text FAC.		*/
	return(SUCCESS);								/* And we're all done.			*/
}                                              

int wpcen(char *screen, int row, int fac, char *text)					/* Put text and fields into screen map.	*/
{											/* Centered.				*/
	register int i,j;								/* Working storage.			*/

	j = strlen(text);								/* Determine length of string.		*/
	i = ((row-1) * WSB_COLS) + (39-(j/2)) + OA_LENGTH;				/* Location in screen map.		*/
	screen[i-1] = fac;								/* Lay in the fac.			*/
	memcpy(&screen[i],text,j);							/* Lay in the data.			*/
	screen[i+j] = FAC_DEFAULT_TEXT;							/* Put an end of text FAC.		*/
	return(SUCCESS);								/* And we're all done.			*/
}

int wget(char *screen, int row, int col, char *text)					/* Retreive text from screen.		*/
{
	register int i,j;								/* Working storage.			*/

	i = ((row-1) * WSB_COLS) + (col-1) + OA_LENGTH;					/* Location in screen map.		*/
	j = strlen(text);								/* Determine length of string.		*/
	memcpy(text,&screen[i],j);							/* Get in the data.			*/
	return(SUCCESS);								/* Tout finis.				*/
}

static void wsh_devices(void)
{
#ifdef AIX
	wpushscr();
	vwang_shut();

	wsystem("smit -C");

	vwang_init_term();								/* Initialize the terminal		*/
	wpopscr();
#else
	werr_message_box("Sorry, this feature is not available on the system on which you are running.");
#endif
}

static void wsh_run_program(char *program)						/* Run a program.			*/
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;

	int prog_given;									/* Name given flag.			*/
	char progname[9], linktype[1], libname[9], volname[7];				/* LINK parameters			*/

	memset(libname,' ',8);
	memset(volname,' ',6);

	prog_given = TRUE;								/* Assune name not given.		*/
	if (memcmp(program,"        ",8) == 0) prog_given = FALSE;			/* Was a program given?			*/
	if (*program == '\0') prog_given = FALSE;

	if (!prog_given)
	{
		get_defs(DEFAULTS_PV,volname);	/* NOTE: If PV/PL is not set then RV/RL	are returned */
		get_defs(DEFAULTS_PL,libname);
	}

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	wsb_add_text(hWsb,  1,  0, "***  Run Program  ***");
	wsb_add_text(hWsb, 24,  0, "Press (HELP) to return to the command processor, (1) to exit.");

	wsb_add_text(hWsb,  6,  7, "Supply the file name of the");
	wsb_add_text(hWsb,  7,  7, "program to be executed and");
	wsb_add_text(hWsb,  8,  7, "press (ENTER)");
	wsb_add_text(hWsb, 11,  7, "Options");
	wsb_add_text(hWsb, 13,  9, "Designate a user library which");
	wsb_add_text(hWsb, 14,  9, "includes programs to be used");
	wsb_add_text(hWsb, 15,  9, "during this run:");

	wsb_add_text(hWsb,   8, 56, "PROGRAM  = ");
	wsb_add_field(hWsb,  8, 67, FAC_DEFAULT_FIELD, program, SIZEOF_FILE);

	wsb_add_text(hWsb,  14, 56, "LIBRARY  = ");
	wsb_add_field(hWsb, 14, 67, FAC_DEFAULT_FIELD, libname, SIZEOF_LIB);

	wsb_add_text(hWsb,  15, 56, "VOLUME   = ");
	wsb_add_field(hWsb, 15, 67, FAC_DEFAULT_FIELD, volname, SIZEOF_VOL);

again:	
	if (prog_given)								/* Program name was given.		*/
	{
		pfkey = 0;							/* Fake an enter.			*/
	}
	else 
	{
		for(;;)
		{
			wsb_display_and_read(hWsb, "0001111533X", &pfkey, &currow, &curcol);

			if (11==pfkey || 15==pfkey)				/* PF15 - PRINT screen			*/
			{
				wsh_print_cmd_screen();
			}
			else
			{
				break;
			}
		}
	}

	if (1 == pfkey)								/* Return				*/
	{
		goto end_run;
	}

	if (0 == pfkey)								/* Run program or return.		*/
	{
		int4 templong, compcode, returncode;

		wsb_get_field(hWsb,  8, 67, progname, SIZEOF_FILE);
		wsb_get_field(hWsb, 14, 67, libname, SIZEOF_LIB);
		wsb_get_field(hWsb, 15, 67, volname, SIZEOF_VOL);

		/* Reset the field attributes and cursor position */
		wsb_add_field(hWsb,  8, 67, FAC_DEFAULT_FIELD, progname, SIZEOF_FILE);
		wsb_add_field(hWsb, 14, 67, FAC_DEFAULT_FIELD, libname, SIZEOF_LIB);
		wsb_add_field(hWsb, 15, 67, FAC_DEFAULT_FIELD, volname, SIZEOF_VOL);
		currow = 0;
		curcol = 0;

		if (memcmp(progname,"        ",8) == 0)					/* Check for a program name		*/
		{
			wsb_perr(hWsb, "ERROR - Program name must be given.");
			wsb_add_field(hWsb,  8, 67, FAC_ERROR_FIELD, progname, SIZEOF_FILE);
			currow = 8;
			curcol = 67;
			goto again;
		}

		if ((memcmp(libname,"        ",8) == 0) && (memcmp(volname,"      ",6) == 0)) 
		{
			linktype[0] = ' ';
		}
		else
		{
			linktype[0] = 'P';						/* Else use the parameters 'P'		*/
		} 

		compcode = 0;								/* Initialize the comp & return codes	*/
		returncode = 0;
		templong = 6;
		wvaset(&templong);							/* Set the arg count to 6		*/

		if (g_prog_running) newlevel();						/* Extra link-level			*/

		LINK2(progname,(int4)8,linktype,(int4)1,libname,(int4)8,volname,(int4)6,
		      &compcode,(int4)4,&returncode,(int4)4);				/* Do the LINK				*/

		wswap(&compcode);							/* Un-swap the comp & return codes	*/
		wswap(&returncode);

		if ( 8 == compcode && 20 == returncode )				/* If not found ...			*/
		{
			linktype[0] = 'S';						/* Try SYSTEM link			*/
			compcode = 0;							/* Initialize the comp & return codes	*/
			returncode = 0;
			templong = 6;
			wvaset(&templong);						/* Set the arg count to 6		*/

			LINK2(progname,(int4)8,linktype,(int4)1,libname,(int4)8,
			      volname,(int4)6,&compcode,(int4)4,&returncode,(int4)4);	/* Do the LINK				*/

			wswap(&compcode);						/* Un-swap the comp & return codes	*/
			wswap(&returncode);
		}

		setlevel(g_savelevel);							/* Restore the link-level		*/

		switch(compcode)
		{
		case 0:
			sprintf(run_complete_message,"Program %8.8s Processing Completed",progname);
			goto end_run;

		case 8:
			switch(returncode)
			{
			case 4:
				wsb_perr(hWsb,"FAILED - Volume was not found.");
				wsb_add_field(hWsb, 15, 67, FAC_ERROR_FIELD, volname, SIZEOF_VOL);
				currow = 15;
				curcol = 67;
				break;
			case 16:
				wsb_perr(hWsb,"FAILED - Library was not found.");
				wsb_add_field(hWsb, 14, 67, FAC_ERROR_FIELD, libname, SIZEOF_LIB);
				currow = 14;
				curcol = 67;
				break;
			case 20:
				wsb_perr(hWsb,"FAILED - Program was not found.");
				wsb_add_field(hWsb,  8, 67, FAC_ERROR_FIELD, progname, SIZEOF_FILE);
				currow = 8;
				curcol = 67;
				break;
			case 28:
				wsb_perr(hWsb,"FAILED - Protection violation.");
				break;
			case 60:
				wsb_perr(hWsb,"FAILED - Insufficient memory.");
				break;
			default:
				wsb_perr(hWsb,"FAILED - Unable to execute program.");
				break;
			}
			break;
		case 16:
		default:
			wsb_perr(hWsb,"Program ABORTED or was CANCELLED");
			break;
		}

		prog_given = FALSE;							/* Force screen to be drawn		*/
		goto again;
	}

end_run:

	wsb_delete(hWsb);
}

static void wsh_submit(void)
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;
	char	errmess[80];
	char	*errptr;
	char 	mode[2], class[2];
	int 	looping;
	char	l_file[9], l_lib[9], l_vol[7];
	char	jobname[9];

	strcpy(l_file,"        ");
	strcpy(l_lib, "        ");
	strcpy(l_vol, "      ");
	get_defs(DEFAULTS_RL,l_lib);
	get_defs(DEFAULTS_RV,l_vol);

	strcpy(jobname,"        ");
	memcpy(jobname,wanguid3(),3);

	get_defs(DEFAULTS_JS,mode);							/* Get the proc status mode data.	*/
	get_defs(DEFAULTS_JC,class);							/* Get the proc class data.		*/
	mode[1] = 0;
	class[1] = 0;
	
	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	wsb_add_text(hWsb, 1,0,"***  Submit Procedure  ***");

	wsb_add_text(hWsb, 5,15,"Identify the Procedure to be Run and Press (ENTER):");

	wsb_add_text(hWsb, 7, 9,"FILE     = xxxxxxxx  in LIBRARY  = xxxxxxxx  on VOLUME   = xxxxxx");

	wsb_add_field(hWsb, 7,20,FAC_DEFAULT_FIELD,l_file, SIZEOF_FILE);
	wsb_add_field(hWsb, 7,44,FAC_DEFAULT_FIELD,l_lib,  SIZEOF_LIB);
	wsb_add_field(hWsb, 7,68,FAC_DEFAULT_FIELD,l_vol,  SIZEOF_VOL);

	wsb_add_text(hWsb,10, 2,"Scheduling Options:  PROCEDURE ID       = xxxxxxxx");
	wsb_add_text(hWsb,11, 2,"                               CLASS    = x                  (\"A\" thru \"Z\")");
	wsb_add_text(hWsb,12, 2,"                               STATUS   = x                  (R-Run / H-Hold)");

	wsb_add_field(hWsb,10,44,FAC_DEFAULT_FIELD,jobname,8);
	wsb_add_field(hWsb,11,44,FAC_DEFAULT_FIELD,class,1);
	wsb_add_field(hWsb,12,44,FAC_DEFAULT_FIELD,mode,1);

	wsb_add_text(hWsb,24,0,"Or Press (HELP) to Return to the Command Processor.");

	for(looping=1; looping;)
	{
		wsb_display_and_read(hWsb, "0001111533X", &pfkey, &currow, &curcol);

		if (1 == pfkey)								/* PF01 - ABORT				*/
		{
			break;
		}

		if (11==pfkey || 15==pfkey)						/* PF15 - PRINT screen			*/
		{
			wsh_print_cmd_screen();
			continue;
		}

		if (0 != pfkey)								/* NOT ENTER (HELP KEY) - ABORT		*/
		{
			break;
		}

		wsb_get_field(hWsb, 7,20,l_file, SIZEOF_FILE);
		wsb_get_field(hWsb, 7,44,l_lib,  SIZEOF_LIB);
		wsb_get_field(hWsb, 7,68,l_vol,  SIZEOF_VOL);
		wsb_get_field(hWsb,10,44,jobname,8);
		wsb_get_field(hWsb,11,44,class,1);
		wsb_get_field(hWsb,12,44,mode,1);

		/* Blank out any previous error messages */
		memset(errmess,' ',sizeof(errmess));
		wsb_add_field(hWsb, 2,  2, FAC_PROT_BLANK, errmess, 79);
		wsb_add_field(hWsb, 3,  5, FAC_PROT_BLANK, errmess, 76);

		if (memcmp(l_file,"        ",SIZEOF_FILE) == 0)
		{
			wsb_add_field(hWsb, 2,  2, FAC_PROT_BLINK, errptr="FILE MUST BE SPECIFIED" , strlen(errptr));
			currow = 7;
			curcol = 20;
		}
		else
		{
			int4	retcode;
			int4	argcnt;
			
			argcnt = 8;
			wvaset(&argcnt);

			SUBMIT(l_file, l_lib, l_vol, jobname, mode, "D", class, &retcode);

			wswap(&retcode);

			if (0 == retcode)
			{
				looping = 0;  /* Stop looping */
			}
			else
			{
				sprintf(errmess,"RC = %ld %s", (long)retcode, submit_err(retcode));

				/* If still looping then Write out error message */
				wsb_add_field(hWsb, 2,  2, FAC_PROT_BLINK, errptr="SUBMIT FAILED -",strlen(errptr));
				wsb_add_field(hWsb, 3,  5, FAC_PROT_BLINK, errmess, strlen(errmess));

				currow = 0;
				curcol = 0;
			}
		}
	}

	wsb_delete(hWsb);
}

static void wsh_utils(void) 								/* Select utilities.			*/
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;
	int	col;

	char	pflist[40];
	uint4 	dflags;

	get_defs(DEFAULTS_FLAGS,&dflags);

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	if (wang_style)
	{
		col = 15;
		
		wsb_add_text(hWsb,  1,   0, "***  Utilities ***");
		wsb_add_text(hWsb, 24,   0, "Press (HELP) to return to the command processor, (1) to exit.");
		wsb_add_text(hWsb,  4, col, "GENERAL");
	}
	else
	{
		col = 10;
		
		wsb_build_non_wang_base(hWsb);
		wsb_add_menu_item(hWsb, 12, col, 1, "Return to MAIN HELP menu");
	}
	strcpy(pflist,"01");

	if (dflags & HELP_DISPLAY) 	
	{
		if (wang_style)
			wsb_add_text(hWsb,  6, col, "(2) DISPLAY a file");
		else
			wsb_add_menu_item(hWsb, 13, col, 2, "DISPLAY a file");
		strcat(pflist,"02");
	}

	if (dflags & HELP_EDIT) 	
	{
		if (wang_style)
			wsb_add_text(hWsb,  8, col, "(3) EDIT a file");
		else
			wsb_add_menu_item(hWsb, 14, col, 3, "EDIT a file");
		strcat(pflist,"03");
	}

	if (dflags & HELP_DISPRINT) 	
	{
		if (wang_style)
			wsb_add_text(hWsb,  10, col, "(4) DISPRINT");
		else
			wsb_add_menu_item(hWsb, 15, col, 4, "DISPRINT");
		strcat(pflist,"04");
	}

	if (dflags & HELP_CRID)
	{
		if (wang_style)
		{
			wsb_add_text(hWsb, 12, col, "(5) REPORT");
			wsb_add_text(hWsb, 14, col, "(6) INQUIRY");
			wsb_add_text(hWsb, 16, col, "(7) CONTROL");
			wsb_add_text(hWsb, 18, col, "(8) DATENTRY");
		}
		else
		{
			wsb_add_menu_item(hWsb, 16, col, 5, "REPORT");
			wsb_add_menu_item(hWsb, 17, col, 6, "INQUIRY");
			wsb_add_menu_item(hWsb, 18, col, 7, "CONTROL");
			wsb_add_menu_item(hWsb, 19, col, 8, "DATENTRY");
		}
		strcat(pflist,"05060708");
	}

	if (wang_style)
		strcat(pflist,"111533X");
	else
		strcpy(pflist,"00111533X");

	for(;;)
	{
		wsb_display_and_read(hWsb, pflist, &pfkey, &currow, &curcol);

		if (pfkey == 1)
		{
			break;
		}
		else if (pfkey == 2 && (dflags & HELP_DISPLAY))				/* DISPLAY?				*/
		{
			wsh_run_program("DISPLAY ");
		}
		else if (pfkey == 3 && (dflags & HELP_EDIT))				/* EDIT?				*/
		{
			wsh_run_program("VSEDIT  ");
		}
		else if (pfkey == 4 && (dflags & HELP_DISPRINT))			/* DISPRINT?				*/
		{
			wsh_run_program("DISPRINT");
		}
		else if (pfkey >= 5 && pfkey <= 8 && (dflags & HELP_CRID))		/* CRID?				*/
		{
			if      (pfkey == 5) wsh_run_program("REPORT  ");
			else if (pfkey == 6) wsh_run_program("INQUIRY ");
			else if (pfkey == 7) wsh_run_program("CONTROL ");
			else if (pfkey == 8) wsh_run_program("DATENTRY");
		}
		else if (11 == pfkey || 15 == pfkey)
		{
			wsh_print_cmd_screen();
		}
		else
		{
			break;
		}
	}

	wsb_delete(hWsb);
}

static void wsh_extras(void)
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;
	char	pflist[40];
	uint4 	dflags;
	int	row,col;

	get_defs(DEFAULTS_FLAGS,&dflags);

	/*
	**	Print screen and setting terminal attributes is not supported
	**	with native screens.
	*/
	if (nativescreens())
	{
		dflags &= ~(HELP_PRINT_SCREEN | HELP_TERMINAL);
	}

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	if (wang_style)
	{
		wsb_add_text(hWsb,  1,  0, "***  Extras ***");
		wsb_add_text(hWsb, 24,  0, "Press (HELP) to return to the command processor, (1) to exit.");
	}
	else
	{
		wsb_build_non_wang_base(hWsb);
	}

	if (wang_style)
	{
		row = 6;
		col = 15;
	}
	else
	{
		row = 12;
		col = 8;
	}
	
	if (!wang_style)
	{
		wsb_add_menu_item(hWsb, row, col, 1, "Return to MAIN HELP menu");
		row++;
	}

	if (dflags & HELP_TERMINAL)
	{
		if (wang_style)
			wsb_add_text(hWsb,  row, col, "(2) Configure TERMINAL");
		else
			wsb_add_menu_item(hWsb, row, col, 2, "Terminal characteristics");
	}
	row++;

#if defined(unix) || defined(VMS) || defined(MSDOS)
	if (dflags & HELP_COMMANDS)
	{
		if (wang_style)
		{
			wsb_add_text(hWsb,  row, col, "(3) Enter COMMANDS");
		}
		else
		{
#ifdef VMS
			wsb_add_menu_item(hWsb, row, col, 3, "Enter DCL commands");
#endif
#ifdef unix
			wsb_add_menu_item(hWsb, row, col, 3, "Enter UNIX commands");
#endif
#ifdef MSDOS
			wsb_add_menu_item(hWsb, row, col, 3, "Enter MS-DOS commands");
#endif
		}
	}
#endif /* unix || VMS || MSDOS */
	row++;

	if (dflags & HELP_USAGE_WRITE)
	{
		if (wang_style)
			wsb_add_text(hWsb,  row, col, "(4) SAVE environment");
		else
			wsb_add_menu_item(hWsb, row, col, 4, "Write usage constants");
	}
	row++;

	row++;
	if (wang_style)
		wsb_add_text(hWsb,  row, col, "(5) SHOW Environment Info");
	else
		wsb_add_menu_item(hWsb, row, col, 5, "Environment Info");

	row++;
	if (wang_style)
		wsb_add_text(hWsb,  row, col, "(6) SHOW Usage Constants");
	else
		wsb_add_menu_item(hWsb, row, col, 6, "Usage Constants");

	row++;
	if (wang_style)
		wsb_add_text(hWsb,  row, col, "(7) SHOW Internal Info");
	else
		wsb_add_menu_item(hWsb, row, col, 7, "Internal Info");

	row++;
	if (wang_style)
		wsb_add_text(hWsb,  row, col, "(8) SHOW Copyright Info");
	else
		wsb_add_menu_item(hWsb, row, col, 8, "Copyright Info");

	row++;
	if (wang_style)
		wsb_add_text(hWsb,  row, col, "(9) SHOW Character Set");
	else
		wsb_add_menu_item(hWsb, row, col, 9, "Character Set");


	if (wang_style)
		strcpy(pflist,"000102030405060708091115282930313233X");
	else
		strcpy(pflist,"001115282930313233X");

	for(;;)
	{
		wsb_display_and_read(hWsb, pflist, &pfkey, &currow, &curcol);

		switch(pfkey)
		{
		case 2:
			if (dflags & HELP_TERMINAL)
			{
				wsh_terminal();
			}
			break;
			
		case 3:
#if defined(unix) || defined(VMS) || defined(MSDOS)
			if (dflags & HELP_COMMANDS)
			{
				wsh_command();
			}
#endif
			break;
		
		case 4:
			if (dflags & HELP_USAGE_WRITE)
			{
				wsh_usewrite();
			}
			break;
		
		case 9:
		case 28:
			wsh_show_charset();
			break;
		
		case 5:
		case 29:
			wsh_env_info();
			break;
		
		case 6:
		case 30:
			wsh_usage_constants();
			break;
			
		case 7:
		case 31:
			wsh_info();
			break;

		case 8:
		case 32:
			wsh_copyright();
			break;

		case 11:
		case 15:
			wsh_print_cmd_screen();
			break;
			
		case 1:
		case 33:

			wsb_delete(hWsb);
			return;

		default:
			break;
		}
	}
}

static void wsb_perr(HWSB hWsb, char *text)
{
	char	msg[65];
	int	len;
	
	len = strlen(text);

	/*
	**	Clear old text
	*/
	memset(msg,' ',sizeof(msg)-1);
	msg[sizeof(msg)-1] = '\0';
	wsb_add_text(hWsb, 20, 16, msg);
	
	if (len)
	{
		memcpy(msg,text,len);
		wsb_add_field(hWsb, 20, 16, FAC_BOLD_TEXT, msg, len);
	}
	else
	{
		wsb_add_field(hWsb, 20, 16, FAC_DEFAULT_TEXT, msg, 1);
	}
	
}

static void wsb_build_non_wang_base(HWSB hWsb)
{
	char	temp[255];

	wsb_add_text(hWsb, 1, 0, "***  WISP HELP Processor  ***");
	wsb_add_text(hWsb, 2, 0, "NeoMedia Technologies Inc.");

	sprintf(temp,"Username:  %s", longuid());
	wsb_add_text(hWsb, 5, 3, temp);

	temp[0] = '\0';
#ifdef FUNC_UNAME
	{
		struct utsname unix_name;

		if ( -1 != uname(&unix_name) )
		{
			sprintf(temp, "System:    %s - %s", unix_name.sysname, unix_name.nodename);
		}
	}
#endif
#ifdef WIN32
	if (0==strcmp(computername(NULL),"Windows"))
	{
		strcpy(temp, "Welcome to Windows");
	}
	else
	{
		sprintf(temp, "Welcome to Windows - %s", computername(NULL));
	}
#endif
	if (!temp[0])
	{
		strcpy(temp,"System:    UNKNOWN");
#ifdef VMS
		strcpy(temp,"System:    VMS");
#endif
#ifdef MSDOS
		strcpy(temp,"System:    MS-DOS");
#endif
#ifdef unix
		strcpy(temp,"System:    UNIX");
#endif
#ifdef WIN32
		strcpy(temp,"System:    WINDOWS");
#endif
	}
	wsb_add_text(hWsb, 6, 3, temp);

	if (g_prog_running)							/* Is a program running?		*/
	{
		if (wisp_progname[0] && wisp_progname[0] != ' ')		/* We know the program name		*/
		{
			sprintf(temp, "Program:   %s",wisp_progname);
			wsb_add_text(hWsb, 7, 3, temp);

			if (wisp_screen[0] && wisp_screen[0] != ' ')		/* We also know the screen name		*/
			{
				sprintf(temp, "Screen:    %s",wisp_screen);
				wsb_add_text(hWsb, 8, 3, temp);
			}
		}
		else
		{
			if (WISPRUNNAME[0] != ' ')
			{
				sprintf(temp, "Program:   %8.8s",WISPRUNNAME);
				wsb_add_text(hWsb, 7, 3, temp);
			}
		}
	}

	memset(temp,'-',WSB_COLS);
	temp[WSB_COLS] = '\0';
	wsb_add_text(hWsb, 10, 1, temp);
	wsb_add_text(hWsb, 23, 1, temp);

	wsb_add_text(hWsb, 24, 0, "(TAB) (SPACE) or (ARROWS) to move.     (ENTER) to select.");
}


/*
**	Routine:	wsh_cancel()
**
**	Function:	To display the CANCEL screen
**
**	Description:	Display the CANCEL screen and return what the user wants to do.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:
**	0		Don't cancel, return to command processor.
**	1		Go ahead and cancel.
**
**	Warnings:	None
**
**	History:	
**	01/13/93	Written by GSL
**
*/
static int wsh_cancel(void)
{
	HWSB	hWsb;
	int pfkey, currow, curcol;

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();
	wsb_add_text(hWsb,  1, 34,  "*** Cancel ***");
	wsb_add_text(hWsb,  8, 13,  "WARNING -");
	wsb_add_text(hWsb,  9, 18,  "Cancel is used to terminate your current program");
	wsb_add_text(hWsb, 10, 18,  "and return all resources to the system. It will");
	wsb_add_text(hWsb, 11, 18,  "attempt to complete any outstanding file I/O and");
	wsb_add_text(hWsb, 12, 18,  "then close all files opened by the program.");
	wsb_add_text(hWsb, 16, 13,  "SELECT:");
	wsb_add_text(hWsb, 18, 18,  "(ENTER)  to begin Cancel Processing");
	wsb_add_text(hWsb, 20, 18,  "(HELP)   to return to the Command Processor");
	
	wsb_display_and_read(hWsb, "0033X", &pfkey, &currow, &curcol);
	wsb_delete(hWsb);

	if (0 == pfkey)
	{
		return(1);
	}

	return(0);
}

/*
**	Routine:	wsh_exit()
**
**	Function:	To display the EXIT screen
**
**	Description:	Display the EXIT screen and return what the user wants to do.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:
**	0		Don't exit, return to command processor.
**	1		Go ahead and exit.
**
**	Warnings:	None
**
*/
static int wsh_exit(void)
{
	HWSB	hWsb;
	int pfkey, currow, curcol;

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	wsb_add_text(hWsb,  1,  0,  "*** Exit ***");
	wsb_add_text(hWsb,  8, 13,  "WARNING -");
	wsb_add_text(hWsb,  9, 18,  "Exit is used to terminate the WISP Command Processor");
	wsb_add_text(hWsb, 10, 18,  "and return you to the operating system.");
	wsb_add_text(hWsb, 16, 13,  "SELECT:");
	wsb_add_text(hWsb, 18, 18,  "(ENTER)  to Exit the WISP Command Processor");
	wsb_add_text(hWsb, 20, 18,  "(HELP)   to return to the WISP Command Processor");

	wsb_display_and_read(hWsb, "0033X", &pfkey, &currow, &curcol);
	wsb_delete(hWsb);

	if (0 == pfkey)
	{
		return(1);
	}

	return(0);
}

static char *formated_time(char *time_string)
{
	time_t time_data;
	

	time_data = time(NULL);								/* Get the time.			*/
	memcpy(time_string,ctime(&time_data),24);
	time_string[16] = time_string[19];
	time_string[17] = time_string[20];
	time_string[18] = time_string[21];
	time_string[19] = time_string[22];
	time_string[20] = time_string[23];
	time_string[21] = (char)0;
	return(time_string);	
}


static int is_help_active = 0;

int ishelpactive(void)
{
	return is_help_active;
}

int sethelpactive(int flag)
{
	return(is_help_active = flag);
}

/*
**	History:
**	$Log: wshelp.c,v $
**	Revision 1.62  1998-01-14 09:26:27-05  gsl
**	Remove the "PROCESSING" message in the run_program routine,
**	it was not needed and caused the screen to flash.
**
**	Revision 1.61  1998-01-07 11:54:02-05  gsl
**	Fix RUN screen where PROGNAME was being cleared
**
**	Revision 1.60  1998-01-07 11:01:21-05  gsl
**	Fix wsb_perr() by null terminating message string.
**	Changed to use new FAC_xxx_FIELD defines
**
**	Revision 1.59  1997-12-18 09:12:33-05  gsl
**	move screen_print() to screen.c
**
**	Revision 1.58  1997-12-15 13:08:39-05  gsl
**	Change PRINT SCREEN functionality to call SCREEN()
**
**	Revision 1.57  1997-12-08 15:46:20-05  gsl
**	Add wsh_show_charset() to display the character set.
**
**	Revision 1.56  1997-12-04 18:14:23-05  gsl
**	change to use wispnt.h
**	changed to use wisplinkpath()
**
**	Revision 1.55  1997-12-01 15:45:04-05  gsl
**	Add the DISPLAY only of the "Progress" message.
**
**	Revision 1.54  1997-10-31 14:27:21-05  gsl
**	Finished the conversion of vwang() to WSB.
**	The only remaining screen in the psuedo blanks
**
**	Revision 1.53  1997-10-29 12:54:09-05  gsl
**	Change most screen IO to use WSB routines
**	(There is still a lot to do.)
**
**	Revision 1.52  1997-10-23 16:26:13-04  gsl
**	change vdisplay() to link_display()
**	Changed wfile_disp() to wsh_run_program("DISPLAY ") to support
**	the external DISPLAY option.
**
**	Revision 1.51  1997-09-30 14:09:31-04  gsl
**	Add support for pfkeys12()
**
**	Revision 1.50  1997-08-23 12:45:13-04  gsl
**	Add (8) Display Error Log  to the Command Processor
**
**	Revision 1.49  1997-08-21 15:15:02-04  gsl
**	Changed to use submit_err() to get the error message from SUBMIT
**
**	Revision 1.48  1997-08-18 16:57:49-04  gsl
**	Add (12) SUBMIT procedure to the Command Processor
**
**	Revision 1.47  1997-07-15 17:43:06-04  gsl
**	Change WUSAGE Command Processor to WISP Command Processor
**
**	Revision 1.46  1997-07-14 08:35:59-04  gsl
**	Fix the way pseudo_blanks are displayed.
**
**	Revision 1.45  1997-05-21 12:11:07-04  gsl
**	Replace an extern with include vdata.h
**
**	Revision 1.44  1997-03-17 12:00:07-05  gsl
**	Split the internal info screen into two screens, one for environment info
**
**	Revision 1.43  1997-03-14 11:18:21-05  gsl
**	Split the Internal Info screen into 2 screens, one for Usage Constants (30)
**	and one for other Internal Info (31) and expanded both
**
**	Revision 1.42  1997-03-13 16:57:13-05  gsl
**	Add wispdir() to internals screen
**
**	Revision 1.41  1997-03-06 16:49:08-05  gsl
**	Added Computername and Server to internal info screen
**
**	Revision 1.40  1997-02-28 17:45:44-05  gsl
**	Add Machine id to iternals screen
**
**	Revision 1.39  1997-02-28 16:23:08-05  gsl
**	In WIN32 code change to use define for default computername
**
**	Revision 1.38  1997-02-17 16:37:19-05  gsl
**	Change Address
**
**	Revision 1.37  1996-12-12 12:47:44-05  gsl
**	Changed Devtech to NeoMedia
**
**	Revision 1.36  1996-11-25 13:18:36-08  gsl
**	Added PF15 - PRINT SCREEN to all the help screens
**
**	Revision 1.35  1996-10-10 09:32:38-07  gsl
**	Add include unistd.h
**
**	Revision 1.34  1996-10-08 17:32:31-07  gsl
**	replace shell_var() with wispshellexe()
**	replace getenv() with wispconfigdir() and wisphomedir()
**
**	Revision 1.33  1996-08-29 17:11:13-07  gsl
**	Added PF31 - Internal Info screen. This is a hidden screen which
**	displays alot of internal info which is not otherwise accessible.
**	For NT removed the PF11 -Enter commands screen, it is not needed as the user
**	can simply open another window.
**	Corrected the link-level logic when started from WSHELL.
**
**	Revision 1.32  1996-08-26 17:11:37-07  gsl
**	Change to call workstation() directly (instead of extract).
**	Removed pre-confirmation screen from (HELP)->(13) screen.
**	and added the personality path
**
**	Revision 1.31  1996-08-19 14:58:06-07  gsl
**	Fix display of platform and computername
**
**	Revision 1.30  1996-07-15 10:24:00-07  gsl
**	Fix for NT
**
**	Revision 1.29  1996-07-08 11:29:19-07  gsl
**	Add missing iclude
**	include.
**
**	Revision 1.28  1996-06-28 16:58:13-07  gsl
**	fix external declarations
**	,
**
**	Revision 1.27  1996-01-08 02:30:55-08  gsl
**	change copyright to 1996
**
 * Revision 1.26  1995/09/25  17:07:51  gsl
 * change to use osddefs.h
 *
 * Revision 1.25  1995/09/25  16:15:43  gsl
 * change _AIX to AIX to standardize the defines
 * Change ifdefs from unix to FUNC_UNAME to be active only when uname()
 * is available.
 *
 * Revision 1.24  1995/06/14  15:20:55  gsl
 * Changed to use wisp_version()
 *
 * Revision 1.23  1995/06/13  14:48:30  gsl
 * fix warning
 *
 * Revision 1.22  1995/06/13  14:36:33  gsl
 * line 1954, change 0240 to POSITION_CURSOR|UNLOCK_KEYBOARD
 *
 * Revision 1.21  1995/05/01  10:52:11  gsl
 * Reformat the copyright screen (HELP)->(32) and add Platform info.
 * Removed the license.h stuff as it was never used.
 *
 * Revision 1.20  1995/04/25  09:55:04  gsl
 * drcs state V3_3_15
 *
 * Revision 1.19  1995/04/24  12:51:23  gsl
 * Standardized all the PFKEY tags
 *
 * Revision 1.18  1995/04/17  11:48:01  gsl
 * drcs state V3_3_14
 *
 * Revision 1.17  1995/04/12  15:46:45  gsl
 * fixed compiler warnings
 *
 * Revision 1.16  1995/03/27  16:16:47  gsl
 * make wcurwidth local and set with call to vwang_wcurwidth()
 *
 * Revision 1.15  1995/03/10  11:33:42  gsl
 * replace video calls with vwang calls
 *
 * Revision 1.14  1995/03/09  16:57:18  gsl
 * change video calls to vwang calls
 *
 * Revision 1.13  1995/03/09  13:40:33  gsl
 * replaced synch_required with calls to vwang_set_synch()
 *
 * Revision 1.12  1995/03/07  10:22:18  gsl
 * replace 24, 80, 1924 with defines
 *
 * Revision 1.11  1995/02/17  16:11:36  gsl
 * remove an unneeded synch_required flag
 *
 * Revision 1.10  1995/02/17  13:05:48  gsl
 * change all the vre stuff to werr_message_box()
 *
 * Revision 1.9  1995/02/15  16:32:32  gsl
 * Removed the VIDEO calls that updated the clock and replace
 * them with vwang calls.
 *
 * Revision 1.8  1995/02/15  11:34:12  gsl
 * Prototyped everything and added standard headers.
 * For AIX changed system command to use "smit -C"
 *
**
**
*/
