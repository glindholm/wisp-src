/*
** WISP - Wang Interchange Source Processor
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
*/

/*
**	File:		wshelp.c
**
**	Project:	wisp/lib
**
**	Purpose:	WISP command processor
**
**	Routines:	
**	WL_wsh_help()
**	WL_get_psb_char()
**	wsh_progprnt()
**	WL_wsc_init()
**	WL_put_screen_text()
**	WL_put_screen_text_centered()
**	WL_get_screen_text()
**	WL_ishelpactive()
**	WL_sethelpactive()
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
#include <sys/wait.h>
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
#include "cobscrn.h"
#include "wfiledis.h"
#include "wmalloc.h"
#include "vssubs.h"

#ifdef WIN32
#include "isonames.h"
#endif

/*
**	Structures and Defines
*/

#ifndef WEXITSTATUS
#define WIFEXITED(x)    ( !((x) & 0xff) )
/* evaluates to the low-order 8 bits of the child exit status   */
#define WEXITSTATUS(x)  (int)(WIFEXITED(x) ? (((x) >> 8) & 0xff) : -1)
/* evaluates to a non-zero value if status returned for abnormal termination */
#endif

/*
**	Globals and Externals
*/
extern struct save_screen *VL_vscrn_stack;						/* Reference to external video struct.	*/


/*
**	Static data
*/

static int	wang_style=1;
static int	g_prog_running=0;
static int	g_savelevel;
static char	run_complete_message[80];

/*
**	Function Prototypes
*/
extern int WL_wsystem_interactive(const char *cmd);
static int wsh_progprnt(int scrn_seq_no);						/* The screen seq. no.  Starts at 1.	*/

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
static const char* sysadmin_cmd(void);
static void wsh_sysadmin(void);
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

static void wsh_display_errlog(void) 	{ WL_link_display(WL_werrpath()); }

static void wsh_print_prog_screen(void) { wsh_progprnt(1); }
static void wsh_print_cmd_screen(void)  { /* wsh_progprnt(0); */ WL_screen_print(); }
static void wsh_run_prog_prompt(void)	{ wsh_run_program("        "); }	    

static void wsh_command(void)
{
#ifdef unix
	static int first = 1;
	
	if (first)
	{
		/*
		**	If the WISPPS1 option is used then set the environment
		**	variable PS1=$(WISPPS1) before shelling-out.
		**
		**	Note: Only have to do this once as we keep PS1 set.
		**	This is the only place we "shell-out". 
		**	Upon return from this process PS1 will revert.
		*/
		const char *ptr = WL_get_wisp_option("WISPPS1");
		first = 0;
		
		if (ptr != NULL)
		{
			char buff[200];

			if (sizeof(buff) > (strlen(ptr) + 5))
			{
				sprintf(buff,"PS1=%s", ptr);
				WL_setenvstr(buff);
			}
			else
			{
				WL_wtrace("WSHELP","PS1","WISPPS1 option is too long to use.");
			}
		}
	}
	
	WL_wsystem_interactive(wispshellexe());

	WL_setlevel(g_savelevel);							/* Restore the link-level		*/
#endif
}

static void wsh_manage_files(void)	
{ 	
	WL_mngfile(); 
}

/*
**	WISPHELP is the cobol callable interface to the HELP screen.
*/
void WISPHELP(void)
{
	wisp_set_progname("");
	wisp_set_screenname("");
	
	WL_wsh_help(1);
}

int WL_wsh_help(int prog_running)							/* Put up the shell screen.		*/
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

	
	WL_wtrace_entry("WISPHELP");

	WL_wpload();									/* Load the personality information.	*/
	WL_get_defs(DEFAULTS_FLAGS,&dflags);						/* Get the defaults flags		*/

	if ( !(dflags & HELP_ENABLED))							/* Help is disabled.			*/
	{
		vwang_write_bell();							/* Ring the bell.			*/
		return 0;
	}

	/*
	**	Print screen and setting terminal attributes is not supported
	**	with native screens.
	*/
	if (wisp_nativescreens())
	{
		dflags &= ~(HELP_PRINT_SCREEN | HELP_TERMINAL);
	}

	/*
	**	The linklevel is incremented to prevent
	**	sideeffects of WL_ppunlink() that can occur 
	**	when another process is spawned from help.
	*/
	g_savelevel = WL_linklevel();

	if (WL_opt_helpstyle == 2)
	{
		wang_style = 0;
	}
	else
	{
		wang_style = 1;
	}

	g_prog_running = prog_running;

	strcpy(allowed_pfkeys,"00");							/* Always allow return.			*/
	WL_sethelpactive(TRUE);								/* Help is now active.			*/

	strcpy(id,WL_longuid());								/* Get the user id.			*/
	sprintf(tty_str,"%4d",WL_workstation());						/* Convert value to a string.		*/
	strcpy(run_complete_message,"  ");						/* Blank the run complete message	*/


	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	if (wang_style)
	{
		time_row = 4;
		time_col = 50;

		wsb_add_text(hWsb, 1,  0, "***  WISP Command Processor  ***");
		wsb_add_text(hWsb, 2,  0, WISP_OWNER);

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
				sprintf(temp, "Welcome to %.32s - %.32s", unix_name.sysname, unix_name.nodename);
			}
		}
#endif
#ifdef WIN32
		if (0==strcmp(WL_computername(NULL),DEF_COMPUTERNAME))
		{
			strcpy(temp, "Welcome to Windows");
		}
		else
		{
			sprintf(temp, "Welcome to Windows - %.50s", WL_computername(NULL));
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
			if (*(wisp_get_progname()))					/* We know the program name		*/
			{
				if (*(wisp_get_screenname()))				/* We also know the screen name		*/
				{
					sprintf(temp, "[Active program: %s    Screen: %s]", 
						wisp_get_progname(), wisp_get_screenname());
				}
				else
				{
					sprintf(temp, "[Active program: %s]", 
						wisp_get_progname());
				}
			}
			else
			{
				if (wisp_get_runname()[0] != ' ')
				{
					sprintf(temp,"[Active program: %8.8s]",wisp_get_runname());
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
		col1 = 5;
		col2 = 45;
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

	if (wang_style && WL_pfkeys12())
	{
		if ((dflags & HELP_SET_FILES)   ||
		    (dflags & HELP_SET_PRINTER) ||
		    (dflags & HELP_SET_PROC))
		{
			wsb_add_text(hWsb, row+1, col1, "(2) SET Usage Constants");
			strcat(allowed_pfkeys,"02");
			func_vector[2] = wsh_usage_file;
		}

		if (dflags & HELP_ERROR_LOG)
		{
			wsb_add_text(hWsb, row+2, col1, "(3) SHOW Error Log");
			strcat(allowed_pfkeys, "03");
			func_vector[3] = wsh_display_errlog;
		}

#ifdef unix
		if ( ((dflags & HELP_PRINT_QUEUE) && WL_opt_printqueue_manager) ||
		     ((dflags & HELP_BATCH_QUEUE) && WL_opt_batchman) )
		{
			wsb_add_text(hWsb, row+3,  col1, "(4) Manage QUEUES");
			strcat(allowed_pfkeys,"04");
			func_vector[4] = wsh_queuemgmnt;

		}
#endif

		if (dflags & HELP_MANAGE_FILES_LIBS)
		{
			wsb_add_text(hWsb, row+4, col1, "(5) Manage FILES/LIBRARIES");
			strcat(allowed_pfkeys, "05");
			func_vector[5] = wsh_manage_files;
		}
		if (dflags & HELP_MANAGE_SYSTEM)
		{
			if (sysadmin_cmd())
			{
				wsb_add_text(hWsb, row+5, col1, "(6) Manage SYSTEM");
				strcat(allowed_pfkeys, "06");
				func_vector[6] = wsh_sysadmin;
			}
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

		if (prog_running && (dflags & HELP_PRINT_SCREEN) && !wisp_get_noprogscrn())
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
	else if (wang_style && !WL_pfkeys12())
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
			wsb_add_text(hWsb, row+4, col1, "(5) Manage FILES/LIBRARIES");
			strcat(allowed_pfkeys, "05");
			func_vector[5] = wsh_manage_files;
		}
		if (dflags & HELP_MANAGE_SYSTEM)
		{
			if (sysadmin_cmd())
			{
				wsb_add_text(hWsb, row+5, col1, "(6) Manage SYSTEM");
				strcat(allowed_pfkeys, "06");
				func_vector[6] = wsh_sysadmin;
			}
		}

#ifdef unix
		if ((dflags & HELP_PRINT_QUEUE) || 
		    (dflags & HELP_BATCH_QUEUE) )
		{
			if (WL_opt_printqueue_manager && (dflags & HELP_PRINT_QUEUE) && 
			    WL_opt_batchman && (dflags & HELP_BATCH_QUEUE) )
			{
				wsb_add_text(hWsb, row+6,  col1, "(7) Manage QUEUES");
				strcat(allowed_pfkeys,"07");
				func_vector[7] = wsh_queuemgmnt;
			}
			else if (WL_opt_printqueue_manager && (dflags & HELP_PRINT_QUEUE))
			{
				wsb_add_text(hWsb, row+6,  col1, "(7) Manage PRINT QUEUE");
				strcat(allowed_pfkeys,"07");
				func_vector[7] = wsh_queue_print;
			}
			else if (WL_opt_batchman && (dflags & HELP_BATCH_QUEUE))
			{
				wsb_add_text(hWsb, row+6,  col1, "(7) Manage BATCH QUEUE");
				strcat(allowed_pfkeys,"07");
				func_vector[7] = wsh_queue_batch;
			}
		}
#endif

		if (dflags & HELP_ERROR_LOG)
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

#ifdef unix
		if (dflags & HELP_COMMANDS)
		{
			wsb_add_text(hWsb, row+2, col2, "(11) Enter COMMANDS");
			strcat(allowed_pfkeys,"11");
			func_vector[11] = wsh_command;
		}
#endif

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

		if (prog_running && (dflags & HELP_PRINT_SCREEN) && !wisp_get_noprogscrn())
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
			wsb_add_menu_item(hWsb, row+4, col1, 5, "Manage Files");
			strcat(allowed_pfkeys, "05");
			func_vector[5] = wsh_manage_files;
		}

		if (dflags & HELP_MANAGE_SYSTEM)
		{
			if (sysadmin_cmd())
			{
				wsb_add_menu_item(hWsb, row+5, col1, 6, "System Admin");
				strcat(allowed_pfkeys, "06");
				func_vector[6] = wsh_sysadmin;
			}
		}

#ifdef unix
		if ((dflags & HELP_PRINT_QUEUE) || 
		    (dflags & HELP_BATCH_QUEUE) )
		{
			if (WL_opt_printqueue_manager && (dflags & HELP_PRINT_QUEUE) && 
			    WL_opt_batchman && (dflags & HELP_BATCH_QUEUE) )
			{
				wsb_add_menu_item(hWsb, row+6, col1, 7, "Queues");
				strcat(allowed_pfkeys,"07");
				func_vector[7] = wsh_queuemgmnt;
			}
			else if (WL_opt_printqueue_manager && (dflags & HELP_PRINT_QUEUE))
			{
				wsb_add_menu_item(hWsb, row+6, col1, 7, "Print Queue");
				strcat(allowed_pfkeys,"07");
				func_vector[7] = wsh_queue_print;
			}
			else if (WL_opt_batchman && (dflags & HELP_BATCH_QUEUE))
			{
				wsb_add_menu_item(hWsb, row+6, col1, 7, "Batch Queue");
				strcat(allowed_pfkeys,"07");
				func_vector[7] = wsh_queue_batch;
			}
		}
#endif

		if (dflags & HELP_ERROR_LOG)
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

#ifdef unix
		if (dflags & HELP_COMMANDS)
		{
			wsb_add_menu_item(hWsb, row+2, col2, 11, "Enter UNIX commands");
			strcat(allowed_pfkeys,"11");
			func_vector[11] = wsh_command;
		}
#endif

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

		if (prog_running && (dflags & HELP_PRINT_SCREEN) && !wisp_get_noprogscrn())
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
				 (wang_style && 16 == pfval && !WL_pfkeys12()) ||
				 (wang_style && 12 == pfval &&  WL_pfkeys12())   )
			{
				if (prog_running)
				{
					if (wsh_cancel())
					{
						wisp_set_LINKCOMPCODE(16);
						SETRETCODE("016");
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
	
	if (WL_linklevel() != g_savelevel) WL_setlevel(g_savelevel);			/* Restore the link-level		*/

	WL_sethelpactive(FALSE);								/* Help no longer active.		*/
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
		col = 5;

		wsb_build_non_wang_base(hWsb);
		wsb_add_menu_item(hWsb, 11, col, 1, "Return to MAIN HELP menu");
	}


	WL_get_defs(DEFAULTS_FLAGS,&dflags);						/* Get the defaults flags		*/

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
	WL_get_psb_char:Get pseudo blank character.
			This routine is passed a selection character (def_select) and returns the pseudo blank char (pb_char)
			and the real selection char (pb_select).
			It knowns how to handle converison from old PERSONALITY files that stored the pseudo blank char
			instead of the selection char.

	char	def_select;		The selection character "B1234"
	char	*pb_char;		The Pseudo blank char (returned)
	char	*pb_select;		The selection char  (returned)

*/
void WL_get_psb_char(char def_select,char *pb_char, char *pb_select)
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
	default:								/* OLD personality stored char itself	*/
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
	unsigned char screen[WSB_LENGTH+1];						/* Pointer to working screen routine.	*/
	unsigned char function, lines, no_mod[2];					/* Working variables.			*/
	char pb_char[4], pb_rend[4], pb_select[4], term[2];	
	int pb_chset, pb_r;
	int valid, i, changed;
	char	def_psb_select;
	int4	def_psb_charset;
	int4	def_psb_rendition;

	WL_wsc_init(screen,0,0);							/* Initialize the screen layout.	*/

	WL_put_screen_text(screen, 1,15,FAC_DEFAULT_TEXT,"*** Change the pseudo blank characteristics ***");
	WL_put_screen_text(screen, 5, 2,FAC_DEFAULT_TEXT,"Pseudo blank character      :");
	WL_put_screen_text(screen, 6, 2,FAC_DEFAULT_TEXT,"Using pseudo blank rendition:");
	WL_put_screen_text(screen,10,10,FAC_DEFAULT_TEXT,"Character Options");
	WL_put_screen_text(screen,10,40,FAC_DEFAULT_TEXT,"Rendition Options");
	WL_put_screen_text(screen,12,10,FAC_DEFAULT_TEXT,"B    blank ");
	WL_put_screen_text(screen,13,10,FAC_DEFAULT_TEXT,"1");
	WL_put_screen_text(screen,14,10,FAC_DEFAULT_TEXT,"2");
	WL_put_screen_text(screen,15,10,FAC_DEFAULT_TEXT,"3");
	WL_put_screen_text(screen,16,10,FAC_DEFAULT_TEXT,"4");
	WL_put_screen_text(screen,12,40,FAC_DEFAULT_TEXT,"N  -  NORMAL");
	WL_put_screen_text(screen,13,40,FAC_DEFAULT_TEXT,"U  -  UNDERSCORE");
	WL_put_screen_text(screen,14,40,FAC_DEFAULT_TEXT,"R  -  REVERSE");
	WL_put_screen_text(screen,15,40,FAC_DEFAULT_TEXT,"B  -  Both UNDERSCORE and REVERSE");
	WL_put_screen_text(screen,18,2,FAC_DEFAULT_TEXT,"The pseudo blank character will");
	WL_put_screen_text(screen,19,2,FAC_DEFAULT_TEXT,"fill the modifiable fields with");
	WL_put_screen_text(screen,20,2,FAC_DEFAULT_TEXT,"the indicated character.");
	WL_put_screen_text(screen,18,37,FAC_DEFAULT_TEXT,"The indicated rendition will be turned");
	WL_put_screen_text(screen,19,37,FAC_DEFAULT_TEXT,"on for modifiable fields and display");
	WL_put_screen_text(screen,20,37,FAC_DEFAULT_TEXT,"with the pseudo blank character.");
	WL_put_screen_text(screen,24,2,FAC_DEFAULT_TEXT,"Change the information as appropriate and press (ENTER), (1) to exit.");

	WL_get_defs(DEFAULTS_PSB_CHAR,&def_psb_select);
	WL_get_defs(DEFAULTS_PSB_SET,&def_psb_charset);
	WL_get_defs(DEFAULTS_PSB_REN,&def_psb_rendition);

	WL_get_psb_char(def_psb_select,pb_char,pb_select);
	pb_select[1] = '\0';								/* Null terminate the string.		*/
	pb_char[1] = '\0';								/* Null terminate the string.		*/
	pb_chset = def_psb_charset;
	pb_r = def_psb_rendition;
	if (pb_r == 2)  pb_rend[0] = 'U';						/* Rendition is UNDERLINE.		*/
	else if (pb_r == 8)  pb_rend[0] = 'R';						/* Rendition is REVERSE.		*/
	else if (pb_r == 10) pb_rend[0] = 'B';						/* Both UNDERSCORE and REVERSE.		*/
	else pb_rend[0] = 'N';								/* Rendition is Normal.			*/
	pb_rend[1] = '\0';								/* Null terminate the string.		*/

	WL_put_screen_text(screen,5,40,FAC_DEFAULT_FIELD,pb_select);			/* Print correct char set from choices.	*/
	WL_put_screen_text(screen, 6,40,FAC_DEFAULT_FIELD,pb_rend);			/* Print correct rendition from choices.*/

	no_mod[0] = no_mod[1] = ' ';
	function = WRITE_ALL;								/* Use display full screen function.	*/
	lines = WSB_ROWS;
	vwang(&function,screen,&lines,"0001X",term,no_mod);				/* Call Wang emulation to fill screen.	*/
	VL_vcharset(VCS_GRAPHICS);

	VL_vmove(12,15);	VL_vprint("%c", PBLANK1);
	VL_vmove(13,15);	VL_vprint("%c", PBLANK2);
	VL_vmove(14,15);	VL_vprint("%c", PBLANK3);
	VL_vmove(15,15);	VL_vprint("%c", PBLANK4);

	VL_vmove(4,33);
	VL_vprint("%c", *pb_char);							/* Print pseudo blank character.	*/
	VL_vcharset(VCS_DEFAULT);
	vwang_set_synch(FALSE);								/* Vprint sets the flag to TRUE which I	*/
											/* don't want so reset to FALSE.	*/
	function = READ_ALTERED;							/* Use read altered function.		*/
	lines = WSB_ROWS;
	
	valid = TRUE;									/* Assume it starts with valid values	*/
	changed = FALSE;								/* Nothing has changed yet		*/

	for(;;)										/* Repeat until data input is valid.	*/
	{
		no_mod[0] = no_mod[1] = ' ';
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

			WL_get_screen_text(screen, 5,40,pb_select);			/* Input the pseudo blank option.	*/
			WL_get_screen_text(screen, 6,40,pb_rend);			/* Input the rendition.			*/

			pb_select[0] = toupper(pb_select[0]);				/* Make sure the char is uppercase.	*/
			if ((i = strpos("B1234",pb_select)) < 0)			/* Was a valid char option given?	*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				WL_put_screen_text(screen, 5,40,FAC_ERROR_FIELD,pb_select);		/* Make it blink.			*/
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
				WL_put_screen_text(screen, 6,40,FAC_ERROR_FIELD,pb_rend);		/* Make it blink.			*/
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
		WL_set_defs(DEFAULTS_PSB_CHAR,&def_psb_select);
		WL_set_defs(DEFAULTS_PSB_SET,&def_psb_charset);
		WL_set_defs(DEFAULTS_PSB_REN,&def_psb_rendition);
		WL_save_defaults();							/* Write out the personality info.	*/
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

	WL_get_defs(DEFAULTS_AUTOTAB,&autotab);						/* Initialize fields from personality.	*/
	WL_get_defs(DEFAULTS_AUTOMOVE,&automove);
	WL_get_defs(DEFAULTS_MP_CURSOR,&mp_cursor);

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
		WL_set_defs(DEFAULTS_AUTOTAB,&autotab);
		WL_set_defs(DEFAULTS_AUTOMOVE,&automove);
		WL_set_defs(DEFAULTS_MP_CURSOR,&mp_cursor);
		WL_save_defaults();							/* Write out the personality info.	*/
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

	WL_get_defs(DEFAULTS_BGCHANGE,&bgchange);						/* Initialize fields from personality.	*/
	WL_get_defs(DEFAULTS_BGCOLOR,&bgcolor);
	WL_get_defs(DEFAULTS_EXCOLOR,&excolor);

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
		WL_set_defs(DEFAULTS_BGCHANGE,&bgchange);
		WL_set_defs(DEFAULTS_BGCOLOR,&bgcolor);
		WL_set_defs(DEFAULTS_EXCOLOR,&excolor);
		WL_save_defaults();							/* Write out the personality info.	*/
		if (bgchange)
		{
			if (excolor) VL_vonexit(NORMALIZE|CLEAR_SCREEN|VSCREEN_LIGHT);
			else         VL_vonexit(NORMALIZE|CLEAR_SCREEN|VSCREEN_DARK);
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

	WL_get_defs(DEFAULTS_IL,def_inlib);		WL_get_defs(DEFAULTS_IV,def_invol);
	WL_get_defs(DEFAULTS_OL,def_outlib);		WL_get_defs(DEFAULTS_OV,def_outvol);
	WL_get_defs(DEFAULTS_RL,def_runlib);		WL_get_defs(DEFAULTS_RV,def_runvol);
	WL_get_defs(DEFAULTS_SL,def_spoolib);		WL_get_defs(DEFAULTS_SV,def_spoolvol);
	WL_get_defs(DEFAULTS_WL,def_worklib);		WL_get_defs(DEFAULTS_WV,def_workvol);

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

		WL_set_defs(DEFAULTS_IL,def_inlib);		WL_set_defs(DEFAULTS_IV,def_invol);
		WL_set_defs(DEFAULTS_OL,def_outlib);		WL_set_defs(DEFAULTS_OV,def_outvol);
		WL_set_defs(DEFAULTS_RL,def_runlib);		WL_set_defs(DEFAULTS_RV,def_runvol);
		WL_set_defs(DEFAULTS_SL,def_spoolib);		WL_set_defs(DEFAULTS_SV,def_spoolvol);
								WL_set_defs(DEFAULTS_WV,def_workvol);

		WL_save_defaults();							/* Write out the personality info.	*/
	}

	wsb_delete(hWsb);

	return rc;
}

static void wsh_copyright(void)								/* Display the copyright screen.	*/
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;
	char 	buff[256];
	char	temp[256];

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	wsb_add_text(hWsb, 1, 0, "*** Copyright Information ***");
	wsb_add_text(hWsb, 4, 0, "The WISP runtime library");
	wsb_add_text(hWsb, 5, 0, "Copyright (c) 1989-" WISP_COPYRIGHT_YEAR_STR "  " WISP_OWNER);
	wsb_add_text(hWsb, 6, 0, WISP_ADDRESS_FULL);
	wsb_add_text(hWsb, 7, 0, "Web: " WISP_WEBSITE "   Email: " WISP_EMAIL);

	sprintf(buff, "WISP Version=[%s]", wisp_version());
	wsb_add_text(hWsb, 12, 0, buff);

	sprintf(temp, "Platform = %s (%2.2s - %s)",
		WL_platform_name(),
		WL_platform_code(),
		WL_platform_define());
	wsb_add_text(hWsb, 14, 0, temp);

#ifdef FUNC_UNAME
	{
		struct utsname unix_name;
		if (-1 != uname(&unix_name))
		{
			sprintf(temp, "OS Sysname: %.65s", unix_name.sysname);
			wsb_add_text(hWsb, 16, 0, temp);
			sprintf(temp, "OS Nodename: %.65s", unix_name.nodename);
			wsb_add_text(hWsb, 17, 0, temp);
			sprintf(temp, "OS Release: %.65s", unix_name.release);
			wsb_add_text(hWsb, 18, 0, temp);
			sprintf(temp, "OS Version: %.65s", unix_name.version);
			wsb_add_text(hWsb, 19, 0, temp);
			sprintf(temp, "OS Machine: %.65s", unix_name.machine);
			wsb_add_text(hWsb, 20, 0, temp);
		}
	}
#endif
#ifdef WIN32
	sprintf(temp, "Windows %.65s", WL_computername(NULL));
	wsb_add_text(hWsb, 16, 0, temp);
#endif

	/*
	**	Bottom of screen
	*/

	if (!wisp_nativescreens())
	{
		wsb_add_text(hWsb, 24, 25,"(ENTER) Return");

		if (WL_pfkeys12())
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

	if (!wisp_nativescreens())
	{
		if (WL_pfkeys12())
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

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	wsb_add_text(hWsb, 1, 0, "*** Usage Constants ***");

	row = 3;
	col = 2;
	
	WL_set_va_count(2);
	EXTRACT2("IL",lib); lib[SIZEOF_LIB] = '\0';
	WL_set_va_count(2);
	EXTRACT2("IV",vol); vol[SIZEOF_VOL] = '\0';
	sprintf(buff,"IL = %8.8s  IV = %6.6s", lib, vol);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("OL",lib); lib[SIZEOF_LIB] = '\0';
	WL_set_va_count(2);
	EXTRACT2("OV",vol); vol[SIZEOF_VOL] = '\0';
	sprintf(buff,"OL = %8.8s  OV = %6.6s", lib, vol);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("RL",lib); lib[SIZEOF_LIB] = '\0';
	WL_set_va_count(2);
	EXTRACT2("RV",vol); vol[SIZEOF_VOL] = '\0';
	sprintf(buff,"RL = %8.8s  RV = %6.6s", lib, vol);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("SL",lib); lib[SIZEOF_LIB] = '\0';
	WL_set_va_count(2);
	EXTRACT2("SV",vol); vol[SIZEOF_VOL] = '\0';
	sprintf(buff,"SL = %8.8s  SV = %6.6s", lib, vol);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("WL",lib); lib[SIZEOF_LIB] = '\0';
	WL_set_va_count(2);
	EXTRACT2("WV",vol); vol[SIZEOF_VOL] = '\0';
	sprintf(buff,"WL = %8.8s  WV = %6.6s", lib, vol);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("PL",lib); lib[SIZEOF_LIB] = '\0';
	WL_set_va_count(2);
	EXTRACT2("PV",vol); vol[SIZEOF_VOL] = '\0';
	sprintf(buff,"PL = %8.8s  PV = %6.6s", lib, vol);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("CL",lib); lib[SIZEOF_LIB] = '\0';
	WL_set_va_count(2);
	EXTRACT2("CV",vol); vol[SIZEOF_VOL] = '\0';
	sprintf(buff,"CL = %8.8s  CV = %6.6s", lib, vol);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("XV",defs); defs[SIZEOF_VOL] = '\0';
	sprintf(buff,"XV = %s", defs);
	wsb_add_text(hWsb, row++, col+15,buff);


	row = 3;
	col = 30;

	WL_set_va_count(2);
	EXTRACT2("PM",defs); defs[1] = '\0';
	sprintf(buff,"PM = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("PC",defs); defs[1] = '\0';
	sprintf(buff,"PC = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("JC",defs); defs[1] = '\0';
	sprintf(buff,"JC = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("JS",defs); defs[1] = '\0';
	sprintf(buff,"JS = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("TT",defs); defs[1] = '\0';
	sprintf(buff,"TT = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("CF",defs); defs[8] = '\0';
	sprintf(buff,"CF = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("C#",defs); defs[4] = '\0';
	sprintf(buff,"C# = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("W$",defs); defs[8] = '\0';
	sprintf(buff,"W$ = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	/*
	   INT4 
	*/

	row = 3;
	col = 45;

	WL_set_va_count(2);
	EXTRACT2("E:",&int4defs);
	WL_wswap(&int4defs);
	sprintf(buff,"E: = %ld", (long)int4defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("FN",&int4defs);
	WL_wswap(&int4defs);
	sprintf(buff,"FN = %ld", (long)int4defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("G#",&int4defs);
	WL_wswap(&int4defs);
	sprintf(buff,"G# = %ld", (long)int4defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("PR",&int4defs);
	WL_wswap(&int4defs);
	sprintf(buff,"PR = %ld", (long)int4defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("JL",&int4defs);
	WL_wswap(&int4defs);
	sprintf(buff,"JL = %ld", (long)int4defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("LI",&int4defs);
	WL_wswap(&int4defs);
	sprintf(buff,"LI = %ld", (long)int4defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("P:",&int4defs);
	WL_wswap(&int4defs);
	sprintf(buff,"P: = %ld", (long)int4defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("T#",&int4defs);
	WL_wswap(&int4defs);
	sprintf(buff,"T# = %ld", (long)int4defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("W#",&int4defs);
	WL_wswap(&int4defs);
	sprintf(buff,"W# = %ld", (long)int4defs);
	wsb_add_text(hWsb, row++, col,buff);


	/*
	**	Second half of screen
	*/
	row = 12;
	col = 2;

	WL_set_va_count(2);
	EXTRACT2("ID",defs); defs[3] = '\0';
	sprintf(buff,"ID = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("I8",defs); defs[8] = '\0';
	sprintf(buff,"I8 = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("IX",defs); defs[32] = '\0';
	sprintf(buff,"IX = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("LU",defs); defs[32] = '\0';
	sprintf(buff,"LU = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("NA",defs); defs[24] = '\0';
	sprintf(buff,"NA = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);

	WL_set_va_count(2);
	EXTRACT2("S$",defs); defs[15] = '\0';
	sprintf(buff,"S$ = %s", defs);
	wsb_add_text(hWsb, row++, col,buff);


	/*
	**	Bottom of screen
	*/
	wsb_add_text(hWsb, 24, 25,"(ENTER) Return");

	if (!wisp_nativescreens())
	{
		if (WL_pfkeys12())
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

	row++;

	sprintf(buff,"RCFILE        = %s", wisprcfilepath());
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"TEMPDEFAULTS  = %s", wisp_temp_defaults_path(NULL));
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"SHAREDMEMCTL  = %s", WL_sm_ctlfile());
	wsb_add_text(hWsb, row++, col,buff);

	row++;

	/*
	**	Bottom of screen
	*/
	sprintf(buff,"PATH (env)    = %-.703s", (ptr=getenv("PATH")) ? ptr : "(nil)");  /* 703 = 9 line * 80 columns - 17 */

	wsb_add_text(hWsb, row++, 2,buff);
	

	wsb_add_text(hWsb, 24, 25,"(ENTER) Return");

	if (!wisp_nativescreens())
	{
		if (WL_pfkeys12())
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

	sprintf(buff,"licensefile = %s", WLIC_license_filepath());
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"WISPTMPDIR  = %s", wisptmpbasedir(NULL));
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"systmpdir   = %s", WL_systmpdir(NULL));
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"wispterm    = %s", wisptermfilepath(NULL));
	wsb_add_text(hWsb, row++, col,buff);

	row++;

	sprintf(buff,"MACHID      = %s", (0==WL_getmachineid(value)) ? value : "(unknown)");
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"COMPNAME    = %s", WL_computername(NULL));
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"SERVER      = %s", wispserver());
	wsb_add_text(hWsb, row++, col,buff);

	if (wisp_acu_cobol())
	{
		strcpy(value,"ACU");
	}
	else if (wisp_mf_cobol())
	{
		strcpy(value,"MF");
	}
	else
	{
		strcpy(value,"(none)");
	}
	sprintf(buff,"RTS         = %s", value);
	wsb_add_text(hWsb, row++, col,buff);

	
	
	/*
	**	Bottom right quarter of screen
	*/

	col = 40;
	row = 12;

	sprintf(buff,"linklevel = %d", WL_linklevel());
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"sortmem   = %d", wispsortmemk());
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"ttyname   = %s", ttyname(0));
	wsb_add_text(hWsb, row++, col,buff);

	WL_ttyid5(value);
	sprintf(buff,"ttyid5    = %s", value);
	wsb_add_text(hWsb, row++, col,buff);

	/*
	**	Next to bottom
	*/
	col = 2;
	row = 16;
	
	WL_wrunconfig(&cfg);

	sprintf(buff,"wrun_cob    = %s", cfg.wrun_cobtype);
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"wrun_run    = %s", cfg.wrun_runcbl);
	wsb_add_text(hWsb, row++, col,buff);

	sprintf(buff,"wrun_opt    = %s", cfg.wrun_options);
	wsb_add_text(hWsb, row++, col,buff);
	

	sprintf(buff,"LINKPATH    = %-.305s", wisplinkpath()); /* 308 = 4 * 80 - 15 */
	wsb_add_text(hWsb, 20, 2,buff);
	

	/*
	**	Bottom of screen
	*/
	wsb_add_text(hWsb, 24, 25,"(ENTER) Return");

	if (!wisp_nativescreens())
	{
		if (WL_pfkeys12())
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

#ifdef unix
static void wsh_queuemgmnt(void)
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;
	char 	okkeys[81];
	int	col;
	uint4   dflags;
	
	WL_get_defs(DEFAULTS_FLAGS,&dflags);						/* Get the defaults flags		*/

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();

	okkeys[0] = '\0';
	
	if (wang_style)
	{
		col = 21;
		
		wsb_add_text(hWsb,  1,  0, "***  Manage Queues  ***");
		wsb_add_text(hWsb, 24,  0, "Press (HELP) to return to the command processor, (1) to exit.");
		strcat(okkeys,"01");
	}
	else
	{
		col = 5;

		wsb_build_non_wang_base(hWsb);
		wsb_add_menu_item(hWsb, 12, col, 1, "Return to MAIN HELP menu");
	}

	if (WL_opt_printqueue_manager && (dflags & HELP_PRINT_QUEUE) )
	{
		if (wang_style)
			wsb_add_text(hWsb,  9, col, "(2) Manage Print Queue.");
		else
			wsb_add_menu_item(hWsb, 13, col, 2, "Manage Print Queue");
		strcat(okkeys,"02");
	}
	
	if (WL_opt_batchman && (dflags & HELP_BATCH_QUEUE))
	{
		if (wang_style)
			wsb_add_text(hWsb, 11, col, "(3) Manage Batch Queue.");
		else
			wsb_add_menu_item(hWsb, 14, col, 3, "Manage Batch Queue");
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
		else if (pfkey == 15 || (pfkey == 11 && WL_pfkeys12())) 
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
	int	rc;
	const char *qtype;
	const char *queue_cmd;
	char	errbuff[1024];

	errno = 0;
	
	if (type==1)
	{
		queue_cmd = WL_opt_printqueue_manager;
		qtype="Print Queue Manager";

		if (NULL == queue_cmd)
		{
			WL_werr_message_box("Print Queue Manager program has not been defined.");
			return;
		}

		rc = WL_wsystem_interactive(queue_cmd);
		
	}
	else 
	{
		qtype = "Batch Queue Manager";
		queue_cmd = WL_batchman_name;

		if (NULL == queue_cmd)
		{
			WL_werr_message_box("Batch Queue Manager program has not been defined.");
			return;
		}

		rc = WL_wsystem_interactive(queue_cmd);

	}

	if (rc)
	{
		switch(WEXITSTATUS(rc))
		{	
		      case 1:
			sprintf(errbuff,"%s could not be run. [cmd=\"%s\", exit=%d, rc=%d]",qtype, queue_cmd, 
				WEXITSTATUS(rc), rc);
			WL_werr_message_box(errbuff);
			break;
		      default:
			sprintf(errbuff,"%s command \"%s\" exited with exit code=%d, rc=%d, errno=%d",
				qtype, queue_cmd, WEXITSTATUS(rc), rc, errno);
			WL_werr_message_box(errbuff);
			break;
		}
	}
}
#endif

static int wsh_uc_print(void)								/* Set the usage constants.		*/
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;

	char mode[2], print_class[2], number[4], form[4], dplines[4];			/* Working versions of field data.	*/
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
	wsb_add_text(hWsb,12, 2,"Default spooled print file print class.      ('A' thru 'Z'):  PRTCLASS =");
	wsb_add_text(hWsb,14, 2,"Default form number.                         (000 thru 254):  FORM_NUM =");
	wsb_add_text(hWsb,16, 2,"Default printer number:                                       PRINTER  =");
	wsb_add_text(hWsb,18, 2,"Default Lines per Page                       (000 thru 255):  LINES    =");

	wsb_add_text(hWsb,20,8,"Change the information as appropriate and press (ENTER)");
	wsb_add_text(hWsb,22,8,"or Press  (2) Set Submit Procedure Defaults");
	wsb_add_text(hWsb,23,8,"          (3) Set File Defaults");
	wsb_add_text(hWsb,24,8,"       (HELP) Return to the Command Processor");

	WL_get_defs(DEFAULTS_PM,mode);							/* Get the print mode data.		*/
	WL_get_defs(DEFAULTS_PC,print_class);						/* Get the print class data.		*/
	WL_get_defs(DEFAULTS_PR,&def_prt_num);						/* Get the printer number.		*/
	WL_get_defs(DEFAULTS_FN,&def_prt_form);						/* Get the printer form number.		*/
	WL_get_defs(DEFAULTS_LI,&def_prt_lines);					/* Get the default lines per page.	*/

	mode[1] = (char)0;
	print_class[1] = (char)0;
	sprintf(number, "%03d",def_prt_num);
	sprintf(form,   "%03d",def_prt_form);		
	sprintf(dplines,"%03d",def_prt_lines);	

	wsb_add_field(hWsb, 4,75,FAC_DEFAULT_FIELD,mode,1);
	wsb_add_field(hWsb,12,75,FAC_DEFAULT_FIELD,print_class,1);
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
		wsb_get_field(hWsb,12,75,print_class,1);
		wsb_get_field(hWsb,14,75,form,3);
		wsb_get_field(hWsb,16,75,number,3);
		wsb_get_field(hWsb,18,75,dplines,3);

		valid = 1;								/* Assume we're valid.			*/
		if (0 == pfkey)								/* Should we validate?			*/
		{
			mode[0] = toupper(mode[0]);					/* Make sure the char is uppercase.	*/
			if (strpos("OSHKP",mode) < 0)					/* Was a valid print class given?	*/
			{
				valid = 0;						/* Oops, then not valid.		*/
				wsb_add_field(hWsb, 4,75,FAC_ERROR_FIELD,mode,1);	/* Make it blink.			*/
				currow = 4;
				curcol = 75;
			}
			print_class[0] = toupper(print_class[0]);			/* Make print class upper case.		*/
						/***** ADD VALIDATION FOR PRINT CLASS HERE ******/
			tlines = atoi(dplines);						/* Convert lines to integer.		*/
			if (tlines < 0 || tlines > 255)
			{
				valid = 0;						/* Oops, then not valid.		*/
				wsb_add_field(hWsb,18,75,FAC_NUM_ERROR_FIELD,dplines,3);	/* Make it blink.		*/
				currow = 18;
				curcol = 75;
			}
		}

		if (valid != 0)
		{
			break;
		}
	}

	if (0 == pfkey)									/* Copy to data base?			*/
	{
		def_prt_num   = atoi(number);
		def_prt_form  = atoi(form);
		def_prt_lines = atoi(dplines);

		WL_set_defs(DEFAULTS_PM,mode);
		WL_set_defs(DEFAULTS_PC,print_class);
		WL_set_defs(DEFAULTS_PR,&def_prt_num);
		WL_set_defs(DEFAULTS_FN,&def_prt_form);
		WL_set_defs(DEFAULTS_LI,&def_prt_lines);

		WL_save_defaults();							/* Write out the personality info.	*/
	}

	wsb_delete(hWsb);

	return rc;
}

static int wsh_uc_submit(void)								/* Set the usage constants.		*/
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;
	char mode[2], job_class[2], t_hr[3], t_min[3], t_sec[3];			/* Working versions of field data.	*/
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

	WL_get_defs(DEFAULTS_JS,mode);							/* Get the proc status mode data.	*/
	WL_get_defs(DEFAULTS_JC,job_class);						/* Get the proc class data.		*/
	WL_get_defs(DEFAULTS_JL,def_proc_cpu);

	mode[1] = 0;
	job_class[1] = 0;

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
	wsb_add_field(hWsb,13,69,FAC_DEFAULT_FIELD,job_class,1);

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
		wsb_get_field(hWsb,13,69,job_class,1);
		wsb_get_field(hWsb,17,69,t_hr,2);
		wsb_get_field(hWsb,17,74,t_min,2);
		wsb_get_field(hWsb,17,79,t_sec,2);

		valid = 1;								/* Assume we're valid.			*/
		if (0 == pfkey)								/* Should we validate?			*/
		{
			mode[0] = toupper(mode[0]);					/* Make sure the char is uppercase.	*/
			if (strpos("RH",mode) < 0)					/* Was a valid print class given?	*/
			{
				valid = 0;						/* Oops, then not valid.		*/
				wsb_add_field(hWsb,11,69,FAC_ERROR_FIELD,mode,1);	/* Make it blink.			*/
				currow = 11;
				curcol = 69;
			}
			job_class[0] = toupper(job_class[0]);				/* Make class upper case.		*/

			if (!isdigit((int)t_hr[0])) t_hr[0] = '0';
			if (!isdigit((int)t_hr[1])) t_hr[1] = '0';
			if (!isdigit((int)t_min[0])) t_min[0] = '0';
			if (!isdigit((int)t_min[1])) t_min[1] = '0';
			if (!isdigit((int)t_sec[0])) t_sec[0] = '0';
			if (!isdigit((int)t_sec[1])) t_sec[1] = '0';
		}

		if(valid != 0)
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

		WL_set_defs(DEFAULTS_JS,mode);
		WL_set_defs(DEFAULTS_JC,job_class);
		WL_set_defs(DEFAULTS_JL,def_proc_cpu);

		WL_save_defaults();							/* Write out the personality info.	*/
	}

	wsb_delete(hWsb);

	return rc;
}

static int wsh_progprnt(int scrn_seq_no)						/* The screen seq. no.  Starts at 1.	*/
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

	if (wisp_nativescreens())
	{
		return 0;
	}

	wcurwidth = vwang_wcurwidth();
	work_area_1_size = (MAX_COLUMNS_PER_LINE * MAX_LINES_PER_SCREEN);		/* Compute the size of the char map.	*/
	work_area_1 = wisp_malloc(work_area_1_size);						/* Allocate space required.		*/

	if (!work_area_1)								/* Space allocated ?			*/
        {
		WL_werr_message_box("Error, wsh_progprnt() unable to obtain required memory.");
		return(0);								/* Scoot along down the line.		*/
	}
                                                            
	work_area_2_size = (wcurwidth + BORDERED_COLUMN_TOTAL) * (MAX_LINES_PER_SCREEN + BORDERED_LINE_TOTAL);
	work_area_2 = wisp_malloc(work_area_2_size);						/* Allocate required space.		*/

	if (!work_area_2)								/* Space allocated ?			*/
	{
		WL_werr_message_box("Error, wsh_progprnt() unable to obtain required memory.");
		return(0);								/* Scoot along down the line.		*/
	}

	if (scrn_seq_no == 0)								/* Get the current screen ?		*/
	{
		l_vchr_map = (char *)VL_vchr_map;						/* Yup.  Point directly to it.		*/
		l_vmap_top = VL_vmap_top;
	}
	else
	{			    							/* Nope.  Strip apart the stack.	*/
		if (VL_vscrn_stack)							/* Is there anything on the stack ?	*/
		{
			vscrn_struct_ptr = VL_vscrn_stack;					/* Point to the top of the stack.	*/
			scrn_seq_no--;							/* One less screen to pop off.		*/
			for (x = scrn_seq_no; x; x--)					/* Strip the stack.			*/
			{
				if (vscrn_struct_ptr)					/* Make sure we're not pointing off to  */
				{							/* never never land.	 		*/
 					vscrn_struct_ptr = vscrn_struct_ptr->prev_ptr;	/* Where's the previous stack entry ?	*/
				}
				else
				{							/* Somethin's wrong.			*/
		 			WL_werr_message_box("Error, wsh_progprnt(). Sequence count, stack element mismatch.");
					return(0);					/* Long gone daddy.			*/
				}
			}
			l_vchr_map = vscrn_struct_ptr->xchr_map;			/* Point to the copy of the char. map.	*/
			l_vmap_top = vscrn_struct_ptr->xmap_top;
		}
		else
		{
			WL_werr_message_box("ERROR, wsh_progprnt(). No program screen to print.");
			return(0);
		}
        }

/*	memcpy(work_area_1, l_vchr_map, work_area_1_size);	*/			/* Make a local copy of the map.	*/
	/*
	**	The character map (vchr_map) may be scrolled by video so we can't just copy it to the work area.
	**	Using the vmap_top and VL_vmlx() we calc where each row begins and load work area one line at a time.
	*/
	ptr = work_area_1;
	for(i=0; i<MAX_LINES_PER_SCREEN; i++)
	{
		memcpy(ptr,&l_vchr_map[VL_vmlx(l_vmap_top,i)*MAX_COLUMNS_PER_LINE],MAX_COLUMNS_PER_LINE);
		ptr += MAX_COLUMNS_PER_LINE;
	}

	WL_strip_facs(work_area_1, work_area_1_size,VIDEO_TYPE);		               /* Remove non-printing characters.	*/
	WL_border_screen(work_area_2,  work_area_1, wcurwidth, MAX_COLUMNS_PER_LINE, MAX_LINES_PER_SCREEN);			

	sprintf(filelibvol,"##%3.3s                 ",WL_wanguid3());

	retcd = 0;
	retcd = WL_di_write_file(work_area_2, work_area_2_size, (wcurwidth + BORDERED_COLUMN_TOTAL), filelibvol, filelibvol);
	if (retcd)									/* Some error when trying to print	*/
	{										/* the current screen.			*/
		WL_werrlog(WERRCODE(86002),retcd,0,0,0,0,0,0,0);
	}
	return(0);
}

#define MAX_PERSONALITY_FILENAME_LEN 77
static void wsh_usewrite(void)
{
	HWSB	hWsb;
	int	pfkey, currow, curcol;
	char 	dst[MAX_PERSONALITY_FILENAME_LEN+1];
	char 	buff[128];

	currow = 0;
	curcol = 0;
	hWsb = wsb_new();
											/* Init destination file name to spaces.*/
	wisp_defaults_path(buff);
	cstr2cobx(dst,buff,MAX_PERSONALITY_FILENAME_LEN);
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
			wsb_get_field(hWsb, 8, 3, buff, MAX_PERSONALITY_FILENAME_LEN);
			buff[MAX_PERSONALITY_FILENAME_LEN] = '\0';
			leftjust(buff,MAX_PERSONALITY_FILENAME_LEN);
			WL_cobx2cstr(dst, buff, strlen(buff));
			WL_load_defaults();
			WL_write_defaults_to_file(dst);

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

int WL_wsc_init(unsigned char *screen, int row, int col)						/* Initialize screen image for vwang().	*/
{
	memset(screen,' ',WSB_LENGTH);							/* Blank the screen.			*/
	screen[OA_ROW] = 1;								/* Set the order area, row # 1.		*/
	screen[OA_WCC] = (char)(POSITION_CURSOR|UNLOCK_KEYBOARD); /* 0240; */		/* Set the allowances.			*/
	screen[OA_CURSOR_ROW] = row;
	screen[OA_CURSOR_COL] = col;
	return(SUCCESS);
}

int WL_put_screen_text(unsigned char *screen, int row, int col, unsigned int fac, char *text)		/* Put text and fields into screen map.	*/
{
	register int i,j;								/* Working storage.			*/

	if ((col-2) < 0) WL_werr_message_box("wsh_dispatch - Invalid field position, FAC off left hand side of screen.");
	i = ((row-1) * WSB_COLS) + (col-1) + OA_LENGTH;					/* Location in screen map.		*/
	j = strlen(text);								/* Determine length of string.		*/
	screen[i-1] = fac;								/* Lay in the fac.			*/
	memcpy(&screen[i],text,j);							/* Lay in the data.			*/
	screen[i+j] = FAC_DEFAULT_TEXT;							/* Put an end of text FAC.		*/
	return(SUCCESS);								/* And we're all done.			*/
}                                              

int WL_put_screen_text_centered(unsigned char *screen, int row, unsigned int fac, char *text)		/* Put text and fields into screen map.	*/
{											/* Centered.				*/
	register int i,j;								/* Working storage.			*/

	j = strlen(text);								/* Determine length of string.		*/
	i = ((row-1) * WSB_COLS) + (39-(j/2)) + OA_LENGTH;				/* Location in screen map.		*/
	screen[i-1] = fac;								/* Lay in the fac.			*/
	memcpy(&screen[i],text,j);							/* Lay in the data.			*/
	screen[i+j] = FAC_DEFAULT_TEXT;							/* Put an end of text FAC.		*/
	return(SUCCESS);								/* And we're all done.			*/
}

int WL_get_screen_text(unsigned char *screen, int row, int col, char *text)			/* Retreive text from screen.		*/
{
	register int i,j;								/* Working storage.			*/

	i = ((row-1) * WSB_COLS) + (col-1) + OA_LENGTH;					/* Location in screen map.		*/
	j = strlen(text);								/* Determine length of string.		*/
	memcpy(text,&screen[i],j);							/* Get in the data.			*/
	return(SUCCESS);								/* Tout finis.				*/
}

static const char* sysadmin_cmd(void)
{
	static char cmd[80];
	static const char *rc = NULL;
       	static int first = 1;
	
	if (first)
	{
		first = 0;

		rc = WL_get_wisp_option_env("WISPSYSADMIN");
		if (!rc || '\0' == *rc || ' ' == *rc)
		{
			rc = NULL;
#ifdef AIX
			rc = "smit -C";
#endif
		}
		else
		{
			strcpy(cmd,rc);
			rc = cmd;
		}
	}
	return rc;
}

static void wsh_sysadmin(void)
{
	const char *cmd;

	if ((cmd = sysadmin_cmd()))
	{
		WL_wsystem_interactive(cmd);
	}
	else
	{
		WL_werr_message_box("Sorry, this feature is not available.");
	}
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
		WL_get_defs(DEFAULTS_RV,volname);	
		WL_get_defs(DEFAULTS_RL,libname);
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
		int4 compcode, returncode;

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

		if (g_prog_running) WL_newlevel();					/* Extra link-level			*/

		WL_set_va_count(6);							/* Set the arg count to 6		*/
		LINK2(	progname,(int4)8,
			linktype,(int4)1,
			libname,(int4)8,
			volname,(int4)6,
			&compcode,(int4)4,
			&returncode,(int4)4);						/* Do the LINK				*/

		WL_wswap(&compcode);							/* Un-swap the comp & return codes	*/
		WL_wswap(&returncode);

		if ( 8 == compcode && 20 == returncode )				/* If not found ...			*/
		{
			linktype[0] = 'S';						/* Try SYSTEM link			*/
			compcode = 0;							/* Initialize the comp & return codes	*/
			returncode = 0;

			WL_set_va_count(6);						/* Set the arg count to 6		*/
			LINK2(	progname,(int4)8,
				linktype,(int4)1,
				libname,(int4)8,
				volname,(int4)6,
				&compcode,(int4)4,
				&returncode,(int4)4);					/* Do the LINK				*/

			WL_wswap(&compcode);						/* Un-swap the comp & return codes	*/
			WL_wswap(&returncode);
		}

		WL_setlevel(g_savelevel);						/* Restore the link-level		*/

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
	char 	mode[2], job_class[2];
	int 	looping;
	char	l_file[9], l_lib[9], l_vol[7];
	char	jobname[9];

	strcpy(l_file,"        ");
	strcpy(l_lib, "        ");
	strcpy(l_vol, "      ");
	WL_get_defs(DEFAULTS_RL,l_lib);
	WL_get_defs(DEFAULTS_RV,l_vol);

	strcpy(jobname,"        ");
	memcpy(jobname,WL_wanguid3(),3);

	WL_get_defs(DEFAULTS_JS,mode);							/* Get the proc status mode data.	*/
	WL_get_defs(DEFAULTS_JC,job_class);						/* Get the proc class data.		*/
	mode[1] = 0;
	job_class[1] = 0;
	
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
	wsb_add_field(hWsb,11,44,FAC_DEFAULT_FIELD,job_class,1);
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
		wsb_get_field(hWsb,11,44,job_class,1);
		wsb_get_field(hWsb,12,44,mode,1);

		/* Blank out any previous error messages */
		memset(errmess,' ',sizeof(errmess));
		wsb_add_field(hWsb, 2,  2, FAC_PROT_BLANK, errmess, 79);
		wsb_add_field(hWsb, 3,  5, FAC_PROT_BLANK, errmess, 76);

		if (memcmp(l_file,"        ",SIZEOF_FILE) == 0)
		{
			errptr="FILE MUST BE SPECIFIED";
			wsb_add_field(hWsb, 2,  2, FAC_PROT_BLINK, errptr , strlen(errptr));
			currow = 7;
			curcol = 20;
		}
		else
		{
			int4	retcode;
			
			WL_set_va_count(8);
			SUBMIT(l_file, l_lib, l_vol, jobname, mode, "D", job_class, &retcode);

			WL_wswap(&retcode);

			if (0 == retcode)
			{
				looping = 0;  /* Stop looping */
			}
			else
			{
				sprintf(errmess,"RC = %ld %s", (long)retcode, WL_submit_err(retcode));

				/* If still looping then Write out error message */
				errptr="SUBMIT FAILED -";
				wsb_add_field(hWsb, 2,  2, FAC_PROT_BLINK, errptr,strlen(errptr));
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

	WL_get_defs(DEFAULTS_FLAGS,&dflags);

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
		col = 5;
		
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

	WL_get_defs(DEFAULTS_FLAGS,&dflags);

	/*
	**	Print screen and setting terminal attributes is not supported
	**	with native screens.
	*/
	if (wisp_nativescreens())
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
		col = 5;
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

#ifdef unix
	if (dflags & HELP_COMMANDS)
	{
		if (wang_style)
		{
			wsb_add_text(hWsb,  row, col, "(3) Enter COMMANDS");
		}
		else
		{
			wsb_add_menu_item(hWsb, row, col, 3, "Enter UNIX commands");
		}
	}
#endif
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
#ifdef unix
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
	wsb_add_text(hWsb, 2, 0, WISP_OWNER);

	sprintf(temp,"Username:  %s", WL_longuid());
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
	if (0==strcmp(WL_computername(NULL),"Windows"))
	{
		strcpy(temp, "Welcome to Windows");
	}
	else
	{
		sprintf(temp, "Welcome to Windows - %s", WL_computername(NULL));
	}
#endif
	if (!temp[0])
	{
		strcpy(temp,"System:    UNKNOWN");
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
		if (*(wisp_get_progname()))					/* We know the program name		*/
		{
			sprintf(temp, "Program:   %s",wisp_get_progname());
			wsb_add_text(hWsb, 7, 3, temp);

			if (*(wisp_get_screenname()))				/* We also know the screen name		*/
			{
				sprintf(temp, "Screen:    %s",wisp_get_screenname());
				wsb_add_text(hWsb, 8, 3, temp);
			}
		}
		else
		{
			if (wisp_get_runname()[0] != ' ')
			{
				sprintf(temp, "Program:   %8.8s",wisp_get_runname());
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

int WL_ishelpactive(void)
{
	return is_help_active;
}

int WL_sethelpactive(int flag)
{
	return(is_help_active = flag);
}

/*
**	ROUTINE:	WL_wsystem_interactive()
**
**	FUNCTION:	Issue an interactive "system()" and handle screen setup.
**
**	DESCRIPTION:	For native screens we clear the screen and shutdown the
**			screen handler.
**			For WISP screens we push the screen then shutdown vwang.
**
**	ARGUMENTS:	The shell command to run.
**
**	GLOBALS:	None
**
**	RETURN:		The "wstat" return code for the system() routine.
**
**	WARNINGS:	This is only meant to be called from HELP screen.
**
*/
int WL_wsystem_interactive(const char *cmd)
{
	int	rc;
	int	old_stderr = -1;

#ifdef WIN32
	if (WL_utils_in_windows())
	{
		extern int WL_wsystem_standalone(const char* cmd);
		
		return WL_wsystem_standalone(cmd);
	}
#endif

	if (g_prog_running) WL_newlevel();	/* Extra link-level			*/
	WL_newlevel();
	
	if (wisp_nativescreens())
	{
		HWSB hWsb;
		int	pfkey ;
		int 	currow = 0;
		int	curcol = 0;
		
		/*
		**	Clear the screen
		*/
		hWsb = wsb_new();
		wsb_display_and_read(hWsb, NULL, &pfkey, &currow, &curcol);
		wsb_delete(hWsb);
		
		WL_shutdown_cobol_screen_handler();
	}
	else
	{
		vwang_wpushscr();
		
		vwang_shut();

#ifdef unix
		vwang_stty_sane();
#endif
	}
	
#ifdef unix
	/*
	 * If stderr is redirected to a file then a spawned shell doesn't display prompts.
	 */
	if (0==isatty(fileno(stderr)) && 1==isatty(fileno(stdout)))
	{
		/*
		 * If stderr is not a tty but stdout is then dup stderr and reassign
		 * the stderr to stdout.
		 */
		old_stderr = dup(fileno(stderr));
		if (-1 != old_stderr)
		{
			dup2(fileno(stdout),fileno(stderr));
		}
	}
#endif

	rc = WL_wsystem(cmd);
	
#ifdef unix
	if (-1 != old_stderr)
	{
		dup2(old_stderr,fileno(stderr)); /* put stderr back */
	}
#endif

	if (wisp_nativescreens())
	{
		WL_start_cobol_screen_handler();
	}
	else
	{
		vwang_synch();
		vwang_set_reinitialize(TRUE);

		vwang_init_term();
		vwang_wpopscr();
	}

	WL_oldlevel();
	WL_ppunlink(WL_linklevel());
	if (g_prog_running) WL_oldlevel();	/* Restore the extra link-level		*/

	return rc;
}


/*
**	History:
**	$Log: wshelp.c,v $
**	Revision 1.126  2011/10/29 20:09:14  gsl
**	Fix ISO routine name warnins on WIN32
**	
**	Revision 1.125  2009/10/18 20:44:47  gsl
**	Copyright
**	
**	Revision 1.124  2007/08/08 18:54:50  gsl
**	TT#74 file names with embedded spaces.
**	use WL_cobx2cstr() to convert the cobol input field to a C string.
**	
**	Revision 1.123  2005/07/11 15:10:34  gsl
**	Moved to Suite 600
**	
**	Revision 1.122  2003/07/08 20:55:23  gsl
**	WISP 5000
**	
**	Revision 1.121  2003/07/01 19:29:54  gsl
**	For (1) RUN use RUNLIB/RUNVOL instead of PROGLIB/PROGVOL as PL/PV is
**	meaningless for the command processor
**	
**	Revision 1.120  2003/06/20 15:48:03  gsl
**	VL_ globals
**	
**	Revision 1.119  2003/06/20 15:37:45  gsl
**	VL_ globals
**	
**	Revision 1.118  2003/06/13 17:35:31  gsl
**	#define WISP_WEBSITE
**	
**	Revision 1.117  2003/04/02 15:01:05  gsl
**	WISPPS1 option
**	
**	Revision 1.116  2003/04/01 22:17:24  gsl
**	Add support for option WISPPS1 for setting PS1 on a shell out
**	
**	Revision 1.115  2003/03/28 20:15:57  gsl
**	Add EXTRACT2
**	
**	Revision 1.114  2003/02/17 22:07:17  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.113  2003/02/07 17:55:21  gsl
**	Rework the platform routines and add AIX HPUX SOLARIS 64-bit
**	
**	Revision 1.112  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.111  2003/02/04 18:29:12  gsl
**	fix -Wall warnings
**	
**	Revision 1.110  2003/02/04 16:30:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.109  2003/01/31 19:08:36  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.108  2003/01/20 17:01:08  gsl
**	rename varibables "class"
**	
**	Revision 1.107  2002/12/10 20:54:06  gsl
**	use WERRCODE()
**	
**	Revision 1.106  2002/12/10 17:09:12  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.105  2002/12/09 21:09:34  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.104  2002/12/04 20:52:15  gsl
**	Add to OPTIONS file
**	WPROC
**	WPROCDEBUG
**	ACPCONFIG
**	ACPMAP
**	WISP_SCRATCH_MODE/WISPSCRATCHMODE
**	WISP_DISPLAY_8BIT/DISPLAY8BIT/WISPDISPLAY8BIT
**	WISPSYSADMIN
**	
**	Revision 1.103  2002/11/08 18:19:50  gsl
**	Enlarge temp vars to prevent overflow
**	
**	Revision 1.102  2002/11/06 20:41:42  gsl
**	Change address to Suite 402
**	
**	Revision 1.101  2002/10/18 17:32:54  gsl
**	Identify if in a  COBOL runtime
**	
**	Revision 1.100  2002/10/16 20:34:52  gsl
**	configure with environments variables vs registry on win32
**	
**	Revision 1.99  2002/08/01 14:45:09  gsl
**	type warnings
**	
**	Revision 1.98  2002/07/30 19:12:39  gsl
**	SETRETCODE
**	
**	Revision 1.97  2002/07/29 21:13:25  gsl
**	setretcode -> SETRETCODE
**	
**	Revision 1.96  2002/07/24 18:27:47  gsl
**	globals
**	
**	Revision 1.95  2002/07/16 16:24:50  gsl
**	Globals
**	
**	Revision 1.94  2002/07/15 20:16:03  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.93  2002/07/15 17:52:52  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.92  2002/07/12 20:40:42  gsl
**	Global unique WL_ changes
**	
**	Revision 1.91  2002/07/12 19:10:21  gsl
**	Global unique WL_ changes
**	
**	Revision 1.90  2002/07/12 17:01:05  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.89  2002/07/11 20:29:19  gsl
**	Fix WL_ globals
**	
**	Revision 1.88  2002/07/11 15:21:44  gsl
**	Fix WL_ globals
**	
**	Revision 1.87  2002/07/10 21:05:37  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.86  2002/07/10 04:27:32  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.85  2002/07/09 04:13:51  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.84  2002/07/02 21:15:38  gsl
**	Rename wstrdup
**	
**	Revision 1.83  2002/07/01 04:02:44  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.82  2002/06/26 20:52:15  gsl
**	Fix phone number
**	
**	Revision 1.81  2002/06/25 15:21:55  gsl
**	Change to use wmalloc()
**	
**	Revision 1.80  2002/06/21 20:49:31  gsl
**	Rework the IS_xxx bit flags and the WFOPEN_mode flags
**	
**	Revision 1.79  2002/03/28 14:42:06  gsl
**	Use define for cpoyright year
**	
**	Revision 1.78  2002-03-26 16:40:03-05  gsl
**	(C) 2002
**	Remove the Library and Screen versions
**
**	Revision 1.77  2001-10-31 15:44:06-05  gsl
**	Change the (29) env screen to include the RETCODE file path, the temp defaults
**	path and the sharedmem control path
**
**	Revision 1.76  2001-09-07 16:00:34-04  gsl
**	Add support for FLAGS ERRLOG, SUBMIT and BATCHQ
**
**	Revision 1.75  2001-09-07 09:06:26-04  gsl
**	Remove VMS and MSDOS ifdefs
**
**	Revision 1.74  2001-09-05 14:41:24-04  gsl
**	Change copyright date to 2001
**
**	Revision 1.73  2000-03-16 10:26:15-05  gsl
**	2000
**
**	Revision 1.72  1999-09-13 15:56:53-04  gsl
**	update copyrights
**
**	Revision 1.71  1999-05-20 14:23:03-04  gsl
**	On unix, in wsystem_interactive() if stderr is not a tty and stdout is
**	then stderr gets reassigned to be stdout.
**	This is needed because acucobol +e option redirects stderr to a file
**	and when you spawn a shell the shell checks if stderr is a tty and if
**	not it doesn't display the prompts.
**	So, it doing an interactive WL_wsystem() we attempt to make stderr a tty.
**
**	Revision 1.70  1998-10-22 17:16:35-04  gsl
**	Clean up the print and batch queue manager program logic
**
**	Revision 1.69  1998-08-03 17:24:38-04  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks.
**
**	Revision 1.68  1998-07-09 16:21:51-04  gsl
**	initialize the no_mod arg to vwang()
**
**	Revision 1.67  1998-05-05 13:20:45-04  gsl
**	Add WL_utils_in_windows() support for WIN32
**
**	Revision 1.66  1998-04-22 15:51:53-04  gsl
**	fix warndings
**
**	Revision 1.65  1998-04-03 13:51:10-05  gsl
**	CLear the programa and screen name in WISPHELP.
**
**	Revision 1.64  1998-03-16 14:32:30-05  gsl
**	Fix for NT
**
**	Revision 1.63  1998-03-16 13:57:13-05  gsl
**	Finish Native Screen handling.
**	Add wsystem_interactive() for running the shell, and unique etc.
**	Add WISPSYSADMIN for the Manage SYSTEM selection
**
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
**	Add support for WL_pfkeys12()
**
**	Revision 1.50  1997-08-23 12:45:13-04  gsl
**	Add (8) Display Error Log  to the Command Processor
**
**	Revision 1.49  1997-08-21 15:15:02-04  gsl
**	Changed to use WL_submit_err() to get the error message from SUBMIT
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
