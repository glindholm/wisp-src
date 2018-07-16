			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include standard header files.							*/

#include <stdio.h>									/* Include standard I/O definitions.	*/
#include <time.h>									/* Include time definitions.		*/
#include <ctype.h>
#ifdef VMS
#include <stdlib.h>
#endif
#ifdef unix
#include <malloc.h>
#include <sys/utsname.h>
#include <signal.h>
static int wsh_prtque();
#endif
#ifdef MSDOS
#include <malloc.h>
#endif

#include <v/video.h>									/* Include video definitions.		*/
#include <v/vlocal.h>									/* Include video definitions.		*/
#include <v/vcap.h>									/* Include video key definitions.	*/
#include <v/vchinese.h>
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

#define ROUTINE		86000

/*						Local definitions.								*/

char 	*strchr();
char	*shell_var();

int noprogscrn = 0;									/* No program screen flag.		*/
static int	wang_style=1;
static int	g_prog_running;
static char	run_complete_message[80];

static int wsc_mem_err();
static int pack();
static int perr();
static int mload();
static int pfkey_value();
static int build_non_wang_base();

extern char wisp_progname[9];								/* Get access to current program name.	*/
extern char wisp_screen[33];								/* And current screen.			*/
extern int synch_required;
extern int wcurwidth;									/* Current Wang screen width.		*/

/*						Get menu information								*/

int wsh_help(prog_running) int prog_running;						/* Put up the shell screen.		*/
{
	char *ctime();									/* The time conversion routine.		*/
	time_t time_data;								/* Time data.				*/
	char old_sec, new_sec;								/* Remember the old second.		*/
	char scrn[1924];								/* Screen contents.			*/
	char function,lines,term[2],no_mod[2];						/* Misc data.				*/
	char vcheck(), c, temp[81];
	int i, j, k;									/* Working registers.			*/
	char allowed_pfkeys[81];							/* Allowed PF keys.			*/
	char id[32], tty_str[5];							/* Vars for extract.			*/
	int4 tty_num, argcnt;
	char	time_temp[24];
	int	row, col1, col2, time_row, time_col;
	int	pfval;
        int 	pfkey_map[33];								/* Map for non-wang style screen pfkeys	*/
	uint4 dflags;


	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

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
	wpload();									/* Load the personality information.	*/
	sethelpactive(TRUE);								/* Help is now active.			*/

	strcpy(id,longuid());								/* Get the user id.			*/
	wswap(&tty_num);
	argcnt=2;
	wvaset(&argcnt);
	EXTRACT("W#",&tty_num);								/* Get the tty number from TTMAP.	*/
	wswap(&tty_num);
	sprintf(tty_str,"%4d",tty_num);							/* Convert value to a string.		*/
	strcpy(run_complete_message,"  ");						/* Blank the run complete message	*/

	get_defs(DEFAULTS_FLAGS,&dflags);						/* Get the defaults flags		*/

	if ( !(dflags & HELP_ENABLED))							/* Help is disabled.			*/
	{
		vbell();								/* Ring the bell.			*/
		sethelpactive(FALSE);							/* Help no longer active.		*/
		return(FAILURE);							/* It failed.				*/
	}

	lines = 24;									/* Use full screen.			*/

	if (wang_style)
	{
		time_row = 3;
		time_col = 49;

		wsc_init(scrn,0,0);							/* Set the screen frame to blank.	*/
		pack(scrn, 0, 24, 0, "***  WISP Command Processor  ***");		/* Store the header.			*/
		pack(scrn, 1, 17, 0, "International Digital Scientific Incorporated");

		sprintf(temp,"Workstation %s Ready",tty_str);
		pack(scrn, 3,  7, 0, temp);

		sprintf(temp,"Hello %s", id);
		pack(scrn,  5, (40 - (int)strlen(temp)/2), 0, temp);			/* Write the ID centered		*/

		temp[0] = '\0';
#ifdef unix
		{
			struct utsname unix_name;

			if ( -1 != uname(&unix_name) )
			{
				sprintf(temp, "Welcome to %s - %s", unix_name.sysname, unix_name.nodename);
			}
		}
#endif	
		if (!temp[0])
		{
			strcpy(temp, "Welcome to the system");
		}
		pack(scrn, 6, (40 - (int)strlen(temp)/2), 0, temp);			/* Write the welcome message		*/
		pack(scrn, 8, 14, 0, "Press (HELP) at any screen to return to this menu.");

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
			pack(scrn, 11, (40 - (int)strlen(temp)/2), 0, temp);
		}
		pack(scrn, 13, 18, 0, "Use the function keys to select a command:");
	}
	else /* NON-Wang style HELP screen */
	{
		memset(pfkey_map,0,sizeof(pfkey_map));

		time_row = 4;
		time_col = 54;

		build_non_wang_base(scrn);

	} /* End of screen header generation */

	if (wang_style)
	{
		row = 15;
		col1 = 4;
		col2 = 46;
	}
	else
	{
		row = 11;
		col1 = 8;
		col2 = 48;
	}

	if (prog_running) 	
	{
		if (wang_style)
			pack(scrn, row+0, col1, 0, "(1) CONTINUE Processing");
		else
			mload(scrn,row+0, col1, 1, "Resume Processing",pfkey_map);
	}
	else 			
	{
		if (wang_style)
			pack(scrn, row+0, col1, 0, "(1) RUN program or procedure");
		else
			mload(scrn,row+0, col1, 1, "Run a program",pfkey_map);
	}
	strcat(allowed_pfkeys,"01");

	if (dflags & HELP_SET_FILES)
	{
		if (wang_style)
			pack(scrn, row+1, col1, 0, "(2) SET File Usage Constants");
		else
			mload(scrn, row+1, col1, 2, "Volume and Directory Defaults",pfkey_map);
		strcat(allowed_pfkeys,"02");
	}

	if (dflags & HELP_SET_PRINTER)
	{
		if (wang_style)
			pack(scrn, row+2, col1, 0, "(3) SET Print Mode Defaults");
		else
			mload(scrn, row+2, col1, 3, "Print parameter Defaults",pfkey_map);
		strcat(allowed_pfkeys,"03");
	}

	if (dflags & HELP_SET_PROC)
	{
		if (wang_style)
			pack(scrn, row+3,  col1, 0, "(4) SET Submit Procedure Defaults");
		else
			mload(scrn, row+3, col1, 4, "Submit parameter Defaults",pfkey_map);
		strcat(allowed_pfkeys,"04");
	}

	if (dflags & HELP_MANAGE_FILES_LIBS)
	{
#ifndef VMS
		if (wang_style)
			pack(scrn, row+5, col1, 0, "(5) Manage FILES/LIBRARIES");
		else
			mload(scrn, row+4, col1, 5, "Manage Files",pfkey_map);
		strcat(allowed_pfkeys, "05");
#endif
	}

	if (dflags & HELP_MANAGE_SYSTEM)
	{
#ifdef _AIX
		if (wang_style)
			pack(scrn, row+6, col1, 0, "(6) Manage SYSTEM");
		else
			mload(scrn, row+5, col1, 6, "SMIT",pfkey_map);
		strcat(allowed_pfkeys, "06");
#endif
	}

	if (dflags & HELP_QUEUE_MNGMNT)
	{
#ifdef VMS
		if (wang_style)
			pack(scrn, row+7,  col1, 0, "(7) Manage QUEUES");
		else
			mload(scrn, row+6, col1, 7, "Queue Management",pfkey_map);
		strcat(allowed_pfkeys,"07");
#endif
#ifdef unix
		if (opt_idsiprint)
		{
			if (wang_style)
				pack(scrn, row+7,  col1, 0, "(7) Manage PRINT QUEUE");
			else
				mload(scrn, row+6, col1, 7, "Print Queue",pfkey_map);
			strcat(allowed_pfkeys,"07");
		}
#endif
	}

	if ((dflags & HELP_EDIT) || (dflags & HELP_DISPRINT) || (dflags & HELP_CRID))
	{
		if (wang_style)
			pack(scrn, row+0, col2, 0, " (9) Use UTILITIES");
		else
			mload(scrn, row+0, col2, 9, "Utilities",pfkey_map);
		strcat(allowed_pfkeys,"09");
	}
	else if (dflags & HELP_DISPLAY)
	{
		if (wang_style)
			pack(scrn, row+0, col2, 0, " (9) Use DISPLAY");
		else
			mload(scrn, row+0, col2, 9, "DISPLAY utility",pfkey_map);
		strcat(allowed_pfkeys,"09");
	}

	if (dflags & HELP_TERMINAL)
	{
		if (wang_style)
			pack(scrn, row+1, col2, 0, "(10) Configure TERMINAL");
		else
			mload(scrn, row+1, col2, 10, "Terminal characteristics",pfkey_map);
		strcat(allowed_pfkeys,"10");
	}

	if (dflags & HELP_COMMANDS)
	{
		if (wang_style)
			pack(scrn, row+2, col2, 0, "(11) Enter COMMANDS");
		else
#ifdef VMS
			mload(scrn, row+2, col2, 11, "Enter DCL commands",pfkey_map);
#endif
#ifdef unix
			mload(scrn, row+2, col2, 11, "Enter UNIX commands",pfkey_map);
#endif
#ifdef MSDOS
			mload(scrn, row+2, col2, 11, "Enter MS-DOS commands",pfkey_map);
#endif

		strcat(allowed_pfkeys,"11");
	}

	if (dflags & HELP_USAGE_WRITE)
	{
		if (wang_style)
			pack(scrn, row+4, col2, 0, "(13) SAVE environment");
		else
			mload(scrn, row+3, col2, 13, "Write usage constants",pfkey_map);
		strcat(allowed_pfkeys,"13");
	}

	if (prog_running && (dflags & HELP_PRINT_SCREEN) && !noprogscrn)
	{
		if (wang_style)
			pack(scrn, row+5, col2, 0, "(14) PRINT PROGRAM screen");
		else
			mload(scrn, row+4, col2, 14, "Print program screen",pfkey_map);
		strcat(allowed_pfkeys,"14");
	}

	if (dflags & HELP_PRINT_SCREEN)
	{
		if (wang_style)
			pack(scrn, row+6, col2, 0, "(15) PRINT COMMAND screen");
		else
			mload(scrn, row+5, col2, 15, "Print command screen",pfkey_map);
		strcat(allowed_pfkeys,"15");
	}

	if (!prog_running)
	{
		if (wang_style)
			pack(scrn, row+7, col2, 0, "(16) EXIT program");
		else
			mload(scrn, row+6, col2, 16, "Exit HELP processor",pfkey_map);
		strcat(allowed_pfkeys,"16");
	}
	else if (dflags & HELP_CANCEL)
	{
		if (wang_style)
			pack(scrn, row+7, col2, 0, "(16) CANCEL Processing");
		else
			mload(scrn, row+6, col2, 16, "CANCEL Processing",pfkey_map);
		strcat(allowed_pfkeys,"16");
	}

	strcat(allowed_pfkeys,"32X");							/* Terminate the allowed PF keys.	*/

	if (!wang_style)
	{
		strcpy(allowed_pfkeys,"0032X");
	}

restart:										/* Return here unless exiting.		*/

	time_data = time(NULL);								/* Get the time.			*/
	memcpy(time_temp,ctime(&time_data),24);
	old_sec = time_temp[18];							/* Remember the last second.		*/

	pack(scrn, time_row, time_col, 24, time_temp);

	function = WRITE_ALL;								/* Use WRITE_ALL			*/
	vwang(&function,scrn,&lines,"00X",term,no_mod);					/* Call Wang emulation to fill screen.	*/

	if (!wang_style) 
	{
		/*
		**	If not wang_style help screen then we don't do the time update stuff as it interferes with
		**	the monkey-bar type menus we are using.
		*/
		goto skip_time_update;
	}

	vmove(0,0); 
	j = voptimize(TRACKING_ONLY);							/* Turn off optimization.		*/
	vset(CURSOR,OFF);								/* Turn the cursor off.			*/
	voptimize(j);									/* Restore the optimization.		*/

	while((c = vcheck()) == '\0')							/* Loop until they type something.	*/
	{
		time_data = time(NULL);							/* Get the time.			*/
		memcpy(time_temp,ctime(&time_data),24);
		new_sec = time_temp[18];

		pack(scrn, time_row, time_col, 24, time_temp);				/* Store it.				*/

		if (old_sec != new_sec)							/* Has the second changed.		*/
		{
			if ((new_sec == '0') || (new_sec == '5'))			/* Even 5 seconds?			*/
			{
				vbuffering(LOGICAL);					/* Logical buffering.			*/
				old_sec = new_sec;					/* Remember the last second.		*/
				j = voptimize(TRACKING_ONLY);				/* Turn off optimization.		*/
				vmove(time_row,time_col);				/* Move to the time location.		*/
				vprint("%24s",ctime(&time_data));			/* Output the time (shortcut).		*/
				vmove(0,0);		/* Why is this necessary? Is the optimizer making a mistake?		*/
				voptimize(j);						/* Restore the optimization.		*/
				vbuffering(AUTOMATIC);					/* Back to automatic buffering.		*/
			}
		}
		vwait(0,0,0,50);							/* Give up the CPU.			*/
	}
	vpushc(c);									/* Push the characte back.		*/
	vmove(0,0);									/* Home again.				*/

skip_time_update:

	time_data = time(NULL);								/* Get the time.			*/
	memcpy(time_temp,ctime(&time_data),24);
	pack(scrn, time_row, time_col, 24, time_temp);					/* Store it.				*/

	synch_required = FALSE;								/* Don't need to synchronize.		*/
	function = DISPLAY_AND_READ;							/* Use DISPLAY_AND_READ.		*/
	vwang(&function,scrn,&lines,allowed_pfkeys,term,no_mod);			/* Call Wang emulation to fill screen.	*/

	if (wang_style)
	{
		pfval = (term[0] - '0')*10 + (term[1] - '0');
	}
	else
	{
		if (term[0] == '0' && term[1] == '0')
		{
			pfval = pfkey_value(scrn,pfkey_map);
		}
		else if (term[0] == '3' && term[1] == '2')
		{
			pfval = 32;
		}
		else
		{
			pfval = 0;
		}
	}

	switch(pfval)
	{
	case 1:										/* Run program or return.		*/
		if (prog_running)							/* Is a program running?		*/
		{
			sethelpactive(FALSE);						/* Help is no longer running.		*/
			return(SUCCESS);						/* Return to the caller.		*/
		}
		else 
		{
			wsh_run_program("        ");					/* No, then run a program.		*/
		}
		break;
	case 2:
		wsh_voldir();								/* Set the usage constants.		*/
		break;
	case 3:
		wsh_setprt();								/* Set the usage constants.		*/
		break;
	case 4:
		wsh_setproc();								/* Set the usage constants.		*/
		break;
	case 5:
#ifndef VMS
		mngfile();								/* Manage files				*/
#endif
		break;
	case 6:
		wsh_devices();								/* Go manage devices.			*/
		break;
	case 7:
#ifdef VMS
		wsh_quemngmnt();						 	/* Manage the queues.			*/
#endif
#ifdef unix
		wsh_prtque();								/* Do the IDSI print que management	*/
#endif
		break;
	case 9:
		wsh_utils();		 						/* Utilities.				*/
		break;
	case 10:
		wsh_terminal();								/* Do you want help terminal menu?	*/
		break;
	case 11:
#ifdef VMS
		wsh_dcl();								/* Issue DCL commands.			*/
#else
		wsh_shell();								/* Issue shell commands.		*/
#endif
		break;
	case 13:
		wsh_usewrite();		 						/* Write usage constants.		*/
		break;
	case 14:
		wsh_progprnt(1);		 					/* Print the program screen.		*/
		break;
	case 15:
		wsh_progprnt(0);		 					/* Print the COMMAND screen.		*/
		break;
	case 16:
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
				sethelpactive(FALSE);					/* Help not active.			*/
				return(FAILURE);					/* Aborted by user.			*/
			}
		}
		break;
	case 32:
		wsh_copyright();		 					/* Display the copyright screen.	*/
		break;
	}

	if ( wang_style && !prog_running )
	{
		/*
		**	Update the screen with the run complete message.
		*/
		strcpy(temp,run_complete_message);
		memset(run_complete_message,' ',strlen(temp));				/* Blank out the run complete message	*/
		pack(scrn, 11, (40 - (int)strlen(temp)/2), 0, temp);
	}

	goto restart;									/* And dispatch the next request.	*/
}

int wsh_terminal() 									/* Get a terminal menu selection.	*/
{
	int i, j, k;									/* Working registers.			*/
	char scrn[1924];
	char function,lines,term[2],no_mod[2];						/* Misc data.				*/
	char okkeys[81];
	int	pfval;
	int	pfkey_map[33];
	uint4 dflags;

	lines = 24;									/* Use full screen.			*/
	if (wang_style)
	{
		wsc_init(scrn,0,0);							/* Set the screen frame to blank.	*/

		pack(scrn,  0, 24,  0, "***  Configure Terminal ***");			/* Store the header.			*/
		pack(scrn, 22,  8,  0, "Press (HELP) to return to the command processor, (1) to exit.");
		strcpy(okkeys,"01");							/* No keys ok yet.			*/
	}
	else
	{
		memset(pfkey_map,0,sizeof(pfkey_map));
		build_non_wang_base(scrn);
	}

	if (!wang_style)
	{
		mload(scrn, 11, 8, 1, "Return to MAIN HELP menu",pfkey_map);
	}

	get_defs(DEFAULTS_FLAGS,&dflags);						/* Get the defaults flags		*/

	if (dflags & HELP_SETPSB)
	{
		if (wang_style)
			pack(scrn,  8, 20, 0, "(2) PSEUDO blank characteristics.");
		else
			mload(scrn,  12, 8, 2, "PSEUDO blank characteristics",pfkey_map);
		strcat(okkeys,"02");
	}

	if (dflags & HELP_SETCURCHAR)
	{
		if (wang_style)
			pack(scrn, 10, 20, 0, "(3) CURSOR characteristics.");
		else
			mload(scrn, 13, 8, 3, "CURSOR characteristics",pfkey_map);
		strcat(okkeys,"03");
	}

	if (dflags & HELP_SCREEN)
	{
		if (wang_style)
			pack(scrn, 12, 20, 0, "(4) SCREEN characteristics.");
		else
			mload(scrn, 14, 8, 4, "SCREEN characteristics.",pfkey_map);
		strcat(okkeys,"04");
	}

	strcat(okkeys,"X");

	if (!wang_style)
		strcpy(okkeys,"00X");

	function = DISPLAY_AND_READ;							/* Display and read.			*/
again:	vwang(&function,scrn,&lines,okkeys,term,no_mod);				/* Call Wang emulation to fill screen.	*/

	if (wang_style)
	{
		pfval = (term[0] - '0')*10 + (term[1] - '0');
	}
	else
	{
		if (term[0] == '0' && term[1] == '0')
		{
			pfval = pfkey_value(scrn,pfkey_map);
		}
		else
		{
			pfval = 0;
		}
	}

	if      (pfval == 2) wsh_setpsb();						/* Set the pseudo blank characteristics.*/
	else if (pfval == 3) wsh_setcurchar();						/* Set the cursor characteristics.	*/
	else if (pfval == 4) wsh_setbackground();					/* Set the cursor background.		*/
	else return(FAILURE);
	goto again;
}

/*
	get_psb_char:	Get pseudo blank character.
			This routine is passed a selection character (def_select) and returns the pseudo blank char (pb_char)
			and the real selection char (pb_select).
			It knowns how to handle converison from old PERSONALITY files that stored the pseudo blank char
			instead of the selection char.
*/
get_psb_char(def_select,pb_char,pb_select)
char	def_select;									/* The selection character "B1234"	*/
char	*pb_char;									/* The Pseudo blank char (returned)	*/
char	*pb_select;									/* The selection char  (returned)	*/
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

int wsh_setpsb()									/* Change pseudo blank characteristics.	*/
{
	char *screen;									/* Pointer to working screen routine.	*/
	char function, lines, term[2], no_mod[2];					/* Working variables.			*/
	char pb_char[4], pb_rend[4], pb_select[4];	
	char dpsb[4];									/* Temp var for display of graphics.	*/
	int pb_chset, pb_r;
	int valid, i, changed;
	char	def_psb_select;
	int4	def_psb_charset;
	int4	def_psb_rendition;

	wpload();									/* Load the personality information.	*/
	if ((screen = malloc(1924)) == 0) wsc_mem_err();				/* Able to get memory?			*/
	wsc_init(screen,0,0);								/* Initialize the screen layout.	*/

	wput(screen, 1,15,PLAIN_TEXT,"*** Change the pseudo blank characteristics ***");/* Layout the screen.			*/
	wput(screen, 5, 2,PLAIN_TEXT,"Pseudo blank character      :");
	wput(screen, 6, 2,PLAIN_TEXT,"Using pseudo blank rendition:");
	wput(screen,10,10,PLAIN_TEXT,"Character Options");
	wput(screen,10,40,PLAIN_TEXT,"Rendition Options");
	wput(screen,12,10,PLAIN_TEXT,"B    blank ");					/* Print a blank.			*/
	wput(screen,13,10,PLAIN_TEXT,"1");
	wput(screen,14,10,PLAIN_TEXT,"2");
	wput(screen,15,10,PLAIN_TEXT,"3");
	wput(screen,16,10,PLAIN_TEXT,"4");
	wput(screen,12,40,PLAIN_TEXT,"N  -  NORMAL");
	wput(screen,13,40,PLAIN_TEXT,"U  -  UNDERSCORE");
	wput(screen,14,40,PLAIN_TEXT,"R  -  REVERSE");
	wput(screen,15,40,PLAIN_TEXT,"B  -  Both UNDERSCORE and REVERSE");
	wput(screen,18,2,PLAIN_TEXT,"The pseudo blank character will");
	wput(screen,19,2,PLAIN_TEXT,"fill the modifiable fields with");
	wput(screen,20,2,PLAIN_TEXT,"the indicated character.");
	wput(screen,18,37,PLAIN_TEXT,"The indicated rendition will be turned");
	wput(screen,19,37,PLAIN_TEXT,"on for modifiable fields and display");
	wput(screen,20,37,PLAIN_TEXT,"with the pseudo blank character.");
	wput(screen,24,2,PLAIN_TEXT,"Change the information as appropriate and depress (RETURN), (1) to exit.");

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

	wput(screen,5,40,UPCASE_FIELD,pb_select);					/* Print correct char set from choices.	*/
	wput(screen, 6,40,UPCASE_FIELD,pb_rend);					/* Print correct rendition from choices.*/

	function = WRITE_ALL;								/* Use display full screen function.	*/
	lines = 24;
	vwang(&function,screen,&lines,"0001X",term,no_mod);				/* Call Wang emulation to fill screen.	*/
	vcharset(GRAPHICS);

	vmove(12,15);
	dpsb[0] = PBLANK1; dpsb[1] = '\0';
	vprint(dpsb);									/* Print a diamond.			*/
	vmove(13,15);
	dpsb[0] = PBLANK2; dpsb[1] = '\0';
	vprint(dpsb);									/* Print a degree symbol.		*/
	vmove(14,15);
	dpsb[0] = PBLANK3; dpsb[1] = '\0';
	vprint(dpsb);									/* Print a centered period.		*/
	vmove(15,15);
	dpsb[0] = PBLANK4; dpsb[1] = '\0';
	vprint(dpsb);									/* Print a square box.			*/
	vmove(4,33);
	vprint(pb_char);								/* Print pseudo blank character.	*/
	vcharset(DEFAULT);
	synch_required = FALSE;								/* Vprint sets the flag to TRUE which I	*/
											/* don't want so reset to FALSE.	*/
	function = READ_ALTERED;							/* Use read altered function.		*/
	lines = 24;
	
	valid = TRUE;									/* Assume it starts with valid values	*/
	changed = FALSE;								/* Nothing has changed yet		*/

	for(;;)										/* Repeat until data input is valid.	*/
	{
		vwang(&function,screen,&lines,"0001X",term,no_mod);			/* Call Wang emulation to fill screen.	*/
		function = DISPLAY_AND_READ_ALTERED;					/* Set up for second time thru loop	*/

		if ((term[0] == '0') && (term[1] == '1'))				/* If PF01 press then abort		*/
		{
			changed = FALSE;
			break;
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
				wput(screen, 5,40,UPCASE_FIELD|BLINK_FAC,pb_select);	/* Make it blink.			*/
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
				wput(screen, 6,40,UPCASE_FIELD|BLINK_FAC,pb_rend);	/* Make it blink.			*/
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
	free(screen);									/* Release the screen memory.		*/
	synch_required = TRUE;								/* Synch now required.			*/
	return(SUCCESS);								/* All done.				*/
}

int wsh_setcurchar()									/* Change cursor characteristics.	*/
{
	char *screen;									/* Pointer to working screen routine.	*/
	char function, lines, term[2], no_mod[2];					/* Working variables.			*/
	int4 automove, autotab, mp_cursor;
	char amfl[2], atfl[2],mpfl[2];
	int valid, i, changed;

	wpload();									/* Load the personality information.	*/
	if ((screen = malloc(1924)) == 0) wsc_mem_err();				/* Able to get memory?			*/
	wsc_init(screen,0,0);								/* Initialize the screen layout.	*/

	wput(screen, 1,15,PLAIN_TEXT,"*** Change the cursor characteristics ***");	/* Layout the screen.			*/
	wput(screen, 5, 5,PLAIN_TEXT,"        Auto tab flag = ");
	wput(screen, 7, 5,PLAIN_TEXT,"       Auto move flag = ");
	wput(screen, 9, 5,PLAIN_TEXT,"Menu pick cursor flag = ");
	wput(screen,24,2,PLAIN_TEXT,"Change the information as appropriate and depress (RETURN), (1) to exit.");

	get_defs(DEFAULTS_AUTOTAB,&autotab);						/* Initialize fields from personality.	*/
	get_defs(DEFAULTS_AUTOMOVE,&automove);
	get_defs(DEFAULTS_MP_CURSOR,&mp_cursor);

	if (autotab) strcpy(atfl,"Y");							/* Init the string value.		*/
	else	      strcpy(atfl,"N");

	if (automove) strcpy(amfl,"Y");			
	else	      strcpy(amfl,"N");

	if (mp_cursor) strcpy(mpfl,"Y");
	else	      strcpy(mpfl,"N");

	wput(screen, 5,30,UPCASE_FIELD,atfl);
	wput(screen, 7,30,UPCASE_FIELD,amfl);
	wput(screen, 9,30,UPCASE_FIELD,mpfl);

	function = DISPLAY_AND_READ_ALTERED;						/* Use display and read altered.	*/
	lines = 24;

	valid = TRUE;									/* Assume it starts with valid values	*/
	changed = FALSE;								/* Nothing has changed yet		*/

	for(;;)										/* Repeat until data input is valid.	*/
	{
		vwang(&function,screen,&lines,"0001X",term,no_mod);			/* Call Wang emulation to fill screen.	*/

		if ((term[0] == '0') && (term[1] == '1'))				/* If PF01 press then abort		*/
		{
			changed = FALSE;
			break;
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
			int pos;

			valid = TRUE;							/* Assume valid values			*/

			wget(screen, 5,30,atfl);
			wget(screen, 7,30,amfl);
			wget(screen, 9,30,mpfl);

			if ((pos = strpos("YNTF10",atfl)) < 0)				/* Was a valid logical given?		*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wput(screen, 5,30,UPCASE_FIELD|BLINK_FAC,atfl);		/* Make it blink.			*/
				screen[2] = 0;						/* Reset cursor position.		*/
				screen[3] = 0;
			}
			else
			{
				if (pos == 0 || pos == 2 || pos == 4) autotab = TRUE;	/* Set auto_tab_flag to TRUE.		*/
				else autotab = FALSE;
			}
			if ((pos = strpos("YNTF10",amfl)) < 0)				/* Was a valid logical given?		*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wput(screen, 7,30,UPCASE_FIELD|BLINK_FAC,amfl);		/* Make it blink.			*/
				screen[2] = 0;						/* Reset cursor position.		*/
				screen[3] = 0;
			}
			else
			{
				if (pos == 0 || pos == 2 || pos == 4) automove = TRUE;	/* Set auto_move_flag to TRUE.		*/
				else automove = FALSE;
			}  
			if ((pos = strpos("YNTF10",mpfl)) < 0)				/* Was a valid logical given?		*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wput(screen, 9,30,UPCASE_FIELD|BLINK_FAC,mpfl);		/* Make it blink.			*/
				screen[2] = 0;						/* Reset cursor position.		*/
				screen[3] = 0;
			}
			else
			{
				if (pos == 0 || pos == 2 || pos == 4) mp_cursor = TRUE;	/* Set mp_cursor to TRUE.		*/
				else mp_cursor = FALSE;
			}  
		}
                                                                                                 
		if (valid) break;
	}

	if (changed && valid)								/* If values changed and are valid	*/
	{
		set_defs(DEFAULTS_AUTOTAB,&autotab);
		set_defs(DEFAULTS_AUTOMOVE,&automove);
		set_defs(DEFAULTS_MP_CURSOR,&mp_cursor);
		save_defaults();							/* Write out the personality info.	*/
	}
	free(screen);									/* Release the screen memory.		*/
	return(SUCCESS);								/* All done.				*/
}

int wsh_setbackground()									/* Change cursor characteristics.	*/
{
	char *screen;									/* Pointer to working screen routine.	*/
	char function, lines, term[2], no_mod[2];					/* Working variables.			*/
	int4 bgchange, bgcolor, excolor;
	char bgch[2], bgco[2], exco[2];
	int valid, i, changed;

	wpload();									/* Load the personality information.	*/
	if ((screen = malloc(1924)) == 0) wsc_mem_err();				/* Able to get memory?			*/
	wsc_init(screen,0,0);								/* Initialize the screen layout.	*/

	wput(screen, 1,15,PLAIN_TEXT,"*** Screen Background Selection ***");		/* Layout the screen.			*/
	wput(screen, 5, 5,PLAIN_TEXT,"    Change background =    (No means leave the background as it is.)");
	wput(screen, 7, 5,PLAIN_TEXT,"          Select grey =    (No means select a black background.)");
	wput(screen, 9, 5,PLAIN_TEXT,"            Exit grey =    (No means select black on exit.)");
	wput(screen,24,2,PLAIN_TEXT,"Change the information as appropriate and depress (RETURN), (1) to exit.");

	get_defs(DEFAULTS_BGCHANGE,&bgchange);						/* Initialize fields from personality.	*/
	get_defs(DEFAULTS_BGCOLOR,&bgcolor);
	get_defs(DEFAULTS_EXCOLOR,&excolor);

	if (bgchange) strcpy(bgch,"Y");							/* Init the string value.		*/
	else	      strcpy(bgch,"N");

	if (bgcolor) strcpy(bgco,"Y");			
	else	     strcpy(bgco,"N");

	if (excolor) strcpy(exco,"Y");
	else         strcpy(exco,"N");

	wput(screen, 5,30,UPCASE_FIELD,bgch);
	wput(screen, 7,30,UPCASE_FIELD,bgco);
	wput(screen, 9,30,UPCASE_FIELD,exco);

	function = DISPLAY_AND_READ_ALTERED;						/* Use display and read altered.	*/
	lines = 24;

	valid = TRUE;									/* Assume it starts with valid values	*/
	changed = FALSE;								/* Nothing has changed yet		*/

	for(;;)										/* Repeat until data input is valid.	*/
	{
		vwang(&function,screen,&lines,"0001X",term,no_mod);			/* Call Wang emulation to fill screen.	*/

		if ((term[0] == '0') && (term[1] == '1'))				/* If PF01 press then abort		*/
		{
			changed = FALSE;
			break;
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
			int pos;

			valid = TRUE;							/* Assume we're valid.			*/

			wget(screen, 5,30,bgch);
			wget(screen, 7,30,bgco);
			wget(screen, 9,30,exco);

			if ((pos = strpos("YNTF10",bgch)) < 0)				/* Was a valid logical given?		*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wput(screen, 5,30,UPCASE_FIELD|BLINK_FAC,bgch);		/* Make it blink.			*/
				screen[2] = 0;						/* Reset cursor position.		*/
				screen[3] = 0;
			}
			else
			{
				if (pos == 0 || pos == 2 || pos == 4) bgchange = TRUE;	/* Set change flag to TRUE.		*/
				else bgchange = FALSE;
			}
			if ((pos = strpos("YNTF10",bgco)) < 0)				/* Was a valid logical given?		*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wput(screen, 7,30,UPCASE_FIELD|BLINK_FAC,bgco);		/* Make it blink.			*/
				screen[2] = 0;						/* Reset cursor position.		*/
				screen[3] = 0;
			}
			else
			{
				if (pos == 0 || pos == 2 || pos == 4) bgcolor = TRUE;	/* Set color to grey.			*/
				else bgcolor  = FALSE;
			}  
			if ((pos = strpos("YNTF10",exco)) < 0)				/* Was a valid logical given?		*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wput(screen, 7,30,UPCASE_FIELD|BLINK_FAC,bgco);		/* Make it blink.			*/
				screen[2] = 0;						/* Reset cursor position.		*/
				screen[3] = 0;
			}
			else
			{
				if (pos == 0 || pos == 2 || pos == 4) excolor = TRUE;	/* Set color to grey.			*/
				else excolor  = FALSE;
			}  
		}                                                                                                                 
		if (valid) break;
	}

	if (changed && valid)								/* If values changed and are valid	*/
	{
		set_defs(DEFAULTS_BGCHANGE,&bgchange);
		set_defs(DEFAULTS_BGCOLOR,&bgcolor);
		set_defs(DEFAULTS_EXCOLOR,&excolor);
		save_defaults();							/* Write out the personality info.	*/
		if (bgchange)
		{
			if (excolor) vonexit(NORMALIZE|CLEAR_SCREEN|LIGHT);
			else         vonexit(NORMALIZE|CLEAR_SCREEN|DARK);
		}
	}
	free(screen);									/* Release the screen memory.		*/
	return(SUCCESS);								/* All done.				*/
}

int wsh_voldir() 									/* Set the usage constants.		*/
{
	char *screen;									/* Pointer to working screen routine.	*/
	char function, lines, term[2], no_mod[2];					/* Working variables.			*/
	char	def_inlib[9], 	def_invol[7];
	char	def_outlib[9], 	def_outvol[7];
	char	def_runlib[9], 	def_runvol[7];
	char	def_spoolib[9], def_spoolvol[7];
	char	def_worklib[9], def_workvol[7];

	wpload();									/* Load the personality information.	*/
	if ((screen = malloc(1924)) == 0) wsc_mem_err();				/* Able to get memory?			*/

	wsc_init(screen,0,0);								/* Initialize the screen layout.	*/
	wput(screen, 1,20,PLAIN_TEXT,"*** Set Volume and Directory Defaults ***");	/* Layout the screen.			*/
	wput(screen, 5, 2,PLAIN_TEXT,"Default location (disk volume and directory) of files, assuming that these");
	wput(screen, 6, 2,PLAIN_TEXT,"values are not provided by the program itself or logically reassigned:");

	wput(screen, 9, 8,PLAIN_TEXT,"Input files:            INLIB   =               INVOL    =");
	wput(screen,11, 8,PLAIN_TEXT,"Output files:           OUTLIB  =               OUTVOL   =");
	wput(screen,13, 8,PLAIN_TEXT,"User programs:          RUNLIB  =               RUNVOL   =");
	wput(screen,15, 8,PLAIN_TEXT,"Spooled print files:    SPOOLLIB=               SPOOLVOL =");
	wput(screen,19, 8,PLAIN_TEXT,"Work/Temporary files:   WORKLIB =               WORKVOL  =");

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

	wput(screen, 9,43,UPCASE_FIELD,def_inlib  );  wput(screen, 9,68,UPCASE_FIELD,def_invol   );
	wput(screen,11,43,UPCASE_FIELD,def_outlib );  wput(screen,11,68,UPCASE_FIELD,def_outvol  );
	wput(screen,13,43,UPCASE_FIELD,def_runlib );  wput(screen,13,68,UPCASE_FIELD,def_runvol  );
	wput(screen,15,43,UPCASE_FIELD,def_spoolib);  wput(screen,15,68,UPCASE_FIELD,def_spoolvol);
	wput(screen,19,43,PLAIN_TEXT,  def_worklib);  wput(screen,19,68,UPCASE_FIELD,def_workvol );

	wput(screen,24,2,PLAIN_TEXT,"Change the information as appropriate and depress (RETURN), (1) to exit.");

	function = DISPLAY_AND_READ_ALTERED;						/* Use display and read altered.	*/
	lines = 24;
	vwang(&function,screen,&lines,"0001X",term,no_mod);				/* Call Wang emulation to fill screen.	*/
                                                       
	if ((no_mod[0] == 'M') && !((term[0] == '0') && (term[1] == '1')))		/* Don't write unless they press enter	*/
	{										/* and they've changed something.	*/
		wget(screen, 9,43,def_inlib  );  		wget(screen, 9,68,def_invol   );
		wget(screen,11,43,def_outlib );  		wget(screen,11,68,def_outvol  );
		wget(screen,13,43,def_runlib );  		wget(screen,13,68,def_runvol  );
		wget(screen,15,43,def_spoolib);  		wget(screen,15,68,def_spoolvol);
		                                 		wget(screen,19,68,def_workvol );

		set_defs(DEFAULTS_IL,def_inlib);		set_defs(DEFAULTS_IV,def_invol);
		set_defs(DEFAULTS_OL,def_outlib);		set_defs(DEFAULTS_OV,def_outvol);
		set_defs(DEFAULTS_RL,def_runlib);		set_defs(DEFAULTS_RV,def_runvol);
		set_defs(DEFAULTS_SL,def_spoolib);		set_defs(DEFAULTS_SV,def_spoolvol);
								set_defs(DEFAULTS_WV,def_workvol);

		save_defaults();							/* Write out the personality info.	*/
	}

	free(screen);									/* Release the screen memory.		*/
	return(SUCCESS);								/* All done.				*/
}

int wsh_copyright()									/* Display the copyright screen.	*/
{
	char *screen;									/* Pointer to working screen routine.	*/
	char function, lines, term[2], no_mod[2];					/* Working variables.			*/
	char buff[80];

	if ((screen = malloc(1924)) == 0) wsc_mem_err();				/* Able to get memory?			*/

	wsc_init(screen,0,0);								/* Initialize the screen layout.	*/
	wpcen(screen, 1,PLAIN_TEXT,"*** Copyright Information ***");			/* Layout the screen.			*/
	wpcen(screen, 8,PLAIN_TEXT,"The WISP runtime library");
	wpcen(screen, 9,PLAIN_TEXT,"Copyright 1988,89,90,91,92,93 International Digital Scientific Inc.");
	wpcen(screen, 10,PLAIN_TEXT,"28460 Avenue Stanford, Suite 100, Valencia CA 91355  (805) 295-1155");

	if (outctx)
	  sprintf(buff,"Version=%s  Library=%d  Screen=%d  ",WISP_VERSION, LIBRARY_VERSION, SCREEN_VERSION);
	else
	  sprintf(buff,"Version=[%s] Library=[%d] Screen=[%d]",WISP_VERSION, LIBRARY_VERSION, SCREEN_VERSION);
	wpcen(screen, 22,PLAIN_TEXT,buff);

#include "license.h"

#ifdef LICENSEE
	wpcen(screen, 12,PLAIN_TEXT,"Licensed to");
	wpcen(screen, 14,PLAIN_TEXT,LICENSEE);
#endif

#ifdef LICENSEE_PHONE
	wpcen(screen, 15,PLAIN_TEXT,LICENSEE_PHONE);
#endif

#ifdef unix
	{
		struct utsname unix_name;
		char	temp[80];

		if ( -1 != uname(&unix_name) )
		{
			sprintf(temp, "%s %s %s %s %s", unix_name.sysname, 
							unix_name.nodename,
							unix_name.release,
							unix_name.version,
							unix_name.machine);
			wpcen(screen, 20, PLAIN_TEXT, temp);
		}
	}
#endif

	function = DISPLAY_AND_READ_ALTERED;						/* Use display and read altered.	*/
	lines = 24;

	vwang(&function,screen,&lines,"0001020304050607080910111213141516X",term,no_mod);/* Call Wang emulation to fill screen.*/
                                                       
	free(screen);									/* Release the screen memory.		*/
	return(SUCCESS);								/* All done.				*/
}


#ifdef VMS
int wsh_dcl()										/* Issue DCL commands.			*/
{
	int	savelevel;
	uint4	vms_status;

	savelevel = linklevel();							/* Save the link-level			*/
	newlevel();									/* Increment the link-level twice to	*/
	newlevel();									/* prevent side effects of ppunlink().	*/
	spawn2 (0,"","Type LOGOFF followed by (RETURN) to continue.",&vms_status);	/* Now Spawn a sub process.		*/
	ppunlink(linklevel());								/* Putparm UNLINK			*/
	setlevel(savelevel);								/* Restore the link-level		*/
	return(SUCCESS);								/* All done.				*/
}
#endif	/* VMS */

#ifdef unix
static int wsh_prtque()
{
	int	pid, rc;
	char	*sh_ptr;
	char 	*prtque_cmd, *getenv();
	int 	st;
	
	sh_ptr = shell_var();

	prtque_cmd = getenv("UNIQUE_MAN");
	if (prtque_cmd==NULL)
	{
		if (opt_pqunique)
		{
			prtque_cmd="unique";
		}
		else
		{
			prtque_cmd="ilpman";
		}
	}

	wpushscr();
	vexit();
	signal(SIGCLD,  SIG_DFL);							/* Use Default DEATH-OF-CHILD signal	*/

	switch(pid=fork())
	{
		case 0:
		{	
			st = execlp(prtque_cmd,prtque_cmd,"-q","-w",(char *)0);
			exit(9);
			break;
		}
		default:
		{
			wwaitpid(pid,&rc);
			break;
		}
	}

	signal(SIGCLD,  SIG_IGN);						/* Ignore DEATH-OF-CHILD signal			*/
	vstate(DEFAULT);							/* Initialize the terminal			*/
	wpopscr();

	if (rc)
	{
		switch(rc)
		{
		      case 1:
			vre_window("IDSI Print Queue Daemon not running.                            Contact system administrator");
			break;
		      case 9:
			vre_window("Cannot find the printq queue management program \"%s\".         Contact system administrator",prtque_cmd);
			break;
		      default:
			vre_window("Error code %d when trying to run \"%s\".                        Contact system administrator",rc,prtque_cmd);
			break;
		}
	}

	return(SUCCESS);								/* All done.				*/
}

int wsh_shell()										/* Issue unix shell commands.		*/
{
	int	pid, rc;
	char	*sh_ptr;
	int	savelevel;

	sh_ptr = shell_var();

	savelevel = linklevel();							/* Save the link-level			*/
	newlevel();									/* Extra link-level to prevent side-	*/
											/* effects of ppunlink().		*/
	wpushscr();
	vexit();
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
	vstate(DEFAULT);							/* Initialize the terminal			*/
	wpopscr();

	ppunlink(linklevel());								/* Putparm UNLINK			*/
	setlevel(savelevel);								/* Restore the link-level		*/
	return(SUCCESS);								/* All done.				*/
}
#endif	/* unix */

#ifdef MSDOS
int wsh_shell()										/* Issue unix shell commands.		*/
{
	int	pid, rc;
	char	*sh_ptr;
	int	savelevel;

	savelevel = linklevel();							/* Save the link-level			*/

	newlevel();									/* Extra link-level to prevent side-	*/
											/* effects of ppunlink().		*/
	newlevel();									/* Increment the link-level for shell	*/

	wpushscr();
	vexit();

	system("COMMAND");

	vstate(DEFAULT);							/* Initialize the terminal			*/
	wpopscr();

	ppunlink(linklevel());								/* Putparm UNLINK			*/
	setlevel(savelevel);								/* Restore the link-level		*/
	return(SUCCESS);								/* All done.				*/
}
#endif	/* MSDOS */

int wsh_setprt()									/* Set the usage constants.		*/
{
	char *screen;									/* Pointer to working screen routine.	*/
	char function, lines, term[2], no_mod[2];
	char mode[2], class[2], number[4], form[4], dplines[4];				/* Working versions of field data.	*/
	int mfac;									/* FAC for the mode string.		*/
	int valid;									/* Validation successful flag.		*/
	int tlines;									/* Integer test for lines per page.	*/
	int4	def_prt_num;
	int4	def_prt_form;
	int4	def_prt_lines;

	wpload();									/* Get personality if needed.		*/
	if ((screen = malloc(1924)) == 0) wsc_mem_err();				/* Able to get memory?			*/
	wsc_init(screen,0,0);								/* Initialize the screen layout.	*/
	wput(screen,1,20,PLAIN_TEXT,"*** Set Print Mode Defaults ***");		/* Layout the screen.			*/

	wput(screen, 5, 2,PLAIN_TEXT,"Print mode defaults for spooled, queued and online printing:  PRNTMODE =");
	wput(screen, 7, 6,PLAIN_TEXT,    "S - Spool printer files. Print then delete.");
	wput(screen, 8, 6,PLAIN_TEXT,    "P - Print and keep. Spool printer files, do not delete.");
	wput(screen, 9, 6,PLAIN_TEXT,    "H - Hold printer files. Spool for hold.");
	wput(screen,10, 6,PLAIN_TEXT,    "K - Keep printer files, do not print.");
#ifdef VMS
	wput(screen,11, 6,PLAIN_TEXT,    "O - Online printing directly to device LPn:");
#endif
	wput(screen,13, 2,PLAIN_TEXT,"Default spooled print file print class.      ('A' thru 'Z'):  PRTCLASS =");
	wput(screen,15, 2,PLAIN_TEXT,"Default form number.                         (000 thru 254):  FORM_NUM =");
#ifdef VMS
	wput(screen,17, 2,PLAIN_TEXT,"Default print device number.   (0 for LP0:, 1 for LP1 etc.):  PRINTER  =");
#else
	wput(screen,17, 2,PLAIN_TEXT,"Default printer number:                                       PRINTER  =");
#endif
	wput(screen,19, 2,PLAIN_TEXT,"Default Lines per Page                       (000 thru 255):  LINES    =");

	wput(screen,24,2,PLAIN_TEXT,"Change the information as appropriate and depress (RETURN), (1) to exit.");

	function = DISPLAY_AND_READ_ALTERED;						/* Use display and read altered.	*/
	lines = 24;

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

	wput(screen, 5,75,UPCASE_FIELD,mode);						/* Display the fields.			*/
	wput(screen,13,75,UPCASE_FIELD,class);
	wput(screen,15,75,NUMERIC_FIELD,form);
	wput(screen,17,75,NUMERIC_FIELD,number);
	wput(screen,19,75,NUMERIC_FIELD,dplines);

	do										/* Repeat until data input is valid.	*/
	{
		vwang(&function,screen,&lines,"0001X",term,no_mod);			/* Call Wang emulation to fill screen.	*/

		wget(screen, 5,75,mode);						/* Display the fields.			*/
		wget(screen,13,75,class);
		wget(screen,15,75,form);
		wget(screen,17,75,number);
		wget(screen,19,75,dplines);

		valid = TRUE;								/* Assume we're valid.			*/
		if ((no_mod[0] == 'M') && !((term[0] == '0') && (term[1] == '1')))	/* Should we validate?			*/
		{
			mode[0] = toupper(mode[0]);					/* Make sure the char is uppercase.	*/
			if (strpos("OSHKP",mode) < 0)					/* Was a valid print class given?	*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wput(screen, 5,75,UPCASE_FIELD|BLINK_FAC,mode);		/* Make it blink.			*/
				screen[2] = 0;						/* Reset cursor position.		*/
				screen[3] = 0;
			}
			class[0] = toupper(class[0]);					/* Make print class upper case.		*/
						/***** ADD VALIDATION FOR PRINT CLASS HERE ******/
			tlines = atoi(dplines);						/* Convert lines to integer.		*/
			if (tlines < 0 || tlines > 255)
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wput(screen,19,75,NUMERIC_FIELD|BLINK_FAC,dplines);	/* Make it blink.			*/
				screen[2] = 0;						/* Reset cursor position.		*/
				screen[3] = 0;
			}
		}                                                                                                                 

	} while(!valid && (no_mod[0] == 'M') && !((term[0] == '0') && (term[1] == '1')));/* Repeat until the data is valid.	*/

	if ((no_mod[0] == 'M') && !((term[0] == '0') && (term[1] == '1')))		/* Ccpy to data base?			*/
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

	free(screen);									/* Release the screen memory.		*/
	return(SUCCESS);								/* All fini...				*/
}


int wsh_setproc()									/* Set the usage constants.		*/
{
	char *screen;									/* Pointer to working screen routine.	*/
	char function, lines, term[2], no_mod[2];
	char mode[2], class[2], t_hr[3], t_min[3], t_sec[3];				/* Working versions of field data.	*/
	int mfac;									/* FAC for the mode string.		*/
	int valid;									/* Validation successful flag.		*/
	char	def_proc_cpu[7];

	wpload();									/* Get personality if needed.		*/
	if ((screen = malloc(1924)) == 0) wsc_mem_err();				/* Able to get memory?			*/
	wsc_init(screen,0,0);								/* Initialize the screen layout.	*/
	wput(screen,1,20,PLAIN_TEXT,"*** Set Submit Procedure Defaults ***");		/* Layout the screen.			*/

	wput(screen, 5, 2,PLAIN_TEXT,"Current defaults for procedure submittal under program control:");
	wput(screen, 8, 6,PLAIN_TEXT,"Default submittal status:");
	wput(screen, 9,12,PLAIN_TEXT,"R - Run,  Proc eligible for scheduling");
	wput(screen,10,12,PLAIN_TEXT,"H - Hold, Do not run proc until released");
	wput(screen,11,22,PLAIN_TEXT,"by submitter or operator");
	wput(screen,11,58,PLAIN_TEXT,"JOBQUEUE =");
	wput(screen,13, 6,PLAIN_TEXT,"Default proc scheduling class  (\"A\" thru \"Z\"):");
	wput(screen,13,58,PLAIN_TEXT,"JOBCLASS =");
	wput(screen,15, 6,PLAIN_TEXT,"Default proc execution time limit:");
	wput(screen,16,12,PLAIN_TEXT,"Central processor time in format HH:MM:SS");
	wput(screen,17,12,PLAIN_TEXT,"- No value (or zero) indicates \"No limit\"");
	wput(screen,17,58,PLAIN_TEXT,"JOBLIMIT =");
	wput(screen,17,72,PLAIN_TEXT,":");
	wput(screen,17,77,PLAIN_TEXT,":");

	wput(screen,24,2,PLAIN_TEXT,"Change the information as appropriate and depress (RETURN), (1) to exit.");

	function = DISPLAY_AND_READ_ALTERED;						/* Use display and read altered.	*/
	lines = 24;

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

	wput(screen,11,69,UPCASE_FIELD,mode);						/* Display the fields.			*/
	wput(screen,13,69,UPCASE_FIELD,class);

	wput(screen,17,69,NUMERIC_FIELD,t_hr);
	wput(screen,17,74,NUMERIC_FIELD,t_min);
	wput(screen,17,79,NUMERIC_FIELD,t_sec);

	do										/* Repeat until data input is valid.	*/
	{
		vwang(&function,screen,&lines,"0001X",term,no_mod);			/* Call Wang emulation to fill screen.	*/

		wget(screen,11,69,mode);						/* Display the fields.			*/
		wget(screen,13,69,class);
		wget(screen,17,69,t_hr);
		wget(screen,17,74,t_min);
		wget(screen,17,79,t_sec);

		valid = TRUE;								/* Assume we're valid.			*/
		if ((no_mod[0] == 'M') && !((term[0] == '0') && (term[1] == '1')))	/* Should we validate?			*/
		{
			mode[0] = toupper(mode[0]);					/* Make sure the char is uppercase.	*/
			if (strpos("RH",mode) < 0)					/* Was a valid print class given?	*/
			{
				valid = FALSE;						/* Oops, then not valid.		*/
				wput(screen,11,69,UPCASE_FIELD|BLINK_FAC,mode);		/* Make it blink.			*/
				screen[2] = 0;						/* Reset cursor position.		*/
				screen[3] = 0;
			}
			class[0] = toupper(class[0]);					/* Make print class upper case.		*/

			if (!isdigit(t_hr[0])) t_hr[0] = '0';
			if (!isdigit(t_hr[1])) t_hr[1] = '0';
			if (!isdigit(t_min[0])) t_min[0] = '0';
			if (!isdigit(t_min[1])) t_min[1] = '0';
			if (!isdigit(t_sec[0])) t_sec[0] = '0';
			if (!isdigit(t_sec[1])) t_sec[1] = '0';
		}

	} while(!valid && (no_mod[0] == 'M') && !((term[0] == '0') && (term[1] == '1')));/* Repeat until the data is valid.	*/

	if ((no_mod[0] == 'M') && !((term[0] == '0') && (term[1] == '1')))		/* Copy to data base?			*/
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

	free(screen);									/* Release the screen memory.		*/
	return(SUCCESS);								/* All fini...				*/
}
                                                                                                                                  

wsh_progprnt(scrn_seq_no)
int scrn_seq_no;									/* The screen seq. no.  Starts at 1.	*/
{                                                                                                                                 
	extern char vchr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference to an external array.	*/
	extern int  vmap_top;

	extern struct save_screen *vscrn_stack;						/* Reference to external video struct.	*/
              
	char *l_vchr_map;								/* Pointer to Video character map.	*/
	int  l_vmap_top;
	char *work_area_1, *work_area_2;						/* A work area.				*/
	char *ptr;
	char filelibvol[23];
	int retcd;
	struct save_screen *vscrn_struct_ptr;						/* Local pointer to a copy of the stack	*/
	int i,x, work_area_1_size, work_area_2_size;					/* Working variables.			*/

	work_area_1_size = (MAX_COLUMNS_PER_LINE * MAX_LINES_PER_SCREEN);		/* Compute the size of the char map.	*/
	work_area_1 = malloc(work_area_1_size);						/* Allocate space required.		*/

	if (!work_area_1)								/* Space allocated ?			*/
        {
		werrvre("Error, wsh_progprnt() unable to obtain required memory.");	/* Nope.  Let them know then...		*/
		return(0);								/* Scoot along down the line.		*/
	}
                                                            
	work_area_2_size = (wcurwidth + BORDERED_COLUMN_TOTAL) * (MAX_LINES_PER_SCREEN + BORDERED_LINE_TOTAL);
	work_area_2 = malloc(work_area_2_size);						/* Allocate required space.		*/

	if (!work_area_2)								/* Space allocated ?			*/
	{
		werrvre("Error, wsh_progprnt() unable to obtain required memory.");	/* Nope.  Let them know then...		*/
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
		 			werrvre("Error, wsh_progprnt(). Sequence count, stack element mismatch.");
					return(0);					/* Long gone daddy.			*/
				}
			}
			l_vchr_map = vscrn_struct_ptr->xchr_map;			/* Point to the copy of the char. map.	*/
			l_vmap_top = vscrn_struct_ptr->xmap_top;
		}
		else
		{
			werrvre("ERROR, wsh_progprnt(). No program screen to print.");
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

	strip_facs(work_area_1, work_area_1_size,1);			               /* Remove non-printing characters.	*/
	border_screen(work_area_2,  work_area_1, wcurwidth, MAX_COLUMNS_PER_LINE, MAX_LINES_PER_SCREEN);			

	sprintf(filelibvol,"##%3.3s                 ",wanguid3());

	retcd = 0;
	retcd = di_write_file(work_area_2, work_area_2_size, (wcurwidth + BORDERED_COLUMN_TOTAL), filelibvol, filelibvol);
	if (retcd)									/* Some error when trying to print	*/
	{										/* the current screen.			*/
		werrlog(ERRORCODE(2),retcd,0,0,0,0,0,0,0);
	}
}

wsh_progdisp()
{
	int	level;

	level = linklevel();								/* save the link-level			*/
	setprogid("DISPLAY");
	newlevel();									/* Increment link-level an extra time	*/
											/* to prevent side-effects of ppunlink.	*/
	wfile_disp();									/* display				*/
	setlevel(level);								/* restore the link-level		*/
	return(0);
}

#ifdef VMS
wsh_quemngmnt()
{
	setup_qm();									/* Setup routine for queue management	*/
	return(0);									/* and then calls quemngmnt.		*/
}
#endif

wsh_usewrite()
{
	char *screen;									/* Pointer to working screen routine.	*/
	char function, lines, term[2], no_mod[2];					/* Working variables.			*/
	char dst[80], answer[4];
	char *ptr; 
	int  i;

	wpload();									/* Load the personality information.	*/
	if ((screen = malloc(1924)) == 0) wsc_mem_err();				/* Able to get memory?			*/
	wsc_init(screen,0,0);								/* Initialize the screen layout.	*/

	wpcen(screen, 1,PLAIN_TEXT,"*** Write Usage Constants ***");			/* Layout the screen.			*/
	wput(screen, 4, 3,PLAIN_TEXT,"Are you sure you want to write the usage constants? ");
	wput(screen, 6, 3,PLAIN_TEXT,"NOTE:  You must enter the complete word YES to write usage constants!.");
	wput(screen,24,2,PLAIN_TEXT,"Change the information as appropriate and depress (RETURN), (1) to exit.");

	function = DISPLAY_AND_READ_ALTERED;						/* Use display and read altered.	*/
	lines = 24;

	strcpy(answer,"NO ");								/* Init to No for write.		*/
	wput(screen, 4,56,UPCASE_FIELD,answer);

	vwang(&function,screen,&lines,"0001X",term,no_mod);				/* Call Wang emulation to fill screen.	*/
	wget(screen, 4,56,answer);							/* Get response to check question.	*/

	if ((no_mod[0] == 'M') && !((term[0] == '0') && (term[1] == '1')))		/* If modifications and not PF1.	*/
	{
		answer[3] = '\0';							/* Null terminate the string.		*/
		if ((i = strcmp("YES",answer)) == 0)					/* Do you really want to write it?	*/
		{
			wsc_init(screen,0,0);
											/* Init destination file name to spaces.*/
			strcpy(dst,"                                                                               ");
			wpcen(screen, 1,PLAIN_TEXT,"*** Write Usage Constants ***");	/* Layout the screen.			*/
			wput(screen, 6, 3,PLAIN_TEXT,"Enter filename to write:");	/* Yes.  So ask for a file name.	*/
			wput(screen, 8,03,STANDARD_FIELD,dst);
			wput(screen,11, 5,PLAIN_TEXT,"Enter a file name to write the usage constants to, or");
			wput(screen,12, 5,PLAIN_TEXT,"press (RETURN) to use the personality default file.");
			wput(screen,24,2,PLAIN_TEXT,"Change the information as appropriate and depress (RETURN), (1) to exit.");

			function = DISPLAY_AND_READ_ALTERED;				/* Use display and read altered.	*/
			lines = 24;

			vwang(&function,screen,&lines,"0001X",term,no_mod);		/* Call Wang emulation to fill screen.	*/
			wget(screen, 8,03,dst);

			if ((no_mod[0] == 'M') && !((term[0] == '0') && (term[1] == '1'))) /* If modifications and not PF1.	*/
			{
				leftjust(dst,sizeof(dst));				/* left justify & null terminate the 	*/
				if (ptr = strchr(dst,' ')) *ptr=0;			/* personality file name.		*/

				vmove(21,10);
				vprint ("WRITING USAGE CONSTANTS using %s",dst);	/* File name specified so display.	*/
				load_defaults();					/* Be sure they exist.			*/
				write_defaults_to_file(dst);				/* Store the file.			*/
			}
			else if ((no_mod[0] != 'M') && !((term[0] == '0') && (term[1] == '1'))) /* If no mods and not PF1.	*/
			{
				dst[0] = '\0';						/* Set file name to null.		*/
				vmove(21,20);
				vprint ("WRITING USAGE CONSTANTS");
				load_defaults();					/* Be sure they exist.			*/
				write_defaults_to_file(dst);				/* Store the file.			*/
			}
		}
	}
	free(screen);									/* Release the screen memory.		*/
	return(SUCCESS);								/* All done.				*/
}

int wsc_init(screen,row,col) char *screen; int row,col;					/* Initialize screen image for vwang().	*/
{
	screen[0] = 1;									/* Set the order area, row # 1.		*/
	screen[1] = 0240;								/* Set the allowances.			*/
	screen[2] = row;
	screen[3] = col;
	memset(&screen[4],' ',1920);							/* Blank the screen.			*/
	return(SUCCESS);
}

int wput(screen,row,col,fac,text) char *screen; int row, col, fac; char *text;		/* Put text and fields into screen map.	*/
{
	register int i,j;								/* Working storage.			*/

	if ((col-2) < 0) werrvre("wsh_dispatch - Invalid field position, FAC off left hand side of screen.");
	i = ((row-1) * 80) + (col-1) + 4;						/* Location in screen map.		*/
	j = strlen(text);								/* Determine length of string.		*/
	screen[i-1] = fac;								/* Lay in the fac.			*/
	memcpy(&screen[i],text,j);							/* Lay in the data.			*/
	screen[i+j] = PLAIN_TEXT;							/* Put an end of text FAC.		*/
	return(SUCCESS);								/* And we're all done.			*/
}                                              

int wpcen(screen,row,fac,text) char *screen; int row, fac; char *text;			/* Put text and fields into screen map.	*/
{											/* Centered.				*/
	register int i,j;								/* Working storage.			*/

	j = strlen(text);								/* Determine length of string.		*/
	i = ((row-1) * 80) + (39-(j/2)) + 4;						/* Location in screen map.		*/
	screen[i-1] = fac;								/* Lay in the fac.			*/
	memcpy(&screen[i],text,j);							/* Lay in the data.			*/
	screen[i+j] = PLAIN_TEXT;							/* Put an end of text FAC.		*/
	return(SUCCESS);								/* And we're all done.			*/
}

int wget(screen,row,col,text) char *screen; int row, col; char *text;			/* Retreive text from screen.		*/
{
	register int i,j;								/* Working storage.			*/

	i = ((row-1) * 80) + (col-1) + 4;						/* Location in screen map.		*/
	j = strlen(text);								/* Determine length of string.		*/
	memcpy(text,&screen[i],j);							/* Get in the data.			*/
	return(SUCCESS);								/* Tout finis.				*/
}

static int wsc_mem_err()
{
	werrvre("Run-time error, unable to allocate memory in wsh_setuc function");	/* Oops, cannot get memory.		*/
	wexit(1L);									/* Unconditionally fatal.		*/
}                                         

static int pack(s, r, c, n, t) char *s; int r, c, n; char *t;				/* Put text into a screen.		*/
{
	register int i;

	if (n) for(i = 0; i < n; i++) s[4+(r*80)+c++] = *t++;				/* Copy n characters.			*/
	else while(*t != '\0') s[4+(r*80)+c++] = *t++;					/* Copy null terminated.		*/
}

static int pfac(s, r, c, n, f) char *s; int r, c, n, f;
{
	s[4-1+(r*80)+c] = f;								/* Store the FAC			*/
	s[4-1+(r*80)+c+n+1] = PLAIN_TEXT;						/* Store end of field.			*/
}

static int gfld(s, r, c, n, f) char *s; int r, c, n; char *f;				/* Return a field null terminated.	*/
{
	register int i;									/* Working registers.			*/

	for (i = 0; i < n; i++) *f++ = s[4+(r*80)+c++];					/* Return the character.		*/
	*f++ = '\0';									/* Null terminate.			*/
}

int wsh_devices()
{
#ifdef _AIX
	wpushscr();
	vexit();

	wsystem("smit");

	vstate(DEFAULT);							/* Initialize the terminal			*/
	wpopscr();
#else
	werrvre("Sorry, this feature is not available on the system on which you are running.");
	synch_required = FALSE;
#endif
}

int wsh_run_program(program) char *program;						/* Run a program.			*/
{
	char scrn[1924];								/* Screen contents.			*/
	char function,lines,term[2],no_mod[2];						/* Misc data.				*/
	int i, j, k;									/* Working registers.			*/
	int prog_given;									/* Name given flag.			*/
	char progname[9], linktype[1], libname[9], volname[7];				/* LINK parameters			*/
	int	savelevel;

	wpload();
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

	lines = 24;									/* Use full screen.			*/
	wsc_init(scrn,0,0);								/* Set the screen frame to blank.	*/
	pack(scrn,  0, 29, 0, "***  Run Program  ***");					/* Store the header.			*/
	pack(scrn, 22, 15, 0, "Press (HELP) to return to the command processor, (1) to exit.");

	pack(scrn,  5,  6, 0, "Supply the file name of the");
	pack(scrn,  6,  6, 0, "program to be executed and");
	pack(scrn,  7,  6, 0, "press (ENTER)");
	pack(scrn, 10,  6, 0, "Options");
	pack(scrn, 12,  8, 0, "Designate a user library which");
	pack(scrn, 13,  8, 0, "includes programs to be used");
	pack(scrn, 14,  8, 0, "during this run:");
	pack(scrn,  7, 55, 0, "PROGRAM  = ");
	pfac(scrn,  7, 66, 8, UPCASE_FIELD);
	pack(scrn,  7, 66, 8, program);
	pack(scrn, 13, 55, 0, "LIBRARY  = ");
	pfac(scrn, 13, 66, 8, UPCASE_FIELD);
	pack(scrn, 13, 66, 8, libname);
	pack(scrn, 14, 55, 0, "VOLUME   = ");
	pfac(scrn, 14, 66, 6, UPCASE_FIELD);
	pack(scrn, 14, 66, 6, volname);

again:	function = DISPLAY_AND_READ;							/* Display and read.			*/
	if (prog_given)									/* Program name was given.		*/
	{
		term[0] = '0';								/* Fake an enter.			*/
		term[1] = '0';
	}
	else vwang(&function,scrn,&lines,"0001X",term,no_mod);				/* No, so get one from the terminal.	*/

	if ((term[0] == '0') && (term[1] == '1'))					/* Return				*/
	{
		return(0);
	}

	if ((term[0] == '0') && (term[1] == '0'))					/* Run program or return.		*/
	{
		int4 templong, compcode, returncode;

		gfld(scrn,  7, 66, 8, progname);
		gfld(scrn, 13, 66, 8, libname);
		gfld(scrn, 14, 66, 6, volname);

		if (memcmp(progname,"        ",8) == 0)					/* Check for a program name		*/
		{
			perr(scrn, "ERROR - Program name must be given.");
			pfac(scrn,  7, 66, 8, STANDARD_FIELD | BLINK_FAC);
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

		perr(scrn,"PROCESSING - Program in progress.");				/* Tell what is going on.		*/
		function = WRITE_ALL;							/* Use WRITE_ALL			*/
		if (!prog_given) vwang(&function,scrn,&lines,"00X",term,no_mod);	/* Call Wang emulation to fill screen.	*/

		savelevel = linklevel();						/* Save the link-level			*/
		newlevel();								/* Extra newlevel to prevent side-effect*/
											/* of the ppunlink().			*/

		compcode = 0;								/* Initialize the comp & return codes	*/
		returncode = 0;
		templong = 6;
		wvaset(&templong);							/* Set the arg count to 6		*/

		LINK2(progname,8,linktype,1,libname,8,volname,6,&compcode,4,&returncode,4);	/* Do the LINK		*/

		wswap(&compcode);							/* Un-swap the comp & return codes	*/
		wswap(&returncode);

		if ( 8 == compcode && 20 == returncode )				/* If not found ...			*/
		{
			linktype[0] = 'S';						/* Try SYSTEM link			*/
			compcode = 0;							/* Initialize the comp & return codes	*/
			returncode = 0;
			templong = 6;
			wvaset(&templong);						/* Set the arg count to 6		*/

			LINK2(progname,8,linktype,1,libname,8,volname,6,&compcode,4,&returncode,4);	/* Do the LINK	*/

			wswap(&compcode);						/* Un-swap the comp & return codes	*/
			wswap(&returncode);
		}
		setlevel(savelevel);							/* Restore link-level			*/

		switch(compcode)
		{
		case 0:
			sprintf(run_complete_message,"Program %8.8s Processing Completed",progname);
			return(0);
			break;
		case 8:
			switch(returncode)
			{
			case 4:
				perr(scrn,"FAILED - Volume was not found.");
				break;
			case 16:
			case 20:
				perr(scrn,"FAILED - Program was not found.");
				break;
			case 28:
				perr(scrn,"FAILED - Protection violation.");
				break;
			case 60:
				perr(scrn,"FAILED - Insufficient memory.");
				break;
			default:
				perr(scrn,"FAILED - Unable to execute program.");
				break;
			}
			break;
		case 16:
		default:
			perr(scrn,"Program ABORTED or was CANCELLED");
			break;
		}

		pfac(scrn,  7, 66, 8, STANDARD_FIELD);
		prog_given = FALSE;							/* Force screen to be drawn		*/
		goto again;
	}
	return(0);
}

int wsh_utils() 									/* Select utilities.			*/
{
	int i, j, k;									/* Working registers.			*/
	int 	pfval;
	char scrn[1924];
	char function,lines,term[2],no_mod[2];						/* Misc data.				*/
	int	pfkey_map[33];
	char	pflist[40];
	uint4 dflags;

	get_defs(DEFAULTS_FLAGS,&dflags);

	if (	 (dflags & HELP_DISPLAY) &&
		!(dflags & HELP_EDIT) &&
		!(dflags & HELP_DISPRINT) &&
		!(dflags & HELP_CRID)		)
	{										/* just do a DISPLAY			*/
		wsh_progdisp();
		return(0);
	}



	lines = 24;									/* Use full screen.			*/
	if (wang_style)
	{
		wsc_init(scrn,0,0);							/* Set the screen frame to blank.	*/

		pack(scrn,  0, 30,  0, "***  Utilities ***");				/* Store the header.			*/
		pack(scrn, 22, 15,  0, "Press (HELP) to return to the command processor, (1) to exit.");
		pack(scrn,  3, 14, 0, "GENERAL");
	}
	else
	{
		memset(pfkey_map,0,sizeof(pfkey_map));
		build_non_wang_base(scrn);
	}

	if (!wang_style)
	{
		mload(scrn, 11, 8, 1, "Return to MAIN HELP menu",pfkey_map);
	}

	if (dflags & HELP_DISPLAY) 	
	{
		if (wang_style)
			pack(scrn,  5, 14, 0, "(2) DISPLAY a file");
		else
			mload(scrn, 12, 8, 2, "DISPLAY a file",pfkey_map);
	}

	if (dflags & HELP_EDIT) 	
	{
		if (wang_style)
			pack(scrn,  7, 14, 0, "(3) EDIT a file");
		else
			mload(scrn, 13, 8, 3, "EDIT a file",pfkey_map);
	}

	if (dflags & HELP_DISPRINT) 	
	{
		if (wang_style)
			pack(scrn,  9, 14, 0, "(4) DISPRINT");
		else
			mload(scrn, 14, 8, 4, "DISPRINT",pfkey_map);
	}

	if (dflags & HELP_CRID)
	{
		if (wang_style)
		{
			pack(scrn, 11, 14, 0, "(5) REPORT");
			pack(scrn, 13, 14, 0, "(6) INQUIRY");
			pack(scrn, 15, 14, 0, "(7) CONTROL");
			pack(scrn, 17, 14, 0, "(8) DATENTRY");
		}
		else
		{
			mload(scrn, 15, 8, 5, "REPORT",pfkey_map);
			mload(scrn, 16, 8, 6, "INQUIRY",pfkey_map);
			mload(scrn, 17, 8, 7, "CONTROL",pfkey_map);
			mload(scrn, 18, 8, 8, "DATENTRY",pfkey_map);
		}
	}

again:	function = DISPLAY_AND_READ;							/* Display and read.			*/
	perr(scrn,"");									/* Tell what is going on.		*/

	if (wang_style)
		strcpy(pflist,"000102030405060708X");
	else
		strcpy(pflist,"00X");

	vwang(&function,scrn,&lines,pflist,term,no_mod);				/* Call Wang emulation to fill screen.	*/

	if (wang_style)
	{
		pfval = (term[0] - '0')*10 + (term[1] - '0');
	}
	else
	{
		if (term[0] == '0' && term[1] == '0')
		{
			pfval = pfkey_value(scrn,pfkey_map);
		}
		else
		{
			pfval = 0;
		}
	}

	if (pfval == 1)									/* Return on pf01			*/
	{
		return(0);
	}
	else if (pfval == 2 && (dflags & HELP_DISPLAY))					/* DISPLAY?				*/
	{
		wsh_progdisp();
	}
	else if (pfval == 3 && (dflags & HELP_EDIT))					/* EDIT?				*/
	{
		perr(scrn,"PROCESSING - Program in progress.");				/* Tell what is going on.		*/
		function = WRITE_ALL;
		vwang(&function,scrn,&lines,"X",term,no_mod);				/* Call Wang emulation to fill screen.	*/
		wsh_run_program("VSEDIT  ");
	}
	else if (pfval == 4 && (dflags & HELP_DISPRINT))				/* DISPRINT?				*/
	{
		perr(scrn,"PROCESSING - Program in progress.");				/* Tell what is going on.		*/
		function = WRITE_ALL;
		vwang(&function,scrn,&lines,"X",term,no_mod);				/* Call Wang emulation to fill screen.	*/
		wsh_run_program("DISPRINT");
	}
	else if (pfval >= 5 && pfval <= 8 && (dflags & HELP_CRID))			/* CRID?				*/
	{
		perr(scrn,"PROCESSING - Program in progress.");				/* Tell what is going on.		*/
		function = WRITE_ALL;
		vwang(&function,scrn,&lines,"X",term,no_mod);				/* Call Wang emulation to fill screen.	*/
		if      (pfval == 5) wsh_run_program("REPORT  ");
		else if (pfval == 6) wsh_run_program("INQUIRY ");
		else if (pfval == 7) wsh_run_program("CONTROL ");
		else if (pfval == 8) wsh_run_program("DATENTRY");
	}

	else return(FALSE);
	goto again;
}

static int perr(scrn, text) char *scrn, *text;
{
	pack(scrn, 19, 15, 0, "                                                                      ");
	pfac(scrn, 19, 15, 55, PLAIN_TEXT);
	if (*text != '\0')
	{
		pack(scrn, 19, 15, 0, text);
		pfac(scrn, 19, 15, 55, BOLD_TEXT);
	}
}


static mload(scrn,row,col,pfvalue,text,pfkey_map)
char	*scrn;
int	row, col;
int	pfvalue;
char	*text;
int	pfkey_map[33];
{
	char	*ptr;

	pfkey_map[pfvalue] = (row+1)*80 + (col+2);					/* Map the row + col to a PFKEY value	*/

	ptr = scrn + 4 + row*80 + col;

	*ptr++ = NUMPROT_FIELD;
	*ptr++ = WANG_MENU_PICK;
	ptr++;
	memcpy(ptr,text, strlen(text));
}
/*
	pfkey_value	Return the equivalent PFKEY value based on the row and col from the order area
*/
static int pfkey_value(scrn,pfkey_map)
char	*scrn;
int	pfkey_map[33];
{
	int 	i;
	int	pos;

	pos = (int)scrn[3]*80 + (int)scrn[2];
	for(i=0;i<33;i++)
	{
		if (pos == pfkey_map[i]) return(i);
	}
	return(0);
}

static build_non_wang_base(scrn)
char *scrn;
{
	char	temp[255];

		wsc_init(scrn,0,0);							/* Set the screen frame to blank.	*/
		pack(scrn, 0, 24, 0, " ***  WISP HELP Processor  ***");		/* Store the header.			*/
		pack(scrn, 1, 17, 0, "International Digital Scientific Incorporated");

		sprintf(temp,"Username:  %s", longuid());
		pack(scrn, 4, 2, 0, temp);

		temp[0] = '\0';
#ifdef unix
		{
			struct utsname unix_name;

			if ( -1 != uname(&unix_name) )
			{
				sprintf(temp, "System:    %s - %s", unix_name.sysname, unix_name.nodename);
			}
		}
#endif
		if (!temp[0])
		{
#ifdef VMS
			strcpy(temp,"System:    VAX/VMS");
#endif
#ifdef MSDOS
			strcpy(temp,"System:    MS-DOS");
#endif
#ifdef unix
			strcpy(temp,"System:    UNIX");
#endif
		}
		pack(scrn, 5, 2, 0, temp);

		if (g_prog_running)							/* Is a program running?		*/
		{
			if (wisp_progname[0] && wisp_progname[0] != ' ')		/* We know the program name		*/
			{
				sprintf(temp, "Program:   %s",wisp_progname);
				pack(scrn, 6, 2, 0, temp);

				if (wisp_screen[0] && wisp_screen[0] != ' ')		/* We also know the screen name		*/
				{
					sprintf(temp, "Screen:    %s",wisp_screen);
					pack(scrn, 7, 2, 0, temp);
				}
			}
			else
			{
				if (WISPRUNNAME[0] != ' ')
				{
					sprintf(temp, "Program:   %8.8s",WISPRUNNAME);
					pack(scrn, 6, 2, 0, temp);
				}
			}
		}
		memset(scrn+4+( 9*80),'-',80);
		memset(scrn+4+(22*80),'-',80);
		pack(scrn,23,10,0,"(TAB) (SPACE) or (ARROWS) to move.     (RETURN) to select.");
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
int wsh_cancel()
{
	char 	scrn[1924];
	char 	okkeys[20];
	char 	function,lines,term[2],no_mod[2];

	wsc_init(scrn,0,0);								/* Set the screen frame to blank.	*/
	pack(scrn,  0, 33,  0, "*** Cancel ***");					/* Store the header.			*/
	pack(scrn,  7, 12,  0, "WARNING -");
	pack(scrn,  8, 17,  0, "Cancel is used to terminate your current program");
	pack(scrn,  9, 17,  0, "and return all resources to the system. It will");
	pack(scrn, 10, 17,  0, "attempt to complete any outstanding file I/O and");
	pack(scrn, 11, 17,  0, "then close all files opened by the program.");
	pack(scrn, 15, 12,  0, "SELECT:");
	pack(scrn, 17, 17,  0, "(ENTER)  to begin Cancel Processing");
	pack(scrn, 19, 17,  0, "(HELP)   to return to the Command Processor");


	function = DISPLAY_AND_READ;							/* Display and read.			*/
	lines = 24;									/* Use full screen.			*/
	strcpy(okkeys,"00X");
	vwang(&function,scrn,&lines,okkeys,term,no_mod);				/* Call Wang emulation to fill screen.	*/

	if (term[0] == '0' && term[1] == '0')
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
**	History:	
**	01/13/93	Written by GSL
**
*/
int wsh_exit()
{
	char 	scrn[1924];
	char 	okkeys[20];
	char 	function,lines,term[2],no_mod[2];

	wsc_init(scrn,0,0);								/* Set the screen frame to blank.	*/
	pack(scrn,  0, 33,  0, "*** Exit ***");						/* Store the header.			*/
	pack(scrn,  7, 12,  0, "WARNING -");
	pack(scrn,  8, 17,  0, "Exit is used to terminate the WUSAGE Command Processor");
	pack(scrn,  9, 17,  0, "and return you to the operating system.");
	pack(scrn, 15, 12,  0, "SELECT:");
	pack(scrn, 17, 17,  0, "(ENTER)  to Exit the Command Processor");
	pack(scrn, 19, 17,  0, "(HELP)   to return to the Command Processor");


	function = DISPLAY_AND_READ;							/* Display and read.			*/
	lines = 24;									/* Use full screen.			*/
	strcpy(okkeys,"00X");
	vwang(&function,scrn,&lines,okkeys,term,no_mod);				/* Call Wang emulation to fill screen.	*/

	if (term[0] == '0' && term[1] == '0')
	{
		return(1);
	}

	return(0);
}


static int is_help_active = 0;

int ishelpactive()
{
	return is_help_active;
}
int sethelpactive(flag)
int flag;
{
	return(is_help_active = flag);
}
