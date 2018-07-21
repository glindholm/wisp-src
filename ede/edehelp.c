/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
******************************************************************************
*/


/*
**	File:		edehelp.c
**
**	Purpose:	The EDE help system
**
**	Routines:	
**	ws_bar_menu()	The HELP bar menu
**	A_WSLINK()	Called to turn on ON-LINE doc feature
**	openhelpmap()	Open the HELPMAP file.
**	filehelp()	Gives HELP based on helpfiles.
**	disphelp()	Displays a single helpfile.
**	first_diff()	Finds the first difference in two strings
**	trim()		Trims a string.
**
*/


/*						Include required header files.							*/


#include <stdio.h>									/* Include video definitions.		*/
#include <stdlib.h>
#include <string.h>

#include <video.h>
#include <vdata.h>
#include <vmenu.h>
#include <vutil.h>

#include "idsistd.h"
#include "vwang.h"
#include "wglobals.h"
#include "wperson.h"
#include "wisplib.h"

/*
**	This are defined in edeoldoc.c and can be customized by the user.
*/
extern void EDEOLDOC();
extern int EDECUSTM(char custtitle[64]);
extern void EDECUSTX();
	

/*						Static and Global Data Definitions.						*/

static int ol_doc = FALSE;								/* Assume on-line documentation off.	*/
static unsigned char *snptr = NULL;							/* Point to the screen number.		*/
static unsigned char *elptr = NULL;
static unsigned char *odptr = NULL;
static char *helpmap;									/* System base location.		*/

static int filehelp();
static int first_diff();
static int trim();
static int disphelp();

#define HELP_CODE 10									/* Define the code for help.		*/
#define EXIT_CODE 99									/* The exit/abort code.			*/
#define WANG_CODE 11
#define GOOD_CODE 12
#define CLOCK_CODE 121
#define NOTE_CODE 122
#define CALC_CODE 123
#define CALEND_CODE 124
#define ZONE_CODE 125
#define PUZZLE_CODE 126
#define CUT_CODE 201
#define PASTE_CODE 202
#define EDECUSTOM_CODE 500

/*
**	Routine:	ws_bar_menu()
**
**	Function:	The standard EDE HELP bar menu.
**
**	Description:	This routine generates the standard EDE bar menu and handles the selections
**			by calling the appropriate routines to handle.
**
**			It also provides a hook into a user defined ON-LINE documentation system.
**			This was originally done for STERLING so is modeled to accomidate there
**			existing system with a windows type look.  When HELP is selected from the
**			EDE bar menu and the on-line doc is active it calls EDEOLDOC passing it
**			the current cursor position and a CONTEXT string.  EDEOLDOC must then 
**			return the help text which is displayed in a window.
**
**	Arguments:
**	curset		Flag if cursor on or off
**	vr		Cursor row
**	vc		Cursor column
**	dummy		(not used)
**	ar		Altered read flag
**	nm		No Modification flag
**	dp		Do pseudoblank processing flag
**
**	History:	
**	04/16/93	Change to call EDEOLDOC. GSL
**	05/18/94	Added to interface documentation. JAK
**
*/
int ws_bar_menu(int curset, int vr, int vc, int dummy, int ar, unsigned char* nm, int dp)
{
	struct hlp_buf 
	{ 
		int hlp_row; 
		int hlp_col; 
		char hlp_program[9]; 
		char hlp_screen[33]; 
		short buf_end; 
	};
	typedef struct hlp_buf help_buf;
	struct hlp_text 
	{
#define	HLP_LINE_SIZE	64
		char hlp_line_0[HLP_LINE_SIZE];	
		char hlp_line_1[HLP_LINE_SIZE]; 
		char hlp_line_2[HLP_LINE_SIZE]; 	
		char hlp_line_3[HLP_LINE_SIZE];
		char hlp_line_4[HLP_LINE_SIZE]; 	
		char hlp_line_5[HLP_LINE_SIZE]; 
		char hlp_line_6[HLP_LINE_SIZE]; 	
		char hlp_line_7[HLP_LINE_SIZE];
		char hlp_line_8[HLP_LINE_SIZE]; 	
		char hlp_line_9[HLP_LINE_SIZE]; 
		short txt_end; 
	};
	typedef struct hlp_text help_text;
	help_text *hl_txt;
        help_buf *hl_buf;
	char blankline[HLP_LINE_SIZE];
	int hl_cnt, hl_rtn;
	struct video_menu menu_bar, menu_exit, menu_good, olh;				/* The default menu bar.		*/
	enum e_vop op_save;								/* Optimization save word.		*/
	int choice;									/* Menu choice.				*/
	int row_current, col_current;							/* Current screen locations.		*/
	char temp[MAX_COLUMNS_PER_LINE];						/* Working storage.			*/
	int mdsave, pfsave;								/* Menu state save words.		*/
	int r, c, k = 0;
	struct cursor_position 
	{ 
		short int sc; 
		short int sr; 
	} curspos;
	unsigned char *ssave;								/* Screen save.				*/
	char	custtitle[64];

	if (WL_ishelpactive())								/* Are we already in help.		*/
	{
		vwang_bad_char();								/* Ring the bells.			*/
		return(FAILURE);							/* Return to the caller.		*/
	}
	else WL_sethelpactive(TRUE);							/* Now help is active.			*/
	VL_vdefer_restore();								/* Cannot be in deferred mode.		*/

	row_current = vcur_lin+1;							/* Remember where on Wang screen.	*/
	col_current = vcur_col+1;

	hl_cnt = hl_rtn = 0;
	hl_txt = (help_text *) 0;
	hl_buf = (help_buf *) 0;
	memset(blankline,' ',HLP_LINE_SIZE);
	op_save = VL_voptimize(VOP_DEFER_MODE);						/* Make sure optimization is on.	*/

	VL_vmenuinit(&menu_bar, BAR_MENU, VMODE_REVERSE, 0, 3, 3);				/* Initialize the menu definition.	*/
	VL_vmenuitem(&menu_bar, "Help", HELP_CODE, DYNAMIC_LINK);				/* Add the help selection.		*/
	VL_vmenuitem(&menu_bar, "Environment", WANG_CODE, DYNAMIC_LINK);			/* Add the environment selection.	*/
	if (EDECUSTM(custtitle))
	{
		/*
		**	If the user has specified a custom memu item then add it here.
		*/
		VL_vmenuitem(&menu_bar, custtitle, EDECUSTOM_CODE, NULL);
	}
	VL_vmenuitem(&menu_bar, "Goodies", GOOD_CODE, &menu_good);				/* Add the goodies selection.		*/
	VL_vmenuinit(&menu_good, 0, VMODE_REVERSE, 0, 0, 0);				/* Add the exit menu.			*/
	VL_vmenuitem(&menu_good, "Clock", CLOCK_CODE, NULL);				/* Add the items.			*/
	VL_vmenuitem(&menu_good, "Calculator", CALC_CODE, NULL);
	VL_vmenuitem(&menu_good, "Calendar", CALEND_CODE, NULL);
	VL_vmenuitem(&menu_good, "Notepad", NOTE_CODE, NULL);
	VL_vmenuitem(&menu_good, "Puzzle", PUZZLE_CODE, NULL);
	VL_vmenuitem(&menu_bar, "Cut", CUT_CODE, NULL);
	VL_vmenuitem(&menu_bar, "Paste", PASTE_CODE, NULL);
	VL_vmenuitem(&menu_bar, "Exit", EXIT_CODE, &menu_exit);				/* Add the exit selection.		*/
	VL_vmenuinit(&menu_exit, 0, VMODE_REVERSE, 0, 0, 0);				/* Add the exit menu.			*/
	VL_vmenuitem(&menu_exit, "Return to program", EXIT_CODE, NULL);			/* Add the single item.			*/

	VL_vmenustatus(&mdsave, &pfsave);							/* Get the menu status.			*/
	VL_vmenu_pfkeys(OFF);								/* Turn PF key processing off.		*/
	VL_vmenumode(STATIC_MENU);								/* This is a static menu.		*/
	choice = VL_vmenugo(&menu_bar);							/* Display the menu.			*/
	VL_vmenu_pfkeys(pfsave);								/* Restore the PF keys.			*/
	VL_vmenumode(mdsave);								/* Restore the menu mode.		*/

	switch (choice)
	{
		case HELP_CODE:								/* Requesting on line help.		*/
		{
			if (ol_doc)
			{
				r = row_current + 3;					/* Place the help nicely near it.	*/
				c = col_current;
				if (r > 14) r = row_current - 12;
				if (col_current <= 1) c = 3;
				while ((c + 70) > VL_vscr_wid-1) c = c-1;

				hl_txt = (help_text *)malloc(sizeof(help_text));
				hl_txt->txt_end = 0;
				curspos.sr = row_current;
				curspos.sc = col_current;
                                hl_rtn = 0;

				ssave = VL_vsss(0,0,MAX_LINES_PER_SCREEN,VL_vscr_wid);
				EDEOLDOC(&curspos,odptr,hl_txt,&hl_cnt,&hl_rtn);
				/*
				**	A return code (hl_rtn) of
				**		0	Success
				**		1	No help, Empty or not found
				**		2	Don't draw a box, help takes full screen
				**			(was drawn by EDEOLDOC ?)
				*/
				VL_vrss(ssave);

	                        if (hl_rtn != 2) VL_vmenuinit(&olh, DISPLAY_ONLY_MENU, VMODE_REVERSE, r, c, 0);
				if (ol_doc && !hl_rtn)
				{
					if (	strncmp(hl_txt->hlp_line_0,blankline,HLP_LINE_SIZE) ||
					    	strncmp(hl_txt->hlp_line_1,blankline,HLP_LINE_SIZE) 	)
						VL_vmenuitem(&olh, hl_txt->hlp_line_0, 0, 0);

					if (	strncmp(hl_txt->hlp_line_1,blankline,HLP_LINE_SIZE) ||
						strncmp(hl_txt->hlp_line_2,blankline,HLP_LINE_SIZE) 	)
						VL_vmenuitem(&olh, hl_txt->hlp_line_1, 0, 0);

					if (	strncmp(hl_txt->hlp_line_2,blankline,HLP_LINE_SIZE) ||
						strncmp(hl_txt->hlp_line_3,blankline,HLP_LINE_SIZE) 	)
						VL_vmenuitem(&olh, hl_txt->hlp_line_2, 0, 0);

					if (	strncmp(hl_txt->hlp_line_3,blankline,HLP_LINE_SIZE) ||
						strncmp(hl_txt->hlp_line_4,blankline,HLP_LINE_SIZE) 	)
						VL_vmenuitem(&olh, hl_txt->hlp_line_3, 0, 0);

					if (	strncmp(hl_txt->hlp_line_4,blankline,HLP_LINE_SIZE) ||
						strncmp(hl_txt->hlp_line_5,blankline,HLP_LINE_SIZE) 	)
						VL_vmenuitem(&olh, hl_txt->hlp_line_4, 0, 0);

					if (	strncmp(hl_txt->hlp_line_5,blankline,HLP_LINE_SIZE) ||
						strncmp(hl_txt->hlp_line_6,blankline,HLP_LINE_SIZE) 	)
						VL_vmenuitem(&olh, hl_txt->hlp_line_5, 0, 0);

					if (	strncmp(hl_txt->hlp_line_6,blankline,HLP_LINE_SIZE) ||
						strncmp(hl_txt->hlp_line_7,blankline,HLP_LINE_SIZE) 	)
						VL_vmenuitem(&olh, hl_txt->hlp_line_6, 0, 0);

					if (	strncmp(hl_txt->hlp_line_7,blankline,HLP_LINE_SIZE) ||
						strncmp(hl_txt->hlp_line_8,blankline,HLP_LINE_SIZE) 	)
						VL_vmenuitem(&olh, hl_txt->hlp_line_7, 0, 0);

					if (	strncmp(hl_txt->hlp_line_8,blankline,HLP_LINE_SIZE) ||
						strncmp(hl_txt->hlp_line_9,blankline,HLP_LINE_SIZE) 	)
						VL_vmenuitem(&olh, hl_txt->hlp_line_8, 0, 0);

					if (	strncmp(hl_txt->hlp_line_9,blankline,HLP_LINE_SIZE)	)
						VL_vmenuitem(&olh, hl_txt->hlp_line_9, 0, 0);

		                        if (hl_txt) free(hl_txt);
				}
				else if (hl_rtn != 2)
				{
					sprintf((char *) temp, "On-line quick help for program %s", wisp_get_progname());
					VL_vmenuitem(&olh, temp, 0, 0);
					VL_vmenuitem(&olh, "", 0, 0);
					VL_vmenuitem(&olh, "Sorry, help is not currently available for", 0, 0);
					VL_vmenuitem(&olh, "this field.  Please see your administrator", 0, 0);
					VL_vmenuitem(&olh, "to have it added.  Thank you.", 0, 0);
					VL_vmenuitem(&olh, "", 0, 0);
					VL_vmenuitem(&olh, "Use the arrow keys to move this window", 0, 0);
					VL_vmenuitem(&olh, "", 0, 0);
					VL_vmenuitem(&olh, "Depress any other key to continue...", 0, 0);
				}

				VL_vdl_lin = row_current - 1;				/* Set where we want the cursor.	*/
				VL_vdl_col = col_current - 1;

				if (hl_rtn != 2) k = VL_vmenugo(&olh);

				if ((ol_doc) && (k == help_key) && !hl_rtn)
				{
					curspos.sr = row_current;
					curspos.sc = col_current;
					vwang_wpushscr();
					EDEOLDOC(&curspos,odptr,hl_txt,&hl_cnt,&hl_rtn);
					vwang_wpopscr();
				}
			}

			else
			{
				k = VL_vml(row_current-1);
				filehelp(k+1,vwang_ws_sof(row_current-1,col_current-1)+1);
			}

			break;
		}

		case CALC_CODE:		{ gcalc(); break; }				/* Does the user want a goodie?		*/
		case CALEND_CODE:	{ gcalend(); break; }
		case CLOCK_CODE:	{ gclock(); break; }
		case NOTE_CODE:		{ gnotepad(); break; }
		case PUZZLE_CODE:	{ gpuzzle(); break; }
		case ZONE_CODE:		{ gzones(); break; }
		case CUT_CODE:		{ vwang_ws_cut(row_current-1,col_current-1); break; }
		case PASTE_CODE:	{ vwang_ws_paste(row_current-1,col_current-1,vr,vc,ar,nm,dp); break; }
		case WANG_CODE:
		{
			WL_sethelpactive(FALSE);						/* Turn off the active help.		*/
			vwang_help(curset);						/* Go to environment processing.	*/
			break;
		}
		case EDECUSTOM_CODE:
		{
			/*
			**	Process the custom menu item by calling EDECUSTX() routine.
			**	This acts like the EDEOLDOC so we need to push and pop the screen.
			*/
			vwang_wpushscr();
			EDECUSTX();
			vwang_wpopscr();
		}
	}

	VL_voptimize(op_save);								/* Put the optimization back as it was.	*/
	WL_sethelpactive(FALSE);								/* Now help is inactive.		*/
	VL_synch_required = FALSE;								/* A synch is not required.		*/
	return(SUCCESS); 								/* All done.				*/
}

/*
**	Routine:	A_WSLINK()
**
**	Function:	To enable and setup the ON-LINE doc feature.
**
**	Description:	This routine is called near the beginning of a user application
**			to enable EDE's custom user ON-LINE doc feature and gets passed
**      		in setup parameters.
**
**	Arguments:
**	ets_link_program_definition	33 character parameter area, as follows:
**					user help pfkey number = signed   char 03
**					user help program name = unsigned char 08
**					blank filler           = unsigned char 02
**					user help prog library = unsigned char 08
**					blank filler           = unsigned char 02
**                                      user help prog volume  = unsigned char 06
**					blank filler           = unsigned char 04
**
**	on_line_doc_passing_values	167 character parameter area, as follows:
**					user application name descrp  = unsigned char 79
**					user computer type	      = unsigned char 01
**					user company name             = unsigned char 30
**					user help document library    = unsigned char 08
**					user help document volume     = unsigned char 06
**					user security classes         = unsigned char 27
**					user application program name = unsigned char 08
**					user screen number            = unsigned char 03
**					user screen phase	      = unsigned char 05
**
**	Globals:
**	elptr		Pointer to values that define user HELP program, passed
**			by user application program (ie. ets_link-program_definition).
**	odptr		Pointer to values passed in by user application program (ie. 
**			on_line_doc_passing_values).
**	snptr		Pointer to screen number field in on_line_doc_passing_values.
**	ol_doc		Enable ON-LINE doc feature.
**
**	Return:		None
**
**	Warnings:	None
**
**
*/
void A_WSLINK(unsigned char *ets_link_program_definition, unsigned char *on_line_doc_passing_values)
{
	elptr = ets_link_program_definition;						/* Remember where the link params are.	*/
	odptr = on_line_doc_passing_values;						/* Remember where the doc values are.	*/
	snptr = odptr + 159;								/* Screen number is 159 bytes in.	*/
	ol_doc = TRUE;									/* On-line documentation is now on.	*/
}

/*
	openhelpmap:	This routine opens the HELPMAP file.
			The first time in it constructs the native path to HELPMAP.

				unix	$(WISPCONFIG)/HELPMAP
				WIN32	$(WISPCONFIG)\\HELPMAP.DAT
*/
#ifdef WIN32 
#define HELPFILE	"HELPMAP.DAT"
#endif
#ifdef unix
#define HELPFILE	"HELPMAP"
#endif

static FILE *openhelpmap(type)
char *type;
{
	FILE 	*fd;
	static int first = 1;
	static char helpmapbuff[80];

	if (first)
	{
		first = 0;
		WL_build_wisp_config_path(HELPFILE,helpmapbuff);
		helpmap = helpmapbuff;
	}
	
	fd = fopen(helpmap,type); 							/* Open the help file.			*/

	return(fd);
}

static int filehelp(r,c) int r,c;							/* Give help using the help files.	*/
{
	char temp[256];									/* Working string.			*/
	FILE *hf;									/* File stuff.				*/
	register int i, j, k, m;
	int sr, sc, ec;
	char sub_progname[133], sub_screen[133];					/* Substitution strings.		*/

	if ((hf = openhelpmap("r")) == NULL) 						/* Open the help file.			*/
	{
		VL_vre_window("Help-F-File %s not found or access denied.",HELPFILE);
		return(FAILURE);							/* Game over...				*/
	}

	sub_progname[0] = CHAR_NULL;							/* Assume no substitution names.	*/
	sub_screen[0] = CHAR_NULL;

	for (m = 0; (m < 3) && (fgets(temp,132,hf) != NULL); m++)			/* Read 1st two lines of the file.	*/
	{
		trim(temp);								/* Trim control characters.		*/
		j = first_diff("PROGRAM_NAME_AT",temp);					/* Is the identifier found.		*/
		k = strlen("PROGRAM_NAME_AT");
		if (j == k)								/* Is this the identifier.		*/
		{
			sscanf(&temp[k+1],"%d %d %d",&sr, &sc, &ec);			/* Decode the screen position.		*/
			ec = (sc + ec) - 1;						/* Set to end col rather than LENGTH.	*/
										/***** ADD VALIDITY CHECKING HERE *****/
			for (i = 0; i <= ec-sc; i++) sub_progname[i] = vwang_charat(sr-1,sc-1+i);	/* Get the sub prog name.	*/
			sub_progname[i] = CHAR_NULL;					/* Null terminate.			*/
			trim(sub_progname);
		}

		j = first_diff("SCREEN_NAME_AT",temp);					/* Is the identifier found.		*/
		k = strlen("SCREEN_NAME_AT");
		if (j == k)								/* Is this the identifier.		*/
		{
			sscanf(&temp[k+1],"%d %d %d",&sr, &sc, &ec);			/* Decode the screen position.		*/
			ec = (sc + ec) - 1;						/* Set to end col rather than LENGTH.	*/
										/***** ADD VALIDITY CHECKING HERE *****/
			for (i = 0; i <= ec-sc; i++) sub_screen[i] = vwang_charat(sr-1,sc-1+i);	/* Get the sub prog name.	*/
			sub_screen[i] = CHAR_NULL;					/* Null terminate.			*/
			trim(sub_screen);
		}
	}
	fclose(hf);									/* Close for later.			*/

	if (*(wisp_get_progname())) strcpy(sub_progname, wisp_get_progname());		/* Copy in the program name.		*/
	if (*(wisp_get_screenname())) strcpy(sub_screen, wisp_get_screenname());	/* Copy in the screen name.		*/

	if (vwang_ws_mod(r-1,c-1))							/* Is cursor in a mod field?		*/
	{
		sprintf( (char *) temp, "%s_%s_%d_%d", sub_progname, sub_screen, r, c);
		if (disphelp(temp)) return(SUCCESS);
		sprintf( (char *) temp, "%s_%s_%d_%d", sub_progname, sub_screen, r, 0);
		if (disphelp(temp)) return(SUCCESS);
		sprintf( (char *) temp, "%s_%s_%d_%d", sub_progname, sub_screen, 0, c);
		if (disphelp(temp)) return(SUCCESS);
	}
	sprintf( (char *) temp, "%s_%s", sub_progname, sub_screen);
	if (disphelp(temp)) return(SUCCESS);
	sprintf( (char *) temp, "%s", sub_progname);
	if (disphelp(temp)) return(SUCCESS);
	if (disphelp("GENERAL_HELP")) return(SUCCESS);
	VL_vre_window("No more help available. ID: %s_%s_%d_%d", sub_progname, sub_screen, r, c);
	return(SUCCESS);
}

static int disphelp(help_descriptor) char *help_descriptor;				/* Display the help from a file.	*/
{
	FILE *hf, *hm;								/* Open a file.				*/
	struct video_menu help_window;							/* Help window structure.		*/
	register int i, j, k = 0;								/* Working registers.			*/
	char temp[256];
	int found;									/* Entry found flag.			*/

	if (!(*help_descriptor)) return(FALSE);						/* Don't match to a NULL string.	*/

	if ((hm = openhelpmap("r")) == NULL) return(FAILURE);				/* Open the help control file.		*/

	found = FALSE;									/* Entry not found yet.			*/
	while (!found && (fgets(temp,132,hm) != NULL))					/* Look for the entry.			*/
	{
		trim(temp);								/* Trim control characters.		*/
		j = first_diff(help_descriptor,temp);					/* Is the identifier found.		*/
		k = strlen(help_descriptor);
		if (j == k) found = TRUE;
	}
	fclose(hm);									/* Now close the file.			*/

	if (!found) return(FALSE);							/* No such record.			*/

        k++;
	while ((temp[k] <= 040) && (temp[k] != CHAR_NULL)) k++;				/* Find the file name.			*/
	trim(&temp[k]);									/* Trim it.				*/
	if ((hf = fopen(&temp[k],"r")) == NULL)						/* Can we open the actual help file?	*/
	{
		k--;									/* Set pointer to beginning of temp.	*/
		VL_vre_window("Help-F-File %s not found or not accessable (privs).",&temp[k]);
		return(FAILURE);
	}

	VL_vmenuinit(&help_window, DISPLAY_ONLY_MENU, VMODE_REVERSE, 0, 0, 0);		/* Init the help window.		*/

	i = 0;										/* Now read the file.			*/
	while ((fgets(temp,132,hf) != NULL) && (i < 14))				/* Read the file.			*/
	{
		trim(temp);								/* Trim the string.			*/
		VL_vmenuitem(&help_window, temp, 0, NULL);					/* Put it in the menu.			*/
	}
	fclose(hf);									/* Close the file.			*/
	VL_vmenuitem(&help_window,"",0,NULL);						/* Final indicator.			*/
	VL_vmenuitem(&help_window,"Depress HELP for more info, any other key to continue...",0,NULL);
	i = VL_vmenugo(&help_window);							/* Display the help.			*/
	if (i == help_key) return(FAILURE);						/* More help is requested.		*/
	return(SUCCESS);								/* Return to the caller.		*/
}

static int first_diff(s1,s2) char *s1, *s2;						/* Find the 1st difference in a string.	*/
{
	register int i;

	i = 0;
	while ((s1[i] == s2[i]) && (s1[i] != CHAR_NULL) && (s2[i] != CHAR_NULL)) i++;	/* Scan until the first difference.	*/
	return(i);
}

static int trim(temp) char *temp;							/* Trim a string.			*/
{
		register int j;
		j = 0;									/* Look for the end of line flag.	*/
		while(temp[j] >= 040) j++;						/* Find the first control character.	*/
		temp[j] = CHAR_NULL;							/* Insert a null.			*/
		return(j);
}
/*
**	History:
**	$Log: edehelp.c,v $
**	Revision 1.26  2003/06/27 15:54:03  gsl
**	fix EDE API
**	
**	Revision 1.25  2003/06/23 15:28:04  gsl
**	VL_ global symbols
**	
**	Revision 1.24  2003/02/05 21:47:53  gsl
**	fix -Wall warnings
**	
**	Revision 1.23  2003/02/04 18:57:01  gsl
**	fix copyright header
**	
**	Revision 1.22  2002/07/16 13:40:24  gsl
**	VL_ globals
**	
**	Revision 1.21  2002/07/15 20:16:04  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.20  2002/07/15 17:09:58  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.19  2002/07/15 14:07:03  gsl
**	vwang globals
**	
**	Revision 1.18  2002/07/12 20:40:37  gsl
**	Global unique WL_ changes
**	
**	Revision 1.17  2002/07/11 20:29:20  gsl
**	Fix WL_ globals
**	
**	Revision 1.16  2002/07/10 21:06:33  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.15  2002/07/09 04:14:07  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.14  2002/06/26 01:42:48  gsl
**	Remove VMS code
**	
**	Revision 1.13  1997/07/08 20:10:58  gsl
**	Change to use new video.h defines
**	
**	Revision 1.12  1997-01-02 17:08:23-05  gsl
**	Add custom menu item logic for STERLING
**
**	Revision 1.11  1996-09-13 10:52:58-07  gsl
**	mod for NT
**	Changed to use WL_build_wisp_config_path() to create helpmap path
**
**
**
*/
