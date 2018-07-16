			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/


/*						Include required header files.							*/

#ifdef	MSDOS
#include <malloc.h>
#include <stdlib.h>
#endif

#include <stdio.h>									/* Include video definitions.		*/
#include <v/video.h>
#include <v/vlocal.h>
#include <v/vdata.h>
#include <v/vmenu.h>
#include "vwang.h"
#include "wglobals.h"

/*						Static and Global Data Definitions.						*/

extern int help_active;									/* Reference the help active flag.	*/
static int ol_doc = FALSE;								/* Assume on-line documentation off.	*/
static unsigned char *snptr;								/* Point to the screen number.		*/
static unsigned char *elptr;
static unsigned char *odptr;
static char *helpmap;									/* System base location.		*/

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

int ws_bar_menu(curset,vr,vc,ak,ar,nm,dp)						/* Put up a menu bar.			*/
int curset,vr,vc,ak,ar,dp;								/* vcur_lin,vcur_col,alt_read,do_pseudo	*/
unsigned	char	*nm;								/* no_mod				*/
{
	struct hlp_buf { int hlp_row; int hlp_col; char hlp_program[9]; char hlp_screen[33]; short buf_end; };
	typedef struct hlp_buf help_buf;
	struct hlp_text {char hlp_line_0[64]; char hlp_line_1[64]; char hlp_line_2[64]; char hlp_line_3[64];
			char hlp_line_4[64]; char hlp_line_5[64]; char hlp_line_6[64]; char hlp_line_7[64];
		        char hlp_line_8[64]; char hlp_line_9[64]; short txt_end; };
	typedef struct hlp_text help_text;
	help_text *hl_txt;
        help_buf *hl_buf;
	char hl_cmp[64];
	int hl_cnt, hl_rtn;
	struct video_menu menu_bar, menu_exit, menu_good, olh;				/* The default menu bar.		*/
	int bar_active;									/* Working parameter.			*/
	int op_save;									/* Optimization save word.		*/
	int choice;									/* Menu choice.				*/
	int row_current, col_current;							/* Current screen locations.		*/
	unsigned char temp[MAX_COLUMNS_PER_LINE];					/* Working storage.			*/
	int mdsave, pfsave;								/* Menu state save words.		*/
	int r, c, k;
	struct cursor_position { short int sc; short int sr; } curspos;
	unsigned char *ssave, *vsss();							/* Screen save.				*/

	if (help_active)								/* Are we already in help.		*/
	{
		ws_bad_char();								/* Ring the bells.			*/
		return(FAILURE);							/* Return to the caller.		*/
	}
	else help_active = TRUE;							/* Now help is active.			*/
	vdefer(RESTORE);								/* Cannot be in deferred mode.		*/

	row_current = vcur_lin+1;							/* Remember where on Wang screen.	*/
	col_current = vcur_col+1;

	hl_cnt = hl_rtn = 0;
	hl_txt = (help_text *) 0;
	hl_buf = (help_buf *) 0;
	memset(hl_cmp,' ',64);
	op_save = voptimize(DEFER_MODE);						/* Make sure optimization is on.	*/

	vmenuinit(&menu_bar, BAR_MENU, REVERSE, 0, 3, 3);				/* Initialize the menu definition.	*/
	vmenuitem(&menu_bar, "Help", HELP_CODE, DYNAMIC_LINK);				/* Add the help selection.		*/
	vmenuitem(&menu_bar, "Environment", WANG_CODE, DYNAMIC_LINK);			/* Add the environment selection.	*/
	vmenuitem(&menu_bar, "Goodies", GOOD_CODE, &menu_good);				/* Add the goodies selection.		*/
	vmenuinit(&menu_good, 0, REVERSE, 0, 0, 0);					/* Add the exit menu.			*/
	vmenuitem(&menu_good, "Clock", CLOCK_CODE, NULL);				/* Add the items.			*/
	vmenuitem(&menu_good, "Calculator", CALC_CODE, NULL);
	vmenuitem(&menu_good, "Calendar", CALEND_CODE, NULL);
	vmenuitem(&menu_good, "Notepad", NOTE_CODE, NULL);
	vmenuitem(&menu_good, "Puzzle", PUZZLE_CODE, NULL);
	vmenuitem(&menu_bar, "Cut", CUT_CODE, NULL);
	vmenuitem(&menu_bar, "Paste", PASTE_CODE, NULL);
	vmenuitem(&menu_bar, "Exit", EXIT_CODE, &menu_exit);				/* Add the exit selection.		*/
	vmenuinit(&menu_exit, 0, REVERSE, 0, 0, 0);					/* Add the exit menu.			*/
	vmenuitem(&menu_exit, "Return to program", EXIT_CODE, NULL);			/* Add the single item.			*/

	vmenustatus(&mdsave, &pfsave);							/* Get the menu status.			*/
	vmenu_pfkeys(OFF);								/* Turn PF key processing off.		*/
	vmenumode(STATIC_MENU);								/* This is a static menu.		*/
	choice = vmenugo(&menu_bar);							/* Display the menu.			*/
	vmenu_pfkeys(pfsave);								/* Restore the PF keys.			*/
	vmenumode(mdsave);								/* Restore the menu mode.		*/

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
				while ((c + 70) > vscr_wid-1) c = c-1;

				hl_txt = (help_text *)malloc(sizeof(help_text));
				hl_txt->txt_end = 0;
				curspos.sr = row_current;
				curspos.sc = col_current;
                                hl_rtn = 0;

				ssave = vsss(0,0,MAX_LINES_PER_SCREEN,vscr_wid);
				SFREU041(&curspos,odptr,hl_txt,&hl_cnt,&hl_rtn);
				vrss(ssave);

	                        if (hl_rtn != 2) vmenuinit(&olh, DISPLAY_ONLY_MENU, REVERSE, r, c, 0);
				if (ol_doc && !hl_rtn)
				{
					if (!strncmp(hl_txt->hlp_line_0,hl_cmp,64) && !strncmp(hl_txt->hlp_line_1,hl_cmp,64) );
					else vmenuitem(&olh, hl_txt->hlp_line_0, 0, 0);
					if (!strncmp(hl_txt->hlp_line_1,hl_cmp,64) && !strncmp(hl_txt->hlp_line_2,hl_cmp,64) );
					else vmenuitem(&olh, hl_txt->hlp_line_1, 0, 0);
					if (!strncmp(hl_txt->hlp_line_2,hl_cmp,64) && !strncmp(hl_txt->hlp_line_3,hl_cmp,64) );
					else vmenuitem(&olh, hl_txt->hlp_line_2, 0, 0);
					if (!strncmp(hl_txt->hlp_line_3,hl_cmp,64) && !strncmp(hl_txt->hlp_line_4,hl_cmp,64) );
					else vmenuitem(&olh, hl_txt->hlp_line_3, 0, 0);
					if (!strncmp(hl_txt->hlp_line_4,hl_cmp,64) && !strncmp(hl_txt->hlp_line_5,hl_cmp,64) );
					else vmenuitem(&olh, hl_txt->hlp_line_4, 0, 0);
					if (!strncmp(hl_txt->hlp_line_5,hl_cmp,64) && !strncmp(hl_txt->hlp_line_6,hl_cmp,64) );
					else vmenuitem(&olh, hl_txt->hlp_line_5, 0, 0);
					if (!strncmp(hl_txt->hlp_line_6,hl_cmp,64) && !strncmp(hl_txt->hlp_line_7,hl_cmp,64) );
					else vmenuitem(&olh, hl_txt->hlp_line_6, 0, 0);
					if (!strncmp(hl_txt->hlp_line_7,hl_cmp,64) && !strncmp(hl_txt->hlp_line_8,hl_cmp,64) );
					else vmenuitem(&olh, hl_txt->hlp_line_7, 0, 0);
					if (!strncmp(hl_txt->hlp_line_8,hl_cmp,64) && !strncmp(hl_txt->hlp_line_9,hl_cmp,64) );
					else vmenuitem(&olh, hl_txt->hlp_line_8, 0, 0);
					if (!strncmp(hl_txt->hlp_line_9,hl_cmp,64));
					else vmenuitem(&olh, hl_txt->hlp_line_9, 0, 0);
		                        if (hl_txt) free(hl_txt);
				}
				else if (hl_rtn != 2)
				{
					sprintf((char *) temp, "On-line quick help for program %s", wisp_progname);
					vmenuitem(&olh, temp, 0, 0);
					vmenuitem(&olh, "", 0, 0);
					vmenuitem(&olh, "Sorry, help is not currently available for", 0, 0);
					vmenuitem(&olh, "this field.  Please see your administrator", 0, 0);
					vmenuitem(&olh, "to have it added.  Thank you.", 0, 0);
					vmenuitem(&olh, "", 0, 0);
					vmenuitem(&olh, "Use the arrow keys to move this window", 0, 0);
					vmenuitem(&olh, "", 0, 0);
					vmenuitem(&olh, "Depress any other key to continue...", 0, 0);
				}

				vdl_lin = row_current - 1;				/* Set where we want the cursor.	*/
				vdl_col = col_current - 1;

				if (hl_rtn != 2) k = vmenugo(&olh);

				if ((ol_doc) && (k == help_key) && !hl_rtn)
				{
					curspos.sr = row_current;
					curspos.sc = col_current;
					wpushscr();
					SFREU041(&curspos,odptr,hl_txt,&hl_cnt,&hl_rtn);
					wpopscr();
				}
			}

			else
			{
				k = vml(row_current-1);
				filehelp(k+1,ws_sof(k,col_current-1)+1);
			}

			break;
		}

		case CALC_CODE:		{ gcalc(); break; }				/* Does the user want a goodie?		*/
		case CALEND_CODE:	{ gcalend(); break; }
		case CLOCK_CODE:	{ gclock(); break; }
		case NOTE_CODE:		{ gnotepad(); break; }
		case PUZZLE_CODE:	{ gpuzzle(); break; }
		case ZONE_CODE:		{ gzones(); break; }
		case CUT_CODE:		{ ws_cut(row_current-1,col_current-1); break; }
		case PASTE_CODE:	{ ws_paste(row_current-1,col_current-1,vr,vc,ak,ar,nm,dp); break; }
		case WANG_CODE:
		{
			help_active = FALSE;						/* Turn off the active help.		*/
			ws_help(curset);						/* Go to environment processing.	*/
			break;
		}
	}

	voptimize(op_save);								/* Put the optimization back as it was.	*/
	help_active = FALSE;								/* Now help is inactive.		*/
	synch_required = FALSE;								/* A synch is not required.		*/
	return(SUCCESS); 								/* All done.				*/
}

A_WSLINK(ets_link_program_definition, on_line_doc_passing_values)
	unsigned char *ets_link_program_definition, *on_line_doc_passing_values;
{
	elptr = ets_link_program_definition;						/* Remember where the link params are.	*/
	odptr = on_line_doc_passing_values;						/* Remember where the doc values are.	*/
	snptr = odptr + 159;								/* Screen number is 159 bytes in.	*/
	ol_doc = TRUE;									/* On-line documentation is now on.	*/
}

/*
	openhelpmap:	This routine opens the HELPMAP file.
			The first time in it constructs the native path to HELPMAP.

				VMS	WISP$CONFIG:HELPMAP.DAT
				unix	$(WISPCONFIG)/HELPMAP
				MSDOS	$(WISPCONFIG)\\HELPMAP.DAT
*/
static FILE *openhelpmap(type)
char *type;
{
	FILE 	*fd;
	static int first = 1;
	static char helpmapbuff[80];
	char	*ptr;
	char	prefix[80];

	if (first)
	{
		first = 0;
		helpmap = helpmapbuff;
#ifdef VMS
		strcpy( helpmapbuff, "WISP$CONFIG:HELPMAP.DAT" );
#else /* NOT VMS */
		helpmap[0] = '\0';
		if ( ptr = (char *)getenv( "WISPCONFIG" ) )
		{
			strcpy(helpmap,ptr);
#ifdef unix
			strcat(helpmap,"/");
#endif /* unix */
#ifdef MSDOS
			strcat(helpmap,"\\");
#endif /* MSDOS */
		}
		strcat(helpmap,"HELPMAP");
#ifdef MSDOS
		strcat(helpmap,".DAT");
#endif /* MSDOS */
#endif /* NOT VMS */
	}
	
	fd = fopen(helpmap,type); 							/* Open the help file.			*/

	return(fd);
}

static int filehelp(r,c) int r,c;							/* Give help using the help files.	*/
{
	char temp[256];									/* Working string.			*/
	FILE *hf, *fopen();								/* File stuff.				*/
	register int i, j, k, m;
	int sr, sc, ec;
	char sub_progname[133], sub_screen[133];					/* Substitution strings.		*/

	if ((hf = openhelpmap("r")) == NULL) 						/* Open the help file.			*/
	{
		vre_window("Help-F-File helpmap.dat not found or not accessable (privs).");
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
			for (i = 0; i <= ec-sc; i++) sub_progname[i] = wscharat(sr-1,sc-1+i);	/* Get the sub prog name.	*/
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
			for (i = 0; i <= ec-sc; i++) sub_screen[i] = wscharat(sr-1,sc-1+i);	/* Get the sub prog name.	*/
			sub_screen[i] = CHAR_NULL;					/* Null terminate.			*/
			trim(sub_screen);
		}
	}
	fclose(hf);									/* Close for later.			*/

	if (wisp_progname[0] != CHAR_NULL) strcpy(sub_progname,wisp_progname);		/* Copy in the program name.		*/
	if (wisp_screen[0] != CHAR_NULL) strcpy(sub_screen,wisp_screen);		/* Copy in the screen name.		*/

	if (ws_mod(vml(r-1),c-1))							/* Is cursor in a mod field?		*/
	{
		sprintf( (char *) temp, "%s_%s_%d_%d", sub_progname, sub_screen, r, c);
		if (disphelp(temp)) return(SUCCESS);
		sprintf( (char *) temp, "%s_%s_%d_%d", sub_progname, sub_screen, r, 0);
		if (disphelp(temp)) return(SUCCESS);
		sprintf( (char *) temp, "%s_%s_%d_%d", sub_progname, sub_screen, 0, c);
		if (disphelp(temp)) return(SUCCESS);
	}
	if (disphelp("GENERAL_HELP")) return(SUCCESS);
	sprintf( (char *) temp, "%s_%s", sub_progname, sub_screen);
	if (disphelp(temp)) return(SUCCESS);
	sprintf( (char *) temp, "%s", sub_progname);
	if (disphelp(temp)) return(SUCCESS);
	vre_window("No more help available. ID: %s_%s_%d_%d", sub_progname, sub_screen, r, c);
	return(SUCCESS);
}

static int disphelp(help_descriptor) char *help_descriptor;				/* Display the help from a file.	*/
{
	FILE *hf, *hm, *fopen();							/* Open a file.				*/
	struct video_menu help_window;							/* Help window structure.		*/
	register int i, j, k;								/* Working registers.			*/
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
		vre_window("Help-F-File %s not found or not accessable (privs).",&temp[k]);
		return(FAILURE);
	}

	vmenuinit(&help_window, DISPLAY_ONLY_MENU, REVERSE, 0, 0, 0);			/* Init the help window.		*/

	i = 0;										/* Now read the file.			*/
	while ((fgets(temp,132,hf) != NULL) && (i < 14))				/* Read the file.			*/
	{
		trim(temp);								/* Trim the string.			*/
		vmenuitem(&help_window, temp, 0, NULL);					/* Put it in the menu.			*/
	}
	fclose(hf);									/* Close the file.			*/
	vmenuitem(&help_window,"",0,NULL);						/* Final indicator.			*/
	vmenuitem(&help_window,"Depress HELP for more info, any other key to continue...",0,NULL);
	i = vmenugo(&help_window);							/* Display the help.			*/
	if (i == help_key) return(FAILURE);						/* More help is requested.		*/
	return(SUCCESS);								/* Return to the caller.		*/
}

static int first_diff(s1,s2) char *s1, *s2;						/* Find the 1st difference in a string.	*/
{
	register int i,j;

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

static int ws_cut(r,c) int r,c;								/* Cut the current field.		*/
{
	register int i, s, e, k;							/* Working registers.			*/
	unsigned char cutbuf[WS_MAX_COLUMNS_ON_A_LINE];					/* Working buffer.			*/

	k = vml(r);									/* Get the scroll index.		*/

	if ((s = ws_sof(k,c)) && (e = ws_eof(k,c)))					/* Are we in a field?			*/
	{
		for (i = s; i <= e; i++)						/* Copy the field.			*/
		{
			if ((cutbuf[i-s] = vchr_map[k][i]) < ' ') cutbuf[i-s] = ' ';	/* Copy a character.			*/
		}
		cutbuf[i-s] = CHAR_NULL;						/* Terminate the string.		*/
		vcut(cutbuf);								/* Output the result.			*/
	}
	else vbell();									/* Oops, not in field so ring the bell.	*/
	return(SUCCESS);								/* Return.				*/
}

static int ws_paste(r,c,vr,vc,ak,ar,nm,dp)						/* Cut the current field.		*/
int r,c,vr,vc,ak,ar,dp;									/* vcur_lin,vcur_col,alt_read,do_pseudo	*/
unsigned	char	*nm;								/* no_mod				*/
{
	register int s, e, k;								/* Working registers.			*/

	k = vml(r);									/* Get the scroll index.		*/
	if (ws_mod(k,c))								/* Are we in a modifyable field?	*/
	{
		s = ws_sof(k,c);
		e = ws_eof(k,c);							/* Are we in a field?			*/
		vdefer(RESTORE);							/* Restore from deferred actions.	*/
		k = voptimize(TRACKING_ONLY);						/* Turn off optimization for vwang.	*/
		ws_clear(vr,vc,ak,ar,nm,dp);						/* Clear the field.			*/
		voptimize(k);								/* Restore the optimization.		*/
		vpaste(e-s+1);								/* Paste only the allowed fields.	*/
		return(SUCCESS);							/* Return.				*/
	}
	else
	{
		vbell();								/* Not modifyable so just ring bell.	*/
		return(FAILURE);							/* Return to the caller.		*/
	}
}

