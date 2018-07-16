			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/


/*						Include required header files.							*/

#include <stdio.h>									/* Include Standard I/O header.		*/
#include <string.h>

#ifdef VMS
#include <descrip.h>									/* Include descriptor layout for VMS.	*/
#include <ssdef.h>									/* Include VMS system messages.		*/
#endif

#ifndef unix	/* VMS or MSDOS */
#include <stdlib.h>
#endif

#ifndef VMS	/* unix or MSDOS */
#include <malloc.h>
#endif

#include <v/video.h>
#include <v/vlocal.h>
#include <v/vdata.h>

#include <ctype.h>									/* Get character type macros.		*/
#include <math.h>									/* Get math macros.			*/


#include "cobrun.h"
#include "sub_char.h"									/* Include substitution table.		*/
#include "wperson.h"									/* Include struct of personality file.	*/
#include "wscrn.h"									/* Include the struct def to hold saved	*/
#include "werrlog.h"									/* Include error logging definitions.	*/
#include "vwang.h"									/* Include wisp/video interface defs.	*/
#include "wglobals.h"									/* Include wisp globals.		*/
#include "scnfacs.h"									/* Include the screen FAC definitions.	*/


#define TAB_NORMAL	0
#define TAB_SPACEBAR	1
#define TAB_NEWLINE	2

/*						Static and Global Data Definitions.						*/

static unsigned char data_map[WS_MAX_LINES_PER_SCREEN * WS_MAX_COLUMNS_ON_A_LINE];	/* What Wang thinks is on the screen.	*/
static unsigned char attr_map[WS_MAX_LINES_PER_SCREEN * WS_MAX_COLUMNS_ON_A_LINE];	/* Wang attributes for each char pos.	*/
static int ws_good = TRUE;								/* Assume first character is good.	*/
static int do_console_log = FALSE;							/* Set console logging to FALSE.	*/
static int old_line = 0;								/* Old (stored) cursor position.	*/
static int old_column = 0;
static int errset = FALSE;								/* Flag that an error bit was set.	*/
static int kb_locked = TRUE;								/* Flag that the keyboard is locked.	*/
static int fast_read = FALSE;								/* Flag controls fast reads (no data).	*/
int help_active = FALSE;								/* Flag to avoid reentrant help.	*/
int netc_pfhelp = FALSE;								/* Flag to avoid reentrant help.	*/
static int sub_table_loaded = FALSE;							/* Substitution table not loaded.	*/
static int terminal_error = FALSE;							/* Flag there is some kind of error.	*/
static unsigned char lbuf[WS_MAX_COLUMNS_ON_A_LINE+2];					/* Line accumulator buffer.		*/
static int lbcount, lbdangle, lbattr;							/* Line accumulator char counters.	*/
static struct SUB_CHAR_TABLE_DEF stable[128];						/* Global storage for the sub. table.	*/
static int menu_fl = FALSE;								/* Flag to depict if at a menu item.	*/
static int cur_on_fl = FALSE;								/* Flag to depict state of cursor.	*/
static int auto_tab;									/* To auto-tab or not to auto-tab.	*/
static int auto_move;									/* To auto-move or not to auto-move.	*/
static int mp_cursor;									/* To display or not cursor on menu pick*/
static int bgchange;									/* To change or not background color	*/
static int bgcolor;									/* Flag to depict curr. background color*/

extern int netroncap;									/* Flag for NetronCap screens.		*/

char wsx_buf = 0;									/* Used by wsxio.			*/
int rts_first = TRUE;									/* First time flag.			*/
int psb_rndt;										/* Rendition for pseudo blanks.		*/
int psb_chset;										/* Character set choice of pseudo blank.*/
int wcurwidth = WS_DEFAULT_COLUMNS_PER_LINE;						/* Current screen width.		*/
unsigned char psb_char;									/* Pseudo blank character.		*/
struct wscrn_struct *wscrn_stack = 0;							/* Global storage for the addr of the	*/
											/* next 'Wisp' screen data structure.	*/

#define	ROUTINE		67000



/*
	vwang		Emulate the wang workstation.

			Note: On a close only "Function" is passed.
*/

int vwang(function, wsb, lines, terminate_list, term, no_mod)
unsigned char *function, *lines, *wsb, *terminate_list, *term, *no_mod;
{
	int 	op_save;								/* Save current level of optimization.	*/
	char	psb_select;
	int	read_done;
	int	i;

	i = (int)*function;
	werrlog(ERRORCODE(1),i,0,0,0,0,0,0,0);

	vgeterr();									/* Clear any pre-existing video error	*/

#ifdef unix
	if (mf_cobol || aix_cobol) 	/* insure stty is properly setup in case animator has changed it */
	{
		vraw_stty_set();
	}
#endif

	if (rts_first) 									/* First time in program?		*/
	{
	 	if (init_screen())							/* Initialize the screen.		*/
		{
			terminal_error = FALSE;						/* Initilization ok so no term error.	*/
			if (wcurwidth != WS_DEFAULT_COLUMNS_PER_LINE)			/* Are we using the default width?	*/
			{
				if (wcurwidth == 80)  vscreen(NARROW);			/* Are we now in 80 column mode?	*/
				else vscreen(WIDE);					/* No, then select wide screen.		*/
				synch_required = TRUE;					/* Now re-synchronize.			*/
			}
		}
		else terminal_error = TRUE;
		set_cl_fl();								/* Set the console logging flag.	*/
	}
	else if (check_scrn() == 0) terminal_error = TRUE;				/* Catch errors.			*/
	else if (synch_required) ws_erap(FULL_SCREEN);					/* Somebody did something behind us.	*/

	op_save = voptimize(TRACKING_ONLY);						/* Turn optimization off (do it here).	*/

	psb_rndt = defaults.psb_rendition;						/* Init the pseudo blank rendition.	*/
	psb_chset = defaults.psb_charset;						/* Init the pseudo blank char set.	*/
	get_psb_char(defaults.psb_select,&psb_char,&psb_select);			/* Init the pseudo blank charcter.	*/
	auto_tab = defaults.autotab;							/* Init the auto tab flag.		*/
	auto_move = defaults.automove;							/* Init the auto move flag.		*/
	mp_cursor = defaults.mp_cursor;							/* Init the menu pick cursor flag.	*/
	bgchange = defaults.bgchange;							/* Init the background change flag.	*/
	bgcolor = defaults.bgcolor;							/* Init the background color.		*/

	fast_read = FALSE;								/* Default read to return data.		*/
	wsx_buf = 0;									/* Clear the keyboard AID byte.		*/
	read_done = 0;									/* A read has not been done.		*/

	switch (*function)								/* What does he want us to do.		*/
	{
		case CLOSE_WORK_STATION:
		{									/* Do not erase the screen in here!!!	*/
			vshut();							/* Cancel pending input only.		*/
			break;
		}
		case WRITE_ALL:								/* Write the full screen.		*/
		{
			if (terminal_error) wsdmp_scrn(wsb,0);				/* Dump screen on errors.		*/
			else ws_write(wsb,*lines,0,no_mod,0);				/* Output the data.			*/
			break;
		}
		case WRITE_ALL_PB:							/* Write the full screen with pseudo	*/
		{									/* blank processing.			*/
			if (terminal_error) wsdmp_scrn(wsb,0);				/* Dump screen on errors.		*/
			else ws_write(wsb,*lines,0,no_mod,1);				/* Output the data.			*/
			break;
		}
		case WRITE_SELECTED:							/* Write selected fields.		*/
		{
			if (terminal_error) wsdmp_scrn(wsb,0);				/* Dump screen on errors.		*/
			else ws_write(wsb,*lines,1,no_mod,0);				/* Output the data.			*/
			break;
		}
		case READ_ALL:								/* Read the screen?			*/
		{
			ws_read(wsb,*lines,READ_ALL,terminate_list,term,no_mod,0,0);	/* Read the workstation.		*/
			read_done = 1;
			break;								/* We're done.				*/
		}
		case READ_ALL_PB:							/* Read the screen with pseudo blank	*/
		{									/* processing.				*/
			ws_read(wsb,*lines,READ_ALL,terminate_list,term,no_mod,1,0);	/* Read the workstation.		*/
			read_done = 1;
			break;								/* We're done.				*/
		}
		case READ_ALTERED:							/* Read altered part of screen?		*/
		{
			ws_read(wsb,*lines,READ_ALTERED,terminate_list,term,no_mod,0,0);/* Read it.				*/
			read_done = 1;
			break;								/* Done...				*/
		}
		case READ_MODIFIABLE:							/* Read MODIFIABLE part of screen?	*/
		{
			ws_read(wsb,*lines,READ_MODIFIABLE,terminate_list,term,no_mod,0,1); /* Read it.				*/
			read_done = 1;
			break;								/* Done...				*/
		}
		case DISPLAY_AND_READ:							/* Write then read screen?		*/
		{
			if (terminal_error) wsdmp_scrn(wsb,0);				/* Dump screen on errors.		*/
			else ws_write(wsb,WS_MAX_LINES_PER_SCREEN,0,no_mod,1);		/* Write full screen.			*/
			if (kb_locked) fast_read = TRUE;				/* Just wrote so don't return data.	*/
			ws_read(wsb,WS_MAX_LINES_PER_SCREEN,READ_ALL,terminate_list,term,no_mod,1,1);
			read_done = 1;
			break;								/* Don't fall through.			*/
		}
		case DISPLAY_AND_READ_ALTERED:						/* Write then read altered fields?	*/
		{
			if (terminal_error) wsdmp_scrn(wsb,0);				/* Dump screen on errors.		*/
			else ws_write(wsb,WS_MAX_LINES_PER_SCREEN,0,no_mod,1);		/* Write full screen.			*/
			if (kb_locked) fast_read = TRUE;				/* Just wrote so don't return data.	*/
			ws_read(wsb,WS_MAX_LINES_PER_SCREEN,READ_ALTERED,terminate_list,term,no_mod,1,1);
			read_done = 1;
			break;
		}
		case INIT_DEFAULT_TABLE:						/* Re-initialize the substitution table	*/
		{									/* to default values.			*/
			load_sub_chars();						/* Do the load of substitution values.	*/
			sub_table_loaded = TRUE;					/* Set the static variable.		*/
			break;
		}
		case LOAD_SUB_TABLE:							/* Set user defined values in sub table.*/
		{
			if (!(sub_table_loaded))					/* Check if default table is loaded.	*/
			{
				load_sub_chars();					/* Do the load of substitution values.	*/
				sub_table_loaded = TRUE;				/* Set the static variable.		*/
			}
			load_user_sub_chars(wsb);					/* Do the load of substitution values.	*/
			break;
		}
		default:								/* Everything else no good.		*/
		{
			werrlog(ERRORCODE(2),*function,0,0,0,0,0,0,0);			/* Invalid function			*/
			voptimize(op_save);						/* Restore optimization level.		*/
			return(FAILURE);						/* Return to the caller.		*/
		}
	}

	voptimize(op_save);								/* Restore optimization level.		*/
	synch_required = FALSE;								/* A synch is not required now.		*/

											/* The following line fixes a problem	*/
											/* with a user calling wsxio() in a	*/
											/* loop checking for a PF Key and then	*/
											/* calling vwang() to verify an action	*/
											/* returning to the wsxio() loop.	*/
	if (read_done) wsx_buf = no_mod[1];						/* Set the keyboard AID byte.		*/

	return(SUCCESS);								/* Return that we were successful.	*/
}



/*					Subroutine to display screen or line.							*/

static int ws_write(wsb,lines,selected,no_mod,do_pseudo)				/* Work station write.			*/
unsigned char *wsb;									/* Work station I/O buffer.		*/
unsigned char lines;									/* Number of lines to write.		*/
	 int selected;									/* Flag indicates selected field write.	*/
unsigned char *no_mod;									/* Pointer to no-modification flag.	*/
int	do_pseudo;									/* Do Pesudo blank processing		*/
{
	extern char vgetc();								/* Reference get character routine.	*/
	int start_row;									/* Row for single line or scrolling.	*/
	int wcc;									/* Write control character.		*/
	int crs_row, crs_col;								/* Cursor position.			*/
	int lin_1st, col_1st;								/* First modifyable field position.	*/
	int lin_err, col_err;								/* First error field location.		*/
	unsigned char last_atr;								/* Last Wang attributes.		*/
	int last_mode;									/* Last VIDEO character rendition.	*/
	unsigned char c;								/* Working character.			*/
	unsigned char xc;								/* Exchanged character value for 'c'.	*/
	unsigned char *dm, *am;								/* Working pointers to character maps.	*/
	int char_set;									/* Character set flag (font) for subst.	*/
	register int vrow;								/* Virtual row number from vml()	*/
	register int row_x, col_x;							/* Row and col index			*/
	int new_row;									/* Is there new data for curr row	*/

	start_row = *wsb++;								/* get the row				*/
	if (start_row == 0) start_row = 1;						/* correct for row == 0			*/

	if ((start_row  < 1) || ((int)(start_row+lines-1) > WS_MAX_LINES_PER_SCREEN))	/* Test for valid values		*/
	{
		werrlog(ERRORCODE(4),start_row,lines,0,0,0,0,0,0);			/* Invalid Row  or Row+lines		*/
		return(FAILURE);
	}
	start_row -= 1;									/* make row zero based			*/

	vbuffering(LOGICAL);								/* Turn on buffering if not on.		*/

	if (bgchange)									/* Should we change the background?	*/
	{
		if (bgcolor && !(vscr_atr & LIGHT)) vscreen(LIGHT);			/* Change the screen color.		*/
		else if (!bgcolor && !(vscr_atr & DARK)) vscreen(DARK);

		if (synch_required) ws_erap(FULL_SCREEN);				/* Somebody did something behind us.	*/
	}

	wcc = *wsb++;									/* Get the write control character.	*/
	if (wcc & UNLOCK_KEYBOARD) kb_locked = FALSE;					/* Get the output options.		*/
	else kb_locked = TRUE;								/* If wcc is zero, lock the keyboard.	*/

	crs_col = *wsb++;								/* Get the cursor column position.	*/
	if ((wcc & POSITION_CURSOR) && (crs_col < 0 || crs_col > wcurwidth))
	{
		werrlog(ERRORCODE(16),crs_col,0,0,0,0,0,0,0);				/* Invalid column in order area.	*/
		return(FAILURE);
	}
	crs_col -= 1;									/* Make zero based			*/

	crs_row = *wsb++;								/* Get the cursor line number.		*/
	if ((wcc & POSITION_CURSOR) && (crs_row < 0 || crs_row > WS_MAX_LINES_PER_SCREEN))
	{
		werrlog(ERRORCODE(18),crs_row,0,0,0,0,0,0,0);				/* Invalid line in order area.		*/
		return(FAILURE);
	}
	crs_row -= 1;									/* make line zerobased			*/

	lin_1st = -1;									/* First mod field not found yet.	*/
	lin_err = -1;									/* No error field found yet.		*/
	errset = FALSE;									/* No errors on the screen yet.		*/
	lbcount = 0;									/* No characters in accumulator yet.	*/
	lbdangle = 0;									/* No dangling spaces in accumulator.	*/
	if (netroncap) do_pseudo = 0;							/* NetronCap screen do not have pseudos.*/

	if ((wcc & ROLL_UP) || (wcc & ROLL_DOWN))					/* Scroll the screen up or down?	*/
	{
		werrlog(ERRORCODE(6),0,0,0,0,0,0,0,0);					/* Scroll not implemented		*/
		return(FAILURE);
	}

	if (wcc & ERASE_AND_PROTECT)							/* Erase and protect from cursor?	*/
	{										/* Do the erase from here.		*/
		if ((start_row+lines) != WS_MAX_LINES_PER_SCREEN)			/* Do we actually need to do it?	*/
		{
			wsmove(start_row,0);						/* Move to the specified location.	*/
			ws_erap(TO_EOS);						/* Erase to the end of the screen.	*/
		}
	}

	for (row_x = 0; row_x < WS_MAX_LINES_PER_SCREEN; row_x++)			/* Loop through each line.		*/
	{
		last_atr = FAC_PROTECT | FAC_LOW_INTENSITY;				/* Reset attributes at start of line.	*/
		last_mode = CLEAR;
		vrow = vml(row_x);							/* Determine map index.			*/
		dm = &data_map[mx(vrow,0)];						/* Point to the current entry.		*/
		am = &attr_map[mx(vrow,0)];

		if ((row_x >= start_row) && (row_x < (int)(start_row+lines)))		/* Is there data for this row?		*/
		{
			new_row = 1;
		}
		else
		{
			new_row = 0;
		}

		for (col_x = 0; col_x < wcurwidth; col_x++)				/* Loop through each column.		*/
		{
			if (new_row)
			{
				c = *wsb++;						/* Get data from input buffer.		*/
			}
			else
			{
				c = *dm;						/* Use the data map			*/
			}
											/* Don't do anything if already in 	*/
											/* map unless modified pseudo blank.	*/
			if (new_row && ((c != *dm) || (last_atr != *am) || mod_pb(c,dm,last_atr,am,do_pseudo)))
			{
				if (c & FAC_CHARACTER)					/* Is this character a FAC?		*/
				{
					if (*no_mod != 'E') c = c & ~FAC_ALTERED;	/* Clear any altered bits that are set.	*/
					if (c != *dm)					/* Different only by altered bits?	*/
					{
						ws_putbuff(' ',row_x,col_x,vrow,CLEAR,DEFAULT);/* Put in output buffer.		*/
						*dm = c;				/* Record the data.			*/
						*am = c;				/* Record the attributes.		*/
					}
					last_atr = c & ~FAC_CHARACTER;			/* Remember the other attributes.	*/
					last_mode = ws_trans_atr(c);			/* Remember the last rendition.		*/
				}
				else if (!selected || (last_atr & FAC_SELECTED))	/* Not FAC character, write if approp.	*/
				{
					*am = last_atr;					/* Record attributes.			*/
					*dm = c;					/* Record the character.		*/

					if (ws_invisible(last_atr))			/* Should this field be invisible?	*/
					{
						ws_putbuff(' ',row_x,col_x,vrow,CLEAR,DEFAULT);
					}
					else if ( do_pseudo && !(last_atr & FAC_PROTECT) && ((wcc & ERASE_FIELDS) || (c == ' ')))
					{
						ws_putbuff(psb_char,row_x,col_x,vrow,psb_rndt|last_mode,psb_chset);
						*dm = ' ';				/* Remember what char really is.	*/
					}
					else if ( do_pseudo && !(last_atr & FAC_PROTECT) && 
										(last_atr & FAC_NUMERIC_ONLY) && (c == ' '))
					{
						ws_putbuff(psb_char,row_x,col_x,vrow,psb_rndt|last_mode,psb_chset);
						*dm = ' ';				/* Remember what char is.		*/
					}
					else if (c == 0)				/* Is it a null?			*/
					{
						if (opt_nulldisplay) 	ws_putbuff('.',row_x,col_x,vrow,last_mode,DEFAULT);
						else 			ws_putbuff(' ',row_x,col_x,vrow,last_mode,DEFAULT); 
					}
					else if (c == PSEUDO_BLANK_CHAR )		/* 0x0B - Wang solid box - pseudo blank.*/
					{
						ws_putbuff(psb_char,row_x,col_x,vrow,psb_rndt|last_mode,psb_chset);
					}
					else
					{
						sub_char(c, &xc, &char_set); 		/* Get character & font from sub table.	*/
						if (!(last_atr & FAC_PROTECT))		/* Is this a modifiable field ?		*/
						{
							int mode;
							if (do_pseudo)  mode = psb_rndt|last_mode;
							else		mode = last_mode;
							ws_putbuff(xc,row_x,col_x,vrow,mode,char_set); /* Put char.		*/
						}
						else if ((xc != ' ') || (last_mode & (REVERSE|UNDERSCORE)))	/* Visible?	*/
						{
							int mode;

							if (xc >= ' ' || !do_pseudo)
								mode = last_mode; 	/* If in the printable range.		*/
							else	mode = psb_rndt|last_mode;
							ws_putbuff(xc,row_x,col_x,vrow,mode,char_set);
						}
						else					/* Not visible. Remove the blink.	*/
						{
							ws_putbuff(' ',row_x,col_x,vrow,(last_mode & ~BLINK),DEFAULT);
						}
					}
				}
			}
			else if (new_row && lbdangle)						/* Are we dangling output?	*/
			{
				if (!((*dm == ' ') || (*dm & FAC_CHARACTER))) lbdangle = 0;	/* Don't dangle if visible.	*/
				else if (last_mode & (REVERSE|UNDERSCORE|BLINK)) lbdangle = 0;	/* Visible whitespace too...	*/
			}

			if ((!(*am & FAC_CHARACTER)) && (lin_1st < 0) && (row_x >= crs_row) && ws_tab_stop(last_atr))
												/* 1st tabstop?			*/
			{
				lin_1st = row_x;						/* Yes, remember it.		*/
				col_1st = col_x;						/* Column too.			*/
			}

			if ((*am & FAC_CHARACTER) && (lin_err < 0) && ws_err_field(*am))    /* Is there an error in this field?	*/
			{
				errset = ON;						/* Yes so flag that there is errors.	*/
				lin_err = row_x;					/* Record where 1st error field is.	*/
				col_err = col_x + 1;
			}

			dm++;								/* Incrememt the buffer pointers.	*/
			am++;
		}
		ws_dumpbuff(vrow,TRUE);							/* Dump the accumulated buffer.		*/
	}
	wsmode(CLEAR);									/* Clear out the renditions.		*/
	wscset(DEFAULT);

	if (wcc & POSITION_CURSOR)									/* Leave cursor alone?	*/
	{
		if ((crs_row >= 0) && (crs_col >=0)) wsmove(crs_row,crs_col);				/* Did caller specify?	*/
		else if ((crs_row == -1) && (errset)) wsmove(lin_err,col_err);				/* An error field?	*/
		else if ((crs_col == -1) && (lin_1st >= 0)) wsmove(lin_1st,col_1st);			/* Move to 1st field.	*/
		else if ((crs_col >= 0) && (crs_row == -1) && (lin_1st >= 0)) wsmove(lin_1st,col_1st);	/* Ditto.		*/
		else wsmove(0,0);									/* Else go home.	*/
		old_line = vcur_lin;							/* Remember final position if moved.	*/
		old_column = vcur_col;
	}

	if (wcc & SOUND_ALARM) vbell();							/* Did he want the bell?		*/
	vbuffering(AUTOMATIC);								/* Dump the buffer to show the screen.	*/
	return(SUCCESS);								/* Return to the caller.		*/
}



/*					Subroutine to read data.								*/

static int ws_read(wsb,lines,read_mode,terminate_list,pfkey,no_mod,do_pseudo,pseudo2space) /* Read the screen.			*/
	unsigned char *wsb, lines;
	int read_mode;
	unsigned char *terminate_list, *pfkey, *no_mod;
	int	do_pseudo;
	int	pseudo2space;								/* Convert pseudo blanks to spaces	*/
{
	extern char vgetc();								/* Reference get character routine.	*/
	unsigned char *wsbr;								/* Pointer to position return area.	*/
	unsigned char *wsbo;								/* Pointer to original screen from Wang.*/
	int 	filling;								/* Flag to indicate screen is filling.	*/
	int 	row;									/* Row to start reading.		*/
	int 	at_edge;								/* Flag to indicate at edge of screen.	*/
	register int i,j,k,m;								/* Working registers.			*/
	unsigned char c, *dwsb;								/* Working character.			*/
	int 	tab_type;								/* Indicate space bar, tab key, newline.*/
	int 	alt_read;								/* Indicate this is an altered read.	*/
	int	v_error;								/* Error number from video		*/

	if (read_mode == READ_ALTERED) alt_read = 1;					/* Set/clear altered flag.		*/
	else			       alt_read = 0;

	wsbo = wsb;									/* Set ptr to original screen from Wang.*/
	dwsb = wsb + 4;									/* Set pointer to dump screen.		*/

	row = *wsb++;									/* Get the row				*/
	if ( row == 0 ) row = 1;							/* Correct for row = 0			*/
	if ((row < 1) || ((int)(row + lines - 1) > WS_MAX_LINES_PER_SCREEN))		/* Validate values.			*/
	{
		werrlog(ERRORCODE(8),row,lines,0,0,0,0,0,0);				/* Invalid Row  or Row+lines		*/
		return(FAILURE);
	}
	row -= 1;									/* Make row zero based			*/
	wsbr = wsb + 1;									/* Remember where column position is.	*/
	wsb = wsb + 3;									/* Move to the data area.		*/
	if (alt_read == 1) *no_mod = 'N';						/* Assume no modifications.		*/
	menu_fl = check_mp(old_line,old_column,BOLD,do_pseudo);				/* If a menu pick then BOLD it.		*/
	if (mp_cursor)									/* If usage constant is set to display.	*/
	{
		vset(CURSOR,VISIBLE);							/* Set cursor on.			*/
		cur_on_fl = TRUE;
	}
	filling = TRUE;									/* And now to fill the screen.		*/
	if (!kb_locked)									/* Don't get input if keyboard locked.	*/
	{
		while (filling)								/* Repeat until an exit event.		*/
		{
			if (!mp_cursor)							/* If no cursor on menu, check status.	*/
			{
				if (!menu_fl) 						/* If not at menu pick and cursor is 	*/
				{							/* off.					*/
					if (!cur_on_fl)
					{
						vset(CURSOR,VISIBLE);			/* Turn the cursor on.			*/
						cur_on_fl = TRUE;			/* Cursor is on so set flag TRUE.	*/
					}
				}
				else if (cur_on_fl)					/* else make sure it is off.		*/
				{
					vset(CURSOR,INVISIBLE);				/* Turn the cursor off.			*/
					cur_on_fl = FALSE;				/* Cursor is off so set flag FALSE.	*/
				}
			}
			k = vml(vcur_lin);						/* Get table index.			*/
			ws_good = TRUE;							/* Assume character will be good.	*/

			i = vgetm();							/* Get a meta character.		*/

			if (v_error = vgeterr())					/* Check if an error occured		*/
			{
				werrlog(ERRORCODE(24),v_error,0,0,0,0,0,0,0);		/* Report error on video read		*/
				wexit(ERRORCODE(24));					/* Exit the process			*/
			}

			if (menu_fl && i == SPACE_BAR)					/* If at a menu pick and hit space bar.	*/
			{
				i = tab_key;						/* Change SPACEBAR into TABKEY		*/
				tab_type = TAB_SPACEBAR;				/* Hit the space bar from a menu pick.	*/
			}
			else tab_type = TAB_NORMAL;					/* Set to normal tab actions.		*/

			if (i == 0)							/* A timeout?				*/
			{
				memcpy(pfkey,"FF",2);					/* Set pfkey to FF.			*/
				no_mod[1] = 0xff;					/* Set AID to ff.			*/
				filling = FALSE;					/* Done filling.			*/
			}
			else if (menu_fl && i >= SPACE_BAR && i < MAX_DISP_RANGE)	/* At menu pick and i is displayable.	*/
			{
				next_mp(vcur_lin,vcur_col,i,do_pseudo);			/* Search next menu pick beg. with i.*/
			}
			else if (auto_move && (i >= SPACE_BAR) && (i < MAX_DISP_RANGE) && !ws_mod(k,vcur_col))
			{
				ws_next(vcur_lin,vcur_col,TAB_NORMAL,do_pseudo);
			}
			else if (((i >= SPACE_BAR) && (i < MAX_DISP_RANGE)) && ws_mod(k,vcur_col)) 
			{								/* Displayable char in mod pos?		*/
				at_edge = FALSE;					/* Not at edge.				*/
				if (attr_map[mx(k,vcur_col)] & FAC_NUMERIC_ONLY)	/* Digits only?				*/
				{
					if (isdigit(i) || (i == '.') || (i == '+') || (i == '-') || (i == ' '))
					{
						m = vcur_col;							/* Remember.	*/
						ws_echo(i,k,vcur_col,alt_read,no_mod,do_pseudo);		/* Echo digit.	*/
						if ((vcur_col == vscr_wid-1) && (vcur_col == m)) at_edge=TRUE;	/* At edge?	*/
					}
					else ws_bad_char();							/* Bells toll.	*/
				}
				else										/* Ok insert.	*/
				{
					if (attr_map[mx(k,vcur_col)] & FAC_UPPERCASE_ONLY) i = toupper(i);	/* Uppercase?	*/
					m = vcur_col;								/* Remember col	*/
					ws_echo(i,k,vcur_col,alt_read,no_mod,do_pseudo);			/* Yes, echo.	*/
					if ((vcur_col == vscr_wid-1) && (vcur_col == m)) at_edge = TRUE;	/* At edge?	*/
				}
				if (at_edge || (attr_map[mx(k,vcur_col)] & FAC_CHARACTER))			/* Full?	*/
				{
					if (auto_tab) ws_next(vcur_lin,vcur_col,TAB_NORMAL,do_pseudo);		/* Yes,autotab.	*/
					else if (!at_edge) vslew(0,-1);						/* No, stay.	*/
				}
			}
			else if (i == newline_key)						/* Hi the Wang EXECUTE key.	*/
			{
				ws_next(vcur_lin,vcur_col,TAB_NEWLINE,do_pseudo);
			}
			else if ((i == return_key) || (i == enter_key))	ws_fkey(0,&filling,terminate_list,pfkey,no_mod);
			else if (vfnkey(i)) 		ws_fkey(i,&filling,terminate_list,pfkey,no_mod);
			else if (i == up_arrow_key)	ws_posit(-1, 0,do_pseudo);
			else if (i == down_arrow_key)	ws_posit( 1, 0,do_pseudo);
			else if (i == left_arrow_key)	ws_posit( 0,-1,do_pseudo);
			else if (i == right_arrow_key)	ws_posit( 0, 1,do_pseudo);
			else if (i == tab_key)		ws_next(vcur_lin,vcur_col,tab_type,do_pseudo);
			else if (i == backtab_key)	ws_last(vcur_lin,vcur_col,do_pseudo);
			else if (i == help_key)
			{
				if (help_active)							/* Is help active?	*/
				{
					strcpy((char *)pfkey,(char *)"FE");				/* Return code FE.	*/
					no_mod[1] = 0xfe;						/* Mode code too.	*/
					filling = FALSE;						/* Now we're done.	*/
				}
				else ws_bar_menu(ON,vcur_lin,vcur_col,k,alt_read,no_mod,do_pseudo);
			}
			else if (i == clear_field_key)	ws_clear(vcur_lin,vcur_col,k,alt_read,no_mod,do_pseudo);
			else if (i == clear_before_key)	ws_deleft(vcur_lin,vcur_col,k,alt_read,no_mod,do_pseudo);
			else if (i == clear_after_key)	ws_delrt(vcur_lin,vcur_col,k,alt_read,no_mod,do_pseudo);
			else if (i == home_key)	  ws_next(0,0,TAB_NORMAL,do_pseudo);
			else if (i == delete_key) ws_delete(k,vcur_col,alt_read,no_mod,do_pseudo); 	    /* delete b4 cursor	*/
			else if (i == insert_key) ws_insert(vcur_lin,vcur_col,k,alt_read,no_mod,do_pseudo); /* open-under-cursor*/
			else if (i == remove_key) ws_remove(vcur_lin,vcur_col,k,alt_read,no_mod,do_pseudo); /* delete & closeup */
			else if ((i == cancel_key) && errset)
			{
				ws_bloff(do_pseudo);					/* Go eliminate blinking fields.	*/
				errset = FALSE;						/* Error fields are now gone.		*/
			}
			else if (netroncap && i == trigger1)
			{
				nc_pop_menu(&filling,terminate_list,no_mod,pfkey); 	/* Pop up window and get pfkey.		*/
			}
			else ws_bad_char();						/* Else beep.				*/
		}
	}

	if (!fast_read)									/* Now read the block back.		*/
	{
		for (i = row; i < (int)(row+lines); i++)				/* Loop through each line.		*/
		{
			k = vml(i);							/* Determine map index.			*/
			for (j = 0; j < wcurwidth; j++)					/* Loop through each column.		*/
			{
				c = data_map[mx(k,j)];					/* Get this character from the map.	*/
				if ((c & FAC_CHARACTER) && (!ws_invisible(c)) & (!(c & FAC_PROTECT)))	/* Mod visible FAC?	*/
				{
					*wsb++ = c & ~FAC_BLINKING;			/* Yes, then clear blinking.		*/
				}
				else if (c == 0x0b && pseudo2space)			/* Do we change pseudo to space ?	*/ 
				{
					*wsb++ = ' ';					/* Change PB to Spaces.			*/
				}
				else *wsb++ = c;					/* Else return the character.		*/
			}
		}
	}
	old_line = vcur_lin;								/* Remember where we ended.		*/
	old_column = vcur_col;
	check_mp(old_line,old_column,CLEAR,do_pseudo);					/* If menu pick, clear before return.	*/
	*wsbr++ = vcur_col + 1;								/* Return ending position to caller.	*/
	*wsbr = vcur_lin + 1;
	vset(CURSOR,INVISIBLE);								/* Turn the cursor off again.		*/
	cur_on_fl = FALSE;								/* Set the flag.			*/
	wsmode(CLEAR);									/* Clear the mode and char set.		*/
	wscset(DEFAULT);

	if (no_mod[1] != 0xff)								/* If we are not timed out.		*/
	{
		kb_locked = TRUE;							/* Always leave the keyboard locked.	*/
		if (do_console_log) wsdmp_scrn(dwsb,1);					/* If console logging is on.		*/
	}

	return(SUCCESS);								/* Return to the caller.		*/
}




static int mod_pb(c,dm,last_atr,am,do_pseudo)						/* Is it a modified pseudo blank?	*/
unsigned char *dm, *am;									/* Working pointers to character maps.	*/
unsigned char c, last_atr;
int do_pseudo;
{
	if (netroncap || !do_pseudo || c != ' ') return(FALSE);				/* Only test if a DISPLAY AND READ,	*/
											/* or not a space. FALSE.		*/
	if (!(last_atr & FAC_PROTECT)) return(TRUE);					/* Is this a modifiable field ?		*/
	else return(FALSE);
}											/* pseudo blank.			*/

static ws_posit(up,right,do_pseudo) 							/* Position cursor.			*/
int up, right;
int	do_pseudo;
{
	register int i,j;								/* Working registers.			*/

	i = vcur_lin + up;								/* Take temp copy of position.		*/
	if (i < 0) i = WS_MAX_LINES_PER_SCREEN - 1;					/* Wrapped up?				*/
	else if (i >= WS_MAX_LINES_PER_SCREEN) i = 0;					/* Wrapped down?			*/

	j = vcur_col + right;								/* Compute new column.			*/
	if (j < 0) j = wcurwidth - 1;							/* Wrapped left?			*/
	else if (j >= wcurwidth) j = 0;							/* Wrapped right?			*/

	check_mp(vcur_lin,vcur_col,CLEAR,do_pseudo);					/* Unbold current menu pick item.	*/
	menu_fl = check_mp(i,j,BOLD,do_pseudo);						/* Check if is a menu pick item.	*/
	return(SUCCESS);								/* Return to the caller.		*/
}

static int ws_invisible(ch) unsigned char ch;						/* Determine if field is displayable.	*/
{
	register int i;									/* Working register.			*/

	i = FALSE;									/* Assume visible.			*/
	if((ch & FAC_LOW_INTENSITY) && (ch & FAC_BLINKING)) i = TRUE;			/* Not visible if both low and blink.	*/
	return(i);									/* Return the result.			*/
}

static int ws_err_field(ch) unsigned char ch;						/* Determine if field has an error.	*/
{
	register int i;									/* Working register.			*/

	i = FALSE;									/* Assume not an error field.		*/
	if (!ws_invisible(ch) && (ch & FAC_BLINKING) && (!(ch & FAC_PROTECT)))		/* Is it an error field?		*/
	{
		i = TRUE;								/* Yes, so flag as true.		*/
	}
	return(i);									/* Return the result.			*/
}

static ws_tag_alt(alt_read,k,j,no_mod) int alt_read,k,j; unsigned char *no_mod;		/* Set flags on altered fields.		*/
{
	if (alt_read)
	{
		while (!(data_map[mx(k,j)] & FAC_CHARACTER))				/* Loop to find the start FAC.		*/
		{
			if ((--j) < 0)							/* Move left one column.		*/
			{
				werrlog(ERRORCODE(10),0,0,0,0,0,0,0,0);			/* No FAC for current field		*/
				return(FAILURE);
			}
		}
		data_map[mx(k,j)] = data_map[mx(k,j)] | FAC_ALTERED;			/* Or in the fact that it is altered.	*/
		*no_mod = 'M';								/* Return the global modified tag.	*/
	}
	return(SUCCESS);								/* Oilswell.				*/
}

int ws_fkey(metachar_key,filling,terminate_list,pfkey,no_mod) 
int 	metachar_key, *filling; 
unsigned char *terminate_list, *pfkey, *no_mod;
{
	char temp[3];									/* Temporary working string.		*/
	register int j;									/* Working register.			*/
	int 	fn_num;									/* function key number			*/
	int	tl_num;
	int	cnt;									/* Counter				*/
	int	invalid_list;								/* Flag if term_list in invalid		*/

	invalid_list = 0;
	sprintf(temp,"%02d", vfnkey(metachar_key));					/* Convert to ascii representation.	*/
	
	if (terminate_list[0] == 'A') *filling = FALSE;					/* 'A' means all keys allowed.		*/

	j = 0;										/* Loop from start of terminate_list.	*/
	cnt = 0;

	while (*filling)								/* Loop until end of list or valid key.	*/
	{
		if (toupper(terminate_list[j]) == 'X')					/* End of list				*/
		{
			break;
		}

		if ( terminate_list[j] < '0' || terminate_list[j] > '3' ||		/* valid values are "00" - "32"		*/
		     !isdigit(terminate_list[j+1]) )
		{
			invalid_list = 1;
		}
		else
		{
			tl_num = (terminate_list[j] - '0')*10 + terminate_list[j+1] - '0';

			if (tl_num < 0 || tl_num > 32)
			{
				invalid_list = 1;
			}
		}

		if (invalid_list)
		{
			werrlog(ERRORCODE(26),terminate_list[j],terminate_list[j+1],0,0,0,0,0,0);
			terminate_list[0] = 'A';					/* Reconstruct list to allow all	*/
			return(FAILURE);
		}
		
		if ((temp[0] == terminate_list[j]) && (temp[1] == terminate_list[j+1]))
		{
			*filling = FALSE;						/* We're not filling any more.		*/
		}
		else 
		{
			j += 2;
		}

		if ( cnt++ > (33+10))							/* term list is too big (corrupt)	*/
		{
			werrlog(ERRORCODE(28),0,0,0,0,0,0,0,0);
			terminate_list[0] = 'A';					/* Reconstruct list to allow all	*/
			return(FAILURE);
		}
	}

	if (*filling) 
	{
		ws_bad_char();								/* Not useable bell key.		*/
	}
	else
	{
		pfkey[0] = temp[0];							/* Return two characters.		*/
		pfkey[1] = temp[1];
		fn_num = vfnkey(metachar_key);						/* What pfkey did they press ?		*/
		no_mod[1] = fn_num + ((fn_num > 16) ? 80 : 64);				/* Return the correct AID byte value.	*/
	}

	return(SUCCESS);								/* Return to the caller.		*/
}

int ws_mod(vrow,cursor_col) 								/* Determine if char is modifyable.	*/
register int vrow,cursor_col;	
{
	return( !(attr_map[mx(vrow,cursor_col)] & (FAC_PROTECT | FAC_CHARACTER)));	/* Not modifable if protected or a FAC.	*/
}

static int ws_delete(k,j,alt_read,no_mod,do_pseudo) 					/* Delete the previous character.	*/
int k,j,alt_read; 
unsigned char *no_mod;
int	do_pseudo;
{
	if ((j-1 < 0) || !ws_mod(k,j-1)) ws_bad_char();					/* Beep if nothing to delete.		*/
	else
	{
		if (do_pseudo)
		{
			wsmode(psb_rndt | ws_trans_atr(attr_map[mx(k,j-1)]));		/* Select the needed rendition.		*/
			wscset(psb_chset);						/* Set to needed character set.		*/
			ws_posit(0,-1,do_pseudo);					/* Move Left 1 position.		*/
			vputc(psb_char);						/* Output the character.		*/
			ws_posit(0,-1,do_pseudo);					/* Move Left 1 position again.		*/
			wscset(DEFAULT);						/* Set back to normal.			*/
		}
		else
		{
			wsmode(ws_trans_atr(attr_map[mx(k,j-1)]));			/* Select the needed rendition.		*/
			ws_posit(0,-1,do_pseudo);					/* Move Left 1 position.		*/
			vputc(' ');							/* Output the character.		*/
			ws_posit(0,-1,do_pseudo);					/* Move Left 1 position again.		*/
		}
		data_map[mx(k,j-1)] = ' ';						/* Remember what it really is.		*/
		ws_tag_alt(alt_read,k,j,no_mod);					/* Set altered flags.			*/
	}
	return(SUCCESS);								/* Back, back, back...			*/
}

static int ws_deleft(i,j,k,alt_read,no_mod,do_pseudo)					/* Clear field cursor left.	*/
int i, j, k,alt_read; unsigned char *no_mod;
int	do_pseudo;
{
	register int c, d, m, n, jd,mode;						/* Working registers.			*/

	if ((m = ws_sof(k,j-1)) && (n = ws_eof(k,j-1)))					/* Should we delete left?		*/
	{
		wsmove(i,m);								/* Move to the start of the field.	*/
		d = 0;									/* Initizlize map column.		*/
		for (c = m; c <= n; c++)						/* Loop from start of field.		*/
		{
			jd = j+d;
			if (do_pseudo)  mode = psb_rndt | ws_trans_atr(attr_map[mx(k,c)]);
			else		mode = ws_trans_atr(attr_map[mx(k,c)]);
			wsmode(mode);							/* Select the needed rendition.		*/
			if ((jd) > n)
			{
				if (do_pseudo)
				{
					wscset(psb_chset);				/* Set the needed character set.	*/
					vprint("%c",psb_char);				/* Output pseudos if past end of field.	*/
					wscset(DEFAULT);				/* Set back to default character set.	*/
				}
				else
				{
					vprint(" ");
				}
				data_map[mx(k,c)] = ' ';				/* Remember what it really is.		*/
			}
			else
			{
				if (vatr_map[k][jd] & GRAPHICS)  wscset(GRAPHICS);	/* If Graphics bit set, change char set.*/
				vputc(vchr_map[k][jd]);					/* Output the character.		*/
				wscset(DEFAULT);					/* Set back to normal.			*/
				attr_map[mx(k,c)] = attr_map[mx(k,jd)];			/* Remember char for swap back.		*/
				data_map[mx(k,c)] = data_map[mx(k,jd)];			/* Remember char for swap back.		*/
			}
			d++;
		}
		wsmove(i,m);								/* Move to the start of the field.	*/
		ws_tag_alt(alt_read,k,j,no_mod);					/* Set altered flags.			*/
	}
	else ws_bad_char();								/* Can't delete so beep.		*/
}

static int ws_delrt(i,j,k,alt_read,no_mod,do_pseudo) 					/* Delete to the right of the cursor.	*/
int i,j,k,alt_read; unsigned char *no_mod;
int	do_pseudo;
{
	int c, n, mode;									/* Working registers.			*/

	if (n = ws_eof(k,j))								/* Should we clear?			*/
	{
		wsmove(i,j);								/* Move to where to clear from.		*/
		for (c = j; c <= n; c++)						/* Loop from start of field.		*/
		{
			if (do_pseudo)
			{
				wsmode(psb_rndt | ws_trans_atr(attr_map[mx(k,c)]));	/* Select the needed rendition.		*/
				wscset(psb_chset);					/* Set the needed character set.	*/
				vprint("%c",psb_char);					/* Output pseudo blanks.		*/
				wscset(DEFAULT);					/* Set back to default character set.	*/
			}
			else
			{
				wsmode(ws_trans_atr(attr_map[mx(k,c)]));		/* Select the needed rendition.		*/
				vprint(" ");
			}
			data_map[mx(k,c)] = ' ';					/* Remember what it really is.		*/
		}
		wsmove(i,j);								/* Move to the start of the field.	*/
		ws_tag_alt(alt_read,k,j,no_mod);					/* Set altered flags.			*/
	}
	else ws_bad_char();								/* Can't clear so beep.			*/

	return(SUCCESS);
}

int ws_clear(cursor_row,cursor_col,vrow,alt_read,no_mod,do_pseudo) 			/* Delete to the current field		*/
int 	cursor_row, cursor_col;								/* The cursor position			*/
int	vrow;										/* Virtual row from vml()		*/
int	alt_read; 
unsigned char *no_mod;
int	do_pseudo;
{
	int	sof_col;								/* Start of field column		*/
	int	eof_col;								/* End of field column			*/
	int 	wrk_col,  mode;								/* Working registers.			*/

	if ((sof_col = ws_sof(vrow,cursor_col)) && (eof_col = ws_eof(vrow,cursor_col)))	/* Are we in a mod field		*/
	{
		wsmove(cursor_row,sof_col);						/* Move to where to clear from.		*/
		for (wrk_col = sof_col; wrk_col <= eof_col; wrk_col++)			/* Loop from start of field.		*/
		{
			if (do_pseudo) mode = ws_trans_atr(attr_map[mx(vrow,wrk_col)]) | psb_rndt;
			else           mode = ws_trans_atr(attr_map[mx(vrow,wrk_col)]);
			wsmode(mode);							/* Select the needed rendition.		*/
			if (do_pseudo)
			{
				wscset(psb_chset);					/* Set the needed character set.	*/
				vprint("%c",psb_char);					/* Output pseudo blank.			*/
				wscset(DEFAULT);					/* Set back to default.			*/
			}
			else
			{
				vprint(" ");
			}
			data_map[mx(vrow,wrk_col)] = ' ';				/* Remember what it really is.		*/
		}
		wsmove(cursor_row,sof_col);						/* Move to the start of the field.	*/
		ws_tag_alt(alt_read,vrow,cursor_col,no_mod);				/* Set altered flags.			*/
	}
	else ws_bad_char();								/* Can't clear so beep.			*/

	return(SUCCESS);
}

/*
**	Routine:	ws_next()
**
**	Function:	To move cursor to the next tabable field.
**
**	Description:	This routine is called to move the cursor to the next available tab stop position.
**			It has to handle three types of "tab" requests, a normal tab, a newline, and a spacebar on a menupick.
**			It will search from the starting position down the screen and wrap back to the top of the screen.
**			If there are no tabable fields it will call ws_bad_char().
**
**
**	Input:		start_row	The starting row
**			start_col	The starting col
**			tab_type	The type of tab 
**						TAB_NORMAL	- a normal tab
**						TAB_SPACEBAR	- spacebar pressed on a monkey-bar menu-pick
**						TAB_NEWLINE	- the newline key (EXECUTE)
**			do_pseudo	Flag to do pseudo blank processing
**			
**
**	Output:		None		Cursor is positioned or bell rung.
**			
**
**	Return:		SUCCESS		Cursor was positioned
**			FAILURE		No tabable fields on this screen
**
**	Warnings:	None
**
**	History:	mm/dd/yy	Written by OLD
**			06/17/92	Documented & rewriten to remove recursion. GSL
**
*/

static int ws_next(start_row,start_col,tab_type,do_pseudo) 			/* Move to the next field.		*/
int 	start_row,start_col,tab_type;
int	do_pseudo;
{
	int 	vrow, x_row, x_col;

	x_row = start_row;							/* Set index values				*/
	x_col = start_col;
	vrow = vml(x_row);							/* Convert table index.				*/

	for(;;)
	{
		/*
		**	Check if a tab stop
		*/

		if (tab_fac(vrow,x_col))					/* if a tab_fac found then validate		*/
		{
			/*
			**	This TABSTOP is valid if
			**		a) This was a normal TAB 
			**		b) This was a SPACEBAR from a menupick and we are at a new menupick
			**		c) This was a NEWLINE and we are on a different line (or earlier on this line)
			*/
			if (	(tab_type == TAB_NORMAL) ||
				(tab_type == TAB_SPACEBAR && menu_pick_combo(vrow,x_col)) ||
				(tab_type == TAB_NEWLINE && (x_row != start_row || x_col < start_col))  )
			{
				check_mp(start_row,start_col,CLEAR,do_pseudo);		/* Clear bold rend if current menu pick.*/
				menu_fl = check_mp(x_row,x_col+1,BOLD,do_pseudo);	/* Bold the new position if a menu pick.*/
				return(SUCCESS);
			}

		}

		/*
		**	Increment loop index
		*/

		x_col++;							/* Increment column index			*/
		if (x_col >= wcurwidth)						/* If end of row then				*/
		{
			x_col = 0;						/* Start next row				*/
			x_row++;						/* Increment row index				*/
			if (x_row >= WS_MAX_LINES_PER_SCREEN)			/* If end of screen then			*/
			{
				x_row = 0;					/* Start at top of screen			*/
			}
			vrow = vml(x_row);					/* reset virtual row index			*/
		}

		if (x_col == start_col && x_row == start_row)			/* Back where we started			*/
		{
			ws_bad_char();
			return(FAILURE);
		}
	}
}

/*
**	Routine:	ws_last()
**
**	Function:	To move cursor to the previous tabable field.
**
**	Description:	This routine is called to move the cursor to the previous available tab stop position.
**			If there are no tabable fields it will call ws_bad_char().
**
**
**	Input:		start_row	The starting row
**			start_col	The starting col
**			do_pseudo	Flag to do pseudo blank processing
**			
**
**	Output:		None		Cursor is positioned or bell rung.
**			
**
**	Return:		SUCCESS		Cursor was positioned
**			FAILURE		No tabable fields on this screen
**
**	Warnings:	None
**
**	History:	mm/dd/yy	Written by OLD
**			06/17/92	Documented & rewriten to remove recursion. GSL
**
*/

static int ws_last(start_row,start_col,do_pseudo) 				/* Move to the last field.			*/
int 	start_row,start_col;
int	do_pseudo;
{
	int 	vrow, x_row, x_col;
	int	org_row, org_col;

	org_row = start_row;
	org_col = start_col;

	/*
	**	Adjust starting position 2 back (we position 1 ahead of FAC when found)
	*/
	start_col -= 2;
	if (start_col < 0)
	{
		start_col += wcurwidth;
		start_row--;
		if (start_row < 0)
		{
			start_row += WS_MAX_LINES_PER_SCREEN;
		}
	}

	x_row = start_row;							/* Set index values				*/
	x_col = start_col;
	vrow = vml(x_row);							/* Convert table index.				*/

	for(;;)
	{
		/*
		**	Check if a tab stop
		*/

		if (tab_fac(vrow,x_col))					/* if a tab_fac found then move			*/
		{
			check_mp(org_row,org_col,CLEAR,do_pseudo);		/* Clear bold rend if current menu pick.	*/
			menu_fl = check_mp(x_row,x_col+1,BOLD,do_pseudo);	/* Bold the new position if a menu pick.	*/
			return(SUCCESS);
		}

		/*
		**	Decrement loop index
		*/

		x_col--;							/* Decrement column index			*/
		if (x_col < 0)							/* If start of row then				*/
		{
			x_col += wcurwidth;					/* End of next row				*/
			x_row--;						/* DEcrement row index				*/
			if (x_row < 0)						/* If start of screen then			*/
			{
				x_row += WS_MAX_LINES_PER_SCREEN;		/* Go to end of screen				*/
			}
			vrow = vml(x_row);					/* reset virtual row index			*/
		}

		if (x_col == start_col && x_row == start_row)			/* Back where we started			*/
		{
			ws_bad_char();
			return(FAILURE);
		}
	}
}


/*
**	Routine:	next_mp()
**
**	Function:	To move cursor to the next menu pick field that starts with the character entered.
**
**	Description:	This routine is called to move the cursor to the next menupick field that starts with the
**			character pressed.
**			It will search from the starting position down the screen and wrap back to the top of the screen.
**			If there is no matching menupick it will call ws_bad_char().
**
**
**	Input:		start_row	The starting row
**			start_col	The starting col
**			the_char	The character pressed
**			do_pseudo	Flag to do pseudo blank processing
**			
**
**	Output:		None		Cursor is positioned or bell rung.
**			
**
**	Return:		SUCCESS		Cursor was positioned
**			FAILURE		No matching menupick fields on this screen
**
**	Warnings:	None
**
**	History:	mm/dd/yy	Written by OLD
**			06/17/92	Documented & rewriten to remove recursion. GSL
**
*/

static int next_mp(start_row,start_col,the_char,do_pseudo) 			/* Move to the next menu pick.			*/
int 	start_row,start_col,the_char;
int	do_pseudo;
{
	int 	vrow, x_row, x_col;

	x_row = start_row;							/* Set index values				*/
	x_col = start_col;
	vrow = vml(x_row);							/* Convert table index.				*/

	for(;;)
	{
		/*
		**	Check if a tab stop
		*/

		if (tab_fac(vrow,x_col))					/* if a tab_fac found then validate		*/
		{
			/*
			**	This TABSTOP is valid if the first character on the screen matches the_char pressed.
			*/
			if (toupper(data_map[mx(vrow,x_col+3)]) == toupper(the_char))	/* See if first char matches input char.*/
			{
				check_mp(start_row,start_col,CLEAR,do_pseudo);		/* Clear bold rend if current menu pick.*/
				menu_fl = check_mp(x_row,x_col+1,BOLD,do_pseudo);	/* Bold the new position if a menu pick.*/
				return(SUCCESS);
			}

		}

		/*
		**	Increment loop index
		*/

		x_col++;							/* Increment column index			*/
		if (x_col >= wcurwidth)						/* If end of row then				*/
		{
			x_col = 0;						/* Start next row				*/
			x_row++;						/* Increment row index				*/
			if (x_row >= WS_MAX_LINES_PER_SCREEN)			/* If end of screen then			*/
			{
				x_row = 0;					/* Start at top of screen			*/
			}
			vrow = vml(x_row);					/* reset virtual row index			*/
		}

		if (x_col == start_col && x_row == start_row)			/* Back where we started			*/
		{
			ws_bad_char();
			return(FAILURE);
		}
	}
}


static int ws_insert(i,j,k,alt_read,no_mod,do_pseudo)					/* Open a column.			*/
int i,j,k,alt_read; unsigned char *no_mod;
int	do_pseudo;
{
	int m, n, mode;									/* Working registers.			*/
	char c0, c1;									/* Working storage.			*/
	char a0, a1;									/* Variable to store sttributes.	*/
	char dm0, dm1;									/* Variable to save wang maps.		*/
	char am0, am1;									/* Variable to save wang maps.		*/

	if (n = ws_eof(k,j))								/* Are we in a field?			*/
	{
		if (vchr_map[k][n] != ' ' && (vchr_map[k][n] != psb_char && (vatr_map[k][n] & psb_chset) ))
		{
			ws_bad_char();							/* Don't push chars out of the field.	*/
		}
		else									/* It is ok to push.			*/
		{
			wsmove(i,j);							/* Move to where to insert the space.	*/
			c0 = vchr_map[k][j];						/* Remember the character there.	*/
			a0 = vatr_map[k][j];						/* Remember the attribute there.	*/
			dm0 = data_map[mx(k,j)];					/* Remember the wang map.		*/
			am0 = attr_map[mx(k,j)];					/* Remember the wang map.		*/
			data_map[mx(k,j)] = ' ';					/* Change to a blank character.		*/
			if (do_pseudo) mode = psb_rndt | ws_trans_atr(attr_map[mx(k,j)]);
			else	       mode = ws_trans_atr(attr_map[mx(k,j)]);
			wsmode(mode);							/* Select the needed rendition.		*/
			if (do_pseudo)
			{
				wscset(psb_chset);					/* Set to needed character set.		*/
				vprint("%c",psb_char);					/* Now output the opened space.		*/
				wscset(DEFAULT);					/* Set back to normal.			*/
			}
			else
			{
				vprint(" ");
			}
			for (m = j+1; m <= n; m++)					/* Loop through the rest of characters.	*/
			{
				c1 = vchr_map[k][m];					/* Remember the next character.		*/
				a1 = vatr_map[k][m];					/* Remember the attribute there.	*/
				dm1 = data_map[mx(k,m)];				/* Remember the wang atribute.		*/
				am1 = attr_map[mx(k,m)];				/* Remember the wang atribute.		*/
				if (a0 & GRAPHICS) wscset(GRAPHICS);			/* If Graphics bit set, change char set.*/
				if (dm0 == PSEUDO_BLANK_CHAR)
				{
					wsmode(mode|psb_rndt);				/* Select the needed rendition.		*/
				}
				else
				{
					wsmode(mode);					/* Select the needed rendition.		*/
				}
				vputc(c0);						/* Substitute the last one.		*/
				wscset(DEFAULT);					/* Set back to normal.			*/
				data_map[mx(k,m)] = dm0;				/* Substitue the space.			*/
				attr_map[mx(k,m)] = am0;				/* Substitue the space attributes.	*/
				c0 = c1;						/* Swap.				*/
				a0 = a1;						/* Swap attributes.			*/
				dm0 = dm1;
				am0 = am1;
			}
			wsmove(i,j);							/* Move to insert.			*/
			ws_tag_alt(alt_read,k,j,no_mod);				/* Set altered flags.			*/
		}
	}
	else ws_bad_char();								/* Not in field so just beep.		*/

	return(SUCCESS);
}

static int ws_remove(cursor_row,cursor_col,vrow,alt_read,no_mod,do_pseudo)		/* Remove a character and close-up	*/
int 	cursor_row,cursor_col;								/* cursor position			*/
int	vrow;										/* virtual row from vml()		*/
int	alt_read; 
unsigned char *no_mod;
int	do_pseudo;
{
	int	eof_col;								/* end of field column			*/
	int	wrk_col;								/* working column			*/
	int  	mode;									/* Working registers.			*/

	if (eof_col = ws_eof(vrow,cursor_col))						/* Should we delete right?		*/
	{
		for (wrk_col = cursor_col; wrk_col <= eof_col; wrk_col++)		/* Loop from start of field.		*/
		{
			if (do_pseudo) mode = ws_trans_atr(attr_map[mx(vrow,wrk_col)]) | psb_rndt;
			else	       mode = ws_trans_atr(attr_map[mx(vrow,wrk_col)]);

			if (wrk_col == eof_col)
			{
				wsmode(mode);						/* Select the needed rendition.		*/
				if (do_pseudo)
				{
					wscset(psb_chset);				/* Set the needed character set.	*/
					vprint("%c",psb_char);				/* Output spaces if past end of field.	*/
					wscset(DEFAULT);				/* Set back to default char set.	*/
				}
				else
				{
					vprint(" ");
				}
				data_map[mx(vrow,wrk_col)] = ' ';			/* Remember what it really is.		*/
			}
			else
			{
				if ( data_map[mx(vrow,wrk_col+1)] == PSEUDO_BLANK_CHAR )
				{
					wsmode(mode|psb_rndt);				/* Select the needed rendition.		*/
				}
				else
				{
					wsmode(mode);					/* Select the needed rendition.		*/
				}

				if (vatr_map[vrow][wrk_col+1] & GRAPHICS) 		/* If Graphics bit set, 		*/
					wscset(psb_chset);				/* change char set.			*/
				vputc(vchr_map[vrow][wrk_col+1]);				/* Output the character.	*/
				wscset(DEFAULT);						/* Set back to normal.		*/
				data_map[mx(vrow,wrk_col)] = data_map[mx(vrow,wrk_col+1)];	/* Remember char for swap back.	*/
				attr_map[mx(vrow,wrk_col)] = attr_map[mx(vrow,wrk_col+1)];	/* Remember char for swap back.	*/
			}
		}
		wsmove(cursor_row,cursor_col);						/* Move to the start of the field.	*/
		ws_tag_alt(alt_read,vrow,cursor_col,no_mod);				/* Set altered flags.			*/
	}
	else ws_bad_char();								/* Can't delete so beep.		*/
	return(SUCCESS);
}

int ws_bad_char()									/* Process invalid characters.		*/
{
	vbell();									/* Ring the bell.			*/
	ws_good = FALSE;								/* Character was not good.		*/
}

/*
**	Routine:	tab_fac()
**
**	Function:	To determine if this is a FAC that starts a field that can be tabbed to.
**
**	Description:	Verify that this is a FAC.
**			Verify that it is a tabstop.
**			Verify it is not concatinated FAC's
**
**	Input:		vrow		The virtual row number
**			col		The column number
**			
**
**	Output:		None
**
**	Return:		SUCCESS = FAC for a tabable field.
**			FAILURE = not a tab FAC
**
**	Warnings:	None
**
**	History:	mm/dd/yy	Written by OLD
**			06/16/92	Documented by GSL
**
*/

static int tab_fac(vrow,col) register int vrow,col;					/* Determine if start of field fac.	*/
{
	int 	x_col;									/* Working register.			*/

	if ( !(attr_map[mx(vrow,col)] & FAC_CHARACTER) ) return(FAILURE);		/* Is this a FAC at all?		*/
	if ( !ws_tab_stop(attr_map[mx(vrow,col)]) ) return(FAILURE);			/* Is this a modifyable or tab field?	*/
	if ( (x_col = col+1) < wcurwidth )						/* Are we past the end of the line?	*/
	{
		if (tab_fac(vrow,x_col)) return(FAILURE);				/* No, then check for concat FAC.	*/
	}

	return(SUCCESS);								/* Modifyable FAC is what we want.	*/
}

static int menu_pick_combo(k,j) int k, j;						/* Determine if LOW PROT NUM fac or	*/
{											/* HIGH PROT NUM fac and is followed by	*/ 
	unsigned char *dm, c;								/* a WANG_MENU_PICK or PSEUDO BLANK_CHAR.*/
	int fl;

	fl = FALSE;									/* Set to assume is not a menu pick.	*/
	dm = &data_map[mx(k,j)];							/* Point to the current entry.		*/
	c = *dm;									/* Get the character.			*/
	if (c == NUMPROT_FIELD || c == NUMPROT_BOLD_FIELD)				/* Is it the needed Fac?		*/
	{
		dm = &data_map[mx(k,j+1)];
		c = *dm;
		if (c == WANG_MENU_PICK || c == PSEUDO_BLANK_CHAR)  fl = TRUE;		/* Is needed fac, WANG_MENU_PCIK or	*/
	}										/* PSEUDO_BLANK_CHAR combo.		*/
	return(fl);
}

int ws_sof(vrow,cursor_col)								/* Get start of current field.		*/ 
register int vrow,cursor_col;
{
	if (cursor_col < 0) return(FAILURE);						/* Return failure if not in field.	*/
	if (!ws_mod(vrow,cursor_col)) return(FAILURE);					/* If not modifiable then not in field	*/
	while ((cursor_col > 0) && (ws_mod(vrow,cursor_col))) --cursor_col;		/* Loop until at start of the field.	*/
	return(cursor_col+1);								/* Return the start of the field.	*/
}

int ws_eof(vrow,cursor_col) 								/* Get end of current field.		*/
register int vrow,cursor_col;
{
	if (cursor_col < 0) return(FAILURE);						/* Return failure if not in field.	*/
	if (!ws_mod(vrow,cursor_col)) return(FAILURE);					/* If not modifiable then not in field	*/
	while ((cursor_col < wcurwidth) && (ws_mod(vrow,cursor_col))) cursor_col++;	/* Loop until at the end of the field.	*/
	return(cursor_col-1);								/* Return the end of the field.		*/
}

static int ws_tab_stop(atr) unsigned char atr;						/* Check for a tab stop position.	*/
{
	register int i;									/* Working register.			*/

	i = FALSE;									/* Assume not a tab stop.		*/
											/* Modifyable or tab stop?		*/
	if (!(atr & FAC_PROTECT)) i = TRUE;						/* Yes, then it is a tab stop location.	*/

	atr = atr & ~(FAC_CHARACTER + FAC_LOW_INTENSITY + FAC_UNDERSCORE);		/* Drop the don't care bits.		*/
	if (atr == (FAC_NUMERIC_ONLY|FAC_PROTECT)) i = TRUE;				/* NUM-PROT? If so it is a tab stop.	*/

	return(i);									/* Return to the caller.		*/
}


static int ws_trans_atr(ch) unsigned char ch;						/* Translate from Wang to VIDEO 	*/
{											/*     attribute formats.		*/
	register int i;									/* Working registers.			*/

	i = BOLD;									/* Assume normal character rendition.	*/
	if (!ws_invisible(ch))								/* Is this a displayable field?		*/
	{
		if (ch & FAC_LOW_INTENSITY) i = 0;					/* Yes, then low intensity?		*/
		if (ch & FAC_BLINKING) i = i | BLINK;					/* Or in blinking.			*/
	}
	if (ch & FAC_UNDERSCORE) i = i | UNDERSCORE;					/* Or in underscore.			*/
	return(i);									/* Send back the VIDEO attributes.	*/
}

static int ws_bloff(do_pseudo)								/* Turn blinking off.			*/
int	do_pseudo;
{
	register int i, j, k;								/* Working variables.			*/

	if (wbackground()) return(SUCCESS);						/* In background so don't do.		*/

	vstate(SAVE);									/* Save state.				*/
	for (i = 0; i < WS_MAX_LINES_PER_SCREEN; i++)					/* Yes, so loop screen.			*/
	{
		k = vml(i);								/* Create table index.			*/
		for (j = 0; j < wcurwidth; j++)						/* Loop each character.			*/
		{
			if (ws_err_field(attr_map[mx(k,j)]))				/* Error in this field?			*/
			{
				attr_map[mx(k,j)] = attr_map[mx(k,j)] & ~FAC_BLINKING;	/* Yes, remove attrib.			*/
				if (attr_map[mx(k,j)] & FAC_CHARACTER) data_map[mx(k,j)] = data_map[mx(k,j)] & ~FAC_BLINKING;
				if (!ws_mod(k,j))					/* Modifyable field?			*/
				{
					wsmode(ws_trans_atr(attr_map[mx(k,j)]));	/* Select rendition.			*/
				}
				else if (do_pseudo)
				{
					wsmode(psb_rndt | ws_trans_atr(attr_map[mx(k,j)]));	/* Field rendition.		*/
				}
				else
				{
					wsmode(ws_trans_atr(attr_map[mx(k,j)]));	/* Field rendition.		*/
				}
				wsmove(i,j);						/* Move to location.		*/
				vputc(vchr_map[k][j]);					/* Reoutput the char.			*/
			}
		}
	}
	vstate(RESTORE);								/* Restore state.			*/
	return(SUCCESS);
}

static ws_echo(i,k,j,ar,no_mod,do_pseudo)						/* Echo input as appropriate.		*/
int i,j,k,ar; unsigned char *no_mod;
int	do_pseudo;
{
	int mode;

	if (do_pseudo)  mode = psb_rndt | ws_trans_atr(attr_map[mx(k,j)]);
	else		mode = ws_trans_atr(attr_map[mx(k,j)]);
	wsmode(mode);									/* Select the appropriate rendition.	*/
	ws_tag_alt(ar,k,j,no_mod);							/* Set flags.				*/

	if (ws_invisible(attr_map[mx(k,j)]))						/* Should characte be echoed?		*/
	{
		if (do_pseudo)
		{
			wscset(psb_chset);						/* No, then echo a pseudo blank.	*/
			vprint("%c",psb_char);
			wscset(DEFAULT);
		}
		else
		{
			vprint(" ");
		}
	}
	else vputc((char)i);								/* Yes, then echo the character.	*/
	data_map[mx(k,j)] = i;								/* Remember the new character.		*/
}

static int load_sub_chars()								/* Default load of the subst. table.	*/
{											/* Index is the Wang input screen value.*/
	register int i, ndx;
							 				/* Load default substitution for	*/
	ndx = 1;									/* non-printable input values.		*/
	stable[ndx].sub_value	= DEC_GRAPHIC_DIAMOND;					/* 0x01 - Wang small diamond.		*/
	stable[ndx++].char_set	= GRAPHICS;
	stable[ndx].sub_value	= DEC_DBL_RIGHT_ARROW;					/* 0x02 - Wang right arrow.		*/
	stable[ndx++].char_set	= ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_DBL_LEFT_ARROW;					/* 0x03 - Wang left arrow.		*/
	stable[ndx++].char_set	= ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_PLUS_MINUS;					/* 0x04 - Plus/Minus sign.		*/
	stable[ndx++].char_set	= GRAPHICS;
	stable[ndx].sub_value	= DEC_MENU_PICK;					/* 0x05 - Wang menu pick.		*/
	stable[ndx++].char_set	= DEFAULT;
	stable[ndx].sub_value	= DEC_GRAPHIC_VERT;					/* 0x06 - Wang vertical bar.		*/
	stable[ndx++].char_set	= GRAPHICS;
	stable[ndx].sub_value	= DEC_GRAPHIC_HORIZ;					/* 0x07 - Wang bold line.		*/
	stable[ndx++].char_set	= GRAPHICS;
	stable[ndx].sub_value	= DEC_GRAPHIC_BOX;					/* 0x08 - Wang dots.			*/
	stable[ndx++].char_set	= GRAPHICS;
	stable[ndx].sub_value	= DEC_CENTERED_PERIOD;					/* 0x09 - Centered period.		*/
	stable[ndx++].char_set	= GRAPHICS;
	stable[ndx].sub_value	= DEC_VERTICAL_BAR;					/* 0x0A - Not equal sign.		*/
	stable[ndx++].char_set	= GRAPHICS;
	stable[ndx].sub_value	= psb_char;						/* 0x0B - Wang solid box.		*/
	stable[ndx++].char_set	= psb_chset;
	stable[ndx].sub_value	= DEC_GRAPHIC_PI;					/* 0x0C - Pi symbol.			*/
	stable[ndx++].char_set	= GRAPHICS;
	stable[ndx].sub_value	= DEC_EXCLAMATION;					/* 0x0D - Wang up_down arrow.		*/
	stable[ndx++].char_set	= ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_FOUR_CORNER;					/* 0x0E - Intersect two lines, corners.	*/
	stable[ndx++].char_set	= GRAPHICS;
	stable[ndx].sub_value	= DEC_PARAGRAPH;					/* 0x0F - Wang paragraph.		*/
	stable[ndx++].char_set	= ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_SUPSCPT_2;					/* 0x10 - Wang solid diamond.		*/
	stable[ndx++].char_set	= ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_SUPSCPT_3;					/* 0x11 - Wang open diamond.		*/
	stable[ndx++].char_set	= ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_BOTM_RIGHT_BOX;					/* 0x12 - Bottom right corner for box.	*/
	stable[ndx++].char_set	= GRAPHICS;
	stable[ndx].sub_value	= DEC_TOP_RIGHT_BOX;					/* 0x13 - Top right corner for box.	*/
	stable[ndx++].char_set	= GRAPHICS;
	stable[ndx].sub_value	= DEC_TOP_LEFT_BOX;					/* 0x14 - Top left corner for box.	*/
	stable[ndx++].char_set	= GRAPHICS;
	stable[ndx].sub_value	= DEC_BOTM_LEFT_BOX;					/* 0x15 - Bottom left corner for box.	*/
	stable[ndx++].char_set	= GRAPHICS;
	stable[ndx].sub_value	= DEC_GRAPHIC_UK_POUND;					/* 0x16 - Wang wishbone.		*/
	stable[ndx++].char_set	= GRAPHICS;
	stable[ndx].sub_value	= DEC_QUESTION_MARK;					/* 0x17 - Wang anchor.			*/
	stable[ndx++].char_set	= ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_VERT_RHORIZ;					/* 0x18 - Vertical with right horizontal.*/
	stable[ndx++].char_set	= GRAPHICS;
	stable[ndx].sub_value	= DEC_VERT_LHORIZ;					/* 0x19 - Vertical with left horizontal.*/
	stable[ndx++].char_set	= GRAPHICS;
	stable[ndx].sub_value	= DEC_GRAPHIC_SQUARE;					/* 0x1A - Wang open box.		*/
	stable[ndx++].char_set	= ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_HORIZ_TVERT;					/* 0x1B - Horizontal with top vertical.	*/
	stable[ndx++].char_set	= GRAPHICS;
	stable[ndx].sub_value	= DEC_HORIZ_BVERT;					/* 0x1C - Horizontal with bottom vert.	*/
	stable[ndx++].char_set	= GRAPHICS;
	stable[ndx].sub_value	= DEC_CENTS_SIGN;					/* 0x1D - Graphic cents sign.		*/
	stable[ndx++].char_set	= ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_ONE_FOURTH;					/* 0x1E - Graphic 1/4 symbol.		*/
	stable[ndx++].char_set	= ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_ONE_HALF;						/* 0x1F - Graphic 1/2 symbol.		*/
	stable[ndx++].char_set	= ROM_GRAPHICS;
	for (i = 32; i < 128; i++)							/* Initialize the table for printable	*/
	{										/*  characters.				*/
		stable[i].sub_value = i;
		stable[i].char_set = DEFAULT;
	}
	stable[96].sub_value	= DEC_GRAPHIC_DEGREE;					/* 0x60 - Degree symbol.		*/
	stable[96].char_set	= GRAPHICS;
	return(SUCCESS);
}

static int load_user_sub_chars(tblptr)							/* Do the load of substitution values.	*/
unsigned char *tblptr;
{
	unsigned char *lpt;								/* Local copy of ptr to sub table.	*/
	int ndx;

	lpt = tblptr;									/* Set the pointer.			*/
	while (*lpt)
	{
		ndx = (int) *lpt++;							/* Get ptr of index into sub table.	*/
		stable[ndx].sub_value = *lpt++;						/* Put value into table.		*/
		stable[ndx].char_set = (int) *lpt++;					/* Put value into table.		*/
	}
}

static sub_char(c, xc, char_set) unsigned char c, *xc;					/* Get the substitution character and	*/
int *char_set;										/*  character set from table.		*/
{
	if (!sub_table_loaded)								/* Has the sub. table been loaded ?	*/
	{
		load_sub_chars();							/* Nope. Load it.			*/
		sub_table_loaded = TRUE;						/* Set the static variable.		*/
	}
	*xc = stable[c].sub_value;							/* Get exchange character.		*/
 	*char_set = stable[c].char_set;							/* Get needed character set (font).	*/
}

static wsdmp_scrn(wsb,type_d)
char *wsb;
int type_d;
{
	char	*ptr;
	int i,j;
	unsigned char tstr[WS_MAX_COLUMNS_ON_A_LINE+4];

	wsb += 4;

	tstr[wcurwidth] = '\n';
	tstr[wcurwidth+1] = '\0';

	if (type_d == 0)									/* This is a bad condition.	*/
	{
		werrlog(ERRORCODE(22),terminal_error,0,0,0,0,0,0,0);				/* Something wrong with output	*/
		ptr = "%%VW-W-DUMPSCREEN Abnormal condition, dump of current screen is:\n\n";
		printf(ptr);
		if (w_err_flag & LOG_LOGFILE) werr_write(ptr);					/* write to error log		*/
	}
	else if (type_d == 1) vset(PRINTER,ON);							/* On printer for output.	*/
	for (i=0; i<WS_MAX_LINES_PER_SCREEN; i++)
	{
		for (j = 0; j < wcurwidth; j++)
		{
			tstr[j] = *wsb++;
			if (tstr[j] & 0x80) tstr[j] = ' ';
		}
		printf("%s",tstr);
		if (w_err_flag & LOG_LOGFILE) werr_write(tstr);					/* write to error log		*/
	}
	if (type_d == 1) vset(PRINTER,OFF);							/* Turn the printer off.	*/
	else wexit(ERRORCODE(22));								/* else unconditional exit.	*/
}

ws_erap(amount) int amount;									/* Erase & protect screen sect.	*/
{
	register int i,j,k;									/* Working registers.		*/
	int last_op;

	last_op = voptimize(TRACKING_ONLY);							/* Turn optimization off.	*/

	if (amount == FULL_SCREEN)								/* Erasing the full screen?	*/
	{
		memset(data_map, ' ', sizeof(data_map));					/* Clear the data array.	*/
		memset(attr_map, FAC_PROTECT | FAC_LOW_INTENSITY, sizeof(attr_map));		/* Initialize the attr array.	*/
	}

	else if (amount == TO_EOS)								/* Erasing to end of screen?	*/
	{
		k = vml(vcur_lin);								/* Get array index.		*/
		for (j = vcur_col; j < wcurwidth; j++)						/* Loop through the columns.	*/
		{
			data_map[mx(k,j)] = ' ';						/* Space out the item.		*/
			attr_map[mx(k,j)] = FAC_PROTECT | FAC_LOW_INTENSITY;			/* Protect the item.		*/
		}
		for (i = vcur_lin+1; i < WS_MAX_LINES_PER_SCREEN; i++)				/* Do remaining lines.		*/
		{
			k = vml(i);								/* Calculate index.		*/
			for (j=0; j < wcurwidth; j++)
			{
				data_map[mx(k,j)] = ' ';					/* Space it out.		*/
				attr_map[mx(k,j)] = FAC_PROTECT | FAC_LOW_INTENSITY;		/* Protect the item.		*/
			}
		}
	}
	verase(amount);										/* Erase as much as specified.	*/
	voptimize(last_op);									/* Restore optimization.	*/
}



int ws_help(curset) int curset;								/* Simulate Wang help function.		*/
{
	int temp_lin, temp_col, temp_atr, temp_set;					/* Temporary state storage.		*/
	char *csave, *asave, *wsa1_save, *wsa2_save, *extra;				/* Char and attribute save pointers.	*/
	register int s,w;								/* Working register.			*/
	int old_menu_exit_key;								/* Orig. menu exit key.			*/

	old_menu_exit_key = menu_get_key(0);						/* What is the orig. menu exit key ?	*/
	menu_set_key(0, fn1_key);							/* Give it a new value.			*/

	if (help_active)								/* Are we already in help.		*/
	{
		ws_bad_char();								/* Ring the bells.			*/
		return(FAILURE);							/* Return to the caller.		*/
	}
	else help_active = TRUE;							/* Now help is active.			*/

	wpushscr();									/* Save all neccessary variables.	*/

	if (wcurwidth != 80)  ws80();							/* Put in 80 col mode for help screens.	*/
	vdefer(RESTORE);								/* Cannot be in deferred mode.		*/

	wsh_help(1);									/* Output the selection menu.		*/
	vset(CURSOR,curset);								/* Turn the cursor back on.		*/
	cur_on_fl = curset;								/* Set the flag.			*/

	vdefer(RESTORE);								/* Restore from optimization.		*/
	wpopscr();									/* Restore all saved variables.		*/

	vmode(vcur_atr);								/* Restore the character rendition.	*/
	vcharset(vchr_set);								/* Restore the character set.		*/
	wsmove(vcur_lin, vcur_col);							/* Restore the position.		*/
	vdefer(RESTORE);								/* Optimization terminates before exit.	*/

	help_active = FALSE;								/* Now help is inactive.		*/
	synch_required = FALSE;								/* A synch is not required.		*/

	menu_set_key(0, old_menu_exit_key);						/* Reset the menu exit key.		*/

	return(SUCCESS); 								/* All done.				*/
}

wpushscr()										/* A function to save the screen addrs	*/
{											/* and variables of a 'Wisp' screen.	*/
	typedef struct wscrn_struct wscrn_struct;
	wscrn_struct *wscrn_struct_ptr;							/* Point to the the save area.		*/
	int x;										/* A local, working variable.		*/

	if (wbackground()) return(0);							/* Don't do if in background.		*/
	if (rts_first)  init_screen();							/* If first push.			*/
	x = sizeof(struct wscrn_struct);
	wscrn_struct_ptr = (wscrn_struct *) malloc(x);					/* Alloc storage for the info to save.	*/
	if (!wscrn_struct_ptr)
	{
		werrlog(ERRORCODE(12),0,0,0,0,0,0,0,0);					/* No memory				*/
		wexit(ERRORCODE(12));							/* Unconditional exit to DCL.		*/
	}

	x = sizeof(attr_map);								/* Size for the data to store.		*/
	wscrn_struct_ptr->saved_ws_atr = malloc(x);					/* Alloc. storage and save the addr.	*/
	if (!wscrn_struct_ptr->saved_ws_atr)
	{
		werrlog(ERRORCODE(12),0,0,0,0,0,0,0,0);					/* No memory				*/
		wexit(ERRORCODE(12));							/* Unconditional exit to DCL.		*/
	}
	memcpy(wscrn_struct_ptr->saved_ws_atr, attr_map, x);				/* Copy contents to the storage area.	*/

	wscrn_struct_ptr->saved_ws_at2 = malloc(x);					/* Alloc. storage and save the addr.	*/
	if (!wscrn_struct_ptr->saved_ws_at2)
	{
		werrlog(ERRORCODE(12),0,0,0,0,0,0,0,0);					/* No memory				*/
		wexit(ERRORCODE(12));							/* Unconditional exit to DCL.		*/
	}
	memcpy(wscrn_struct_ptr->saved_ws_at2, data_map, x);				/* Copy contents to the storage area.	*/

	wscrn_struct_ptr->saved_ws_good = ws_good;					/* Save the following variables..	*/
	wscrn_struct_ptr->saved_old_line = old_line;
	wscrn_struct_ptr->saved_old_column = old_column;
	wscrn_struct_ptr->saved_errset = errset;
	wscrn_struct_ptr->saved_kb_locked = kb_locked;
	wscrn_struct_ptr->saved_fast_read = fast_read;
	wscrn_struct_ptr->saved_screen_width = wcurwidth;

	wscrn_struct_ptr->prev_wscrn_struct_ptr = (wscrn_struct *) wscrn_stack;		/* Save the address of the prior screen	*/
	wscrn_stack = (wscrn_struct *) wscrn_struct_ptr;				/* Save addr. of the new top screen.	*/

	vpushscr();									/* Save the corresponding video structs.*/
	return(SUCCESS);
}

wpopscr()										/* A function to restore the screen and	*/
{											/* associated variables.		*/
	struct wscrn_struct *wscrn_struct_ptr;						/* Local pointer to the save area.	*/
	int x;										/* A working variable.			*/

	if (wbackground()) return(0);							/* Don't do if in background.		*/
	wscrn_struct_ptr = wscrn_stack;							/* Point to the most recently saved 	*/
											/* structure of pointers and variables.	*/
	if (!wscrn_struct_ptr)								/* Are we pointing to never never land?	*/
	{
		werrlog(ERRORCODE(14),0,0,0,0,0,0,0,0);					/* Stack empty				*/
		return(0);								/* Outta here.				*/
	}

 	x = sizeof(attr_map);								/* What's the size of this item ?	*/
	memcpy(attr_map, wscrn_struct_ptr->saved_ws_atr, x);				/* Copy the saved bytes.		*/
	free(wscrn_struct_ptr->saved_ws_atr);						/* Free up this area.			*/
	memcpy(data_map, wscrn_struct_ptr->saved_ws_at2, x);
	free(wscrn_struct_ptr->saved_ws_at2);						/* Free up this area.			*/

	ws_good = wscrn_struct_ptr->saved_ws_good;					/* Restore these integer variables.	*/
	old_line = wscrn_struct_ptr->saved_old_line;
	old_column = wscrn_struct_ptr->saved_old_column;
	errset = wscrn_struct_ptr->saved_errset;
	kb_locked = wscrn_struct_ptr->saved_kb_locked;
	fast_read = wscrn_struct_ptr->saved_fast_read;
	wcurwidth = wscrn_struct_ptr->saved_screen_width;

	wscrn_stack = wscrn_struct_ptr->prev_wscrn_struct_ptr;				/* Get address of the new top screen.	*/
	free(wscrn_struct_ptr);								/* Free up the allocated area.		*/

	if (vscr_wid != wcurwidth)							/* If need to reset screen width.	*/
	{
		if (wcurwidth == 132) ws132();						/* Put back to what it was before.	*/
		else ws80();
	}
	vpopscr();									/* Get the video structs.		*/
	synch_required = FALSE;								/* Synch is not required now.		*/
	return(SUCCESS);
}

ws_putbuff(c,i,j,k,rendition,font) unsigned char c; int i,j,k,rendition,font;		/* Put char. in line buf. accumulator.	*/
{
	if (c == ' ')									/* Special case for space...		*/
	{
		if (!visible(c,rendition) && !visible(vchr_map[k][j],vatr_map[k][j])) return(SUCCESS);	/* No change, just ret.	*/
	}

	if ((i != vcur_lin) || (j != vcur_col+lbcount) || (rendition != vcur_atr) || (font != vchr_set))
	{
		ws_dumpbuff(k,FALSE);							/* Dump what is currently in buffer.	*/
		wsmove(i,j);								/* Now move to where we want to be.	*/
		wsmode(rendition);							/* Select the desired new rendition.	*/
		wscset(font);								/* Select the desired new font.		*/
	}

	lbuf[lbcount++] = c;								/* Store the next character.		*/
	lbattr = font|rendition;							/* Remember the attributes.		*/
	if ((c == ' ') && !(rendition&REVERSE) && !(rendition&UNDERSCORE)) lbdangle++;	/* Count invisible dangling spaces.	*/
	else lbdangle = 0;								/* Anything else terminates the dangle.	*/
}

ws_dumpbuff(k,eol) int k,eol;								/* Dump the current buffer.		*/
{
	if (lbcount)									/* Anything in output accumulator.	*/
	{
		if (eol && (lbdangle > 4)) lbcount = lbcount - lbdangle;		/* Any dangling spaces?			*/
		if (lbcount)								/* Anything left?			*/
		{
			lbuf[lbcount] = '\000';						/* Yes, deposit a null.			*/
			vrawprint(lbuf);						/* Print the string.			*/
			memcpy(&vchr_map[k][vcur_col],lbuf,lbcount);			/* Store data in video's maps.		*/
			memset(&vatr_map[k][vcur_col],lbattr,lbcount);			/* Store the attributes.		*/
			vcur_col = vcur_col + lbcount;					/* Move the tracking counter.		*/
			if (vcur_col >= wcurwidth) vcur_col = wcurwidth - 1;		/* At the edge?				*/
			lbcount = 0;							/* Reset the counter.			*/
		}
		if (eol && (lbdangle > 4)) verase(TO_EOL);				/* Erase to the end of the line.	*/
		lbdangle = 0;								/* Reset the dangle counter.		*/
	}
}

static int set_cl_fl()									/* Set console logging flag.		*/
{
#ifdef VMS
	unsigned long stat;								/* Status returned form SYS$TRNLNM.	*/
$DESCRIPTOR(t_desc,"LNM$PROCESS_TABLE");						/* Define system service descriptors.	*/
$DESCRIPTOR(l_desc,"DO_CONSOLE_LOG");

	if ((stat = SYS$TRNLNM(0,&t_desc,&l_desc,0,0)) == SS$_NORMAL)			/* Verify if logical exists.		*/
	{										/* Found.				*/
		do_console_log = TRUE;							/* Set to do console logging.		*/
	}
	else do_console_log = FALSE;							/* Not found so no console logging.	*/
#endif
}

static int check_mp(line,col,rend,do_pseudo)						/* See if it is a menu pick item.	*/
int line, col, rend;
int	do_pseudo;
{
	if ( menu_pick_combo(line,col-1) )						/* Is it a menu pick item?	*/
	{
		if (cur_on_fl)								/* If cursor is on then turn off while  */
		{									/*  rewriting the line.			*/
			vset(CURSOR,INVISIBLE);
		}
		rewrite_mp(line,col,rend,do_pseudo);					/* Re-write line with bold attribute.	*/
		wsmove(line,col);							/* Put cursor back to where it was.	*/
		if (cur_on_fl)								/* If cursor was on then turn it back   */
		{									/* on.					*/
			vset(CURSOR,VISIBLE);
		}
		return(TRUE);								/* Return TRUE, was a menu pick.	*/
	}
	else
	{
		wsmove(line,col);							/* Move to new position.		*/
		return(FALSE);								/* Return FALSE, was not a menu pick.	*/
	}
}

static int rewrite_mp(line,col,rend,do_pseudo)						/* Rewrite line or until next FAC.	*/
int line, col, rend;
int	do_pseudo;
{
	unsigned char c;								/* Working character.			*/
	unsigned char xc;								/* Exchanged character value for 'c'.	*/
	unsigned char *dm, *am, last_atr, *fc;
	int last_mode, char_set;
	int j, k, mode;

	last_atr = FAC_PROTECT | FAC_LOW_INTENSITY;					/* Reset attributes at start of line.	*/
	k = vml(line);									/* Determine map index.			*/
	dm = &data_map[mx(k,col)];							/* Point to the current line.		*/
	am = &attr_map[mx(k,col)];
	fc = &data_map[mx(k,col-1)];							/* Get the FAC character.		*/
	last_mode = ws_trans_atr(*fc);							/* Set the last mode.			*/

	vbuffering(LOGICAL);								/* Turn on buffering if not on.		*/

	for (j = col; j < wcurwidth; j++)						/* Loop through each column.		*/
	{
		c = *dm;								/* Get the character from the map.	*/
		last_atr = *am;								/* Get the character attributes.	*/

		if (c & FAC_CHARACTER)							/* Is this character a FAC? 		*/
		{
			j = wcurwidth;							/* Get out of loop.			*/
		}
		else
		{
			if (ws_invisible(last_atr) || c == 0)				/* Should this field be invisible?	*/
			{
				ws_putbuff(' ',line,j,k,CLEAR,DEFAULT);			/* Put in output buffer.		*/
			}
			else
			{
				sub_char(c, &xc, &char_set); 				/* Get character & font from sub table.	*/
				if (!(last_atr & FAC_PROTECT))				/* Is this a modifiable field ?		*/
				{
					if (do_pseudo)  mode = last_mode|rend|psb_rndt;
					else		mode = last_mode|rend;
					ws_putbuff(xc,line,j,k,mode,char_set); 		/* Put char.				*/
				}
				else if ((xc != ' ') || (last_mode & (REVERSE|UNDERSCORE)))	/* Visible?			*/
				{
					if ((xc >= ' ' || !do_pseudo) && (c != PSEUDO_BLANK_CHAR))
						mode = last_mode; 			/* If in the printable range.		*/
					else	mode = last_mode|psb_rndt;
					if (j == col && rend == BOLD)			/* Is current item so put defined symbol*/
					{
						xc = CURRENT_MENU_PICK;			/* Defined in vwang.h			*/
					}
					ws_putbuff(xc,line,j,k,mode|rend,char_set);
				}
				else							/* Not visible. Remove the blink.	*/
				{
					if (c == PSEUDO_BLANK_CHAR)			/* Is an explicit psuedo blank char.	*/
						mode = (last_mode & ~BLINK)|psb_rndt;
					else	mode = last_mode & ~BLINK;		/* Set the needed mode.			*/
					ws_putbuff(' ',line,j,k,mode,DEFAULT); 		/* Put char.				*/
				}
			}
			dm++;								/* Incrememt the buffer pointers.	*/
			am++;
		}
	}
	ws_dumpbuff(k,TRUE);								/* Dump the accumulated buffer.		*/
	vbuffering(AUTOMATIC);								/* Dump the buffer to show the screen.	*/
	wsmode(CLEAR);									/* Clear out the renditions.		*/
	wscset(DEFAULT);
}

static int mx(row,column) int row,column;						/* Calculate array index.		*/
{
	return((row * wcurwidth) + column);						/* Return the value.			*/
}

int ws80()										/* Select 80 column screen.		*/
{
	if (rts_first)  init_screen();							/* If first time setting.		*/
	wsetwid(NARROW);								/* Set the screen width.		*/
	return(SUCCESS);
}
int ws132()										/* Select a 132 column screen.		*/
{
	if (rts_first)  init_screen();							/* If first time setting.		*/
	wsetwid(WIDE);									/* Select the screen width.		*/
	return(SUCCESS);
}
static int wsetwid(wd) int wd;
{
	int op_save;									/* Save current level of optimization.	*/

	op_save = voptimize(OFF);							/* Turn optimization off.		*/
	vscreen(wd);									/* Select the screen width.		*/
	if (wd == WIDE) wcurwidth = 132;						/* Remember the current width.		*/
	else		wcurwidth = 80;
	synch_required = TRUE;								/* Now force a synchronize.		*/
	vdefer(RESTORE);								/* Restore from any optimization.	*/
	voptimize(op_save);								/* Restore optimization level.		*/

	return(SUCCESS);								/* Return to the caller.		*/
}

char wscharat(r,c)									/* Get a character from a row and col.	*/
int r, c;
{
	return(data_map[mx(vml(r),c)]);							/* Get this character from the map.	*/
	
}


