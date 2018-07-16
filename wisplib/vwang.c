			/************************************************************************/
			/*		WISP - Wang Interchange Source Pre-processor		*/
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

#ifdef unix
#include <sys/types.h>
#else
#include <time.h>
#endif

#include "idsistd.h"
#include "cobrun.h"
#include "sub_char.h"									/* Include substitution table.		*/
#include "wperson.h"									/* Include struct of personality file.	*/
#include "werrlog.h"									/* Include error logging definitions.	*/
#include "vwang.h"									/* Include wisp/video interface defs.	*/
#include "wglobals.h"									/* Include wisp globals.		*/
#include "scnfacs.h"									/* Include the screen FAC definitions.	*/

#include <v/vchinese.h>

#define TAB_NORMAL	0
#define TAB_SPACEBAR	1
#define TAB_NEWLINE	2

struct wscrn_struct
{
	char	saved_aid;
	char	*saved_ws_atr;
	char	*saved_ws_at2;
	char	*saved_fac_table;
	char	*saved_toupper_table;
	char	*saved_numeric_table;
	int	saved_ws_good;
	int	saved_old_line;
	int	saved_old_column;
	int	saved_errset;
	int	saved_fast_read;
	int	saved_screen_width;
	int	saved_max_data_range;
	struct EDIT *saved_edit;
	time_t	saved_stop_time;
	struct	wscrn_struct *prev_wscrn_struct_ptr;
};

/*
**	Global data
*/
int netc_pfhelp = FALSE;								/* Flag to avoid reentrant help.	*/
extern int netroncap;									/* Flag for NetronCap screens.		*/
int rts_first = TRUE;									/* First time flag.			*/
int psb_rndt;										/* Rendition for pseudo blanks.		*/
int psb_chset;										/* Character set choice of pseudo blank.*/
int wcurwidth = WS_DEFAULT_COLUMNS_PER_LINE;						/* Current screen width.		*/
unsigned char psb_char;									/* Pseudo blank character.		*/
struct wscrn_struct *wscrn_stack = 0;							/* Global storage for the addr of the	*/
											/* next 'Wisp' screen data structure.	*/
/*
**	Static data.
*/
static unsigned char data_map[WS_MAX_LINES_PER_SCREEN * WS_MAX_COLUMNS_ON_A_LINE];	/* What Wang thinks is on the screen.	*/
static unsigned char attr_map[WS_MAX_LINES_PER_SCREEN * WS_MAX_COLUMNS_ON_A_LINE];	/* Wang attributes for each char pos.	*/

static int use_user_fac_table = FALSE;	  /* flag means use user's table in current vwang instance */
static int restore_fac_table = TRUE;	  /* flag means restore the default table */

static unsigned char fac_table[128];	  /* this is the fac table vwang always uses */
static unsigned char user_fac_table[128]; /* this is the fac table provided by the user */
static unsigned char def_fac_table[128] = /* this is the default fac table */
		{ 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F,
		  0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F,
		  0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF,
		  0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF,
		  0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF,
		  0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF,
		  0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF,
		  0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF 
		};

static unsigned char user_toupper_table[128];	      /* this is an up/low table since toupper() don't know about int'l stuff */
static unsigned char def_toupper_table[128] =	      /* this is an up/low table for DEC mappings */
		{ 
		  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		  0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF,
		  0x00, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0x00, 0x00
		};

static unsigned char user_numeric_table[128];	      /* this is an numeric table since isnum() doesn't know about int'l stuff */
static unsigned char def_numeric_table[128] =	      /* this is an numeric table for DEC mappings */
		{
		  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		  0x00, 0x01, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x01, 0x01, 0x00, 0x00, 0x00, 
		  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		};

static int ws_good = TRUE;								/* Assume first character is good.	*/
static int do_console_log = FALSE;							/* Set console logging to FALSE.	*/
static int old_line = 0;								/* Old (stored) cursor position.	*/
static int old_column = 0;
static int errset = FALSE;								/* Flag that an error bit was set.	*/
static int fast_read = FALSE;								/* Flag controls fast reads (no data).	*/
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

static unsigned char tab_map[WS_MAX_COLUMNS_ON_A_LINE]=
"	X   X	X   X	X   X	X   X	X					  X	   ";
static int vwang_tab_mode = 0;
static int cursor_on_multibyte=0;							/* flag indicating cursor is sitting on */
											/* a multibyte character		*/
static int multi_input_pos=0;								/* index into current multibyte ideogram*/

static int max_data_range;

#define EDIT_DATA_SIZE 2048
#define EDIT_MAX_ROWS 24
#define EDIT_MAX_COLS 80

struct EDIT
{
	char data_buf[EDIT_MAX_ROWS][EDIT_MAX_COLS];				/* this is an the actual data */
	int wrap_count[EDIT_MAX_ROWS];	

	char hilite_buf[EDIT_MAX_ROWS][EDIT_MAX_COLS];				/* this is a map of the hilited parts. */
	int  line_size[EDIT_MAX_ROWS];						/* this is the length of each line (on screen) */
										/* of each display line. */
	char cut_buf[EDIT_DATA_SIZE];						/* this is the buffer to hold the cut data */

	int  wrap_flag[EDIT_MAX_ROWS];						/* marks newlines */
	int  edit_row,edit_col;
	int  mark_row,mark_col;							/* offset of the mark into data[] */
	int  cut_bytes;								/* number of bytes in the cut buffer */
	int  mode_ins;								/* mode 1=insert 0=overwrite */
	int  wrapped_mid_word;
	int  wrap_moved_curs;
	
	int top_row, bottom_row;						/* boundaries of window */
	int left_col, right_col;
	int width, height;							/* dimensions of the window */
	int upper;								/* flag indicates upper only window */
};
#define WRAP_DOWN 1
#define WRAP_UP 2

#define FAC_EDIT_END 0xff
#define FAC_EDIT_UPPER 0xfd
#define FAC_EDIT_UPLOW 0xfc

struct EDIT *the_edit=NULL;
int in_edit();
void edit_init_struct();
void edit_init_window();
void edit_erase_box();
void edit_main();
void edit_compute_pos();
void edit_compute_rc();
void edit_putchar();
void edit_redraw();
void edit_cut_block();
void edit_paste_block();
void edit_copy_block();
void edit_show_hilite();
void edit_tab();
void edit_clear_before();
void edit_clear_after();

int edit_delleft();
int edit_delright();
void edit_unmark();
void edit_init_data();

static int ws_write();
static int ws_read();
static int tab_fac();
static int menu_pick_combo();
static int ws_trans_atr();
static int load_sub_chars();
static int load_user_sub_chars();
static int wsdmp_scrn();
static int set_cl_fl();
static int check_mp();
static int mx();
static int wsetwid();
static int mod_pb();
static int ws_posit();
static int ws_invisible();
static int ws_err_field();
static int ws_delete();
static int ws_deleft();
static int ws_delrt();
static int ws_next();
static int ws_last();
static int next_mp();
static int ws_insert();
static int ws_remove();
static int ws_tab_stop();
static int ws_bloff();
static int ws_echo();
static int sub_char();
static int rewrite_mp();

FILE *foo=NULL;
static char spaces[80]={' '};

static int edit_window = FALSE;

static time_t	stop_time();
static void	set_stop_time();
static int	timed_read();


#define ROUTINE		67000



/*
	vwang		Emulate the wang workstation.

			Note: On a close only "Function" is passed.
*/

int vwang(function, wsb, lines, terminate_list, term, no_mod)
unsigned char *function;
unsigned char *wsb;
unsigned char *lines;
unsigned char *terminate_list;
unsigned char *term;
unsigned char *no_mod;
{
	int	op_save;								/* Save current level of optimization.	*/
	char	psb_select;
	int	i;
	
	i = (int)*function;
	werrlog(ERRORCODE(1),i,0,0,0,0,0,0,0);

	vgeterr();									/* Clear any pre-existing video error	*/

#ifdef unix
	if (!ishelpactive() && isdebug())
	{
		/*
		**	This saves the current stty values so it can be restored by vstate(RESTORE_DEBUG);
		**	It then ensures the stty is in-sync with video's expectations in case the COBOL debugger has changed it.
		**	It then force video to resynchronize it's maps and redraw the screen. We assume that
		**	the debugger has filled the screen with source code etc. so each time in we want to
		**	completely redraw the screen.
		*/
		vstate(SAVE_DEBUG);
		if (!rts_first)
		{
			if ( (	READ_ALL	== *function ||
				READ_ALL_PB	== *function ||
				READ_ALTERED	== *function ||
				READ_MODIFIABLE == *function	)  ||
			   ( (	WRITE_ALL	== *function ||
				WRITE_ALL_PB	== *function ||
				WRITE_SELECTED	== *function	) && *lines != 24 )  )
			{
				/*
				**	Force a HARD REFRESH if we are doing a read or
				**	if doing a partial screen write.
				*/
				vdefer(RESTORE);
				vrefresh(HARD_REFRESH);
				vcontrol(DUMP_OUTPUT);
				synch_required = FALSE;
			}
			else
			{
				/*
				**	Force a clear of the screen to get rid of the debugger screen.
				*/
				synch_required = TRUE;
			}
		}
	}
#endif

	if (rts_first)									/* First time in program?		*/
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

	get_defs(DEFAULTS_PSB_REN,&psb_rndt);						/* Init the pseudo blank rendition.	*/
	get_defs(DEFAULTS_PSB_SET,&psb_chset);						/* Init the pseudo blank char set.	*/
	{
		char	def_psb_select;
		get_defs(DEFAULTS_PSB_CHAR,&def_psb_select);
		get_psb_char(def_psb_select,&psb_char,&psb_select);			/* Init the pseudo blank charcter.	*/
	}
	get_defs(DEFAULTS_AUTOTAB,&auto_tab);						/* Init the auto tab flag.		*/
	get_defs(DEFAULTS_AUTOMOVE,&auto_move);						/* Init the auto move flag.		*/
	get_defs(DEFAULTS_MP_CURSOR,&mp_cursor);					/* Init the menu pick cursor flag.	*/
	get_defs(DEFAULTS_BGCHANGE,&bgchange);						/* Init the background change flag.	*/
	get_defs(DEFAULTS_BGCOLOR,&bgcolor);						/* Init the background color.		*/

	fast_read = FALSE;								/* Default read to return data.		*/

	if (use_user_fac_table)								/* using the user fac table ?		*/
	{
		use_user_fac_table=FALSE;						/* clear the flag--it's a one time use	*/
		memcpy(fac_table,user_fac_table,sizeof(fac_table));			/* copy his table			*/
		restore_fac_table = TRUE;						/* restore the def table on next weewong*/
		max_data_range = EIGHT_BIT_DATA;
	}
	else
	{
		if (restore_fac_table)							/* if last call used user table, restore*/
		{
			memcpy(fac_table,def_fac_table,sizeof(fac_table));
			restore_fac_table=FALSE;					/* only need to do it once		*/
		}
		max_data_range = SEVEN_BIT_DATA;
	}

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
			break;								/* We're done.				*/
		}
		case READ_ALL_PB:							/* Read the screen with pseudo blank	*/
		{									/* processing.				*/
			ws_read(wsb,*lines,READ_ALL,terminate_list,term,no_mod,1,0);	/* Read the workstation.		*/
			break;								/* We're done.				*/
		}
		case READ_ALTERED:							/* Read altered part of screen?		*/
		{
			ws_read(wsb,*lines,READ_ALTERED,terminate_list,term,no_mod,0,0);/* Read it.				*/
			break;								/* Done...				*/
		}
		case READ_MODIFIABLE:							/* Read MODIFIABLE part of screen?	*/
		{
			ws_read(wsb,*lines,READ_MODIFIABLE,terminate_list,term,no_mod,0,1); /* Read it.				*/
			break;								/* Done...				*/
		}
		case DISPLAY_AND_READ:							/* Write then read screen?		*/
		{
			if (terminal_error) wsdmp_scrn(wsb,0);				/* Dump screen on errors.		*/
			else ws_write(wsb,WS_MAX_LINES_PER_SCREEN,0,no_mod,1);		/* Write full screen.			*/
			if (vwang_aid()==AID_LOCKED) fast_read = TRUE;			/* Just wrote so don't return data.	*/
			ws_read(wsb,WS_MAX_LINES_PER_SCREEN,READ_ALL,terminate_list,term,no_mod,1,1);
			break;								/* Don't fall through.			*/
		}
		case DISPLAY_AND_READ_ALTERED:						/* Write then read altered fields?	*/
		{
			if (terminal_error) wsdmp_scrn(wsb,0);				/* Dump screen on errors.		*/
			else ws_write(wsb,WS_MAX_LINES_PER_SCREEN,0,no_mod,1);		/* Write full screen.			*/
			if (vwang_aid()==AID_LOCKED) fast_read = TRUE;			/* Just wrote so don't return data.	*/
			ws_read(wsb,WS_MAX_LINES_PER_SCREEN,READ_ALTERED,terminate_list,term,no_mod,1,1);
			break;
		}
		case INIT_DEFAULT_TABLE:						/* Re-initialize the substitution table */
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
		case SET_TABS:
		{
			memcpy(tab_map,wsb,wcurwidth);
			++vwang_tab_mode;
			break;
		}
		case TAB_MODE_OFF:
		{
			vwang_tab_mode = 0;
			break;
		}
		default:								/* Everything else no good.		*/
		{
			werrlog(ERRORCODE(2),*function,0,0,0,0,0,0,0);			/* Invalid function			*/
			voptimize(op_save);						/* Restore optimization level.		*/
			return(FAILURE);						/* Return to the caller.		*/
		}
	}

#ifdef unix
	if (!ishelpactive() && isdebug()) vstate(RESTORE_DEBUG);			/* Restore the debugger term state.	*/
#endif

	voptimize(op_save);								/* Restore optimization level.		*/
	synch_required = FALSE;								/* A synch is not required now.		*/

	return(SUCCESS);								/* Return that we were successful.	*/
}



/*					Subroutine to display screen or line.							*/

static int ws_write(wsb, lines, selected, no_mod, do_pseudo)				/* Work station write.			*/
unsigned char *wsb;									/* Work station I/O buffer.		*/
unsigned char lines;									/* Number of lines to write.		*/
int selected;										/* Flag indicates selected field write. */
unsigned char *no_mod;									/* Pointer to no-modification flag.	*/
int do_pseudo;										/* Do Pesudo blank processing		*/
{
	extern char vgetc();								/* Reference get character routine.	*/
	int start_row;									/* Row for single line or scrolling.	*/
	int wcc;									/* Write control character.		*/
	int crs_row, crs_col;								/* Cursor position.			*/
	int lin_1st, col_1st;								/* First modifyable field position.	*/
	int lin_err, col_err;								/* First error field location.		*/
	unsigned char last_atr;								/* Last Wang attributes.		*/
	int last_mode;									/* Last VIDEO character rendition.	*/
	unsigned char c_orig,c_mapped;							/* Working character.			*/
	unsigned char xc;								/* Exchanged character value for 'c'.	*/
	unsigned char *dm, *am;								/* Working pointers to character maps.	*/
	int char_set;									/* Character set flag (font) for subst. */
	register int vrow;								/* Virtual row number from vml()	*/
	register int row_x, col_x;							/* Row and col index			*/
	int new_row;									/* Is there new data for curr row	*/
	unsigned char *map_multibyte();
	int row_actual;

	start_row = *wsb++;								/* get the row				*/
	if (start_row == 0) start_row = 1;						/* correct for row == 0			*/

	if ((start_row	< 1) || ((int)(start_row+lines-1) > WS_MAX_LINES_PER_SCREEN))	/* Test for valid values		*/
	{
		werrlog(ERRORCODE(4),start_row,lines,0,0,0,0,0,0);			/* Invalid Row	or Row+lines		*/
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
	if (wcc & UNLOCK_KEYBOARD) 
	{
		set_aid(AID_UNLOCKED);
	}
	else 
	{
		set_aid(AID_LOCKED);
	}

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
	for (row_actual = row_x = 0; row_x < WS_MAX_LINES_PER_SCREEN; row_x++)		/* Loop through each line.		*/
	{
		unsigned int scrbyte;
		
		if ((row_x >= start_row) && (row_x < (int)(start_row+lines)))		/* Is there data for this row?		*/
		  new_row = TRUE;
		else
		  new_row = FALSE;

		dm = &data_map[mx(row_x,0)];						/* Point to the current entry.		*/
		
		if (new_row)
		{
			(void)map_multibyte(dm,dm);					/* wipe multibyte data from map to	*/
											/* prevent optimization			*/
			for (col_x=0;  col_x < wcurwidth; ++col_x)
			{
				scrbyte = wsb[row_actual*wcurwidth+col_x];
				scrbyte = fac(scrbyte);
				if (scrbyte == FAC_EDIT_END || scrbyte == FAC_EDIT_UPPER || scrbyte == FAC_EDIT_UPLOW)
				{
					if (!the_edit)
					  edit_init_struct();
					
					switch (scrbyte)
					{
					      case FAC_EDIT_UPPER:
					      { 
						      the_edit->top_row = row_x;
						      the_edit->left_col = col_x+1;
						      the_edit->upper = TRUE;
						      if (lin_1st < 0)
						      {
							      lin_1st = row_x;
							      col_1st = col_x+1;
						      }
						      break;
					      }
					    case FAC_EDIT_UPLOW:
					    { 
						    the_edit->top_row = row_x;
						    the_edit->left_col = col_x+1;
						    the_edit->upper = FALSE;
						    if (lin_1st < 0)
						    {
							    lin_1st = row_x;
							    col_1st = col_x+1;
						    }
						    break;
					    }
					    case FAC_EDIT_END:
					    {
						    the_edit->bottom_row = row_x;
						    the_edit->right_col	  = col_x-1;
						    break;
					    }
				      }
					edit_window = TRUE;
				}
			}
			++row_actual;
		}
	}
	if (edit_window==TRUE)
	{
		if (the_edit->top_row < 0			  ||
		    the_edit->bottom_row < 0			  ||
		    the_edit->left_col < 0			  ||
		    the_edit->right_col < 0			  ||
		    the_edit->bottom_row < the_edit->top_row	  ||
		    the_edit->right_col < the_edit->left_col	  ||
		    the_edit->right_col > wcurwidth		  ||
		    the_edit->bottom_row > WS_MAX_LINES_PER_SCREEN 
		    )
		{
			edit_window=FALSE;
			free(the_edit);
			the_edit=NULL;
			lin_1st = -1;
			col_1st = -1;
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
		  new_row = TRUE;
		else
		  new_row = FALSE;

		for (col_x = 0; col_x < wcurwidth; col_x++)				/* Loop through each column.		*/
		{
			if (new_row)
			  c_orig = *wsb++;						/* Get data from input buffer.		*/
			else
			  c_orig = *dm;							/* Use the data map			*/

			c_mapped = fac(c_orig);						/* map it using fac table.  all FAC ops */
											/* must be done to c_mapped, not c_orig */

											/* Don't do anything if already in	*/
											/* map unless modified pseudo blank.	*/
			if (new_row && ((c_orig != *dm) || (last_atr != *am) || mod_pb(c_orig,dm,last_atr,am,do_pseudo)))
			{
				if (c_mapped == FAC_EDIT_END || c_mapped == FAC_EDIT_UPPER || c_mapped == FAC_EDIT_UPLOW)
				{
					c_mapped = ' ';
					ws_putbuff(' ',row_x,col_x,vrow,CLEAR,DEFAULT);/* Put in output buffer.		*/
					*dm = ' ';
				}
				else if (c_mapped & FAC_CHARACTER)			/* Is this character a FAC?		*/
				{
					if (*no_mod != 'E') 
					  c_mapped = c_mapped & ~FAC_ALTERED;		/* Clear any altered bits that are set. */
					if (c_mapped != *am)				/* Different only by altered bits?	*/
					{
						ws_putbuff(' ',row_x,col_x,vrow,CLEAR,DEFAULT);/* Put in output buffer.		*/
						*dm = c_orig;				/* Record the data.			*/
						*am = c_mapped;				/* Record the attributes.		*/
					}
					last_atr = c_mapped & ~FAC_CHARACTER;		/* Remember the other attributes.	*/
					last_mode = ws_trans_atr(c_mapped);		/* Remember the last rendition.		*/
				}
				else if (!selected || (last_atr & FAC_SELECTED))	/* Not FAC character, write if approp.	*/
				{
					*am = last_atr;					/* Record attributes.			*/
					*dm = c_orig;					/* Record the character.		*/

					if (ws_invisible(last_atr))			/* Should this field be invisible?	*/
					{
						ws_putbuff(' ',row_x,col_x,vrow,CLEAR,DEFAULT);
					}
					else if ( do_pseudo && !(last_atr & FAC_PROTECT) && 
						 ((wcc & ERASE_FIELDS) || (c_orig == ' ')))
					{
						ws_putbuff(psb_char,row_x,col_x,vrow,psb_rndt|last_mode,psb_chset);
						*dm = ' ';				/* Remember what char really is.	*/
					}
					else if ( do_pseudo && !(last_atr & FAC_PROTECT) && 
										(last_atr & FAC_NUMERIC_ONLY) && (c_orig == ' '))
					{
						ws_putbuff(psb_char,row_x,col_x,vrow,psb_rndt|last_mode,psb_chset);
						*dm = ' ';				/* Remember what char is.		*/
					}
					else if (c_orig == 0)				/* Is it a null?			*/
					{
						if (opt_nulldisplay)	ws_putbuff('.',row_x,col_x,vrow,last_mode,DEFAULT);
						else			ws_putbuff(' ',row_x,col_x,vrow,last_mode,DEFAULT); 
					}
					else if (c_orig == PSEUDO_BLANK_CHAR )		/* 0x0B - Wang solid box - pseudo blank.*/
					{
						ws_putbuff(psb_char,row_x,col_x,vrow,psb_rndt|last_mode,psb_chset);
					}
					else
					{
						sub_char(c_orig, &xc, &char_set);	/* Get character & font from sub table. */
						if (in_edit(row_x,col_x))
						{				
							ws_putbuff(xc,row_x,col_x,vrow,BOLD,char_set);
						}
						else if (!(last_atr & FAC_PROTECT))	/* Is this a modifiable field ?		*/
						{
							int mode;
							if (do_pseudo)	mode = psb_rndt|last_mode;
							else		mode = last_mode;
							ws_putbuff(xc,row_x,col_x,vrow,mode,char_set); /* Put char.		*/
						}
						else if ((xc != ' ') || (last_mode & (REVERSE|UNDERSCORE)))	/* Visible?	*/
						{
							int mode;

							if (xc >= ' ' || !do_pseudo)
								mode = last_mode;	/* If in the printable range.		*/
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
			else		 
			{
				if (c_mapped & FAC_CHARACTER)
				{
					last_atr = c_mapped & ~FAC_CHARACTER;		/* Remember the other attributes.	*/
					last_mode = ws_trans_atr(c_mapped);		/* Remember the last rendition.		*/
				}
			}

			if (new_row && lbdangle)						/* Are we dangling output?	*/
			{
				if (!((*dm == ' ') || (fac(*dm) & FAC_CHARACTER))) lbdangle = 0; /* Don't dangle if visible.	*/
				else if (last_mode & (REVERSE|UNDERSCORE|BLINK)) lbdangle = 0;	/* Visible whitespace too...	*/
			}

			if (((!(*am & FAC_CHARACTER)) && (lin_1st < 0) && (row_x >= crs_row) && ws_tab_stop(last_atr)) ||
				((lin_1st <0) && ((fac(c_orig)==FAC_EDIT_UPPER) || (fac(c_orig)==FAC_EDIT_UPLOW))))
												/* 1st tabstop?			*/
			{
				lin_1st = row_x;						/* Yes, remember it.		*/
				col_1st = col_x;						/* Column too.			*/
			}

			if ((*am & FAC_CHARACTER) && (lin_err < 0) && ws_err_field(*am))    /* Is there an error in this field? */
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

	if (the_edit)
	{
		edit_init_window();
	}

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

static int ws_read(wsb, lines, read_mode, terminate_list, pfkey, no_mod, do_pseudo, pseudo2space)
unsigned char *wsb;
unsigned char lines;
int read_mode;
unsigned char *terminate_list;
unsigned char *pfkey;
unsigned char *no_mod;
int do_pseudo;
int pseudo2space;									/* Convert pseudo blanks to spaces	*/
{
	extern char vgetc();								/* Reference get character routine.	*/
	unsigned char *wsbr;								/* Pointer to position return area.	*/
	unsigned char *wsbo;								/* Pointer to original screen from Wang.*/
	int	filling;								/* Flag to indicate screen is filling.	*/
	int	row;									/* Row to start reading.		*/
	int	at_edge;								/* Flag to indicate at edge of screen.	*/
	register int i,j,k,m;								/* Working registers.			*/
	unsigned char c, *dwsb;								/* Working character.			*/
	int	tab_type;								/* Indicate space bar, tab key, newline.*/
	int	alt_read;								/* Indicate this is an altered read.	*/
	int	v_error;								/* Error number from video		*/
	long	seconds_remaining;							/* The seconds remaining in a timed read*/
	
	int	eofield, multiblock=0;							/* end of cur field, number of bytes to */
											/* block (attempt to inp multi w/o room)*/
	unsigned char c_orig, c_mapped;
	
	if (read_mode == READ_ALTERED) alt_read = 1;					/* Set/clear altered flag.		*/
	else			       alt_read = 0;

	wsbo = wsb;									/* Set ptr to original screen from Wang.*/
	dwsb = wsb + 4;									/* Set pointer to dump screen.		*/

	multi_input_pos=0;

	row = *wsb++;									/* Get the row				*/
	if ( row == 0 ) row = 1;							/* Correct for row = 0			*/
	if ((row < 1) || ((int)(row + lines - 1) > WS_MAX_LINES_PER_SCREEN))		/* Validate values.			*/
	{
		werrlog(ERRORCODE(8),row,lines,0,0,0,0,0,0);				/* Invalid Row	or Row+lines		*/
		return(FAILURE);
	}
	row -= 1;									/* Make row zero based			*/
	wsbr = wsb + 1;									/* Remember where column position is.	*/
	wsb = wsb + 3;									/* Move to the data area.		*/
	if (alt_read == 1) *no_mod = 'N';						/* Assume no modifications.		*/
	menu_fl = check_mp(old_line,old_column,BOLD,do_pseudo);				/* If a menu pick then BOLD it.		*/
	if (mp_cursor)									/* If usage constant is set to display. */
	{
		vset(CURSOR,VISIBLE);							/* Set cursor on.			*/
		cur_on_fl = TRUE;
	}
	filling = TRUE;									/* And now to fill the screen.		*/
	if (vwang_aid() == AID_UNLOCKED)						/* Don't get input if keyboard locked.	*/
	{
		while (filling)								/* Repeat until an exit event.		*/
		{
			if (!mp_cursor)							/* If no cursor on menu, check status.	*/
			{
				if (!menu_fl)						/* If not at menu pick and cursor is	*/
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

			if (timed_read(&seconds_remaining))				/* If doing a timed read.		*/
			{
				int	timed_out;

				if (seconds_remaining > 0)				/* If time remaining			*/
				{
					i = vgetm_timed((int)seconds_remaining,&timed_out);	/* Do a timed read		*/
				}
				else
				{
					timed_out = TRUE;				/* We have timed out			*/
				}

				if (timed_out)
				{
					memcpy(pfkey,"	",2);				/* Set pfkey to BLANKS.			*/
					filling = FALSE;				/* Done filling.			*/
					i = 0;						/* Dummy the meta character		*/
					continue;					/* Break out of this loop		*/
				}
			}
			else	/* Do a normal non-timed read */
			{
				i = vgetm();						/* Get a meta character.		*/
			}

			if (v_error = vgeterr())					/* Check if an error occured		*/
			{
				werrlog(ERRORCODE(24),v_error,0,0,0,0,0,0,0);		/* Report error on video read		*/
				wexit(ERRORCODE(24));					/* Exit the process			*/
			}

			if (outctx)							/* using a language translation file?	*/
			{								/* if so, then keep track of any multi	*/
											/* byte sequences that come thru the	*/
											/* input stream				*/
				if (multi_input_pos == 3)				/* got 3rd byte?			*/
				  multi_input_pos=0;					/* yes, then start over			*/
				if (i < 256)						/* make sure it's within table size	*/
				{
					/* check to see if its a valid byte in a multi sequence */
					if ((outctx->indbuf)[i+(multi_input_pos<<8)]==0xff)   /* 0xff means not valid		*/
					{
						multi_input_pos=0;
					}
					else
					  ++multi_input_pos;				/* else bump the counter		*/
				}
				else
				  multi_input_pos=0;
			}

			if (menu_fl && i == SPACE_BAR)					/* If at a menu pick and hit space bar. */
			{
				i = tab_key;						/* Change SPACEBAR into TABKEY		*/
				tab_type = TAB_SPACEBAR;				/* Hit the space bar from a menu pick.	*/
			}
			else tab_type = TAB_NORMAL;					/* Set to normal tab actions.		*/

			if (i == 0)							/* A timeout?				*/
			{
				/*
				**	This is to handle the case of vgetm() returning a NULL char.
				**	This will probably never happen but if it does it means a timeout has occurred.
				*/
				memcpy(pfkey,"	",2);					/* Set pfkey to BLANKS.			*/
				filling = FALSE;					/* Done filling.			*/
			}
			else if (in_edit(vcur_lin,vcur_col))
			{
				edit_main(i,vcur_lin,vcur_col,&filling,terminate_list,pfkey,no_mod);
			}
			else if (menu_fl && i >= SPACE_BAR && i < max_data_range)	/* At menu pick and i is displayable.	*/
			{
				next_mp(vcur_lin,vcur_col,i,do_pseudo);			/* Search next menu pick beg. with i.*/
			}
			else if (auto_move && (i >= SPACE_BAR) && (i < max_data_range) && !ws_mod(k,vcur_col))
			{
				ws_next(vcur_lin,vcur_col,TAB_NORMAL,do_pseudo);
			}
			else if (((i >= SPACE_BAR) && (i < max_data_range)) && ws_mod(k,vcur_col)) 
			{								/* Displayable char in mod pos?		*/
				at_edge = FALSE;					/* Not at edge.				*/
				if (attr_map[mx(k,vcur_col)] & FAC_NUMERIC_ONLY)	/* Digits only?				*/
				{
					if (max_data_range == SEVEN_BIT_DATA || i<=SEVEN_BIT_DATA)
					{
						if (isdigit(i) || (i == '.') || (i == '+') || (i == '-') || (i == ' '))
						{
							m = vcur_col;					   /* Remember.	 */
							ws_echo(i,k,vcur_col,alt_read,no_mod,do_pseudo);   /* Echo digit.*/
							if ((vcur_col == vscr_wid-1) && (vcur_col == m))   /* At edge?	 */
							  at_edge=TRUE; 
						}
						else ws_bad_char();					   /* Bells toll.*/
					}
					else
					{
						if (user_numeric_table[i-128] || (i == '.') || 
						    (i == '+') || (i == '-') || (i == ' '))
						{
							m = vcur_col;					   /* Remember.	 */
							ws_echo(i,k,vcur_col,alt_read,no_mod,do_pseudo);   /* Echo digit.*/
							if ((vcur_col == vscr_wid-1) && (vcur_col == m))   /* At edge?	 */
							  at_edge=TRUE; 
						}
						else ws_bad_char();					   /* Bells toll.*/
					}
				}
				else										/* Ok insert.	*/
				{
					/* if user starts a multibyte, make sure there's room in the field */
					if (multi_input_pos==1 && (ws_eof(k,vcur_col) - vcur_col)<(outctx->srcsize-1))
					{
						multiblock=outctx->srcsize;  /* setup to block the bytes */  
					}
					if (multiblock)					/* are we blocking a multi sequence?	*/
					{
						ws_bad_char();				/* yup, beep  */
						--multiblock;				/* and decrement */
						continue;
					}
					if ((attr_map[mx(k,vcur_col)] & FAC_UPPERCASE_ONLY) && !multi_input_pos) 
					{
						if (max_data_range == SEVEN_BIT_DATA || i <= SEVEN_BIT_DATA)
						{
							i = toupper(i); /* Uppercase?	*/
						}
						else
						{
							if (user_toupper_table[i-128] > 0)
							  i = user_toupper_table[i-128];
						}
					}
					
					m = vcur_col;								/* Remember col */
					ws_echo(i,k,vcur_col,alt_read,no_mod,do_pseudo);			/* Yes, echo.	*/
					if ((vcur_col == vscr_wid-1) && (vcur_col == m)) at_edge = TRUE;	/* At edge?	*/
				}
				if (at_edge || (attr_map[mx(k,vcur_col)] & FAC_CHARACTER))			/* Full?	*/
				{
					if (auto_tab) ws_next(vcur_lin,vcur_col,TAB_NORMAL,do_pseudo);		/* Yes,autotab. */
					else if (!at_edge) ws_posit(0,-1,do_pseudo);				/* No, stay.	*/

				}
			}
			else if (i == newline_key)						/* Hi the Wang EXECUTE key.	*/
			{
				ws_next(vcur_lin,vcur_col,TAB_NEWLINE,do_pseudo);
			}
			else if ((i == return_key) || (i == enter_key)) ws_fkey(0,&filling,terminate_list,pfkey,no_mod);
			else if (vfnkey(i))		ws_fkey(i,&filling,terminate_list,pfkey,no_mod);
			else if (i == up_arrow_key)	ws_posit(-1, 0,do_pseudo);
			else if (i == down_arrow_key)	ws_posit( 1, 0,do_pseudo);
			else if (i == left_arrow_key)	ws_posit( 0,-1,do_pseudo);
			else if (i == right_arrow_key)	ws_posit( 0, 1,do_pseudo);
			else if (i == tab_key)
			{
				/* decide if tab goes to next field or next tab stop */
				if (vwang_tab_mode)
				{
					int cnt;
					
					/* compute offset to next tab stop */
					for (cnt=1; tab_map[vcur_col+cnt] == ' ' && (vcur_col+cnt)<80 ; ++cnt);
					/* call ws_posit to do the work */
					if (vcur_col+cnt<80)
					  ws_posit( 0, cnt, do_pseudo);
					else
					{
						/* wrap to start of next line */
						for (cnt=0; tab_map[cnt] == ' ' && cnt < 80 ; ++cnt);
						if (cnt<80)
						  ws_posit( 1, cnt - vcur_col, do_pseudo);
					}
				}
				else
				{
					ws_next(vcur_lin,vcur_col,tab_type,do_pseudo);
				}
			}
			else if (i == backtab_key)	ws_last(vcur_lin,vcur_col,do_pseudo);
			else if (i == help_key)
			{
				if (ishelpactive())							/* Is help active?	*/
				{
					strcpy((char *)pfkey,(char *)"FE");				/* Return code FE.	*/
					no_mod[1] = AID_HELP;						/* Mode code too.	*/
					filling = FALSE;						/* Now we're done.	*/
				}
				else 
				{
					ws_bar_menu(ON,vcur_lin,vcur_col,k,alt_read,no_mod,do_pseudo);

					/* reset this flag that may be changed, Help style two will leave this 
					 * set since it's menupick based instead of PFkey based.  
					 */
					menu_fl = check_mp(vcur_lin,vcur_col,BOLD,do_pseudo); 
				}
			}
			else if (i == clear_field_key)	ws_clear(vcur_lin,vcur_col,k,alt_read,no_mod,do_pseudo);
			else if (i == clear_before_key) ws_deleft(vcur_lin,vcur_col,k,alt_read,no_mod,do_pseudo);
			else if (i == clear_after_key)	ws_delrt(vcur_lin,vcur_col,k,alt_read,no_mod,do_pseudo);
			else if (i == home_key)	  ws_next(0,0,TAB_NORMAL,do_pseudo);
			else if (i == delete_key) ws_delete(k,vcur_col,alt_read,no_mod,do_pseudo);	    /* delete b4 cursor */
			else if (i == insert_key) ws_insert(vcur_lin,vcur_col,k,alt_read,no_mod,do_pseudo); /* open-under-cursor*/
			else if (i == remove_key) ws_remove(vcur_lin,vcur_col,k,alt_read,no_mod,do_pseudo); /* delete & closeup */
			else if ((i == cancel_key) && errset)
			{
				ws_bloff(do_pseudo);					/* Go eliminate blinking fields.	*/
				errset = FALSE;						/* Error fields are now gone.		*/
			}
			else if (netroncap && i == trigger1)
			{
				nc_pop_menu(&filling,terminate_list,no_mod,pfkey);	/* Pop up window and get pfkey.		*/
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
				unsigned char ufac;

				c_orig = data_map[mx(k,j)];				/* Get this character from the map.	*/
				c_mapped = attr_map[mx(k,j)];				/* Get the mapped one too.		*/

				if (c_mapped & FAC_CHARACTER)				/* use the mapped for FAC comparison	*/
				{
					if (!ws_invisible(c_mapped) && !(c_mapped & FAC_PROTECT))	/* Mod visible FAC?	*/
					{
						c_mapped &= ~FAC_BLINKING;
					}
					ufac = unfac(c_mapped);				/* now unmap it back using user's table */
					if (ufac == 0xff)			/* there is no fac in his table so use original */
					  ufac=c_orig;
				}
				else 
				  ufac=0;
				if (c_orig == 0x0b && pseudo2space && 
					!(c_mapped & FAC_PROTECT)	)		/* Do we change pseudo to space ?	*/ 
				{
					*wsb++ = ' ';					/* Change PB to Spaces.			*/
				}
				else *wsb++ = ufac?ufac:c_orig;				/* Else return the character.		*/
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

	if (vwang_aid() != AID_UNLOCKED)						/* If we are not timed out.		*/
	{
		if (do_console_log) wsdmp_scrn(dwsb,1);					/* If console logging is on.		*/
	}

	if (the_edit)
	{
		edit_erase_box();
		free(the_edit);
		the_edit=NULL;
		edit_window=FALSE;
	}
	vwang_timeout(0);								/* Cancel any timeout			*/
	return(SUCCESS);								/* Return to the caller.		*/
}




static int mod_pb(c, dm, last_atr, am, do_pseudo)
unsigned char c;
unsigned char *dm;									/* Working pointers to character maps.	*/
unsigned char last_atr;
unsigned char *am;
int do_pseudo;										/* Is it a modified pseudo blank?	*/
{
	if (netroncap || !do_pseudo || c != ' ') return(FALSE);				/* Only test if a DISPLAY AND READ,	*/
											/* or not a space. FALSE.		*/
	if (!(last_atr & FAC_PROTECT)) return(TRUE);					/* Is this a modifiable field ?		*/
	else return(FALSE);
}											/* pseudo blank.			*/

static ws_posit(up, right, do_pseudo)							/* Position cursor.			*/
int up;
int right;
int do_pseudo;									
{
	register int row,col,idx;							/* Working registers.			*/
	
	row = vcur_lin + up;								/* Take temp copy of position.		*/
	if (row < 0) row = WS_MAX_LINES_PER_SCREEN - 1;					/* Wrapped up?				*/
	else if (row >= WS_MAX_LINES_PER_SCREEN) row = 0;				/* Wrapped down?			*/

	col = vcur_col + right;								/* Compute new column.			*/
	if (col < 0) col = wcurwidth - 1;						/* Wrapped left?			*/
	else if (col >= wcurwidth) col = 0;						/* Wrapped right?			*/

	col = adjust_for_multibyte(row,col,up,right);

	check_mp(vcur_lin,vcur_col,CLEAR,do_pseudo);					/* Unbold current menu pick item.	*/
	menu_fl = check_mp(row,col,BOLD,do_pseudo);					/* Check if is a menu pick item.	*/

	
	return(SUCCESS);								/* Return to the caller.		*/
}

static int ws_invisible(ch)								
unsigned char ch;									/* Determine if field is displayable.	*/
{
	register int i;									/* Working register.			*/

	i = FALSE;									/* Assume visible.			*/
	if((ch & FAC_LOW_INTENSITY) && (ch & FAC_BLINKING)) i = TRUE;			/* Not visible if both low and blink.	*/
	return(i);									/* Return the result.			*/
}

static int ws_err_field(ch)
unsigned char ch;									/* Determine if field has an error.	*/
{
	register int i;									/* Working register.			*/

	i = FALSE;									/* Assume not an error field.		*/
	if (!ws_invisible(ch) && (ch & FAC_BLINKING) && (!(ch & FAC_PROTECT)))		/* Is it an error field?		*/
	{
		i = TRUE;								/* Yes, so flag as true.		*/
	}
	return(i);									/* Return the result.			*/
}

static ws_tag_alt(alt_read, k, j, no_mod)
int alt_read;
int k;
int j;
unsigned char *no_mod;									/* Set flags on altered fields.		*/
{
	if (alt_read)
	{	/* jec: must use attr map for fac comparison */
		while (!(attr_map[mx(k,j)] & FAC_CHARACTER))				/* Loop to find the start FAC.		*/
		{
			if ((--j) < 0)							/* Move left one column.		*/
			{
				werrlog(ERRORCODE(10),0,0,0,0,0,0,0,0);			/* No FAC for current field		*/
				return(FAILURE);
			}
		}
		attr_map[mx(k,j)] = attr_map[mx(k,j)] | FAC_ALTERED;			/* Or in the fact that it is altered.	*/
		*no_mod = 'M';								/* Return the global modified tag.	*/
	}
	return(SUCCESS);								/* Oilswell.				*/
}

/*** NOTE: this is not a static because it is also used in EDE (edenetc.c) ***/
int ws_fkey(metachar_key, filling, terminate_list, pfkey, no_mod)
int metachar_key;
int *filling;
unsigned char *terminate_list;
unsigned char *pfkey;
unsigned char *no_mod;
{
	char temp[3];									/* Temporary working string.		*/
	register int j;									/* Working register.			*/
	int	tl_num;
	int	cnt;									/* Counter				*/
	int	invalid_list;								/* Flag if term_list in invalid		*/

	invalid_list = 0;
	/*
	**	NOTE: If enter key then metachar_key has been dummied up with a 0.
	*/
	sprintf(temp,"%02d", vfnkey(metachar_key));					/* Convert to ascii representation.	*/
	
	if (terminate_list[0] == 'A') *filling = FALSE;					/* 'A' means all keys allowed.		*/

	j = 0;										/* Loop from start of terminate_list.	*/
	cnt = 0;

	while (*filling)								/* Loop until end of list or valid key. */
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

		if (metachar_key) /* metachar_key has been dummied up with a 0 for ENTER key */
			no_mod[1] = meta_aid(metachar_key);				/* Return the correct AID byte value.	*/
		else
			no_mod[1] = AID_ENTER;
		set_aid(no_mod[1]);							/* Set the AID char (lock keyboard)	*/
	}

	return(SUCCESS);								/* Return to the caller.		*/
}

int ws_mod(vrow, cursor_col)
register int vrow;
register int cursor_col;								/* Determine if char is modifyable.	*/
				
{
	return( !(attr_map[mx(vrow,cursor_col)] & (FAC_PROTECT | FAC_CHARACTER)));	/* Not modifable if protected or a FAC. */
}

static int ws_delete(k, j, alt_read, no_mod, do_pseudo)
int k;
int j;
int alt_read;
unsigned char *no_mod;
int do_pseudo;										/* Delete the previous character.	*/
{
	int multibyte_save;
	extern struct xlcontext *outctx;
	int mbyteind;
	
	if ((j-1 < 0) || !ws_mod(k,j-1)) ws_bad_char();					/* Beep if nothing to delete.		*/
	else
	{
		if (do_pseudo)
		{
			wsmode(psb_rndt | ws_trans_atr(attr_map[mx(k,j-1)]));		/* Select the needed rendition.		*/
			wscset(psb_chset);						/* Set to needed character set.		*/
			ws_posit(0,-1,do_pseudo);					/* Move Left 1 position.		*/

			/* cursor_on_multibyte is set elsewhere */
			multibyte_save=cursor_on_multibyte;				/* need to save value, because it will	*/
											/* be cleared after ws_posit below	*/

			if (cursor_on_multibyte)			    /* if cursor is on a multibyte, erase several bytes */
			{
				/* don't worry about outctx being referenced here if chinese support not 
				   enabled, cursor_on_multibyte will never be true if outctx is false */
				for (mbyteind=0; mbyteind < (outctx->srcsize); ++mbyteind)
				  vputc(psb_char);					/* Output pseudo blank			*/

				for (mbyteind=0; mbyteind < (outctx->srcsize); ++mbyteind) /* now position back			*/
				  ws_posit(0,-1,do_pseudo);				/* this clears cursor_on_multibyte	*/
			}
			else
			{
				vputc(psb_char);					/* Output the character.		*/
				ws_posit(0,-1,do_pseudo);				/* Move Left 1 position again.		*/
			}
			wscset(DEFAULT);						/* Set back to normal.			*/
		}
		else
		{	/* same as above,  but use space not pseudoblank */
			
			wsmode(ws_trans_atr(attr_map[mx(k,j-1)]));			/* Select the needed rendition.		*/
			ws_posit(0,-1,do_pseudo);					/* Move Left 1 position.		*/
			multibyte_save=cursor_on_multibyte;
			if (cursor_on_multibyte)
			{
				for (mbyteind=0; mbyteind < (outctx->srcsize); ++mbyteind)
				  vputc(' ');						/* Output the character		*/
				for (mbyteind=0; mbyteind < (outctx->srcsize); ++mbyteind)
				  ws_posit(0,-1,do_pseudo);
			}
			else
			{
				vputc(' ');						/* Output the character.		*/
				ws_posit(0,-1,do_pseudo);				/* Move Left 1 position again.		*/
			}
		}
		if (multibyte_save)	  /* now put spaces in the data_map */
		{
			for (mbyteind=1; mbyteind <= (outctx->srcsize); ++mbyteind)
			  if (j>mbyteind) 
			  {
				  data_map[mx(k,j-mbyteind)] = ' ';				
				  ws_tag_alt(alt_read,k,j-mbyteind,no_mod);		/* Set altered flags.			*/
			  }
		}
		else
		{
			data_map[mx(k,j-1)] = ' ';					/* Remember what it really is.		*/
			ws_tag_alt(alt_read,k,j,no_mod);				/* Set altered flags.			*/
		}
	}
	return(SUCCESS);								/* Back, back, back...			*/
}
#ifdef OLD
static int ws_delete(k,j,alt_read,no_mod,do_pseudo)					/* Delete the previous character.	*/
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
#endif
static int ws_deleft(i, j, k, alt_read, no_mod, do_pseudo)
int i;
int j;
int k;
int alt_read;
unsigned char *no_mod;
int do_pseudo;										/* Clear field cursor left.	*/
{
	register int c, d, m, n, jd,mode;						/* Working registers.			*/

	if ((m = ws_sof(k,j-1)) && (n = ws_eof(k,j-1)))					/* Should we delete left?		*/
	{
		wsmove(i,m);								/* Move to the start of the field.	*/
		d = 0;									/* Initizlize map column.		*/
		for (c = m; c <= n; c++)						/* Loop from start of field.		*/
		{
			jd = j+d;
			if (do_pseudo)	mode = psb_rndt | ws_trans_atr(attr_map[mx(k,c)]);
			else		mode = ws_trans_atr(attr_map[mx(k,c)]);
			wsmode(mode);							/* Select the needed rendition.		*/
			if ((jd) > n)
			{
				if (do_pseudo)
				{
					wscset(psb_chset);				/* Set the needed character set.	*/
					vprint("%c",psb_char);				/* Output pseudos if past end of field. */
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
				if (vatr_map[k][jd] & GRAPHICS)	 wscset(GRAPHICS);	/* If Graphics bit set, change char set.*/
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

static int ws_delrt(i, j, k, alt_read, no_mod, do_pseudo)
int i;
int j;
int k;
int alt_read;
unsigned char *no_mod;
int do_pseudo;										/* Delete to the right of the cursor.	*/
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

int ws_clear(cursor_row, cursor_col, vrow, alt_read, no_mod, do_pseudo)			/* Delete to the current field		*/
int cursor_row;										/* The cursor position			*/
int cursor_col;
int vrow;										/* Virtual row from vml()		*/
int alt_read;
unsigned char *no_mod;
int do_pseudo;			
{
	int	sof_col;								/* Start of field column		*/
	int	eof_col;								/* End of field column			*/
	int	wrk_col,  mode;								/* Working registers.			*/

	if ((sof_col = ws_sof(vrow,cursor_col)) && (eof_col = ws_eof(vrow,cursor_col))) /* Are we in a mod field		*/
	{
		wsmove(cursor_row,sof_col);						/* Move to where to clear from.		*/
		for (wrk_col = sof_col; wrk_col <= eof_col; wrk_col++)			/* Loop from start of field.		*/
		{
			if (do_pseudo) mode = ws_trans_atr(attr_map[mx(vrow,wrk_col)]) | psb_rndt;
			else	       mode = ws_trans_atr(attr_map[mx(vrow,wrk_col)]);
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

static int ws_next(start_row, start_col, tab_type, do_pseudo)
int start_row;
int start_col;
int tab_type;
int do_pseudo;										/* Move to the next field.		*/
{
	int	vrow, x_row, x_col;

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
				(tab_type == TAB_NEWLINE && (x_row != start_row || x_col < start_col))	)
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
static int ws_last(start_row, start_col, do_pseudo)
int start_row;
int start_col;
int do_pseudo;										/* Move to the last field.	*/
{
	int	vrow, x_row, x_col;
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
static int next_mp(start_row, start_col, the_char, do_pseudo)
int start_row;
int start_col;
int the_char;
int do_pseudo;										/* Move to the next menu pick.		*/
{
	int	vrow, x_row, x_col;

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
static int ws_insert(cursor_row, cursor_col, vrow, alt_read, no_mod, do_pseudo)
int cursor_row;
int cursor_col;
int vrow;
int alt_read;
unsigned char *no_mod;
int do_pseudo;										/* Open a column.			*/
{
	int wrk_col, eof_col, mode;							/* Working registers.			*/
	unsigned char *rowbuf, *map_multibyte();
	int	multi_value, multi_idx;
	int saved, saveind;
	unsigned char savedata[80], savevchr[80], saveattr[80];

	rowbuf = map_multibyte(&data_map[mx(cursor_row,0)],NULL);		       /* map out multibyte chars on the line  */
	
	if (eof_col = ws_eof(cursor_row,cursor_col))				       /* Are we in a field?			*/
	{
		if ( vchr_map[vrow][eof_col] != ' ' && 
		     vchr_map[vrow][eof_col] != psb_char && 
		    !(vatr_map[vrow][eof_col] & psb_chset) )
		{
			ws_bad_char();							/* Don't push chars out of the field.	*/
		}
		wsmove(cursor_row,cursor_col);						/* Move to where to insert the space.	*/
		vset(CURSOR,INVISIBLE);
		cur_on_fl=FALSE;

		if (rowbuf[wrk_col=eof_col])
		{
			while (rowbuf[wrk_col]==rowbuf[eof_col])
			{
				data_map[mx(cursor_row,wrk_col)] = do_pseudo?PSEUDO_BLANK_CHAR:' ';
				--wrk_col;
			}
		}

		memcpy(savedata,							/* save it */
		       &data_map[mx(cursor_row,cursor_col)],
		       saved = eof_col-cursor_col);
		memcpy(savevchr,
		       &vchr_map[vrow][cursor_col],
		       saved);
		memcpy(&data_map[mx(cursor_row,cursor_col+1)],			       /* now adjust vwang's maps	       */
		       savedata,
		       saved);
		memcpy(saveattr,							/* save it */
		       &attr_map[mx(cursor_row,cursor_col)],
		       saved);
		memcpy(&attr_map[mx(cursor_row,cursor_col+1)],
		       saveattr,
		       saved);
		data_map[mx(cursor_row,cursor_col)] = ' ';			/* insert a blank char in the map.		*/
		if (do_pseudo) 
		  mode = psb_rndt | ws_trans_atr(attr_map[mx(cursor_row,cursor_col)]);
		else	       
		  mode = ws_trans_atr(attr_map[mx(cursor_row,cursor_col)]);
		wsmode(mode);							/* Select the needed rendition.		*/

		if (do_pseudo)
		{
			wscset(psb_chset);					/* Set to needed character set.		*/
			vprint("%c",psb_char);					/* Now output the opened space.		*/
			wscset(DEFAULT);					/* Set back to normal.			*/
		}
		else
		{
			vprint(" ");						/* print the inserted space */
		}
		/* now loop thru the video maps and print the contents a space to the right. */
		/* the savechar and saveattr temp vars are needed because printing vmap[x] at position x+1 */
		/* destroys the value at x+1, we still need */
		for (saveind = 0 ; saveind < saved; ++saveind)
		{
			int outchar;
			
			if ( savedata[saveind] == PSEUDO_BLANK_CHAR )
			{
				wsmode(mode|psb_rndt);				/* Select the needed rendition.		*/
			}
			else
			{
				wsmode(mode);					/* Select the needed rendition.		*/
			}
			
			if (saveattr[saveind] & GRAPHICS)			/* If Graphics bit set,			*/
			  wscset(psb_chset);					/* change char set.			*/
			if ((outchar=savedata[saveind])==' ' && savevchr[saveind]==psb_char)
			  outchar = psb_char;
			if (outchar == PSEUDO_BLANK_CHAR)
			{
				outchar = psb_char;
				data_map[mx(cursor_row,cursor_col+saveind+1)] = ' ';
			}
			vputc(outchar);						/* Output the character.	*/
			wscset(DEFAULT);					/* Set back to normal.		*/
		}
#ifdef OLD
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
			for (m = j+1; m <= n; m++)					/* Loop through the rest of characters. */
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
#endif
		wsmove(cursor_row,cursor_col);						/* Move back to insert pos		*/
		ws_tag_alt(alt_read,vrow,cursor_col,no_mod);				/* Set altered flags.			*/
		vset(CURSOR,VISIBLE);
		cur_on_fl=TRUE;
	}
	else ws_bad_char();								/* Not in field so just beep.		*/

	return(SUCCESS);
}

static int ws_remove(cursor_row, cursor_col, vrow, alt_read, no_mod, do_pseudo)		/* Remove a character and close-up	*/
int cursor_row;										/* cursor position			*/
int cursor_col;
int vrow;										/* virtual row from vml()		*/
int alt_read;
unsigned char *no_mod;
int do_pseudo;										
{
	int	eof_col;								/* end of field column			*/
	int	wrk_col;								/* working column			*/
	int	mode;									/* Working registers.			*/
	unsigned char *rowbuf, *map_multibyte();
	int	multi_value, multi_idx;
	
	rowbuf = map_multibyte(&data_map[mx(vrow,0)],NULL);			 /* map out multibyte chars on the line	 */
	
	multi_value = rowbuf[cursor_col];
	if (multi_value)
	  multi_idx = outctx->srcsize;
	else
	  multi_idx = 1;
	if (eof_col = ws_eof(vrow,cursor_col))				 /* Should we delete right?		*/
	{
		if (do_pseudo) mode = ws_trans_atr(attr_map[mx(cursor_row,cursor_col)]) | psb_rndt;
		else	       mode = ws_trans_atr(attr_map[mx(cursor_row,cursor_col)]);

		memcpy(&data_map[mx(cursor_row,cursor_col)],				/* do a fast mod of the maps  */
		       &data_map[mx(cursor_row,cursor_col+multi_idx)],
		       eof_col-cursor_col-multi_idx+1);
		
		memcpy(&attr_map[mx(cursor_row,cursor_col)],
		       &attr_map[mx(cursor_row,cursor_col+multi_idx)],
		       eof_col-cursor_col-multi_idx+1);
		
		vset(CURSOR,INVISIBLE);
		cur_on_fl=FALSE;

		/* now reprint the line without the removed character */
		for (wrk_col = cursor_col; wrk_col <= eof_col; ++wrk_col)
		{
			if (wrk_col > eof_col-multi_idx)
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
				if ( data_map[mx(vrow,wrk_col)] == PSEUDO_BLANK_CHAR )
				{
					wsmode(mode|psb_rndt);				/* Select the needed rendition.		*/
				}
				else
				{
					wsmode(mode);					/* Select the needed rendition.		*/
				}
				
				if (vatr_map[vrow][wrk_col+multi_idx] & GRAPHICS)	/* If Graphics bit set,			*/
				  wscset(psb_chset);					/* change char set.			*/
				vputc(vchr_map[vrow][wrk_col+multi_idx]);		/* Output the character.	*/
				wscset(DEFAULT);						/* Set back to normal.		*/
			}
		}
		wsmove(cursor_row,cursor_col);					/* Move to the start of the field.	*/
		ws_tag_alt(alt_read,vrow,cursor_col,no_mod);				/* Set altered flags.			*/
		vset(CURSOR,VISIBLE);
		cur_on_fl=TRUE;
	  }
	  else ws_bad_char();								/* Can't delete so beep.		*/
	return(SUCCESS);
#ifdef OLD
	int	mode;									/* Working registers.			*/

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

				if (vatr_map[vrow][wrk_col+1] & GRAPHICS)		/* If Graphics bit set,			*/
					wscset(psb_chset);				/* change char set.			*/
				vputc(vchr_map[vrow][wrk_col+1]);				/* Output the character.	*/
				wscset(DEFAULT);						/* Set back to normal.		*/
				data_map[mx(vrow,wrk_col)] = data_map[mx(vrow,wrk_col+1)];	/* Remember char for swap back. */
				attr_map[mx(vrow,wrk_col)] = attr_map[mx(vrow,wrk_col+1)];	/* Remember char for swap back. */
			}
		}
		wsmove(cursor_row,cursor_col);						/* Move to the start of the field.	*/
		ws_tag_alt(alt_read,vrow,cursor_col,no_mod);				/* Set altered flags.			*/
	}
	else ws_bad_char();								/* Can't delete so beep.		*/
	return(SUCCESS);
#endif
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

static int tab_fac(vrow, col)
register int vrow;
register int col;									/* Determine if start of field fac.	*/
{
	int	x_col;									/* Working register.			*/

	if (the_edit && vrow==the_edit->top_row && col==the_edit->left_col-1)
	  return SUCCESS;
	
	if ( !(attr_map[mx(vrow,col)] & FAC_CHARACTER) ) return(FAILURE);		/* Is this a FAC at all?		*/
	if ( !ws_tab_stop(attr_map[mx(vrow,col)]) ) return(FAILURE);			/* Is this a modifyable or tab field?	*/
	if ( (x_col = col+1) < wcurwidth )						/* Are we past the end of the line?	*/
	{
		if (tab_fac(vrow,x_col)) return(FAILURE);				/* No, then check for concat FAC.	*/
	}

	return(SUCCESS);								/* Modifyable FAC is what we want.	*/
}

static int menu_pick_combo(k, j)
int k;
int j;											/* Determine if LOW PROT NUM fac or	*/
{											/* HIGH PROT NUM fac and is followed by */ 
	unsigned char c;								/* a WANG_MENU_PICK or PSEUDO BLANK_CHAR.*/
	int fl;

	fl = FALSE;									/* Set to assume is not a menu pick.	*/
	c = attr_map[mx(k,j)];								/* Point to the current entry.		*/
	if (c == NUMPROT_FIELD || c == NUMPROT_BOLD_FIELD)				/* Is it the needed Fac?		*/
	{
		c = data_map[mx(k,j+1)];
		if (c == WANG_MENU_PICK || c == PSEUDO_BLANK_CHAR)  fl = TRUE;		/* Is needed fac, WANG_MENU_PCIK or	*/
	}										/* PSEUDO_BLANK_CHAR combo.		*/
	return(fl);
}

int ws_sof(vrow, cursor_col)
register int vrow;
register int cursor_col;								/* Get start of current field.		*/ 
			     
{
	if (cursor_col < 0) return(FAILURE);						/* Return failure if not in field.	*/
	if (!ws_mod(vrow,cursor_col)) return(FAILURE);					/* If not modifiable then not in field	*/
	while ((cursor_col > 0) && (ws_mod(vrow,cursor_col))) --cursor_col;		/* Loop until at start of the field.	*/
	return(cursor_col+1);								/* Return the start of the field.	*/
}

int ws_eof(vrow, cursor_col)
register int vrow;
register int cursor_col;								/* Get end of current field.		*/
			     
{
	if (cursor_col < 0) return(FAILURE);						/* Return failure if not in field.	*/
	if (!ws_mod(vrow,cursor_col)) return(FAILURE);					/* If not modifiable then not in field	*/
	while ((cursor_col < wcurwidth) && (ws_mod(vrow,cursor_col))) cursor_col++;	/* Loop until at the end of the field.	*/
	return(cursor_col-1);								/* Return the end of the field.		*/
}

static int ws_tab_stop(atr)
unsigned char atr;									/* Check for a tab stop position.	*/
{
	register int i;									/* Working register.			*/

	i = FALSE;									/* Assume not a tab stop.		*/
											/* Modifyable or tab stop?		*/
	if (!(atr & FAC_PROTECT)) i = TRUE;						/* Yes, then it is a tab stop location. */

	atr = atr & ~(FAC_CHARACTER + FAC_LOW_INTENSITY + FAC_UNDERSCORE);		/* Drop the don't care bits.		*/
	if (atr == (FAC_NUMERIC_ONLY|FAC_PROTECT)) i = TRUE;				/* NUM-PROT? If so it is a tab stop.	*/

	return(i);									/* Return to the caller.		*/
}


static int ws_trans_atr(ch)
unsigned char ch;									/* Translate from Wang to VIDEO		*/
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

static int ws_bloff(do_pseudo)
int do_pseudo;										/* Turn blinking off.			*/
		  
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
#ifdef OLD
				if (attr_map[mx(k,j)] & FAC_CHARACTER) 
				  data_map[mx(k,j)] = data_map[mx(k,j)] & ~FAC_BLINKING; 
#endif
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

static ws_echo(the_char, cursor_row, cursor_col, alt_read, no_mod, do_pseudo)
int the_char;
int cursor_row;
int cursor_col;
int alt_read;
unsigned char *no_mod;
int do_pseudo;										/* Echo input as appropriate.		*/
{
	int mode, index, tmpval=0, eofield;
	unsigned char *rowbuf, *map_multibyte();
	
	rowbuf = map_multibyte(&data_map[mx(cursor_row,0)],NULL);		       /* map out multibyte chars on the line  */

	if (do_pseudo)	mode = psb_rndt | ws_trans_atr(attr_map[mx(cursor_row,cursor_col)]);
	else		mode = ws_trans_atr(attr_map[mx(cursor_row,cursor_col)]);
	wsmode(mode);									/* Select the appropriate rendition.	*/
	ws_tag_alt(alt_read,cursor_row,cursor_col,no_mod);				/* Set flags.				*/

	if (multi_input_pos==1)								/* is this echo part of a multibyte?	*/
	{
											/* if so, need to clear 3 or more spaces*/
		for (index=cursor_col, eofield=ws_eof(cursor_row, cursor_col);
		     (index < cursor_col+outctx->srcsize) && (index < eofield) && !tmpval ; ++index)
		{
			vprint(" ");
			data_map[mx(cursor_row,index)]=' ';
			tmpval = rowbuf[index];
		} 
		if (tmpval)								/* clearing spaces stepped on another	*/
		{									/* multi, which must now also be cleared*/
			while (tmpval == rowbuf[index])
			{
				vprint(" ");
				data_map[mx(cursor_row,index)]=' ';
				++index;
			}
		}
		wsmove(cursor_row,cursor_col);
	}
	else if (tmpval = rowbuf[index = cursor_col])
	{
		while (tmpval == rowbuf[index])
		{
			vprint(" ");
			data_map[mx(cursor_row,index)]=' ';
			++index;
		}
		wsmove(cursor_row,cursor_col);
	}
	if (ws_invisible(attr_map[mx(cursor_row,cursor_col)]))				/* Should character be echoed?		*/
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
	else vputc((char)the_char);							/* Yes, then echo the character.	*/
	data_map[mx(cursor_row,cursor_col)] = the_char;
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
	stable[ndx].sub_value	= DEC_FOUR_CORNER;					/* 0x0E - Intersect two lines, corners. */
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
	stable[ndx].sub_value	= DEC_HORIZ_TVERT;					/* 0x1B - Horizontal with top vertical. */
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
#ifdef OLD
/*	Somebody wanted the back quote substituted for the degree symbol.
	Remove this because it doesn't make any sense and it causes a problem
	with IVS chinese characters as they use the backquote as a lead in char.
	GSL 12/16/92*/

	stable[96].sub_value	= DEC_GRAPHIC_DEGREE;					/* 0x60 - Degree symbol.		*/
	stable[96].char_set	= GRAPHICS;
#endif
	return(SUCCESS);
}

static int load_user_sub_chars(tblptr)
unsigned char *tblptr;									/* Do the load of substitution values.	*/
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

static sub_char(c, xc, char_set)							/* Get the substitution character and	*/
unsigned char c;									/*  character set from table.		*/
unsigned char *xc;
int *char_set;										
{
	if (!sub_table_loaded)								/* Has the sub. table been loaded ?	*/
	{
		load_sub_chars();							/* Nope. Load it.			*/
		sub_table_loaded = TRUE;						/* Set the static variable.		*/
	}
	if (c>128)
	{
		*xc = c;
		*char_set= DEFAULT;
		return 0;
	}
	*xc = stable[c].sub_value;							/* Get exchange character.		*/
	*char_set = stable[c].char_set;							/* Get needed character set (font).	*/
	return 0;
}

static wsdmp_scrn(wsb, type_d)
unsigned char *wsb;
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

ws_erap(amount)
int amount;											/* Erase & protect screen sect. */
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



int ws_help(curset)
int curset;										/* Simulate Wang help function.		*/
{
	int temp_lin, temp_col, temp_atr, temp_set;					/* Temporary state storage.		*/
	char *csave, *asave, *wsa1_save, *wsa2_save, *extra;				/* Char and attribute save pointers.	*/
	register int s,w;								/* Working register.			*/
	int old_menu_exit_key;								/* Orig. menu exit key.			*/
	int optsave;

	optsave=optimization;								/* need to save this.			*/
	old_menu_exit_key = menu_get_key(0);						/* What is the orig. menu exit key ?	*/
	menu_set_key(0, fn1_key);							/* Give it a new value.			*/

	if (ishelpactive())								/* Are we already in help.		*/
	{
		ws_bad_char();								/* Ring the bells.			*/
		return(FAILURE);							/* Return to the caller.		*/
	}
	else sethelpactive(TRUE);							/* Now help is active.			*/

	wpushscr();									/* Save all neccessary variables.	*/

	if (wcurwidth != 80)  ws80();							/* Put in 80 col mode for help screens. */
	vdefer(RESTORE);								/* Cannot be in deferred mode.		*/

	wsh_help(1);									/* Output the selection menu.		*/
	vset(CURSOR,curset);								/* Turn the cursor back on.		*/
	cur_on_fl = curset;								/* Set the flag.			*/

	vdefer(RESTORE);								/* Restore from optimization.		*/
	wpopscr();									/* Restore all saved variables.		*/

	vmode(vcur_atr);								/* Restore the character rendition.	*/
	vcharset(vchr_set);								/* Restore the character set.		*/
	wsmove(vcur_lin, vcur_col);							/* Restore the position.		*/
	vdefer(RESTORE);								/* Optimization terminates before exit. */

	sethelpactive(FALSE);								/* Now help is inactive.		*/
	synch_required = FALSE;								/* A synch is not required.		*/

	menu_set_key(0, old_menu_exit_key);						/* Reset the menu exit key.		*/
	
	optimization=optsave;								/* vwang expects a certain opt level	*/
											/* that's not correctly set here	*/
	return(SUCCESS);								/* All done.				*/
}

wpushscr()										/* A function to save the screen addrs	*/
{											/* and variables of a 'Wisp' screen.	*/
	typedef struct wscrn_struct wscrn_struct;
	wscrn_struct *wscrn_struct_ptr;							/* Point to the the save area.		*/
	int x;										/* A local, working variable.		*/

	if (wbackground()) return(0);							/* Don't do if in background.		*/
	if (rts_first)	init_screen();							/* If first push.			*/
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

	wscrn_struct_ptr->saved_fac_table = malloc(sizeof(fac_table));			/* Alloc. storage and save the addr.	*/
	if (!wscrn_struct_ptr->saved_fac_table)
	{
		werrlog(ERRORCODE(12),0,0,0,0,0,0,0,0);					/* No memory				*/
		wexit(ERRORCODE(12));							/* Unconditional exit to DCL.		*/
	}
	memcpy(wscrn_struct_ptr->saved_fac_table, fac_table, sizeof(fac_table));	/* Copy contents to the storage area.	*/

	wscrn_struct_ptr->saved_toupper_table = malloc(sizeof(user_toupper_table));	/* Alloc. storage and save the addr.	*/
	if (!wscrn_struct_ptr->saved_toupper_table)
	{
		werrlog(ERRORCODE(12),0,0,0,0,0,0,0,0);					/* No memory				*/
		wexit(ERRORCODE(12));							/* Unconditional exit to DCL.		*/
	}
	memcpy(wscrn_struct_ptr->saved_toupper_table, user_toupper_table, sizeof(user_toupper_table));

	wscrn_struct_ptr->saved_numeric_table = malloc(sizeof(user_numeric_table));
	if (!wscrn_struct_ptr->saved_numeric_table)
	{
		werrlog(ERRORCODE(12),0,0,0,0,0,0,0,0);					/* No memory				*/
		wexit(ERRORCODE(12));							/* Unconditional exit to DCL.		*/
	}
	memcpy(wscrn_struct_ptr->saved_numeric_table,user_numeric_table,
	       sizeof(user_numeric_table));
	
	wscrn_struct_ptr->saved_ws_good = ws_good;					/* Save the following variables..	*/
	wscrn_struct_ptr->saved_old_line = old_line;
	wscrn_struct_ptr->saved_old_column = old_column;
	wscrn_struct_ptr->saved_errset = errset;
	wscrn_struct_ptr->saved_aid = vwang_aid();
	wscrn_struct_ptr->saved_fast_read = fast_read;
	wscrn_struct_ptr->saved_screen_width = wcurwidth;
	wscrn_struct_ptr->saved_stop_time = stop_time();
	wscrn_struct_ptr->saved_max_data_range = max_data_range;
	wscrn_struct_ptr->prev_wscrn_struct_ptr = (wscrn_struct *) wscrn_stack;		/* Save the address of the prior screen */
	wscrn_struct_ptr->saved_edit = the_edit;
	the_edit=NULL;

	wscrn_stack = (wscrn_struct *) wscrn_struct_ptr;				/* Save addr. of the new top screen.	*/


	vpushscr();									/* Save the corresponding video structs.*/

	set_stop_time(0);
	set_aid(AID_LOCKED);
	return(SUCCESS);
}

wpopscr()										/* A function to restore the screen and */
{											/* associated variables.		*/
	struct wscrn_struct *wscrn_struct_ptr;						/* Local pointer to the save area.	*/
	int x;										/* A working variable.			*/

	if (wbackground()) return(0);							/* Don't do if in background.		*/
	wscrn_struct_ptr = wscrn_stack;							/* Point to the most recently saved	*/
											/* structure of pointers and variables. */
	if (!wscrn_struct_ptr)								/* Are we pointing to never never land? */
	{
		werrlog(ERRORCODE(14),0,0,0,0,0,0,0,0);					/* Stack empty				*/
		return(0);								/* Outta here.				*/
	}

	x = sizeof(attr_map);								/* What's the size of this item ?	*/
	memcpy(attr_map, wscrn_struct_ptr->saved_ws_atr, x);				/* Copy the saved bytes.		*/
	free(wscrn_struct_ptr->saved_ws_atr);						/* Free up this area.			*/
	memcpy(data_map, wscrn_struct_ptr->saved_ws_at2, x);
	free(wscrn_struct_ptr->saved_ws_at2);						/* Free up this area.			*/

	memcpy(fac_table, wscrn_struct_ptr->saved_fac_table, sizeof(fac_table));
	free(wscrn_struct_ptr->saved_fac_table);
	restore_fac_table=TRUE;
	
	memcpy(user_toupper_table, wscrn_struct_ptr->saved_toupper_table, sizeof(user_toupper_table));
	free(wscrn_struct_ptr->saved_toupper_table);
	memcpy(user_numeric_table, wscrn_struct_ptr->saved_numeric_table, sizeof(user_numeric_table));
	free(wscrn_struct_ptr->saved_numeric_table);

	ws_good = wscrn_struct_ptr->saved_ws_good;					/* Restore these integer variables.	*/
	old_line = wscrn_struct_ptr->saved_old_line;
	old_column = wscrn_struct_ptr->saved_old_column;
	errset = wscrn_struct_ptr->saved_errset;
	set_aid(wscrn_struct_ptr->saved_aid);
	fast_read = wscrn_struct_ptr->saved_fast_read;
	wcurwidth = wscrn_struct_ptr->saved_screen_width;
	set_stop_time(wscrn_struct_ptr->saved_stop_time);
	max_data_range = wscrn_struct_ptr->saved_max_data_range;
	wscrn_stack = wscrn_struct_ptr->prev_wscrn_struct_ptr;				/* Get address of the new top screen.	*/
	the_edit=wscrn_struct_ptr->saved_edit;

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

ws_putbuff(c, i, j, k, rendition, font)
unsigned char c;
int i;
int j;
int k;
int rendition;
int font;										/* Put char. in line buf. accumulator.	*/
{
	if (c == ' ')									/* Special case for space...		*/
	{
		if (!visible(c,rendition) && !visible(vchr_map[k][j],vatr_map[k][j])) return(SUCCESS);	/* No change, just ret. */
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
	else lbdangle = 0;								/* Anything else terminates the dangle. */
}

ws_dumpbuff(k, eol)
int k;
int eol;										/* Dump the current buffer.		*/
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
#include "vwang.d"

	if ((stat = SYS$TRNLNM(0,&t_desc,&l_desc,0,0)) == SS$_NORMAL)			/* Verify if logical exists.		*/
	{										/* Found.				*/
		do_console_log = TRUE;							/* Set to do console logging.		*/
	}
	else do_console_log = FALSE;							/* Not found so no console logging.	*/
#endif
}

static int check_mp(line, col, rend, do_pseudo)
int line;
int col;
int rend;
int do_pseudo;										/* See if it is a menu pick item.	*/
{
	if ( menu_pick_combo(line,col-1) )						/* Is it a menu pick item?	*/
	{
		if (cur_on_fl)								/* If cursor is on then turn off while	*/
		{									/*  rewriting the line.			*/
			vset(CURSOR,INVISIBLE);
		}
		rewrite_mp(line,col,rend,do_pseudo);					/* Re-write line with bold attribute.	*/
		wsmove(line,col);							/* Put cursor back to where it was.	*/
		if (cur_on_fl)								/* If cursor was on then turn it back	*/
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

static int rewrite_mp(line, col, rend, do_pseudo)
int line;
int col;
int rend;
int do_pseudo;										/* Rewrite line or until next FAC.	*/
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
	fc = &attr_map[mx(k,col-1)];							/* Get the FAC character.		*/
	last_mode = ws_trans_atr(*fc);							/* Set the last mode.			*/

	vbuffering(LOGICAL);								/* Turn on buffering if not on.		*/

	for (j = col; j < wcurwidth; j++)						/* Loop through each column.		*/
	{
		c = *dm;								/* Get the character from the map.	*/
		last_atr = *am;								/* Get the character attributes.	*/

		if (last_atr & FAC_CHARACTER)						/* Is this character a FAC?		*/
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
				sub_char(c, &xc, &char_set);				/* Get character & font from sub table. */
				if (!(last_atr & FAC_PROTECT))				/* Is this a modifiable field ?		*/
				{
					if (do_pseudo)	mode = last_mode|rend|psb_rndt;
					else		mode = last_mode|rend;
					ws_putbuff(xc,line,j,k,mode,char_set);		/* Put char.				*/
				}
				else if ((xc != ' ') || (last_mode & (REVERSE|UNDERSCORE)))	/* Visible?			*/
				{
					if ((xc >= ' ' || !do_pseudo) && (c != PSEUDO_BLANK_CHAR))
						mode = last_mode;			/* If in the printable range.		*/
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
					ws_putbuff(' ',line,j,k,mode,DEFAULT);		/* Put char.				*/
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

static int mx(row, column)
int row;
int column;										/* Calculate array index.		*/
{
	return((row * wcurwidth) + column);						/* Return the value.			*/
}

int ws80()										/* Select 80 column screen.		*/
{
	if (rts_first)	init_screen();							/* If first time setting.		*/
	wsetwid(NARROW);								/* Set the screen width.		*/
	return(SUCCESS);
}
int ws132()										/* Select a 132 column screen.		*/
{
	if (rts_first)	init_screen();							/* If first time setting.		*/
	wsetwid(WIDE);									/* Select the screen width.		*/
	return(SUCCESS);
}
static int wsetwid(wd)
int wd; 
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

char wscharat(r, c)
int r;
int c;											/* Get a character from a row and col.	*/
{
	return(data_map[mx(vml(r),c)]);							/* Get this character from the map.	*/
	
}

/*
**	Routine:	adjust_for_multibyte()
**
**	Function:	adjust the cursor so that user cannot move into the middle
**			of a multibyte (ie, chinese) character
**
**	Description:	the current line is examined, and a "map" of chinese data is
**			created.  The location of the cursor is compared with possible
**			chinese data "under" it, along with the operation that brought
**			us to this location to determine if the cursor should be moved			     
**			
**	Input:		  row,col	    the row and column as computed by ws_posit
**					    (the column may be in the middle of a multibyte)
**			  dvert		    the requested change in vertical position for current
**					    cursor move operation
**			  dhoriz	    the requested change in horizontal position for current
**					    cursor move operation
**
**					    dvert and dhoriz are the values ws_posit
**					    received
**
**	Output:		a flag (cursor_on_multibyte) is set if the cursor is on a multibyte
**			character
**
**	Return:		the correct cursor column is returned
**
**	Warnings:	None
**
**	History:	11/6/92			written JEC
**
**
*/

int adjust_for_multibyte(row, col, dvert, dhoriz)
int row;
int col;
int dvert;
int dhoriz;
{
	int i,j,k,l;
	char *p;
	int ret;
	unsigned char *rowbuf,*map_multibyte();
	
	rowbuf = map_multibyte((unsigned char*)&vchr_map[row][0],NULL);
	if ((l=rowbuf[col])==0 || col==0)
	{
		ret=col;
		cursor_on_multibyte=0;
		goto exit;
	}
	cursor_on_multibyte=1;
	if (dvert != 0)
	{
		if (rowbuf[col-1] != l)
		{
			ret = col;
			goto exit;
		}
		if ((col-2<0) || rowbuf[col-2] != l )
		{
			ret = col-1;
			goto exit;
		}
		if ((col-3<0) || rowbuf[col-3] != l )
		{
			ret = col-2;
			goto exit;
		}
	}
	if (dhoriz <0)
	{
		while (rowbuf[col]==l) --col;
		ret= ++col;
		goto exit;
		
	}
	if (dhoriz >0)
	{
		if (!(col>0 && rowbuf[col-1] != l))
		  while (rowbuf[col]==l) ++col;
		ret=col;
		goto exit;
	}
      exit:
	return ret;
}
/*
**	Routine:	map_multibyte()
**
**	Function:	map the chinese characters on a line of screen data
**
**	Description:	using the f1indbuf from video, determine if there are multibyte
**			characters in the passed rowdata array.	 (f1indbuf contains the
**			list of valid bytes for each pos in the multibyte format 1).
**			If caller provided rowmap, operate on it, changing all multibyte
**			bytes to 0x01's.  Otherwise, provide him a map with of the line,
**			normal data being null bytes, multibyte data being nonzero values,
**			with adjacent multibytes having differing values.
**			
**	Input:		  row,col	    the row and column as computed by ws_posit
**					    (the column may be in the middle of a multibyte)
**			  dvert		    the requested change in vertical position for current
**					    cursor move operation
**			  dhoriz	    the requested change in horizontal position for current
**					    cursor move operation
**
**					    dvert and dhoriz are the values ws_posit
**					    received
**
**	Output:		a flag (cursor_on_multibyte) is set if the cursor is on a multibyte
**			character
**
**	Return:		the correct cursor column is returned
**
**	Warnings:	None
**
**	History:	11/6/92			written JEC
**
**
*/
unsigned char *map_multibyte(rowdata, caller_rowbuf)
unsigned char *rowdata;
unsigned char *caller_rowbuf;
{
	static unsigned char my_rowbuf[WS_MAX_COLUMNS_ON_A_LINE];	
	unsigned char *rowbuf, *rptr;
	int rowind, mbyteind;
	int mcounter;  /* an int to count multibyte chars on a line.  this value is inserted 
			  at the location of each respective multibyte */
	extern struct xlcontext *outctx;  /* defined in vchinese.h */

	if (caller_rowbuf)
	{
		rowbuf=caller_rowbuf;
	}
	else
	{
		rowbuf=my_rowbuf;
		memset(my_rowbuf,0,sizeof(my_rowbuf));
	}
	if (outctx==NULL)
	{
		return rowbuf;
	}
	rptr = rowdata;
	mcounter=1;
	for (rowind=0; rowind<wcurwidth; ++rowind)
	{
		if ((rowind > (vcur_col - multi_input_pos)) && 
		    (rowind < vcur_col))
		{
			++rowind;
			continue;
		}
		for (mbyteind=0; mbyteind < (outctx->srcsize); ++mbyteind)
		{
			if ((outctx->indbuf)[rptr[rowind+mbyteind]+(mbyteind<<8)]==0xff)
			{
				break;
			}
		}
		if (mbyteind == (outctx->srcsize))
		{
			if (!caller_rowbuf)
			{
				for (mbyteind=0; mbyteind < (outctx->srcsize); ++mbyteind)
				{
					rowbuf[rowind+mbyteind] = mcounter;
				}
				++mcounter;
			}
			else
			{
				for (mbyteind=0; mbyteind < (outctx->srcsize); ++mbyteind)
				{
					rowbuf[rowind+mbyteind] = 1;
				}
			}
			rowind += (outctx->srcsize)-1;
		}
	}
	return rowbuf;
}

/*
**	Routine:	SETFACS()
**
**	Function:	To setup the FAC tables
**
**	Description:	This routine copies the user's provided
**			tables, described below
**
**	Arguments:	new_table     - the pseudo<-->true FAC map
**			toupper_table  - how to map lower to upper 
**			numeric_table - which bytes are numeric
**
**	Globals:	user_fac_table	   - current fac table
**			user_toupper_table  - current lower to upper map
**			user_numeric_table - current numeric map
**
**	Return:		none
**
**	Warnings:	None
**
**	History:	
**	01/06/93	Modified to take the toupper_table and numeric_table
			by JEC.
**	12/01/92	Written by JEC
**
*/
int SETFACS(new_table, toupper_table, numeric_table)
unsigned char *new_table;
unsigned char *toupper_table;
unsigned char *numeric_table;
{											/* Prerequisite routine to change the	*/
											/*   the meaning and mapping of facs.	*/
	register int i;									/* Working variable.			*/

	for (i = 0; i < 128; i++)							/* Check each character.		*/
	{
		if ( new_table[i] == CHAR_NULL) 
		  user_fac_table[i] = new_table[i];	/* Nulls mean it is not a fac.		*/
		else if (( new_table[i] >= 0x80) && (new_table[i] <= 0xFF))		/* Is it in the valid fac range?	*/
		{
			user_fac_table[i] = new_table[i];				/* Possible fac value change.		*/
		}
		else
		{	/* Report the error.	*/
			vre_window("SETFACS-F-Invalid value %d for FAC, table item %d.",new_table[i],i);
			use_user_fac_table=FALSE;
			return 0;
		}
	}
	if (toupper_table)
	{
		memcpy(user_toupper_table,toupper_table,sizeof(user_toupper_table));
	}
	else
	{
		memcpy(user_toupper_table,def_toupper_table,sizeof(user_toupper_table));
	}
	if (numeric_table)
	{
		memcpy(user_numeric_table,numeric_table,sizeof(user_numeric_table));
	}
	else
	{
		memcpy(user_numeric_table,def_numeric_table,sizeof(user_numeric_table));
	}
	use_user_fac_table=TRUE;
	return 0;
}

/*
**	Routine:	SET8BIT()
**
**	Function:	To setup the FAC tables.. called from COBOL
**
**	Description:	This routine builds tables based on byte lists from
**			COBOL (see below)
**
**	Arguments:	faclist	       - the pseudo<-->true FAC list
**			lowuplist      - how to map lower to upper 
**			numlist	       - which bytes are numeric
**
**	Globals:	user_fac_table	   - current fac table
**			user_toupper_table  - current lower to upper map
**			user_numeric_table - current numeric map
**
**	Return:		none
**
**	Warnings:	None
**
**	History:	
**	02/10/93	Written by JEC
**
*/
SET8BIT(faclist, lowuplist, numlist)
unsigned char *faclist;
unsigned char *lowuplist;
unsigned char *numlist;
{
	int idx;
	unsigned char *ptr;

	for (idx=0; idx<128; ++idx)
	{
		user_fac_table[idx]=idx+128;
		user_toupper_table[idx]=0;
		user_numeric_table[idx]=0;
	}
	/* setup FAC table */
	for (ptr=faclist; *ptr != '\0'; ptr += 2)
	{
		if (user_fac_table[((int) *ptr)-128] == (int) *ptr)
		  user_fac_table[((int) *ptr)-128] = 0;
		if (((int) *ptr) != ((int) *(ptr+1)))
		  user_fac_table[((int) *(ptr+1))-128] = *ptr;
	}
	/* setup toupper table */
	for (ptr=lowuplist; *ptr != '\0'; ptr += 2)
	{
		user_toupper_table[((int) *ptr)-128] = *(ptr+1);
	}
	/* setup num table */
	for (ptr=numlist; *ptr != '\0'; ++ptr)
	{
		user_numeric_table[((int) *ptr)-128] = 1;
	}
	use_user_fac_table=TRUE;
}
/*
**	Routine:	valid_char_data()
**
**	Function:	determine if given character is valid for 
**			display
**
**	Arguments:	value	       - the char
**
**	Globals:	max_data_range - set by vwang based on setfacs
**
**	Return:		True/false if valid
**
**	Warnings:	None
**
**	History:	
**	02/10/93	Written by JEC
**
*/
valid_char_data(value)
char value;
{
	if (value >= ' ' && value < max_data_range)
	  return TRUE;
	else
	  return FALSE;
}
/*
**	Routine:	fac()
**
**	Function:	To convert a pseudo FAC into a true FAC
**
**	Description:	This routine examines the FAC table to convert
**			a pseudo to a true FAC
**
**	Arguments:	pseudo_FAC  - the fake FAC
**
**	Globals:	fac_table   - the current fac map
**
**	Return:		the true FAC (if any) or zero
**
**	Warnings:	None
**
**	History:	
**	12/01/92	Written by JEC
**
*/
fac(pseudo_FAC)
int pseudo_FAC;
{
	return ((pseudo_FAC)>=128 && (pseudo_FAC)<=255) ? fac_table[(pseudo_FAC-128)] : 0;
}
/*
**	Routine:	unfac()
**
**	Function:	To convert a pseudo FAC back into a true FAC
**
**	Description:	This routine examines the FAC table to convert
**			a pseudo back to a true FAC
**
**	Arguments:	true_FAC  - a real FAC value
**
**	Globals:	fac_table   - the current fac map
**
**	Return:		the pseudo FAC value (if any) or 0xff
**
**	Warnings:	None
**
**	History:	
**	12/01/92	Written by JEC
**
*/
unfac(true_FAC)
int true_FAC;
{
	register int idx;
	
	for (idx=0; idx<128; ++idx)
	  if ((unsigned char)(true_FAC & 0xff) == fac_table[idx])
	    return idx+128;
	return 0xff;
}

/*
**	Routine:	fac_pre_vwang()
**
**	Function:	To convert a pseudo FAC into a true FAC
**
**	Description:	This routine examines the user_fac_table to convert
**			a pseudo to a true FAC.	 This routine is called by
**			wscreen() to convert its FAC values.  Wscreen cannot
**			call fac() above because fac uses the actual FAC table
**			which is not valid until a vwang call.	This routine
**			converts the FACs based on the data passed in to this
**			module using SET8BIT or SETFACS, and it can be called
**			prior to the actual vwang().  This also applies to the routine
**			unfac_pre_vwang().
**
**	Arguments:	pseudo_FAC  - the fake FAC
**
**	Globals:	user_fac_table	 - the fac map that will be used for the next
**					   vwang call
**
**	Return:		the true FAC (if any) or zero
**
**	Warnings:	None
**
**	History:	
**	02/23/93	Written by JEC
**
*/
fac_pre_vwang(pseudo_FAC)
int pseudo_FAC;
{
	if ((pseudo_FAC)<128 || (pseudo_FAC)>255 || !use_user_fac_table)
	{
		return pseudo_FAC;
	}
	else
	{
		
		return user_fac_table[(pseudo_FAC-128)];
	}
}
/*
**	Routine:	unfac_pre_vwang()
**
**	Function:	To convert a pseudo FAC back into a true FAC
**
**	Description:	This routine examines the user FAC table to convert
**			a pseudo back to a true FAC.. see description for
**			fac_pre_vwang() above.
**
**	Arguments:	true_FAC  - a real FAC value
**
**	Globals:	user_fac_table	 - the fac map to be used for the next vwang
**
**	Return:		the pseudo FAC value (if any) or 0xff
**
**	Warnings:	None
**
**	History:	
**	2/23/93		Written by JEC
**
*/
unfac_pre_vwang(true_FAC)
int true_FAC;
{
	register int idx;
	
	if ((true_FAC)<128 || (true_FAC)>255 || !use_user_fac_table)
	{
		return true_FAC;
	}

	for (idx=0; idx<128; ++idx)
	  if ((unsigned char)(true_FAC & 0xff) == user_fac_table[idx])
	    return idx+128;
	return 0xff;
}


/*
**	Routine:	vwang_aid()
**
**	Function:	To extract the current AID character.
**
**	Description:	This routine returns the current AID character.
**			This AID character is set by the set_aid() routine.
**
**	Arguments:	None
**
**	Globals:	current_aid.
**
**	Return:		The current AID char.
**
**	Warnings:	None
**
**	History:	
**	12/16/92	Written by GSL
**
*/

static char current_aid = AID_LOCKED;

char vwang_aid()
{
	return(current_aid);
}

/*
**	Routine:	set_aid()
**
**	Function:	To set the current AID character.
**
**	Description:	This routine is used to set the current AID character.
**			This AID character can then be extracted by vwang_aid().
**
**	Arguments:
**	aid		The new AID char.
**
**	Globals:	current_aid.
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	12/16/92	Written by GSL
**
*/
void set_aid(aid)
char aid;
{
	current_aid = aid;
}

/*
**	Routine:	vwang_timeout()
**
**	Function:	To set a timeout value for a vwang read.
**
**	Description:	This routine is called to setup a timeout for the following vwang read.
**			You pass it the number of seconds before timeout and it calculates when (by the clock) 
**			when the read will timeout. (Note: we add 1 extra second to ensure we don't timeout
**			before we have a chance to do the read.)
**			
**
**	Arguments:
**	seconds		The seconds to wait before a timeout.
**
**	Globals:	
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	12/16/92	Written by GSL
**
*/
void vwang_timeout(seconds)
long int seconds;
{
	if (seconds < 1)
	{
		set_stop_time(0);
	}
	else
	{
		set_stop_time(time(NULL) + seconds + 1);
	}
}

/*
**	Routine:	timed_read()
**
**	Function:	To check if doing a timed read.
**
**	Description:	This routine tell if we are doing a timed read and how much time is remaining
**			before we should timeout.
**			It compares the current time to the stop time to give the time remaining.
**
**	Arguments:
**	seconds		The seconds to left before a timeout. (May be negative)
**
**	Globals:	None
**
**	Return:
**	TRUE		Doing a timed read
**	FALSE		The timer is not set.
**
**	Warnings:	None
**
**	History:	
**	12/16/92	Written by GSL
**
*/
static int timed_read(seconds_remaining)
long int *seconds_remaining;
{
	if (stop_time())
	{
		*seconds_remaining = stop_time() - time(NULL);
		return(TRUE);
	}
	else
	{
		return(FALSE);
	}
}

/*
**	Routine:	stop_time()
**
**	Function:	To return the stop time for a timed read
**
**	Description:	This routine returns vwang_stop_time. This is set when the user has requested
**			a timed read.  It gives the time when the read should timeout.
**
**	Arguments:	None
**
**	Globals:
**	vwang_stop_time This currently set stop time, 0 mean no timeout was set.
**
**	Return:		The stop time.
**
**	Warnings:	None
**
**	History:	
**	12/17/92	Written by GSL
**
*/
static time_t vwang_stop_time = 0;
static time_t stop_time()
{
	return( vwang_stop_time );
}
/*
**	Routine:	set_stop_time()
**
**	Function:	To set the stop time for a timed read.
**
**	Description:	This routine sets vwang_stop_time.
**
**	Arguments:
**	the_time	The stop time.
**
**	Globals:
**	vwang_stop_time This currently set stop time, 0 mean no timeout was set.
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	12/17/92	Written by GSL
**
*/
static void set_stop_time(the_time)
time_t the_time;
{
	vwang_stop_time = the_time;
}

/*
**	Routine:	meta_aid()
**
**	Function:	To convert a meta character into an AID char.
**
**	Description:	This routine takes a metachar and returns the corresponding AID character.
**			If the metachar was not a pfkey then a NULL is returned.
**
**	Arguments:
**	metachar	The meta character
**
**	Globals:	None
**
**	Return:		The AID char or NULL
**
**	Warnings:	None
**
**	History:	
**	12/17/92	Written by GSL
**
*/
char meta_aid(metachar)
int metachar;
{
	int	pfk;
	char	aid;

	aid = (char)0;								/* Set AID to NULL				*/
	pfk = vfnkey(metachar);							/* Change meta into a pfkey number 1-32		*/
	if (pfk)								/* If a Pfkey change to an AID			*/
	{ 
		aid = pfk + ((pfk > 16) ? 80 : 64);				/* Add offsets to change into AID		*/
	}
	else									/* Not a PF-key from 1 to 32.		*/
	{
		if ( (enter_key	 == metachar) || 
		     (return_key == metachar)	 )				/* Is it the ENTER or RETURN key?	*/
		{
			aid = AID_ENTER;					/* If so, return '@' sign.		*/
		}
	}
	return(aid);
}

/*
**	Routine:	in_edit()
**
**	Function:	determine if the cursor is in the edit window
**
**	Description:	This routine checks the the_edit struct first, then
**			compares row and col to the edit boundaries
**			
**	Arguments:
**	row		cursor row
**	col		cursor col
**
**	Globals:	the_edit edit struct
**
**	Return:		TRUE or FALSE
**
**	Warnings:	None
**
**	History:	
**	02/04/93	Written by JEC
**
*/
int in_edit(row, col)
int row;
int col;
{
	if (the_edit==NULL)							/* not in edit if the_edit is NULL */
	  return FALSE;
	
	if (row >= the_edit->top_row  && row <= the_edit->bottom_row &&		/* check row and col */
	    col >= the_edit->left_col && col <= the_edit->right_col)
	  return TRUE;
	else
	  return FALSE;
}
/*
**	Routine:	edit_init_struct()
**
**	Function:	Initialize the the_edit struct for an edit session
**
**	Description:	This routine callocs a struct an inits a few fields
**
**	Arguments:	None
**
**	Globals:	the_edit
**
**	Return:		void
**
**	Warnings:	None
**
**	History:	
**	02/04/93	written by JEC 
**
*/
void edit_init_struct()
{
	int idx;
	
	the_edit=(struct EDIT *)calloc(1,sizeof(struct EDIT));			/* get the struct */
	the_edit->mode_ins = 1;							/* start in insert mode */
	the_edit->mark_row =  -1;						/* mark == -1 means no active mark */
	the_edit->top_row = the_edit->bottom_row =
	  the_edit->left_col = the_edit->right_col = -1;
	for (idx=0; idx<EDIT_MAX_ROWS; ++idx)
	{
		the_edit->line_size[idx]= -1;
	}
}
/*
**	Routine:	edit_init_window()
**
**	Function:	Remaining init for edit window
**
**	Description:	This routine draws a box, initializes the data and pointer
**			lists of the_edit
**
**	Arguments:	None
**
**	Globals:	the_edit
**
**	Return:		void
**
**	Warnings:	None
**
**	History:	
**	02/04/93	written by JEC 
**
*/
void edit_init_window()
{
	int row_idx,col_idx;

	vmove(the_edit->top_row-1,the_edit->left_col-1);			/* first draw the box */
	vline(VERTICAL,the_edit->bottom_row - the_edit->top_row+3);

	vmove(the_edit->top_row-1,the_edit->left_col-1);
	vline(HORIZONTAL,the_edit->right_col-the_edit->left_col+3);

	vmove(the_edit->bottom_row+1,the_edit->left_col-1);
	vline(HORIZONTAL,the_edit->right_col-the_edit->left_col+3);

	vmove(the_edit->top_row-1,the_edit->right_col+1);
	vline(VERTICAL,the_edit->bottom_row - the_edit->top_row+3);

	the_edit->width = the_edit->right_col - the_edit->left_col + 1;
	the_edit->height = the_edit->bottom_row - the_edit->top_row + 1;

	memset(the_edit->data_buf,' ',sizeof(the_edit->data_buf));
	edit_init_data();							/* grab the data already on the screen */
	the_edit->edit_row = -1;
	the_edit->edit_col = -1;

	edit_redraw();								/* redraw what we have */
}
/*
**	Routine:	edit_erase_box()
**
**	Function:	erase the box from the screen
**
**	Description:	This routine writes bogus info to the data_map to force
**			vwang to erase it.. (vwang is not aware of the box)
**
**	Arguments:	None
**
**	Globals:	the_edit
**
**	Return:		void
**
**	Warnings:	None
**
**	History:	
**	02/04/93	written by JEC 
**
*/
void edit_erase_box()
{
	int row_idx, col_idx;
	
	/* loop and insert 0x01's into the data_map.. on the next time through */
	/* vwang (ws_write) will be fooled into writing spaces where the 0x01's are */

	for (row_idx = the_edit->top_row-1; row_idx <= the_edit->bottom_row; ++row_idx)
	{	  
		data_map[mx(row_idx,the_edit->left_col-1)] = 0x01;
		data_map[mx(row_idx,the_edit->right_col+1)] = 0x01;
	}
	for (col_idx = the_edit->left_col-1; col_idx<=the_edit->right_col+1; ++col_idx)
	{
		data_map[mx(the_edit->top_row-1,col_idx)] = 0x01;
		data_map[mx(the_edit->bottom_row+1,col_idx)] = 0x01;
	}
}
/*
**	Routine:	edit_main()
**
**	Function:	Main edit dispatch function
**
**	Description:	this routine acts similar to wsread.. it decides what
**			what to do with the input
**
**	Arguments:
**	input		the input keystroke
**	row		current cursor row
**	col		current cursor col
**	filling		same as wsread
**	terminate_list	same as wsread
**	pfkey		same as wsread
**	no_mod		same as wsread
**
**	Globals:	vcur_line, vcur_col
**			the_edit, max_data_range,
**			keydefs from vdara.h
**
**	Return:		void
**
**	Warnings:	None
**
**	History:	
**	02/04/93	written by JEC 
**
*/
void edit_main(input, row, col, filling, terminate_list, pfkey, no_mod)
int input;
int row;
int col;
int *filling;
unsigned char *terminate_list;
unsigned char *pfkey;
unsigned char *no_mod;
{
	int do_pseudo=FALSE;
	int vmrow=vml(row);
	
	edit_compute_pos(row,col);
	
	if (input == return_key)
	{
		edit_unmark();
		edit_redraw();
		ws_fkey(0,filling,terminate_list,pfkey,no_mod);			/* behave as a normal return */
	}
	else if (vfnkey(input))							/* function keys are also normal */
	{
		ws_fkey(input,filling,terminate_list,pfkey,no_mod);
	}
	else if (input == up_arrow_key)						/* arrows move normally but may require	 */
	{									/* hilite update */
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;		/* clear the wrapped word flag */

		ws_posit(-1, 0,do_pseudo);
		edit_compute_pos(vcur_lin,vcur_col);				/* so compute new cpos */
		edit_show_hilite();						/* and call the hilite showing routine */
	}
	else if (input == down_arrow_key)	
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;		/* clear the wrapped word flag */
		ws_posit( 1, 0,do_pseudo);
		edit_compute_pos(vcur_lin,vcur_col);
		edit_show_hilite();
	}
	else if (input == left_arrow_key)	
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;		/* clear the wrapped word flag */
		ws_posit( 0,-1,do_pseudo);
		edit_compute_pos(vcur_lin,vcur_col);
		edit_show_hilite();
	}
	else if (input == right_arrow_key)	
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;		/* clear the wrapped word flag */
		ws_posit( 0, 1,do_pseudo);
		edit_compute_pos(vcur_lin,vcur_col);
		edit_show_hilite();
	}
	else if (input == key_mark)						/* mark key pressed, mark the current cpos */
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;		/* clear the wrapped word flag */
		the_edit->mark_row = the_edit->edit_row;
		the_edit->mark_col = the_edit->edit_col;
	}
	else if ((input >= SPACE_BAR) && (input < max_data_range))		/* its a valid data character */
	{
		edit_unmark();							/* unmark if marked */
		if (the_edit->upper == TRUE)					/* COMPUTRON needed UPPERONLY capability */
		{								/* it's determined by the FAC used in the upper */
										/* left corner */
			if (max_data_range == SEVEN_BIT_DATA || input <= SEVEN_BIT_DATA) /* 8 bit not active or byte is <128 */
			{
				input = toupper(input);				/* so use the standard to upper */
			}
			else
			{
				if (user_toupper_table[input-128])		/* use the toupper table provided by user  */
				  input = user_toupper_table[input-128];	/* to convert lower to upper */
			}
		}
		edit_putchar(input);						/* now insert the character into the data */
		edit_redraw();							/* and redraw the window */

	}		
	else if (input == newline_key)						/* we treat newline as a paragraph separator */
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;		/* clear the wrapped word flag */
		edit_unmark();							/* unmark if marked */
		edit_putchar('\n');						/* stick a newline in at the current cpos */
		edit_redraw();							/* redraw it */
	}
	else if (input == tab_key)						
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;		/* clear the wrapped word flag */
		edit_tab(row,col);						/* move the cursor to the next tabstop */
		edit_compute_pos(vcur_lin,vcur_col);				/* compute new cpos */
		edit_show_hilite();						/* adjust hilited area if necessary */
	}
	else if (input == backtab_key)						/* backtab key is normal */
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;		/* clear the wrapped word flag */
		ws_last(row,col,do_pseudo);
	}
	else if (input == help_key)						/* help is normal */
	{
		if (ishelpactive())							/* Is help active?	*/
		{
			strcpy((char *)pfkey,(char *)"FE");				/* Return code FE.	*/
			no_mod[1] = AID_HELP;						/* Mode code too.	*/
			*filling = FALSE;						/* Now we're done.	*/
		}
		else ws_bar_menu(ON,row,col,vmrow,0,no_mod,do_pseudo);
	}
	else if (input == clear_before_key)					/* these haven't been well tested */
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;		/* clear the wrapped word flag */
		edit_clear_before(the_edit->edit_row,the_edit->edit_col);
		edit_redraw();
	}
	else if (input == clear_after_key)					/* these haven't been well tested */
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;		/* clear the wrapped word flag */
		edit_clear_after(the_edit->edit_row,the_edit->edit_col);		
		edit_redraw();
	}
	else if (input == home_key)						/* go to upper left of edit window */
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;		/* clear the wrapped word flag */
		wsmove(the_edit->top_row,the_edit->left_col);
	}
	else if (input == delete_key)						/* destructive backspace */
	{
		if (the_edit->mark_row != -1)
		{
			edit_cut_block();
			edit_redraw();
		}
		else
		{
			edit_delleft();
			edit_redraw();
		}
	}
	else if (input == insert_key)						/* toggle insert mode */
	{
		the_edit->mode_ins = 1 - the_edit->mode_ins;
	}
	else if (input == remove_key)						/* delete char under cursor */
	{
		edit_delright();
		edit_redraw();
	}
	else if (input == key_copy)						/* copy range */
	{
		edit_copy_block();
		edit_redraw();
	}
	else if (input == key_cut)						/* copy range and delete it */
	{
		edit_cut_block();
		edit_redraw();
	}
	else if (input == key_paste)						/* paste the copy buf at cpos */
	{
		edit_paste_block();
		edit_redraw();
	}
	else if (netroncap && input == trigger1)				/* what does this do? */
	{
	}
	else ws_bad_char();						/* Else beep.				*/

}
/*
**	Routine:	edit_compute_pos()
**
**	Function:	given row and column compute the appropriate cpos into the
**			edit data area
**
**	Arguments:	row and col
**	
**	Globals:	the_edit
**
**	Return:		void
**
**	Warnings:	side effect is that it adjusts the_edit->cpos
**
**	History:	
**	02/04/93	written by JEC 
**
*/
void edit_compute_pos(row, col)
int row;
int col;
{
	the_edit->edit_row = row - the_edit->top_row;				/* otherwise, simply subtract to get the  */
	the_edit->edit_col = col - the_edit->left_col;				/* zero relative row and col */

	return;
}
/*
**	Routine:	edit_putchar()
**
**	Function:	insert a character into the data at cpos
**
**	Description:	this routine inserts a character into the map, adding
**			deferred spaces if necessary
**
**	Arguments:
**	input		the input character
**
**	Globals:	the_edit
**
**	Return:		void
**
**	Warnings:	None
**
**	History:	
**	02/04/93	written by JEC 
**
*/
void edit_putchar(input)
int input;
{
	int currow,curcol,cursize;
	register int copyidx,scanidx;
	register char *dest,*src;
	
#ifdef EDITPUTCHARDBG
	if (foo==NULL)
	{
		foo=fopen("bar","a");
	}
#endif

	/* setup working vars */
	currow=the_edit->edit_row;
	curcol=the_edit->edit_col;		     
	cursize=the_edit->line_size[currow];

	if (input=='\n')       /* inserting paragraph sep */
	{
		if (the_edit->edit_row >= the_edit->height-1)
		{
			return;
		}
		edit_open_line(currow);	    /* create a new empty line below the current one */
		
		dest= &(the_edit->data_buf[currow+1][0]);      /* setup to copy to that new line */
		src = &(the_edit->data_buf[currow][curcol]);   /* starting at the current cursor pos */
		
		if (curcol > cursize)			       /* we are past the end of data */
		{
			cursize = curcol;		       /* increase the size */
		}
		
		copyidx = cursize - curcol;		       /* copyidx is the amount of data we want to copy down to new line */
		
		the_edit->line_size[currow+1] = copyidx;     /* set the size of the new line  */
		if (the_edit->line_size[currow+1] == -1)
		{
			the_edit->line_size[currow+1] = 0;     
		}
		while (copyidx)				       /* now do the coyp  */
		{
			*dest = *src;			       /* copy the data */
			*src = ' ';			       /* erase the end of the current line */
			++dest;
			++src; 
			--copyidx;
		}
		
		/* current line size is decreased */
		the_edit->line_size[currow] = cursize = curcol /*-1*/;
		
		/* set the flag to indicate theres a paragraph sep here */
		the_edit->wrap_flag[currow] = TRUE;
		
		++the_edit->edit_row;	     /* cursor moves to beginning of next line */
		the_edit->edit_col = 0;
		
		the_edit->wrapped_mid_word=FALSE;		/* clear the wrapped word flag */
		edit_wrap(currow+1,WRAP_UP,the_edit->width - the_edit->line_size[currow+1]); /* wrap remaining lines */
		return;
	}
	else if (cursize == the_edit->width && the_edit->mode_ins)
	{
		edit_wrap(currow,WRAP_DOWN,1);
		currow=the_edit->edit_row;
		curcol=the_edit->edit_col;
		cursize=the_edit->line_size[currow];
	}
	else if (curcol > cursize)
	{
		the_edit->line_size[currow] = cursize = curcol;
	} 
	if (curcol==0 && input!=' ' && the_edit->wrapped_mid_word==TRUE)
	{
		edit_wrap(currow-1,WRAP_DOWN,1);
		the_edit->edit_col += the_edit->wrap_count[currow];
		edit_putchar(input);
		return;
	}
	else if (curcol==0 && input==' ' && the_edit->wrapped_mid_word==TRUE)
	{
		the_edit->wrapped_mid_word=FALSE;
		the_edit->wrap_moved_curs=FALSE;
		return;
	}
	else if (curcol==0 && input==' ' && the_edit->wrap_moved_curs==TRUE)
	{
		the_edit->wrap_moved_curs=FALSE;
		the_edit->wrapped_mid_word=FALSE;
		return;
	}
	else if (the_edit->wrap_moved_curs)
	{
		the_edit->wrap_moved_curs=FALSE;
		edit_putchar(input);
		return;
	}
	else
	{
		the_edit->wrapped_mid_word=FALSE;
		the_edit->wrap_moved_curs=FALSE;
	}
	if (the_edit->mode_ins)
	{
		copyidx = cursize-curcol;
		dest = &(the_edit->data_buf[currow][cursize+1]);
		src = dest-1;
		
		while (copyidx>=0)
		{
			*dest-- = *src--;
			--copyidx;
		}
		
		*(++src) = input;
	}
	else
	{
		the_edit->data_buf[currow][curcol]=input;
	}
	if (!(input == ' ' && curcol>=cursize))
	{
		++the_edit->line_size[currow];
	}

	/* scan back so as to not include trailing spaces in line size */
	for (scanidx = the_edit->width-1; 
	     scanidx >= 0 && the_edit->data_buf[currow][scanidx]==' ';
	     --scanidx);
	++scanidx;
	if (scanidx < the_edit->line_size[currow])
	{
		the_edit->line_size[currow] = scanidx;
	}

	++the_edit->edit_col;
	if (the_edit->edit_col>=the_edit->width)
	{
		++the_edit->edit_row;
		the_edit->edit_col=0;
		if (input!=' ')
		{
			the_edit->wrapped_mid_word=TRUE;
		}
	}
}
/*
**	Routine:	edit_wrap()
**
**	Function:	perform word wrap on the data
**
**	Description:	this routine does one of two things, depending on "direction":
**			  - wrap the last word of the current line
**			    down to the next line and then wrap remaining lines
**  
**			  - wrap the first word of the next line back to the 
**			    end of the current line and then wrap remaining lines
**
**	Arguments:
**	lineno		line number to begin
**	direction	WRAP_DOWN or WRAP_UP
**
**	Globals:	the_edit
**
**	Return:		void
**
**	Warnings:	None
**
**	History:	
**	07/04/93	written by JEC 
**
*/
edit_wrap(lineno, direction, line_space)
int lineno;
int direction;
int line_space;
{
	int scanidx,copyidx,space_found,cutidx;
	char *dest,*src;
	char word_to_copy[EDIT_MAX_COLS];
	int newsize,loop_again;
	int space_needed, free_space;
	
	if (lineno >= the_edit->height-1)
	{
		return(0);
	}
#ifdef EDITWRAPDBG
	if (foo==NULL)
	{
		foo=fopen("bar","a");
	}

	fprintf(foo,"edit_wrap(lineno=%d,direction=%s,%s=%d)\n",
		lineno,direction==WRAP_UP?"UP":"DOWN",direction==WRAP_UP?"space_available":"space_needed",
		line_space);
	fflush(foo);
#endif	
	
	switch (direction)
	{
	      case WRAP_UP:
		if (the_edit->wrap_flag[lineno] || the_edit->line_size[lineno+1]==0)
		{
			edit_chk_wrap_back(lineno+1);
			return(0);
		}
		free_space=line_space;
		newsize = the_edit->line_size[lineno];
		if (the_edit->data_buf[lineno][newsize]=='.')
		{
			free_space -= 2;
			newsize += 2;
		}
		else if (!the_edit->wrap_moved_curs && newsize!=0)
		{
			free_space -= 1;
			newsize += 1;
		}
		else
		{
			the_edit->wrap_moved_curs=FALSE;
		}
		if (free_space<=0)
		{
			return(0);
		}
#ifdef EDITWRAPDBG
		fprintf(foo,"	 Free space adjusted to %d.  Line size adjusted to %d:\n",free_space,newsize);
		if (newsize)
		{
			fprintf(foo,"	>%*.*s<\n",the_edit->width,the_edit->width,spaces);
			fprintf(foo,"	>%*.*s<\n",newsize,newsize,&(the_edit->data_buf[lineno][0]));
			fprintf(foo,"	 %*.*s^\n",newsize,newsize,spaces);
		}
		else
		{
			fprintf(foo,"	 *** line is empty *** \n");
		}
		fflush(foo);
#endif		
		dest = &(the_edit->data_buf[lineno][newsize]);

		src = &(the_edit->data_buf[lineno+1][0]);
		scanidx=0;
		copyidx= -1;
		while (1)
		{
			for (; scanidx < the_edit->line_size[lineno+1] && src[scanidx]!=' ';)
			{
				++scanidx;
			}
			if (scanidx>free_space)
			{
				if (copyidx == -1)
				{
					return(0);
				}
				else
				  break;
			}
			copyidx=scanidx;
			if (scanidx==the_edit->line_size[lineno+1])
			{
				cutidx = scanidx;
				break;
			}
			while (scanidx < the_edit->line_size[lineno+1] && src[scanidx]==' ')
			{
				++scanidx;
			}
			cutidx = scanidx;
			if (scanidx>free_space || scanidx==the_edit->line_size[lineno+1])
			{
				cutidx = scanidx;
				break;
			}
		}
		if (copyidx==0)
		{
			return(0);
		}
		--copyidx;
		while (the_edit->data_buf[lineno+1][copyidx]==' ')
		{
			--copyidx;
		}
#ifdef EDITWRAPDBG
		fprintf(foo,"get >%*.*s< from next line\n",copyidx+1,copyidx+1,&(the_edit->data_buf[lineno+1][0]));
		fprintf(foo,"cut >%*.*s< from next line\n",cutidx,cutidx,&(the_edit->data_buf[lineno+1][0]));
		fflush(foo);
#endif
		if ((copyidx+1) > free_space)
		{
			return(0);
		}
		scanidx = copyidx;
		while (scanidx>=0)
		{
			dest[scanidx]=src[scanidx];
			--scanidx;
		}
		the_edit->line_size[lineno] = newsize + copyidx+1;
#ifdef EDITWRAPDBG
		fprintf(foo,"current line %d size %d-->%*.*s<\n",lineno,the_edit->line_size[lineno],
			the_edit->line_size[lineno],the_edit->line_size[lineno],&(the_edit->data_buf[lineno][0]));
		fflush(foo);
#endif
		dest = &(the_edit->data_buf[lineno+1][0]);
		src =  dest + cutidx;
		scanidx = the_edit->line_size[lineno+1]-cutidx;
		while (scanidx>=0)
		{
			*dest++ = *src++;
			--scanidx;
		}
		the_edit->line_size[lineno+1] -= cutidx;
		if (the_edit->line_size[lineno+1] < 0)		  /* size could wind up <0 because the scanner above */
		{						  /* will grab spaces at the end of a line, giving a */
			the_edit->line_size[lineno+1]=0;	  /* wrong count */
		}
		if (the_edit->line_size[lineno+1]==0)
		{
			if (the_edit->wrap_flag[lineno+1])
			{
				the_edit->wrap_flag[lineno+1]=FALSE;
				the_edit->wrap_flag[lineno]=TRUE;
			}
		}
		
		while (cutidx>=0)
		{
			*dest++ = ' ';
			--cutidx;
		}
#ifdef EDITWRAPDBG
		fprintf(foo,"next line %d size %d-->%*.*s<\n",lineno+1,the_edit->line_size[lineno+1],
			the_edit->line_size[lineno+1],the_edit->line_size[lineno+1],&(the_edit->data_buf[lineno+1][0]));
		fflush(foo);
#endif
		edit_chk_wrap_back(lineno+1);  /* this is recursive.. edit_chk_wrap_back calls edit_wrap */
		break;
		
	      case WRAP_DOWN:
		space_needed=line_space;
		scanidx=the_edit->line_size[lineno]-1;
		space_found = the_edit->width -	 the_edit->line_size[lineno];
		copyidx = 0; cutidx = scanidx;
#ifdef EDITWRAPDBG
		fprintf(foo,"	need %d bytes on current line.	Line has %d bytes free\n",
			space_needed,space_found);
		fflush(foo);
#endif		
		if (space_found > space_needed)
		{
			return(0);
		}
		while (space_found < space_needed)
		{
			while (scanidx > 0 && the_edit->data_buf[lineno][scanidx]!=' ')
			{
				--scanidx;
				++space_found;
			}
			if (scanidx==0)
			{
				copyidx=0;
			}
			else
			{
				copyidx = scanidx+1;
			}
			while (scanidx > 0 && the_edit->data_buf[lineno][scanidx]==' ')
			{
				--scanidx;
				++space_found;
			}
			if (scanidx==0)
			{
				cutidx=0;
			}
			else
			{
				cutidx =  scanidx+1;
			}

		}
#ifdef EDITWRAPDBG
		fprintf(foo,"	cutidx=%d, copyidx=%d\n",cutidx,copyidx);
		fprintf(foo,"	>%*.*s<\n",the_edit->width,the_edit->width,spaces);
		fprintf(foo,"	>%*.*s<\n",the_edit->line_size[lineno],the_edit->line_size[lineno],
			&(the_edit->data_buf[lineno][0]));
		fprintf(foo,"	 %*.*s^%*.*s^\n",cutidx,cutidx,spaces,(copyidx-(cutidx+1)),(copyidx-(cutidx+1)),spaces);
			
		fprintf(foo,"copy >%*.*s< to next line line\n",
			the_edit->line_size[lineno]-copyidx,the_edit->line_size[lineno]-copyidx,
			&(the_edit->data_buf[lineno][copyidx]));
		fprintf(foo,"cut >%*.*s< from current line\n",
			the_edit->line_size[lineno]-cutidx,the_edit->line_size[lineno]-cutidx,
			&(the_edit->data_buf[lineno][cutidx]));
		fflush(foo);
#endif
		newsize=cutidx;
		if (copyidx == the_edit->edit_col && lineno == the_edit->edit_row && lineno<the_edit->height)
		{
			/* cursor should wrap */
			the_edit->edit_col = 0;	 
			the_edit->edit_row = lineno+1;
			the_edit->wrap_moved_curs = TRUE;
		}
		if (copyidx < the_edit->edit_col && lineno == the_edit->edit_row && lineno<the_edit->height)
		{
			the_edit->edit_col -= copyidx;
			the_edit->edit_row = lineno+1;
			the_edit->wrap_moved_curs = TRUE;
		}
		for (scanidx=0; copyidx<=the_edit->line_size[lineno]; ++scanidx, ++copyidx)
		{
			word_to_copy[scanidx]=the_edit->data_buf[lineno][copyidx];
			the_edit->data_buf[lineno][copyidx]=' ';
		}
		word_to_copy[scanidx]=(char)0;
		copyidx = scanidx-1;				    /* copyidx is now the size of the word we're wrapping */
		the_edit->wrap_count[lineno+1]=0;
		if (!the_edit->wrap_flag[lineno] || (the_edit->wrap_flag[lineno]&&the_edit->line_size[lineno]==0))
		{
			if (copyidx && (the_edit->line_size[lineno+1] + scanidx > the_edit->width))
			{
				edit_wrap(lineno+1,WRAP_DOWN,copyidx+1);
			}
			the_edit->line_size[lineno]=newsize;
			src = &(the_edit->data_buf[lineno+1][the_edit->line_size[lineno+1]]);
			dest = src + scanidx;
			scanidx = the_edit->line_size[lineno+1]+1;
			
			while (scanidx)
			{
				*dest-- = *src--;
				--scanidx;
			}
			if (copyidx)
			{
				the_edit->wrap_count[lineno+1]=copyidx;
				the_edit->line_size[lineno+1] += copyidx+1;
				++src;
				while (copyidx>=0)
				{
					src[copyidx] = word_to_copy[copyidx];
					--copyidx;
				}
			}
		}
		else
		{
			edit_open_line(lineno);
			the_edit->line_size[lineno]=newsize;
			the_edit->wrap_flag[lineno]=FALSE;
			++lineno;
			dest = &(the_edit->data_buf[lineno][0]);
			src = word_to_copy;
			the_edit->line_size[lineno]=copyidx+1;
			the_edit->wrap_count[lineno]=copyidx;
			while (copyidx>=0)
			{
				*dest++ = *src++;
				--copyidx;
			}
			the_edit->wrap_flag[lineno]=TRUE;
		}
		return(0);
		
		break;
	}
}
/*
**	Routine:	edit_tab()
**
**	Function:	move the edit cpos to the next tab stop
**
**	Arguments:
**	row and col	the current cursor location
**
**	Globals:	the_edit
**
**	Return:		void
**
**	Warnings:	adjusts cpos
**
**	History:	
**	02/04/93	written by JEC 
**
*/
void edit_tab(row, col)
int row;
int col;
{
	int adjcol;  /* the zero-relative column */
	int newpos;  /* computed pos */

	adjcol = col - the_edit->left_col;					/* this is familiar by now */
	for (newpos = adjcol+1; newpos % 5; ++newpos);				/* now loop till mod 5 is zero */

	if (newpos > the_edit->width)						/* no tabbing past the end */
	  ws_bad_char();
	else
	  wsmove(row,newpos+the_edit->left_col);				/* ok move there */
}
/*
**	Routine:	edit_clear_before()
**
**	Function:	clear from cursor to start of line
**
**	Arguments:	row and col
**
**	Globals:	the_edit
**
**	Warnings:	takes a chunk out of data map
**
**	History:	
**	02/04/93	written by JEC 
**
*/
void edit_clear_before(row, col)
int row;
int col;
{
	char *dest;
	int eraseidx;
	
	dest = &(the_edit->data_buf[row][col-1]);
	eraseidx = col-1;
	while (eraseidx>=0)
	{
		*dest-- = ' ';
		--eraseidx;
	}
}
/*
**	Routine:	edit_clear_after()
**
**	Function:	clear from cursor to end of line
**
**	Arguments:	row and col
**
**	Globals:	the_edit
**
**	Warnings:	takes a chunk out of data map
**
**	History:	
**	02/04/93	written by JEC 
**
*/
void edit_clear_after(row, col)
int row;
int col;
{
	char *dest;
	int eraseidx;
	
	dest = &(the_edit->data_buf[row][col]);
	eraseidx = the_edit->line_size[row] - col;
	if (eraseidx <= 0)
	{
		return;
	}
	while (eraseidx)
	{
		*dest++ = ' ';
		--eraseidx;
	}
	the_edit->line_size[row] = col;
}
/*
**	Routine:	edit_delleft()
**
**	Function:	destructive backspace
**
**	Arguments:	row and col
**
**	Globals:	the_edit
**
**	Warnings:	takes a byte out of data map
**
**	History:	
**	02/04/93	written by JEC 
**
*/
int edit_delleft()
{
	int row,col;
	
	register int scanidx;
	register char *dest,*src;
	
	row=the_edit->edit_row;
	col=the_edit->edit_col;
	
	if (col==0)
	{
		int adjcol= -1;
		
		if (row==0)
		{
			ws_bad_char();
			return(0);
		}
		--row;
		col = the_edit->line_size[row];

		if (col==0)
		{
			adjcol = 0;
		}
		else if (the_edit->data_buf[row][col-1]=='.')
		{
			adjcol = col;
		}
		else
		{
			adjcol = col;
		}
		the_edit->wrap_flag[row]=FALSE;

		the_edit->edit_row=row;
		the_edit->wrap_moved_curs=TRUE;
		edit_chk_wrap_back(row);

		if (col != the_edit->line_size[row])
		{
			the_edit->edit_col=adjcol;
		}
		else
		{
			if (the_edit->line_size[row])
			{
				the_edit->edit_col=col+1;
			}
			else
			{
				the_edit->edit_col=0;
			}
		}
		if (the_edit->edit_col>= (the_edit->width))
		{
			the_edit->edit_col = the_edit->width-1; 
			the_edit->line_size[row] =  the_edit->edit_col; 
			the_edit->data_buf[the_edit->edit_row][the_edit->edit_col]=' ';
		}
	} 
	else
	{
		int savesize;
		
		if (col <= the_edit->line_size[row])
		{		    
			dest = &(the_edit->data_buf[row][col-1]);
			src = dest+1;
			scanidx = the_edit->line_size[row] - col;
			while (scanidx)
			{
				*dest++ = *src++;
				--scanidx;
			}
			the_edit->data_buf[row][the_edit->line_size[row]-1]=' ';
			--the_edit->line_size[row];
			edit_chk_wrap_back(row);
		}
		else
		{
			the_edit->wrap_flag[row]=FALSE;
			savesize = the_edit->line_size[row];
			the_edit->line_size[row] = col-1;
			edit_chk_wrap_back(row);
			if (the_edit->line_size[row] == col-1)
			{
				the_edit->line_size[row] = savesize;
			}
		}
		--the_edit->edit_col;
	}
}
edit_chk_wrap_back(row)
int row;
{
	int size = the_edit->line_size[row];
	register int scanidx;
	register char *ptr;
	
	if (row == the_edit->height)
	{
		return(0);
	}
	ptr= &(the_edit->data_buf[row+1][0]);
	if (the_edit->line_size[row+1]== 0 || *ptr==' ')
	{
		return(0);
	}
	for (scanidx=0; *ptr!=' ' && scanidx<the_edit->line_size[row+1]; )
	{
		++scanidx;
		++ptr;
	}
	if ((the_edit->width - size) > scanidx)
	{
		edit_wrap(row,WRAP_UP,the_edit->width - size);
	}
}	     
/*
**	Routine:	edit_delright()
**
**	Function:	delete the character under the cursor
**
**	Arguments:	row and col
**
**	Globals:	the_edit
**
**	Warnings:	takes a byte out of data map
**
**	History:	
**	02/04/93	written by JEC 
**
*/
int edit_delright()
{
	int row,col;
	
	row=the_edit->edit_row;
	col=the_edit->edit_col;

	if (col==(the_edit->width-1))					/* at end of data, can't delete */
	{
		ws_bad_char();
	}
	else
	{
		++the_edit->edit_col;
		edit_delleft();
	}
}
/*
**	Routine:	edit_redraw()
**
**	Function:	redraw the data.. handle word wrap..
**
**	Description:	This routine also generates the offset pointer list
**			and length list
**
**	Arguments:	none
**
**	Globals:	the_edit
**
**	Return:		void
**
**	Warnings:	makes all kinds of adjustments to the_edit members
**		       
**	History:	
**	02/04/93	written by JEC 
	06/17/93	rewritten by JEC
**
*/

void edit_redraw()
{
	register int row_idx, col_idx;
	
	vset(CURSOR,INVISIBLE);
	wsmode(BOLD);
	for (row_idx=col_idx=0; row_idx < the_edit->height; )			/* now loop through the shadow buf byte by byte */
	{									/* output any differences */
		if ((data_map[mx(the_edit->top_row+row_idx,			/* this if stmt checks for diffs */
				the_edit->left_col+col_idx)]!=
		    the_edit->data_buf[row_idx][col_idx])) 
		{
			wsmove(the_edit->top_row+row_idx,the_edit->left_col+col_idx); /* move to the location */
			vputc(the_edit->data_buf[row_idx][col_idx]);		/* put the char */
			data_map[mx(the_edit->top_row+row_idx,the_edit->left_col+col_idx)] =
			  the_edit->data_buf[row_idx][col_idx];		/* stick it in the vwang data map */
		}
		++col_idx;							/* inc the row and col appropriately */
		if (col_idx == the_edit->width)
		{
			col_idx=0;
			++row_idx;
		}
	}

	wsmove(the_edit->top_row+the_edit->edit_row, the_edit->left_col+the_edit->edit_col);
	edit_show_hilite();							/* now show hilite.. edit_show_hilite must */
	wsmode(CLEAR);
	vset(CURSOR,VISIBLE);
}
/*
**	Routine:	edit_show_hilite()
**
**	Function:	hilite an area of the edit window if the user has marked a spot
**
**	Arguments:	None
**
**	Globals:	the_edit
**
**	Return:		none
**
**	Warnings:	must be called after edit_redraw when the shadow_buf is valid
**
**	History:	
**	02/04/93	written by JEC 
**
*/
void edit_show_hilite()
{
	int markrow, markcol, reversed;
	int cursorrow,cursorcol;
	
	int row_idx, col_idx;
	int startrow, startcol, endrow, endcol;
	
	int selected;

	if (the_edit->mark_row == -1)						/* do nothing if no mark set */
	  return;

#ifdef EDITSHOWHI
	if (foo==NULL)
	{
		foo=fopen("bar","a");
	}
#endif
	markrow = the_edit->mark_row;
	markcol = the_edit->mark_col;
	cursorrow = the_edit->edit_row;
	cursorcol = the_edit->edit_col;

	if ((markrow < cursorrow) ||						/* now determine which set of x/y is lower */
	    ((markrow==cursorrow) && (markcol < cursorcol)))
	{
		startrow = markrow;						/* and setup startxxx and endxxx appropriately */
		startcol = markcol;
		endrow = cursorrow;
		endcol = cursorcol;
	}
	else
	{
		startrow = cursorrow;
		startcol = cursorcol;
		endrow = markrow;
		endcol = markcol;
	}	

	memset(the_edit->hilite_buf,0,sizeof(the_edit->hilite_buf));		/* zero the buffer */

	for (row_idx = startrow, col_idx = startcol;				/* now loop through the hilite buffer */
	     (row_idx < endrow) || (row_idx == endrow && col_idx <= endcol);
	     )
	{
		the_edit->hilite_buf[row_idx][col_idx] = 1;			/* and put a 1 where things should be hilited */

		++col_idx;							/* inc the indices */
		if (col_idx > the_edit->width)
		{
			col_idx=0;
			++row_idx;
		}
	}

#define ISHILITED(ch) ((ch & FAC_SELECTED) ? 1 : 0)				/* macro is for readability */

	vset(CURSOR,INVISIBLE);							/* inviso cursor */
	for (row_idx=col_idx=0; row_idx <= the_edit->height; )			/* loop through the map */
	{
#ifdef EDITSHOWHI
		fprintf(foo,"%c%c ",the_edit->hilite_buf[row_idx][col_idx])?'1':'0',
		    ISHILITED(attr_map[mx(row_idx+the_edit->top_row,col_idx+the_edit->left_col)])?'1':'0')
#endif

		if ((selected = the_edit->hilite_buf[row_idx][col_idx]) !=	/* if screen does not match hilite_buf */
		    ISHILITED(attr_map[mx(row_idx+the_edit->top_row,col_idx+the_edit->left_col)]))
		{
			if (selected)						/* decide whether to hilite or dim it */
			{
				wsmode(REVERSE);				/* set mode, move and put */
				wsmove(the_edit->top_row+row_idx,the_edit->left_col+col_idx);
				vputc(the_edit->data_buf[row_idx][col_idx]);
				attr_map[mx(the_edit->top_row+row_idx,the_edit->left_col+col_idx)] =
				  FAC_SELECTED;					/* record it in attr map */
			}
			else
			{
				wsmode(BOLD);					/* clear it */
				wsmove(the_edit->top_row+row_idx,the_edit->left_col+col_idx);
				vputc(the_edit->data_buf[row_idx][col_idx]);
				attr_map[mx(the_edit->top_row+row_idx,the_edit->left_col+col_idx)] =
				  '\f';						/* this is the normal value for attr map */
			}
		}
		++col_idx;							/* increment */
		if (col_idx == the_edit->width)
		{
#ifdef EDITSHOWHI
			fprintf(foo,"\n");
#endif
			col_idx=0;
			++row_idx;
		}
	}			  
	vset(CURSOR,VISIBLE);							/* cursor back on */
	wsmode(BOLD);								/* mode normal */
	wsmove(cursorrow+the_edit->top_row,cursorcol+the_edit->left_col);	/* put the cursor back where it started */
}
/*
**	Routine:	edit_unmark()
**
**	Function:	dim all the stuff on the screen, and clear mark value
**
**	Description:	this routine expects edit_redraw to do the actual work.
**			we stick 1's in the data_map to trick edit_redraw into redisplaying
**			the stuff that was hilited, but dim instead
**
**	Arguments:	None
**
**	Globals:	the_edit
**
**	Warnings:	follow by a call to edit_redraw
**
**	History:	
**	02/04/93	written by JEC 
**
*/
void edit_unmark()
{
	int row_idx, col_idx;

	if (the_edit->mark_row == -1)
	  return;
	
	for (row_idx=col_idx=0; row_idx <= the_edit->height; )
	{
		if (the_edit->hilite_buf[row_idx][col_idx])
		{								/* if this was hilited, prepare to unhilite */
			data_map[mx(the_edit->top_row+row_idx,the_edit->left_col+col_idx)] = 1;
			the_edit->hilite_buf[row_idx][col_idx]=0;
			attr_map[mx(the_edit->top_row+row_idx,the_edit->left_col+col_idx)] =  '\f';
		}
		++col_idx;
		if (col_idx == the_edit->width)
		{
			col_idx=0;
			++row_idx;
		}
	}			  
	the_edit->mark_row = -1;							/* clear the mark */
}
/*
**	Routine:	edit_init_data()
**
**	Function:	init the data struct by loading whatever was on the screen into it
**
**	Description:	This routine attempts to guess where the newlines (paragraph seperators)
**			were.  it will probably get it wrong sometimes , but that's OK
**
**	Arguments:	None
**
**	Globals:	the_edit
**
**	Return:		void
**
**	Warnings:	write data into data_map
**
**	History:	
**	02/04/93	written by JEC 
**
*/
void edit_init_data()
{
	int rowidx;
	int scanidx;
		
	for (rowidx = 0; rowidx < the_edit->height; )
	{
		memcpy(&(the_edit->data_buf[rowidx][0]),
		       &(data_map[mx(the_edit->top_row+rowidx,the_edit->left_col)]),
		       the_edit->width); 
		for (scanidx=the_edit->width-1; scanidx>=0 && the_edit->data_buf[rowidx][scanidx]==' '; --scanidx);
		the_edit->line_size[rowidx] = scanidx + 1;
		++rowidx;
	}
	for (rowidx = the_edit->height-1; rowidx >=0; --rowidx)
	{
		if (the_edit->line_size[rowidx] !=0)
		{
			break;
		}
	}
	if (rowidx>=1)
	{
		--rowidx;
		while (rowidx >= 0)
		{
			if (the_edit->line_size[rowidx]==0)
			{
				the_edit->wrap_flag[rowidx]=TRUE;
			}
			--rowidx;
		}
	}
}
/*
**	Routine:	edit_copycut_block()
**
**	Function:	copy or copy and cut a block from the_edit->data_buf to the_edit->cut_buf
**
**	Arguments:	None
**
**	Globals:	the_edit (mark_{row|col},edit_{row|col},cut_bytes)
**
**	Return:		void
**
**	Warnings:	None
**
**	History:	
**	02/04/93	written by JEC 
**
*/
void edit_copycut_block(cutflag)
int cutflag;
{
	int row_idx, col_idx;
	int start_row, start_col;
	int end_row, end_col;
	int copy_byte_cnt;

	if (the_edit->mark_row== -1)
	{
		return;
	}
	if (the_edit->mark_row < the_edit->edit_row)					/* compute the start and end points */
	{
		start_row = the_edit->mark_row;
		start_col = the_edit->mark_col;
		end_row = the_edit->edit_row;
		end_col = the_edit->edit_col;
	}
	else if (the_edit->mark_row > the_edit->edit_row)
	{
		start_row = the_edit->edit_row;
		start_col = the_edit->edit_col;
		end_row = the_edit->mark_row;
		end_col = the_edit->mark_col;
	}
	else if (the_edit->mark_col < the_edit->edit_col)
	{
		start_row = the_edit->mark_row;
		start_col = the_edit->mark_col;
		end_row = the_edit->edit_row;
		end_col = the_edit->edit_col;
	}
	else 
	{
		start_row = the_edit->edit_row;
		start_col = the_edit->edit_col;
		end_row = the_edit->mark_row;
		end_col = the_edit->mark_col;
	}
	if (end_row==start_row)
	{
		for (copy_byte_cnt=0, col_idx = start_col; col_idx<end_col; ++col_idx, ++copy_byte_cnt)
		{
			the_edit->cut_buf[copy_byte_cnt] = the_edit->data_buf[start_row][col_idx];
		}
	}
	else
	{
		for (copy_byte_cnt = 0, row_idx = start_row; row_idx <= end_row; ++row_idx)
		{
			if (row_idx == start_row)
			{
				for (col_idx = start_col; col_idx<=the_edit->line_size[row_idx]; ++col_idx, ++copy_byte_cnt)
				{
					the_edit->cut_buf[copy_byte_cnt] = the_edit->data_buf[row_idx][col_idx];
				}
				if (the_edit->wrap_flag[row_idx] || the_edit->line_size[row_idx]==0)
				{
					the_edit->cut_buf[copy_byte_cnt++] = '\n';
				}
			}
			else if (row_idx == end_row) 
			{
				for (col_idx = 0; col_idx < end_col && col_idx<=the_edit->line_size[row_idx]; ++col_idx, ++copy_byte_cnt)
				{
					the_edit->cut_buf[copy_byte_cnt] = the_edit->data_buf[row_idx][col_idx];
				}
				if (the_edit->wrap_flag[row_idx] || the_edit->line_size[row_idx]==0)
				{
					the_edit->cut_buf[copy_byte_cnt++] = '\n';
				}
			}
			else
			{
				for (col_idx = 0; col_idx<=the_edit->line_size[row_idx]; ++col_idx, ++copy_byte_cnt)
				{
					the_edit->cut_buf[copy_byte_cnt] = the_edit->data_buf[row_idx][col_idx];
				}
				if (the_edit->wrap_flag[row_idx] || the_edit->line_size[row_idx]==0)
				{
					the_edit->cut_buf[copy_byte_cnt++] = '\n';
				}
			}
		}
	}			
	if (cutflag)
	{
		edit_cut_data(start_row,start_col,end_row,end_col);
	}

	the_edit->cut_bytes=copy_byte_cnt;					/* record the byte count */
	edit_unmark();								/* unmark the block */
}
/*
**	Routine:	edit_cut_block()
**
**	Function:	copy a block from data to cut_buf and remove the block from data
**
**	Arguments:	
**
**	Globals:	the_edit, (mark,cpos,cut_bytes)
**
**	Return:		void
**
**	Warnings:	None
**
**	History:	
**	02/04/93	written by JEC 
**
*/
void edit_cut_block()
{
	edit_copycut_block(1);
}
/*
**	Routine:	edit_cut_block()
**
**	Function:	copy a block from data to cut_buf and remove the block from data
**
**	Arguments:	
**
**	Globals:	the_edit, (mark,cpos,cut_bytes)
**
**	Return:		void
**
**	Warnings:	None
**
**	History:	
**	02/04/93	written by JEC 
**
*/
void edit_copy_block()
{
	edit_copycut_block(0);
}
/*
**	Routine:	edit_paste_block()
**
**	Function:	insert the cut_buf into data buffer
**
**	Arguments:
**
**	Globals:	the_edit, (mark,cpos,cut_bytes)
**
**	Returns:	void
**
**	Warnings:	
**
**	History:	
**	02/04/93	written by JEC 
**
*/
void edit_paste_block()
{
	int pasteidx;
	
	for (pasteidx=0; pasteidx < the_edit->cut_bytes; ++pasteidx)
	{
		edit_putchar(the_edit->cut_buf[pasteidx]);
		if (the_edit->edit_row >= (the_edit->height-1) &&
		    the_edit->edit_col >= (the_edit->width-1))
		  break;
	}
	
}
edit_open_line(lineno)
int lineno;
{
	int lineidx;

	for (lineidx = lineno+2; lineidx< the_edit->height-1; ++lineidx)
	{
		if (the_edit->line_size[lineidx]==0) /* is this needed ? */
		{
			break;
		}
	}
	
	for ( ; lineidx >= lineno+2; --lineidx)
	{
		memcpy(&(the_edit->data_buf[lineidx][0]),&(the_edit->data_buf[lineidx-1][0]),
		       EDIT_MAX_COLS);
		the_edit->line_size[lineidx] = the_edit->line_size[lineidx-1];
		the_edit->wrap_flag[lineidx] = the_edit->wrap_flag[lineidx-1];
	}
	memset(&(the_edit->data_buf[lineno+1][0]),' ',EDIT_MAX_COLS);
	the_edit->line_size[lineno+1] = 0;
	the_edit->wrap_flag[lineidx] = 0;
}
edit_cut_line(lineno)
int lineno;
{
	int lineidx;
	
	for (lineidx = lineno; lineidx < the_edit->height ; ++lineidx)
	{
		memcpy(&(the_edit->data_buf[lineidx][0]),&(the_edit->data_buf[lineidx+1][0]),
		       EDIT_MAX_COLS);
		the_edit->line_size[lineidx] = the_edit->line_size[lineidx+1];
		the_edit->wrap_flag[lineidx] = the_edit->wrap_flag[lineidx+1];
	}
	memset(&(the_edit->data_buf[lineidx][0]),' ',EDIT_MAX_COLS);
	the_edit->line_size[lineidx] = the_edit->line_size[lineidx];
	the_edit->wrap_flag[lineidx] = the_edit->wrap_flag[lineidx];
}
edit_cut_data(start_row, start_col, end_row, end_col)
int start_row;
int start_col;
int end_row;
int end_col;
{
	int row_idx, col_idx, cutidx, cutsize, copy_byte_cnt, copyidx;
	char *dest, *src;
	char copybuf[2048];
	
	if (start_row==end_row)
	{
		if (end_col> the_edit->line_size[start_row])
		{
			end_col =  the_edit->line_size[start_row];
		}
		cutsize=end_col - start_col;
		dest = &(the_edit->data_buf[start_row][start_col]);
		src = dest + cutsize;
		cutidx = the_edit->line_size[start_row] - cutsize;
		while (cutidx)
		{
			*dest++ = *src++;
			--cutidx;
		}
		while (cutsize)
		{
			*dest++ = ' ';
			--cutsize;
		}
		the_edit->line_size[start_row] -= (end_col - start_col);
		the_edit->edit_row = start_row;
		the_edit->edit_col = start_col;
		return(0);
	}
	else if ((end_row-start_row)>1) 
	{
		for (row_idx = start_row+1; row_idx < end_row; ++row_idx)
		{
			edit_cut_line(start_row+1);
		}
		end_row = start_row+1;
	}
	/* at this point end_row is the row right after start row. */

	edit_clear_after(start_row,start_col);

	for (copy_byte_cnt = 0, row_idx = end_row; row_idx < the_edit->height; ++row_idx)
	{
		if (row_idx == end_row)
		{
			for (col_idx = end_col; col_idx<=the_edit->line_size[row_idx]; ++col_idx, ++copy_byte_cnt)
			{
				copybuf[copy_byte_cnt] = the_edit->data_buf[row_idx][col_idx];
				the_edit->data_buf[row_idx][col_idx] = ' ';
			}
			if (the_edit->wrap_flag[row_idx] || the_edit->line_size[row_idx]==0)
			{
				copybuf[copy_byte_cnt++] = '\n';
			}
		}
		else
		{
			for (col_idx = 0; col_idx<=the_edit->line_size[row_idx]; ++col_idx, ++copy_byte_cnt)
			{
				copybuf[copy_byte_cnt] = the_edit->data_buf[row_idx][col_idx];
				the_edit->data_buf[row_idx][col_idx] = ' ';
			}
			if (the_edit->wrap_flag[row_idx] || the_edit->line_size[row_idx]==0)
			{
				copybuf[copy_byte_cnt++] = '\n';
			}
		}
		the_edit->line_size[row_idx] = 0;
		the_edit->wrap_flag[row_idx] = 0;
	}
	edit_cut_line(end_row);
	
	the_edit->edit_row = start_row;
	the_edit->edit_col = start_col;
	
	for (copyidx = copy_byte_cnt-1; copyidx >= 0; --copyidx)
	{
		if (copybuf[copyidx]!='\n')
		{
			break;
		}
		--copy_byte_cnt;
	}
	for (copyidx=0; copyidx< copy_byte_cnt; ++copyidx)
	{
		edit_putchar(copybuf[copyidx]);
	}
	the_edit->edit_row = start_row;
	the_edit->edit_col = start_col;

	edit_unmark();
}

