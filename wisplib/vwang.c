/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/

/*
**	File:		vwang.c
**
**	Project:	wisp/lib
**
**	Purpose:	Wang workstation emulation
**
**	Routines:	
**	vwang()
**	vwang_ws_fkey()
**	vwang_ws_mod()
**	ws_clear()
**	vwang_bad_char()
**	vwang_ws_sof()
**	vwang_ws_eof()
**	vwang_ws_erap()
**	vwang_help()
**	vwang_wpushscr()
**	vwang_wpopscr()
**	WS80()
**	WS132()
**	vwang_charat()
**	SETFACS()
**	SET8BIT()
**	vwang_valid_char_data()
**	vwang_fac()
**	vwang_unfac()
**	vwang_fac_pre_filter()
**	vwang_unfac_pre_filter()
**	vwang_aid()
**	vwang_set_aid()
**	vwang_timeout()
**	vwang_meta_aid()
**	use_custom_vwang()
**	custom_vwang()
**      vwang_title()
*/

/*
**	Includes
*/

#include <stdio.h>									/* Include Standard I/O header.		*/
#include <stdlib.h>
#include <string.h>

#ifdef unix
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#endif

#include <ctype.h>									/* Get character type macros.		*/
#include <math.h>									/* Get math macros.			*/

#ifdef unix
#include <sys/types.h>
#else
#include <time.h>
#endif

/*
**	VIDEO
*/
#include "video.h"
#include "vlocal.h"
#include "vdata.h"
#include "vcap.h"
#include "vchinese.h"

#include "idsistd.h"
#include "wperson.h"									/* Include struct of personality file.	*/
#include "vwang.h"									/* Include wisp/video interface defs.	*/
#include "wglobals.h"									/* Include wisp globals.		*/
#include "scnfacs.h"									/* Include the screen FAC definitions.	*/
#include "wisplib.h"
#include "wexit.h"
#include "assert.h"
#include "wsfns.h"
#include "menu.h"
#include "wmalloc.h"
#include "costar.h"
#include "wispcfg.h"

#include "werrlog.h"									/* Include error logging definitions.	*/
#include "idsisubs.h"

/*
**	Structures and Defines
*/

#define W4W_HOTSPOT_VMODE		(VMODE_REVERSE)
#define W4W_TABSTOP_VMODE		(VMODE_REVERSE | VMODE_BLINK)


#define SIZEOF_SCREEN_MAP	(WS_MAX_LINES_PER_SCREEN * WS_MAX_COLUMNS_ON_A_LINE)	

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

#define DEFAULT_ATTR	(FAC_PROTECTED_ON | FAC_RENDITION_DIM)			/* Default attr_map value		*/
#define IS_PRINTABLE_CHAR(the_char,the_max) \
        (   (EIGHT_BIT_DATA==the_max && the_char > 0 && the_char <= the_max) \
         || (SEVEN_BIT_DATA==the_max && the_char > 0 && the_char <= the_max && is_printable[the_char]) )

#define IS_WANG_CHAR(x) ((x)>0 && (x)<128)
#define IS_8BIT_CHAR(x)	((x)>0 && (x)<256)
#define ANSI2WANG(x)	((IS_8BIT_CHAR(x)) ? tt_ansi2wang[x] : x)
#define WANG2ANSI(x) 	((IS_WANG_CHAR(x)) ? tt_wang2ansi[x] : x)
#define TERM2WANG(x)	((IS_8BIT_CHAR(x)) ? tt_term2wang[x] : x)
#define WANG2TERM(x) 	((IS_WANG_CHAR(x)) ? tt_wang2term[x] : x)
	

/*
**	Globals and Externals
*/

static int rts_first = TRUE;							/* First time flag.			*/
										/* next 'Wisp' screen data structure.	*/

/*
**	Static data.
*/

static unsigned char tt_ansi2wang[256];		/* Ansi character set to Wang character set translation table */
static unsigned char tt_wang2ansi[128];		/* Wang character set to Ansi character set translation table */
static unsigned char tt_term2wang[256];		/* Terminal character set to Wang character set translation table */
static unsigned char tt_wang2term[128];		/* Wang character set to Terminal character set translation table */
static signed char is_printable[128];		/* Which Wang characters are printable 0=no -1=yes */

static int wcurwidth = WS_DEFAULT_COLUMNS_PER_LINE;	/* Current screen width.		*/
static struct wscrn_struct *wscrn_stack = 0;		/* Global storage for the addr of the	*/

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
static char lbuf[WS_MAX_COLUMNS_ON_A_LINE+2];						/* Line accumulator buffer.		*/
static int lbcount, lbdangle, lbattr;							/* Line accumulator char counters.	*/
static struct
{
	unsigned char sub_value;							/* Substitution key value.		*/
	int  char_set;									/* Needed character set mode.		*/
} stable[128];										/* Global storage for the sub. table.	*/
static int menu_fl = FALSE;								/* Flag to depict if at a menu item.	*/
static int cursor_on_flag = FALSE;							/* Flag to depict state of cursor.	*/
static int auto_move;									/* To auto-move or not to auto-move.	*/
static int mp_cursor;									/* To display or not cursor on menu pick*/
static int bgchange;									/* To change or not background color	*/
static int bgcolor;									/* Flag to depict curr. background color*/

static int psb_rndt;		       							/* Rendition for pseudo blanks.		*/
static int psb_chset;									/* Character set choice of pseudo blank.*/
static char psb_char;					       				/* Pseudo blank character.		*/

static unsigned char tab_map[WS_MAX_COLUMNS_ON_A_LINE]=
"	X   X	X   X	X   X	X   X	X					  X	   ";
static int vwang_tab_mode = 0;
static int cursor_on_multibyte=0;							/* flag indicating cursor is sitting on */
											/* a multibyte character		*/
static int multi_input_pos=0;								/* index into current multibyte ideogram*/
static int max_data_range = SEVEN_BIT_DATA;

static struct EDIT *the_edit=NULL;

#ifdef EDITWRAPDBG
static char spaces[WSB_COLS]={' '};
static FILE *foo=NULL;
#endif

static int edit_window = FALSE;

static char dummy_term[2];
static unsigned char dummy_no_mod[2];

static char *function_name[] = {
		"ZERO",
		"WRITE_ALL",
		"READ_ALL",
		"DISPLAY_AND_READ",
		"CLOSE_WORK_STATION",
		"WRITE_SELECTED",
		"READ_ALTERED",
		"DISPLAY_AND_READ_ALTERED",
		"LOAD_SUB_TABLE",
		"INIT_DEFAULT_TABLE",
		"READ_MODIFIABLE",
		"WRITE_ALL_PB",
		"READ_ALL_PB",
		"SET_TABS",
		"TAB_MODE_OFF",
		NULL	};

static char	*costar_before_read_api = NULL;
static char	*costar_after_read_api = NULL;
static char	*costar_before_write_api = NULL;
static char	*costar_after_write_api = NULL;

static int	w4w_tabstop_vmode = W4W_TABSTOP_VMODE;

static int	opt_nullisdot = 0;

/*
**	Function Prototypes
*/

static int ws_write(unsigned char *wsb, 
		    unsigned char lines, 
		    int do_mod_only, 
		    unsigned char *no_mod, 
		    int do_pseudo);
static int ws_read(unsigned char *wsb,
		   unsigned char lines,
		   int read_mode,
		   const char *terminate_list,
		   char *pfkey,			
		   unsigned char *no_mod,
		   int do_pseudo,
		   int pseudo2space);
static int mod_pb(unsigned char c,unsigned char last_atr,int do_pseudo);
static int ws_posit(int up, int right, int do_pseudo);
static int ws_tag_alt(int alt_read, int row, int col, unsigned char *no_mod);		/* Set flags on altered fields.		*/
static int ws_backspace(int row,int col,int alt_read,unsigned char *no_mod,int do_pseudo); /* Delete the previous character.	*/
static void ws_clearbefore(int row,int col,int alt_read,unsigned char *no_mod,int do_pseudo);	/* Clear field cursor left.	*/
static int ws_clearafter(int row,int col,int alt_read,unsigned char *no_mod,int do_pseudo); 	/* Clear field after cursor.	*/
static int ws_clearfield(int cursor_row,int cursor_col,int alt_read,unsigned char *no_mod,int do_pseudo);
static int ws_next(int start_row, int start_col, int tab_type, int do_pseudo);		/* Move to the next field.		*/
static int ws_last(int start_row, int start_col, int do_pseudo);			/* Move to the last field.		*/
static int next_mp(int start_row, int start_col, int the_char, int do_pseudo);		/* Move to the next menu pick.		*/
static int ws_insert(int cursor_row, int cursor_col, int alt_read, unsigned char *no_mod, int do_pseudo);
static int ws_remove(int cursor_row, int cursor_col, int alt_read, unsigned char *no_mod, int do_pseudo);
static int tab_fac(int row, int col);							/* Determine if start of field fac.	*/
static int menu_pick_combo_fac(int row, int col);					/* Determine if LOW PROT NUM fac or	*/
static int ws_trans_atr(fac_t ch);							/* Translate from Wang to VIDEO		*/
static int ws_bloff(int do_pseudo);							/* Turn blinking off.			*/
static void ws_echo(int the_char,int cursor_row,int cursor_col,int alt_read,unsigned char *no_mod,int do_pseudo);
static int load_sub_chars(void);							/* Default load of the subst. table.	*/
static void load_user_sub_chars(unsigned char *tblptr);					/* Do the load of substitution values.	*/
static int sub_char(unsigned char c,unsigned char *xc,int *char_set);			/* Get the substitution character and	*/
static void wsdmp_scrn(unsigned char *wsb, int type_d);
static void ws_putbuff(unsigned char c, int row, int col, int rendition, int font);	/* Put char. in line buf. accumulator.	*/
static void ws_dumpbuff(int row,int eol);						/* Dump the current buffer.		*/
static int check_mp(int line,int col,int rend,int do_pseudo);				/* See if it is a menu pick item.	*/
static void rewrite_mp(int line, int col, int rend, int do_pseudo);			/* Rewrite line or until next FAC.	*/
static unsigned char *data_map_ptr(int row, int col);
static fac_t *attr_map_ptr(int row, int col);
static unsigned char *vchr_map_ptr(int row, int col);
static unsigned char *vatr_map_ptr(int row, int col);
static int wsetwid(int wd);

static int adjust_for_multibyte(int row, int col, int dvert, int dhoriz);
static unsigned char *map_multibyte(unsigned char *rowdata,unsigned char *caller_rowbuf);

static int timed_read(long int *seconds_remaining);
static time_t stop_time(void);
static void set_stop_time(time_t the_time);

static int in_edit(int row, int col);
static void edit_init_struct(void);
static void edit_init_window(void);
static void edit_erase_box(void);
static void edit_main(int input,int row,int col,int *filling,
		      const char *terminate_list,
			char *pfkey,unsigned char *no_mod);
static void edit_compute_pos(int row, int col);
static void edit_putchar(int input);
static void edit_wrap(int lineno, int direction, int line_space);
static void edit_tab(int row, int col);
static void edit_clear_before(int row, int col);
static void edit_clear_after(int row, int col);
static void edit_delleft(void);
static int edit_chk_wrap_back(int row);
static void edit_delright(void);
static void edit_redraw(void);
static void edit_show_hilite(void);
static void edit_unmark(void);
static void edit_init_data(void);
static void edit_copycut_block(int cutflag);
static void edit_cut_block(void);
static void edit_copy_block(void);
static void edit_paste_block(void);
static void edit_open_line(int lineno);
static void edit_cut_line(int lineno);
static void edit_cut_data(int start_row, int start_col, int end_row, int end_col);
static int wsmode(int control);								/* Select character rendition.		*/
static int wscset(int char_set);							/* Switch to requested character set.	*/

static void mark_hotspots(int row_x, int numcols);
static void copy_clean_row(int row_x, int numcols, char *the_row);

static int meta_pfkey(int pfkey);
static int wsmove(int line,int column);							/* Move to a location on the screen.	*/
static int check_scrn(void);

static int vwputc(int c);
static int mousenonmod(void);


void WS_REWRITE(
		unsigned char *wsb, 
	const	unsigned char lines[1], 
		unsigned char no_mod[2])
{
	const	unsigned char function[1] = { WRITE_ALL };
			 char term_pfkey[2];

	no_mod[0] = ' ';	/* Clear the status with spaces */
	no_mod[1] = ' ';
	vwang(	function,
		wsb,
		lines,
		"X",
		term_pfkey,
		no_mod);
}

void WS_READ(
		unsigned char *wsb,
	const	unsigned char lines[1],
			 char term_pfkey[2],
		unsigned char no_mod[2])
{
	const	unsigned char function[1] = { READ_ALL };
	const		 char *terminate_list = "A";

	vwang(	function,
		wsb,
		lines,
		terminate_list,
		term_pfkey,
		no_mod);
}

void WS_READ_ALT(
		unsigned char *wsb,
	const	unsigned char lines[1],
			 char term_pfkey[2],
		unsigned char no_mod[2])
{
	const	unsigned char function[1] = { READ_ALTERED };
	const		 char *terminate_list = "A";

	vwang(	function,
		wsb,
		lines,
		terminate_list,
		term_pfkey,
		no_mod);
}

void WS_CLOSE()
{
	unsigned char function[1] = { CLOSE_WORK_STATION };
	unsigned char wsb[1924]; 
	unsigned char lines[1] = { 24 };
		 char term_pfkey[2];
	unsigned char no_mod[2];

	vwang(	function,
		wsb,
		lines,
		"X",
		term_pfkey,
		no_mod);
}

/*
	vwang		Emulate the wang workstation.

			Note: On a close only "Function" is passed.
*/

int vwang(
	const	unsigned char function[1],
		unsigned char *wsb,
	const	unsigned char lines[1],
	const		 char *terminate_list,
			 char term[2],
		unsigned char no_mod[2])
{
	enum e_vop	op_save;							/* Save current level of optimization.	*/
	char	def_psb_select;
	char	psb_select;
	int	i;
	static int first = 1;
	
	if (first)
	{
		vwang_load_charmap(0);							/* Load the translation tables		*/
		
		if (use_costar())
		{
			char *ptr;
			
			costar_before_read_api  = (ptr = getenv("W4WPREREAD"))   ? wisp_strdup(ptr) : NULL;
			costar_after_read_api   = (ptr = getenv("W4WPOSTREAD"))  ? wisp_strdup(ptr) : NULL;
			costar_before_write_api = (ptr = getenv("W4WPREWRITE"))  ? wisp_strdup(ptr) : NULL;
			costar_after_write_api  = (ptr = getenv("W4WPOSTWRITE")) ? wisp_strdup(ptr) : NULL;

			w4w_tabstop_vmode = costar_tabstop_vmode();
		}

		if (OPTION_NULLISDOT)
		{
			opt_nullisdot = 1;
		}

		first = 0;
	}
	
	vwang_init_video();
	if (wisp_winsshd() && !wbackground())
	{
		/*
		** Hack to force vrawinit()
		** Added in 5.1.10 (5.1.50) to support WinSSHD - What problem was being solved?
		*/
		vrawprint(0);
	}

	if (use_custom_vwang())
	{
		if (0 == custom_vwang(function,wsb,lines,terminate_list,term,no_mod))
			return 0;
	}

	
	
	i = (int)*function;
	if (i >= 1 && i <= 14)
	{
		char tracebuff[200];
		sprintf(tracebuff,"Function=%d %s", i, function_name[i]);

		switch(i)
		{
		case CLOSE_WORK_STATION:
		case TAB_MODE_OFF:
		case LOAD_SUB_TABLE:
		case INIT_DEFAULT_TABLE:
			/* nothing else to trace */
			break;

		case SET_TABS:
			sprintf(&tracebuff[strlen(tracebuff)], " Tabs=[%80.80s]", wsb);
			break;

		case DISPLAY_AND_READ:			/* Write then read screen?		*/
		case DISPLAY_AND_READ_ALTERED:		/* Write then read altered fields?	*/
		case READ_ALL:				/* Read the screen?			*/
		case READ_ALL_PB:			/* Read the screen with pseudo blank	*/
		case READ_ALTERED:			/* Read altered part of screen?		*/
		case READ_MODIFIABLE:			/* Read MODIFIABLE part of screen?	*/
			{
				char ttermlist[200];
				int term_idx;

				for(term_idx=0;term_idx<sizeof(ttermlist)-1;term_idx++)
				{
					ttermlist[term_idx] = terminate_list[term_idx];
					if ('A' == ttermlist[term_idx] || 'X' == ttermlist[term_idx] )
					{
						ttermlist[++term_idx] = '\0';
						break;
					}
					if (!isdigit((int)ttermlist[term_idx]))
					{
						ttermlist[term_idx] = '\0';
						strcat(ttermlist," (BAD)");
						break;
					}
				}

				sprintf(&tracebuff[strlen(tracebuff)], " TermList=[%s]", ttermlist);
			}

			/* FALL THROUGH to add "Lines" to trace */

		case WRITE_ALL:				/* Write the full screen.		*/
		case WRITE_ALL_PB:			/* Write the full screen with pseudo	*/
		case WRITE_SELECTED:			/* Write selected (modified only) fields*/

			sprintf(&tracebuff[strlen(tracebuff)], " Lines[%d]", (int)*lines);
			break;
		}
		WL_wtrace("WVIDEO", "FUNCTION", "%s", tracebuff);
	}
	else
	{
		WL_wtrace("WVIDEO", "FUNCTION", "Function=%d UNKNOWN", i);
	}

	if (NULL == term) 	term   = dummy_term;
	if (NULL == no_mod) 	no_mod = dummy_no_mod;

	VL_vgeterr();									/* Clear any pre-existing video error	*/

#ifdef unix
	if (!WL_ishelpactive() && VL_vsharedscreen())
	{
		/*
		**	This saves the current stty values so it can be restored by VL_vstate(VSTATE_RESTORE_STTY);
		**	It then ensures the stty is in-sync with video's expectations in case the COBOL debugger has changed it.
		**	It then force video to resynchronize it's maps and redraw the screen. We assume that
		**	the debugger has filled the screen with source code etc. so each time in we want to
		**	completely redraw the screen.
		*/
		VL_vstate(VSTATE_SAVE_STTY);
		if (!rts_first)
		{
			if ( (	READ_ALL	== *function ||
				READ_ALL_PB	== *function ||
				READ_ALTERED	== *function ||
				READ_MODIFIABLE == *function	)  ||
			   ( (	WRITE_ALL	== *function ||
				WRITE_ALL_PB	== *function ||
				WRITE_SELECTED	== *function	) && *lines != WSB_ROWS )  )
			{
				/*
				**	Force a HARD REFRESH if we are doing a read or
				**	if doing a partial screen write.
				*/
				VL_vdefer_restore();
				VL_vrefresh(HARD_REFRESH);
				VL_vcontrol_flush();
				vwang_set_synch(FALSE);
			}
			else
			{
				/*
				**	Force a clear of the screen to get rid of the debugger screen.
				*/
				vwang_set_synch(TRUE);
			}
		}
	}
#endif

	if (rts_first)									/* First time in program?		*/
	{
		if (vwang_init_screen())						/* Initialize the screen.		*/
		{
			terminal_error = FALSE;						/* Initilization ok so no term error.	*/
			if (wcurwidth != WS_DEFAULT_COLUMNS_PER_LINE)			/* Are we using the default width?	*/
			{
				if (wcurwidth == 80)  VL_vscreen(VSCREEN_NARROW);		/* Are we now in 80 column mode?	*/
				else VL_vscreen(VSCREEN_WIDE);				/* No, then select wide screen.		*/
				vwang_set_synch(TRUE);					/* Now re-synchronize.			*/
			}
		}
		else terminal_error = TRUE;
	}
	else if (check_scrn() == 0) terminal_error = TRUE;				/* Catch errors.			*/
	else if (VL_synch_required) vwang_ws_erap(FULL_SCREEN);				/* Somebody did something behind us.	*/

	/*
	**	Turn down the video optimization so that nothing
	**	gets deferred because we use vrawprint().
	*/
	op_save = VL_voptimize(VOP_DATA_AND_CONTROLS);					/* Turn optimization off (do it here).	*/

	WL_get_defs(DEFAULTS_PSB_REN,&psb_rndt);					/* Init the pseudo blank rendition.	*/
	WL_get_defs(DEFAULTS_PSB_SET,&psb_chset);					/* Init the pseudo blank char set.	*/
	WL_get_defs(DEFAULTS_PSB_CHAR,&def_psb_select);					/* Get the pb selection B,1,2,3,4	*/
	WL_get_psb_char(def_psb_select,&psb_char,&psb_select);				/* Get the real pseudo blank character.	*/

	WL_get_defs(DEFAULTS_AUTOMOVE,&auto_move);					/* Init the auto move flag.		*/
	WL_get_defs(DEFAULTS_MP_CURSOR,&mp_cursor);					/* Init the menu pick cursor flag.	*/
	WL_get_defs(DEFAULTS_BGCHANGE,&bgchange);					/* Init the background change flag.	*/
	WL_get_defs(DEFAULTS_BGCOLOR,&bgcolor);						/* Init the background color.		*/

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
		{
			/* Do not erase the screen in here!!!	*/
			VL_vdefer_restore();						/* Short cut, bring screen up to date.	*/
			VL_vshut();							/* Cancel pending input only.		*/
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
		case WRITE_SELECTED:							/* Write selected (modified only) fields*/
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
			sub_table_loaded = FALSE;					/* Force a reload.			*/
			vwang_load_charmap(1);						/* Force reload translation tables	*/
			break;
		}
		case LOAD_SUB_TABLE:							/* Set user defined values in sub table.*/
		{
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
			werrlog(WERRCODE(67002),*function,0,0,0,0,0,0,0);			/* Invalid function			*/
			VL_voptimize(op_save);						/* Restore optimization level.		*/
			return(FAILURE);						/* Return to the caller.		*/
		}
	}

#ifdef unix
	if (!WL_ishelpactive() && VL_vsharedscreen())
	{
		VL_vstate(VSTATE_RESTORE_STTY);						/* Restore the debugger term state.	*/
	}
#endif

	VL_voptimize(op_save);								/* Restore optimization level.		*/
	vwang_set_synch(FALSE);								/* A synch is not required now.		*/

	

	switch((int)*function)
	{
	case DISPLAY_AND_READ:			/* Write then read screen?		*/
	case DISPLAY_AND_READ_ALTERED:		/* Write then read altered fields?	*/
	case READ_ALL:				/* Read the screen?			*/
	case READ_ALL_PB:			/* Read the screen with pseudo blank	*/
	case READ_ALTERED:			/* Read altered part of screen?		*/
	case READ_MODIFIABLE:			/* Read MODIFIABLE part of screen?	*/
		WL_wtrace("WVIDEO", "RETURN", "READ Termkey=[%2.2s] Status=[%2.2s]", term, no_mod);
		break;
	}

	return(SUCCESS);								/* Return that we were successful.	*/
}



/*
**	Routine:	ws_write()
**
**	Function:	Subroutine to display screen or line.
**
**	Description:	{Full detailed description}...
**
**	Arguments:
**	wsb		Work station I/O buffer.
**	lines		Number of lines to write.
**	do_mod_only	Flag indicates selected (modified only) field write.
**	no_mod		Pointer to no-modification flag.
**	do_pseudo	Do Pesudo blank processing
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	mm/dd/yy	Written by xxx
**
*/
static int ws_write(unsigned char *wsb, unsigned char lines, int do_mod_only, unsigned char *no_mod, int do_pseudo)
{
	int start_row;									/* Row for single line or scrolling.	*/
	int wcc;									/* Write control character.		*/
	int crs_row, crs_col;								/* Cursor position.			*/
	int lin_1st, col_1st;								/* First modifyable field position.	*/
	int lin_err, col_err;								/* First error field location.		*/
	unsigned char last_atr;								/* Last Wang attributes.		*/
	int last_mode;									/* Last VIDEO character rendition.	*/
	unsigned char c_orig;								/* Working character.			*/
	fac_t c_mapfac;
	unsigned char *dm;								/* Working pointers to character maps.	*/
	fac_t *am;		
	register int row_x, col_x;							/* Row and col index			*/
	int new_row;									/* Is there new data for curr row	*/
	int row_actual;
	int use_costar_flag = 0;
	int use_w4w_flag = 0;

#ifdef DEBUG_GEN_WSB_FILES
        {
	/*
	**	This is some debugging code which dumps the WSB out to a file.
	*/
	if (WSB_ROWS == lines)
	{
		static int seq=0;
		char	path[256];
		int	fh;

		sprintf(path,"vwang%03d.wsb",seq++);
		fh = open(path,O_WRONLY|O_CREAT|O_TRUNC,0666);
		if (-1 != fh)
		{
			write(fh,wsb,WSB_LENGTH);
			close(fh);
		}
	}
        }
#endif /* DEBUG_GEN_WSB_FILES */

	start_row = wsb[OA_ROW];							/* get the row				*/
	if (start_row == 0) start_row = 1;						/* correct for row == 0			*/

	if ((start_row	< 1) || ((int)(start_row+lines-1) > WS_MAX_LINES_PER_SCREEN))	/* Test for valid values		*/
	{
		werrlog(WERRCODE(67004),start_row,lines,0,0,0,0,0,0);			/* Invalid Row	or Row+lines		*/
		return(FAILURE);
	}
	start_row -= 1;									/* make row zero based			*/

	wcc = wsb[OA_WCC];								/* Get the write control character.	*/

	if ((wcc & ROLL_UP) || (wcc & ROLL_DOWN))					/* Scroll the screen up or down?	*/
	{
		werrlog(WERRCODE(67006),0,0,0,0,0,0,0,0);					/* Scroll not implemented		*/
		return(FAILURE);
	}

	crs_col = wsb[OA_CURSOR_COL];							/* Get the cursor column position.	*/
	if ((wcc & POSITION_CURSOR) && (crs_col < 0 || crs_col > wcurwidth))
	{
		werrlog(WERRCODE(67016),crs_col,0,0,0,0,0,0,0);				/* Invalid column in order area.	*/
		return(FAILURE);
	}
	crs_col -= 1;									/* Make zero based			*/

	crs_row = wsb[OA_CURSOR_ROW];							/* Get the cursor line number.		*/
	if ((wcc & POSITION_CURSOR) && (crs_row < 0 || crs_row > WS_MAX_LINES_PER_SCREEN))
	{
		werrlog(WERRCODE(67018),crs_row,0,0,0,0,0,0,0);				/* Invalid line in order area.		*/
		return(FAILURE);
	}
	crs_row -= 1;									/* make line zerobased			*/

	wsb += OA_LENGTH;

	lin_1st = -1;									/* First mod field not found yet.	*/
	lin_err = -1;									/* No error field found yet.		*/
	errset = FALSE;									/* No errors on the screen yet.		*/
	lbcount = 0;									/* No characters in accumulator yet.	*/
	lbdangle = 0;									/* No dangling spaces in accumulator.	*/
	if (WLNC_use_netroncap()) 
	{
		do_pseudo = 0;								/* NetronCap screen do not have pseudos.*/
	}

	use_w4w_flag = use_w4w();

	if ((use_costar_flag = use_costar()))
	{
		/*
		**	Do not do pseudo blank processing with COSTAR
		*/
		do_pseudo = 0;
		use_w4w_flag = 1;

		if (costar_before_write_api)
		{
			costar_ctrl_api(costar_before_write_api);
		}
	}

	if (wcc & UNLOCK_KEYBOARD) 
	{
		vwang_set_aid(AID_UNLOCKED);
	}
	else 
	{
		vwang_set_aid(AID_LOCKED);
	}

	VL_vbuffering_start();								/* Turn on buffering if not on.		*/

	if (bgchange)									/* Should we change the background?	*/
	{
		if      (bgcolor && !VL_vscreen_check(VSCREEN_LIGHT)) VL_vscreen(VSCREEN_LIGHT);	/* Change the screen color.	*/
		else if (!bgcolor && !VL_vscreen_check(VSCREEN_DARK)) VL_vscreen(VSCREEN_DARK);

		if (VL_synch_required) vwang_ws_erap(FULL_SCREEN);			/* Somebody did something behind us.	*/
	}

	if (wcc & ERASE_AND_PROTECT)							/* Erase and protect from cursor?	*/
	{										/* Do the erase from here.		*/
		if ((start_row+lines) != WS_MAX_LINES_PER_SCREEN)			/* Do we actually need to do it?	*/
		{
			wsmove(start_row,0);						/* Move to the specified location.	*/
			vwang_ws_erap(TO_EOS);						/* Erase to the end of the screen.	*/
		}
	}

	/*
	**	Loop thru the WSB looking for EDIT window FACs
	*/
	for (row_actual = row_x = 0; row_x < WS_MAX_LINES_PER_SCREEN; row_x++)		/* Loop through each line.		*/
	{
		unsigned char scrbyte;

		if ((row_x >= start_row) && (row_x < (int)(start_row+lines)))		/* Is there data for this row?		*/
			new_row = TRUE;
		else
			new_row = FALSE;

		dm = data_map_ptr(row_x,0);						/* Point to the current entry.		*/
		
		if (new_row)
		{
			map_multibyte(dm,dm);						/* wipe multibyte data from map to	*/
											/* prevent optimization			*/
			for (col_x=0;  col_x < wcurwidth; ++col_x)
			{
				scrbyte = wsb[row_actual*wcurwidth+col_x];
				scrbyte = vwang_fac(scrbyte);
				if (scrbyte == FAC_EDIT_END || scrbyte == FAC_EDIT_UPPER || scrbyte == FAC_EDIT_UPLOW)
				{
					if (!the_edit) edit_init_struct();
					
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

	/*
	**	If an EDIT window was found in the last loop then
	**	validate it.
	*/
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
		last_atr = DEFAULT_ATTR;						/* Reset attributes at start of line.	*/
		last_mode = VMODE_CLEAR;
		dm = data_map_ptr(row_x,0);						/* Point to the current entry.		*/
		am = attr_map_ptr(row_x,0);

		if ((row_x >= start_row) && (row_x < (int)(start_row+lines)))		/* Is there data for this row?		*/
			new_row = TRUE;
		else
			new_row = FALSE;

		for (col_x = 0; col_x < wcurwidth; col_x++)				/* Loop through each column.		*/
		{
			if (new_row)
			{
				c_orig = *wsb++;					/* Get data from input buffer.		*/
			}
			else
			{
				c_orig = *dm;						/* Use the data map			*/
			}

			/*
			** 	c_mapfac will be the fac or 0 if this is not a fac
			*/
			c_mapfac = vwang_fac(c_orig);					/* map it using fac table.  all FAC ops */
											/* must be done to c_mapfac, not c_orig */
			if (use_costar_flag && new_row)
			{
				/*
				**	Map the FACs for COSTAR
				*/
				if (c_mapfac)
				{
					c_mapfac = costar_fac(c_mapfac);
				}
			}
											/* Don't do anything if already in	*/
											/* map unless modified pseudo blank.	*/
			if (new_row && ((c_orig != *dm) || (last_atr != *am) || mod_pb(c_orig,last_atr,do_pseudo)))
			{
				if (c_mapfac == FAC_EDIT_END || c_mapfac == FAC_EDIT_UPPER || c_mapfac == FAC_EDIT_UPLOW)
				{
					c_mapfac = ' ';
					ws_putbuff(' ',row_x,col_x,VMODE_CLEAR,VCS_DEFAULT);/* Put in output buffer.		*/
					*dm = ' ';
				}
				else if (FAC_FAC(c_mapfac))				/* Is this character a FAC?		*/
				{
					if (*no_mod != 'E') 
					{
						c_mapfac = FAC_CLEAR_MODIFIED(c_mapfac);/* Clear any altered bits that are set. */
					}

					/*
					**	The attribute map (am) value for a FAC is the FAC value.
					*/
					
					if (c_mapfac != *am)				/* Different only by altered bits?	*/
					{
						unsigned char facchar;
						int font;

						/*
						**	DISPFAC allows facs to be mapped to displayable
						**	characters if you want the facs to be visible.
						**	This is used for "screen scraping".
						*/
						WL_get_dispfac_char(c_orig,&facchar,&font);

						ws_putbuff(facchar,row_x,col_x,VMODE_CLEAR,font); /* Put in output buffer.	*/
						*dm = c_orig;				/* Record the data.			*/
						*am = c_mapfac;				/* Record the attributes.		*/
					}

					/*
					**	The last attribute is the last FAC value with the x80 bit stripped off.
					*/
					last_atr = FAC_CLEAR_FAC(c_mapfac);		/* Remember the other attributes.	*/
					last_mode = ws_trans_atr(c_mapfac);		/* Remember the last rendition.		*/
				}
				else if (!do_mod_only || FAC_MODIFIED(last_atr))
				{
					/*
					**	Not a FAC character.
					**
					**	This is done if we are processing all fields or
					**	if only processing mod field (selected) and this field is modifiable
					*/

					/*
					**	If ERASE_FIELDS flag then erase modifiable fields
					*/
					if ( !FAC_PROTECTED(last_atr) && (wcc & ERASE_FIELDS))
					{
						c_orig = ' ';
					}

					/*
					**	If do_pseudo then convert spaces to pseudo blanks in modifiable fields
					*/
					if ( do_pseudo && !FAC_PROTECTED(last_atr) && (' ' == c_orig) )
					{
						c_orig = PSEUDO_BLANK_CHAR;
					}
					

					*am = last_atr;					/* Record attributes.			*/
					*dm = c_orig;					/* Record the character.		*/


					/*
					**	Write out the individual character.
					**
					**	Handle special cases first.
					*/
					if (FAC_BLANK(last_atr))
					{
						/*
						**	If the BLANK attribute is on then always display a space.
						*/
						if (use_costar_flag)
						{
							/*
							**	Blank edit fields need to be underlined
							*/
							ws_putbuff(' ',row_x,col_x,last_mode,VCS_DEFAULT);
						}
						else
						{
							ws_putbuff(' ',row_x,col_x,VMODE_CLEAR,VCS_DEFAULT);
						}
					}
					else if (0 == c_orig)				/* Is it a null?			*/
					{
						/*
						**	NUL character is normally displayed as a space unless
						**	the NULLISDOT option is set.
						*/
						if (opt_nullisdot)	
						{
							ws_putbuff('.',row_x,col_x,last_mode,VCS_DEFAULT);
						}
						else
						{
							ws_putbuff(' ',row_x,col_x,last_mode,VCS_DEFAULT); 
						}
					}
					else if (FAC_PROTECTED(last_atr))
					{
						/*
						**	In a protected area of the screen
						*/
						if (FAC_NUMERIC(last_atr) && col_x > 0 && tab_fac(row_x,col_x-1))
						{
							/*
							**	NUMPROT MENU TAB-STOP (monkey bar)
							*/
							if (use_w4w_flag)
							{
								/*
								**	Use the w4w tabstop rendition
								**	and set the atrribute map to force a re-draw.
								*/
								ws_putbuff(c_orig,row_x,col_x,w4w_tabstop_vmode,VCS_DEFAULT);
								*am = FAC_SET_UNDERSCORED(*am);
							}
							else if (PSEUDO_BLANK_CHAR == c_orig)
							{
								/*
								**	Use the pseudo blank rendition
								*/
								ws_putbuff(c_orig,row_x,col_x, last_mode|psb_rndt,VCS_DEFAULT);
							}
							else
							{
								/*
								**	No special redition
								*/
								ws_putbuff(c_orig,row_x,col_x,last_mode,VCS_DEFAULT);
							}
						}
						else
						{
							/*
						        **	In a protected area of the screen - no special processing
							*/
							ws_putbuff(c_orig, row_x, col_x, last_mode, VCS_DEFAULT);
						}
					}
					else if (in_edit(row_x,col_x))
					{
						/*
						**	In an edit box (modifiable field), use bold redition.
						*/
						ws_putbuff(c_orig,row_x,col_x,VMODE_BOLD,VCS_DEFAULT);
					}
					else if (do_pseudo || PSEUDO_BLANK_CHAR == c_orig)
					{
						/*
						**	In a modifiable field.
						**	Use the pseudo blank rendition because:
						**	1) This is a pseudo blank character or
						**	2) Doing pseudo blank processing which can mean underlining mod fields
						*/
						ws_putbuff(c_orig, row_x, col_x, last_mode|psb_rndt, VCS_DEFAULT);
					}
					else
					{
						/*
						**	In a modifiable field - no special processing
						*/
						ws_putbuff(c_orig, row_x, col_x, last_mode, VCS_DEFAULT);
					}
				}
			}
			else		 
			{
				if (FAC_FAC(c_mapfac))
				{
					last_atr = FAC_CLEAR_FAC(c_mapfac);		/* Remember the other attributes.	*/
					last_mode = ws_trans_atr(c_mapfac);		/* Remember the last rendition.		*/
				}
			}

			if (new_row && lbdangle)					/* Are we dangling output?		*/
			{
				if ( !((*dm == ' ') || FAC_FAC(vwang_fac(*dm))) ) 
					lbdangle = 0; 					/* Don't dangle if visible.		*/
				else if (last_mode & (VMODE_REVERSE|VMODE_UNDERSCORE|VMODE_BLINK)) 
					lbdangle = 0;					/* Visible whitespace too...		*/
			}

			/*
			**	Looking for first tabstop on the screen, check if this is it.
			*/
			if (lin_1st < 0 && col_x > 0)
			{
				if ((row_x >= crs_row) && tab_fac(row_x,col_x-1) )
				{
					lin_1st = row_x;
					col_1st = col_x;
				}
				else if ( (vwang_fac(c_orig)==FAC_EDIT_UPPER) || (vwang_fac(c_orig)==FAC_EDIT_UPLOW) )
				{
					lin_1st = row_x;
					col_1st = col_x;
				}
				
			}

			if (FAC_FAC(*am) && (lin_err < 0) && FAC_BLINK(*am) && !FAC_PROTECTED(*am))
			{								/* Is there an error in this field? 	*/
				errset = ON;						/* Yes so flag that there is errors.	*/
				lin_err = row_x;					/* Record where 1st error field is.	*/
				col_err = col_x + 1;
			}

			dm++;								/* Incrememt the buffer pointers.	*/
			am++;
		}

		if (use_w4w_flag && new_row)
		{
			mark_hotspots(row_x, wcurwidth);				/* Mark the hotspots on this row	*/
		}

		ws_dumpbuff(row_x,TRUE);						/* Dump the accumulated buffer.		*/
	}
	wsmode(VMODE_CLEAR);								/* Clear out the renditions.		*/
	wscset(VCS_DEFAULT);

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
		old_line = VL_vcur_lin;							/* Remember final position if moved.	*/
		old_column = VL_vcur_col;
	}

	if (wcc & SOUND_ALARM) VL_vbell();						/* Did he want the bell?		*/
	VL_vbuffering_end();							/* Dump the buffer to show the screen.	*/

	if (use_costar_flag)
	{
		if (costar_after_write_api)
		{
			costar_ctrl_api(costar_after_write_api);
		}
	}

	return(SUCCESS);								/* Return to the caller.		*/
}



/*					Subroutine to read data.								*/

static int ws_read(
		   unsigned char *wsb,
		   unsigned char lines,
		   int read_mode,
		   const char *terminate_list,
		   char *pfkey,
		   unsigned char *no_mod,
		   int do_pseudo,
		   int pseudo2space)
{
	unsigned char *wsbr;								/* Pointer to position return area.	*/
	unsigned char *wsbo;								/* Pointer to original screen from Wang.*/
	int	filling;								/* Flag to indicate screen is filling.	*/
	int	row;									/* Row to start reading.		*/
	int	at_edge;								/* Flag to indicate at edge of screen.	*/
	int	the_meta_char = 0;
	int	cached_meta_char = 0;
	int	save_col;
	unsigned char *dwsb;								/* Working character.			*/
	int	tab_type;								/* Indicate space bar, tab key, newline.*/
	int	alt_read;								/* Indicate this is an altered read.	*/
	int	v_error;								/* Error number from video		*/
	long	seconds_remaining;							/* The seconds remaining in a timed read*/
	
	int	multiblock=0;								/* end of cur field, number of bytes to */
											/* block (attempt to inp multi w/o room)*/
	unsigned char c_orig;
	fac_t 	c_mapfac;
	int	auto_tab;								/* To auto-tab or not to auto-tab.	*/
	int	use_costar_flag = 0;
	int	use_w4w_flag = 0;

	WL_get_defs(DEFAULTS_AUTOTAB,&auto_tab);					/* Init the auto tab flag.		*/

	if (read_mode == READ_ALTERED) alt_read = 1;					/* Set/clear altered flag.		*/
	else			       alt_read = 0;

	wsbo = wsb;									/* Set ptr to original screen from Wang.*/
	dwsb = wsb + 4;									/* Set pointer to dump screen.		*/

	multi_input_pos=0;

	row = *wsb++;									/* Get the row				*/
	if ( row == 0 ) row = 1;							/* Correct for row = 0			*/
	if ((row < 1) || ((int)(row + lines - 1) > WS_MAX_LINES_PER_SCREEN))		/* Validate values.			*/
	{
		werrlog(WERRCODE(67008),row,lines,0,0,0,0,0,0);				/* Invalid Row	or Row+lines		*/
		return(FAILURE);
	}
	row -= 1;									/* Make row zero based			*/
	wsbr = wsb + 1;									/* Remember where column position is.	*/
	wsb = wsb + 3;									/* Move to the data area.		*/
	if (alt_read == 1) *no_mod = 'N';						/* Assume no modifications.		*/

	use_w4w_flag = use_w4w();

	if ((use_costar_flag = use_costar()))
	{
		/*
		**	Setup for a costar read.  Enable the mouse.
		*/
		use_w4w_flag = 1;
		do_pseudo = 0;
		costar_enable_mouse(1);
		costar_errtext("");

		if (costar_before_read_api)
		{
			costar_ctrl_api(costar_before_read_api);
		}
	}

	menu_fl = check_mp(old_line,old_column,VMODE_BOLD,do_pseudo);			/* If a menu pick then BOLD it.		*/
	if (mp_cursor)									/* If usage constant is set to display. */
	{
		VL_vset_cursor_on();							/* Set cursor on.			*/
		cursor_on_flag = TRUE;
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
					if (!cursor_on_flag)
					{
						VL_vset_cursor_on();			/* Turn the cursor on.			*/
						cursor_on_flag = TRUE;			/* Cursor is on so set flag TRUE.	*/
					}
				}
				else if (cursor_on_flag)				/* else make sure it is off.		*/
				{
					VL_vset_cursor_off();				/* Turn the cursor off.			*/
					cursor_on_flag = FALSE;				/* Cursor is off so set flag FALSE.	*/
				}
			}
			ws_good = TRUE;							/* Assume character will be good.	*/

			if (cached_meta_char)
			{
				the_meta_char = cached_meta_char;
				cached_meta_char = 0;
			}
			else if (timed_read(&seconds_remaining))			/* If doing a timed read.		*/
			{
				int	timed_out;

				if (seconds_remaining > 0)				/* If time remaining			*/
				{							/* Do a timed read			*/
					the_meta_char = VL_vgetm_timed((int)seconds_remaining,&timed_out);	
				}
				else
				{
					timed_out = TRUE;				/* We have timed out			*/
				}

				if (timed_out)
				{
					memcpy(pfkey,"	",2);				/* Set pfkey to BLANKS.			*/
					filling = FALSE;				/* Done filling.			*/
					the_meta_char = 0;				/* Dummy the meta character		*/

					continue;					/* Break out of this loop		*/
				}

				/*
				**	Translate a Terminal character into a Wang character
				*/
				the_meta_char = TERM2WANG(the_meta_char);
			}
			else	/* Do a normal non-timed read */
			{
				the_meta_char = VL_vgetm();				/* Get a meta character.		*/

				the_meta_char = TERM2WANG(the_meta_char);
			}

			if ((v_error = VL_vgeterr()))					/* Check if an error occured		*/
			{
				werrlog(WERRCODE(67024),v_error,0,0,0,0,0,0,0);		/* Report error on video read		*/
				wexit(WERRCODE(67024));					/* Exit the process			*/
			}

			if (IVS_outctx)							/* using a language translation file?	*/
			{								/* if so, then keep track of any multi	*/
											/* byte sequences that come thru the	*/
											/* input stream				*/
				if (multi_input_pos == 3)				/* got 3rd byte?			*/
					multi_input_pos=0;				/* yes, then start over			*/
				if (the_meta_char < 256)				/* make sure it's within table size	*/
				{
					/* check to see if its a valid byte in a multi sequence */
					/* 0xff means not valid		*/
					if ((IVS_outctx->indbuf)[the_meta_char+(multi_input_pos<<8)]==0xff)   
					{
						multi_input_pos=0;
					}
					else
					{
						++multi_input_pos;			/* else bump the counter		*/
					}
				}
				else
				{
					multi_input_pos=0;
				}
			}

			if (menu_fl && the_meta_char == SPACE_BAR)			/* If at a menu pick and hit space bar. */
			{
				the_meta_char = tab_key;				/* Change SPACEBAR into TABKEY		*/
				tab_type = TAB_SPACEBAR;				/* Hit the space bar from a menu pick.	*/
			}
			else
			{
				tab_type = TAB_NORMAL;					/* Set to normal tab actions.		*/
			}

			if (the_meta_char == 0)						/* A timeout?				*/
			{
				/*
				**	This is to handle the case of VL_vgetm() returning a NULL char.
				**	This will probably never happen but if it does it means a timeout has occurred.
				*/
				memcpy(pfkey,"	",2);					/* Set pfkey to BLANKS.			*/
				filling = FALSE;					/* Done filling.			*/
			}
			else if (in_edit(VL_vcur_lin,VL_vcur_col))
			{
				edit_main(the_meta_char,VL_vcur_lin,VL_vcur_col,&filling,terminate_list,pfkey,no_mod);
			}
			else if (menu_fl && IS_PRINTABLE_CHAR(the_meta_char,max_data_range))
			{								/* At menu pick and i is displayable.	*/
				next_mp(VL_vcur_lin,VL_vcur_col,the_meta_char,do_pseudo);	/* Search next menu pick beg. with i.*/
			}
			else if (auto_move && IS_PRINTABLE_CHAR(the_meta_char,max_data_range) && !vwang_ws_mod(VL_vcur_lin,VL_vcur_col))
			{
				ws_next(VL_vcur_lin,VL_vcur_col,TAB_NORMAL,do_pseudo);
			}
			else if (IS_PRINTABLE_CHAR(the_meta_char,max_data_range) && vwang_ws_mod(VL_vcur_lin,VL_vcur_col))
			{								/* Displayable char in mod pos?		*/
				at_edge = FALSE;					/* Not at edge.				*/
				if (FAC_NUMERIC(*attr_map_ptr(VL_vcur_lin,VL_vcur_col)))	/* Digits only?				*/
				{
					if (max_data_range == SEVEN_BIT_DATA || the_meta_char<=SEVEN_BIT_DATA)
					{
						if (isdigit((int)the_meta_char) || 
							(the_meta_char=='.') || 
							(the_meta_char==',') || 
							(the_meta_char=='+') || 
							(the_meta_char=='-') || 
							(the_meta_char==' ')	)
						{
							save_col = VL_vcur_col;
							ws_echo(the_meta_char,VL_vcur_lin,VL_vcur_col,alt_read,no_mod,do_pseudo);
							if ((VL_vcur_col == VL_vscr_wid-1) && (VL_vcur_col == save_col))
								at_edge=TRUE; 
						}
						else 
						{
							vwang_bad_char();		   /* Bells toll.*/
						}
					}
					else
					{
						if (user_numeric_table[the_meta_char-128] || 
							(the_meta_char == '.') || 
							(the_meta_char == ',') || 
							(the_meta_char == '+') || 
							(the_meta_char == '-') || 
							(the_meta_char == ' ')	)
						{
							save_col = VL_vcur_col;
							ws_echo(the_meta_char,VL_vcur_lin,VL_vcur_col,alt_read,no_mod,do_pseudo);
							if ((VL_vcur_col == VL_vscr_wid-1) && 
							    (VL_vcur_col == save_col)      ) /* At edge?	 */
								at_edge=TRUE; 
						}
						else 
						{
							vwang_bad_char();		/* Bells toll.*/
						}
					}
				}
				else							/* Ok insert.	*/
				{
					/* if user starts a multibyte, make sure there's room in the field */
					if (multi_input_pos==1 && (vwang_ws_eof(VL_vcur_lin,VL_vcur_col) - VL_vcur_col)<(IVS_outctx->srcsize-1))
					{
						multiblock=IVS_outctx->srcsize;		/* setup to block the bytes */  
					}
					if (multiblock)					/* are we blocking a multi sequence?	*/
					{
						vwang_bad_char();			/* yup, beep  */
						--multiblock;				/* and decrement */
						continue;
					}
					if (FAC_UPPER(*attr_map_ptr(VL_vcur_lin,VL_vcur_col)) && !multi_input_pos) 
					{
						if (max_data_range == SEVEN_BIT_DATA || the_meta_char <= SEVEN_BIT_DATA)
						{
							the_meta_char = toupper(the_meta_char); /* Uppercase?	*/
						}
						else
						{
							if (user_toupper_table[the_meta_char-128] > 0)
								the_meta_char = user_toupper_table[the_meta_char-128];
						}
					}
					
					save_col = VL_vcur_col;					/* Remember col */
					ws_echo(the_meta_char,VL_vcur_lin,VL_vcur_col,alt_read,no_mod,do_pseudo);
					if ((VL_vcur_col == VL_vscr_wid-1) && 
					    (VL_vcur_col == save_col)      ) at_edge = TRUE;	/* At edge?	*/
				}
				if (at_edge || FAC_FAC(*attr_map_ptr(VL_vcur_lin,VL_vcur_col)))	/* Full?	*/
				{
					if (auto_tab) 
					{
						ws_next(VL_vcur_lin,VL_vcur_col,TAB_NORMAL,do_pseudo);/* Yes,autotab. */
					}
					else
					{
						if (at_edge) ws_posit( 0, 1,do_pseudo);
					}
				}
			}
			else if (the_meta_char == newline_key)					/* Hi the Wang EXECUTE key.	*/
			{
				ws_next(VL_vcur_lin,VL_vcur_col,TAB_NEWLINE,do_pseudo);
			}
			else if ((the_meta_char == return_key) || 
				 (the_meta_char == enter_key)    ) 
			{
				vwang_ws_fkey(0,&filling,terminate_list,pfkey,no_mod);
			}
			else if (VL_vfnkey(the_meta_char))		vwang_ws_fkey(the_meta_char,&filling,terminate_list,pfkey,no_mod);
			else if (the_meta_char == up_arrow_key)		ws_posit(-1, 0,do_pseudo);
			else if (the_meta_char == down_arrow_key)	ws_posit( 1, 0,do_pseudo);
			else if (the_meta_char == left_arrow_key)	ws_posit( 0,-1,do_pseudo);
			else if (the_meta_char == right_arrow_key)	ws_posit( 0, 1,do_pseudo);
			else if (the_meta_char == tab_key)
			{
				/* decide if tab goes to next field or next tab stop */
				if (vwang_tab_mode)
				{
					int cnt;
					
					/* compute offset to next tab stop */
					for (cnt=1; tab_map[VL_vcur_col+cnt] == ' ' && (VL_vcur_col+cnt)<WSB_COLS ; ++cnt);
					/* call ws_posit to do the work */
					if (VL_vcur_col+cnt<WSB_COLS)
						ws_posit( 0, cnt, do_pseudo);
					else
					{
						/* wrap to start of next line */
						for (cnt=0; tab_map[cnt] == ' ' && cnt < WSB_COLS ; ++cnt);
						if (cnt<WSB_COLS)
							ws_posit( 1, cnt - VL_vcur_col, do_pseudo);
					}
				}
				else
				{
					ws_next(VL_vcur_lin,VL_vcur_col,tab_type,do_pseudo);
				}
			}
			else if (the_meta_char == backtab_key)	
			{
				ws_last(VL_vcur_lin,VL_vcur_col,do_pseudo);
			}
			else if (the_meta_char == home_key)
			{
				check_mp(VL_vcur_lin,VL_vcur_col,VMODE_CLEAR,do_pseudo);	/* Clear bold rend if current menu pick.*/
				ws_next(0,0,TAB_NORMAL,do_pseudo);
			}
			else if (the_meta_char == help_key)
			{
				if (WL_ishelpactive())						/* Is help active?	*/
				{
					memcpy(pfkey, "FE", 2);					/* Return code FE.	*/
					no_mod[1] = AID_HELP;					/* Mode code too.	*/
					filling = FALSE;					/* Now we're done.	*/
				}
				else 
				{
					if (use_costar_flag) costar_enable_mouse(0);		/* Disable the mouse		*/

					if (EDE_using())
					{
						ws_bar_menu(ON,VL_vcur_lin,VL_vcur_col,0,alt_read,no_mod,do_pseudo);
					}
					else
					{
						vwang_help(ON);
					}

					/* reset this flag that may be changed, Help style two will leave this 
					 * set since it's menupick based instead of PFkey based.  
					 */
					menu_fl = check_mp(VL_vcur_lin,VL_vcur_col,VMODE_BOLD,do_pseudo); 

					if (use_costar_flag)
					{
						costar_enable_mouse(1);				/* Enable the mouse		*/
						costar_errtext("");
					}
				}
			}
			else if (the_meta_char == clear_field_key)
			{
				ws_clearfield(VL_vcur_lin,VL_vcur_col,alt_read,no_mod,do_pseudo);
			}
			else if (the_meta_char == clear_before_key) 
			{
				ws_clearbefore(VL_vcur_lin,VL_vcur_col,alt_read,no_mod,do_pseudo);
			}
			else if (the_meta_char == clear_after_key)
			{
				ws_clearafter(VL_vcur_lin,VL_vcur_col,alt_read,no_mod,do_pseudo);
			}
			else if (the_meta_char == delete_key) 
			{
				ws_backspace(VL_vcur_lin,VL_vcur_col,alt_read,no_mod,do_pseudo);	/* delete b4 cursor 		*/
			}
			else if (the_meta_char == insert_key) 
			{
				ws_insert(VL_vcur_lin,VL_vcur_col,alt_read,no_mod,do_pseudo);		/* open-under-cursor		*/
			}
			else if (the_meta_char == remove_key) 
			{
				ws_remove(VL_vcur_lin,VL_vcur_col,alt_read,no_mod,do_pseudo);		/* delete & closeup 		*/
			}
			else if ((the_meta_char == cancel_key) && errset)
			{
				ws_bloff(do_pseudo);					/* Go eliminate blinking fields.	*/
				errset = FALSE;						/* Error fields are now gone.		*/
			}
			else if (WLNC_use_netroncap() && the_meta_char == trigger1)
			{
				if (EDE_using())
				{
					nc_pop_menu(&filling,terminate_list,no_mod,pfkey);	/* Pop up window and get pfkey.		*/
				}
			}
			else if (v_mouse_click == the_meta_char && use_w4w_flag)
			{
				int	m_row, m_col, got_mouse_position;

				/*
				**	Get the mouse position
				*/
				got_mouse_position = 0;
				
				if (use_costar_flag && 0==costar_get_mouse_position(&m_row, &m_col))
				{
					got_mouse_position = 1;
				}

#ifdef WIN32
				if (!use_costar_flag && 0==vrawntcn_get_mouse_position(&m_row, &m_col))
				{
					got_mouse_position = 1;
				}
#endif
				if (got_mouse_position)
				{
					/*
					**	If mouse was clicked in a modifiable location then move cursor.
					*/
					if (vwang_ws_mod(m_row,m_col) || tab_fac(m_row,m_col-1))
					{
						ws_posit((m_row-VL_vcur_lin),(m_col-VL_vcur_col),do_pseudo);
					}
					else if (!FAC_FAC(*attr_map_ptr(m_row,m_col)) && 
						  FAC_NUMERIC(*attr_map_ptr(m_row,m_col)))
					{
						/*
						**	In a NUMPROT field (tabstop) 
						**	Position cursor to the tabstop
						*/
						for(;!tab_fac(m_row,m_col-1); m_col--) { /* Back up to just after the FAC */ }
						ws_posit((m_row-VL_vcur_lin),(m_col-VL_vcur_col),do_pseudo);
					}
					else
					{
						/*
						**	Check for W4WMAP hotspots
						*/
						char	the_row[WS_MAX_COLUMNS_ON_A_LINE+1];
						int	pfcode;

						copy_clean_row(m_row, wcurwidth, the_row);
						pfcode = w4w_click_row(m_col, the_row);
						if (-1 != pfcode)
						{
							cached_meta_char = meta_pfkey(pfcode);
						}
						else
						{
							cached_meta_char = 0;

							/*
							**	If allowed to mouse position to a non-mod field
							**	then position it.
							*/
							if (mousenonmod())
							{
								ws_posit((m_row-VL_vcur_lin),(m_col-VL_vcur_col),do_pseudo);
							}
						}
					}
				}
				else
				{
					vwang_bad_char();
				}
			}
			else 
			{
				vwang_bad_char();					/* Else beep.				*/
			}
		}
	}

	if (!fast_read)									/* Now read the block back.		*/
	{
		int	row_idx, col_idx;

		for (row_idx = row; row_idx < (int)(row+lines); row_idx++)		/* Loop through each line.		*/
		{
			for (col_idx = 0; col_idx < wcurwidth; col_idx++)		/* Loop through each column.		*/
			{
				unsigned char ufac;

				c_orig = *data_map_ptr(row_idx,col_idx);		/* Get this character from the map.	*/
				c_mapfac = *attr_map_ptr(row_idx,col_idx);		/* Get the mapped one too.		*/

				if (FAC_FAC(c_mapfac))					/* use the mapped for FAC comparison	*/
				{
					if (FAC_BLINK(c_mapfac) && !FAC_PROTECTED(c_mapfac))	/* Mod visible FAC?	*/
					{
						c_mapfac = FAC_SET_BRIGHT(c_mapfac);
					}
					ufac = vwang_unfac(c_mapfac);		/* now unmap it back using user's table */
					if (ufac == 0xff)			/* there is no fac in his table so use original */
					{
						ufac=c_orig;
					}
				}
				else 
				{
					ufac=0;
				}

				if (c_orig == PSEUDO_BLANK_CHAR && pseudo2space && 
					!FAC_PROTECTED(c_mapfac)	)		/* Do we change pseudo to space ?	*/ 
				{
					*wsb++ = ' ';					/* Change PB to Spaces.			*/
				}
				else 
				{
					*wsb++ = ufac?ufac:c_orig;			/* Else return the character.		*/
				}
			}
		}
	}
	old_line = VL_vcur_lin;								/* Remember where we ended.		*/
	old_column = VL_vcur_col;
	check_mp(old_line,old_column,VMODE_CLEAR,do_pseudo);				/* If menu pick, clear before return.	*/
	*wsbr++ = VL_vcur_col + 1;								/* Return ending position to caller.	*/
	*wsbr = VL_vcur_lin + 1;
	VL_vset_cursor_off();								/* Turn the cursor off again.		*/
	cursor_on_flag = FALSE;								/* Set the flag.			*/
	wsmode(VMODE_CLEAR);								/* Clear the mode and char set.		*/
	wscset(VCS_DEFAULT);

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
	vwang_timeout((int4)0);								/* Cancel any timeout			*/

	if (use_costar_flag)
	{
		/*
		**	Disable the mouse.
		*/
		costar_enable_mouse(0);
		costar_errtext("Working...");
		if (costar_after_read_api)
		{
			costar_ctrl_api(costar_after_read_api);
		}
	}

	return(SUCCESS);								/* Return to the caller.		*/
}




static int mod_pb(unsigned char c,unsigned char last_atr,int do_pseudo)
{
	if (!do_pseudo || c != ' ') return(FALSE);					/* Only test if a DISPLAY AND READ,	*/
											/* or not a space. FALSE.		*/
	if (!FAC_PROTECTED(last_atr)) return(TRUE);					/* Is this a modifiable field ?		*/
	else return(FALSE);
}											/* pseudo blank.			*/

static int ws_posit(int up, int right, int do_pseudo)					/* Position cursor.			*/
{
	register int row,col;								/* Working registers.			*/
	
	row = VL_vcur_lin + up;								/* Take temp copy of position.		*/
	if (row < 0) row = WS_MAX_LINES_PER_SCREEN - 1;					/* Wrapped up?				*/
	else if (row >= WS_MAX_LINES_PER_SCREEN) row = 0;				/* Wrapped down?			*/

	col = VL_vcur_col + right;								/* Compute new column.			*/
	if (col < 0) col = wcurwidth - 1;						/* Wrapped left?			*/
	else if (col >= wcurwidth) col = 0;						/* Wrapped right?			*/

	col = adjust_for_multibyte(row,col,up,right);

	check_mp(VL_vcur_lin,VL_vcur_col,VMODE_CLEAR,do_pseudo);				/* Unbold current menu pick item.	*/
	menu_fl = check_mp(row,col,VMODE_BOLD,do_pseudo);				/* Check if is a menu pick item.	*/

	
	return(SUCCESS);								/* Return to the caller.		*/
}

static int ws_tag_alt(int alt_read, int row, int col, unsigned char *no_mod)		/* Set flags on altered fields.		*/
{
	if (alt_read)
	{	/* jec: must use attr map for fac comparison */
		while (!FAC_FAC(*attr_map_ptr(row,col)))				/* Loop to find the start FAC.		*/
		{
			if ((--col) < 0)						/* Move left one column.		*/
			{
				werrlog(WERRCODE(67010),0,0,0,0,0,0,0,0);			/* No FAC for current field		*/
				return(FAILURE);
			}
		}
		*attr_map_ptr(row,col) = FAC_SET_MODIFIED(*attr_map_ptr(row,col));	/* Or in the fact that it is altered.	*/
		*no_mod = 'M';								/* Return the global modified tag.	*/
	}
	return(SUCCESS);								/* Oilswell.				*/
}

/*** NOTE: this is not a static because it is also used in EDE (edenetc.c) ***/
int vwang_ws_fkey(int metachar_key,int *filling,const char *terminate_list,char *pfkey,unsigned char *no_mod)
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
	sprintf(temp,"%02d", VL_vfnkey(metachar_key));					/* Convert to ascii representation.	*/
	
	if (terminate_list[0] == 'A') *filling = FALSE;					/* 'A' means all keys allowed.		*/

	j = 0;										/* Loop from start of terminate_list.	*/
	cnt = 0;

	while (*filling)								/* Loop until end of list or valid key. */
	{
		if (toupper(terminate_list[j]) == 'X')					/* End of list				*/
		{
			break;
		}

		if ( terminate_list[j] < '0' || terminate_list[j] > '3' ||		/* valid values are "00" - "33"		*/
		     !isdigit((int)terminate_list[j+1]) )
		{
			invalid_list = 1;
		}
		else
		{
			tl_num = (terminate_list[j] - '0')*10 + terminate_list[j+1] - '0';

			if (tl_num < 0 || tl_num > 33)
			{
				invalid_list = 1;
			}
		}

		if (invalid_list)
		{
			werrlog(WERRCODE(67026),terminate_list[j],terminate_list[j+1],0,0,0,0,0,0);
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
			werrlog(WERRCODE(67028),0,0,0,0,0,0,0,0);
			return(FAILURE);
		}
	}

	if (*filling) 
	{
		vwang_bad_char();							/* Not useable bell key.		*/
	}
	else
	{
		pfkey[0] = temp[0];							/* Return two characters.		*/
		pfkey[1] = temp[1];

		if (metachar_key) /* metachar_key has been dummied up with a 0 for ENTER key */
			no_mod[1] = vwang_meta_aid(metachar_key);			/* Return the correct AID byte value.	*/
		else
			no_mod[1] = AID_ENTER;
		vwang_set_aid(no_mod[1]);						/* Set the AID char (lock keyboard)	*/
	}

	return(SUCCESS);								/* Return to the caller.		*/
}

int vwang_ws_mod(int cursor_row, int cursor_col)					/* Determine if char is modifyable.	*/
{
	/*
	**	If PROTECTED or on a FAC then not modifiable.
	*/
	if (FAC_PROTECTED(*attr_map_ptr(cursor_row,cursor_col)))
	{
		return FALSE;
	}
	else if (FAC_FAC(*attr_map_ptr(cursor_row,cursor_col)))
	{
		return FALSE;
	}
	else
	{
		return TRUE;
	}
}

/*
**	ws_backspace():	Destructive backspace.
*/
static int ws_backspace(int row,int col,int alt_read,unsigned char *no_mod,int do_pseudo)/* Delete the previous character.	*/
{
	int	mode;
	int 	remove_size;
	int	i;
	
	if ((col-1 < 0) || !vwang_ws_mod(row,col-1))
	{
		vwang_bad_char();							/* Beep if nothing to delete.		*/
	}
	else
	{
		ws_posit(0,-1,do_pseudo);						/* Move Left 1 position.		*/

		if (cursor_on_multibyte)	/* if cursor is on a multibyte, erase several bytes */
		{
			remove_size = IVS_outctx->srcsize;

			/*
			**	Reset this flag
			*/
			cursor_on_multibyte = 0;
		}
		else
		{
			remove_size = 1;
		}

		col -= remove_size;
		
		for(i=0; i<remove_size; i++)
		{
			if (do_pseudo)
			{
				mode = ws_trans_atr(*attr_map_ptr(row, col)) | psb_rndt;
				*data_map_ptr(row, col+i) = PSEUDO_BLANK_CHAR;
			}
			else
			{
				mode = ws_trans_atr(*attr_map_ptr(row, col));
				*data_map_ptr(row, col+i) = ' ';
			}
			ws_putbuff(*data_map_ptr(row,col+i), row, col+i, mode, VCS_DEFAULT);
		}

		ws_dumpbuff(row, TRUE);
		wsmove(row,col);							/* Move back to insert pos		*/
		ws_tag_alt(alt_read,row,col,no_mod);					/* Set altered flags.			*/

		/*
		**	Reset this flag, we are on a space now.
		*/
		cursor_on_multibyte = 0;
		
	}
	return(SUCCESS);								/* Back, back, back...			*/
}

static void ws_clearfieldcols(int row, int first_col, int last_col, int do_pseudo)
{
	int	col;
	int	mode;
	unsigned char blank_char;
	
	if (do_pseudo)
	{
		mode = ws_trans_atr(*attr_map_ptr(row,first_col)) | psb_rndt;
		blank_char = PSEUDO_BLANK_CHAR;
	}
	else
	{
		mode = ws_trans_atr(*attr_map_ptr(row,first_col));
		blank_char = ' ';
	}
	
	
	for(col=first_col; col<=last_col; col++)
	{
		*data_map_ptr(row,col) = blank_char;
		ws_putbuff(blank_char, row, col, mode, VCS_DEFAULT);
	}

	ws_dumpbuff(row, FALSE);
}

/*
**	Clear before and closeup
*/
static void ws_clearbefore(int row,int col,int alt_read,unsigned char *no_mod,int do_pseudo)	/* Clear field cursor left.	*/
{
	int 	sof_col, eof_col, mode;
	int	i1, i2;
	unsigned char blank_char;

	if ((sof_col = vwang_ws_sof(row,col)) && (eof_col = vwang_ws_eof(row,col)))		/* Should we delete left?		*/
	{
		if (col == sof_col)
		{
			/* nothing to do */
			return;
		}

		if (do_pseudo)	
		{
			mode = psb_rndt | ws_trans_atr(*attr_map_ptr(row,sof_col));
			blank_char = PSEUDO_BLANK_CHAR;
		}
		else
		{
			mode = ws_trans_atr(*attr_map_ptr(row,sof_col));
			blank_char = ' ';
		}
		
		/*
		**	Shift the data starting at the cursor position to the beginning of the field
		*/
		for(i1 = col, i2 = sof_col; i1<=eof_col; i1++, i2++)
		{
			*data_map_ptr(row,i2) = *data_map_ptr(row,i1);
			ws_putbuff(*data_map_ptr(row,i2), row, i2, mode, VCS_DEFAULT);
		}

		/*
		**	Blank the end of the field
		*/
		for(;i2<=eof_col; i2++)
		{
			*data_map_ptr(row,i2) = blank_char;
			ws_putbuff(blank_char, row, i2, mode, VCS_DEFAULT);
		}
		
		ws_dumpbuff(row, FALSE);

		/*
		**	Move to begining of field
		*/
		wsmove(row,sof_col);							/* Move to the start of the field.	*/
		ws_tag_alt(alt_read,row,sof_col,no_mod);				/* Set altered flags.			*/

	}
	else vwang_bad_char();								/* Can't delete so beep.		*/
}

/* Delete to the right of the cursor.	*/
static int ws_clearafter(int row,int col,int alt_read,unsigned char *no_mod,int do_pseudo)  
{
	int 	eof_col;

	if ((eof_col = vwang_ws_eof(row,col)))						/* Should we clear?			*/
	{
		ws_clearfieldcols(row,col,eof_col,do_pseudo);
		wsmove(row,col);							/* Move to the start of the field.	*/
		ws_tag_alt(alt_read,row,col,no_mod);					/* Set altered flags.			*/
	}
	else vwang_bad_char();								/* Can't clear so beep.			*/

	return(SUCCESS);
}

static int ws_clearfield(int row, int col, int alt_read, unsigned char *no_mod, int do_pseudo)	
											/* Delete to the current field		*/
{
	int	sof_col;								/* Start of field column		*/
	int	eof_col;								/* End of field column			*/

	if ((sof_col = vwang_ws_sof(row,col)) && (eof_col = vwang_ws_eof(row,col))) 		/* Are we in a mod field	*/
	{
		ws_clearfieldcols(row,sof_col,eof_col,do_pseudo);
		wsmove(row,sof_col);							/* Move to where to clear from.		*/
		ws_tag_alt(alt_read, row, col, no_mod);					/* Set altered flags.			*/
	}
	else vwang_bad_char();								/* Can't clear so beep.			*/

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
**			If there are no tabable fields it will call vwang_bad_char().
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

static int ws_next(int start_row, int start_col, int tab_type, int do_pseudo)	/* Move to the next field.		*/
{
	int	x_row, x_col;

	x_row = start_row;							/* Set index values				*/
	x_col = start_col;

	for(;;)
	{
		/*
		**	Check if a tab stop
		*/

		if (tab_fac(x_row,x_col))					/* if a tab_fac found then validate		*/
		{
			/*
			**	This TABSTOP is valid if
			**		a) This was a normal TAB 
			**		b) This was a SPACEBAR from a menupick and we are at a new menupick
			**		c) This was a NEWLINE and we are on a different line (or earlier on this line)
			*/
			if (	(tab_type == TAB_NORMAL) ||
				(tab_type == TAB_SPACEBAR && menu_pick_combo_fac(x_row,x_col)) ||
				(tab_type == TAB_NEWLINE && (x_row != start_row || x_col < start_col))	)
			{
				check_mp(start_row,start_col,VMODE_CLEAR,do_pseudo);	/* Clear bold rend if current menu pick.*/
				menu_fl = check_mp(x_row,x_col+1,VMODE_BOLD,do_pseudo);	/* Bold the new position if a menu pick.*/
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
		}

		if (x_col == start_col && x_row == start_row)			/* Back where we started			*/
		{
			vwang_bad_char();
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
**			If there are no tabable fields it will call vwang_bad_char().
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
static int ws_last(int start_row, int start_col, int do_pseudo)			/* Move to the last field.	*/
{
	int	x_row, x_col;
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

	for(;;)
	{
		/*
		**	Check if a tab stop
		*/

		if (tab_fac(x_row,x_col))					/* if a tab_fac found then move			*/
		{
			check_mp(org_row,org_col,VMODE_CLEAR,do_pseudo);	/* Clear bold rend if current menu pick.	*/
			menu_fl = check_mp(x_row,x_col+1,VMODE_BOLD,do_pseudo);	/* Bold the new position if a menu pick.	*/
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
		}

		if (x_col == start_col && x_row == start_row)			/* Back where we started			*/
		{
			vwang_bad_char();
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
**			If there is no matching menupick it will call vwang_bad_char().
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
static int next_mp(int start_row, int start_col, int the_char, int do_pseudo)	/* Move to the next menu pick.		*/
{
	int	x_row, x_col;

	x_row = start_row;							/* Set index values				*/
	x_col = start_col;

	for(;;)
	{
		/*
		**	Check if a menu pick fac
		*/

		if (menu_pick_combo_fac(x_row,x_col))
		{
			/*
			**	This TABSTOP is valid if the first character on the screen matches the_char pressed.
			*/
			if (toupper(*data_map_ptr(x_row,x_col+3)) == toupper(the_char))	/* See if first char matches input char.*/
			{
				check_mp(start_row,start_col,VMODE_CLEAR,do_pseudo);	/* Clear bold rend if current menu pick.*/
				menu_fl = check_mp(x_row,x_col+1,VMODE_BOLD,do_pseudo);	/* Bold the new position if a menu pick.*/
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
		}

		if (x_col == start_col && x_row == start_row)			/* Back where we started			*/
		{
			vwang_bad_char();
			return(FAILURE);
		}
	}
}

static int ws_insert(int cursor_row, int cursor_col, int alt_read, unsigned char *no_mod, int do_pseudo)
											/* Open a column.			*/
{
	int eof_col, mode;								/* Working registers.			*/
	unsigned char *rowbuf;
	int saved, saveind;
	unsigned char savedata[WSB_COLS];
	fac_t saveattr[WSB_COLS];

	rowbuf = map_multibyte(data_map_ptr(cursor_row,0),NULL);		       /* map out multibyte chars on the line  */
	
	if ((eof_col = vwang_ws_eof(cursor_row,cursor_col)))			       /* Are we in a field?			*/
	{
		if (  *data_map_ptr(cursor_row,eof_col) != ' ' && 
		      *data_map_ptr(cursor_row,eof_col) != '\0' &&
		      *data_map_ptr(cursor_row,eof_col) != PSEUDO_BLANK_CHAR )
		{
			vwang_bad_char();						/* Don't push chars out of the field.	*/
			return FAILURE;
		}
		wsmove(cursor_row,cursor_col);						/* Move to where to insert the space.	*/
		VL_vset_cursor_off();
		cursor_on_flag=FALSE;

		memcpy(savedata,							/* save it */
		       data_map_ptr(cursor_row,cursor_col),
		       saved = eof_col-cursor_col);
		memcpy(data_map_ptr(cursor_row,cursor_col+1),				/* now adjust vwang's maps	       */
		       savedata,
		       saved);

		memcpy(saveattr,							/* save it */
		       attr_map_ptr(cursor_row,cursor_col),
		       saved);
		memcpy(attr_map_ptr(cursor_row,cursor_col+1),
		       saveattr,
		       saved);

		if (do_pseudo) 
		{
			/* Insert a pseudoblank char in the map.*/
			mode = ws_trans_atr(*attr_map_ptr(cursor_row,cursor_col)) | psb_rndt;
			*data_map_ptr(cursor_row,cursor_col) = PSEUDO_BLANK_CHAR;
		}
		else
		{
			/* Insert a blank char in the map.	*/
			mode = ws_trans_atr(*attr_map_ptr(cursor_row,cursor_col));
			*data_map_ptr(cursor_row,cursor_col) = ' ';
		}
		ws_putbuff(*data_map_ptr(cursor_row,cursor_col), cursor_row, cursor_col, mode, VCS_DEFAULT);

		/* now loop thru the video maps and print the contents a space to the right. */
		for (saveind = 0 ; saveind < saved; saveind++)
		{
			ws_putbuff(savedata[saveind], cursor_row, cursor_col+saveind+1, mode, VCS_DEFAULT);
		}

		ws_dumpbuff(cursor_row, TRUE);

		wsmove(cursor_row,cursor_col);						/* Move back to insert pos		*/
		ws_tag_alt(alt_read,cursor_row,cursor_col,no_mod);			/* Set altered flags.			*/
		VL_vset_cursor_on();
		cursor_on_flag=TRUE;
	}
	else vwang_bad_char();								/* Not in field so just beep.		*/

	return(SUCCESS);
}

/*
**	ws_remove: Delete the current character and close-up the space by shifting
**		   the rest of the line one character to the left.
*/
static int ws_remove(int cursor_row, int cursor_col, int alt_read, unsigned char *no_mod, int do_pseudo)
{
	int	eof_col;								/* end of field column			*/
	int	wrk_col;								/* working column			*/
	int	mode;									/* Working registers.			*/
	unsigned char *rowbuf;
	int	multi_value, multi_idx;

	/*
	**	If press remove on a multi-byte character then we need to remove
	**	all the characters.
	**
	**	multi_idx is the number of bytes that were removed.
	*/
	rowbuf = map_multibyte(data_map_ptr(cursor_row,0),NULL);			 /* map out multibyte chars on the line	 */
	multi_value = rowbuf[cursor_col];
	if (multi_value)
		multi_idx = IVS_outctx->srcsize;
	else
		multi_idx = 1;

	if ((eof_col = vwang_ws_eof(cursor_row,cursor_col)))				 /* Should we delete right?		*/
	{
		if (do_pseudo) mode = ws_trans_atr(*attr_map_ptr(cursor_row,cursor_col)) | psb_rndt;
		else	       mode = ws_trans_atr(*attr_map_ptr(cursor_row,cursor_col));

		memcpy(data_map_ptr(cursor_row,cursor_col),				/* do a fast mod of the maps  */
		       data_map_ptr(cursor_row,cursor_col+multi_idx),
		       eof_col-cursor_col-multi_idx+1);
		
		memcpy(attr_map_ptr(cursor_row,cursor_col),
		       attr_map_ptr(cursor_row,cursor_col+multi_idx),
		       eof_col-cursor_col-multi_idx+1);
		
		VL_vset_cursor_off();
		cursor_on_flag=FALSE;

		/* now reprint the line spacing out the removed character */
		for (wrk_col = cursor_col; wrk_col <= eof_col; ++wrk_col)
		{
			if (wrk_col > eof_col-multi_idx)
			{
				if (do_pseudo)
				{
					*data_map_ptr(cursor_row,wrk_col) = PSEUDO_BLANK_CHAR;
				}
				else
				{
					*data_map_ptr(cursor_row,wrk_col) = ' ';
				}
			}

			ws_putbuff(*data_map_ptr(cursor_row,wrk_col), cursor_row, wrk_col, mode, VCS_DEFAULT);

		}

		ws_dumpbuff(cursor_row, TRUE);

		wsmove(cursor_row,cursor_col);						/* Move to the start of the field.	*/
		ws_tag_alt(alt_read,cursor_row,cursor_col,no_mod);			/* Set altered flags.			*/
		VL_vset_cursor_on();
		cursor_on_flag=TRUE;
	}
	else vwang_bad_char();								/* Can't delete so beep.		*/

	return(SUCCESS);
}

int vwang_bad_char(void)								/* Process invalid characters.		*/
{
	VL_vbell();									/* Ring the bell.			*/
	ws_good = FALSE;								/* Character was not good.		*/
	return 0;
}

int vwang_ws_cut(int row, int col)							/* Cut the current field.		*/
{
	int col_idx, sof_col, eof_col;							/* Working registers.			*/
	char cutbuf[WS_MAX_COLUMNS_ON_A_LINE];						/* Working buffer.			*/

	if ((sof_col = vwang_ws_sof(row,col)) && (eof_col = vwang_ws_eof(row,col)))	/* Are we in a field?			*/
	{
		for (col_idx = sof_col; col_idx <= eof_col; col_idx++)			/* Copy the field.			*/
		{
			if ((cutbuf[col_idx-sof_col] = *vchr_map_ptr(row,col_idx)) < ' ') 
			{
				cutbuf[col_idx-sof_col] = ' ';				/* Copy a character.			*/
			}
		}
		cutbuf[col_idx-sof_col] = CHAR_NULL;					/* Terminate the string.		*/
		VL_vcut(cutbuf);								/* Output the result.			*/
	}
	else VL_vbell();									/* Oops, not in field so ring the bell.	*/
	return(SUCCESS);								/* Return.				*/
}

int vwang_ws_paste(int row, int col, int vr, int vc, int alt_read, unsigned char *no_mod, int do_pseudo)
											/* Cut the current field.		*/
{
	register int sof_col, eof_col;
	enum e_vop save_opt;

	if (vwang_ws_mod(row,col))							/* Are we in a modifyable field?	*/
	{
		sof_col = vwang_ws_sof(row,col);
		eof_col = vwang_ws_eof(row,col);					/* Are we in a field?			*/
		VL_vdefer_restore();							/* Restore from deferred actions.	*/
		save_opt = VL_voptimize(VOP_TRACKING_ONLY);				/* Turn off optimization for vwang.	*/
		ws_clearfield(vr,vc,alt_read,no_mod,do_pseudo);				/* Clear the field.			*/
		VL_voptimize(save_opt);							/* Restore the optimization.		*/
		VL_vpaste(eof_col-sof_col+1);						/* Paste only the allowed fields.	*/
		return(SUCCESS);							/* Return.				*/
	}
	else
	{
		VL_vbell();								/* Not modifyable so just ring bell.	*/
		return(FAILURE);							/* Return to the caller.		*/
	}
}

/*
**	Routine:	tab_fac()
**
**	Function:	To determine if this is a FAC that starts a field that can be tabbed to.
**
**	Description:	Verify that this is a FAC.
**			Verify it is not concatinated FAC's
**			Verify that it is a tabstop. (MODIFIABLE or NUMPROT)
**
**			Valid tabstop fac column offsets are 0 - 78 (wcurwidth-2) 
**
**	Input:		
**	row		The fac row number  	0-23
**	col		The fac column number  	0-79  (wcurwidth-1))
**			
**
**	Output:		None
**
**	Return:		SUCCESS = FAC for a tabable field.
**			FAILURE = not a tab FAC
**
**	Warnings:	None
**
*/
static int tab_fac(int row, int col)							/* Determine if start of field fac.	*/
{
	fac_t	the_fac;
	
	if (col < 0 || col >= (wcurwidth-1)) 
	{
		return(FAILURE);
	}

	if (the_edit && row==the_edit->top_row && col==the_edit->left_col-1)
	{
		return SUCCESS;
	}
	
	the_fac = *attr_map_ptr(row,col);
	
	if ( !FAC_FAC(the_fac) ) 
	{
		/* Not a FAC */
		return(FAILURE);
	}

	if ( FAC_FAC(*attr_map_ptr(row,col+1)) ) 
	{
		/* The next char is a FAC so this is not a tabstop (Verified behaviour on the Wang) */
		return(FAILURE);
	}

	if (!FAC_PROTECTED(the_fac))
	{
		/* Modifiable fields are tab stops */
		return (SUCCESS);
	}
	
	if (FAC_NUMERIC(the_fac))
	{
		/*
		**	If protected numeric then a tab stop.
		*/
		return(SUCCESS);
	}

	return(FAILURE);
}

/*
**	ROUTINE:	menu_pick_combo_fac()
**
**	FUNCTION:	Test if this position is a FAC for a menu pick combo.
**
**	DESCRIPTION:	The FAC is either numeric protected or numeric protected bold.
**			The following character is either x0B or x05.
**
**			Valid menu pick fac columns are 0 - 78 (wcurwidth-2)
**
**	ARGUMENTS:	
**	row		The fac row offset 	0-23
**	col		The fac column offset	0-79 (wcurwidth-1)
**
**	GLOBALS:	
**	wcurwidth	The screen width 80 or 132.
**
**	RETURN:		
**	FALSE		Not a menu pick combo
**	TRUE		Is a menu pick combo fac.
**
**	WARNINGS:	none
**
*/
static int menu_pick_combo_fac(int row, int col)					/* Determine if LOW PROT NUM fac or	*/
{											/* HIGH PROT NUM fac and is followed by */ 
	fac_t the_fac;									/* a WANG_MENU_PICK or PSEUDO BLANK_CHAR.*/
	unsigned char the_char;

	if (col < 0 || col >= (wcurwidth-1))
	{
		return FALSE;
	}

	the_fac = *attr_map_ptr(row,col);						/* Point to the current entry.		*/
	if (the_fac == NUMPROT_FIELD || the_fac == NUMPROT_BOLD_FIELD)			/* Is it the needed Fac?		*/
	{
		the_char = *data_map_ptr(row,col+1);
		if (the_char == WANG_MENU_PICK || the_char == PSEUDO_BLANK_CHAR)  
		{
			return TRUE;							/* Is needed fac, WANG_MENU_PICK or	*/
		}
	}										/* PSEUDO_BLANK_CHAR combo.		*/
	return FALSE;
}

/* Return the first column which is part of this field (0==Not a field) */
int vwang_ws_sof(int cursor_row, int cursor_col)					/* Get start of current field.		*/ 
{
	if (cursor_col < 0) return(FAILURE);						/* Return failure if not in field.	*/
	if (!vwang_ws_mod(cursor_row,cursor_col)) return(FAILURE);				/* If not modifiable then not in field	*/
	while ((cursor_col > 0) && (vwang_ws_mod(cursor_row,cursor_col))) --cursor_col;	/* Loop until at start of the field.	*/
	return(cursor_col+1);								/* Return the start of the field.	*/
}

/* Return the last column which is part of this field (0==Not a field) */
int vwang_ws_eof(int cursor_row, int cursor_col)					/* Get end of current field.		*/
{
	if (cursor_col < 0) return(FAILURE);						/* Return failure if not in field.	*/
	if (!vwang_ws_mod(cursor_row,cursor_col)) return(FAILURE);			/* If not modifiable then not in field	*/
	while ((cursor_col < wcurwidth) && (vwang_ws_mod(cursor_row,cursor_col))) cursor_col++;/* Loop until at the end of the field.	*/
	return(cursor_col-1);								/* Return the end of the field.		*/
}

/*
**	ROUTINE:	ws_trans_atr()
**
**	FUNCTION:	Translate Wang FAC display attributes into a video mode.
**
**	DESCRIPTION:	
**
**	ARGUMENTS:
**	ch		The FAC (or attribute map character)
**
**	GLOBALS:	None
**
**	RETURN:		The video mode
**
**	WARNINGS:	None
**
*/
static int ws_trans_atr(fac_t the_fac)							/* Translate from Wang to VIDEO		*/
{											/*     attribute formats.		*/
	static int use_costar_flag = -1;
	int v_mode;

	if (-1 == use_costar_flag)
	{
		use_costar_flag = use_costar();
	}
	
	v_mode = 0;

	switch(the_fac & FAC_MASK_RENDITION)
	{
	case FAC_RENDITION_BRIGHT:	v_mode = VMODE_BOLD;			break;
	case FAC_RENDITION_DIM:		v_mode = 0;				break;
	case FAC_RENDITION_BLINK:	v_mode = VMODE_BOLD | VMODE_BLINK;	break;	/* On Wang BLINK = BLINK + BOLD		*/
	case FAC_RENDITION_BLANK:	v_mode = 0;				break;
	}

	if (FAC_UNDERSCORED(the_fac)) 
	{
		v_mode |= VMODE_UNDERSCORE;						/* Or in underscore.			*/
	}

	if (2==use_costar_flag)
	{
		/*
		**	For COSTAR V2 mode add the REVERSE mode to modifiable fields.
		*/
		if (!FAC_PROTECTED(the_fac))
		{
			v_mode |= VMODE_REVERSE;
		}
	}
	
	return(v_mode);									/* Send back the VIDEO attributes.	*/
}

static int ws_bloff(int do_pseudo)							/* Turn blinking off.			*/
{
	register int row, col;								/* Working variables.			*/

	if (wbackground()) return(SUCCESS);						/* In background so don't do.		*/

	VL_vstate(VSTATE_SAVE);								/* Save state.				*/
	for (row = 0; row < WS_MAX_LINES_PER_SCREEN; row++)				/* Yes, so loop screen.			*/
	{
		for (col = 0; col < wcurwidth; col++)					/* Loop each character.			*/
		{
			if ( FAC_BLINK(*attr_map_ptr(row,col)) && 
			    !FAC_PROTECTED(*attr_map_ptr(row,col)))			/* Error in this field?			*/
			{
				*attr_map_ptr(row,col) = FAC_SET_BRIGHT(*attr_map_ptr(row,col));/* Yes, remove attrib.		*/

				if (!vwang_ws_mod(row,col))				/* Modifyable field?			*/
				{
					wsmode(ws_trans_atr(*attr_map_ptr(row,col)));	/* Select rendition.			*/
				}
				else if (do_pseudo)
				{
					wsmode(psb_rndt | ws_trans_atr(*attr_map_ptr(row,col)));/* Field rendition.		*/
				}
				else
				{
					wsmode(ws_trans_atr(*attr_map_ptr(row,col)));	/* Field rendition.		*/
				}
				wsmove(row,col);					/* Move to location.		*/
				VL_vputc(*vchr_map_ptr(row,col));				/* Reoutput the char.			*/
			}
		}
	}
	VL_vstate(VSTATE_RESTORE);								/* Restore state.			*/
	return(SUCCESS);
}

static void ws_echo(int the_char,int cursor_row,int cursor_col,int alt_read,unsigned char *no_mod,int do_pseudo)
											/* Echo input as appropriate.		*/
{
	int mode, index, tmpval=0, eofield;
	unsigned char *rowbuf;
	
	rowbuf = map_multibyte(data_map_ptr(cursor_row,0),NULL);			/* map out multibyte chars on the line  */

	if (do_pseudo)	mode = psb_rndt | ws_trans_atr(*attr_map_ptr(cursor_row,cursor_col));
	else		mode = ws_trans_atr(*attr_map_ptr(cursor_row,cursor_col));
	wsmode(mode);									/* Select the appropriate rendition.	*/

	wscset(VCS_DEFAULT);						/* Echoing is always done in the default character set. */

	ws_tag_alt(alt_read,cursor_row,cursor_col,no_mod);				/* Set flags.				*/

	if (multi_input_pos==1)								/* is this echo part of a multibyte?	*/
	{
											/* if so, need to clear 3 or more spaces*/
		for (index=cursor_col, eofield=vwang_ws_eof(cursor_row, cursor_col);
		     (index < cursor_col+IVS_outctx->srcsize) && (index < eofield) && !tmpval ; ++index)
		{
			VL_vprint(" ");
			*data_map_ptr(cursor_row,index) = ' ';
			tmpval = rowbuf[index];
		} 
		if (tmpval)								/* clearing spaces stepped on another	*/
		{									/* multi, which must now also be cleared*/
			while (tmpval == rowbuf[index])
			{
				VL_vprint(" ");
				*data_map_ptr(cursor_row,index) = ' ';
				++index;
			}
		}
		wsmove(cursor_row,cursor_col);
	}
	else if ((tmpval = rowbuf[index = cursor_col]))
	{
		while (tmpval == rowbuf[index])
		{
			VL_vprint(" ");
			*data_map_ptr(cursor_row,index) = ' ';
			++index;
		}
		wsmove(cursor_row,cursor_col);
	}

	if (FAC_BLANK(*attr_map_ptr(cursor_row,cursor_col)))				/* Should character be echoed?		*/
	{
		if (do_pseudo)
		{
			wscset(psb_chset);						/* No, then echo a pseudo blank.	*/
			VL_vprint("%c",psb_char);
			wscset(VCS_DEFAULT);
		}
		else
		{
			VL_vprint(" ");
		}
	}
	else 
	{
		vwputc( the_char );							/* Yes, then echo the character.	*/
	}
	
	*data_map_ptr(cursor_row,cursor_col) = the_char;
}
static int load_sub_chars(void)								/* Default load of the subst. table.	*/
{											/* Index is the Wang input screen value.*/
	register int i, ndx;

	if (sub_table_loaded)
	{
		return SUCCESS;
	}
	
	sub_table_loaded = TRUE;							/* Set the static variable.		*/
											/* Load default substitution for	*/
	ndx = 1;									/* non-printable input values.		*/
	stable[ndx].sub_value	= DEC_GRAPHIC_DIAMOND;					/* 0x01 - Wang small diamond.		*/
	stable[ndx++].char_set	= VCS_GRAPHICS;
	stable[ndx].sub_value	= DEC_DBL_RIGHT_ARROW;					/* 0x02 - Wang right arrow.		*/
	stable[ndx++].char_set	= VCS_ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_DBL_LEFT_ARROW;					/* 0x03 - Wang left arrow.		*/
	stable[ndx++].char_set	= VCS_ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_PLUS_MINUS;					/* 0x04 - Plus/Minus sign.		*/
	stable[ndx++].char_set	= VCS_GRAPHICS;
	stable[ndx].sub_value	= DEC_MENU_PICK;					/* 0x05 - Wang menu pick.		*/
	stable[ndx++].char_set	= VCS_DEFAULT;
	stable[ndx].sub_value	= DEC_GRAPHIC_VERT;					/* 0x06 - Wang vertical bar.		*/
	stable[ndx++].char_set	= VCS_GRAPHICS;
	stable[ndx].sub_value	= DEC_GRAPHIC_HORIZ;					/* 0x07 - Wang bold line.		*/
	stable[ndx++].char_set	= VCS_GRAPHICS;
	stable[ndx].sub_value	= DEC_GRAPHIC_BOX;					/* 0x08 - Wang dots.			*/
	stable[ndx++].char_set	= VCS_GRAPHICS;
	stable[ndx].sub_value	= DEC_CENTERED_PERIOD;					/* 0x09 - Centered period.		*/
	stable[ndx++].char_set	= VCS_GRAPHICS;
	stable[ndx].sub_value	= DEC_VERTICAL_BAR;					/* 0x0A - Not equal sign.		*/
	stable[ndx++].char_set	= VCS_GRAPHICS;
	stable[ndx].sub_value	= psb_char;						/* 0x0B - Wang solid box.		*/
	stable[ndx++].char_set	= psb_chset;
	stable[ndx].sub_value	= DEC_GRAPHIC_PI;					/* 0x0C - Pi symbol.			*/
	stable[ndx++].char_set	= VCS_GRAPHICS;
	stable[ndx].sub_value	= DEC_EXCLAMATION;					/* 0x0D - Wang up_down arrow.		*/
	stable[ndx++].char_set	= VCS_ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_FOUR_CORNER;					/* 0x0E - Intersect two lines, corners. */
	stable[ndx++].char_set	= VCS_GRAPHICS;
	stable[ndx].sub_value	= DEC_PARAGRAPH;					/* 0x0F - Wang paragraph.		*/
	stable[ndx++].char_set	= VCS_ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_SUPSCPT_2;					/* 0x10 - Wang solid diamond.		*/
	stable[ndx++].char_set	= VCS_ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_SUPSCPT_3;					/* 0x11 - Wang open diamond.		*/
	stable[ndx++].char_set	= VCS_ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_BOTM_RIGHT_BOX;					/* 0x12 - Bottom right corner for box.	*/
	stable[ndx++].char_set	= VCS_GRAPHICS;
	stable[ndx].sub_value	= DEC_TOP_RIGHT_BOX;					/* 0x13 - Top right corner for box.	*/
	stable[ndx++].char_set	= VCS_GRAPHICS;
	stable[ndx].sub_value	= DEC_TOP_LEFT_BOX;					/* 0x14 - Top left corner for box.	*/
	stable[ndx++].char_set	= VCS_GRAPHICS;
	stable[ndx].sub_value	= DEC_BOTM_LEFT_BOX;					/* 0x15 - Bottom left corner for box.	*/
	stable[ndx++].char_set	= VCS_GRAPHICS;
	stable[ndx].sub_value	= DEC_GRAPHIC_UK_POUND;					/* 0x16 - Wang wishbone.		*/
	stable[ndx++].char_set	= VCS_GRAPHICS;
	stable[ndx].sub_value	= DEC_QUESTION_MARK;					/* 0x17 - Wang anchor.			*/
	stable[ndx++].char_set	= VCS_ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_VERT_RHORIZ;					/* 0x18 - Vertical with right horizontal.*/
	stable[ndx++].char_set	= VCS_GRAPHICS;
	stable[ndx].sub_value	= DEC_VERT_LHORIZ;					/* 0x19 - Vertical with left horizontal.*/
	stable[ndx++].char_set	= VCS_GRAPHICS;
	stable[ndx].sub_value	= DEC_GRAPHIC_SQUARE;					/* 0x1A - Wang open box.		*/
	stable[ndx++].char_set	= VCS_ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_HORIZ_TVERT;					/* 0x1B - Horizontal with top vertical. */
	stable[ndx++].char_set	= VCS_GRAPHICS;
	stable[ndx].sub_value	= DEC_HORIZ_BVERT;					/* 0x1C - Horizontal with bottom vert.	*/
	stable[ndx++].char_set	= VCS_GRAPHICS;
	stable[ndx].sub_value	= DEC_CENTS_SIGN;					/* 0x1D - Graphic cents sign.		*/
	stable[ndx++].char_set	= VCS_ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_ONE_FOURTH;					/* 0x1E - Graphic 1/4 symbol.		*/
	stable[ndx++].char_set	= VCS_ROM_GRAPHICS;
	stable[ndx].sub_value	= DEC_ONE_HALF;						/* 0x1F - Graphic 1/2 symbol.		*/
	stable[ndx++].char_set	= VCS_ROM_GRAPHICS;
	for (i = 32; i < 128; i++)							/* Initialize the table for printable	*/
	{										/*  characters.				*/
		stable[i].sub_value = i;
		stable[i].char_set = VCS_DEFAULT;
	}

	return(SUCCESS);
}

static void load_user_sub_chars(unsigned char *tblptr)					/* Do the load of substitution values.	*/
{
	unsigned char *lpt;								/* Local copy of ptr to sub table.	*/
	int ndx;

	load_sub_chars();								/* Do the load of substitution values.	*/

	lpt = tblptr;									/* Set the pointer.			*/
	while (*lpt)
	{
		ndx = (int) *lpt++;							/* Get ptr of index into sub table.	*/
		stable[ndx].sub_value = *lpt++;						/* Put value into table.		*/
		stable[ndx].char_set = (int) *lpt++;					/* Put value into table.		*/
	}
}

static int sub_char(unsigned char c,unsigned char *xc,int *char_set)			/* Get the substitution character and	*/
											/*  character set from table.		*/
{
	if (!sub_table_loaded)								/* Has the sub. table been loaded ?	*/
	{
		load_sub_chars();							/* Nope. Load it.			*/
	}
	if (c>128)
	{
		*xc = c;
		*char_set= VCS_DEFAULT;
		return 0;
	}
	*xc = stable[c].sub_value;							/* Get exchange character.		*/
	*char_set = stable[c].char_set;							/* Get needed character set (font).	*/
	return 0;
}

static void wsdmp_scrn(unsigned char *wsb, int type_d)
{
	char	*ptr;
	int i,j;
	char tstr[WS_MAX_COLUMNS_ON_A_LINE+4];

	wsb += 4;

	tstr[wcurwidth] = '\n';
	tstr[wcurwidth+1] = '\0';

	if (type_d == 0)								/* This is a bad condition.	*/
	{
		werrlog(WERRCODE(67022),terminal_error,0,0,0,0,0,0,0);			/* Something wrong with output	*/
		ptr = "%%VW-W-DUMPSCREEN Abnormal condition, dump of current screen is:\n\n";
		printf(ptr);
		if (WL_get_wispdebug() != WISPDEBUG_NONE) WL_werr_write(ptr);		/* write to error log		*/
	}
	else if (type_d == 1) VL_vset(VSET_PRINTER,ON);					/* On printer for output.	*/
	for (i=0; i<WS_MAX_LINES_PER_SCREEN; i++)
	{
		for (j = 0; j < wcurwidth; j++)
		{
			tstr[j] = *wsb++;
			if (tstr[j] & (char)0x80) tstr[j] = ' ';
		}
		printf("%s",tstr);
		if (WL_get_wispdebug() != WISPDEBUG_NONE) WL_werr_write(tstr);		/* write to error log		*/
	}
	if (type_d == 1) VL_vset(VSET_PRINTER,OFF);					/* Turn the printer off.	*/
	else wexit(WERRCODE(67000+22));							/* else unconditional exit.	*/
}

void vwang_ws_erap(int amount)								/* Erase & protect screen sect. */
{
	register int i,j;								/* Working registers.		*/
	enum e_vop last_op;

	if (wbackground()) return;
	
	last_op = VL_voptimize(VOP_DATA_AND_CONTROLS);					/* Turn optimization down.	*/

	if (amount == FULL_SCREEN)							/* Erasing the full screen?	*/
	{
		memset(data_map_ptr(0,0), ' ', SIZEOF_SCREEN_MAP);			/* Clear the data array.	*/
		memset(attr_map_ptr(0,0), DEFAULT_ATTR, SIZEOF_SCREEN_MAP);		/* Initialize the attr array.	*/
	}

	else if (amount == TO_EOS)							/* Erasing to end of screen?	*/
	{
		for (j = VL_vcur_col; j < wcurwidth; j++)					/* Loop through the columns.	*/
		{
			*data_map_ptr(VL_vcur_lin,j) = ' ';				/* Space out the item.		*/
			*attr_map_ptr(VL_vcur_lin,j) = DEFAULT_ATTR;			/* Protect the item.		*/
		}
		for (i = VL_vcur_lin+1; i < WS_MAX_LINES_PER_SCREEN; i++)			/* Do remaining lines.		*/
		{
			memset(data_map_ptr(i,0),' ',wcurwidth);
			memset(attr_map_ptr(i,0),DEFAULT_ATTR,wcurwidth);
		}
	}
	VL_verase(amount);								/* Erase as much as specified.	*/
	VL_voptimize(last_op);								/* Restore optimization.	*/
}



int vwang_help(int curset)								/* Simulate Wang help function.		*/
{
	int old_menu_exit_key;								/* Orig. menu exit key.			*/
	enum e_vop optsave;

	vwang_init_video();
	
	optsave=VL_voptlevel();								/* need to save this.			*/

	old_menu_exit_key = WL_menu_get_key(0);						/* What is the orig. menu exit key ?	*/
	WL_menu_set_key(0, fn1_key);							/* Give it a new value.			*/

	if (WL_ishelpactive())								/* Are we already in help.		*/
	{
		vwang_bad_char();								/* Ring the bells.			*/
		return(FAILURE);							/* Return to the caller.		*/
	}
	else WL_sethelpactive(TRUE);							/* Now help is active.			*/

	vwang_wpushscr();								/* Save all neccessary variables.	*/

	if (wcurwidth != 80)  WS80();							/* Put in 80 col mode for help screens. */
	VL_vdefer_restore();								/* Cannot be in deferred mode.		*/

	WL_wsh_help(1);									/* Output the selection menu.		*/
	if (curset)
		VL_vset_cursor_on();
	else
		VL_vset_cursor_off();
	cursor_on_flag = curset;							/* Set the flag.			*/

	VL_vdefer_restore();								/* Restore from optimization.		*/
	vwang_wpopscr();								/* Restore all saved variables.		*/

	VL_vdefer_restore();								/* Optimization terminates before exit. */

	WL_sethelpactive(FALSE);							/* Now help is inactive.		*/
	vwang_set_synch(FALSE);								/* A synch is not required.		*/

	WL_menu_set_key(0, old_menu_exit_key);						/* Reset the menu exit key.		*/
	
	VL_voptimize(optsave);								/* vwang expects a certain opt level	*/
											/* that's not correctly set here	*/
	return(SUCCESS);								/* All done.				*/
}

int vwang_wpushscr(void)								/* A function to save the screen addrs	*/
{											/* and variables of a 'Wisp' screen.	*/
	typedef struct wscrn_struct wscrn_struct;
	wscrn_struct *wscrn_struct_ptr;							/* Point to the the save area.		*/
	int x;										/* A local, working variable.		*/

	if (wbackground()) return(0);							/* Don't do if in background.		*/
	if (rts_first)	vwang_init_screen();						/* If first push.			*/
	x = sizeof(struct wscrn_struct);
	wscrn_struct_ptr = (wscrn_struct *) wisp_malloc(x);				/* Alloc storage for the info to save.	*/

	x = SIZEOF_SCREEN_MAP;								/* Size for the data to store.		*/
	wscrn_struct_ptr->saved_ws_atr = wisp_malloc(x);				/* Alloc. storage and save the addr.	*/
	memcpy(wscrn_struct_ptr->saved_ws_atr, attr_map_ptr(0,0), x);			/* Copy contents to the storage area.	*/

	wscrn_struct_ptr->saved_ws_at2 = wisp_malloc(x);				/* Alloc. storage and save the addr.	*/
	memcpy(wscrn_struct_ptr->saved_ws_at2, data_map_ptr(0,0), x);			/* Copy contents to the storage area.	*/

	wscrn_struct_ptr->saved_fac_table = wisp_malloc(sizeof(fac_table));		/* Alloc. storage and save the addr.	*/
	memcpy(wscrn_struct_ptr->saved_fac_table, fac_table, sizeof(fac_table));	/* Copy contents to the storage area.	*/

	wscrn_struct_ptr->saved_toupper_table = wisp_malloc(sizeof(user_toupper_table));/* Alloc. storage and save the addr.	*/
	memcpy(wscrn_struct_ptr->saved_toupper_table, user_toupper_table, sizeof(user_toupper_table));

	wscrn_struct_ptr->saved_numeric_table = wisp_malloc(sizeof(user_numeric_table));
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


	VL_vpushscr();									/* Save the corresponding video structs.*/

	set_stop_time((time_t)0);
	vwang_set_aid(AID_LOCKED);
	return(SUCCESS);
}

int vwang_wpopscr(void)									/* A function to restore the screen and */
{											/* associated variables.		*/
	struct wscrn_struct *wscrn_struct_ptr;						/* Local pointer to the save area.	*/
	int x;										/* A working variable.			*/

	if (wbackground()) return(0);							/* Don't do if in background.		*/
	wscrn_struct_ptr = wscrn_stack;							/* Point to the most recently saved	*/
											/* structure of pointers and variables. */
	if (!wscrn_struct_ptr)								/* Are we pointing to never never land? */
	{
		werrlog(WERRCODE(67014),0,0,0,0,0,0,0,0);					/* Stack empty				*/
		return(0);								/* Outta here.				*/
	}

	x = SIZEOF_SCREEN_MAP;								/* What's the size of this item ?	*/
	memcpy(attr_map_ptr(0,0), wscrn_struct_ptr->saved_ws_atr, x);			/* Copy the saved bytes.		*/
	free(wscrn_struct_ptr->saved_ws_atr);						/* Free up this area.			*/
	memcpy(data_map_ptr(0,0), wscrn_struct_ptr->saved_ws_at2, x);
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
	vwang_set_aid(wscrn_struct_ptr->saved_aid);
	fast_read = wscrn_struct_ptr->saved_fast_read;
	wcurwidth = wscrn_struct_ptr->saved_screen_width;
	set_stop_time(wscrn_struct_ptr->saved_stop_time);
	max_data_range = wscrn_struct_ptr->saved_max_data_range;
	wscrn_stack = wscrn_struct_ptr->prev_wscrn_struct_ptr;				/* Get address of the new top screen.	*/
	the_edit=wscrn_struct_ptr->saved_edit;

	free(wscrn_struct_ptr);								/* Free up the allocated area.		*/

	if (VL_vscr_wid != wcurwidth)							/* If need to reset screen width.	*/
	{
		if (wcurwidth == 132) WS132();						/* Put back to what it was before.	*/
		else WS80();
	}
	VL_vpopscr();									/* Get the video structs.		*/
	vwang_set_synch(FALSE);								/* Synch is not required now.		*/

	if (use_costar())
	{
		if (costar_after_write_api)
		{
			costar_ctrl_api(costar_after_write_api);
		}
	}

	return(SUCCESS);
}

int vwang_terminal_control_char(unsigned char the_char)
{
#ifdef WIN32
	switch(the_char)
	{
	case 0:		/* NULL */
	case 7:		/* BELL	*/
	case 8:		/* Backspace */
	case 9:		/* TAB */
	case 10:	/* Linefeed */
	case 11:	/* Vtab */
	case 12:	/* Formfeed */
	case 13:	/* Caridge return */
	case 14:	/* S0 */
	case 15:	/* S1 */
	case 27:	/* Escape */
		return 1;
	}
#endif

#ifdef unix
	if (the_char < ' ')
	{
		return 1;
	}
#endif
	return 0;
}

static void ws_putbuff(unsigned char the_char, int row, int col, int rendition, int font)/* Put char. in line buf. accumulator.	*/
{
	unsigned char terminal_char;
	
	if (PSEUDO_BLANK_CHAR == the_char)
	{
		font = psb_chset;
		terminal_char = psb_char;
	}
	else
	{
		/*
		**	Do tranlation with substitution table.
		*/
		sub_char(the_char, &terminal_char, &font);

		/*
		**	Do the CHARMAP translation
		*/
		terminal_char = WANG2TERM(terminal_char);
	}

	/*
	**	Blank out control characters. 
	*/
	if (vwang_terminal_control_char(terminal_char))
	{
		terminal_char = ' ';
		font = VCS_DEFAULT;
	}

	if ((row != VL_vcur_lin) || (col != VL_vcur_col+lbcount) || (rendition != VL_vcur_atr) || (font != VL_vchr_set))
	{
		ws_dumpbuff(row,FALSE);							/* Dump what is currently in buffer.	*/
		wsmove(row,col);							/* Now move to where we want to be.	*/
		wsmode(rendition);							/* Select the desired new rendition.	*/
		wscset(font);								/* Select the desired new font.		*/
	}

	lbuf[lbcount++] = terminal_char;						/* Store the next character.		*/
	lbattr = font|rendition;							/* Remember the attributes.		*/

	if ((the_char == ' ') && !(rendition & VMODE_REVERSE) && !(rendition & VMODE_UNDERSCORE))
		lbdangle++;								/* Count invisible dangling spaces.	*/
	else
		lbdangle = 0;								/* Anything else terminates the dangle. */
}

static void ws_dumpbuff(int row,int eol)						/* Dump the current buffer.		*/
{
	if (lbcount)									/* Anything in output accumulator.	*/
	{
		if (eol && (lbdangle > 4)) 
		{
			lbcount = lbcount - lbdangle;					/* Any dangling spaces?			*/
		}
		
		if (lbcount)								/* Anything left?			*/
		{
			lbuf[lbcount] = '\000';						/* Yes, deposit a null.			*/
			vrawprint(lbuf);						/* Print the string.			*/
			memcpy(vchr_map_ptr(row,VL_vcur_col),lbuf,lbcount);		/* Store data in video's maps.		*/
			memset(vatr_map_ptr(row,VL_vcur_col),lbattr,lbcount);		/* Store the attributes.		*/
			VL_vcur_col = VL_vcur_col + lbcount;					/* Move the tracking counter.		*/
			if (VL_vcur_col >= wcurwidth)
			{
				VL_vcur_col = wcurwidth - 1;				/* At the edge?				*/
			}
			lbcount = 0;							/* Reset the counter.			*/
		}

		if (eol && (lbdangle > 4)) 
		{
			VL_verase(TO_EOL);						/* Erase to the end of the line.	*/
		}
		
		lbdangle = 0;								/* Reset the dangle counter.		*/
	}
}

static int check_mp(int row,int col,int rend,int do_pseudo)				/* See if it is a menu pick item.	*/
{
	if ( menu_pick_combo_fac(row,col-1) )						/* Is it a menu pick item?	*/
	{
		if (cursor_on_flag)							/* If cursor is on then turn off while	*/
		{									/*  rewriting the line.			*/
			VL_vset_cursor_off();
		}
		rewrite_mp(row,col,rend,do_pseudo);					/* Re-write line with bold attribute.	*/
		wsmove(row,col);							/* Put cursor back to where it was.	*/
		if (cursor_on_flag)							/* If cursor was on then turn it back	*/
		{									/* on.					*/
			VL_vset_cursor_on();
		}
		return(TRUE);								/* Return TRUE, was a menu pick.	*/
	}
	else
	{
		wsmove(row,col);							/* Move to new position.		*/
		return(FALSE);								/* Return FALSE, was not a menu pick.	*/
	}
}

/*
**	ROUTINE:	rewrite_mp()
**
**	FUNCTION:	Rewrite the "menu pick" to either CLEAR it or BOLD it.
**
**	DESCRIPTION:	{Full detailed description}...
**
**	ARGUMENTS:	
**	row		Row position
**	col		Col position
**	rend		The new rendition: either 0=VMODE_CLEAR or 1=VMODE_BOLD
**	do_pseudo	Flag to change spaces into pseudo blanks in modifiable fields.
**
**	GLOBALS:	?
**
**	RETURN:		?
**
**	WARNINGS:	?
**
*/
static void rewrite_mp(int line, int col, int rend, int do_pseudo)			/* Rewrite line or until next FAC.	*/
{
	unsigned char c;								/* Working character.			*/
	unsigned char *dm;
	fac_t *am, *fc, last_atr;
	int last_mode;
	int col_idx, mode;
	int	use_costar_flag = 0;
	int	use_w4w_flag = 0;

	use_w4w_flag = use_w4w();
	use_costar_flag = use_costar();

	last_atr = DEFAULT_ATTR;							/* Reset attributes at start of line.	*/
	dm = data_map_ptr(line,col);							/* Point to the current line.		*/
	am = attr_map_ptr(line,col);
	fc = attr_map_ptr(line,col-1);							/* Get the FAC character.		*/
	last_mode = ws_trans_atr(*fc);							/* Set the last mode.			*/

	VL_vbuffering_start();								/* Turn on buffering if not on.		*/

	for (col_idx = col; col_idx < wcurwidth; col_idx++)				/* Loop through each column.		*/
	{
		c = *dm;								/* Get the character from the map.	*/
		last_atr = *am;								/* Get the character attributes.	*/

		if (FAC_FAC(last_atr))							/* Is this character a FAC?		*/
		{
			col_idx = wcurwidth;						/* Get out of loop.			*/
			break;
		}
		else
		{
			/*
			**	This is a condensed subset of ws_write() and maybe should be combined
			*/
			if (FAC_BLANK(last_atr) || c == 0)				/* Should this field be invisible?	*/
			{
				ws_putbuff(' ',line,col_idx,VMODE_CLEAR,VCS_DEFAULT);	/* Put in output buffer.		*/
			}
			else
			{
				mode = last_mode|rend;

				if (!FAC_PROTECTED(last_atr))				/* Is this a modifiable field ?		*/
				{
					if (do_pseudo || PSEUDO_BLANK_CHAR == c)
					{
						mode |= psb_rndt;
					}

					ws_putbuff(c,line,col_idx,mode,VCS_DEFAULT);	/* Put char.				*/
				}
				else
				{
					if (col_idx == col && rend == VMODE_BOLD)	/* Is current item so put defined symbol*/
					{
						c = CURRENT_MENU_PICK;			/* Defined in vwang.h			*/
					}

					if (col_idx == col && use_w4w_flag)
					{
						mode = w4w_tabstop_vmode;
					}
					else if (PSEUDO_BLANK_CHAR == c)
					{
						mode |= psb_rndt;
					}

					ws_putbuff(c,line,col_idx,mode,VCS_DEFAULT);
				}
			}
			dm++;								/* Incrememt the buffer pointers.	*/
			am++;
		}
	}
	ws_dumpbuff(line,FALSE);							/* Dump the accumulated buffer.		*/
	VL_vbuffering_end();								/* Dump the buffer to show the screen.	*/
	wsmode(VMODE_CLEAR);								/* Clear out the renditions.		*/
	wscset(VCS_DEFAULT);
}

static unsigned char *data_map_ptr(int row, int col)
{
	static unsigned char data_map[SIZEOF_SCREEN_MAP];				/* What Wang thinks is on the screen.	*/

	return &data_map[ (row * wcurwidth) + col ];
}

/*
**	ROUTINE:	attr_map_ptr()
**
**	FUNCTION:	Return a ptr into the attribute map for a given row and col location.
**
**	DESCRIPTION:	The attribute map contains the attributes for each position on the screen.
**			The values are the Wang FAC values.
**			The position of FAC's contain the FAC value.
**			All other positions contain the corresponding FAC bits with the high x80 bit
**			stripped off.
**
**			In other words:
**			- The attribute of a FAC is the FAC value. (x80 - xFF)
**			- The attribute of a non-FAC is previous FAC with the x80 bit stripped (x00-x7F)
**
**	ARGUMENTS:	
**	row		The row offset (0-23)
**	col		The colume offset (0-79) or (0-131)
**
**	GLOBALS:	
**	wcurwidth	The current width of the screen 80 or 132.
**
**	RETURN:		Pointer into the attribute map for the given row and col.
**
**	WARNINGS:	No checking is done.
**
*/
static fac_t *attr_map_ptr(int row, int col)
{
	static fac_t attr_map[SIZEOF_SCREEN_MAP];					/* Wang attributes for each char pos.	*/

	return &attr_map[ (row * wcurwidth) + col ];
}

static unsigned char *vchr_map_ptr(int row, int col)
{
	return &VL_vchr_map[VL_vml(row)][col];
}

static unsigned char *vatr_map_ptr(int row, int col)
{
	return &VL_vatr_map[VL_vml(row)][col];
}

int WS80(void)										/* Select 80 column screen.		*/
{
	if (rts_first)	vwang_init_screen();						/* If first time setting.		*/
	wsetwid(VSCREEN_NARROW);							/* Set the screen width.		*/
	return(SUCCESS);
}

int WS132(void)										/* Select a 132 column screen.		*/
{
	if (rts_first)	vwang_init_screen();						/* If first time setting.		*/
	wsetwid(VSCREEN_WIDE);								/* Select the screen width.		*/
	return(SUCCESS);
}

static int wsetwid(int wd)
{
	enum e_vop op_save;								/* Save current level of optimization.	*/

	if (wbackground()) return FAILURE;

	op_save = VL_voptimize(VOP_DATA_AND_CONTROLS);					/* Turn optimization down.		*/
	VL_vscreen(wd);									/* Select the screen width.		*/
	if (wd == VSCREEN_WIDE) 
		wcurwidth = 132;							/* Remember the current width.		*/
	else	
		wcurwidth = 80;
	vwang_set_synch(TRUE);								/* Now force a synchronize.		*/
	VL_vdefer_restore();								/* Restore from any optimization.	*/
	VL_voptimize(op_save);								/* Restore optimization level.		*/

	return(SUCCESS);								/* Return to the caller.		*/
}

char vwang_charat(int r, int c)								/* Get a character from a row and col.	*/
{
	return(*data_map_ptr(r,c));							/* Get this character from the map.	*/
	
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

static int adjust_for_multibyte(int row, int col, int dvert, int dhoriz)
{
	int l;
	int ret;
	unsigned char *rowbuf;
	
	rowbuf = map_multibyte(vchr_map_ptr(row,0),NULL);
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
static unsigned char *map_multibyte(unsigned char *rowdata,unsigned char *caller_rowbuf)
{
	static unsigned char my_rowbuf[WS_MAX_COLUMNS_ON_A_LINE];	
	unsigned char *rowbuf, *rptr;
	int rowind, mbyteind;
	int mcounter;  /* an int to count multibyte chars on a line.  this value is inserted 
			  at the location of each respective multibyte */

	if (caller_rowbuf)
	{
		rowbuf=caller_rowbuf;
	}
	else
	{
		rowbuf=my_rowbuf;
		memset(my_rowbuf,0,sizeof(my_rowbuf));
	}
	if (IVS_outctx==NULL)
	{
		return rowbuf;
	}
	rptr = rowdata;
	mcounter=1;
	for (rowind=0; rowind<wcurwidth; ++rowind)
	{
		if ((rowind > (VL_vcur_col - multi_input_pos)) && 
		    (rowind < VL_vcur_col))
		{
			++rowind;
			continue;
		}
		for (mbyteind=0; mbyteind < (IVS_outctx->srcsize); ++mbyteind)
		{
			if ((IVS_outctx->indbuf)[rptr[rowind+mbyteind]+(mbyteind<<8)]==0xff)
			{
				break;
			}
		}
		if (mbyteind == (IVS_outctx->srcsize))
		{
			if (!caller_rowbuf)
			{
				for (mbyteind=0; mbyteind < (IVS_outctx->srcsize); ++mbyteind)
				{
					rowbuf[rowind+mbyteind] = mcounter;
				}
				++mcounter;
			}
			else
			{
				for (mbyteind=0; mbyteind < (IVS_outctx->srcsize); ++mbyteind)
				{
					rowbuf[rowind+mbyteind] = 1;
				}
			}
			rowind += (IVS_outctx->srcsize)-1;
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
**			by JEC.
**	12/01/92	Written by JEC
**
*/
int SETFACS(unsigned char *new_table,unsigned char *toupper_table,unsigned char *numeric_table)
{											/* Prerequisite routine to change the	*/
											/*   the meaning and mapping of facs.	*/
	register int i;									/* Working variable.			*/

	for (i = 0; i < 128; i++)							/* Check each character.		*/
	{
		if ( new_table[i] == CHAR_NULL)
		{
			user_fac_table[i] = new_table[i];				/* Nulls mean it is not a fac.		*/
		}
		else if ( new_table[i] >= 0x80)						/* Is it in the valid fac range?	*/
		{
			user_fac_table[i] = new_table[i];				/* Possible fac value change.		*/
		}
		else
		{	/* Report the error.	*/
			char	buff[128];
			sprintf(buff,"SETFACS-F-Invalid value %d for FAC, table item %d.",new_table[i],i);
			WL_werr_message_box(buff);
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
void SET8BIT(unsigned char *faclist,unsigned char *lowuplist,unsigned char *numlist)
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
**	Routine:	vwang_valid_char_data()
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
int vwang_valid_char_data(char value)
{
	if (value >= ' ' && value < max_data_range)
		return TRUE;
	else
		return FALSE;
}
/*
**	Routine:	vwang_fac()
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
fac_t vwang_fac(fac_t pseudo_FAC)
{
	fac_t	the_fac;

	if ((pseudo_FAC)>=128)
	{
		the_fac = fac_table[(pseudo_FAC-128)];
	}
	else
	{
		the_fac =  0;
	}

	return the_fac;
}
/*
**	Routine:	vwang_unfac()
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
fac_t vwang_unfac(fac_t true_FAC)
{
	fac_t idx;
	
	for (idx=0; idx<128; ++idx)
	{
		if ((fac_t)(true_FAC & 0xff) == fac_table[idx])
		{
			return idx+128;
		}
	}
	return 0xff;
}

/*
**	Routine:	vwang_fac_pre_filter()
**
**	Function:	To convert a pseudo FAC into a true FAC
**
**	Description:	This routine examines the user_fac_table to convert
**			a pseudo to a true FAC.	 This routine is called by
**			WSCREEN() to convert its FAC values.  WSCREEN cannot
**			call fac() above because fac uses the actual FAC table
**			which is not valid until a vwang call.	This routine
**			converts the FACs based on the data passed in to this
**			module using SET8BIT or SETFACS, and it can be called
**			prior to the actual vwang().  This also applies to the routine
**			vwang_unfac_pre_filter().
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
fac_t vwang_fac_pre_filter(fac_t pseudo_FAC)
{
	if ((pseudo_FAC)<128 || !use_user_fac_table)
	{
		return pseudo_FAC;
	}
	else
	{
		
		return user_fac_table[(pseudo_FAC-128)];
	}
}
/*
**	Routine:	vwang_unfac_pre_filter()
**
**	Function:	To convert a pseudo FAC back into a true FAC
**
**	Description:	This routine examines the user FAC table to convert
**			a pseudo back to a true FAC.. see description for
**			vwang_fac_pre_filter() above.
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
fac_t vwang_unfac_pre_filter(fac_t true_FAC)
{
	register fac_t idx;
	
	if ((true_FAC)<128 || !use_user_fac_table)
	{
		return true_FAC;
	}

	for (idx=0; idx<128; ++idx)
	{
		if ((unsigned char)(true_FAC & 0xff) == user_fac_table[idx])
		{
			return idx+128;
		}
	}
	
	return 0xff;
}


/*
**	Routine:	vwang_aid()
**
**	Function:	To extract the current AID character.
**
**	Description:	This routine returns the current AID character.
**			This AID character is set by the vwang_set_aid() routine.
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

char vwang_aid(void)
{
	return(current_aid);
}

/*
**	Routine:	vwang_aid_read_unlocked()
**
**	Function:	Get the current AID char and handling special case of READ when UNLOCKED
**
**	Description:	Get the current AID char.
**			If UNLOCKED then there is the special case of the user pressing a pfkey
**			which would lock the workstation but this has not been registered because
**			a read has not been done.
**			So... if UNLOCKED check if a key has been press and if so issue a one second
**			read of the workstation to allow the AID char to be updated then return
**			the AID char.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		The current AID char.
**
**	Warnings:	None
**
*/
char vwang_aid_read_unlocked(void)
{
	if (vwang_aid() == AID_UNLOCKED)						/* If keyboard is unlocked then		*/
	{
		unsigned char vw_function[1];						/* vwang parameters.			*/
		unsigned char vw_wsb[WSB_LENGTH];
		unsigned char vw_lines[1];

		char	c;

		/*
		**	To compensate for not modelling a client/server we check to see if
		**	there are keystrokes waiting to be read and if so we do a 1 second Wait.
		*/

		if ((c = (char)VL_vcheck()))							/* If available get next char		*/
		{
			VL_vpushc(c);							/* We got a char so push it back	*/

			vw_function[0] = READ_MODIFIABLE;
			memset(vw_wsb,' ',WSB_LENGTH);
			vw_wsb[0] = (unsigned char)1;
			vw_lines[0]  = (unsigned char)WSB_ROWS;

			vwang_timeout((int4)1);						/* Set the timer for 1 second.		*/
			vwang(vw_function,vw_wsb,vw_lines,"A",NULL,NULL); 		/* Go do the I/O action.		*/
			vwang_timeout((int4)0);						/* Cancel timer				*/
		}
	}

	return(vwang_aid());
}

/*
**	Routine:	vwang_set_aid()
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
void vwang_set_aid(char aid)
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
void vwang_timeout(int4 seconds)
{
	if (seconds < 1)
	{
		set_stop_time((time_t)0);
	}
	else
	{
		set_stop_time( (time_t)(time(NULL) + seconds + 1));
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
static int timed_read(long int *seconds_remaining)
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
static time_t stop_time(void)
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
static void set_stop_time(time_t the_time)
{
	vwang_stop_time = the_time;
}

/*
**	Routine:	vwang_meta_aid()
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
char vwang_meta_aid(int metachar)
{
	int	pfk;
	char	aid;

	aid = (char)0;								/* Set AID to NULL				*/
	pfk = VL_vfnkey(metachar);							/* Change meta into a pfkey number 1-32		*/
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
static int in_edit(int row, int col)
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
static void edit_init_struct(void)
{
	int idx;
	
	the_edit=(struct EDIT *)wisp_calloc(1,sizeof(struct EDIT));		/* get the struct */
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
static void edit_init_window(void)
{

	VL_vmove(the_edit->top_row-1,the_edit->left_col-1);			/* first draw the box */
	VL_vline(VLINE_VERTICAL,the_edit->bottom_row - the_edit->top_row+3);

	VL_vmove(the_edit->top_row-1,the_edit->left_col-1);
	VL_vline(VLINE_HORIZONTAL,the_edit->right_col-the_edit->left_col+3);

	VL_vmove(the_edit->bottom_row+1,the_edit->left_col-1);
	VL_vline(VLINE_HORIZONTAL,the_edit->right_col-the_edit->left_col+3);

	VL_vmove(the_edit->top_row-1,the_edit->right_col+1);
	VL_vline(VLINE_VERTICAL,the_edit->bottom_row - the_edit->top_row+3);

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
static void edit_erase_box(void)
{
	int row_idx, col_idx;
	
	/* loop and insert 0x01's into the data_map.. on the next time through */
	/* vwang (ws_write) will be fooled into writing spaces where the 0x01's are */

	for (row_idx = the_edit->top_row-1; row_idx <= the_edit->bottom_row; ++row_idx)
	{	  
		*data_map_ptr(row_idx,the_edit->left_col-1) = 0x01;
		*data_map_ptr(row_idx,the_edit->right_col+1) = 0x01;
	}
	for (col_idx = the_edit->left_col-1; col_idx<=the_edit->right_col+1; ++col_idx)
	{
		*data_map_ptr(the_edit->top_row-1,col_idx) = 0x01;
		*data_map_ptr(the_edit->bottom_row+1,col_idx) = 0x01;
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
**	Globals:	vcur_line, VL_vcur_col
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
static void edit_main(
		      int input,
		      int row,
		      int col,
		      int *filling,
		      const char *terminate_list,			
		      char *pfkey,
		      unsigned char *no_mod)
{
	int do_pseudo=FALSE;
	
	edit_compute_pos(row,col);
	
	if (input == return_key)
	{
		edit_unmark();
		edit_redraw();
		vwang_ws_fkey(0,filling,terminate_list,pfkey,no_mod);		/* behave as a normal return */
	}
	else if (VL_vfnkey(input))							/* function keys are also normal */
	{
		vwang_ws_fkey(input,filling,terminate_list,pfkey,no_mod);
	}
	else if (input == up_arrow_key)						/* arrows move normally but may require	 */
	{									/* hilite update */
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;				/* clear the wrapped word flag */

		ws_posit(-1, 0,do_pseudo);
		edit_compute_pos(VL_vcur_lin,VL_vcur_col);				/* so compute new cpos */
		edit_show_hilite();						/* and call the hilite showing routine */
	}
	else if (input == down_arrow_key)	
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;				/* clear the wrapped word flag */
		ws_posit( 1, 0,do_pseudo);
		edit_compute_pos(VL_vcur_lin,VL_vcur_col);
		edit_show_hilite();
	}
	else if (input == left_arrow_key)	
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;				/* clear the wrapped word flag */
		ws_posit( 0,-1,do_pseudo);
		edit_compute_pos(VL_vcur_lin,VL_vcur_col);
		edit_show_hilite();
	}
	else if (input == right_arrow_key)	
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;				/* clear the wrapped word flag */
		ws_posit( 0, 1,do_pseudo);
		edit_compute_pos(VL_vcur_lin,VL_vcur_col);
		edit_show_hilite();
	}
	else if (input == key_mark)						/* mark key pressed, mark the current cpos */
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;				/* clear the wrapped word flag */
		the_edit->mark_row = the_edit->edit_row;
		the_edit->mark_col = the_edit->edit_col;
	}
	else if (IS_PRINTABLE_CHAR(input,max_data_range))			/* its a valid data character */
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
		the_edit->wrapped_mid_word=FALSE;				/* clear the wrapped word flag */
		edit_unmark();							/* unmark if marked */
		edit_putchar('\n');						/* stick a newline in at the current cpos */
		edit_redraw();							/* redraw it */
	}
	else if (input == tab_key)						
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;				/* clear the wrapped word flag */
		edit_tab(row,col);						/* move the cursor to the next tabstop */
		edit_compute_pos(VL_vcur_lin,VL_vcur_col);				/* compute new cpos */
		edit_show_hilite();						/* adjust hilited area if necessary */
	}
	else if (input == backtab_key)						/* backtab key is normal */
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;				/* clear the wrapped word flag */
		ws_last(row,col,do_pseudo);
	}
	else if (input == help_key)						/* help is normal */
	{
		if (WL_ishelpactive())						/* Is help active?	*/
		{
			strcpy((char *)pfkey,(char *)"FE");			/* Return code FE.	*/
			no_mod[1] = AID_HELP;					/* Mode code too.	*/
			*filling = FALSE;					/* Now we're done.	*/
		}
		else 
		{
			if (EDE_using())
			{
				ws_bar_menu(ON,row,col,0,0,no_mod,do_pseudo);
			}
			else
			{
				vwang_help(ON);
			}
		}
	}
	else if (input == clear_before_key)					/* these haven't been well tested */
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;				/* clear the wrapped word flag */
		edit_clear_before(the_edit->edit_row,the_edit->edit_col);
		edit_redraw();
	}
	else if (input == clear_after_key)					/* these haven't been well tested */
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;				/* clear the wrapped word flag */
		edit_clear_after(the_edit->edit_row,the_edit->edit_col);		
		edit_redraw();
	}
	else if (input == home_key)						/* go to upper left of edit window */
	{
		the_edit->wrap_moved_curs = FALSE;
		the_edit->wrapped_mid_word=FALSE;				/* clear the wrapped word flag */
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
	else if (WLNC_use_netroncap() && input == trigger1)			/* what does this do? */
	{
	}
	else vwang_bad_char();							/* Else beep.				*/

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
static void edit_compute_pos(int row, int col)
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
static void edit_putchar(int input)
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
		edit_open_line(currow);				/* create a new empty line below the current one */
		
		dest= &(the_edit->data_buf[currow+1][0]);      /* setup to copy to that new line */
		src = &(the_edit->data_buf[currow][curcol]);   /* starting at the current cursor pos */
		
		if (curcol > cursize)			       /* we are past the end of data */
		{
			cursize = curcol;		       /* increase the size */
		}
		
		copyidx = cursize - curcol;		       /* copyidx is the amount of data we want to copy down to new line */
		
		the_edit->line_size[currow+1] = copyidx;     	/* set the size of the new line  */
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
		
		++the_edit->edit_row;	     			/* cursor moves to beginning of next line */
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
static void edit_wrap(int lineno, int direction, int line_space)
{
	int scanidx,copyidx,space_found,cutidx;
	char *dest,*src;
	char word_to_copy[EDIT_MAX_COLS];
	int newsize;
	int space_needed, free_space;
	
	if (lineno >= the_edit->height-1)
	{
		return;
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
			return;
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
			return;
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
					return;
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
			return;
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
			return;
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
			return;
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
		return;
		
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
static void edit_tab(int row, int col)
{
	int adjcol;  /* the zero-relative column */
	int newpos;  /* computed pos */

	adjcol = col - the_edit->left_col;					/* this is familiar by now */
	for (newpos = adjcol+1; newpos % 5; ++newpos);				/* now loop till mod 5 is zero */

	if (newpos > the_edit->width)						/* no tabbing past the end */
	  vwang_bad_char();
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
static void edit_clear_before(int row, int col)
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
static void edit_clear_after(int row, int col)
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
static void edit_delleft(void)
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
			vwang_bad_char();
			return;
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

static int edit_chk_wrap_back(int row)
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
	return(0);
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
static void edit_delright(void)
{
	int row,col;
	
	row=the_edit->edit_row;
	col=the_edit->edit_col;

	if (col==(the_edit->width-1))					/* at end of data, can't delete */
	{
		vwang_bad_char();
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

static void edit_redraw(void)
{
	register int row_idx, col_idx;
	
	VL_vset_cursor_off();
	wsmode(VMODE_BOLD);
	for (row_idx=col_idx=0; row_idx < the_edit->height; )			/* now loop through the shadow buf byte by byte */
	{									/* output any differences */
		if ((*data_map_ptr(the_edit->top_row+row_idx,			/* this if stmt checks for diffs */
				the_edit->left_col+col_idx) !=
		    the_edit->data_buf[row_idx][col_idx])) 
		{
			wsmove(the_edit->top_row+row_idx,the_edit->left_col+col_idx); /* move to the location */
			vwputc(the_edit->data_buf[row_idx][col_idx]);		/* put the char */
			*data_map_ptr(the_edit->top_row+row_idx,the_edit->left_col+col_idx) =
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
	wsmode(VMODE_CLEAR);
	VL_vset_cursor_on();
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
static void edit_show_hilite(void)
{
	int markrow, markcol;
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

#define ISHILITED(ch) (FAC_MODIFIED(ch) ? 1 : 0)				/* macro is for readability */

	VL_vset_cursor_off();							/* inviso cursor */
	for (row_idx=col_idx=0; row_idx <= the_edit->height; )			/* loop through the map */
	{
#ifdef EDITSHOWHI
		fprintf(foo,"%c%c ",the_edit->hilite_buf[row_idx][col_idx])?'1':'0',
		    ISHILITED(*attr_map_ptr(row_idx+the_edit->top_row,col_idx+the_edit->left_col))?'1':'0')
#endif

		if ((selected = the_edit->hilite_buf[row_idx][col_idx]) !=	/* if screen does not match hilite_buf */
		    ISHILITED(*attr_map_ptr(row_idx+the_edit->top_row,col_idx+the_edit->left_col)))
		{
			if (selected)						/* decide whether to hilite or dim it */
			{
				wsmode(VMODE_REVERSE);				/* set mode, move and put */
				wsmove(the_edit->top_row+row_idx,the_edit->left_col+col_idx);
				vwputc(the_edit->data_buf[row_idx][col_idx]);
				*attr_map_ptr(the_edit->top_row+row_idx,the_edit->left_col+col_idx) = FAC_MODIFIED_ON;
										/* record it in attr map */
			}
			else
			{
				wsmode(VMODE_BOLD);				/* clear it */
				wsmove(the_edit->top_row+row_idx,the_edit->left_col+col_idx);
				vwputc(the_edit->data_buf[row_idx][col_idx]);
				*attr_map_ptr(the_edit->top_row+row_idx,the_edit->left_col+col_idx) = '\f';
										/* this is the normal value for attr map */
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
	VL_vset_cursor_on();							/* cursor back on */
	wsmode(VMODE_BOLD);							/* mode normal */
	wsmove(cursorrow+the_edit->top_row,cursorcol+the_edit->left_col);	/* put the cursor back where it started */
}

static int wsmode(int control)							/* Select character rendition.		*/
{
	return VL_vmode(control);
}


static int wsmove(int line,int column)						/* Move to a location on the screen.	*/
{
	return VL_vmove(line,column);
}


static int wscset(int char_set)							/* Switch to requested character set.	*/
{
	return VL_vcharset(char_set);
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
static void edit_unmark(void)
{
	int row_idx, col_idx;

	if (the_edit->mark_row == -1)
	  return;
	
	for (row_idx=col_idx=0; row_idx <= the_edit->height; )
	{
		if (the_edit->hilite_buf[row_idx][col_idx])
		{								/* if this was hilited, prepare to unhilite */
			*data_map_ptr(the_edit->top_row+row_idx,the_edit->left_col+col_idx) = 1;
			the_edit->hilite_buf[row_idx][col_idx]=0;
			*attr_map_ptr(the_edit->top_row+row_idx,the_edit->left_col+col_idx) =  '\f';
		}
		++col_idx;
		if (col_idx == the_edit->width)
		{
			col_idx=0;
			++row_idx;
		}
	}			  
	the_edit->mark_row = -1;						/* clear the mark */
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
static void edit_init_data(void)
{
	int rowidx;
	int scanidx;
		
	for (rowidx = 0; rowidx < the_edit->height; )
	{
		memcpy(&(the_edit->data_buf[rowidx][0]),
		       data_map_ptr(the_edit->top_row+rowidx,the_edit->left_col),
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
static void edit_copycut_block(int cutflag)
{
	int row_idx, col_idx;
	int start_row, start_col;
	int end_row, end_col;
	int copy_byte_cnt;

	if (the_edit->mark_row== -1)
	{
		return;
	}
	if (the_edit->mark_row < the_edit->edit_row)	/* compute the start and end points */
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
				for (col_idx = 0; col_idx < end_col && col_idx<=the_edit->line_size[row_idx]; 
						++col_idx, ++copy_byte_cnt)
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
static void edit_cut_block(void)
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
static void edit_copy_block(void)
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
static void edit_paste_block(void)
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
static void edit_open_line(int lineno)
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
static void edit_cut_line(int lineno)
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
static void edit_cut_data(int start_row, int start_col, int end_row, int end_col)
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
		return;
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

/*
**	Routine:	vwang_flush()
**
**	Function:	Flush all pending video operations.
**
**	Description:	This is a front end to the video routine VL_vdefer_restore()
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
*/
void vwang_flush(void)
{
	if (!wbackground())
	{
		VL_vdefer_restore();
	}
}

/*
**	Routine:	vwang_write_bell()
**
**	Function:	Use vwang to ring the bell.
**
**	Description:	Issue a vwang write that contains no data but the WCC is set to ring the bell.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
*/
void vwang_write_bell(void)
{
	if (!wbackground())
	{
		unsigned char	function[1], lines[1];

		function[0] = WRITE_ALL;
		lines[0] = 0;

		vwang(function,(unsigned char*)"\001\100\000\000",lines,"A",NULL,NULL);
	}
}

int vwang_set_synch(int synch)
{
	VL_synch_required = synch;
	return synch;
}

void vwang_shut(void)
{
	if (!wbackground())
	{
		WL_wtrace("WVIDEO", "SHUT", "Shutting down screen handler");

		costar_errtext("");

		VL_vexit();
	}
}

void vwang_clear_on_shut(void)
{
	VL_vonexit( NORMALIZE | CLEAR_SCREEN );
}

void vwang_noclear_on_shut(void)
{
	VL_vonexit( NORMALIZE );
}

void vwang_synch(void)
{
	if (!wbackground())
	{
		VL_vsynch();
	}
}

#ifdef unix
void vwang_stty_save(void)
{
	if (!wbackground()) vraw_stty_save();
}

void vwang_stty_restore(void)
{
	if (!wbackground()) vraw_stty_restore();
}

void vwang_stty_sane(void)
{
	if (!wbackground()) vraw_stty_sane();
}

void vwang_stty_sync(void)
{
	if (!wbackground()) vraw_stty_sync();
}
#endif

int vwang_set_reinitialize(int flag)
{
	rts_first = flag;
	return flag;
}

void vwang_pre_init(void)
{
	/*
		This is called the first time into initwisp2() 
		when not in background.
	*/
	WL_wtrace("WVIDEO", "PREPARE", "Prepare screen handler");
	vwang_init_video();
	VL_set_isdebug();
}

void vwang_init_term(void)
{
	if (!wbackground())
	{
		VL_vstate(VSTATE_DEFAULT);
	}
}

int vwang_wcurwidth(void)
{
	return wcurwidth;
}

void vwang_bell(int cnt)
{
	if (wbackground()) return;
	
	while( cnt > 0 )
	{
		VL_vbell();						/* Ring it bell_count times.			*/
		cnt--;
	}
}

int vwang_keypressed(int discard)
{
	int inchar;

	if (wbackground()) return 0;
	
	if ((inchar = VL_vcheck()))						/* See if there is a keypress in bufr.	*/
	{
		if (!discard)
		{
			VL_vpushc((char)inchar);				/* Push the char.			*/
		}
		return 1;
	}
	return 0;
}

/*
**	ROUTINE:	vwang_init_video()
**
**	FUNCTION:	Tell VIDEO what videocap & error log files to use.
**
**	DESCRIPTION:	Get the wispterm and tell video.
**			Also set the error log file for video to use.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	It is important that this gets called before any
**			video library routines get called.
**
*/
void vwang_init_video(void)
{
	static int first = 1;
	
	if (first)
	{
		WL_wtrace("WVIDEO", "PREP", "Prepare screen handler");

		if(WL_wtracing())
		{
			VL_trace_enable();
		}

		VL_vre_set_logfile(WL_werrpath());	/* Set video to log errors to the wisperr.log file */
		
		if (!wbackground())
		{
			VL_vcap_set_vcapfile(wisptermfilepath(NULL), wispterm(NULL));
		}
		first = 0;
	}
}

/*
**	ROUTINE:	mark_hotspots()
**
**	FUNCTION:	Mark the hotspots on this row.
**
**	DESCRIPTION:	Scan the given row for hotspot text strings and mark them
**			by setting there displayable attribute to REVERSE video.
**			(Also change the attr map to underline to prevent
**			optimization from not redrawing the chars.)
**
**	ARGUMENTS:
**	row_x		The row number
**	numcols		The number of columns in the row (normally 80)
**
**	GLOBALS:
**	data_map
**	attr_map
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
static void mark_hotspots(int row_x, int numcols)
{
	static int first=1;
	static int hotspot_vmode = W4W_HOTSPOT_VMODE;
	char	the_row[WS_MAX_COLUMNS_ON_A_LINE+1];
	char	the_mask[WS_MAX_COLUMNS_ON_A_LINE+1];

	if (first)
	{
		first = 0;
		if (use_costar())
		{
			hotspot_vmode = costar_hotspot_vmode();
		}
	}

	/*
	**	Take a copy of the row and replace all non-ascii char with spaces.
	*/
	copy_clean_row(row_x, numcols, the_row);

	/*
	**	Create a mask of the hotspot positions
	*/
	if (w4w_mask_row(the_row, the_mask))
	{
		fac_t 	*am;
		int	col;

		am = attr_map_ptr(row_x,0);

		for(col=0; col < numcols; col++)
		{
			if ('X' == the_mask[col])
			{
				/*
				**	Change the attribute to reversed for each char in hotspot
				*/
				ws_putbuff(the_row[col], row_x, col, hotspot_vmode, VCS_DEFAULT);

				/*
				**	Change the attribute map to prevent optimization from not redrawing
				*/
				am[col] = FAC_SET_UNDERSCORED(am[col]);
			}
		}
	}
}

/*
**	ROUTINE:	copy_clean_row()
**
**	FUNCTION:	Get a copy of a given row and clean it of non-printables and modifiables.
**
**	DESCRIPTION:	
**			The variable the_row will contain a copy of the row with all
**			FACs and non-printables changed to spaces and all modifiable
**			fields also changed to spaces.
**
**			Hidden fields will also be changed to spaces.
**
**	ARGUMENTS:	
**	row_x		The row number
**	numcols		The number of columns in the row (normally 80)
**	the_row		The returned cleaned row
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
static void copy_clean_row(int row_x, int numcols, char *the_row)
{
	int	col;
	unsigned char *dm;
	fac_t *am;

	dm = data_map_ptr(row_x,0);
	am = attr_map_ptr(row_x,0);

	/*
	**	Take a copy of the row and replace all non-ascii char with spaces.
	*/
	memcpy(the_row,dm,numcols);
	for(col=0; col<numcols; col++)
	{
		/*
		**	Replace any non-printable chars and modifiable positions with spaces.
		*/
		if ( the_row[col] < ' ' || the_row[col] > '~' || !FAC_PROTECTED(am[col]) || FAC_BLANK(am[col]) )
		{
			the_row[col] = ' ';
		}
	}
	the_row[numcols] = (char)0;
}

/*
**	ROUTINE:	meta_pfkey()
**
**	FUNCTION:	Change a pfkey number into a pfkey meta character
**
**	DESCRIPTION:	{Full detailed description}...
**
**	ARGUMENTS:
**	pfkey		The pfkey number 
**			0 	= Enter
**			1 - 32	= Pfkey 1 - 32
**			33	= Help
**
**	GLOBALS:	None
**
**	RETURN:		The meta character or 0 if invalid pfkey.
**
**	WARNINGS:	None
**
*/
static int meta_pfkey(int pfkey)
{
	int	meta_char;
	
	switch (pfkey)
	{
	case 0:		meta_char = enter_key;		break;

	case 1:		meta_char = fn1_key;		break;
	case 2:		meta_char = fn2_key;		break;
	case 3:		meta_char = fn3_key;		break;
	case 4:		meta_char = fn4_key;		break;
	case 5:		meta_char = fn5_key;		break;
	case 6:		meta_char = fn6_key;		break;
	case 7:		meta_char = fn7_key;		break;
	case 8:		meta_char = fn8_key;		break;
	case 9:		meta_char = fn9_key;		break;
	case 10:	meta_char = fn10_key;		break;
	case 11:	meta_char = fn11_key;		break;
	case 12:	meta_char = fn12_key;		break;
	case 13:	meta_char = fn13_key;		break;
	case 14:	meta_char = fn14_key;		break;
	case 15:	meta_char = fn15_key;		break;
	case 16:	meta_char = fn16_key;		break;
	case 17:	meta_char = fn17_key;		break;
	case 18:	meta_char = fn18_key;		break;
	case 19:	meta_char = fn19_key;		break;
	case 20:	meta_char = fn20_key;		break;
	case 21:	meta_char = fn21_key;		break;
	case 22:	meta_char = fn22_key;		break;
	case 23:	meta_char = fn23_key;		break;
	case 24:	meta_char = fn24_key;		break;
	case 25:	meta_char = fn25_key;		break;
	case 26:	meta_char = fn26_key;		break;
	case 27:	meta_char = fn27_key;		break;
	case 28:	meta_char = fn28_key;		break;
	case 29:	meta_char = fn29_key;		break;
	case 30:	meta_char = fn30_key;		break;
	case 31:	meta_char = fn31_key;		break;
	case 32:	meta_char = fn32_key;		break;

	case 33:	meta_char = help_key;		break;

	default:	meta_char = 0;			break;
	}

	return meta_char;
}

/*
**	This came from initscrn.c
*/
static int no_term = 0;
extern const char *WL_language_path();								/* this is from wperson.c. it is*/

int vwang_init_screen(void)
{
	static int term_type = VT300;								/* Default to VT100 terminal.	*/
	static int kp_on = FALSE;								/* Assume keypad will be off.	*/
	int retcod;

	if (wbackground()) 
	{
		no_term = 1;
		return(0);									/* In background, no terminal.	*/
	}

	vwang_init_video();

	retcod = 1;										/* Assume success.		*/
	if (rts_first)										/* Is this the first time?	*/
	{
		int4	def_bgchange, def_excolor, def_bgcolor;

		rts_first = FALSE;								/* No longer the 1st time.	*/
		VL_voptimize(VOP_DATA_AND_CONTROLS);						/* Select appropriate optimiz.	*/

		VL_vstate(DEFAULT);								/* Select default screen setup.	*/
		if (VL_vscr_wid == 80) VL_vscreen(VSCREEN_NARROW);					/* Select dark narrow screen.	*/
		else VL_vscreen(VSCREEN_WIDE);							/* else dark wide screen.	*/

		WL_get_defs(DEFAULTS_BGCHANGE,(char*)&def_bgchange);
		WL_get_defs(DEFAULTS_EXCOLOR,(char*)&def_excolor);
		WL_get_defs(DEFAULTS_BGCOLOR,(char*)&def_bgcolor);

		VL_vonexit(NORMALIZE|CLEAR_SCREEN);						/* Clear the screen on exit	*/
		if (def_bgchange && def_excolor) VL_vonexit(NORMALIZE|CLEAR_SCREEN|LIGHT);
		if (def_bgchange && !def_excolor) VL_vonexit(NORMALIZE|CLEAR_SCREEN|DARK);

		if (def_bgchange)								/* Should we change background?	*/
		{	
			if (def_bgcolor) VL_vscreen(VSCREEN_LIGHT);				/* Set the screen grey.		*/
			else VL_vscreen(VSCREEN_DARK);						/* Set the screen black.	*/
		}
		else VL_vscreen(VSCREEN_NOOP);							/* Then leave it alone always.	*/

		if ( strlen(WL_language_path()) )						/* if language was specified,   */
			IVS_vlanguage( WL_language_path() );					/* use it and call vlanguage    */
	}

	if ((term_type == VT100) && kp_on) VL_vset(VSET_KEYPAD,APPLICATIONS);			/* Applications keypad on?	*/
	else VL_vset(VSET_KEYPAD,NORMAL);
	VL_vset_cursor_off();									/* Don't show the cursor.	*/
	vwang_ws_erap(FULL_SCREEN);									/* Erase the full screen.	*/
	VL_vmode(VMODE_CLEAR);									/* Return to normal rendition.	*/

	return(retcod);	       									/* Return to caller, all ok...	*/
}

static int check_scrn(void)
{
	if (rts_first) return(-1);								/* Hasn't been checked yet.	*/

	if (no_term)	return(0);								/* Say no terminal.		*/
	else		return(1);								/* Say yes terminal.		*/
}

/*
**	ROUTINE:	vwang_title()
**
**	FUNCTION:	set the title (if appropriate) for the screen
**
**	DESCRIPTION:	currently implemented for WIN32 console titles
**			and Costar.
**
**			Pass NULL as the_title to retrieve the last 
**			title without changing it.
**
**	ARGUMENTS:	
**	the_title       desired title (or NULL)
**
**	GLOBALS:	None
**
**	RETURN:		The last title set (may be NULL)	
**
**	WARNINGS:	
**
*/
const char* vwang_title(const char *the_title)
{
	static char* last_title = NULL;

	if (WL_get_wisp_option("NOTITLE"))
	{
		return NULL;
	}

	if (NULL != the_title)
	{
		if (NULL != last_title)
		{
			free(last_title);
		}

		last_title = wisp_strdup(the_title);
		
		if (!wbackground() && !wisp_nativescreens())
		{
			if (use_costar())
			{
				WL_wtrace("WTITLE","SET","CoStar Title=[%s]", the_title);
				costar_title(last_title);
			}
			else if (!wisp_winsshd() && !wisptelnet())
			{
				WL_wtrace("WTITLE","SET","Console Title=[%s]", the_title);
				VL_vtitle(last_title);
			}
		}
	}

	return last_title;
}

/*
**	ROUTINE:	WTITLE()
**
**	FUNCTION:	Cobol frontend to vwang_title()
**
**	DESCRIPTION:	Strip and null terminate the title string then call vwang_title()
**
**	ARGUMENTS:	
**	the_title	Blank padded char[80] with window title. (not null terminated)
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	none
**
*/
void WTITLE(const char the_title[WTITLE_TITLE_LEN])
{
	char	title[WTITLE_TITLE_LEN+1];
	int	i;

	memcpy(title,the_title,WTITLE_TITLE_LEN);
	title[80] = (char)0;
	
	for(i=strlen(title) - 1; i >=0 && ' '==title[i]; i--)
	{
		title[i] = (char)0;
	}

	vwang_title(title);	
}

/*
**	ROUTINE:	vwang_load_charmap()
**
**	FUNCTION:	Load the CHARMAP translation tables.
**
**	DESCRIPTION:	The default CHARMAP path defaults to $(WISPCONFIG)/CHARMAP
**			but can be overridden by environment variable $CHARMAP.
**
**	ARGUMENTS:	
**	force		Force reload flag
**
**	GLOBALS:	
**	tt_ansi2wang	The Ansi to Wang character set mapping table
**	tt_wang2ansi	The Wang to Ansi character set mapping table
**	tt_term2wang	The Terminal to Wang character set mapping table
**	tt_wang2term	The Wang to Terminal character set mapping table
**
**	RETURN:		None
**
**	WARNINGS:	No warnings are issued unless tracing is turned on.
**
*/
void vwang_load_charmap(int force)
{
	static int first = 1;
	int i;

	if (first || force)
	{
		FILE 	*the_file;
		char	inlin[80];
		int	cnt;
		int	ansichar, wangchar, termchar;
		char	charmap_path[256];
		char	*ptr;

		first = 0;
		
		/*
		**	CHARMAP must not conflict with the VWANG substition table.
		**	First make sure sub table is loaded
		**	Reset any sub table mappings which are used by CHARMAP.
		*/
		load_sub_chars();
	
		/*
		**	Default assumes no translation
		*/
		for(i=0; i<256; i++) { tt_ansi2wang[i] = i; }
		for(i=0; i<128; i++) { tt_wang2ansi[i] = i; }
		for(i=0; i<256; i++) { tt_term2wang[i] = i; }
		for(i=0; i<128; i++) { tt_wang2term[i] = i; }

		/*
		**	Default printable is 32-126 (space - '~')
		*/
		memset(is_printable, 0, 128);
		for(i=32; i<=126; i++) { is_printable[i] = -1; }

		/*
		**	Load the CHARMAP values
		*/
		if ((ptr = getenv("CHARMAP")))
		{
			strcpy(charmap_path,ptr);
			WL_wtrace("CHARMAP", "LOAD", "Found environment variable CHARMAP=%s",charmap_path);
		}
		else
		{
			WL_build_wisp_config_path("CHARMAP",charmap_path);
		}
		the_file = fopen(charmap_path,"r");
		if (the_file)
		{
			WL_wtrace("CHARMAP", "LOAD", "Loading from file CHARMAP=%s",charmap_path);

			/*
			**	WangChar (1-127) Ansichar (1-255) TermChar (1-255)
			*/
			while(fgets(inlin,sizeof(inlin)-1,the_file))
			{
				WL_remove_eol(inlin);

				if (strlen(inlin) > 0 && inlin[0] != '#') /* Not a comment */
				{
					cnt = sscanf(inlin,"%d %d %d", &wangchar, &ansichar, &termchar);
					
					if ( 3 != cnt )
					{
						WL_wtrace("CHARMAP", "LOAD", "Syntax error [%s]",inlin);
					}
					else if (!IS_WANG_CHAR(wangchar))
					{
						WL_wtrace("CHARMAP", "LOAD", "Invalid WangChar=%d, outside range 1-127 [%s]",
						       wangchar, inlin);
					}
					else if (!IS_8BIT_CHAR(ansichar))
					{
						WL_wtrace("CHARMAP", "LOAD", "Invalid AnsiChar=%d, outside range 1-255 [%s]",
						       ansichar, inlin);
					}
					else if (!IS_8BIT_CHAR(termchar))
					{
						WL_wtrace("CHARMAP", "LOAD", "Invalid TermChar=%d, outside range 1-255 [%s]",
						       termchar, inlin);
					}
					else if (tt_ansi2wang[ansichar] != ansichar)
					{
						WL_wtrace("CHARMAP", "LOAD", "Invalid double-mapping of AnsiChar=%d (WangChar=%d)",
						       ansichar, wangchar);
					}
					else if (tt_wang2ansi[wangchar] != wangchar)
					{
						WL_wtrace("CHARMAP", "LOAD", "Invalid double-mapping of WangChar=%d (AnsiChar=%d)",
						       wangchar, ansichar);
					}
					else if (tt_term2wang[termchar] != termchar)
					{
						WL_wtrace("CHARMAP", "LOAD", "Invalid double-mapping of TermChar=%d (WangChar=%d)",
						       termchar, wangchar);
					}
					else if (tt_wang2term[wangchar] != wangchar)
					{
						WL_wtrace("CHARMAP", "LOAD", "Invalid double-mapping of WangChar=%d (TermChar=%d)",
						       wangchar, termchar);
					}
					else
					{
						/* WL_wtrace("CHARMAP", "LOAD", "Mapping WangChar=%d AnsiChar=%d TermChar=%d",
						       wangchar, ansichar, termchar); */

						tt_ansi2wang[ansichar] = wangchar; 
						tt_wang2ansi[wangchar] = ansichar; 

						tt_term2wang[termchar] = wangchar; 
						tt_wang2term[wangchar] = termchar; 

						/*
						**	CHARMAP must not conflict with the VWANG substition table.
						**	Reset any sub table mappings which are used by CHARMAP.
						*/
						stable[wangchar].sub_value = wangchar;
						stable[wangchar].char_set  = VCS_DEFAULT;

						/*
						**	The WangChar is printable
						*/
						is_printable[wangchar] = -1;
					}
				}
			}
			
			fclose(the_file);
		}
	}
}

void vwang_ansi2wang(unsigned char *buff, int len)
{ 
	int	i;
	
	for(i=0; i<len; i++)
	{
/*  warning: comparison is always true due to limited range of data type */
		buff[i] = (unsigned char) ANSI2WANG(buff[i]);
	}
}

void vwang_wang2ansi(unsigned char *buff, int len)
{ 
	int	i;
	
	for(i=0; i<len; i++)
	{
		buff[i] = (unsigned char) WANG2ANSI(buff[i]);
	}
}

void vwang_term2wang(unsigned char *buff, int len)
{ 
	int	i;
	
	for(i=0; i<len; i++)
	{
/*  warning: comparison is always true due to limited range of data type */
		buff[i] = (unsigned char) TERM2WANG(buff[i]);
	}
}

void vwang_wang2term(unsigned char *buff, int len)
{ 
	int	i;
	
	for(i=0; i<len; i++)
	{
		buff[i] = (unsigned char) WANG2TERM(buff[i]);
	}
}

void vwang_subtable(unsigned char *buff, int len)
{
	int	i;
	unsigned char	schar;
	int	char_set;
	
	for(i=0; i<len; i++)
	{
		sub_char(buff[i], &schar, &char_set);

		if (buff[i] != schar || char_set != VCS_DEFAULT)
		{
			if (char_set != VCS_DEFAULT)
			{
				buff[i] = '*';
			}
			else
			{
				buff[i] = schar;
			}
		}
	}
}

void WANSI2WANG(unsigned char *buff, int2 *len)
{
	vwang_ansi2wang(buff, *len);
}
void WWANG2ANSI(unsigned char *buff, int2 *len)
{
	vwang_wang2ansi(buff, *len);
}


static int vwputc(int c)
{
	return VL_vputc((char)WANG2TERM(c));
}

static int mousenonmod(void)
{
	static int rc = -1;

	if (-1 == rc)
	{
		if (WL_get_wisp_option("MOUSENONMOD"))
		{
			rc = 1;
		}
		else
		{
			rc = 0;
		}
	}

	return rc;
}


/*
**	History:
**	$Log: vwang.c,v $
**	Revision 1.140  2011/10/20 01:02:47  gsl
**	mark warnings
**	
**	Revision 1.139  2011/08/22 03:09:59  gsl
**	Support for WinSSHd on Windows
**	
**	Revision 1.138  2010/01/11 04:27:12  gsl
**	fix warnings: comparing fac_t < 255
**	
**	Revision 1.137  2003/12/02 14:30:08  gsl
**	Added NOTITLE option
**	
**	Revision 1.136  2003/06/27 15:54:03  gsl
**	fix EDE API
**	
**	Revision 1.135  2003/06/20 15:37:45  gsl
**	VL_ globals
**	
**	Revision 1.134  2003/05/22 14:08:21  gsl
**	Add WTITLE_TITLE_LEN
**	
**	Revision 1.133  2003/04/04 16:34:10  gsl
**	Trace pfkeys
**	
**	Revision 1.132  2003/03/17 20:29:19  gsl
**	Remove overly large and non-useful traces
**	
**	Revision 1.131  2003/03/12 18:18:10  gsl
**	FIx -Wall warnings
**	
**	Revision 1.130  2003/02/04 18:29:12  gsl
**	fix -Wall warnings
**	
**	Revision 1.129  2003/02/04 17:05:01  gsl
**	Fix -Wall warnings
**	
**	Revision 1.128  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.127  2002/12/13 21:17:30  gsl
**	vwang_title() fixes
**	
**	Revision 1.126  2002/12/13 19:25:47  gsl
**	In WTITLE() was cobol style parameter to costar_title() instead of null terminated C string.
**	
**	Revision 1.125  2002/12/10 20:54:09  gsl
**	use WERRCODE()
**	
**	Revision 1.124  2002/12/03 22:15:13  gsl
**	Replace the w_err_flag bitmask with wispdebug mode that can be set to "FULL"
**	"ERRORS" or "NONE" to simplify.
**	
**	Revision 1.123  2002/10/18 19:14:08  gsl
**	Cleanup
**	
**	Revision 1.122  2002/08/01 15:07:35  gsl
**	type warnings
**	
**	Revision 1.121  2002/08/01 14:45:10  gsl
**	type warnings
**	
**	Revision 1.120  2002/08/01 14:09:10  gsl
**	type warnings
**	
**	Revision 1.119  2002/08/01 02:46:29  gsl
**	Replace vwang calls with WS_CLOSE WS_READ WS_READ_ALT WS_REWRITE
**	
**	Revision 1.118  2002/07/31 21:00:28  gsl
**	globals
**	
**	Revision 1.117  2002/07/26 18:19:15  gsl
**	wscreen -> WSCREEN
**	
**	Revision 1.116  2002/07/17 17:25:59  gsl
**	fix vbuffering_end problem
**	
**	Revision 1.115  2002/07/16 14:11:45  gsl
**	VL_ globals
**	
**	Revision 1.114  2002/07/16 13:40:17  gsl
**	VL_ globals
**	
**	Revision 1.113  2002/07/15 20:16:02  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.112  2002/07/15 17:52:50  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.111  2002/07/15 17:09:59  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.110  2002/07/15 14:07:01  gsl
**	vwang globals
**	
**	Revision 1.109  2002/07/15 13:29:00  gsl
**	IVS_ globals
**	
**	Revision 1.108  2002/07/12 20:40:39  gsl
**	Global unique WL_ changes
**	
**	Revision 1.107  2002/07/11 20:29:15  gsl
**	Fix WL_ globals
**	
**	Revision 1.106  2002/07/10 21:05:27  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.105  2002/07/09 04:13:55  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.104  2002/07/02 21:15:30  gsl
**	Rename wstrdup
**	
**	Revision 1.103  2002/07/01 04:02:40  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.102  2001/10/15 13:56:34  gsl
**	Change vwang_set_videocap() to vwang_init_video()
**	Add setting the video error log file.
**	
**	Revision 1.101  2001-09-25 11:08:05-04  gsl
**	Remove unneeded ifdefs
**
**	Revision 1.100  1999-09-13 15:55:19-04  gsl
**	Fix return codes from the edit_xxx() routines
**
**	Revision 1.99  1999-08-30 12:50:09-04  gsl
**	Fix the earlier changes to WTITLE() and vwang_title().
**	Moved the conditional call to costar_title() from vwang_title() to WTITLE().
**	WTITLE is only used from COBOL.
**	vwang_title() is used internally for Windows and titles and should not
**	be used for Co*STAR titles.
**
**	Revision 1.98  1999-07-06 10:44:56-04  gsl
**	Fixed Tracker item #305. With videocap files that used a graphics mode
**	for pseudo-blank processing (e.g. vt220) in lowercase fields, the backspace
**	processing would turn-on graphics mode and the ws_echo() would not restore
**	it to the default character set wscset(VSC_DEFAULT). Change ws_echo() to
**	always set the charset to default.
**
**	Revision 1.97  1999-05-11 12:51:01-04  gsl
**	Change vwang_title() to call costar_title() if using costar.
**
**	Revision 1.96  1998-12-14 16:03:48-05  gsl
**	Redo the last change to use thw use_w4w() routine
**
**	Revision 1.95  1998-12-14 15:41:54-05  gsl
**	On WIN32 use w4w only if using direct I/O (not telnet)
**
**	Revision 1.94  1998-10-16 13:37:56-04  gsl
**	Fixed bug trk#725 The clear field and clear after logic was passing
**	the EOL flag to dump so dump was erasing to EOL when it should not be.
**	This only effected READ and REWRITE logic because pseudo blank processing
**	changed the dangle logic in the dump routine.
**
**	Revision 1.93  1998-09-30 17:12:06-04  gsl
**	fix calls to w4w_xxx_row()
**
**	Revision 1.92  1998-06-22 11:38:30-04  gsl
**	Removed the ws_tab_stop() and replace it's use with tab_fac().
**	Rewrote and documented tab_fac().
**	Documented attr_map_ptr()
**	Fixed next_mp() to use menu_pick_combo() instead of tab_fac()
**
**	Revision 1.91  1998-06-18 18:36:48-04  gsl
**	Trk 516 & 534.
**	The NUMPROT tab-stop fields that used a pseudo blank (x0b) instead of
**	a menu-pick (x05) were broken in 4.2.
**	The rendition of the pseudo blank was not being used when in a protected
**	field. When an underlined space was the pseudo blank (default) the
**	space was displayed without the underline rendition (just a space).
**	Fixed ws_write() and rewrite_mp() to handle numprot-pseudo-blank tabstops.
**
**	Revision 1.90  1998-05-21 13:25:58-04  gsl
**	Add support for COSTARV2 attribute mode
**
**	Revision 1.89  1998-05-13 15:27:34-04  gsl
**	Change the IS_PRINTABLE_CHAR() macro logic to use the is_printable[] table.
**	The printable range for wangchars is 32-126 plus the wang chars in CHARMAP.
**	This fixes the problem #526 where not using CHARMAP and vwang thinks
**	the ESC char is a valid entry.
**
**	Revision 1.88  1998-04-13 17:08:23-04  gsl
**	Added MOUSENONMOD option to allow mouse positioning to a nonmodifiable location
**	on the screen
**
**	Revision 1.87  1998-04-13 13:16:07-04  gsl
**	For synch and stty first check not in background
**
**	Revision 1.86  1998-03-16 11:45:32-05  gsl
**	Now that HELP has been converted for Native screens, re-add the
**	tests for WL_ishelpactive() to the VL_vsharedscreen() test
**
**	Revision 1.85  1998-03-06 11:41:28-05  gsl
**	Add the costar_after_write_api following a vwang_wpopscr()
**
**	Revision 1.84  1998-01-15 15:58:52-05  gsl
**	Fix ws_insert() to allow you to push a NULL off the end of a line
**
**	Revision 1.83  1997-12-19 10:42:49-05  gsl
**	Add vwang_terminal_control_char() routine
**
**	Revision 1.82  1997-12-18 21:02:08-05  gsl
**	Fix CHARMAP processing to understand Terminal character set
**
**	Revision 1.81  1997-12-18 09:14:07-05  gsl
**	fix warning
**
**	Revision 1.80  1997-12-15 13:16:24-05  gsl
**	Add support for non-ASCII characters thru CHARMAP character mapping
**
**	Revision 1.79  1997-11-21 16:36:58-05  gsl
**	Fixed the VL_vsharedscreen() logic to work from HELP
**
**	Revision 1.78  1997-10-27 14:30:39-05  gsl
**	Allow 33=help in terminate_list
**
**	Revision 1.77  1997-10-20 14:50:42-04  gsl
**	Add nativescreens() support to vwang_title()
**
**	Revision 1.76  1997-09-22 13:14:19-04  gsl
**	Change isdebug() into the more general VL_vsharedscreen()
**
**	Revision 1.75  1997-08-25 16:11:46-04  gsl
**	Add COSTAR pre & post, read & write hooks for customizing costar
**
**	Revision 1.74  1997-07-15 21:50:45-04  gsl
**	Added "if not in background" to a number of routines.
**	vwang_shut() was calling a costar routine in background that
**	was trying to init the terminal.
**
**	Revision 1.73  1997-07-15 17:12:29-04  gsl
**	Previously a SPACE char with a BLINK attribute was treated as invisible
**	(it really is invisible on a real vt220) however when attributes are
**	simulated with colors then it is not invisible (NT, COSTAR, PC terminal
**	emulators).
**	Fixed so that SPACE chars are written without stripping the BLINK attribute.
**
**	Revision 1.72  1997-07-14 08:28:21-04  gsl
**	Change so works with COSTAR on WIN32.
**	Change to use new video.h interfaces
**
**	Revision 1.71  1997-05-21 09:10:02-04  gsl
**	Fixed bug when HELP key pressed from within the HELP screen, a memory
**	overwrite was occuring.
**	Removed ifdef OLD_VIDEO code
**
**	Revision 1.70  1997-04-16 08:23:09-04  gsl
**	Change to use WL_wtrace()
**
**	Revision 1.69  1997-03-24 22:24:50-05  gsl
**	Fix rewrite_mp() as it was erasing to end-of-line when it wasn't
**	save to. This was causing the right side of screen to blank out as
**	you moved down menu-picks on the left.
**
**	Revision 1.68  1997-02-24 20:43:07-05  gsl
**	Added WTITLE() a cobol nterface to the vwang_title() routines
**
**	Revision 1.67  1997-01-14 20:07:14-05  gsl
**	vwang_title() only calls vtitle() if not in background
**
**	Revision 1.66  1997-01-11 15:55:39-08  gsl
**	Fix the w4w logic so correct on UNIX - had been broken when NT/95
**	code was added
**
**	Revision 1.65  1997-01-11 12:40:43-08  gsl
**	Did some cleanup to isolate video variables.
**	wsmode() now simple calls VL_vmode()
**	wsmove() calls VL_vmove()
**	wscset() calls vcharset()
**
**	Revision 1.64  1997-01-09 11:43:11-08  gsl
**	Make wsmove() and chek_scrn() static
**
**	Revision 1.63  1996-11-18 15:52:56-08  jockc
**	added vwang_title call to set screen title if appropriate
**
**	Revision 1.62  1996-11-13 17:20:29-08  gsl
**	Changed to use vcontrol_flush()
**
**	Revision 1.61  1996-11-04 16:11:06-08  gsl
**	Add include of wispcfg.h
**
**	Revision 1.60  1996-11-04 15:48:17-08  gsl
**	Add vwang_init_video() routine which is called to tell video which
**	videocap file to use.
**	It is then called from vwang_pre_init(), vwang(), and vwang_init_screen(), it
**	must be called before any video routines are called.
**
**	Revision 1.59  1996-08-16 14:46:36-07  gsl
**	Change to use fac_t for fac data items types
**
**	Revision 1.58  1996-08-16 11:09:57-07  gsl
**	Split the COSTAR code in COSTAR and W4W
**
**	Revision 1.57  1996-08-15 17:20:30-07  gsl
**	Combine the costar and WIN32 mouse handline
**
**	Revision 1.56  1996-08-15 13:30:07-07  gsl
**	W4W was displaying hidden (BLANK) fields - fixed to treat hidden fields as spaces
**	fixed ws_putbuff() optimization of space characters.
**
**	Revision 1.55  1996-08-15 11:20:55-07  jockc
**	added preliminary mouse support for win32
**
**	Revision 1.54  1996-07-26 10:36:44-07  gsl
**	Fix signed vs unsigned warnings
**
**	Revision 1.53  1996-07-18 16:33:19-07  gsl
**	Enclose costar code in ifdef COSTAR.
**	Make the vwang_stty_xxx() routines unix only
**
**	Revision 1.52  1996-07-18 13:28:36-07  gsl
**	add vraw_stty_sync()
**
**	Revision 1.51  1996-07-17 15:13:23-07  gsl
**	Fix video prototype warmings
**
**	Revision 1.50  1996-07-17 14:53:53-07  gsl
**	Changed to use wmalloc() and fixed video prototype warnings
**
**	Revision 1.49  1996-07-17 09:24:44-07  gsl
**	Add missing includes.
**	Change a vre() to an ASSERT
**
**	Revision 1.48  1996-07-15 10:11:36-07  gsl
**	Fix for NT
**	Move wsmove() to here
**
**	Revision 1.47  1996-07-09 17:21:55-07  gsl
**	Fix signed vs unsigned char warnings
**
**	Revision 1.46  1996-07-09 17:00:34-07  gsl
**	Add vwang_keypressed() as a frontend for VL_vcheck()
**
**	Revision 1.45  1996-06-28 16:52:10-07  gsl
**	Add routine vwang_pre_init()
**
**	Revision 1.44  1996-06-28 09:11:09-07  gsl
**	added the routines form initscrn.c.
**	changed delete() to unlink()
**
**	Revision 1.43  1996-06-26 17:16:51-07  gsl
**	Add vwang_bell()
**
**	Revision 1.42  1995-08-22 06:45:47-07  gsl
**	fix warning on DG
**
 * Revision 1.41  1995/07/06  16:59:49  gsl
 * Add the W4WMAP mouse pfkey processing.
 * This handles the hotspot stuff all from vwang().
 *
 * Revision 1.40  1995/07/05  16:52:15  gsl
 * changed the costar_hotspot() routine to call w4w_mask_row()
 *
 * Revision 1.39  1995/07/05  15:36:20  scass
 * Added custom vwang hook
 * (SFG enhancement)
 *
 * Revision 1.37  1995/06/26  10:58:40  gsl
 * Move the load hotspots file stuff to costar.c
 *
 * Revision 1.36  1995/06/13  09:25:59  gsl
 * Fix questionable syntax x=&y --> x = &y and
 * Add strdup() define #ifdef NOSTRDUP
 *
 * Revision 1.35  1995/05/01  09:47:48  gsl
 * change calls to costar_message() to costar_errtext()
 *
 * Revision 1.34  1995/04/25  09:54:17  gsl
 * drcs state V3_3_15
 *
 * Revision 1.33  1995/04/17  11:47:24  gsl
 * drcs state V3_3_14
 *
 * Revision 1.32  1995/04/05  09:33:11  gsl
 * Moved the COSTAR routines to costar.h
 * and costar.c files.
 * Changed to use all the new video.h defines.
 *
 * Revision 1.31  1995/04/04  11:00:54  gsl
 * Add mouse TAB-STOP support for costar
 * Add costar Working... message
 * Fix mouse support on help
 *
 * Revision 1.30  1995/03/31  16:51:16  gsl
 * fix blink rendition plus add comments and remove netroncap externals
 *
 * Revision 1.29  1995/03/31  15:23:32  gsl
 * fix the costar optimazation error
 *
 * Revision 1.28  1995/03/27  17:00:43  gsl
 * removed some globals and added vwang_curwidth()
 *
 * Revision 1.27  1995/03/24  16:59:53  gsl
 * removed the rest of the vml() stuff
 * added vchr_map_ptr() and vatr_map_ptr() and directed all access
 * to these video maps thru these routines.
 *
 * Revision 1.26  1995/03/24  15:49:25  gsl
 * BIG TIME cleanup!!!
 * Moved ws_cut() and ws_paste() from edehelp.c to here.
 * Removed all the mx_row/vrow/vml(row) stuff so that all col & row refer
 * to the actual col & row positions.
 * All access to the data_map[] and attr_map[] now go thru two routines
 * data_map_ptr() and attr_map_ptr()
 *
 * Revision 1.25  1995/03/22  17:08:20  gsl
 * renamed i,j,k,n,m,c to meaningful names in ws_deleft()
 *
 * Revision 1.24  1995/03/22  16:42:35  gsl
 * change vrow (formerly "k") to mx_row = vml(cursor_row)
 * Also change hotspot logic to require only leading spaces.
 * Standardized access to the data_map and attr_map
 *
 * Revision 1.23  1995/03/21  16:16:48  gsl
 * Add the costar mouse support routines
 *
 * Revision 1.22  1995/03/20  15:39:26  gsl
 * In ws_read() replaced i,j,k,m with meaningful data names.
 *
 * Revision 1.21  1995/03/20  14:43:06  gsl
 * changed to read the hotspots from a file for costar
 *
 * Revision 1.20  1995/03/17  16:12:14  gsl
 * Added the first pass at CoStar hotspot logic
 *
 * Revision 1.19  1995/03/17  11:20:38  gsl
 * Added the first layer of support for CoStar, when the costar flag is
 * on then all EDIT fields are underlined and all non-edit fields
 * are not underlined.
 * Also switched to the new FAC macros.
 * Did a lot of cosmetic cleanup
 *
 * Revision 1.18  1995/03/10  16:49:58  gsl
 * merge in wsmode() and wscset()
 *
 * Revision 1.17  1995/03/09  17:03:13  gsl
 * added vwang routines as front-ends to video routines
 *
 * Revision 1.16  1995/03/09  13:43:12  gsl
 * add vwang_set_synch() routine
 *
 * Revision 1.15  1995/03/07  09:59:26  gsl
 * replace literals 24, 80, 1924 with defines
 *
 * Revision 1.14  1995/02/28  13:25:30  gsl
 * Add debug code to dump a screen to a file.
 *
 * Revision 1.13  1995/02/17  12:51:49  gsl
 * move vwang_message_box() to werrvre.c and renamed werr_message_box().
 *
 * Revision 1.12  1995/02/17  12:10:11  gsl
 * add vwang_message_box() as a replacement for vre_window()
 *
 * Revision 1.11  1995/02/15  16:30:06  gsl
 * Add vwang_write_bell()
 * Rings the bell by doing a vwang write with the WCC alarm set.
 *
 * Revision 1.10  1995/02/14  16:57:08  gsl
 * Add vwang_aid_read_unlocked() routine
 * This logic was in wsxio.c however it contains video calls so
 * is better placed here.
 *
 * Revision 1.9  1995/02/14  16:01:51  gsl
 * add vwang_flush() as a frontend to video vdefer(RESTORE) routine.
 *
 * Revision 1.8  1995/02/14  15:06:39  gsl
 * fixed header
 *
 * Revision 1.7  1995/02/14  14:33:03  gsl
 * added a vdefer(RESTORE) to the CLOSE_WORK_STATION funtion
 * so it could be called from WSXIO()
 *
**
*/
