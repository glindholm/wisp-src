			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

			/************************************************************************/
			/* WARNING: If you add any parameters to this file, make sure BOTH the	*/
			/*	    definition point and the reference point are defined.	*/
			/************************************************************************/

#define MAX_ESC 20								/* Maximum length of terminal control sequence.	*/
#define MAX_GENERIC 64								/* Maximum generic control keys.		*/

#ifndef FILE
#include <stdio.h>
#endif

#ifdef VIDEO_DATA_ROOT

/*						Global "general" data.								*/

int tttype = UNKNOWN;								/* Terminal type (obtained from vterminal()).	*/
int vclrhome = FALSE;								/* On full screen erase, cursor stays put.	*/
int vmovebias = 0;								/* Move style is ANSI to begin.			*/
int synch_required = FALSE;							/* Coordination flag between VIDEO and vwang().	*/
int exit_options = NORMALIZE | MOVE_AND_SCROLL;					/* Default exit action is move and scroll.	*/
int debugging = FALSE;								/* The debugger is not on.			*/
int verbose = TRUE;								/* Report internal errors.			*/
int deferred = FALSE;								/* Assume actions are not deferred.		*/
int new_screen = TRUE;								/* Assume first screen is new.			*/
int vscr_wid = 80;								/* Actual current width (flag in vset tables).	*/
int vis_space = REVERSE|UNDERSCORE|BLINK;					/* Visible on normal screen in these cases.	*/
int rvis_space = REVERSE|UNDERSCORE|BLINK|BOLD;					/* Visible on reverse screen in these cases.	*/
int force_200 = TRUE;								/* Attempt to force VT100 into VT220 mode.	*/
int color_first = TRUE;								/* Force a color change on first vscreen call.	*/
int width_first = TRUE;								/* Force a width change on first vscreen call.	*/
FILE *macro_input = NULL;							/* File pointer to macro input file.		*/
FILE *macro_output = NULL;							/* File pointer to macro output file.		*/
unsigned char abort_character = ABORT_CHARACTER;				/* Abort program when this character entered.	*/
unsigned char refresh_character = REFRESH_CHARACTER;				/* Refresh the screen on this character.	*/
unsigned char trigger_character = TRIGGER_CHARACTER;				/* Bring up the triggered event.		*/
unsigned char vchs_default = 'B';						/* Use "A" for U.K. or "B" for U.S./Canada.	*/

/*					Screen, and line attributes and screen map.						*/

unsigned char vchr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE] = { 0 };	/* Define the character map.			*/
unsigned char vatr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE] = { 0 };	/* Define the attribute map.			*/
unsigned char vmap_cng[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE] = { 0 };	/* Define the change tracking map.		*/
int vlin_cng[MAX_LINES_PER_SCREEN] = { 0 };					/* Define line change table.			*/
int vscr_cng = 0;								/* No changes to the screen yet.		*/
int vmap_top = 0;								/* Virtual top of char and attribute maps.	*/

/*					Tab control data.									*/

int vtab_map[MAX_COLUMNS_PER_LINE] =						/* Tab stops (must be kept sorted).		*/
			{ 8, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88,
			 96,104,112,120,128,133,133,133,133,133,133,
			133,133,133,133,133,133,133,133,133,133,133,
			133,133,133,133,133,133,133,133,133,133,133,
			133,133,133,133,133,133,133,133,133,133,133,
			133,133,133,133,133,133,133,133,133,133,133,
			133,133,133,133,133,133,133,133,133,133,133,
#ifdef MSDOS
			133,133,133	/* 80 columns for MS-DOS */
#else	/* VMS or unix */
			133,133,133,133,133,133,133,133,133,133,133,
			133,133,133,133,133,133,133,133,133,133,133,
			133,133,133,133,133,133,133,133,133,133,133,
			133,133,133,133,133,133,133,133,133,133,133,
			133,133,133,133,133,133,133,133,133,133,133
#endif	/* VMS or unix */
			};

/*					Data involved in screen tracking.							*/

int vcur_lin = 0;								/* Current line on screen.			*/
int vcur_col = 0;								/* Current column on screen.			*/
int vcur_atr = 0;								/* Current character attributes.		*/
int vchr_set = 0;								/* Current character set.			*/
int vcur_set[INT_SET_TABLE_SIZE] = {NORMAL,NORMAL,ANSI,JUMP,TOP_OF_SCREEN,	/* Current terminal settings.			*/
	OFF,ON,OFF,OFF,OFF,ON,OFF,UNLOCKED,REPLACE,VISIBLE,NONE,FULL_SCREEN };
int vlin_atr[MAX_LINES_PER_SCREEN] = { 0 };					/* Line attributes.				*/
int vscr_atr = NARROW|DARK;							/* Assumed screen attributes.			*/
int vrol_top = 0;								/* Top of current scrolling area.		*/
int vrol_bot = MAX_LINES_PER_SCREEN-1;						/* Bottom of current scrolling area.		*/


/*		Data involved in deferred optimization (captured the first time deferred action enabled).			*/

int tcur_lin = 0;								/* True line number (when optimizing).		*/
int tcur_col = 0;								/* True column (when optimizing).		*/
int tcur_atr = 0;								/* True character rendition.			*/
int tchr_set = 0;								/* True character set.				*/
int tcur_set[INT_SET_TABLE_SIZE] = {NORMAL,NORMAL,ANSI,JUMP,TOP_OF_SCREEN,	/* True settings (during defer).		*/
	OFF,ON,OFF,OFF,OFF,ON,OFF,UNLOCKED,REPLACE,VISIBLE,NONE,FULL_SCREEN };
int tlin_atr[MAX_LINES_PER_SCREEN] = { 0 };					/* Line attributes.				*/
int tscr_atr = NARROW|DARK;							/* Assumed screen attributes.			*/
int trol_top = 0;								/* Top of current scrolling area.		*/
int trol_bot = MAX_LINES_PER_SCREEN-1;						/* Bottom of current scrolling area.		*/

/*	Optimization control flags (all optimization is initially off to allow the first action to actually go through).	*/

int optimization = DEFER_MODE;							/* Master optimization starts in defered mode.	*/
int vpri_op = OFF;								/* Print optimization.				*/
int vmov_op = OFF;								/* Optimize move cursor actions in vmove()	*/
int vmod_op = OFF;								/* Optimize rendition mode actions in vmode()	*/
int vset_op[INT_SET_TABLE_SIZE] = { 0 };					/* Terminal setup elements start in off state.	*/
int vchs_op = OFF;								/* Character set optimization is off.		*/
int vrol_op = OFF;								/* Scroll area optimization is off.		*/
int vscr_op = OFF;								/* Screen width and color optimization is off.	*/
int vera_op = OFF;								/* Erase optimization is off.			*/
int vsiz_op = ON;								/* Line size optimization starts on.		*/
int vlin_op = ON;								/* Line drawing optimization starts on.		*/

/*					Data involved when state saved and restored.						*/

int scur_lin = 0;								/* Saved line number (when optimizing).		*/
int scur_col = 0;								/* Saved column (when optimizing).		*/
int scur_atr = 0;								/* Saved character rendition.			*/
int schr_set = 0;								/* Saved character set.				*/

/*					Data used for cursor positioning during dialogue menus.					*/

int vdl_lin = -1;								/* -1 Indicates not valid.			*/
int vdl_col = 0;

/*					Data to control dumping of the output buffer.						*/

int holding_output = FALSE;							/* Start in automatic buffering mode.		*/
int vb_count = 0;								/* Characters in output buffer.			*/
int vb_pure = FALSE;								/* Output line feeds as new lines (aka C).	*/
int video_inited = FALSE;							/* Video initialization control flag.		*/
int state_active = FALSE;							/* State 0 active control flag.			*/

/*					Paste buffer control information.							*/

int paste_index = 0;								/* Index into the paste buffer.			*/
unsigned char paste_buffer[MAX_COLUMNS_PER_LINE] = { 0 };			/* Buffer to hold paste information.		*/


/*					Meta character processing definitions.							*/

int meta_substitute[SUBSTITUTION_TABLE_SIZE][2] = { 0 };			/* Substitution array,  initialize to no subs.	*/

/*					Soft key definitions.									*/

int kp_mode		=	0;
int fk_mode		=	0;
int k0_mode		=	0;
int k1_mode		=	0;
int k2_mode		=	0;
int k3_mode		=	0;
int k4_mode		=	0;
int k5_mode		=	0;
int k6_mode		=	0;
int k7_mode		=	0;

int gold_key 		= 	0;
int blue_key		=	GENERIC_SELECT+VMBIAS;
int grey_key		=	0;
int pink_key		=	0;

int home_key		=	GENERIC_HOME+VMBIAS;

int up_arrow_key	=	GENERIC_UP+VMBIAS;
int down_arrow_key	=	GENERIC_DOWN+VMBIAS;
int left_arrow_key	=	GENERIC_LEFT+VMBIAS;
int right_arrow_key	=	GENERIC_RIGHT+VMBIAS;

int tab_key		=	GENERIC_TAB+VMBIAS;
int backtab_key		=	GENERIC_BACKTAB+VMBIAS;
int newline_key		=	GENERIC_NEWLINE+VMBIAS;

int delete_key		=	GENERIC_DELETE+VMBIAS;
int insert_key		=	GENERIC_INSERT+VMBIAS;
int remove_key		=	GENERIC_REMOVE+VMBIAS;
int clear_field_key	=	GENERIC_CLEAR_FIELD+VMBIAS;
int clear_after_key	=	GENERIC_CLEAR_AFTER+VMBIAS;
int clear_before_key	=	GENERIC_CLEAR_BEFORE+VMBIAS;
int cancel_key		=	GENERIC_CANCEL | BLUE_BIT;
int enter_key		=	GENERIC_ENTER+VMBIAS;
int return_key		=	GENERIC_RETURN+VMBIAS;
int help_key		=	GENERIC_HELP+VMBIAS;

int fn1_key		=	GENERIC_PF1+VMBIAS;
int fn2_key		=	GENERIC_PF2+VMBIAS;
int fn3_key		=	GENERIC_PF3+VMBIAS;
int fn4_key		=	GENERIC_PF4+VMBIAS;
int fn5_key		=	GENERIC_PF5+VMBIAS;
int fn6_key		=	GENERIC_PF6+VMBIAS;
int fn7_key		=	GENERIC_PF7+VMBIAS;
int fn8_key		=	GENERIC_PF8+VMBIAS;
int fn9_key		=	GENERIC_PF9+VMBIAS;
int fn10_key		=	GENERIC_PF10+VMBIAS;
int fn11_key		=	GENERIC_PF11+VMBIAS;
int fn12_key		=	GENERIC_PF12+VMBIAS;
int fn13_key		= 	GENERIC_PF13+VMBIAS;
int fn14_key		= 	GENERIC_PF14+VMBIAS;
int fn15_key		= 	GENERIC_PF15+VMBIAS;
int fn16_key		=	GENERIC_PF16+VMBIAS;
int fn17_key		=	GENERIC_PF17+VMBIAS;
int fn18_key		=	GENERIC_PF18+VMBIAS;
int fn19_key		=	GENERIC_PF19+VMBIAS;
int fn20_key		=	GENERIC_PF20+VMBIAS;
int fn21_key		=	GENERIC_PF21+VMBIAS;
int fn22_key		=	GENERIC_PF22+VMBIAS;
int fn23_key		=	GENERIC_PF23+VMBIAS;
int fn24_key		=	GENERIC_PF24+VMBIAS;
int fn25_key		=	GENERIC_PF25+VMBIAS;
int fn26_key		=	GENERIC_PF26+VMBIAS;
int fn27_key		=	GENERIC_PF27+VMBIAS;
int fn28_key		=	GENERIC_PF28+VMBIAS;
int fn29_key		= 	GENERIC_PF29+VMBIAS;
int fn30_key		= 	GENERIC_PF30+VMBIAS;
int fn31_key		= 	GENERIC_PF31+VMBIAS;
int fn32_key		=	GENERIC_PF32+VMBIAS;
int trigger1 		= 	TRIGGER1+VMBIAS;
int trigger2 		= 	TRIGGER2+VMBIAS;
int trigger3 		= 	TRIGGER3+VMBIAS;
int trigger4 		= 	TRIGGER4+VMBIAS;
int trigger5 		= 	TRIGGER5+VMBIAS;
int trigger6 		= 	TRIGGER6+VMBIAS;
int trigger7 		= 	TRIGGER7+VMBIAS;
int trigger8 		= 	TRIGGER8+VMBIAS;
int key_user1 		= 	VKEY_USER1+VMBIAS;

int key_paste    =       GENERIC_PASTE+VMBIAS;
int key_cut      =       GENERIC_CUT+VMBIAS;
int key_copy     =       GENERIC_COPY+VMBIAS;
int key_mark     =       GENERIC_MARK+VMBIAS;

/*					Data involved in forms processing.							*/

int form_count;										/* Number of valid forms in table.	*/
int current_form;									/* Number of the current form.		*/
int vp_term_key;									/* Forms termination key.		*/
int highest_line_written;								/* Highest form line written.		*/
int freeze_top;										/* Top of freeze area.			*/
char window_message[82];								/* Message line.			*/
char vform_data_buffer[1920];								/* Data buffer area for forms.		*/
char head_form_name[HP_FORM_NAME_SIZE+1];						/* Name of head form.			*/
struct video_form *vformdata;								/* Pointer to the video form data.	*/
struct video_form *vformcurrent;							/* Pointer to the current data.		*/
char *vformproc;									/* Pointer to the processing data.	*/
char *vinitdata;									/* Pointer to the initialization data.	*/

#else	/* #ifdef VIDEO_DATA_ROOT */

/*						Reference "general" data.							*/

extern int tttype;
extern int vclrhome;
extern int vmovebias;
extern int synch_required;
extern int exit_options;
extern int debugging;
extern int verbose;
extern int deferred;
extern int new_screen;
extern int vscr_wid;
extern int vis_space;
extern int rvis_space;
extern int force_200;
extern int color_first;
extern int width_first;
extern FILE *macro_input;
extern FILE *macro_output;
extern unsigned char abort_character;
extern unsigned char refresh_character;
extern unsigned char vchs_default;

/*					Screen, and line attributes and screen map.						*/

extern unsigned char vchr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];
extern unsigned char vatr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];
extern unsigned char vmap_cng[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];
extern int vlin_cng[MAX_LINES_PER_SCREEN];
extern int vscr_cng;
extern int vmap_top;

/*					Tab control data.									*/

extern int vtab_map[MAX_COLUMNS_PER_LINE];

/*					Data involved in screen tracking.							*/

extern int vcur_lin;
extern int vcur_col;
extern int vcur_atr;
extern int vchr_set;
extern int vcur_set[INT_SET_TABLE_SIZE];
extern int vlin_atr[MAX_LINES_PER_SCREEN];
extern int vscr_atr;
extern int vrol_top;
extern int vrol_bot;


/*		Data involved in deferred optimization (captured the first time deferred action enabled).			*/

extern int tcur_lin;
extern int tcur_col;
extern int tcur_atr;
extern int tchr_set;
extern int tcur_set[INT_SET_TABLE_SIZE];
extern int tlin_atr[MAX_LINES_PER_SCREEN];
extern int tscr_atr;
extern int trol_top;
extern int trol_bot;


/*	Optimization control flags (all optimization is initially off to allow the first action to actually go through).	*/

extern int optimization;
extern int vpri_op;
extern int vmov_op;
extern int vmod_op;
extern int vset_op[INT_SET_TABLE_SIZE];
extern int vchs_op;
extern int vrol_op;
extern int vscr_op;
extern int vera_op;
extern int vsiz_op;
extern int vlin_op;

/*					Data involved when state saved and restored.						*/

extern int scur_lin;
extern int scur_col;
extern int scur_atr;
extern int schr_set;

/*					Data for controling the cursor when in dialogue boxes.					*/

extern int vdl_lin;
extern int vdl_col;

/*					Data to control dumping of the output buffer.						*/

extern int holding_output;
extern int vb_count;
extern int vb_pure;
extern int video_inited;
extern int state_active;

/*					Cut/Paste control									*/

extern int paste_index;
extern unsigned char paste_buffer[MAX_COLUMNS_PER_LINE];

/*					Meta character processing definitions.							*/

extern int meta_substitute[SUBSTITUTION_TABLE_SIZE][2];

/*					Soft key definitions.									*/

extern int kp_mode;
extern int fk_mode;
extern int k0_mode;
extern int k1_mode;
extern int k2_mode;
extern int k3_mode;
extern int k4_mode;
extern int k5_mode;
extern int k6_mode;
extern int k7_mode;

extern int gold_key;
extern int blue_key;
extern int grey_key;
extern int pink_key;

extern int home_key;

extern int up_arrow_key;
extern int down_arrow_key;
extern int left_arrow_key;
extern int right_arrow_key;

extern int tab_key;
extern int backtab_key;
extern int newline_key;

extern int delete_key;
extern int insert_key;
extern int remove_key;
extern int clear_field_key;
extern int clear_after_key;
extern int clear_before_key;
extern int cancel_key;
extern int enter_key;
extern int return_key;
extern int help_key;

extern int fn1_key;
extern int fn2_key;
extern int fn3_key;
extern int fn4_key;
extern int fn5_key;
extern int fn6_key;
extern int fn7_key;
extern int fn8_key;
extern int fn9_key;
extern int fn10_key;
extern int fn11_key;
extern int fn12_key;
extern int fn13_key;
extern int fn14_key;
extern int fn15_key;
extern int fn16_key;
extern int fn17_key;
extern int fn18_key;
extern int fn19_key;
extern int fn20_key;
extern int fn21_key;
extern int fn22_key;
extern int fn23_key;
extern int fn24_key;
extern int fn25_key;
extern int fn26_key;
extern int fn27_key;
extern int fn28_key;
extern int fn29_key;
extern int fn30_key;
extern int fn31_key;
extern int fn32_key;

extern int trigger1;
extern int trigger2;
extern int trigger3;
extern int trigger4;
extern int trigger5;
extern int trigger6;
extern int trigger7;
extern int trigger8;

extern int key_user1;

extern int key_paste;
extern int key_cut;
extern int key_copy;
extern int key_mark;

/*					Data involved in forms processing.							*/

extern int form_count;									/* Number of valid forms in table.	*/
extern int current_form;								/* Number of the current form.		*/
extern int vp_term_key;									/* Forms termination key.		*/
extern int highest_line_written;							/* Highest form line written.		*/
extern int freeze_top;									/* Top of frozen area.			*/
extern char window_message[82];								/* Message line.			*/
extern char vform_data_buffer[1920];							/* Data buffer area for forms.		*/
extern char head_form_name[HP_FORM_NAME_SIZE+1];					/* Name of head form.			*/
extern struct video_form *vformdata;							/* Pointer to the video form data.	*/
extern struct video_form *vformcurrent;							/* Pointer to the current data.		*/
extern char *vformproc;									/* Pointer to the processing data.	*/
extern char *vinitdata;									/* Pointer to initialization data.	*/

#endif	/* #else	#ifdef VIDEO_DATA_ROOT */

/*					Terminal control sequence definitions.						*/

#ifndef ESC_COMPAT
#define ESC_COMPAT

#define rvrsidx_esc	vcapdef[SCROLL_REVERSE]					/* Reverse line feed (reverse index).	*/
#define snglwide_esc	""          						/* Set line to single width.		*/
#define dblwide_esc	""          						/* Set line to double width.		*/
#define dbltop_esc	""          						/* Set line to top half of double hi.	*/
#define dblbot_esc	""          						/* Set line to bottom half double hi.	*/
#define efuls_esc	vcapdef[CLR_SCREEN]					/* Clear full screen.			*/
#define efbos_esc	""          						/* Clear from beginning of screen.	*/
#define eteos_esc	vcapdef[CLEAR_EOS]					/* Clear to the end of screen.		*/
#define ecurl_esc	""          						/* Clear current line.			*/
#define efbol_esc	vcapdef[CLEAR_BOL]					/* Clear from beginning of line.	*/
#define eteol_esc	vcapdef[CLEAR_EOL]					/* Clear to the end of current line.	*/
#define mdclr_esc	vcapdef[EXIT_ATTRIBUTE_MODE]				/* Clear all renditions.		*/
#define mdbld_esc	vcapdef[ENTER_BOLD_MODE]				/* Set mode to bold rendition.		*/
#define mdundr_esc	vcapdef[ENTER_UNDERLINE_MODE]				/* Set mode to underscore rendition.	*/
#define mdblk_esc	vcapdef[ENTER_BLINK_MODE]				/* Set mode to blink rendition.		*/
#define mdrvrs_esc	vcapdef[ENTER_REVERSE_MODE]				/* Set mode to reverse rendition.	*/
#define chterm_esc	""          						/* Find what type of terminal it is??	*/
#define swide_esc	vcapdef[WIDE_MODE]					/* Set screen to wide.			*/
#define snarw_esc	vcapdef[NARROW_MODE]					/* Set screen to narrow.		*/
#define slight_esc	vcapdef[SCREEN_REVERSE_MODE]				/* Set screen to light.			*/
#define sdark_esc	vcapdef[SCREEN_NORMAL_MODE]				/* Set screen to dark.			*/
#define scrarea_esc	vcapdef[CHANGE_SCROLL_REGION]				/* Select the scrolling area.		*/
#define mvrowcol_esc	vcapdef[CURSOR_ADDRESS]					/* Move to given position.		*/
#define mvleft_esc	""          						/* Move cursor left (for > 3 pos.)	*/
#define mvright_esc	""          						/* Move cursor right (for > 3 pos.)	*/
#define mvup_esc	vcapdef[CURSOR_UP]					/* Move cursor up one row.		*/
#define mvupup_esc	""          						/* Move cursor up several rows. 	*/
#define mvdown_esc	""          						/* Move cursor down several rows.	*/
#define defchs_esc	vcapdef[EXIT_GRAPHICS_MODE]				/* Default character set		*/
#define grchs_esc	vcapdef[ENTER_GRAPHICS_MODE]				/* Graphics character font set		*/
#define romstdchs_esc	""          						/* ROM standard character font set	*/
#define romgrchs_esc	""          						/* ROM Graphics character font set	*/
#define dlldchs_esc	""          						/* Downline loaded character set	*/
#define uschs_esc	""          						/* US/Canada character font set		*/
#define ukchs_esc	""          						/* UK character font set		*/
#define vt2207bit_esc	""          						/* Set terminal to 7bit VT220.		*/
#define termansi_esc	""          						/* Set terminal to ANSI mode.		*/
#define kpapmd_esc	""          						/* Put keypad in applications mode.	*/
#define kpnorm_esc	""          						/* Put keypad in normal (numeric) mode.	*/
#define scrlsmooth_esc	""          						/* Select smooth scroll.		*/
#define scrljump_esc	""          						/* Select jump (fast) scroll.		*/
#define origscrl_esc	""          						/* Origin is scroll region.		*/
#define origtos_esc	""          						/* Origin is top of screen.		*/
#define arapon_esc	vcapdef[ENTER_AM_MODE]					/* Auto wrap mode is on.		*/
#define arapoff_esc	vcapdef[EXIT_AM_MODE]					/* Auto wrap mode is off.		*/
#define arepton_esc	""          						/* Auto repeat is on.			*/
#define areptoff_esc	""          						/* Auto repeat is off.			*/
#define ilaceon_esc	""          						/* Interlace mode is on.		*/
#define ilaceoff_esc	""          						/* Interlace mode is off.		*/
#define nlmdon_esc	""          						/* Line feed is a new line.		*/
#define nlmdoff_esc	""          						/* Line feed is a line feed.		*/
#define keylckon_esc	""          						/* Lock the keyboard.			*/
#define keylckoff_esc	""          						/* Unlock the keyboard.			*/
#define insmdon_esc	vcapdef[ENTER_INSERT_MODE]				/* Insert mode on.			*/
#define insmdoff_esc	vcapdef[EXIT_INSERT_MODE]				/* Insert mode off.			*/
#define ptermff_esc	""          						/* Print screen on form feed.		*/
#define ptermnone_esc	""          						/* No print terminator.			*/
#define pextscrl_esc	""          						/* Print scroll region only.		*/
#define pextfull_esc	""          						/* Print full screen.			*/
#define aprnton_esc	""          						/* Auto print mode on.			*/
#define aprntoff_esc	""          						/* Auto print mode off.			*/
#define prnton_esc	""          						/* Turn local printer on.		*/
#define prntoff_esc	""          						/* Turn local printer off.		*/
#define cursron_esc	vcapdef[CURSOR_VISIBLE]					/* Turn text cursor on.			*/
#define cursroff_esc	vcapdef[CURSOR_INVISIBLE]				/* Turn text cursor off.		*/

#endif	/* #ifndef ESC_COMPAT */
