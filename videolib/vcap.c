static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

/*
**	File:		vcap.c
**
**	Project:	VIDEO
**
**	RCS:		$Source:$
**
**	Purpose:	Video Capabilities Files
**
**	Routines:	
*/

/*
**	Includes
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#ifdef unix
#include <signal.h>
#include <curses.h>
#undef vline
#include <term.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>

#ifdef WIN32
#include <io.h>
#endif

#include "video.h"
#include "vlocal.h"
#include "vcap.h"
#include "vintdef.h"
#include "vmodules.h"
#include "vraw.h"

/*
**	Structures and Defines
*/

#ifdef unix
#define DEFTERM "vt100"
#endif

#ifdef WIN32
#define DEFTERM "WINCON"
#endif



/*
**	Globals and Externals
*/
extern void vbldfilepath();
void vc_add_stddefs();
void build_vc_meta();
void vcloadsetup();
int vexists(const char* name);

static void vcloadvideocap(char* vcpath, char* wterm_t);

/*
	The following defines the keywords in the VIDEOCAP file and equates them to the above defines for both
	capabilities and function keys.

	The order of the following is not significant.
*/
typedef struct 
{
	char *name;
	int type;
	int index;

} vc_load;

static vc_load vc_load_defs[] =
{

/* CAPABLITIES */

	{ "reset_terminal",		ISCAP,	RESET_TERMINAL },
	{ "change_scroll_region",	ISCAP,	CHANGE_SCROLL_REGION },
	{ "clear_screen", 		ISCAP,	CLR_SCREEN },
	{ "clear_bol", 			ISCAP,	CLEAR_BOL },
	{ "clear_eol", 			ISCAP,	CLEAR_EOL },
	{ "clear_eos", 			ISCAP,	CLEAR_EOS },
	{ "cursor_address", 		ISCAP,	CURSOR_ADDRESS },
	{ "cursor_down", 		ISCAP,	CURSOR_DOWN },
	{ "cursor_invisible", 		ISCAP,	CURSOR_INVISIBLE },
	{ "cursor_visible", 		ISCAP,	CURSOR_VISIBLE },
	{ "cursor_left", 		ISCAP,	CURSOR_LEFT },
	{ "cursor_right", 		ISCAP,	CURSOR_RIGHT },
	{ "cursor_up", 			ISCAP,	CURSOR_UP },
	{ "enter_blink_mode", 		ISCAP,	ENTER_BLINK_MODE },
	{ "enter_bold_mode", 		ISCAP,	ENTER_BOLD_MODE },
	{ "enter_graphics_mode",	ISCAP,	ENTER_GRAPHICS_MODE },
	{ "enter_reverse_mode", 	ISCAP,	ENTER_REVERSE_MODE },
	{ "enter_underline_mode",	ISCAP,	ENTER_UNDERLINE_MODE },
	{ "exit_attribute_mode",	ISCAP,	EXIT_ATTRIBUTE_MODE },
	{ "exit_graphics_mode",	        ISCAP,	EXIT_GRAPHICS_MODE },
	{ "init_terminal", 		ISCAP,	INIT_TERMINAL },
	{ "pad", 			ISCAP,	PAD },
	{ "pseudo_blanks",		ISCAP,	PSEUDO_BLANKS },
	{ "scroll_reverse", 		ISCAP,	SCROLL_REVERSE },
	{ "screen_132", 		ISCAP,	WIDE_MODE },
	{ "screen_80",  		ISCAP,	NARROW_MODE },
	{ "screen_normal",		ISCAP,  SCREEN_NORMAL_MODE},
	{ "screen_reverse",		ISCAP,  SCREEN_REVERSE_MODE},
	{ "color_lookup_table_0",	ISCAP,	COLOR_LOOKUP_TABLE_0},
	{ "color_lookup_table_1",	ISCAP,	COLOR_LOOKUP_TABLE_1},
	{ "color_lookup_table_2",	ISCAP,	COLOR_LOOKUP_TABLE_2},
	{ "color_lookup_table_3",	ISCAP,	COLOR_LOOKUP_TABLE_3},
	{ "color_lookup_table_4",	ISCAP,	COLOR_LOOKUP_TABLE_4},
	{ "color_lookup_table_5",	ISCAP,	COLOR_LOOKUP_TABLE_5},
	{ "color_lookup_table_6",	ISCAP,	COLOR_LOOKUP_TABLE_6},
	{ "color_lookup_table_7",	ISCAP,	COLOR_LOOKUP_TABLE_7},
	{ "color_lookup_table_8",	ISCAP,	COLOR_LOOKUP_TABLE_8},
	{ "color_lookup_table_9",	ISCAP,	COLOR_LOOKUP_TABLE_9},

	{ "lines", 			ISNUM,	LINES },

/* FUNCTION KEYS */

	{ "key_f0", 			ISKEY,	VKEY_F0 },
	{ "key_f1", 			ISKEY,	VKEY_F1 },
	{ "key_f2", 			ISKEY,	VKEY_F2 },
	{ "key_f3", 			ISKEY,	VKEY_F3 },
	{ "key_f4", 			ISKEY,	VKEY_F4 },
	{ "key_f5", 			ISKEY,	VKEY_F5 },
	{ "key_f6", 			ISKEY,	VKEY_F6 },
	{ "key_f7", 			ISKEY,	VKEY_F7 },
	{ "key_f8", 			ISKEY,	VKEY_F8 },
	{ "key_f9", 			ISKEY,	VKEY_F9 },
	{ "key_f10", 			ISKEY,	VKEY_F10 },
	{ "key_f11", 			ISKEY,	VKEY_F11 },
	{ "key_f12", 			ISKEY,	VKEY_F12 },
	{ "key_f13", 			ISKEY,	VKEY_F13 },
	{ "key_f14", 			ISKEY,	VKEY_F14 },
	{ "key_f15", 			ISKEY,	VKEY_F15 },
	{ "key_f16", 			ISKEY,	VKEY_F16 },
	{ "key_f17", 			ISKEY,	VKEY_F17 },
	{ "key_f18", 			ISKEY,	VKEY_F18 },
	{ "key_f19", 			ISKEY,	VKEY_F19 },
	{ "key_f20", 			ISKEY,	VKEY_F20 },
	{ "key_f21", 			ISKEY,	VKEY_F21 },
	{ "key_f22", 			ISKEY,	VKEY_F22 },
	{ "key_f23", 			ISKEY,	VKEY_F23 },
	{ "key_f24", 			ISKEY,	VKEY_F24 },
	{ "key_f25", 			ISKEY,	VKEY_F25 },
	{ "key_f26", 			ISKEY,	VKEY_F26 },
	{ "key_f27", 			ISKEY,	VKEY_F27 },
	{ "key_f28", 			ISKEY,	VKEY_F28 },
	{ "key_f29", 			ISKEY,	VKEY_F29 },
	{ "key_f30", 			ISKEY,	VKEY_F30 },
	{ "key_f31", 			ISKEY,	VKEY_F31 },
	{ "key_f32", 			ISKEY,	VKEY_F32 },
	{ "key_f33", 			ISKEY,	VKEY_F33 },
	{ "key_f34", 			ISKEY,	VKEY_F34 },
	{ "key_f35", 			ISKEY,	VKEY_F35 },
	{ "key_f36", 			ISKEY,	VKEY_F36 },
	{ "key_f37", 			ISKEY,	VKEY_F37 },
	{ "key_f38", 			ISKEY,	VKEY_F38 },
	{ "key_f39", 			ISKEY,	VKEY_F39 },
	{ "key_f40", 			ISKEY,	VKEY_F40 },
	{ "key_f41", 			ISKEY,	VKEY_F41 },
	{ "key_f42", 			ISKEY,	VKEY_F42 },
	{ "key_f43", 			ISKEY,	VKEY_F43 },
	{ "key_f44", 			ISKEY,	VKEY_F44 },
	{ "key_f45", 			ISKEY,	VKEY_F45 },
	{ "key_f46", 			ISKEY,	VKEY_F46 },
	{ "key_f47", 			ISKEY,	VKEY_F47 },
	{ "key_f48", 			ISKEY,	VKEY_F48 },
	{ "key_f49", 			ISKEY,	VKEY_F49 },
	{ "key_f50", 			ISKEY,	VKEY_F50 },
	{ "key_f51", 			ISKEY,	VKEY_F51 },
	{ "key_f52", 			ISKEY,	VKEY_F52 },
	{ "key_f53", 			ISKEY,	VKEY_F53 },
	{ "key_f54", 			ISKEY,	VKEY_F54 },
	{ "key_f55", 			ISKEY,	VKEY_F55 },
	{ "key_f56", 			ISKEY,	VKEY_F56 },
	{ "key_f57", 			ISKEY,	VKEY_F57 },
	{ "key_f58", 			ISKEY,	VKEY_F58 },
	{ "key_f59", 			ISKEY,	VKEY_F59 },
	{ "key_f60", 			ISKEY,	VKEY_F60 },
	{ "key_f61", 			ISKEY,	VKEY_F61 },
	{ "key_f62", 			ISKEY,	VKEY_F62 },
	{ "key_f63", 			ISKEY,	VKEY_F63 },
	{ "key_f64", 			ISKEY,	VKEY_F64 },
	{ "key_home", 			ISKEY,	VKEY_HOME },
	{ "key_backspace", 		ISKEY,	VKEY_BACKSPACE },
	{ "key_down_arrow", 		ISKEY,	VKEY_DOWN_ARROW },
	{ "key_left_arrow", 		ISKEY,	VKEY_LEFT_ARROW },
	{ "key_right_arrow", 		ISKEY,	VKEY_RIGHT_ARROW },
	{ "key_up_arrow", 		ISKEY,	VKEY_UP_ARROW },
	{ "key_delete", 		ISKEY,	VKEY_DELETE },
	{ "key_insert", 		ISKEY,	VKEY_INSERT },
	{ "key_next_scr", 		ISKEY,	VKEY_NEXT_SCR },
	{ "key_prev_scr", 		ISKEY,	VKEY_PREV_SCR },
	{ "key_select", 		ISKEY,	VKEY_SELECT },
	{ "key_tab", 			ISKEY,	VKEY_TAB },
	{ "key_help", 			ISKEY,	VKEY_HELP },
	{ "key_do", 			ISKEY,	VKEY_DO },
	{ "key_find", 			ISKEY,	VKEY_FIND },
	{ "key_user1", 			ISKEY,	VKEY_USER1 },
	{ "key_user2", 			ISKEY,	VKEY_USER2 },
	{ "key_user3", 			ISKEY,	VKEY_USER3 },
	{ "key_user4", 			ISKEY,	VKEY_USER4 },
	{ "key_user5", 			ISKEY,	VKEY_USER5 },
	{ "key_user6", 			ISKEY,	VKEY_USER6 },
	{ "key_user7", 			ISKEY,	VKEY_USER7 },
	{ "key_user8", 			ISKEY,	VKEY_USER8 },
        { "generic_pf1", 		ISKEY,	GENERIC_PF1 },
	{ "generic_pf2",       		ISKEY,	GENERIC_PF2 },
	{ "generic_pf3",       		ISKEY,	GENERIC_PF3 },
	{ "generic_pf4",       		ISKEY,	GENERIC_PF4 },
	{ "generic_pf5",       		ISKEY,	GENERIC_PF5 },
	{ "generic_pf6",       		ISKEY,	GENERIC_PF6 },
	{ "generic_pf7",       		ISKEY,	GENERIC_PF7 },
	{ "generic_pf8",       		ISKEY,	GENERIC_PF8 },
	{ "generic_pf9",       		ISKEY,	GENERIC_PF9 },
	{ "generic_pf10",      		ISKEY,	GENERIC_PF10 },
	{ "generic_pf11",      		ISKEY,	GENERIC_PF11 },
	{ "generic_pf12",      		ISKEY,	GENERIC_PF12 },
	{ "generic_pf13",      		ISKEY,	GENERIC_PF13 },
	{ "generic_pf14",      		ISKEY,	GENERIC_PF14 },
	{ "generic_pf15",      		ISKEY,	GENERIC_PF15 },
	{ "generic_pf16",      		ISKEY,	GENERIC_PF16 },
	{ "generic_pf17",      		ISKEY,	GENERIC_PF17 },
	{ "generic_pf18",      		ISKEY,	GENERIC_PF18 },
	{ "generic_pf19",      		ISKEY,	GENERIC_PF19 },
	{ "generic_pf20",      		ISKEY,	GENERIC_PF20 },
	{ "generic_pf21",      		ISKEY,	GENERIC_PF21 },
	{ "generic_pf22",      		ISKEY,	GENERIC_PF22 },
	{ "generic_pf23",      		ISKEY,	GENERIC_PF23 },
	{ "generic_pf24",      		ISKEY,	GENERIC_PF24 },
	{ "generic_pf25",      		ISKEY,	GENERIC_PF25 },
	{ "generic_pf26",      		ISKEY,	GENERIC_PF26 },
	{ "generic_pf27",      		ISKEY,	GENERIC_PF27 },
	{ "generic_pf28",      		ISKEY,	GENERIC_PF28 },
	{ "generic_pf29",      		ISKEY,	GENERIC_PF29 },
	{ "generic_pf30",      		ISKEY,	GENERIC_PF30 },
	{ "generic_pf31",      		ISKEY,	GENERIC_PF31 },
	{ "generic_pf32",      		ISKEY,	GENERIC_PF32 },
	{ "generic_pf33",      		ISKEY,	GENERIC_PF33 },
	{ "generic_pf34",      		ISKEY,	GENERIC_PF34 },
	{ "generic_pf35",      		ISKEY,	GENERIC_PF35 },
	{ "generic_pf36",      		ISKEY,	GENERIC_PF36 },
	{ "generic_pf37",      		ISKEY,	GENERIC_PF37 },
	{ "generic_pf38",      		ISKEY,	GENERIC_PF38 },
	{ "generic_pf39",      		ISKEY,	GENERIC_PF39 },
	{ "generic_pf40",      		ISKEY,	GENERIC_PF40 },
	{ "generic_pf41",      		ISKEY,	GENERIC_PF41 },
	{ "generic_pf42",      		ISKEY,	GENERIC_PF42 },
	{ "generic_pf43",      		ISKEY,	GENERIC_PF43 },
	{ "generic_pf44",      		ISKEY,	GENERIC_PF44 },
	{ "generic_pf45",      		ISKEY,	GENERIC_PF45 },
	{ "generic_pf46",      		ISKEY,	GENERIC_PF46 },
	{ "generic_pf47",      		ISKEY,	GENERIC_PF47 },
	{ "generic_pf48",      		ISKEY,	GENERIC_PF48 },
	{ "generic_pf49",      		ISKEY,	GENERIC_PF49 },
	{ "generic_pf50",      		ISKEY,	GENERIC_PF50 },
	{ "generic_pf51",      		ISKEY,	GENERIC_PF51 },
	{ "generic_pf52",      		ISKEY,	GENERIC_PF52 },
	{ "generic_pf53",      		ISKEY,	GENERIC_PF53 },
	{ "generic_pf54",      		ISKEY,	GENERIC_PF54 },
	{ "generic_pf55",      		ISKEY,	GENERIC_PF55 },
	{ "generic_pf56",      		ISKEY,	GENERIC_PF56 },
	{ "generic_pf57",      		ISKEY,	GENERIC_PF57 },
	{ "generic_pf58",      		ISKEY,	GENERIC_PF58 },
	{ "generic_pf59",      		ISKEY,	GENERIC_PF59 },
	{ "generic_pf60",      		ISKEY,	GENERIC_PF60 },
	{ "generic_pf61",      		ISKEY,	GENERIC_PF61 },
	{ "generic_pf62",      		ISKEY,	GENERIC_PF62 },
	{ "generic_pf63",      		ISKEY,	GENERIC_PF63 },
	{ "generic_pf64",      		ISKEY,	GENERIC_PF64 },
	{ "generic_home",      		ISKEY,	GENERIC_HOME },
	{ "generic_help",      		ISKEY,	GENERIC_HELP },
	{ "generic_up",        		ISKEY,	GENERIC_UP },
	{ "generic_down",      		ISKEY,	GENERIC_DOWN },
	{ "generic_left",      		ISKEY,	GENERIC_LEFT },
	{ "generic_right",  	   	ISKEY,	GENERIC_RIGHT },
	{ "generic_tab",       		ISKEY,	GENERIC_TAB },
	{ "generic_backtab",   		ISKEY,	GENERIC_BACKTAB },
	{ "generic_delete",    		ISKEY,	GENERIC_DELETE },
	{ "generic_insert",    		ISKEY,	GENERIC_INSERT },
	{ "generic_remove",    		ISKEY,	GENERIC_REMOVE },
	{ "generic_next_scr",	  	ISKEY,	GENERIC_NEXT_SCR },
	{ "generic_prev_scr",  		ISKEY,	GENERIC_PREV_SCR },
	{ "generic_select",  		ISKEY,	GENERIC_SELECT },
	{ "generic_cancel",  		ISKEY,	GENERIC_CANCEL },
	{ "generic_enter",  		ISKEY,	GENERIC_ENTER },
	{ "generic_return",  		ISKEY,	GENERIC_RETURN },
	{ "generic_refresh",  		ISKEY,	GENERIC_REFRESH },
	{ "generic_clear_field",	ISKEY,	GENERIC_CLEAR_FIELD },
	{ "generic_clear_after",	ISKEY,	GENERIC_CLEAR_AFTER },
	{ "generic_clear_before",  	ISKEY,	GENERIC_CLEAR_BEFORE },
	{ "generic_null",  	        ISKEY,	GENERIC_NULL },
	{ "generic_newline",	        ISKEY,	GENERIC_NEWLINE },
        { "generic_mark",	        ISKEY,	GENERIC_MARK },
	{ "generic_cut",  	        ISKEY,	GENERIC_CUT },
	{ "generic_copy",  	        ISKEY,	GENERIC_COPY },
	{ "generic_paste",	        ISKEY,	GENERIC_PASTE },
	{ "generic_mouse",	        ISKEY,	GENERIC_MOUSE },
	{ "trigger1",  	        	ISKEY,	TRIGGER1 },
	{ "trigger2",  	        	ISKEY,	TRIGGER2 },
	{ "trigger3",  	        	ISKEY,	TRIGGER3 },
	{ "trigger4",  	        	ISKEY,	TRIGGER4 },
	{ "trigger5",  	        	ISKEY,	TRIGGER5 },
	{ "trigger6",  	        	ISKEY,	TRIGGER6 },
	{ "trigger7",  	        	ISKEY,	TRIGGER7 },
	{ "trigger8",  	        	ISKEY,	TRIGGER8 },
	{ "shift_F1",			ISKEY,	SHIFT_F1 },
	{ "shift_F2",			ISKEY,	SHIFT_F2 },
	{ "shift_F3",			ISKEY,	SHIFT_F3 },
	{ "shift_F4",			ISKEY,	SHIFT_F4 },
	{ "shift_F5",			ISKEY,	SHIFT_F5 },
	{ "shift_F6",			ISKEY,	SHIFT_F6 },
	{ "shift_F7",			ISKEY,	SHIFT_F7 },
	{ "shift_F8",			ISKEY,	SHIFT_F8 },
	{ "shift_F9",			ISKEY,	SHIFT_F9 },
	{ "shift_F10",			ISKEY,	SHIFT_F10 },
	{ "shift_F11",			ISKEY,	SHIFT_F11 },
	{ "shift_F12",			ISKEY,	SHIFT_F12 },
	{ "ctrl_F1",			ISKEY,	CTRL_F1 },
	{ "ctrl_F2",			ISKEY,	CTRL_F2 },
	{ "ctrl_F3",			ISKEY,	CTRL_F3 },
	{ "ctrl_F4",			ISKEY,	CTRL_F4 },
	{ "ctrl_F5",			ISKEY,	CTRL_F5 },
	{ "ctrl_F6",			ISKEY,	CTRL_F6 },
	{ "ctrl_F7",			ISKEY,	CTRL_F7 },
	{ "ctrl_F8",			ISKEY,	CTRL_F8 },
	{ "ctrl_F9",			ISKEY,	CTRL_F9 },
	{ "ctrl_F10",			ISKEY,	CTRL_F10 },
	{ "ctrl_F11",			ISKEY,	CTRL_F11 },
	{ "ctrl_F12",			ISKEY,	CTRL_F12 },
	{ "alt_F1",			ISKEY,	ALT_F1 },
	{ "alt_F2",			ISKEY,	ALT_F2 },
	{ "alt_F3",			ISKEY,	ALT_F3 },
	{ "alt_F4",			ISKEY,	ALT_F4 },
	{ "alt_F5",			ISKEY,	ALT_F5 },
	{ "alt_F6",			ISKEY,	ALT_F6 },
	{ "alt_F7",			ISKEY,	ALT_F7 },
	{ "alt_F8",			ISKEY,	ALT_F8 },
	{ "alt_F9",			ISKEY,	ALT_F9 },
	{ "alt_F10",			ISKEY,	ALT_F10 },
	{ "alt_F11",			ISKEY,	ALT_F11 },
	{ "alt_F12",			ISKEY,	ALT_F12 },
/*
	The following are not used.
*/
	{ "tab", 			ISCAP,	TAB },
	{ "enter_insert_mode", 		ISCAP,	ENTER_INSERT_MODE },
	{ "exit_insert_mode", 		ISCAP,	EXIT_INSERT_MODE },
	{ "init_file", 			ISCAP,	INIT_FILE },
	{ "enter_kp_xmit", 		ISCAP,	ENTER_KP_XMIT },
	{ "exit_kp_xmit", 		ISCAP,	EXIT_KP_XMIT },
	{ "virtual_term_num", 		ISCAP,	VIRTUAL_TERM_NUM },
	{ "save_cursor", 		ISCAP,	SAVE_CURSOR },
	{ "restore_cursor", 		ISCAP,	RESTORE_CURSOR },
	{ "bell", 			ISCAP,	BELL },
	{ "back_tab", 			ISCAP,	BACK_TAB },
	{ "carriage_return", 		ISCAP,	CARRIAGE_RETURN },
	{ "clear_all_tabs", 		ISCAP,	CLEAR_ALL_TABS },
	{ "cursor_home", 		ISCAP,	CURSOR_HOME },
	{ "cursor_normal", 		ISCAP,	CURSOR_NORMAL },
	{ "delete_char", 		ISCAP,	DELETE_CHAR },
	{ "delete_line", 		ISCAP,	DELETE_LINE },
	{ "enter_alt_charset", 		ISCAP,	ENTER_ALT_CHARSET },
	{ "enter_am_mode", 		ISCAP,	ENTER_AM_MODE },
	{ "enter_delete_mode", 		ISCAP,	ENTER_DELETE_MODE },
	{ "enter_dim_mode", 		ISCAP,	ENTER_DIM_MODE },
	{ "enter_protected_mode",	ISCAP,	ENTER_PROTECTED_MODE },
	{ "enter_secure_mode", 		ISCAP,	ENTER_SECURE_MODE },
	{ "enter_standout_mode",	ISCAP,	ENTER_STANDOUT_MODE },
	{ "exit_alt_charset", 		ISCAP,	EXIT_ALT_CHARSET },
	{ "exit_am_mode", 		ISCAP,	EXIT_AM_MODE },
	{ "exit_delete_mode", 		ISCAP,	EXIT_DELETE_MODE },
	{ "exit_standout_mode", 	ISCAP,	EXIT_STANDOUT_MODE },
	{ "exit_underline_mode",	ISCAP,	EXIT_UNDERLINE_MODE },
        { "graphstr",                   ISCAP,	GRAPHSTR },
	{ "insert_char", 		ISCAP,	INSERT_CHAR },
	{ "insert_line", 		ISCAP,	INSERT_LINE },
	{ "other_keys", 		ISCAP,	OTHER_KEYS },
	{ "newline", 			ISCAP,	NEWLINE },

	{ "columns", 			ISNUM,	COLUMNS },
	{ "so_blanks", 			ISNUM,	SO_BLANKS },
	{ "us_blanks", 			ISNUM,	US_BLANKS },

	{ (char *)0, 0, 0 }
};

/*
	CAPABILITIES structure.

	The enteries are POSITION DEPENDENT based on the define.
*/

/*
**	The default CAPABLITIES are VT100/VT220
*/
static char *vcapdef[VC_CAP_COUNT+1] = 
{
	"",
	"",							     /* BACKSPACE             */
	"",							     /* AUTO_MARGINS          */
	"",							     /* HAS_HW_TABS           */
	"",							     /* NL_IGN_AFTER_WRAP     */
	"",							     /* MOVE_STANDOUT_MODE    */
	"",							     /* INIT_FILE             */
	"\033>\033[?4l\033[?7h\033[?8h\033[?25h",		     /* RESET_TERMINAL        */
	"",							     /* ENTER_KP_XMIT         */
	"",							     /* EXIT_KP_XMIT          */ 
	"\011",							     /* TAB                   */
	"",							     /* VIRTUAL_TERM_NUM      */
	"\0337",						     /* SAVE_CURSOR           */
	"\0338",						     /* RESTORE_CURSOR        */
	"\007",							     /* BELL                  */
	"",							     /* BACK_TAB              */
	"\015",							     /* CARRIAGE_RETURN       */
	"\033[%i%d;%dr",					     /* CHANGE_SCROLL_REGION  */
	"",							     /* CLEAR_ALL_TABS        */
	"\033[ ; H\033[2J",					     /* CLR_SCREEN            */
	"",							     /* CLEAR_BOL             */
	"\033[K",						     /* CLEAR_EOL             */
	"\033[J",						     /* CLEAR_EOS             */
	"\033[%i%d;%dH",					     /* CURSOR_ADDRESS        */
	"\012",							     /* CURSOR_DOWN           */
	"\033[H",						     /* CURSOR_HOME           */
	"\033[?25l",						     /* CURSOR_INVISIBLE      */
	"\033[?25h",						     /* CURSOR_VISIBLE        */
	"\010",							     /* CURSOR_LEFT           */
	"",							     /* CURSOR_NORMAL         */
	"\033[C",						     /* CURSOR_RIGHT          */
	"\033[A",						     /* CURSOR_UP             */
	"",							     /* DELETE_CHAR           */
	"",							     /* DELETE_LINE           */
	"",							     /* ENTER_ALT_CHARSET     */
	"",							     /* ENTER_AM_MODE         */
	"\033[5m",						     /* ENTER_BLINK_MODE      */
	"\033[1m",						     /* ENTER_BOLD_MODE       */
	"",							     /* ENTER_DELETE_MODE     */
	"",							     /* ENTER_DIM_MODE        */
	"",							     /* ENTER_INSERT_MODE     */
	"",							     /* ENTER_PROTECTED_MODE  */
	"\033[7m",						     /* ENTER_REVERSE_MODE    */
	"",							     /* ENTER_SECURE_MODE     */
	"\033[7m",						     /* ENTER_STANDOUT_MODE   */
	"\033[4m",						     /* ENTER_UNDERLINE_MODE  */
	"",							     /* EXIT_ALT_CHARSET      */
	"",							     /* EXIT_AM_MODE          */
	"\033[m",						     /* EXIT_ATTRIBUTE_MODE   */
	"",							     /* EXIT_DELETE_MODE      */
	"",							     /* EXIT_INSERT_MODE      */
	"",							     /* EXIT_STANDOUT_MODE    */
	"",							     /* EXIT_UNDERLINE_MODE   */
	"\033[1;24r\033[?7l\033[?20l\033<",			     /* INIT_TERMINAL         */
	"",							     /* INSERT_CHAR           */
	"",							     /* INSERT_LINE           */
	"",							     /* OTHER_KEYS            */
	"\n",							     /* NEWLINE               */
	"\033M",						     /* SCROLL_REVERSE        */
	"",							     /* COLUMNS               */
	"\000\000\000\000",					     /* LINES (default 0=24)  */
	"",							     /* SO_BLANKS             */
	"",							     /* US_BLANKS             */
#ifdef WIN32
	"",							     /* GRAPHSTR              */
#else
	"xqlkmjwvtun\253\273",					     /* GRAPHSTR              */
#endif
	"\033(0",						     /* ENTER_GRAPHICS_MODE   */
	"\033(B",						     /* EXIT_GRAPHICS_MODE    */
	"",							     /* PAD                   */
#ifdef WIN32
	"",							     /* PSEUDO_BLANKS         */
#else
	"`f~a",							     /* PSEUDO_BLANKS         */
#endif
	"",							     /* INDEX                 */
	"\033M",						     /* REVERSE_INDEX         */
        "\033[?3h",						     /* WIDE_MODE	      */
	"\033[?3l",						     /* NARROW_MODE           */
	"\033[?5l",						     /* SCREEN_NORMAL_MODE    */
	"\033[?5h",						     /* SCREEN_REVERSE_MODE   */
	"0123456789ABCDEFFEDCBA9876543210",			     /* COLOR_LOOKUP_TABLE_0  */
	"0123456789ABCDEFFEDCBA9876543210",			     /* COLOR_LOOKUP_TABLE_1  */
	"0123456789ABCDEFFEDCBA9876543210",			     /* COLOR_LOOKUP_TABLE_2  */
	"0123456789ABCDEFFEDCBA9876543210",			     /* COLOR_LOOKUP_TABLE_3  */
	"0123456789ABCDEFFEDCBA9876543210",			     /* COLOR_LOOKUP_TABLE_4  */
	"0123456789ABCDEFFEDCBA9876543210",			     /* COLOR_LOOKUP_TABLE_5  */
	"0123456789ABCDEFFEDCBA9876543210",			     /* COLOR_LOOKUP_TABLE_6  */
	"0123456789ABCDEFFEDCBA9876543210",			     /* COLOR_LOOKUP_TABLE_7  */
	"0123456789ABCDEFFEDCBA9876543210",			     /* COLOR_LOOKUP_TABLE_8  */
	"0123456789ABCDEFFEDCBA9876543210"			     /* COLOR_LOOKUP_TABLE_9  */
};


struct keylist
{
	int index;
	char *value;
};
static struct keylist stdkeys[] =
{
      {GENERIC_PF1  ,"\00601"},
      {GENERIC_PF2  ,"\00602"},
      {GENERIC_PF3  ,"\00603"},
      {GENERIC_PF4  ,"\00604"},
      {GENERIC_PF5  ,"\00605"},
      {GENERIC_PF6  ,"\00606"},
      {GENERIC_PF7  ,"\00607"},
      {GENERIC_PF8  ,"\00608"},
      {GENERIC_PF9  ,"\00609"},
      {GENERIC_PF10 ,"\00610"},
      {GENERIC_PF11 ,"\00611"},
      {GENERIC_PF12 ,"\00612"},
      {GENERIC_PF13 ,"\00613"},
      {GENERIC_PF14 ,"\00614"},
      {GENERIC_PF15 ,"\00615"},
      {GENERIC_PF16 ,"\00616"},
      {GENERIC_PF17 ,"\00617"},
      {GENERIC_PF18 ,"\00618"},
      {GENERIC_PF19 ,"\00619"},
      {GENERIC_PF20 ,"\00620"},
      {GENERIC_PF21 ,"\00621"},
      {GENERIC_PF22 ,"\00622"},
      {GENERIC_PF23 ,"\00623"},
      {GENERIC_PF24 ,"\00624"},
      {GENERIC_PF25 ,"\00625"},
      {GENERIC_PF26 ,"\00626"},
      {GENERIC_PF27 ,"\00627"},
      {GENERIC_PF28 ,"\00628"},
      {GENERIC_PF29 ,"\00629"},
      {GENERIC_PF30 ,"\00630"},
      {GENERIC_PF31 ,"\00631"},
      {GENERIC_PF32 ,"\00632"},
      {GENERIC_UP   ,"\006u"},
      {GENERIC_DOWN ,"\006d"},
      {GENERIC_LEFT ,"\006l"},
      {GENERIC_RIGHT,"\006r"},
      {GENERIC_UP   ,"\006U"},
      {GENERIC_DOWN ,"\006D"},
      {GENERIC_LEFT ,"\006L"},
      {GENERIC_RIGHT,"\006R"},
      {GENERIC_UP   ,"\006\014"},
      {GENERIC_DOWN ,"\006\012"},
      {GENERIC_LEFT ,"\006\010"},
      {GENERIC_RIGHT,"\006\013"},
      {GENERIC_SELECT,"\006s"},
      {GENERIC_SELECT,"\006S"},
      {GENERIC_HELP  ,"\006?"},
      {GENERIC_HELP  ,"\005"},
      {GENERIC_HOME  ,"\006h"},
      {GENERIC_HOME  ,"\006H"}, 
      {GENERIC_TAB   ,"\011"},  
      {GENERIC_BACKTAB ,"\006\011"},
      {GENERIC_BACKTAB ,"\006!"},
      {GENERIC_DELETE ,"\006x"},
      {GENERIC_INSERT ,"\006i"},
      {GENERIC_DELETE ,"\006X"},
      {GENERIC_INSERT ,"\006I"},
      {GENERIC_PREV_SCR ,"\006\020"},
      {GENERIC_NEXT_SCR ,"\006\016"},
      {GENERIC_CANCEL ,"\006c"},
      {GENERIC_ENTER ,"\012"},
      {GENERIC_ENTER ,"\006e"},
      {GENERIC_ENTER ,"\006E"},
      {GENERIC_RETURN ,"\015"},
      {GENERIC_RETURN ,"\006m"},
      {GENERIC_RETURN ,"\006M"},
      {GENERIC_REFRESH ,"\027"},
      {GENERIC_REFRESH ,"\022"},
      {GENERIC_NEWLINE ,"\006N"},
      {GENERIC_NEWLINE ,"\006n"},
      {GENERIC_MOUSE   ,DEF_GENERIC_MOUSE},
      {0,0} 
};

int vcap_padding;

/*
	FUNCTION KEYS structure.

	The enteries are POSITION DEPENDENT based on the define.

	The first column is the key-stroke-sequence.
	If the second column is NULL then the third column tells what value to return for this key-stroke.
	If not NULL the second column is a redirection to a different "row" in the table.  It will continue
	following second column redirections until it finds a NULL, it will then use that row's thrid column value.

	If there is an entry in the second column then the third column is not used.

        9/16-JEC-The fourth column is the source item. It indicates the source of the key;
                 it is used during load to determine when to override an existing definition (ie,
                 videocap overrides terminfo)

	NOTE: If the second column is not NULL then the third column is ignored.

*/

#define VKEY struct vkey_s

VKEY
{
	char 	*value;
	VKEY 	*eval_to;
	int 	id;
	int     source;
};

static VKEY vkeydef[VC_KEY_LAST+VC_KEY_EXTRA];
static VKEY vt220def[] =
{
	"", NULL, 0,0,		/* element 0 in array not used */
	"", NULL, 0,0,		/* PF0 not used */
	"\033OP",          &vkeydef[GENERIC_PF1 ],	GENERIC_PF1, /* KEY_F1   */ SRC_EMPTY,
	"\033OQ",          &vkeydef[GENERIC_PF2 ],	GENERIC_PF2, /* KEY_F2   */ SRC_EMPTY,
	"\033OR",          &vkeydef[GENERIC_PF3 ],	GENERIC_PF3, /* KEY_F3   */ SRC_EMPTY,
	"\033OS",          &vkeydef[GENERIC_PF4 ],      GENERIC_PF4, /* KEY_F4   */ SRC_EMPTY,
	"\033[18~",        &vkeydef[GENERIC_PF5 ],	GENERIC_PF5, /* KEY_F5   */ SRC_EMPTY,
	"\033[19~",        &vkeydef[GENERIC_PF6 ],	GENERIC_PF6, /* KEY_F6   */ SRC_EMPTY,
	"\033[20~",        &vkeydef[GENERIC_PF7 ],	GENERIC_PF7, /* KEY_F7   */ SRC_EMPTY,
	"\033[21~",        &vkeydef[GENERIC_PF8 ],	GENERIC_PF8, /* KEY_F8   */ SRC_EMPTY,
	"\033[23~",        &vkeydef[GENERIC_PF9 ],	GENERIC_PF9, /* KEY_F9   */ SRC_EMPTY,
	"\033[24~",        &vkeydef[GENERIC_PF10],	GENERIC_PF10, /* KEY_F10  */ SRC_EMPTY,
	"\033[25~",        &vkeydef[GENERIC_PF11],	GENERIC_PF11, /* KEY_F11  */ SRC_EMPTY,
	"\033[26~",        &vkeydef[GENERIC_PF12],	GENERIC_PF12, /* KEY_F12  */ SRC_EMPTY,
	"\033[31~",        &vkeydef[GENERIC_PF13],	GENERIC_PF13, /* KEY_F13  */ SRC_EMPTY,
	"\033[32~",        &vkeydef[GENERIC_PF14],	GENERIC_PF14, /* KEY_F14  */ SRC_EMPTY,
	"\033[33~",        &vkeydef[GENERIC_PF15],	GENERIC_PF15, /* KEY_F15  */ SRC_EMPTY,
	"\033[34~",        &vkeydef[GENERIC_PF16],	GENERIC_PF16, /* KEY_F16  */ SRC_EMPTY,
	"\033[4~\033OP",   &vkeydef[GENERIC_PF17],	GENERIC_PF17, /* KEY_F17  */ SRC_EMPTY,
	"\033[4~\033OQ",   &vkeydef[GENERIC_PF18],	GENERIC_PF18, /* KEY_F18  */ SRC_EMPTY,
	"\033[4~\033OR",   &vkeydef[GENERIC_PF19],	GENERIC_PF19, /* KEY_F19  */ SRC_EMPTY,
	"\033[4~\033OS",   &vkeydef[GENERIC_PF20],	GENERIC_PF20, /* KEY_F20  */ SRC_EMPTY,
	"\033[4~\033[18~", &vkeydef[GENERIC_PF21],	GENERIC_PF21, /* KEY_F21  */ SRC_EMPTY,
	"\033[4~\033[19~", &vkeydef[GENERIC_PF22],	GENERIC_PF22, /* KEY_F22  */ SRC_EMPTY,
	"\033[4~\033[20~", &vkeydef[GENERIC_PF23],	GENERIC_PF23, /* KEY_F23  */ SRC_EMPTY,
	"\033[4~\033[21~", &vkeydef[GENERIC_PF24],	GENERIC_PF24, /* KEY_F24  */ SRC_EMPTY,
	"\033[4~\033[23~", &vkeydef[GENERIC_PF25],	GENERIC_PF25, /* KEY_F25  */ SRC_EMPTY,
	"\033[4~\033[24~", &vkeydef[GENERIC_PF26],	GENERIC_PF26, /* KEY_F26  */ SRC_EMPTY,
	"\033[4~\033[25~", &vkeydef[GENERIC_PF27],	GENERIC_PF27, /* KEY_F27  */ SRC_EMPTY,
	"\033[4~\033[26~", &vkeydef[GENERIC_PF28],	GENERIC_PF28, /* KEY_F28  */ SRC_EMPTY,
	"\033[4~\033[31~", &vkeydef[GENERIC_PF29],	GENERIC_PF29, /* KEY_F29  */ SRC_EMPTY,
	"\033[4~\033[32~", &vkeydef[GENERIC_PF30],	GENERIC_PF30, /* KEY_F30  */ SRC_EMPTY,
	"\033[4~\033[33~", &vkeydef[GENERIC_PF31],	GENERIC_PF31, /* KEY_F31  */ SRC_EMPTY,
	"\033[4~\033[34~", &vkeydef[GENERIC_PF32],      GENERIC_PF32, /* KEY_F32  */ SRC_EMPTY,
	"",                NULL,	       		VKEY_HOME,      /* KEY_HOME         */ SRC_EMPTY,
	"\177",		   &vkeydef[GENERIC_DELETE],	VKEY_BACKSPACE, /* KEY_BACKSPACE    */ SRC_EMPTY,
	"\033[B",          &vkeydef[GENERIC_DOWN ],	GENERIC_DOWN, /* KEY_DOWN_ARROW   */ SRC_EMPTY,
	"\033[D",	   &vkeydef[GENERIC_LEFT ],     GENERIC_LEFT, /* KEY_LEFT_ARROW   */ SRC_EMPTY,
	"\033[C",	   &vkeydef[GENERIC_RIGHT],     GENERIC_RIGHT, /*KEY_RIGHT_ARROW   */ SRC_EMPTY,
	"\033[A",	   &vkeydef[GENERIC_UP   ],     GENERIC_UP,   /* KEY_UP_ARROW     */ SRC_EMPTY,
	"\00601",          NULL,                        GENERIC_PF1,  SRC_EMPTY,
	"\00602",          NULL,                        GENERIC_PF2,  SRC_EMPTY,
	"\00603",          NULL,                        GENERIC_PF3,  SRC_EMPTY,
	"\00604",          NULL,                        GENERIC_PF4,  SRC_EMPTY,
	"\00605",          NULL,                        GENERIC_PF5,  SRC_EMPTY,
	"\00606",          NULL,                        GENERIC_PF6,  SRC_EMPTY,
	"\00607",          NULL,                        GENERIC_PF7,  SRC_EMPTY,
	"\00608",          NULL,                        GENERIC_PF8,  SRC_EMPTY,
	"\00609",          NULL,                        GENERIC_PF9,  SRC_EMPTY,
	"\00610",          NULL,                        GENERIC_PF10, SRC_EMPTY,
	"\00611",          NULL,                        GENERIC_PF11, SRC_EMPTY,
	"\00612",          NULL,                        GENERIC_PF12, SRC_EMPTY,
	"\00613",          NULL,                        GENERIC_PF13, SRC_EMPTY,
	"\00614",          NULL,                        GENERIC_PF14, SRC_EMPTY,
	"\00615",          NULL,                        GENERIC_PF15, SRC_EMPTY,
	"\00616",          NULL,                        GENERIC_PF16, SRC_EMPTY,
	"\00617",          NULL,                        GENERIC_PF17, SRC_EMPTY,
	"\00618",          NULL,                        GENERIC_PF18, SRC_EMPTY,
	"\00619",          NULL,                        GENERIC_PF19, SRC_EMPTY,
	"\00620",          NULL,                        GENERIC_PF20, SRC_EMPTY,
	"\00621",          NULL,                        GENERIC_PF21, SRC_EMPTY,
	"\00622",          NULL,                        GENERIC_PF22, SRC_EMPTY,
	"\00623",          NULL,                        GENERIC_PF23, SRC_EMPTY,
	"\00624",          NULL,                        GENERIC_PF24, SRC_EMPTY,
	"\00625",          NULL,                        GENERIC_PF25, SRC_EMPTY,
	"\00626",          NULL,                        GENERIC_PF26, SRC_EMPTY,
	"\00627",          NULL,                        GENERIC_PF27, SRC_EMPTY,
	"\00628",          NULL,                        GENERIC_PF28, SRC_EMPTY,
	"\00629",          NULL,                        GENERIC_PF29, SRC_EMPTY,
	"\00630",          NULL,                        GENERIC_PF30, SRC_EMPTY,
	"\00631",          NULL,                        GENERIC_PF31, SRC_EMPTY,
	"\00632",          NULL,                        GENERIC_PF32, SRC_EMPTY,
	"\006H",           NULL,                        GENERIC_HOME, SRC_EMPTY,
	"\006?",	   NULL,    			GENERIC_HELP, SRC_EMPTY,
	"\006U",	   NULL,     			GENERIC_UP, SRC_EMPTY,
	"\006D",	   NULL,     			GENERIC_DOWN, SRC_EMPTY,
        "\006L",	   NULL,     			GENERIC_LEFT, SRC_EMPTY,
        "\006R",	   NULL,     			GENERIC_RIGHT, SRC_EMPTY,
        "",	  	   NULL,     			GENERIC_TAB, SRC_EMPTY,
        "\006X",	   NULL,     			GENERIC_DELETE, SRC_EMPTY,
	"\006I",	   NULL,     			GENERIC_INSERT, SRC_EMPTY,
	"\006\016",	   NULL,     			GENERIC_NEXT_SCR, SRC_EMPTY,
	"\006\020",        NULL,                        GENERIC_PREV_SCR, SRC_EMPTY,
	"\033[3~",	   &vkeydef[GENERIC_REMOVE],	VKEY_DELETE, SRC_EMPTY,
	"\033[2~",	   &vkeydef[GENERIC_INSERT],	VKEY_INSERT, SRC_EMPTY,
	"",		   &vkeydef[GENERIC_NEXT_SCR], 	VKEY_NEXT_SCR, SRC_EMPTY,
	"",		   &vkeydef[GENERIC_PREV_SCR],	VKEY_PREV_SCR, SRC_EMPTY,
	"",		   &vkeydef[GENERIC_SELECT],	VKEY_SELECT, SRC_EMPTY,
	"\006\011", 	   NULL,     			GENERIC_BACKTAB, SRC_EMPTY,
	"",		   NULL,     			GENERIC_REMOVE, SRC_EMPTY,
	"\006S",	   NULL,     			GENERIC_SELECT, SRC_EMPTY,
	"\006\003",	   NULL,		        GENERIC_CANCEL, SRC_EMPTY,
	"\015",		   NULL,     			GENERIC_RETURN, SRC_EMPTY,
        "\012",            NULL,                        GENERIC_ENTER, SRC_EMPTY,
	"\030",		   NULL,     			GENERIC_REFRESH, SRC_EMPTY,
	"\011",		   &vkeydef[GENERIC_TAB],	VKEY_TAB, SRC_EMPTY,
	"\033[28~",	   &vkeydef[GENERIC_HELP],	VKEY_HELP, SRC_EMPTY,
	"\033[29~",	   &vkeydef[GENERIC_PF16],	VKEY_DO, SRC_EMPTY,
	"\033[1~",	   &vkeydef[GENERIC_HOME],	VKEY_FIND, SRC_EMPTY,
	"",		   NULL,			VKEY_USER1, SRC_EMPTY,
	"",		   NULL,			VKEY_USER2, SRC_EMPTY,
	"",		   NULL,			VKEY_USER3, SRC_EMPTY,
	"",		   NULL,			VKEY_USER4, SRC_EMPTY,
	"",		   NULL,			VKEY_USER5, SRC_EMPTY,
	"",		   NULL,			VKEY_USER6, SRC_EMPTY,
	"",		   NULL,			VKEY_USER7, SRC_EMPTY,
	"",		   NULL,			VKEY_USER8, SRC_EMPTY,
	"\033[4~\033[2~",  NULL,			GENERIC_CLEAR_FIELD,  SRC_EMPTY,
	"\033[4~\033[3~",  NULL,			GENERIC_CLEAR_AFTER,  SRC_EMPTY,
	"\033[4~\033[1~",  NULL,      			GENERIC_CLEAR_BEFORE, SRC_EMPTY,
	"\033[4~\033[4~",  NULL,      			GENERIC_NULL, SRC_EMPTY,
	"\016",		   NULL,			TRIGGER1, SRC_EMPTY,
	"",		   NULL,			TRIGGER2, SRC_EMPTY,
	"",		   NULL,			TRIGGER3, SRC_EMPTY,
	"",		   NULL,			TRIGGER4, SRC_EMPTY,
	"",		   NULL,			TRIGGER5, SRC_EMPTY,
	"",		   NULL,			TRIGGER6, SRC_EMPTY,
	"",		   NULL,			TRIGGER7, SRC_EMPTY,
	"",		   NULL,			TRIGGER8, SRC_EMPTY,
	"",		   NULL,			VKEY_F33, SRC_EMPTY,
	"",		   NULL,			VKEY_F34, SRC_EMPTY,
	"",		   NULL,			VKEY_F35, SRC_EMPTY,
	"",		   NULL,			VKEY_F36, SRC_EMPTY,
	"",		   NULL,			VKEY_F37 , SRC_EMPTY,
	"",		   NULL,			VKEY_F38, SRC_EMPTY,
	"",		   NULL,			VKEY_F39, SRC_EMPTY,
	"",		   NULL,			VKEY_F40, SRC_EMPTY,
	"",		   NULL,			VKEY_F41 , SRC_EMPTY,
	"",		   NULL,			VKEY_F42 , SRC_EMPTY,
	"",		   NULL,			VKEY_F43, SRC_EMPTY,
	"",		   NULL,			VKEY_F44, SRC_EMPTY,
	"",		   NULL,			VKEY_F45, SRC_EMPTY,
	"",		   NULL,			VKEY_F46, SRC_EMPTY,
	"",		   NULL,			VKEY_F47, SRC_EMPTY,
	"",		   NULL,			VKEY_F48, SRC_EMPTY,
	"",		   NULL,			VKEY_F49, SRC_EMPTY,
	"",		   NULL,			VKEY_F50 , SRC_EMPTY,
	"",		   NULL,			VKEY_F51, SRC_EMPTY,
	"",		   NULL,			VKEY_F52, SRC_EMPTY,
	"",		   NULL,			VKEY_F53 , SRC_EMPTY,
	"",		   NULL,			VKEY_F54, SRC_EMPTY,
	"",		   NULL,			VKEY_F55, SRC_EMPTY,
	"",		   NULL,			VKEY_F56, SRC_EMPTY,
	"",		   NULL,			VKEY_F57, SRC_EMPTY,
	"",		   NULL,			VKEY_F58, SRC_EMPTY,
	"",		   NULL,			VKEY_F59, SRC_EMPTY,
	"",		   NULL,			VKEY_F60, SRC_EMPTY,
	"",		   NULL,			VKEY_F61, SRC_EMPTY,
	"",		   NULL,			VKEY_F62, SRC_EMPTY,
	"",		   NULL,			VKEY_F63, SRC_EMPTY,
	"",		   NULL,			VKEY_F64, SRC_EMPTY,
	"\00633",	   NULL,			GENERIC_PF33, SRC_EMPTY,
	"\00634",	   NULL,			GENERIC_PF34, SRC_EMPTY,
	"\00635",	   NULL,			GENERIC_PF35, SRC_EMPTY,
	"\00636",	   NULL,			GENERIC_PF36, SRC_EMPTY,
	"\00637",	   NULL,			GENERIC_PF37, SRC_EMPTY,
	"\00638",	   NULL,			GENERIC_PF38, SRC_EMPTY,
	"\00639",	   NULL,			GENERIC_PF39, SRC_EMPTY,
	"\00640",	   NULL,			GENERIC_PF40, SRC_EMPTY,
	"\00641",	   NULL,			GENERIC_PF41, SRC_EMPTY,
	"\00642",	   NULL,			GENERIC_PF42, SRC_EMPTY,
	"\00643",	   NULL,			GENERIC_PF43, SRC_EMPTY,
	"\00644",	   NULL,			GENERIC_PF44, SRC_EMPTY,
	"\00645",	   NULL,			GENERIC_PF45, SRC_EMPTY,
	"\00646",	   NULL,			GENERIC_PF46, SRC_EMPTY,
	"\00647",	   NULL,			GENERIC_PF47, SRC_EMPTY,
	"\00648",	   NULL,			GENERIC_PF48, SRC_EMPTY,
	"\00649",	   NULL,			GENERIC_PF49, SRC_EMPTY,
	"\00650",	   NULL,			GENERIC_PF50, SRC_EMPTY,
	"\00651",	   NULL,			GENERIC_PF51, SRC_EMPTY,
	"\00652",	   NULL,			GENERIC_PF52, SRC_EMPTY,
	"\00653",	   NULL,			GENERIC_PF53, SRC_EMPTY,
	"\00654",	   NULL,			GENERIC_PF54, SRC_EMPTY,
	"\00655",	   NULL,			GENERIC_PF55, SRC_EMPTY,
	"\00656",	   NULL,			GENERIC_PF56, SRC_EMPTY,
	"\00657",	   NULL,			GENERIC_PF57, SRC_EMPTY,
	"\00658",	   NULL,			GENERIC_PF58, SRC_EMPTY,
	"\00659",	   NULL,			GENERIC_PF59, SRC_EMPTY,
	"\00660",	   NULL,			GENERIC_PF60, SRC_EMPTY,
	"\00661",	   NULL,			GENERIC_PF61, SRC_EMPTY,
	"\00662",	   NULL,			GENERIC_PF62, SRC_EMPTY,
	"\00663",	   NULL,			GENERIC_PF63, SRC_EMPTY,
	"\00664",	   NULL,			GENERIC_PF64, SRC_EMPTY,
	"\006N",           NULL,                        GENERIC_NEWLINE, SRC_EMPTY,
	"",		   NULL,			SHIFT_F1, SRC_EMPTY,
	"",		   NULL,			SHIFT_F2, SRC_EMPTY,
	"",		   NULL,			SHIFT_F3, SRC_EMPTY,
	"",		   NULL,			SHIFT_F4, SRC_EMPTY,
	"",		   NULL,			SHIFT_F5, SRC_EMPTY,
	"",		   NULL,			SHIFT_F6, SRC_EMPTY,
	"",		   NULL,			SHIFT_F7, SRC_EMPTY,
	"",		   NULL,			SHIFT_F8, SRC_EMPTY,
	"",		   NULL,			SHIFT_F9, SRC_EMPTY,
	"",		   NULL,			SHIFT_F10, SRC_EMPTY,
	"",		   NULL,			SHIFT_F11, SRC_EMPTY,
	"",		   NULL,			SHIFT_F12, SRC_EMPTY,
	"",		   NULL,			CTRL_F1, SRC_EMPTY,
	"",		   NULL,			CTRL_F2, SRC_EMPTY,
	"",		   NULL,			CTRL_F3, SRC_EMPTY,
	"",		   NULL,			CTRL_F4, SRC_EMPTY,
	"",		   NULL,			CTRL_F5, SRC_EMPTY,
	"",		   NULL,			CTRL_F6, SRC_EMPTY,
	"",		   NULL,			CTRL_F7, SRC_EMPTY,
	"",		   NULL,			CTRL_F8, SRC_EMPTY,
	"",		   NULL,			CTRL_F9, SRC_EMPTY,
	"",		   NULL,			CTRL_F10, SRC_EMPTY,
	"",		   NULL,			CTRL_F11, SRC_EMPTY,
	"",		   NULL,			CTRL_F12, SRC_EMPTY,
	"",		   NULL,			ALT_F1, SRC_EMPTY,
	"",		   NULL,			ALT_F2, SRC_EMPTY,
	"",		   NULL,			ALT_F3, SRC_EMPTY,
	"",		   NULL,			ALT_F4, SRC_EMPTY,
	"",		   NULL,			ALT_F5, SRC_EMPTY,
	"",		   NULL,			ALT_F6, SRC_EMPTY,
	"",		   NULL,			ALT_F7, SRC_EMPTY,
	"",		   NULL,			ALT_F8, SRC_EMPTY,
	"",		   NULL,			ALT_F9, SRC_EMPTY,
	"",		   NULL,			ALT_F10, SRC_EMPTY,
	"",		   NULL,			ALT_F11, SRC_EMPTY,
	"",		   NULL,			ALT_F12, SRC_EMPTY,
	"\006z",           NULL,                        GENERIC_CUT, SRC_EMPTY,
	"\006v",           NULL,                        GENERIC_PASTE, SRC_EMPTY,
	"\006c",           NULL,                        GENERIC_COPY, SRC_EMPTY,
	"\006 ",           NULL,                        GENERIC_MARK, SRC_EMPTY,
	DEF_GENERIC_MOUSE, NULL,                        GENERIC_MOUSE, SRC_EMPTY,

/* End of defined list; following are extras.				*/
	"\006h",           NULL,                        GENERIC_HOME, SRC_EMPTY,
	"\006u",	   NULL,     			GENERIC_UP, SRC_EMPTY,
	"\006d",	   NULL,     			GENERIC_DOWN, SRC_EMPTY,
        "\006l",	   NULL,     			GENERIC_LEFT, SRC_EMPTY,
        "\006r",	   NULL,   	  		GENERIC_RIGHT, SRC_EMPTY,
        "\006x",	   NULL,     			GENERIC_DELETE, SRC_EMPTY,
	"\006i",	   NULL,     			GENERIC_INSERT, SRC_EMPTY,
	"\006\013",	   NULL,     			GENERIC_UP, SRC_EMPTY,
	"\006\012",	   NULL,	     		GENERIC_DOWN, SRC_EMPTY,
        "\006\010",	   NULL,     			GENERIC_LEFT, SRC_EMPTY,
        "\006\014",	   NULL,     			GENERIC_RIGHT, SRC_EMPTY,
	"\006s",	   NULL,     			GENERIC_SELECT, SRC_EMPTY,
	"\005", 	   NULL,    			GENERIC_HELP, SRC_EMPTY,
	"\001",            NULL,                        GENERIC_HOME, SRC_EMPTY,
	"\022",		   NULL,     			GENERIC_REFRESH, SRC_EMPTY,
	"\010",		   NULL,			GENERIC_BACKTAB, SRC_EMPTY,
        "\006n",           NULL,                        GENERIC_NEWLINE, SRC_EMPTY,
	"\033[6~",         NULL,                        GENERIC_NEWLINE, SRC_EMPTY,
	"\033[5~",         NULL,                        GENERIC_BACKTAB, SRC_EMPTY
};


#ifdef WIN32
static VKEY pckbddef[] =
{
	"", NULL, 0,0,		/* element 0 in array not used */
	"", NULL, 0,0,		/* PF0 not used */
	"\035\073",	&vkeydef[GENERIC_PF1 ],		VKEY_F1, SRC_EMPTY,
	"\035\074",	&vkeydef[GENERIC_PF2 ],		VKEY_F2, SRC_EMPTY,
	"\035\075",	&vkeydef[GENERIC_PF3 ],		VKEY_F3, SRC_EMPTY,
	"\035\076",	&vkeydef[GENERIC_PF4 ], 	VKEY_F4, SRC_EMPTY,
	"\035\077",	&vkeydef[GENERIC_PF5 ],		VKEY_F5, SRC_EMPTY,
	"\035\100",	&vkeydef[GENERIC_PF6 ],		VKEY_F6, SRC_EMPTY,
	"\035\101",	&vkeydef[GENERIC_PF7 ],		VKEY_F7, SRC_EMPTY,
	"\035\102",	&vkeydef[GENERIC_PF8 ],		VKEY_F8, SRC_EMPTY,
	"\035\103",	&vkeydef[GENERIC_PF9 ],		VKEY_F9, SRC_EMPTY,
	"\035\104",	&vkeydef[GENERIC_PF10],		VKEY_F10, SRC_EMPTY,
	"\035\127",	&vkeydef[GENERIC_PF11],		VKEY_F11, SRC_EMPTY,
	"\035\130",	&vkeydef[GENERIC_PF12],		VKEY_F12, SRC_EMPTY,
	"",		NULL,				VKEY_F13, SRC_EMPTY,
	"",		NULL,				VKEY_F14, SRC_EMPTY,
	"",		NULL,				VKEY_F15, SRC_EMPTY,
	"",		NULL,				VKEY_F16, SRC_EMPTY,
	"",		NULL,				VKEY_F17, SRC_EMPTY,
	"",		NULL,				VKEY_F18, SRC_EMPTY,
	"",		NULL,				VKEY_F19, SRC_EMPTY,
	"",		NULL,				VKEY_F20, SRC_EMPTY,
	"",		NULL,				VKEY_F21, SRC_EMPTY,
	"",		NULL,				VKEY_F22, SRC_EMPTY,
	"",		NULL,				VKEY_F23, SRC_EMPTY,
	"",		NULL,				VKEY_F24, SRC_EMPTY,
	"",		NULL,				VKEY_F25, SRC_EMPTY,
	"",		NULL,				VKEY_F26, SRC_EMPTY,
	"",		NULL,				VKEY_F27, SRC_EMPTY,
	"",		NULL,				VKEY_F28, SRC_EMPTY,
	"",		NULL,				VKEY_F29, SRC_EMPTY,
	"",		NULL,				VKEY_F30, SRC_EMPTY,
	"",		NULL,				VKEY_F31, SRC_EMPTY,
	"",		NULL,				VKEY_F32, SRC_EMPTY,

	"\035\107",	&vkeydef[GENERIC_HELP],		VKEY_HOME,SRC_EMPTY,
	"\010",		&vkeydef[GENERIC_DELETE],	VKEY_BACKSPACE,	/* VKEY_BACKSPACE    */SRC_EMPTY,
	"\035\120",	&vkeydef[GENERIC_DOWN ],	GENERIC_DOWN,	/* VKEY_DOWN_ARROW   */SRC_EMPTY,
	"\035\113",	&vkeydef[GENERIC_LEFT ],	GENERIC_LEFT,	/* VKEY_LEFT_ARROW   */SRC_EMPTY,
	"\035\115",	&vkeydef[GENERIC_RIGHT],	GENERIC_RIGHT,	/* VKEY_RIGHT_ARROW  */SRC_EMPTY,
	"\035\110",	&vkeydef[GENERIC_UP   ],	GENERIC_UP,  	/* VKEY_UP_ARROW     */SRC_EMPTY,

	"\00601",          NULL,                        GENERIC_PF1, SRC_EMPTY,
	"\00602",          NULL,                        GENERIC_PF2, SRC_EMPTY,
	"\00603",          NULL,                        GENERIC_PF3, SRC_EMPTY,
	"\00604",          NULL,                        GENERIC_PF4, SRC_EMPTY,
	"\00605",          NULL,                        GENERIC_PF5, SRC_EMPTY,
	"\00606",          NULL,                        GENERIC_PF6, SRC_EMPTY,
	"\00607",          NULL,                        GENERIC_PF7,  SRC_EMPTY,
	"\00608",          NULL,                        GENERIC_PF8,  SRC_EMPTY,
	"\00609",          NULL,                        GENERIC_PF9,  SRC_EMPTY,
	"\00610",          NULL,                        GENERIC_PF10, SRC_EMPTY,
	"\00611",          NULL,                        GENERIC_PF11, SRC_EMPTY,
	"\00612",          NULL,                        GENERIC_PF12, SRC_EMPTY,
	"\00613",          NULL,                        GENERIC_PF13, SRC_EMPTY,
	"\00614",          NULL,                        GENERIC_PF14, SRC_EMPTY,
	"\00615",          NULL,                        GENERIC_PF15, SRC_EMPTY,
	"\00616",          NULL,                        GENERIC_PF16, SRC_EMPTY,
	"\00617",          NULL,                        GENERIC_PF17, SRC_EMPTY,
	"\00618",          NULL,                        GENERIC_PF18, SRC_EMPTY,
	"\00619",          NULL,                        GENERIC_PF19, SRC_EMPTY,
	"\00620",          NULL,                        GENERIC_PF20, SRC_EMPTY,
	"\00621",          NULL,                        GENERIC_PF21, SRC_EMPTY,
	"\00622",          NULL,                        GENERIC_PF22, SRC_EMPTY,
	"\00623",          NULL,                        GENERIC_PF23, SRC_EMPTY,
	"\00624",          NULL,                        GENERIC_PF24, SRC_EMPTY,
	"\00625",          NULL,                        GENERIC_PF25, SRC_EMPTY,
	"\00626",          NULL,                        GENERIC_PF26, SRC_EMPTY,
	"\00627",          NULL,                        GENERIC_PF27, SRC_EMPTY,
	"\00628",          NULL,                        GENERIC_PF28, SRC_EMPTY,
	"\00629",          NULL,                        GENERIC_PF29, SRC_EMPTY,
	"\00630",          NULL,                        GENERIC_PF30, SRC_EMPTY,
	"\00631",          NULL,                        GENERIC_PF31, SRC_EMPTY,
	"\00632",          NULL,                        GENERIC_PF32, SRC_EMPTY,

	"\006H",           NULL,                        GENERIC_HOME, SRC_EMPTY,
	"\006?",	   NULL,    			GENERIC_HELP, SRC_EMPTY,
	"\006U",	   NULL,     			GENERIC_UP, SRC_EMPTY,
	"\006D",	   NULL,     			GENERIC_DOWN, SRC_EMPTY,
        "\006L",	   NULL,     			GENERIC_LEFT, SRC_EMPTY,
        "\006R",	   NULL,     			GENERIC_RIGHT, SRC_EMPTY,
        "",	  	   NULL,     			GENERIC_TAB, SRC_EMPTY,
        "\006X",	   NULL,     			GENERIC_DELETE, SRC_EMPTY,
	"\006I",	   NULL,     			GENERIC_INSERT, SRC_EMPTY,
	"\006\016",	   NULL,     			GENERIC_NEXT_SCR, SRC_EMPTY,
	"\006\020",        NULL,                        GENERIC_PREV_SCR, SRC_EMPTY,

	"\035\123",	   &vkeydef[GENERIC_REMOVE],	VKEY_DELETE, SRC_EMPTY,
	"\035\122",	   &vkeydef[GENERIC_INSERT],	VKEY_INSERT, SRC_EMPTY,
	"\035\121",	   &vkeydef[GENERIC_NEWLINE], 	VKEY_NEXT_SCR, SRC_EMPTY,
	"\035\111",	   &vkeydef[GENERIC_BACKTAB],	VKEY_PREV_SCR, SRC_EMPTY,
	"",		   NULL,			VKEY_SELECT, SRC_EMPTY,

	"\006\011", 	   NULL,     			GENERIC_BACKTAB, SRC_EMPTY,
	"",		   NULL,     			GENERIC_REMOVE, SRC_EMPTY,
	"\006S",	   NULL,     			GENERIC_SELECT, SRC_EMPTY,
	"\006\003",	   NULL,		        GENERIC_CANCEL, SRC_EMPTY,
	"\015",		   NULL,     			GENERIC_RETURN, SRC_EMPTY,
        "\012",            NULL,                        GENERIC_ENTER, SRC_EMPTY,
	"\030",		   NULL,     			GENERIC_REFRESH, SRC_EMPTY,
	"\011",		   &vkeydef[GENERIC_TAB],	VKEY_TAB, SRC_EMPTY,
	"\035\001",    	   &vkeydef[GENERIC_HELP],	VKEY_HELP,	/* ESCAPE KEY */  	SRC_EMPTY,
	"\035\117",	   &vkeydef[GENERIC_PF16],	VKEY_DO,	/* END key */ 		SRC_EMPTY,
	"\035\035\017",	   &vkeydef[GENERIC_BACKTAB],	VKEY_FIND,	/* CRTL+TAB == BACKTAB */ SRC_EMPTY,
	"",		   NULL,			VKEY_USER1, SRC_EMPTY,
	"",		   NULL,			VKEY_USER2, SRC_EMPTY,
	"",		   NULL,			VKEY_USER3, SRC_EMPTY,
	"",		   NULL,			VKEY_USER4, SRC_EMPTY,
	"",		   NULL,			VKEY_USER5, SRC_EMPTY,
	"",		   NULL,			VKEY_USER6, SRC_EMPTY,
	"",		   NULL,			VKEY_USER7, SRC_EMPTY,
	"",		   NULL,			VKEY_USER8, SRC_EMPTY,
	"\035\035\110",	   NULL,			GENERIC_CLEAR_FIELD,  SRC_EMPTY,
	"\035\035\115",	   NULL,			GENERIC_CLEAR_AFTER,  SRC_EMPTY,
	"",		   NULL,      			GENERIC_CLEAR_BEFORE, SRC_EMPTY,
	"",		   NULL,      			GENERIC_NULL, SRC_EMPTY,
	"",		   NULL,			TRIGGER1, SRC_EMPTY,
	"",		   NULL,			TRIGGER2, SRC_EMPTY,
	"",		   NULL,			TRIGGER3, SRC_EMPTY,
	"",		   NULL,			TRIGGER4, SRC_EMPTY,
	"",		   NULL,			TRIGGER5, SRC_EMPTY,
	"",		   NULL,			TRIGGER6, SRC_EMPTY,
	"",		   NULL,			TRIGGER7, SRC_EMPTY,
	"",		   NULL,			TRIGGER8, SRC_EMPTY,
	"",		   NULL,			VKEY_F33, SRC_EMPTY,
	"",		   NULL,			VKEY_F34, SRC_EMPTY,
	"",		   NULL,			VKEY_F35, SRC_EMPTY,
	"",		   NULL,			VKEY_F36, SRC_EMPTY,
	"",		   NULL,			VKEY_F37 , SRC_EMPTY,
	"",		   NULL,			VKEY_F38, SRC_EMPTY,
	"",		   NULL,			VKEY_F39, SRC_EMPTY,
	"",		   NULL,			VKEY_F40, SRC_EMPTY,
	"",		   NULL,			VKEY_F41 , SRC_EMPTY,
	"",		   NULL,			VKEY_F42 , SRC_EMPTY,
	"",		   NULL,			VKEY_F43, SRC_EMPTY,
	"",		   NULL,			VKEY_F44, SRC_EMPTY,
	"",		   NULL,			VKEY_F45, SRC_EMPTY,
	"",		   NULL,			VKEY_F46, SRC_EMPTY,
	"",		   NULL,			VKEY_F47, SRC_EMPTY,
	"",		   NULL,			VKEY_F48, SRC_EMPTY,
	"",		   NULL,			VKEY_F49, SRC_EMPTY,
	"",		   NULL,			VKEY_F50 , SRC_EMPTY,
	"",		   NULL,			VKEY_F51, SRC_EMPTY,
	"",		   NULL,			VKEY_F52, SRC_EMPTY,
	"",		   NULL,			VKEY_F53 , SRC_EMPTY,
	"",		   NULL,			VKEY_F54, SRC_EMPTY,
	"",		   NULL,			VKEY_F55, SRC_EMPTY,
	"",		   NULL,			VKEY_F56, SRC_EMPTY,
	"",		   NULL,			VKEY_F57, SRC_EMPTY,
	"",		   NULL,			VKEY_F58, SRC_EMPTY,
	"",		   NULL,			VKEY_F59, SRC_EMPTY,
	"",		   NULL,			VKEY_F60, SRC_EMPTY,
	"",		   NULL,			VKEY_F61, SRC_EMPTY,
	"",		   NULL,			VKEY_F62, SRC_EMPTY,
	"",		   NULL,			VKEY_F63, SRC_EMPTY,
	"",		   NULL,			VKEY_F64, SRC_EMPTY,
	"\00633",	   NULL,			GENERIC_PF33, SRC_EMPTY,
	"\00634",	   NULL,			GENERIC_PF34, SRC_EMPTY,
	"\00635",	   NULL,			GENERIC_PF35, SRC_EMPTY,
	"\00636",	   NULL,			GENERIC_PF36, SRC_EMPTY,
	"\00637",	   NULL,			GENERIC_PF37, SRC_EMPTY,
	"\00638",	   NULL,			GENERIC_PF38, SRC_EMPTY,
	"\00639",	   NULL,			GENERIC_PF39, SRC_EMPTY,
	"\00640",	   NULL,			GENERIC_PF40, SRC_EMPTY,
	"\00641",	   NULL,			GENERIC_PF41, SRC_EMPTY,
	"\00642",	   NULL,			GENERIC_PF42, SRC_EMPTY,
	"\00643",	   NULL,			GENERIC_PF43, SRC_EMPTY,
	"\00644",	   NULL,			GENERIC_PF44, SRC_EMPTY,
	"\00645",	   NULL,			GENERIC_PF45, SRC_EMPTY,
	"\00646",	   NULL,			GENERIC_PF46, SRC_EMPTY,
	"\00647",	   NULL,			GENERIC_PF47, SRC_EMPTY,
	"\00648",	   NULL,			GENERIC_PF48, SRC_EMPTY,
	"\00649",	   NULL,			GENERIC_PF49, SRC_EMPTY,
	"\00650",	   NULL,			GENERIC_PF50, SRC_EMPTY,
	"\00651",	   NULL,			GENERIC_PF51, SRC_EMPTY,
	"\00652",	   NULL,			GENERIC_PF52, SRC_EMPTY,
	"\00653",	   NULL,			GENERIC_PF53, SRC_EMPTY,
	"\00654",	   NULL,			GENERIC_PF54, SRC_EMPTY,
	"\00655",	   NULL,			GENERIC_PF55, SRC_EMPTY,
	"\00656",	   NULL,			GENERIC_PF56, SRC_EMPTY,
	"\00657",	   NULL,			GENERIC_PF57, SRC_EMPTY,
	"\00658",	   NULL,			GENERIC_PF58, SRC_EMPTY,
	"\00659",	   NULL,			GENERIC_PF59, SRC_EMPTY,
	"\00660",	   NULL,			GENERIC_PF60, SRC_EMPTY,
	"\00661",	   NULL,			GENERIC_PF61, SRC_EMPTY,
	"\00662",	   NULL,			GENERIC_PF62, SRC_EMPTY,
	"\00663",	   NULL,			GENERIC_PF63, SRC_EMPTY,
	"\00664",	   NULL,			GENERIC_PF64, SRC_EMPTY,
        "\006N",           NULL,                        GENERIC_NEWLINE, SRC_EMPTY,

	"\035\052\073",	&vkeydef[GENERIC_PF11],		SHIFT_F1, SRC_EMPTY,
	"\035\052\074",	&vkeydef[GENERIC_PF12],		SHIFT_F2, SRC_EMPTY,
	"\035\052\075",	&vkeydef[GENERIC_PF13],		SHIFT_F3, SRC_EMPTY,
	"\035\052\076",	&vkeydef[GENERIC_PF14],		SHIFT_F4, SRC_EMPTY,
	"\035\052\077",	&vkeydef[GENERIC_PF15],		SHIFT_F5, SRC_EMPTY,
	"\035\052\100",	&vkeydef[GENERIC_PF16],		SHIFT_F6, SRC_EMPTY,
	"\035\052\101",	&vkeydef[GENERIC_PF17],		SHIFT_F7, SRC_EMPTY,
	"\035\052\102",	&vkeydef[GENERIC_PF18],		SHIFT_F8, SRC_EMPTY,
	"\035\052\103",	&vkeydef[GENERIC_PF19],		SHIFT_F9, SRC_EMPTY,
	"\035\052\104",	&vkeydef[GENERIC_PF20],		SHIFT_F10, SRC_EMPTY,
	"\035\052\127",	&vkeydef[GENERIC_PF21],		SHIFT_F11, SRC_EMPTY,
	"\035\052\130",	&vkeydef[GENERIC_PF22],		SHIFT_F12, SRC_EMPTY,

	"\035\035\073",	&vkeydef[GENERIC_PF21],		CTRL_F1, SRC_EMPTY,
	"\035\035\074",	&vkeydef[GENERIC_PF22],		CTRL_F2, SRC_EMPTY,
	"\035\035\075",	&vkeydef[GENERIC_PF23],		CTRL_F3, SRC_EMPTY,
	"\035\035\076",	&vkeydef[GENERIC_PF24],		CTRL_F4, SRC_EMPTY,
	"\035\035\077",	&vkeydef[GENERIC_PF25],		CTRL_F5, SRC_EMPTY,
	"\035\035\100",	&vkeydef[GENERIC_PF26],		CTRL_F6, SRC_EMPTY,
	"\035\035\101",	&vkeydef[GENERIC_PF27],		CTRL_F7, SRC_EMPTY,
	"\035\035\102",	&vkeydef[GENERIC_PF28],		CTRL_F8, SRC_EMPTY,
	"\035\035\103",	&vkeydef[GENERIC_PF29],		CTRL_F9, SRC_EMPTY,
	"\035\035\104",	&vkeydef[GENERIC_PF30],		CTRL_F10, SRC_EMPTY,
	"\035\035\127",	&vkeydef[GENERIC_PF31],		CTRL_F11, SRC_EMPTY,
	"\035\035\130",	&vkeydef[GENERIC_PF32],		CTRL_F12, SRC_EMPTY,

	"\035\070\073",	&vkeydef[GENERIC_PF31],		ALT_F1, SRC_EMPTY,
	"\035\070\074",	&vkeydef[GENERIC_PF32],		ALT_F2, SRC_EMPTY,
	"",		   NULL,			ALT_F3, SRC_EMPTY,
	"",		   NULL,			ALT_F4, SRC_EMPTY,
	"",		   NULL,			ALT_F5, SRC_EMPTY,
	"",		   NULL,			ALT_F6, SRC_EMPTY,
	"",		   NULL,			ALT_F7, SRC_EMPTY,
	"",		   NULL,			ALT_F8, SRC_EMPTY,
	"",		   NULL,			ALT_F9, SRC_EMPTY,
	"",		   NULL,			ALT_F10, SRC_EMPTY,
	"",		   NULL,			ALT_F11, SRC_EMPTY,
	"",		   NULL,			ALT_F12, SRC_EMPTY,
	DEF_GENERIC_MOUSE, NULL,                        GENERIC_MOUSE, SRC_EMPTY,

/* End of defined list; following are extras.				*/
	"\006h",           NULL,                        GENERIC_HOME, SRC_EMPTY,
	"\006u",	   NULL,     			GENERIC_UP, SRC_EMPTY,
	"\006d",	   NULL,     			GENERIC_DOWN, SRC_EMPTY,
        "\006l",	   NULL,     			GENERIC_LEFT, SRC_EMPTY,
        "\006r",	   NULL,   	  		GENERIC_RIGHT, SRC_EMPTY,
        "\006x",	   NULL,     			GENERIC_DELETE, SRC_EMPTY,
	"\006i",	   NULL,     			GENERIC_INSERT, SRC_EMPTY,
	"\006\013",	   NULL,     			GENERIC_UP, SRC_EMPTY,
	"\006\012",	   NULL,	     		GENERIC_DOWN, SRC_EMPTY,
        "\006\010",	   NULL,     			GENERIC_LEFT, SRC_EMPTY,
        "\006\014",	   NULL,     			GENERIC_RIGHT, SRC_EMPTY,
	"\006s",	   NULL,     			GENERIC_SELECT, SRC_EMPTY,
	"\005", 	   NULL,    			GENERIC_HELP, SRC_EMPTY,
	"\001",            NULL,                        GENERIC_HOME, SRC_EMPTY,
	"\022",		   NULL,     			GENERIC_REFRESH, SRC_EMPTY,
	"\010",		   NULL,			GENERIC_BACKTAB, SRC_EMPTY,
        "\006n",           NULL,                        GENERIC_NEWLINE, SRC_EMPTY,
	"\033[6~",         NULL,                        GENERIC_NEWLINE, SRC_EMPTY,
	"\033[5~",         NULL,                        GENERIC_BACKTAB, SRC_EMPTY
};
#endif /* WIN32 */
					
static VKEY *vkp;						


#define VC_META_NODE struct vc_meta_node

VC_META_NODE
{
	int ch;
	int key_id;
	int next_hash;
	VC_META_NODE **next;
};

static int vgetmeta(VC_META_NODE **node, int size);


#define HASHPOINT 5


#define PATHSIZE 128

#define VCMEMSIZE 4096
#define STACKSIZE 64

#define ST_ZED -1

#define TYPE_LIT 0
#define TYPE_SYM 1
#define TYPE_NUM 2
#define TYPE_MLT 3

#define C_QUOTE '"'
#define C_ESC '\\'
#define C_CTL '^'
#define C_PIPE '|'
#define C_NULL (char)0

#define S_START 0
#define S_LIT0 1
#define S_LIT 2
#define S_SYM 3
#define S_CTL0 4
#define S_ESC0 5
#define S_CTL 6
#define S_ESC 7
#define S_END 8
#define S_TERM 9
#define S_ERROR -1

#define RV_STATE_CNT 10

#define E_NORM  0
#define E_QUOTE 1
#define E_PIPE 2
#define E_ESC 3
#define E_CTL 4
#define E_NULL 5

#define RV_EVENT_CNT 6

#define BSEARCH_FAILED -1



/*
**	Static data
*/

static int loaded=0;
static int vcloadbehavior = VCAP_NEED_NOWRAP|VCAP_NEED_FKEYS1_32|VCAP_WARN_PRMSG;

static int vkey_ext = VC_KEY_LAST;
static VC_META_NODE meta_top,*meta_p,**meta_pp;
static unsigned char cstack[STACKSIZE];
static int cstackp= -1;
static int vcline = 0;
static int stab[RV_EVENT_CNT][RV_STATE_CNT] =
{     /*     START    LIT0    LIT      SYM     CTL0     ESC0    CTL     ESC     S_TERM*/
/*NORM*/  { S_SYM,   S_LIT,  S_LIT,  S_SYM,   S_CTL,   S_ESC,  S_LIT,  S_LIT,   0 },
/*QUOTE*/ { S_LIT0,  S_END,  S_END,  S_ERROR, S_ERROR, S_ESC,  S_END,  S_END,   0 },
/*OR*/    { S_START, S_LIT,  S_LIT,  S_END,   S_CTL,   S_ESC,  S_LIT,  S_LIT,   0 },
/*ESC*/   { S_ERROR, S_ESC0, S_ESC0, S_ERROR, S_CTL,   S_ESC,  S_ESC0, S_ESC0,  0 },
/*CIRC*/  { S_ERROR, S_CTL0, S_CTL0, S_ERROR, S_CTL,   S_ESC,  S_CTL0, S_CTL0,  0 },
/*NULL*/  { S_TERM,  S_ERROR,S_ERROR,S_END,   S_ERROR, S_ERROR,S_ERROR,S_ERROR, 0 },
};


static char *vcap_filepath = NULL;
static char *vcap_termtype = NULL;

/*
**	Static Function Prototypes
*/

#ifdef AIX
#undef regcmp
#define regcmp(x,y) re_comp(x)
#undef regex
#define regex(x,y) re_exec(y)
char *re_comp();
int re_exec();
#else
char *regex();
char *regcmp();
#endif

static int pop(void);
static int push(int ch);
static char *gmem();
static void vc_add_key();
static int nextfield();
static int matchcap();
static int rvalue();
static void fix_seq();
static void doload(FILE *vcfile, char* vcpath);
static void set_terminfo_key(char *id, int index, char *value);

static int vcsort();
static int vcm_sort_node();

static	char *vcstrdup();

#ifdef unix
static int vcloadterminfo(void);
#endif

char* vkeyvalue(int key)
{
	return vkeydef[key].value;
}

char* vcapvalue(int cap)
{
	return vcapdef[cap];
}


/*
**	ROUTINE:	vcap_set_vcapfile()
**
**	FUNCTION:	Set the videocap file path and terminal type.
**
**	DESCRIPTION:	This routine should be called before any other 
**			video routines are called.(Before vcapload()).
**
**			If this routine is not called then vcapload() will
**			use it's own logic to figure out the terminal type.
**
**	ARGUMENTS:	
**	vcappath	The full path to the videocap file.
**			e.g. "/disk1/wisp/config/videocap/vt100"
**
**	termtype	The terminal type. e.g. "vt100".
**
**	GLOBALS:	
**	vcap_filepath	Set the videocap filepath.
**	vcap_termtype	Set the terminal type.
**
**	RETURN:		none
**
**	WARNINGS:	none
**
*/
void vcap_set_vcapfile(const char *vcappath, const char* termtype)
{
	if (vcap_filepath)
	{
		free(vcap_filepath);
		vcap_filepath = NULL;
	}
	if (vcap_termtype)
	{
		free(vcap_termtype);
		vcap_termtype = NULL;
	}

	if (vcappath && *vcappath)
	{
		vcap_filepath = (char*)malloc(strlen(vcappath)+1);
		strcpy(vcap_filepath, vcappath);
	}

	if (termtype && *termtype)
	{
		vcap_termtype = (char*)malloc(strlen(termtype)+1);
		strcpy(vcap_termtype, termtype);
	}
}

/*
**	ROUTINE:	vcmetaprint()
**
**	FUNCTION:	Recursively dump the meta tables
**
**	DESCRIPTION:	This is only used to debug videocap problems
**
**	ARGUMENTS:	
**	fh		Open file handle to print to.
**	node_pp		The arrary of nodes to dump
**	hash		The max array index for the node_pp array
**	level		Recursion level
**	prefix		Prefix of character above this node level
**
**	GLOBALS:	none
**
**	RETURN:		Number of elements
**
**	WARNINGS:	none
**
*/
static int vcapmetaprint(FILE *fh, VC_META_NODE **node_pp, int hash, int level, char* prefix)
{
	int	i;
	char	prebuf[40];
	int 	size;
	
	size = hash+1;
	
	if (!node_pp || size<1) return 0;

	fprintf(fh, "[%d] ", level);
	
	fprintf(fh, "node_pp=0x%08x size=%d\n", node_pp, size);
	
	for(i=0; i<size; i++)
	{
		sprintf(prebuf,"%s\\%03o", prefix, node_pp[i]->ch);
		if (node_pp[i]->next) 
		{
			strcat(prebuf,",");
		}

		fprintf(fh, "[%d][%2d of %2d] ", level, i+1, size);
		fprintf(fh, "node_pp[%2d]=0x%08x ch=%3d key_id=%3d    <%s>\n",
			i, node_pp[i], node_pp[i]->ch, node_pp[i]->key_id, prebuf);
		if (node_pp[i]->next) 
		{
			vcapmetaprint(fh, node_pp[i]->next, node_pp[i]->next_hash, level+1, prebuf);
		}
	}
	return size;
}

/*
**	ROUTINE:	vcapmetadump()
**
**	FUNCTION:	Dump the meta tables
**
**	DESCRIPTION:	This is only used to debug videocap problems
**			Set environment variable "VCAPMETADUMP" to activate
**
**	ARGUMENTS:	none
**
**	GLOBALS:	none
**
**	RETURN:		none
**
**	WARNINGS:	none
**
*/
static void vcapmetadump(void)
{
	FILE *fh;

	if (getenv("VCAPMETADUMP"))
	{
		fh = fopen("vcapdump.txt","w");
	
		vcapmetaprint(fh, meta_top.next, meta_top.next_hash, 0, "");

		fclose(fh);
	}
}

/*
**	vcapload()	Load the videocap file into the internal tables
**
**			vcapload()
**			    vcloadvideocap()	- load from file or defaults
**				doload()	- load from file
**			    vc_add_stddefs()	- load stddefs
**			    build_vc_meta()	- build parse tree for key defs
*/
int vcapload(void)									/* load up the vcap file 		*/
{
	int ret=0;
	char wterm_t[80];
	char vcpath[PATHSIZE];								/* path of desc file for current term	*/

	if (loaded) return 0;

	/*
	**	Get the termtype
	*/
	if (vcap_termtype)
	{
		strcpy(wterm_t,vcap_termtype);
	}
	else
	{
		char *term_t;

		/*
		**	This is a remnant of the old way.
		**	The correct way to set termtype is to call vcap_set_vcapfile().
		*/
		if (!(term_t=getenv("WISPTERM")))
		{
			if (!(term_t=getenv("OPEN3KTERM")))
			{
				if (!(term_t=getenv("TERM")))
				{
#ifdef unix
					if (! (vcloadbehavior & VCAP_NOWARN_RETSTAT) )
					{
						vre("VIDEO-E-NOTERM $TERM environment variable not defined");
					}
					ret |= VCAP_WARN_NOWCONFIG;
#endif
					term_t=DEFTERM;
				}
			}
		}
		strcpy(wterm_t,term_t);

	}

	/*
	**	Get the path to the videocap file
	*/
	if (vcap_filepath)
	{
		strcpy(vcpath,vcap_filepath);
	}
	else
	{
		char vcdir[PATHSIZE];
		char *wc;

		if (wc=getenv("VIDEOINFO"))
		{
			strcpy(vcdir,wc);
		}
		else if (wc=getenv("VIDEOCAP"))
		{
			strcpy(vcdir,wc);
		}
		else if (wc=getenv("WISPCONFIG"))
		{
			vbldfilepath(vcdir,wc,"videocap");
		}
		else if (wc=getenv("OPEN3KCONFIG"))
		{
			vbldfilepath(vcdir,wc,"videocap");
		}
		else
		{
			strcpy(vcdir,VIDEOINFODIR);
		}
	
		vbldfilepath(vcpath,vcdir,wterm_t);

		/* Try first with .vcap then without */
		strcat(vcpath,".vcap");
		if (!vexists(vcpath))
		{
			/* Check without ext */
			vcpath[strlen(vcpath) - 5] = '\0';
			if (!vexists(vcpath))
			{
				/* Didn't find it so put ext back on */
				strcat(vcpath, ".vcap");
			}				
		}
		
	}

#ifdef unix
	memset(vkeydef,0,sizeof(vkeydef));
	memset(vcapdef,0,sizeof(vcapdef));

	if (NULL == getenv("VNOTERMINFO"))
	{
		vcloadterminfo();
	}
#endif

	vcloadvideocap(vcpath,wterm_t);

	vc_add_stddefs();
	
	build_vc_meta();

	if (vcapdef[PAD]) 
	{
		vcap_padding=atoi(vcapdef[PAD]);
	}
	else 
	{
		vcap_padding=0;
	}

	/*
	**	Ensure pseudo_blanks and graphstr are properly set.
	*/
	if (!vcapdef[PSEUDO_BLANKS] || strlen(vcapdef[PSEUDO_BLANKS]) < PSEUDO_BLANKS_LEN)
	{
		static char the_pseudo_blanks[PSEUDO_BLANKS_LEN+1];

		vcapdef[PSEUDO_BLANKS] = the_pseudo_blanks;

#ifdef WIN32
		if (vrawdirectio())
		{
			strcpy(the_pseudo_blanks, "\004\370\026\260");
		}
		else
		{
			strcpy(the_pseudo_blanks, "\xF8\xF9\xB1\xFE");
		}
#else
		if (0 == memcmp(vcapterm(),"vt",2))
		{
			strcpy(the_pseudo_blanks, "`f~a");
		}
		else
		{
			strcpy(the_pseudo_blanks, "#*^~");
		}
#endif
	}
	
	if (!vcapdef[GRAPHSTR] || strlen(vcapdef[GRAPHSTR]) < GRAPHSTR_LEN)
	{
		static char the_graphstr[GRAPHSTR_LEN+1];

		vcapdef[GRAPHSTR] = the_graphstr;

#ifdef WIN32
		if (vrawdirectio())
		{
			strcpy(the_graphstr, "\263\304\332\277\300\331\302\301\303\264\305\021\020");
		}
		else
		{
			strcpy(the_graphstr, "xqlkmjwvtun\253\273");
		}
#else
		if (0 == memcmp(vcapterm(),"vt",2))
		{
			strcpy(the_graphstr, "xqlkmjwvtun\253\273");
		}
		else
		{
			strcpy(the_graphstr, "|-****--||+<>");
		}
#endif
	}

	loaded = 1;

	vcapmetadump();

	return 0;
}


void vcloaded(void)
{
	loaded=TRUE;
}

/*
**	ROUTINE:	vcapterm()
**
**	FUNCTION:	Get the terminal type.
**
**	DESCRIPTION:	Return vcaptermvalue as set in vcloadvideocap() routine.
**
**	ARGUMENTS:	None
**
**	GLOBALS:       
**	vcaptermvalue	The terminal type value
**
**	RETURN:		The terminal type
**
**	WARNINGS:	If called before vcapload() this will return "UNKNOWN".
**
*/
static char vcaptermvalue[40] = "UNKNOWN";
const char* vcapterm(void)
{
	return vcaptermvalue;
}

static void vcloadvideocap(char* vcpath, char* wterm_t)
{
	FILE *vcfile;

	qsort(vc_load_defs,								/* sort vc_load_defs array */
	      sizeof(vc_load_defs)/sizeof(vc_load)-1,					/* sizeof less one (don't sort NULL elem)*/
	      sizeof(vc_load),								/* sizeof one element */
	      (int(*)(const void *,const void *))vcsort);				/* sort routine */

	if (!(vcfile=fopen(vcpath,"r")))						/* open videocap file */
	{
		/*
		**	Open of videocap file failed
		*/
#ifdef unix
		if (!(vcloadbehavior & VCAP_NOWARN_RETSTAT))
		{
			vre("VIDEOCAP-E-OPEN Error %d opening videocap file %s",errno,vcpath);
		}
		memcpy(vkeydef,vt220def,sizeof(vt220def));
		strcpy(vcaptermvalue, "vt220");
#endif
#ifdef WIN32
		if (vrawdirectio())
		{
			memcpy(vkeydef,pckbddef,sizeof(pckbddef));
			strcpy(vcaptermvalue, DEFTERM);
		}
		else
		{
			memcpy(vkeydef,vt220def,sizeof(vt220def));
			strcpy(vcaptermvalue, "vt220");
		}
#endif
	}
	else 
	{
		doload(vcfile,vcpath);
		fclose(vcfile);
		strcpy(vcaptermvalue, wterm_t);
	}
}

void vbldfilepath(char* path, char* dir, char* file)
{
#ifdef unix
	sprintf(path,"%s/%s",dir,file);
#endif
#ifdef WIN32
	sprintf(path,"%s\\%s",dir,file);
#endif
}

void vc_add_stddefs(void)
{
	register int idx;
	
	for (idx=0; stdkeys[idx].value; ++idx)
	{
		vc_add_key(stdkeys[idx].index,stdkeys[idx].value,NOMAPPING,SRC_STDDEFS);
	}
}

static void vc_add_key(int index, char* value, int symbidx, int source)
                                                /* symbidx:  -1 for literal key, index of dest key if mapped */
{	  
	VKEY *key_main, *key_symb;
	
	key_main = &vkeydef[index];		      /* this keys structure */
	if (symbidx != NOMAPPING)
	{
		key_symb = &vkeydef[symbidx];
	}
	else
	{
		key_symb = NULL;
	}

	if (symbidx == NOMAPPING)        	      /* is key literal, ie (does it have a value, byte sequence) */
	{
#ifdef unix
		if (source==SRC_VIDEOINFO)
		{
			register idx;
			for (idx=0; idx<vkey_ext; ++idx)
			{
				if (vkeydef[idx].value && !strcmp(value,vkeydef[idx].value))
				{
					vkeydef[idx].value=NULL;
					vkeydef[idx].id=0;
					vkeydef[idx].source=SRC_EMPTY;
					vkeydef[idx].eval_to=NULL;
				}
			}
		}
#endif
		if (key_main->source == SRC_EMPTY || (key_main->source == SRC_TERMINFO && source==SRC_VIDEOINFO))
		{	  
			key_main->source = source;
			key_main->id = index;  	      /* no, so give it an index (means this key can be returned by vgetm() */
			key_main->value = vcstrdup(value);
		}	  
		else
		{
			vkp = &vkeydef[vkey_ext++];   /* grab the extra struct */
			vkp->id = 0;		      /* id is 0, this sequence generates an id from the other struct  */
			vkp->value = vcstrdup(value);   /* copy value */
			vkp->eval_to = key_main;      /* and point to requested struct */
		}	
	}
	else					      /* or a 'mapped' key */
	{
		if (key_symb->source == SRC_EMPTY)
		{	  
			key_main->id = index;
			key_symb->id = 0;		      /* clear it's id */
			key_symb->eval_to = key_main; /* and point to current */
		}	
		else if (key_symb->eval_to==NULL)
		{
			key_main->id = index;
			key_symb->id = 0;		      /* clear it's id */
			key_symb->eval_to = key_main;
		}
			
	}
}

void vc_map_key(int index1, int index2)
{
	vkeydef[index1].eval_to = &vkeydef[index2];
	vkeydef[index1].id = 0;
}

int vc_gotkey(int index)
{
	register int scan;
	register VKEY *keyscanp;
	
	if (vkeydef[index].value && strlen(vkeydef[index].value))
	{
		return TRUE;
	}
	if (vkeydef[index].id)
	{
		for (scan=0; scan<vkey_ext; ++scan)
		{
			if (vkeydef[scan].value == NULL || strlen(vkeydef[scan].value)==0)
			{
				continue;
			}
			keyscanp= &vkeydef[scan];
			while (keyscanp->eval_to)
			{
				keyscanp = keyscanp->eval_to;
			}
			if (keyscanp->id == vkeydef[index].id)
			{
				return TRUE;
			}
		}
	}
	return FALSE;
}

static void doload(FILE *vcfile, char* vcpath)
{
	char name[64];
	unsigned char value[128], subval[64];
	int type,cap,eval;
	char *gmem();
	
	while (nextfield(vcfile,name,value))						/* load fields */
	{
		cap=matchcap(name);							/* match our vc_load_defs array */
		if (cap<0)								/* capability unknown */
		{
			vre("VIDEO-C-BADCAP bad key name (lvalue) [%s line %d]",vcpath,vcline);
			continue;
		}
		switch (vc_load_defs[cap].type)						/* check type key/cap/bool/num */
		{
		case ISKEY:								/* is a key def */
			while (rvalue(subval,value,&type))
			{
				switch (type)						/* type of value specified */
				{
				case TYPE_LIT:						/* key sequence description */
					vc_add_key(vc_load_defs[cap].index,
						    (char*)subval, NOMAPPING, SRC_VIDEOINFO);
					break;
				case TYPE_SYM:						/* symbol: logical xlation */
					eval=matchcap(subval);				/* find struct for symbol specified */
					if (eval<0)					/* bad */
					{
						vre("VIDEO-C-BADCAP bad key name (rvalue) [%s line %d]",
							vcpath,vcline);
						break;
					}
					vc_add_key(vc_load_defs[cap].index,NULL,
						   vc_load_defs[eval].index,SRC_VIDEOINFO);
					
					break;
				}
			}
			break;

		case ISCAP:
			rvalue(subval,value,&type);					/* cut out the field */
			switch (type)
			{
#ifdef unix
				char *fixtcti, *vtctoti();
#endif
			case TYPE_LIT:							/* dup the literal sequence */
#ifdef unix
				if (vc_load_defs[cap].index == CHANGE_SCROLL_REGION ||
				    vc_load_defs[cap].index == CURSOR_ADDRESS)
				{
					fixtcti = vtctoti(subval);
					vcapdef[vc_load_defs[cap].index] = vcstrdup(fixtcti);
				}
				else
				{
					vcapdef[vc_load_defs[cap].index] = vcstrdup((char *)subval);
				}
#else
				vcapdef[vc_load_defs[cap].index] = vcstrdup((char *)subval);				
#endif
				fix_seq(vcapdef[vc_load_defs[cap].index]);
				break;
			case TYPE_SYM:
				eval=matchcap(value);						/* symbol: find it */
				vcapdef[vc_load_defs[cap].index] = vcstrdup(vcapdef[eval]);/* copy his sequence */
				break;
			}
			break;

		case ISNUM:
			rvalue(subval,value,&type);
			vcapdef[vc_load_defs[cap].index] = gmem(sizeof(int));
			*(vcapdef[vc_load_defs[cap].index]) = atoi((char *)subval);
			break;
		}
	}
}

static int rvalue(sub,val,type) unsigned char *sub,*val; int *type;
{
	int state=S_START;									/* machine state		*/
	int event, ch;										/* event and current input char	*/
	unsigned char *valsave;									/* save start of field buf	*/

	valsave=val;

       	while (state!=S_END)
	{
		ch = *val++;
		switch (ch)								/* decide what event */
		{
		case C_QUOTE: 
			event=E_QUOTE;
			break;
		case C_PIPE:
			event=E_PIPE;
			break;
		case C_ESC:
			event=E_ESC;
			break;
		case C_CTL:
			event=E_CTL;
			break;
		case C_NULL:
			event=E_NULL;
			--val;
			break;
		default:
			event=E_NORM;
			break;
		}
		
		state = stab[event][state];						/* find next state */
		switch (state)
		{
		case S_LIT0:
			*type=TYPE_LIT;							/* init the type */
			break;
		case S_SYM:
			*type=TYPE_SYM;							/* fall thru */
		case S_LIT: 
			*sub++ = ch;							/* copy back */
			break;
		case S_ESC:							/* is it 'E'? if so, pass ESC */
			switch (ch)
			{
#ifdef WIN32
			case 'X': case 'x':
				*sub++ = EXT_PREFIX;
				break;
#endif
			case 'E': case 'e':
				*sub++ = 27;
				break;
			case 'n':
				*sub++ = '\n';
				break;
			case 'l':
				*sub++ = '\012';
				break;
			case 'r':
				*sub++ = '\015';
				break;
			case 't':
				*sub++ = '\011';
				break;
			case 'b':
				*sub++ = '\010';
				break;
			case 'f':
				*sub++ = '\014';
				break;
			case 's':
				*sub++ = ' ';
				break;
			default:
				if (ch >= '0' && ch <= '9')
				{
					int tmp;
					
					tmp = (ch-0x30)*64;
					tmp += (*val++ -0x30)*8;
					tmp += (*val++ -0x30);
					*sub++ = tmp?tmp:0x80;
				}
				else
				  *sub++ = ch;
				break;
			}
			break;
		case S_CTL:
		{
			int tmp;
			tmp= ch=='?'?127: toupper(ch) & 0xbf;			/* is it '?'? if so, pass DEL */
			*sub++ = tmp?tmp:0x80;
			break;			
		}
		case S_END:
			continue;							/* end of field */
		case S_TERM:
			return FALSE;							/* no more data */
		default:
			break;
		}
	}
	*sub=(char)0;
	strcpy((char *)valsave,(char *)val);
	return TRUE;
}

void build_vc_meta()									/* build the vc_meta parse tree */
{
	int i,j,ch;
	unsigned char ch_list[256];
	VC_META_NODE **build_vc_node();

	
	memset(&meta_top,0,sizeof(VC_META_NODE));					/* zed mem for top node  */

	for (i=0,j=0,memset(ch_list,0,sizeof(ch_list));i<VC_KEY_COUNT+VC_KEY_EXTRA;++i)	
	{
		if (!vkeydef[i].value) continue;					/* null value, skip */
		ch = *((unsigned char *)vkeydef[i].value);				/* get 1st byte */
		if (!strchr((char *)ch_list,(char)ch))					/* don't already have it */
			ch_list[j++]=ch;
	}
	j--;
	
	meta_top.ch= -1;
	meta_top.key_id= -1;
	meta_top.next_hash= j;
	meta_top.next = build_vc_node(ch_list,0,(unsigned char *)"");
}

VC_META_NODE **build2_vc_node();
VC_META_NODE **build_vc_node(chars,depth,prev_seq)
unsigned char *chars;
int depth;
unsigned char *prev_seq;
{
	unsigned char ch_list[256];
  	int cnt,i,j,k,ch,gotkey,len,key;
	VC_META_NODE **arr,*node;
	unsigned char cur_seq[128];

	cnt=strlen((char *)chars);
	arr=(VC_META_NODE**)gmem(sizeof(VC_META_NODE*)*cnt);
	for (i=0; i<cnt; ++i)
	{
		for (gotkey=0,j=0,k=0,memset(ch_list,0,sizeof(ch_list));j<VC_KEY_COUNT+VC_KEY_EXTRA;++j)
		{
			if ( !vkeydef[j].value ) 
				continue;				/* null value, skip */
			len=strlen(vkeydef[j].value);
			if (depth>(len-1)) 
				continue;

			if (*((unsigned char *)vkeydef[j].value+depth)!=chars[i]) 
				continue;

			if (depth) 
				if (strncmp((char *)vkeydef[j].value,(char *)prev_seq,depth)) 
					continue;	/* char not equal */
			if (depth==(len-1))						/* a complete key */
			{
				node = *(arr+i) = (VC_META_NODE*)gmem(sizeof(VC_META_NODE));
				node->ch= chars[i];
				if (!vkeydef[j].eval_to) 
					node->key_id = vkeydef[j].id;
				else
				{
					for (vkp= &vkeydef[j]; vkp->eval_to; vkp=vkp->eval_to);
					node->key_id = vkp->id;
				}
				node->next_hash = -1;
				node->next = (VC_META_NODE **)NULL;
				++gotkey;
				key = vkeydef[j].id;
				break;
			}
			ch = *((unsigned char*)vkeydef[j].value+depth+1);		/* get nth byte */
			if (!strchr((char *)ch_list,(char)ch))				/* don't already have it */
				ch_list[k++]=ch;
		}
		if (!gotkey)
		{
			sprintf((char *)cur_seq,"%s%c",prev_seq,chars[i]);
			node = *(arr+i) = (VC_META_NODE*)gmem(sizeof(VC_META_NODE));
			node->ch= chars[i];
			node->key_id = 0;
			node->next_hash= k-1;
			node->next = build2_vc_node(ch_list,depth+1,cur_seq);
		}
		else gotkey=0;
	}
	qsort(arr,cnt,sizeof(VC_META_NODE*),(int(*)(const void *,const void *))vcm_sort_node);
	return arr;
	
}

VC_META_NODE **build2_vc_node(chars,depth,prev_seq)
unsigned char *chars;
int depth;
unsigned char *prev_seq;
{
	unsigned char ch_list[256];
  	int cnt,i,j,k,ch,gotkey,len,key;
	VC_META_NODE **arr,*node;
	unsigned char cur_seq[128];
	
	cnt=strlen((char *)chars);
	arr=(VC_META_NODE**)gmem(sizeof(VC_META_NODE*)*cnt);
	for (i=0; i<cnt; ++i)
	{
		for (gotkey=0,j=0,k=0,memset(ch_list,0,sizeof(ch_list));j<VC_KEY_COUNT+VC_KEY_EXTRA;++j)
		{
			if ( !vkeydef[j].value ) 
				continue;				/* null value, skip */
			len=strlen(vkeydef[j].value);
			if (depth>(len-1)) 
				continue;
			
			if (*((unsigned char *)vkeydef[j].value+depth)!=chars[i]) 
				continue;

			if (depth) 
				if (strncmp((char *)vkeydef[j].value,(char *)prev_seq,depth)) 
					continue;	/* char not equal */

			if (depth==(len-1))						/* a complete key */
			{
				node = *(arr+i) = (VC_META_NODE*)gmem(sizeof(VC_META_NODE));
				node->ch= chars[i];
				if (!vkeydef[j].eval_to) 
					node->key_id = vkeydef[j].id;
				else
				{
					for (vkp= &vkeydef[j]; vkp->eval_to; vkp=vkp->eval_to);
					node->key_id = vkp->id;
				}
				node->next_hash = -1;
				node->next = (VC_META_NODE **)NULL;
				++gotkey;
				key = vkeydef[j].id;
				break;
			}
			ch = *(vkeydef[j].value+depth+1);				/* get nth byte */
			if (!strchr((char *)ch_list,(char)ch))				/* don't already have it */
				ch_list[k++]=ch;
		}
		if (!gotkey)
		{
			sprintf((char *)cur_seq,"%s%c",prev_seq,chars[i]);
			node = *(arr+i) = (VC_META_NODE*)gmem(sizeof(VC_META_NODE));
			node->ch= chars[i];
			node->key_id = 0;
			node->next_hash= k-1;
			node->next = build_vc_node(ch_list,depth+1,cur_seq);
		}
		else gotkey=0;
	}
	qsort(arr,cnt,sizeof(VC_META_NODE*),(int(*)(const void *,const void *))vcm_sort_node);
	return arr;
	
}

static int vcm_sort_node(p1,p2)
VC_META_NODE **p1,**p2;
{
	return ((*p1)->ch)-((*p2)->ch);
}

/*
 * vcparm is no longer used under Unix only. Other OSs use it. since terminfo has been integrated, it was necessary to
 * choose either termcap style parameterized strings (which vcparm decodes) or 
 * terminfo style (which tparm decodes).  Since we could possibly have both types
 * at once (we load terminfo and videoinfo), we either need to convert the videocap (termcap style)
 * strings to terminfo or vise versa.  We are converting the videocap defs to terminfo style
 * and using the tparm instead of vcparm.  The routine vtctoti() converts the videocap strings
 * into a tparm digestible form.
 *
 */
#define VCPOUT(x) vcpbuf[opos++]=(x)
#define TOGGLEITEM(x) (x) = 1-(x)
char *vcparm(str,a1,a2)									/* instantiate a parameterized string */
char *str;										/* format string  */
int a1,a2;										/* integer arguments */
{
	static char vcpbuf[128];
	register int opos, ch, perc;
	int chs[2], item;
	char tmp[4], *p;
	
	chs[0]=a1;
	chs[1]=a2;
	perc=opos=item=0;
	while (ch = *str++)
	{
		switch (ch)
		{
		case '%': 
			if (perc) 
			{
				VCPOUT(ch);              /* is %% */
				perc=0;
			}
			else 
				perc++;                  /* % leadin */
			break;
		case '+':
			if (perc)                  /* is %+ */
			{
				ch = *str++;       /* get next */
				chs[item]+=ch;     /* add it */
				VCPOUT(chs[item]); /* output */
				TOGGLEITEM(item);      /* other item */
				perc=0;
			}
			else
				VCPOUT(ch);
			break;
		case '.':
			if (perc)
			{
				VCPOUT(chs[item]);
				TOGGLEITEM(item);
				perc=0;
			}
			else 
				VCPOUT(ch);
			break;
		case '2':
			if (perc)
			{
				sprintf(tmp,"%02d",chs[item]);
				VCPOUT(tmp[0]);
				VCPOUT(tmp[1]);
				TOGGLEITEM(item);
				perc=0;
			}
			else
				VCPOUT(ch);
			break;
		case '3':
			if (perc)
			{
				sprintf(tmp,"%03d",chs[item]);
				VCPOUT(tmp[0]);
				VCPOUT(tmp[1]);
				VCPOUT(tmp[2]);
				TOGGLEITEM(item);
				perc=0;
			}
			else
				VCPOUT(ch);
			break;
		case '>':
			if (perc)
			{
				tmp[0] = *str++;
				tmp[1] = *str++;
				if (chs[item]>tmp[0]) chs[item]+=tmp[1];
				VCPOUT(chs[item]);
				TOGGLEITEM(item);
				perc=0;
			}
			else
				VCPOUT(ch);
			break;
		case 'B':
			if (perc)
			{
				tmp[0] = chs[item];
				chs[item] = (tmp[0]/10)*16 + (tmp[0]%10);
				perc=0;
			}
			else
				VCPOUT(ch);
			break;
		case 'd':
			if (perc)
			{
			        sprintf(tmp,"%d",chs[item]);
				for (p=tmp; *p; ++p) VCPOUT(*p);
				TOGGLEITEM(item);
				perc=0;
			}
			else
				VCPOUT(ch);
			break;
		case 'i':
			if (perc)
			{
				++chs[0];
				++chs[1];
				perc=0;
			}
			else
				VCPOUT(ch);
			break;
		case 'n':
			if (perc)
			{
				chs[0] ^= 0140;
				chs[1] ^= 0140;
				perc=0;
			}
			else
				VCPOUT(ch);
			break;
		case 'r':
			if (perc)
			{
				TOGGLEITEM(item);
				perc=0;
			}
			else VCPOUT(ch);
			break;
		default:
			VCPOUT(ch);
		}
	}
        VCPOUT('\0');
	return vcpbuf;
}
int vgetm_timed(int seconds, int* status)
{
	int ch;

	vrawtimeout(seconds);
	ch = vgetmeta(meta_top.next,meta_top.next_hash);				/* get a new key, use top of tree */
	if (*status = vrawtimeout_check())
	{
		vrawtimeout_clear();
		ch = 0;
	}
	if (ch<0) 
		ch = pop();
	vrawtimeout(0);
	return ch;
}

int vgetm()											/* get a key, meta or normal */
{
	int ch;

	ch = vgetmeta(meta_top.next,meta_top.next_hash);				/* get a new key, use top of tree */
	if (ch<0) 
		ch = pop();

	return ch;
}

/* recursively called func to traverse the vc_meta tree starting point for current level */
static int vgetmeta(VC_META_NODE **node, int size)
{
        char vgetc();
	int ch,ret;
	static int reading_kbd=TRUE;
	register int low,high,median;

	ch=pop();
	if (ch<0) 
		ch=(unsigned char)vgetc();						/* grab a char from kbd */

	if (ch==(char)0)
	{
		if (vrawtimeout_check())
		{
			return -1;
		}
		else
		{
			ch = 0x80;
		}
	}
	for (low=0,high=size,meta_p=(VC_META_NODE *)NULL,meta_pp=node,median=(low+high)/2;
		(meta_pp[median]->ch) != ch && low<=high;)
	{
		median = (low+high)/2;
		if ((meta_pp[median]->ch)>ch) 
			high = median-1; 

		if ((meta_pp[median]->ch)<ch) 
			low = median+1;
	}
	if (meta_pp[median]->ch == ch) 
		meta_p = *(meta_pp+median);
       
	if (!meta_p)									/* no match */
	{
		push(ch);
		return -1;
	}
	if (meta_p->key_id)								/* got a key */
	{
		push(ST_ZED);								/* can discard keystrokes in the stack */
		return meta_p->key_id+VMBIAS;
	}
	ret = vgetmeta(meta_p->next,meta_p->next_hash);
	if (ret<0) 
		push(ch);
	return ret;
}
static int pop(void)
{
	if (cstackp>=0)
	{
		return cstack[cstackp--];
	}
	else return -1;
}

static int push(int ch)
{
	if (ch==ST_ZED) 
		cstackp = -1;
	else 
		cstack[++cstackp]=ch;
	return 0;
}

static int nextfield(FILE *fp, char* name, char* cap)
{
	char inbuf[256];
	int npos,cpos,part,len;
	char *status,*p;
	
	memset(inbuf,0,sizeof(inbuf));							/* zed field buffer */
	do 
	{
		if ((status=fgets(inbuf,sizeof(inbuf)-1,fp))==0) 
		{
			return 0;
		}
		++vcline;
		
		/* Remove # comment lines */
		if ('#' == inbuf[0] )
		{
			inbuf[0] = '\0';
		}
		
		/* Remove trailing whitespace */
		len = strlen(inbuf);
		while(len > 0 && (' '==inbuf[len-1] || '\t'==inbuf[len-1] || '\n'==inbuf[len-1]))
		{
			inbuf[--len] = '\0';
		}
		
	} while (len <= 0);								/* skipping comments and empty records */

	for (p=inbuf,npos=cpos=part=0; *p;)
	{
		/* 
		**	part==0  LHS	"name"
		**	part==1  RHS	"cap"
		*/
		if (part==0 && (*p=='#' || *p=='=') )		
		{
			++p;							/* Point past the "="				*/
			++part;							/* Switch to LHS "cap"				*/
			while (*p==' '||*p=='\t') ++p;				/* Skip whitespace after "="			*/
			continue;						/* continue in case we've got a NULL		*/
		}
		if (part==0) 
			name[npos++]= *p++;					/* Build the "name"				*/
		else 
			cap[cpos++]= *p++;					/* Build the "cap"				*/
	}
	name[npos]=(char)0;							
	cap[cpos]= (char)0;
	--cpos;
	--npos;
	while ( cap[cpos]==' '||  cap[cpos]=='\t')  cap[cpos--]=(char)0;	/* remove trailing whitespace			*/
	while (name[npos]==' '|| name[npos]=='\t') name[npos--]=(char)0;

	return TRUE;	
}

static int vcsort(p1,p2) vc_load *p1, *p2;
{
	return strcmp(p1->name,p2->name);
}

static char *vcstrdup(char *str)
{
	static char *mem = NULL;
	static char *cur;
	int len;
	char *ret;

	if (!mem)
	{
		cur=mem=(char *)calloc(VCMEMSIZE,sizeof(char));
		if (!mem) 
		{
			vre("VIDEO-C-VCNOMEM vcap can't calloc %d bytes",VCMEMSIZE);
			exit(0);
		}
	}

	if ( !str )
	{
		ret = NULL;
		return ret;
	}
	
	len=strlen(str)+1;
	ret = cur;
	cur += len;
	if (cur > mem+VCMEMSIZE)
	{
		vre("VIDEO-C-MEMUSED vcap used %d bytes. increase VCMEMSIZE",VCMEMSIZE);
		exit(0);
	}
	strcpy(ret,str);
	return ret;
}

static char *gmem(int size)
{
	char *ret;
	
	ret=(char *)calloc(size,sizeof(char));
	if (!ret) 
	{
		vre("VIDEO-C-VCNOMEM vcap can't calloc %d bytes",size);
		exit(0);
	}
	return ret;
}

static int matchcap(name)
unsigned char *name;
{
	register int low,high,median;

	for (low=0,high=sizeof(vc_load_defs)/sizeof(vc_load)-1,median=(low+high)/2;
	     strcmp((char *)name,vc_load_defs[median].name) && low<=high;
	     )
	{
		median=(low+high)/2;
		if (strcmp((char *)name,vc_load_defs[median].name)<0) high=median-1;
		if (strcmp((char *)name,vc_load_defs[median].name)>0) low=median+1;
	}
	if (strcmp((char *)name,vc_load_defs[median].name)==0) 
	{
		return median;
	}
	else 
	{
		vre("VIDEO-E-BADCAPNAME undefined field name [%s] at line %d",name,vcline);
		return -1;
	}
}

static void fix_seq(p)
char *p;
{
	char temp[100],*savep;
	int i,esc,circ;
	
	for (i=0,circ=esc=0,savep=p,memset(temp,0,sizeof(temp)); *p; ++p)
	{
		switch (*p)
		{
		case '^':
			if (!esc) circ++;
			else { temp[i++] = *p; esc=0; }
			break;
		case '\\':
			if (!esc) esc++;
			else { temp[i++] = *p; esc=0; }
			break;
		case 'E':
			if (esc) 
			{
				temp[i++] = 27;
				esc=0;
			}
			else if (circ)
			{
				temp[i++] = 'E' & 0xbf;
				circ=0;
			}
			else temp[i++] = *p;
			break;
		default:
			if (esc)
			{
				temp[i++] = *p;
				esc=0;
			}
			else if (circ)
			{
				temp[i++] = toupper(*p) & 0xbf;
				circ=0;
			}			
			else temp[i++] = *p;
		}
	}
	strcpy(savep,temp);
}
void vcloadsetup(flag)
int flag;
{
	vcloadbehavior = flag;
}
#ifdef unix
static int vcloadterminfo()
{
	int rc;
	int size;
	char tmpbuf[1024];
	char *trm;
	
	trm=getenv("TERM");
	setupterm(trm,0,&rc);
	if (rc==1)
	{
		if (cursor_address)
		{
			vcapdef[CURSOR_ADDRESS] = vcstrdup((char*) (cursor_address));
			fix_seq(vcapdef[CURSOR_ADDRESS]);
		}
		vcapdef[INIT_TERMINAL] = gmem(1024);
		vcapdef[RESET_TERMINAL] = gmem(1024);		
		if (init_1string)
		{
			strcat(vcapdef[INIT_TERMINAL],init_1string);
		}
		if (init_2string)
		{
			strcat(vcapdef[INIT_TERMINAL],init_2string);
		}
		if (init_3string)
		{
			strcat(vcapdef[INIT_TERMINAL],init_3string);
		}
		if (enter_ca_mode)
		{
			strcat(vcapdef[INIT_TERMINAL],enter_ca_mode);
		}
		if (!init_1string && !init_2string && !init_3string && !enter_ca_mode)
		{
			if (reset_1string)
			{
				strcat(vcapdef[INIT_TERMINAL],reset_1string);
			}
			if (reset_2string)
			{
				strcat(vcapdef[INIT_TERMINAL],reset_2string);
			}
			if (reset_3string)
			{
				strcat(vcapdef[INIT_TERMINAL],reset_3string);
			}
		}
		if (keypad_xmit)
		{
			strcat(vcapdef[INIT_TERMINAL],keypad_xmit);
		}
		if (reset_1string)
		{
			strcat(vcapdef[RESET_TERMINAL],reset_1string);
		}
		if (reset_2string)
		{
			strcat(vcapdef[RESET_TERMINAL],reset_2string);
		}
		if (reset_3string)
		{
			strcat(vcapdef[RESET_TERMINAL],reset_3string);
		}
		if (exit_ca_mode)
		{
			strcat(vcapdef[RESET_TERMINAL],exit_ca_mode);
		}
		if (keypad_local)
		{
			strcat(vcapdef[RESET_TERMINAL],keypad_local);
		}
		
#define VC_COPY_TI(def,cap) if (cap) vcapdef[def] = vcstrdup((char *)cap); else vcapdef[def]=""

		VC_COPY_TI(SAVE_CURSOR,save_cursor);
		VC_COPY_TI(RESTORE_CURSOR,restore_cursor);
		VC_COPY_TI(CHANGE_SCROLL_REGION,change_scroll_region);

		if (clear_screen)
		{
			char *p;
			
			p=vcstrdup(clear_screen);
			vcapdef[CLR_SCREEN]=p;
			
		}
		
#ifdef clr_bol
		VC_COPY_TI(CLEAR_BOL,clr_bol);
#endif
		VC_COPY_TI(CLEAR_EOL,clr_eol);
		VC_COPY_TI(CLEAR_EOS,clr_eos);
		VC_COPY_TI(CURSOR_DOWN,cursor_down);
		VC_COPY_TI(CURSOR_UP,cursor_up);
		VC_COPY_TI(CURSOR_LEFT,cursor_left);
		VC_COPY_TI(CURSOR_RIGHT,cursor_right);
		VC_COPY_TI(CURSOR_HOME,cursor_home);
		if (cursor_visible)
		{
			VC_COPY_TI(CURSOR_VISIBLE,cursor_visible);
		}
		else
		{
			VC_COPY_TI(CURSOR_VISIBLE,cursor_normal);
		}
		VC_COPY_TI(CURSOR_INVISIBLE,cursor_invisible);		
		VC_COPY_TI(ENTER_BLINK_MODE,enter_blink_mode);
		VC_COPY_TI(ENTER_BOLD_MODE,enter_bold_mode);
		VC_COPY_TI(ENTER_REVERSE_MODE,enter_reverse_mode);
		VC_COPY_TI(ENTER_STANDOUT_MODE,enter_standout_mode);
		VC_COPY_TI(ENTER_UNDERLINE_MODE,enter_underline_mode);		
		VC_COPY_TI(EXIT_ATTRIBUTE_MODE,exit_attribute_mode);
		VC_COPY_TI(SCROLL_REVERSE,scroll_reverse);
		if (strncmp(trm,"vt1",3)==0||
		    strncmp(trm,"vt2",3)==0||
		    strncmp(trm,"vt3",3)==0)
		{
			/* use vt style default line and pblanks stuff here */
		}
		else
		{
			/* use regular ascii chars and/or flag to warn */
		}
		VC_COPY_TI(REVERSE_INDEX,scroll_reverse);
		VC_COPY_TI(ENTER_GRAPHICS_MODE,enter_alt_charset_mode);
		VC_COPY_TI(EXIT_GRAPHICS_MODE,exit_alt_charset_mode);		
		
		vcapdef[GRAPHSTR] = gmem(14);

		vcapdef[NARROW_MODE]="";
		vcapdef[WIDE_MODE]="";
                vcapdef[SCREEN_NORMAL_MODE]="";
	        vcapdef[SCREEN_REVERSE_MODE]="";
		vcapdef[ENTER_AM_MODE]="";
		vcapdef[EXIT_AM_MODE]="";
		
		VC_COPY_TI(ENTER_INSERT_MODE,enter_insert_mode);
		VC_COPY_TI(EXIT_INSERT_MODE,exit_insert_mode);
		

#ifdef box_chars_1
		if (box_chars_1)
		{
			vcapdef[GRAPHSTR][SINGLE_UPPER_LEFT_CORNER]  = box_chars_1[0];
			vcapdef[GRAPHSTR][SINGLE_HORIZONTAL_BAR]     = box_chars_1[1];
			vcapdef[GRAPHSTR][SINGLE_UPPER_RIGHT_CORNER] = box_chars_1[2];
			vcapdef[GRAPHSTR][SINGLE_VERTICAL_BAR]       = box_chars_1[3];
			vcapdef[GRAPHSTR][SINGLE_LOWER_RIGHT_CORNER] = box_chars_1[4];
			vcapdef[GRAPHSTR][SINGLE_LOWER_LEFT_CORNER]  = box_chars_1[5];
			vcapdef[GRAPHSTR][SINGLE_UPPER_TEE]          = box_chars_1[6];
			vcapdef[GRAPHSTR][SINGLE_RIGHT_TEE]          = box_chars_1[7];
			vcapdef[GRAPHSTR][SINGLE_LOWER_TEE]          = box_chars_1[8];
			vcapdef[GRAPHSTR][SINGLE_LEFT_TEE]           = box_chars_1[9];
			vcapdef[GRAPHSTR][SINGLE_CROSS]              = box_chars_1[10];
		}
		else
		{
			vcapdef[GRAPHSTR][SINGLE_UPPER_LEFT_CORNER]  = '*';
			vcapdef[GRAPHSTR][SINGLE_HORIZONTAL_BAR]     = '-';
			vcapdef[GRAPHSTR][SINGLE_UPPER_RIGHT_CORNER] = '*';
			vcapdef[GRAPHSTR][SINGLE_VERTICAL_BAR]       = '|';
			vcapdef[GRAPHSTR][SINGLE_LOWER_RIGHT_CORNER] = '*';
			vcapdef[GRAPHSTR][SINGLE_LOWER_LEFT_CORNER]  = '*';
			vcapdef[GRAPHSTR][SINGLE_UPPER_TEE]          = '*';
			vcapdef[GRAPHSTR][SINGLE_RIGHT_TEE]          = '*';
			vcapdef[GRAPHSTR][SINGLE_LOWER_TEE]          = '*';
			vcapdef[GRAPHSTR][SINGLE_LEFT_TEE]           = '*';
			vcapdef[GRAPHSTR][SINGLE_CROSS]              = '*';
			vcapdef[ENTER_GRAPHICS_MODE] = "";
			vcapdef[EXIT_GRAPHICS_MODE] = "";
		}
#else
		vcapdef[GRAPHSTR][SINGLE_UPPER_LEFT_CORNER]  = '*';
		vcapdef[GRAPHSTR][SINGLE_HORIZONTAL_BAR]     = '-';
		vcapdef[GRAPHSTR][SINGLE_UPPER_RIGHT_CORNER] = '*';
		vcapdef[GRAPHSTR][SINGLE_VERTICAL_BAR]       = '|';
		vcapdef[GRAPHSTR][SINGLE_LOWER_RIGHT_CORNER] = '*';
		vcapdef[GRAPHSTR][SINGLE_LOWER_LEFT_CORNER]  = '*';
		vcapdef[GRAPHSTR][SINGLE_UPPER_TEE]          = '*';
		vcapdef[GRAPHSTR][SINGLE_RIGHT_TEE]          = '*';
		vcapdef[GRAPHSTR][SINGLE_LOWER_TEE]          = '*';
		vcapdef[GRAPHSTR][SINGLE_LEFT_TEE]           = '*';
		vcapdef[GRAPHSTR][SINGLE_CROSS]              = '*';
		vcapdef[ENTER_GRAPHICS_MODE] = "";
		vcapdef[EXIT_GRAPHICS_MODE] = "";
#endif
		set_terminfo_key("kd", VKEY_DOWN_ARROW, key_down);
		set_terminfo_key("ku", VKEY_UP_ARROW, key_up);
		set_terminfo_key("kl", VKEY_LEFT_ARROW, key_left);
		set_terminfo_key("kr", VKEY_RIGHT_ARROW, key_right);

		set_terminfo_key("k1", VKEY_F1, key_f1);
		set_terminfo_key("k2", VKEY_F2, key_f2);
		set_terminfo_key("k3", VKEY_F3, key_f3);
		set_terminfo_key("k4", VKEY_F4, key_f4);
		set_terminfo_key("k5", VKEY_F5, key_f5);
		set_terminfo_key("k6", VKEY_F6, key_f6);
		set_terminfo_key("k7", VKEY_F7, key_f7);
		set_terminfo_key("k8", VKEY_F8, key_f8);
		set_terminfo_key("k9", VKEY_F9, key_f9);
		set_terminfo_key("k;", VKEY_F10, key_f10);
		set_terminfo_key("F1", VKEY_F11, key_f11);
		set_terminfo_key("F2", VKEY_F12, key_f12);
		set_terminfo_key("F3", VKEY_F13, key_f13);
		set_terminfo_key("F4", VKEY_F14, key_f14);
		set_terminfo_key("F5", VKEY_F15, key_f15);
		set_terminfo_key("F6", VKEY_F16, key_f16);
		set_terminfo_key("F7", VKEY_F17, key_f17);
		set_terminfo_key("F8", VKEY_F18, key_f18);
		set_terminfo_key("F9", VKEY_F19, key_f19);
		set_terminfo_key("FA", VKEY_F20, key_f20);
		set_terminfo_key("FB", VKEY_F21, key_f21);
		set_terminfo_key("FC", VKEY_F22, key_f22);
		set_terminfo_key("FD", VKEY_F23, key_f23);
		set_terminfo_key("FE", VKEY_F24, key_f24);
		set_terminfo_key("FF", VKEY_F25, key_f25);
		set_terminfo_key("FG", VKEY_F26, key_f26);
		set_terminfo_key("FH", VKEY_F27, key_f27);
		set_terminfo_key("FI", VKEY_F28, key_f28);
		set_terminfo_key("FJ", VKEY_F29, key_f29);
		set_terminfo_key("FK", VKEY_F30, key_f30);
		set_terminfo_key("FL", VKEY_F31, key_f31);
		set_terminfo_key("FM", VKEY_F32, key_f32);

		set_terminfo_key("kP", VKEY_PREV_SCR, key_ppage);
		set_terminfo_key("kN", VKEY_NEXT_SCR, key_npage);
		set_terminfo_key("ic", VKEY_INSERT, key_ic);

		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

static void set_terminfo_key(char *id, int index, char *value)
{
	char *ptr;

	if ( ptr = tgetstr(id, NULL) )
	{
		vc_add_key(index,value,NOMAPPING,SRC_TERMINFO);
	}
	else
	{
		vc_add_key(index,NULL,NOMAPPING,SRC_TERMINFO);
	}
}

#endif /* unix */

char GetASCIIGraphicsChar( unsigned char graph_char )
{
	if ( graph_char == vcapdef[GRAPHSTR][SINGLE_UPPER_LEFT_CORNER] ) return('*');
	else if ( graph_char == vcapdef[GRAPHSTR][SINGLE_HORIZONTAL_BAR] ) return('-');
	else if ( graph_char == vcapdef[GRAPHSTR][SINGLE_UPPER_RIGHT_CORNER] ) return('*');
	else if ( graph_char == vcapdef[GRAPHSTR][SINGLE_VERTICAL_BAR] ) return('|');
	else if ( graph_char == vcapdef[GRAPHSTR][SINGLE_LOWER_RIGHT_CORNER] ) return('*');
	else if ( graph_char == vcapdef[GRAPHSTR][SINGLE_LOWER_LEFT_CORNER] ) return('*');
	else if ( graph_char == vcapdef[GRAPHSTR][SINGLE_UPPER_TEE] ) return('*');
	else if ( graph_char == vcapdef[GRAPHSTR][SINGLE_RIGHT_TEE] ) return('*');
	else if ( graph_char == vcapdef[GRAPHSTR][SINGLE_LOWER_TEE] ) return('*');
	else if ( graph_char == vcapdef[GRAPHSTR][SINGLE_LEFT_TEE] ) return('*');
	else if ( graph_char == vcapdef[GRAPHSTR][SINGLE_CROSS] ) return('*');
	else if ( graph_char == vcapdef[GRAPHSTR][LEFT_POINTER] ) return('<');
	else if ( graph_char == vcapdef[GRAPHSTR][RIGHT_POINTER] ) return('>');
	else	return(' ');
}

char *vtctoti(char* strin)
{
	static char buffer[128];
	char tmpbuf[128];
	int parm=0, rflag=FALSE;
	register char *scanidx;
	
	for (scanidx=strin; *scanidx; ++scanidx)
	{
		if (*scanidx == '%') 
		{
			scanidx++;
			switch (*scanidx) 
			{
			case 'r':
				rflag = TRUE;
				break;
			default:
				break;	/* ignore */
			}
		}
	}

	buffer[0]=(char)0;
	while (*strin) 
	{
		switch (*strin) 
		{
		case '%':
			strin++;
			switch (*strin) 
			{
			case '%':
				strcat(buffer,"%%");
				break;
			case 'i':
				strcat(buffer,"%i");
				break;			
			case 'd':
				parm++;
				if ((rflag) && (parm <= 2)) 
				{
					if (parm == 1)
					{
						strcat(buffer,"%p2%d");
					}
					else
					{
						strcat(buffer,"%p1%d");
					}
				}
				else
				{
					sprintf(tmpbuf,"%%p%d%%d", parm);
					strcat(buffer,tmpbuf);
				}
				break;
			case '2':
				parm++;
				if ((rflag) && (parm <= 2)) 
				{
					if (parm == 1)
					{
						strcat(buffer,"%p2%02d");
					}
					else
					{
						strcat(buffer,"%p1%02d");
					}
				}
				else
				{
					sprintf(tmpbuf,"%%p%d%%02d", parm);
					strcat(buffer,tmpbuf);
				}
				break;
			case '3':
				parm++;
				if ((rflag) && (parm <= 2)) 
				{
					if (parm == 1)
					{
						strcat(buffer,"%p2%03d");
					}
					else
					{
						strcat(buffer,"%p1%03d");
					}
				}
				else
				{
					sprintf(tmpbuf,"%%p%d%%03d", parm);
					strcat(buffer,tmpbuf);
				}
				break;
			case '.':
				parm++;
				if ((rflag) && (parm <= 2)) 
				{
					if (parm == 1)
					{
						strcat(buffer,"%p2%c");
					}
					else
					{
						strcat(buffer,"%p1%c");
					}
				}
				else
				{
					sprintf(tmpbuf,"%%p%d%%c", parm);
					strcat(buffer,tmpbuf);
				}
				break;
			case '+':
				strin++;
				parm++;
				if ((rflag) && (parm <= 2)) 
				{
					if (parm == 1)
					{
						sprintf(tmpbuf,"%%p2%%'%c'%%+%%c", *strin);
						strcat(buffer,tmpbuf);
					}
					else
					{
						sprintf(tmpbuf,"%%p1%%'%c'%%+%%c", *strin);
						strcat(buffer,tmpbuf);
					}
				}
				else
				{
					sprintf(tmpbuf,"%%p%d%%'%c'%%+%%c", parm, *strin);
					strcat(buffer,tmpbuf);
				}
				break;
			default:
				break;
			}
			++strin;
			break;
		default:
			tmpbuf[0]= *strin;
			tmpbuf[1]= (char)0;
			strcat(buffer,tmpbuf);
			++strin;
			break;
		}
	}
	return buffer;
}

	
/*
**	The following are support routines
*/

int vcapnull( control_string, cap_string, dispfl)					/* Test if control is defined and warn	*/
char *control_string;
char *cap_string;
int dispfl;
{
	if ( ! control_string || ! *control_string )
	{
		if (dispfl) vre("%%VIDEOCAP-W-NOTDEFINED Capability %s is not defined",cap_string);
		return( 1 );
	}
	else return(0);
}


/*
**	Routine:	vexists()
**
**	Function:	To test if a file or directory exists
**
**	Description:	Routine stat() is used to check if the file exists.
**			This will use the effective UID not the real UID.
**
**	Arguments:
**	name		The file name.
**
**	Return:
**	1		The file exists
**	0		The file does not exist
**
**
*/
int vexists(const char* name)
{
	struct stat buf;

	return((0==stat(name,&buf)) ? 1:0);
}



/*
**	History:
**	$Log: vcap.c,v $
**	Revision 1.40  2001-10-12 16:08:03-04  gsl
**	Change writeing errors to stderr into calls to vre() that now get logged.
**
**	Revision 1.39  2001-10-12 14:50:56-04  gsl
**	Fix nextfield() to properly handle (ignore) blank lines.
**	Remove OLD code
**
**	Revision 1.38  2001-10-11 13:38:13-04  gsl
**	Add support for .vcap extension
**
**	Revision 1.37  2001-10-11 11:22:06-04  gsl
**	Remove VMS & MSDOS
**
**	Revision 1.36  1999-02-17 15:56:36-05  gsl
**	make vgetmeta() a static routine.
**
**	Revision 1.35  1999-01-19 10:25:33-05  gsl
**	Fix the curses vline problem for AIX
**
**	Revision 1.34  1997-09-25 10:31:52-04  scass
**	Added fix for vline defined in curses conflicting with
**	video's vline().  Needed for DG-UX Intel port.
**
**	Revision 1.33  1997-08-25 13:51:32-04  gsl
**	Change the WIN32 pfkeys defaults to use the new scan codes.
**	Add facility to dump the metakeys tables to vcapdump.txt when
**	the environment variable VCAPMETADUMP is set
**
**	Revision 1.32  1997-07-12 17:31:36-04  gsl
**	FIx memory overwrite with the_pseudo_blanks and the_graphstr.
**
**	Revision 1.31  1997-07-08 16:39:28-04  gsl
**	Localize all the vcapdef and vkeydef variables.
**	Add support for COSTAR with WIN32
**	Add validation of pseudo_blanks and graphstr capabilites.
**	Add vcapterm() routine to get the terminal type
**
**	Revision 1.30  1997-06-06 14:00:47-04  scass
**	Changed _AIX t0 AIX  also changed type of id to a
**	const so will not get warning for tgetstr().
**
**	Revision 1.27  1997-04-30 08:56:44-04  scass
**	Added prototype for tgetstr() because not in
**	curses.h for AIX.
**
**	Revision 1.26  1997-04-29 16:33:49-04  scass
**	Changed way terminfo is used to set key value.
**	Fix for VNOTERMINFO problem.
**
**	Revision 1.25  1997-02-17 10:34:16-05  gsl
**	Made vcloadterminfo() static
**
**	Revision 1.24  1996-11-12 12:58:26-05  jockc
**	missed one of the vtimeout functions
**
**	Revision 1.23  1996-11-11 15:05:27-08  jockc
**	change vtimeout_* functions to vrawtimeout_*.. removed decl
**	of vtimeout_clear (it's in vmodules.h)
**
**	Revision 1.22  1996-10-11 15:15:59-07  gsl
**	drcs update
**
**
**
*/
