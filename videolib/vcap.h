			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1991				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/


#ifndef VCAP_HEADER
#define VCAP_HEADER

#ifdef MSDOS
#define DEFWISPCONFIG "\\WISP\\CONFIG"
#define DEFTERM "MS-DOS"
#define VCAPEXT ".VC"
#else	/* VMS or unix */
#define DEFWISPCONFIG "/usr/wisp/config"
#define DEFTERM "vt100"
#endif	/* VMS or unix */

#define ISKEY  1
#define ISCAP  2
#define ISBOOL 3
#define ISNUM  4

/*	Define the indexes into the graphics string graphstr definition. Never remove, only and to the end.			*/

#define SINGLE_VERTICAL_BAR		0
#define SINGLE_HORIZONTAL_BAR		1
#define SINGLE_UPPER_LEFT_CORNER	2
#define SINGLE_UPPER_RIGHT_CORNER	3
#define SINGLE_LOWER_LEFT_CORNER	4
#define SINGLE_LOWER_RIGHT_CORNER	5
#define SINGLE_UPPER_TEE		6
#define SINGLE_LOWER_TEE		7
#define SINGLE_LEFT_TEE			8
#define SINGLE_RIGHT_TEE		9
#define SINGLE_CROSS		       10
#define LEFT_POINTER		       11
#define RIGHT_POINTER		       12

/*	The defines for video CAPABILITIES ***** Only add to the end. Don't delete any. *****					*/

#define BACKSPACE 		1
#define AUTO_MARGINS 		2
#define HAS_HW_TABS 		3
#define NL_IGN_AFTER_WRAP 	4
#define MOVE_STANDOUT_MODE 	5
#define INIT_FILE 	   	6
#define RESET_TERMINAL 		7
#define ENTER_KP_XMIT 		8
#define EXIT_KP_XMIT 		9
#define TAB 			10
#define VIRTUAL_TERM_NUM 	11
#define SAVE_CURSOR 		12
#define RESTORE_CURSOR 		13
#define BELL 			14
#define BACK_TAB 		15
#define CARRIAGE_RETURN 	16
#define CHANGE_SCROLL_REGION 	17
#define CLEAR_ALL_TABS 		18
#define CLR_SCREEN 		19
#define CLEAR_BOL 		20
#define CLEAR_EOL 		21
#define CLEAR_EOS 	     	22
#define CURSOR_ADDRESS 		23
#define CURSOR_DOWN 		24
#define CURSOR_HOME 		25
#define CURSOR_INVISIBLE      	26
#define CURSOR_VISIBLE 		27
#define CURSOR_LEFT 		28
#define CURSOR_NORMAL 		29
#define CURSOR_RIGHT 		30	
#define CURSOR_UP 		31
#define DELETE_CHAR 		32	
#define DELETE_LINE 		33	
#define ENTER_ALT_CHARSET 	34
#define ENTER_AM_MODE 	     	35
#define ENTER_BLINK_MODE 	36
#define ENTER_BOLD_MODE 	37
#define ENTER_DELETE_MODE 	38
#define ENTER_DIM_MODE 		39
#define ENTER_INSERT_MODE 	40
#define ENTER_PROTECTED_MODE 	41
#define ENTER_REVERSE_MODE 	42
#define ENTER_SECURE_MODE 	43
#define ENTER_STANDOUT_MODE 	44
#define ENTER_UNDERLINE_MODE 	45
#define EXIT_ALT_CHARSET 	46
#define EXIT_AM_MODE 		47
#define EXIT_ATTRIBUTE_MODE 	48
#define EXIT_DELETE_MODE 	49
#define EXIT_INSERT_MODE      	50
#define EXIT_STANDOUT_MODE 	51
#define EXIT_UNDERLINE_MODE 	52
#define INIT_TERMINAL 		53
#define INSERT_CHAR 		54
#define INSERT_LINE 		55
#define OTHER_KEYS 		56
#define NEWLINE 		57
#define SCROLL_REVERSE 		58
#define COLUMNS 		59
#define LINES 			60
#define SO_BLANKS 		61
#define US_BLANKS 		62
#define GRAPHSTR                63
#define ENTER_GRAPHICS_MODE     64
#define EXIT_GRAPHICS_MODE      65
#define PAD                     66
#define PSEUDO_BLANKS           67
#define INDEX                   68
#define REVERSE_INDEX           69
#define WIDE_MODE               70
#define NARROW_MODE             71
#define SCREEN_NORMAL_MODE	72
#define SCREEN_REVERSE_MODE	73
#define COLOR_LOOKUP_TABLE_0	74
#define COLOR_LOOKUP_TABLE_1	75
#define COLOR_LOOKUP_TABLE_2	76
#define COLOR_LOOKUP_TABLE_3	77
#define COLOR_LOOKUP_TABLE_4	78
#define COLOR_LOOKUP_TABLE_5	79
#define COLOR_LOOKUP_TABLE_6	80
#define COLOR_LOOKUP_TABLE_7	81
#define COLOR_LOOKUP_TABLE_8	82
#define COLOR_LOOKUP_TABLE_9	83

#define VC_CAP_COUNT	COLOR_LOOKUP_TABLE_9

/*	The defines for FUNCTION KEYS ***** Only add to end. Never delete any. *****						*/

#define KEY_F0 			1
#define KEY_F1 			2
#define KEY_F2 			3
#define KEY_F3 			4
#define KEY_F4 			5
#define KEY_F5 			6
#define KEY_F6 			7
#define KEY_F7 			8
#define KEY_F8 			9
#define KEY_F9 			10
#define KEY_F10			11
#define KEY_F11			12
#define KEY_F12			13
#define KEY_F13			14
#define KEY_F14			15
#define KEY_F15			16
#define KEY_F16			17
#define KEY_F17			18
#define KEY_F18			19
#define KEY_F19			20
#define KEY_F20			21
#define KEY_F21			22
#define KEY_F22			23
#define KEY_F23			24
#define KEY_F24			25
#define KEY_F25			26
#define KEY_F26			27
#define KEY_F27			28
#define KEY_F28			29
#define KEY_F29			30
#define KEY_F30                 31
#define KEY_F31                 32
#define KEY_F32                 33
#define KEY_HOME 		34
#define KEY_BACKSPACE 		35
#define KEY_DOWN_ARROW 		36
#define KEY_LEFT_ARROW 		37
#define KEY_RIGHT_ARROW 	38
#define KEY_UP_ARROW 		39
#define GENERIC_PF1 		40
#define GENERIC_PF2 		41
#define GENERIC_PF3 		42
#define GENERIC_PF4 		43
#define GENERIC_PF5 		44
#define GENERIC_PF6 		45
#define GENERIC_PF7 		46
#define GENERIC_PF8 		47
#define GENERIC_PF9 		48
#define GENERIC_PF10 		49
#define GENERIC_PF11 		50
#define GENERIC_PF12 		51
#define GENERIC_PF13 		52
#define GENERIC_PF14 		53
#define GENERIC_PF15 		54
#define GENERIC_PF16 		55
#define GENERIC_PF17 		56
#define GENERIC_PF18 		57
#define GENERIC_PF19 		58
#define GENERIC_PF20 		59
#define GENERIC_PF21 		60
#define GENERIC_PF22 		61
#define GENERIC_PF23 		62
#define GENERIC_PF24 		63
#define GENERIC_PF25 		64
#define GENERIC_PF26 		65
#define GENERIC_PF27 		66
#define GENERIC_PF28 		67
#define GENERIC_PF29 		68
#define GENERIC_PF30 		69
#define GENERIC_PF31 		70
#define GENERIC_PF32 		71
#define GENERIC_HOME 		72
#define GENERIC_HELP 		73
#define GENERIC_UP 		74
#define GENERIC_DOWN 		75
#define GENERIC_LEFT 		76
#define GENERIC_RIGHT 		77
#define GENERIC_TAB 		78
#define GENERIC_DELETE 		79
#define GENERIC_INSERT 		80
#define GENERIC_NEXT_SCR 	81
#define GENERIC_PREV_SCR 	82
#define KEY_DELETE              83
#define KEY_INSERT              84
#define KEY_NEXT_SCR            85
#define KEY_PREV_SCR            86
#define KEY_SELECT              87
#define GENERIC_BACKTAB         88
#define GENERIC_REMOVE          89
#define GENERIC_SELECT          90
#define GENERIC_CANCEL          91
#define GENERIC_RETURN          92 
#define GENERIC_ENTER           93
#define GENERIC_REFRESH         94
#define KEY_TAB                 95
#define KEY_HELP                96
#define KEY_DO                  97
#define KEY_FIND                98
#define KEY_USER1               99
#define KEY_USER2		100
#define KEY_USER3               101
#define KEY_USER4               102
#define KEY_USER5               103
#define KEY_USER6               104
#define KEY_USER7               105
#define KEY_USER8               106
#define GENERIC_CLEAR_FIELD     107
#define GENERIC_CLEAR_AFTER     108
#define GENERIC_CLEAR_BEFORE    109
#define GENERIC_NULL            110
#define TRIGGER1                111
#define TRIGGER2                112
#define TRIGGER3                113
#define TRIGGER4                114
#define TRIGGER5                115
#define TRIGGER6                116
#define TRIGGER7                117
#define TRIGGER8                118
#define KEY_F33 		119
#define KEY_F34 		120
#define KEY_F35 		121
#define KEY_F36 		122
#define KEY_F37 		123
#define KEY_F38 		124
#define KEY_F39 		125
#define KEY_F40 		126
#define KEY_F41 		127 
#define KEY_F42 		128 
#define KEY_F43 		129
#define KEY_F44 		130
#define KEY_F45 		131
#define KEY_F46 		132
#define KEY_F47 		133
#define KEY_F48 		134
#define KEY_F49 		135
#define KEY_F50 		136 
#define KEY_F51 		137
#define KEY_F52 		138
#define KEY_F53 		139 
#define KEY_F54 		140
#define KEY_F55 		141
#define KEY_F56 		142
#define KEY_F57 		143
#define KEY_F58 		144
#define KEY_F59 		145
#define KEY_F60 		146
#define KEY_F61 		147
#define KEY_F62 		148
#define KEY_F63 		149
#define KEY_F64 		150
#define GENERIC_PF33 		151
#define GENERIC_PF34 		152
#define GENERIC_PF35 		153
#define GENERIC_PF36 		154
#define GENERIC_PF37 		155
#define GENERIC_PF38 		156
#define GENERIC_PF39 		157
#define GENERIC_PF40 		158
#define GENERIC_PF41 		159
#define GENERIC_PF42 		160
#define GENERIC_PF43 		161
#define GENERIC_PF44 		162
#define GENERIC_PF45 		163
#define GENERIC_PF46 		164
#define GENERIC_PF47 		165
#define GENERIC_PF48 		166
#define GENERIC_PF49 		167
#define GENERIC_PF50 		168
#define GENERIC_PF51 		169
#define GENERIC_PF52 		170
#define GENERIC_PF53 		171
#define GENERIC_PF54 		172
#define GENERIC_PF55 		173
#define GENERIC_PF56 		174
#define GENERIC_PF57 		175
#define GENERIC_PF58 		176
#define GENERIC_PF59 		177
#define GENERIC_PF60 		178
#define GENERIC_PF61 		179
#define GENERIC_PF62 		180
#define GENERIC_PF63 		181
#define GENERIC_PF64 		182
#define GENERIC_NEWLINE         183
#define SHIFT_F1		184
#define SHIFT_F2		185
#define SHIFT_F3		186
#define SHIFT_F4		187
#define SHIFT_F5		188
#define SHIFT_F6		189
#define SHIFT_F7		190
#define SHIFT_F8		191
#define SHIFT_F9		192
#define SHIFT_F10		193
#define SHIFT_F11		194
#define SHIFT_F12		195
#define CTRL_F1			196
#define CTRL_F2			197
#define CTRL_F3			198
#define CTRL_F4			199
#define CTRL_F5			200
#define CTRL_F6			201
#define CTRL_F7			202
#define CTRL_F8			203
#define CTRL_F9			204
#define CTRL_F10		205
#define CTRL_F11		206
#define CTRL_F12		207
#define ALT_F1			208
#define ALT_F2			209
#define ALT_F3			210
#define ALT_F4			211
#define ALT_F5			212
#define ALT_F6			213
#define ALT_F7			214
#define ALT_F8			215
#define ALT_F9			216
#define ALT_F10			217
#define ALT_F11			218
#define ALT_F12			219
					/* Add new keys here, before VC_KEY_COUNT, and continue numbering as above.		*/
#define VC_KEY_COUNT 		220	/* This one is NOT a Video Cap KEY, renumber it to be the last entry before VC_KEY_LAST	*/

#define VC_KEY_LAST 		VC_KEY_COUNT
#define VC_KEY_EXTRA 		256

#define VMBIAS 			128

typedef struct 
{
	char *name;
	int type;
	int index;

} vc_load;

/*
	The following defines the keywords in the VIDEOCAP file and equates them to the above defines for both
	capabilities and function keys.

	The order of the following is not significant.
*/
#ifdef VCAP
vc_load vc_load_defs[] =
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

	{ "key_f0", 			ISKEY,	KEY_F0 },
	{ "key_f1", 			ISKEY,	KEY_F1 },
	{ "key_f2", 			ISKEY,	KEY_F2 },
	{ "key_f3", 			ISKEY,	KEY_F3 },
	{ "key_f4", 			ISKEY,	KEY_F4 },
	{ "key_f5", 			ISKEY,	KEY_F5 },
	{ "key_f6", 			ISKEY,	KEY_F6 },
	{ "key_f7", 			ISKEY,	KEY_F7 },
	{ "key_f8", 			ISKEY,	KEY_F8 },
	{ "key_f9", 			ISKEY,	KEY_F9 },
	{ "key_f10", 			ISKEY,	KEY_F10 },
	{ "key_f11", 			ISKEY,	KEY_F11 },
	{ "key_f12", 			ISKEY,	KEY_F12 },
	{ "key_f13", 			ISKEY,	KEY_F13 },
	{ "key_f14", 			ISKEY,	KEY_F14 },
	{ "key_f15", 			ISKEY,	KEY_F15 },
	{ "key_f16", 			ISKEY,	KEY_F16 },
	{ "key_f17", 			ISKEY,	KEY_F17 },
	{ "key_f18", 			ISKEY,	KEY_F18 },
	{ "key_f19", 			ISKEY,	KEY_F19 },
	{ "key_f20", 			ISKEY,	KEY_F20 },
	{ "key_f21", 			ISKEY,	KEY_F21 },
	{ "key_f22", 			ISKEY,	KEY_F22 },
	{ "key_f23", 			ISKEY,	KEY_F23 },
	{ "key_f24", 			ISKEY,	KEY_F24 },
	{ "key_f25", 			ISKEY,	KEY_F25 },
	{ "key_f26", 			ISKEY,	KEY_F26 },
	{ "key_f27", 			ISKEY,	KEY_F27 },
	{ "key_f28", 			ISKEY,	KEY_F28 },
	{ "key_f29", 			ISKEY,	KEY_F29 },
	{ "key_f30", 			ISKEY,	KEY_F30 },
	{ "key_f31", 			ISKEY,	KEY_F31 },
	{ "key_f32", 			ISKEY,	KEY_F32 },
	{ "key_f33", 			ISKEY,	KEY_F33 },
	{ "key_f34", 			ISKEY,	KEY_F34 },
	{ "key_f35", 			ISKEY,	KEY_F35 },
	{ "key_f36", 			ISKEY,	KEY_F36 },
	{ "key_f37", 			ISKEY,	KEY_F37 },
	{ "key_f38", 			ISKEY,	KEY_F38 },
	{ "key_f39", 			ISKEY,	KEY_F39 },
	{ "key_f40", 			ISKEY,	KEY_F40 },
	{ "key_f41", 			ISKEY,	KEY_F41 },
	{ "key_f42", 			ISKEY,	KEY_F42 },
	{ "key_f43", 			ISKEY,	KEY_F43 },
	{ "key_f44", 			ISKEY,	KEY_F44 },
	{ "key_f45", 			ISKEY,	KEY_F45 },
	{ "key_f46", 			ISKEY,	KEY_F46 },
	{ "key_f47", 			ISKEY,	KEY_F47 },
	{ "key_f48", 			ISKEY,	KEY_F48 },
	{ "key_f49", 			ISKEY,	KEY_F49 },
	{ "key_f50", 			ISKEY,	KEY_F50 },
	{ "key_f51", 			ISKEY,	KEY_F51 },
	{ "key_f52", 			ISKEY,	KEY_F52 },
	{ "key_f53", 			ISKEY,	KEY_F53 },
	{ "key_f54", 			ISKEY,	KEY_F54 },
	{ "key_f55", 			ISKEY,	KEY_F55 },
	{ "key_f56", 			ISKEY,	KEY_F56 },
	{ "key_f57", 			ISKEY,	KEY_F57 },
	{ "key_f58", 			ISKEY,	KEY_F58 },
	{ "key_f59", 			ISKEY,	KEY_F59 },
	{ "key_f60", 			ISKEY,	KEY_F60 },
	{ "key_f61", 			ISKEY,	KEY_F61 },
	{ "key_f62", 			ISKEY,	KEY_F62 },
	{ "key_f63", 			ISKEY,	KEY_F63 },
	{ "key_f64", 			ISKEY,	KEY_F64 },
	{ "key_home", 			ISKEY,	KEY_HOME },
	{ "key_backspace", 		ISKEY,	KEY_BACKSPACE },
	{ "key_down_arrow", 		ISKEY,	KEY_DOWN_ARROW },
	{ "key_left_arrow", 		ISKEY,	KEY_LEFT_ARROW },
	{ "key_right_arrow", 		ISKEY,	KEY_RIGHT_ARROW },
	{ "key_up_arrow", 		ISKEY,	KEY_UP_ARROW },
	{ "key_delete", 		ISKEY,	KEY_DELETE },
	{ "key_insert", 		ISKEY,	KEY_INSERT },
	{ "key_next_scr", 		ISKEY,	KEY_NEXT_SCR },
	{ "key_prev_scr", 		ISKEY,	KEY_PREV_SCR },
	{ "key_select", 		ISKEY,	KEY_SELECT },
	{ "key_tab", 			ISKEY,	KEY_TAB },
	{ "key_help", 			ISKEY,	KEY_HELP },
	{ "key_do", 			ISKEY,	KEY_DO },
	{ "key_find", 			ISKEY,	KEY_FIND },
	{ "key_user1", 			ISKEY,	KEY_USER1 },
	{ "key_user2", 			ISKEY,	KEY_USER2 },
	{ "key_user3", 			ISKEY,	KEY_USER3 },
	{ "key_user4", 			ISKEY,	KEY_USER4 },
	{ "key_user5", 			ISKEY,	KEY_USER5 },
	{ "key_user6", 			ISKEY,	KEY_USER6 },
	{ "key_user7", 			ISKEY,	KEY_USER7 },
	{ "key_user8", 			ISKEY,	KEY_USER8 },
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
char *vcapdef[VC_CAP_COUNT+1] = 
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
	"xqlkmjwvtun\253\273",					     /* GRAPHSTR              */
	"\033(0",						     /* ENTER_GRAPHICS_MODE   */
	"\033(B",						     /* EXIT_GRAPHICS_MODE    */
	"",							     /* PAD                   */
	"`f~a",							     /* PSEUDO_BLANKS         */
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

int padding;

#else
extern int padding;
extern vc_load vc_load_defs[];
extern char *vcapdef[];
#endif

#define VKEY struct vkey_s

VKEY
{
	char *value;
	VKEY *eval_to;
	int id;
};

/*
	FUNCTION KEYS structure.

	The second column is a redirection to the third column.

	The enteries are POSITION DEPENDENT based on the define.
*/
#ifdef VCAP
VKEY vkeydef[VC_KEY_LAST+VC_KEY_EXTRA];
VKEY vt220def[] =
{
	"", NULL, 0,		/* element 0 in array not used */
	"", NULL, 0,		/* PF0 not used */
	"\033OP",          &vkeydef[GENERIC_PF1 ],	GENERIC_PF1, /* KEY_F1   */
	"\033OQ",          &vkeydef[GENERIC_PF2 ],	GENERIC_PF2, /* KEY_F2   */
	"\033OR",          &vkeydef[GENERIC_PF3 ],	GENERIC_PF3, /* KEY_F3   */
	"\033OS",          &vkeydef[GENERIC_PF4 ],      GENERIC_PF4, /* KEY_F4   */
	"\033[18~",        &vkeydef[GENERIC_PF5 ],	GENERIC_PF5, /* KEY_F5   */
	"\033[19~",        &vkeydef[GENERIC_PF6 ],	GENERIC_PF6, /* KEY_F6   */
	"\033[20~",        &vkeydef[GENERIC_PF7 ],	GENERIC_PF7, /* KEY_F7   */
	"\033[21~",        &vkeydef[GENERIC_PF8 ],	GENERIC_PF8, /* KEY_F8   */
	"\033[23~",        &vkeydef[GENERIC_PF9 ],	GENERIC_PF9, /* KEY_F9   */
	"\033[24~",        &vkeydef[GENERIC_PF10],	GENERIC_PF10, /* KEY_F10  */
	"\033[25~",        &vkeydef[GENERIC_PF11],	GENERIC_PF11, /* KEY_F11  */
	"\033[26~",        &vkeydef[GENERIC_PF12],	GENERIC_PF12, /* KEY_F12  */
	"\033[31~",        &vkeydef[GENERIC_PF13],	GENERIC_PF13, /* KEY_F13  */
	"\033[32~",        &vkeydef[GENERIC_PF14],	GENERIC_PF14, /* KEY_F14  */
	"\033[33~",        &vkeydef[GENERIC_PF15],	GENERIC_PF15, /* KEY_F15  */
	"\033[34~",        &vkeydef[GENERIC_PF16],	GENERIC_PF16, /* KEY_F16  */
	"\033[4~\033OP",   &vkeydef[GENERIC_PF17],	GENERIC_PF17, /* KEY_F17  */
	"\033[4~\033OQ",   &vkeydef[GENERIC_PF18],	GENERIC_PF18, /* KEY_F18  */
	"\033[4~\033OR",   &vkeydef[GENERIC_PF19],	GENERIC_PF19, /* KEY_F19  */
	"\033[4~\033OS",   &vkeydef[GENERIC_PF20],	GENERIC_PF20, /* KEY_F20  */
	"\033[4~\033[18~", &vkeydef[GENERIC_PF21],	GENERIC_PF21, /* KEY_F21  */
	"\033[4~\033[19~", &vkeydef[GENERIC_PF22],	GENERIC_PF22, /* KEY_F22  */
	"\033[4~\033[20~", &vkeydef[GENERIC_PF23],	GENERIC_PF23, /* KEY_F23  */
	"\033[4~\033[21~", &vkeydef[GENERIC_PF24],	GENERIC_PF24, /* KEY_F24  */
	"\033[4~\033[23~", &vkeydef[GENERIC_PF25],	GENERIC_PF25, /* KEY_F25  */
	"\033[4~\033[24~", &vkeydef[GENERIC_PF26],	GENERIC_PF26, /* KEY_F26  */
	"\033[4~\033[25~", &vkeydef[GENERIC_PF27],	GENERIC_PF27, /* KEY_F27  */
	"\033[4~\033[26~", &vkeydef[GENERIC_PF28],	GENERIC_PF28, /* KEY_F28  */
	"\033[4~\033[31~", &vkeydef[GENERIC_PF29],	GENERIC_PF29, /* KEY_F29  */
	"\033[4~\033[32~", &vkeydef[GENERIC_PF30],	GENERIC_PF30, /* KEY_F30  */
	"\033[4~\033[33~", &vkeydef[GENERIC_PF31],	GENERIC_PF31, /* KEY_F31  */
	"\033[4~\033[34~", &vkeydef[GENERIC_PF32],      GENERIC_PF32, /* KEY_F32  */
	"",                NULL,	       		KEY_HOME,      /* KEY_HOME         */
	"\177",		   &vkeydef[GENERIC_DELETE],	KEY_BACKSPACE, /* KEY_BACKSPACE    */
	"\033[B",          &vkeydef[GENERIC_DOWN ],	GENERIC_DOWN, /* KEY_DOWN_ARROW   */
	"\033[D",	   &vkeydef[GENERIC_LEFT ],     GENERIC_LEFT, /* KEY_LEFT_ARROW   */
	"\033[C",	   &vkeydef[GENERIC_RIGHT],     GENERIC_RIGHT, /*KEY_RIGHT_ARROW   */
	"\033[A",	   &vkeydef[GENERIC_UP   ],     GENERIC_UP,   /* KEY_UP_ARROW     */
	"\00601",          NULL,                        GENERIC_PF1, 
	"\00602",          NULL,                        GENERIC_PF2, 
	"\00603",          NULL,                        GENERIC_PF3, 
	"\00604",          NULL,                        GENERIC_PF4, 
	"\00605",          NULL,                        GENERIC_PF5, 
	"\00606",          NULL,                        GENERIC_PF6, 
	"\00607",          NULL,                        GENERIC_PF7, 
	"\00608",          NULL,                        GENERIC_PF8, 
	"\00609",          NULL,                        GENERIC_PF9, 
	"\00610",          NULL,                        GENERIC_PF10,
	"\00611",          NULL,                        GENERIC_PF11,
	"\00612",          NULL,                        GENERIC_PF12,
	"\00613",          NULL,                        GENERIC_PF13,
	"\00614",          NULL,                        GENERIC_PF14,
	"\00615",          NULL,                        GENERIC_PF15,
	"\00616",          NULL,                        GENERIC_PF16,
	"\00617",          NULL,                        GENERIC_PF17,
	"\00618",          NULL,                        GENERIC_PF18,
	"\00619",          NULL,                        GENERIC_PF19,
	"\00620",          NULL,                        GENERIC_PF20,
	"\00621",          NULL,                        GENERIC_PF21,
	"\00622",          NULL,                        GENERIC_PF22,
	"\00623",          NULL,                        GENERIC_PF23,
	"\00624",          NULL,                        GENERIC_PF24,
	"\00625",          NULL,                        GENERIC_PF25,
	"\00626",          NULL,                        GENERIC_PF26,
	"\00627",          NULL,                        GENERIC_PF27,
	"\00628",          NULL,                        GENERIC_PF28,
	"\00629",          NULL,                        GENERIC_PF29,
	"\00630",          NULL,                        GENERIC_PF30,
	"\00631",          NULL,                        GENERIC_PF31,
	"\00632",          NULL,                        GENERIC_PF32,
	"\006H",           NULL,                        GENERIC_HOME,
	"\006?",	   NULL,    			GENERIC_HELP,
	"\006U",	   NULL,     			GENERIC_UP,
	"\006D",	   NULL,     			GENERIC_DOWN,
        "\006L",	   NULL,     			GENERIC_LEFT,
        "\006R",	   NULL,     			GENERIC_RIGHT,
        "",	  	   NULL,     			GENERIC_TAB,
        "\006X",	   NULL,     			GENERIC_DELETE,
	"\006I",	   NULL,     			GENERIC_INSERT,
	"\006\016",	   NULL,     			GENERIC_NEXT_SCR,
	"\006\020",        NULL,                        GENERIC_PREV_SCR,
	"\033[3~",	   &vkeydef[GENERIC_REMOVE],	KEY_DELETE,
	"\033[2~",	   &vkeydef[GENERIC_INSERT],	KEY_INSERT,
	"",		   &vkeydef[GENERIC_NEXT_SCR], 	KEY_NEXT_SCR,
	"",		   &vkeydef[GENERIC_PREV_SCR],	KEY_PREV_SCR,
	"",		   &vkeydef[GENERIC_SELECT],	KEY_SELECT,
	"\006\011", 	   NULL,     			GENERIC_BACKTAB,
	"",		   NULL,     			GENERIC_REMOVE,
	"\006S",	   NULL,     			GENERIC_SELECT,
	"\006\003",	   NULL,		        GENERIC_CANCEL,
	"\015",		   NULL,     			GENERIC_RETURN,
        "\012",            NULL,                        GENERIC_ENTER,
	"\030",		   NULL,     			GENERIC_REFRESH,
	"\011",		   &vkeydef[GENERIC_TAB],	KEY_TAB,
	"\033[28~",	   &vkeydef[GENERIC_HELP],	KEY_HELP,
	"\033[29~",	   &vkeydef[GENERIC_PF16],	KEY_DO,
	"\033[1~",	   &vkeydef[GENERIC_HOME],	KEY_FIND,
	"",		   NULL,			KEY_USER1,
	"",		   NULL,			KEY_USER2,
	"",		   NULL,			KEY_USER3,
	"",		   NULL,			KEY_USER4,
	"",		   NULL,			KEY_USER5,
	"",		   NULL,			KEY_USER6,
	"",		   NULL,			KEY_USER7,
	"",		   NULL,			KEY_USER8,
	"\033[4~\033[2~",  NULL,			GENERIC_CLEAR_FIELD, 
	"\033[4~\033[3~",  NULL,			GENERIC_CLEAR_AFTER, 
	"\033[4~\033[1~",  NULL,      			GENERIC_CLEAR_BEFORE,
	"\033[4~\033[4~",  NULL,      			GENERIC_NULL,
	"\016",		   NULL,			TRIGGER1,
	"",		   NULL,			TRIGGER2,
	"",		   NULL,			TRIGGER3,
	"",		   NULL,			TRIGGER4,
	"",		   NULL,			TRIGGER5,
	"",		   NULL,			TRIGGER6,
	"",		   NULL,			TRIGGER7,
	"",		   NULL,			TRIGGER8,
	"",		   NULL,			KEY_F33,
	"",		   NULL,			KEY_F34,
	"",		   NULL,			KEY_F35,
	"",		   NULL,			KEY_F36,
	"",		   NULL,			KEY_F37 ,
	"",		   NULL,			KEY_F38,
	"",		   NULL,			KEY_F39,
	"",		   NULL,			KEY_F40,
	"",		   NULL,			KEY_F41 ,
	"",		   NULL,			KEY_F42 ,
	"",		   NULL,			KEY_F43,
	"",		   NULL,			KEY_F44,
	"",		   NULL,			KEY_F45,
	"",		   NULL,			KEY_F46,
	"",		   NULL,			KEY_F47,
	"",		   NULL,			KEY_F48,
	"",		   NULL,			KEY_F49,
	"",		   NULL,			KEY_F50 ,
	"",		   NULL,			KEY_F51,
	"",		   NULL,			KEY_F52,
	"",		   NULL,			KEY_F53 ,
	"",		   NULL,			KEY_F54,
	"",		   NULL,			KEY_F55,
	"",		   NULL,			KEY_F56,
	"",		   NULL,			KEY_F57,
	"",		   NULL,			KEY_F58,
	"",		   NULL,			KEY_F59,
	"",		   NULL,			KEY_F60,
	"",		   NULL,			KEY_F61,
	"",		   NULL,			KEY_F62,
	"",		   NULL,			KEY_F63,
	"",		   NULL,			KEY_F64,
	"\00633",	   NULL,			GENERIC_PF33,
	"\00634",	   NULL,			GENERIC_PF34,
	"\00635",	   NULL,			GENERIC_PF35,
	"\00636",	   NULL,			GENERIC_PF36,
	"\00637",	   NULL,			GENERIC_PF37,
	"\00638",	   NULL,			GENERIC_PF38,
	"\00639",	   NULL,			GENERIC_PF39,
	"\00640",	   NULL,			GENERIC_PF40,
	"\00641",	   NULL,			GENERIC_PF41,
	"\00642",	   NULL,			GENERIC_PF42,
	"\00643",	   NULL,			GENERIC_PF43,
	"\00644",	   NULL,			GENERIC_PF44,
	"\00645",	   NULL,			GENERIC_PF45,
	"\00646",	   NULL,			GENERIC_PF46,
	"\00647",	   NULL,			GENERIC_PF47,
	"\00648",	   NULL,			GENERIC_PF48,
	"\00649",	   NULL,			GENERIC_PF49,


	"\00650",	   NULL,			GENERIC_PF50,
	"\00651",	   NULL,			GENERIC_PF51,
	"\00652",	   NULL,			GENERIC_PF52,
	"\00653",	   NULL,			GENERIC_PF53,
	"\00654",	   NULL,			GENERIC_PF54,
	"\00655",	   NULL,			GENERIC_PF55,
	"\00656",	   NULL,			GENERIC_PF56,
	"\00657",	   NULL,			GENERIC_PF57,
	"\00658",	   NULL,			GENERIC_PF58,
	"\00659",	   NULL,			GENERIC_PF59,
	"\00660",	   NULL,			GENERIC_PF60,
	"\00661",	   NULL,			GENERIC_PF61,
	"\00662",	   NULL,			GENERIC_PF62,
	"\00663",	   NULL,			GENERIC_PF63,
	"\00664",	   NULL,			GENERIC_PF64,
        "\006N",           NULL,                        GENERIC_NEWLINE,
	"",		   NULL,			SHIFT_F1,
	"",		   NULL,			SHIFT_F2,
	"",		   NULL,			SHIFT_F3,
	"",		   NULL,			SHIFT_F4,
	"",		   NULL,			SHIFT_F5,
	"",		   NULL,			SHIFT_F6,
	"",		   NULL,			SHIFT_F7,
	"",		   NULL,			SHIFT_F8,
	"",		   NULL,			SHIFT_F9,
	"",		   NULL,			SHIFT_F10,
	"",		   NULL,			SHIFT_F11,
	"",		   NULL,			SHIFT_F12,
	"",		   NULL,			CTRL_F1,
	"",		   NULL,			CTRL_F2,
	"",		   NULL,			CTRL_F3,
	"",		   NULL,			CTRL_F4,
	"",		   NULL,			CTRL_F5,
	"",		   NULL,			CTRL_F6,
	"",		   NULL,			CTRL_F7,
	"",		   NULL,			CTRL_F8,
	"",		   NULL,			CTRL_F9,
	"",		   NULL,			CTRL_F10,
	"",		   NULL,			CTRL_F11,
	"",		   NULL,			CTRL_F12,
	"",		   NULL,			ALT_F1,
	"",		   NULL,			ALT_F2,
	"",		   NULL,			ALT_F3,
	"",		   NULL,			ALT_F4,
	"",		   NULL,			ALT_F5,
	"",		   NULL,			ALT_F6,
	"",		   NULL,			ALT_F7,
	"",		   NULL,			ALT_F8,
	"",		   NULL,			ALT_F9,
	"",		   NULL,			ALT_F10,
	"",		   NULL,			ALT_F11,
	"",		   NULL,			ALT_F12,
/* End of defined list; following are extras.				*/
	"\006h",           NULL,                        GENERIC_HOME,
	"\006u",	   NULL,     			GENERIC_UP,
	"\006d",	   NULL,     			GENERIC_DOWN,
        "\006l",	   NULL,     			GENERIC_LEFT,
        "\006r",	   NULL,   	  		GENERIC_RIGHT,
        "\006x",	   NULL,     			GENERIC_DELETE,
	"\006i",	   NULL,     			GENERIC_INSERT,
	"\006\013",	   NULL,     			GENERIC_UP,
	"\006\012",	   NULL,	     		GENERIC_DOWN,
        "\006\010",	   NULL,     			GENERIC_LEFT,
        "\006\014",	   NULL,     			GENERIC_RIGHT,
	"\006s",	   NULL,     			GENERIC_SELECT,
	"\005", 	   NULL,    			GENERIC_HELP,
	"\001",            NULL,                        GENERIC_HOME,
	"\022",		   NULL,     			GENERIC_REFRESH,
	"\010",		   NULL,			GENERIC_BACKTAB,
        "\006n",           NULL,                        GENERIC_NEWLINE,
	"\033[6~",         NULL,                        GENERIC_NEWLINE,
	"\033[5~",         NULL,                        GENERIC_BACKTAB
};
VKEY vt100def[] =
{
	"", NULL, 0,		/* element 0 in array not used */
	"", NULL, 0,		/* PF0 not used */
	"\033OP",          &vkeydef[GENERIC_PF1 ],	GENERIC_PF1, /* KEY_F1   */
	"\033OQ",          &vkeydef[GENERIC_PF2 ],	GENERIC_PF2, /* KEY_F2   */
	"\033OR",          &vkeydef[GENERIC_PF3 ],	GENERIC_PF3, /* KEY_F3   */
	"\033OS",          &vkeydef[GENERIC_PF4 ],      GENERIC_PF4, /* KEY_F4   */
	"\033Ow",          &vkeydef[GENERIC_PF5 ],	GENERIC_PF5, /* KEY_F5   */
	"\033Ox",          &vkeydef[GENERIC_PF6 ],	GENERIC_PF6, /* KEY_F6   */
	"\033Oy",          &vkeydef[GENERIC_PF7 ],	GENERIC_PF7, /* KEY_F7   */
	"\033Om",          &vkeydef[GENERIC_PF8 ],	GENERIC_PF8, /* KEY_F8   */
	"\033Ot",          &vkeydef[GENERIC_PF9 ],	GENERIC_PF9, /* KEY_F9   */
	"\033Ou",          &vkeydef[GENERIC_PF10],	GENERIC_PF10, /* KEY_F10  */
	"\033Ov",          &vkeydef[GENERIC_PF11],	GENERIC_PF11, /* KEY_F11  */
	"\033Ol",          &vkeydef[GENERIC_PF12],	GENERIC_PF12, /* KEY_F12  */
	"\033Oq",          &vkeydef[GENERIC_PF13],	GENERIC_PF13, /* KEY_F13  */
	"\033Or",          &vkeydef[GENERIC_PF14],	GENERIC_PF14, /* KEY_F14  */
	"\033Os",          &vkeydef[GENERIC_PF15],	GENERIC_PF15, /* KEY_F15  */
	"\033On",          &vkeydef[GENERIC_PF16],	GENERIC_PF16, /* KEY_F16  */
	"\033Op\033OP",    &vkeydef[GENERIC_PF17],	GENERIC_PF17, /* KEY_F17  */
	"\033Op\033OQ",    &vkeydef[GENERIC_PF18],	GENERIC_PF18, /* KEY_F18  */
	"\033Op\033OR",    &vkeydef[GENERIC_PF19],	GENERIC_PF19, /* KEY_F19  */
	"\033Op\033OS",    &vkeydef[GENERIC_PF20],	GENERIC_PF20, /* KEY_F20  */
	"\033Op\033Ow",    &vkeydef[GENERIC_PF21],	GENERIC_PF21, /* KEY_F21  */
	"\033Op\033Ox",    &vkeydef[GENERIC_PF22],	GENERIC_PF22, /* KEY_F22  */
	"\033Op\033Oy",    &vkeydef[GENERIC_PF23],	GENERIC_PF23, /* KEY_F23  */
	"\033Op\033Om",    &vkeydef[GENERIC_PF24],	GENERIC_PF24, /* KEY_F24  */
	"\033Op\033Ot",    &vkeydef[GENERIC_PF25],	GENERIC_PF25, /* KEY_F25  */
	"\033Op\033Ou",    &vkeydef[GENERIC_PF26],	GENERIC_PF26, /* KEY_F26  */
	"\033Op\033Ov",    &vkeydef[GENERIC_PF27],	GENERIC_PF27, /* KEY_F27  */
	"\033Op\033Ol",    &vkeydef[GENERIC_PF28],	GENERIC_PF28, /* KEY_F28  */
	"\033Op\033Oq",    &vkeydef[GENERIC_PF29],	GENERIC_PF29, /* KEY_F29  */
	"\033Op\033Or",    &vkeydef[GENERIC_PF30],	GENERIC_PF30, /* KEY_F30  */
	"\033Op\033Os",    &vkeydef[GENERIC_PF31],	GENERIC_PF31, /* KEY_F31  */
	"\033Op\033On",    &vkeydef[GENERIC_PF32],      GENERIC_PF32, /* KEY_F32  */
	"",                NULL,	       		KEY_HOME,      /* KEY_HOME         */
	"\177",		   &vkeydef[GENERIC_DELETE],	KEY_BACKSPACE, /* KEY_BACKSPACE    */
	"\033[B",          &vkeydef[GENERIC_DOWN ],	GENERIC_DOWN, /* KEY_DOWN_ARROW   */
	"\033[D",	   &vkeydef[GENERIC_LEFT ],     GENERIC_LEFT, /* KEY_LEFT_ARROW   */
	"\033[C",	   &vkeydef[GENERIC_RIGHT],     GENERIC_RIGHT, /*KEY_RIGHT_ARROW   */
	"\033[A",	   &vkeydef[GENERIC_UP   ],     GENERIC_UP,   /* KEY_UP_ARROW     */
	"\00601",          NULL,                        GENERIC_PF1, 
	"\00602",          NULL,                        GENERIC_PF2, 
	"\00603",          NULL,                        GENERIC_PF3, 
	"\00604",          NULL,                        GENERIC_PF4, 
	"\00605",          NULL,                        GENERIC_PF5, 
	"\00606",          NULL,                        GENERIC_PF6, 
	"\00607",          NULL,                        GENERIC_PF7, 
	"\00608",          NULL,                        GENERIC_PF8, 
	"\00609",          NULL,                        GENERIC_PF9, 
	"\00610",          NULL,                        GENERIC_PF10,
	"\00611",          NULL,                        GENERIC_PF11,
	"\00612",          NULL,                        GENERIC_PF12,
	"\00613",          NULL,                        GENERIC_PF13,
	"\00614",          NULL,                        GENERIC_PF14,
	"\00615",          NULL,                        GENERIC_PF15,
	"\00616",          NULL,                        GENERIC_PF16,
	"\00617",          NULL,                        GENERIC_PF17,
	"\00618",          NULL,                        GENERIC_PF18,
	"\00619",          NULL,                        GENERIC_PF19,
	"\00620",          NULL,                        GENERIC_PF20,
	"\00621",          NULL,                        GENERIC_PF21,
	"\00622",          NULL,                        GENERIC_PF22,
	"\00623",          NULL,                        GENERIC_PF23,
	"\00624",          NULL,                        GENERIC_PF24,
	"\00625",          NULL,                        GENERIC_PF25,
	"\00626",          NULL,                        GENERIC_PF26,
	"\00627",          NULL,                        GENERIC_PF27,
	"\00628",          NULL,                        GENERIC_PF28,
	"\00629",          NULL,                        GENERIC_PF29,
	"\00630",          NULL,                        GENERIC_PF30,
	"\00631",          NULL,                        GENERIC_PF31,
	"\00632",          NULL,                        GENERIC_PF32,
	"\006H",           NULL,                        GENERIC_HOME,
	"\006?",	   NULL,    			GENERIC_HELP,
	"\006U",	   NULL,     			GENERIC_UP,
	"\006D",	   NULL,     			GENERIC_DOWN,
        "\006L",	   NULL,     			GENERIC_LEFT,
        "\006R",	   NULL,     			GENERIC_RIGHT,
        "",	  	   NULL,     			GENERIC_TAB,
        "\006X",	   NULL,     			GENERIC_DELETE,
	"\006I",	   NULL,     			GENERIC_INSERT,
	"\006\016",	   NULL,     			GENERIC_NEXT_SCR,
	"\006\020",        NULL,                        GENERIC_PREV_SCR,
	"",	   	   &vkeydef[GENERIC_REMOVE],	KEY_DELETE,
	"",	   	   &vkeydef[GENERIC_INSERT],	KEY_INSERT,
	"",	   	   &vkeydef[GENERIC_NEXT_SCR], 	KEY_NEXT_SCR,
	"",	   	   &vkeydef[GENERIC_PREV_SCR],	KEY_PREV_SCR,
	"",		   &vkeydef[GENERIC_SELECT],	KEY_SELECT,
	"\006\011", 	   NULL,     			GENERIC_BACKTAB,
	"",		   NULL,     			GENERIC_REMOVE,
	"\006S",	   NULL,     			GENERIC_SELECT,
	"\006\003",	   NULL,		        GENERIC_CANCEL,
	"\015",		   NULL,     			GENERIC_RETURN,
        "\012",            NULL,                        GENERIC_ENTER,
	"\030",		   NULL,     			GENERIC_REFRESH,
	"\011",		   &vkeydef[GENERIC_TAB],	KEY_TAB,
	"",	   	   &vkeydef[GENERIC_HELP],	KEY_HELP,
	"",	   	   &vkeydef[GENERIC_PF16],	KEY_DO,
	"",	   	   &vkeydef[GENERIC_HOME],	KEY_FIND,
	"",		   NULL,			KEY_USER1,
	"",		   NULL,			KEY_USER2,
	"",		   NULL,			KEY_USER3,
	"",		   NULL,			KEY_USER4,
	"",		   NULL,			KEY_USER5,
	"",		   NULL,			KEY_USER6,
	"",		   NULL,			KEY_USER7,
	"",		   NULL,			KEY_USER8,
	"",  		   NULL,			GENERIC_CLEAR_FIELD, 
	"",  		   NULL,			GENERIC_CLEAR_AFTER, 
	"",		   NULL,      			GENERIC_CLEAR_BEFORE,
	"",		   NULL,      			GENERIC_NULL,
	"\016",		   NULL,			TRIGGER1,
	"",		   NULL,			TRIGGER2,
	"",		   NULL,			TRIGGER3,
	"",		   NULL,			TRIGGER4,
	"",		   NULL,			TRIGGER5,
	"",		   NULL,			TRIGGER6,
	"",		   NULL,			TRIGGER7,
	"",		   NULL,			TRIGGER8,
	"",		   NULL,			KEY_F33,
	"",		   NULL,			KEY_F34,
	"",		   NULL,			KEY_F35,
	"",		   NULL,			KEY_F36,
	"",		   NULL,			KEY_F37 ,
	"",		   NULL,			KEY_F38,
	"",		   NULL,			KEY_F39,
	"",		   NULL,			KEY_F40,
	"",		   NULL,			KEY_F41 ,
	"",		   NULL,			KEY_F42 ,
	"",		   NULL,			KEY_F43,
	"",		   NULL,			KEY_F44,
	"",		   NULL,			KEY_F45,
	"",		   NULL,			KEY_F46,
	"",		   NULL,			KEY_F47,
	"",		   NULL,			KEY_F48,
	"",		   NULL,			KEY_F49,
	"",		   NULL,			KEY_F50 ,
	"",		   NULL,			KEY_F51,
	"",		   NULL,			KEY_F52,
	"",		   NULL,			KEY_F53 ,
	"",		   NULL,			KEY_F54,
	"",		   NULL,			KEY_F55,
	"",		   NULL,			KEY_F56,
	"",		   NULL,			KEY_F57,
	"",		   NULL,			KEY_F58,
	"",		   NULL,			KEY_F59,
	"",		   NULL,			KEY_F60,
	"",		   NULL,			KEY_F61,
	"",		   NULL,			KEY_F62,
	"",		   NULL,			KEY_F63,
	"",		   NULL,			KEY_F64,
	"\00633",	   NULL,			GENERIC_PF33,
	"\00634",	   NULL,			GENERIC_PF34,
	"\00635",	   NULL,			GENERIC_PF35,
	"\00636",	   NULL,			GENERIC_PF36,
	"\00637",	   NULL,			GENERIC_PF37,
	"\00638",	   NULL,			GENERIC_PF38,
	"\00639",	   NULL,			GENERIC_PF39,
	"\00640",	   NULL,			GENERIC_PF40,
	"\00641",	   NULL,			GENERIC_PF41,
	"\00642",	   NULL,			GENERIC_PF42,
	"\00643",	   NULL,			GENERIC_PF43,
	"\00644",	   NULL,			GENERIC_PF44,
	"\00645",	   NULL,			GENERIC_PF45,
	"\00646",	   NULL,			GENERIC_PF46,
	"\00647",	   NULL,			GENERIC_PF47,
	"\00648",	   NULL,			GENERIC_PF48,
	"\00649",	   NULL,			GENERIC_PF49,
	"\00650",	   NULL,			GENERIC_PF50,
	"\00651",	   NULL,			GENERIC_PF51,
	"\00652",	   NULL,			GENERIC_PF52,
	"\00653",	   NULL,			GENERIC_PF53,
	"\00654",	   NULL,			GENERIC_PF54,
	"\00655",	   NULL,			GENERIC_PF55,
	"\00656",	   NULL,			GENERIC_PF56,
	"\00657",	   NULL,			GENERIC_PF57,
	"\00658",	   NULL,			GENERIC_PF58,
	"\00659",	   NULL,			GENERIC_PF59,
	"\00660",	   NULL,			GENERIC_PF60,
	"\00661",	   NULL,			GENERIC_PF61,
	"\00662",	   NULL,			GENERIC_PF62,
	"\00663",	   NULL,			GENERIC_PF63,
	"\00664",	   NULL,			GENERIC_PF64,
	"\006N",           NULL,                        GENERIC_NEWLINE,
	"",		   NULL,			SHIFT_F1,
	"",		   NULL,			SHIFT_F2,
	"",		   NULL,			SHIFT_F3,
	"",		   NULL,			SHIFT_F4,
	"",		   NULL,			SHIFT_F5,
	"",		   NULL,			SHIFT_F6,
	"",		   NULL,			SHIFT_F7,
	"",		   NULL,			SHIFT_F8,
	"",		   NULL,			SHIFT_F9,
	"",		   NULL,			SHIFT_F10,
	"",		   NULL,			SHIFT_F11,
	"",		   NULL,			SHIFT_F12,
	"",		   NULL,			CTRL_F1,
	"",		   NULL,			CTRL_F2,
	"",		   NULL,			CTRL_F3,
	"",		   NULL,			CTRL_F4,
	"",		   NULL,			CTRL_F5,
	"",		   NULL,			CTRL_F6,
	"",		   NULL,			CTRL_F7,
	"",		   NULL,			CTRL_F8,
	"",		   NULL,			CTRL_F9,
	"",		   NULL,			CTRL_F10,
	"",		   NULL,			CTRL_F11,
	"",		   NULL,			CTRL_F12,
	"",		   NULL,			ALT_F1,
	"",		   NULL,			ALT_F2,
	"",		   NULL,			ALT_F3,
	"",		   NULL,			ALT_F4,
	"",		   NULL,			ALT_F5,
	"",		   NULL,			ALT_F6,
	"",		   NULL,			ALT_F7,
	"",		   NULL,			ALT_F8,
	"",		   NULL,			ALT_F9,
	"",		   NULL,			ALT_F10,
	"",		   NULL,			ALT_F11,
	"",		   NULL,			ALT_F12,

/* End of defined list; following are extras.				*/
	"\006h",           NULL,                        GENERIC_HOME,
	"\006u",	   NULL,     			GENERIC_UP,
	"\006d",	   NULL,     			GENERIC_DOWN,
        "\006l",	   NULL,     			GENERIC_LEFT,
        "\006r",	   NULL,     			GENERIC_RIGHT,
        "\006x",	   NULL,     			GENERIC_DELETE,
	"\006i",	   NULL,     			GENERIC_INSERT,
	"\006\013",	   NULL,     			GENERIC_UP,
	"\006\012",	   NULL,     			GENERIC_DOWN,
        "\006\010",	   NULL,     			GENERIC_LEFT,
        "\006\014",	   NULL,     			GENERIC_RIGHT,
	"\006s",	   NULL,     			GENERIC_SELECT,
	"\005", 	   NULL,    			GENERIC_HELP,
	"\001",            NULL,                        GENERIC_HOME,
	"\022",		   NULL,     			GENERIC_REFRESH,
	"\010",		   NULL,			GENERIC_BACKTAB,
	"\033OM", 	   NULL, 			GENERIC_RETURN,
	"\006n",           NULL,                        GENERIC_NEWLINE,
	"\033[6~",         NULL,                        GENERIC_NEWLINE,
	"\033[5~",         NULL,                        GENERIC_BACKTAB
};							
VKEY *vkp;						
#else							
extern VKEY vkeydef[];
extern VKEY *vkp;
#endif

#ifdef VCAP
int vkey_ext = VC_KEY_LAST;

#define VC_META_NODE struct vc_meta_node

VC_META_NODE
{
	int ch;
	int key_id;
	int next_hash;
	VC_META_NODE **next;
};

VC_META_NODE meta_top,*meta_p,**meta_pp;

#define HASHPOINT 5

int leadin_hash;	

#define PATHSIZE 128

#define VCMEMSIZE 4096
#define STACKSIZE 64

#define ST_ZED -1

char cstack[STACKSIZE];
int cstackp= -1;

#ifdef unix
char vcdir[64];
#endif
#ifdef MSDOS
char vcdir[64];
#endif
#ifdef VMS
char *vcpathfmt = "WISP$CONFIG:%s.VCAP";
#endif
char vcpath[PATHSIZE];									/* path of desc file for current term */

char temp[256];

int vcap_loaded = FALSE;
int vcline = 0;

#define TYPE_LIT 0
#define TYPE_SYM 1
#define TYPE_NUM 2
#define TYPE_MLT 3

#define vre printf
#define vexit exit

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

int stab[RV_EVENT_CNT][RV_STATE_CNT] =
{     /*     START    LIT0    LIT      SYM     CTL0     ESC0    CTL     ESC     S_TERM*/
/*NORM*/  { S_SYM,   S_LIT,  S_LIT,  S_SYM,   S_CTL,   S_ESC,  S_LIT,  S_LIT,   0 },
/*QUOTE*/ { S_LIT0,  S_END,  S_END,  S_ERROR, S_ERROR, S_ESC,  S_END,  S_END,   0 },
/*OR*/    { S_START, S_LIT,  S_LIT,  S_END,   S_CTL,   S_ESC,  S_LIT,  S_LIT,   0 },
/*ESC*/   { S_ERROR, S_ESC0, S_ESC0, S_ERROR, S_CTL,   S_ESC,  S_ESC0, S_ESC0,  0 },
/*CIRC*/  { S_ERROR, S_CTL0, S_CTL0, S_ERROR, S_CTL,   S_ESC,  S_CTL0, S_CTL0,  0 },
/*NULL*/  { S_TERM,  S_ERROR,S_ERROR,S_END,   S_ERROR, S_ERROR,S_ERROR,S_ERROR, 0 },
};

#ifdef _AIX
#undef regcmp
#define regcmp(x,y) re_comp(x)
#undef regex
#define regex(x,y) re_exec(y)
char *re_comp();
int re_exec();
#endif

#ifdef ultrix
#undef regcmp
#define regcmp(x,y) re_comp(x)
#undef regex
#define regex(x,y) re_exec(y)
char *re_comp();
int re_exec();
#endif

#ifndef ultrix
#ifndef _AIX
char *regex();
char *regcmp();
#endif
#endif

#endif /*VCAP*/

#define PBLANK1 vcapdef[PSEUDO_BLANKS][0]
#define PBLANK2 vcapdef[PSEUDO_BLANKS][1]
#define PBLANK3 vcapdef[PSEUDO_BLANKS][2]
#define PBLANK4 vcapdef[PSEUDO_BLANKS][3]

#endif /*defined VCAP_HEADER*/

