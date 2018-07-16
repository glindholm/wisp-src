/* 
	Copyright (c) 1994-1996 DevTech Migrations, All rights reserved.
	$Id:$
*/
/************************************************************************/
/*	      VIDEO - Video Interactive Development Environment		*/
/*			Copyright (c) 1988, 1989, 1990			*/
/*	 An unpublished work of International Digital Scientific Inc.	*/
/*			    All rights reserved.			*/
/************************************************************************/

/*
**	File:		video.h
**
**	Project:	VIDEOLIB
**
**	RCS:		$Source:$
**
**	Purpose:	General video header
**
*/

#ifndef video_H
#define video_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/


/************************************************************************/
/*	Standard definitions						*/
/************************************************************************/

#ifndef CHAR_NULL			/* Use so VMS compiler likes it.*/
#define CHAR_NULL	'\000'		/* Standard null character def.	*/
#endif

#ifndef FALSE
#define FALSE		0		/* Standard false definition.	*/
#endif

#ifndef TRUE
#define TRUE		!FALSE		/* Standard true definition.	*/
#endif

#define FAILURE		0		/* Standard failure definition.	*/
#define SUCCESS		1		/* Standard success definition.	*/
#define OPTIMIZED	2		/* Action was optimized def.	*/

/************************************************************************/
/*	Define terminal geometry					*/
/************************************************************************/

#define	MAX_LINES_PER_SCREEN	24	/* Standard VT220 screen.	*/
#if defined(MSDOS) || defined(WIN32)
#define MAX_COLUMNS_PER_LINE	80	/* Normally in 80 column mode.	*/
#else	/* VMS and unix */
#define MAX_COLUMNS_PER_LINE   132	/* Normally in 80 column mode.	*/
#endif	/* VMS and unix */

/************************************************************************/
/*	vbuffering() parameters.					*/
/************************************************************************/

#define VBUFF_LOGICAL   1 /* Logical buffering (under program control).	*/
#define VBUFF_AUTOMATIC 2 /* Automatic buffering (WOW will do it).	*/

#define VBUFF_START	1 /* Start buffered operation			*/
#define VBUFF_END	2 /* End buffered operation			*/

int vbuffering(int state);
int vbuffering_start(void);
int vbuffering_end(void);

/************************************************************************/
/*	verase and vrefresh parameters.					*/
/************************************************************************/

#define FULL_SCREEN   0			/* Also define as 0 for vset	*/
#define TO_EOS	      2			/* Erase to end of screen	*/
#define TO_EOL        5			/* Erase to end of line		*/

int verase(int control);


#define HARD_REFRESH  6			/* Erase full screen (hard).	*/
#define ALL_MENUS     7			/* Erase all menus.		*/
#define TOP_MENU      8			/* Erase the top menu level.	*/
#define TO_BOTTOM_MENU 9		/* Erase all but bottom menu.	*/

int vrefresh(int what);


/************************************************************************/
/*	vmode parameters						*/
/************************************************************************/

#define CLEAR      0				/* Clear char rendition	*/
#define	BOLD       1				/* Select bold rend	*/
#define	UNDERSCORE 2				/* Underscore rendition */
#define	BLINK      4				/* Blinking rendition	*/
#define	REVERSE    8				/* Reverse video rend	*/

#define VMODE_CLEAR      0			/* Clear char rendition	*/
#define VMODE_BOLD       1			/* Select bold rend	*/
#define VMODE_UNDERSCORE 2			/* Underscore rendition */
#define VMODE_BLINK      4			/* Blinking rendition	*/
#define VMODE_REVERSE    8			/* Reverse video rend	*/

int vmode(int control);

/************************************************************************/
/*	vcharset parameters						*/
/************************************************************************/

#define DEFAULT	      0		/* Default (US or UK) 			*/
#define GRAPHICS      16	/* Special graphics character set	*/

#define VCS_DEFAULT	  0	/* Default (US or UK) 			*/
#define VCS_UK            1	/* Character set for UK			*/
#define VCS_US            2	/* Character set for US			*/
#define VCS_GRAPHICS      16	/* Special graphics character set	*/
#define VCS_ROM_STANDARD  32	/* Alternate character ROM standard	*/
#define VCS_ROM_GRAPHICS  64	/* Alternate ROM special grapics	*/
#define VCS_DOWN_LOADED  128	/* Down loaded character set.		*/

int vcharset(int char_set);

/************************************************************************/
/*	vline parameters						*/
/************************************************************************/

#define VERTICAL	0			/* Draw vertical line	*/
#define	HORIZONTAL	3			/* Horizontal line.	*/
#define FAT_VERTICAL    6			/* Reverse space vert.	*/
#define FAT_HORIZONTAL  7			/* Reverse space horiz.	*/

#define VLINE_VERTICAL		0		/* Draw vertical line	*/
#define VLINE_HORIZONTAL	3		/* Horizontal line.	*/
#define VLINE_FAT_VERTICAL	6		/* Reverse space vert.	*/
#define VLINE_FAT_HORIZONTAL	7		/* Reverse space horiz.	*/

#ifdef vline
#undef vline
#endif
extern int vline (int type, int length);

/************************************************************************/
/*      vstate() parameter definitions.					*/
/************************************************************************/

/* #define DEFAULT	0		   		As per vchset()	*/
#define	SAVE	       -1		/* Save the current term state.	*/
#define RESTORE		1		/* Restore a pushed term state.	*/

#define VSTATE_DEFAULT	        0
#define VSTATE_SAVE	       -1	/* Save the current term state.	*/
#define VSTATE_RESTORE		1	/* Restore a pushed term state.	*/
#define VSTATE_SAVE_STTY       -2	/* Save STTY state.		*/
#define VSTATE_RESTORE_STTY	2	/* Restore a STTY state.	*/

int vstate(int action);


/************************************************************************/
/*	video get string vgets(string,count,filter,tc) definitions 	*/
/************************************************************************/

		  /* Values returned from vgets	*/

#define	NO_INPUT		   0	/* No input was given		*/
#define	TERM_VT			   1	/* Input was terminated with VT	*/
#define	TERM_LF			   2	/*   "    "      "       "   LF	*/
#define	TERM_HT			   3	/*   "    "      "       "   HT	*/
#define	TERM_BS			   4	/*   "    "      "       "   BS	*/
#define	TERM_CR			   5	/*   "    "      "       "   CR	*/
#define	TERM_FIELD_FULL		   6	/* Full field termination	*/

/************************************************************************/
/*	vdefer() parameter definitions.					*/
/************************************************************************/

enum e_vdefer 
{ 
	VDEFER_SAVE		= -1, 
	VDEFER_OFF		= 0,
	VDEFER_RESTORE		= 1, 
	VDEFER_MOTION_ONLY	= 2
};
	
int vdefer(enum e_vdefer state);
int vdefer_restore(void);
int vdefer_save(void);
enum e_vdefer vdeferred(void);


/************************************************************************/
/*	video setup (vset) parameters					*/
/************************************************************************/

#define	VSET_KEYPAD		0
#define				NORMAL		0
#define				APPLICATIONS	1

#define	VSET_PRINTER		(VSET_KEYPAD+1)
#define				OFF		0
#define				ON		1

#define	VSET_CURSOR		(VSET_PRINTER+1)
#define				INVISIBLE	0
#define				VISIBLE		1


#define VSET_TABLE_SIZE      	(VSET_CURSOR+1)

int vset(int item, int state);

int vset_cursor_on(void);
int vset_cursor_off(void);


/************************************************************************/
/*	vscreen definitions.						*/
/************************************************************************/

#define NARROW		1			/* Set screen narrow.	*/
#define WIDE		2			/* Set screen wide.	*/
#define	LIGHT		4			/* Set screen light.	*/
#define DARK		8			/* Set screen dark.	*/

#define VSCREEN_DEFAULT		0		/* Default narrow+dark	*/
#define VSCREEN_NARROW		1		/* Set screen narrow.	*/
#define VSCREEN_WIDE		2		/* Set screen wide.	*/
#define VSCREEN_LIGHT		4		/* Set screen light.	*/
#define VSCREEN_DARK		8		/* Set screen dark.	*/
#define VSCREEN_NOOP		16		/* Clear first flags	*/

int vscreen(int state);
int vscreen_check(int state);

/************************************************************************/
/*	vnewline and vlinefeed definitions.				*/
/************************************************************************/

#define	FORWARD		0	/* Do a new-line in forward direction.	*/
/* #define REVERSE	8 */	/* Do a new-line in reverse direction.	*/

enum e_vlf
{
	VLF_FORWARD	= 0,
	VLF_REVERSE	= 8
};

int vnewline(enum e_vlf direction);
int vlinefeed(enum e_vlf direction);

/************************************************************************/
/*	voptimize() control values.					*/
/************************************************************************/



enum e_vop 
{
	VOP_OFF		  		= 0, 	/* As per vset.				*/
	VOP_TRACKING_ONLY 		= 1,	/* Only track output, no optimization.	*/
	VOP_DATA_ONLY	  		= 2,	/* Data only optimization.		*/
	VOP_DATA_AND_CONTROLS 		= 3,	/* Optimize data and controls.		*/
	VOP_DEFER_MOTION_ONLY 		= 4,	/* Optimize data and movement.		*/
	VOP_DEFER_MODE	  		= 5	/* Deferred action optimization.	*/
};

enum e_vop voptimize(enum e_vop new_op);
enum e_vop voptlevel(void);

/************************************************************************/
/*	vterminal response parameters					*/
/************************************************************************/

#define UNKNOWN_TERMINAL 	0	/* Unknown terminal type.	*/
#define VT100   	1		/* VT100 class terminal.	*/
#define VT200   	2		/* VT200 class terminal.	*/
#define VT300   	3 		/* VT300 class terminal.	*/
#define PRO350_RT	5		/* Pro350 running RT-11.	*/
#define ATT605		6		/* AT&T 605 terminal.		*/
#define OTHER_ANSI_TERMINAL 9		/* Some other ANSI terminal.	*/
#define IBM_PC		10		/* IBM PC (direct).		*/
#define GENERIC_TERMINAL	20	/* Generic terminal.		*/
#define UNSUPPORTED_TERMINAL	99	/* Unsupported terminal.	*/

/************************************************************************/
/*	vlist function call definitions.				*/
/************************************************************************/

#define DISPLAY_LIST	0
#define INIT_LIST	1
#define ADD_HEADER	2
#define ADD_FOOTER	3
#define ADD_COLUMN	4
#define INSERT_COLUMN	5
#define DELETE_COLUMN	6
#define REPLACE_COLUMN	7
#define RESIZE_LIST	8
#define SET_FUNCTIONS	9
#define FREE_LIST	10
#define NO_DISPLAY_LIST 11
#define DISP_ROW_ITEM	12

/************************************************************************/
/*	vlist finction key definition.					*/
/************************************************************************/

#define ALLOW_KEY	0		/* Allow termination of scan.	*/
#define UP_PAGE		1
#define DOWN_PAGE	2
#define VLIST_TOP	3
#define VLIST_BOTTOM	4
#define RIGHT_COL	5
#define LEFT_COL	6
#define RIGHT_PAGE	7
#define LEFT_PAGE	8
#define SELECT_ROW	9
#define DESELECT_ROW	10

/************************************************************************/
/*	vlist column type definitions.					*/
/************************************************************************/

#define RSHORT		0		/* Regular short type def.	*/
#define USHORT		1		/* Unsigned short type def.	*/
#define RLONG		2		/* Regular long type def.	*/
#define ULONG		3		/* Unsigned long type def.	*/
#define TEXTIT		4		/* String text type def.	*/
#define SEPARATOR	5		/* String separator type def.	*/

/************************************************************************/
/*	vkbmap() function definitions					*/
/************************************************************************/

#define INITIALIZE	0		/* Initialize keyboard mapping.	*/
#define LOAD_FROM_FILE	1		/* Load keyboard map from file.	*/
#define UNLOCK		2		/* Unlock the current map.	*/

/************************************************************************/
/*	verror() option codes.						*/
/************************************************************************/

/* #define CLEAR	0		   Clear error (as per vmode())	*/
#define WAIT_FOR_INPUT	1		/* Output an error and wait.	*/
/* #define AUTOMATIC	2   Automatically clear error on on next input.	*/

/************************************************************************/
/*	vonexit()							*/
/************************************************************************/

/* #define NARROW	1			   Set screen narrow.	*/
/* #define WIDE		2			   Set screen wide.	*/
/* #define LIGHT	4			   Set screen light.	*/
/* #define DARK		8			   Set screen dark.	*/
#define CLEAR_SCREEN	16			/* Clear the screen.	*/
#define MOVE_HOME	32			/* Move home.		*/
#define MOVE_AND_SCROLL	64			/* Move and scroll.	*/
#define NORMALIZE	128			/* Normalize terminal.	*/
#define MOVE_BOTTOM	256			/* Bottom (no scroll)	*/

#define VONX_CLEAR_SCREEN	16			/* Clear the screen.	*/
#define VONX_MOVE_HOME		32			/* Move home.		*/
#define VONX_MOVE_AND_SCROLL	64			/* Move and scroll.	*/
#define VONX_NORMALIZE		128			/* Normalize terminal.	*/
#define VONX_MOVE_BOTTOM	256			/* Bottom (no scroll)	*/

int vonexit(int newvalue);

/************************************************************************/
/*	vmacro()							*/
/************************************************************************/

#define START_SAVE	1		/* Start keystroke macro.	*/
#define END_SAVE	2		/* End saving keystroke macro.	*/
#define START_RESTORE	3		/* Start restoring a macro.	*/

int vmacro(int action);

/* vutil */
void vre_set_logfile(const char* filepath);
unsigned char *vsss(int row, int col, int rows, int cols);				/* Save a screen segment.		*/
int vrss(unsigned char *loc);								/* Restore a screen segment.		*/
void vtitle(const char *titlestr);

/*
**	MISC Function Prototypes
*/
extern int gcalc (void);
extern int gcalend (void);
extern int gclock (void);
extern int gnotepad (void);
extern int gpuzzle (void);
extern int gzones (void);

extern void libvideo_version (char *version);

extern int isdebug (void);
extern void set_isdebug_false (void);
extern void set_isdebug_true (void);
extern void set_isdebug (void);

extern void set_vsharedscreen_true(void);
extern int vsharedscreen(void);

extern int valert (char *message, char *option_enter, char *option_1, char *option_16);
extern int vbell (void);
extern int vbuffering (int state);
extern int vbuffering_on (void);
extern const char* vcapterm(void);
extern int vcapload (void);
extern int vcapnull (char *control_string, char *cap_string, int dispfl);
extern int vcharset (int char_set);
extern char vcheck (void);
extern int vcontrol (char *string);
extern int vcontrol_flush(void);
extern int vcap_reset_terminal(void);
extern int vcap_init_terminal(void);
extern int vcut (char *string);
extern int vedge (register int line);
extern int verase (int control);
extern void vexit (void);
extern int vfnkey (int key);
extern char vgetc (void);
extern char vgetcto (int timer);
extern int vgeterr (void);
extern int vgetm (void);
extern int vgetm_timed (int seconds, int *status);
extern int vgoto (int nl, int nc, int ol, int oc, int op);
extern int vgrid (int irow, int icol, int nrows, int ncols, int rden, int cden);
extern int visible (char c, int a);
extern void vlanguage (char *path);
extern int vml (int y);
extern int vmlx (int top, int y);
extern int vmode (int control);
extern int vmove (int line, int column);
extern int vonexit (int newvalue);
extern int vpaste (int max);
extern int vpopscr (void);
extern int vprint (char *format, ...);
extern int vpushc (char char_to_be_pushed);
extern void vpushscr (void);
extern int vputc (char ch);
extern int vputlen (char *outstr, char *instr, int length);
extern void vraw_stty_restore (void);
extern int vraw_stty_sane (void);
extern void vraw_stty_save (void);
extern void vraw_stty_sync (void);
extern void vrawattribute(int atr);
extern void vrawmove(int row, int col);
extern int vrawprint (char *buf);
extern int vrawflush(void);
extern int vrawputc (char ch);
extern int vrefresh (int what);
extern int vrelease (void);
extern int vroll (int top, int bottom);
extern int vscroll_frwd_avail(void);
extern int vscroll_rvrs_avail(void);
extern int vscreen (int state);
extern int vset (int item, int state);
extern void vseterr (int error);
extern void vshut (void);
extern int vslew (int nrows, int ncols);
extern int vstate (int action);
extern void vsynch (void);
extern int vtext (int display, int row, int column, char *text, ...);
extern int vrawntcn_get_mouse_position( int *row, int *col );
extern void* vtty_alloc (void);
extern int vtty_set (void* tt);
extern int vtty_get (void* tt);
extern void vcap_set_vcapfile(const char *vcappath, const char* termtype);
extern char GetASCIIGraphicsChar(unsigned char graph_char);
extern void vwait(int seconds, int hundredths);

#ifndef VIDEO_META_DEFS
#define VIDEO_META_DEFS

/*	The defines for FUNCTION KEYS ***** Only add to end. Never delete any. *****						*/

#define VKEY_F0 			1
#define VKEY_F1 			2
#define VKEY_F2 			3
#define VKEY_F3 			4
#define VKEY_F4 			5
#define VKEY_F5 			6
#define VKEY_F6 			7
#define VKEY_F7 			8
#define VKEY_F8 			9
#define VKEY_F9 			10
#define VKEY_F10			11
#define VKEY_F11			12
#define VKEY_F12			13
#define VKEY_F13			14
#define VKEY_F14			15
#define VKEY_F15			16
#define VKEY_F16			17
#define VKEY_F17			18
#define VKEY_F18			19
#define VKEY_F19			20
#define VKEY_F20			21
#define VKEY_F21			22
#define VKEY_F22			23
#define VKEY_F23			24
#define VKEY_F24			25
#define VKEY_F25			26
#define VKEY_F26			27
#define VKEY_F27			28
#define VKEY_F28			29
#define VKEY_F29			30
#define VKEY_F30                 31
#define VKEY_F31                 32
#define VKEY_F32                 33
#define VKEY_HOME 		34
#define VKEY_BACKSPACE 		35
#define VKEY_DOWN_ARROW 		36
#define VKEY_LEFT_ARROW 		37
#define VKEY_RIGHT_ARROW 	38
#define VKEY_UP_ARROW 		39
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
#define VKEY_DELETE              83
#define VKEY_INSERT              84
#define VKEY_NEXT_SCR            85
#define VKEY_PREV_SCR            86
#define VKEY_SELECT              87
#define GENERIC_BACKTAB         88
#define GENERIC_REMOVE          89
#define GENERIC_SELECT          90
#define GENERIC_CANCEL          91
#define GENERIC_RETURN          92 
#define GENERIC_ENTER           93
#define GENERIC_REFRESH         94
#define VKEY_TAB                 95
#define VKEY_HELP                96
#define VKEY_DO                  97
#define VKEY_FIND                98
#define VKEY_USER1               99
#define VKEY_USER2		100
#define VKEY_USER3               101
#define VKEY_USER4               102
#define VKEY_USER5               103
#define VKEY_USER6               104
#define VKEY_USER7               105
#define VKEY_USER8               106
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
#define VKEY_F33 		119
#define VKEY_F34 		120
#define VKEY_F35 		121
#define VKEY_F36 		122
#define VKEY_F37 		123
#define VKEY_F38 		124
#define VKEY_F39 		125
#define VKEY_F40 		126
#define VKEY_F41 		127 
#define VKEY_F42 		128 
#define VKEY_F43 		129
#define VKEY_F44 		130
#define VKEY_F45 		131
#define VKEY_F46 		132
#define VKEY_F47 		133
#define VKEY_F48 		134
#define VKEY_F49 		135
#define VKEY_F50 		136 
#define VKEY_F51 		137
#define VKEY_F52 		138
#define VKEY_F53 		139 
#define VKEY_F54 		140
#define VKEY_F55 		141
#define VKEY_F56 		142
#define VKEY_F57 		143
#define VKEY_F58 		144
#define VKEY_F59 		145
#define VKEY_F60 		146
#define VKEY_F61 		147
#define VKEY_F62 		148
#define VKEY_F63 		149
#define VKEY_F64 		150
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

#define GENERIC_CUT             220
#define GENERIC_PASTE           221
#define GENERIC_COPY            222
#define GENERIC_MARK            223

#define GENERIC_MOUSE		224
					/* Add new keys here, before VC_KEY_COUNT, and continue numbering as above.		*/
#define VC_KEY_COUNT 		225	/* This one is NOT a Video Cap KEY, renumber it to be the last entry before VC_KEY_LAST	*/

#define VC_KEY_LAST 		VC_KEY_COUNT
#define VC_KEY_EXTRA 		256

#define VMBIAS 			256

#define	GOLD_BIT		04000	/* Bit indicates gold function.	*/
#define GREY_BIT	       010000	/* Bit indicates grey function.	*/
#define	PINK_BIT	       020000	/* Bit indicates pink function.	*/
#define BLUE_BIT	       040000	/* Bit indicates blue function.	*/


/*					Soft key definitions.									*/

#define kp_mode			0
#define fk_mode			0
#define k0_mode			0
#define k1_mode			0
#define k2_mode			0
#define k3_mode			0
#define k4_mode			0
#define k5_mode			0
#define k6_mode			0
#define k7_mode			0

#define gold_key		0
#define blue_key		GENERIC_SELECT+VMBIAS
#define grey_key		0
#define pink_key		0

#define home_key		GENERIC_HOME+VMBIAS

#define up_arrow_key		GENERIC_UP+VMBIAS
#define down_arrow_key		GENERIC_DOWN+VMBIAS
#define left_arrow_key		GENERIC_LEFT+VMBIAS
#define right_arrow_key		GENERIC_RIGHT+VMBIAS

#define tab_key			GENERIC_TAB+VMBIAS
#define backtab_key		GENERIC_BACKTAB+VMBIAS
#define newline_key		GENERIC_NEWLINE+VMBIAS

#define delete_key		GENERIC_DELETE+VMBIAS
#define insert_key		GENERIC_INSERT+VMBIAS
#define remove_key		GENERIC_REMOVE+VMBIAS
#define clear_field_key		GENERIC_CLEAR_FIELD+VMBIAS
#define clear_after_key		GENERIC_CLEAR_AFTER+VMBIAS
#define clear_before_key	GENERIC_CLEAR_BEFORE+VMBIAS
#define cancel_key		(GENERIC_CANCEL | BLUE_BIT)
#define enter_key		GENERIC_ENTER+VMBIAS
#define return_key		GENERIC_RETURN+VMBIAS
#define help_key		GENERIC_HELP+VMBIAS

#define fn1_key			GENERIC_PF1+VMBIAS
#define fn2_key			GENERIC_PF2+VMBIAS
#define fn3_key			GENERIC_PF3+VMBIAS
#define fn4_key			GENERIC_PF4+VMBIAS
#define fn5_key			GENERIC_PF5+VMBIAS
#define fn6_key			GENERIC_PF6+VMBIAS
#define fn7_key			GENERIC_PF7+VMBIAS
#define fn8_key			GENERIC_PF8+VMBIAS
#define fn9_key			GENERIC_PF9+VMBIAS
#define fn10_key		GENERIC_PF10+VMBIAS
#define fn11_key		GENERIC_PF11+VMBIAS
#define fn12_key		GENERIC_PF12+VMBIAS
#define fn13_key		GENERIC_PF13+VMBIAS
#define fn14_key		GENERIC_PF14+VMBIAS
#define fn15_key		GENERIC_PF15+VMBIAS
#define fn16_key		GENERIC_PF16+VMBIAS
#define fn17_key		GENERIC_PF17+VMBIAS
#define fn18_key		GENERIC_PF18+VMBIAS
#define fn19_key		GENERIC_PF19+VMBIAS
#define fn20_key		GENERIC_PF20+VMBIAS
#define fn21_key		GENERIC_PF21+VMBIAS
#define fn22_key		GENERIC_PF22+VMBIAS
#define fn23_key		GENERIC_PF23+VMBIAS
#define fn24_key		GENERIC_PF24+VMBIAS
#define fn25_key		GENERIC_PF25+VMBIAS
#define fn26_key		GENERIC_PF26+VMBIAS
#define fn27_key		GENERIC_PF27+VMBIAS
#define fn28_key		GENERIC_PF28+VMBIAS
#define fn29_key		GENERIC_PF29+VMBIAS
#define fn30_key		GENERIC_PF30+VMBIAS
#define fn31_key		GENERIC_PF31+VMBIAS
#define fn32_key		GENERIC_PF32+VMBIAS
#define trigger1 		TRIGGER1+VMBIAS
#define trigger2 		TRIGGER2+VMBIAS
#define trigger3 		TRIGGER3+VMBIAS
#define trigger4 		TRIGGER4+VMBIAS
#define trigger5 		TRIGGER5+VMBIAS
#define trigger6 		TRIGGER6+VMBIAS
#define trigger7 		TRIGGER7+VMBIAS
#define trigger8 		TRIGGER8+VMBIAS

#define key_user1 		VKEY_USER1+VMBIAS

#define key_paste           	GENERIC_PASTE+VMBIAS
#define key_cut             	GENERIC_CUT+VMBIAS

#ifdef key_copy
#undef key_copy
#endif
#define key_copy            	GENERIC_COPY+VMBIAS

#ifdef key_mark
#undef key_mark
#endif
#define key_mark            	GENERIC_MARK+VMBIAS

#define v_mouse_click		GENERIC_MOUSE+VMBIAS

#endif /* VIDEO_META_DEFS */



#endif /* video_H */

/*
**	History:
**	$Log: video.h,v $
**	Revision 1.32  2001/10/15 13:27:08  gsl
**	vutil proto
**	
**	Revision 1.31  1997-09-22 12:23:15-04  gsl
**	Change the set_debug_ routines to set_isdebug_
**	Change the VSTATE_RESTORE/SAVE_DEBUG to _STTY
**	add the vsharedscreen() routines
**
**	Revision 1.30  1997-07-15 13:02:54-04  gsl
**	Put bitwise OR into parens
**	Was causing mouse movement to fail if there was an error field
**	on the screen.
**
**	Revision 1.29  1997-07-08 17:08:58-04  gsl
**	Remove many unneeded defines.
**	Replace some defines with enums
**	Make more specific defines for each function.
**
**	Revision 1.28  1997-06-05 10:57:51-04  scass
**	Added #ifdef #undef directives for vline, key_copy, and key_mark
**
**	Revision 1.27  1997-05-21 13:54:30-04  gsl
**	Added all the metakey defines from vdata.h
**
**	Revision 1.26  1997-01-11 15:50:00-05  gsl
**	Added VOP_ defines for voptimize() and added VSCREEN_NOOP for vscreen()
**
**	Revision 1.25  1996-11-13 17:28:47-08  gsl
**	Remove DUMP_OUTPUT replaced with vcontrol_flush()
**	Add vrawflush()
**
**	Revision 1.24  1996-11-12 15:07:34-08  jockc
**	added proto for vtitle
**
**	Revision 1.23  1996-10-10 11:38:26-07  scass
**	Added prototyped for vtty_alloc(), vtty_set(),
**	and vtty_get().
**
**	Revision 1.22  1996-09-03 15:14:27-07  gsl
**	Add vrawntcn_get_mouse_position()
**
**	Revision 1.21  1996-08-01 10:41:53-07  jockc
**	added prototypes for 2 NT console functions
**
**	Revision 1.20  1996-07-26 09:42:29-07  gsl
**	Fix some prototypes that have had unsigned removed
**
**	Revision 1.19  1996-07-25 16:41:50-07  gsl
**	Change dataname from "new" to "newvalue" as "new" is a C++ reserved word.
**
**	Revision 1.18  1996-07-18 09:26:51-07  jockc
**	fixed some vrawntcn prototypes (vshut, vrawstty*)
**
**	Revision 1.17  1996-07-17 14:41:29-07  gsl
**	Add prototypes
**
**	Revision 1.16  1996-07-17 14:30:49-07  gsl
**	Add function prototypes for all the video API calls
**
**	Revision 1.15  1996-07-16 13:18:18-07  gsl
**	Add prototypes
**
**	Revision 1.14  1996-07-12 10:09:29-07  gsl
**	Add standard headers and prototypes.
**
**
**
*/
