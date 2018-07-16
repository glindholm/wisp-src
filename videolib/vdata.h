/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
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

#ifndef VDATA_H
#define VDATA_H

#ifndef FILE
#include <stdio.h>
#endif

#include "vlocal.h"

#define MAX_ESC 25								/* Maximum length of terminal control sequence.	*/
#define MAX_GENERIC 64								/* Maximum generic control keys.		*/

#define SUBSTITUTION_TABLE_SIZE    64						/* Size of substitution table.			*/

#define VMAP_CNG_OLDDATA ((unsigned char) 255)  				/* Was -1 but vmap_cng is unsigned */

#ifdef VIDEO_DATA_ROOT

/*						Global "general" data.								*/

int tttype = UNKNOWN_TERMINAL;							/* Terminal type (obtained from vterminal()).	*/
int vclrhome = FALSE;								/* On full screen erase, cursor stays put.	*/
int vmovebias = 0;								/* Move style is ANSI to begin.			*/
int synch_required = FALSE;							/* Coordination flag between VIDEO and vwang().	*/
int exit_options = VONX_NORMALIZE | VONX_MOVE_AND_SCROLL;			/* Default exit action is move and scroll.	*/
int debugging = FALSE;								/* The debugger is not on.			*/
int verbose = TRUE;								/* Report internal errors.			*/
int vscr_wid = 80;								/* Actual current width (flag in vset tables).	*/
int vis_space = VMODE_REVERSE|VMODE_UNDERSCORE|VMODE_BLINK;			/* Visible on normal screen in these cases.	*/
int rvis_space = VMODE_REVERSE|VMODE_UNDERSCORE|VMODE_BLINK|VMODE_BOLD;		/* Visible on reverse screen in these cases.	*/
int force_200 = TRUE;								/* Attempt to force VT100 into VT220 mode.	*/
int color_first = TRUE;								/* Force a color change on first vscreen call.	*/
int width_first = TRUE;								/* Force a width change on first vscreen call.	*/
FILE *macro_input = NULL;							/* File pointer to macro input file.		*/
FILE *macro_output = NULL;							/* File pointer to macro output file.		*/
unsigned char refresh_character = REFRESH_CHARACTER;				/* Refresh the screen on this character.	*/
unsigned char trigger_character = TRIGGER_CHARACTER;				/* Bring up the triggered event.		*/
unsigned char vchs_default = 'B';						/* Use "A" for U.K. or "B" for U.S./Canada.	*/

/*					Screen, and line attributes and screen map.						*/

unsigned char vchr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE] = { 0 };	/* Define the character map.			*/
unsigned char vatr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE] = { 0 };	/* Define the attribute map.			*/
unsigned char vmap_cng[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE] = { 0 };	/* Define the change tracking map.		*/
int vmap_top = 0;								/* Virtual top of char and attribute maps.	*/


/*					Data involved in screen tracking.							*/

int vcur_lin = 0;								/* Current line on screen.			*/
int vcur_col = 0;								/* Current column on screen.			*/
int vcur_atr = 0;								/* Current character attributes.		*/
int vchr_set = 0;								/* Current character set.			*/
int vcur_set[VSET_TABLE_SIZE] = {NORMAL,OFF,VISIBLE};				/* Current terminal settings.			*/
int vscr_atr = VSCREEN_NARROW|VSCREEN_DARK;					/* Assumed screen attributes.			*/
int vrol_top = 0;								/* Top of current scrolling area.		*/
int vrol_bot = MAX_LINES_PER_SCREEN-1;						/* Bottom of current scrolling area.		*/


/*		Data involved in deferred optimization (captured the first time deferred action enabled).			*/

int tcur_lin = 0;								/* True line number (when optimizing).		*/
int tcur_col = 0;								/* True column (when optimizing).		*/

/*	Optimization control flags (all optimization is initially off to allow the first action to actually go through).	*/

int vmov_op = OFF;								/* Optimize move cursor actions in vmove()	*/
int vmod_op = OFF;								/* Optimize rendition mode actions in vmode()	*/
int vchs_op = OFF;								/* Character set optimization is off.		*/
int vrol_op = OFF;								/* Scroll area optimization is off.		*/
int vscr_op = OFF;								/* Screen width and color optimization is off.	*/
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


#else	/* #ifdef VIDEO_DATA_ROOT */

/*						Reference "general" data.							*/

extern int tttype;
extern int vclrhome;
extern int vmovebias;
extern int synch_required;
extern int exit_options;
extern int debugging;
extern int verbose;
extern int vscr_wid;
extern int vis_space;
extern int rvis_space;
extern int force_200;
extern int color_first;
extern int width_first;
extern FILE *macro_input;
extern FILE *macro_output;
extern unsigned char refresh_character;
extern unsigned char vchs_default;

/*					Screen, and line attributes and screen map.						*/

extern unsigned char vchr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];
extern unsigned char vatr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];
extern unsigned char vmap_cng[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];
extern int vmap_top;


/*					Data involved in screen tracking.							*/

extern int vcur_lin;
extern int vcur_col;
extern int vcur_atr;
extern int vchr_set;
extern int vcur_set[VSET_TABLE_SIZE];
extern int vscr_atr;
extern int vrol_top;
extern int vrol_bot;


/*		Data involved in deferred optimization (captured the first time deferred action enabled).			*/

extern int tcur_lin;
extern int tcur_col;


/*	Optimization control flags (all optimization is initially off to allow the first action to actually go through).	*/

extern int vmov_op;
extern int vmod_op;
extern int vchs_op;
extern int vrol_op;
extern int vscr_op;
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

extern int vb_count;
extern int vb_pure;
extern int video_inited;
extern int state_active;

/*					Cut/Paste control									*/

extern int paste_index;
extern unsigned char paste_buffer[MAX_COLUMNS_PER_LINE];

/*					Meta character processing definitions.							*/

extern int meta_substitute[SUBSTITUTION_TABLE_SIZE][2];



#endif	/* NOT VIDEO_DATA_ROOT */


/*					Terminal control sequence definitions.						*/

#ifndef ESC_COMPAT
#define ESC_COMPAT

#define rvrsidx_esc	vcapvalue(SCROLL_REVERSE)				/* Reverse line feed (reverse index).	*/
#define snglwide_esc	""          						/* Set line to single width.		*/
#define dblwide_esc	""          						/* Set line to double width.		*/
#define dbltop_esc	""          						/* Set line to top half of double hi.	*/
#define dblbot_esc	""          						/* Set line to bottom half double hi.	*/
#define efuls_esc	vcapvalue(CLR_SCREEN)					/* Clear full screen.			*/
#define efbos_esc	""          						/* Clear from beginning of screen.	*/
#define eteos_esc	vcapvalue(CLEAR_EOS)					/* Clear to the end of screen.		*/
#define ecurl_esc	""          						/* Clear current line.			*/
#define efbol_esc	vcapvalue(CLEAR_BOL)					/* Clear from beginning of line.	*/
#define eteol_esc	vcapvalue(CLEAR_EOL)					/* Clear to the end of current line.	*/
#define mdclr_esc	vcapvalue(EXIT_ATTRIBUTE_MODE)				/* Clear all renditions.		*/
#define mdbld_esc	vcapvalue(ENTER_BOLD_MODE)				/* Set mode to bold rendition.		*/
#define mdundr_esc	vcapvalue(ENTER_UNDERLINE_MODE)				/* Set mode to underscore rendition.	*/
#define mdblk_esc	vcapvalue(ENTER_BLINK_MODE)				/* Set mode to blink rendition.		*/
#define mdrvrs_esc	vcapvalue(ENTER_REVERSE_MODE)				/* Set mode to reverse rendition.	*/
#define chterm_esc	""          						/* Find what type of terminal it is??	*/
#define swide_esc	vcapvalue(WIDE_MODE)					/* Set screen to wide.			*/
#define snarw_esc	vcapvalue(NARROW_MODE)					/* Set screen to narrow.		*/
#define slight_esc	vcapvalue(SCREEN_REVERSE_MODE)				/* Set screen to light.			*/
#define sdark_esc	vcapvalue(SCREEN_NORMAL_MODE)				/* Set screen to dark.			*/
#define scrarea_esc	vcapvalue(CHANGE_SCROLL_REGION)				/* Select the scrolling area.		*/
#define mvrowcol_esc	vcapvalue(CURSOR_ADDRESS)					/* Move to given position.		*/
#define mvleft_esc	""          						/* Move cursor left (for > 3 pos.)	*/
#define mvright_esc	""          						/* Move cursor right (for > 3 pos.)	*/
#define mvup_esc	vcapvalue(CURSOR_UP)					/* Move cursor up one row.		*/
#define mvupup_esc	""          						/* Move cursor up several rows. 	*/
#define mvdown_esc	""          						/* Move cursor down several rows.	*/
#define defchs_esc	vcapvalue(EXIT_GRAPHICS_MODE)				/* Default character set		*/
#define grchs_esc	vcapvalue(ENTER_GRAPHICS_MODE)				/* Graphics character font set		*/
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
#define arapon_esc	vcapvalue(ENTER_AM_MODE)					/* Auto wrap mode is on.		*/
#define arapoff_esc	vcapvalue(EXIT_AM_MODE)					/* Auto wrap mode is off.		*/
#define arepton_esc	""          						/* Auto repeat is on.			*/
#define areptoff_esc	""          						/* Auto repeat is off.			*/
#define ilaceon_esc	""          						/* Interlace mode is on.		*/
#define ilaceoff_esc	""          						/* Interlace mode is off.		*/
#define nlmdon_esc	""          						/* Line feed is a new line.		*/
#define nlmdoff_esc	""          						/* Line feed is a line feed.		*/
#define keylckon_esc	""          						/* Lock the keyboard.			*/
#define keylckoff_esc	""          						/* Unlock the keyboard.			*/
#define insmdon_esc	vcapvalue(ENTER_INSERT_MODE)				/* Insert mode on.			*/
#define insmdoff_esc	vcapvalue(EXIT_INSERT_MODE)				/* Insert mode off.			*/
#define ptermff_esc	""          						/* Print screen on form feed.		*/
#define ptermnone_esc	""          						/* No print terminator.			*/
#define pextscrl_esc	""          						/* Print scroll region only.		*/
#define pextfull_esc	""          						/* Print full screen.			*/
#define aprnton_esc	""          						/* Auto print mode on.			*/
#define aprntoff_esc	""          						/* Auto print mode off.			*/
#define prnton_esc	""          						/* Turn local printer on.		*/
#define prntoff_esc	""          						/* Turn local printer off.		*/
#define cursron_esc	vcapvalue(CURSOR_VISIBLE)					/* Turn text cursor on.			*/
#define cursroff_esc	vcapvalue(CURSOR_INVISIBLE)				/* Turn text cursor off.		*/

#endif	/* #ifndef ESC_COMPAT */

#endif /* VDATA_H */

/*
**	History:
**	$Log: vdata.h,v $
**	Revision 1.16  1998-10-13 14:47:58-04  gsl
**	Add VMAP_CNG_OLDDATA define
**
**	Revision 1.15  1997-07-12 17:49:33-04  gsl
**	moved deferred to vdefer.c
**
**	Revision 1.14  1997-07-08 16:53:41-04  gsl
**	Change to use vcapvalue() to access the vcapdef[] values
**	Remove non-global data items
**
**	Revision 1.13  1997-05-21 13:53:23-04  gsl
**	Change all the metakey variables into defines and move to video.h
**
**	Revision 1.12  1996-07-18 12:12:13-04  jockc
**	add ifdef for WIN32 for 80 columns in tab table
**
**	Revision 1.11  1996-03-12 05:27:45-08  gsl
**	Move holding_output to vsection.c
**
**
**
*/
