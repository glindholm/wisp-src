/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
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
******************************************************************************
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

/*
**	Old symbols
*/

#define vchr_map VL_vchr_map
#define vatr_map VL_vatr_map
#define vmap_cng VL_vmap_cng
#define vmap_top VL_vmap_top

#define vcur_lin VL_vcur_lin
#define vcur_col VL_vcur_col
#define vcur_atr VL_vcur_atr
#define vchr_set VL_vchr_set
#define vcur_set VL_vcur_set
#define vscr_atr VL_vscr_atr
#define vrol_top VL_vrol_top
#define vrol_bot VL_vrol_bot


#define MAX_ESC 25								/* Maximum length of terminal control sequence.	*/
#define MAX_GENERIC 64								/* Maximum generic control keys.		*/

#define SUBSTITUTION_TABLE_SIZE    64						/* Size of substitution table.			*/

#define VMAP_CNG_OLDDATA ((unsigned char) 255)  				/* Was -1 but vmap_cng is unsigned */

#ifdef VIDEO_DATA_ROOT

/*						Global "general" data.								*/

int VL_synch_required = FALSE;							/* Coordination flag between VIDEO and vwang().	*/
int VL_exit_options = VONX_NORMALIZE | VONX_MOVE_AND_SCROLL;			/* Default exit action is move and scroll.	*/
int VL_verbose = TRUE;								/* Report internal errors.			*/
int VL_vscr_wid = 80;								/* Actual current width (flag in vset tables).	*/
FILE *VL_macro_input = NULL;							/* File pointer to macro input file.		*/
FILE *VL_macro_output = NULL;							/* File pointer to macro output file.		*/

/*					Screen, and line attributes and screen map.						*/

unsigned char VL_vchr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Define the character map.			*/
unsigned char VL_vatr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Define the attribute map.			*/
unsigned char VL_vmap_cng[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Define the change tracking map.		*/
int VL_vmap_top = 0;								/* Virtual top of char and attribute maps.	*/


/*					Data involved in screen tracking.							*/

int VL_vcur_lin = 0;								/* Current line on screen.			*/
int VL_vcur_col = 0;								/* Current column on screen.			*/
int VL_vcur_atr = 0;								/* Current character attributes.		*/
int VL_vchr_set = 0;								/* Current character set.			*/
int VL_vcur_set[VSET_TABLE_SIZE] = {NORMAL,OFF,VISIBLE};			/* Current terminal settings.			*/
int VL_vscr_atr = VSCREEN_NARROW|VSCREEN_DARK;					/* Assumed screen attributes.			*/
int VL_vrol_top = 0;								/* Top of current scrolling area.		*/
int VL_vrol_bot = MAX_LINES_PER_SCREEN-1;					/* Bottom of current scrolling area.		*/


/*		Data involved in deferred optimization (captured the first time deferred action enabled).			*/

int VL_tcur_lin = 0;								/* True line number (when optimizing).		*/
int VL_tcur_col = 0;								/* True column (when optimizing).		*/

/*	Optimization control flags (all optimization is initially off to allow the first action to actually go through).	*/

int VL_vmov_op = OFF;								/* Optimize move cursor actions in vmove()	*/
int VL_vmod_op = OFF;								/* Optimize rendition mode actions in vmode()	*/
int VL_vchs_op = OFF;								/* Character set optimization is off.		*/
int VL_vrol_op = OFF;								/* Scroll area optimization is off.		*/
int VL_vscr_op = OFF;								/* Screen width and color optimization is off.	*/
int VL_vlin_op = ON;								/* Line drawing optimization starts on.		*/


/*					Data used for cursor positioning during dialogue menus.					*/

int VL_vdl_lin = -1;								/* -1 Indicates not valid.			*/
int VL_vdl_col = 0;

/*					Data to control dumping of the output buffer.						*/

int VL_vb_count = 0;								/* Characters in output buffer.			*/
int VL_vb_pure = FALSE;								/* Output line feeds as new lines (aka C).	*/
int VL_video_inited = FALSE;							/* Video initialization control flag.		*/
int VL_state_active = FALSE;							/* State 0 active control flag.			*/

/*					Paste buffer control information.							*/

int VL_paste_index = 0;								/* Index into the paste buffer.			*/
unsigned char VL_paste_buffer[MAX_COLUMNS_PER_LINE] = { 0 };			/* Buffer to hold paste information.		*/


/*					Soft key definitions.									*/


#else	/* #ifdef VIDEO_DATA_ROOT */

/*						Reference "general" data.							*/

extern int VL_synch_required;
extern int VL_exit_options;
extern int VL_verbose;
extern int VL_vscr_wid;
extern FILE *VL_macro_input;
extern FILE *VL_macro_output;

/*					Screen, and line attributes and screen map.						*/

extern unsigned char VL_vchr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];
extern unsigned char VL_vatr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];
extern unsigned char VL_vmap_cng[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];
extern int VL_vmap_top;

/*					Data involved in screen tracking.							*/

extern int VL_vcur_lin;
extern int VL_vcur_col;
extern int VL_vcur_atr;
extern int VL_vchr_set;
extern int VL_vcur_set[VSET_TABLE_SIZE];
extern int VL_vscr_atr;
extern int VL_vrol_top;
extern int VL_vrol_bot;


/*		Data involved in deferred optimization (captured the first time deferred action enabled).			*/

extern int VL_tcur_lin;
extern int VL_tcur_col;


/*	Optimization control flags (all optimization is initially off to allow the first action to actually go through).	*/

extern int VL_vmov_op;
extern int VL_vmod_op;
extern int VL_vchs_op;
extern int VL_vrol_op;
extern int VL_vscr_op;
extern int VL_vlin_op;

/*					Data for controling the cursor when in dialogue boxes.					*/

extern int VL_vdl_lin;
extern int VL_vdl_col;

/*					Data to control dumping of the output buffer.						*/

extern int VL_vb_count;
extern int VL_vb_pure;
extern int VL_video_inited;
extern int VL_state_active;

/*					Cut/Paste control									*/

extern int VL_paste_index;
extern unsigned char VL_paste_buffer[MAX_COLUMNS_PER_LINE];



#endif	/* NOT VIDEO_DATA_ROOT */


/*					Terminal control sequence definitions.						*/

#ifndef ESC_COMPAT
#define ESC_COMPAT

#define rvrsidx_esc	VL_vcapvalue(SCROLL_REVERSE)				/* Reverse line feed (reverse index).	*/
#define snglwide_esc	""          						/* Set line to single width.		*/
#define dblwide_esc	""          						/* Set line to double width.		*/
#define dbltop_esc	""          						/* Set line to top half of double hi.	*/
#define dblbot_esc	""          						/* Set line to bottom half double hi.	*/
#define efuls_esc	VL_vcapvalue(CLR_SCREEN)				/* Clear full screen.			*/
#define efbos_esc	""          						/* Clear from beginning of screen.	*/
#define eteos_esc	VL_vcapvalue(CLEAR_EOS)					/* Clear to the end of screen.		*/
#define ecurl_esc	""          						/* Clear current line.			*/
#define efbol_esc	VL_vcapvalue(CLEAR_BOL)					/* Clear from beginning of line.	*/
#define eteol_esc	VL_vcapvalue(CLEAR_EOL)					/* Clear to the end of current line.	*/
#define mdclr_esc	VL_vcapvalue(EXIT_ATTRIBUTE_MODE)			/* Clear all renditions.		*/
#define mdbld_esc	VL_vcapvalue(ENTER_BOLD_MODE)				/* Set mode to bold rendition.		*/
#define mdundr_esc	VL_vcapvalue(ENTER_UNDERLINE_MODE)			/* Set mode to underscore rendition.	*/
#define mdblk_esc	VL_vcapvalue(ENTER_BLINK_MODE)				/* Set mode to blink rendition.		*/
#define mdrvrs_esc	VL_vcapvalue(ENTER_REVERSE_MODE)			/* Set mode to reverse rendition.	*/
#define chterm_esc	""          						/* Find what type of terminal it is??	*/
#define swide_esc	VL_vcapvalue(WIDE_MODE)					/* Set screen to wide.			*/
#define snarw_esc	VL_vcapvalue(NARROW_MODE)				/* Set screen to narrow.		*/
#define slight_esc	VL_vcapvalue(SCREEN_REVERSE_MODE)			/* Set screen to light.			*/
#define sdark_esc	VL_vcapvalue(SCREEN_NORMAL_MODE)			/* Set screen to dark.			*/
#define scrarea_esc	VL_vcapvalue(CHANGE_SCROLL_REGION)			/* Select the scrolling area.		*/
#define mvrowcol_esc	VL_vcapvalue(CURSOR_ADDRESS)				/* Move to given position.		*/
#define mvleft_esc	""          						/* Move cursor left (for > 3 pos.)	*/
#define mvright_esc	""          						/* Move cursor right (for > 3 pos.)	*/
#define mvup_esc	VL_vcapvalue(CURSOR_UP)					/* Move cursor up one row.		*/
#define mvupup_esc	""          						/* Move cursor up several rows. 	*/
#define mvdown_esc	""          						/* Move cursor down several rows.	*/
#define defchs_esc	VL_vcapvalue(EXIT_GRAPHICS_MODE)			/* Default character set		*/
#define grchs_esc	VL_vcapvalue(ENTER_GRAPHICS_MODE)			/* Graphics character font set		*/
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
#define arapon_esc	VL_vcapvalue(ENTER_AM_MODE)				/* Auto wrap mode is on.		*/
#define arapoff_esc	VL_vcapvalue(EXIT_AM_MODE)				/* Auto wrap mode is off.		*/
#define arepton_esc	""          						/* Auto repeat is on.			*/
#define areptoff_esc	""          						/* Auto repeat is off.			*/
#define ilaceon_esc	""          						/* Interlace mode is on.		*/
#define ilaceoff_esc	""          						/* Interlace mode is off.		*/
#define nlmdon_esc	""          						/* Line feed is a new line.		*/
#define nlmdoff_esc	""          						/* Line feed is a line feed.		*/
#define keylckon_esc	""          						/* Lock the keyboard.			*/
#define keylckoff_esc	""          						/* Unlock the keyboard.			*/
#define insmdon_esc	VL_vcapvalue(ENTER_INSERT_MODE)				/* Insert mode on.			*/
#define insmdoff_esc	VL_vcapvalue(EXIT_INSERT_MODE)				/* Insert mode off.			*/
#define ptermff_esc	""          						/* Print screen on form feed.		*/
#define ptermnone_esc	""          						/* No print terminator.			*/
#define pextscrl_esc	""          						/* Print scroll region only.		*/
#define pextfull_esc	""          						/* Print full screen.			*/
#define aprnton_esc	""          						/* Auto print mode on.			*/
#define aprntoff_esc	""          						/* Auto print mode off.			*/
#define prnton_esc	""          						/* Turn local printer on.		*/
#define prntoff_esc	""          						/* Turn local printer off.		*/
#define cursron_esc	VL_vcapvalue(CURSOR_VISIBLE)				/* Turn text cursor on.			*/
#define cursroff_esc	VL_vcapvalue(CURSOR_INVISIBLE)				/* Turn text cursor off.		*/

#endif	/* #ifndef ESC_COMPAT */

#endif /* VDATA_H */

/*
**	History:
**	$Log: vdata.h,v $
**	Revision 1.26  2003/06/20 15:38:08  gsl
**	VL_ globals
**	
**	Revision 1.25  2003/06/20 15:37:44  gsl
**	VL_ globals
**	
**	Revision 1.24  2003/06/20 15:04:28  gsl
**	VL_ globals
**	
**	Revision 1.23  2003/02/05 21:15:03  gsl
**	fix -Wall warnings
**	
**	Revision 1.22  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.21  2002/07/17 21:06:01  gsl
**	VL_ globals
**	
**	Revision 1.20  2002/07/16 13:40:23  gsl
**	VL_ globals
**	
**	Revision 1.19  2002/07/15 20:16:08  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.18  2002/07/15 17:10:02  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.17  2002/07/12 20:40:44  gsl
**	Global unique WL_ changes
**	
**	Revision 1.16  1998/10/13 18:47:58  gsl
**	Add VMAP_CNG_OLDDATA define
**	
**	Revision 1.15  1997-07-12 17:49:33-04  gsl
**	moved deferred to vdefer.c
**
**	Revision 1.14  1997-07-08 16:53:41-04  gsl
**	Change to use VL_vcapvalue() to access the vcapdef[] values
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
