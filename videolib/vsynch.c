			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

#include "video.h"									/* Reference all video definitions.	*/
#include "vlocal.h"									/* Reference all internal definition.	*/

vsynch()						/* Warn video et all that something has	gone on behind its back.	*/
{
	extern unsigned char vchr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];	/* Video character map.			*/
	extern unsigned char vatr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];
	extern unsigned char vmap_cng[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];
	extern int synch_required, deferred, new_screen, vscr_wid;			/* Video data.				*/
	extern int vpri_op, vmov_op, vmod_op, vchs_op, vrol_op, vscr_op, vera_op;
	extern int vcur_lin, vcur_col, vcur_atr, vchr_set, vmap_top;

	memset(vchr_map,' ',sizeof(vchr_map));						/* Clear video's maps.			*/
	memset(vatr_map,0,sizeof(vatr_map));
	memset(vmap_cng,0,sizeof(vmap_cng));

	synch_required = TRUE;								/* Synchronize vwang next time.		*/
	deferred = FALSE;								/* Actions are not deferred.		*/
	new_screen = TRUE;								/* Next screen is new.			*/
	vscr_wid = 80;									/* Reset width.				*/
	vmap_top = 0;									/* Reset top of screen.			*/

	vcur_lin = 0;									/* Current line on screen.		*/
	vcur_col = 0;									/* Current column on screen.		*/
	vcur_atr = 0;									/* Current character attributes.	*/
	vchr_set = 0;									/* Current character set.		*/

	vmov_op = OFF;									/* Optimize move cursor actions.	*/
	vmod_op = OFF;									/* Optimize rendition.			*/
	vchs_op = OFF;									/* Character set optimization is off.	*/
	vrol_op = OFF;									/* Scroll area optimization is off.	*/
	vscr_op = OFF;									/* Screen width.			*/
	vera_op = OFF;									/* Erase optimization is off.		*/
}
