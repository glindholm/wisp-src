static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

#include <string.h>
#include "video.h"									/* Reference all video definitions.	*/
#include "vlocal.h"									/* Reference all internal definition.	*/
#include "vmodules.h"
#include "vdata.h"

void vsynch()						/* Warn video et all that something has	gone on behind its back.	*/
{
	memset(vchr_map,' ',sizeof(vchr_map));						/* Clear video's maps.			*/
	memset(vatr_map,0,sizeof(vatr_map));
	memset(vmap_cng,0,sizeof(vmap_cng));

	synch_required = TRUE;								/* Synchronize vwang next time.		*/
	vdefer(VDEFER_OFF);								/* Actions are not deferred.		*/
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
}
/*
**	History:
**	$Log: vsynch.c,v $
**	Revision 1.11  1997-07-09 12:30:21-04  gsl
**	Removed externs
**	Removed obsolete items
**
**	Revision 1.10  1996-10-11 18:16:21-04  gsl
**	drcs update
**
**
**
*/
