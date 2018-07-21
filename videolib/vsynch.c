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

#include <string.h>
#include "video.h"									/* Reference all video definitions.	*/
#include "vlocal.h"									/* Reference all internal definition.	*/
#include "vmodules.h"
#include "vdata.h"

void VL_vsynch()						/* Warn video et all that something has	gone on behind its back.	*/
{
	memset(vchr_map,' ',sizeof(vchr_map));						/* Clear video's maps.			*/
	memset(vatr_map,0,sizeof(vatr_map));
	memset(vmap_cng,0,sizeof(vmap_cng));

	VL_synch_required = TRUE;							/* Synchronize vwang next time.		*/
	vdefer(VDEFER_OFF);								/* Actions are not deferred.		*/
	VL_vscr_wid = 80;									/* Reset width.				*/
	vmap_top = 0;									/* Reset top of screen.			*/

	vcur_lin = 0;									/* Current line on screen.		*/
	vcur_col = 0;									/* Current column on screen.		*/
	vcur_atr = 0;									/* Current character attributes.	*/
	vchr_set = 0;									/* Current character set.		*/

	VL_vmov_op = OFF;									/* Optimize move cursor actions.	*/
	VL_vmod_op = OFF;									/* Optimize rendition.			*/
	VL_vchs_op = OFF;									/* Character set optimization is off.	*/
	VL_vrol_op = OFF;									/* Scroll area optimization is off.	*/
	VL_vscr_op = OFF;									/* Screen width.			*/
}
/*
**	History:
**	$Log: vsynch.c,v $
**	Revision 1.16  2003/06/20 15:04:28  gsl
**	VL_ globals
**	
**	Revision 1.15  2003/01/31 19:25:55  gsl
**	Fix copyright header
**	
**	Revision 1.14  2002/07/16 13:40:19  gsl
**	VL_ globals
**	
**	Revision 1.13  2002/07/15 20:16:15  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.12  2002/07/12 20:40:45  gsl
**	Global unique WL_ changes
**	
**	Revision 1.11  1997/07/09 16:30:21  gsl
**	Removed externs
**	Removed obsolete items
**	
**	Revision 1.10  1996-10-11 18:16:21-04  gsl
**	drcs update
**
**
**
*/
