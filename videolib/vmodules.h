/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/


/*
**	File:		vmodules.h
**
**	Project:	???
**
**	RCS:		$Source:$
**
**	Purpose:	???
**
*/

#ifndef vmodules_H
#define vmodules_H
/*
**	Includes
*/
#include <stdio.h>

#ifndef INTDEF_H
#include "vintdef.h"
#endif

#include "vutil.h"
#include "vmenu.h"
#include "vline.h"
#include "verase.h"
#include "vtrim.h"
#include "vprint.h"
#include "vscreen.h"
#include "vmove.h"

#include "video.h"

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/

extern int VL_gcal2();
extern int VL_imemcpy();
extern int VL_imemset();
extern void VL_vbldfilepath();
extern int VL_vdetpos();
extern int VL_vgrid();

extern int  VL_vmap();

extern char vrawcheck();
extern int vrawcursor();
extern void vrawerase();
extern void vrawerasetype();
extern int vrawexit();
extern char vrawinput();
extern int vrawscroll();
extern void vrawsetcursor();
extern void vrawsetscroll();
extern void vrawtimeout();
extern void vrawtimeout_set();
extern void vrawtimeout_clear();
extern int vrawtimeout_check();

extern int VL_vtrigger();
extern void v_modeflag();

FILE *VL_vopenf();

#endif /* vmodules_H */

/*
**	History:
**	$Log: vmodules.h,v $
**	Revision 1.22  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.21  2002/07/18 21:04:22  gsl
**	Remove MSDOS code
**	
**	Revision 1.20  2002/07/15 20:56:39  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.19  2002/07/15 20:16:11  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.18  2002/07/15 17:52:55  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.17  2002/07/15 17:10:04  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.16  1997/07/09 15:05:33  gsl
**	Remove old routines
**	
**	Revision 1.15  1997-01-08 16:38:49-05  gsl
**	Remove strpos() as all have been replaced with strstr()
**
**	Revision 1.14  1996-11-12 09:13:39-08  jockc
**	renamed vrawtimeout_* in this module
**
**	Revision 1.13  1996-07-17 14:41:11-07  gsl
**	remove duplicate prototypes
**
**	Revision 1.12  1996-07-17 14:29:51-07  gsl
**	Removed duplicate function definitions
**
**	Revision 1.11  1996-03-28 14:30:00-08  gsl
**	Move a number of declares into there own header files
**
**
*/
