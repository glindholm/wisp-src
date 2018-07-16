/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
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

extern int gcal2();
extern int gcalculator();
extern int gpreferences();
extern int imemcpy();
extern int imemset();
extern void jredit();
extern void vbldfilepath();
extern int vdetpos();
extern int vfblank();
extern int vgrid();
extern void vkeyhelp();

extern int vmap();
extern int vmacro();
extern int4 vmenu();
extern void vputlocal_error_message();
extern char vrawcheck();
extern int vrawcursor();
extern void vrawerase();
extern void vrawerasetype();
extern int vrawexit();
extern char vrawinput();
extern int vrawscroll();
extern void vrawsetcursor();
extern void vrawsetscroll();
extern int vscroll();
extern void vtrace();
extern void vrawtimeout();
extern void vrawtimeout_set();
extern void vrawtimeout_clear();
extern int vrawtimeout_check();
extern int vtrigger();
extern void v_modeflag();

#endif /* vmodules_H */

/*
**	History:
**	$Log: vmodules.h,v $
**	Revision 1.16  1997-07-09 11:05:33-04  gsl
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
