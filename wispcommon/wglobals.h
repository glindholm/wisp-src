/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
******************************************************************************
*/


/*
**									
**			WGLOBALS.H	-	Wisp Global variables.	
**									
**			Any global variables used by WISP can be placed here.
**			This module can be linked with modules that need these
**			globals but do not need every module from WISP too.
**
**			
*/


#ifndef WGLOBALS_DEF
#define WGLOBALS_DEF

#include <stdio.h>
#include <time.h>
#ifdef unix
#include <sys/types.h>
#endif

#include "idsistd.h"
#include "wfiles.h"

const char *WL_get_wisp_option(const char *keyword);

/*
**	VMS is completely brain dead when it comes to initialization of extern variables; it reserves space and initializes
**	it when it sees a variable declared [extern]. For VMS we always include the init clause and not the extern.
**
**	The above is true BUT fixed it by adding a call to wglobals() a dummy routine to force it to include wglobals.c.
*/

#ifdef  EXT_WGLOBALS
#define EXTERN_DEF
#define INIT_DEF_ZERO   =0
#define INIT_DEF_ONE    =1
#define INIT_DEF_NULL   =NULL
#else
#define EXTERN_DEF extern
#define INIT_DEF_ZERO
#define INIT_DEF_ONE
#define INIT_DEF_NULL
#endif

void wisp_set_LINKPARM(int i);
int  wisp_get_LINKPARM();

void wisp_set_LINKCOMPCODE(int4 i);
int4 wisp_get_LINKCOMPCODE();

void wisp_set_LINKRETCODE(int4 i);
int4 wisp_get_LINKRETCODE();

void wisp_set_LOGOFFFLAG(int4 i);
int4 wisp_get_LOGOFFFLAG();

void wisp_set_CANEXITFLAG(int4 i);
int4 wisp_get_CANEXITFLAG();

void wisp_set_WSTARTTIME();
time_t wisp_get_WSTARTTIME();

void wisp_set_noswap(int i);
int  wisp_get_noswap();

#define WISP_RUNNAME_SIZE	8
void wisp_set_runname(const char name[WISP_RUNNAME_SIZE]);
const char* wisp_get_runname();

const char* wisp_get_filelock();
const char* wisp_get_hardlock();

#define PROGNAME_SIZE	8
void wisp_set_progname(const char* progname);
const char* wisp_get_progname();

#define SCREENNAME_SIZE	32
void wisp_set_screenname(const char* screenname);
const char* wisp_get_screenname();

void wisp_set_noprogscrn(int flag);
int wisp_get_noprogscrn();

EXTERN_DEF wisp_fstruct	*WL_g_temp_file_list INIT_DEF_NULL;			/* The list of temp files		*/
EXTERN_DEF wisp_pstruct *WL_g_print_file_list INIT_DEF_NULL;			/* The list of print files		*/

enum e_printqueue
{
	PQ_DEFAULT = 0,
	PQ_UNIQUE,
	PQ_LP,
	PQ_ILP,
	PQ_NP,
	PQ_GENERIC
};
EXTERN_DEF enum e_printqueue WL_opt_printqueue;						/* Which Print Queue mechanism?		*/
EXTERN_DEF char             *WL_opt_printqueue_manager;					/* Print Queue Manager command or NULL	*/

EXTERN_DEF int		WL_opt_helpstyle	INIT_DEF_ONE;				/* 1=Wang HELP style 2=non-Wang style	*/

EXTERN_DEF int		WL_opt_batchman		INIT_DEF_ZERO;				/* Use a batch queue manage product	*/
EXTERN_DEF char		*WL_batchman_name;


EXTERN_DEF int		ede_synch		INIT_DEF_ZERO;				/* Synchronization flag for EDE.	*/

#undef EXTERN_DEF
#undef INIT_DEF_ZERO

void WL_globals();

#endif

/*
**	History:
**	$Log: wglobals.h,v $
**	Revision 1.25  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.24  2002/10/11 20:39:52  gsl
**	Detect runtime Cobol type without needing INITWISP call.
**	For ACU set in sub85.c,
**	For utils set via WRUNCONFIG
**	Default to MF on UNIX
**	
**	Revision 1.23  2002/07/12 20:40:45  gsl
**	Global unique WL_ changes
**	
**	Revision 1.22  2002/07/12 19:10:25  gsl
**	Global unique WL_ changes
**	
**	Revision 1.21  2002/07/11 20:29:21  gsl
**	Fix WL_ globals
**	
**	Revision 1.20  2002/07/09 04:14:02  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.19  2002/07/01 04:02:45  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.18  2002/06/26 01:42:51  gsl
**	Remove VMS code
**	
**	Revision 1.17  2002/06/21 03:50:25  gsl
**	Remove softlock
**	
**	Revision 1.16  1998/10/22 18:06:30  gsl
**	Change the flist and plist to g_temp_file_list and g_print_file_list.
**	
**	Revision 1.15  1998-10-21 10:10:44-04  gsl
**	change print queue options
**
**	Revision 1.14  1997-10-21 09:50:16-04  gsl
**	remove WISPPROGID
**
**	Revision 1.13  1997-04-28 17:57:49-04  gsl
**	Removed ACUFILESTAT, it has been replace with lastacufilestat()
**
**	Revision 1.12  1996-08-22 20:31:12-04  gsl
**	Removed PGRPID - use wgetpgrp() it get it.
**
**	Revision 1.11  1996-07-23 11:17:58-07  gsl
**	drcs update
**
**
**
*/
