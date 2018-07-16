/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
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

EXTERN_DEF int4			LINKPARM 	INIT_DEF_ZERO;				/* Were we started by a LINK		*/

#ifdef VMS
EXTERN_DEF int4			tabtype 	INIT_DEF_ZERO;
#endif


EXTERN_DEF int4			LINKCOMPCODE 	INIT_DEF_ZERO;
EXTERN_DEF int4			LINKRETCODE  	INIT_DEF_ZERO;
EXTERN_DEF int4			LOGOFFFLAG   	INIT_DEF_ZERO;
EXTERN_DEF int4			CANEXITFLAG  	INIT_DEF_ZERO;
EXTERN_DEF int  		lnk_depth    	INIT_DEF_ZERO;				/* Call stack depth on VMS		*/

EXTERN_DEF time_t		WSTARTTIME	INIT_DEF_ZERO;				/* The start time of the process	*/
EXTERN_DEF int 			swap_words 	INIT_DEF_ONE;				/* Define the word swap flag.		*/
EXTERN_DEF int 			noswap_words 	INIT_DEF_ZERO;				/* Opposite of swap_words		*/
EXTERN_DEF int			werrno 		INIT_DEF_ZERO;				/* Wisp errno				*/

EXTERN_DEF uint4        	wextstat1 	INIT_DEF_ZERO;				/* Wisp extended error number.		*/
EXTERN_DEF uint4        	wextstat2 	INIT_DEF_ZERO;
EXTERN_DEF char			wfilestat[2];						/* Wisp file status of last wfilechk	*/
EXTERN_DEF char			filelock[2];						/* Locked file - file I/O status	*/
EXTERN_DEF char			hardlock[2];						/* Hard lock on record - file status	*/
EXTERN_DEF char			softlock[2];						/* Soft lock on record - file status	*/
EXTERN_DEF char 		WISPRUNNAME[8];						/* Define the Run name field. 		*/
EXTERN_DEF char			WISPTRANVER[21];					/* The Translator version		*/
EXTERN_DEF int			create_gbl	INIT_DEF_ZERO;				/* Flag to decide if need to really	*/
											/* create shared memory file or not.	*/
EXTERN_DEF char 		wisp_progname[9];					/* Define the program name field.	*/
EXTERN_DEF char 		wisp_screen[33];					/* Define the screen name field.	*/

EXTERN_DEF fstruct 		*flist		INIT_DEF_NULL;				/* the pointer to the list of files	*/
EXTERN_DEF fstruct 		*flptr		INIT_DEF_NULL;				/* the pointer to the current entry	*/

EXTERN_DEF pstruct 		*plist 		INIT_DEF_NULL;				/* pointer to printer files		*/
EXTERN_DEF pstruct 		*plptr		INIT_DEF_NULL;

EXTERN_DEF int		opt_errflag_found	INIT_DEF_ZERO;				/* Found ERRFLAG in OPTIONS file	*/
EXTERN_DEF int		opt_errflag		INIT_DEF_ZERO;				/* The value of ERRFLAG from OPTIONS	*/
EXTERN_DEF int		opt_signalsoff		INIT_DEF_ZERO;				/* Turn signal trapping off		*/
EXTERN_DEF int		opt_nulldisplay		INIT_DEF_ZERO;				/* Display NULLs as a space.		*/
EXTERN_DEF int		opt_outputverifyoff	INIT_DEF_ZERO;				/* Suppress PF3 to continue screens	*/
EXTERN_DEF int		opt_createvolumeon	INIT_DEF_ZERO;				/* Auto create volume if not found	*/
EXTERN_DEF int		opt_idsiprint		INIT_DEF_ONE;				/* Use the IDSI print spooler		*/
EXTERN_DEF int		opt_pqilp		INIT_DEF_ZERO;				/* Use ILP				*/
EXTERN_DEF int		opt_pqunique		INIT_DEF_ONE;				/* Use UNIQUE (default)			*/
EXTERN_DEF int		opt_idnumeric		INIT_DEF_ZERO;				/* Return numeric user ID from EXTRACT.	*/
EXTERN_DEF int		opt_idfive		INIT_DEF_ZERO;				/* Return char 5-7 user ID from EXTRACT.*/
EXTERN_DEF int		opt_allstatuskeys	INIT_DEF_ZERO;				/* Pass all status keys thru to declr	*/
EXTERN_DEF int		opt_helpstyle		INIT_DEF_ONE;				/* 1=Wang HELP style 2=non-Wang style	*/

EXTERN_DEF int		opt_batchqueue		INIT_DEF_ZERO;				/* Use a batch queue product		*/
EXTERN_DEF char		batchqueue_name[255];
EXTERN_DEF int		opt_batchman		INIT_DEF_ZERO;				/* Use a batch queue manage product	*/
EXTERN_DEF char		batchman_name[255];


EXTERN_DEF int		ede_synch		INIT_DEF_ZERO;				/* Synchronization flag for EDE.	*/

#undef EXTERN_DEF
#undef INIT_DEF_ZERO

void wglobals();

#endif

/*
**	History:
**	$Log: wglobals.h,v $
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
