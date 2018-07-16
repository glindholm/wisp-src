			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

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

#include <time.h>
#ifdef unix
#include <sys/types.h>
#endif
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

EXTERN_DEF long			LINKPARM 	INIT_DEF_ZERO;				/* Were we started by a LINK		*/

#ifdef VMS
EXTERN_DEF long			tabtype 	INIT_DEF_ZERO;
#endif


EXTERN_DEF long			LINKCOMPCODE 	INIT_DEF_ZERO;
EXTERN_DEF long			LINKRETCODE  	INIT_DEF_ZERO;
EXTERN_DEF long			LOGOFFFLAG   	INIT_DEF_ZERO;
EXTERN_DEF long			CANEXITFLAG  	INIT_DEF_ZERO;
EXTERN_DEF int 			retstat      	INIT_DEF_ZERO;				/* Global used by isexec		*/
EXTERN_DEF int  		lnk_depth    	INIT_DEF_ZERO;				/* Call stack depth on VMS		*/

EXTERN_DEF time_t		WSTARTTIME	INIT_DEF_ZERO;				/* The start time of the process	*/
EXTERN_DEF int 			swap_words 	INIT_DEF_ONE;				/* Define the word swap flag.		*/
EXTERN_DEF int 			noswap_words 	INIT_DEF_ZERO;				/* Opposite of swap_words		*/
EXTERN_DEF int			werrno 		INIT_DEF_ZERO;				/* Wisp errno				*/

EXTERN_DEF unsigned long	wextstat1 	INIT_DEF_ZERO;				/* Wisp extended error number.		*/
EXTERN_DEF unsigned long	wextstat2 	INIT_DEF_ZERO;
EXTERN_DEF char			ACUFILESTAT[4];						/* Acucobol extended file status	*/
EXTERN_DEF char			wfilestat[2];						/* Wisp file status of last wfilechk	*/
EXTERN_DEF char			filelock[2];						/* Locked file - file I/O status	*/
EXTERN_DEF char			hardlock[2];						/* Hard lock on record - file status	*/
EXTERN_DEF char			softlock[2];						/* Soft lock on record - file status	*/
EXTERN_DEF char 		WISPRUNNAME[8];						/* Define the Run name field. 		*/
EXTERN_DEF char 		WISPPROGID[8];						/* Define the COBOL program ID field. 	*/
EXTERN_DEF char			WISPTRANVER[21];					/* The Translator version		*/
EXTERN_DEF int			create_gbl	INIT_DEF_ZERO;				/* Flag to decide if need to really	*/
											/* create shared memory file or not.	*/
EXTERN_DEF int 			PGRPID		INIT_DEF_ZERO;				/* Global Process Group ID.		*/
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
EXTERN_DEF int		opt_idsiprint		INIT_DEF_ZERO;				/* Use the IDSI print spooler		*/
EXTERN_DEF int		opt_idnumeric		INIT_DEF_ZERO;				/* Return numeric user ID from EXTRACT.	*/
EXTERN_DEF int		opt_allstatuskeys	INIT_DEF_ZERO;				/* Pass all status keys thru to declr	*/
EXTERN_DEF int		opt_helpstyle		INIT_DEF_ONE;				/* 1=Wang HELP style 2=non-Wang style	*/

EXTERN_DEF char		werrlog_path[80];

EXTERN_DEF int		ede_synch		INIT_DEF_ZERO;				/* Synchronization flag for EDE.	*/

#undef EXTERN_DEF
#undef INIT_DEF_ZERO

#endif

