/* This routine provides an interface to C routines */

/*
** Copyright (C) 1993-1995,1997-2003,2006-2008,2010,2012,2016 Micro Focus.
** All rights reserved.
*/

/* Users of the ACUCOBOL-GT runtime may freely modify and distribute	*/
/* this file as they see fit in order to support an ACUCOBOL-GT based	*/
/* application.  */

/*  System header files should be included first, before any ACUCOBOL	*/
/*  header files (like sub.h).  (And include windows.h here, so any	*/
/*  other inclusions of it will be no-ops.)  */

#ifdef	ACU_SOURCE_FILENAME
#undef	ACU_SOURCE_FILENAME
#endif	/* ACU_SOURCE_FILENAME */
#define	ACU_SOURCE_FILENAME	"lib/sub.c"
const char what_lib_sub_c_str[] = "@(#) " ACU_SOURCE_FILENAME " $Date: 2017-12-22 17:24:40 +0000 (Fri, 22 Dec 2017) $$Rev: 72641 $";

#include <stdio.h>
#include <string.h>
#if	defined(_WINDOWS) || defined(_WIN64)
#define	_WINSOCKAPI_	/* Prevent inclusion of winsock.h */
#include <windows.h>
#endif	/* _WINDOWS */
#include <stdarg.h>

#include "sub.h"

#include "rtcallbk.h"       /* rm-style callback table definition */

/* Include the direct 'C' interface */
#include "direct.c"

/* Include the RM/COBOL-85 compatible 'C' interface */
#include "sub85.c"

/* Include the new RM/COBOL-85 compatible 'C' interface that includes */
/* the window handle, RUNTIME_CALLS_TABLE, and GIVING at ARGUMENT_ENTRY[-1] */
#include "sub85rm.c"

/* Include runtime configuration file */
#include "config85.c"

#ifdef	FORTRAN
/* Include the direct FORTRAN interface */
#include "fortran.c"
#endif	/* FORTRAN */

#undef	ACU_SOURCE_FILENAME
#define	ACU_SOURCE_FILENAME	"lib/sub.c"


int sub(int, char *[]);
int exam_args(int, char *[]);
int AStartup(char *);
void AShutdown(int);

/* On Windows, this data item must be exported in the DLL */
MFACU_TABLES_t	MFACU_TABLES = {
	CURRENT_MFACU_TABLES_VERSION,
	LIBTABLE,
	LIBDIRECT,
	EXTDATA,
	NULL
};

/* The "sub" function is executed by each COBOL CALL before searching	*/
/* for the called file.  This routine can intercept certain calls to 	*/
/* be handled by C routines.						*/

/* The "sub" function, receives two parameters: an argument count	*/
/* and a vector of passed parameters, just like the C routine "main".	*/
/* The first parameter is always a pointer to the name being called.	*/
/* The remaining parameters are pointers to each USING parameter.	*/
/* Each parameter is passed as an address of the physical data area	*/
/* the variable occupies, it is up to the C routine to know its format.	*/

/* The routine can return a 0 if it handled the particular CALL, the	*/
/* runtime system will consider the CALL to have been completed.	*/
/* It should return -1 if it doesn't handle the CALL.  The runtime	*/
/* system will continue its searching algorithm.  Finally, any positive	*/
/* return will be treated as an error and cause a STOP RUN to be	*/
/* performed.								*/

int
sub(int argc A_UNUSED, char *argv[] A_UNUSED)
{
    /* Sample 'SYSTEM' call.  Note that this is redundant because	*/
    /* of the other 'SYSTEM' sample in sub85.c.  For this reason,	*/
    /* it is commented out here.  */
#if	0
    if (strcmp(argv[0], "SYSTEM") == 0) {
	w_reset_term();
	system(argv[1]);
	w_set_term();
	return Okay;
    }
#endif	/* 0 */
	return NotFound;
} /* sub */


/* exam_args is called immediately upon startup and is passed the 	*/
/* command line arguments that were passed to Acurun.  This can be used	*/
/* to examine the arguments for special values.  Return 0 for default	*/
/* handling by Acurun, -1 to halt or 1 to cause Acurun to ignore 	*/
/* errors in the command line arguments.  This last case should be used	*/
/* if you introduce new arguments that Acurun can't understand.  Note	*/
/* that arguments used by Acurun cannot have their meaning changed.	*/
/* It is also not possible to supply arguments of the form "-f arg" 	*/
/* because Acurun will interpert "arg" to be the name of the program to	*/
/* run.  You can, however, put additional arguments after the program	*/
/* name.  */

/* The runtime ignores all arguments that begin with a double dash (--)	*/
/* except for the few that it knows about and knows how to handle.  If	*/
/* you add arguments to the runtime using this function, it is best to	*/
/* use --argument.  Note that the runtime has reserved for itself most	*/
/* single-character arguments that start with a dash (such as -d, -e, 	*/
/* etc.)								*/

int
exam_args(int argc A_UNUSED, char *argv[] A_UNUSED)
{
    return 0;
} /* exam_args */


/* startup is called after initialization but before the main program	*/
/* is loaded.  It can return 0 to continue, or anything else to cause	*/
/* a halt.  It is passed the name of the program to load.  */

int
AStartup(char *pgm_name A_UNUSED)
{
    return 0;
} /* AStartup */


/* shutdown is called after all files are closed but before the final	*/
/* halt of the program.  It is passed 0 if this is a normal STOP RUN,	*/
/* 1 if this is due to an error.  */

void
AShutdown(int error_halt A_UNUSED)
{
    return;
} /* AShutdown */

/* */
