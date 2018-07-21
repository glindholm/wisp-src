/*
	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
*/

/*
**	File:		envs.h
**
**	Project:	wproc
**
**	Purpose:	Defines for wproc environment variables.
**
*/

#ifndef envs_H
#define envs_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/
#define	WPROC_TOPFILE_ENV	"WPROC_TOPFILE"		/* The top procedure file name in this process group	*/
#define WPROC_TOPGID_ENV	"WPROC_TOPGID"		/* The GID when topfile is set, used to determine reset */
#define WPROC_BASELEVEL_ENV	"WPROC_BASELEVEL"	/* The starting nesting_level from previous wproc	*/
#define WPROC_TRACELEVEL_ENV	"WPROC_TRACELEVEL"	/* The level a trace is started (and can be stopped.)	*/
#define WPROC_TRACEACTIVE_ENV	"WPROC_TRACEACTIVE"	/* Is a trace active ("true"/"false")			*/
#define WPROC_TRACEFILE_ENV	"WPROC_TRACEFILE"	/* The trace file name					*/
#define WPROC_TRACEFLAGS_ENV	"WPROC_TRACEFLAGS"	/* The trace flags in use.				*/

/*
**	Function Prototypes
*/

#endif /* envs_H */

/*
**	History:
**	$Log: envs.h,v $
**	Revision 1.5  1998/08/31 19:50:33  gsl
**	drcs update
**	
**	Revision 1.4  1998-08-31 15:13:46-04  gsl
**	drcs update
**
**	Revision 1.3  1995-10-16 09:50:23-04  gsl
**	Removed the GLOBALS & RESET environment varibals and add the TOPGID var.
**	The TOPGID is the GID for the top file in the chain, this should remain
**	constant except across a SUBMIT.  If the GID is not the same as TOPGID
**	then a RESET is implide
**
 * Revision 1.2  1995/10/12  16:52:55  gsl
 * Add WPROC_RESET and comment the environment vars
 *
 * Revision 1.1  1995/10/12  12:20:49  gsl
 * Initial revision
 *
**
*/

