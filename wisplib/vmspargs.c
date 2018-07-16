static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*	
**	vmspargs.c
		This routine was included in LINKSUBS.C but needed to be broken into it's own piece so
		as to activate a correct sharable library under VMS.  5/22/91  SMB
*/

#include <stdio.h>

#if defined(MSDOS) || defined(_MSC_VER)
#include <io.h>
#endif

#include "idsistd.h"
#include "wglobals.h"
#include "wdefines.h"
#include "werrlog.h"
#include "link.h"
#include "wexit.h"

#define ROUTINE		28000

extern	char	LINKPARMFILLER[8];						/* Filler to test bug */
extern	char	LINKPARMKEY[80];						/* The key to the temp file name.		*/
extern	char	LINKPARMPRG[80];						/* The PROGNAME to link to.			*/
extern	int4	LINKPARMCNT;							/* The number of arguments passed (0-32)	*/
extern	char	*LINKPARMPTR[MAX_LINK_PARMS];					/* The passed arguments				*/
extern	int4	LINKPARMLEN[MAX_LINK_PARMS];					/* The lengths of the passed arguments		*/

void VMSPARGS(void)							/* This routine is call on the way back from a LINK 	*/
									/* with VMSCOBOL. It writes the LINKAGE parameters	*/
									/* from the linked program to the tmp file.		*/
{
	FILE	*fp;
	int4	size,i;
	static	int	first=1;

	if (!LINKPARM) return;

	if (!first) return;
	first = 0;

	if ( 0 == access ( LINKPARMKEY, 0 ) )						/* If it already exists;		*/
		unlink( LINKPARMKEY ) ;						/* Delete it before opening		*/

	fp = fopen(LINKPARMKEY,"w");							/* Open the Parm file			*/
	if ( !fp )
	{
		werrlog(ERRORCODE(12),LINKPARMKEY,errno,"VMSPARGS",0,0,0,0,0);
		wexit(ERRORCODE(12));
	}

	size = strlen(LINKPARMPRG);
	fwrite( &size, 4, 1, fp );							/* Write the program path		*/
	fwrite( LINKPARMPRG, (int)size, 1, fp );

	fwrite( &LINKPARMCNT, 4, 1, fp );						/* Write the parmcnt			*/

	for ( i=0; i < LINKPARMCNT; i++ )						/* Write the parms			*/
	{
		fwrite( &LINKPARMLEN[i], 4, 1, fp );
		fwrite( LINKPARMPTR[i],(int)LINKPARMLEN[i],1,fp);
	}

	size = -1;
	fwrite( &LINKCOMPCODE,	4, 1, fp );						/* Write the comp code			*/
	fwrite( &LINKRETCODE,	4, 1, fp );						/* Write the return code		*/
	fwrite( &size,		4, 1, fp );						/* Write the CANCEL EXIT flag		*/
	fwrite( &LOGOFFFLAG,	4, 1, fp );						/* Write the LOGOFF flag		*/

	fclose(fp);
}
/*
**	History:
**	$Log: vmspargs.c,v $
**	Revision 1.9  1996-08-19 18:33:05-04  gsl
**	drcs update
**
**
**
*/
