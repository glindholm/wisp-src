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

extern	char	g_link_pfiller[8];						/* Filler to test bug */
extern	char	g_link_parmfile[80];						/* The key to the temp file name.		*/
extern	char	g_link_pname[80];						/* The PROGNAME to link to.			*/
extern	int4	g_link_parmcnt;							/* The number of arguments passed (0-32)	*/
extern	char	*g_link_parmptr[MAX_LINK_PARMS];				/* The passed arguments				*/
extern	int4	g_link_parmlen[MAX_LINK_PARMS];					/* The lengths of the passed arguments		*/

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

	if ( 0 == access ( g_link_parmfile, 0 ) )					/* If it already exists;		*/
		unlink( g_link_parmfile ) ;						/* Delete it before opening		*/

	fp = fopen(g_link_parmfile,"w");						/* Open the Parm file			*/
	if ( !fp )
	{
		werrlog(ERRORCODE(12),g_link_parmfile,errno,"VMSPARGS",0,0,0,0,0);
		wexit(ERRORCODE(12));
	}

	size = strlen(g_link_pname);
	fwrite( &size, 4, 1, fp );							/* Write the program path		*/
	fwrite( g_link_pname, (int)size, 1, fp );

	fwrite( &g_link_parmcnt, 4, 1, fp );						/* Write the parmcnt			*/

	for ( i=0; i < g_link_parmcnt; i++ )						/* Write the parms			*/
	{
		fwrite( &g_link_parmlen[i], 4, 1, fp );
		fwrite( g_link_parmptr[i],(int)g_link_parmlen[i],1,fp);
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
**	Revision 1.10  1998-12-09 10:41:56-05  gsl
**	Update global names
**
**	Revision 1.9  1996-08-19 18:33:05-04  gsl
**	drcs update
**
**
**
*/
