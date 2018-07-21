/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
*/

/*
**	File:		linksubs.c
**
**	Project:	wisp/lib
**
**	Purpose:	Subroutines for Wang USERSUB LINK
**
*/


#include <stdio.h>
#include <errno.h>
#include <string.h>

#ifdef unix
#include <ctype.h>
#include <signal.h>
#include <unistd.h>
#endif

#ifdef WIN32
#include <direct.h>
#include <io.h>
#include <process.h>
#endif

#include "idsistd.h"
#include "idsisubs.h"
#include "link.h"
#include "wglobals.h"
#include "wdefines.h"
#include "wisplib.h"
#include "osddefs.h"
#include "paths.h"
#include "wexit.h"
#include "wperson.h"
#include "wanguid.h"

#include "werrlog.h"

#ifdef WIN32
#include "isonames.h"
#endif


/*
	The following should be OK even though they are global 
	because only used for true dynamic linking so reenterant
	code is not required.

	They are global because were used in vmspargs.c. (VMS)
*/

static char	g_link_parmfile[COB_FILEPATH_LEN + 1];	/* The key to the temp file name.		*/
static char	g_link_pname[80];			/* The PROGNAME to link to.			*/
static int4	g_link_parmcnt = 0;			/* The number of arguments passed (0-32)	*/
static char	*g_link_parmptr[MAX_LINK_PARMS];	/* The passed arguments				*/
static int4	g_link_parmlen[MAX_LINK_PARMS];		/* The lengths of the passed arguments		*/


/*
	DYNAMIC LINKING WITH PARAMETERS
	===============================

	Program "A" does a call "LINK" to link to "B", parameters are pass thru tempfile "X".

	From program "A" LINK will call "WL_writeunixlink" to create "X" then fork and exec "B".
	Program "B" will call "ACUGARGS" to read then delete file "X".
	Program "B" runs normally then at exit calls "ACUPARGS" to re-create "X" with updated values, 
	then "B" exits.
	LINK calls "WL_readunixlink" to read then delete file "X" and update program "A"s data area.
	LINK finishes and program "A" continues.

*/

/*
**	ROUTINE:	WL_writeunixlink()
**
**	FUNCTION:	Write the temp link parm file (used by unix and WIN32)
**
**	DESCRIPTION:	{Full detailed description}...
**
**	ARGUMENTS:
**	pname		Program to link to (fullpath)
**	parmcnt		Number of parameters
**	parm_list	The parameters
**	len_lisst	The parameter lengths
**	linkkey		The returned "key" to the temp parm file
**
*/
void WL_writeunixlink(const char *pname, int parmcnt, struct str_parm *parm_list, struct str_len *len_list, char *linkkey)
{
	FILE	*fp;
	int4	size, i;
	int4	l_CANEXITFLAG;

	sprintf(linkkey,"%s%sLINK_%s_%u", wisplinkdir(NULL), DIR_SEPARATOR_STR, WL_longuid(), (unsigned)getpid());		
	WL_makepath(linkkey);

	fp = fopen(linkkey,FOPEN_WRITE_BINARY);						/* Open the parm file			*/
	if ( !fp )
	{
		WL_werrlog(WERRCODE(28012),linkkey,errno,"WL_writeunixlink",0,0,0,0,0);
		WL_wexit(WERRCODE(28012));
	}

	size = strlen( pname );								/* Write the program to exec		*/
	fwrite( &size, 4, 1, fp );
	fwrite( pname, size, 1, fp );

	fwrite( &parmcnt, 4, 1, fp );							/* Write the parm count			*/

	for ( i=0; i < parmcnt; i++ )							/* Write the parms			*/
	{
		size = len_list->len[i];
		fwrite( &size, 4, 1, fp );
		fwrite( parm_list->parm[i],size,1,fp);
	}

	size = -1;
	l_CANEXITFLAG = wisp_get_CANEXITFLAG();

	fwrite( &size,		4, 1, fp );						/* Write the compcode			*/
	fwrite( &size, 		4, 1, fp );						/* Write the returncode			*/
	fwrite( &l_CANEXITFLAG,	4, 1, fp );						/* Write the Cancel-Exit flag		*/
	fwrite( &size,		4, 1, fp );						/* Write the LOGOFF flag		*/

	fflush(fp);
	fclose(fp);

	WL_wtrace("LINK","PARMFILE","Created PARMFILE [%s] pname=[%s] parmcnt=[%d]",
	       linkkey, pname, parmcnt);
}

/*
**	ROUTINE:	WL_readunixlink()
**
**	FUNCTION:	Read and delete the temp link parm file (used by unix and WIN32)
**
**	DESCRIPTION:	
**			This routine is called after the LINK has completed
**			to read the tmp parm file back into the parms from
**			the calling program.
**
**			- if file is unchanged then program "B" doesn't understand this protocol.
**			- if file is missing then program "B" terminated abnormally.
**			- if file is changed then program "B" terminated normally.
**
**	ARGUMENTS:
**	parmcnt		Number of parameters
**	parm_list	The parameters
**	len_lisst	The parameter lengths
**	linkkey		The "key" to the temp parm file
**	compcode	Returned completion code
**				16  = Temp file not found - linked to program aborted
**				-1  = Temp file unchanged - linked to program doesn't understand protocol
**	returncode	Returned return code
**
**
*/
void WL_readunixlink(int parmcnt, struct str_parm *parm_list, struct str_len *len_list, const char *linkkey, 
		  int4 *compcode, int4 *returncode)
{
	FILE	*fp;
	int4	size,i;
	char	tempstr[80];
	int4	l_LOGOFFFLAG = 0;
	size_t	rc;

	fp = fopen(linkkey,FOPEN_READ_BINARY);				/* Open the Parm file			*/
	if ( !fp )
	{
		WL_wtrace("LINK","PARMFILE","Remove: Unable to open PARMFILE [%s] for reading [errno=%d]", linkkey, errno);
		
		*compcode = 16;						/* File missing, program aborted.	*/
		*returncode = 0;
		return;
	}

	/*
	**	Each fread should return an rc==1 (1 full item read)
	*/
	rc = fread( &size, 4, 1, fp );					/* Read the program path		*/
	if (1==rc){ rc = fread( tempstr, size, 1, fp );	}

	if (1==rc){ rc = fread( &size, 4, 1, fp ); }			/* Read the parmcnt			*/

	for ( i=0; i < parmcnt; i++ )					/* Read the parms			*/
	{
		if (1==rc){ rc = fread( &size, 4, 1, fp ); }
		if (1==rc){ rc = fread( parm_list->parm[i],size,1,fp); }
	}

	if (1==rc){ rc = fread( compcode,    4, 1, fp ); }		/* Read the comp code			*/
	if (1==rc){ rc = fread( returncode,  4, 1, fp ); }		/* Read the return code			*/
	if (1==rc){ rc = fread( tempstr,     4, 1, fp ); }		/* Read the Cancel-Exit flag		*/
	if (1==rc){ rc = fread( &l_LOGOFFFLAG, 4, 1, fp ); }		/* Read the LOGOFF flag			*/

	if (rc != 1)
	{
		/*
		**	Error reading the file
		*/
		WL_werrlog_error(WERRCODE(28020),"LINK","PARMFILE"
			"Remove: Error reading PARMFILE [%s] [errno=%d]", linkkey, errno);
		
		*compcode = 16;	
		*returncode = 0;
	}

	fclose(fp);
	wisp_unlink(linkkey);						/* delete the tmp file			*/

	if (rc != 1)
	{
		return;
	}


	if ( *compcode == -1 ) 						/* Program didn't understand protocol.	*/
	{
		*returncode = -1;
		l_LOGOFFFLAG = 0;
	}

	wisp_set_LOGOFFFLAG(l_LOGOFFFLAG);

	WL_wtrace("LINK","PARMFILE","Removed PARMFILE [%s] compcode=[%d] retcode=[%d] logoff=[%d]",
	       linkkey, *compcode, *returncode, l_LOGOFFFLAG);
}

/*
	LINKGARG

		This routine is call on the way into a LINK. It reads the tmp parameter file and loads the parameters into the
		parameters and deletes the file. 
		A pcount=-1 means that no parameter file was found.
*/

void LINKGARG(pname,pcount,p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,p11,p12,p13,p14,p15,p16,
		      p17,p18,p19,p20,p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,p31,p32)

char	*pname;									/* Program to call				*/
int2	*pcount;								/* Number of parms.				*/
char	*p1,*p2,*p3,*p4,*p5,*p6,*p7,*p8,*p9,*p10,*p11,*p12,*p13,*p14,*p15,*p16; /* Parms					*/
char	*p17,*p18,*p19,*p20,*p21,*p22,*p23,*p24,*p25,*p26,*p27,*p28,*p29,*p30,*p31,*p32;
{
	char	linkkey[COB_FILEPATH_LEN];					/* Link parm area key				*/
	const char *ptr;

	ptr = getenv(WISP_LINK_ENV);
	if ( NULL == ptr )							/* If no linkkey then not coming from a link	*/
	{
		*pcount = 0;
		*pname  = '\0';
		return;
	}
	WL_cstr2cobx(linkkey,ptr,COB_FILEPATH_LEN);

	ACUGARGS(linkkey,pname, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,p11,p12,p13,p14,p15,p16,
				p17,p18,p19,p20,p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,p31,p32);

	*pcount = (short) g_link_parmcnt;
}

void ISRUNUSING(void)
{
	wisp_set_LINKPARM(0);	/* Were coming from a RUN USING not a LINK 	*/
}

void LINKPARG(void)
{
	ACUPARGS();
}

void LINKNARG( int2* argcount )
{
	ACUNARGS( argcount );
}

/*
	The original ACUCOBOL versions
*/

void ACUGARGS(linkkey,pname, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,p11,p12,p13,p14,p15,p16,
			p17,p18,p19,p20,p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,p31,p32)
										/* This routine is call on the way into a LINK	*/
										/* from ACULINK. It reads the tmp parameter	*/
										/* file and loads the parameters into the	*/
										/* parameters from ACULINK. ACULINK then CALLs	*/
										/* the LINKed to program.			*/

char	*linkkey;								/* Link parm area key				*/
char	*pname;									/* Program to call				*/
char	*p1,*p2,*p3,*p4,*p5,*p6,*p7,*p8,*p9,*p10,*p11,*p12,*p13,*p14,*p15,*p16; /* Parms					*/
char	*p17,*p18,*p19,*p20,*p21,*p22,*p23,*p24,*p25,*p26,*p27,*p28,*p29,*p30,*p31,*p32;
{
	FILE	*fp;
	int4	size,i;
	char	tempstr[COB_FILEPATH_LEN];
	int4	l_CANEXITFLAG;
	size_t	rc;

	wisp_set_LINKPARM(1);								/* We are inside an ACU link		*/

	g_link_parmptr[0] = p1;								/* Save all the ptrs into ACU_COBOL	*/
	g_link_parmptr[1] = p2;
	g_link_parmptr[2] = p3;
	g_link_parmptr[3] = p4;
	g_link_parmptr[4] = p5;
	g_link_parmptr[5] = p6;
	g_link_parmptr[6] = p7;
	g_link_parmptr[7] = p8;
	g_link_parmptr[8] = p9;
	g_link_parmptr[9] = p10;
	g_link_parmptr[10] = p11;
	g_link_parmptr[11] = p12;
	g_link_parmptr[12] = p13;
	g_link_parmptr[13] = p14;
	g_link_parmptr[14] = p15;
	g_link_parmptr[15] = p16;
	g_link_parmptr[16] = p17;
	g_link_parmptr[17] = p18;
	g_link_parmptr[18] = p19;
	g_link_parmptr[19] = p20;
	g_link_parmptr[20] = p21;
	g_link_parmptr[21] = p22;
	g_link_parmptr[22] = p23;
	g_link_parmptr[23] = p24;
	g_link_parmptr[24] = p25;
	g_link_parmptr[25] = p26;
	g_link_parmptr[26] = p27;
	g_link_parmptr[27] = p28;
	g_link_parmptr[28] = p29;
	g_link_parmptr[29] = p30;
	g_link_parmptr[30] = p31;
	g_link_parmptr[31] = p32;

	WL_cobx2cstr(g_link_parmfile, linkkey, COB_FILEPATH_LEN);

	fp = fopen(g_link_parmfile,FOPEN_READ_BINARY);					/* Open the Parm file			*/
	if ( !fp )
	{
		WL_werrlog(WERRCODE(28012),g_link_parmfile,errno,"LINKGARG",0,0,0,0,0);
		WL_wexit(WERRCODE(28012));
	}

	memset ( pname,' ',80);
	size = 0;
	rc = fread( &size, 4, 1, fp );							/* Read the program path		*/
	if (1==rc) { fread( pname, size, 1, fp ); }

	memcpy(g_link_pname, pname, size );
	g_link_pname[size] = '\0';

	if (1==rc) { fread( &g_link_parmcnt, 4, 1, fp ); }				/* Read the parmcnt			*/

	for ( i=0; i < g_link_parmcnt; i++ )						/* Read the parms			*/
	{
		if (1==rc) { fread( &g_link_parmlen[i], 4, 1, fp ); }
		if (1==rc) { fread( g_link_parmptr[i],g_link_parmlen[i],1,fp); }
	}

	l_CANEXITFLAG = wisp_get_CANEXITFLAG();

	if (1==rc) { fread( &size,	4, 1, fp ); }			/* Read the comp code			*/
	if (1==rc) { fread( &size,	4, 1, fp ); }			/* Read the return code			*/
	if (1==rc) { fread( &l_CANEXITFLAG, 4, 1, fp );	}		/* Read the CANCEL-EXIT flag		*/
	if (1==rc) { fread( tempstr,      4, 1, fp ); }			/* Read the LOGOFF flag			*/

	if (rc != 1)
	{
		/*
		**	Error reading the file
		*/
		WL_werrlog_error(WERRCODE(28020),"LINK","PARMFILE"
			"Consume: Error reading PARMFILE [%s] [errno=%d]", 
			g_link_parmfile, errno);		
	}
	else
	{
		WL_wtrace("LINK","PARMFILE","Consumed PARMFILE [%s] pname=[%s] parmcnt=[%d]",
		       g_link_parmfile, g_link_pname, g_link_parmcnt);
	}

	fclose(fp);

	wisp_unlink(g_link_parmfile);					/* Delete the temp file			*/

}

void ACUPARGS()								/* This routine is call on the way back from a LINK 	*/
									/* with ACU_COBOL. It writes the LINKAGE parameters	*/
									/* from the linked program to the tmp file.		*/
{
	FILE	*fp;
	int4	size,i;
	static	int	first=1;
	int4	l_LINKCOMPCODE;
	int4	l_LINKRETCODE;
	int4	l_LOGOFFFLAG;

	if (!wisp_get_LINKPARM()) return;

	if (!first) return;
	first = 0;

	fp = fopen(g_link_parmfile,FOPEN_WRITE_BINARY);					/* Open the Parm file			*/
	if ( !fp )
	{
		WL_werrlog(WERRCODE(28012),g_link_parmfile,errno,"LINKPARG",0,0,0,0,0);
		WL_wexit(WERRCODE(28012));
	}

	size = strlen(g_link_pname);
	fwrite( &size, 4, 1, fp );							/* Write the program path		*/
	fwrite( g_link_pname, size, 1, fp );

	fwrite( &g_link_parmcnt, 4, 1, fp );						/* Write the parmcnt			*/

	for ( i=0; i < g_link_parmcnt; i++ )						/* Write the parms			*/
	{
		fwrite( &g_link_parmlen[i], 4, 1, fp );
		fwrite( g_link_parmptr[i],g_link_parmlen[i],1,fp);
	}

	l_LINKCOMPCODE = wisp_get_LINKCOMPCODE();
	l_LINKRETCODE = wisp_get_LINKRETCODE();
	size = -1;
	l_LOGOFFFLAG = wisp_get_LOGOFFFLAG();

	fwrite( &l_LINKCOMPCODE, 4, 1, fp );						/* Write the comp code			*/
	fwrite( &l_LINKRETCODE,  4, 1, fp );						/* Write the return code		*/
	fwrite( &size,		4, 1, fp );						/* Write the CANCEL EXIT flag		*/
	fwrite( &l_LOGOFFFLAG,   4, 1, fp );						/* Write the LOGOFF flag		*/

	fclose(fp);

	WL_wtrace("LINK","PARMFILE","Restored PARMFILE [%s] pname=[%s] parmcnt=[%d] compcode=[%d] retcode=[%d]",
	       g_link_parmfile, g_link_pname, g_link_parmcnt, 
	       l_LINKCOMPCODE, l_LINKRETCODE);
}


void ACUNARGS( short* argcount )							/* Return the number of ARGs passed	*/
{
	short i;

	i = g_link_parmcnt;
	*argcount = i;
}


/*	End of	link.c	*/
/*
**	History:
**	$Log: linksubs.c,v $
**	Revision 1.36  2011/10/29 20:09:14  gsl
**	Fix ISO routine name warnins on WIN32
**	
**	Revision 1.35  2003/07/02 18:06:15  gsl
**	fix bug in LOGOFF flag passing across link levels
**	
**	Revision 1.34  2003/07/02 15:02:56  gsl
**	fix bug in LOGOFF flag passing across link levels
**	
**	Revision 1.33  2003/01/31 17:17:42  gsl
**	Fix Wall warnings and copyright
**	
**	Revision 1.32  2002/12/11 17:03:06  gsl
**	use wisp_unlink()
**	
**	Revision 1.31  2002/12/10 17:09:18  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.30  2002/07/18 21:04:27  gsl
**	Remove MSDOS code
**	
**	Revision 1.29  2002/07/12 17:00:58  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.28  2002/07/10 21:05:19  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.27  2002/07/10 04:27:37  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.26  2002/07/01 04:02:39  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.25  2002/06/26 01:42:46  gsl
**	Remove VMS code
**	
**	Revision 1.24  2001/11/08 16:47:32  gsl
**	Add missing include
**	
**	Revision 1.23  2001-10-31 15:33:52-05  gsl
**	replace mkdir() with WL_makepath()
**
**	Revision 1.22  2001-10-10 15:43:52-04  gsl
**	Rename the LINK temp param file to LINK_userid_pid to help prevent
**	problems if files not deleted
**
**	Revision 1.21  2001-10-10 15:20:01-04  gsl
**	Remove VMS and MSDOS
**
**	Revision 1.20  1998-12-09 10:55:57-05  gsl
**	Add tracing and renamed globals
**
**	Revision 1.19  1998-10-09 15:04:25-04  gsl
**	In LINKGARGS() fixed parameter to ACUGARGS which was too small.
**
**	Revision 1.18  1998-08-03 16:52:00-04  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks
**
**	Revision 1.17  1996-09-10 11:44:22-04  gsl
**	Remove redefine of mkdir() - now in win32std.h from idsistd.h
**
**	Revision 1.16  1996-08-28 17:55:34-07  gsl
**	Document some routines
**
**	Revision 1.15  1996-08-23 14:01:21-07  gsl
**	Changed to use wisplinkdir()
**
**	Revision 1.14  1996-08-19 15:32:26-07  gsl
**	drcs update
**
**
**
*/
