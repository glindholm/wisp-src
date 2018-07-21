/*
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
*/


/*
**									
**			WGLOBALS.C	-	Wisp Global variables.	
**									
**			Any global variables used by WISP can be placed here.
**			This module can be linked with modules that need these
**			globals but do not need every module from WISP too.
*/


#include <time.h>

#define EXT_WGLOBALS
#include "idsistd.h"
#include "wglobals.h"
#include "cobrun.h"

void WL_globals()
{
	return;	
}

static int LINKPARM = 0;				/* Were we started by a LINK		*/
void wisp_set_LINKPARM(int4 i)	{ LINKPARM = i;}
int4 wisp_get_LINKPARM()	{ return LINKPARM;}

static int4 LINKCOMPCODE = 0;
void wisp_set_LINKCOMPCODE(int4 i)	{ LINKCOMPCODE = i;}
int4 wisp_get_LINKCOMPCODE()		{ return LINKCOMPCODE;}

static int4 LINKRETCODE = 0;
void wisp_set_LINKRETCODE(int4 i)	{ LINKRETCODE = i;}
int4 wisp_get_LINKRETCODE()		{ return LINKRETCODE;}

static int4 LOGOFFFLAG = 0;
void wisp_set_LOGOFFFLAG(int4 i)	{ LOGOFFFLAG = i;}
int4 wisp_get_LOGOFFFLAG()		{ return LOGOFFFLAG;}

static int4 CANEXITFLAG = 0;
void wisp_set_CANEXITFLAG(int4 i)	{ CANEXITFLAG = i;}
int4 wisp_get_CANEXITFLAG()		{ return CANEXITFLAG;}

static time_t WSTARTTIME = 0;				/* The start time of the process	*/
void wisp_set_WSTARTTIME()		{ time(&WSTARTTIME); }
time_t wisp_get_WSTARTTIME()		{ return WSTARTTIME;}

static int noswap_words = 0;				/* Opposite of swap_words		*/
void wisp_set_noswap(int i)		{ noswap_words = i; }
int  wisp_get_noswap()			{ return noswap_words; }

static char WISPRUNNAME[WISP_RUNNAME_SIZE+1] = { "        " };	/* Define the Run name field. 		*/
void wisp_set_runname(const char name[WISP_RUNNAME_SIZE]) 
{
	memcpy(WISPRUNNAME, name, WISP_RUNNAME_SIZE);
	WISPRUNNAME[WISP_RUNNAME_SIZE] = '\0';
}
const char* wisp_get_runname()		{ return WISPRUNNAME; }

/*
**	COBOL type and LOCK values
**	- Default to Micro Focus on Unix(because ACU can be set in sub85.c)
*/
#define FILELOCK_MF	"9A"
#define HARDLOCK_MF	"9D"
#define FILELOCK_ACU	"93"
#define HARDLOCK_ACU	"99"

#ifdef WIN32
#define FILELOCK_DEF	FILELOCK_ACU
#define HARDLOCK_DEF	HARDLOCK_ACU
#else
#define FILELOCK_DEF	FILELOCK_MF
#define HARDLOCK_DEF	HARDLOCK_MF
#endif

const char* wisp_get_hardlock()		
{
	if (wisp_acu_cobol())
	{
		return HARDLOCK_ACU;
	}
	if (wisp_mf_cobol())
	{
		return HARDLOCK_MF;
	}

	return HARDLOCK_DEF; 
}
const char* wisp_get_filelock()		
{ 
	if (wisp_acu_cobol())
	{
		return FILELOCK_ACU;
	}
	if (wisp_mf_cobol())
	{
		return FILELOCK_MF;
	}

	return FILELOCK_DEF; 
}


static char wisp_progname[PROGNAME_SIZE+1] = {""};
void wisp_set_progname(const char* progname)
{
	int i;
	for (i=0; i<PROGNAME_SIZE; i++)
	{
		if (progname[i] == '\0' ||
		    progname[i] == ' ')
		{
			break;
		}
		wisp_progname[i] = progname[i];
	}
	wisp_progname[i] = '\0';
}
const char* wisp_get_progname()
{
	return wisp_progname;
}

static char wisp_screen[SCREENNAME_SIZE+1] = {""};
void wisp_set_screenname(const char* screenname)
{
	int i;
	for (i=0; i<SCREENNAME_SIZE; i++)
	{
		if (screenname[i] == '\0' ||
		    screenname[i] == ' ')
		{
			break;
		}
		wisp_screen[i] = screenname[i];
	}
	wisp_screen[i] = '\0';
}
const char* wisp_get_screenname()
{
	return wisp_screen;
}

static int  noprogscrn = 0; /* No program screen flag.		*/
void wisp_set_noprogscrn(int flag)	{ noprogscrn = flag; }
int  wisp_get_noprogscrn()		{ return noprogscrn; }


/*
**	History:
**	$Log: wglobals.c,v $
**	Revision 1.19  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.18  2002/10/18 19:14:07  gsl
**	Cleanup
**	
**	Revision 1.17  2002/10/18 17:32:55  gsl
**	Identify if in a  COBOL runtime
**	
**	Revision 1.16  2002/10/11 20:39:51  gsl
**	Detect runtime Cobol type without needing INITWISP call.
**	For ACU set in sub85.c,
**	For utils set via WRUNCONFIG
**	Default to MF on UNIX
**	
**	Revision 1.15  2002/07/12 19:10:20  gsl
**	Global unique WL_ changes
**	
**	Revision 1.14  2002/07/11 20:29:18  gsl
**	Fix WL_ globals
**	
**	Revision 1.13  2002/07/09 04:13:53  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.12  2002/07/02 04:00:37  gsl
**	change acu_cobol and mf_cobol to wisp_acu_cobol() and wisp_mf_cobol()
**	
**	Revision 1.11  2002/07/01 04:02:43  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.10  2002/06/25 18:18:40  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.9  1996/08/19 22:33:17  gsl
**	drcs update
**	
**
**
*/
