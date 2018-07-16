/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
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
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/


/*----
This is the initial input GETPARM screen. if vse_native is true 
it displays the getparm as a system_name style getparm,
otherwise it uses a std wang style input.
It is passed an error message.
It returns the value of the pressed pfkey
------*/
#include "vsedit.h"
#include "vseglb.h"
#include "vsegp.h"
#include "vseutl.h"
#include "wisplib.h"

#ifdef unix
static char lang_opts[]="(COBOL=wcb/cob,PROCEDURE=wps,BASIC=bas,SHELL=sh,none)";
#endif
#ifdef WIN32
static char lang_opts[]="(COBOL=wcb/cob,PROCEDURE=wps,BASIC=bas,BATCH=bat,none)";
#endif
static char specify1[]="Please specify the name of the file to be edited.";
static char specify2[]="When creating a new file, leave the file name blank.";
static char specify3[]="Please specify the library and volume for:";
static char source[]="Source files . . . . . . . . . .";
static char go_native[]="(Press (5) for NATIVE  entry mode or (16) to exit.)";
static char go_vs[]    ="(Press (5) for GENERIC entry mode or (16) to exit.)";

int vse_input(char *emsg)
{
	char	error_message[81];

	
	if (strlen(emsg) > 0)
	{
		sprintf(error_message,"\224SORRY\204- %.70s",emsg);
	}
	else
	{
		error_message[0] = (char)0;
	}

	gppfkeys=GP_PF_05|GP_PF_16;
	WL_wswap(&gppfkeys);

	GPSETUP();
	GPSTD("INPUT   ","EDITOR",1);
	GPMSG(VSE_COPYRIGHT);
	GPCTEXT(error_message,9,2);
	GPKW("LANGUAGE",vse_gp_input_language,VSE_LANGUAGE_LEN,11,2,"U");
	GPCTEXT(lang_opts,11,23);
	GPCTEXT(specify1,14,24);
	GPCTEXT(specify2,15,24);
	if(vse_native)
	{
		vse_untrunc(vse_sysname,VSE_SYSNAME_LEN);
		GPSYSNAME(vse_sysname,18);
		GPCTEXT(go_vs,24,14);
	}
	else
	{
		GPCTEXT(specify3,17,2);
		GPFILE(vse_gp_input_file,14,2);
		GPCTEXT(source,18,5);
		GPLIB(vse_gp_input_library,18,38);
		GPVOL(vse_gp_input_volume,18,61);
		GPCTEXT(go_native,24,14);
	}

	GPENTER();
	GPPFS(&gppfkeys);

	vse_input_pick = display_and_read_gp();

	leftjust(vse_gp_input_language,VSE_LANGUAGE_LEN);

	if (vse_native)
	{
		vse_trunc(vse_sysname);
	}

	return(vse_input_pick);
}

/*
**	History:
**	$Log: vseinp.c,v $
**	Revision 1.15  2010/01/10 00:36:15  gsl
**	refactor utils to add vse_ prefix to avoid conflicts with trunc
**	vse_trunc
**	
**	Revision 1.14  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.13  2002/07/12 17:17:07  gsl
**	Global unique WL_ changes
**	
**	Revision 1.12  2002/06/26 01:42:50  gsl
**	Remove VMS code
**	
**	Revision 1.11  1996/09/03 22:24:07  gsl
**	drcs update
**	
**
**
*/
