static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

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
#ifdef VMS
static char lang_opts[]="(COBOL=wcb/cob,PROCEDURE=wps,BASIC=bas,COM=com,none)";
#endif
#if defined(MSDOS) || defined(WINNT)
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
	wswap(&gppfkeys);

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
		untrunc(vse_sysname,VSE_SYSNAME_LEN);
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
		trunc(vse_sysname);
	}

	return(vse_input_pick);
}

/*
**	History:
**	$Log: vseinp.c,v $
**	Revision 1.11  1996-09-03 18:24:07-04  gsl
**	drcs update
**
**
**
*/
