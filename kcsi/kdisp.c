/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

/*----
Interface between REPORT INQUIRY and CONTROL to new
IDSI WISP display.
------*/
#include <stdio.h>
#include "kwisp.h"
#include "kcsifunc.h"



#define	FILE_NAME		0
#define	FILE_LIB		8
#define	FILE_VOL		16
#define	FILE_SYS_NAME		22
#define	FILE_SYS_NAME_LEN	81
#define	FILE_SPEC_LEN		(22 + FILE_SYS_NAME_LEN)

#define	DISP_OK			"00"
#define	INVALID_TYPE		"99"
#define	OPEN_ERROR		"95"

#define Set_return(y)		memcpy(rc,y,2);

static void do_display(char *name,char *rc);
char *WL_wfname(int4 *mode, char *p_vol, char *p_lib, char *p_file, char *native_path);

static FILE *kdf;
/*----
Receives a file spec type and return code. Formats a Unix name
and checks for existence as is, and with .prt extension. Result
if found is passed to IDSI display.
------*/
void KDISP(char* fspec,char* type, char* rc)
{
	int4 mode;
	char *eoname;

	Set_return(DISP_OK);

/* Only 'P'rint type is legal for now */
	if(*type != 'P')
		{
		Set_return(INVALID_TYPE);
		return;
		}
/* Build the name */
	mode = IS_PRINTFILE;
	eoname = WL_wfname(&mode,
		&fspec[FILE_VOL],
		&fspec[FILE_LIB],
		&fspec[FILE_NAME],
		&fspec[FILE_SYS_NAME]);

	*eoname = 0;
	do_display(&fspec[FILE_SYS_NAME],rc);
}
static void do_display(char *name,char *rc)
{

/* Try the file name as is */
	kdf = fopen(name,"r");
	if(kdf)
	   fclose(kdf);
	else
	    {
	    strcat(name,".prt");
	    kdf = fopen(name,"r");
	    if(kdf)
		fclose(kdf);
	    else
		{
		Set_return(OPEN_ERROR);
		return;
		}
	    }
/* Otherwise we found it one way or the other so display it */
	WL_link_display(name);
}
/*
**	History:
**	$Log: kdisp.c,v $
**	Revision 1.9  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.8  2002/07/12 19:10:24  gsl
**	Global unique WL_ changes
**	
**	Revision 1.7  2002/07/10 21:06:25  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.6  2002/06/21 20:48:15  gsl
**	Rework the IS_xxx bit flags and now include from wcommon.h instead of duplicate
**	definitions.
**	
**	Revision 1.5  1997/10/23 17:56:25  gsl
**	CHanged to use link_display*(
**	
**	Revision 1.4  1996-09-17 19:45:40-04  gsl
**	drcs update
**
**
**
*/
