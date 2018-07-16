static char copyright[]="Copyright (c) 1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wshell.c
**
**	Project:	WISPUTILS
**
**	RCS:		$Source:$
**
**	Purpose:	Wang Command Processor
**
**	Routines:	
*/

/*
**	Includes
*/
#include <stdio.h>
#include <stdlib.h>

#include "vwang.h"
#include "wexit.h"
#include "wglobals.h"
#include "werrlog.h"
#include "wisplib.h"

#define EXT_FILEXT
#include "filext.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/
extern int  noprogscrn;
void license_warning(void);

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/


int main(int argc, char* argv[])
{
	char scrn[1924];
	char function,lines,term[2],no_mod[2];

	initglbs("WSHELL  ");
	strcpy(wisp_progname,"WSHELL");
	strcpy(wisp_screen,"WSHELL");

	noprogscrn=1;				/* No program screen to print.		*/

	vwang_title("WISP SHELL");
	init_screen();
	wsc_init(scrn,0,0);

	function = WRITE_ALL;
	lines = 24;

	license_warning();
	
	/* Call Wang emulation to fill screen.	*/
	/* So HELP will have a screen to push.	*/
	vwang(&function,scrn,&lines,"0001X",term,no_mod);

	wsh_help(0);

	wexit(0);
	return 0;
}

/*
**	History:
**	$Log: wshell.c,v $
**	Revision 1.5  1997-04-15 18:06:55-04  gsl
**	Removed the setting of w_err_flag as it is now done in initglbs()
**
**	Revision 1.4  1997-03-21 10:36:02-05  gsl
**	Add call to license_warning() so that wshell checks if licensed
**
**	Revision 1.3  1996-11-18 18:54:21-05  jockc
**	added call to vwang_title to set screen title
**
**	Revision 1.2  1996-07-24 10:24:51-07  gsl
**	Add missing include
**
**	Revision 1.1  1996-07-24 10:16:38-07  gsl
**	Initial revision
**
**
**
**
*/
