/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

/*
**	File:		wshell.c
**
**	Project:	WISPUTILS
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

#include "filext.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/
void WL_license_warning(void);

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/


int main(int argc, char* argv[])
{
	unsigned char scrn[1924];
	unsigned char function,lines,no_mod[2];
	char term[2];

	WL_initglbs("WSHELL  ");
	wisp_set_progname("WSHELL");
	wisp_set_screenname("WSHELL");

	wisp_set_noprogscrn(1);				/* No program screen to print.		*/

	vwang_title("WISP SHELL");
	vwang_init_screen();
	WL_wsc_init(scrn,0,0);

	function = WRITE_ALL;
	lines = 24;

	WL_license_warning();
	
	/* Call Wang emulation to fill screen.	*/
	/* So HELP will have a screen to push.	*/
	vwang(&function,scrn,&lines,"0001X",term,no_mod);

	WL_wsh_help(0);

	WL_wexit(0);
	return 0;
}

/*
**	History:
**	$Log: wshell.c,v $
**	Revision 1.11  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.10  2002/08/01 15:07:34  gsl
**	type warnings
**	
**	Revision 1.9  2002/07/11 20:29:22  gsl
**	Fix WL_ globals
**	
**	Revision 1.8  2002/07/10 21:06:32  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.7  2002/07/09 04:13:48  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.6  2002/06/25 18:18:37  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.5  1997/04/15 22:06:55  gsl
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
