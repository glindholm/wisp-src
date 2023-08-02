/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
**
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
**
******************************************************************************
*/


/** 
 ** program: wdelwrk
 **
 ** ident: "@(#)wdelwrk.c        1.0      ULTRIX/SYS5/AIX  3/27/90"
 ** 
 ** purpose: clean out wisp work files
 **
 **
 **/
#ifdef unix

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "filext.h"
#include "wcommon.h"
#include "wperson.h"
#include "intdef.h"
#include "wfname.h"
#include "wisplib.h"

int main()
{
	char	fullpath[100],cmd[100];							/* work buffers */
	char	def_workvol[6], def_worklib[8];
	char	*end_ptr;
	
	WL_initglbs("WDELWRK ");

	WL_get_defs(DEFAULTS_WV,def_workvol);
	WL_get_defs(DEFAULTS_WL,def_worklib);

	end_ptr = WL_wanglib2path(def_workvol, def_worklib, fullpath);
	*end_ptr = '\0';
	end_ptr--;
	if ( *end_ptr == '/' ) *end_ptr = '\0';

	sprintf(cmd,"rm -fr %s",fullpath);						/* build a command line */
	system(cmd);									/* do it  */

	return 0;
}	

#include "wutils.h"
#endif /* unix */

/*
**	History:
**	$Log: wdelwrk.c,v $
**	Revision 1.17  2003/02/04 20:42:49  gsl
**	fix -Wall warnings
**	
**	Revision 1.16  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.15  2002/07/12 19:10:23  gsl
**	Global unique WL_ changes
**	
**	Revision 1.14  2002/07/11 16:04:52  gsl
**	Fix warnings
**	
**	Revision 1.13  2002/07/10 21:06:30  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.12  2002/06/28 04:02:57  gsl
**	Work on native version of wfopen and wfname
**	
**	Revision 1.11  2002/06/26 01:42:47  gsl
**	Remove VMS code
**	
**	Revision 1.10  2002/06/25 18:18:35  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.9  1997/06/10 19:52:29  scass
**	Changed long to int4 for portability.
**	
**	Revision 1.8  1996-07-23 14:13:07-04  gsl
**	drcs update
**
**
**
*/
