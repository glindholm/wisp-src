/*
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
*/


/*
	werrpath.c	Initialize werrlog_path.
*/

#include "idsistd.h"
#include "wdefines.h"
#include "wglobals.h"
#include "idsisubs.h"
#include "wperson.h"
#include "wmalloc.h"
#include "wispcfg.h"

const char* WL_werrpath(void)
{
	static char *the_path=NULL;

	if (!the_path)
	{
		char buff[128];

		buildfilepath( buff, wisphomedir(NULL), WISP_ERROR_FILE );
		the_path = wisp_strdup(buff);
	}
	return the_path;
}

/*
**	History:
**	$Log: werrpath.c,v $
**	Revision 1.14  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.13  2002/07/10 21:05:29  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.12  2002/07/02 21:15:32  gsl
**	Rename wstrdup
**	
**	Revision 1.11  1996/10/09 00:28:18  gsl
**	Add include wispcfg.h
**	
**	Revision 1.10  1996-08-23 14:06:19-07  gsl
**	Changed to use wisphomedir()
**
**	Revision 1.9  1996-08-19 15:33:11-07  gsl
**	drcs update
**
**
**
*/
