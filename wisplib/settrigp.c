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


#include "idsistd.h"
#include "wfiles.h"
#include "wcommon.h"
#include "werrlog.h"
#include "wfname.h"
#include "filext.h"

static char trigpname[256];								/* The native trigger file name.	*/

int SETTRIGPROG(char* vol,	/* the WANG volume name	(6 chars)	*/
		char* lib,	/* The WANG library name (8 chars)	*/
		char* file)	/* The file name	(8 chars)	*/
{
	int4 	mode;									/* The mode of opening			*/
	char	*ptr;

	WL_wtrace("SETTRIGPROG","ENTRY","Entry into SETTRIGPROG(%6.6s, %8.8s, %8.8s)",
		vol, lib, file);

	mode = 0;
	ptr=WL_wfname(&mode,vol,lib,file,trigpname);					/* Get the native system name.		*/
	*ptr = '\0';									/* Be sure to null terminate.		*/

	return(1);									/* Return Success.			*/
}
/*
**	History:
**	$Log: settrigp.c,v $
**	Revision 1.15  2003/03/19 21:11:44  gsl
**	Remove USE_PVPL flag
**	
**	Revision 1.14  2003/01/31 18:54:38  gsl
**	Fix copyright header
**	
**	Revision 1.13  2002/12/09 21:09:31  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.12  2002/07/12 19:10:16  gsl
**	Global unique WL_ changes
**	
**	Revision 1.11  2002/06/21 20:49:29  gsl
**	Rework the IS_xxx bit flags and the WFOPEN_mode flags
**	
**	Revision 1.10  2002/06/21 03:10:41  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.9  1996/08/19 22:32:55  gsl
**	drcs update
**	
**
**
*/
