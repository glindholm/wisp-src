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


/*
**	File:		link.h
**
**	Purpose:	To hold defines and structs used in linking.
**
**
**	History:	
**	12/22/92	Written by GSL
**
*/

#ifndef LINK_H
#define LINK_H

#include "idsistd.h"

#define 	MAX_LINK_PARMS		32						/* Maximum number of parameters to LINK	*/
#define 	NULL_FILE_NAME		"*NULL*"					/* No LINK arg file, See VMSLINKSUB.WCB	*/

struct	str_parm	{ char *parm[MAX_LINK_PARMS]; };			/* Structure for *parm_list function argument.	*/
struct	str_len		{ int4   len[MAX_LINK_PARMS]; };			/* Structure for  *len_list function argument.	*/

void WL_writeunixlink(const char *pname, int parmcnt, struct str_parm *parm_list, struct str_len *len_list, char *linkkey);
void WL_readunixlink(int parmcnt, struct str_parm *parm_list, struct str_len *len_list, const char *linkkey, 
		  int4 *compcode, int4 *returncode);

void WL_call_acucobol(char* name, int parmcnt, char* parms[], int lens[], int* rc);
void WL_call_acucobol_error(int rc, int4 *wang_retcode, int4 *wang_compcode, char *link_filespec);

void WL_call_mfcobol(char* name, int parmcnt, char* parms[], int lens[], int* rc);

void WL_wwaitpid(int pid, int* rc);	/* Wait for process pid to complete		*/

#endif /* LINK_H */



/*
**	History:
**	$Log: link.h,v $
**	Revision 1.12  2003/08/25 21:10:18  gsl
**	MF Native Screens
**	
**	Revision 1.11  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.10  2002/07/12 19:10:24  gsl
**	Global unique WL_ changes
**	
**	Revision 1.9  2002/07/10 21:06:34  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.8  1996/09/05 00:20:22  gsl
**	moved softlink prototypes to wperson.h
**	
**	Revision 1.7  1996-09-03 14:42:25-07  gsl
**	Add prototypes for USESOFTLINK() and USEHARDLINK()
**
**	Revision 1.6  1996-08-28 17:54:50-07  gsl
**	Add prototypes
**
**	Revision 1.5  1996-07-23 11:17:48-07  gsl
**	drcs update
**
**
**
*/
