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
**	File:		putparm.h
**
**	Purpose:	To declare function headers for PUTPARM
**
**
**	History:	
**	08/31/92	Written by GSL
**	12/11/92	Added mergex_fmtlist. GSL
**
*/

#ifndef putparm_H
#define putparm_H

#include "sharemem.h"

extern int WL_write_fmtlist(SHMH *prb, FMTLIST *fmtlist);
extern int WL_size_fmtlist(FMTLIST *fmtlist, int *cnt, int *mem);
extern int WL_load_fmtlist(SHMH *parm_area, FMTLIST **fmtlist_ptr);
extern int WL_mergex_fmtlist(FMTLIST *src, FMTLIST *dest);
extern int WL_merge_fmtlist(FMTLIST *src, FMTLIST **dest_ptr);
extern int WL_free_fmtlist(FMTLIST *fmtlist);
extern KEYWSHM *WL_find_prb_keyword(SHMH *parm_a, char *key, int kw_num);
extern int WL_search_parm_area(char *dest, char *key, int4 maxlen, SHMH *parm_a);

#endif


/*
**	History:
**	$Log: putparm.h,v $
**	Revision 1.8  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.7  2002/07/11 20:29:21  gsl
**	Fix WL_ globals
**	
**	Revision 1.6  1996/07/23 18:17:49  gsl
**	drcs update
**	
**
**
*/
