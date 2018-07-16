/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
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

extern int write_fmtlist(SHMH *prb, FMTLIST *fmtlist);
extern int size_fmtlist(FMTLIST *fmtlist, int *cnt, int *mem);
extern int load_fmtlist(SHMH *parm_area, FMTLIST **fmtlist_ptr);
extern int mergex_fmtlist(FMTLIST *src, FMTLIST *dest);
extern int merge_fmtlist(FMTLIST *src, FMTLIST **dest_ptr);
extern int free_fmtlist(FMTLIST *fmtlist);
extern KEYWSHM *find_prb_keyword(SHMH *parm_a, char *key, int kw_num);
extern int search_parm_area(char *dest, char *key, int4 maxlen, SHMH *parm_a);

#endif


/*
**	History:
**	$Log: putparm.h,v $
**	Revision 1.6  1996-07-23 14:17:49-04  gsl
**	drcs update
**
**
**
*/
