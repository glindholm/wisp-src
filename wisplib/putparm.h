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

int	PUTPARM();
int	write_fmtlist();
int	size_fmtlist();
int	load_fmtlist();
int	mergex_fmtlist();
int	merge_fmtlist();
int	free_fmtlist();
KEYWSHM *find_prb_keyword();
int	search_parm_area();

#endif


