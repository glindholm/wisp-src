/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		vsemov.h
**
**	Purpose:	To ...
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#ifndef vsemov_H
#define vsemov_H

#include "idsistd.h"
#include "vseglb.h"

void vse_mov_lin(void);
void vse_cpy_lin(void);
int vse_renumberer(int4 number, int4 incr, int4 start, int4 end, int4 *count);
int do_renumber(TEXT *txt_start, TEXT *txt_end, int4 number, int4 incr);
int validate_numincr(char *number_field, int4 *number, char *incr_field, int4 *incr);
int vse_xcopy(void);

#endif /* vsemov_H */
/*
**	History:
**	$Log: vsemov.h,v $
**	Revision 1.5  1996/09/03 22:24:09  gsl
**	drcs update
**	
**
**
*/
