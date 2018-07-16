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
**	File:		vsebasic.h
**
**	Purpose:	Header for vsebasic.c
**
**
**	History:
**	10/26/94	Written by GSL
**
*/

#ifndef vsebasic_H
#define vsebasic_H

#include "idsistd.h"
#include "vseglb.h"

void init_hash(void);
void find_linenum( TEXT *txt );
void update_linenum( int4 from, int4 to );
void delete_linenum( int4 lineno );
void update_branch(void);
void free_linenum(void);

#endif /* vsebasic_H */
/*
**	History:
**	$Log: vsebasic.h,v $
**	Revision 1.5  1996/09/03 22:24:00  gsl
**	drcs update
**	
**
**
*/
