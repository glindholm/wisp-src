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
**	File:		vsedmod.h
**
**	Purpose:	To ...
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#ifndef vsedmod_H
#define vsedmod_H

#include "idsistd.h"
#include "vseglb.h"

void vse_ed_mod(void);
int mode_upper(void);
int language_case(void);
void vse_ed_mod_col(void);
int add_modcode(TEXT *txt);
void spaceout(char *str, int4 len);

#endif /* vsedmod_H */
/*
**	History:
**	$Log: vsedmod.h,v $
**	Revision 1.5  1996-09-03 18:24:03-04  gsl
**	drcs update
**
**
**
*/
