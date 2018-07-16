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
**	File:		vsedfnd.h
**
**	Purpose:	To ...
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#ifndef vsedfnd_H
#define vsedfnd_H

#include "idsistd.h"

void restart_find(void);
void vse_ed_find(void);
int vse_ed_change(void);
int validate_range(char *start_field, int4 *start_line, char *end_field, int4 *end_line);
int validate_linenum(char *num_string, int4 *num);
int get_line_and_index(int row, int4 *line);

#endif /* vsedfnd_H */
/*
**	History:
**	$Log: vsedfnd.h,v $
**	Revision 1.5  1996/09/03 22:24:01  gsl
**	drcs update
**	
**
**
*/
