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
**	File:		vsetxt.h
**
**	Purpose:	To ...
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#ifndef vsetxt_H
#define vsetxt_H

#include "vseglb.h"

TEXT *new_text(char *str);
TEXT *over_text(TEXT *txt, char *str);
void insert_text(TEXT *txt1, TEXT *txt2, TEXT *newtxt);
void del_text(TEXT *txt);
void append_text(TEXT *txt);
void free_text(void);
void free_text_list(TEXT *first);
void free_one_text(TEXT *txt);


#endif /* vsetxt_H */
/*
**	History:
**	$Log: vsetxt.h,v $
**	Revision 1.5  1996/09/03 22:24:12  gsl
**	drcs update
**	
**
**
*/
