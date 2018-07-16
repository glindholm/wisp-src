/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		keylist.h
**
**	Purpose:	To define items needed for process file KEY clauses.
**
**
**	History:
**	05/07/93	Split these apart from wisp.h. GSL
**
*/

#ifndef KEYLIST_H
#define KEYLIST_H


#define MAX_KEY_LIST		100							/* Maximum number fo keys to correct.	*/
#define KL_LEAD		1								/* Key list type LEADING SEPARATE	*/
#define KL_COMP		2								/*               COMPUTATIONAL		*/
struct key_list_struct
{
	char	name[40];
	char	qual[40];
	short	type;
};
typedef struct key_list_struct key_list_struct;

EXT key_list_struct key_list[MAX_KEY_LIST];						/* The list of keys.			*/
EXT int kl_count	INIT_FALSE;							/* How many are there.			*/


#endif /* KEYLIST_H */
/*
**	History:
**	$Log: keylist.h,v $
**	Revision 1.5  1996-08-30 21:56:04-04  gsl
**	drcs update
**
**
**
*/
