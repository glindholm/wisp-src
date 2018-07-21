/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
*/


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
**	Revision 1.6  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.5  1996/08/31 01:56:04  gsl
**	drcs update
**	
**
**
*/
