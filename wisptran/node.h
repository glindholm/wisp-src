			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		node.h
**
**	Purpose:	To define the NODE structure.
**
**
**	History:
**	05/19/93	Written by GSL
**
*/

#ifndef NODE_H
#define NODE_H

#include "token.h"

#ifndef NULL
#define NULL	((void *)0)
#endif

struct Node_struct
{
	int		type;
#define		NODE_UNKNOWN	0
#define		NODE_START	1
#define		NODE_END	2
#define		NODE_TOKEN	3
#define		NODE_DATAITEM	4
#define		NODE_STATEMENT	5
#define		NODE_PARENS	6
	struct Node_struct	*next;
	struct Node_struct	*down;
	TOKEN			*token;
};
typedef struct Node_struct * NODE;

NODE makenode();
NODE maketoknode();
NODE cleartoknode();
NODE tie_next();
NODE tie_down();
NODE tie_bottom();
NODE tie_end();

#endif /* NODE_H */

