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

enum Node_type {NODE_UNKNOWN,
		NODE_START,
		NODE_END,
		NODE_TOKEN,
		NODE_DATAITEM,
		NODE_STATEMENT,
		NODE_PARENS };

struct Node_struct
{
	enum   Node_type	type;
	struct Node_struct	*next;
	struct Node_struct	*down;
	TOKEN			*token;
};
typedef struct Node_struct * NODE;

extern NODE makenode(enum Node_type type, NODE down, NODE next, TOKEN *token);
extern NODE maketoknode(TOKEN *token);
extern NODE free_token_from_node(NODE the_node);
extern NODE cleartoknode(NODE curr);
extern NODE tie_next(NODE curr, NODE new);
extern NODE tie_down(NODE curr, NODE new);
extern NODE tie_end(NODE curr, NODE new);
extern NODE tie_bottom(NODE curr, NODE new);
extern NODE tie_end_many(/* many */);
extern NODE delete_tree(NODE tree);
extern NODE unhook_sub_tree(NODE parent_tree, NODE sub_tree);
extern NODE shiftinsert_clause_before_node(NODE curr_node, NODE clause_node);

#endif /* NODE_H */

/*
**	History:
**	$Log: node.h,v $
**	Revision 1.7  1998/02/10 20:05:53  gsl
**	Added shiftinsert_clause_before_node()
**	
**	Revision 1.6  1996-08-30 21:56:06-04  gsl
**	drcs update
**
**
**
*/
