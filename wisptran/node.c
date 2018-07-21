/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/


/*
**	File:		node.c
**
**	Purpose:	Node manipulation routines.
**
**	Routines:	
**
*/

#include "proto.h"
#include "wmalloc.h"
#include "token.h"
#include "node.h"


NODE makenode(enum Node_type type, NODE down, NODE next, TOKEN *token)
{
	NODE	new;

	new = (NODE) wmalloc(sizeof(struct Node_struct));

	new->type = type;
	new->next = next;
	new->down = down;
	new->token = token;

	return(new);
}

NODE maketoknode(TOKEN *token)
{
	return(makenode(NODE_TOKEN,NULL,NULL,token));
}

NODE free_token_from_node(NODE the_node)
{
	if (the_node && the_node->token)
	{
		free_token(the_node->token);
		the_node->token = NULL;
	}

	return the_node;
}

NODE cleartoknode(NODE curr)
{
	free_token_from_node(curr);
	delete_tree(curr->down);
	curr->down = NULL;
	return(curr);
}

/*
**	Routine:	tie_next()
**
**	Function:	Ties a node into the next list after the current node.
**
**	Description:	To ...
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	mm/dd/yy	Written by xxx
**
*/
NODE tie_next(NODE curr, NODE new)
{
	new->next = curr->next;
	curr->next = new;	
	return(new);
}

/*
**	Routine:	tie_down()
**
**	Function:	Ties a node into the down list after the current node.
**
**	Description:	To ...
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	mm/dd/yy	Written by xxx
**
*/
NODE tie_down(NODE curr, NODE new)
{
	new->down = curr->down;
	curr->down = new;
	return(new);
}

/*
**	Routine:	tie_end()
**
**	Function:	Ties a node to the end of the next chain.
**
**	Description:	To ...
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	mm/dd/yy	Written by xxx
**
*/
NODE tie_end(NODE curr, NODE end)
{
	if (!curr) return(end);

	if (!curr->next)
	{
		curr->next = end;
	}
	else
	{
		tie_end(curr->next,end);
	}
	return(curr);
}

NODE tie_bottom(NODE curr, NODE bottom)
{
	if (!curr) return(bottom);

	if (!curr->down)
	{
		curr->down = bottom;
	}
	else
	{
		tie_bottom(curr->down,bottom);
	}
	return(curr);
}

NODE tie_end_many(int count, NODE a1, NODE a2, NODE a3, NODE a4, NODE a5, NODE a6, NODE a7, NODE a8, NODE a9, NODE a10)
{
	if (1 == count)
	{
		return(a1);
	}
	else if (2 == count)
	{
		return(tie_end(a1,a2));
	}
	else if ( count > 2 )
	{
		return(tie_end(a1,tie_end_many(count-1,a2,a3,a4,a5,a6,a7,a8,a9,a10,NULL)));
	}
	else
	{
		return(NULL);
	}
}

NODE delete_tree(NODE tree)
{
	if (!tree) return NULL;

	delete_tree(tree->down);
	delete_tree(tree->next);

	free_token(tree->token);
	wfree(tree);
	return NULL;
}

/*
**	Routine:	unhook_sub_tree()
**
**	Function:	Unhook a sub tree from a parent tree.
**
**	Description:	Parent_tree will have a node which points to sub_tree.
**			Find that node an replace the pointer with NULL.
**
**	Arguments:
**	parent_tree	The tree that contains sub_tree.
**	sub_tree	The tree to unhook.
**
**	Globals:	None
**
**	Return:		The sub_tree of NULL if not found.
**
**	Warnings:	None
**
**	History:	
**	07/22/94	Written by GSL
**
*/
NODE unhook_sub_tree(NODE parent_tree, NODE sub_tree)
{

	if (!parent_tree) return NULL;

	if (parent_tree->next == sub_tree)
	{
		parent_tree->next = NULL;
		return sub_tree;
	}
	if (parent_tree->down == sub_tree)
	{
		parent_tree->down = NULL;
		return sub_tree;
	}

	if (unhook_sub_tree(parent_tree->down,sub_tree))
	{
		return sub_tree;
	}

	return unhook_sub_tree(parent_tree->next,sub_tree);
}

/*
**	ROUTINE:	shiftinsert_clause_before_node()
**
**	FUNCTION:	Insert a node tree before given node.
**
**	DESCRIPTION:	The given node is copied after itself then the original is
**			changed into a STATEMENT with the new clause tied down.
**
**	ARGUMENTS:	
**	curr_node	The node to insert before
**	clause_node	The clause to insert.
**
**	GLOBALS:	None
**
**	RETURN:		The new curr_node.
**
**	WARNINGS:	None
**
*/
NODE shiftinsert_clause_before_node(NODE curr_node, NODE clause_node)
{
	/*
	**	Create a new node after curr_node with all the values from curr_node.
	*/
	curr_node->next = makenode(curr_node->type, curr_node->down, curr_node->next, curr_node->token);
	curr_node->down = clause_node;
	curr_node->token = NULL;
	curr_node->type = NODE_STATEMENT;

	return curr_node->next;
}


/*
**	History:
**	$Log: node.c,v $
**	Revision 1.10  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.9  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.8  1998/02/10 20:05:28  gsl
**	Added shiftinsert_clause_before_node()
**	
**	Revision 1.7  1996-08-30 21:56:06-04  gsl
**	drcs update
**
**
**
*/
