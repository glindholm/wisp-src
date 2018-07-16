			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		node.c
**
**	Purpose:	Node manipulation routines.
**
**	Routines:	
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#include "wmalloc.h"
#include "token.h"
#include "node.h"


NODE makenode(type,down,next,token)
int	type;
NODE	down;
NODE	next;
TOKEN	*token;
{
	NODE	new;

	new = (NODE) wmalloc(sizeof(struct Node_struct));

	new->type = type;
	new->next = next;
	new->down = down;
	new->token = token;

	return(new);
}

NODE maketoknode(token)
TOKEN *token;
{
	return(makenode(NODE_TOKEN,NULL,NULL,token));
}

void free_token_from_node(the_node)
NODE the_node;
{
	if (the_node && the_node->token)
	{
		free_token(the_node->token);
		the_node->token = NULL;
	}
}

NODE cleartoknode(curr)
NODE curr;
{
	free_token_from_node(curr);
	delete_tree(curr->down);
	curr->down = NULL;
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
NODE tie_next(curr,new)
NODE curr;
NODE new;
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
NODE tie_down(curr,new)
NODE curr;
NODE new;
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
NODE tie_end(curr,end)
NODE curr;
NODE end;
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

NODE tie_bottom(curr,bottom)
NODE curr;
NODE bottom;
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

NODE tie_end_many(count,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
int	count;
NODE	a1,a2,a3,a4,a5,a6,a7,a8,a9,a10;
{
	switch(count)
	{
	case  1: return(a1);	break;
	case  2: return(tie_end(a1,a2));	break;
	case  3: return(tie_end(a1,tie_end_many(2,a2,a3)));	break;
	case  4: return(tie_end(a1,tie_end_many(3,a2,a3,a4)));	break;
	case  5: return(tie_end(a1,tie_end_many(4,a2,a3,a4,a5)));	break;
	case  6: return(tie_end(a1,tie_end_many(5,a2,a3,a4,a5,a6)));	break;
	case  7: return(tie_end(a1,tie_end_many(6,a2,a3,a4,a5,a6,a7)));	break;
	case  8: return(tie_end(a1,tie_end_many(7,a2,a3,a4,a5,a6,a7,a8)));	break;
	case  9: return(tie_end(a1,tie_end_many(8,a2,a3,a4,a5,a6,a7,a8,a9)));	break;
	case 10: return(tie_end(a1,tie_end_many(9,a2,a3,a4,a5,a6,a7,a8,a9,a10)));	break;
	}
	return(NULL);
}

delete_tree(tree)
NODE tree;
{
	if (!tree) return;

	delete_tree(tree->down);
	delete_tree(tree->next);

	free_token(tree->token);
	wfree(tree);
	return;
}
